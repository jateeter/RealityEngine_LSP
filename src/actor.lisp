(in-package #:reality-engine-lsp)

(defstruct envelope
  message
  reply-lock
  reply-condition
  reply-value
  reply-error
  replied-p)

(defstruct (actor (:constructor %make-actor))
  name
  mailbox
  lock
  condition
  thread
  running-p
  state
  handler
  supervisor)

(defun make-actor (name handler &key state supervisor)
  (let* ((lock (bt:make-lock (format nil "~a-lock" name)))
         (condition (bt:make-condition-variable :name (format nil "~a-cv" name)))
         (actor (%make-actor :name name
                             :mailbox nil
                             :lock lock
                             :condition condition
                             :running-p t
                             :state state
                             :handler handler
                             :supervisor supervisor)))
    (setf (actor-thread actor)
          (bt:make-thread (lambda () (actor-loop actor))
                          :name (format nil "actor/~a" name)))
    actor))

(defun actor-loop (actor)
  (loop while (actor-running-p actor)
        for envelope = (dequeue-message actor)
        when envelope
          do (handler-case
                 (let ((result (funcall (actor-handler actor)
                                        (actor-state actor)
                                        (envelope-message envelope))))
                   (reply-envelope envelope result nil))
               (error (condition)
                 (when (actor-supervisor actor)
                   (funcall (actor-supervisor actor) actor condition))
                 (reply-envelope envelope nil condition)))))

(defun dequeue-message (actor)
  (bt:with-lock-held ((actor-lock actor))
    (loop while (and (actor-running-p actor) (null (actor-mailbox actor)))
          do (bt:condition-wait (actor-condition actor) (actor-lock actor)))
    (when (actor-mailbox actor)
      (let ((envelope (car (last (actor-mailbox actor)))))
        (setf (actor-mailbox actor) (butlast (actor-mailbox actor)))
        envelope))))

(defun reply-envelope (envelope value error)
  (when (envelope-reply-lock envelope)
    (bt:with-lock-held ((envelope-reply-lock envelope))
      (setf (envelope-reply-value envelope) value
            (envelope-reply-error envelope) error
            (envelope-replied-p envelope) t)
      (bt:condition-notify (envelope-reply-condition envelope)))))

(defun actor-tell (actor message)
  (bt:with-lock-held ((actor-lock actor))
    (push (make-envelope :message message) (actor-mailbox actor))
    (bt:condition-notify (actor-condition actor)))
  t)

(defun actor-ask (actor message &key (timeout 30))
  (let ((lock (bt:make-lock "ask-lock"))
        (condition (bt:make-condition-variable :name "ask-cv"))
        envelope
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (setf envelope (make-envelope :message message
                                  :reply-lock lock
                                  :reply-condition condition
                                  :replied-p nil))
    (bt:with-lock-held ((actor-lock actor))
      (push envelope (actor-mailbox actor))
      (bt:condition-notify (actor-condition actor)))
    (bt:with-lock-held (lock)
      (loop until (envelope-replied-p envelope)
            for remaining = (/ (- deadline (get-internal-real-time))
                               internal-time-units-per-second)
            do (when (<= remaining 0)
                 (error "Actor ask timed out for ~a" (actor-name actor)))
               (bt:condition-wait condition lock :timeout remaining))
      (unless (envelope-replied-p envelope)
        (error "Actor ask timed out for ~a" (actor-name actor)))
      (when (envelope-reply-error envelope)
        (error (envelope-reply-error envelope)))
      (envelope-reply-value envelope))))

(defun stop-actor (actor)
  (setf (actor-running-p actor) nil)
  (bt:with-lock-held ((actor-lock actor))
    (bt:condition-notify (actor-condition actor)))
  actor)

(defun state-actor (name state)
  (make-actor name
              (lambda (actor-state message)
                (etypecase message
                  (function (funcall message actor-state))
                  (cons (apply (car message) actor-state (cdr message)))))
              :state state
              :supervisor (lambda (actor condition)
                            (format *error-output* "~&actor ~a failed: ~a~%"
                                    (actor-name actor) condition))))
