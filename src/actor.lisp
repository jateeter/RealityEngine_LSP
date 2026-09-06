(in-package #:reality-engine-lsp)

;;;; Actor wrapper, on lparallel.
;;;;
;;;; This was 116 lines of hand-rolled synchronisation: a mailbox held as a list
;;;; with push/butlast, a lock and condition variable to signal arrivals, a
;;;; second lock and condition variable per reply, and a deadline loop around
;;;; `condition-wait` to implement ask-with-timeout. Every one of those is a
;;;; structure lparallel already provides, and provides in the primitive rather
;;;; than assembled from parts (RealityEngine_LSP#92, ARBITER_CONTRACT.md §7.4).
;;;;
;;;;   mailbox list + lock + condvar   ->  lparallel.queue:queue
;;;;     push / (car (last …)) / butlast was FIFO in O(n); a queue is FIFO in
;;;;     O(1), and `pop-queue` blocks natively, so the condition variable and
;;;;     the wait loop both disappear.
;;;;
;;;;   envelope reply-lock/-condition  ->  a one-shot reply queue
;;;;   /-value/-error/-replied-p           `try-pop-queue :timeout` is the read
;;;;                                       block, and it carries the deadline
;;;;                                       the old code spelled out by hand.
;;;;
;;;;   the dedicated bt:make-thread    ->  KEPT, deliberately.
;;;;
;;;; That last one matters. The actor exists to SERIALISE access to its state —
;;;; that is its whole purpose here, not a performance choice — and a worker
;;;; from the shared kernel cannot do it safely: a handler that asks another
;;;; actor would occupy a worker while waiting for a task that needs one, which
;;;; is pool starvation and deadlocks with a full kernel. lparallel supplies the
;;;; queues and the parallel map; it does not supply mutual exclusion, and
;;;; pretending otherwise is how this class of rewrite goes wrong.
;;;;
;;;; The parallelism lparallel is actually here for is INSIDE the handlers: see
;;;; `with-machine-snapshot` below, which is the fan-out #92 is about.

(defvar *kernel-worker-count* nil
  "Workers in the shared kernel. Set once at startup by `ensure-kernel`.")

(defun ensure-kernel (&optional worker-count)
  "Create the shared lparallel kernel if it does not exist.

   Sized once at startup rather than per request: a kernel spawns its workers on
   creation, so making one per call would cost more than the fan-out saves. The
   C++ engine reserves against a fixed pool for the same reason."
  (unless lparallel:*kernel*
    ;; No CPU-count dependency: the corpus fan-out is bounded by machine count
    ;; rather than by cores, and adding a package to ask the OS how many it has
    ;; buys a number this does not need to be exact. Overridable where a
    ;; deployment knows better than the default.
    (let ((n (or worker-count
                 (ignore-errors (parse-integer (or (uiop:getenv "RE_KERNEL_WORKERS") "")))
                 8)))
      (setf *kernel-worker-count* n
            lparallel:*kernel*
            (lparallel:make-kernel
             n
             :name "reality-engine-lsp"
             ;; Each worker gets its OWN *random-state*.
             ;;
             ;; `make-id` ends in `(random most-positive-fixnum)`, and `random`
             ;; mutates the random state it reads. That state is global, so two
             ;; workers minting ids at the same instant race on it — and the
             ;; symptom is not a crash but duplicate or correlated ids, which
             ;; look like valid ids and are wrong. Every id minted on this path
             ;; ends up in a response.
             ;;
             ;; `:bindings` evaluates its form once per worker, so each thread
             ;; gets a distinct state seeded from the OS. This is the one piece
             ;; of shared mutable state the machine fan-out actually touches:
             ;; `process-machine-input` is otherwise local to its machine, and
             ;; `transition-sequence` reads no specials.
             :bindings (list (cons '*random-state* '(make-random-state t)))))))
  lparallel:*kernel*)

(defun shutdown-kernel ()
  (when lparallel:*kernel*
    (lparallel:end-kernel :wait t)
    (setf lparallel:*kernel* nil *kernel-worker-count* nil)))

(defstruct envelope
  message
  ;; A one-shot queue rather than a lock/condvar/value/flag quartet. The reply
  ;; is a single value handed across once, which is exactly what a queue of one
  ;; carries, and `try-pop-queue` gives the timed read the old code hand-rolled.
  reply-queue)

(defstruct (actor (:constructor %make-actor))
  name
  mailbox
  thread
  running-p
  state
  handler
  supervisor)

(defun make-actor (name handler &key state supervisor)
  (ensure-kernel)
  (let ((actor (%make-actor :name name
                            :mailbox (lparallel.queue:make-queue)
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
        for envelope = (lparallel.queue:pop-queue (actor-mailbox actor))
        when envelope
          do (handler-case
                 (let ((result (funcall (actor-handler actor)
                                        (actor-state actor)
                                        (envelope-message envelope))))
                   (reply-envelope envelope (list :ok result)))
               (error (condition)
                 (when (actor-supervisor actor)
                   (funcall (actor-supervisor actor) actor condition))
                 (reply-envelope envelope (list :error condition))))))

(defun reply-envelope (envelope outcome)
  "Hand the outcome back. Tagged (:ok value) / (:error condition) so a handler
   that legitimately returns NIL is not read as a failure — the old code
   distinguished those with a separate `replied-p` flag."
  (when (envelope-reply-queue envelope)
    (lparallel.queue:push-queue outcome (envelope-reply-queue envelope))))

(defun actor-tell (actor message)
  (lparallel.queue:push-queue (make-envelope :message message)
                              (actor-mailbox actor))
  t)

(defun actor-ask (actor message &key (timeout 30))
  "Send MESSAGE and block for the reply, or signal after TIMEOUT seconds.

   The deadline loop this replaces computed remaining time against
   `internal-time-units-per-second` on every wakeup; `try-pop-queue` takes the
   timeout directly."
  (let* ((reply (lparallel.queue:make-queue))
         (envelope (make-envelope :message message :reply-queue reply)))
    (lparallel.queue:push-queue envelope (actor-mailbox actor))
    (multiple-value-bind (outcome present-p)
        (lparallel.queue:try-pop-queue reply :timeout timeout)
      (unless present-p
        (error "Actor ask timed out for ~a" (actor-name actor)))
      (ecase (first outcome)
        (:ok    (second outcome))
        (:error (error (second outcome)))))))

(defun stop-actor (actor)
  (setf (actor-running-p actor) nil)
  ;; Wake the loop so it observes running-p. pop-queue blocks indefinitely, so
  ;; without this the thread would sit on an empty mailbox forever — the old
  ;; code notified its condition variable for the same reason.
  (lparallel.queue:push-queue nil (actor-mailbox actor))
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

;;;; ── The fan-out this exists for ──────────────────────────────────────────
;;;;
;;;; `maphash` over the machine table is a serial walk with an unspecified order
;;;; and no unit a worker can be handed (#92). Two separate problems, and the
;;;; fix has to address both: a snapshot gives the unit, sorting gives the order.

(defun machine-snapshot (machines-table)
  "Machines as an ordered vector, taken as one consistent sample.

   Ordered by id so the result does not depend on hash-table iteration order.
   `maphash`'s order is unspecified, and the same corpus walked by two runtimes
   in two orders is how RealityEngine_CI#197 reported fifteen identical regions
   as a three-way divergence."
  (let ((out '()))
    (maphash (lambda (id machine) (push (cons id machine) out)) machines-table)
    (map 'vector #'cdr (sort out #'string< :key #'car))))

(defun machine-slice (machine universal)
  "MACHINE's input region of UNIVERSAL — the mirror of merging its output back.

   A machine's input is the slice at its declared perceptualMapping.input,
   exactly as its output is written to a slice at another. A machine mapped
   outside the presented space yields NIL rather than signalling: the universe is
   larger than any one deployment's space, and refusing would make a partial
   space unusable rather than partial."
  (let* ((mapping (machine-mapping machine))
         (region  (and mapping (mapping-input mapping))))
    (if (null region)
        nil
        (let ((offset (region-offset region))
              (len    (region-length region)))
          (if (or (< offset 0) (< len 0) (> (+ offset len) (length universal)))
              nil
              (subseq universal offset (+ offset len)))))))

(defmacro with-machine-snapshot ((var machines-table) &body body)
  "Collect the machines once, then run BODY over the snapshot.

   Collection is the atomic half: the set is sampled as one consistent view, so
   a machine added or removed partway cannot appear in some results and not
   others (RealityEngine_CI#254, property 1)."
  `(let ((,var (machine-snapshot ,machines-table)))
     ,@body))

(defun pmap-machines (function snapshot)
  "Apply FUNCTION to every machine in SNAPSHOT, in parallel, order preserved.

   `pmapcar` is the fan-out: each element is a unit a worker can take, and the
   result vector is in snapshot order regardless of completion order, so the
   output is canonical without a sort afterwards."
  (ensure-kernel)
  (lparallel:pmap 'list function snapshot))
