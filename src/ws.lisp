(in-package #:reality-engine-lsp)

;;; SSE broadcast surface — wire-compatible event types with _AI WebSocket
;;; (_AI: /ws, events: state-update, push-result, agent.completion.received,
;;;  carekit.ingest, mqtt-ingest, dispatch-updated)
;;; _LSP equivalent: GET /api/events (Server-Sent Events, same JSON payloads).

(defstruct sse-client
  (queue nil)
  (lock (bt:make-lock "sse-c-lock"))
  (cvar (bt:make-condition-variable "sse-c-cvar")))

(defvar *sse-clients* nil)
(defvar *sse-lock* (bt:make-lock "sse-registry"))

(defun register-sse-client (client)
  (bt:with-lock-held (*sse-lock*)
    (push client *sse-clients*)))

(defun unregister-sse-client (client)
  (bt:with-lock-held (*sse-lock*)
    (setf *sse-clients* (remove client *sse-clients* :test #'eq))))

(defun broadcast (event)
  "Push an event to all connected SSE clients.
Wire-compatible with _AI broadcast() — identical event type field names."
  (bt:with-lock-held (*sse-lock*)
    (dolist (client *sse-clients*)
      (handler-case
          (bt:with-lock-held ((sse-client-lock client))
            (setf (sse-client-queue client)
                  (nconc (sse-client-queue client) (list event)))
            (bt:condition-notify (sse-client-cvar client)))
        (error () nil)))))

(defun client-dequeue (client timeout-secs)
  "Return the next queued event for client, blocking up to timeout-secs.
Returns nil on timeout (caller should send a keepalive comment)."
  (bt:with-lock-held ((sse-client-lock client))
    (when (null (sse-client-queue client))
      (bt:condition-wait (sse-client-cvar client) (sse-client-lock client)
                         :timeout timeout-secs))
    (when (sse-client-queue client)
      (let ((ev (first (sse-client-queue client))))
        (setf (sse-client-queue client) (rest (sse-client-queue client)))
        ev))))

(defun sse-events-handler ()
  "Hunchentoot handler for GET /api/events — SSE stream to the client.
Each event is sent as 'data: <json>\\n\\n'.  Keepalive comments are sent
every 15 s so proxies do not close idle connections."
  (unless (string= (symbol-name (hunchentoot:request-method*)) "GET")
    (setf (hunchentoot:return-code*) 405)
    (return-from sse-events-handler "Method Not Allowed"))
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (setf (hunchentoot:header-out "X-Accel-Buffering") "no")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (let* ((stream (hunchentoot:send-headers))
         (client (make-sse-client)))
    (register-sse-client client)
    (handler-case
        (progn
          (write-string ": connected\n\n" stream)
          (force-output stream)
          (loop
            (let ((ev (client-dequeue client 15)))
              (handler-case
                  (progn
                    (if ev
                        (format stream "data: ~a~%~%" (json-stringify ev))
                        (write-string ": keepalive\n\n" stream))
                    (force-output stream))
                (error () (return))))))
      (error () nil))
    (unregister-sse-client client))
  "")
