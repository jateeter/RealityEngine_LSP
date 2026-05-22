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
  "Push an event to all connected SSE and WebSocket clients.
Wire-compatible with _AI broadcast() — identical event type field names."
  (bt:with-lock-held (*sse-lock*)
    (dolist (client *sse-clients*)
      (handler-case
          (bt:with-lock-held ((sse-client-lock client))
            (setf (sse-client-queue client)
                  (nconc (sse-client-queue client) (list event)))
            (bt:condition-notify (sse-client-cvar client)))
        (error () nil))))
  (ws-broadcast event))

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

;;; RE SSE broadcast surface — separate registry from PE /api/events.
;;; RE endpoint: GET /api/engine/stream
;;; Event type: step-result (emitted after every POST /api/perceive)

(defvar *re-sse-clients* nil)
(defvar *re-sse-lock* (bt:make-lock "re-sse-registry"))

(defun re-register-sse-client (client)
  (bt:with-lock-held (*re-sse-lock*)
    (push client *re-sse-clients*)))

(defun re-unregister-sse-client (client)
  (bt:with-lock-held (*re-sse-lock*)
    (setf *re-sse-clients* (remove client *re-sse-clients* :test #'eq))))

(defun re-broadcast (event)
  "Push a step-result event to all connected RE /api/engine/stream SSE clients."
  (bt:with-lock-held (*re-sse-lock*)
    (dolist (client *re-sse-clients*)
      (handler-case
          (bt:with-lock-held ((sse-client-lock client))
            (setf (sse-client-queue client)
                  (nconc (sse-client-queue client) (list event)))
            (bt:condition-notify (sse-client-cvar client)))
        (error () nil)))))

(defun re-sse-stream-handler ()
  "Hunchentoot handler for GET /api/engine/stream — SSE stream of step results."
  (unless (string= (symbol-name (hunchentoot:request-method*)) "GET")
    (setf (hunchentoot:return-code*) 405)
    (return-from re-sse-stream-handler "Method Not Allowed"))
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (setf (hunchentoot:header-out "X-Accel-Buffering") "no")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (let* ((stream (hunchentoot:send-headers))
         (client (make-sse-client)))
    (re-register-sse-client client)
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
    (re-unregister-sse-client client))
  "")

;;;--------------------------------------------------------------------------
;;; WebSocket broadcast surface — GET /ws
;;; Same JSON event payloads as SSE /api/events; RFC 6455 handshake +
;;; unmasked server-to-client text frames.  Broadcast fans out from
;;; the existing broadcast() entry point so all callers are covered.
;;;--------------------------------------------------------------------------

;;; — Pure-Lisp SHA-1 (RFC 3174) — required for the RFC 6455 handshake key.

(defun ws-rot32 (x n)
  "Left-rotate 32-bit integer X by N bits."
  (logand #xFFFFFFFF (logior (ash x n) (ash x (- n 32)))))

(defun ws-sha1 (bytes)
  "Return a 20-byte (unsigned-byte 8) array containing the SHA-1 digest of BYTES."
  (let* ((n (length bytes))
         (padded-len (* 64 (ceiling (+ n 9) 64)))
         (msg (make-array padded-len :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace msg bytes)
    (setf (aref msg n) #x80)
    (let ((bits (* n 8)))
      (loop for i from 1 to 8
            do (setf (aref msg (- padded-len i)) (logand bits #xff))
               (setf bits (ash bits -8))))
    (let ((h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE)
          (h3 #x10325476) (h4 #xC3D2E1F0))
      (loop for base from 0 below padded-len by 64
            do (let ((w (make-array 80 :initial-element 0)))
                 (loop for i from 0 to 15
                       do (let ((off (+ base (* i 4))))
                            (setf (aref w i)
                                  (logior (ash (aref msg off)           24)
                                          (ash (aref msg (+ off 1))     16)
                                          (ash (aref msg (+ off 2))      8)
                                               (aref msg (+ off 3))))))
                 (loop for i from 16 to 79
                       do (setf (aref w i)
                                (ws-rot32 (logxor (aref w (- i  3))
                                                  (aref w (- i  8))
                                                  (aref w (- i 14))
                                                  (aref w (- i 16)))
                                          1)))
                 (let ((a h0) (b h1) (c h2) (d h3) (e h4))
                   (loop for i from 0 to 79
                         do (multiple-value-bind (f k)
                                (cond
                                  ((< i 20)
                                   (values (logior (logand b c)
                                                   (logand (logand #xFFFFFFFF (lognot b)) d))
                                           #x5A827999))
                                  ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                                  ((< i 60)
                                   (values (logior (logand b c) (logand b d) (logand c d))
                                           #x8F1BBCDC))
                                  (t        (values (logxor b c d) #xCA62C1D6)))
                              (let ((temp (logand #xFFFFFFFF
                                                  (+ (ws-rot32 a 5) f e k (aref w i)))))
                                (setf e d  d c  c (ws-rot32 b 30)  b a  a temp))))
                   (setf h0 (logand #xFFFFFFFF (+ h0 a))
                         h1 (logand #xFFFFFFFF (+ h1 b))
                         h2 (logand #xFFFFFFFF (+ h2 c))
                         h3 (logand #xFFFFFFFF (+ h3 d))
                         h4 (logand #xFFFFFFFF (+ h4 e))))))
      (let ((out (make-array 20 :element-type '(unsigned-byte 8))))
        (loop for i from 0 to 4
              for h in (list h0 h1 h2 h3 h4)
              do (setf (aref out (+ (* i 4) 0)) (ldb (byte 8 24) h)
                       (aref out (+ (* i 4) 1)) (ldb (byte 8 16) h)
                       (aref out (+ (* i 4) 2)) (ldb (byte 8  8) h)
                       (aref out (+ (* i 4) 3)) (ldb (byte 8  0) h)))
        out))))

;;; — Handshake accept-key computation (RFC 6455 section 4.2.2) ————————————

(defparameter +ws-guid+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun ws-accept-key (client-key)
  "Compute Sec-WebSocket-Accept from the client's Sec-WebSocket-Key."
  (cl-base64:usb8-array-to-base64-string
   (ws-sha1 (babel:string-to-octets
             (concatenate 'string client-key +ws-guid+)
             :encoding :utf-8))))

;;; — Frame encoding (RFC 6455 section 5.2, server→client unmasked) ————————

(defun ws-text-frame (text)
  "Return a WebSocket text frame byte array for TEXT."
  (let* ((payload (babel:string-to-octets text :encoding :utf-8))
         (plen    (length payload))
         (header  (cond ((< plen 126)
                         (list #x81 plen))
                        ((< plen 65536)
                         (list #x81 #x7E (ash plen -8) (logand plen #xff)))
                        (t
                         (list #x81 #x7F
                               0 0 0 0
                               (ldb (byte 8 24) plen) (ldb (byte 8 16) plen)
                               (ldb (byte 8  8) plen) (ldb (byte 8  0) plen)))))
         (hlen    (length header))
         (frame   (make-array (+ hlen plen) :element-type '(unsigned-byte 8))))
    (replace frame (coerce header '(simple-array (unsigned-byte 8) (*))))
    (replace frame payload :start1 hlen)
    frame))

(defun ws-ping-frame ()
  "Return a 2-byte WebSocket ping frame (opcode 9, no payload)."
  (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(#x89 0)))

;;; — Client registry ————————————————————————————————————————————————————

(defstruct ws-client
  stream
  (queue nil)
  (lock (bt:make-lock "ws-c-lock"))
  (cvar (bt:make-condition-variable "ws-c-cvar")))

(defvar *ws-clients* nil)
(defvar *ws-lock* (bt:make-lock "ws-registry"))

(defun ws-register-client (client)
  (bt:with-lock-held (*ws-lock*)
    (push client *ws-clients*)))

(defun ws-unregister-client (client)
  (bt:with-lock-held (*ws-lock*)
    (setf *ws-clients* (remove client *ws-clients* :test #'eq))))

(defun ws-client-dequeue (client timeout-secs)
  "Return the next queued frame for CLIENT, blocking up to TIMEOUT-SECS.
Returns NIL on timeout (caller should send a keepalive ping)."
  (bt:with-lock-held ((ws-client-lock client))
    (when (null (ws-client-queue client))
      (bt:condition-wait (ws-client-cvar client) (ws-client-lock client)
                         :timeout timeout-secs))
    (when (ws-client-queue client)
      (let ((frame (first (ws-client-queue client))))
        (setf (ws-client-queue client) (rest (ws-client-queue client)))
        frame))))

(defun ws-broadcast (event)
  "Enqueue EVENT as a WebSocket text frame for all connected /ws clients."
  (let ((frame (ws-text-frame (json-stringify event))))
    (bt:with-lock-held (*ws-lock*)
      (dolist (client *ws-clients*)
        (handler-case
            (bt:with-lock-held ((ws-client-lock client))
              (setf (ws-client-queue client)
                    (nconc (ws-client-queue client) (list frame)))
              (bt:condition-notify (ws-client-cvar client)))
          (error () nil))))))

;;; — Handler ——————————————————————————————————————————————————————————————

(defun ws-handler ()
  "Hunchentoot handler for GET /ws — WebSocket broadcast of PE events.
Completes the RFC 6455 upgrade handshake, registers the client, then
loops: dequeue frames (15 s timeout) → write frame / send ping on timeout."
  (let ((upgrade (hunchentoot:header-in* :upgrade))
        (key     (hunchentoot:header-in* :sec-websocket-key)))
    (unless (and upgrade (string-equal upgrade "websocket") key)
      (setf (hunchentoot:return-code*) 400)
      (return-from ws-handler "WebSocket upgrade required"))
    (setf (hunchentoot:return-code*) 101)
    (setf (hunchentoot:content-length*) 0)
    (setf (hunchentoot:header-out "Upgrade") "websocket")
    (setf (hunchentoot:header-out "Connection") "Upgrade")
    (setf (hunchentoot:header-out "Sec-WebSocket-Accept") (ws-accept-key key))
    (let* ((stream (hunchentoot:send-headers))
           (client (make-ws-client :stream stream)))
      (ws-register-client client)
      (handler-case
          (loop
            (let ((frame (ws-client-dequeue client 15)))
              (handler-case
                  (progn
                    (write-sequence (or frame (ws-ping-frame)) stream)
                    (force-output stream))
                (error () (return)))))
        (error () nil))
      (ws-unregister-client client))
    ""))
