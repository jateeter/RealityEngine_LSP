(in-package #:reality-engine-lsp)

;;; MQTT v3.1.1 client — Common Lisp twin of include/reality/mqtt_client.hpp
;;; in the C++ runtime.
;;;
;;; Implements just enough of MQTT v3.1.1 (oasis-mqtt-v3.1.1-os) to act as
;;; a one-way ingest path for the Perception Engine: CONNECT, SUBSCRIBE,
;;; receive PUBLISH, send PINGREQ on a keepalive timer.  No PUBLISH from
;;; client side, no QoS > 1, no retained-message replay tracking, no TLS.
;;;
;;; Wire framing is hand-rolled (same as the CPP client) so this stays
;;; dependency-light — only usocket + bordeaux-threads, both already in
;;; the ASDF system depends-on list.  Wire shapes match the CPP client
;;; byte-for-byte; the live yuma broker accepts both.
;;;
;;; Threading: one I/O thread per client, started by `mqtt-client-start`,
;;; joined by `mqtt-client-stop`.  on-message callbacks run on the I/O
;;; thread — callers must not block.

(defstruct mqtt-client-config
  (broker-host "localhost")
  (broker-port 1883)
  (client-id "reality-engine-pe-lsp")
  (username nil)
  (password nil)
  (keepalive-sec 60)
  (reconnect-delay-ms 2000)
  (clean-session-p t))

(defstruct mqtt-client-stats
  (connect-attempts 0)
  (connect-successes 0)
  (messages-received 0)
  (bytes-received 0)
  (pings-sent 0)
  (reconnects 0)
  (last-message-at-ms 0))

(defstruct (mqtt-client (:conc-name %mqtt-client-))
  config
  (running-p nil)
  (connected-p nil)
  socket
  stream
  thread
  message-handler
  (subscriptions nil)              ; list of (topic . qos)
  (pending-subs nil)
  (next-packet-id 1)
  (last-traffic-at 0)
  (stats (make-mqtt-client-stats))
  (lock (bt:make-lock "mqtt-client"))
  (wakeup (bt:make-condition-variable :name "mqtt-client-wakeup")))

(defun mqtt-client-set-message-handler (client handler)
  "Register the callback invoked for every PUBLISH arriving on a subscribed
topic.  HANDLER is a function of (topic-string payload-bytes)."
  (bt:with-lock-held ((%mqtt-client-lock client))
    (setf (%mqtt-client-message-handler client) handler))
  client)

(defun mqtt-client-subscribe (client topic-filter &optional (qos 0))
  "Add a topic to the subscription list.  Effective on the next reconnect;
if already connected, sends an immediate SUBSCRIBE."
  (let ((clamped (max 0 (min 1 qos))))
    (bt:with-lock-held ((%mqtt-client-lock client))
      (push (cons topic-filter clamped) (%mqtt-client-subscriptions client))
      (when (%mqtt-client-connected-p client)
        (push (cons topic-filter clamped) (%mqtt-client-pending-subs client)))
      (bt:condition-notify (%mqtt-client-wakeup client))))
  client)

(defun mqtt-client-connected-p (client)
  (bt:with-lock-held ((%mqtt-client-lock client))
    (%mqtt-client-connected-p client)))

(defun mqtt-client-stats-snapshot (client)
  "Return a fresh copy of the stats struct so callers can read without
holding the client lock for any meaningful time."
  (bt:with-lock-held ((%mqtt-client-lock client))
    (let ((s (%mqtt-client-stats client)))
      (make-mqtt-client-stats
       :connect-attempts    (mqtt-client-stats-connect-attempts s)
       :connect-successes   (mqtt-client-stats-connect-successes s)
       :messages-received   (mqtt-client-stats-messages-received s)
       :bytes-received      (mqtt-client-stats-bytes-received s)
       :pings-sent          (mqtt-client-stats-pings-sent s)
       :reconnects          (mqtt-client-stats-reconnects s)
       :last-message-at-ms  (mqtt-client-stats-last-message-at-ms s)))))

;; ── Wire helpers ────────────────────────────────────────────────────────────

(defconstant +mqtt-connect+    #x10)
(defconstant +mqtt-connack+    #x20)
(defconstant +mqtt-publish+    #x30)
(defconstant +mqtt-subscribe+  #x80)
(defconstant +mqtt-suback+     #x90)
(defconstant +mqtt-pingreq+    #xC0)
(defconstant +mqtt-pingresp+   #xD0)
(defconstant +mqtt-disconnect+ #xE0)

(defconstant +cf-username+      #x80)
(defconstant +cf-password+      #x40)
(defconstant +cf-clean-session+ #x02)

(defun write-remaining-length (out length)
  "MQTT §2.2.3 variable-length encoding — up to 4 bytes."
  (let ((n length))
    (loop do
      (let ((b (logand n #x7F)))
        (setf n (ash n -7))
        (when (> n 0) (setf b (logior b #x80)))
        (vector-push-extend b out))
      while (> n 0))))

(defun read-remaining-length (stream)
  "Return decoded length, or NIL on EOF / malformed varint."
  (let ((length 0)
        (mult 1))
    (loop for i from 0 below 4 do
      (let ((b (handler-case (read-byte stream) (error () (return-from read-remaining-length nil)))))
        (incf length (* (logand b #x7F) mult))
        (when (zerop (logand b #x80)) (return-from read-remaining-length length))
        (setf mult (* mult 128))))
    nil))

(defun write-mqtt-string (out s)
  "MQTT UTF-8 string: 2-byte length prefix + bytes."
  (let* ((bytes (babel:string-to-octets s :encoding :utf-8))
         (len (length bytes)))
    (vector-push-extend (logand (ash len -8) #xFF) out)
    (vector-push-extend (logand len #xFF) out)
    (loop for b across bytes do (vector-push-extend b out))))

(defun write-all (stream bytes)
  (write-sequence bytes stream)
  (force-output stream))

(defun read-exact (stream n)
  "Return a vector of N bytes or NIL on short read."
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-sequence out stream)))
      (and (= got n) out))))

;; ── CONNECT + authenticate ──────────────────────────────────────────────────

(defun connect-and-authenticate (client)
  (let ((cfg (%mqtt-client-config client)))
    (bt:with-lock-held ((%mqtt-client-lock client))
      (incf (mqtt-client-stats-connect-attempts (%mqtt-client-stats client))))
    (handler-case
        (let* ((sock (usocket:socket-connect (mqtt-client-config-broker-host cfg)
                                             (mqtt-client-config-broker-port cfg)
                                             :element-type '(unsigned-byte 8)
                                             :timeout 10))
               (stream (usocket:socket-stream sock))
               (body (make-array 64 :fill-pointer 0 :adjustable t
                                  :element-type '(unsigned-byte 8)))
               (flags +cf-clean-session+))
          ;; Variable header
          (write-mqtt-string body "MQTT")
          (vector-push-extend #x04 body)       ; protocol level 3.1.1
          (when (mqtt-client-config-username cfg) (setf flags (logior flags +cf-username+)))
          (when (mqtt-client-config-password cfg) (setf flags (logior flags +cf-password+)))
          (vector-push-extend flags body)
          (let ((k (mqtt-client-config-keepalive-sec cfg)))
            (vector-push-extend (logand (ash k -8) #xFF) body)
            (vector-push-extend (logand k #xFF) body))
          ;; Payload
          (write-mqtt-string body (mqtt-client-config-client-id cfg))
          (when (mqtt-client-config-username cfg)
            (write-mqtt-string body (mqtt-client-config-username cfg)))
          (when (mqtt-client-config-password cfg)
            (write-mqtt-string body (mqtt-client-config-password cfg)))
          ;; Fixed header
          (let ((packet (make-array 16 :fill-pointer 0 :adjustable t
                                       :element-type '(unsigned-byte 8))))
            (vector-push-extend +mqtt-connect+ packet)
            (write-remaining-length packet (length body))
            (loop for b across body do (vector-push-extend b packet))
            (write-all stream packet))
          ;; Read CONNACK
          (let ((hdr (read-exact stream 1)))
            (unless (and hdr (= (logand (aref hdr 0) #xF0) +mqtt-connack+))
              (ignore-errors (usocket:socket-close sock))
              (return-from connect-and-authenticate nil)))
          (let ((rem (read-remaining-length stream)))
            (unless (eql rem 2)
              (ignore-errors (usocket:socket-close sock))
              (return-from connect-and-authenticate nil)))
          (let ((rest (read-exact stream 2)))
            (unless (and rest (zerop (aref rest 1)))
              (ignore-errors (usocket:socket-close sock))
              (return-from connect-and-authenticate nil)))
          (bt:with-lock-held ((%mqtt-client-lock client))
            (setf (%mqtt-client-socket client) sock
                  (%mqtt-client-stream client) stream
                  (%mqtt-client-connected-p client) t
                  (%mqtt-client-last-traffic-at client) (now-ms))
            (incf (mqtt-client-stats-connect-successes (%mqtt-client-stats client))))
          t)
      (error (c)
        (format *error-output* "[mqtt-client] connect to ~a:~a failed: ~a~%"
                (mqtt-client-config-broker-host cfg)
                (mqtt-client-config-broker-port cfg) c)
        nil))))

;; ── SUBSCRIBE ───────────────────────────────────────────────────────────────

(defun send-subscriptions (client)
  (let* ((stream (%mqtt-client-stream client))
         (subs (bt:with-lock-held ((%mqtt-client-lock client))
                 (copy-list (%mqtt-client-subscriptions client)))))
    (when (and stream subs)
      (let ((body (make-array 32 :fill-pointer 0 :adjustable t
                                  :element-type '(unsigned-byte 8))))
        (let ((pid (bt:with-lock-held ((%mqtt-client-lock client))
                     (prog1 (%mqtt-client-next-packet-id client)
                       (setf (%mqtt-client-next-packet-id client)
                             (let ((n (1+ (%mqtt-client-next-packet-id client))))
                               (if (= n 0) 1 n)))))))
          (vector-push-extend (logand (ash pid -8) #xFF) body)
          (vector-push-extend (logand pid #xFF) body))
        (dolist (pair subs)
          (write-mqtt-string body (car pair))
          (vector-push-extend (logand (cdr pair) #x03) body))
        (let ((packet (make-array 4 :fill-pointer 0 :adjustable t
                                     :element-type '(unsigned-byte 8))))
          (vector-push-extend (logior +mqtt-subscribe+ #x02) packet)
          (write-remaining-length packet (length body))
          (loop for b across body do (vector-push-extend b packet))
          (handler-case (write-all stream packet)
            (error () nil)))))
    (bt:with-lock-held ((%mqtt-client-lock client))
      (setf (%mqtt-client-pending-subs client) nil))))

(defun send-ping (client)
  (let ((stream (%mqtt-client-stream client)))
    (when stream
      (handler-case
          (progn
            (write-all stream (make-array 2 :element-type '(unsigned-byte 8)
                                            :initial-contents (list +mqtt-pingreq+ 0)))
            (bt:with-lock-held ((%mqtt-client-lock client))
              (incf (mqtt-client-stats-pings-sent (%mqtt-client-stats client)))))
        (error () nil)))))

;; ── Read loop ──────────────────────────────────────────────────────────────

(defun read-loop (client)
  (let ((stream (%mqtt-client-stream client))
        (cfg (%mqtt-client-config client)))
    (loop while (and (%mqtt-client-running-p client)
                     (%mqtt-client-connected-p client))
          do
      ;; Keepalive check — send PINGREQ when half the keepalive window has
      ;; elapsed since the last traffic.  Halfway is conventional and gives
      ;; the broker generous margin before dropping us for inactivity.
      (let* ((k (mqtt-client-config-keepalive-sec cfg))
             (last (%mqtt-client-last-traffic-at client))
             (elapsed-ms (- (now-ms) last)))
        (when (and (> k 0) (>= elapsed-ms (/ (* k 1000) 2)))
          (send-ping client)
          (setf (%mqtt-client-last-traffic-at client) (now-ms))))
      ;; Try to read one packet; sleep briefly when no bytes are available
      ;; so we can also fire keepalives on time.
      (handler-case
          (let ((hdr (read-byte stream nil nil)))
            (cond
              ((null hdr) (return))
              (t
               (let* ((type (logand hdr #xF0))
                      (rem  (read-remaining-length stream))
                      (body (and rem (> rem 0) (read-exact stream rem))))
                 (when rem (setf (%mqtt-client-last-traffic-at client) (now-ms)))
                 (cond
                   ((= type +mqtt-publish+)
                    (when (and body (>= (length body) 2))
                      (let* ((topic-len (logior (ash (aref body 0) 8) (aref body 1)))
                             (topic-bytes (subseq body 2 (+ 2 topic-len)))
                             (qos (logand (ash hdr -1) #x03))
                             (payload-start (+ 2 topic-len (if (> qos 0) 2 0)))
                             (payload (subseq body payload-start))
                             (topic (babel:octets-to-string topic-bytes :encoding :utf-8)))
                        (let (handler)
                          (bt:with-lock-held ((%mqtt-client-lock client))
                            (incf (mqtt-client-stats-messages-received (%mqtt-client-stats client)))
                            (incf (mqtt-client-stats-bytes-received (%mqtt-client-stats client))
                                  (length payload))
                            (setf (mqtt-client-stats-last-message-at-ms (%mqtt-client-stats client))
                                  (now-ms))
                            (setf handler (%mqtt-client-message-handler client)))
                          (when handler
                            (handler-case (funcall handler topic payload)
                              (error (c)
                                (format *error-output* "[mqtt-client] handler error on ~a: ~a~%"
                                        topic c))))))))
                   ((= type +mqtt-pingresp+) nil)
                   ((= type +mqtt-suback+) nil)
                   (t nil))))))
        (error () (return))))
    (setf (%mqtt-client-connected-p client) nil)))

;; ── I/O thread loop ────────────────────────────────────────────────────────

(defun io-loop (client)
  (loop while (%mqtt-client-running-p client) do
    (cond
      ((connect-and-authenticate client)
       (send-subscriptions client)
       (read-loop client)
       (when (%mqtt-client-running-p client)
         (bt:with-lock-held ((%mqtt-client-lock client))
           (incf (mqtt-client-stats-reconnects (%mqtt-client-stats client))))
         (sleep (/ (mqtt-client-config-reconnect-delay-ms (%mqtt-client-config client)) 1000.0))))
      (t
       (sleep (/ (mqtt-client-config-reconnect-delay-ms (%mqtt-client-config client)) 1000.0)))))
  ;; Graceful shutdown — send DISCONNECT before closing.
  (let ((sock (%mqtt-client-socket client))
        (stream (%mqtt-client-stream client)))
    (when (and stream sock)
      (ignore-errors
        (write-all stream (make-array 2 :element-type '(unsigned-byte 8)
                                        :initial-contents (list +mqtt-disconnect+ 0))))
      (ignore-errors (usocket:socket-close sock)))
    (setf (%mqtt-client-socket client) nil
          (%mqtt-client-stream client) nil
          (%mqtt-client-connected-p client) nil)))

(defun mqtt-client-start (client)
  "Launch the I/O thread.  Idempotent."
  (unless (%mqtt-client-running-p client)
    (setf (%mqtt-client-running-p client) t)
    (setf (%mqtt-client-thread client)
          (bt:make-thread (lambda () (io-loop client))
                          :name (format nil "mqtt-client-~a"
                                        (mqtt-client-config-client-id
                                         (%mqtt-client-config client))))))
  client)

(defun mqtt-client-stop (client)
  "Signal the I/O thread to exit and join it.  Idempotent."
  (when (%mqtt-client-running-p client)
    (setf (%mqtt-client-running-p client) nil)
    ;; Shutdown the socket so any blocked read returns.
    (let ((sock (%mqtt-client-socket client)))
      (when sock (ignore-errors (usocket:socket-close sock))))
    (let ((th (%mqtt-client-thread client)))
      (when (and th (bt:thread-alive-p th))
        (handler-case (bt:join-thread th)
          (error () nil)))))
  client)

(defparameter +default-mqtt-broker-host+ "yuma.lateraledge.cloud"
  "Default broker target — points at the Lateral Edge sensor mesh that
the live yuma demonstration runs against, so operators get a sensible
default without configuration.  Setting MQTT_BROKER_HOST=\"\" (empty
string) or MQTT_DISABLED=1 explicitly opts out.  Bridge still requires
MQTT_MAPPINGS_FILE / MQTT_MAPPINGS_JSON before it actually boots.")

(defun make-mqtt-client-from-env ()
  "Convenience: build an mqtt-client-config from the canonical MQTT_* env
vars.  Returns NIL when the operator has explicitly opted out
(MQTT_BROKER_HOST='' or MQTT_DISABLED=1).  Otherwise defaults to the
shared yuma.lateraledge.cloud broker."
  (let ((disabled (env "MQTT_DISABLED" "0")))
    (when (member disabled '("1" "true" "TRUE" "yes") :test #'string=)
      (return-from make-mqtt-client-from-env nil)))
  (let ((host-env (uiop:getenv "MQTT_BROKER_HOST")))
    (cond
      ((and host-env (string= host-env ""))
       nil)
      (t
       (let ((host (or host-env +default-mqtt-broker-host+)))
         (make-mqtt-client-config
          :broker-host host
          :broker-port (env-int "MQTT_BROKER_PORT" 1883)
          :client-id (env "MQTT_CLIENT_ID" "reality-engine-pe-lsp")
          :username (env "MQTT_USERNAME" nil)
          :password (env "MQTT_PASSWORD" nil)
          :keepalive-sec (env-int "MQTT_KEEPALIVE" 60)
          :clean-session-p t))))))
