(in-package #:reality-engine-lsp)

;;; MqttBridge — wires the LSP MQTT client + mapping registry to the
;;; Perception Engine's signal-ingest path.  Same contract as the CPP and
;;; AI bridges.
;;;
;;; On every accepted PUBLISH the bridge fans out to every matching rule
;;; (mqtt-match-all-topics), runs extract + normalize + length-validate,
;;; then calls the caller-supplied ingest callback with the same
;;; {sensor-id offset length values ttl-ms topic mapping-id} payload that
;;; the AI / CPP bridges produce.  Per the design rule, nothing downstream
;;; of decode knows MQTT is involved — the same code path the HTTP
;;; POST /api/signals endpoint uses ingests the values.
;;;
;;; Push policy uses a single bridge-wide debounce thread:
;;;   :immediate → calls push-trigger inline
;;;   :debounced → schedules a wakeup N ms after the last accepted message
;;;   :manual    → never auto-pushes (operator hits POST /api/push)

(defstruct (mqtt-bridge (:conc-name %mqtt-bridge-)
                        (:constructor %make-mqtt-bridge))
  client
  registry                          ; mqtt-mapping-registry
  ingest-callback                   ; (sensor-id offset length values ttl-ms topic mapping-id)
  push-trigger                      ; thunk
  (stats-mu (bt:make-lock "mqtt-bridge-stats"))
  ;; Bridge-level counters — read by /api/mqtt/status.  Per-mapping counters
  ;; live on the registry's metrics array, mutated inside on-message.
  (messages-received 0)
  (messages-mapped 0)
  (messages-rejected 0)
  (messages-unmatched 0)
  (messages-retained-dropped 0)
  (pushes-triggered 0)
  ;; Debounce coordination
  (debounce-running-p nil)
  (debounce-thread nil)
  (debounce-mu (bt:make-lock "mqtt-bridge-debounce"))
  (debounce-cv (bt:make-condition-variable :name "mqtt-bridge-debounce"))
  (next-push-at-ms 0))

(defun make-mqtt-bridge (config registry ingest-callback push-trigger)
  "Build a bridge.  REGISTRY is an mqtt-mapping-registry; INGEST-CALLBACK
is a function of seven args matching the AI / CPP shape; PUSH-TRIGGER is
a thunk invoked when push policy demands a push.

The bridge wires up its message handler eagerly but does NOT connect to
the broker — call mqtt-bridge-start for that."
  (let* ((client (make-mqtt-client :config config))
         (bridge (%make-mqtt-bridge :client client
                                    :registry registry
                                    :ingest-callback ingest-callback
                                    :push-trigger push-trigger)))
    (mqtt-client-set-message-handler client (lambda (topic payload)
                                              (mqtt-bridge-on-message bridge topic payload)))
    ;; Pre-subscribe to every unique topicFilter so the SUBSCRIBE goes out
    ;; on the first connect.  QoS = max declared across rules that share a
    ;; filter (later rule wins ties).
    (let ((seen (make-hash-table :test #'equal)))
      (loop for rule across (mqtt-mapping-registry-rules registry) do
        (let* ((filter (mqtt-mapping-rule-topic-filter rule))
               (existing (gethash filter seen))
               (qos (mqtt-mapping-rule-qos rule)))
          (when (or (null existing) (> qos existing))
            (setf (gethash filter seen) qos))))
      (maphash (lambda (filter qos)
                 (mqtt-client-subscribe client filter qos))
               seen))
    bridge))

(defun mqtt-bridge-start (bridge)
  (mqtt-client-start (%mqtt-bridge-client bridge))
  (unless (%mqtt-bridge-debounce-running-p bridge)
    (setf (%mqtt-bridge-debounce-running-p bridge) t)
    (setf (%mqtt-bridge-debounce-thread bridge)
          (bt:make-thread (lambda () (mqtt-bridge-debounce-loop bridge))
                          :name "mqtt-bridge-debounce")))
  bridge)

(defun mqtt-bridge-stop (bridge)
  (when (%mqtt-bridge-debounce-running-p bridge)
    (setf (%mqtt-bridge-debounce-running-p bridge) nil)
    (bt:with-lock-held ((%mqtt-bridge-debounce-mu bridge))
      (bt:condition-notify (%mqtt-bridge-debounce-cv bridge)))
    (let ((th (%mqtt-bridge-debounce-thread bridge)))
      (when (and th (bt:thread-alive-p th))
        (handler-case (bt:join-thread th) (error () nil)))))
  (mqtt-client-stop (%mqtt-bridge-client bridge))
  bridge)

(defun mqtt-bridge-connected-p (bridge)
  (and bridge (mqtt-client-connected-p (%mqtt-bridge-client bridge))))

(defun mqtt-bridge-stats-snapshot (bridge)
  "Return an alist of bridge-level counters for /api/mqtt/status."
  (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
    `(("messagesReceived"       . ,(%mqtt-bridge-messages-received bridge))
      ("messagesMapped"         . ,(%mqtt-bridge-messages-mapped bridge))
      ("messagesRejected"       . ,(%mqtt-bridge-messages-rejected bridge))
      ("messagesUnmatched"      . ,(%mqtt-bridge-messages-unmatched bridge))
      ("messagesRetainedDropped" . ,(%mqtt-bridge-messages-retained-dropped bridge))
      ("pushesTriggered"        . ,(%mqtt-bridge-pushes-triggered bridge)))))

(defun mqtt-bridge-registry (bridge)
  (%mqtt-bridge-registry bridge))

;; ── Push policy ────────────────────────────────────────────────────────────

(defun mqtt-bridge-schedule-push (bridge debounce-ms)
  "Mark a pending push to fire DEBOUNCE-MS in the future.  Multiple calls
within the window coalesce to a single push fired DEBOUNCE-MS after the
LAST update."
  (let ((target (+ (now-ms) debounce-ms)))
    (bt:with-lock-held ((%mqtt-bridge-debounce-mu bridge))
      (when (> target (%mqtt-bridge-next-push-at-ms bridge))
        (setf (%mqtt-bridge-next-push-at-ms bridge) target)
        (bt:condition-notify (%mqtt-bridge-debounce-cv bridge))))))

(defun mqtt-bridge-debounce-loop (bridge)
  (loop while (%mqtt-bridge-debounce-running-p bridge) do
    (let ((target (%mqtt-bridge-next-push-at-ms bridge)))
      (cond
        ((zerop target)
         ;; Nothing pending — wait up to 200ms for a wakeup notification.
         (bt:with-lock-held ((%mqtt-bridge-debounce-mu bridge))
           (handler-case
               (bt:condition-wait (%mqtt-bridge-debounce-cv bridge)
                                  (%mqtt-bridge-debounce-mu bridge)
                                  :timeout 0.2)
             (error () nil))))
        (t
         (let ((sleep-for-ms (- target (now-ms))))
           (cond
             ((> sleep-for-ms 0)
              (bt:with-lock-held ((%mqtt-bridge-debounce-mu bridge))
                (handler-case
                    (bt:condition-wait (%mqtt-bridge-debounce-cv bridge)
                                       (%mqtt-bridge-debounce-mu bridge)
                                       :timeout (/ sleep-for-ms 1000.0))
                  (error () nil))))
             (t
              ;; Deadline reached — clear pending push and fire.
              (let (fire)
                (bt:with-lock-held ((%mqtt-bridge-debounce-mu bridge))
                  (when (<= (%mqtt-bridge-next-push-at-ms bridge) (now-ms))
                    (setf (%mqtt-bridge-next-push-at-ms bridge) 0)
                    (setf fire t)))
                (when fire
                  (let ((trigger (%mqtt-bridge-push-trigger bridge)))
                    (when trigger
                      (handler-case (funcall trigger)
                        (error (c)
                          (format *error-output* "[mqtt-bridge] push trigger error: ~a~%" c)))
                      (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
                        (incf (%mqtt-bridge-pushes-triggered bridge)))))))))))))))

;; ── Per-message dispatch ───────────────────────────────────────────────────

(defun bump-rejected (rule-metrics topic err)
  (when rule-metrics
    (incf (mqtt-mapping-metrics-rejected rule-metrics))
    (setf (mqtt-mapping-metrics-last-error rule-metrics)
          (format nil "[~a] ~a" topic err))
    (setf (mqtt-mapping-metrics-last-error-at rule-metrics) (now-ms))))

(defun mqtt-bridge-on-message (bridge topic payload)
  (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
    (incf (%mqtt-bridge-messages-received bridge)))
  (let* ((registry (%mqtt-bridge-registry bridge))
         (matches (mqtt-match-all-topics registry topic)))
    (cond
      ((null matches)
       (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
         (incf (%mqtt-bridge-messages-unmatched bridge))))
      (t
       (let ((any-immediate nil)
             (min-debounce nil))
         (dolist (match matches)
           (let* ((idx (car match))
                  (captures (cdr match))
                  (rule (aref (mqtt-mapping-registry-rules registry) idx))
                  (metrics (aref (mqtt-mapping-registry-metrics registry) idx)))
             (incf (mqtt-mapping-metrics-received metrics))
             (setf (mqtt-mapping-metrics-last-message-at metrics) (now-ms))
             (multiple-value-bind (values err) (mqtt-decode rule payload)
               (cond
                 (err
                  (bump-rejected metrics topic err)
                  (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
                    (incf (%mqtt-bridge-messages-rejected bridge))))
                 (t
                  (let ((sensor-id (mqtt-resolve-sensor-id rule topic captures)))
                    (let ((cb (%mqtt-bridge-ingest-callback bridge)))
                      (when cb
                        (handler-case (funcall cb
                                                sensor-id
                                                (mqtt-mapping-rule-offset rule)
                                                (mqtt-mapping-rule-length rule)
                                                values
                                                (mqtt-mapping-rule-ttl-ms rule)
                                                topic
                                                (mqtt-mapping-rule-id rule))
                          (error (c)
                            (format *error-output* "[mqtt-bridge] ingest error on ~a: ~a~%"
                                    sensor-id c)))))
                    (incf (mqtt-mapping-metrics-mapped metrics))
                    (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
                      (incf (%mqtt-bridge-messages-mapped bridge)))
                    (let ((mode (mqtt-mapping-rule-push-mode rule))
                          (db (mqtt-mapping-rule-debounce-ms rule)))
                      (cond
                        ((string= mode "immediate") (setf any-immediate t))
                        ((string= mode "debounced")
                         (when (or (null min-debounce) (< db min-debounce))
                           (setf min-debounce db)))
                        (t nil)))))))))
         ;; Push policy across the fan-out: a single immediate beats any
         ;; debounced; otherwise schedule one debounced push using the
         ;; smallest declared window across the fan-out.
         (cond
           (any-immediate
            (let ((trigger (%mqtt-bridge-push-trigger bridge)))
              (when trigger
                (handler-case (funcall trigger)
                  (error (c)
                    (format *error-output* "[mqtt-bridge] immediate push error: ~a~%" c))))
              (bt:with-lock-held ((%mqtt-bridge-stats-mu bridge))
                (incf (%mqtt-bridge-pushes-triggered bridge)))))
           (min-debounce
            (mqtt-bridge-schedule-push bridge min-debounce))))))))

(defun mqtt-bridge-inject-message (bridge topic payload)
  "Test hatch — drive the full extract → normalize → ingest pipeline with
a synthetic payload.  Mirrors MqttBridge::inject_message in CPP."
  (mqtt-bridge-on-message bridge topic payload))

;; ── Env-driven config loader ──────────────────────────────────────────────

(defun mqtt-bridge-from-environment ()
  "Build a (cons mqtt-client-config mqtt-mapping-registry) pair from the
canonical MQTT_* env vars.  Returns NIL when MQTT_BROKER_HOST is unset
or no mappings resolve.

Honours MQTT_MAPPINGS_FILE (path) or MQTT_MAPPINGS_JSON (inline) — same
contract as AI / CPP."
  (let ((cfg (make-mqtt-client-from-env)))
    (unless cfg (return-from mqtt-bridge-from-environment nil))
    (let* ((file (env "MQTT_MAPPINGS_FILE" nil))
           (inline-json (env "MQTT_MAPPINGS_JSON" nil))
           (registry nil))
      (cond
        ((and file (not (string= file "")))
         (handler-case
             (let ((raw (with-open-file (in file :direction :input
                                              :if-does-not-exist nil)
                          (when in
                            (with-output-to-string (out)
                              (loop for ch = (read-char in nil nil)
                                    while ch do (write-char ch out)))))))
               (when raw
                 (setf registry (mqtt-mapping-registry-from-json (parse-json raw)))))
           (error (c)
             (format *error-output* "[mqtt-bridge] MQTT_MAPPINGS_FILE parse error: ~a~%" c)
             (setf registry nil))))
        ((and inline-json (not (string= inline-json "")))
         (handler-case
             (setf registry (mqtt-mapping-registry-from-json (parse-json inline-json)))
           (error (c)
             (format *error-output* "[mqtt-bridge] MQTT_MAPPINGS_JSON parse error: ~a~%" c)
             (setf registry nil)))))
      (cond
        ((null registry)
         (format *error-output* "[mqtt-bridge] MQTT_BROKER_HOST set but no mappings resolved — bridge disabled~%")
         nil)
        ((zerop (length (mqtt-mapping-registry-rules registry)))
         (format *error-output* "[mqtt-bridge] mappings file is empty — bridge disabled~%")
         nil)
        (t
         ;; allow-overlap=t suppresses overlap warnings (matches mqtt-mapping.lisp
         ;; contract: when MQTT_ALLOW_REGION_OVERLAP=1, no warnings are produced).
         (let* ((allow (member (env "MQTT_ALLOW_REGION_OVERLAP" "0")
                               '("1" "true" "TRUE" "yes")
                               :test #'string=))
                (warnings (mqtt-validate-overlaps registry allow)))
           (dolist (w warnings) (format *error-output* "[mqtt-bridge] warning: ~a~%" w)))
         (cons cfg registry))))))
