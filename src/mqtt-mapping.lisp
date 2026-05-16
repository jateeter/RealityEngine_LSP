(in-package #:reality-engine-lsp)

;;; MQTT mapping registry — Common Lisp twin of
;;; include/reality/mqtt_mapping.hpp in the C++ runtime.
;;;
;;; This module provides only the contract: schema parser, topic-filter
;;; matcher (+ / # wildcards), sensorId template substitution, payload
;;; extractor (csv-float, json, raw), normalizer (passthrough / minmax /
;;; linear) and length validation.  The MQTT client (TCP framing, CONNECT,
;;; SUBSCRIBE, PUBLISH receive, PINGREQ) is intentionally deferred — once
;;; the LSP perception service has an ingest path equivalent to the C++
;;; bridge, the broker driver can be slotted in below without changing
;;; downstream consumers.
;;;
;;; The design rule is the same as the C++ side: MQTT topics describe the
;;; outside world; this registry decides how that world projects into
;;; perceptual space.  Topic strings never embed RE offsets.

(defstruct mqtt-extract-rule
  (type "csv-float")     ; "csv-float" | "json" | "raw" | "single-float"
  (pointer "")           ; for type=="json" — JSON pointer ("/value")
  (index nil))           ; for type=="csv-float" — pick this index; nil = all

(defstruct mqtt-normalize-rule
  (mode "passthrough")   ; "passthrough" | "minmax" | "linear"
  (min 0.0d0)
  (max 1.0d0)
  (scale 1.0d0)
  (offset 0.0d0)
  (clamp-p t))

(defstruct mqtt-mapping-rule
  id
  topic-filter
  (sensor-id-template "")
  (offset 0)
  (length 1)
  extract                ; mqtt-extract-rule
  normalize              ; mqtt-normalize-rule
  (ttl-ms 30000)
  (qos 0)
  (accept-retained-p t)
  (push-mode "debounced")  ; "debounced" | "manual" | "immediate"
  (debounce-ms 250))

(defstruct mqtt-mapping-metrics
  (received 0)
  (mapped 0)
  (rejected 0)
  (stale 0)
  (last-message-at 0)
  (last-error "")
  (last-error-at 0))

(defstruct mqtt-mapping-registry
  (rules (make-array 0 :adjustable t :fill-pointer 0))
  (metrics (make-array 0 :adjustable t :fill-pointer 0)))

;; ── Schema parser ───────────────────────────────────────────────────────────

(defun mqtt-parse-extract (json-obj)
  (if (jobject-p json-obj)
      (make-mqtt-extract-rule
       :type (or (jstring json-obj "type" nil) "csv-float")
       :pointer (or (jstring json-obj "pointer" nil) "")
       :index (jnumber json-obj "index" nil))
      (make-mqtt-extract-rule)))

(defun mqtt-parse-normalize (json-obj)
  (if (jobject-p json-obj)
      (make-mqtt-normalize-rule
       :mode (or (jstring json-obj "mode" nil) "passthrough")
       :min (or (jnumber json-obj "min" nil) 0.0d0)
       :max (or (jnumber json-obj "max" nil) 1.0d0)
       :scale (or (jnumber json-obj "scale" nil) 1.0d0)
       :offset (or (jnumber json-obj "offset" nil) 0.0d0)
       :clamp-p (jbool json-obj "clamp" t))
      (make-mqtt-normalize-rule)))

(defun mqtt-mapping-registry-from-json (json-root)
  (let* ((reg (make-mqtt-mapping-registry))
         (defaults (jget json-root "defaults"))
         (default-ttl (or (and (jobject-p defaults) (jnumber defaults "ttlMs" nil)) 30000))
         (default-qos (or (and (jobject-p defaults) (jnumber defaults "qos" nil)) 0))
         (default-retained (if (jobject-p defaults) (jbool defaults "acceptRetained" t) t))
         (default-push-mode (or (and (jobject-p defaults) (jstring defaults "pushMode" nil)) "debounced"))
         (default-debounce (or (and (jobject-p defaults) (jnumber defaults "debounceMs" nil)) 250))
         (mappings (jget json-root "mappings")))
    (unless (jarray-p mappings)
      (error "mqtt-mappings: missing top-level \"mappings\" array"))
    (dolist (m (jarray-list mappings))
      (unless (jobject-p m)
        (error "mqtt-mappings: each mapping must be a JSON object"))
      (let* ((id (jstring m "id" nil))
             (filter (jstring m "topicFilter" nil))
             (region (jget m "region")))
        (when (or (null id) (zerop (length id)) (null filter) (zerop (length filter)))
          (error "mqtt-mappings: id and topicFilter are required"))
        (let ((rule (make-mqtt-mapping-rule
                     :id id
                     :topic-filter filter
                     :sensor-id-template (or (jstring m "sensorIdTemplate" nil) "")
                     :offset (if (jobject-p region) (truncate (or (jnumber region "offset" nil) 0)) 0)
                     :length (max 1 (if (jobject-p region) (truncate (or (jnumber region "length" nil) 1)) 1))
                     :extract (mqtt-parse-extract (jget m "extract"))
                     :normalize (mqtt-parse-normalize (jget m "normalize"))
                     :ttl-ms (truncate (or (jnumber m "ttlMs" nil) default-ttl))
                     :qos (truncate (or (jnumber m "qos" nil) default-qos))
                     :accept-retained-p (jbool m "acceptRetained" default-retained)
                     :push-mode (or (jstring m "pushMode" nil) default-push-mode)
                     :debounce-ms (truncate (or (jnumber m "debounceMs" nil) default-debounce)))))
          (vector-push-extend rule (mqtt-mapping-registry-rules reg))
          (vector-push-extend (make-mqtt-mapping-metrics) (mqtt-mapping-registry-metrics reg)))))
    reg))

;; ── Topic-filter matching (MQTT v3.1.1 §4.7) ────────────────────────────────

(defun mqtt-split-topic (s)
  (let ((out nil)
        (current (make-string-output-stream)))
    (loop for c across s do
      (if (char= c #\/)
          (progn (push (get-output-stream-string current) out)
                 (setf current (make-string-output-stream)))
          (write-char c current)))
    (push (get-output-stream-string current) out)
    (nreverse out)))

(defun mqtt-match-topic (registry topic)
  "Return (rule-index . captures) for the first matching rule, or NIL."
  (let ((rules (mqtt-mapping-registry-rules registry))
        (topic-levels (mqtt-split-topic topic)))
    (loop for i from 0 below (length rules)
          for rule = (aref rules i)
          for filter-levels = (mqtt-split-topic (mqtt-mapping-rule-topic-filter rule))
          for captures = nil
          for fi = 0 then (1+ fi)
          do (block try
               (let ((ti 0) (ok t))
                 (loop while (< fi (length filter-levels)) do
                   (let ((f (nth fi filter-levels)))
                     (cond
                       ((string= f "#")
                        ;; Multi-level wildcard absorbs the rest.
                        (let ((tail (with-output-to-string (out)
                                      (loop for k from ti below (length topic-levels)
                                            for first = t then nil
                                            do (unless first (write-char #\/ out))
                                               (write-string (nth k topic-levels) out)))))
                          (push tail captures))
                        (setf ti (length topic-levels))
                        (incf fi)
                        (loop-finish))
                       ((>= ti (length topic-levels))
                        (setf ok nil) (loop-finish))
                       ((string= f "+")
                        (push (nth ti topic-levels) captures))
                       ((not (string= f (nth ti topic-levels)))
                        (setf ok nil) (loop-finish)))
                     (incf ti)
                     (incf fi)))
                 (when (and ok
                            (= fi (length filter-levels))
                            (= ti (length topic-levels)))
                   (return-from mqtt-match-topic
                     (cons i (nreverse captures)))))))
    nil))

;; ── sensorId template substitution ─────────────────────────────────────────

(defun mqtt-resolve-sensor-id (rule topic captures)
  (let ((tmpl (mqtt-mapping-rule-sensor-id-template rule)))
    (if (zerop (length tmpl))
        topic
        (with-output-to-string (out)
          (let ((i 0) (n (length tmpl)))
            (loop while (< i n) do
              (if (char= (aref tmpl i) #\{)
                  (let ((close (position #\} tmpl :start (1+ i))))
                    (if close
                        (let* ((idx-str (subseq tmpl (1+ i) close))
                               (idx (handler-case (parse-integer idx-str) (error () nil))))
                          (when (and idx (>= idx 1) (<= idx (length captures)))
                            (write-string (nth (1- idx) captures) out))
                          (setf i (1+ close)))
                        (progn (write-char (aref tmpl i) out)
                               (incf i))))
                  (progn (write-char (aref tmpl i) out)
                         (incf i)))))))))

;; ── Extraction ─────────────────────────────────────────────────────────────

(defun mqtt-payload-to-string (payload-bytes)
  (cond
    ((stringp payload-bytes) payload-bytes)
    ((listp payload-bytes) (map 'string #'code-char payload-bytes))
    ((arrayp payload-bytes) (map 'string #'code-char (coerce payload-bytes 'list)))
    (t "")))

(defun mqtt-parse-csv-floats (s)
  (let ((out nil)
        (current (make-string-output-stream)))
    (flet ((flush ()
             (let ((tok (get-output-stream-string current)))
               (setf current (make-string-output-stream))
               (unless (zerop (length tok))
                 (push (handler-case (let ((*read-default-float-format* 'double-float))
                                       (with-input-from-string (in tok) (read in)))
                         (error () :nan))
                       out)))))
      (loop for c across s do
        (if (or (char= c #\,) (char= c #\Space) (char= c #\Newline)
                (char= c #\Return) (char= c #\Tab))
            (flush)
            (write-char c current)))
      (flush))
    (nreverse out)))

(defun mqtt-navigate-pointer (root pointer)
  "Walk a JSON pointer (RFC 6901 lite) through a parsed JSON tree.  Returns
the addressed value or :missing when any component is absent."
  (cond
    ((or (string= pointer "") (string= pointer "/")) root)
    ((char/= (aref pointer 0) #\/) :missing)
    (t
     (let ((cur root) (pos 1) (n (length pointer)))
       (loop while (and (<= pos n) (not (eq cur :missing))) do
         (let* ((next (position #\/ pointer :start pos))
                (token (subseq pointer pos (or next n))))
           (setf pos (if next (1+ next) (1+ n)))
           (cond
             ((jobject-p cur)
              (let ((found (multiple-value-bind (v present-p) (gethash token cur)
                             (if present-p v :missing))))
                (setf cur found)))
             ((jarray-p cur)
              (let ((idx (handler-case (parse-integer token) (error () nil))))
                (if (and idx (>= idx 0) (< idx (length (jarray-list cur))))
                    (setf cur (nth idx (jarray-list cur)))
                    (setf cur :missing))))
             (t (setf cur :missing)))))
       cur))))

(defun mqtt-extract (rule payload)
  "Return values list + error string.  When error is non-empty, values is nil."
  (let* ((text (mqtt-payload-to-string payload))
         (type (mqtt-extract-rule-type (mqtt-mapping-rule-extract rule)))
         (extract (mqtt-mapping-rule-extract rule)))
    (cond
      ((or (string= type "raw") (string= type "single-float"))
       (handler-case
           (values (list (let ((*read-default-float-format* 'double-float))
                           (with-input-from-string (in text) (read in))))
                   nil)
         (error (c) (values nil (format nil "raw float parse: ~a" c)))))
      ((string= type "csv-float")
       (let ((all (mqtt-parse-csv-floats text))
             (idx (mqtt-extract-rule-index extract)))
         (cond
           ((some (lambda (v) (eq v :nan)) all)
            (values nil "csv contained a non-finite token"))
           (idx
            (if (and (>= idx 0) (< idx (length all)))
                (values (list (nth idx all)) nil)
                (values nil (format nil "csv index ~a out of range (have ~a)" idx (length all)))))
           (t (values all nil)))))
      ((string= type "json")
       (let ((parsed (handler-case (parse-json text) (error (c) (return-from mqtt-extract
                                                                  (values nil (format nil "json parse: ~a" c))))))
             (ptr (mqtt-extract-rule-pointer extract)))
         (let ((node (mqtt-navigate-pointer parsed ptr)))
           (cond
             ((eq node :missing)
              (values nil (format nil "json pointer \"~a\" not found" ptr)))
             ((numberp node) (values (list (coerce node 'double-float)) nil))
             ((jarray-p node)
              (let ((items (mapcar (lambda (x) (if (numberp x) (coerce x 'double-float) :nan))
                                   (jarray-list node))))
                (if (some (lambda (v) (eq v :nan)) items)
                    (values nil "json array contained a non-numeric element")
                    (values items nil))))
             ((stringp node)
              (handler-case (values (list (let ((*read-default-float-format* 'double-float))
                                            (with-input-from-string (in node) (read in))))
                                    nil)
                (error () (values nil (format nil "json string \"~a\" is not numeric" node)))))
             (t (values nil "json pointer target is not number / array / string"))))))
      (t (values nil (format nil "unknown extract.type: ~a" type))))))

;; ── Normalization ──────────────────────────────────────────────────────────

(defun mqtt-clamp-unit (v)
  (cond ((not (realp v)) :nan)
        ((< v 0.0d0) 0.0d0)
        ((> v 1.0d0) 1.0d0)
        (t v)))

(defun mqtt-normalize (rule raw-values)
  "Return (normalized-values . error-string).  error is nil when valid."
  (let ((norm (mqtt-mapping-rule-normalize rule))
        (out nil))
    (dolist (v raw-values)
      (unless (and (realp v) (not (eq v :nan))
                   (not (and (floatp v) (or (sb-ext:float-nan-p v)
                                            (= v sb-ext:double-float-positive-infinity)
                                            (= v sb-ext:double-float-negative-infinity)))))
        (return-from mqtt-normalize (values nil "value is not finite")))
      (let* ((mode (mqtt-normalize-rule-mode norm))
             (transformed
              (cond
                ((string= mode "passthrough") v)
                ((string= mode "minmax")
                 (let ((denom (- (mqtt-normalize-rule-max norm) (mqtt-normalize-rule-min norm))))
                   (when (zerop denom)
                     (return-from mqtt-normalize (values nil "normalize.min == normalize.max")))
                   (/ (- v (mqtt-normalize-rule-min norm)) denom)))
                ((string= mode "linear")
                 (+ (* v (mqtt-normalize-rule-scale norm)) (mqtt-normalize-rule-offset norm)))
                (t (return-from mqtt-normalize
                     (values nil (format nil "unknown normalize.mode: ~a" mode))))))
             (final (if (mqtt-normalize-rule-clamp-p norm)
                        (mqtt-clamp-unit transformed)
                        transformed)))
        (when (eq final :nan)
          (return-from mqtt-normalize (values nil "value not finite after normalize")))
        (push final out)))
    (values (nreverse out) nil)))

(defun mqtt-decode (rule payload)
  "Run extract + normalize + length-validate.  Returns (values-list . error)."
  (multiple-value-bind (raw err) (mqtt-extract rule payload)
    (when err (return-from mqtt-decode (values nil err))))
  (multiple-value-bind (raw _) (mqtt-extract rule payload)
    (declare (ignore _))
    (multiple-value-bind (normalized nerr) (mqtt-normalize rule raw)
      (when nerr (return-from mqtt-decode (values nil nerr)))
      (unless (= (length normalized) (mqtt-mapping-rule-length rule))
        (return-from mqtt-decode
          (values nil (format nil "transformed value count ~a != region.length ~a"
                              (length normalized) (mqtt-mapping-rule-length rule)))))
      (values normalized nil))))

;; ── Overlap validation ──────────────────────────────────────────────────────

(defun mqtt-validate-overlaps (registry &optional allow-overlap)
  (when allow-overlap (return-from mqtt-validate-overlaps nil))
  (let ((rules (mqtt-mapping-registry-rules registry))
        (warnings nil))
    (loop for i from 0 below (length rules) do
      (loop for j from (1+ i) below (length rules) do
        (let* ((a (aref rules i)) (b (aref rules j))
               (a-start (mqtt-mapping-rule-offset a))
               (a-end (+ a-start (mqtt-mapping-rule-length a)))
               (b-start (mqtt-mapping-rule-offset b))
               (b-end (+ b-start (mqtt-mapping-rule-length b))))
          (when (and (< a-start b-end) (< b-start a-end))
            (push (format nil "mappings \"~a\" [~a,~a) and \"~a\" [~a,~a) overlap"
                          (mqtt-mapping-rule-id a) a-start a-end
                          (mqtt-mapping-rule-id b) b-start b-end)
                  warnings)))))
    (nreverse warnings)))

;; ── JSON serialization ─────────────────────────────────────────────────────

(defun mqtt-mapping-registry-to-json (registry)
  (let ((arr (arr))
        (rules (mqtt-mapping-registry-rules registry))
        (metrics (mqtt-mapping-registry-metrics registry)))
    (loop for i from 0 below (length rules) do
      (let* ((r (aref rules i))
             (m (aref metrics i))
             (e (mqtt-mapping-rule-extract r))
             (n (mqtt-mapping-rule-normalize r))
             (extract-obj (obj "type" (mqtt-extract-rule-type e))))
        (unless (zerop (length (mqtt-extract-rule-pointer e)))
          (setf (jget extract-obj "pointer") (mqtt-extract-rule-pointer e)))
        (when (mqtt-extract-rule-index e)
          (setf (jget extract-obj "index") (mqtt-extract-rule-index e)))
        (vector-push-extend
         (obj
          "id" (mqtt-mapping-rule-id r)
          "topicFilter" (mqtt-mapping-rule-topic-filter r)
          "sensorIdTemplate" (mqtt-mapping-rule-sensor-id-template r)
          "region" (obj "offset" (mqtt-mapping-rule-offset r)
                        "length" (mqtt-mapping-rule-length r))
          "extract" extract-obj
          "normalize" (obj "mode" (mqtt-normalize-rule-mode n)
                           "min" (mqtt-normalize-rule-min n)
                           "max" (mqtt-normalize-rule-max n)
                           "scale" (mqtt-normalize-rule-scale n)
                           "offset" (mqtt-normalize-rule-offset n)
                           "clamp" (json-bool (mqtt-normalize-rule-clamp-p n)))
          "ttlMs" (mqtt-mapping-rule-ttl-ms r)
          "qos" (mqtt-mapping-rule-qos r)
          "acceptRetained" (json-bool (mqtt-mapping-rule-accept-retained-p r))
          "pushMode" (mqtt-mapping-rule-push-mode r)
          "debounceMs" (mqtt-mapping-rule-debounce-ms r)
          "counters" (obj "received" (mqtt-mapping-metrics-received m)
                          "mapped" (mqtt-mapping-metrics-mapped m)
                          "rejected" (mqtt-mapping-metrics-rejected m)
                          "stale" (mqtt-mapping-metrics-stale m)
                          "lastMessageAtMs" (mqtt-mapping-metrics-last-message-at m)
                          "lastError" (mqtt-mapping-metrics-last-error m)
                          "lastErrorAtMs" (mqtt-mapping-metrics-last-error-at m)))
         (jarray-list arr))))
    (obj "mappings" arr)))
