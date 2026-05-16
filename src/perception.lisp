(in-package #:reality-engine-lsp)

(defstruct source
  id kind name active-p region pattern frequency amplitude dc-offset
  machine-id machine-name sequence-name sequence-metadata test-sequence inputs cursor loop-p
  sensor-id last-value last-updated ttl-ms)

(defstruct perception-engine
  dimension sources match-algorithm last-push auto-running-p auto-interval-ms)

(defun make-perception-engine-state (dimension)
  (make-perception-engine :dimension dimension
                          :sources (make-hash-table :test #'equal)
                          :match-algorithm "gte"
                          :last-push nil
                          :auto-running-p nil
                          :auto-interval-ms 1000))

(defun sensor-stale-p (source &optional (now (now-ms)))
  "Return true when a sensor source's last-updated timestamp is older than
its TTL window.  Aligns LSP staleness semantics with the C++ runtime — a
stale sensor sources contributes zeros to assembled perception vectors so
machines downstream don't keep reading a value that was supposed to expire."
  (let ((kind (source-kind source))
        (last-updated (or (source-last-updated source) 0))
        (ttl (or (source-ttl-ms source) 0)))
    (and (string= kind "sensor")
         (> last-updated 0)
         (> ttl 0)
         (> (- now last-updated) ttl))))

(defun source-json (source)
  (let* ((now (now-ms))
         (out (obj "id" (source-id source)
                   "type" (source-kind source)
                   "kind" (source-kind source)
                   "name" (source-name source)
                   "active" (json-bool (source-active-p source))
                   "region" (region-json (source-region source)))))
    (cond
      ((string= (source-kind source) "test")
       (setf (jget out "machineId") (or (source-machine-id source) "")
             (jget out "machineName") (or (source-machine-name source) "")
             (jget out "sequenceName") (or (source-sequence-name source) "")
             (jget out "metadata") (or (source-sequence-metadata source) (obj))
             (jget out "sequence") (or (source-test-sequence source) (obj))
             (jget out "inputs") (vectorize (mapcar #'vectorize (source-inputs source)))
             (jget out "cursor") (or (source-cursor source) 0)
             (jget out "loop") (json-bool (source-loop-p source))))
      ((string= (source-kind source) "sensor")
       (let* ((last-updated (or (source-last-updated source) 0))
              (ttl (or (source-ttl-ms source) 5000))
              (age (if (> last-updated 0) (- now last-updated) 0))
              (stale-p (sensor-stale-p source now)))
         (setf (jget out "sensorId") (or (source-sensor-id source) "")
               (jget out "lastValue") (vectorize (or (source-last-value source) nil))
               (jget out "lastUpdated") last-updated
               (jget out "ttlMs") ttl
               (jget out "ageMs") age
               (jget out "stale") (json-bool stale-p))))
      (t
       (setf (jget out "pattern") (or (source-pattern source) "constant")
             (jget out "frequency") (or (source-frequency source) 1.0d0)
             (jget out "amplitude") (or (source-amplitude source) 1.0d0)
             (jget out "dcOffset") (or (source-dc-offset source) 0.0d0))))
    out))

(defun source-from-json (json)
  (let* ((kind (string-downcase (or (jstring json "type" nil)
                                    (jstring json "kind" nil)
                                    "simulated")))
         (region (if (jobject-p (jget json "region"))
                     (make-region-from-json (jget json "region"))
                     (make-region :offset 0 :length 1))))
    (make-source
     :id (or (jstring json "id" nil) (make-id "source"))
     :kind kind
     :name (or (jstring json "name" nil) "source")
     :active-p (jbool json "active" t)
     :region region
     :pattern (or (jstring json "pattern" nil) "constant")
     :frequency (or (jnumber json "frequency" nil) 1.0d0)
     :amplitude (or (jnumber json "amplitude" nil) 1.0d0)
     :dc-offset (or (jnumber json "dcOffset" nil) 0.0d0)
     :machine-id (jstring json "machineId" nil)
     :machine-name (jstring json "machineName" nil)
     :sequence-name (jstring json "sequenceName" nil)
     :sequence-metadata (or (jget json "metadata") (obj))
     :test-sequence (or (jget json "sequence") (obj))
     :inputs (mapcar #'numbers-from-json (jarray-list (or (jget json "inputs") (arr))))
     :cursor 0
     :loop-p (jbool json "loop" t)
     :sensor-id (jstring json "sensorId" nil)
     :last-value (numbers-from-json (or (jget json "lastValue") (arr)))
     :last-updated (or (jnumber json "lastUpdated" nil) 0)
     :ttl-ms (or (jnumber json "ttlMs" nil) 5000))))

(defun ensure-source-id (engine source)
  (unless (source-id source)
    (setf (source-id source) (make-id "source")))
  (setf (gethash (source-id source) (perception-engine-sources engine)) source)
  source)

(defun sample-source (source dimension)
  (let ((values (make-list dimension :initial-element 0.0d0)))
    (when (source-active-p source)
      (let* ((region (source-region source))
             (offset (region-offset region))
             (length (region-length region))
             (payload
               (cond
                 ((string= (source-kind source) "test")
                  (let ((inputs (source-inputs source)))
                    (when inputs
                      (let* ((cursor (or (source-cursor source) 0))
                             (index (min cursor (1- (length inputs))))
                             (selected (nth index inputs)))
                        (if (< (1+ cursor) (length inputs))
                            (incf (source-cursor source))
                            (when (source-loop-p source)
                              (setf (source-cursor source) 0)))
                        selected))))
                 ((string= (source-kind source) "sensor")
                  ;; TTL eviction — when a sensor source has gone stale we
                  ;; drop its contribution to the assembled vector.  Matches
                  ;; CPP behaviour where the C++ engine's sample path checks
                  ;; the TTL window on every sample call.
                  (when (not (sensor-stale-p source))
                    (source-last-value source)))
                 (t
                  (make-list length :initial-element (or (source-dc-offset source) 0.0d0))))))
        (loop for value in (or payload nil)
              for i from offset
              repeat length
              when (< i dimension)
                do (setf (nth i values) value))))
    values))

(defun assemble-perception-vector (engine)
  (let* ((dimension (perception-engine-dimension engine))
         (assembled (make-list dimension :initial-element 0.0d0)))
    (maphash
     (lambda (_ source)
       (declare (ignore _))
       (let ((sample (sample-source source dimension)))
         (loop for value in sample
               for i from 0
               when (not (zerop value))
                 do (setf (nth i assembled) value))))
     (perception-engine-sources engine))
    assembled))

(defun perception-state-json (engine)
  (obj "dimension" (perception-engine-dimension engine)
       "matchAlgorithm" (perception-engine-match-algorithm engine)
       "sources" (vectorize (mapcar #'source-json (object-values (perception-engine-sources engine))))
       "lastPush" (or (perception-engine-last-push engine) +json-null+)
       "autoRunning" (json-bool (perception-engine-auto-running-p engine))
       "autoIntervalMs" (perception-engine-auto-interval-ms engine)))
