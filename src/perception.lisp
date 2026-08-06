(in-package #:reality-engine-lsp)

(defstruct source
  id kind name active-p region pattern frequency amplitude dc-offset
  machine-id machine-name sequence-name sequence-metadata test-sequence inputs cursor loop-p
  sensor-id last-value last-updated ttl-ms
  ;; Provenance — which integration feeds this source ("mqtt", "openclaw",
  ;; "ollama", "healthkit", "carekit", "localai", "signal").  NIL for
  ;; manually created sources; omitted from JSON when unset.
  origin)

(defstruct perception-engine
  dimension sources match-algorithm last-push auto-running-p auto-interval-ms
  persistent-vector global-step)

(defun make-perceptual-buffer (dimension)
  "Adjustable double-float buffer of DIMENSION zeros.

Deliberately an array rather than a list.  The perceptual space grows to
cover machines whose perceptualMapping extends past the configured default —
14384 for the current deployment corpus — and the previous list
representation indexed with NTH made assembly O(n^2): a full-dimension list
per source, each element written by walking the list from the head."
  (make-array (max 0 dimension) :element-type 'double-float
                                :initial-element 0.0d0
                                :adjustable t))

(defun make-perception-engine-state (dimension)
  (make-perception-engine :dimension dimension
                          :sources (make-hash-table :test #'equal)
                          :match-algorithm "gte"
                          :last-push nil
                          :auto-running-p nil
                          :auto-interval-ms 1000
                          :persistent-vector (make-perceptual-buffer dimension)
                          :global-step 0))

(defun ensure-perception-dimension (engine required-end &optional context)
  "Grow the perceptual space so [0, REQUIRED-END) is addressable.

The RE grows its space to fit every loaded machine's mapping, so the PE must
too — otherwise a source whose region starts past the dimension is stored,
counted and returned by /api/pe/sources, then silently dropped at assembly
and its machine never receives input."
  (let ((current (or (perception-engine-dimension engine) 0)))
    (when (> required-end current)
      (let ((pv (perception-engine-persistent-vector engine)))
        (setf (perception-engine-persistent-vector engine)
              (if (and pv (adjustable-array-p pv))
                  (adjust-array pv required-end :initial-element 0.0d0)
                  (let ((grown (make-perceptual-buffer required-end)))
                    (when pv
                      (loop for i from 0 below (min (length pv) required-end)
                            do (setf (aref grown i) (elt pv i))))
                    grown))))
      (setf (perception-engine-dimension engine) required-end)
      (format *error-output*
              "~&[PerceptionEngine] perceptionDimension grew ~a -> ~a~@[ for ~a~]~%"
              current required-end context)))
  (perception-engine-dimension engine))

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
               (jget out "stale") (json-bool stale-p))
         (when (source-origin source)
           (setf (jget out "origin") (source-origin source)))))
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
     :ttl-ms (or (jnumber json "ttlMs" nil) 5000)
     :origin (jstring json "origin" nil))))

(defun ensure-source-id (engine source)
  (unless (source-id source)
    (setf (source-id source) (make-id "source")))
  ;; Grow to cover the source's region before registering it, so a machine
  ;; whose perceptualMapping.input starts past the configured dimension still
  ;; receives input on every push.
  (let ((region (source-region source)))
    (when region
      (ensure-perception-dimension
       engine
       (+ (region-offset region) (region-length region))
       (format nil "source '~a' region [~a,~a)"
               (or (source-name source) (source-id source))
               (region-offset region)
               (+ (region-offset region) (region-length region))))))
  (setf (gethash (source-id source) (perception-engine-sources engine)) source)
  source)

(defun sample-source (source dimension)
  "Return (values PAYLOAD OFFSET LENGTH) for SOURCE, or NIL when inactive.

Previously returned a freshly allocated DIMENSION-length list with the
payload written at OFFSET, which cost O(dimension) allocation and O(n^2)
writes per source.  Callers now place the payload themselves."
  (declare (ignorable dimension))
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
      (when payload
        (values payload offset length)))))

(defun update-from-perceptual-space (engine ps)
  "Adopt the RE's post-merge perceptual space.

The RE grows to fit every loaded machine's mapping, so PS may be longer than
our dimension — grow to match rather than truncating to it."
  (ensure-perception-dimension engine (length ps)
                               "perceptual space returned by the Reality Engine")
  (let ((pv (perception-engine-persistent-vector engine))
        (dim (perception-engine-dimension engine)))
    (loop for i from 0 below dim do
      (setf (aref pv i) (coerce (or (elt ps i) 0) 'double-float)))))

(defun assemble-perception-vector (engine)
  (let* ((dimension (perception-engine-dimension engine))
         (pv (perception-engine-persistent-vector engine))
         (assembled (make-array dimension :element-type 'double-float
                                          :initial-element 0.0d0)))
    (when pv
      (loop for i from 0 below (min dimension (length pv))
            do (setf (aref assembled i) (elt pv i))))
    (maphash
     (lambda (_ source)
       (declare (ignore _))
       (multiple-value-bind (payload offset length) (sample-source source dimension)
         (when payload
           ;; Growth should make this unreachable; if a region still does not
           ;; fit, say which machine lost its input rather than dropping it
           ;; silently.
           (when (> (+ offset length) dimension)
             (format *error-output*
                     "~&[PerceptionEngine] source '~a' region [~a,~a) exceeds perceptionDimension ~a — region not written (machineId=~a, sourceId=~a)~%"
                     (or (source-name source) (source-id source))
                     offset (+ offset length) dimension
                     (or (source-machine-id source) "") (or (source-id source) "")))
           (loop for value in payload
                 for i from offset
                 repeat length
                 when (and (< i dimension) (not (zerop value)))
                   do (setf (aref assembled i) (coerce value 'double-float))))))
     (perception-engine-sources engine))
    (coerce assembled 'list)))

(defun perception-state-json (engine)
  (obj "dimension" (perception-engine-dimension engine)
       "matchAlgorithm" (perception-engine-match-algorithm engine)
       "globalStep" (or (perception-engine-global-step engine) 0)
       "sources" (vectorize (mapcar #'source-json (object-values (perception-engine-sources engine))))
       "assembledVector" (vectorize (assemble-perception-vector engine))
       "lastPush" (or (perception-engine-last-push engine) +json-null+)
       "auto" (obj "running" (json-bool (perception-engine-auto-running-p engine))
                   "intervalMs" (perception-engine-auto-interval-ms engine))))
