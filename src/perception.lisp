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

(defun source-validated-active-p (source &optional (now (now-ms)))
  "Recompute whether SOURCE can currently supply a value.

Activity is *validated*, never assigned (RealityEngine_CI#163 point 3): the
answer is derived from the source's own state under the rules below and never
read back from its previously stored flag, so the same source in the same
condition validates identically on every runtime.

  sensor     — holds a value inside its TTL window.  Reuses SENSOR-STALE-P,
               the predicate the assembly path already applies to zero a
               stale sensor's contribution, so the reported flag and the
               actual contribution can no longer disagree.
  test       — its interned sequence is non-empty.  A test source with no
               inputs supplies nothing, so calling it active would be an
               assignment rather than a validation.
  simulated  — generates from the global step, so it always has a value.

NOW is a parameter so a caller validating many sources reads the clock once:
two identically configured sensors must not validate differently because the
loop crossed a millisecond boundary."
  (let ((kind (source-kind source)))
    (cond
      ((string= kind "sensor")
       (and (source-last-value source)
            (not (sensor-stale-p source now))
            t))
      ((string= kind "test")
       (and (source-inputs source) t))
      (t t))))

(defun record-sensor-value (source values &optional (now (now-ms)))
  "Store a sensor reading and let the value earn activity.

Every sensor ingress path funnels through here.  Before, a value arriving set
lastValue and lastUpdated but never touched the active flag, which was safe
only because nothing ever cleared it — sensors became active at registration
and stayed that way.  Now that reset validates activity, an expired sensor is
correctly deactivated, and without this a later reading would leave it
inactive forever: SAMPLE-SOURCE gates on the flag, so the source would
contribute zeros while holding a fresh value.  That would be a worse defect
than the reporting one this replaces."
  (setf (source-last-value source) values
        (source-last-updated source) now)
  (setf (source-active-p source) (source-validated-active-p source now))
  source)

(defun reset-perception-engine (engine)
  "Reset playback state in place, keeping the registered sources.

Resets what a run accumulates — global step, the persistent vector, and the
playback cursor of every *test* source — then *validates* each source's
activity, matching C++ `PerceptionEngine::reset` and Scala
`PerceptionEngine.reset()`.

Reset does not assign activity (RealityEngine_CI#163 point 3). It used to:
every test source was forced active and every other kind was left holding
whatever flag it happened to carry, so a sensor whose TTL had expired before
the reset was still advertised `active: true` afterwards by `/api/sources` and
`/api/state` — a source contributing nothing, reported as live, on a
byte-compared payload. Now the flag is recomputed from each source's own state
by SOURCE-VALIDATED-ACTIVE-P once the run state above has been cleared.

The prior flag is deliberately not carried forward. An operator-deactivated
source is run state, not configuration, so a source paused before the reset is
re-activated by it if it validates active. The clock is read once for the whole
pass so two identically configured sensors cannot disagree.

Membership is untouched either way: reset never creates a source and never
removes one (contract point 4).

Sources deliberately survive. Replacing the engine struct wholesale (the
previous behaviour) discarded them, so `POST /api/reset` silently emptied the
PE: after a reset this runtime assembled all-zero vectors and contributed
nothing, while C++ and Scala kept replaying their sequences. Three runtimes
given the same corpus then presented three different inputs, and the trajectory
comparison reported it as engine divergence (RealityEngine_CI corpus parity
sweep, 2026-08-19).

Removing a source is a separate operation and is still supported: `DELETE
/api/sources/:id`, and the corpus-driven path that drops a machine's source when
the machine leaves the dynamic corpus. Reset means \"start this run again\",
not \"forget what is connected\".

Left alone on purpose: dimension, match algorithm, and the auto-push settings.
Those are configuration rather than run state, and C++ and Scala do not clear
them either — the previous implementation reset the match algorithm to \"gte\"
and the auto interval to 1000ms as a side effect of rebuilding the struct."
  (setf (perception-engine-global-step engine) 0)
  (setf (perception-engine-last-push engine) nil)
  (let ((pv (perception-engine-persistent-vector engine)))
    (when pv (fill pv 0.0d0)))
  (let ((sources (perception-engine-sources engine))
        (now (now-ms)))
    (when sources
      (maphash (lambda (id source)
                 (declare (ignore id))
                 ;; Rewind the playback cursor of test sources only. C++ and
                 ;; Scala rewind test cursors and re-seed RandomWalk; this
                 ;; runtime zeroed `cursor' for every kind, reaching into
                 ;; sensor and simulated sources that do not have a playback
                 ;; position (#64). The cursor is read only by the "test"
                 ;; branch of SAMPLE-SOURCE and advanced only by the "test"
                 ;; branch of ADVANCE-PERCEPTION-ENGINE, so writing it on the
                 ;; other kinds was touching a field that is not theirs.
                 ;;
                 ;; There is no RandomWalk seed to re-seed here: this
                 ;; runtime's simulated branch is stateless, deriving its
                 ;; payload from `dcOffset' rather than from a walk carried
                 ;; between steps. Nothing is owed on that half of the
                 ;; parity, and if a stateful pattern generator ever lands it
                 ;; re-seeds here.
                 (when (string= (source-kind source) "test")
                   (setf (source-cursor source) 0))
                 (setf (source-active-p source)
                       (source-validated-active-p source now)))
               sources)))
  engine)

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

(defun source-json (source)
  (let* ((now (now-ms))
         ;; `active' is what this source will actually contribute on the next
         ;; assembly, not the raw stored flag.  The flag alone went stale
         ;; between resets: nothing runs when a sensor's TTL lapses, so an
         ;; expired sensor kept advertising `active: true` on /api/sources and
         ;; /api/state while SAMPLE-SOURCE was already zeroing it.  This
         ;; runtime computed the staleness for serialization (`stale', below)
         ;; and then declined to use it for the one field that should reflect
         ;; it.  Both halves matter: the stored flag still gates, so a source
         ;; an operator paused via PATCH /api/sources/:id or a finished
         ;; non-looping test source still reports inactive.
         (active (and (source-active-p source)
                      (source-validated-active-p source now)))
         ;; `kind' was a duplicate of `type' unique to this runtime; the
         ;; canonical source shape is the C++ one (RealityEngine_CI#91).
         (out (obj "id" (source-id source)
                   "type" (source-kind source)
                   "name" (source-name source)
                   "active" (json-bool active)
                   "region" (region-json (source-region source)))))
    (cond
      ((string= (source-kind source) "test")
       (setf (jget out "machineId") (or (source-machine-id source) "")
             (jget out "machineName") (or (source-machine-name source) "")
             (jget out "sequenceName") (or (source-sequence-name source) "")
             (jget out "metadata") (or (source-sequence-metadata source) (obj))
             (jget out "sequence") (or (source-test-sequence source) (obj))
             (jget out "inputs") (vectorize (mapcar #'vectorize (source-inputs source)))
             ;; `cursor' is internal playback state, not part of the canonical
             ;; source shape — no other runtime exposes it.
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

(defun sources-in-canonical-order (engine)
  "Sources ordered by (name, id).

The sources table is keyed by id and ids are generated per runtime, so every
PE listed sources differently — C++ by id, Scala and LSP by hash order,
TypeScript by insertion order.  Four engines, four orderings, on an endpoint
under byte comparison."
  (sort (object-values (perception-engine-sources engine))
        (lambda (a b)
          (let ((na (or (source-name a) "")) (nb (or (source-name b) "")))
            (cond ((string< na nb) t)
                  ((string> na nb) nil)
                  (t (and (string< (or (source-id a) "") (or (source-id b) "")) t)))))))

(defun advance-perception-engine (engine)
  "Advance playback by one step: global step, and each active test source's cursor.

Called once per push, after the vector has been assembled and sent — matching
`PerceptionEngine::advance` (C++) and `PerceptionEngine.advance()` (Scala).

The cursor used to advance inside SAMPLE-SOURCE, which is called from
ASSEMBLE-PERCEPTION-VECTOR, which is called by `/api/state` as well as by the
push path. Reading the engine therefore advanced its playback: a caller that
polled state before each push consumed two vectors per push, so this runtime
walked a machine's interned sequence at a different rate from C++ and Scala and
its trajectory diverged from theirs. Observation must not mutate the thing
observed (RealityEngine_CI corpus parity sweep, 2026-08-19)."
  (incf (perception-engine-global-step engine))
  (let ((sources (perception-engine-sources engine)))
    (when sources
      (maphash
       (lambda (id source)
         (declare (ignore id))
         (when (and (source-active-p source)
                    (string= (source-kind source) "test"))
           (let* ((inputs (source-inputs source))
                  (count  (length inputs))
                  (cursor (or (source-cursor source) 0)))
             (when (plusp count)
               (if (< (1+ cursor) count)
                   (setf (source-cursor source) (1+ cursor))
                   (progn
                     (setf (source-cursor source) 0)
                     (unless (source-loop-p source)
                       (setf (source-active-p source) nil))))))))
       sources)))
  engine)

(defun sample-source (source dimension)
  "Return (values PAYLOAD OFFSET LENGTH) for SOURCE, or NIL when inactive.

A pure read: it reports what the source currently publishes and does not
advance it. ADVANCE-PERCEPTION-ENGINE does that, once per push.

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
                             (index (min cursor (1- (length inputs)))))
                        (nth index inputs)))))
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

(defun clamp-cell (value)
  "Coerce VALUE to a perceptual cell: double-float in [0,1]."
  (let ((v (coerce (or value 0) 'double-float)))
    (max 0.0d0 (min 1.0d0 v))))

(defun assemble-perception-vector (engine)
  (let* ((dimension (perception-engine-dimension engine))
         (pv (perception-engine-persistent-vector engine))
         (assembled (make-array dimension :element-type 'double-float
                                          :initial-element 0.0d0)))
    (when pv
      (loop for i from 0 below (min dimension (length pv))
            do (setf (aref assembled i) (elt pv i))))
    ;; Canonical order, not hash order. Two machines may declare the same input
    ;; region — AGX032 and AGX054 both map [228:232] — and a source owns its
    ;; region, so where regions overlap the last writer wins. Iterating the
    ;; sources table with MAPHASH made that winner depend on hash order, which is
    ;; unspecified and differs per runtime; C++ walked a std::map keyed by
    ;; source id and Scala walked a Map in its own hash order, so the three
    ;; runtimes assembled different input vectors from identical corpora
    ;; (RealityEngine_CI corpus parity sweep, 2026-08-19).
    ;;
    ;; SOURCES-IN-CANONICAL-ORDER sorts by (name, id) — the same order already
    ;; used for the listing endpoints, and derived from corpus-declared names
    ;; rather than runtime-minted ids.
    (dolist (source (sources-in-canonical-order engine))
     (let ()
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
           ;; Every cell of the region is written, zeros included, and clamped
           ;; to [0,1] — the same rule C++ (PerceptionEngine::assemble_vector)
           ;; and Scala (PerceptionEngine.assembleVector) apply. A source owns
           ;; its region: what it publishes is what the RE must see.
           ;;
           ;; This used to skip zero-valued cells, so a cell a source drove low
           ;; kept whatever the persistent vector held from the previous step,
           ;; and the RE was handed an input the source never published. A CES
           ;; needing a cell to go low then never matched again — silently, with
           ;; no output and no error. DLX011's req/ack handshake needs
           ;; [1,0,0,0] then [0,1,0,0]; this runtime presented [1,1,0,0] at the
           ;; second step and fired nothing across six pushes where C++ and
           ;; Scala each fired twice (#53).
           ;;
           ;; Clamping was absent for the same reason the zeros were: the write
           ;; was treated as an overlay rather than as the region's value. An
           ;; unclamped 2.0 reaching a comparator whose threshold is above 1.0
           ;; makes the same machine decide differently per runtime.
           (loop for value in payload
                 for i from offset
                 repeat length
                 when (< i dimension)
                   do (setf (aref assembled i) (clamp-cell value)))))))
    (coerce assembled 'list)))

(defun perception-state-json (engine)
  (obj "perceptionDimension" (perception-engine-dimension engine)
       "matchAlgorithm" (perception-engine-match-algorithm engine)
       "globalStep" (or (perception-engine-global-step engine) 0)
       "sources" (vectorize (mapcar #'source-json (sources-in-canonical-order engine)))
       "assembledVector" (vectorize (assemble-perception-vector engine))
       "lastPush" (or (perception-engine-last-push engine) +json-null+)
       "auto" (obj "running" (json-bool (perception-engine-auto-running-p engine))
                   "intervalMs" (perception-engine-auto-interval-ms engine))))
