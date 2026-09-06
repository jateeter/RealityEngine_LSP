(in-package #:reality-engine-lsp)

(defstruct reality-state
  dimension machines machine-dir perceptual-space history history-limit include-machine-results-p include-active-regions-p
  ;; Audit trail for POST /api/engine/process — what GET /api/engine/history
  ;; serves, here and on every other runtime.
  ;;
  ;; These records used to be pushed onto `history` alongside step records, and
  ;; both endpoints served that one list. So /api/engine/history returned step
  ;; records this runtime alone put there, and /api/perceptual-simulation/history
  ;; returned engine-process envelopes interleaved with the steps. Two surfaces,
  ;; one list, neither meaning what it does on C++ (RealityEngine_CI#148).
  ;; `history` is now the step history only, and this is the audit trail.
  engine-history
  ;; Trajectory histories — see SURFACE_SPEC.md, "Trajectory histories".
  ;; Oldest-first, capped at +trajectory-capacity+.
  isre-history osre-history
  include-perceptual-space-p vector-store sequences qdrant-url collection-name started-at
  event-bus-subscriptions latched-event-bits step-count mapping-version
  ;; CES coverage counters — mirror the canonical CesCoverageRegistry and
  ;; RealityEngine_CPP/CesCoverageRegistry so the same Prometheus scrape
  ;; config covers all three runtimes.  Keyed by tab-joined identifiers.
  cov-matched cov-activated cov-outputs cov-steps cov-paging cov-deprecated
  ;; Semantic audit trail — re:SequenceObservation records emitted while
  ;; machines process input (RealityEngine_Machines
  ;; docs/SEMANTIC_AUDIT_CONTRACT.md, milestone M5).  Newest-last list,
  ;; truncated to +semantic-audit-capacity+.
  semantic-audit
  checkpoints
  match-threshold
  sampler-running-p sampler-strategy sampler-interval-ms sampler-sample-count
  sim-buffer sim-buffered-region sim-buffered-delay
  ;; Arbitration records for the most recent step (ARBITER_CONTRACT.md 6).
  ;; A resolution nobody can observe is indistinguishable from no resolution,
  ;; and a suppressed contribution has to stay attributable.
  arbitration)

(defun compose-key (producer-machine-id producer-sequence-id)
  (format nil "~a|~a" producer-machine-id producer-sequence-id))

(defun semantics-key-for-rel (rel)
  "Manifest keys are <domain>/<stem>: domains/<d>/X.json -> d/X, else core/X."
  (let ((no-ext (if (uiop:string-suffix-p rel ".json")
                    (subseq rel 0 (- (length rel) 5))
                    rel)))
    (if (uiop:string-prefix-p "domains/" no-ext)
        (subseq no-ext 8)
        (format nil "core/~a" no-ext))))

(defun machine-json-list-rows (machine-dir &optional semantics-by-key)
  "Rows for GET /api/machines/json/list — recursive so files in domain
subdirectories (machines/domains/<name>/) are included. SEMANTICS-BY-KEY,
when provided, is the \"machines\" object of the corpus OWL semantics
manifest; matching rows gain semanticsIri/semanticsHash (roadmap M4)."
  (let ((dir (uiop:ensure-directory-pathname machine-dir))
        (rows nil))
    (when (uiop:directory-exists-p dir)
      ;; collect-json-files-recursive returns truenames; relativize against
      ;; the directory truename so relFile survives symlinked roots.
      (let ((dir-name (namestring (truename dir))))
        (dolist (path (collect-json-files-recursive dir))
          (let* ((full (namestring path))
                 (rel (if (and (> (length full) (length dir-name))
                               (string= dir-name full :end2 (length dir-name)))
                          (subseq full (length dir-name))
                          (file-namestring path)))
                 (rel-clean (substitute #\/ #\\ rel))
                 (row (obj "filename" (file-namestring path)
                           "relFile" rel-clean
                           "name" (pathname-name path)
                           "description" ""
                           "version" "1.0.0"
                           "metadata" (obj)
                           "sequenceCount" 0)))
            (when semantics-by-key
              (let ((entry (jget semantics-by-key (semantics-key-for-rel rel-clean))))
                (when (jobject-p entry)
                  (setf (jget row "semanticsIri") (jstring entry "iri" +json-null+))
                  (setf (jget row "semanticsHash") (jstring entry "sha256" +json-null+)))))
            (push row rows)))))
    (nreverse rows)))

(defun resolve-machine-json-path (machine-dir name)
  "Resolve NAME to a machine JSON file under MACHINE-DIR. Tries the flat
path first, then falls back to a recursive basename search so files in
domain subdirectories load by filename (corpus filenames are unique)."
  (when (search ".." name)
    (error "Invalid machine name: ~a" name))
  (let* ((dir (uiop:ensure-directory-pathname machine-dir))
         (filename (if (and (>= (length name) 5)
                            (string= ".json" name :start2 (- (length name) 5)))
                       name
                       (format nil "~a.json" name)))
         (flat (merge-pathnames filename dir)))
    (if (probe-file flat)
        flat
        (or (find filename (collect-json-files-recursive dir)
                  :key #'file-namestring :test #'string=)
            flat))))

(defun ensure-space-length (state length)
  "Grow the perceptual space so [0, LENGTH) is addressable.

Regression guard for #24: a machine whose perceptualMapping runs past the
configured dimension must still receive input.  The growth path is
GROW-PERCEPTUAL-SPACE (doubling, amortised O(1)) rather than the old
append + make-list, which allocated a fresh full-length list and measured it
with two more O(n) LENGTH calls."
  (setf (reality-state-perceptual-space state)
        (grow-perceptual-space (reality-state-perceptual-space state) length))
  (when (> length (reality-state-dimension state))
    (setf (reality-state-dimension state) length)))

(defun register-compose-subscriptions (state machine)
  (let* ((compose (jget (machine-metadata machine) "compose"))
         (subscriptions (and (jobject-p compose) (jget compose "subscriptions"))))
    (when (jarray-p subscriptions)
      (dolist (sub (jarray-list subscriptions))
        (let ((producer-machine-id (jstring sub "producerMachineId" nil))
              (producer-sequence-id (jstring sub "producerSequenceId" nil))
              (bit-offset (jnumber sub "bitOffset" nil)))
          (when (and producer-machine-id producer-sequence-id bit-offset (>= bit-offset 0))
            (let* ((bit (truncate bit-offset))
                   (key (compose-key producer-machine-id producer-sequence-id))
                   (row (obj "producerMachineId" producer-machine-id
                             "producerSequenceId" producer-sequence-id
                             "subscriberMachineId" (machine-id machine)
                             "bitOffset" bit)))
              (push row (gethash key (reality-state-event-bus-subscriptions state)))
              (ensure-space-length state (1+ bit)))))))))

(defun unregister-compose-subscriptions (state machine-id)
  (let ((updates nil)
        (deletes nil))
    (maphash
     (lambda (key rows)
       (let ((remaining (remove-if (lambda (row)
                                     (string= (jstring row "subscriberMachineId" "") machine-id))
                                   rows)))
         (if remaining
             (push (cons key remaining) updates)
             (push key deletes))))
     (reality-state-event-bus-subscriptions state))
    (dolist (update updates)
      (setf (gethash (car update) (reality-state-event-bus-subscriptions state))
            (cdr update)))
    (dolist (key deletes)
      (remhash key (reality-state-event-bus-subscriptions state)))))

(defun event-bus-subscription-count (state)
  (let ((count 0))
    (maphash (lambda (_ rows)
               (declare (ignore _))
               (incf count (length rows)))
             (reality-state-event-bus-subscriptions state))
    count))

(defun put-machine (state machine)
  (unregister-compose-subscriptions state (machine-id machine))
  (setf (gethash (machine-id machine) (reality-state-machines state)) machine)
  (incf (reality-state-mapping-version state))
  (when (machine-mapping machine)
    (ensure-space-length state (max (+ (region-offset (mapping-input (machine-mapping machine)))
                                      (region-length (mapping-input (machine-mapping machine))))
                                   (+ (region-offset (mapping-output (machine-mapping machine)))
                                      (region-length (mapping-output (machine-mapping machine)))))))
  (register-compose-subscriptions state machine)
  machine)

(defun make-reality-state-from-config (&key machine-dir dimension)
  (let ((state
          (make-reality-state
           :dimension dimension
           :machines (make-hash-table :test #'equal)
           :machine-dir machine-dir
           :perceptual-space (make-perceptual-space dimension)
           :history nil
           :engine-history nil
           :isre-history nil
           :osre-history nil
           :history-limit (env-int "RE_HISTORY_LIMIT" 250)
           :include-machine-results-p (env-bool "RE_INCLUDE_MACHINE_RESULTS" t)
           ;; Defaults stay full so no existing caller changes shape and the
           ;; parity gates keep comparing identical key sets (SURFACE_SPEC.md).
           :include-active-regions-p (env-bool "RE_INCLUDE_ACTIVE_REGIONS" t)
           :include-perceptual-space-p (env-bool "RE_INCLUDE_PERCEPTUAL_SPACE" t)
           :vector-store (make-hash-table :test #'equal)
           :sequences (make-hash-table :test #'equal)
           :qdrant-url (env "QDRANT_URL" "http://localhost:4333")
           :collection-name (env "QDRANT_REALITY_COLLECTION" "reality-events")
           :started-at (now-ms)
           :event-bus-subscriptions (make-hash-table :test #'equal)
           :latched-event-bits (make-hash-table :test #'equal)
           :step-count 0
           :mapping-version 0
           :cov-matched    (make-hash-table :test #'equal)
           :cov-activated  (make-hash-table :test #'equal)
           :cov-outputs    (make-hash-table :test #'equal)
           :cov-steps      (make-hash-table :test #'equal)
           :cov-paging     (make-hash-table :test #'equal)
           :cov-deprecated (make-hash-table :test #'equal)
           :checkpoints    (make-hash-table :test #'equal)
           :match-threshold 0.5d0
           :sampler-running-p nil
           :sampler-strategy "manual"
           :sampler-interval-ms 0
           :sampler-sample-count 0
           :sim-buffer nil
           :sim-buffered-region nil
           :sim-buffered-delay 100)))
    (dolist (machine (load-machines-from-directory machine-dir))
      (put-machine state machine))
    state))

(defun record-history (state item)
  (push item (reality-state-history state))
  (when (> (length (reality-state-history state)) (reality-state-history-limit state))
    (setf (reality-state-history state)
          (subseq (reality-state-history state) 0 (reality-state-history-limit state)))))

;; Newest-first and capped at 256, matching C++'s record_engine_history.
(defconstant +engine-history-capacity+ 256)

(defun record-engine-history (state item)
  (push item (reality-state-engine-history state))
  (when (> (length (reality-state-engine-history state)) +engine-history-capacity+)
    (setf (reality-state-engine-history state)
          (subseq (reality-state-engine-history state) 0 +engine-history-capacity+))))

(defconstant +trajectory-capacity+ 1024)

;; Dense vector -> sparse trajectory entry. A cell absent from `nonZero` is
;; zero; `length` keeps the dense width so the reconstruction is exact.
(defun sparse-trajectory (step-number dense)
  ;; MAP NIL rather than DOLIST: DENSE is the perceptual space, now a vector.
  (let ((cells nil)
        (index 0))
    (map nil (lambda (value)
               (unless (zerop value)
                 (push (obj "index" index "value" value) cells))
               (incf index))
         dense)
    (obj "stepNumber" step-number
         "length" (length dense)
         "nonZero" (vectorize (nreverse cells)))))

;; Appends ISRE(n) and OSRE(n) together. They are captured at their own
;; observation points inside the step and recorded in one action, so no observer
;; can see a step whose trajectories are half-written.
;;
;; Oldest-first. The step history is newest-first because it is read as "what
;; just happened"; these are read as sequences to be compared element by element,
;; and the index of the first disagreement is the answer they exist to give.
(defun record-trajectory (state isre osre)
  (setf (reality-state-isre-history state)
        (last (append (reality-state-isre-history state) (list isre)) +trajectory-capacity+)
        (reality-state-osre-history state)
        (last (append (reality-state-osre-history state) (list osre)) +trajectory-capacity+)))

;; Ascending stepNumber. `from` is the first stepNumber to include; `limit` caps
;; the entries returned from there (0 or nil = all).
(defun trajectory-window (entries from limit)
  (let ((selected (remove-if (lambda (entry) (< (jnumber entry "stepNumber" 0) from)) entries)))
    (vectorize (if (and limit (> limit 0))
                   (subseq selected 0 (min limit (length selected)))
                   selected))))

(defun required-dimension (state)
  (let ((required (reality-state-dimension state)))
    (maphash
     (lambda (_ machine)
       (declare (ignore _))
       (when (machine-mapping machine)
         (let* ((mapping (machine-mapping machine))
                (input-end (+ (region-offset (mapping-input mapping))
                              (region-length (mapping-input mapping))))
                (output-end (+ (region-offset (mapping-output mapping))
                               (region-length (mapping-output mapping)))))
           (setf required (max required input-end output-end)))))
     (reality-state-machines state))
    required))

(defun stats-json (state)
  (let ((machine-count (hash-table-count (reality-state-machines state)))
        (sequence-count 0)
        (vector-count 0))
    (maphash
     (lambda (_ machine)
       (declare (ignore _))
       (incf sequence-count (hash-table-count (machine-sequences machine)))
       (dolist (sequence (machine-sequence-list machine))
         (incf vector-count (hash-table-count (sequence-vectors sequence)))))
     (reality-state-machines state))
    (obj "machineCount" machine-count
         "sequenceCount" sequence-count
         "vectorCount" vector-count
         "dimension" (reality-state-dimension state)
         "requiredDimension" (required-dimension state)
         "historySize" (length (reality-state-history state))
         "mappingVersion" (reality-state-mapping-version state)
         "eventBusSubscriptionCount" (event-bus-subscription-count state)
         "latchedEventBitCount" (hash-table-count (reality-state-latched-event-bits state)))))

(defun reset-reality-state (state)
  (maphash (lambda (_ machine)
             (declare (ignore _))
             (dolist (sequence (machine-sequence-list machine))
               (reset-sequence sequence)))
           (reality-state-machines state))
  (setf (reality-state-perceptual-space state)
        (make-perceptual-space (reality-state-dimension state))
        (reality-state-history state) nil
        (reality-state-engine-history state) nil
        (reality-state-isre-history state) nil
        (reality-state-osre-history state) nil
        (reality-state-latched-event-bits state) (make-hash-table :test #'equal)
        (reality-state-step-count state) 0)
  ;; CES coverage deliberately survives the reset.
  ;;
  ;; It answers "has this sequence EVER emitted output" — the help text says
  ;; "never emitted output", and C++ agrees by construction: CesCoverageRegistry
  ;; has a reset() and nothing calls it, so its counters are cumulative for the
  ;; life of the process.
  ;;
  ;; Clearing them here made the metric useless under any harness that resets
  ;; between iterations, which is what the corpus parity loop does. Coverage
  ;; started empty every iteration, only the sequences firing within that
  ;; iteration's steps were recorded, and `ces_unfired_sequences` climbed toward
  ;; the sequence total as the corpus grew — 1661 of 1661 at 372 machines, while
  ;; C++ held at 33. On "Unfired Critical Event Sequences by runtime" that is a
  ;; line rising monotonically with corpus size against a flat one
  ;; (RealityEngine_CI#218).
  ;;
  ;; Reset clears what a run accumulates — step count, histories, the perceptual
  ;; space, per-vector activation. Coverage is not that: it is a record of what
  ;; the corpus has been shown to do, and a reset does not un-show it.
  state)

;; ── CES coverage helpers ────────────────────────────────────────────────────
;; Mirror AI's CesCoverageRegistry / CPP's CesCoverageRegistry: every step
;; bumps a small set of tab-joined hash counters that the /api/metrics route
;; later renders as Prometheus text.

(defun coverage-bump (table key)
  (setf (gethash key table) (1+ (or (gethash key table) 0))))

(defun coverage-key (&rest parts)
  ;; Tab-joined identifier — mirrors the AI/CPP CesCoverageRegistry which
  ;; uses tab-joined hash keys to avoid nested-map allocation on the hot
  ;; path.  `~c` consumes a character, hence `code-char 9` rather than a
  ;; literal string containing a tab.
  (with-output-to-string (out)
    (loop for p in parts
          for first = t then nil
          do (unless first (write-char (code-char 9) out))
             (princ p out))))

(defun record-machine-coverage (state machine transition-json)
  "Walk one machine's transition result and bump the per-vector / per-sequence
counters.  Called once per machine per step from process-perceptual-input."
  (let ((mid (machine-id machine))
        (mname (machine-name machine)))
    (coverage-bump (reality-state-cov-steps state) (coverage-key mid mname))
    (let ((seq-results (jget transition-json "sequenceResults")))
      (when (jobject-p seq-results)
        (dolist (sid (object-keys-sorted seq-results))
          (let ((sr (jget seq-results sid)))
            (let ((matched (jget sr "matchedEvents")))
              (when (jarray-p matched)
                (dolist (vid (jarray-list matched))
                  (when (stringp vid)
                    (coverage-bump (reality-state-cov-matched state)
                                   (coverage-key mid mname sid vid))))))
            (let ((activated (jget sr "activatedEvents")))
              (when (jarray-p activated)
                (dolist (vid (jarray-list activated))
                  (when (stringp vid)
                    (coverage-bump (reality-state-cov-activated state)
                                   (coverage-key mid mname sid vid))))))
            (let ((asserted (jget sr "assertedOutputs")))
              (when (and (jarray-p asserted) (> (length (jarray-list asserted)) 0))
                (setf (gethash (coverage-key mid mname sid)
                               (reality-state-cov-outputs state))
                      (+ (or (gethash (coverage-key mid mname sid)
                                      (reality-state-cov-outputs state)) 0)
                         (length (jarray-list asserted))))))))))))

(defparameter +semantic-audit-capacity+ 1000)

(defun record-semantic-audit (state machine transition-json)
  "Append one re:SequenceObservation per matched step of every sequence in
TRANSITION-JSON.  Determination fields come from the first asserted output;
IRIs are attached at read time by the /api/audit/semantics route."
  (let ((mid (machine-id machine))
        (mname (machine-name machine))
        (seq-results (jget transition-json "sequenceResults"))
        (new nil))
    (when (jobject-p seq-results)
      (dolist (sid (object-keys-sorted seq-results))
        (let* ((sr (jget seq-results sid))
               (matched (jget sr "matchedEvents"))
               (asserted (jget sr "assertedOutputs"))
               (outputs (and (jarray-p asserted) (jarray-list asserted)))
               (first-output (car outputs))
               (completed (and first-output t))
               (out-meta (and first-output (jget first-output "metadata"))))
          (when (jarray-p matched)
            (dolist (vid (jarray-list matched))
              (when (stringp vid)
                (push (obj "at" (now-ms)
                           "machineId" mid
                           "machineName" mname
                           "sequenceId" sid
                           "stepId" vid
                           "completed" (json-bool completed)
                           "determinationId" (if first-output
                                                 (jstring first-output "id" +json-null+)
                                                 +json-null+)
                           "actionCode" (if (jobject-p out-meta)
                                            (jstring out-meta "action" +json-null+)
                                            +json-null+)
                           "ragStatus" (if (jobject-p out-meta)
                                           (jstring out-meta "ragStatusCode" +json-null+)
                                           +json-null+))
                      new)))))))
    (when new
      (let ((combined (append (reality-state-semantic-audit state) (nreverse new))))
        (setf (reality-state-semantic-audit state)
              (if (> (length combined) +semantic-audit-capacity+)
                  (last combined +semantic-audit-capacity+)
                  combined))))))

(defun record-merge-coverage (state operation)
  "Bump paging-decision and deprecated-fire counters from one merge-batch
entry.  Called for each operation in the sorted merge batch.

The two counters move differently now that an operation covers a whole machine.
The paging decision is recorded ONCE per operation, with the joined governance:
its totals drop from per-firing to per-machine-per-step, which is expected and
not a regression — dashboards reading it will step down. The deprecated-fire
counter is bumped once per deprecated CONTRIBUTOR, which keeps it counting
firings exactly as it did before the fold moved."
  (let ((gov (jget operation "governance"))
        (mid (jstring operation "machineId" ""))
        (mname (jstring operation "machineName" "")))
    (when (jobject-p gov)
      (coverage-bump (reality-state-cov-paging state)
                     (coverage-key (jstring gov "ownerTeam" "unrouted")
                                   (jstring gov "processStatus" "unknown")
                                   (jstring gov "ragStatusCode" "unknown")
                                   mid)))
    (dolist (fire (jarray-list (jget operation "%deprecatedFires")))
      (coverage-bump (reality-state-cov-deprecated state)
                     (coverage-key mid mname
                                   (jstring fire "sequenceId" "")
                                   (jstring fire "replacedBy" ""))))))

;; ── Prometheus text exposition ──────────────────────────────────────────────
;; Render the coverage counters in the standard Prometheus text-format so the
;; same scrape config covers AI + CPP + LSP.  Every metric line is stamped
;; with runtime="lsp"; metric names + labels match AI's
;; CesCoverageRegistry.toPrometheusText byte-for-byte.

(defun prom-escape-label (value)
  (with-output-to-string (out)
    (loop for c across (or value "") do
      (cond ((char= c #\\) (write-string "\\\\" out))
            ((char= c #\") (write-string "\\\"" out))
            ((char= c #\Newline) (write-string "\\n" out))
            (t (write-char c out))))))

(defun prom-labels (pairs)
  "Render a label set as {k=\"v\",...}.  Empty set returns empty string."
  (if (null pairs)
      ""
      (with-output-to-string (out)
        (write-char #\{ out)
        (loop for (k . v) in pairs
              for first = t then nil
              do (unless first (write-char #\, out))
                 (write-string k out)
                 (write-string "=\"" out)
                 (write-string (prom-escape-label v) out)
                 (write-char #\" out))
        (write-char #\} out))))

(defun split-coverage-key (key)
  (let (parts (current (make-string-output-stream)))
    (loop for c across key do
      (if (char= c (code-char 9))
          (progn (push (get-output-stream-string current) parts)
                 (setf current (make-string-output-stream)))
          (write-char c current)))
    (push (get-output-stream-string current) parts)
    (nreverse parts)))

(defun prom-metric-totals (state)
  "Compute machine/sequence/vector totals for the gauges."
  (let ((machines (hash-table-count (reality-state-machines state)))
        (sequences 0) (vectors 0))
    (maphash (lambda (_ m)
               (declare (ignore _))
               (dolist (seq (machine-sequence-list m))
                 (incf sequences)
                 (incf vectors (hash-table-count (sequence-vectors seq)))))
             (reality-state-machines state))
    (values machines sequences vectors)))

(defun prom-per-machine-unfired (state)
  "For each machine: (machineId, machineName, sequenceCount, vectorCount,
unfiredSequences, unfiredVectors).  Mirrors AI snapshot.perMachine."
  (let ((rows nil))
    (maphash
     (lambda (_ m)
       (declare (ignore _))
       (let ((mid (machine-id m)) (mname (machine-name m))
             (seq-total 0) (seq-fired 0) (vec-total 0) (vec-fired 0))
         (dolist (seq (machine-sequence-list m))
           (incf seq-total)
           (let ((seq-key (coverage-key mid mname (sequence-id seq))))
             (when (gethash seq-key (reality-state-cov-outputs state))
               (incf seq-fired)))
           (maphash
            (lambda (_ v)
              (declare (ignore _))
              (incf vec-total)
              (let ((vk (coverage-key mid mname (sequence-id seq) (reality-event-id v))))
                (when (or (gethash vk (reality-state-cov-matched state))
                          (gethash vk (reality-state-cov-activated state)))
                  (incf vec-fired))))
            (sequence-vectors seq)))
         (push (list mid mname seq-total vec-total
                     (- seq-total seq-fired) (- vec-total vec-fired)) rows)))
     (reality-state-machines state))
    (nreverse rows)))

(defun prometheus-text-of (state &optional (runtime-tag "lsp"))
  "Render the CES coverage telemetry as Prometheus text-format exposition.
Every metric line — including the global gauges — is stamped with
runtime=runtime-tag so a single scrape target identifies the source runtime."
  (let ((base `(("runtime" . ,runtime-tag))))
    (multiple-value-bind (machines sequences vectors) (prom-metric-totals state)
      (with-output-to-string (out)
        (flet ((emit-help (name help type)
                 (format out "# HELP ~a ~a~%# TYPE ~a ~a~%" name help name type))
               (emit (name labels value)
                 (format out "~a~a ~a~%" name (prom-labels labels) value)))
          (emit-help "ces_machines_total"  "Number of machines loaded into the reality engine." "gauge")
          (emit "ces_machines_total" base machines)
          (emit-help "ces_sequences_total" "Number of sequences across all registered machines." "gauge")
          (emit "ces_sequences_total" base sequences)
          (emit-help "ces_vectors_total"   "Number of event vectors across all registered machines." "gauge")
          (emit "ces_vectors_total" base vectors)

          (emit-help "ces_vector_matched_total"
                     "Number of times a vector matched its input during a transition phase." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 4)
                         (emit "ces_vector_matched_total"
                               (append base
                                       `(("machine" . ,(nth 1 parts))
                                         ("machine_id" . ,(nth 0 parts))
                                         ("sequence" . ,(nth 2 parts))
                                         ("vector" . ,(nth 3 parts))))
                               count))))
                   (reality-state-cov-matched state))

          (emit-help "ces_vector_activated_total"
                     "Number of times a vector was activated as a successor in a transition." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 4)
                         (emit "ces_vector_activated_total"
                               (append base
                                       `(("machine" . ,(nth 1 parts))
                                         ("machine_id" . ,(nth 0 parts))
                                         ("sequence" . ,(nth 2 parts))
                                         ("vector" . ,(nth 3 parts))))
                               count))))
                   (reality-state-cov-activated state))

          (emit-help "ces_sequence_outputs_total"
                     "Number of asserted outputs emitted by a sequence." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 3)
                         (emit "ces_sequence_outputs_total"
                               (append base
                                       `(("machine" . ,(nth 1 parts))
                                         ("machine_id" . ,(nth 0 parts))
                                         ("sequence" . ,(nth 2 parts))))
                               count))))
                   (reality-state-cov-outputs state))

          (emit-help "ces_machine_steps_total"
                     "Number of process_input calls observed for this machine." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 2)
                         (emit "ces_machine_steps_total"
                               (append base
                                       `(("machine" . ,(nth 1 parts))
                                         ("machine_id" . ,(nth 0 parts))))
                               count))))
                   (reality-state-cov-steps state))

          ;; `machine` (name) label is derived from machine_id so the
          ;; dashboard `machine=~"$machine"` filter resolves; the event
          ;; hash keys carry only machine_id because owner_team /
          ;; process_status are the partitioning axes.
          (emit-help "ces_paging_decisions_total"
                     "Number of governance-resolved paging decisions issued by the engine." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 4)
                         (let* ((mid (nth 3 parts))
                                (m (gethash mid (reality-state-machines state)))
                                (mname (if m (machine-name m) "")))
                           (emit "ces_paging_decisions_total"
                                 (append base
                                         `(("owner_team" . ,(nth 0 parts))
                                           ("process_status" . ,(nth 1 parts))
                                           ("rag_status_code" . ,(nth 2 parts))
                                           ("machine_id" . ,mid)
                                           ("machine" . ,mname)))
                                 count)))))
                   (reality-state-cov-paging state))

          (emit-help "ces_deprecated_fires_total"
                     "Number of times a deprecated sequence emitted output." "counter")
          (maphash (lambda (k count)
                     (let ((parts (split-coverage-key k)))
                       (when (= (length parts) 4)
                         (emit "ces_deprecated_fires_total"
                               (append base
                                       `(("machine" . ,(nth 1 parts))
                                         ("machine_id" . ,(nth 0 parts))
                                         ("sequence" . ,(nth 2 parts))
                                         ("replaced_by" . ,(nth 3 parts))))
                               count))))
                   (reality-state-cov-deprecated state))

          ;; One corpus walk per scrape.  The five families below each select a
          ;; different column out of the *same* rows, so this used to call
          ;; prom-per-machine-unfired five times — five maphashes over every
          ;; machine, sequence and vector, and ~3,500 freshly consed
          ;; coverage-key strings per walk.  Bind it once and reuse.
          ;;
          ;; The per-machine label set is identical across all five families
          ;; too, so it is built once here rather than re-appended per family.
          ;; Emission order and label order are unchanged, which matters: this
          ;; payload is byte-compared against the C++ and Scala runtimes
          ;; (RealityEngine_Machines/docs/PE_METRICS_CONTRACT.md), so only the
          ;; traversal count may change, never the text.
          (let* ((unfired-rows (prom-per-machine-unfired state))
                 (unfired-labels (mapcar (lambda (row)
                                           (append base `(("machine" . ,(nth 1 row))
                                                          ("machine_id" . ,(nth 0 row)))))
                                         unfired-rows)))
            (emit-help "ces_unfired_sequences"
                       "Number of sequences in this machine that have never emitted output." "gauge")
            (loop for row in unfired-rows for labels in unfired-labels
                  do (emit "ces_unfired_sequences" labels (nth 4 row)))

            (emit-help "ces_unfired_vectors"
                       "Number of vectors in this machine that have never matched or activated." "gauge")
            (loop for row in unfired-rows for labels in unfired-labels
                  do (emit "ces_unfired_vectors" labels (nth 5 row)))

            (emit-help "ces_machine_sequence_count"
                       "Total sequences declared by this machine." "gauge")
            (loop for row in unfired-rows for labels in unfired-labels
                  do (emit "ces_machine_sequence_count" labels (nth 2 row)))

            (emit-help "ces_machine_vector_count"
                       "Total vectors declared by this machine." "gauge")
            (loop for row in unfired-rows for labels in unfired-labels
                  do (emit "ces_machine_vector_count" labels (nth 3 row)))

            ;; Zero-baseline counter series so dashboards plot rate() /
            ;; by(machine) before any events fire.  Event-keyed series
            ;; above carry sequence / vector sub-labels (distinct Prom
            ;; label sets) so they coexist with these baselines;
            ;; ces_machine_steps_total shares its baseline label shape,
            ;; so we skip machines already seen in cov-steps.
            (let ((seen-steps (make-hash-table :test #'equal)))
              (maphash (lambda (k v)
                         (declare (ignore v))
                         (let ((parts (split-coverage-key k)))
                           (when (= (length parts) 2)
                             (setf (gethash (nth 0 parts) seen-steps) t))))
                       (reality-state-cov-steps state))
              (loop for row in unfired-rows for labels in unfired-labels
                    do (emit "ces_vector_matched_total"   labels 0)
                       (emit "ces_vector_activated_total" labels 0)
                       (emit "ces_sequence_outputs_total" labels 0)
                       (emit "ces_deprecated_fires_total" labels 0)
                       (unless (gethash (nth 0 row) seen-steps)
                         (emit "ces_machine_steps_total"  labels 0)))))

          (let ((uptime-ms (- (now-ms) (reality-state-started-at state))))
            (emit-help "ces_registry_uptime_seconds"
                       "Seconds since the coverage registry was instantiated." "gauge")
            (emit "ces_registry_uptime_seconds" base
                  (format nil "~,3f" (/ uptime-ms 1000.0))))

          ;; re_runtime_* gauges — same shape as AI/CPP for cross-runtime
          ;; vector-space + mapping monitoring.
          (emit-help "re_runtime_dimension"
                     "Current dimension of the shared perceptual space." "gauge")
          (emit "re_runtime_dimension" base (reality-state-dimension state))
          (emit-help "re_runtime_required_dimension"
                     "Max(offset+length) across all registered machine mappings." "gauge")
          (emit "re_runtime_required_dimension" base (required-dimension state))
          (emit-help "re_runtime_mapping_version"
                     "Monotonic version bumped on every put-machine/remove-machine." "gauge")
          (emit "re_runtime_mapping_version" base (reality-state-mapping-version state)))))))

(defun assemble-input-vector (state body)
  ;; Each branch must test key *presence*, not `jarray-p' of a possibly-absent
  ;; value — see `jarray-present-p'.  With plain `jarray-p' the "vector" branch
  ;; matched every request and the sparse/domain forms were unreachable.
  (cond
    ((jarray-present-p body "vector")
     (numbers-from-json (jget body "vector")))
    ((jarray-present-p body "sparseVector")
     (let ((length (reality-state-dimension state)))
       (dolist (entry (jarray-list (jget body "sparseVector")))
         (setf length (max length (1+ (truncate (jnumber entry "index" 0))))))
       (let ((values (make-list length :initial-element 0.0d0)))
         (dolist (entry (jarray-list (jget body "sparseVector")))
           (setf (nth (truncate (jnumber entry "index" 0)) values)
                 (or (jnumber entry "value" 0.0d0) 0.0d0)))
         values)))
    ((jarray-present-p body "domainVectors")
     (let ((length (reality-state-dimension state)))
       (dolist (entry (jarray-list (jget body "domainVectors")))
         (let ((offset (truncate (jnumber entry "offset" 0)))
               (values (numbers-from-json (jget entry "values"))))
           (setf length (max length (+ offset (length values))))))
       (let ((out (make-list length :initial-element 0.0d0)))
         (dolist (entry (jarray-list (jget body "domainVectors")))
           (loop for value in (numbers-from-json (jget entry "values"))
                 for i from (truncate (jnumber entry "offset" 0))
                 do (setf (nth i out) value)))
         out)))
    (t nil)))

(defun sequence-by-id (machine sequence-id)
  (gethash sequence-id (machine-sequences machine)))

(defun parse-comma-values (value)
  (remove nil
          (mapcar (lambda (part)
                    (handler-case
                        (let ((parsed (read-from-string part)))
                          (when (numberp parsed) parsed))
                      (error () nil)))
                  (split-string (or value "") #\,))))

(defun compact-query-p (query)
  (let ((value (and (hash-table-p* query) (gethash "compact" query))))
    (and value (member value '("true" "1") :test #'string=))))

(defparameter +cell-packing-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun allowed-bits-per-element-p (bits)
  (member bits '(1 2 4 8)))

(defun cell-integer (value bits index)
  (let ((max-value (1- (ash 1 bits))))
    (unless (and (realp value)
                 (<= 0 value)
                 (<= value max-value)
                 (= value (truncate value)))
      (error "cell[~a]=~a does not fit in ~a-bit cell (range 0..~a)"
             index value bits max-value))
    (truncate value)))

(defun validate-cell-range (values bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (loop for value in values
        for index from 0
        collect (cell-integer value bits index)))

(defun pack-cells (values bits)
  (let* ((cells (validate-cell-range values bits))
         (bytes (make-array (ceiling (* (length cells) bits) 8)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (if (= bits 8)
        (loop for cell in cells
              for index from 0
              do (setf (aref bytes index) cell))
        (let ((mask (1- (ash 1 bits))))
          (loop for cell in cells
                for index from 0
                for bit-index = (* index bits)
                for byte-index = (floor bit-index 8)
                for shift = (- 8 bits (mod bit-index 8))
                do (setf (aref bytes byte-index)
                         (logior (aref bytes byte-index)
                                 (ash (logand cell mask) shift))))))
    bytes))

(defun unpack-cells (bytes length bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (let ((required (ceiling (* length bits) 8)))
    (when (< (length bytes) required)
      (error "bytes too small for packed cell payload")))
  (if (= bits 8)
      (loop for index below length collect (aref bytes index))
      (let ((mask (1- (ash 1 bits))))
        (loop for index below length
              for bit-index = (* index bits)
              for byte-index = (floor bit-index 8)
              for shift = (- 8 bits (mod bit-index 8))
              collect (logand (ash (aref bytes byte-index) (- shift)) mask)))))

(defun encode-packed-base64 (values bits)
  (let ((bytes (pack-cells values bits)))
    (with-output-to-string (stream)
      (loop for index from 0 below (length bytes) by 3
            for b0 = (aref bytes index)
            for b1-present = (< (1+ index) (length bytes))
            for b2-present = (< (+ index 2) (length bytes))
            for b1 = (if b1-present (aref bytes (1+ index)) 0)
            for b2 = (if b2-present (aref bytes (+ index 2)) 0)
            for triple = (logior (ash b0 16) (ash b1 8) b2)
            do (progn
                 (write-char (char +cell-packing-base64-alphabet+
                                   (ldb (byte 6 18) triple))
                             stream)
                 (write-char (char +cell-packing-base64-alphabet+
                                   (ldb (byte 6 12) triple))
                             stream)
                 (write-char (if b1-present
                                 (char +cell-packing-base64-alphabet+
                                       (ldb (byte 6 6) triple))
                                 #\=)
                             stream)
                 (write-char (if b2-present
                                 (char +cell-packing-base64-alphabet+
                                       (ldb (byte 6 0) triple))
                                 #\=)
                             stream))))))

(defun storage-footprint (length bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (let* ((float64-bytes (* length 8))
         (packed-bytes (ceiling (* length bits) 8))
         (shrink-factor (if (zerop packed-bytes) 0.0d0 (/ float64-bytes packed-bytes))))
    (obj "float64Bytes" float64-bytes
         "packedBytes" packed-bytes
         "shrinkFactor" shrink-factor)))

(defun machine-bits-per-element (machine)
  (let ((bits (and (machine-mapping machine)
                   (mapping-bits-per-element (machine-mapping machine)))))
    (if (allowed-bits-per-element-p bits) bits 8)))

(defun values-packed-json (values bits)
  (let ((out (obj "bitsPerElement" bits
                  "length" (length values))))
    (handler-case
        (setf (jget out "base64") (encode-packed-base64 values bits))
      (error (e)
        (setf (jget out "error") (princ-to-string e))))
    out))

(defun add-packed-merge-values (state merge-batch)
  (dolist (operation merge-batch)
    (let* ((machine (gethash (jstring operation "machineId" "")
                             (reality-state-machines state)))
           (bits (if machine (machine-bits-per-element machine) 8))
           (values (numbers-from-json (jget operation "values"))))
      (setf (jget operation "valuesPacked") (values-packed-json values bits))))
  merge-batch)

(defun storage-footprint-json (state)
  (let ((per-machine nil)
        (width-histogram (obj "1" 0 "2" 0 "4" 0 "8" 0))
        (total-cells 0)
        (total-float64-bytes 0)
        (total-packed-bytes 0))
    (dolist (machine (object-values-sorted (reality-state-machines state)))
      (when (machine-mapping machine)
        (let* ((mapping (machine-mapping machine))
               (bits (machine-bits-per-element machine))
               (cells (+ (region-length (mapping-input mapping))
                         (region-length (mapping-output mapping))))
               (footprint (storage-footprint cells bits)))
          (incf total-cells cells)
          (incf total-float64-bytes (jnumber footprint "float64Bytes" 0))
          (incf total-packed-bytes (jnumber footprint "packedBytes" 0))
          (setf (jget width-histogram (write-to-string bits))
                (1+ (or (jnumber width-histogram (write-to-string bits) 0) 0)))
          (push (obj "machineId" (machine-id machine)
                     "machineName" (machine-name machine)
                     "bitsPerElement" bits
                     "inputCells" (region-length (mapping-input mapping))
                     "outputCells" (region-length (mapping-output mapping))
                     "float64Bytes" (jnumber footprint "float64Bytes" 0)
                     "packedBytes" (jnumber footprint "packedBytes" 0)
                     "shrinkFactor" (jnumber footprint "shrinkFactor" 0))
                per-machine))))
    (obj "machinesRegistered" (hash-table-count (reality-state-machines state))
         "totalCells" total-cells
         "totalFloat64Bytes" total-float64-bytes
         "totalPackedBytes" total-packed-bytes
         "cumulativeShrinkFactor" (if (zerop total-packed-bytes)
                                      0.0d0
                                      (/ total-float64-bytes total-packed-bytes))
         "widthHistogram" width-histogram
         "perMachine" (vectorize (nreverse per-machine)))))

(defun join-governance (machine pending-outputs)
  "The PagingDecision for a folded contribution: the join over its contributors'
severity ranks.

Governance is resolved per contributing sequence against THAT sequence's own
asserted values, exactly as it was when each asserted output was its own merge
operation. It is deliberately NOT resolved against the folded value: a
triggerConfig rule is written for one CES's output and need not match the fold,
and changing the matching semantics is not part of moving the fold. 135 of 1328
corpus machines have an `outputMatches` pattern that maps to more than one RAG
code, so the sequence filter is doing real work — matching on the folded values
alone would be genuinely ambiguous for them.

Selection is by highest `severity-rank`, ties broken by lexicographically
smallest sequence id. `severity-rank` is an ordered chain
(GREEN/absent 0 < AMBER 1 < RED 2 < life-safety 3), so the maximum over it is
the same lattice join the fold vocabulary already defines — deterministic,
symmetric (a maximum over a set does not depend on enumeration order), closed
(it returns a contributor's rank rather than inventing one) and
safety-preserving: a RED-governed firing cannot be hidden by a GREEN one that
folded alongside it. That last property is what SEVERITY arbitration exists to
guarantee, so taking the join preserves its intent rather than approximating it.

The winner's decision travels WHOLE. Composing one from the ragStatusCode of one
rule and the ownerTeam of another would describe no rule that exists, and would
page a team for a status it never declared.

Note the fold and this join can disagree by construction — `meet` can select a
value from a low-severity contributor while a high-severity one also fired. That
is intended: the value is the machine's, the severity is the evidence's."
  (let ((best nil)
        (best-rank -1)
        (best-sequence-id nil))
    (dolist (po pending-outputs)
      (let ((decision (resolve-governance machine
                                          (pending-output-sequence-id po)
                                          (pending-output-values po))))
        (when decision
          (let ((rank (severity-rank (jstring decision "ragStatusCode" nil)))
                (sequence-id (pending-output-sequence-id po)))
            (when (or (> rank best-rank)
                      (and (= rank best-rank)
                           (string< sequence-id best-sequence-id)))
              (setf best decision
                    best-rank rank
                    best-sequence-id sequence-id))))))
    best))

(defun deprecated-contributor-fires (machine pending-outputs)
  "One entry per contributing output whose sequence is deprecated.

Per contributing OUTPUT rather than per distinct sequence: a sequence that
asserted twice in a step bumped `ces_deprecated_fires_total` twice before the
fold moved, and the counter is a count of firings. Collapsing it to the
sequence set would quietly halve it for those machines."
  (let ((fires nil))
    (dolist (po pending-outputs)
      (let ((sequence (sequence-by-id machine (pending-output-sequence-id po))))
        (when (and sequence (sequence-deprecated-at sequence))
          (push (obj "sequenceId" (sequence-id sequence)
                     "replacedBy" (or (sequence-replaced-by sequence) ""))
                fires))))
    (nreverse fires)))

(defun machine-contribution-record (machine pending-outputs)
  "What MACHINE's completed Reality Events amount to, before any fold.

Deliberately separate from the merge operation, because the two answer different
questions and one of them can be absent while the other is not. This record says
WHICH CESs completed; the merge operation says WHAT VALUE the machine presents
because of them. A fold refusal withdraws the value without retracting the
firings, so the event bus is driven from this record and arbitration from the
operation.

Without the split the two contract clauses cannot both hold: a refusing machine
emits no operation, so a bus reading the batch would silently stop delivering
every subscription that machine feeds — and those subscriptions write into the
perceptual space, so meta machines downstream of a refusing producer would go
quiet with nothing in the response to show it."
  (obj "machineId" (machine-id machine)
       "sequenceIds"
       (vectorize (sort (remove-duplicates (mapcar #'pending-output-sequence-id pending-outputs)
                                           :test #'string= :from-end t)
                        #'string<))
       ;; Order-preserved union: first occurrence wins, in the contributor order
       ;; PROCESS-MACHINE-INPUT enumerated. The provenance chain is the evidence
       ;; trail, and reordering it would make the same step render differently
       ;; for no reason.
       "provenance"
       (vectorize (remove-duplicates (loop for po in pending-outputs
                                           append (jarray-list (pending-output-provenance po)))
                                     :test #'equal :from-end t))))

(defun joined-ces-id (sequence-ids)
  "The arbitration key for a folded contribution: SEQUENCE-IDS joined with
commas, no spaces — \"aihr-hw-degradation,aihr-net-fault\".

This is an OPAQUE KEY, not a sequence identifier. Nothing may look a sequence up
by it, and nothing in this runtime does: both SEQUENCE-BY-ID call sites take a
`pending-output-sequence-id`, which is always one real id. Its readers are the
MEAN tie-order key and the `/api/arbitration` serialisation, both of which treat
it as an opaque string (FOLD_PLACEMENT.md A3).

SEQUENCE-IDS arrives sorted and deduplicated from MACHINE-CONTRIBUTION-RECORD,
so the join inherits both properties and is stable across runtimes. A one-element
set renders as the bare id, which is what keeps single-contributor machines
unchanged on the wire; an empty set renders as the empty string, which
ARBITRATION-CONTRIBUTION-JSON already reports as null.

Taking one member instead — the smallest, say — would render identically for the
overwhelming majority and silently discard the rest exactly where the evidence
matters most: a cell contested by a machine that reached its value from several
CESs. C++ joins, so a runtime that picked would disagree with it on precisely
those contributions."
  (format nil "~{~a~^,~}" sequence-ids))

(defun merge-operation-json (machine contribution pending-outputs values)
  "One merge operation: MACHINE's whole contribution to its output region.

The batch used to carry one entry per asserted output, so a machine with seven
completed Reality Events put seven values into the same cell and the per-cell
arbiter resolved contention that belonged to the machine. FallDetection made
that concrete — its seven sequences assert 0,1,2,3,4,4,0 on output index 0, and
the resolved value was 2.0 on C++/LSP and 0.0 on Scala, neither the maximum nor
the minimum, because only a subset fires on any step and the answer depended on
which subset plus each runtime's tie-break. Folding first leaves one determinate
value per cell, so no arbiter rule, tie-break or shard ordering can make the
runtimes differ on it (RealityEngine_CI#154, #158).

VALUES is the folded vector, computed once in PROCESS-MACHINE-INPUT and shared
with the PE-facing `mergedOutputVector`. `sequenceIds` replaces the scalar
`sequenceId`: the machine presents one output and the evidence for it is the set
of CESs that completed."
  (let* ((governance (join-governance machine pending-outputs))
         (fires (deprecated-contributor-fires machine pending-outputs))
         ;; Attached when ANY contributor is deprecated, reported from the
         ;; lexicographically smallest deprecated sequence so the choice is
         ;; deterministic rather than dependent on which one happened to fire
         ;; first.
         (deprecation
           (when fires
             (sequence-deprecation-json
              (sequence-by-id machine
                              (first (sort (mapcar (lambda (fire) (jstring fire "sequenceId" ""))
                                                   fires)
                                           #'string<))))))
         ;; The identity and evidence fields are taken from CONTRIBUTION rather
         ;; than recomputed, so the set the event bus fans out over and the set
         ;; the batch reports are the same set by construction.
         (out (obj "region" (region-json (mapping-output (machine-mapping machine)))
                   "machineId" (jstring contribution "machineId" "")
                   "sequenceIds" (jget contribution "sequenceIds")
                   "values" (vectorize values)
                   "provenance" (jget contribution "provenance"))))
    (when governance
      (setf (jget out "governance") governance))
    (when deprecation
      (setf (jget out "deprecation") deprecation))
    ;; Internal, stripped before the batch is serialised — same `%`-prefixed
    ;; idiom TRANSITION-SEQUENCE uses for `%outputs`. The deprecated-fire
    ;; counter is per firing and the reported `deprecation` block is one record,
    ;; so the counter cannot be driven from the wire shape.
    (setf (jget out "%deprecatedFires") fires)
    out))

(defun sorted-event-bus-writes (writes)
  (sort writes
        (lambda (left right)
          (let ((ls (jstring left "subscriberMachineId" ""))
                (rs (jstring right "subscriberMachineId" ""))
                (lb (or (jnumber left "bitOffset" 0) 0))
                (rb (or (jnumber right "bitOffset" 0) 0))
                (lm (jstring left "producerMachineId" ""))
                (rm (jstring right "producerMachineId" ""))
                (lq (jstring left "producerSequenceId" ""))
                (rq (jstring right "producerSequenceId" "")))
            (cond
              ((not (string= ls rs)) (string< ls rs))
              ((/= lb rb) (< lb rb))
              ((not (string= lm rm)) (string< lm rm))
              (t (string< lq rq)))))))

(defun apply-event-bus (state contributions)
  "Fan the step's CONTRIBUTIONS out to compose/meta subscriptions.

Subscriptions are keyed on (producer machine, producer sequence), so this is the
one consumer of per-sequence identity whose behaviour must be IDENTICAL after
the fold moved rather than merely analogous: it writes into the perceptual
space, and a meta machine that quietly stops firing is a corpus change, not a
reporting difference.

Driven from the contributor records, NOT from the merge batch. The bus asks
which CESs completed, and that is independent of whether the machine's fold
produced a presentable value: a machine whose declared transformation refuses
contributes no merge operation, and a bus reading the batch would then drop
every subscription that machine feeds. Withdrawing a value must not retract the
firings that produced it.

Each record carries the contributing SET, so the lookup iterates it and keys per
element. Every (producer machine, producer sequence) subscription that would
have fired before still fires; reading one arbitrarily chosen member would leave
meta machines observing a producer they never subscribed to. The dedup key
already included the sequence, so it needs no change — two asserted outputs from
one sequence collapsed to a single write before, and the set is deduplicated, so
they still do."
  (let ((writes nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (operation contributions)
      (dolist (sequence-id (jarray-list (jget operation "sequenceIds")))
        (let* ((key (compose-key (jstring operation "machineId" "") sequence-id))
               (subscriptions (gethash key (reality-state-event-bus-subscriptions state))))
          (dolist (subscription subscriptions)
            (let* ((bit (truncate (or (jnumber subscription "bitOffset" 0) 0)))
                   (dedup (format nil "~a|~a|~a|~a"
                                  (jstring subscription "subscriberMachineId" "")
                                  bit
                                  (jstring operation "machineId" "")
                                  sequence-id)))
              (unless (gethash dedup seen)
                (setf (gethash dedup seen) t)
                (push (obj "producerMachineId" (jstring operation "machineId" "")
                           "producerSequenceId" sequence-id
                           "subscriberMachineId" (jstring subscription "subscriberMachineId" "")
                           "bitOffset" bit
                           "value" 1.0d0
                           "provenance" (jget operation "provenance"))
                      writes)
                (setf (gethash bit (reality-state-latched-event-bits state)) t)))))))
    (let ((sorted (sorted-event-bus-writes writes)))
      (dolist (write sorted)
        (let ((bit (truncate (or (jnumber write "bitOffset" 0) 0))))
          (ensure-space-length state (1+ bit))
          (setf (aref (reality-state-perceptual-space state) bit)
                (coerce (or (jnumber write "value" 1.0d0) 1.0d0) 'double-float))))
      sorted)))

(defun apply-latched-event-bits (state)
  (maphash (lambda (bit _)
             (declare (ignore _))
             (ensure-space-length state (1+ bit))
             (setf (aref (reality-state-perceptual-space state) bit) 1.0d0))
           (reality-state-latched-event-bits state)))

(defun active-region-sort-key (r)
  "Canonical sort key for one active region: offset, length, machineId, type."
  (list (jnumber r "offset" 0)
        (jnumber r "length" 0)
        (jstring r "machineId" "")
        (jstring r "type" "")))

(defun active-region-key< (a b)
  "Lexicographic compare of two ACTIVE-REGION-SORT-KEY lists."
  (loop for x in a
        for y in b
        do (cond
             ((and (numberp x) (/= x y)) (return (< x y)))
             ((and (stringp x) (string/= x y)) (return (string< x y))))
        finally (return nil)))

(defun sort-active-regions (regions)
  "Canonical ordering — offset, length, machineId, type (SURFACE_SPEC.md,
\"Active regions\").

The regions are accumulated by walking the machine table, and each runtime walks
its own in its own order: all three reported the same fifteen regions in three
different orders (#197). Because no two agreed byte-for-byte, the clustering in
the universal-vectors stage never found a majority, and every divergence there
reported as \"runtimes split evenly\" whatever the engines had actually done.

`machineId' is in the key so the order is total. offset+length+type alone is
not: two machines may share a region, which is precisely the contended case the
arbiter exists for.

Replaces an NREVERSE, which only undid the push order and carried no meaning."
  (sort (copy-list regions)
        #'active-region-key<
        :key #'active-region-sort-key))

;; include-active-regions defaults to the runtime option rather than to nil.
;;
;; A plain &key defaults to nil, which for an omission flag means "omit" — so
;; every call site that forgot to pass it would silently drop the field from the
;; response, and there are five of them. That is not a failure anything raises:
;; the step is well formed, the key is simply gone, and a consumer walking the
;; response sees a runtime that reports no active regions. Defaulting to the
;; declared option makes the correct behaviour the one you get by saying
;; nothing, so a future call site cannot omit the field by omission.
(defun process-perceptual-input (state input &key override include-machine-results include-perceptual-space
                                                  (include-active-regions (reality-state-include-active-regions-p state))
                                                  compact)
  ;; include-perceptual-space is accepted and ignored: SURFACE_SPEC.md makes
  ;; perceptualSpace unconditional in the push response. Kept in the lambda list
  ;; so existing callers (and RE_INCLUDE_PERCEPTUAL_SPACE) do not become errors.
  (declare (ignore include-perceptual-space))
  (ensure-space-length state (max (reality-state-dimension state) (length input)))
  ;; Seed the space from INPUT in place and zero the tail, rather than
  ;; rebuilding it with append + make-list, which allocated a fresh
  ;; ~17k-cons structure (~271 KB) before any machine ran (#60).
  ;;
  ;; INPUT is sometimes the space itself — /api/simulation/step and the MCP
  ;; step tool feed the current space straight back in. Copying then would be
  ;; a self-overwrite; there is also nothing to copy, since it is already the
  ;; state being seeded.
  (let ((space (reality-state-perceptual-space state)))
    (unless (eq input space)
      (let ((i 0)
            (limit (length space)))
        (map nil (lambda (value)
                   (when (< i limit)
                     (setf (aref space i) (coerce (or value 0) 'double-float))
                     (incf i)))
             input)
        (when (< i limit)
          (fill space 0.0d0 :start i)))))
  (apply-latched-event-bits state)
  ;; ISRE(n) observation point. This is the input space reality event the corpus
  ;; is about to be presented with: every extract-region below reads exactly this
  ;; state, so capturing it here — after the latched bits are re-applied and
  ;; before the first extract — records what the corpus read rather than an
  ;; approximation of it. The arbitration feedback from step n-1 is already
  ;; merged in; the gap between this and the seed is what arbitration did.
  (let ((isre (sparse-trajectory (reality-state-step-count state)
                                 (reality-state-perceptual-space state)))
        (osre nil)
        (machine-results (make-hash-table :test #'equal))
        (merge-batch nil)
        ;; One record per machine that completed at least one Reality Event,
        ;; whether or not its fold produced a value. Kept separate from
        ;; MERGE-BATCH because a refusal removes the machine from the batch and
        ;; must not remove it from the event bus.
        (contributions nil)
        (active-regions nil))
    (dolist (machine (object-values-sorted (reality-state-machines state)))
      (let ((id (machine-id machine)))
       (when (machine-mapping machine)
         (let* ((mapping (machine-mapping machine))
                (machine-input (extract-region (reality-state-perceptual-space state)
                                               (mapping-input mapping)))
                (result (process-machine-input machine machine-input :override override)))
           ;; Coverage tracking — once per machine per step, regardless of
           ;; whether the caller wanted machine-results in the response.
           (record-machine-coverage state machine (transition-result-json result))
           (record-semantic-audit state machine (transition-result-json result))
           (when include-machine-results
             ;; Canonical entry shape, matching C++ and Scala:
             ;;   {machineId, machineName, inputRegion, inputVector,
             ;;    outputRegion, outputVector, transitionResult}
             ;;
             ;; This used to emit the transition result *itself* as the entry, so
             ;; the object carried arbiterMetadata/sequenceResults/machineOutput
             ;; at the top level and had no machineName, no outputRegion and no
             ;; outputVector at all. Two consequences, both silent:
             ;;
             ;;   * `machineResults` had a different key set per runtime on a
             ;;     surface SURFACE_SPEC.md governs as uniform.
             ;;   * This runtime's own PE aggregator reads
             ;;     transitionResult.arbiterMetadata.shouldOutput, outputRegion
             ;;     and outputVector to merge machine outputs into the next
             ;;     InputSpaceVector. None of those keys existed here, so the
             ;;     aggregator matched nothing and merged nothing — machine
             ;;     outputs never fed back into the perceptual space, and the
             ;;     divergence showed up as a cell where two machines' outputs
             ;;     disagreed (cell 3969, AgHarvestReadinessAssessor [3967:3971]
             ;;     vs AGX055 [3959:3971] — RealityEngine_CI corpus parity
             ;;     sweep, 2026-08-19).
             ;; Presenting the machine's output is the Reality Engine's job and
             ;; the last thing it does in the step. Folded here, inside the
             ;; actor, so it runs once the machine's own work has completed and
             ;; nothing concurrent is in flight.
             ;;
             ;; `pending-outputs` is the collection: one entry per completed
             ;; Reality Event. `machine-output` is a single member of it chosen
             ;; by the arbiter, and which member that is has differed per
             ;; runtime — the same corpus presented one runtime's pick to its PE
             ;; and another's to its own (RealityEngine_CI#154). The fold
             ;; replaces the pick; outputVector keeps the pick so nothing that
             ;; reads it today changes.
             ;;
             ;; `merged` below is read straight off the transition result and is
             ;; the SAME value the merge operation carries — one computation
             ;; feeding both the PE-facing field and arbitration. Recomputing it
             ;; here would let the value the PE reads drift from the value the
             ;; corpus resolved, and nothing in a single step would show it.
             (let* ((out-mapping (mapping-output mapping))
                    (machine-out (transition-result-machine-output result))
                    (out-values  (and machine-out (output-vector-vector machine-out)))
                    (transformation (output-merge-name
                                     (machine-output-merge-transformation machine)))
                    (merged (transition-result-merged-output result)))
               (setf (gethash id machine-results)
                     (obj "machineId"        id
                          "machineName"      (or (machine-name machine) "")
                          "inputRegion"      (region-json (mapping-input mapping))
                          "inputEvent"      (vectorize machine-input)
                          "outputRegion"     (if out-mapping
                                                (region-json out-mapping)
                                                +json-null+)
                          "mergedOutputVector" (if merged (vectorize merged) +json-null+)
                          "outputMergeTransformation" transformation
                          "outputVector"     (vectorize (or out-values nil))
                          "transitionResult" (transition-result-json result)))))
           (push (obj "offset" (region-offset (mapping-input mapping))
                      "length" (region-length (mapping-input mapping))
                      "machineId" id
                      "type" "input")
                 active-regions)
           ;; ONE operation per machine per output region. The machine's fold is
           ;; its single contribution: the arbiter now resolves only genuine
           ;; contention — between machines, and between machines and
           ;; integrations — which is what SEVERITY was for. Intra-machine cell
           ;; contention was never contention at all; two CESs share a position
           ;; only where the constructor established that the position is
           ;; identical across every fold configuration, so the arbiter
           ;; resolving it was the defect (RealityEngine_CI#154).
           ;;
           ;; A NIL merged output contributes no VALUE, and that covers two cases
           ;; that mean the same thing for arbitration: the machine completed no
           ;; Reality Event, and the machine's declared transformation refused
           ;; because no chain top was supplied. Neither is a vector of zeros —
           ;; zeros would be a positive claim about every cell in the region.
           ;;
           ;; The contributor record is collected regardless, because a refusal
           ;; withdraws the value without retracting the firings: the event bus
           ;; asks which CESs completed, and every subscription that would have
           ;; fired before must still fire.
           (let ((pending (transition-result-pending-outputs result))
                 (merged (transition-result-merged-output result)))
             (when pending
               (let ((contribution (machine-contribution-record machine pending)))
                 (push contribution contributions)
                 (when merged
                   (push (merge-operation-json machine contribution pending merged)
                         merge-batch)))))))))
    (setf merge-batch (sorted-merge-operations merge-batch)
          ;; Sorted on the same key for the same reason: the bus dedups and
          ;; re-sorts its writes, so this cannot change the result, but a step
          ;; whose intermediate order depends on hash iteration is one whose
          ;; determinism has to be argued rather than read.
          contributions (sorted-merge-operations contributions))
    ;; Paging + deprecation coverage is per-merge-entry; counted after sort
    ;; so the bump order matches AI/CPP exactly.  `%deprecatedFires` is the
    ;; internal per-firing detail the counter needs and the wire must not see,
    ;; so it is stripped in the same pass that consumes it.
    (dolist (op merge-batch)
      (record-merge-coverage state op)
      (remhash "%deprecatedFires" op))
    (when compact
      (add-packed-merge-values state merge-batch))
    ;; GATHER -> RESOLVE -> COMMIT (ARBITER_CONTRACT.md 2).
    ;;
    ;; merge-batch is canonically sorted, which made the previous
    ;; apply-each-in-order loop deterministic — but determinism is not
    ;; resolution. On a contended cell the last operation still won, and a stable
    ;; wrong value reproduces perfectly and reads as correct. Gather turns each
    ;; operation into per-cell contributions carrying the governance severity
    ;; already resolved from triggerConfig (the 4.3.1 join), resolve reduces per
    ;; cell under the declared rule, and commit writes exactly once per cell.
    (let ((by-cell (make-hash-table :test #'eql)))
      (dolist (operation merge-batch)
        (let* ((region (make-region-from-json (jget operation "region")))
               (values (numbers-from-json (jget operation "values")))
               (governance (jget operation "governance"))
               (rag (and governance (jstring governance "ragStatusCode" nil))))
          (loop for i from 0 below (min (region-length region) (length values))
                for cell = (+ (region-offset region) i)
                do (push (make-contribution
                          :cell cell
                          :value (nth i values)
                          :provider "machine"
                          :origin-id (jstring operation "machineId" "")
                          ;; A folded contribution has no single CES, so `cesId`
                          ;; carries the whole set as an opaque comma-joined key
                          ;; (FOLD_PLACEMENT.md A3). One contributor renders as
                          ;; the bare id, so nothing changes for the machines
                          ;; that have one. `outputIndex` is gone, so the id it
                          ;; used to render is pinned at its single remaining
                          ;; value; both fields feed the MEAN tie-order and the
                          ;; arbitration record's attribution, and `origin-id`
                          ;; alone is now unique per cell per machine, so
                          ;; neither can affect a resolution.
                          :ces-id (joined-ces-id (jarray-list (jget operation "sequenceIds")))
                          :output-vector-id "0"
                          :rag-status-code rag)
                         (gethash cell by-cell)))
          (push (obj "offset" (region-offset region)
                     "length" (region-length region)
                     "machineId" (jstring operation "machineId" "")
                     "type" "output")
                active-regions)))
      (multiple-value-bind (resolved records)
          (resolve-all by-cell (reality-state-step-count state))
        ;; OSRE(n) observation point. The corpus's output for this step exists as
        ;; a single-valued vector at exactly one instant: after resolution, as it
        ;; is committed. Recording it in the same loop as the writes is what makes
        ;; the entry and the space agree by construction rather than by a later
        ;; read that could observe a different state.
        (let ((cells nil))
          (dolist (pair resolved)
            (setf (reality-state-perceptual-space state)
                  (merge-region (reality-state-perceptual-space state)
                                (make-region :offset (car pair) :length 1)
                                (list (cdr pair))))
            (unless (zerop (cdr pair))
              (push (obj "index" (car pair) "value" (cdr pair)) cells)))
          (setf osre (obj "stepNumber" (reality-state-step-count state)
                          "length" (length (reality-state-perceptual-space state))
                          "nonZero" (vectorize (sort (nreverse cells) #'<
                                                     :key (lambda (c) (jnumber c "index" 0)))))))
        (setf (reality-state-arbitration state) records)))
    (let* ((event-bus (apply-event-bus state contributions))
           (step-number (reality-state-step-count state))
           ;; No "success" inside the step. Every caller already wraps this as
           ;; (obj "success" t "step" step), so it was a duplicate one level
           ;; down, and neither the C++ nor the Scala step object carries it.
           ;; It made the universal-vector parity signature differ from C++ on
           ;; every event: the harness collects any object holding mergeBatch,
           ;; so the step itself contributed a spurious {"success": true}.
           ;; Key set fixed by SURFACE_SPEC.md, "POST /api/push response shape".
           ;;
           ;; No "inputEvent": this was the only runtime that emitted one. The
           ;; Perception Engine assembled that vector and sent it, so echoing it
           ;; back is redundant, and C++'s SimulationStep has no step-level
           ;; input vector to echo.
           ;;
           ;; machineResults is omitted under compact rather than emitted empty.
           ;; An empty object is not the same as an absent key to a consumer
           ;; walking the response, and the other two omit it.
           (step (obj "stepNumber" step-number
                     "timestamp" (now-ms)
                     "mergeBatch" (vectorize merge-batch)
                     "eventBus" (vectorize event-bus))))
      (setf (reality-state-step-count state) (1+ step-number))
      ;; activeRegions is omitted, not emptied, when not requested
      ;; (SURFACE_SPEC.md, "Requesting less than the full step"). An empty array
      ;; is the claim that no regions were active, which is a different
      ;; statement from "not asked for" — and the parity stage compares key
      ;; sets, so emptying it would read as agreement between a runtime with
      ;; nothing to report and one that was never asked.
      (when include-active-regions
        (setf (jget step "activeRegions") (vectorize (sort-active-regions active-regions))))
      (when include-machine-results
        (setf (jget step "machineResults") machine-results))
      ;; Always present, compact or not. This was gated on
      ;; include-perceptual-space, so a compact push returned no Reality Event
      ;; at all — the engine computed the right answer and did not report it,
      ;; which is what the cross-runtime parity stage read as divergence
      ;; (RealityEngine_Scala#43).
      (setf (jget step "perceptualSpace") (perceptual-space-snapshot (reality-state-perceptual-space state))
            (jget step "perceptualSpaceIsDebugProjection") t)
      (record-trajectory state isre osre)
      (record-history state step)
      step)))

(defun active-vectors-json (state)
  (let (rows)
    (dolist (machine (object-values-sorted (reality-state-machines state)))
      (dolist (sequence (machine-sequence-list machine))
        (dolist (vector (object-values-sorted (sequence-vectors sequence)))
          (when (reality-event-active-p vector)
            (push (obj "machineId" (machine-id machine)
                       "sequenceId" (sequence-id sequence)
                       "vector" (active-vector-json vector))
                  rows)))))
    (vectorize (nreverse rows))))

(defun active-vector-json (vector)
  (obj "elements" (vectorize (mapcar #'vector-element-json (reality-event-elements vector)))
       "id" (reality-event-id vector)
       "isActive" (json-bool (reality-event-active-p vector))
       "isInitial" (json-bool (reality-event-initial-p vector))
       "matchAlgorithm" (reality-event-match-algorithm vector)
       "metadata" (or (reality-event-metadata vector) (obj))
       "nextEventIds" (vectorize (reality-event-next-ids vector))
       "outputEvents" (vectorize
                        (mapcar (lambda (output)
                                  (obj "id" (output-vector-id output)
                                       "metadata" (or (output-vector-metadata output) (obj))
                                       "vector" (vectorize (output-vector-vector output))))
                                (reality-event-output-vectors vector)))
       "state" (if (reality-event-active-p vector) "active" "inactive")
       "wasJustMatched" (json-bool (reality-event-just-matched-p vector))))

(defun semantic-bus-registry-path (machine-dir)
  (let ((explicit (env "SEMANTIC_BUS_REGISTRY" nil)))
    (when (and explicit (> (length explicit) 0))
      (return-from semantic-bus-registry-path explicit)))
  (let ((cursor (uiop:ensure-directory-pathname machine-dir)))
    (loop repeat 6
          while cursor
          for candidate = (merge-pathnames "domains/semantic-bus-registry.json" cursor)
          when (probe-file candidate)
            do (return (namestring candidate))
          do (let ((parent (uiop:pathname-parent-directory-pathname cursor)))
               (setf cursor (and parent (not (equal parent cursor)) parent)))
          finally (return (namestring (merge-pathnames "../domains/semantic-bus-registry.json"
                                                        (uiop:ensure-directory-pathname machine-dir)))))))

(defun semantic-bus-registry-json (state)
  (let* ((path (semantic-bus-registry-path (reality-state-machine-dir state)))
         (registry (parse-json (safe-read-file path))))
    (unless (jarray-present-p registry "semanticBuses")
      (error "invalid registry shape at ~a" path))
    registry))

;; OWL semantics manifest (RealityEngine_Machines semantics/abox-manifest.json):
;; per-machine semantic identity — ABox IRI + content hash — for the
;; cross-engine semantic-equivalence surface (roadmap milestone M4).
(defun semantics-manifest-path (machine-dir)
  (let ((explicit (env "SEMANTICS_MANIFEST" nil)))
    (when (and explicit (> (length explicit) 0))
      (return-from semantics-manifest-path explicit)))
  (let ((cursor (uiop:ensure-directory-pathname machine-dir)))
    (loop repeat 6
          while cursor
          for candidate = (merge-pathnames "semantics/abox-manifest.json" cursor)
          when (probe-file candidate)
            do (return (namestring candidate))
          do (let ((parent (uiop:pathname-parent-directory-pathname cursor)))
               (setf cursor (and parent (not (equal parent cursor)) parent)))
          finally (return (namestring (merge-pathnames "../semantics/abox-manifest.json"
                                                        (uiop:ensure-directory-pathname machine-dir)))))))

(defun semantics-manifest-json (state)
  (let* ((path (semantics-manifest-path (reality-state-machine-dir state)))
         (manifest (parse-json (safe-read-file path))))
    (unless (jobject-p (jget manifest "machines"))
      (error "invalid semantics manifest shape at ~a" path))
    manifest))

(defun sanitize-iri-local (local)
  "Restrict an ABox IRI local name to the generator's PN_LOCAL subset
(scripts/generate-owl.py sanitize()) so runtime IRIs match the corpus."
  (if (or (null local) (zerop (length local)))
      "unnamed"
      (map 'string
           (lambda (c)
             (if (or (alphanumericp c) (char= c #\_) (char= c #\-)) c #\_))
           local)))

(defun semantic-audit-json (state limit)
  "Records for GET /api/audit/semantics — the newest LIMIT observations with
IRIs joined from the corpus semantics manifest."
  (let* ((bases (make-hash-table :test #'equal))
         (manifest (ignore-errors (semantics-manifest-json state)))
         (ontology-known (and manifest t))
         (all (reality-state-semantic-audit state))
         (bounded (max 0 (min limit +semantic-audit-capacity+)))
         (records (if (> (length all) bounded) (last all bounded) all)))
    (declare (ignore ontology-known))
    (when manifest
      (maphash (lambda (key entry)
                 (declare (ignore key))
                 (let ((iri (jstring entry "iri" nil))
                       (name (jstring entry "name" nil)))
                   (when (and iri name)
                     (let ((hash (position #\# iri)))
                       (when hash
                         (setf (gethash name bases) (subseq iri 0 hash)))))))
               (jget manifest "machines")))
    (obj "records"
         (vectorize
          (mapcar
           (lambda (r)
             (let* ((mname (jstring r "machineName" ""))
                    (base (gethash mname bases))
                    (det (jstring r "determinationId" nil)))
               (flet ((iri (prefix local)
                        (if (and base local)
                            (format nil "~a#~a-~a" base prefix (sanitize-iri-local local))
                            +json-null+)))
                 (obj "type" "re:SequenceObservation"
                      "at" (jnumber r "at" 0)
                      "machineId" (jstring r "machineId" "")
                      "machineName" mname
                      "machineIri" (if base (format nil "~a#machine" base) +json-null+)
                      "sequenceId" (jstring r "sequenceId" "")
                      "sequenceIri" (iri "seq" (jstring r "sequenceId" nil))
                      "stepId" (jstring r "stepId" "")
                      "stepIri" (iri "step" (jstring r "stepId" nil))
                      "completed" (jget r "completed" +json-false+)
                      "determinationIri" (if det (iri "out" det) +json-null+)
                      "actionCode" (jget r "actionCode" +json-null+)
                      "ragStatus" (jget r "ragStatus" +json-null+)))))
           records))
         "count" (length records))))

(defun find-semantics-entry (manifest name)
  "Return (values key entry) for the manifest machine named NAME, else nil."
  (maphash (lambda (key entry)
             (when (and (jobject-p entry) (string= (jstring entry "name" "") name))
               (return-from find-semantics-entry (values key entry))))
           (jget manifest "machines"))
  nil)

(defun percent-decode (s)
  "Percent-decode a path parameter (machine names contain spaces)."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length s))
          do (let ((c (char s i)))
               (if (and (char= c #\%) (< (+ i 2) (length s))
                        (digit-char-p (char s (+ i 1)) 16)
                        (digit-char-p (char s (+ i 2)) 16))
                   (progn
                     (write-char (code-char (parse-integer s :start (+ i 1) :end (+ i 3) :radix 16)) out)
                     (incf i 3))
                   (progn (write-char c out) (incf i)))))))

(defun find-semantic-bus-json (registry id)
  (find id
        (jarray-list (jget registry "semanticBuses"))
        :test #'string=
        :key (lambda (bus) (jstring bus "id" ""))))

(defun arbitration-contribution-json (c)
  "One contributor as GET /api/arbitration reports it.

`cesId` is emitted verbatim and is an OPAQUE KEY: since the fold moved into the
machine's atomic step a machine contributes the fold of every CES that
completed, so the field carries their comma-joined, sorted, deduplicated set —
`\"a,b\"` — and reduces to a bare id only when one CES contributed
(FOLD_PLACEMENT.md A3). A reader wanting the individual CESs must split on the
comma; it must not pass this value anywhere a sequence id is expected. Machines
with a single contributing sequence, which is nearly all of them, are unchanged
on this surface."
  (obj "provider" (contribution-provider c)
       "determinism" (determinism-name (determinism-of (contribution-provider c)))
       "originId" (contribution-origin-id c)
       "cesId" (if (string= (contribution-ces-id c) "") :null (contribution-ces-id c))
       "outputVectorId" (if (string= (contribution-output-vector-id c) "")
                            :null (contribution-output-vector-id c))
       "ragStatusCode" (or (contribution-rag-status-code c) :null)
       "value" (contribution-value c)))

(defun arbitration-json (state)
  "GET /api/arbitration — records from the most recent step."
  (let ((records (reality-state-arbitration state)))
    (obj "registryEntries" (arbitration-registry-size)
         "registrySource" (or *arbitration-source* :null)
         "shards" (arbiter-shards)
         "count" (length records)
         "records"
         (apply #'arr
                (mapcar (lambda (r)
                          (obj "instant" (arbitration-record-instant r)
                               "cell" (arbitration-record-cell r)
                               "rule" (arbitration-record-rule r)
                               "resolved" (arbitration-record-resolved r)
                               "contributors"
                               (apply #'arr (mapcar #'arbitration-contribution-json
                                                    (arbitration-record-contributors r)))
                               "suppressed"
                               (apply #'arr (mapcar #'arbitration-contribution-json
                                                    (arbitration-record-suppressed r)))))
                        records)))))

(defun machine-graph-json (state)
  (let (nodes edges)
    ;; nodes are collected with push and the function ends with (nreverse
    ;; nodes), so iterate in canonical order — reversing here too would undo it.
    (dolist (machine (machines-in-canonical-order (reality-state-machines state)))
      (let ((id (machine-id machine)))
       (push (obj "id" id
                  "name" (machine-name machine)
                  "description" (machine-description machine)
                  "inputMapping" (if (machine-mapping machine)
                                      (region-json (mapping-input (machine-mapping machine)))
                                      +json-null+)
                  "outputMapping" (if (machine-mapping machine)
                                       (region-json (mapping-output (machine-mapping machine)))
                                       +json-null+)
                  "metadata" (or (machine-metadata machine) (obj)))
             nodes)))
    ;; Edges: source output region overlaps target input region.
    ;; Mirrors Scala PerceptualSpaceRuntime.rebuildEdgeCache — interval
    ;; intersection test, one directed edge per overlapping (source, target) pair.
    ;; Canonical order here too — edges are pushed, so iterate reversed to end
    ;; up ordered, and the (source, target) pair order becomes deterministic
    ;; across runtimes rather than following hash iteration.
    ;; Same here — edges are pushed and nreversed at the end.
    (let ((machine-list (machines-in-canonical-order (reality-state-machines state))))
      (dolist (source machine-list)
        (when (machine-mapping source)
          (dolist (target machine-list)
            (when (and (machine-mapping target)
                       (not (string= (machine-id source) (machine-id target))))
              (let* ((src-out (mapping-output (machine-mapping source)))
                     (tgt-in  (mapping-input  (machine-mapping target)))
                     (src-end (+ (region-offset src-out) (region-length src-out)))
                     (tgt-end (+ (region-offset tgt-in)  (region-length tgt-in))))
                (unless (or (<= src-end (region-offset tgt-in))
                            (>= (region-offset src-out) tgt-end))
                  (push (obj "source"       (machine-id source)
                             "target"       (machine-id target)
                             "sourceRegion" (region-json src-out)
                             "targetRegion" (region-json tgt-in)
                             "overlap"      t)
                        edges))))))))
    ;; perceptualSpaceDimension mirrors C++ (Json::Object{..., {"perceptualSpaceDimension",
    ;; space.dimension()}}), which is the canonical shape.  LSP omitted it, and
    ;; that field was the entire remaining difference on GET /api/machine-graph
    ;; once the solidus fix landed (RealityEngine_CI#91).
    (obj "nodes" (vectorize (nreverse nodes))
         "edges" (vectorize (nreverse edges))
         "perceptualSpaceDimension" (reality-state-dimension state))))

(defun transitions-inhibited-control (state)
  "The transitionsInhibited control, in the shape SURFACE_SPEC declares."
  (let ((value (obj)))
    (maphash (lambda (id machine)
               (setf (jget value id) (json-bool (machine-transitions-inhibited machine))))
             (reality-state-machines state))
    (obj "name" "transitionsInhibited"
         "scope" "machine"
         "value" value
         "default" (json-bool nil)
         "mutable" (json-bool t))))

(defun set-transitions-inhibited (state machine-id value)
  "Set the control for one machine, or for every machine when MACHINE-ID is NIL.

   Returns :MISSING when a named machine does not exist, so the route can answer
   404 rather than accepting a write that lands nowhere."
  (if machine-id
      (let ((machine (gethash machine-id (reality-state-machines state))))
        (if (null machine)
            :missing
            (progn (setf (machine-transitions-inhibited machine) value)
                   (transitions-inhibited-control state))))
      (progn
        (maphash (lambda (id machine)
                   (declare (ignore id))
                   (setf (machine-transitions-inhibited machine) value))
                 (reality-state-machines state))
        (transitions-inhibited-control state))))

(defun reality-routes (actor)
  (list
   (make-route "GET" "/" (lambda (_ body query)
                           (declare (ignore _ body query))
                           (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
   (make-route "GET" "/api" (lambda (_ body query)
                              (declare (ignore _ body query))
                              (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
   (make-route "GET" "/api/health" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response (obj "status" "healthy"))))
   (make-route "GET" "/api/config" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response
                                     (actor-ask actor
                                                (lambda (state)
                                                  (obj "eventDimension" (reality-state-dimension state)
                                                       "matchThreshold" 0.5d0
                                                       "qdrantUrl" (reality-state-qdrant-url state)
                                                       "collectionName" (reality-state-collection-name state)))))))
   (make-route "PUT" "/api/config/dimension" (lambda (_ body query)
                                             (declare (ignore _ body))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let ((dimension (parse-integer (or (gethash "dimension" query)
                                                                                               (write-to-string (reality-state-dimension state)))
                                                                                           :junk-allowed t)))
                                                             (setf (reality-state-dimension state) dimension)
                                                             (obj "success" t "dimension" dimension)))))))
   (make-route "POST" "/api/vectors/search" (lambda (_ body query)
                                             (declare (ignore _ query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let* ((query-vector (numbers-from-json (jget body "vector")))
                                                                  (limit (truncate (or (jnumber body "limit" 10) 10)))
                                                                  (threshold (jnumber body "threshold" nil))
                                                                  (rows nil))
                                                             (maphash
                                                              (lambda (_ vector-json)
                                                                (declare (ignore _))
                                                                (when (< (length rows) limit)
                                                                  (let ((score (cosine query-vector (numbers-from-json (or (jget vector-json "vector")
                                                                                                                           (jget vector-json "values")
                                                                                                                           (arr))))))
                                                                    (when (or (null threshold) (>= score threshold))
                                                                      (push (obj "vector" vector-json "score" score) rows)))))
                                                              (reality-state-vector-store state))
                                                             (obj "results" (vectorize (nreverse rows)))))))))
   (make-route "POST" "/api/vectors" (lambda (_ body query)
                                      (declare (ignore _ query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (let ((id (or (jstring body "id" nil) (make-id "vector"))))
                                                      (setf (jget body "id") id
                                                            (gethash id (reality-state-vector-store state)) body)
                                                      (obj "success" t "vector" body)))))))
   (make-route "GET" "/api/vectors/:id" (lambda (params body query)
                                        (declare (ignore body query))
                                        (json-response (obj "message" "Vector retrieval endpoint" "id" (gethash "id" params)))))
   (make-route "DELETE" "/api/vectors/:id" (lambda (params body query)
                                           (declare (ignore body query))
                                           (json-response
                                            (actor-ask actor
                                                       (lambda (state)
                                                         (remhash (gethash "id" params) (reality-state-vector-store state))
                                                         (obj "success" t "id" (gethash "id" params)))))))
   (make-route "POST" "/api/sequences/persist" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (json-response (obj "success" t))))
   (make-route "GET" "/api/sequences" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (obj "sequences" (vectorize
                                                                       (mapcar (lambda (s) (sequence-json s :full t))
                                                                               (object-values (reality-state-sequences state))))))))))
   (make-route "POST" "/api/sequences" (lambda (_ body query)
                                        (declare (ignore _ query))
                                        (json-response
                                         (actor-ask actor
                                                    (lambda (state)
                                                      (let ((sequence (parse-sequence body)))
                                                        (setf (gethash (sequence-id sequence) (reality-state-sequences state)) sequence)
                                                        (obj "success" t "sequence" (sequence-json sequence :full t))))))))
   (make-route "GET" "/api/sequences/:id" (lambda (params body query)
                                           (declare (ignore body query))
                                           (let ((result (actor-ask actor
                                                                    (lambda (state)
                                                                      (gethash (gethash "id" params) (reality-state-sequences state))))))
                                             (if result
                                                 (json-response (obj "sequence" (sequence-json result :full t)))
                                                 (error-response "Sequence not found" 404)))))
   (make-route "POST" "/api/engine/reset" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor (lambda (state) (reset-reality-state state) (obj "success" t))))))
   (make-route "GET" "/api/engine/stats" (lambda (_ body query)
                                          (declare (ignore _ body query))
                                          (json-response (actor-ask actor (lambda (state) (obj "stats" (stats-json state)))))))
   (make-route "GET" "/api/runtime/metrics" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (json-response (actor-ask actor (lambda (state) (obj "stats" (stats-json state) "domainWorkerPool" (obj "semantics" "actor-mailbox")))))))
   (make-route "GET" "/api/runtime/vector-space" (lambda (_ body query)
                                                  (declare (ignore _ body query))
                                                  (json-response
                                                   (actor-ask actor
                                                              (lambda (state)
                                                                (obj "dimension" (reality-state-dimension state)
                                                                     "requiredDimension" (required-dimension state)
                                                                     "encoding" "dense-float64-clamped-0-1"
                                                                     "mappingVersion" (reality-state-mapping-version state)
                                                                     "eventBusSubscriptionCount" (event-bus-subscription-count state))))))
   (make-route "GET" "/api/runtime/storage-footprint" (lambda (_ body query)
                                                       (declare (ignore _ body query))
                                                       (json-response
                                                        (actor-ask actor
                                                                   (lambda (state)
                                                                     (storage-footprint-json state))))))
   (make-route "GET" "/api/runtime/options" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (obj "historyLimit" (reality-state-history-limit state)
                                                                "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))
                                                                "includeActiveRegions" (json-bool (reality-state-include-active-regions-p state))))))))
   (make-route "PATCH" "/api/runtime/options" (lambda (_ body query)
                                               (declare (ignore _ query))
                                               (json-response
                                                (actor-ask actor
                                                           (lambda (state)
                                                             (when (jnumber body "historyLimit" nil)
                                                               (setf (reality-state-history-limit state) (truncate (jnumber body "historyLimit"))))
                                                             (when (not (eq (jget body "includeMachineResults" :missing) :missing))
                                                               (setf (reality-state-include-machine-results-p state) (jbool body "includeMachineResults" t)))
                                                             (when (not (eq (jget body "includePerceptualSpace" :missing) :missing))
                                                               (setf (reality-state-include-perceptual-space-p state) (jbool body "includePerceptualSpace" t)))
                                                             (when (not (eq (jget body "includeActiveRegions" :missing) :missing))
                                                               (setf (reality-state-include-active-regions-p state) (jbool body "includeActiveRegions" t)))
                                                             (obj "historyLimit" (reality-state-history-limit state)
                                                                  "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                  "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))
                                                                "includeActiveRegions" (json-bool (reality-state-include-active-regions-p state))))))))
   (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor (lambda (state) (obj "activeEvents" (active-vectors-json state)))))))
   (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                            (declare (ignore _ body))
                                            (let ((limit (parse-integer (or (gethash "limit" query) "0") :junk-allowed t)))
                                              (json-response
                                               (actor-ask actor
                                                          (lambda (state)
                                                            (obj "history" (vectorize (if (and limit (> limit 0))
                                                                                          (subseq (reality-state-engine-history state)
                                                                                                  0 (min limit (length (reality-state-engine-history state))))
                                                                                          (reality-state-engine-history state))))))))))
   ;; Trajectory histories — SURFACE_SPEC.md, "Trajectory histories".
   (make-route "GET" "/api/engine/osre-history" (lambda (_ body query)
                                                  (declare (ignore _ body))
                                                  (json-response
                                                   (actor-ask actor
                                                              (lambda (state)
                                                                (obj "history" (trajectory-window
                                                                                (reality-state-osre-history state)
                                                                                (or (parse-integer (or (gethash "from" query) "0") :junk-allowed t) 0)
                                                                                (parse-integer (or (gethash "limit" query) "0") :junk-allowed t))))))))
   (make-route "GET" "/api/engine/isre-history" (lambda (_ body query)
                                                  (declare (ignore _ body))
                                                  (json-response
                                                   (actor-ask actor
                                                              (lambda (state)
                                                                (obj "history" (trajectory-window
                                                                                (reality-state-isre-history state)
                                                                                (or (parse-integer (or (gethash "from" query) "0") :junk-allowed t) 0)
                                                                                (parse-integer (or (gethash "limit" query) "0") :junk-allowed t))))))))
   (make-route "POST" "/api/engine/process" (lambda (_ body query)
                                             (declare (ignore _ query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let ((input (numbers-from-json (jget body "vector")))
                                                                 (outputs nil))
                                                             (maphash
                                                              (lambda (_ machine)
                                                                (declare (ignore _))
                                                                (let ((result (process-machine-input machine input)))
                                                                  (when (transition-result-machine-output result)
                                                                    (push (output-vector-json (transition-result-machine-output result)) outputs))))
                                                              (reality-state-machines state))
                                                             (let ((result (obj "inputEvent" (vectorize input)
                                                                                "timestamp" (now-ms)
                                                                                "outputs" (vectorize (nreverse outputs)))))
                                                               (record-engine-history state (obj "type" "engine-process" "result" result))
                                                               (obj "result" result))))))))
   (make-route "GET" "/api/machines" (lambda (_ body query)
                                      (declare (ignore _ body))
                                      (let ((summary-p (and (hash-table-p* query)
                                                            (member (gethash "summary" query)
                                                                    '("true" "1") :test #'string=))))
                                        (json-response
                                         (actor-ask actor
                                                    (lambda (state)
                                                      (obj "machines" (vectorize
                                                                       (mapcar (if summary-p
                                                                                   #'machine-summary-json
                                                                                   #'machine-json)
                                                                               (machines-in-canonical-order (reality-state-machines state)))))))))))
   (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                          (declare (ignore body query))
                                          ;; The LET closed with an empty body here, which put the
                                          ;; IF outside it referencing a free MACHINE — so every
                                          ;; read of an existing machine answered HTTP 500 "The
                                          ;; variable MACHINE is unbound" instead of the machine
                                          ;; (#42).
                                          (let ((machine (actor-ask actor (lambda (state) (gethash (gethash "id" params) (reality-state-machines state))))))
                                            (if machine
                                                (json-response (obj "machine" (machine-json machine :full t)))
                                                (error-response "Machine not found" 404))))))
   (make-route "POST" "/api/machines" (lambda (_ body query)
                                       (declare (ignore _ query))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (let ((machine (machine-from-json body)))
                                                       (put-machine state machine)
                                                       (obj "success" t "machine" (machine-json machine :full t))))))))
   (make-route "PUT" "/api/machines/:id" (lambda (params body query)
                                          (declare (ignore query))
                                          (json-response
                                           (actor-ask actor
                                                      (lambda (state)
                                                        (let ((machine (machine-from-json body (gethash "id" params))))
                                                          (put-machine state machine)
                                                          (obj "success" t "machine" (machine-json machine :full t)))))))
   (make-route "DELETE" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore body query))
                                             (json-response
                                              (actor-ask actor
                                                           (lambda (state)
                                                             (unregister-compose-subscriptions state (gethash "id" params))
                                                             (let ((removed (remhash (gethash "id" params) (reality-state-machines state))))
                                                               (when removed
                                                                 (incf (reality-state-mapping-version state)))
                                                               (obj "success" (json-bool removed))))))))
   (make-route "POST" "/api/machines/:id/process" (lambda (params body query)
                                                   (declare (ignore query))
                                                   (let ((result (actor-ask actor
                                                                            (lambda (state)
                                                                              (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                                (when machine
                                                                                  (transition-result-json
                                                                                   (process-machine-input machine (numbers-from-json (jget body "inputEvent"))))))))))
                                                     (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/:id/whatif" (lambda (params body query)
                                                  (declare (ignore query))
                                                  (let ((result (actor-ask actor
                                                                           (lambda (state)
                                                                             (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                               (when machine
                                                                                 (transition-result-json
                                                                                  (process-machine-input
                                                                                   (machine-from-json (machine-json machine :full t))
                                                                                   (numbers-from-json (jget body "inputEvent"))))))))))
                                                    (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/:id/process-universal" (lambda (params body query)
                                                             (declare (ignore query))
                                                             (let ((result (actor-ask actor
                                                                                      (lambda (state)
                                                                                        (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                                          (when (and machine (machine-mapping machine))
                                                                                            (let ((input (extract-region (numbers-from-json (jget body "universalInputSpace"))
                                                                                                                         (mapping-input (machine-mapping machine)))))
                                                                                              (transition-result-json (process-machine-input machine input)))))))))
                                                               (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/process-universal/all" (lambda (_ body query)
                                                             (declare (ignore _ query))
                                                             (json-response
                                                              (actor-ask actor
                                                                         (lambda (state)
                                                                           (let ((universal (numbers-from-json (jget body "universalInputSpace")))
                                                                                 (results (make-hash-table :test #'equal)))
                                                                             (maphash
                                                                              (lambda (id machine)
                                                                                (when (machine-mapping machine)
                                                                                  (setf (gethash id results)
                                                                                        (transition-result-json
                                                                                         (process-machine-input machine
                                                                                                                (extract-region universal (mapping-input (machine-mapping machine))))))))
                                                                              (reality-state-machines state))
                                                                             (obj "results" results)))))))
   (make-route "GET" "/api/machines/json/list" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (json-response
                                                 (actor-ask actor
                                                            (lambda (state)
                                                              (obj "machines" (vectorize (machine-json-list-rows (reality-state-machine-dir state)))))))))
   (make-route "GET" "/api/machines/json/:name" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (handler-case
                                                     (json-response
                                                      (actor-ask actor
                                                                 (lambda (state)
                                                                   (let* ((name (gethash "name" params))
                                                                          (path (resolve-machine-json-path (reality-state-machine-dir state) name))
                                                                          (machine (load-machine-from-file path)))
                                                                     (put-machine state machine)
                                                                     (obj "success" t "machine" (machine-json machine :full t) "message" "Machine loaded successfully")))))
                                                   (error (condition) (error-response (princ-to-string condition) 404)))))
   (make-route "POST" "/api/machines/json/import" (lambda (_ body query)
                                                   (declare (ignore _ query))
                                                   (json-response
                                                    (actor-ask actor
                                                               (lambda (state)
                                                                 (let ((machine (machine-from-json body)))
                                                                   (put-machine state machine)
                                                                   (obj "success" t "machine" (machine-json machine :full t))))))))
   (make-route "GET" "/api/machines/:id/export" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (let ((machine (actor-ask actor (lambda (state) (gethash (gethash "id" params) (reality-state-machines state)))))))
                                                   (if machine
                                                       (json-response (obj "version" "1.0.0" "machine" (machine-json machine :full t)))
                                                       (error-response "Machine not found" 404)))))
   (make-route "GET" "/api/machine-graph" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor #'machine-graph-json))))
   (make-route "POST" "/api/perceptual-simulation/step" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor (lambda (state)
                                                                                           (let ((step (process-perceptual-input state (reality-state-perceptual-space state)
                                                                                                                                  :include-machine-results t
                                                                                                                                  :include-perceptual-space t)))
                                                                                             (obj "success" t "step" step)))))))
   (make-route "POST" "/api/perceptual-simulation/reset" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (json-response (actor-ask actor (lambda (state) (reset-reality-state state) (obj "success" t))))))
   (make-route "GET" "/api/perceptual-simulation/state" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor (lambda (state) (obj "running" +json-false+
                                                                                                             "dimension" (reality-state-dimension state)
                                                                                                             "perceptualSpace" (perceptual-space-snapshot (reality-state-perceptual-space state))))))))
   (make-route "GET" "/api/perceptual-simulation/history" (lambda (_ body query)
                                                           (declare (ignore _ body query))
                                                           (json-response (actor-ask actor (lambda (state) (obj "history" (vectorize (reality-state-history state))))))))
   (make-route "POST" "/api/perception/diagnostic" (lambda (_ body query)
                                                    (declare (ignore _ query))
                                                    (json-response (obj "universalInputSpace" (jget body "universalInputSpace")
                                                                        "resolvedInputs" (obj)))))
   (make-route "POST" "/api/perceive" (lambda (_ body query)
                                       (declare (ignore _))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (let ((input (assemble-input-vector state body)))
                                                       (if input
                                                           (process-perceptual-input
                                                            state input
                                                            ;; Only an explicit override overrides.
                                                            ;; `matchAlgorithm' reports the algorithm in
                                                            ;; effect; it is not an instruction to replace
                                                            ;; every element's declared comparatorType, and
                                                            ;; reading it as one made the corpus's per-element
                                                            ;; matching semantics unreachable in normal
                                                            ;; operation (RealityEngine_CI#201).
                                                            :override (jstring body "matchAlgorithmOverride" nil)
                                                            :include-machine-results (jbool body "includeMachineResults"
                                                                                            (if (jbool body "compact" nil)
                                                                                                nil
                                                                                                (reality-state-include-machine-results-p state)))
                                                            :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                             (reality-state-include-perceptual-space-p state))
                                                            ;; Not folded into `compact`, which omits exactly
                                                            ;; machineResults and nothing else.
                                                            :include-active-regions (jbool body "includeActiveRegions"
                                                                                          (reality-state-include-active-regions-p state))
                                                            :compact (or (jbool body "compact" nil)
                                                                         (compact-query-p query)))
                                                           (obj "error" "Provide exactly one of: vector, sparseVector, domainVectors"))))))))))

(defun demo-machine-response (state target-name file-name display-name)
  "Compute the demo response for the machine named TARGET-NAME.
  Returns the result hash-table on success, or a cons (:error . message) on failure."
  (let ((machine nil))
    (maphash (lambda (_ m)
               (declare (ignore _))
               (when (string= (machine-name m) target-name)
                 (setf machine m)))
             (reality-state-machines state))
    (cond
      ((null machine)
       (cons :error
             (format nil "~a machine not found. Please ensure ~a is loaded."
                     display-name file-name)))
      (t
       (let* ((seqs (object-values-sorted (machine-sequences machine)))
              (seq-names (mapcar #'sequence-name seqs))
              (seq-count (length seqs))
              (metadata (or (machine-metadata machine) (obj)))
              (input-seqs-list (jarray-list (or (jget metadata "inputSequences") #())))
              (first-seq (first input-seqs-list))
              (input-vector-count
               (if first-seq
                   (length (jarray-list (or (jget first-seq "events") #())))
                   0))
              (meta-response
               (let ((result (make-hash-table :test #'equal)))
                 (when (hash-table-p* metadata)
                   (maphash (lambda (k v) (setf (gethash k result) v)) metadata))
                 (setf (gethash "name" result)              (machine-name machine)
                       (gethash "description" result)       (or (machine-description machine) "")
                       (gethash "machineId" result)         (machine-id machine)
                       (gethash "totalSequences" result)    seq-count
                       (gethash "sequenceNames" result)     (vectorize seq-names)
                       (gethash "totalInputVectors" result) input-vector-count)
                 result)))
         (obj "success"            t
              "machine"            (machine-json machine :full t)
              "metadata"           meta-response
              "sequencesLoaded"    seq-count
              "inputVectorsLoaded" input-vector-count))))))

(defun reality-routes (actor)
  (labels ((state-json (fn)
             (json-response (actor-ask actor fn))))
    (list
     (make-route "GET" "/" (lambda (_ body query)
                             (declare (ignore _ body query))
                             (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
     (make-route "GET" "/api" (lambda (_ body query)
                                (declare (ignore _ body query))
                                (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
     (make-route "GET" "/api/health" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (json-response (obj "status" "healthy"))))
     (make-route "GET" "/api/config" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (state-json (lambda (state)
                                                     (obj "eventDimension" (reality-state-dimension state)
                                                          "matchThreshold" 0.5d0
                                                          "qdrantUrl" (reality-state-qdrant-url state)
                                                          "collectionName" (reality-state-collection-name state))))))
     (make-route "PUT" "/api/config/dimension" (lambda (_ body query)
                                                 (declare (ignore _ body))
                                                 (state-json (lambda (state)
                                                               (let ((dim (parse-integer (or (gethash "dimension" query)
                                                                                             (write-to-string (reality-state-dimension state)))
                                                                                         :junk-allowed t)))
                                                                 (setf (reality-state-dimension state) dim)
                                                                 (obj "success" t "dimension" dim))))))
     (make-route "PUT" "/api/config/threshold" (lambda (_ body query)
                                                 (declare (ignore _ body))
                                                 (state-json (lambda (state)
                                                               (let ((threshold (or (ignore-errors
                                                                                     (read-from-string (or (gethash "threshold" query) "")))
                                                                                    0.5d0)))
                                                                 (setf (reality-state-match-threshold state) threshold)
                                                                 (obj "success" t "threshold" threshold))))))
     ;; ── Vectors ──────────────────────────────────────────────────────────────
     (make-route "POST" "/api/vectors/search" (lambda (_ body query)
                                               (declare (ignore _ query))
                                               (state-json (lambda (state)
                                                             (let* ((query-vector (numbers-from-json (jget body "vector")))
                                                                    (limit (truncate (or (jnumber body "limit" 10) 10)))
                                                                    (threshold (jnumber body "threshold" nil))
                                                                    (rows nil))
                                                               (maphash
                                                                (lambda (_ vector-json)
                                                                  (declare (ignore _))
                                                                  (when (< (length rows) limit)
                                                                    (let ((score (cosine query-vector (numbers-from-json (or (jget vector-json "vector")
                                                                                                                             (jget vector-json "values")
                                                                                                                             (arr))))))
                                                                      (when (or (null threshold) (>= score threshold))
                                                                        (push (obj "vector" vector-json "score" score) rows)))))
                                                                (reality-state-vector-store state))
                                                               (obj "results" (vectorize (nreverse rows))))))))
     (make-route "POST" "/api/vectors" (lambda (_ body query)
                                        (declare (ignore _ query))
                                        (state-json (lambda (state)
                                                      (let ((id (or (jstring body "id" nil) (make-id "vector"))))
                                                        (setf (jget body "id") id
                                                              (gethash id (reality-state-vector-store state)) body)
                                                        (obj "success" t "vector" body))))))
     (make-route "GET" "/api/vectors/:id" (lambda (params body query)
                                           (declare (ignore body query))
                                           (json-response (obj "message" "Vector retrieval endpoint" "id" (gethash "id" params)))))
     (make-route "DELETE" "/api/vectors/:id" (lambda (params body query)
                                              (declare (ignore body query))
                                              (state-json (lambda (state)
                                                            (remhash (gethash "id" params) (reality-state-vector-store state))
                                                            (obj "success" t "id" (gethash "id" params))))))
     (make-route "GET" "/api/engine/stats" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (state-json (lambda (state) (obj "stats" (stats-json state))))))
     ;; JSON runtime stats — same payload as /api/engine/stats plus a
     ;; domainWorkerPool marker.  Kept for parity with the older route table.
     (make-route "GET" "/api/runtime/metrics" (lambda (_ body query)
                                               (declare (ignore _ body query))
                                               (state-json (lambda (state)
                                                             (obj "stats" (stats-json state)
                                                                  "domainWorkerPool" (obj "semantics" "actor-mailbox"))))))
     ;; Prometheus text-format exposition.  Metric names + labels match AI's
     ;; /api/metrics so a single Prometheus scrape config covers all three
     ;; runtimes.  Every line carries runtime="lsp" for cross-runtime filtering.
     (make-route "GET" "/api/metrics" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (text-response
                                        (actor-ask actor (lambda (state) (prometheus-text-of state "lsp")))
                                        200
                                        "text/plain; version=0.0.4; charset=utf-8")))
     (make-route "GET" "/api/runtime/options" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (state-json (lambda (state)
                                                              (obj "historyLimit" (reality-state-history-limit state)
                                                                   "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                   "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))
                                                                "includeActiveRegions" (json-bool (reality-state-include-active-regions-p state)))))))
     (make-route "PATCH" "/api/runtime/options" (lambda (_ body query)
                                                   (declare (ignore _ query))
                                                   (state-json (lambda (state)
                                                                 (when (jnumber body "historyLimit" nil)
                                                                   (setf (reality-state-history-limit state) (truncate (jnumber body "historyLimit"))))
                                                                 (when (not (eq (jget body "includeMachineResults" :missing) :missing))
                                                                   (setf (reality-state-include-machine-results-p state) (jbool body "includeMachineResults" t)))
                                                                 (when (not (eq (jget body "includePerceptualSpace" :missing) :missing))
                                                                   (setf (reality-state-include-perceptual-space-p state) (jbool body "includePerceptualSpace" t)))
                                                                 (obj "historyLimit" (reality-state-history-limit state)
                                                                      "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                      "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))
                                                                "includeActiveRegions" (json-bool (reality-state-include-active-regions-p state)))))))
     (make-route "GET" "/api/runtime/vector-space" (lambda (_ body query)
                                                     (declare (ignore _ body query))
                                                     (state-json (lambda (state)
                                                                   (obj "dimension" (reality-state-dimension state)
                                                                        "requiredDimension" (required-dimension state)
                                                                        "encoding" "dense-float64-clamped-0-1"
                                                                        "mappingVersion" (reality-state-mapping-version state)
                                                                        "eventBusSubscriptionCount" (event-bus-subscription-count state)))))
     (make-route "GET" "/api/runtime/storage-footprint" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (state-json #'storage-footprint-json)))
     (make-route "GET" "/api/governance/route" (lambda (_ body query)
                                                 (declare (ignore _ body))
                                                 (let ((machine-id (gethash "machineId" query))
                                                       (sequence-id (gethash "sequenceId" query))
                                                       (values-text (gethash "values" query)))
                                                   (cond
                                                     ((or (null machine-id) (null sequence-id) (null values-text))
                                                      (error-response "machineId, sequenceId, and values query parameters are required" 400))
                                                     (t
                                                      (let ((result (actor-ask actor
                                                                               (lambda (state)
                                                                                 (let ((machine (gethash machine-id (reality-state-machines state))))
                                                                                   (cond
                                                                                     ((null machine) :missing)
                                                                                     (t (resolve-governance machine sequence-id
                                                                                                            (parse-comma-values values-text)))))))))
                                                        (cond
                                                          ((eq result :missing)
                                                           (error-response (format nil "Machine not found: ~a" machine-id) 404))
                                                          ((null result)
                                                           (error-response
                                                            (format nil "No triggerConfig rule matches (sequenceId=~a, values=~a)"
                                                                    sequence-id values-text)
                                                            404))
                                                          (t
                                                           (json-response (obj "success" t "decision" result))))))))))
     (make-route "POST" "/api/engine/reset" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state) (reset-reality-state state) (obj "success" t)))))
     (make-route "POST" "/api/engine/process" (lambda (_ body query)
                                                (declare (ignore _ query))
                                                (state-json (lambda (state)
                                                              ;; Map across MACHINES, in parallel, over one atomic
                                                              ;; collection of the active Reality Event space
                                                              ;; (SURFACE_SPEC.md, "POST /api/engine/process";
                                                              ;; RealityEngine_CI#254).
                                                              ;;
                                                              ;; This was a `maphash`, which had two defects and not
                                                              ;; one: it is serial, and its order is unspecified. The
                                                              ;; snapshot fixes both — it is the unit a worker can be
                                                              ;; handed, and it is sorted by machine id, so `pmap`
                                                              ;; returns results in a canonical order rather than in
                                                              ;; completion order.
                                                              ;;
                                                              ;; Safe to fan out because machines are independent at
                                                              ;; this boundary: `process-machine-input` touches only
                                                              ;; its own machine's sequences. The one piece of shared
                                                              ;; mutable state it reaches is `*random-state*` via
                                                              ;; `make-id`, and each kernel worker is given its own —
                                                              ;; see `ensure-kernel`.
                                                              ;; A Universal Reality Event is decomposed; anything else
                                                              ;; is applied whole. Length decides, against the declared
                                                              ;; dimension (SURFACE_SPEC.md). The route used to hand the
                                                              ;; raw vector to every machine, so a universal event met a
                                                              ;; machine whose input region is a handful of cells,
                                                              ;; matched nothing, and returned a well-formed empty
                                                              ;; result (RealityEngine_CI#267).
                                                              (let* ((input (numbers-from-json (jget body "vector")))
                                                                     (universal (= (length input)
                                                                                   (reality-state-dimension state)))
                                                                     (snapshot (machine-snapshot (reality-state-machines state)))
                                                                     (results (pmap-machines
                                                                               (lambda (machine)
                                                                                 (process-machine-input
                                                                                  machine
                                                                                  (if universal
                                                                                      (machine-slice machine input)
                                                                                      input)))
                                                                               snapshot))
                                                                     (outputs (loop for r in results
                                                                                    for out = (transition-result-machine-output r)
                                                                                    when out collect (output-vector-json out))))
                                                                (let ((result (obj "inputEvent" (vectorize input)
                                                                                   "timestamp" (now-ms)
                                                                                   "outputs" (vectorize outputs))))
                                                                  (record-engine-history state (obj "type" "engine-process" "result" result))
                                                                  (obj "result" result)))))))
     ;; ── /api/engine/config — one pathway for every runtime control ─────
     ;;
     ;; SURFACE_SPEC.md, "/api/engine/config". The control's name, scope and
     ;; default come from that document, not from here: three runtimes each
     ;; choosing a reasonable value is how historyLimit became 256/250/1000.
     (make-route "GET" "/api/engine/config" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (json-response
                                               (actor-ask actor
                                                          (lambda (state)
                                                            (obj "controls"
                                                                 (vectorize (list (transitions-inhibited-control state)))))))))
     (make-route "GET" "/api/engine/config/:control" (lambda (params body query)
                                                       (declare (ignore body query))
                                                       (let ((name (gethash "control" params)))
                                                         (if (string/= name "transitionsInhibited")
                                                             (error-response (format nil "Unknown control: ~a" name) 404)
                                                             (json-response
                                                              (actor-ask actor #'transitions-inhibited-control))))))
     (make-route "PUT" "/api/engine/config/:control" (lambda (params body query)
                                                       (declare (ignore query))
                                                       (let ((name (gethash "control" params)))
                                                         (cond
                                                           ((string/= name "transitionsInhibited")
                                                            (error-response (format nil "Unknown control: ~a" name) 404))
                                                           ((eq (jget body "value" :missing) :missing)
                                                            (error-response "transitionsInhibited requires a boolean `value`" 400))
                                                           (t
                                                            (let* ((value (jbool body "value" nil))
                                                                   (machine-id (jstring body "machine" nil))
                                                                   (result (actor-ask actor
                                                                                      (lambda (state)
                                                                                        (set-transitions-inhibited state machine-id value)))))
                                                              ;; A write naming a machine that does not exist is 404,
                                                              ;; never a silent no-op answering 200.
                                                              (if (eq result :missing)
                                                                  (error-response (format nil "Machine not found: ~a" machine-id) 404)
                                                                  (json-response result))))))))
     (make-route "DELETE" "/api/engine/config/:control" (lambda (params body query)
                                                          (declare (ignore body query))
                                                          (let ((name (gethash "control" params)))
                                                            (if (string/= name "transitionsInhibited")
                                                                (error-response (format nil "Unknown control: ~a" name) 404)
                                                                ;; "Restore the declared default", not "remove the
                                                                ;; control" — controls are fixed by the specification.
                                                                (json-response
                                                                 (actor-ask actor
                                                                            (lambda (state)
                                                                              (set-transitions-inhibited state nil nil))))))))
     (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                               (declare (ignore _ body query))
                                               (state-json (lambda (state) (obj "history" (vectorize (reality-state-engine-history state)))))))
     ;; Trajectory histories — SURFACE_SPEC.md, "Trajectory histories".
     (make-route "GET" "/api/engine/osre-history" (lambda (_ body query)
                                                    (declare (ignore _ body))
                                                    (state-json (lambda (state)
                                                                  (obj "history" (trajectory-window
                                                                                  (reality-state-osre-history state)
                                                                                  (or (parse-integer (or (gethash "from" query) "0") :junk-allowed t) 0)
                                                                                  (parse-integer (or (gethash "limit" query) "0") :junk-allowed t)))))))
     (make-route "GET" "/api/engine/isre-history" (lambda (_ body query)
                                                    (declare (ignore _ body))
                                                    (state-json (lambda (state)
                                                                  (obj "history" (trajectory-window
                                                                                  (reality-state-isre-history state)
                                                                                  (or (parse-integer (or (gethash "from" query) "0") :junk-allowed t) 0)
                                                                                  (parse-integer (or (gethash "limit" query) "0") :junk-allowed t)))))))
     (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state) (obj "activeEvents" (active-vectors-json state))))))
     (make-route "GET" "/api/machines" (lambda (_ body query)
                                         (declare (ignore _ body))
                                         (let ((summary-p (and (hash-table-p* query)
                                                               (member (gethash "summary" query)
                                                                       '("true" "1") :test #'string=))))
                                           (state-json (lambda (state)
                                                         (obj "machines" (vectorize
                                                                          (mapcar (if summary-p
                                                                                      #'machine-summary-json
                                                                                      #'machine-json)
                                                                                  (machines-in-canonical-order (reality-state-machines state))))))))))
     (make-route "GET" "/api/buses/semantic" (lambda (_ body query)
                                                     (declare (ignore _ body query))
                                                     (handler-case
                                                         (state-json #'semantic-bus-registry-json)
                                                       (error (e)
                                                         (error-response (format nil "semantic bus registry unavailable: ~a" e) 404)))))
     (make-route "GET" "/api/buses/semantic/:id" (lambda (params body query)
                                                         (declare (ignore body query))
                                                         (handler-case
                                                             (let* ((registry (actor-ask actor #'semantic-bus-registry-json))
                                                                    (bus (find-semantic-bus-json registry (gethash "id" params))))
                                                               (if bus
                                                                   (json-response (obj "bus" bus))
                                                                   (error-response "Semantic bus not found" 404)))
                                                           (error (e)
                                                             (error-response (format nil "semantic bus registry unavailable: ~a" e) 404)))))
     ;; Semantic audit trail (SEMANTIC_AUDIT_CONTRACT.md, milestone M5):
     ;; re:SequenceObservation records emitted while machines process input,
     ;; IRI-enriched from the corpus semantics manifest at read time.
     (make-route "GET" "/api/audit/semantics" (lambda (_ body query)
                                                (declare (ignore _ body))
                                                (let ((limit (or (ignore-errors
                                                                   (parse-integer (or (gethash "limit" query) "100")))
                                                                 100)))
                                                  (json-response
                                                   (actor-ask actor
                                                              (lambda (state)
                                                                (semantic-audit-json state limit)))))))
     ;; OWL semantic identity (roadmap M4): IRI + ABox content hash from the
     ;; corpus semantics/abox-manifest.json, keyed by machine name. Contract
     ;; mirrors the C++, Scala, and TypeScript engines.
     (make-route "GET" "/api/machines/semantics/:name" (lambda (params body query)
                                                         (declare (ignore body query))
                                                         (handler-case
                                                             (let ((manifest (actor-ask actor #'semantics-manifest-json))
                                                                   (name (percent-decode (gethash "name" params))))
                                                               (multiple-value-bind (key entry) (find-semantics-entry manifest name)
                                                                 (if entry
                                                                     (json-response (obj "name" name
                                                                                         "machineKey" key
                                                                                         "semanticsIri" (jstring entry "iri" +json-null+)
                                                                                         "semanticsHash" (jstring entry "sha256" +json-null+)
                                                                                         "sourceFile" (jstring entry "sourceFile" +json-null+)
                                                                                         "ontology" (jstring manifest "ontology" +json-null+)))
                                                                     (error-response (format nil "No semantics manifest entry for machine: ~a" name) 404))))
                                                           (error (e)
                                                             (error-response (format nil "semantics manifest unavailable: ~a" e) 404)))))
     (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore body query))
                                             ;; Same empty-LET defect as the other machines/:id
                                             ;; handler above (#42).
                                             (let ((machine (actor-ask actor (lambda (state)
                                                                               (gethash (gethash "id" params)
                                                                                        (reality-state-machines state))))))
                                               (if machine
                                                   (json-response (obj "machine" (machine-json machine :full t)))
                                                   (error-response "Machine not found" 404)))))
     (make-route "POST" "/api/machines" (lambda (_ body query)
                                          (declare (ignore _ query))
                                          (state-json (lambda (state)
                                                        (let ((machine (machine-from-json body)))
                                                          (put-machine state machine)
                                                          (obj "success" t "machine" (machine-json machine :full t)))))))
     (make-route "PUT" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore query))
                                             (state-json (lambda (state)
                                                           (let ((machine (machine-from-json body (gethash "id" params))))
                                                             (put-machine state machine)
                                                             (obj "success" t "machine" (machine-json machine :full t)))))))
     (make-route "PATCH" "/api/machines/:id" (lambda (params body query)
                                               (declare (ignore query))
                                               (let ((result (actor-ask actor
                                                                        (lambda (state)
                                                                          (let ((machine (gethash (gethash "id" params)
                                                                                                  (reality-state-machines state))))
                                                                            (when machine
                                                                              (when (jstring body "name" nil)
                                                                                (setf (machine-name machine) (jstring body "name" nil)))
                                                                              (when (jstring body "description" nil)
                                                                                (setf (machine-description machine) (jstring body "description" nil)))
                                                                              (when (jobject-p (jget body "metadata"))
                                                                                (let ((existing (or (machine-metadata machine) (obj))))
                                                                                  (maphash (lambda (k v) (setf (gethash k existing) v))
                                                                                           (jget body "metadata"))
                                                                                  (setf (machine-metadata machine) existing)))
                                                                              (put-machine state machine)
                                                                              (obj "success" t "machine" (machine-json machine :full t))))))))
                                                 (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "DELETE" "/api/machines/:id" (lambda (params body query)
                                               (declare (ignore body query))
                                               (state-json (lambda (state)
                                                             (unregister-compose-subscriptions state (gethash "id" params))
                                                             (let ((removed (remhash (gethash "id" params)
                                                                                     (reality-state-machines state))))
                                                               (when removed
                                                                 (incf (reality-state-mapping-version state)))
                                                               (obj "success" (json-bool removed)))))))
     (make-route "POST" "/api/machines/:id/process-universal" (lambda (params body query)
                                                               (declare (ignore query))
                                                               (let ((result (actor-ask actor
                                                                                        (lambda (state)
                                                                                          (let ((machine (gethash (gethash "id" params)
                                                                                                                  (reality-state-machines state))))
                                                                                            (when (and machine (machine-mapping machine))
                                                                                              (transition-result-json
                                                                                               (process-machine-input machine
                                                                                                                      (extract-region
                                                                                                                       (numbers-from-json (jget body "universalInputSpace"))
                                                                                                                       (mapping-input (machine-mapping machine)))))))))))
                                                                 (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "POST" "/api/machines/process-universal/all" (lambda (_ body query)
                                                               (declare (ignore _ query))
                                                               (state-json (lambda (state)
                                                                             (let ((universal (numbers-from-json (jget body "universalInputSpace")))
                                                                                   (results (make-hash-table :test #'equal)))
                                                                               (maphash (lambda (id machine)
                                                                                          (when (machine-mapping machine)
                                                                                            (setf (gethash id results)
                                                                                                  (transition-result-json
                                                                                                   (process-machine-input machine
                                                                                                                          (extract-region universal
                                                                                                                                          (mapping-input (machine-mapping machine))))))))
                                                                                        (reality-state-machines state))
                                                                               (obj "results" results))))))
     (make-route "POST" "/api/machines/:id/whatif" (lambda (params body query)
                                                    (declare (ignore query))
                                                    (let ((result (actor-ask actor
                                                                             (lambda (state)
                                                                               (let ((machine (gethash (gethash "id" params)
                                                                                                       (reality-state-machines state))))
                                                                                 (when machine
                                                                                   (transition-result-json
                                                                                    (process-machine-input
                                                                                     (machine-from-json (machine-json machine :full t))
                                                                                     (numbers-from-json (jget body "inputEvent"))))))))))
                                                      (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "POST" "/api/machines/:id/whatif-universal" (lambda (params body query)
                                                              (declare (ignore query))
                                                              (let ((result (actor-ask actor
                                                                                       (lambda (state)
                                                                                         (let ((machine (gethash (gethash "id" params)
                                                                                                                 (reality-state-machines state))))
                                                                                           (when (and machine (machine-mapping machine))
                                                                                             (let* ((copy (machine-from-json (machine-json machine :full t)))
                                                                                                    (input (extract-region
                                                                                                            (numbers-from-json (jget body "universalInputSpace"))
                                                                                                            (mapping-input (machine-mapping copy)))))
                                                                                               (transition-result-json (process-machine-input copy input)))))))))
                                                                (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "GET" "/api/machines/json/list" (lambda (_ body query)
                                                  (declare (ignore _ body query))
                                                  (state-json (lambda (state)
                                                                (obj "machines" (vectorize (machine-json-list-rows
                                                                                            (reality-state-machine-dir state)
                                                                                            (ignore-errors (jget (semantics-manifest-json state) "machines")))))))))
     (make-route "GET" "/api/machines/json/:name" (lambda (params body query)
                                                   (declare (ignore body query))
                                                   (handler-case
                                                       (state-json (lambda (state)
                                                                     (let* ((name (gethash "name" params))
                                                                            (path (resolve-machine-json-path (reality-state-machine-dir state) name))
                                                                            (machine (load-machine-from-file path)))
                                                                       (put-machine state machine)
                                                                       (obj "success" t "machine" (machine-json machine :full t)
                                                                            "message" "Machine loaded successfully"))))
                                                     (error (c) (error-response (princ-to-string c) 404)))))
     (make-route "POST" "/api/machines/json/import" (lambda (_ body query)
                                                     (declare (ignore _ query))
                                                     (state-json (lambda (state)
                                                                   (let ((machine (machine-from-json body)))
                                                                     (put-machine state machine)
                                                                     (obj "success" t "machine" (machine-json machine :full t)))))))
     (make-route "GET" "/api/machines/:id/export" (lambda (params body query)
                                                   (declare (ignore body query))
                                                   (let ((machine (actor-ask actor
                                                                             (lambda (state)
                                                                               (gethash (gethash "id" params)
                                                                                        (reality-state-machines state))))))
                                                     (if machine
                                                         (json-response (obj "version" "1.0.0"
                                                                             "machine" (machine-json machine :full t)))
                                                         (error-response "Machine not found" 404)))))
     ;; The merge knob, readable always and settable only while unlocked.
     ;;
     ;; Declared on the machine (`outputMergeTransformation`, default "or") so it
     ;; is carried from the moment the machine is interned, and mutable here so a
     ;; run can be retuned between steps without reloading the corpus. It is a
     ;; training variable; both properties are needed.
     ;;
     ;; The interlock starts LOCKED. Retuning a training variable by accident
     ;; produces a run whose results mean nothing and which nothing
     ;; distinguishes from a valid one, so unlocking is a separate act.
     (make-route "GET" "/api/machines/:id/output-merge"
                 (lambda (params body query)
                   (declare (ignore body query))
                   (json-response
                    (actor-ask actor
                               (lambda (state)
                                 (let ((machine (gethash (gethash "id" params)
                                                         (reality-state-machines state))))
                                   (if machine
                                       (obj "machineId" (machine-id machine)
                                            "machineName" (or (machine-name machine) "")
                                            "outputMergeTransformation"
                                            (output-merge-name
                                             (machine-output-merge-transformation machine))
                                            "locked" (json-bool (machine-output-merge-locked machine))
                                            ;; Advertised from the same list the
                                            ;; PUT validates against, so the two
                                            ;; cannot drift as transformations
                                            ;; are added.
                                            "available" (vectorize +output-merge-transformations+))
                                       +json-null+)))))))
     (make-route "PUT" "/api/machines/:id/output-merge"
                 (lambda (params body query)
                   (declare (ignore query))
                   (let ((outcome
                           (actor-ask actor
                                      (lambda (state)
                                        (let* ((machine (gethash (gethash "id" params)
                                                                 (reality-state-machines state)))
                                               (requested (jstring body "outputMergeTransformation" nil)))
                                          (cond
                                            ((null machine) (obj "%status" 404 "error" "Machine not found"))
                                            ((null requested)
                                             (obj "%status" 400 "error" "outputMergeTransformation must be a string"))
                                            ((not (member (string-downcase requested)
                                                          +output-merge-transformations+ :test #'string=))
                                             (obj "%status" 400 "error"
                                                  (format nil "Unknown output merge transformation: ~a" requested)))
                                            ;; 423 rather than 403: the refusal is about the
                                            ;; resource's current state and is cleared by
                                            ;; unlocking, not about who is asking.
                                            ((machine-output-merge-locked machine)
                                             (obj "%status" 423 "error"
                                                  "outputMergeTransformation is locked; unlock it before changing a training variable"))
                                            (t
                                             (setf (machine-output-merge-transformation machine)
                                                   (output-merge-name requested))
                                             (obj "%status" 200 "success" t
                                                  "machineId" (machine-id machine)
                                                  "outputMergeTransformation"
                                                  (machine-output-merge-transformation machine)
                                                  "locked" (json-bool (machine-output-merge-locked machine))))))))))
                     (let ((status (jnumber outcome "%status" 200)))
                       (remhash "%status" outcome)
                       (if (= status 200)
                           (json-response outcome)
                           (error-response (jstring outcome "error" "error") status))))))
     (make-route "PUT" "/api/machines/:id/output-merge/lock"
                 (lambda (params body query)
                   (declare (ignore query))
                   (let ((outcome
                           (actor-ask actor
                                      (lambda (state)
                                        (let ((machine (gethash (gethash "id" params)
                                                                (reality-state-machines state))))
                                          (if (null machine)
                                              (obj "%status" 404 "error" "Machine not found")
                                              (let ((locked (jbool body "locked" t)))
                                                (setf (machine-output-merge-locked machine) locked)
                                                (obj "%status" 200 "success" t
                                                     "machineId" (machine-id machine)
                                                     "locked" (json-bool locked)))))))))
                     (let ((status (jnumber outcome "%status" 200)))
                       (remhash "%status" outcome)
                       (if (= status 200)
                           (json-response outcome)
                           (error-response (jstring outcome "error" "error") status))))))
     (make-route "GET" "/api/machines/:id/checkpoints" (lambda (params body query)
                                                         (declare (ignore body query))
                                                         (state-json (lambda (state)
                                                                       (let* ((mid (gethash "id" params))
                                                                              (sub (gethash mid (reality-state-checkpoints state))))
                                                                         (obj "checkpoints"
                                                                              (vectorize
                                                                               (let (rows)
                                                                                 (when sub
                                                                                   (maphash (lambda (_ cp)
                                                                                              (declare (ignore _))
                                                                                              (push (obj "id" (gethash "id" cp)
                                                                                                         "label" (gethash "label" cp)
                                                                                                         "timestamp" (gethash "timestamp" cp))
                                                                                                    rows))
                                                                                            sub))
                                                                                 (nreverse rows)))))))))
     (make-route "POST" "/api/machines/:id/checkpoints" (lambda (params body query)
                                                          (declare (ignore query))
                                                          (let ((result (actor-ask actor
                                                                                   (lambda (state)
                                                                                     (let* ((mid (gethash "id" params))
                                                                                            (machine (gethash mid (reality-state-machines state))))
                                                                                       (when machine
                                                                                         (let* ((cp-id (make-id "checkpoint"))
                                                                                                (label (or (jstring body "label" nil) "checkpoint"))
                                                                                                (clone (machine-from-json (machine-json machine :full t)))
                                                                                                (cp (obj "id" cp-id "label" label
                                                                                                         "timestamp" (now-ms) "machine" clone))
                                                                                                (cps (reality-state-checkpoints state))
                                                                                                (sub (or (gethash mid cps)
                                                                                                         (setf (gethash mid cps)
                                                                                                               (make-hash-table :test #'equal)))))
                                                                                           (setf (gethash cp-id sub) cp)
                                                                                           (obj "success" t "checkpointId" cp-id))))))))
                                                            (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "POST" "/api/machines/:machineId/checkpoints/:cpId/restore" (lambda (params body query)
                                                                               (declare (ignore body query))
                                                                               (let ((result (actor-ask actor
                                                                                                        (lambda (state)
                                                                                                          (let* ((mid (gethash "machineId" params))
                                                                                                                 (cp-id (gethash "cpId" params))
                                                                                                                 (sub (gethash mid (reality-state-checkpoints state)))
                                                                                                                 (cp (when sub (gethash cp-id sub))))
                                                                                                            (when cp
                                                                                                              (unregister-compose-subscriptions state mid)
                                                                                                              (remhash mid (reality-state-machines state))
                                                                                                              (put-machine state (gethash "machine" cp))
                                                                                                              (obj "success" t)))))))
                                                                                 ;; Inside the LET, not after it. One closing paren too many above
                                                                                 ;; ended the LET at the binding, so RESULT here was outside its own
                                                                                 ;; scope and every restore answered 500 "RESULT is unbound" — while
                                                                                 ;; the restore itself had already been applied by the actor (#57).
                                                                                 (if result (json-response result) (error-response "Checkpoint not found" 404))))))
     (make-route "DELETE" "/api/machines/:machineId/checkpoints/:cpId" (lambda (params body query)
                                                                         (declare (ignore body query))
                                                                         (state-json (lambda (state)
                                                                                       (let* ((mid (gethash "machineId" params))
                                                                                              (cp-id (gethash "cpId" params))
                                                                                              (sub (gethash mid (reality-state-checkpoints state)))
                                                                                              (removed (when sub (remhash cp-id sub))))
                                                                                         (obj "success" (json-bool removed)))))))
     ;; ── Sequences ────────────────────────────────────────────────────────────
     (make-route "GET" "/api/sequences" (lambda (_ body query)
                                          (declare (ignore _ body query))
                                          (state-json (lambda (state)
                                                        (obj "sequences" (vectorize
                                                                          (mapcar (lambda (s) (sequence-json s :full t))
                                                                                  (object-values (reality-state-sequences state)))))))))
     (make-route "POST" "/api/sequences" (lambda (_ body query)
                                           (declare (ignore _ query))
                                           (state-json (lambda (state)
                                                         (let ((sequence (parse-sequence body)))
                                                           (setf (gethash (sequence-id sequence) (reality-state-sequences state)) sequence)
                                                           (obj "success" t "sequence" (sequence-json sequence :full t)))))))
     (make-route "GET" "/api/sequences/:id" (lambda (params body query)
                                              (declare (ignore body query))
                                              (let ((result (actor-ask actor
                                                                       (lambda (state)
                                                                         (gethash (gethash "id" params) (reality-state-sequences state))))))
                                                (if result
                                                    (json-response (obj "sequence" (sequence-json result :full t)))
                                                    (error-response "Sequence not found" 404)))))
     (make-route "POST" "/api/sequences/persist" (lambda (_ body query)
                                                   (declare (ignore _ body query))
                                                   (json-response (obj "success" t))))
     (make-route "DELETE" "/api/sequences/:id" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (let ((result (actor-ask actor
                                                                          (lambda (state)
                                                                            (let ((sid (gethash "id" params)))
                                                                              (when (gethash sid (reality-state-sequences state))
                                                                                (remhash sid (reality-state-sequences state))
                                                                                t))))))
                                                   (if result (json-response (obj "success" t)) (error-response "Sequence not found" 404)))))
     (make-route "POST" "/api/sequences/:id/reset" (lambda (params body query)
                                                     (declare (ignore body query))
                                                     (let ((result (actor-ask actor
                                                                              (lambda (state)
                                                                                (let ((seq (gethash (gethash "id" params)
                                                                                                    (reality-state-sequences state))))
                                                                                  (when seq (reset-sequence seq) t))))))
                                                       (if result (json-response (obj "success" t)) (error-response "Sequence not found" 404)))))
     (make-route "POST" "/api/sequences/:id/vectors" (lambda (params body query)
                                                       (declare (ignore query))
                                                       (let ((result (actor-ask actor
                                                                                (lambda (state)
                                                                                  (let ((seq (gethash (gethash "id" params)
                                                                                                      (reality-state-sequences state))))
                                                                                    (when seq
                                                                                      (let ((vector (parse-reality-event body)))
                                                                                        (setf (gethash (reality-event-id vector)
                                                                                                       (sequence-vectors seq))
                                                                                              vector)
                                                                                        (obj "success" t "vector" (reality-event-json vector)))))))))
                                                         (if result (json-response result) (error-response "Sequence not found" 404)))))
     (make-route "POST" "/api/machines/:id/process" (lambda (params body query)
                                                      (declare (ignore query))
                                                      (let ((result (actor-ask actor
                                                                               (lambda (state)
                                                                                 (let ((machine (gethash (gethash "id" params)
                                                                                                         (reality-state-machines state))))
                                                                                   (when machine
                                                                                     (transition-result-json
                                                                                      (process-machine-input machine (numbers-from-json (jget body "inputEvent"))))))))))
                                                        (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "GET" "/api/machine-graph" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json #'machine-graph-json)))
     ;; Arbitration records for the most recent step (ARBITER_CONTRACT.md 6).
     ;;
     ;; Registered in the *live* reality-routes. The function is defined twice
     ;; and the second shadows the first; #48's addition went into the shadowed
     ;; one, so the endpoint 404'd while looking present in the source. The
     ;; hosted arbiter stage caught it:
     ;;   FAIL lsp:lsp-1: GET /api/arbitration -> 404 No route for GET /api/arbitration
     ;;
     ;; A suppressed contribution has to stay attributable — "the agent's answer
     ;; was discarded" is the operational fact the domain bus exists to surface.
     ;; Wire shape matches the Scala, C++ and TS runtimes exactly; byte
     ;; equivalence is this contract's acceptance test.
     (make-route "GET" "/api/arbitration" (lambda (_ body query)
                                            (declare (ignore _ body query))
                                            (state-json #'arbitration-json)))
     ;; ── Perceptual simulation ─────────────────────────────────────────────────
     (make-route "POST" "/api/perceptual-simulation/step" (lambda (_ body query)
                                                            (declare (ignore _ body query))
                                                            (state-json (lambda (state)
                                                                          (let ((step (process-perceptual-input state (reality-state-perceptual-space state)
                                                                                                                :include-machine-results t
                                                                                                                :include-perceptual-space t)))
                                                                            (obj "success" t "step" step))))))
     (make-route "POST" "/api/perceptual-simulation/reset" (lambda (_ body query)
                                                             (declare (ignore _ body query))
                                                             (state-json (lambda (state) (reset-reality-state state) (obj "success" t)))))
     (make-route "POST" "/api/perceptual-simulation/start" (lambda (_ body query)
                                                             (declare (ignore _ body query))
                                                             (json-response (obj "success" t))))
     (make-route "POST" "/api/perceptual-simulation/stop" (lambda (_ body query)
                                                            (declare (ignore _ body query))
                                                            (json-response (obj "success" t))))
     (make-route "GET" "/api/perceptual-simulation/state" (lambda (_ body query)
                                                            (declare (ignore _ body query))
                                                            (state-json (lambda (state)
                                                                          (obj "running" +json-false+
                                                                               "dimension" (reality-state-dimension state)
                                                                               "perceptualSpace" (perceptual-space-snapshot (reality-state-perceptual-space state)))))))
     (make-route "GET" "/api/perceptual-simulation/history" (lambda (_ body query)
                                                              (declare (ignore _ body query))
                                                              (state-json (lambda (state)
                                                                            (obj "history" (vectorize (reality-state-history state)))))))
     (make-route "POST" "/api/perceptual-simulation/configure/chunk" (lambda (_ body query)
                                                                       (declare (ignore _ query))
                                                                       (state-json (lambda (state)
                                                                                     (when (jbool body "reset" nil)
                                                                                       (setf (reality-state-sim-buffer state) nil))
                                                                                     (dolist (v (jarray-list (or (jget body "events") (arr))))
                                                                                       (push (numbers-from-json v) (reality-state-sim-buffer state)))
                                                                                     (let ((cfg (or (and (jobject-p (jget body "config")) (jget body "config")) body)))
                                                                                       (when (jobject-p (jget cfg "inputRegion"))
                                                                                         (setf (reality-state-sim-buffered-region state)
                                                                                               (make-region-from-json (jget cfg "inputRegion"))))
                                                                                       (when (jnumber cfg "stepDelayMs" nil)
                                                                                         (setf (reality-state-sim-buffered-delay state)
                                                                                               (truncate (jnumber cfg "stepDelayMs" 100)))))
                                                                                     (obj "success" t "bufferedVectors" (length (reality-state-sim-buffer state)))))))
     (make-route "POST" "/api/perceptual-simulation/configure/commit" (lambda (_ body query)
                                                                        (declare (ignore _ body query))
                                                                        (state-json (lambda (state)
                                                                                      (setf (reality-state-sim-buffer state) nil)
                                                                                      (obj "success" t)))))
     ;; ── Sampler ───────────────────────────────────────────────────────────────
     (make-route "POST" "/api/sampler/start" (lambda (_ body query)
                                               (declare (ignore _ query))
                                               (state-json (lambda (state)
                                                             (setf (reality-state-sampler-running-p state) t
                                                                   (reality-state-sampler-strategy state)    (or (jstring body "strategy" nil) "manual")
                                                                   (reality-state-sampler-interval-ms state) (truncate (or (jnumber body "intervalMs" 0) 0)))
                                                             (obj "success" t
                                                                  "stats" (obj "isRunning" +json-true+
                                                                               "sampleCount" (reality-state-sampler-sample-count state)
                                                                               "strategy" (reality-state-sampler-strategy state)
                                                                               "intervalMs" (reality-state-sampler-interval-ms state)))))))
     (make-route "POST" "/api/sampler/stop" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state)
                                                            (setf (reality-state-sampler-running-p state) nil)
                                                            (obj "success" t)))))
     (make-route "POST" "/api/sampler/sample" (lambda (_ body query)
                                                (declare (ignore _ query))
                                                (state-json (lambda (state)
                                                              (incf (reality-state-sampler-sample-count state))
                                                              (let ((result (obj "inputEvent" (vectorize (numbers-from-json (jget body "data")))
                                                                                 "processingTimestamp" (now-ms))))
                                                                (obj "success" t "result" result))))))
     (make-route "GET" "/api/sampler/stats" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state)
                                                            (obj "stats" (obj "isRunning" (json-bool (reality-state-sampler-running-p state))
                                                                              "sampleCount" (reality-state-sampler-sample-count state)
                                                                              "lastSampleTimestamp" +json-null+
                                                                              "strategy" (or (reality-state-sampler-strategy state) "manual")
                                                                              "intervalMs" (or (reality-state-sampler-interval-ms state) 0)))))))
     ;; ── Perception ────────────────────────────────────────────────────────────
     (make-route "POST" "/api/perception/observe" (lambda (_ body query)
                                                    (declare (ignore _ query))
                                                    (let ((data (numbers-from-json (jget body "data"))))
                                                      (json-response (obj "success" t
                                                                          "inputEvent" (vectorize data)
                                                                          "transformations" (arr)
                                                                          "processingTimestamp" (now-ms))))))
     (make-route "POST" "/api/perception/diagnostic" (lambda (_ body query)
                                                       (declare (ignore _ query))
                                                       (json-response (obj "universalInputSpace" (jget body "universalInputSpace")
                                                                           "resolvedInputs" (obj)))))
     ;; ── Demos ────────────────────────────────────────────────────────────────
     (make-route "GET" "/api/demo/multi-step" (lambda (_ body query)
       (declare (ignore _ body query))
       (let ((r (actor-ask actor (lambda (state)
                                   (demo-machine-response state
                                     "Multi-Step State Machine"
                                     "MultiStep.json"
                                     "Multi-Step State Machine")))))
         (if (and (consp r) (eq (car r) :error))
             (error-response (cdr r) 404)
             (json-response r)))))
     (make-route "GET" "/api/demo/data-center" (lambda (_ body query)
       (declare (ignore _ body query))
       (let ((r (actor-ask actor (lambda (state)
                                   (demo-machine-response state
                                     "Data Center Monitoring"
                                     "DataCenterMonitoring.json"
                                     "Data Center Monitoring")))))
         (if (and (consp r) (eq (car r) :error))
             (error-response (cdr r) 404)
             (json-response r)))))
     (make-route "GET" "/api/demo/kleene-star" (lambda (_ body query)
       (declare (ignore _ body query))
       (let ((r (actor-ask actor (lambda (state)
                                   (demo-machine-response state
                                     "Kleene Star Operator"
                                     "KleeneStar.json"
                                     "Kleene Star Operator")))))
         (if (and (consp r) (eq (car r) :error))
             (error-response (cdr r) 404)
             (json-response r)))))
     (make-route "POST" "/api/perceive" (lambda (_ body query)
                                          (declare (ignore _))
                                          (state-json (lambda (state)
                                                        (let ((input (assemble-input-vector state body)))
                                                          (if input
                                                              (let ((step (process-perceptual-input
                                                                           state input
                                                                           ;; Only an explicit override overrides
                                                                           ;; -- see the note on the other push
                                                                           ;; path (RealityEngine_CI#201).
                                                                           :override (jstring body "matchAlgorithmOverride" nil)
                                                                           :include-machine-results (jbool body "includeMachineResults"
                                                                                                           (if (jbool body "compact" nil)
                                                                                                               nil
                                                                                                               (reality-state-include-machine-results-p state)))
                                                                           :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                                            (reality-state-include-perceptual-space-p state))
                                                                           ;; Not folded into `compact`, which omits exactly
                                                                           ;; machineResults and nothing else (SURFACE_SPEC.md).
                                                                           :include-active-regions (jbool body "includeActiveRegions"
                                                                                                         (reality-state-include-active-regions-p state))
                                                                           :compact (or (jbool body "compact" nil)
                                                                                        (compact-query-p query)))))
                                                                (re-broadcast (obj "type" "step-result" "step" step))
                                                                step)
                                                              (obj "error" "Provide exactly one of: vector, sparseVector, domainVectors"))))))))))

(defun start-reality-service (&key (port 5601) (machine-dir "../RealityEngine_Machines/machines") (dimension 7680))
  (let* ((state (make-reality-state-from-config :machine-dir machine-dir :dimension dimension))
         (actor (state-actor "reality-service" state)))
    ;; WORKAROUND, not a fix — RealityEngine_CI#281.
    ;;
    ;; On the hosted regression lane this runtime reports one more trajectory
    ;; entry than C++ and Scala — after a reset and eight pushes, isre- and
    ;; osre-history hold 9 where the others hold 8 — and because the parity
    ;; stage compares index-wise, that offset produced a second, downstream
    ;; finding: "isre-history diverges at step 0 cell 0", this engine reading
    ;; 0.5 where the other two read 0.0.
    ;;
    ;; The 0.5 is the tell. It is a half-activated cell, not a different answer:
    ;; the first sequence of the corpus stutters as it is created, so the state
    ;; the first step records is mid-activation rather than settled. Everything
    ;; after it is downstream of that one entry.
    ;;
    ;; Sequences are created here, during `make-reality-state-from-config`. The
    ;; part of `reset-reality-state` that matters is therefore not the history
    ;; clearing but the `reset-sequence` it runs over every machine's sequences,
    ;; which puts a stuttered first sequence back to its initial state before
    ;; anything can observe it.
    ;;
    ;; Safe here specifically because nothing has been served — the HTTP server
    ;; starts below — so this re-initialises load-time state only, never a
    ;; caller's work. Machines stay registered; re-initialised sequences are
    ;; where a freshly loaded corpus should already be.
    ;;
    ;; Remove this once the stutter itself is fixed. It treats the symptom, and
    ;; leaving it unlabelled would hide the cause.
    (reset-reality-state state)
    (start-http-server port (reality-routes actor) :name "reality-engine-lsp"
                       :extra-dispatchers
                       (list (hunchentoot:create-prefix-dispatcher
                              "/api/engine/stream"
                              #'re-sse-stream-handler)))))

(defun start-reality-from-environment ()
  (let ((machine-dir (env "MACHINES_DIR" "../RealityEngine_Machines/machines")))
    ;; Declares how each contended universal-vector position resolves. Loaded
    ;; before the corpus so the first step already arbitrates rather than
    ;; falling back (ARBITER_CONTRACT.md 5).
    (load-arbitration-registry machine-dir)
    (start-reality-service :port (env-int "REALITY_ENGINE_PORT" 5601)
                           :machine-dir machine-dir
                           :dimension (env-int "VECTOR_DIMENSION" 7680))))
