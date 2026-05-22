(in-package #:reality-engine-lsp)

(defstruct reality-state
  dimension machines machine-dir perceptual-space history history-limit include-machine-results-p
  include-perceptual-space-p vector-store sequences qdrant-url collection-name started-at
  event-bus-subscriptions latched-event-bits step-count mapping-version
  ;; CES coverage counters — mirror RealityEngine_AI/CesCoverageRegistry and
  ;; RealityEngine_CPP/CesCoverageRegistry so the same Prometheus scrape
  ;; config covers all three runtimes.  Keyed by tab-joined identifiers.
  cov-matched cov-activated cov-outputs cov-steps cov-paging cov-deprecated
  checkpoints
  match-threshold
  sampler-running-p sampler-strategy sampler-interval-ms sampler-sample-count
  sim-buffer sim-buffered-region sim-buffered-delay)

(defun compose-key (producer-machine-id producer-sequence-id)
  (format nil "~a|~a" producer-machine-id producer-sequence-id))

(defun ensure-space-length (state length)
  (when (> length (length (reality-state-perceptual-space state)))
    (setf (reality-state-perceptual-space state)
          (append (reality-state-perceptual-space state)
                  (make-list (- length (length (reality-state-perceptual-space state)))
                             :initial-element 0.0d0))))
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
           :perceptual-space (make-list dimension :initial-element 0.0d0)
           :history nil
           :history-limit (env-int "RE_HISTORY_LIMIT" 250)
           :include-machine-results-p (env-bool "RE_INCLUDE_MACHINE_RESULTS" t)
           :include-perceptual-space-p (env-bool "RE_INCLUDE_PERCEPTUAL_SPACE" t)
           :vector-store (make-hash-table :test #'equal)
           :sequences (make-hash-table :test #'equal)
           :qdrant-url (env "QDRANT_URL" "http://localhost:4333")
           :collection-name (env "QDRANT_REALITY_COLLECTION" "reality-vectors")
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
        (make-list (reality-state-dimension state) :initial-element 0.0d0)
        (reality-state-history state) nil
        (reality-state-latched-event-bits state) (make-hash-table :test #'equal)
        (reality-state-step-count state) 0
        (reality-state-cov-matched    state) (make-hash-table :test #'equal)
        (reality-state-cov-activated  state) (make-hash-table :test #'equal)
        (reality-state-cov-outputs    state) (make-hash-table :test #'equal)
        (reality-state-cov-steps      state) (make-hash-table :test #'equal)
        (reality-state-cov-paging     state) (make-hash-table :test #'equal)
        (reality-state-cov-deprecated state) (make-hash-table :test #'equal))
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
            (let ((matched (jget sr "matchedVectors")))
              (when (jarray-p matched)
                (dolist (vid (jarray-list matched))
                  (when (stringp vid)
                    (coverage-bump (reality-state-cov-matched state)
                                   (coverage-key mid mname sid vid))))))
            (let ((activated (jget sr "activatedVectors")))
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

(defun record-merge-coverage (state operation)
  "Bump paging-decision and deprecated-fire counters from one merge-batch
entry.  Called for each operation in the sorted merge batch."
  (let ((gov (jget operation "governance"))
        (dep (jget operation "deprecation"))
        (mid (jstring operation "machineId" ""))
        (sid (jstring operation "sequenceId" ""))
        (mname (jstring operation "machineName" "")))
    (when (jobject-p gov)
      (coverage-bump (reality-state-cov-paging state)
                     (coverage-key (jstring gov "ownerTeam" "unrouted")
                                   (jstring gov "processStatus" "unknown")
                                   (jstring gov "ragStatusCode" "unknown")
                                   mid)))
    (when (jobject-p dep)
      (coverage-bump (reality-state-cov-deprecated state)
                     (coverage-key mid mname sid
                                   (jstring dep "replacedBy" ""))))))

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
              (let ((vk (coverage-key mid mname (sequence-id seq) (reality-vector-id v))))
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
          (emit-help "ces_machines_total"  "Number of machines registered with the simulator." "gauge")
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

          (emit-help "ces_unfired_sequences"
                     "Number of sequences in this machine that have never emitted output." "gauge")
          (dolist (row (prom-per-machine-unfired state))
            (emit "ces_unfired_sequences"
                  (append base `(("machine" . ,(nth 1 row))
                                 ("machine_id" . ,(nth 0 row))))
                  (nth 4 row)))

          (emit-help "ces_unfired_vectors"
                     "Number of vectors in this machine that have never matched or activated." "gauge")
          (dolist (row (prom-per-machine-unfired state))
            (emit "ces_unfired_vectors"
                  (append base `(("machine" . ,(nth 1 row))
                                 ("machine_id" . ,(nth 0 row))))
                  (nth 5 row)))

          (emit-help "ces_machine_sequence_count"
                     "Total sequences declared by this machine." "gauge")
          (dolist (row (prom-per-machine-unfired state))
            (emit "ces_machine_sequence_count"
                  (append base `(("machine" . ,(nth 1 row))
                                 ("machine_id" . ,(nth 0 row))))
                  (nth 2 row)))

          (emit-help "ces_machine_vector_count"
                     "Total vectors declared by this machine." "gauge")
          (dolist (row (prom-per-machine-unfired state))
            (emit "ces_machine_vector_count"
                  (append base `(("machine" . ,(nth 1 row))
                                 ("machine_id" . ,(nth 0 row))))
                  (nth 3 row)))

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
            (dolist (row (prom-per-machine-unfired state))
              (let* ((mid (nth 0 row))
                     (mname (nth 1 row))
                     (labels (append base `(("machine" . ,mname)
                                            ("machine_id" . ,mid)))))
                (emit "ces_vector_matched_total"   labels 0)
                (emit "ces_vector_activated_total" labels 0)
                (emit "ces_sequence_outputs_total" labels 0)
                (emit "ces_deprecated_fires_total" labels 0)
                (unless (gethash mid seen-steps)
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
  (cond
    ((jarray-p (jget body "vector"))
     (numbers-from-json (jget body "vector")))
    ((jarray-p (jget body "sparseVector"))
     (let ((length (reality-state-dimension state)))
       (dolist (entry (jarray-list (jget body "sparseVector")))
         (setf length (max length (1+ (truncate (jnumber entry "index" 0))))))
       (let ((values (make-list length :initial-element 0.0d0)))
         (dolist (entry (jarray-list (jget body "sparseVector")))
           (setf (nth (truncate (jnumber entry "index" 0)) values)
                 (or (jnumber entry "value" 0.0d0) 0.0d0)))
         values)))
    ((jarray-p (jget body "domainVectors"))
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

(defun merge-operation-json (machine sequence output output-index)
  (let* ((values (output-vector-vector output))
         (governance (resolve-governance machine (sequence-id sequence) values))
         (deprecation (sequence-deprecation-json sequence))
         (out (obj "region" (region-json (mapping-output (machine-mapping machine)))
                   "machineId" (machine-id machine)
                   "sequenceId" (sequence-id sequence)
                   "outputIndex" output-index
                   "values" (vectorize values)
                   "provenance" (vectorize (output-vector-provenance output)))))
    (when governance
      (setf (jget out "governance") governance))
    (when deprecation
      (setf (jget out "deprecation") deprecation))
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

(defun apply-event-bus (state merge-batch)
  (let ((writes nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (operation merge-batch)
      (let* ((key (compose-key (jstring operation "machineId" "")
                               (jstring operation "sequenceId" "")))
             (subscriptions (gethash key (reality-state-event-bus-subscriptions state))))
        (dolist (subscription subscriptions)
          (let* ((bit (truncate (or (jnumber subscription "bitOffset" 0) 0)))
                 (dedup (format nil "~a|~a|~a|~a"
                                (jstring subscription "subscriberMachineId" "")
                                bit
                                (jstring operation "machineId" "")
                                (jstring operation "sequenceId" ""))))
            (unless (gethash dedup seen)
              (setf (gethash dedup seen) t)
              (push (obj "producerMachineId" (jstring operation "machineId" "")
                         "producerSequenceId" (jstring operation "sequenceId" "")
                         "subscriberMachineId" (jstring subscription "subscriberMachineId" "")
                         "bitOffset" bit
                         "value" 1.0d0
                         "provenance" (jget operation "provenance"))
                    writes)
              (setf (gethash bit (reality-state-latched-event-bits state)) t))))))
    (let ((sorted (sorted-event-bus-writes writes)))
      (dolist (write sorted)
        (let ((bit (truncate (or (jnumber write "bitOffset" 0) 0))))
          (ensure-space-length state (1+ bit))
          (setf (nth bit (reality-state-perceptual-space state)) (or (jnumber write "value" 1.0d0) 1.0d0))))
      sorted)))

(defun apply-latched-event-bits (state)
  (maphash (lambda (bit _)
             (declare (ignore _))
             (ensure-space-length state (1+ bit))
             (setf (nth bit (reality-state-perceptual-space state)) 1.0d0))
           (reality-state-latched-event-bits state)))

(defun process-perceptual-input (state input &key override include-machine-results include-perceptual-space compact)
  (ensure-space-length state (max (reality-state-dimension state) (length input)))
  (setf (reality-state-perceptual-space state)
        (append input (make-list (max 0 (- (reality-state-dimension state) (length input)))
                                 :initial-element 0.0d0)))
  (apply-latched-event-bits state)
  (let ((machine-results (make-hash-table :test #'equal))
        (merge-batch nil)
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
           (when include-machine-results
             (setf (gethash id machine-results) (transition-result-json result)))
           (push (obj "offset" (region-offset (mapping-input mapping))
                      "length" (region-length (mapping-input mapping))
                      "machineId" id
                      "type" "input")
                 active-regions)
           (when (jbool (transition-result-arbiter-metadata result) "shouldOutput" nil)
             (dolist (sequence-id (object-keys-sorted (transition-result-sequence-outputs result)))
               (let ((sequence (sequence-by-id machine sequence-id))
                     (outputs (gethash sequence-id (transition-result-sequence-outputs result)))
                     (index 0))
                 (dolist (output outputs)
                   (when sequence
                     (push (merge-operation-json machine sequence output index) merge-batch)
                     (incf index))))))))))
    (setf merge-batch (sorted-merge-operations merge-batch))
    ;; Paging + deprecation coverage is per-merge-entry; counted after sort
    ;; so the bump order matches AI/CPP exactly.
    (dolist (op merge-batch) (record-merge-coverage state op))
    (when compact
      (add-packed-merge-values state merge-batch))
    (dolist (operation merge-batch)
      (let ((region (make-region-from-json (jget operation "region")))
            (values (numbers-from-json (jget operation "values"))))
        (setf (reality-state-perceptual-space state)
              (merge-region (reality-state-perceptual-space state) region values))
        (push (obj "offset" (region-offset region)
                   "length" (region-length region)
                   "machineId" (jstring operation "machineId" "")
                   "type" "output")
              active-regions)))
    (let* ((event-bus (apply-event-bus state merge-batch))
           (step-number (reality-state-step-count state))
           (step (obj "success" t
                     "stepNumber" step-number
                     "timestamp" (now-ms)
                     "inputVector" (vectorize input)
                     "machineResults" (if include-machine-results machine-results (obj))
                     "mergeBatch" (vectorize merge-batch)
                     "eventBus" (vectorize event-bus)
                     "activeRegions" (vectorize (nreverse active-regions)))))
      (setf (reality-state-step-count state) (1+ step-number))
      (when include-perceptual-space
        (setf (jget step "perceptualSpace") (vectorize (reality-state-perceptual-space state))
              (jget step "perceptualSpaceIsDebugProjection") t))
      (record-history state step)
      step)))

(defun active-vectors-json (state)
  (let (rows)
    (maphash
     (lambda (machine-id machine)
       (dolist (sequence (machine-sequence-list machine))
         (maphash
          (lambda (_ vector)
            (declare (ignore _))
            (when (reality-vector-active-p vector)
              (push (obj "machineId" machine-id
                         "sequenceId" (sequence-id sequence)
                         "vector" (reality-vector-json vector))
                    rows)))
          (sequence-vectors sequence))))
     (reality-state-machines state))
    (vectorize (nreverse rows))))

(defun machine-graph-json (state)
  (let (nodes edges)
    (maphash
     (lambda (id machine)
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
             nodes))
     (reality-state-machines state))
    (obj "nodes" (vectorize (nreverse nodes))
         "edges" (vectorize (nreverse edges)))))

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
                                     (json-response (obj "status" "healthy" "timestamp" (now-ms) "version" "0.1.0-lsp"))))
   (make-route "GET" "/api/config" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response
                                     (actor-ask actor
                                                (lambda (state)
                                                  (obj "vectorDimension" (reality-state-dimension state)
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
                                                                "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))))))))
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
                                                             (obj "historyLimit" (reality-state-history-limit state)
                                                                  "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                  "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))))))))
   (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor (lambda (state) (obj "activeVectors" (active-vectors-json state)))))))
   (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                            (declare (ignore _ body))
                                            (let ((limit (parse-integer (or (gethash "limit" query) "0") :junk-allowed t)))
                                              (json-response
                                               (actor-ask actor
                                                          (lambda (state)
                                                            (obj "history" (vectorize (if (and limit (> limit 0))
                                                                                          (subseq (reality-state-history state)
                                                                                                  0 (min limit (length (reality-state-history state))))
                                                                                          (reality-state-history state))))))))))
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
                                                             (let ((result (obj "inputVector" (vectorize input)
                                                                                "timestamp" (now-ms)
                                                                                "outputs" (vectorize (nreverse outputs)))))
                                                               (record-history state (obj "type" "engine-process" "result" result))
                                                               (obj "result" result))))))))
   (make-route "GET" "/api/machines" (lambda (_ body query)
                                      (declare (ignore _ body query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (obj "machines" (vectorize
                                                                     (mapcar #'machine-json
                                                                             (object-values (reality-state-machines state))))))))))
   (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                          (declare (ignore body query))
                                          (let ((machine (actor-ask actor (lambda (state) (gethash (gethash "id" params) (reality-state-machines state)))))))
                                            (if machine
                                                (json-response (obj "machine" (machine-json machine :full t)))
                                                (error-response "Machine not found" 404)))))
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
                                                                                   (process-machine-input machine (numbers-from-json (jget body "inputVector"))))))))))
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
                                                                                   (numbers-from-json (jget body "inputVector"))))))))))
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
                                                              (let ((rows nil)
                                                                    (dir (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
                                                                (when (uiop:directory-exists-p dir)
                                                                  (dolist (path (uiop:directory-files dir "*.json"))
                                                                    (push (obj "filename" (file-namestring path)
                                                                               "name" (pathname-name path)
                                                                               "description" ""
                                                                               "version" "1.0.0"
                                                                               "metadata" (obj)
                                                                               "sequenceCount" 0)
                                                                          rows)))
                                                                (obj "machines" (vectorize (nreverse rows)))))))))
   (make-route "GET" "/api/machines/json/:name" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (handler-case
                                                     (json-response
                                                      (actor-ask actor
                                                                 (lambda (state)
                                                                   (let* ((name (gethash "name" params))
                                                                          (filename (if (uiop:string-suffix-p ".json" name) name (format nil "~a.json" name)))
                                                                          (path (merge-pathnames filename (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
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
                                                                                                             "perceptualSpace" (vectorize (reality-state-perceptual-space state))))))))
   (make-route "GET" "/api/perceptual-simulation/history" (lambda (_ body query)
                                                           (declare (ignore _ body query))
                                                           (json-response (actor-ask actor (lambda (state) (obj "history" (vectorize (reality-state-history state))))))))
   (make-route "POST" "/api/preception/diagnostic" (lambda (_ body query)
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
                                                            :override (or (jstring body "matchAlgorithmOverride" nil)
                                                                          (jstring body "matchAlgorithm" nil))
                                                            :include-machine-results (jbool body "includeMachineResults"
                                                                                            (if (jbool body "compact" nil)
                                                                                                nil
                                                                                                (reality-state-include-machine-results-p state)))
                                                            :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                             (reality-state-include-perceptual-space-p state))
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
                   (length (jarray-list (or (jget first-seq "vectors") #())))
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
                                       (json-response (obj "status" "healthy" "timestamp" (now-ms) "version" "0.1.0-lsp"))))
     (make-route "GET" "/api/config" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (state-json (lambda (state)
                                                     (obj "vectorDimension" (reality-state-dimension state)
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
                                                                   "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state)))))))
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
                                                                      "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state)))))))
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
                                                              (let ((input (numbers-from-json (jget body "vector")))
                                                                    (outputs nil))
                                                                (maphash (lambda (_ machine)
                                                                           (declare (ignore _))
                                                                           (let ((result (process-machine-input machine input)))
                                                                             (when (transition-result-machine-output result)
                                                                               (push (output-vector-json (transition-result-machine-output result)) outputs))))
                                                                         (reality-state-machines state))
                                                                (let ((result (obj "inputVector" (vectorize input)
                                                                                   "timestamp" (now-ms)
                                                                                   "outputs" (vectorize (nreverse outputs)))))
                                                                  (record-history state (obj "type" "engine-process" "result" result))
                                                                  (obj "result" result)))))))
     (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                               (declare (ignore _ body query))
                                               (state-json (lambda (state) (obj "history" (vectorize (reality-state-history state)))))))
     (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state) (obj "activeVectors" (active-vectors-json state))))))
     (make-route "GET" "/api/machines" (lambda (_ body query)
                                         (declare (ignore _ body query))
                                         (state-json (lambda (state)
                                                       (obj "machines" (vectorize
                                                                        (mapcar #'machine-json
                                                                                (object-values-sorted (reality-state-machines state)))))))))
     (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore body query))
                                             (let ((machine (actor-ask actor (lambda (state)
                                                                               (gethash (gethash "id" params)
                                                                                        (reality-state-machines state)))))))
                                               (if machine
                                                   (json-response (obj "machine" (machine-json machine :full t)))
                                                   (error-response "Machine not found" 404))))
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
                                                                                     (numbers-from-json (jget body "inputVector"))))))))))
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
                                                                (let ((rows nil)
                                                                      (dir (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
                                                                  (when (uiop:directory-exists-p dir)
                                                                    (dolist (path (uiop:directory-files dir "*.json"))
                                                                      (push (obj "filename" (file-namestring path)
                                                                                 "name" (pathname-name path)
                                                                                 "description" ""
                                                                                 "version" "1.0.0"
                                                                                 "metadata" (obj)
                                                                                 "sequenceCount" 0)
                                                                            rows)))
                                                                  (obj "machines" (vectorize (nreverse rows))))))))
     (make-route "GET" "/api/machines/json/:name" (lambda (params body query)
                                                   (declare (ignore body query))
                                                   (handler-case
                                                       (state-json (lambda (state)
                                                                     (let* ((name (gethash "name" params))
                                                                            (filename (if (uiop:string-suffix-p ".json" name)
                                                                                          name
                                                                                          (format nil "~a.json" name)))
                                                                            (path (merge-pathnames
                                                                                   filename
                                                                                   (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
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
                                                                                                              (obj "success" t))))))))
                                                                                 (if result (json-response result) (error-response "Checkpoint not found" 404)))))
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
                                                                                      (let ((vector (parse-reality-vector body)))
                                                                                        (setf (gethash (reality-vector-id vector)
                                                                                                       (sequence-vectors seq))
                                                                                              vector)
                                                                                        (obj "success" t "vector" (reality-vector-json vector)))))))))
                                                         (if result (json-response result) (error-response "Sequence not found" 404)))))
     (make-route "POST" "/api/machines/:id/process" (lambda (params body query)
                                                      (declare (ignore query))
                                                      (let ((result (actor-ask actor
                                                                               (lambda (state)
                                                                                 (let ((machine (gethash (gethash "id" params)
                                                                                                         (reality-state-machines state))))
                                                                                   (when machine
                                                                                     (transition-result-json
                                                                                      (process-machine-input machine (numbers-from-json (jget body "inputVector"))))))))))
                                                        (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "GET" "/api/machine-graph" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json #'machine-graph-json)))
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
                                                                               "perceptualSpace" (vectorize (reality-state-perceptual-space state)))))))
     (make-route "GET" "/api/perceptual-simulation/history" (lambda (_ body query)
                                                              (declare (ignore _ body query))
                                                              (state-json (lambda (state)
                                                                            (obj "history" (vectorize (reality-state-history state)))))))
     (make-route "POST" "/api/perceptual-simulation/configure/chunk" (lambda (_ body query)
                                                                       (declare (ignore _ query))
                                                                       (state-json (lambda (state)
                                                                                     (when (jbool body "reset" nil)
                                                                                       (setf (reality-state-sim-buffer state) nil))
                                                                                     (dolist (v (jarray-list (or (jget body "vectors") (arr))))
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
                                                              (let ((result (obj "inputVector" (vectorize (numbers-from-json (jget body "data")))
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
                                                                          "inputVector" (vectorize data)
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
                                                                           :override (or (jstring body "matchAlgorithmOverride" nil)
                                                                                         (jstring body "matchAlgorithm" nil))
                                                                           :include-machine-results (jbool body "includeMachineResults"
                                                                                                           (if (jbool body "compact" nil)
                                                                                                               nil
                                                                                                               (reality-state-include-machine-results-p state)))
                                                                           :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                                            (reality-state-include-perceptual-space-p state))
                                                                           :compact (or (jbool body "compact" nil)
                                                                                        (compact-query-p query)))))
                                                                (re-broadcast (obj "type" "step-result" "step" step))
                                                                step)
                                                              (obj "error" "Provide exactly one of: vector, sparseVector, domainVectors")))))))))))

(defun start-reality-service (&key (port 3299) (machine-dir "../RealityEngine_AI/examples/machines") (dimension 768))
  (let* ((state (make-reality-state-from-config :machine-dir machine-dir :dimension dimension))
         (actor (state-actor "reality-service" state)))
    (start-http-server port (reality-routes actor) :name "reality-engine-lsp"
                       :extra-dispatchers
                       (list (hunchentoot:create-prefix-dispatcher
                              "/api/engine/stream"
                              #'re-sse-stream-handler)))))

(defun start-reality-from-environment ()
  (start-reality-service :port (env-int "REALITY_ENGINE_PORT" 3299)
                         :machine-dir (env "MACHINES_DIR" "../RealityEngine_AI/examples/machines")
                         :dimension (env-int "VECTOR_DIMENSION" 768)))
