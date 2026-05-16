(defpackage #:reality-engine-lsp.tests
  (:use #:cl #:reality-engine-lsp))

(in-package #:reality-engine-lsp.tests)

(defun assert-true (value message)
  (unless value
    (error "Assertion failed: ~a" message)))

(defun assert-equal (expected actual message)
  (unless (equal expected actual)
    (error "Assertion failed: ~a expected=~s actual=~s" message expected actual)))

(defun assert-error (thunk message)
  (let ((raised nil))
    (handler-case
        (funcall thunk)
      (error () (setf raised t)))
    (unless raised
      (error "Assertion failed: ~a" message))))

(defun make-test-state (&optional (dimension 8))
  (reality-engine-lsp::make-reality-state
   :dimension dimension
   :machines (make-hash-table :test #'equal)
   :machine-dir "/tmp"
   :perceptual-space (make-list dimension :initial-element 0.0d0)
   :history nil
   :history-limit 25
   :include-machine-results-p t
   :include-perceptual-space-p t
   :vector-store (make-hash-table :test #'equal)
   :sequences (make-hash-table :test #'equal)
   :qdrant-url "http://localhost:4333"
   :collection-name "test"
   :started-at (reality-engine-lsp::now-ms)
   :event-bus-subscriptions (make-hash-table :test #'equal)
   :latched-event-bits (make-hash-table :test #'equal)
   :step-count 0
   :mapping-version 0
   :cov-matched    (make-hash-table :test #'equal)
   :cov-activated  (make-hash-table :test #'equal)
   :cov-outputs    (make-hash-table :test #'equal)
   :cov-steps      (make-hash-table :test #'equal)
   :cov-paging     (make-hash-table :test #'equal)
   :cov-deprecated (make-hash-table :test #'equal)))

(defun one-bit-machine (id sequence-id input-offset output-offset &key metadata value)
  (machine-from-json
   (reality-engine-lsp::obj
    "id" id
    "name" id
    "arbiterRule" "passthrough"
    "metadata" (or metadata (reality-engine-lsp::obj))
    "perceptualMapping" (reality-engine-lsp::obj
                         "input" (reality-engine-lsp::obj "offset" input-offset "length" 1)
                         "output" (reality-engine-lsp::obj "offset" output-offset "length" (length (or value (list 1)))))
    "sequences" (reality-engine-lsp::vectorize
                 (list
                  (reality-engine-lsp::obj
                   "id" sequence-id
                   "name" sequence-id
                   "vectors" (reality-engine-lsp::vectorize
                              (list
                               (reality-engine-lsp::obj
                                "id" (format nil "~a-vector" sequence-id)
                                "isInitial" t
                                "elements" (reality-engine-lsp::vectorize
                                            (list (reality-engine-lsp::obj "value" 1 "threshold" 0.5)))
                                "outputVectors" (reality-engine-lsp::vectorize
                                                 (list
                                                  (reality-engine-lsp::obj
                                                   "id" (format nil "~a-out" sequence-id)
                                                   "vector" (reality-engine-lsp::vectorize (or value (list 1)))))))))))))))

(defun compose-metadata (&rest subscriptions)
  (reality-engine-lsp::obj
   "compose" (reality-engine-lsp::obj
              "subscriptions" (reality-engine-lsp::vectorize subscriptions))))

(defun compose-subscription (producer-machine-id producer-sequence-id bit-offset)
  (reality-engine-lsp::obj
   "producerMachineId" producer-machine-id
   "producerSequenceId" producer-sequence-id
   "bitOffset" bit-offset))

(defun first-merge (step)
  (aref (reality-engine-lsp::jget step "mergeBatch") 0))

(defun sta-fixture (&key life-safety clean)
  (reality-engine-lsp::obj
   "version" "1.0.0"
   "machine" (reality-engine-lsp::obj
              "id" (if life-safety "sta-life" "sta-routine")
              "name" (if life-safety "STA Life" "STA Routine")
              "metadata" (if life-safety
                             (reality-engine-lsp::obj "severity" "life-safety")
                             (reality-engine-lsp::obj "severity" "routine"))
              "arbiterRule" "passthrough"
              "perceptualMapping" (reality-engine-lsp::obj
                                   "input" (reality-engine-lsp::obj "offset" 0 "length" 2)
                                   "output" (reality-engine-lsp::obj "offset" 2 "length" 1))
              "sequences" (reality-engine-lsp::vectorize
                           (list
                            (reality-engine-lsp::obj
                             "id" "sta-seq"
                             "name" "STA Sequence"
                             "vectors" (reality-engine-lsp::vectorize
                                        (list
                                         (reality-engine-lsp::obj
                                          "id" "sta-a"
                                          "isInitial" t
                                          "elements" (reality-engine-lsp::vectorize
                                                      (list
                                                       (reality-engine-lsp::obj "value" 0 "threshold" 0.5)
                                                       (reality-engine-lsp::obj "value" 0 "threshold" 0.5)))
                                          "nextVectorIds" (reality-engine-lsp::vectorize (list "sta-b")))
                                         (reality-engine-lsp::obj
                                          "id" "sta-b"
                                          "isInitial" nil
                                          "elements" (reality-engine-lsp::vectorize
                                                      (list
                                                       (reality-engine-lsp::obj "value" 1 "threshold" 0.5)
                                                       (reality-engine-lsp::obj "value" (if clean 0 1) "threshold" 0.5)))
                                          "outputVectors" (reality-engine-lsp::vectorize
                                                           (list
                                                            (reality-engine-lsp::obj
                                                             "id" "sta-out"
                                                             "vector" (reality-engine-lsp::vectorize (list 1))))))))))))))

(defun run-tests ()
  (let* ((machine-json (reality-engine-lsp::obj
                       "id" "machine-test"
                       "name" "Test"
                       "arbiterRule" "passthrough"
                       "perceptualMapping" (reality-engine-lsp::obj
                                            "input" (reality-engine-lsp::obj "offset" 0 "length" 1)
                                            "output" (reality-engine-lsp::obj "offset" 1 "length" 1))
                       "sequences" (reality-engine-lsp::vectorize
                                    (list
                                     (reality-engine-lsp::obj
                                      "id" "seq"
                                      "name" "Seq"
                                      "vectors" (reality-engine-lsp::vectorize
                                                 (list
                                                  (reality-engine-lsp::obj
                                                   "id" "v1"
                                                   "isInitial" t
                                                   "elements" (reality-engine-lsp::vectorize
                                                               (list (reality-engine-lsp::obj "value" 1)))
                                                   "outputVectors" (reality-engine-lsp::vectorize
                                                                    (list
                                                                     (reality-engine-lsp::obj
                                                                      "id" "out"
                                                                      "vector" (reality-engine-lsp::vectorize (list 1)))))))))))))
         (machine (machine-from-json machine-json))
         (result (process-machine-input machine (list 1))))
    (assert-true (reality-engine-lsp::transition-result-machine-output result)
                 "machine output should fire on matching initial vector"))
  (let* ((report (reality-engine-lsp::compute-sta-report (sta-fixture :life-safety t :clean nil)))
         (summary (reality-engine-lsp::jget report "summary")))
    (assert-true (reality-engine-lsp::jbool report "lifeSafety" nil)
                 "STA report should mark life-safety machines")
    (assert-equal 1 (reality-engine-lsp::jnumber summary "intraViolations" nil)
                  "STA report should count HD>1 intra-sequence violations"))
  (assert-error (lambda () (machine-from-json (sta-fixture :life-safety t :clean nil)))
                "life-safety machine with STA violation should be rejected")
  (assert-true (machine-from-json (sta-fixture :life-safety t :clean t))
               "clean life-safety machine should load")
  (assert-true (machine-from-json (sta-fixture :life-safety nil :clean nil))
               "non-life-safety machine with STA violation should load")
  (let ((state (make-test-state 8)))
    (reality-engine-lsp::put-machine state (one-bit-machine "machine-z" "seq-z" 0 5))
    (reality-engine-lsp::put-machine state (one-bit-machine "machine-a" "seq-a" 0 6))
    (let* ((step (reality-engine-lsp::process-perceptual-input state (list 1)
                                                            :include-machine-results t
                                                            :include-perceptual-space t))
           (batch (coerce (reality-engine-lsp::jget step "mergeBatch") 'list)))
      (assert-equal (list "machine-a" "machine-z")
                    (mapcar (lambda (op) (reality-engine-lsp::jstring op "machineId" "")) batch)
                    "mergeBatch should be sorted by machineId")
      (assert-equal (list 1) (reality-engine-lsp::numbers-from-json (reality-engine-lsp::jget (first batch) "values"))
                    "mergeBatch should carry output values")
      (assert-equal (list "seq-a-vector")
                    (coerce (reality-engine-lsp::jget (first batch) "provenance") 'list)
                    "mergeBatch should carry provenance")))
  (let* ((metadata (reality-engine-lsp::obj
                    "governance" (reality-engine-lsp::obj
                                  "ownerTeam" "machine-team"
                                  "runbook" "https://runbook"
                                  "escalationPolicy" "pager"
                                  "sla" (reality-engine-lsp::obj "error" 60)
                                  "contact" (reality-engine-lsp::obj "primary" "primary@example.org"))
                    "triggerConfig" (reality-engine-lsp::obj
                                     "rules" (reality-engine-lsp::vectorize
                                              (list
                                               (reality-engine-lsp::obj
                                                "sequenceId" "seq-gov"
                                                "outputMatches" (reality-engine-lsp::vectorize (list 4 3))
                                                "ragStatusCode" "RED"
                                                "processStatus" "error"
                                                "description" "page"
                                                "governance" (reality-engine-lsp::obj
                                                              "ownerTeam" "rule-team"
                                                              "slaSeconds" 30)))))))
         (state (make-test-state 8)))
    (reality-engine-lsp::put-machine state
                                     (one-bit-machine "machine-gov" "seq-gov" 0 2
                                                      :metadata metadata
                                                      :value (list 4 3)))
    (let* ((op (first-merge (reality-engine-lsp::process-perceptual-input
                             state (list 1)
                             :include-machine-results t
                             :include-perceptual-space t)))
           (governance (reality-engine-lsp::jget op "governance")))
      (assert-equal "RED" (reality-engine-lsp::jstring governance "ragStatusCode" "")
                    "governance ragStatusCode should be stamped")
      (assert-equal "rule-team" (reality-engine-lsp::jstring governance "ownerTeam" "")
                    "rule governance should override machine owner")
      (assert-equal 30 (reality-engine-lsp::jnumber governance "slaSeconds" nil)
                    "rule SLA should override machine SLA")
      (assert-equal "https://runbook" (reality-engine-lsp::jstring governance "runbook" "")
                    "machine runbook should fill PagingDecision")))
  (let* ((metadata (reality-engine-lsp::obj
                    "governance" (reality-engine-lsp::obj
                                  "ownerTeam" "machine-team"
                                  "runbook" "https://warning-runbook"
                                  "sla" (reality-engine-lsp::obj "warning" 1800))
                    "triggerConfig" (reality-engine-lsp::obj
                                     "rules" (reality-engine-lsp::vectorize
                                              (list
                                               (reality-engine-lsp::obj
                                                "sequenceId" "seq-warn"
                                                "outputMatches" (reality-engine-lsp::vectorize (list 2 2))
                                                "ragStatusCode" "AMBER"
                                                "processStatus" "warning"))))))
         (machine (one-bit-machine "machine-warn" "seq-warn" 0 2
                                   :metadata metadata
                                   :value (list 2 2)))
         (decision (reality-engine-lsp::resolve-governance machine "seq-warn" (list 2 2))))
    (assert-equal "machine-team" (reality-engine-lsp::jstring decision "ownerTeam" "")
                  "machine governance should supply ownerTeam")
    (assert-equal 1800 (reality-engine-lsp::jnumber decision "slaSeconds" nil)
                  "machine SLA should be selected by processStatus")
    (assert-equal "AMBER" (reality-engine-lsp::jstring decision "ragStatusCode" "")
                  "RAG status should come from trigger rule"))
  (let* ((metadata (reality-engine-lsp::obj
                    "triggerConfig" (reality-engine-lsp::obj
                                     "rules" (reality-engine-lsp::vectorize
                                              (list
                                               (reality-engine-lsp::obj
                                                "sequenceId" "seq-legacy"
                                                "outputMatches" (reality-engine-lsp::vectorize (list 1))
                                                "ragStatusCode" "GREEN"
                                                "processStatus" "info"))))))
         (machine (one-bit-machine "machine-legacy" "seq-legacy" 0 2 :metadata metadata))
         (decision (reality-engine-lsp::resolve-governance machine "seq-legacy" (list 1))))
    (assert-equal "unrouted" (reality-engine-lsp::jstring decision "ownerTeam" "")
                  "legacy rule-only machine should fall back to unrouted")
    (assert-equal "machine-fallback" (reality-engine-lsp::jstring decision "source" "")
                  "legacy rule-only machine should report fallback source"))
  (let* ((machine (one-bit-machine "machine-dep" "seq-dep" 0 2 :value (list 1)))
         (sequence (gethash "seq-dep" (reality-engine-lsp::machine-sequences machine)))
         (state (make-test-state 8)))
    (setf (reality-engine-lsp::sequence-schema-version sequence) "1.0.0"
          (reality-engine-lsp::sequence-deprecated-at sequence) "2026-02-01"
          (reality-engine-lsp::sequence-replaced-by sequence) "seq-new")
    (reality-engine-lsp::put-machine state machine)
    (let* ((op (first-merge (reality-engine-lsp::process-perceptual-input
                             state (list 1)
                             :include-machine-results t
                             :include-perceptual-space t)))
           (deprecation (reality-engine-lsp::jget op "deprecation")))
      (assert-equal "2026-02-01" (reality-engine-lsp::jstring deprecation "since" "")
                    "deprecated sequence should stamp since")
      (assert-equal "seq-new" (reality-engine-lsp::jstring deprecation "replacedBy" "")
                    "deprecated sequence should stamp replacement")
      (assert-true (> (reality-engine-lsp::jnumber deprecation "ageDays" 0) 0)
                   "deprecated sequence should stamp ageDays")))
  (let* ((state (make-test-state 12))
         (agent-meta (compose-metadata
                      (compose-subscription "producer" "producer-seq" 1))))
    (reality-engine-lsp::put-machine state (one-bit-machine "agent" "agent-seq" 1 4 :metadata agent-meta))
    (reality-engine-lsp::put-machine state (one-bit-machine "producer" "producer-seq" 0 3))
    (assert-equal 1 (reality-engine-lsp::event-bus-subscription-count state)
                  "compose subscription count should include registered metadata")
    (let* ((step-1 (reality-engine-lsp::process-perceptual-input
                    state (list 1 0)
                    :include-machine-results t
                    :include-perceptual-space t))
           (event-bus (coerce (reality-engine-lsp::jget step-1 "eventBus") 'list)))
      (assert-equal 1 (length event-bus) "compose producer should emit one event-bus write")
      (assert-equal 1 (reality-engine-lsp::jnumber (first event-bus) "bitOffset" nil)
                    "compose write should target subscriber bit"))
    (let* ((step-2 (reality-engine-lsp::process-perceptual-input
                    state (list 0 0)
                    :include-machine-results t
                    :include-perceptual-space t))
           (batch (coerce (reality-engine-lsp::jget step-2 "mergeBatch") 'list)))
      (assert-true (find "agent-seq" batch
                         :key (lambda (op) (reality-engine-lsp::jstring op "sequenceId" ""))
                         :test #'string=)
                   "latched compose bit should let subscriber fire on the next step")))
  (let* ((state (make-test-state 32))
         (agent-meta (compose-metadata
                      (compose-subscription "producer-c" "producer-c-seq" 9)
                      (compose-subscription "producer-a" "producer-a-seq" 7)
                      (compose-subscription "producer-b" "producer-b-seq" 8)
                      (compose-subscription "producer-a" "producer-a-seq" 7))))
    (reality-engine-lsp::put-machine state (one-bit-machine "agent-sort" "agent-sort-seq" 7 23 :metadata agent-meta))
    (reality-engine-lsp::put-machine state (one-bit-machine "producer-c" "producer-c-seq" 2 22))
    (reality-engine-lsp::put-machine state (one-bit-machine "producer-a" "producer-a-seq" 0 20))
    (reality-engine-lsp::put-machine state (one-bit-machine "producer-b" "producer-b-seq" 1 21))
    (assert-equal 4 (reality-engine-lsp::event-bus-subscription-count state)
                  "compose subscription count should include duplicate declarations")
    (let* ((step (reality-engine-lsp::process-perceptual-input
                  state (list 1 1 1)
                  :include-machine-results t
                  :include-perceptual-space t))
           (event-bus (coerce (reality-engine-lsp::jget step "eventBus") 'list)))
      (assert-equal (list 7 8 9)
                    (mapcar (lambda (write) (reality-engine-lsp::jnumber write "bitOffset" nil))
                            event-bus)
                    "eventBus writes should be deduped and sorted by subscriber/bit/producer/sequence")
      (assert-equal (list "producer-a" "producer-b" "producer-c")
                    (mapcar (lambda (write) (reality-engine-lsp::jstring write "producerMachineId" ""))
                            event-bus)
                    "eventBus writes should preserve producer identity")))
  (let* ((state (make-test-state 16))
         (agent-meta (compose-metadata
                      (compose-subscription "producer-reset" "producer-reset-seq" 5))))
    (reality-engine-lsp::put-machine state (one-bit-machine "agent-reset" "agent-reset-seq" 5 8 :metadata agent-meta))
    (reality-engine-lsp::put-machine state (one-bit-machine "producer-reset" "producer-reset-seq" 0 7))
    (reality-engine-lsp::process-perceptual-input state (list 1)
                                                  :include-machine-results t
                                                  :include-perceptual-space t)
    (assert-equal 1 (hash-table-count (reality-engine-lsp::reality-state-latched-event-bits state))
                  "compose writes should latch event bits")
    (reality-engine-lsp::reset-reality-state state)
    (assert-equal 0 (hash-table-count (reality-engine-lsp::reality-state-latched-event-bits state))
                  "reset should clear latched compose bits"))
  (let* ((cells (list 0 1 2 3 0 1 2 3 2))
         (packed (reality-engine-lsp::pack-cells cells 2)))
    (assert-equal 3 (length packed) "2-bit cells should pack into three bytes")
    (assert-equal 27 (aref packed 0) "2-bit packing should use the Option A1 MSB-first layout")
    (assert-equal cells (reality-engine-lsp::unpack-cells packed (length cells) 2)
                  "2-bit cells should round-trip")
    (assert-equal "Gw==" (reality-engine-lsp::encode-packed-base64 (list 0 1 2 3) 2)
                  "single packed byte should base64 encode identically to AI")
    (let ((footprint (reality-engine-lsp::storage-footprint 4128 2)))
      (assert-equal 33024 (reality-engine-lsp::jnumber footprint "float64Bytes" nil)
                    "float64 footprint should remain engine-native")
      (assert-equal 1032 (reality-engine-lsp::jnumber footprint "packedBytes" nil)
                    "packed footprint should use declared cell width")
      (assert-equal 32 (reality-engine-lsp::jnumber footprint "shrinkFactor" nil)
                    "2-bit storage shrink factor should match AI"))
    (assert-error (lambda () (reality-engine-lsp::pack-cells (list 4) 2))
                  "out-of-range 2-bit cells should be rejected"))
  (let* ((machine (machine-from-json
                   (reality-engine-lsp::obj
                    "id" "packed-loader"
                    "name" "Packed Loader"
                    "arbiterRule" "passthrough"
                    "perceptualMapping" (reality-engine-lsp::obj
                                         "input" (reality-engine-lsp::obj "offset" 0 "length" 1)
                                         "output" (reality-engine-lsp::obj "offset" 1 "length" 4)
                                         "bitsPerElement" 2)
                    "sequences" (reality-engine-lsp::vectorize nil))))
         (mapping (reality-engine-lsp::machine-mapping machine))
         (mapping-json (reality-engine-lsp::mapping-json mapping)))
    (assert-equal 2 (reality-engine-lsp::mapping-bits-per-element mapping)
                  "loader should preserve perceptualMapping.bitsPerElement")
    (assert-equal 2 (reality-engine-lsp::jnumber mapping-json "bitsPerElement" nil)
                  "mapping JSON should expose bitsPerElement"))
  (let* ((state (make-test-state 8))
         (machine (machine-from-json
                   (reality-engine-lsp::obj
                    "id" "packed-machine"
                    "name" "Packed Machine"
                    "arbiterRule" "passthrough"
                    "perceptualMapping" (reality-engine-lsp::obj
                                         "input" (reality-engine-lsp::obj "offset" 0 "length" 1)
                                         "output" (reality-engine-lsp::obj "offset" 1 "length" 4)
                                         "bitsPerElement" 2)
                    "sequences" (reality-engine-lsp::vectorize
                                 (list
                                  (reality-engine-lsp::obj
                                   "id" "packed-seq"
                                   "name" "Packed Sequence"
                                   "vectors" (reality-engine-lsp::vectorize
                                              (list
                                               (reality-engine-lsp::obj
                                                "id" "packed-start"
                                                "isInitial" t
                                                "elements" (reality-engine-lsp::vectorize
                                                            (list (reality-engine-lsp::obj "value" 1 "threshold" 0.5)))
                                                "outputVectors" (reality-engine-lsp::vectorize
                                                                 (list
                                                                  (reality-engine-lsp::obj
                                                                   "id" "packed-out"
                                                                   "vector" (reality-engine-lsp::vectorize
                                                                             (list 0 1 2 3)))))))))))))))
    (reality-engine-lsp::put-machine state machine)
    (let* ((step (reality-engine-lsp::process-perceptual-input
                  state (list 1)
                  :include-machine-results t
                  :include-perceptual-space t
                  :compact t))
           (packed (reality-engine-lsp::jget (first-merge step) "valuesPacked"))
           (footprint (reality-engine-lsp::storage-footprint-json state)))
      (assert-equal "Gw==" (reality-engine-lsp::jstring packed "base64" "")
                    "compact merge batch should include packed base64 values")
      (assert-equal 2 (reality-engine-lsp::jnumber packed "bitsPerElement" nil)
                    "compact merge batch should use machine mapping bitsPerElement")
      (assert-equal 4 (reality-engine-lsp::jnumber packed "length" nil)
                    "compact merge batch should preserve unpacked length")
      (assert-equal 1 (reality-engine-lsp::jnumber (reality-engine-lsp::jget footprint "widthHistogram") "2" nil)
                    "storage footprint histogram should count 2-bit machines")
      (assert-equal 2 (reality-engine-lsp::jnumber footprint "totalPackedBytes" nil)
                    "storage footprint should total packed bytes")))
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (assert-true (find "/api/governance/route" patterns :test #'string=)
                 "Reality routes should expose governance resolver"))
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (assert-true (find "/api/runtime/storage-footprint" patterns :test #'string=)
                 "Reality routes should expose storage footprint resolver"))

  ;; /api/metrics Prometheus text-format emission — verifies cross-runtime
  ;; parity with AI/CPP.  Every metric line must carry runtime="lsp" and the
  ;; canonical metric names (ces_*, re_runtime_*) must all be present.
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (assert-true (find "/api/metrics" patterns :test #'string=)
                 "Reality routes should expose /api/metrics Prometheus endpoint"))
  (let* ((state (make-test-state 8))
         (text (reality-engine-lsp::prometheus-text-of state "lsp")))
    (assert-true (search "runtime=\"lsp\"" text)
                 "Prometheus emission should stamp runtime=\"lsp\"")
    (dolist (name '("ces_machines_total"
                    "ces_sequences_total"
                    "ces_vectors_total"
                    "ces_vector_matched_total"
                    "ces_vector_activated_total"
                    "ces_sequence_outputs_total"
                    "ces_machine_steps_total"
                    "ces_paging_decisions_total"
                    "ces_deprecated_fires_total"
                    "ces_unfired_sequences"
                    "ces_unfired_vectors"
                    "ces_machine_sequence_count"
                    "ces_machine_vector_count"
                    "ces_registry_uptime_seconds"
                    "re_runtime_dimension"
                    "re_runtime_required_dimension"
                    "re_runtime_mapping_version"))
      (assert-true (search (format nil "# HELP ~a " name) text)
                   (format nil "Prometheus emission should declare ~a" name))))

  ;; ── Sensor TTL parity with C++ ─────────────────────────────────────────
  ;; A stale sensor (last-updated older than ttl-ms) should drop out of the
  ;; assembled perception vector and report stale=true in source-json.
  (let* ((engine (reality-engine-lsp::make-perception-engine-state 4))
         (now (reality-engine-lsp::now-ms))
         (fresh (reality-engine-lsp::make-source
                 :id "s-fresh" :kind "sensor" :name "fresh"
                 :active-p t
                 :region (reality-engine-lsp::make-region :offset 0 :length 1)
                 :sensor-id "fresh-sid"
                 :last-value (list 0.7d0)
                 :last-updated (- now 1000) :ttl-ms 5000))
         (stale (reality-engine-lsp::make-source
                 :id "s-stale" :kind "sensor" :name "stale"
                 :active-p t
                 :region (reality-engine-lsp::make-region :offset 1 :length 1)
                 :sensor-id "stale-sid"
                 :last-value (list 0.9d0)
                 :last-updated (- now 60000) :ttl-ms 5000)))
    (reality-engine-lsp::ensure-source-id engine fresh)
    (reality-engine-lsp::ensure-source-id engine stale)
    (let ((vec (reality-engine-lsp::assemble-perception-vector engine)))
      (assert-equal 0.7d0 (nth 0 vec) "fresh sensor contributes its value")
      (assert-equal 0.0d0 (nth 1 vec) "stale sensor contributes zero"))
    (let ((js (reality-engine-lsp::source-json stale)))
      (assert-true (reality-engine-lsp::jbool js "stale" nil) "stale sensor reports stale=true in JSON")
      (assert-true (> (reality-engine-lsp::jnumber js "ageMs" 0) 0) "stale sensor reports positive ageMs")))

  ;; ── MQTT mapping registry parity ──────────────────────────────────────
  (let* ((json-text "{\"mappings\":[
                       {\"id\":\"zone-temp\",
                        \"topicFilter\":\"sensors/zone/+/temp\",
                        \"sensorIdTemplate\":\"zone.{1}.temp\",
                        \"region\":{\"offset\":0,\"length\":1},
                        \"extract\":{\"type\":\"json\",\"pointer\":\"/value\"},
                        \"normalize\":{\"mode\":\"minmax\",\"min\":0,\"max\":100,\"clamp\":true},
                        \"ttlMs\":15000,
                        \"pushMode\":\"immediate\"}]}")
         (parsed (reality-engine-lsp::parse-json json-text))
         (registry (reality-engine-lsp::mqtt-mapping-registry-from-json parsed)))
    (assert-equal 1 (length (reality-engine-lsp::mqtt-mapping-registry-rules registry))
                  "one mapping rule parsed")
    (let* ((match (reality-engine-lsp::mqtt-match-topic registry "sensors/zone/3/temp")))
      (assert-true match "topic matched against wildcard filter")
      (when match
        (let* ((rule-index (car match))
               (captures (cdr match))
               (rule (aref (reality-engine-lsp::mqtt-mapping-registry-rules registry) rule-index))
               (sid (reality-engine-lsp::mqtt-resolve-sensor-id rule "sensors/zone/3/temp" captures)))
          (assert-equal "zone.3.temp" sid "sensorId template interpolated capture"))))

    ;; Decode end-to-end: JSON payload {"value":50} → minmax(0,100) → 0.5
    (let ((rule (aref (reality-engine-lsp::mqtt-mapping-registry-rules registry) 0)))
      (multiple-value-bind (values err) (reality-engine-lsp::mqtt-decode rule "{\"value\":50}")
        (assert-true (null err) (format nil "decode succeeded; got error: ~a" err))
        (assert-equal 1 (length values) "decode returned one value")
        (when values
          (assert-equal 0.5d0 (coerce (car values) 'double-float)
                        "JSON 50 → minmax(0,100) → 0.5"))))

    ;; Bad payload (missing pointer) is rejected.
    (let ((rule (aref (reality-engine-lsp::mqtt-mapping-registry-rules registry) 0)))
      (multiple-value-bind (values err) (reality-engine-lsp::mqtt-decode rule "{\"missing\":1}")
        (assert-true (and (null values) err) "missing JSON pointer rejected")))

    ;; Length mismatch is rejected — a JSON array with 2 elements into a
    ;; 1-wide region should fail length validation.
    (let* ((multi-json "{\"mappings\":[{\"id\":\"a\",\"topicFilter\":\"x\",
                          \"region\":{\"offset\":0,\"length\":1},
                          \"extract\":{\"type\":\"csv-float\"}}]}")
           (multi-reg (reality-engine-lsp::mqtt-mapping-registry-from-json
                       (reality-engine-lsp::parse-json multi-json)))
           (rule (aref (reality-engine-lsp::mqtt-mapping-registry-rules multi-reg) 0)))
      (multiple-value-bind (values err) (reality-engine-lsp::mqtt-decode rule "1,2")
        (assert-true (and (null values) err) "length mismatch rejected"))))

  ;; ── Overlap detection ─────────────────────────────────────────────────
  (let* ((json-text "{\"mappings\":[
                       {\"id\":\"a\",\"topicFilter\":\"x\",\"region\":{\"offset\":0,\"length\":3},\"extract\":{\"type\":\"csv-float\"}},
                       {\"id\":\"b\",\"topicFilter\":\"y\",\"region\":{\"offset\":2,\"length\":2},\"extract\":{\"type\":\"csv-float\"}}]}")
         (registry (reality-engine-lsp::mqtt-mapping-registry-from-json
                    (reality-engine-lsp::parse-json json-text)))
         (warnings (reality-engine-lsp::mqtt-validate-overlaps registry nil)))
    (assert-equal 1 (length warnings) "overlapping regions reported as one warning")
    (assert-true (null (reality-engine-lsp::mqtt-validate-overlaps registry t))
                 "allow-overlap suppresses warnings"))

  ;; ── MQTT bridge in-process dispatcher ─────────────────────────────────
  ;; Drives the full extract → normalize → ingest pipeline with no broker
  ;; via mqtt-bridge-inject-message — same hatch the CPP and AI tests use.
  (let* ((reg-json "{\"mappings\":[{
                      \"id\":\"zone-temp\",
                      \"topicFilter\":\"sensors/zone/+/temp\",
                      \"sensorIdTemplate\":\"zone.{1}.temp\",
                      \"region\":{\"offset\":0,\"length\":1},
                      \"extract\":{\"type\":\"json\",\"pointer\":\"/value\"},
                      \"normalize\":{\"mode\":\"minmax\",\"min\":0,\"max\":100,\"clamp\":true},
                      \"ttlMs\":15000,
                      \"pushMode\":\"immediate\"}]}")
         (registry (reality-engine-lsp::mqtt-mapping-registry-from-json
                    (reality-engine-lsp::parse-json reg-json)))
         (config (reality-engine-lsp::make-mqtt-client-config :broker-host "127.0.0.1"
                                                              :broker-port 1
                                                              :client-id "test"))
         (ingested-sensors nil)
         (ingested-values nil)
         (ingested-ttls nil)
         (push-count 0)
         (bridge (reality-engine-lsp::make-mqtt-bridge
                  config registry
                  (lambda (sid offset length values ttl-ms topic mapping-id)
                    (declare (ignore offset length topic mapping-id))
                    (push sid ingested-sensors)
                    (push values ingested-values)
                    (push ttl-ms ingested-ttls))
                  (lambda () (incf push-count)))))
    ;; Don't start() — we drive synthetically via inject-message.
    (reality-engine-lsp::mqtt-bridge-inject-message bridge
                                                    "sensors/zone/3/temp"
                                                    "{\"value\":50}")
    (assert-equal 1 (length ingested-sensors) "exactly one ingest after one valid message")
    (when ingested-sensors
      (assert-equal "zone.3.temp" (car ingested-sensors) "sensor-id resolved via template")
      (assert-equal 0.5d0 (coerce (car (car ingested-values)) 'double-float)
                    "JSON 50 → minmax(0,100) → 0.5"))
    (assert-equal 15000 (car ingested-ttls) "TTL carried from mapping rule")
    (assert-equal 1 push-count "immediate push mode fired exactly once")

    ;; Bad payload — length validation rejects.  Counters should reflect it.
    (reality-engine-lsp::mqtt-bridge-inject-message bridge
                                                    "sensors/zone/3/temp"
                                                    "{\"missing\":1}")
    (assert-equal 1 (length ingested-sensors) "rejected payload is not ingested")
    (let ((stats (reality-engine-lsp::mqtt-bridge-stats-snapshot bridge)))
      (assert-true (>= (cdr (assoc "messagesRejected" stats :test #'string=)) 1)
                   "bridge counts the rejected message"))

    ;; Unmatched topic — separate counter, no ingest.
    (reality-engine-lsp::mqtt-bridge-inject-message bridge "unknown/topic" "")
    (let ((stats (reality-engine-lsp::mqtt-bridge-stats-snapshot bridge)))
      (assert-true (>= (cdr (assoc "messagesUnmatched" stats :test #'string=)) 1)
                   "bridge counts the unmatched topic")))

  ;; ── MQTT bridge fan-out: one message → many rules ─────────────────────
  (let* ((reg-json "{\"mappings\":[
                      {\"id\":\"temp\",\"topicFilter\":\"s/x/v1\",
                       \"region\":{\"offset\":0,\"length\":1},
                       \"extract\":{\"type\":\"json\",\"pointer\":\"/t\"},
                       \"normalize\":{\"mode\":\"passthrough\",\"clamp\":false}},
                      {\"id\":\"humid\",\"topicFilter\":\"s/x/v1\",
                       \"region\":{\"offset\":1,\"length\":1},
                       \"extract\":{\"type\":\"json\",\"pointer\":\"/h\"},
                       \"normalize\":{\"mode\":\"passthrough\",\"clamp\":false}}]}")
         (registry (reality-engine-lsp::mqtt-mapping-registry-from-json
                    (reality-engine-lsp::parse-json reg-json)))
         (config (reality-engine-lsp::make-mqtt-client-config :broker-host "127.0.0.1"
                                                              :broker-port 1
                                                              :client-id "test"))
         (ingested-mappings nil)
         (bridge (reality-engine-lsp::make-mqtt-bridge
                  config registry
                  (lambda (sid offset length values ttl-ms topic mapping-id)
                    (declare (ignore sid offset length values ttl-ms topic))
                    (push mapping-id ingested-mappings))
                  (lambda () nil))))
    (reality-engine-lsp::mqtt-bridge-inject-message bridge "s/x/v1" "{\"t\":0.25,\"h\":0.75}")
    (assert-equal 2 (length ingested-mappings) "one PUBLISH → two ingests on fan-out")
    (assert-true (and (find "temp" ingested-mappings :test #'string=)
                       (find "humid" ingested-mappings :test #'string=))
                 "both fan-out mappings dispatched"))

  (format t "~&RealityEngine_LSP core tests passed.~%")
  t)
