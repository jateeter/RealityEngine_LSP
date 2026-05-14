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
   :mapping-version 0))

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
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (assert-true (find "/api/governance/route" patterns :test #'string=)
                 "Reality routes should expose governance resolver"))
  (format t "~&RealityEngine_LSP core tests passed.~%")
  t)
