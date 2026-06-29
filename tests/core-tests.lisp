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

;; ── Cross-runtime parity helpers (AI + C++ + LSP must agree) ──────────────
;;
;; The same corpus walked by:
;;   RealityEngine_AI/src/__tests__/AiTriggerDispatch.test.ts
;;   RealityEngine_CPP/tests/e2e_ai_trigger_dispatch.cpp
;; reports 895/4480/3586/3586 — pinning the same counters here catches any
;; LSP-side drift in process-machine-input or resolve-governance.  And the
;; Yuma 3-tick cascade (AGX051→AGX055→AgYieldOptimizationAI) asserts the
;; same mergeBatch shapes both other runtimes already enforce.

(defparameter +ai-machines-dir+
  (or (probe-file "../RealityEngine_Machines/machines/")
      (probe-file "../../RealityEngine_Machines/machines/")
      (probe-file "../RealityEngine_AI/examples/machines/")
      (probe-file "../../RealityEngine_AI/examples/machines/")))

(defun reset-machine (machine)
  (dolist (sequence (reality-engine-lsp::machine-sequence-list machine))
    (reality-engine-lsp::reset-sequence sequence))
  machine)

(defun zero-region (list offset length)
  (loop for i from offset below (+ offset length)
        do (setf (nth i list) 0.0d0))
  list)

(defun find-merge-by-machine (batch machine-id)
  (find machine-id (coerce batch 'list)
        :key (lambda (op) (reality-engine-lsp::jstring op "machineId" ""))
        :test #'string=))

(defun envelope-for (machine sequence-id values)
  "Build the envelope-field bundle the dispatcher would emit, or NIL on miss.
   Mirrors envelopeFor in AiTriggerDispatch.test.ts."
  (let ((decision (reality-engine-lsp::resolve-governance machine sequence-id values)))
    (unless decision (return-from envelope-for nil))
    (let* ((md (reality-engine-lsp::machine-metadata machine))
           (binding (reality-engine-lsp::ces-dispatch-binding
                     md (reality-engine-lsp::vectorize values)))
           (write-back (reality-engine-lsp::jget binding "writeBack"))
           (sla (reality-engine-lsp::jget decision "slaSeconds")))
      (reality-engine-lsp::obj
       "agent"             (reality-engine-lsp::jstring binding "agent" "")
       "trigger"           (reality-engine-lsp::jstring binding "trigger" "")
       "autonomyMode"      (reality-engine-lsp::jstring binding "autonomyMode" "")
       "writeBackType"     (reality-engine-lsp::jstring write-back "type" "")
       "ragStatusCode"     (reality-engine-lsp::jstring decision "ragStatusCode" "")
       "processStatus"     (reality-engine-lsp::jstring decision "processStatus" "")
       "ownerTeam"         (reality-engine-lsp::jstring decision "ownerTeam" "")
       "slaSeconds"        (if (or (eq sla reality-engine-lsp::+json-null+) (null sla)) nil sla)))))

(defun walk-corpus-for-envelopes (machine-dir)
  "Auto-discover machines, replay each inputSequences[] entry, return a plist
   of (:machines :sequences :outputs :envelopes :failures).  Same skip rules
   as the AI + C++ counterparts: bypass machines without triggerConfig+
   agentBinding; skip baseline (expectedOutputCount=0) sequences;
   require SLA only for paging tiers (processStatus ∈ {error, warning})."
  (let ((machines 0) (sequences 0) (outputs 0) (envelopes 0) (failures nil))
    (dolist (path (sort (uiop:directory-files machine-dir "*.json") #'string< :key #'namestring))
      (let* ((raw (reality-engine-lsp::safe-read-file (namestring path)))
             (root (handler-case (reality-engine-lsp::parse-json raw)
                     (error (c) (push (format nil "~a: parse failed — ~a" (file-namestring path) c) failures)
                            nil))))
        (unless root (return))
        (let* ((machine-obj (reality-engine-lsp::jget root "machine"))
               (md (reality-engine-lsp::jget machine-obj "metadata"))
               (tc (reality-engine-lsp::jget md "triggerConfig"))
               (rules (and (reality-engine-lsp::jobject-p tc) (reality-engine-lsp::jget tc "rules")))
               (binding (reality-engine-lsp::ces-dispatch-binding md)))
          (when (and (reality-engine-lsp::jarray-p rules)
                     (> (length (reality-engine-lsp::jarray-list rules)) 0)
                     (reality-engine-lsp::jobject-p (reality-engine-lsp::jget md "agentBinding"))
                     (> (length (reality-engine-lsp::jstring binding "agent" "")) 0)
                     (> (length (reality-engine-lsp::jstring binding "trigger" "")) 0))
            (incf machines)
            (let ((machine (handler-case (reality-engine-lsp::load-machine-from-file path)
                             (error (c) (push (format nil "~a: load failed — ~a" (file-namestring path) c) failures)
                                    nil))))
              (when machine
                (let ((input-seqs (reality-engine-lsp::jget machine-obj "inputSequences")))
                  (when (reality-engine-lsp::jarray-p input-seqs)
                    (dolist (seq-json (reality-engine-lsp::jarray-list input-seqs))
                      (reset-machine machine)
                      (incf sequences)
                      (let* ((seq-name (reality-engine-lsp::jstring seq-json "name" "unnamed"))
                             (seq-meta (reality-engine-lsp::jget seq-json "metadata"))
                             (scenario (reality-engine-lsp::jstring seq-meta "scenario" ""))
                             (expected-count (reality-engine-lsp::jnumber seq-meta "expectedOutputCount" nil))
                             (vectors-json (reality-engine-lsp::jget seq-json "vectors")))
                        ;; Baseline sequences (expectedOutputCount: 0) intentionally do not fire.
                        ;; Same skip applied by the AI + C++ tests.  Use cond, NOT
                        ;; (return) — `return` from a dolist exits the entire loop and
                        ;; would drop the remaining sequences for the machine.
                        (cond
                          ((eql expected-count 0) nil)
                          ((not (reality-engine-lsp::jarray-p vectors-json))
                           (push (format nil "~a / ~a: missing input vectors" (file-namestring path) seq-name) failures))
                          (t
                        (let ((fired nil))
                          (dolist (vec-json (reality-engine-lsp::jarray-list vectors-json))
                            (let* ((input (reality-engine-lsp::numbers-from-json vec-json))
                                   (tr (reality-engine-lsp::process-machine-input machine input)))
                              (maphash
                               (lambda (sid outs)
                                 (dolist (ov outs)
                                   (push (cons sid (reality-engine-lsp::output-vector-vector ov)) fired)
                                   (incf outputs)))
                               (reality-engine-lsp::transition-result-sequence-outputs tr))))
                          (when (and expected-count (> expected-count 0)
                                     (/= (length fired) expected-count))
                            (push (format nil "~a / ~a: expected ~a output(s), got ~a"
                                          (file-namestring path) seq-name expected-count (length fired)) failures))
                          (let ((envelopes-this-run 0))
                            (dolist (pair fired)
                              (let ((env (envelope-for machine (car pair) (cdr pair))))
                                (when env
                                  (incf envelopes-this-run)
                                  (incf envelopes)
                                  (let ((where (format nil "~a / ~a / ~a" (file-namestring path) seq-name scenario))
                                        (ps (reality-engine-lsp::jstring env "processStatus" ""))
                                        (rs (reality-engine-lsp::jstring env "ragStatusCode" ""))
                                        (ot (reality-engine-lsp::jstring env "ownerTeam" ""))
                                        (agent (reality-engine-lsp::jstring env "agent" ""))
                                        (trigger (reality-engine-lsp::jstring env "trigger" ""))
                                        (mode (reality-engine-lsp::jstring env "autonomyMode" ""))
                                        (write-back-type (reality-engine-lsp::jstring env "writeBackType" ""))
                                        (sla (reality-engine-lsp::jget env "slaSeconds")))
                                    (when (zerop (length agent)) (push (format nil "~a: agentBinding.agent empty" where) failures))
                                    (when (zerop (length trigger)) (push (format nil "~a: agentBinding.trigger empty" where) failures))
                                    (unless (member mode '("observe" "advise" "supervised-act" "automated-act") :test #'string=)
                                      (push (format nil "~a: autonomyMode='~a' not in supported modes" where mode) failures))
                                    (when (zerop (length write-back-type))
                                      (push (format nil "~a: agentBinding.writeBack.type empty" where) failures))
                                    (unless (member rs '("RED" "AMBER" "GREEN") :test #'string=)
                                      (push (format nil "~a: ragStatusCode='~a' not in {RED,AMBER,GREEN}" where rs) failures))
                                    (unless (member ps '("error" "warning" "info" "ok") :test #'string=)
                                      (push (format nil "~a: processStatus='~a' not in {error,warning,info,ok}" where ps) failures))
                                    (when (or (zerop (length ot)) (string= ot "unrouted"))
                                      (push (format nil "~a: ownerTeam unrouted (governance not backfilled)" where) failures))
                                    (when (member ps '("error" "warning") :test #'string=)
                                      (unless (and sla (numberp sla) (> sla 0))
                                        (push (format nil "~a: paging tier '~a' has no slaSeconds — envelope would page with no contract" where ps) failures)))))))
                            (when (zerop envelopes-this-run)
                              (push (format nil "~a / ~a: no fired output matched any triggerConfig rule — envelope would be dropped"
                                            (file-namestring path) seq-name) failures))))))))))))))))
    (list :machines machines :sequences sequences :outputs outputs :envelopes envelopes
          :failures (nreverse failures))))

(defun merge-operation-for-pe-dispatch (machine sequence-id values)
  (let ((governance (reality-engine-lsp::resolve-governance machine sequence-id values)))
    (when governance
      (reality-engine-lsp::obj
       "region" (reality-engine-lsp::region-json
                 (reality-engine-lsp::mapping-output
                  (reality-engine-lsp::machine-mapping machine)))
       "machineId" (reality-engine-lsp::machine-id machine)
       "sequenceId" sequence-id
       "outputIndex" 0
       "values" (reality-engine-lsp::vectorize values)
       "governance" governance))))

(defun pe-record-for (state machine sequence-id values)
  (let ((operation (merge-operation-for-pe-dispatch machine sequence-id values)))
    (when operation
      (reality-engine-lsp::record-dispatch-envelope
       state
       operation
       (reality-engine-lsp::obj "name" (reality-engine-lsp::machine-name machine)
                                 "metadata" (reality-engine-lsp::machine-metadata machine))))))

(defun walk-corpus-through-pe-dispatch (machine-dir)
  "Replay the shared machine corpus through the LSP PE dispatch-ledger path.
This tests PE-owned bridge behavior: RE-style merge operations enter PE,
and PE records async dispatch envelopes without requiring live RE HTTP."
  (let ((state (reality-engine-lsp::make-perception-state-from-config
                :dimension 768
                :reality-url "http://localhost:3299"
                :localai-url "http://localhost:8000"
                :localai-machine-dir "../localAIStack/data/machines"))
        (machines 0) (sequences 0) (outputs 0) (records 0) (failures nil))
    (setf (reality-engine-lsp::perception-state-dispatch-ledger-limit state) 5000)
    (dolist (path (sort (uiop:directory-files machine-dir "*.json") #'string< :key #'namestring))
      (let* ((raw (reality-engine-lsp::safe-read-file (namestring path)))
             (root (handler-case (reality-engine-lsp::parse-json raw)
                     (error (c) (push (format nil "~a: parse failed — ~a" (file-namestring path) c) failures)
                            nil))))
        (unless root (return))
        (let* ((machine-obj (reality-engine-lsp::jget root "machine"))
               (md (reality-engine-lsp::jget machine-obj "metadata"))
               (tc (reality-engine-lsp::jget md "triggerConfig"))
               (rules (and (reality-engine-lsp::jobject-p tc) (reality-engine-lsp::jget tc "rules")))
               (binding (reality-engine-lsp::ces-dispatch-binding md)))
          (when (and (reality-engine-lsp::jarray-p rules)
                     (> (length (reality-engine-lsp::jarray-list rules)) 0)
                     (reality-engine-lsp::jobject-p (reality-engine-lsp::jget md "agentBinding"))
                     (> (length (reality-engine-lsp::jstring binding "agent" "")) 0)
                     (> (length (reality-engine-lsp::jstring binding "trigger" "")) 0))
            (incf machines)
            (let ((machine (handler-case (reality-engine-lsp::load-machine-from-file path)
                             (error (c) (push (format nil "~a: load failed — ~a" (file-namestring path) c) failures)
                                    nil))))
              (when machine
                (let ((input-seqs (reality-engine-lsp::jget machine-obj "inputSequences")))
                  (when (reality-engine-lsp::jarray-p input-seqs)
                    (dolist (seq-json (reality-engine-lsp::jarray-list input-seqs))
                      (reset-machine machine)
                      (incf sequences)
                      (let* ((seq-meta (reality-engine-lsp::jget seq-json "metadata"))
                             (expected-count (reality-engine-lsp::jnumber seq-meta "expectedOutputCount" nil))
                             (vectors-json (reality-engine-lsp::jget seq-json "vectors")))
                        (cond
                          ((eql expected-count 0) nil)
                          ((not (reality-engine-lsp::jarray-p vectors-json))
                           (push (format nil "~a: missing input vectors" (file-namestring path)) failures))
                          (t
                           (let ((fired nil))
                             (dolist (vec-json (reality-engine-lsp::jarray-list vectors-json))
                               (let* ((input (reality-engine-lsp::numbers-from-json vec-json))
                                      (tr (reality-engine-lsp::process-machine-input machine input)))
                                 (maphash
                                  (lambda (sid outs)
                                    (dolist (ov outs)
                                      (push (cons sid (reality-engine-lsp::output-vector-vector ov)) fired)
                                      (incf outputs)))
                                  (reality-engine-lsp::transition-result-sequence-outputs tr))))
                             (dolist (pair fired)
                               (let ((record (pe-record-for state machine (car pair) (cdr pair))))
                                 (when record
                                   (incf records)
                                   (let* ((envelope (reality-engine-lsp::jget record "envelope"))
                                          (dispatch (reality-engine-lsp::jget envelope "dispatch"))
                                          (target (reality-engine-lsp::jstring dispatch "agent" ""))
                                          (trigger (reality-engine-lsp::jstring dispatch "trigger" ""))
                                          (mode (reality-engine-lsp::jstring dispatch "autonomyMode" ""))
                                          (write-back (reality-engine-lsp::jget dispatch "writeBack"))
                                          (write-back-type (reality-engine-lsp::jstring write-back "type" "")))
                                     (when (zerop (length target))
                                       (push (format nil "~a: PE dispatch target empty" (file-namestring path)) failures))
                                     (when (zerop (length trigger))
                                       (push (format nil "~a: PE aiTrigger empty" (file-namestring path)) failures))
                                     (unless (member mode '("observe" "advise" "supervised-act" "automated-act") :test #'string=)
                                       (push (format nil "~a: PE autonomyMode invalid" (file-namestring path)) failures))
                                     (when (zerop (length write-back-type))
                                       (push (format nil "~a: PE writeBack.type empty" (file-namestring path)) failures)))))))))))))))))))
    (list :state state
          :machines machines
          :sequences sequences
          :outputs outputs
          :records records
          :failures (nreverse failures))))

;; Yuma tier-1 input patterns lifted from each AGX051-054 inputSequences[]
;; block — same constants the AI + C++ cascade tests use.
(defparameter +tier1-normal-input+ '(1 1 0 1))
(defparameter +tier1-urgent-ticks+ '((1 1 1 1) (1 0 1 0) (0 0 0 0)))

(defun cascade-state ()
  "Build a fresh reality-state seeded with AGX051-055 + AgYieldOptimizationAI."
  (let ((state (reality-engine-lsp::make-reality-state
                :dimension 0
                :machines (make-hash-table :test #'equal)
                :machine-dir (namestring +ai-machines-dir+)
                :perceptual-space (make-list 0 :initial-element 0.0d0)
                :history nil :history-limit 25
                :include-machine-results-p t :include-perceptual-space-p t
                :vector-store (make-hash-table :test #'equal)
                :sequences (make-hash-table :test #'equal)
                :qdrant-url "http://localhost:4333" :collection-name "test"
                :started-at (reality-engine-lsp::now-ms)
                :event-bus-subscriptions (make-hash-table :test #'equal)
                :latched-event-bits (make-hash-table :test #'equal)
                :step-count 0 :mapping-version 0
                :cov-matched    (make-hash-table :test #'equal)
                :cov-activated  (make-hash-table :test #'equal)
                :cov-outputs    (make-hash-table :test #'equal)
                :cov-steps      (make-hash-table :test #'equal)
                :cov-paging     (make-hash-table :test #'equal)
                :cov-deprecated (make-hash-table :test #'equal))))
    (dolist (file '("AGX051_yuma-aqua-maintenance-forecaster.json"
                    "AGX052_yuma-do-probe-reliability-tracker.json"
                    "AGX053_yuma-vpd-hvac-service-planner.json"
                    "AGX054_yuma-co2-safety-compliance-officer.json"
                    "AGX055_yuma-facility-ai-synthesis-bridge.json"
                    "AgYieldOptimizationAI.json"))
      (reality-engine-lsp::put-machine state
        (reality-engine-lsp::load-machine-from-file (merge-pathnames file +ai-machines-dir+))))
    state))

(defun stage1-input (state tick-values)
  "Build the stage-1 input vector: zero-fill to dim, then write the tier-1
   sensor regions.  The simulator overwrites the entire perceptual space
   with this input every call, so AGX052-054 sensors must be re-driven on
   every tick of the AGX051 escalation."
  (let* ((dim (reality-engine-lsp::reality-state-dimension state))
         (v (make-list dim :initial-element 0.0d0)))
    (loop for x in tick-values        for i from 40  do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 84  do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 184 do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 228 do (setf (nth i v) x))
    v))

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
  ;; Strict-STA must be opted in explicitly — same convention as
  ;; MachineLoader.loadFromJSON({strictSta:true}) on AI and
  ;; load_machine_from_json_string(.., LoadOptions{strictSta:true}) on CPP.
  (assert-error (lambda () (machine-from-json (sta-fixture :life-safety t :clean nil) nil :strict-sta t))
                "life-safety machine with STA violation should be rejected when strict-sta is on")
  (assert-true (machine-from-json (sta-fixture :life-safety t :clean nil))
               "life-safety machine with STA violation loads when strict-sta is off (AI/CPP parity default)")
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
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (dolist (pattern '("/api/buses/semantic"
                       "/api/buses/semantic/:id"))
      (assert-true (find pattern patterns :test #'string=)
                   (format nil "Reality routes should expose ~a" pattern))))

  ;; /api/metrics Prometheus text-format emission — verifies cross-runtime
  ;; parity with AI/CPP.  Every metric line must carry runtime="lsp" and the
  ;; canonical metric names (ces_*, re_runtime_*) must all be present.
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::reality-routes nil)))))
    (assert-true (find "/api/metrics" patterns :test #'string=)
                 "Reality routes should expose /api/metrics Prometheus endpoint"))
  (let ((patterns (mapcar #'reality-engine-lsp::route-pattern
                          (reality-engine-lsp::flatten-routes
                           (reality-engine-lsp::perception-routes nil)))))
    (dolist (pattern '("/api/integrations/status"
                       "/api/integrations/completions"
                       "/api/triggers/status"
                       "/api/dispatch/ledger"
                       "/api/dispatch/records/:id"
                       "/api/integrations/ollama/status"
                       "/api/integrations/ollama/dispatch"
                       "/api/integrations/openai/status"
                       "/api/integrations/openai/dispatch"
                       "/api/integrations/healthkit/status"
                       "/api/integrations/healthkit/ingest"
                       "/api/integrations/carekit/status"
                       "/api/integrations/carekit/ingest"))
      (assert-true (find pattern patterns :test #'string=)
                   (format nil "Perception routes should expose ~a" pattern))))
  (let* ((state (reality-engine-lsp::make-perception-state-from-config
                 :dimension 5000
                 :reality-url "http://localhost:3299"
                 :localai-url "http://localhost:8000"
                 :localai-machine-dir "../localAIStack/data/machines"))
         (completion (reality-engine-lsp::ingest-completion
                      state
                      (reality-engine-lsp::obj
                       "provider" "e2e"
                       "agent" "e2e"
                       "sourceMappingId" "agent-completion-risk"
                       "values" (reality-engine-lsp::vectorize (list 1 0 0.75 0)))))
         (signal (reality-engine-lsp::jget completion "signal"))
         (source (reality-engine-lsp::jget signal "source")))
    (assert-equal "agent.e2e.completion"
                  (reality-engine-lsp::jstring source "sensorId" "")
                  "completion ingest should commit through PE source mapping")
    (assert-equal 4200
                  (reality-engine-lsp::jnumber (reality-engine-lsp::jget source "region") "offset" nil)
                  "completion source should use configured mapping offset")
    (assert-equal nil
                  (reality-engine-lsp::jget completion "source")
                  "completion response should not expose legacy top-level source")
    (let ((missing (reality-engine-lsp::ingest-completion
                    state
                    (reality-engine-lsp::obj
                     "sourceMappingId" "missing-completion-mapping"
                     "values" (reality-engine-lsp::vectorize (list 1))))))
      (assert-equal 404 (car missing)
                    "unknown explicit completion sourceMappingId should return 404")
      (assert-equal "Unknown sourceMappingId \"missing-completion-mapping\""
                    (reality-engine-lsp::jstring (cdr missing) "error" "")
                    "unknown completion mapping should match CPP/AI error text")))
  ;; ── HealthKit Spezi bridge — canonical contract ──────────────────────────
  ;; Mirrors CPP e2e_healthkit_spezi.sh: token auth rejection, resolved shape
  ;; with region + source.lastValue for all three Spezi sensor types.
  (let* ((state (reality-engine-lsp::make-perception-state-from-config
                 :dimension 5000
                 :reality-url "http://localhost:3299"
                 :localai-url "http://localhost:8000"
                 :localai-machine-dir "../localAIStack/data/machines"))
         (mappings (reality-engine-lsp::perception-state-source-mappings state)))
    (setf (gethash "healthkit:HKCorrelationTypeIdentifierBloodPressure" mappings)
          (reality-engine-lsp::obj "id" "healthkit:HKCorrelationTypeIdentifierBloodPressure"
                                   "sensorIdTemplate" "healthkit.blood-pressure"
                                   "name" "HealthKit Blood Pressure"
                                   "region" (reality-engine-lsp::obj "offset" 4320 "length" 4)
                                   "ttlMs" 900000))
    (setf (gethash "healthkit:HKWorkoutTypeIdentifierWorkout" mappings)
          (reality-engine-lsp::obj "id" "healthkit:HKWorkoutTypeIdentifierWorkout"
                                   "sensorIdTemplate" "healthkit.exercise"
                                   "name" "HealthKit Exercise"
                                   "region" (reality-engine-lsp::obj "offset" 4330 "length" 4)
                                   "ttlMs" 900000))
    (setf (gethash "healthkit:HKCategoryTypeIdentifierSleepAnalysis" mappings)
          (reality-engine-lsp::obj "id" "healthkit:HKCategoryTypeIdentifierSleepAnalysis"
                                   "sensorIdTemplate" "healthkit.sleep"
                                   "name" "HealthKit Sleep Analysis"
                                   "region" (reality-engine-lsp::obj "offset" 4340 "length" 4)
                                   "ttlMs" 900000))
    (setf (reality-engine-lsp::perception-state-healthkit-bridge-token state) "spezi-test-token")
    ;; Token auth rejection
    (let ((bad (reality-engine-lsp::ingest-healthkit
                state
                (reality-engine-lsp::obj "bridgeToken" "wrong-token"
                                         "type" "HKCorrelationTypeIdentifierBloodPressure"
                                         "values" (reality-engine-lsp::vectorize (list 0.72d0 0.48d0 0.24d0 0.99d0))))))
      (assert-equal 401 (car bad) "HealthKit wrong token should return 401"))
    ;; Blood pressure: sensorId, sourceMappingId, region.offset, source.lastValue
    (let* ((result (cdr (reality-engine-lsp::ingest-healthkit
                         state
                         (reality-engine-lsp::obj "bridgeToken" "spezi-test-token"
                                                  "type" "HKCorrelationTypeIdentifierBloodPressure"
                                                  "values" (reality-engine-lsp::vectorize (list 0.72d0 0.48d0 0.24d0 0.99d0))))))
           (sample (first (reality-engine-lsp::jarray-list (reality-engine-lsp::jget result "resolved")))))
      (assert-true (reality-engine-lsp::jbool result "success" nil)
                   "HealthKit BP ingest success should be true")
      (assert-equal "healthkit.blood-pressure"
                    (reality-engine-lsp::jstring sample "sensorId" "")
                    "HealthKit BP sensorId should match sensorIdTemplate")
      (assert-equal "healthkit:HKCorrelationTypeIdentifierBloodPressure"
                    (reality-engine-lsp::jstring sample "sourceMappingId" "")
                    "HealthKit BP sourceMappingId should match registry key")
      (assert-equal 4320
                    (reality-engine-lsp::jnumber (reality-engine-lsp::jget sample "region") "offset" nil)
                    "HealthKit BP region.offset should be 4320")
      (assert-true (reality-engine-lsp::jget (reality-engine-lsp::jget sample "source") "lastValue")
                   "HealthKit BP source.lastValue should be present"))
    ;; Exercise: sensorId and region.offset
    (let* ((result (cdr (reality-engine-lsp::ingest-healthkit
                         state
                         (reality-engine-lsp::obj "bridgeToken" "spezi-test-token"
                                                  "type" "HKWorkoutTypeIdentifierWorkout"
                                                  "values" (reality-engine-lsp::vectorize (list 0.65d0 0.58d0 0.42d0 0.97d0))))))
           (sample (first (reality-engine-lsp::jarray-list (reality-engine-lsp::jget result "resolved")))))
      (assert-equal "healthkit.exercise"
                    (reality-engine-lsp::jstring sample "sensorId" "")
                    "HealthKit exercise sensorId should match sensorIdTemplate")
      (assert-equal 4330
                    (reality-engine-lsp::jnumber (reality-engine-lsp::jget sample "region") "offset" nil)
                    "HealthKit exercise region.offset should be 4330"))
    ;; Sleep: sensorId and region.offset
    (let* ((result (cdr (reality-engine-lsp::ingest-healthkit
                         state
                         (reality-engine-lsp::obj "bridgeToken" "spezi-test-token"
                                                  "type" "HKCategoryTypeIdentifierSleepAnalysis"
                                                  "values" (reality-engine-lsp::vectorize (list 0.82d0 0.12d0 0.18d0 0.96d0))))))
           (sample (first (reality-engine-lsp::jarray-list (reality-engine-lsp::jget result "resolved")))))
      (assert-equal "healthkit.sleep"
                    (reality-engine-lsp::jstring sample "sensorId" "")
                    "HealthKit sleep sensorId should match sensorIdTemplate")
      (assert-equal 4340
                    (reality-engine-lsp::jnumber (reality-engine-lsp::jget sample "region") "offset" nil)
                    "HealthKit sleep region.offset should be 4340")))
  (let* ((state (reality-engine-lsp::make-perception-state-from-config
                 :dimension 5000
                 :reality-url "http://localhost:3299"
                 :localai-url "http://localhost:8000"
                 :localai-machine-dir "../localAIStack/data/machines"))
         (carekit (cdr (reality-engine-lsp::ingest-carekit
                        state
                        (reality-engine-lsp::obj
                         "sampleType" "task-adherence"
                         "taskId" "morning-medication"
                         "carePlanId" "care-plan-a"
                         "sourceMappingId" "carekit-task"
                         "values" (reality-engine-lsp::vectorize (list 1 0 0.8 0.95))))))
         (result (first (reality-engine-lsp::jarray-list (reality-engine-lsp::jget carekit "results")))))
    (assert-equal "carekit.task-adherence"
                  (reality-engine-lsp::jstring result "sensorId" "")
                  "CareKit ingest should commit through PE source mapping")
    (assert-equal "carekit-task"
                  (reality-engine-lsp::jstring result "sourceMappingId" "")
                  "CareKit ingest should preserve mapping id")
    (assert-equal 4310
                  (reality-engine-lsp::jnumber (reality-engine-lsp::jget (reality-engine-lsp::jget result "source") "region") "offset" nil)
                  "CareKit source should use configured mapping offset"))
  (let ((state (reality-engine-lsp::make-perception-state-from-config
                :dimension 16
                :reality-url "http://localhost:3299"
                :localai-url "http://localhost:8000"
                :localai-machine-dir "../localAIStack/data/machines")))
    (bt:with-lock-held ((reality-engine-lsp::perception-state-machine-catalog-lock state))
      (setf (gethash "machine-e2e"
                     (reality-engine-lsp::perception-state-machine-catalog state))
            (reality-engine-lsp::obj "id" "machine-e2e"
                                     "metadata" (reality-engine-lsp::obj
                                                 "dispatchableAgent" "test-agent"
                                                 "aiTrigger" "ON_MATCH"))))
    (let* ((record (reality-engine-lsp::record-dispatch-envelope
                    state
                    (reality-engine-lsp::obj
                     "machineId" "machine-e2e"
                     "sequenceId" "seq-e2e"
                     "values" (reality-engine-lsp::vectorize (list 1 0))
                     "region" (reality-engine-lsp::obj "offset" 4 "length" 2)
                     "governance" (reality-engine-lsp::obj "ownerTeam" "e2e-team"
                                                           "ragStatusCode" "RED"
                                                           "processStatus" "error")))))
      (assert-equal "recorded"
                    (reality-engine-lsp::jstring record "status" "")
                    "dispatch envelope should be PE-owned ledger record")
      (assert-equal 1
                    (length (reality-engine-lsp::perception-state-dispatch-ledger state))
                    "dispatch ledger should retain PE-owned record")))
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

  ;; ── Cross-runtime parity (AI + C++ + LSP) ─────────────────────────────
  ;; Skipped automatically when the AI corpus directory isn't sibling-located
  ;; (e.g. CI runs that haven't checked out RealityEngine_AI).
  (cond
    ((null +ai-machines-dir+)
     (format t "~&[parity] skipping — RealityEngine_AI/examples/machines not found alongside RealityEngine_LSP~%"))
    (t
     (let* ((result (walk-corpus-for-envelopes +ai-machines-dir+))
            (machines  (getf result :machines))
            (sequences (getf result :sequences))
            (outputs   (getf result :outputs))
            (envelopes (getf result :envelopes))
            (failures  (getf result :failures)))
       (when failures
         (format *error-output* "~&[parity] envelope-dispatch failures (first 10):~%")
         (dolist (f (subseq failures 0 (min 10 (length failures))))
           (format *error-output* "  - ~a~%" f)))
       (assert-equal nil failures "AiTriggerDispatch parity — corpus walk must produce no failures")
       ;; Counter parity — same numbers reported by:
       ;;   RealityEngine_AI  AiTriggerDispatch.test.ts          (jest)
       ;;   RealityEngine_CPP e2e_ai_trigger_dispatch            (C++ exec)
       (format t "~&[parity] LSP walked corpus: machines=~a sequences=~a outputs=~a envelopes=~a (AI/CPP target: 895/4480/3586/3586)~%"
               machines sequences outputs envelopes)
       (assert-equal 895  machines  "AiTriggerDispatch parity — machinesWithTriggers != 895 (AI/CPP value)")
       (assert-equal 4480 sequences "AiTriggerDispatch parity — inputSequencesRun  != 4480 (AI/CPP value)")
       (assert-equal 3586 outputs   "AiTriggerDispatch parity — outputsProduced    != 3586 (AI/CPP value)")
       (assert-equal 3586 envelopes "AiTriggerDispatch parity — envelopesResolved  != 3586 (AI/CPP value)"))

     ;; PE dispatch parity — same corpus and counts, but exercised through the
     ;; PE-owned dispatch ledger path that records async bridge envelopes after
     ;; RE returns a mergeBatch.
     (let* ((pe-result (walk-corpus-through-pe-dispatch +ai-machines-dir+))
            (pe-state (getf pe-result :state))
            (machines  (getf pe-result :machines))
            (sequences (getf pe-result :sequences))
            (outputs   (getf pe-result :outputs))
            (records   (getf pe-result :records))
            (failures  (getf pe-result :failures)))
       (when failures
         (format *error-output* "~&[parity] PE dispatch failures (first 10):~%")
         (dolist (f (subseq failures 0 (min 10 (length failures))))
           (format *error-output* "  - ~a~%" f)))
       (format t "~&[parity] LSP PE dispatch corpus: machines=~a sequences=~a outputs=~a records=~a (CPP target: 895/4480/3586/3586)~%"
               machines sequences outputs records)
       (assert-equal nil failures "PE dispatch parity — corpus walk must produce no failures")
       (assert-equal 895  machines  "PE dispatch parity — machinesWithTriggers != 895 (CPP value)")
       (assert-equal 4480 sequences "PE dispatch parity — inputSequencesRun  != 4480 (CPP value)")
       (assert-equal 3586 outputs   "PE dispatch parity — outputsProduced    != 3586 (CPP value)")
       (assert-equal 3586 records   "PE dispatch parity — ledger records      != 3586 (CPP envelopesResolved)")
       (assert-equal 3586
                     (reality-engine-lsp::perception-state-envelopes-created pe-state)
                     "PE dispatch parity — envelopesCreated counter drifted"))

     ;; AGX051 pin — urgent_maint resolves to aquaculture_predictive_maintenance_agent / RED / sla=900.
     (let* ((m (reality-engine-lsp::load-machine-from-file
                (merge-pathnames "AGX051_yuma-aqua-maintenance-forecaster.json" +ai-machines-dir+)))
            (env (envelope-for m "agx-051-urgent-maint" '(1 0 0 0))))
       (assert-true env                                                                "AGX051 urgent_maint: envelope unresolved")
       (assert-equal "aquaculture_predictive_maintenance_agent"                        (reality-engine-lsp::jstring env "agent" "")       "AGX051 urgent_maint: dispatch agent")
       (assert-equal "agriculture-yuma-aqua-maintenance-forecaster-maintenance"        (reality-engine-lsp::jstring env "trigger" "")     "AGX051 urgent_maint: aiTrigger")
       (assert-equal "advise"                                                          (reality-engine-lsp::jstring env "autonomyMode" "") "AGX051 urgent_maint: autonomyMode")
       (assert-equal "pe-sensor"                                                       (reality-engine-lsp::jstring env "writeBackType" "") "AGX051 urgent_maint: writeBack.type")
       (assert-equal "RED"                                                              (reality-engine-lsp::jstring env "ragStatusCode" "")    "AGX051 urgent_maint: ragStatusCode")
       (assert-equal "error"                                                            (reality-engine-lsp::jstring env "processStatus" "")    "AGX051 urgent_maint: processStatus")
       (assert-equal "agriculture-operations"                                           (reality-engine-lsp::jstring env "ownerTeam" "")        "AGX051 urgent_maint: ownerTeam")
       (assert-equal 900                                                                (reality-engine-lsp::jget env "slaSeconds")             "AGX051 urgent_maint: slaSeconds != 900")
       (let ((green (envelope-for m "agx-051-normal" '(0 0 0 1))))
         (assert-true green                                                             "AGX051 normal: envelope unresolved")
         (assert-equal "GREEN" (reality-engine-lsp::jstring green "ragStatusCode" "")   "AGX051 normal: ragStatusCode"))
       (let* ((pe-state (reality-engine-lsp::make-perception-state-from-config
                         :dimension 768
                         :reality-url "http://localhost:3299"
                         :localai-url "http://localhost:8000"
                         :localai-machine-dir "../localAIStack/data/machines"))
              (record (pe-record-for pe-state m "agx-051-urgent-maint" '(1 0 0 0)))
              (pe-env (reality-engine-lsp::jget record "envelope"))
              (gov (reality-engine-lsp::jget pe-env "governance")))
         (assert-true record "AGX051 PE urgent_maint: dispatch record unresolved")
         (assert-equal "aquaculture_predictive_maintenance_agent" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "agent" "") "AGX051 PE urgent_maint: dispatch agent")
         (assert-equal "agriculture-yuma-aqua-maintenance-forecaster-maintenance" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "trigger" "") "AGX051 PE urgent_maint: aiTrigger")
         (assert-equal "advise" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "autonomyMode" "") "AGX051 PE urgent_maint: autonomyMode")
         (assert-equal "pe-sensor" (reality-engine-lsp::jstring (reality-engine-lsp::jget (reality-engine-lsp::jget pe-env "dispatch") "writeBack") "type" "") "AGX051 PE urgent_maint: writeBack.type")
         (assert-equal "RED" (reality-engine-lsp::jstring gov "ragStatusCode" "") "AGX051 PE urgent_maint: ragStatusCode")
         (assert-equal "error" (reality-engine-lsp::jstring gov "processStatus" "") "AGX051 PE urgent_maint: processStatus")
         (assert-equal 900 (reality-engine-lsp::jget gov "slaSeconds") "AGX051 PE urgent_maint: slaSeconds != 900")))

     ;; AGX055 pin — five sequences route to agriculture_yield_optimization_ai with matching RAG.
     (let ((m (reality-engine-lsp::load-machine-from-file
               (merge-pathnames "AGX055_yuma-facility-ai-synthesis-bridge.json" +ai-machines-dir+))))
       (dolist (case '(("agx-055-aqua-urgent"     (1 0 0 0 0 0 0 0 0 0 0 0) "RED")
                       ("agx-055-do-urgent"       (0 0 0 1 0 0 0 0 0 0 0 0) "RED")
                       ("agx-055-climate-urgent"  (0 0 0 0 0 0 1 0 0 0 0 0) "RED")
                       ("agx-055-safety-urgent"   (0 0 0 0 0 0 0 0 0 1 0 0) "RED")
                       ("agx-055-facility-stable" (0 0 0 0 0 0 0 0 0 0 0 1) "GREEN")))
	         (let ((env (envelope-for m (first case) (second case))))
	           (assert-true env (format nil "AGX055 ~a: envelope unresolved" (first case)))
	           (assert-equal "agriculture_yield_optimization_ai" (reality-engine-lsp::jstring env "agent" "") (format nil "AGX055 ~a: dispatchableAgent" (first case)))
	           (assert-equal "ag-yield-optimization-ai-yuma-facility-bridge" (reality-engine-lsp::jstring env "trigger" "") (format nil "AGX055 ~a: aiTrigger" (first case)))
	           (assert-equal "advise" (reality-engine-lsp::jstring env "autonomyMode" "") (format nil "AGX055 ~a: autonomyMode" (first case)))
	           (assert-equal "pe-sensor" (reality-engine-lsp::jstring env "writeBackType" "") (format nil "AGX055 ~a: writeBack.type" (first case)))
	           (assert-equal (third case) (reality-engine-lsp::jstring env "ragStatusCode" "") (format nil "AGX055 ~a: ragStatusCode" (first case)))
           (assert-equal "agriculture-operations"                   (reality-engine-lsp::jstring env "ownerTeam" "")         (format nil "AGX055 ~a: ownerTeam" (first case)))
         (let* ((pe-state (reality-engine-lsp::make-perception-state-from-config
                           :dimension 768
                           :reality-url "http://localhost:3299"
                           :localai-url "http://localhost:8000"
                           :localai-machine-dir "../localAIStack/data/machines"))
                (record (pe-record-for pe-state m (first case) (second case)))
                (pe-env (reality-engine-lsp::jget record "envelope"))
                (gov (reality-engine-lsp::jget pe-env "governance")))
	           (assert-true record (format nil "AGX055 PE ~a: dispatch record unresolved" (first case)))
	           (assert-equal "agriculture_yield_optimization_ai" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "agent" "") (format nil "AGX055 PE ~a: dispatchableAgent" (first case)))
	           (assert-equal "ag-yield-optimization-ai-yuma-facility-bridge" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "trigger" "") (format nil "AGX055 PE ~a: aiTrigger" (first case)))
	           (assert-equal "advise" (reality-engine-lsp::jstring (reality-engine-lsp::jget pe-env "dispatch") "autonomyMode" "") (format nil "AGX055 PE ~a: autonomyMode" (first case)))
	           (assert-equal "pe-sensor" (reality-engine-lsp::jstring (reality-engine-lsp::jget (reality-engine-lsp::jget pe-env "dispatch") "writeBack") "type" "") (format nil "AGX055 PE ~a: writeBack.type" (first case)))
	           (assert-equal (third case) (reality-engine-lsp::jstring gov "ragStatusCode" "") (format nil "AGX055 PE ~a: ragStatusCode" (first case)))))))

     ;; Bridge perceptual contract — AGX055.output == AgYieldOptimizationAI.input == length 12.
     (let* ((bridge-root (reality-engine-lsp::parse-json (reality-engine-lsp::safe-read-file (namestring (merge-pathnames "AGX055_yuma-facility-ai-synthesis-bridge.json" +ai-machines-dir+)))))
            (yield-root  (reality-engine-lsp::parse-json (reality-engine-lsp::safe-read-file (namestring (merge-pathnames "AgYieldOptimizationAI.json"                 +ai-machines-dir+)))))
            (bridge-out (reality-engine-lsp::jget (reality-engine-lsp::jget (reality-engine-lsp::jget bridge-root "machine") "perceptualMapping") "output"))
            (yield-in   (reality-engine-lsp::jget (reality-engine-lsp::jget (reality-engine-lsp::jget yield-root "machine") "perceptualMapping") "input")))
       (assert-equal (reality-engine-lsp::jnumber bridge-out "offset" nil) (reality-engine-lsp::jnumber yield-in "offset" nil) "bridge contract — output.offset != yield input.offset")
       (assert-equal (reality-engine-lsp::jnumber bridge-out "length" nil) (reality-engine-lsp::jnumber yield-in "length" nil) "bridge contract — output.length != yield input.length")
       (assert-equal 12 (reality-engine-lsp::jnumber bridge-out "length" nil)                                                   "bridge contract — length != 12"))

     ;; Yuma cascade — 3-tick AGX051 escalation, AGX055 fires AQUA_URGENT, [3959]=1.
     (let* ((state (cascade-state))
            (agx051-id (reality-engine-lsp::machine-id (gethash "machine-agx051-yuma-aqua-maintenance-forecaster"      (reality-engine-lsp::reality-state-machines state))))
            (agx055-id (reality-engine-lsp::machine-id (gethash "machine-agx055-yuma-facility-ai-synthesis-bridge"     (reality-engine-lsp::reality-state-machines state))))
            (yield-id  (reality-engine-lsp::machine-id (gethash "machine-agyieldoptimizationai"                        (reality-engine-lsp::reality-state-machines state))))
            (m051-final nil))
       ;; Stage 1 — 3 ticks of escalation
       (dolist (tick +tier1-urgent-ticks+)
         (let* ((step (reality-engine-lsp::process-perceptual-input
                       state (stage1-input state tick)
                       :include-machine-results t :include-perceptual-space t))
                (batch (reality-engine-lsp::jget step "mergeBatch")))
           (let ((m051 (find-merge-by-machine batch agx051-id))
                 (m055 (find-merge-by-machine batch agx055-id)))
             (when m051 (setf m051-final m051))
             (assert-equal nil m055 "stage 1 tick: AGX055 must not fire before tier-1 outputs propagate"))))
       (assert-true m051-final "stage 1: AGX051 never fired URGENT_MAINT across 3-tick escalation")
       (when m051-final
         (assert-equal "agx-051-urgent-maint" (reality-engine-lsp::jstring m051-final "sequenceId" "") "AGX051 sequenceId")
         (assert-equal '(1 0 0 0) (reality-engine-lsp::numbers-from-json (reality-engine-lsp::jget m051-final "values")) "AGX051 values != URGENT_MAINT one-hot")
         (let ((gov (reality-engine-lsp::jget m051-final "governance")))
           (assert-equal "RED"   (reality-engine-lsp::jstring gov "ragStatusCode" "") "AGX051 gov.rag")
           (assert-equal 900     (reality-engine-lsp::jnumber gov "slaSeconds" nil)   "AGX051 gov.sla != 900")))

       ;; Stage 2 — carry-forward perceptual space, zero tier-1 sensors, AGX055 fires
       (let* ((stage2 (copy-list (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage2 40 4))  (_ (zero-region stage2 84 4))
              (_ (zero-region stage2 184 4)) (_ (zero-region stage2 228 4))
              (s2 (reality-engine-lsp::process-perceptual-input
                   state stage2 :include-machine-results t :include-perceptual-space t))
              (batch2 (reality-engine-lsp::jget s2 "mergeBatch"))
              (m055 (find-merge-by-machine batch2 agx055-id)))
         (declare (ignore _))
         (assert-true m055 "stage 2: AGX055 did not fire — bridge contract broken")
         (when m055
           (assert-equal "agx-055-aqua-urgent" (reality-engine-lsp::jstring m055 "sequenceId" "") "AGX055 sequenceId")
           (assert-equal '(1 0 0 0 0 0 0 0 0 0 0 0) (reality-engine-lsp::numbers-from-json (reality-engine-lsp::jget m055 "values")) "AGX055 values != AQUA_URGENT one-hot")
           (let ((gov (reality-engine-lsp::jget m055 "governance")))
             (assert-equal "RED" (reality-engine-lsp::jstring gov "ragStatusCode" "") "AGX055 gov.rag")
             (assert-equal 600   (reality-engine-lsp::jnumber gov "slaSeconds" nil)   "AGX055 gov.sla != 600")))
         (assert-equal nil (find-merge-by-machine batch2 yield-id) "stage 2: AgYieldOptimizationAI fired before AGX055 projection landed"))

       ;; Stage 3 — projection landing: perceptualSpace[3959]=1, [3960:3971]=0
       (let* ((stage3 (copy-list (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage3 40 4))  (_ (zero-region stage3 84 4))
              (_ (zero-region stage3 184 4)) (_ (zero-region stage3 228 4))
              (_ (zero-region stage3 256 16)))
         (declare (ignore _))
         (assert-true (>= (length stage3) 3971) "stage 3: perceptualSpace not grown to 3971")
         (assert-equal 1 (nth 3959 stage3) "stage 3: AQUA_URGENT bit at [3959] missing")
         (loop for i from 3960 below 3971 do
               (assert-equal 0 (nth i stage3) (format nil "stage 3: stray bit at [~a] — one-hot projection violated" i)))
         (reality-engine-lsp::process-perceptual-input state stage3
                                                       :include-machine-results t :include-perceptual-space t)))

     ;; Stable path — all-NORMAL inputs → AGX055 FACILITY_STABLE / GREEN
     (let* ((state (cascade-state))
            (agx055-id (reality-engine-lsp::machine-id (gethash "machine-agx055-yuma-facility-ai-synthesis-bridge" (reality-engine-lsp::reality-state-machines state))))
            (input (let ((v (make-list (reality-engine-lsp::reality-state-dimension state) :initial-element 0.0d0)))
                     (loop for x in +tier1-normal-input+ for i from 40  do (setf (nth i v) x))
                     (loop for x in +tier1-normal-input+ for i from 84  do (setf (nth i v) x))
                     (loop for x in +tier1-normal-input+ for i from 184 do (setf (nth i v) x))
                     (loop for x in +tier1-normal-input+ for i from 228 do (setf (nth i v) x))
                     v)))
       (reality-engine-lsp::process-perceptual-input state input :include-machine-results t :include-perceptual-space t)
       (let* ((stage2 (copy-list (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage2 40 4))  (_ (zero-region stage2 84 4))
              (_ (zero-region stage2 184 4)) (_ (zero-region stage2 228 4))
              (s2 (reality-engine-lsp::process-perceptual-input state stage2 :include-machine-results t :include-perceptual-space t))
              (m055 (find-merge-by-machine (reality-engine-lsp::jget s2 "mergeBatch") agx055-id)))
         (declare (ignore _))
         (assert-true m055 "stable path: AGX055 did not fire FACILITY_STABLE")
         (when m055
           (assert-equal "agx-055-facility-stable" (reality-engine-lsp::jstring m055 "sequenceId" "") "stable path: sequenceId")
           (assert-equal '(0 0 0 0 0 0 0 0 0 0 0 1) (reality-engine-lsp::numbers-from-json (reality-engine-lsp::jget m055 "values")) "stable path: values != FACILITY_STABLE one-hot")
           (assert-equal "GREEN" (reality-engine-lsp::jstring (reality-engine-lsp::jget m055 "governance") "ragStatusCode" "") "stable path: rag != GREEN"))))))

  (format t "~&RealityEngine_LSP core tests passed.~%")
  t)
