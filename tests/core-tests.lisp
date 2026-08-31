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
   :perceptual-space (reality-engine-lsp::make-perceptual-space dimension)
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
;;   RealityEngine_CPP/tests/e2e_ai_trigger_dispatch.cpp
;; reports 1058/5126/4251/4251 (recursive walk incl. machines/domains/) — pinning the same counters here catches any
;; LSP-side drift in process-machine-input or resolve-governance.  And the
;; Yuma 3-tick cascade (AGX051→AGX055→AgYieldOptimizationAI) asserts the
;; same mergeBatch shapes both other runtimes already enforce.

;; The canonical corpus, and the only one. Two fallbacks into a deprecated
;; TypeScript prototype's examples/machines/ are gone: that repository was
;; frozen in June 2026, so a probe that found it would have run these tests
;; against a corpus nobody maintains — silently, since a fallback that hits
;; looks exactly like one that does not.
(defparameter +corpus-machines-dir+
  (or (probe-file "../RealityEngine_Machines/machines/")
      (probe-file "../../RealityEngine_Machines/machines/")))

(defun reset-machine (machine)
  (dolist (sequence (reality-engine-lsp::machine-sequence-list machine))
    (reality-engine-lsp::reset-sequence sequence))
  machine)

(defun zero-region (space offset length)
  "Zero [OFFSET, OFFSET+LENGTH) of SPACE, which may be a vector or a list.
The perceptual space is a vector since #60; some callers still pass lists."
  (if (vectorp space)
      (fill space 0.0d0 :start offset :end (min (+ offset length) (length space)))
      (loop for i from offset below (+ offset length)
            do (setf (nth i space) 0.0d0)))
  space)

(defun find-merge-by-machine (batch machine-id)
  (find machine-id (coerce batch 'list)
        :key (lambda (op) (reality-engine-lsp::jstring op "machineId" ""))
        :test #'string=))

(defun merge-sequence-ids (op)
  "The contributing CES set carried by one merge operation, as a list."
  (reality-engine-lsp::jarray-list (reality-engine-lsp::jget op "sequenceIds")))

(defun merge-values (op)
  "The folded values one merge operation carries, as integers.

The operation carries the FOLD, and the fold returns double-floats: 1.0d0 where
the corpus JSON parsed to the integer 1.  The wire is unchanged — write-json
renders a whole double without a fractional part, so both emit `1` — and the
packed-cell path takes either.  Only a Lisp-level EQUAL sees the difference, so
these assertions compare the numbers rather than their representation."
  (mapcar #'round (reality-engine-lsp::numbers-from-json
                   (reality-engine-lsp::jget op "values"))))

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
    (dolist (path (reality-engine-lsp::collect-json-files-recursive machine-dir))
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
       ;; One machine, one operation, carrying the contributing set — the shape
       ;; the RE emits since the fold moved into the machine's atomic step.
       "sequenceIds" (reality-engine-lsp::vectorize (list sequence-id))
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
    (dolist (path (reality-engine-lsp::collect-json-files-recursive machine-dir))
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
                :machine-dir (namestring +corpus-machines-dir+)
                :perceptual-space (reality-engine-lsp::make-perceptual-space 0)
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
        (reality-engine-lsp::load-machine-from-file (reality-engine-lsp::resolve-machine-json-path +corpus-machines-dir+ file))))
    state))

(defun stage1-input (state tick-values)
  "Build the stage-1 input vector: zero-fill to dim, then write the tier-1
   sensor regions.  The engine overwrites the entire perceptual space
   with this input every call, so AGX052-054 sensors must be re-driven on
   every tick of the AGX051 escalation."
  (let* ((dim (reality-engine-lsp::reality-state-dimension state))
         (v (make-list dim :initial-element 0.0d0)))
    (loop for x in tick-values        for i from 40  do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 84  do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 184 do (setf (nth i v) x))
    (loop for x in +tier1-normal-input+ for i from 228 do (setf (nth i v) x))
    v))

;; ── output merge fold ────────────────────────────────────────────────────────
;; The Boolean gates are pinned here cell for cell. 1273 binary corpus machines
;; fold with them, so a silent change to one is a change to every result they
;; have ever produced, and the corpus walk further down counts outputs without
;; looking at their values.
;;
;; The five multi-valued transformations are checked against the properties the
;; fold contract requires — closure, symmetry, determinism — by exhaustion over
;; the chain rather than by assertion, the way the reference implementation at
;; RealityEngine_CI scripts/experiment-mv-transforms.py checks itself. The full
;; 6820-fold differential against that reference (5 transformations x chain
;; {0..3} x n=1..5, zero differences, matching what C++ reported) needs Python
;; on the box and is run out of band; what is kept here stands alone.

(defun merge-fold (collection name &key chain-top)
  (reality-engine-lsp::fold-output-vectors collection name :chain-top chain-top))

(defun merge-fold1 (values name &key chain-top)
  "Fold a single cell — one contribution per member of VALUES."
  (round (first (merge-fold (mapcar #'list values) name :chain-top chain-top))))

(defun merge-tuples (alphabet n)
  "Every length-N tuple over ALPHABET."
  (if (zerop n)
      (list nil)
      (loop for item in alphabet
            append (mapcar (lambda (rest) (cons item rest))
                           (merge-tuples alphabet (1- n))))))

(defun merge-permutations (values)
  "Every distinct permutation of VALUES."
  (if (null (cdr values))
      (list (copy-list values))
      (loop for item in (remove-duplicates values)
            append (mapcar (lambda (rest) (cons item rest))
                           (merge-permutations (remove item values :count 1))))))

(defparameter +mv-names+
  '("meet" "join" "strong-conjunction" "strong-disjunction" "discrete-median"))

(defun output-merge-tests ()
  ;; An empty collection presents no output. Not a vector of zeros — a machine
  ;; that completed no Reality Event asserted nothing, which is a different
  ;; claim from asserting zero.
  (assert-equal nil (merge-fold nil "or") "empty collection folds to no output")
  (assert-equal nil (merge-fold nil "meet") "empty collection folds to no output under MV too")

  ;; Boolean gates, unchanged. k per cell is 2, 1, 1 over this collection.
  (let ((collection '((1 0 1) (1 1 0))))
    (assert-equal '(1.0d0 1.0d0 1.0d0) (merge-fold collection "or")   "or(k>=1)")
    (assert-equal '(1.0d0 0.0d0 0.0d0) (merge-fold collection "and")  "and(k=n)")
    (assert-equal '(0.0d0 1.0d0 1.0d0) (merge-fold collection "xor")  "xor(k odd)")
    (assert-equal '(0.0d0 0.0d0 0.0d0) (merge-fold collection "nor")  "nor(k=0)")
    (assert-equal '(0.0d0 1.0d0 1.0d0) (merge-fold collection "nand") "nand(k<n)")
    (assert-equal (merge-fold collection "or") (merge-fold collection nil)
                  "an undeclared transformation is or")
    (assert-equal (merge-fold collection "or") (merge-fold collection "not-a-gate")
                  "an unknown transformation falls back to or, as before")
    ;; A contribution shorter than the widest asserts nothing in the cells it
    ;; does not reach, under both families.
    (assert-equal '(1.0d0 0.0d0 0.0d0) (merge-fold '((1 0 1) (1)) "and")
                  "a missing cell does not assert")
    (assert-equal '(1.0d0 0.0d0 0.0d0) (merge-fold '((1 2 3) (1)) "meet" :chain-top 3)
                  "a missing cell is the chain bottom"))

  ;; The worked examples from the specification, chain {0..3}.
  (assert-equal 3 (merge-fold1 '(1 1 1) "strong-disjunction" :chain-top 3)
                "three weak indicators accumulate to confirmed")
  (assert-equal 2 (merge-fold1 '(3 3 2) "strong-conjunction" :chain-top 3)
                "near-unanimous high agreement survives")
  (assert-equal 0 (merge-fold1 '(3 3 0) "strong-conjunction" :chain-top 3)
                "one absolute disagreement extinguishes")
  (assert-equal 3 (merge-fold1 '(3 3 0 3) "discrete-median" :chain-top 3)
                "a transient dropout is filtered")
  (assert-equal 0 (merge-fold1 '(3 3 0 3) "meet" :chain-top 3) "a single 0 vetoes the meet")
  (assert-equal 3 (merge-fold1 '(3 3 0 3) "join" :chain-top 3) "the join is the envelope")
  ;; For an even n the median is the LOWER middle element — floor(median) over
  ;; integers, a selection and never a division. 2 here, not 2.5 and not 3.
  (assert-equal 2 (merge-fold1 '(1 2 3 4) "discrete-median" :chain-top 4)
                "even n takes the lower middle element")

  ;; An early exit stops the arithmetic for a cell but must still leave every
  ;; cursor sitting on the next cell, or the contributions it skipped read one
  ;; cell behind for the rest of the fold. Only visible past the first cell, so
  ;; each of these is a collection wide enough to show it, chosen so that the
  ;; second cell differs if the skipped contributions were not advanced.
  (assert-equal '(0.0d0 1.0d0) (merge-fold '((3 2) (0 1) (0 3)) "meet" :chain-top 3)
                "meet advances the cursors it skipped when the chain bottom absorbs")
  (assert-equal '(3.0d0 3.0d0) (merge-fold '((0 0) (3 1) (1 3)) "join" :chain-top 3)
                "join advances the cursors it skipped when it reaches the chain top")
  (assert-equal '(2.0d0 2.0d0) (merge-fold '((2 0) (1 1) (0 3)) "strong-disjunction" :chain-top 2)
                "strong-disjunction advances the cursors it skipped when it saturates")

  ;; Cells are stored as double-floats but an MV value is a position on a chain,
  ;; so the conversion is explicit and rounds ties AWAY FROM ZERO. CL's ROUND is
  ;; round-half-to-even and would fold 2.5 to 2, while C++ llround and Scala both
  ;; fold it to 3. This assertion is the tripwire on anyone simplifying
  ;; MV-CELL-INTEGER back to a bare ROUND.
  (assert-equal 3 (merge-fold1 '(2.5d0 3.0d0) "meet" :chain-top 3)
                "2.5 rounds away from zero to 3, not half-to-even to 2")
  (assert-equal 4 (merge-fold1 '(3.5d0 4.0d0) "meet" :chain-top 4)
                "3.5 rounds away from zero to 4, which half-to-even would also give")
  (assert-equal 2 (merge-fold1 '(2.4d0 3.0d0) "meet" :chain-top 3) "2.4 rounds to 2")
  (assert-equal 0 (merge-fold1 '(-1.0d0 3.0d0) "meet" :chain-top 3)
                "below the chain bottom clamps to the bottom")

  ;; Exhaustive over the chain {0..3} for collections to n=4.
  (let ((alphabet '(0 1 2 3))
        (k 3))
    (dolist (name +mv-names+)
      (loop for n from 1 to 4
            do (dolist (tuple (merge-tuples alphabet n))
                 (let ((got (merge-fold1 tuple name :chain-top k)))
                   ;; Closure — the fold may not leave the chain.
                   (assert-true (member got alphabet)
                                (format nil "~a ~s -> ~a left the chain" name tuple got))
                   ;; Symmetry — the collection carries no order, so the fold
                   ;; may not depend on one. This is the property that broke
                   ;; when the arbiter's pick stood in for the fold and each
                   ;; runtime picked a different member (RealityEngine_CI#154).
                   (dolist (permutation (merge-permutations tuple))
                     (assert-equal got (merge-fold1 permutation name :chain-top k)
                                   (format nil "~a ~s is order dependent" name tuple)))
                   ;; Meet, join and median return a contributor — stronger than
                   ;; closure. Bitwise or of 1 and 2 is 3: inside the chain but
                   ;; asserted by nobody, which on an ordinal severity ladder
                   ;; invents a rung.
                   (when (member name '("meet" "join" "discrete-median") :test #'string=)
                     (assert-true (member got tuple)
                                  (format nil "~a ~s -> ~a fabricated a value" name tuple got)))))
               ;; Idempotence where it is claimed, and only there.
               (when (member name '("meet" "join" "discrete-median") :test #'string=)
                 (dolist (x alphabet)
                   (assert-equal x (merge-fold1 (make-list n :initial-element x) name :chain-top k)
                                 (format nil "~a is not idempotent at ~a" name x))))))
    ;; The strong operations are NOT idempotent, by design — x (+) x saturates
    ;; and x (.) x extinguishes, which is the point of them. Asserted so that
    ;; "fixing" it later has to be deliberate.
    (assert-equal 2 (merge-fold1 '(1 1) "strong-disjunction" :chain-top 3)
                  "strong-disjunction accumulates rather than repeating")
    (assert-equal 0 (merge-fold1 '(1 1) "strong-conjunction" :chain-top 3)
                  "strong-conjunction extinguishes rather than repeating")

    ;; De Morgan duality under ¬x = k − x, as specified.
    (loop for n from 1 to 4
          do (dolist (tuple (merge-tuples alphabet n))
               (assert-equal (merge-fold1 tuple "strong-conjunction" :chain-top k)
                             (- k (merge-fold1 (mapcar (lambda (x) (- k x)) tuple)
                                               "strong-disjunction" :chain-top k))
                             (format nil "de Morgan fails at ~s" tuple)))))

  ;; The median is a quickselect, not a sort. Cross-checked against a sort over
  ;; random collections, including the all-equal and already-ordered columns
  ;; that are a partition scheme's worst cases.
  (dotimes (trial 400)
    (let* ((n (1+ (random 9)))
           (values (loop repeat n collect (random 6)))
           (expected (nth (floor (1- n) 2) (sort (copy-list values) #'<))))
      (assert-equal expected (merge-fold1 values "discrete-median")
                    (format nil "quickselect disagrees with a sort at ~s" values))))
  (dolist (values '((2 2 2 2 2) (0 1 2 3 4) (4 3 2 1 0) (1 1 2 2) (5)))
    (assert-equal (nth (floor (1- (length values)) 2) (sort (copy-list values) #'<))
                  (merge-fold1 values "discrete-median")
                  (format nil "quickselect disagrees with a sort at ~s" values)))

  ;; An absent chain top: the two strong operations REFUSE and the machine
  ;; presents nothing. Isolated to MV-FOLD-REFUSES-P and pinned here, because a
  ;; guessed k is unsound in both directions and neither direction is loud.
  ;; Folding at k=1 clamps FallDetection's ladder [0,1,2,3,4,4,0] to 1 under (+)
  ;; — the flattening the MV vocabulary exists to prevent — and yields 8 under
  ;; (.), outside both the chain {0,1} and the machine's alphabet {0..4}. All
  ;; three runtimes refuse.
  (let ((collection '((1 0 1) (1 1 0))))
    (assert-equal nil (merge-fold collection "strong-disjunction")
                  "strong-disjunction refuses without a chain top")
    (assert-equal nil (merge-fold collection "strong-conjunction")
                  "strong-conjunction refuses without a chain top")
    ;; Refusal is about the missing parameter, not about the data: supply k and
    ;; the same collection folds.
    (assert-true (merge-fold collection "strong-disjunction" :chain-top 1)
                 "strong-disjunction folds once a chain top is supplied"))
  ;; The smallest witness that (.) does not clamp — 3 is outside {0,1}. Asserted
  ;; so that anyone reinstating a k=1 fallback has to walk past it.
  (assert-equal 3 (merge-fold1 '(2 2) "strong-conjunction" :chain-top 1)
                "strong-conjunction leaves the chain when k understates the alphabet")
  ;; The other three take no k, or take one only as an optimisation, and stay
  ;; total without it. join in particular stays the plain maximum, and stays
  ;; symmetric even where the data has left the chain it was told about.
  (assert-equal 0 (merge-fold1 '(3 3 0 3) "meet") "meet needs no chain top")
  (assert-equal 3 (merge-fold1 '(3 3 0 3) "discrete-median") "discrete-median needs no chain top")
  (assert-equal 5 (merge-fold1 '(1 3 5) "join") "join without a chain top is the maximum")
  (assert-equal 5 (merge-fold1 '(5 3 1) "join") "join without a chain top is order independent")

  ;; Every name the endpoints advertise is a name the fold implements.
  (dolist (name reality-engine-lsp::+output-merge-transformations+)
    (assert-true (merge-fold '((1)) name :chain-top 1)
                 (format nil "~a is advertised but does not fold" name)))
  (assert-equal 10 (length reality-engine-lsp::+output-merge-transformations+)
                "five Boolean gates and five multi-valued transformations")
  t)

;; ── fold placement ───────────────────────────────────────────────────────────
;; The fold used to sit beside the machine's atomic step rather than in it: it
;; produced `mergedOutputVector` for the Perception Engine to read, and
;; arbitration went on resolving the unfolded collection. So a machine with
;; seven completed Reality Events put seven values into the same cell and the
;; per-cell arbiter resolved contention that belonged to the machine — which is
;; how FallDetection's ladder resolved to 2.0 on C++/LSP and 0.0 on Scala,
;; neither the maximum nor the minimum (RealityEngine_CI#154, #158).
;;
;; These cover the four properties of the move that no live probe can establish,
;; because each is a statement about what the engine does NOT do: contribute per
;; sequence, resolve governance against the fold, drop a subscription, or emit
;; zeros where it refused.

(defun fold-sequence-json (sequence-id output-values &optional (output-count 1))
  "One CES completing on the machine's single input cell, emitting OUTPUT-VALUES.

Every sequence of a fixture keys on the same cell so that one push completes all
of them at once — which is the case the fold exists for.  They cannot be given
separate input cells: the default comparator tests binary AGREEMENT per cell
rather than `>=`, so a sequence declaring 0 where a sibling declares 1 requires
that cell to be low and the two can never complete on the same input.

OUTPUT-COUNT above 1 gives the CES several output vectors, so it asserts several
times in one step.  That is the only way to put a repeated sequence id into the
contributor list: declaring the same id twice in a machine's `sequences` does
not do it, because the loader keys sequences by id and the second declaration
replaces the first."
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
                                 (loop for i from 0 below output-count
                                       collect (reality-engine-lsp::obj
                                                "id" (format nil "~a-out-~a" sequence-id i)
                                                "vector" (reality-engine-lsp::vectorize
                                                          output-values)))))))))

(defun fold-machine (id sequences &key (input-offset 0) (output-offset 16)
                                       transformation chain-top metadata)
  "A machine whose SEQUENCES all write the SAME output region.

SEQUENCES is a list of (sequence-id output-values [output-count]), all completing
together on the machine's one input cell.  Overlapping output positions within one machine
are the case the fold exists for — the constructor assigns two CESs the same
position only where that position is identical across every fold configuration,
so it is the machine's own composition rather than contention the arbiter should
ever have seen."
  (let* ((output-length (reduce #'max sequences :key (lambda (s) (length (second s)))
                                                :initial-value 0))
         (mapping (reality-engine-lsp::obj
                   "input" (reality-engine-lsp::obj "offset" input-offset
                                                    "length" 1)
                   "output" (reality-engine-lsp::obj "offset" output-offset
                                                     "length" output-length))))
    (when chain-top
      (setf (reality-engine-lsp::jget mapping "outputAlphabetTop") chain-top))
    (machine-from-json
     (reality-engine-lsp::obj
      "id" id
      "name" id
      "arbiterRule" "passthrough"
      "metadata" (or metadata (reality-engine-lsp::obj))
      "outputMergeTransformation" (or transformation "or")
      "perceptualMapping" mapping
      "sequences" (reality-engine-lsp::vectorize
                   (mapcar (lambda (spec)
                             (fold-sequence-json (first spec) (second spec)
                                                 (or (third spec) 1)))
                           sequences))))))

(defun governance-rule (sequence-id output-matches rag process-status owner-team)
  (reality-engine-lsp::obj
   "sequenceId" sequence-id
   "outputMatches" (reality-engine-lsp::vectorize output-matches)
   "ragStatusCode" rag
   "processStatus" process-status
   "description" (format nil "rule for ~a" sequence-id)
   "governance" (reality-engine-lsp::obj "ownerTeam" owner-team)))

(defun governance-metadata (&rest rules)
  (reality-engine-lsp::obj
   "triggerConfig" (reality-engine-lsp::obj
                    "rules" (reality-engine-lsp::vectorize rules))))

(defun fold-step (state input)
  (reality-engine-lsp::process-perceptual-input state input
                                                :include-machine-results t
                                                :include-perceptual-space t))

(defun fold-batch (step)
  (coerce (reality-engine-lsp::jget step "mergeBatch") 'list))

(defun fold-placement-tests ()
  ;; ── §8 — a single contributing sequence must read exactly as it did ────────
  ;;
  ;; This is the property that keeps the corpus unaffected: 1326 of its 1328
  ;; machines assert only 0/1 and fold with `or`, and the remaining two declare
  ;; `join` — and every one of those transformations is the identity on a
  ;; one-element collection. `sequenceIds` becomes a one-element array and the
  ;; joined governance is that sequence's own.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-solo" '(("seq-solo" (1 0 1)))
                   :metadata (governance-metadata
                              (governance-rule "seq-solo" '(1 0 1) "AMBER" "warning" "team-solo"))))
    (let* ((batch (fold-batch (fold-step state (list 1))))
           (op (first batch)))
      (assert-equal 1 (length batch) "one machine contributes exactly one operation")
      (assert-equal '("seq-solo") (merge-sequence-ids op)
                    "a single contributor yields a one-element sequenceIds")
      (assert-equal '(1 0 1) (merge-values op)
                    "the folded value of one contributor is that contributor")
      (assert-equal '("seq-solo-vector")
                    (coerce (reality-engine-lsp::jget op "provenance") 'list)
                    "provenance of one contributor is that contributor's chain")
      (assert-true (null (reality-engine-lsp::jget op "outputIndex"))
                   "outputIndex is gone — one operation covers the machine")
      (let ((gov (reality-engine-lsp::jget op "governance")))
        (assert-equal "AMBER" (reality-engine-lsp::jstring gov "ragStatusCode" "")
                      "the join over one contributor is that contributor's decision")
        (assert-equal "team-solo" (reality-engine-lsp::jstring gov "ownerTeam" "")
                      "and it travels whole"))))

  ;; The general form of the property above, stated over the transformations
  ;; rather than over one fixture: folding a one-element collection returns that
  ;; element. True for `or`, `and` and `xor` on {0,1} and for all five
  ;; multi-valued transformations — which is every transformation the corpus
  ;; uses, since all 1328 machines fold with `or`.
  (dolist (name '("or" "and" "xor" "meet" "join" "discrete-median"
                  "strong-conjunction" "strong-disjunction"))
    (assert-equal '(1.0d0 0.0d0 1.0d0) (merge-fold '((1 0 1)) name :chain-top 1)
                  (format nil "~a is not the identity on a single contributor" name)))

  ;; Where it is NOT the identity, pinned rather than assumed — these are the
  ;; two boundaries of §8's byte-identity claim, and both are reachable only by
  ;; a machine the corpus does not currently contain.
  ;;
  ;;   nor and nand invert a lone contributor: nor(k=1,n=1) is false and
  ;;   nand(k=0,n=1) is true. No corpus machine declares either.
  (assert-equal '(0.0d0 1.0d0) (merge-fold '((1 0)) "nor")
                "nor inverts a single contributor — byte-identity does not hold for it")
  (assert-equal '(0.0d0 1.0d0) (merge-fold '((1 0)) "nand")
                "nand inverts a single contributor — byte-identity does not hold for it")
  ;;   and a Boolean gate answers only "asserted or not", so it binarises a
  ;;   contributor that asserted an ordinal value. Exactly two corpus machines
  ;;   assert outside {0,1} — FallDetection and FallSensorMotionPreaggregator —
  ;;   and they are the machines this whole line of work is about (#158).
  (assert-equal '(1.0d0 1.0d0) (merge-fold '((4 3)) "or")
                "a Boolean gate binarises a non-binary contributor")
  (assert-equal '(4.0d0 3.0d0) (merge-fold '((4 3)) "join" :chain-top 4)
                "a multi-valued transformation preserves the ordinal value")

  ;; ── §3 — governance is the join over contributors, resolved per sequence ──
  ;;
  ;; The severity chain GREEN/absent 0 < AMBER 1 < RED 2 is already ordered, so
  ;; the join is a maximum over it: deterministic, symmetric, and
  ;; safety-preserving — a RED-governed firing cannot be hidden by a GREEN one
  ;; that folded beside it, which is exactly what SEVERITY arbitration exists to
  ;; guarantee.
  ;;
  ;; Both fixtures fold to (1 1), which matches NO rule. That is deliberate: it
  ;; demonstrates the resolution is per contributing sequence against that
  ;; sequence's own asserted values, and never against the folded value.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-severity" '(("seq-a-amber" (1 0)) ("seq-b-red" (0 1)))
                   :metadata (governance-metadata
                              (governance-rule "seq-a-amber" '(1 0) "AMBER" "warning" "team-amber")
                              (governance-rule "seq-b-red"   '(0 1) "RED"   "error"   "team-red"))))
    (let* ((op (first (fold-batch (fold-step state (list 1)))))
           (gov (reality-engine-lsp::jget op "governance")))
      (assert-equal '("seq-a-amber" "seq-b-red") (merge-sequence-ids op)
                    "sequenceIds is the sorted contributing set")
      (assert-equal '(1 1) (merge-values op)
                    "the machine contributes one folded value, not one per firing")
      (assert-equal "RED" (reality-engine-lsp::jstring gov "ragStatusCode" "")
                    "the join takes the highest severity rank among contributors")
      ;; Whole, not composed: every field comes from the RED rule. A record
      ;; mixing one rule's status with another's owner describes no rule that
      ;; exists, and would page a team for a status it never declared.
      (assert-equal "error" (reality-engine-lsp::jstring gov "processStatus" "")
                    "the winning contributor's processStatus travels with it")
      (assert-equal "team-red" (reality-engine-lsp::jstring gov "ownerTeam" "")
                    "the winning contributor's ownerTeam travels with it")
      (assert-equal "rule for seq-b-red" (reality-engine-lsp::jstring gov "description" "")
                    "the winning contributor's description travels with it")
      (assert-equal "seq-b-red" (reality-engine-lsp::jstring gov "sequenceId" "")
                    "the decision names the sequence it was resolved for")))

  ;; The tie. Equal severity ranks are broken by lexicographically smallest
  ;; sequence id, so the answer does not depend on which contributor happened to
  ;; fire first or on the order the machine enumerated them.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-tie" '(("seq-z-amber" (0 1)) ("seq-a-amber" (1 0)))
                   :metadata (governance-metadata
                              (governance-rule "seq-z-amber" '(0 1) "AMBER" "warning" "team-z")
                              (governance-rule "seq-a-amber" '(1 0) "AMBER" "info"    "team-a"))))
    (let* ((op (first (fold-batch (fold-step state (list 1)))))
           (gov (reality-engine-lsp::jget op "governance")))
      (assert-equal '("seq-a-amber" "seq-z-amber") (merge-sequence-ids op)
                    "both sequences contributed")
      (assert-equal "seq-a-amber" (reality-engine-lsp::jstring gov "sequenceId" "")
                    "an AMBER/AMBER tie resolves to the lexicographically smallest id")
      (assert-equal "team-a" (reality-engine-lsp::jstring gov "ownerTeam" "")
                    "and that contributor's decision travels whole")
      (assert-equal "info" (reality-engine-lsp::jstring gov "processStatus" "")
                    "not composed from the other tied rule")))

  ;; No contributor resolves a rule — absent, not an empty decision.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-ungoverned" '(("seq-ungoverned" (1)))))
    (let ((op (first (fold-batch (fold-step state (list 1))))))
      (assert-true (null (reality-engine-lsp::jget op "governance"))
                   "no contributor resolved governance, so the operation carries none")))

  ;; ── §4 — deprecation attaches when ANY contributor is deprecated ──────────
  (let* ((state (make-test-state 32))
         (machine (fold-machine "machine-dep-join"
                                '(("seq-z-dep" (0 1)) ("seq-a-dep" (1 0))))))
    (dolist (spec '(("seq-z-dep" "2026-01-01" "seq-z-new")
                    ("seq-a-dep" "2026-01-02" "seq-a-new")))
      (let ((sequence (gethash (first spec) (reality-engine-lsp::machine-sequences machine))))
        (setf (reality-engine-lsp::sequence-deprecated-at sequence) (second spec)
              (reality-engine-lsp::sequence-replaced-by sequence) (third spec))))
    (reality-engine-lsp::put-machine state machine)
    (let* ((op (first (fold-batch (fold-step state (list 1)))))
           (dep (reality-engine-lsp::jget op "deprecation")))
      (assert-equal "seq-a-new" (reality-engine-lsp::jstring dep "replacedBy" "")
                    "the reported deprecation is the lexicographically smallest deprecated contributor")
      ;; The counter still counts firings, not machines: both deprecated
      ;; contributors bump it, under their own sequence ids.
      (assert-equal 2 (let ((total 0))
                        (maphash (lambda (_ n) (declare (ignore _)) (incf total n))
                                 (reality-engine-lsp::reality-state-cov-deprecated state))
                        total)
                    "record-deprecated-fire runs once per deprecated contributor")
      (assert-true (null (reality-engine-lsp::jget op "%deprecatedFires"))
                   "the internal per-firing detail must not reach the wire")))

  ;; ── §5 — the event bus fires for EVERY contributing sequence ──────────────
  ;;
  ;; The one consumer whose behaviour must be identical rather than analogous:
  ;; subscriptions are keyed on (producer machine, producer sequence) and the
  ;; write goes into the perceptual space, so a meta machine that stops firing
  ;; is a corpus change, not a reporting difference. Reading one arbitrarily
  ;; chosen member of the set would fire one of these two subscribers and
  ;; silently drop the other.
  (let ((state (make-test-state 40)))
    (reality-engine-lsp::put-machine
     state
     (one-bit-machine "agent-fold" "agent-fold-seq" 20 30
                      :metadata (compose-metadata
                                 (compose-subscription "producer-fold" "fold-seq-b" 13)
                                 (compose-subscription "producer-fold" "fold-seq-a" 12))))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "producer-fold" '(("fold-seq-a" (1)) ("fold-seq-b" (1)))))
    (assert-equal 2 (reality-engine-lsp::event-bus-subscription-count state)
                  "both subscriptions registered")
    (let* ((step (fold-step state (list 1)))
           (producer (find-merge-by-machine (reality-engine-lsp::jget step "mergeBatch")
                                            "producer-fold"))
           (event-bus (coerce (reality-engine-lsp::jget step "eventBus") 'list)))
      (assert-equal '("fold-seq-a" "fold-seq-b") (merge-sequence-ids producer)
                    "the producer contributes one operation naming both sequences")
      (assert-equal 2 (length event-bus)
                    "every contributing sequence's subscription still fires")
      (assert-equal '(12 13)
                    (mapcar (lambda (write) (reality-engine-lsp::jnumber write "bitOffset" nil))
                            event-bus)
                    "both subscriber bits are written")
      (assert-equal '("fold-seq-a" "fold-seq-b")
                    (mapcar (lambda (write)
                              (reality-engine-lsp::jstring write "producerSequenceId" ""))
                            event-bus)
                    "each write is attributed to the sequence that produced it")))

  ;; ── §2 — a fold refusal contributes NO operation ──────────────────────────
  ;;
  ;; Not a vector of zeros: zeros are a positive claim about every cell in the
  ;; region, and would reach the arbiter as a contribution that could suppress a
  ;; real one. A machine whose declared transformation cannot be evaluated
  ;; presents nothing, exactly as one that completed no Reality Event does.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-refuses" '(("seq-refuse-a" (1 0)) ("seq-refuse-b" (1 0)))
                   :transformation "strong-conjunction"))
    (let* ((step (fold-step state (list 1)))
           (result (reality-engine-lsp::jget (reality-engine-lsp::jget step "machineResults")
                                             "machine-refuses")))
      (assert-equal nil (fold-batch step)
                    "a refusing machine contributes no merge operation")
      (assert-true (eq (reality-engine-lsp::jget result "mergedOutputVector")
                       reality-engine-lsp::+json-null+)
                   "and presents no merged output to the Perception Engine")
      ;; The output region specifically — the input cell the push asserted is
      ;; still set, and it is the output side that a zero-vector contribution
      ;; would have claimed.
      (assert-true (every #'zerop
                          (subseq (reality-engine-lsp::reality-state-perceptual-space state)
                                  16 18))
                   "and writes nothing into its output region")))

  ;; ── §2 against §5 — a refusal withdraws the VALUE, not the FIRINGS ────────
  ;;
  ;; The two clauses collide if the event bus is driven off the merge batch: §2
  ;; says a refusing machine contributes no operation, and §5 says every
  ;; (producer machine, producer sequence) subscription that would have fired
  ;; still fires. With no operation to iterate there is nothing to fan out from,
  ;; and the subscriptions go silent — which §5 forbids.
  ;;
  ;; Resolved by driving the bus from the contributor record instead. The bus
  ;; asks which CESs COMPLETED, which is independent of whether the fold produced
  ;; a presentable value, so a producer whose transformation refuses still feeds
  ;; every meta machine subscribed to it while contributing nothing to
  ;; arbitration. Getting this wrong is close to undetectable in a single step:
  ;; the response looks well formed, and only the meta machine's silence
  ;; downstream shows it.
  (let ((state (make-test-state 40)))
    (reality-engine-lsp::put-machine
     state
     (one-bit-machine "agent-refuse" "agent-refuse-seq" 20 30
                      :metadata (compose-metadata
                                 (compose-subscription "producer-refuse" "refuse-seq-b" 13)
                                 (compose-subscription "producer-refuse" "refuse-seq-a" 12))))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "producer-refuse" '(("refuse-seq-a" (1)) ("refuse-seq-b" (1)))
                   :transformation "strong-disjunction"))
    (let* ((step (fold-step state (list 1)))
           (event-bus (coerce (reality-engine-lsp::jget step "eventBus") 'list)))
      (assert-equal nil (find-merge-by-machine (reality-engine-lsp::jget step "mergeBatch")
                                               "producer-refuse")
                    "the refusing producer contributes no merge operation")
      (assert-equal 2 (length event-bus)
                    "but every subscription it feeds still fires")
      (assert-equal '(12 13)
                    (mapcar (lambda (write) (reality-engine-lsp::jnumber write "bitOffset" nil))
                            event-bus)
                    "both subscriber bits are written despite the refusal")
      (assert-equal '("refuse-seq-a" "refuse-seq-b")
                    (mapcar (lambda (write)
                              (reality-engine-lsp::jstring write "producerSequenceId" ""))
                            event-bus)
                    "each write is still attributed to the sequence that completed")))

  ;; ── A3 — `cesId` is the comma-joined contributing set ─────────────────────
  ;;
  ;; A folded contribution has no single CES, so the arbitration record carries
  ;; the whole set as one opaque key: sorted, deduplicated, comma-joined, no
  ;; spaces. Taking one member would render identically for nearly every machine
  ;; and discard the rest exactly where the evidence matters — a cell contested
  ;; by a machine that reached its value from several CESs. C++ joins, so a
  ;; runtime that picked would disagree with it on those contributions only,
  ;; which is the hardest class of divergence to find.
  ;;
  ;; Contested here on purpose: an uncontended cell resolves to its single
  ;; contributor and emits NO arbitration record (contract 4.5), so a second
  ;; machine has to write the same cell for one to exist at all.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     ;; `ces-b` asserts TWICE in the step, so it enters the contributor list
     ;; twice and the join has genuine deduplication to do. Declaring the id
     ;; twice in `sequences` would not exercise it — the loader keys sequences
     ;; by id, so the second declaration would simply replace the first.
     (fold-machine "m-joined" '(("ces-b" (1) 2) ("ces-a" (1)))
                   :input-offset 0 :output-offset 16))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "m-solo" '(("ces-solo" (1)))
                   :input-offset 0 :output-offset 16))
    (fold-step state (list 1))
    (let* ((records (reality-engine-lsp::reality-state-arbitration state))
           (contributors (and records
                              (reality-engine-lsp::arbitration-record-contributors
                               (first records))))
           (ces-ids (sort (mapcar #'reality-engine-lsp::contribution-ces-id contributors)
                          #'string<)))
      (assert-equal 1 (length records) "the shared output cell produced one arbitration record")
      ;; "ces-b" contributed twice and must appear once: the set is deduplicated
      ;; before it is joined, and sorted, so "ces-a" precedes "ces-b" regardless
      ;; of the order the machine enumerated them.
      (assert-equal '("ces-a,ces-b" "ces-solo") ces-ids
                    "cesId is the sorted, deduplicated, comma-joined contributing set")
      ;; The invariant that keeps this surface unchanged for nearly every
      ;; machine: one contributor renders as the bare id, with no delimiter.
      (assert-true (notany (lambda (id) (find #\, id))
                           (remove "ces-a,ces-b" ces-ids :test #'string=))
                   "a one-element set renders as the bare id, no comma")
      ;; No spaces anywhere — the join is a wire format shared with C++, and a
      ;; stray separator would make the two runtimes disagree on a key that
      ;; nothing compares numerically and everything compares as a string.
      (assert-true (notany (lambda (id) (find #\Space id)) ces-ids)
                   "the join carries no spaces")))

  ;; The PE dispatch ledger and its replay path carry the SET, not a scalar.
  ;;
  ;; Replay copies its fields off the original record, so it was reading a
  ;; `sequenceId` key that stopped existing when the ledger moved to
  ;; `sequenceIds`. `jstring` of a missing key is the supplied default, so the
  ;; replay recorded "" and looked perfectly well formed — a replay that had
  ;; forgotten which CESs produced the determination it was replaying. That is
  ;; the silent-degradation failure this whole line of work exists to remove, so
  ;; it is pinned rather than left to the next reader to notice.
  (let* ((pe-state (reality-engine-lsp::make-perception-state-from-config
                    :dimension 64
                    :reality-url "http://localhost:3299"
                    :localai-url "http://localhost:8000"
                    :localai-machine-dir "../localAIStack/data/machines"))
         (machine-json (reality-engine-lsp::obj
                        "name" "m-replay"
                        "metadata" (reality-engine-lsp::obj
                                    "dispatchableAgent" "agent-x"
                                    "aiTrigger" "trigger-x"
                                    "agentBinding" (reality-engine-lsp::obj
                                                    "agent" "agent-x"
                                                    "trigger" "trigger-x"
                                                    "mode" "advise"))))
         (operation (reality-engine-lsp::obj
                     "region" (reality-engine-lsp::obj "offset" 16 "length" 1)
                     "machineId" "m-replay"
                     "sequenceIds" (reality-engine-lsp::vectorize '("ces-a" "ces-b"))
                     "values" (reality-engine-lsp::vectorize '(1))
                     "governance" (reality-engine-lsp::obj "ragStatusCode" "RED"
                                                           "processStatus" "error")))
         (record (reality-engine-lsp::record-dispatch-envelope pe-state operation machine-json)))
    (assert-true record "dispatch record was not created")
    (assert-equal '("ces-a" "ces-b")
                  (reality-engine-lsp::jarray-list
                   (reality-engine-lsp::jget record "sequenceIds"))
                  "the ledger record carries the contributing set")
    (assert-equal '("ces-a" "ces-b")
                  (reality-engine-lsp::jarray-list
                   (reality-engine-lsp::jget
                    (reality-engine-lsp::jget
                     (reality-engine-lsp::jget record "envelope") "ces")
                    "sequenceIds"))
                  "and so does the envelope it wraps")
    (let ((replayed (reality-engine-lsp::replay-dispatch-record
                     pe-state (reality-engine-lsp::jstring record "id" ""))))
      (assert-true replayed "replay produced no record")
      (assert-equal '("ces-a" "ces-b")
                    (reality-engine-lsp::jarray-list
                     (reality-engine-lsp::jget replayed "sequenceIds"))
                    "a replay must not forget which CESs produced the determination")))

  ;; Refusal is about the missing parameter, not the data: the same machine with
  ;; k declared on its perceptualMapping folds and contributes.
  (let ((state (make-test-state 32)))
    (reality-engine-lsp::put-machine
     state
     (fold-machine "machine-declares-k" '(("seq-k-a" (1 0)) ("seq-k-b" (1 0)))
                   :transformation "strong-conjunction"
                   :chain-top 1))
    (let* ((step (fold-step state (list 1)))
           (op (first (fold-batch step))))
      (assert-true op "a declared chain top lets the same machine fold")
      (assert-equal '("seq-k-a" "seq-k-b") (merge-sequence-ids op)
                    "both sequences contributed to the folded value")
      ;; (.) over [1,1] at k=1 is max(0, 2 - 1) = 1; over [0,0] it extinguishes.
      (assert-equal '(1 0) (merge-values op)
                    "strong-conjunction folds the collection at the declared k")))

  ;; The declared k must survive a machine round trip, or a copy made by any
  ;; route that rebuilds from machine-json would refuse a fold its original
  ;; performed.
  (let* ((machine (fold-machine "machine-k-roundtrip" '(("seq-k" (1)))
                                :transformation "strong-disjunction"
                                :chain-top 3))
         (copy (machine-from-json (reality-engine-lsp::machine-json machine :full t))))
    (assert-equal 3 (reality-engine-lsp::machine-chain-top copy)
                  "outputAlphabetTop survives machine-json -> machine-from-json"))
  t)

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
      (assert-equal (list 1) (merge-values (first batch))
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
      (assert-true (find-if (lambda (op) (member "agent-seq" (merge-sequence-ids op) :test #'string=))
                            batch)
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
                    ;; `join` + a declared chain top, because the merge batch now
                    ;; carries the FOLD rather than the raw asserted output. The
                    ;; Boolean gates answer only "asserted or not", so folding an
                    ;; ordinal ladder with one flattens 0,1,2,3 to 0,1,1,1 — the
                    ;; packed payload below would become "FQ==" and the 2-bit
                    ;; cells would carry a flag instead of a rung
                    ;; (RealityEngine_CI#158). The binarising case is asserted
                    ;; directly beneath this block so the boundary stays visible.
                    "outputMergeTransformation" "join"
                    "perceptualMapping" (reality-engine-lsp::obj
                                         "input" (reality-engine-lsp::obj "offset" 0 "length" 1)
                                         "output" (reality-engine-lsp::obj "offset" 1 "length" 4)
                                         "bitsPerElement" 2
                                         "outputAlphabetTop" 3)
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
  ;; The same machine without the multi-valued declaration. Its ONE contributing
  ;; sequence asserts 0,1,2,3 and folds with the default `or`, which reports only
  ;; whether a cell was asserted — so the operation carries 0,1,1,1 and packs to
  ;; "FQ==" rather than "Gw==".
  ;;
  ;; This is the limit of the byte-identity property: a single contributing
  ;; sequence reads exactly as it did only where the fold is the IDENTITY on it,
  ;; which for a Boolean gate requires the asserted values to already be in
  ;; {0,1}. The corpus satisfies that today — of 1328 machines, 1326 fold with
  ;; `or` and assert only 0/1, and the two that assert an ordinal ladder,
  ;; FallDetection and FallSensorMotionPreaggregator, now declare `join`, which
  ;; preserves the value. So nothing in the corpus currently lands here.
  ;;
  ;; Pinned anyway, because the property is one declaration away from being
  ;; violated: a machine asserting outside {0,1} while leaving
  ;; outputMergeTransformation to default is well formed, loads without
  ;; complaint, and would have its ladder flattened to a flag by a fold that
  ;; never used to reach arbitration (RealityEngine_CI#158). Better asserted
  ;; here than discovered as a corpus divergence.
  (let* ((state (make-test-state 8))
         (machine (fold-machine "packed-machine-boolean" '(("packed-seq-boolean" (0 1 2 3)))
                                :input-offset 0 :output-offset 1)))
    (setf (reality-engine-lsp::mapping-bits-per-element
           (reality-engine-lsp::machine-mapping machine))
          2)
    (reality-engine-lsp::put-machine state machine)
    (let* ((step (reality-engine-lsp::process-perceptual-input
                  state (list 1)
                  :include-machine-results t
                  :include-perceptual-space t
                  :compact t))
           (op (first-merge step)))
      (assert-equal '(0 1 1 1) (merge-values op)
                    "a Boolean gate reports assertion, not the ordinal value")
      (assert-equal "FQ==" (reality-engine-lsp::jstring
                            (reality-engine-lsp::jget op "valuesPacked") "base64" "")
                    "and the packed payload follows the folded value")))
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
                    "unknown completion mapping should match CPP/AI error text"))
    (let* ((mapping (reality-engine-lsp::source-mapping-by-id state "agent-completion-risk"))
           (values (reality-engine-lsp::completion-values-from-content
                    "{\"completed\":1,\"failed\":0,\"confidence\":0.75,\"actionClass\":0}"
                    mapping)))
      (assert-equal '(1.0d0 0.0d0 0.75d0 0.0d0)
                    (coerce values 'list)
                    "provider completion extraction should follow sourceMappingId pointers")
      (assert-error
       (lambda ()
         (reality-engine-lsp::completion-values-from-content
          "{\"completed\":1,\"failed\":0,\"confidence\":0.75}"
          mapping))
       "provider completion extraction should reject missing required mapping pointers")))
  (let* ((state (reality-engine-lsp::make-perception-state-from-config
                 :dimension 5000
                 :reality-url "http://localhost:3299"
                 :localai-url "http://localhost:8000"
                 :localai-machine-dir "../localAIStack/data/machines"))
         (actor (reality-engine-lsp::state-actor "mcp-provider-test" state)))
    (unwind-protect
         (let* ((tools (reality-engine-lsp::mcp-build-tools actor "http://localhost:3299"))
                (names (mapcar (lambda (tool) (getf tool :name)) tools))
                (list-response (reality-engine-lsp::mcp-dispatch
                                (reality-engine-lsp::obj "method" "tools/list" "id" 1)
                                actor
                                tools))
                (wire-names (mapcar (lambda (tool)
                                      (reality-engine-lsp::jstring tool "name" ""))
                                    (reality-engine-lsp::jarray-list
                                     (reality-engine-lsp::jget
                                      (reality-engine-lsp::jget list-response "result")
                                      "tools"))))
                (completion-tool (find "integrations.completion" tools
                                       :key (lambda (tool) (getf tool :name))
                                       :test #'string=))
                (post-completion-tool (find "integrations.post_completion" tools
                                            :key (lambda (tool) (getf tool :name))
                                            :test #'string=))
                (provider-tool (find "integrations.dispatch_provider" tools
                                     :key (lambda (tool) (getf tool :name))
                                     :test #'string=))
                (openai-tool (find "integrations.dispatch_openai" tools
                                   :key (lambda (tool) (getf tool :name))
                                   :test #'string=))
                (ollama-tool (find "integrations.dispatch_ollama" tools
                                   :key (lambda (tool) (getf tool :name))
                                   :test #'string=)))
           (dolist (name '("integrations.completion"
                           "integrations.post_completion"
                           "integrations.dispatch_provider"
                           "integrations.dispatch_openai"
                           "integrations.dispatch_ollama"))
             (assert-true (find name names :test #'string=)
                          (format nil "LSP MCP should expose ~a" name)))
           (assert-true (find "integrations.dispatch_openai" wire-names :test #'string=)
                        "MCP tools/list should publish provider dispatcher tools")
           (assert-true post-completion-tool
                        "LSP MCP should expose the post_completion alias")
           (assert-true provider-tool
                        "LSP MCP should expose the generic provider dispatcher")
           (assert-true ollama-tool
                        "LSP MCP should expose the Ollama dispatcher")
           (let ((denied (funcall (getf openai-tool :fn)
                                  (reality-engine-lsp::obj "dispatch_id" "d-1"))))
             (assert-true (reality-engine-lsp::jbool denied "isError" nil)
                          "mutating provider MCP tools should fail closed by default"))
           (let* ((allowed (let ((reality-engine-lsp::*mcp-allow-mutation-override* t))
                             (funcall (getf completion-tool :fn)
                                      (reality-engine-lsp::obj
                                       "provider" "mcp-test"
                                       "agent" "mcp-test"
                                       "sourceMappingId" "agent-completion-risk"
                                       "values" (reality-engine-lsp::vectorize (list 1 0 0.5 0))))))
                  (text (reality-engine-lsp::jstring
                         (aref (reality-engine-lsp::jget allowed "content") 0)
                         "text"
                         ""))
                  (payload (reality-engine-lsp::parse-json text))
                  (source (reality-engine-lsp::jget (reality-engine-lsp::jget payload "signal") "source")))
             (assert-equal nil
                           (reality-engine-lsp::jget allowed "isError")
                           "policy-enabled completion MCP call should not be marked as error")
             (assert-equal "agent.mcp-test.completion"
                           (reality-engine-lsp::jstring source "sensorId" "")
                           "policy-enabled MCP completion should commit through source mappings")))
      (reality-engine-lsp::stop-actor actor)))
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
  ;; ── Cold catalog vs no dispatch binding (#63) ─────────────────────────
  ;; Both used to leave record-dispatch-envelope as a bare NIL and land on the
  ;; droppedNoDispatch counter, so a PE that lost the startup race with its RE
  ;; reported a minute of transient infrastructure failure as deliberate
  ;; no-binding decisions.
  (let ((state (reality-engine-lsp::make-perception-state-from-config
                :dimension 16
                :reality-url "http://localhost:3299"
                :localai-url "http://localhost:8000"
                :localai-machine-dir "../localAIStack/data/machines"))
        (operation (reality-engine-lsp::obj
                    "machineId" "machine-cold"
                    "sequenceIds" (reality-engine-lsp::vectorize (list "seq-cold"))
                    "values" (reality-engine-lsp::vectorize (list 1))
                    "governance" (reality-engine-lsp::obj "ownerTeam" "t"
                                                          "ragStatusCode" "RED"
                                                          "processStatus" "error"))))
    (assert-true (reality-engine-lsp::machine-catalog-cold-p state)
                 "a freshly constructed catalog is cold")
    (multiple-value-bind (record reason)
        (reality-engine-lsp::record-dispatch-envelope state operation)
      (assert-true (not record) "a cold catalog cannot resolve a dispatch")
      (assert-equal :catalog-cold reason
                    "a drop against a cold catalog reports :catalog-cold"))

    ;; A successful refresh warms the catalog even when the RE holds no
    ;; machines — "loaded and empty" is not "never loaded".
    (bt:with-lock-held ((reality-engine-lsp::perception-state-machine-catalog-lock state))
      (setf (reality-engine-lsp::perception-state-machine-catalog-refreshed-at state)
            (reality-engine-lsp::now-ms)))
    (assert-true (not (reality-engine-lsp::machine-catalog-cold-p state))
                 "a catalog that loaded zero machines is warm, not cold")
    (multiple-value-bind (record reason)
        (reality-engine-lsp::record-dispatch-envelope state operation)
      (assert-true (not record) "an absent machine still drops")
      (assert-equal :no-dispatch reason
                    "a drop against a warm catalog reports :no-dispatch"))

    ;; A machine present but declaring no agent/trigger is a genuine
    ;; no-binding decision, not a cold-catalog drop.
    (bt:with-lock-held ((reality-engine-lsp::perception-state-machine-catalog-lock state))
      (setf (gethash "machine-cold"
                     (reality-engine-lsp::perception-state-machine-catalog state))
            (reality-engine-lsp::obj "id" "machine-cold"
                                     "metadata" (reality-engine-lsp::obj))))
    (multiple-value-bind (record reason)
        (reality-engine-lsp::record-dispatch-envelope state operation)
      (assert-true (not record) "a machine with no binding drops")
      (assert-equal :no-dispatch reason
                    "no agent/trigger is a no-binding drop, not a cold one"))

    (let ((status (reality-engine-lsp::triggers-status-json state)))
      (assert-equal 0 (reality-engine-lsp::jnumber status "droppedCatalogCold" nil)
                    "triggers status exposes the cold-drop counter")
      (assert-true (not (reality-engine-lsp::jbool status "machineCatalogCold" t))
                   "triggers status reports the warm catalog as not cold")))

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
    ;; Derived freshness is NOT part of the sensor payload (#176). `ageMs` and
    ;; `stale` were emitted here and by the Manager TypeScript PE, and by
    ;; neither C++ nor Scala, so GET /api/sources could not be byte-compared
    ;; across runtimes at all. They were removed rather than canonicalised:
    ;; nothing consumed them, and `active` already answers what `stale` was for.
    ;;
    ;; This assertion still required them after that change, which is how it
    ;; went red on main.
    (let ((js (reality-engine-lsp::source-json stale)))
      (assert-true (eq (reality-engine-lsp::jget js "stale" :missing) :missing)
                   "sensor payload carries no `stale` key (SURFACE_SPEC.md, Sensor source payload)")
      (assert-true (eq (reality-engine-lsp::jget js "ageMs" :missing) :missing)
                   "sensor payload carries no `ageMs` key")
      ;; What replaced them: a caller wanting the arithmetic has both operands.
      (assert-true (> (reality-engine-lsp::jnumber js "lastUpdated" 0) 0)
                   "lastUpdated stays on the payload")
      (assert-true (> (reality-engine-lsp::jnumber js "ttlMs" 0) 0)
                   "ttlMs stays on the payload"))

    ;; ── Reset validates activity, it does not assign it (#65) ─────────────
    ;; RealityEngine_CI#163 point 3. The expired sensor already contributed
    ;; zeros above; what was wrong was the reported flag, which reset forced
    ;; back to a value nothing had checked.
    (assert-true (not (reality-engine-lsp::jbool (reality-engine-lsp::source-json stale) "active" t))
                 "an expired sensor must not report active, even before the reset")
    (reality-engine-lsp::reset-perception-engine engine)
    (assert-true (reality-engine-lsp::source-active-p fresh)
                 "a sensor holding a value inside its TTL validates active across reset")
    (assert-true (not (reality-engine-lsp::source-active-p stale))
                 "a sensor whose TTL expired validates inactive across reset")
    (assert-true (not (reality-engine-lsp::jbool (reality-engine-lsp::source-json stale) "active" t))
                 "GET /api/sources reports active=false for the expired sensor")

    ;; A later value must re-earn activity, or the source is stranded
    ;; contributing zeros while holding a fresh reading.
    (reality-engine-lsp::record-sensor-value stale (list 0.9d0))
    (assert-true (reality-engine-lsp::source-active-p stale)
                 "a value arriving re-activates a sensor reset had validated inactive")
    (assert-equal 0.9d0 (nth 1 (reality-engine-lsp::assemble-perception-vector engine))
                  "the re-activated sensor contributes its value again"))

  ;; Reset validates the other kinds too: a test source with an empty
  ;; sequence supplies nothing, so calling it active would be assignment
  ;; rather than validation.  Reset does not preserve an explicit pause —
  ;; an operator-deactivated source is run state, and reset clears run state.
  (let* ((engine (reality-engine-lsp::make-perception-engine-state 4))
         (empty (reality-engine-lsp::make-source
                 :id "t-empty" :kind "test" :name "empty test"
                 :active-p t :cursor 3
                 :region (reality-engine-lsp::make-region :offset 0 :length 1)
                 :inputs nil :loop-p t))
         (filled (reality-engine-lsp::make-source
                  :id "t-filled" :kind "test" :name "filled test"
                  :active-p nil :cursor 2
                  :region (reality-engine-lsp::make-region :offset 1 :length 1)
                  :inputs (list (list 1.0d0) (list 0.0d0)) :loop-p t))
         (sim (reality-engine-lsp::make-source
               :id "s-sim" :kind "simulated" :name "sim"
               :active-p nil :cursor 7
               :region (reality-engine-lsp::make-region :offset 2 :length 1)
               :pattern "constant" :dc-offset 0.5d0)))
    (dolist (s (list empty filled sim))
      (reality-engine-lsp::ensure-source-id engine s))
    (reality-engine-lsp::reset-perception-engine engine)
    (assert-true (not (reality-engine-lsp::source-active-p empty))
                 "a test source with an empty sequence validates inactive")
    (assert-true (reality-engine-lsp::source-active-p filled)
                 "a test source with a non-empty sequence validates active")
    (assert-true (reality-engine-lsp::source-active-p sim)
                 "a simulated source validates active; reset does not preserve a pause")
    (assert-equal 0 (reality-engine-lsp::source-cursor filled)
                  "reset rewinds the test cursor")
    ;; ...and only the test cursor (#64). `cursor' is read and advanced by the
    ;; "test" branches alone, so zeroing it on other kinds wrote a field that
    ;; is not theirs. C++ and Scala rewind test cursors only.
    (assert-equal 7 (reality-engine-lsp::source-cursor sim)
                  "reset leaves a simulated source's cursor alone"))

  ;; ── Ingress is the only origin of activity for an integration source ───
  ;; An integration (sensor-kind) source that has never received a value must
  ;; report inactive at every observation point, through any sequence of
  ;; register and reset. Only a value arriving may originate activity, and it
  ;; expires with that value's TTL.
  (let* ((engine (reality-engine-lsp::make-perception-engine-state 4))
         (unfed (reality-engine-lsp::make-source
                 :id "s-unfed" :kind "sensor" :name "never fed"
                 ;; even asked for active at registration
                 :active-p t
                 :region (reality-engine-lsp::make-region :offset 0 :length 1)
                 :sensor-id "unfed-sid"
                 :last-value nil :last-updated 0 :ttl-ms 5000)))
    (reality-engine-lsp::ensure-source-id engine unfed)
    (assert-true (not (reality-engine-lsp::jbool
                       (reality-engine-lsp::source-json unfed) "active" t))
                 "an unfed sensor reports inactive at registration")
    (reality-engine-lsp::reset-perception-engine engine)
    (assert-true (not (reality-engine-lsp::source-active-p unfed))
                 "reset cannot activate a sensor that was never fed")
    (assert-true (not (reality-engine-lsp::jbool
                       (reality-engine-lsp::source-json unfed) "active" t))
                 "an unfed sensor still reports inactive after reset")
    (reality-engine-lsp::record-sensor-value unfed (list 0.4d0))
    (assert-true (reality-engine-lsp::jbool
                  (reality-engine-lsp::source-json unfed) "active" nil)
                 "the first value earns activity")
    ;; ...and it expires with that value's TTL.
    (setf (reality-engine-lsp::source-last-updated unfed)
          (- (reality-engine-lsp::now-ms) 60000))
    (assert-true (not (reality-engine-lsp::jbool
                       (reality-engine-lsp::source-json unfed) "active" t))
                 "ingress-earned activity expires with the value's TTL"))

  ;; ── Assembled vector: a source owns its region (#53) ──────────────────
  ;; Zero-valued cells used to be skipped, so a cell a source drove low kept
  ;; whatever the persistent vector held from the previous step. The RE was
  ;; then handed an input the source never published, and a CES needing a cell
  ;; to go low never matched again — no output, no error. Measured against the
  ;; DLX011 req/ack handshake, this runtime fired nothing across six pushes
  ;; where C++ and Scala each fired twice.
  (let* ((engine (reality-engine-lsp::make-perception-engine-state 4))
         (now (reality-engine-lsp::now-ms))
         (source (reality-engine-lsp::make-source
                  :id "s-region" :kind "sensor" :name "region owner"
                  :active-p t
                  :region (reality-engine-lsp::make-region :offset 0 :length 2)
                  :sensor-id "region-sid"
                  :last-value (list 1.0d0 0.0d0)
                  :last-updated now :ttl-ms 60000)))
    (reality-engine-lsp::ensure-source-id engine source)
    ;; Stale state under the region, exactly as adopting the RE's post-merge
    ;; perceptual space leaves it.
    (reality-engine-lsp::update-from-perceptual-space engine (list 1.0d0 1.0d0 0.0d0 0.0d0))
    (let ((vec (reality-engine-lsp::assemble-perception-vector engine)))
      (assert-equal 1.0d0 (nth 0 vec) "source's 1.0 is written")
      (assert-equal 0.0d0 (nth 1 vec) "source's 0.0 overwrites stale 1.0 rather than being skipped"))
    ;; The handshake step that used to be impossible: drive the region low
    ;; where it was high.
    (setf (reality-engine-lsp::source-last-value source) (list 0.0d0 1.0d0)
          (reality-engine-lsp::source-last-updated source) (reality-engine-lsp::now-ms))
    (let ((vec (reality-engine-lsp::assemble-perception-vector engine)))
      (assert-equal 0.0d0 (nth 0 vec) "a cell driven low reaches the RE as low")
      (assert-equal 1.0d0 (nth 1 vec) "the cell driven high reaches the RE as high")))

  ;; Cells are clamped to [0,1], as C++ and Scala both do. An unclamped 2.0
  ;; reaching a comparator whose threshold is above 1.0 makes the same machine
  ;; decide differently per runtime.
  (let* ((engine (reality-engine-lsp::make-perception-engine-state 3))
         (source (reality-engine-lsp::make-source
                  :id "s-clamp" :kind "sensor" :name "clamp"
                  :active-p t
                  :region (reality-engine-lsp::make-region :offset 0 :length 3)
                  :sensor-id "clamp-sid"
                  :last-value (list 2.5d0 -1.0d0 0.4d0)
                  :last-updated (reality-engine-lsp::now-ms) :ttl-ms 60000)))
    (reality-engine-lsp::ensure-source-id engine source)
    (let ((vec (reality-engine-lsp::assemble-perception-vector engine)))
      (assert-equal 1.0d0 (nth 0 vec) "above-range value clamps to 1.0")
      (assert-equal 0.0d0 (nth 1 vec) "below-range value clamps to 0.0")
      (assert-equal 0.4d0 (nth 2 vec) "in-range value passes through unchanged")))

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

  ;; ── Cross-runtime parity (C++ + LSP + Scala) ──────────────────────────
  ;; Skipped automatically when the corpus isn't sibling-located (e.g. CI runs
  ;; that haven't checked out RealityEngine_Machines).
  (cond
    ((null +corpus-machines-dir+)
     (format t "~&[parity] skipping — RealityEngine_Machines/machines not found alongside RealityEngine_LSP~%"))
    (t
     (let* ((result (walk-corpus-for-envelopes +corpus-machines-dir+))
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
       ;;   RealityEngine_CPP e2e_ai_trigger_dispatch            (C++ exec)
       (format t "~&[parity] LSP walked corpus: machines=~a sequences=~a outputs=~a envelopes=~a (CPP target: 1058/5126/4251/4251)~%"
               machines sequences outputs envelopes)
       (assert-equal 1058  machines  "AiTriggerDispatch parity — machinesWithTriggers != 1058 (CPP value)")
       (assert-equal 5126 sequences "AiTriggerDispatch parity — inputSequencesRun  != 5126 (CPP value)")
       (assert-equal 4251 outputs   "AiTriggerDispatch parity — outputsProduced    != 4251 (CPP value)")
       (assert-equal 4251 envelopes "AiTriggerDispatch parity — envelopesResolved  != 4251 (CPP value)"))

     ;; PE dispatch parity — same corpus and counts, but exercised through the
     ;; PE-owned dispatch ledger path that records async bridge envelopes after
     ;; RE returns a mergeBatch.
     (let* ((pe-result (walk-corpus-through-pe-dispatch +corpus-machines-dir+))
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
       (format t "~&[parity] LSP PE dispatch corpus: machines=~a sequences=~a outputs=~a records=~a (CPP target: 1058/5126/4251/4251)~%"
               machines sequences outputs records)
       (assert-equal nil failures "PE dispatch parity — corpus walk must produce no failures")
       (assert-equal 1058  machines  "PE dispatch parity — machinesWithTriggers != 1058 (CPP value)")
       (assert-equal 5126 sequences "PE dispatch parity — inputSequencesRun  != 5126 (CPP value)")
       (assert-equal 4251 outputs   "PE dispatch parity — outputsProduced    != 4251 (CPP value)")
       (assert-equal 4251 records   "PE dispatch parity — ledger records      != 4251 (CPP envelopesResolved)")
       (assert-equal 4251
                     (reality-engine-lsp::perception-state-envelopes-created pe-state)
                     "PE dispatch parity — envelopesCreated counter drifted"))

     ;; AGX051 pin — urgent_maint resolves to aquaculture_predictive_maintenance_agent / RED / sla=900.
     (let* ((m (reality-engine-lsp::load-machine-from-file
                (reality-engine-lsp::resolve-machine-json-path +corpus-machines-dir+ "AGX051_yuma-aqua-maintenance-forecaster.json")))
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
               (reality-engine-lsp::resolve-machine-json-path +corpus-machines-dir+ "AGX055_yuma-facility-ai-synthesis-bridge.json"))))
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
     (let* ((bridge-root (reality-engine-lsp::parse-json (reality-engine-lsp::safe-read-file (namestring (reality-engine-lsp::resolve-machine-json-path +corpus-machines-dir+ "AGX055_yuma-facility-ai-synthesis-bridge.json")))))
            (yield-root  (reality-engine-lsp::parse-json (reality-engine-lsp::safe-read-file (namestring (reality-engine-lsp::resolve-machine-json-path +corpus-machines-dir+ "AgYieldOptimizationAI.json")))))
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
         (assert-equal '("agx-051-urgent-maint") (merge-sequence-ids m051-final) "AGX051 sequenceIds")
         (assert-equal '(1 0 0 0) (merge-values m051-final) "AGX051 values != URGENT_MAINT one-hot")
         (let ((gov (reality-engine-lsp::jget m051-final "governance")))
           (assert-equal "RED"   (reality-engine-lsp::jstring gov "ragStatusCode" "") "AGX051 gov.rag")
           (assert-equal 900     (reality-engine-lsp::jnumber gov "slaSeconds" nil)   "AGX051 gov.sla != 900")))

       ;; Stage 2 — carry-forward perceptual space, zero tier-1 sensors, AGX055 fires
       (let* ((stage2 (reality-engine-lsp::perceptual-space-snapshot (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage2 40 4))  (_ (zero-region stage2 84 4))
              (_ (zero-region stage2 184 4)) (_ (zero-region stage2 228 4))
              (s2 (reality-engine-lsp::process-perceptual-input
                   state stage2 :include-machine-results t :include-perceptual-space t))
              (batch2 (reality-engine-lsp::jget s2 "mergeBatch"))
              (m055 (find-merge-by-machine batch2 agx055-id)))
         (declare (ignore _))
         (assert-true m055 "stage 2: AGX055 did not fire — bridge contract broken")
         (when m055
           (assert-equal '("agx-055-aqua-urgent") (merge-sequence-ids m055) "AGX055 sequenceIds")
           (assert-equal '(1 0 0 0 0 0 0 0 0 0 0 0) (merge-values m055) "AGX055 values != AQUA_URGENT one-hot")
           (let ((gov (reality-engine-lsp::jget m055 "governance")))
             (assert-equal "RED" (reality-engine-lsp::jstring gov "ragStatusCode" "") "AGX055 gov.rag")
             (assert-equal 600   (reality-engine-lsp::jnumber gov "slaSeconds" nil)   "AGX055 gov.sla != 600")))
         (assert-equal nil (find-merge-by-machine batch2 yield-id) "stage 2: AgYieldOptimizationAI fired before AGX055 projection landed"))

       ;; Stage 3 — projection landing: perceptualSpace[3959]=1, [3960:3971]=0
       (let* ((stage3 (reality-engine-lsp::perceptual-space-snapshot (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage3 40 4))  (_ (zero-region stage3 84 4))
              (_ (zero-region stage3 184 4)) (_ (zero-region stage3 228 4))
              (_ (zero-region stage3 256 16)))
         (declare (ignore _))
         (assert-true (>= (length stage3) 3971) "stage 3: perceptualSpace not grown to 3971")
         (assert-equal 1 (round (elt stage3 3959)) "stage 3: AQUA_URGENT bit at [3959] missing")
         (loop for i from 3960 below 3971 do
               (assert-equal 0 (round (elt stage3 i)) (format nil "stage 3: stray bit at [~a] — one-hot projection violated" i)))
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
       (let* ((stage2 (reality-engine-lsp::perceptual-space-snapshot (reality-engine-lsp::reality-state-perceptual-space state)))
              (_ (zero-region stage2 40 4))  (_ (zero-region stage2 84 4))
              (_ (zero-region stage2 184 4)) (_ (zero-region stage2 228 4))
              (s2 (reality-engine-lsp::process-perceptual-input state stage2 :include-machine-results t :include-perceptual-space t))
              (m055 (find-merge-by-machine (reality-engine-lsp::jget s2 "mergeBatch") agx055-id)))
         (declare (ignore _))
         (assert-true m055 "stable path: AGX055 did not fire FACILITY_STABLE")
         (when m055
           (assert-equal '("agx-055-facility-stable") (merge-sequence-ids m055) "stable path: sequenceIds")
           (assert-equal '(0 0 0 0 0 0 0 0 0 0 0 1) (merge-values m055) "stable path: values != FACILITY_STABLE one-hot")
           (assert-equal "GREEN" (reality-engine-lsp::jstring (reality-engine-lsp::jget m055 "governance") "ragStatusCode" "") "stable path: rag != GREEN"))))))

  ;; ── /api/perceive input forms (regression for the jarray-p / missing-key
  ;; trap: a missing key is NIL, NIL is a list, so `jarray-p' matched every
  ;; optional array key and the first cond branch shadowed the rest) ────────
  (let ((state (make-test-state 16)))
    (assert-true (not (reality-engine-lsp::jarray-present-p
                       (reality-engine-lsp::obj) "vector"))
                 "jarray-present-p: absent key must not count as an array")
    (assert-true (reality-engine-lsp::jarray-present-p
                  (reality-engine-lsp::obj "vector" (reality-engine-lsp::vectorize '()))
                  "vector")
                 "jarray-present-p: present-but-empty array must count")

    ;; Dense form.
    (assert-equal '(1.0d0 2.0d0)
                  (reality-engine-lsp::assemble-input-vector
                   state (reality-engine-lsp::obj
                          "vector" (reality-engine-lsp::vectorize '(1.0d0 2.0d0))))
                  "assemble-input-vector: dense vector form")

    ;; Sparse form — previously unreachable; index 3 must land in slot 3.
    (let ((sparse (reality-engine-lsp::assemble-input-vector
                   state (reality-engine-lsp::obj
                          "sparseVector" (reality-engine-lsp::vectorize
                                          (list (reality-engine-lsp::obj "index" 3 "value" 7.0d0)))))))
      (assert-true sparse "assemble-input-vector: sparseVector returned nil")
      (assert-equal 7.0d0 (nth 3 sparse) "assemble-input-vector: sparseVector index 3")
      (assert-equal 0.0d0 (nth 0 sparse) "assemble-input-vector: sparseVector pads with zeros"))

    ;; Domain form — previously unreachable.
    (let ((domain (reality-engine-lsp::assemble-input-vector
                   state (reality-engine-lsp::obj
                          "domainVectors" (reality-engine-lsp::vectorize
                                           (list (reality-engine-lsp::obj
                                                  "offset" 2
                                                  "values" (reality-engine-lsp::vectorize '(5.0d0 6.0d0)))))))))
      (assert-true domain "assemble-input-vector: domainVectors returned nil")
      (assert-equal 5.0d0 (nth 2 domain) "assemble-input-vector: domainVectors offset 2")
      (assert-equal 6.0d0 (nth 3 domain) "assemble-input-vector: domainVectors offset 3"))

    ;; No recognised input key at all still yields nil so the route 400s.
    (assert-true (null (reality-engine-lsp::assemble-input-vector
                        state (reality-engine-lsp::obj "unrelated" 1)))
                 "assemble-input-vector: unknown body must yield nil"))

  ;; JSON false must survive a round trip as false, not collapse to null.
  ;;
  ;; yason maps both `false' and `null' to NIL unless booleans are parsed as
  ;; symbols, and write-json renders NIL as `null'.  Every boolean false on the
  ;; LSP surface therefore came back as null and disagreed with C++ and Scala —
  ;; the cross-runtime byte-equivalence drift in RealityEngine_CI#91.
  (let* ((source "{\"a\":false,\"b\":true,\"c\":null,\"d\":[false,true,null],\"e\":{\"f\":false},\"s\":\"false\"}")
         (round-trip (with-output-to-string (stream)
                       (reality-engine-lsp::write-json
                        (reality-engine-lsp::parse-json source) stream))))
    (assert-equal source round-trip
                  "parse-json/write-json must round-trip false, true and null exactly")

    (let ((parsed (reality-engine-lsp::parse-json source)))
      ;; false and null must remain distinguishable after parsing.
      (assert-true (eq (reality-engine-lsp::jget parsed "a")
                       reality-engine-lsp::+json-false+)
                   "parsed false must be +json-false+, not NIL")
      (assert-true (null (reality-engine-lsp::jget parsed "c"))
                   "parsed null must be NIL")
      (assert-true (eq (reality-engine-lsp::jget parsed "b") t)
                   "parsed true must be T")
      ;; jbool reads the marker as false rather than as a truthy symbol.
      (assert-true (null (reality-engine-lsp::jbool parsed "a" t))
                   "jbool must read +json-false+ as false")))

  ;; Outbound HTTP must be bounded (#40).
  ;;
  ;; A listening socket that never accepts still completes the TCP handshake
  ;; from the backlog, so the client connects and then waits on a reply that
  ;; never comes — the exact shape that made /api/integrations/ollama/status
  ;; outlive the regression harness's request budget. Drakma has no usable
  ;; read timeout on SBCL, so without an explicit bound this call never
  ;; returns and the test hangs rather than fails.
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
         (port (usocket:get-local-port listener))
         (url (format nil "http://127.0.0.1:~a/api/tags" port))
         (start (get-internal-real-time)))
    (unwind-protect
         (progn
           ;; Must be an ERROR, not a bare serious-condition: every caller
           ;; wraps these in handler-case (error ...), and a timeout that is
           ;; not an error would escape as a 500 instead of reachable:false.
           (assert-error (lambda () (reality-engine-lsp::http-get-json url))
                         "a peer that never answers must fail rather than hang")
           (let ((elapsed (/ (float (- (get-internal-real-time) start))
                             internal-time-units-per-second)))
             (assert-true (< elapsed 30)
                          (format nil "bounded request should give up promptly; took ~,1f s"
                                  elapsed))))
      (usocket:socket-close listener)))

  ;; ── Output arbiter conformance (ARBITER_CONTRACT.md) ──────────────────────
  ;;
  ;; These cover the properties no live probe can establish: that resolution
  ;; does not depend on the order contributions arrive in. That is the whole
  ;; basis for accumulating them in an actor mailbox, and a violation would be
  ;; invisible in any single run.
  (flet ((contrib (value provider origin &optional rag)
           (reality-engine-lsp::make-contribution
            :cell 1 :value value :provider provider :origin-id origin
            :ces-id "seq" :output-vector-id "ov" :rag-status-code rag))
         (entry (rule &optional within)
           (reality-engine-lsp::make-arbitration-entry
            :cell 1 :rule rule :within-rank within)))

    ;; An unregistered surface must not outrank a reading by default.
    (assert-equal :generated (reality-engine-lsp::determinism-of "some-future-surface")
                  "unregistered provider classifies as generated")
    (assert-equal :deterministic (reality-engine-lsp::determinism-of "machine")
                  "machine is deterministic")

    ;; The generated value is larger, so a MAX-based merge would take it.
    (multiple-value-bind (value record)
        (reality-engine-lsp::resolve-cell
         1 0 (list (contrib 0 "machine" "m1") (contrib 1 "acp" "a1")) (entry "PRECEDENCE"))
      (assert-equal 0 value "PRECEDENCE: generated never overrides deterministic")
      (assert-equal 1 (length (reality-engine-lsp::arbitration-record-suppressed record))
                    "the generated contribution is recorded as suppressed"))

    ;; Two machine determinations plus an agent: RED asserts 0, AMBER asserts 1.
    ;; MAX would take 1; SEVERITY within the winning class must take 0.
    (assert-equal 0 (reality-engine-lsp::resolve-cell
                     1 0 (list (contrib 1 "machine" "m-amber" "AMBER")
                               (contrib 0 "machine" "m-red" "RED")
                               (contrib 1 "acp" "a1"))
                     (entry "PRECEDENCE" "SEVERITY"))
                  "withinRank SEVERITY is applied rather than falling back to MAX")

    (assert-equal 0 (reality-engine-lsp::resolve-cell
                     1 0 (list (contrib 1 "machine" "a" "AMBER") (contrib 0 "machine" "b" "RED"))
                     (entry "SEVERITY"))
                  "SEVERITY resolves by RAG rank before value")

    ;; A single contributor resolves to itself and emits no record (contract 4.5).
    (multiple-value-bind (value record)
        (reality-engine-lsp::resolve-cell 1 0 (list (contrib 0.42d0 "acp" "a1")) nil)
      (assert-equal 0.42d0 value "a single contributor resolves to itself")
      (assert-true (null record) "a single contributor emits no record"))

    ;; Acceptance criteria 2 and 4 — the externally visible form of the
    ;; commutative-monoid requirement, and the property most likely to break
    ;; once sources arrive asynchronously.
    (let* ((base (list (contrib 1 "machine" "m-amber" "AMBER")
                       (contrib 0 "machine" "m-red" "RED")
                       (contrib 0.7d0 "acp" "a1")
                       (contrib 0.3d0 "mqtt" "s1")))
           (e (entry "PRECEDENCE" "SEVERITY"))
           (first-result (reality-engine-lsp::resolve-cell 1 0 base e)))
      (dotimes (i 24)
        (let ((shuffled (sort (copy-list base) #'< :key (lambda (c) (random 1000)))))
          (assert-equal first-result (reality-engine-lsp::resolve-cell 1 0 shuffled e)
                        "resolution is invariant under contribution order")))))

  ;; ── Perceptual space representation (#60) ─────────────────────────────
  ;; The space is a growable double-float vector, not a list. These guard the
  ;; properties the conversion has to preserve rather than the speed it buys.
  (let ((space (reality-engine-lsp::make-perceptual-space 8)))
    (assert-true (vectorp space) "the perceptual space is a vector")
    (assert-equal 8 (length space) "length is the logical dimension")
    (assert-true (every #'zerop space) "a fresh space is zeroed")

    ;; Growth (regression guard for #24: a machine mapping past the dimension
    ;; must still be addressable). Contents survive; new cells read zero.
    (setf (aref space 3) 0.5d0)
    (let ((grown (reality-engine-lsp::grow-perceptual-space space 40)))
      (assert-equal 40 (length grown) "growth extends the logical length")
      (assert-equal 0.5d0 (aref grown 3) "growth preserves existing cells")
      (assert-true (every #'zerop (subseq grown 8 40)) "grown cells read zero")
      ;; Growth doubles capacity, so it is amortised rather than one
      ;; reallocation per appended cell.
      (assert-true (>= (array-dimension grown 0) 40) "capacity covers the length")

      ;; A snapshot must detach: VECTORIZE returns a vector argument unchanged,
      ;; so serializing the live space would alias it and the next step's
      ;; in-place writes would rewrite an already-emitted response.
      (let ((snap (reality-engine-lsp::perceptual-space-snapshot grown)))
        (setf (aref grown 3) 0.9d0)
        (assert-equal 0.5d0 (elt snap 3)
                      "a snapshot does not track later writes to the space")
        (assert-true (not (eq snap grown)) "a snapshot is a distinct object"))))

  ;; extract-region and merge-region are O(region), and merge-region still
  ;; grows the space when the region runs past the end.
  (let ((space (reality-engine-lsp::make-perceptual-space 8)))
    (reality-engine-lsp::merge-region
     space (reality-engine-lsp::make-region :offset 2 :length 3) (list 1 0 1))
    (assert-equal '(1.0d0 0.0d0 1.0d0)
                  (reality-engine-lsp::extract-region
                   space (reality-engine-lsp::make-region :offset 2 :length 3))
                  "merge-region writes, extract-region reads back")
    (assert-equal '(0.0d0 0.0d0)
                  (reality-engine-lsp::extract-region
                   space (reality-engine-lsp::make-region :offset 20 :length 2))
                  "extract-region zero-fills past the end")
    (let ((grown (reality-engine-lsp::merge-region
                  space (reality-engine-lsp::make-region :offset 30 :length 2)
                  (list 1 1))))
      (assert-true (>= (length grown) 32) "merge-region grows to fit the region")
      (assert-equal '(1.0d0 1.0d0)
                    (reality-engine-lsp::extract-region
                     grown (reality-engine-lsp::make-region :offset 30 :length 2))
                    "values written past the old end are readable"))
    ;; Lists are still accepted — universalInputSpace callers extract from a
    ;; plain numbers-from-json result, which is not the shared space.
    (assert-equal '(2 3)
                  (reality-engine-lsp::extract-region
                   (list 1 2 3 4) (reality-engine-lsp::make-region :offset 1 :length 2))
                  "extract-region still reads a list"))

  (output-merge-tests)
  (fold-placement-tests)

  ;; The cesgen oracle set — see tests/oracle-parity-tests.lisp. Runs last:
  ;; it walks the whole corpus and is by far the slowest check here.
  (oracle-parity-tests)

  (format t "~&RealityEngine_LSP core tests passed.~%")
  t)
