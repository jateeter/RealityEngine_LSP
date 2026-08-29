;;;; The cesgen oracle set, run against the LSP engine.
;;;;
;;;; RealityEngine_Machines/oracles.json is derived from the corpus by
;;;; RealityEngine_CI/scripts/cesgen-oracles.mjs: for every vector that emits
;;;; output, given input pattern P the engine must produce output O at the
;;;; machine's output region after the right number of steps.
;;;;
;;;; The set's value is cross-runtime — identical pass-sets over one oracle set
;;;; is parity evidence no single-runtime suite can give. It was consumed by C++
;;;; alone, so "identical pass-sets" was never actually checked; the generator's
;;;; docstring described a contract with participants that had never joined.
;;;; This is the LSP half, alongside
;;;; RealityEngine_CPP/tests/cesgen_oracles_parity.cpp and
;;;; RealityEngine_Scala/.../CesgenOraclesParitySpec.scala.
;;;;
;;;; The three harnesses are deliberately the same shape — same oracle file,
;;;; same per-machine isolation, same comparison rule — because a harness that
;;;; differs in what it asserts cannot demonstrate parity of what it asserts
;;;; about.
;;;;
;;;; Skips rather than fails when the corpus or the oracle set is absent: this
;;;; suite runs in environments that check out RealityEngine_LSP alone.

(in-package #:reality-engine-lsp.tests)

(defparameter *oracle-repo*
  (merge-pathnames "../RealityEngine_Machines/"
                   (asdf:system-source-directory '#:reality-engine-lsp))
  "RealityEngine_Machines, as a sibling checkout.")

(defun oracle-file ()
  (merge-pathnames "oracles.json" *oracle-repo*))

(defun oracle-machines-dir ()
  (merge-pathnames "machines/" *oracle-repo*))

(defun read-text-file (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun corpus-file-map (dir)
  "Basename -> path over the whole corpus.

Walks recursively: the corpus lives in machines/domains/<name>/ and filenames
are globally unique, which is the addressing the oracles rely on."
  (let ((map (make-hash-table :test #'equal)))
    (labels ((walk (d)
               (dolist (entry (directory (merge-pathnames "*.*" d)))
                 (cond
                   ((null (pathname-name entry)) (walk entry))
                   ((equal (pathname-type entry) "json")
                    (setf (gethash (format nil "~a.json" (pathname-name entry)) map)
                          entry))))))
      (walk dir))
    map))

;; A machine presents ONE Reality Event per instant: the engine folds its
;; collection of potential outputs at the completion boundary
;; (RealityEngine_CI/docs/FOLD_PLACEMENT.md).  mergeBatch carries one operation
;; per machine, so an individual outputVector — the unit the oracles are
;; generated in, and the right unit to generate — is no longer separately
;; observable in it.
;;
;; Under a monotone fold a contribution can only add, so the assertion that
;; survives is that the expectation is subsumed by the folded operation.  The
;; other transformations can lower a position, so subsumption would be unsound
;; there and the exact match is kept.  No corpus machine selects one today,
;; which is why it is written down rather than left implied.
(defparameter *monotone-folds* '("or" "max" "join" "strong-disjunction"))

(defun monotone-fold-p (name)
  (and (member name *monotone-folds* :test #'string=) t))

(defun dense-oracle-input (offset length values)
  "A vector sized to cover the region, not to a fixed dimension.

Clamping to a default dimension writes nothing for a machine mapped above it,
and the corpus has many: AgricultureAgx001010Interconnect reads [13031:13061].
The engine then sees an all-zero input, matches whichever sequence declares
all-zero elements, and reports that sequence's output — which looks exactly
like an engine defect and is not one.  Mirrors dense_input in the C++ harness."
  (let ((buf (make-list (+ offset length) :initial-element 0)))
    (loop for v in values
          for i from 0 below length
          do (setf (nth (+ offset i) buf) v))
    buf))

(defun oracle-subsumed-p (expected folded)
  "Every expected position present in FOLDED at at least its magnitude."
  (and (>= (length folded) (length expected))
       (loop for e in expected
             for f in folded
             always (>= (+ f 1d-9) e))))

(defun merge-op-values (op)
  (mapcar (lambda (x) (coerce x 'double-float))
          (reality-engine-lsp::jarray-list (reality-engine-lsp::jget op "values"))))

(defun merge-op-sequence-ids (op)
  (mapcar (lambda (x) (format nil "~a" x))
          (reality-engine-lsp::jarray-list
           (or (reality-engine-lsp::jget op "sequenceIds")
               (reality-engine-lsp::vectorize nil)))))

(defun run-one-oracle (oracle raw-cache machines)
  "Run ORACLE.  Returns NIL on pass or a failure description string."
  (let* ((jget #'reality-engine-lsp::jget)
         (id        (funcall jget oracle "id"))
         (file      (funcall jget oracle "machineFile"))
         (seq-id    (format nil "~a" (funcall jget oracle "sequenceId")))
         (in-region (funcall jget oracle "inputRegion"))
         (expected  (funcall jget oracle "expected"))
         (exp-region (funcall jget expected "region"))
         (in-offset  (round (funcall jget in-region "offset")))
         (in-length  (round (funcall jget in-region "length")))
         (exp-offset (round (funcall jget exp-region "offset")))
         (exp-length (round (funcall jget exp-region "length")))
         (exp-values (mapcar (lambda (x) (coerce x 'double-float))
                             (reality-engine-lsp::jarray-list
                              (funcall jget expected "values"))))
         ;; Absent means "or" — the corpus default, and what an oracle file
         ;; generated before the field existed must still mean.
         (fold (string-downcase
                (or (funcall jget oracle "outputMergeTransformation") "or")))
         (path (gethash file machines)))
    (unless path
      (return-from run-one-oracle (format nil "~a — machine not in corpus: ~a" id file)))
    (handler-case
        (let* ((raw (or (gethash file raw-cache)
                        (setf (gethash file raw-cache)
                              (reality-engine-lsp::parse-json (read-text-file path)))))
               (machine (reality-engine-lsp::machine-from-json raw))
               (dimension (max (+ in-offset in-length) (+ exp-offset exp-length)))
               (state (make-test-state dimension))
               (last-step nil))
          (reality-engine-lsp::put-machine state machine)
          (dolist (row (reality-engine-lsp::jarray-list (funcall jget oracle "inputs")))
            (setf last-step
                  (reality-engine-lsp::process-perceptual-input
                   state
                   (dense-oracle-input in-offset in-length
                                       (reality-engine-lsp::jarray-list row))
                   :include-machine-results t)))
          (let* ((batch (if last-step
                            (reality-engine-lsp::jarray-list
                             (funcall jget last-step "mergeBatch"))
                            nil))
                 (monotone (monotone-fold-p fold))
                 (hit (some (lambda (op)
                              (let ((region (funcall jget op "region")))
                                (and region
                                     (= (round (funcall jget region "offset")) exp-offset)
                                     (= (round (funcall jget region "length")) exp-length)
                                     (member seq-id (merge-op-sequence-ids op) :test #'string=)
                                     (if monotone
                                         (oracle-subsumed-p exp-values (merge-op-values op))
                                         (equal exp-values (merge-op-values op))))))
                            batch)))
            (if hit
                nil
                (format nil "~a — expected ~{~a ~}not ~a mergeBatch (~{~a ~})"
                        id exp-values
                        (if monotone "subsumed by" "present in")
                        (mapcar #'merge-op-values batch)))))
      (error (e) (format nil "~a — error: ~a" id e)))))

(defun oracle-parity-tests ()
  (let ((oracle-path (oracle-file))
        (machines-dir (oracle-machines-dir)))
    (cond
      ((not (probe-file oracle-path))
       (format t "~&  SKIP oracle parity — no oracle set at ~a~%" oracle-path)
       t)
      ((not (probe-file machines-dir))
       (format t "~&  SKIP oracle parity — no corpus at ~a~%" machines-dir)
       t)
      (t
       (let* ((doc (reality-engine-lsp::parse-json (read-text-file oracle-path)))
              (oracles (reality-engine-lsp::jarray-list
                        (reality-engine-lsp::jget doc "oracles")))
              (machines (corpus-file-map machines-dir))
              (raw-cache (make-hash-table :test #'equal))
              (failures '())
              (passed 0))
         (assert-true (plusp (length oracles)) "oracle set is not empty")
         (dolist (o oracles)
           (let ((failure (run-one-oracle o raw-cache machines)))
             (if failure (push failure failures) (incf passed))))
         (setf failures (nreverse failures))
         (format t "~&  oracle parity: ~a of ~a passed~%" passed (length oracles))
         (when failures
           (format t "~&  ~a failed:~%" (length failures))
           (loop for f in failures
                 for i from 0 below 20
                 do (format t "    ~a~%" f))
           (when (> (length failures) 20)
             (format t "    ... and ~a more~%" (- (length failures) 20))))
         (assert-true (null failures)
                      (format nil "~a of ~a cesgen oracles failed against the LSP engine"
                              (length failures) (length oracles)))
         t)))))
