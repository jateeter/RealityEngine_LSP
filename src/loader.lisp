(in-package #:reality-engine-lsp)

(defparameter +sta-default-threshold+ 0.5d0)

(defun canonical-machine-id-fragment (name)
  (with-output-to-string (out)
    (loop for ch across (string-downcase name)
          do (write-char (if (alphanumericp ch) ch #\-) out))))

(defun sta-element-state (element)
  (let ((threshold (or (jnumber element "threshold" nil) +sta-default-threshold+))
        (value (or (jnumber element "value" 0.0d0) 0.0d0)))
    (if (>= value threshold) 1 0)))

(defun sta-vector-state (vector-json)
  (mapcar #'sta-element-state
          (jarray-list (or (jget vector-json "elements") (arr)))))

(defun sta-hamming-distance (left right)
  (when (= (length left) (length right))
    (loop for a in left
          for b in right
          count (/= a b))))

(defun sta-life-safety-p (machine-root)
  (string= (jstring (or (jget machine-root "metadata") (obj)) "severity" "")
           "life-safety"))

(defun sta-vector-index (machine-root)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (sequence (jarray-list (or (jget machine-root "sequences") (arr))))
      (dolist (vector (jarray-list (or (jget sequence "vectors") (arr))))
        (when (jstring vector "id" nil)
          (setf (gethash (jstring vector "id" nil) index)
                (obj "sequenceId" (jstring sequence "id" "")
                     "vector" vector)))))
    index))

(defun compute-sta-report (json)
  (let* ((machine-root (if (jobject-p (jget json "machine")) (jget json "machine") json))
         (vector-index (sta-vector-index machine-root))
         (sequences nil)
         (intra-violations 0)
         (inter-jumps 0))
    (dolist (sequence (jarray-list (or (jget machine-root "sequences") (arr))))
      (let ((local (make-hash-table :test #'equal))
            (transitions nil)
            (max-intra 0)
            (any-violation nil))
        (dolist (vector (jarray-list (or (jget sequence "vectors") (arr))))
          (when (jstring vector "id" nil)
            (setf (gethash (jstring vector "id" nil) local) vector)))
        (dolist (vector (jarray-list (or (jget sequence "vectors") (arr))))
          (let ((from-id (jstring vector "id" ""))
                (from-state (sta-vector-state vector)))
            (dolist (next-id (mapcar #'princ-to-string
                                     (jarray-list (or (jget vector "nextVectorIds") (arr)))))
              (let ((local-next (gethash next-id local)))
                (cond
                  (local-next
                   (let* ((to-state (sta-vector-state local-next))
                          (hd (sta-hamming-distance from-state to-state))
                          (violation (or (null hd) (> hd 1))))
                     (when hd (setf max-intra (max max-intra hd)))
                     (when violation
                       (setf any-violation t)
                       (incf intra-violations))
                     (push (obj "from" from-id
                                "to" next-id
                                "kind" "intra"
                                "hd" (or hd +json-null+)
                                "fromState" (vectorize from-state)
                                "toState" (vectorize to-state)
                                "violation" (json-bool violation)
                                "error" (if hd +json-null+ "element-count-mismatch"))
                           transitions)))
                  ((gethash next-id vector-index)
                   (let* ((found (gethash next-id vector-index))
                          (target (jget found "vector"))
                          (hd (sta-hamming-distance from-state (sta-vector-state target))))
                     (incf inter-jumps)
                     (push (obj "from" from-id
                                "to" next-id
                                "kind" "inter-sequence"
                                "hd" (or hd +json-null+)
                                "targetSequenceId" (jstring found "sequenceId" ""))
                           transitions)))
                  (t
                   (setf any-violation t)
                   (incf intra-violations)
                   (push (obj "from" from-id
                              "to" next-id
                              "kind" "dangling"
                              "hd" +json-null+
                              "violation" t
                              "error" "next vector id not found in machine")
                         transitions)))))))
        (push (obj "id" (jstring sequence "id" "")
                   "name" (or (jstring sequence "name" nil) +json-null+)
                   "transitions" (vectorize (nreverse transitions))
                   "maxIntraHD" max-intra
                   "anyViolation" (json-bool any-violation))
              sequences)))
    (obj "machineId" (or (jstring machine-root "id" nil) +json-null+)
         "machineName" (or (jstring machine-root "name" nil) +json-null+)
         "lifeSafety" (json-bool (sta-life-safety-p machine-root))
         "declared" (or (jget (or (jget machine-root "metadata") (obj)) "singleTransitionAssumption") +json-null+)
         "sequences" (vectorize (nreverse sequences))
         "summary" (obj "intraViolations" intra-violations
                        "interJumps" inter-jumps
                        "drift" +json-null+))))

(defun sta-offender-lines (report)
  (let (lines)
    (dolist (sequence (jarray-list (jget report "sequences")))
      (dolist (transition (jarray-list (jget sequence "transitions")))
        (when (or (jbool transition "violation" nil)
                  (not (eq (jget transition "error" +json-null+) +json-null+)))
          (push (format nil "~a: ~a -> ~a (HD=~a~a)"
                        (jstring sequence "id" "")
                        (jstring transition "from" "")
                        (jstring transition "to" "")
                        (if (eq (jget transition "hd") +json-null+)
                            "null"
                            (princ-to-string (jget transition "hd")))
                        (if (eq (jget transition "error" +json-null+) +json-null+)
                            ""
                            (format nil ", ~a" (jget transition "error"))))
                lines))))
    (nreverse lines)))

(defun assert-sta-for-life-safety (json)
  (let* ((report (compute-sta-report json))
         (summary (jget report "summary"))
         (violations (truncate (or (jnumber summary "intraViolations" 0) 0))))
    (when (and (jbool report "lifeSafety" nil) (> violations 0))
      (error "STA violation in life-safety machine \"~a\": ~a intra-sequence transition(s) with HD>1.~%~{  - ~a~%~}"
             (or (jstring report "machineName" nil)
                 (jstring report "machineId" nil)
                 "?")
             violations
             (sta-offender-lines report)))
    report))

(defun parse-vector-element (item)
  (cond
    ((numberp item) (make-vector-element :value item :comparator nil :threshold nil))
    ((jobject-p item)
     (make-vector-element :value (or (jnumber item "value" 0.0d0) 0.0d0)
                          :comparator (jstring item "comparatorType" nil)
                          :threshold (jnumber item "threshold" nil)))
    (t (make-vector-element :value 0.0d0))))

(defun parse-output-vector (item)
  (make-output-vector
   :id (or (jstring item "id" nil) (make-id "output"))
   :vector (numbers-from-json (or (jget item "vector") (jget item "values") (arr)))
   :metadata (or (jget item "metadata") (obj))
   :timestamp (or (jnumber item "timestamp" nil) 0)
   :provenance (jarray-list (or (jget item "provenance") (arr)))))

(defun parse-reality-vector (item &optional machine-match-algorithm)
  "Parse one reality vector.

MACHINE-MATCH-ALGORITHM, when supplied, wins over anything on the vector
itself.  That is what C++ does -- `rv.matchAlgorithm = machine.matchAlgorithm`
is an unconditional assignment after the vector is built -- and C++ is the
canonical definition.  Reading a per-vector value here instead meant every
vector fell back to the \"gte\" default, so the two corpus machines that
declare `\"matchAlgorithm\": \"equals\"` at machine level had all their vectors
matching with the weaker predicate: LSP could advance a sequence where C++ and
Scala would not, on identical input (RealityEngine_LSP#31).

The optional argument keeps the API call sites working, which build a vector
from a request body with no machine in scope."
  (let* ((id (or (jstring item "id" nil) (make-id "vector")))
         (initial-p (jbool item "isInitial" nil))
         (elements (mapcar #'parse-vector-element (jarray-list (or (jget item "elements") (arr)))))
         (next-ids (mapcar #'princ-to-string (jarray-list (or (jget item "nextVectorIds") (arr)))))
         (outputs (mapcar #'parse-output-vector (jarray-list (or (jget item "outputVectors") (arr))))))
    (make-reality-vector :id id
                         :elements elements
                         :initial-p initial-p
                         ;; Honour a serialised isActive when the document
                         ;; carries one, and fall back to isInitial when it does
                         ;; not.
                         ;;
                         ;; Corpus machine JSON declares no isActive, so loading
                         ;; from the corpus is unchanged and still satisfies the
                         ;; rule that every initial RE is active. A checkpoint,
                         ;; though, round-trips the machine through
                         ;; machine-json/machine-from-json to snapshot it, and
                         ;; deriving activation from isInitial there discarded
                         ;; exactly what the checkpoint exists to preserve:
                         ;; restore returned the machine to its *initial*
                         ;; activation rather than to the step it was captured
                         ;; at, silently dropping every RE the run had armed
                         ;; (#57).
                         :active-p (jbool item "isActive" initial-p)
                         :match-algorithm (comparator-name
                                           (or machine-match-algorithm
                                               (jstring item "matchAlgorithm" "gte")))
                         :metadata (or (jget item "metadata") (obj))
                         :next-ids next-ids
                         :output-vectors outputs
                         :just-matched-p nil
                         :predecessor-chain nil)))

(defun parse-sequence (item &optional machine-match-algorithm)
  (let ((vectors (make-hash-table :test #'equal))
        (sequence (make-ces :id (or (jstring item "id" nil) (make-id "sequence"))
                            :name (or (jstring item "name" nil) "unnamed")
                            :metadata (or (jget item "metadata") (obj))
                            :schema-version (jstring item "schemaVersion" nil)
                            :deprecated-at (jstring item "deprecatedAt" nil)
                            :replaced-by (jstring item "replacedBy" nil)
                            :vectors nil)))
    (dolist (vector-json (jarray-list (or (jget item "vectors") (arr))))
      (let ((vector (parse-reality-vector vector-json machine-match-algorithm)))
        (setf (gethash (reality-vector-id vector) vectors) vector)))
    (setf (sequence-vectors sequence) vectors)
    sequence))

(defun parse-mapping (item)
  (when (and (jobject-p item) (jobject-p (jget item "input")) (jobject-p (jget item "output")))
    (make-mapping :input (make-region-from-json (jget item "input"))
                  :output (make-region-from-json (jget item "output"))
                  :bits-per-element (let ((bits (jnumber item "bitsPerElement" nil)))
                                      (when (and bits (member (truncate bits) '(1 2 4 8)))
                                        (truncate bits)))
                  ;; k, the machine's declared alphabet top. The schema requires
                  ;; it of exactly the machines that select one of the two
                  ;; Lukasiewicz strong operations and allows it on any other;
                  ;; a value below 1 is not a chain, so it is read as undeclared
                  ;; and the fold refuses rather than folding at a nonsense k.
                  :output-alphabet-top (let ((k (jnumber item "outputAlphabetTop" nil)))
                                         (when (and k (>= (truncate k) 1))
                                           (truncate k))))))

;; STA strictness defaults to NIL — matches AI's MachineLoader.loadFromJSON
;; (`options.strictSta` opt-in) and C++'s `LoadOptions{strictSta=false}` so
;; the same machine corpus loads identically across all three runtimes.
;; Callers that need the hot-path STA gate (patient-safety hosts, replay
;; harnesses) pass `:strict-sta t` explicitly.
(defun machine-from-json (json &optional forced-id &key (strict-sta nil))
  (when strict-sta
    (assert-sta-for-life-safety json))
  (let* ((root (if (jobject-p (jget json "machine")) (jget json "machine") json))
         (metadata (or (jget root "metadata") (obj)))
         (sequences (make-hash-table :test #'equal))
         (machine (make-machine
                   :id (or forced-id (jstring root "id" nil) (make-id "machine"))
                   :name (or (jstring root "name" nil) "unnamed")
                   :description (or (jstring root "description" nil) "")
                   :metadata metadata
                   :mapping (parse-mapping (jget root "perceptualMapping"))
                   :match-algorithm (comparator-name (jstring root "matchAlgorithm" "gte"))
                   :arbiter-rule (arbiter-name (jstring root "arbiterRule" "passthrough"))
                   ;; Read at intern time so the machine carries it from the
                   ;; moment it is loaded. Absent means "or", which is what
                   ;; every runtime already does.
                   :output-merge-transformation
                   (output-merge-name (jstring root "outputMergeTransformation" "or"))
                   :sequences nil)))
    (when (jarray-present-p root "inputSequences")
      (setf (jget metadata "inputSequences") (jget root "inputSequences")))
    (dolist (sequence-json (jarray-list (or (jget root "sequences") (arr))))
      (let ((sequence (parse-sequence sequence-json (machine-match-algorithm machine))))
        (setf (gethash (sequence-id sequence) sequences) sequence)))
    (setf (machine-sequences machine) sequences)
    machine))

(defun load-machine-from-file (path)
  (let* ((json (parse-json (safe-read-file path)))
         (ver  (jstring json "version" nil)))
    (unless ver
      (error "~a: missing required field: version" (file-namestring path)))
    (let* ((dot   (position #\. ver))
           (major (if dot (ignore-errors (parse-integer (subseq ver 0 dot))) 0)))
      (unless (eql major 1)
        (error "~a: incompatible machine JSON version: ~a (current: 1.0.0)"
               (file-namestring path) ver)))
    (machine-from-json json (format nil "machine-~a" (canonical-machine-id-fragment (pathname-name path))))))

(defun collect-json-files-recursive (dir)
  "Return all .json files under DIR sorted lexicographically."
  (let ((result '()))
    (labels ((walk (d)
               (dolist (f (uiop:directory-files d "*.json"))
                 (push f result))
               (dolist (sub (uiop:subdirectories d))
                 (walk sub))))
      (walk (uiop:ensure-directory-pathname dir)))
    (sort result #'string< :key #'namestring)))

(defun load-machines-from-directory (directory)
  (let ((dir (uiop:ensure-directory-pathname directory))
        (machines nil))
    (when (uiop:directory-exists-p dir)
      (dolist (path (collect-json-files-recursive dir))
        (handler-case
            (push (load-machine-from-file path) machines)
          (error (condition)
            (format *error-output* "~&Skipping machine ~a: ~a~%" path condition)))))
    (nreverse machines)))
