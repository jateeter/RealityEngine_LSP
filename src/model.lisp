(in-package #:reality-engine-lsp)

(defstruct region offset length)
(defstruct mapping input output bits-per-element)
(defstruct vector-element value comparator threshold)
(defstruct output-vector id vector metadata timestamp provenance)
(defstruct reality-vector
  id elements initial-p active-p match-algorithm metadata next-ids output-vectors just-matched-p predecessor-chain)
(defstruct (ces (:constructor make-ces) (:conc-name sequence-))
  id name metadata schema-version deprecated-at replaced-by vectors)
(defstruct machine id name description metadata mapping match-algorithm arbiter-rule sequences)
(defstruct transition-result input-vector timestamp sequence-results sequence-outputs machine-output arbiter-metadata)

(defun comparator-name (value)
  (string-downcase (or value "gte")))

(defun arbiter-name (value)
  (string-downcase (or value "passthrough")))

(defun make-region-from-json (value)
  (make-region :offset (truncate (or (jnumber value "offset" 0) 0))
               :length (truncate (or (jnumber value "length" 0) 0))))

(defun region-json (region)
  (obj "offset" (region-offset region)
       "length" (region-length region)))

(defun mapping-json (mapping)
  (if mapping
      (let ((out (obj "input" (region-json (mapping-input mapping))
                      "output" (region-json (mapping-output mapping)))))
        (when (mapping-bits-per-element mapping)
          (setf (jget out "bitsPerElement") (mapping-bits-per-element mapping)))
        out)
      +json-null+))

(defun vector-element-json (element)
  (let ((out (obj "value" (vector-element-value element))))
    (when (vector-element-comparator element)
      (setf (jget out "comparatorType") (vector-element-comparator element)))
    (when (vector-element-threshold element)
      (setf (jget out "threshold") (vector-element-threshold element)))
    out))

(defun output-vector-json (output)
  (obj "id" (output-vector-id output)
       "vector" (vectorize (output-vector-vector output))
       "metadata" (or (output-vector-metadata output) (obj))
       "timestamp" (or (output-vector-timestamp output) 0)
       "provenance" (vectorize (or (output-vector-provenance output) nil))))

(defun reality-vector-json (vector)
  (obj "id" (reality-vector-id vector)
       "matchAlgorithm" (reality-vector-match-algorithm vector)
       "elements" (vectorize (mapcar #'vector-element-json (reality-vector-elements vector)))
       "state" (if (reality-vector-active-p vector) "active" "inactive")
       "isActive" (json-bool (reality-vector-active-p vector))
       "nextVectorIds" (vectorize (reality-vector-next-ids vector))
       "outputVectors" (vectorize (mapcar #'output-vector-json (reality-vector-output-vectors vector)))
       "isInitial" (json-bool (reality-vector-initial-p vector))
       "wasJustMatched" (json-bool (reality-vector-just-matched-p vector))
       "metadata" (or (reality-vector-metadata vector) (obj))))

(defun sequence-json (sequence &key full)
  (let* ((vectors (object-values-sorted (sequence-vectors sequence)))
         (initials (remove-if-not #'reality-vector-initial-p vectors))
         (outputs (remove-if-not (lambda (v) (reality-vector-output-vectors v)) vectors))
         (out (obj "id" (sequence-id sequence)
                   "name" (sequence-name sequence)
                   "vectors" (if full
                                 (vectorize (mapcar #'reality-vector-json vectors))
                                 (vectorize (mapcar #'reality-vector-json vectors)))
                   "initialVectorIds" (vectorize (mapcar #'reality-vector-id initials))
                   "outputVectorIds" (vectorize (mapcar #'reality-vector-id outputs))
                   "metadata" (or (sequence-metadata sequence) (obj)))))
    (when (sequence-schema-version sequence)
      (setf (jget out "schemaVersion") (sequence-schema-version sequence)))
    (when (sequence-deprecated-at sequence)
      (setf (jget out "deprecatedAt") (sequence-deprecated-at sequence)))
    (when (sequence-replaced-by sequence)
      (setf (jget out "replacedBy") (sequence-replaced-by sequence)))
    out))

;; ── Canonical ordering ─────────────────────────────────────────────────────
;;
;; Machines and sequences live in hash tables keyed by id, and ids are
;; generated per runtime — so iteration order differed between C++, LSP and
;; Scala and the same corpus serialized to different bytes.  Ordering by
;; content rather than by identity is what makes the comparison meaningful.
;;
;; Machines sort by (metadata.domain, name, id); sequences by (name, id).  The
;; trailing id keeps the order total, and metadata.domain is absent on a
;; handful of corpus machines, which sort first under an empty key.

(defun machine-domain (machine)
  "metadata.domain, or \"\" when absent."
  (let ((meta (machine-metadata machine)))
    (or (and meta (jstring meta "domain" nil)) "")))

(defun string-triple< (a1 a2 a3 b1 b2 b3)
  "Lexicographic compare of two 3-tuples of strings."
  (cond ((string< a1 b1) t)
        ((string> a1 b1) nil)
        ((string< a2 b2) t)
        ((string> a2 b2) nil)
        (t (and (string< a3 b3) t))))

(defun machines-in-canonical-order (machines)
  "Machines from a hash table, ordered by (metadata.domain, name, id)."
  (sort (object-values machines)
        (lambda (a b)
          (string-triple< (machine-domain a) (or (machine-name a) "") (or (machine-id a) "")
                          (machine-domain b) (or (machine-name b) "") (or (machine-id b) "")))))

(defun machine-sequence-list (machine)
  "Sequences ordered by (name, id).

Previously object-values-sorted, which orders by hash key — the sequence id —
producing \"MEMORY ALERT SET\" before \"RESET\" on one runtime and after it on
another."
  (sort (object-values (machine-sequences machine))
        (lambda (a b)
          (let ((na (or (sequence-name a) "")) (nb (or (sequence-name b) "")))
            (cond ((string< na nb) t)
                  ((string> na nb) nil)
                  (t (and (string< (or (sequence-id a) "") (or (sequence-id b) "")) t)))))))

(defun machine-summary-json (machine)
  "Minimal projection used by the PE catalog refresher — id, name, metadata only.
Omits sequences, vectors, and perceptualMapping to keep the response small."
  (obj "id"       (machine-id machine)
       "name"     (machine-name machine)
       "metadata" (or (machine-metadata machine) (obj))))

(defun machine-json (machine &key full)
  (let* ((sequences (machine-sequence-list machine))
         (sequence-ids (mapcar #'sequence-id sequences))
         (sequence-jsons (if full
                             (mapcar (lambda (s) (sequence-json s :full t)) sequences)
                             (mapcar (lambda (s) (obj "id" (sequence-id s) "name" (sequence-name s))) sequences)))
         (total-vectors (loop for s in sequences sum (hash-table-count (sequence-vectors s)))))
    (obj "id" (machine-id machine)
         "name" (machine-name machine)
         "description" (or (machine-description machine) "")
         "matchAlgorithm" (machine-match-algorithm machine)
         "arbiterRule" (machine-arbiter-rule machine)
         "sequenceCount" (length sequences)
         "totalVectors" total-vectors
         "sequenceIds" (vectorize sequence-ids)
         "sequences" (vectorize sequence-jsons)
         "metadata" (or (machine-metadata machine) (obj))
         "perceptualMapping" (mapping-json (machine-mapping machine)))))

(defun vector-provenance-chain (vector)
  (append (or (reality-vector-predecessor-chain vector) nil)
          (list (reality-vector-id vector))))

(defun reset-reality-vector (vector)
  (setf (reality-vector-active-p vector) (reality-vector-initial-p vector)
        (reality-vector-just-matched-p vector) nil
        (reality-vector-predecessor-chain vector) nil)
  vector)

(defun match-element (element input-value override &optional vector-match-algorithm)
  "Compare one element.  Comparator precedence is

    explicit override > element comparatorType > vector matchAlgorithm > gte

which is C++'s

    overrideType.value_or(elem.comparatorType.value_or(matchAlgorithm))

with the vector's matchAlgorithm inherited from its machine.  The per-element
comparatorType still wins, so a machine that sets one comparator at machine
level and a different one on an individual element keeps both.

VECTOR-MATCH-ALGORITHM was previously not consulted at all -- the fallback was
the literal \"gte\" -- so a machine declaring \"matchAlgorithm\": \"equals\"
was evaluated with the weaker predicate no matter what the loader recorded
(RealityEngine_LSP#31)."
  (let* ((type (comparator-name (or override
                                    (vector-element-comparator element)
                                    vector-match-algorithm
                                    "gte")))
         (expected (coerce (vector-element-value element) 'double-float))
         (actual (coerce input-value 'double-float))
         (threshold (or (vector-element-threshold element) 0.5d0)))
    (cond
      ((member type '("equals" "custom") :test #'string=)
       (values (= expected actual) (if (= expected actual) 1.0d0 0.0d0)))
      ((string= type "threshold")
       (let* ((limit (or (vector-element-threshold element) 0.1d0))
              (diff (abs (- expected actual)))
              (ok (<= diff limit)))
         (values ok (if ok (if (zerop limit) 1.0d0 (- 1.0d0 (/ diff limit))) 0.0d0))))
      ((string= type "pattern")
       (let* ((score (- 1.0d0 (abs (- expected actual))))
              (ok (>= score threshold)))
         (values ok score)))
      (t
       (let* ((input-high (>= actual threshold))
              (value-high (>= expected threshold))
              (ok (eq input-high value-high))
              (score (if ok
                         (if input-high
                             (if (< threshold 1.0d0) (/ (- actual threshold) (- 1.0d0 threshold)) 1.0d0)
                             (if (> threshold 0.0d0) (/ (- threshold actual) threshold) 1.0d0))
                         0.0d0)))
         (values ok (clamp01 score)))))))

(defun match-reality-vector (vector input &key override)
  (let ((elements (reality-vector-elements vector)))
    (if (/= (length elements) (length input))
        (values nil 0.0d0 (obj "error" "Vector dimension mismatch"))
        (loop with total = 0.0d0
              for element in elements
              for actual in input
              for index from 0
              do (multiple-value-bind (ok score)
                     (match-element element actual override
                                    (reality-vector-match-algorithm vector))
                   (unless ok
                     (return (values nil (/ total (max 1 (length elements)))
                                     (obj "failedAtIndex" index))))
                   (incf total score))
              finally (return (values t (/ total (max 1 (length elements))) (obj)))))))

(defun transition-vector (vector input &key override)
  (multiple-value-bind (matched score metadata) (match-reality-vector vector input :override override)
    (unless matched
      (unless (reality-vector-initial-p vector)
        (setf (reality-vector-active-p vector) nil
              (reality-vector-predecessor-chain vector) nil))
      (return-from transition-vector
        (values nil nil nil score metadata nil)))
    (let* ((chain (vector-provenance-chain vector))
           (outputs (mapcar (lambda (out)
                              (make-output-vector
                               :id (output-vector-id out)
                               :vector (copy-list (output-vector-vector out))
                               :metadata (or (output-vector-metadata out) (obj))
                               :timestamp (now-ms)
                               :provenance chain))
                            (reality-vector-output-vectors vector)))
           (final-p outputs)
           (transitional-p (and (not (reality-vector-initial-p vector)) (not final-p))))
      (when (and transitional-p (reality-vector-next-ids vector))
        (setf (reality-vector-active-p vector) nil
              (reality-vector-predecessor-chain vector) nil))
      (values t (reality-vector-next-ids vector) outputs score metadata chain))))

(defun transition-sequence (sequence input &key override)
  (let ((matched nil)
        (activated nil)
        (outputs nil)
        (pending (make-hash-table :test #'equal)))
    (maphash (lambda (_ vector)
               (declare (ignore _))
               (setf (reality-vector-just-matched-p vector) nil))
             (sequence-vectors sequence))
    (dolist (vector (remove-if-not #'reality-vector-active-p (object-values-sorted (sequence-vectors sequence))))
      (multiple-value-bind (ok next-ids emitted _score _metadata chain)
          (transition-vector vector input :override override)
        (declare (ignore _score _metadata))
        (when ok
          (push (reality-vector-id vector) matched)
          (when (reality-vector-output-vectors vector)
            (setf (reality-vector-just-matched-p vector) t))
          (dolist (next-id next-ids)
            (unless (gethash next-id pending)
              (setf (gethash next-id pending) chain)))
          (setf outputs (append outputs emitted)))))
    (maphash (lambda (id chain)
               (let ((next (gethash id (sequence-vectors sequence))))
                 (when (and next (not (reality-vector-active-p next)))
                   (setf (reality-vector-active-p next) t
                         (reality-vector-predecessor-chain next) chain)
                   (push id activated))))
             pending)
    (obj "matchedVectors" (vectorize (nreverse matched))
         "activatedVectors" (vectorize (nreverse activated))
         "assertedOutputs" (vectorize (mapcar #'output-vector-json outputs))
         "%outputs" outputs)))

(defun reset-sequence (sequence)
  (maphash (lambda (_ vector)
             (declare (ignore _))
             (reset-reality-vector vector))
           (sequence-vectors sequence))
  sequence)

(defun process-machine-input (machine input &key override)
  (let ((sequence-results (make-hash-table :test #'equal))
        (sequence-outputs (make-hash-table :test #'equal))
        (all-outputs nil)
        (sequences-with-output 0))
    (dolist (sequence (machine-sequence-list machine))
      (let* ((result (transition-sequence sequence input :override override))
             (outputs (jget result "%outputs")))
        (remhash "%outputs" result)
        (when outputs (incf sequences-with-output))
        (setf all-outputs (append all-outputs outputs)
              (gethash (sequence-id sequence) sequence-results) result
              (gethash (sequence-id sequence) sequence-outputs) outputs)))
    (let* ((total (hash-table-count (machine-sequences machine)))
           (rule (arbiter-name (machine-arbiter-rule machine)))
           (should-output (cond
                            ((string= rule "and") (and (> total 0) (= sequences-with-output total)))
                            ((string= rule "or") (> sequences-with-output 0))
                            (t all-outputs)))
           (machine-output (when (and should-output all-outputs)
                             (let ((first (first all-outputs)))
                               (make-output-vector
                                :id (make-id "machine-output")
                                :vector (output-vector-vector first)
                                :metadata (obj "arbiter" t "combinedFrom" (length all-outputs))
                                :timestamp (now-ms)
                                :provenance (output-vector-provenance first))))))
      (make-transition-result
       :input-vector input
       :timestamp (now-ms)
       :sequence-results sequence-results
       :sequence-outputs sequence-outputs
       :machine-output machine-output
       :arbiter-metadata (obj "rule" rule
                              "totalInputs" total
                              "sequencesWithOutput" sequences-with-output
                              "shouldOutput" (json-bool should-output))))))

(defun values-equal-p (left right)
  (and (= (length left) (length right))
       (loop for a in left
             for b in right
             always (= (coerce a 'double-float) (coerce b 'double-float)))))

(defun resolve-governance (machine sequence-id values)
  (let* ((metadata (machine-metadata machine))
         (trigger (jget metadata "triggerConfig"))
         (rules (and (jobject-p trigger) (jget trigger "rules"))))
    (unless (jarray-p rules)
      (return-from resolve-governance nil))
    (let ((match nil))
      (dolist (rule (jarray-list rules))
        (when (and (jobject-p rule)
                   (string= (jstring rule "sequenceId" "") sequence-id)
                   (values-equal-p values (numbers-from-json (jget rule "outputMatches"))))
          (setf match rule)
          (return)))
      (unless match
        (return-from resolve-governance nil))
      (let* ((machine-gov (jget metadata "governance"))
             (has-machine-gov (jobject-p machine-gov))
             (rule-gov (jget match "governance"))
             (has-rule-gov (jobject-p rule-gov))
             (process-status (jstring match "processStatus" nil))
             (sla-from-rule (and has-rule-gov (jnumber rule-gov "slaSeconds" nil)))
             (sla-from-machine (and has-machine-gov
                                    process-status
                                    (jobject-p (jget machine-gov "sla"))
                                    (jnumber (jget machine-gov "sla") process-status nil)))
             (rule-contact (and has-rule-gov (jget rule-gov "contact")))
             (machine-contact (and has-machine-gov (jget machine-gov "contact")))
             (contact (obj)))
        (let ((primary (or (and (jobject-p rule-contact) (jstring rule-contact "primary" nil))
                           (and (jobject-p machine-contact) (jstring machine-contact "primary" nil))))
              (secondary (or (and (jobject-p rule-contact) (jstring rule-contact "secondary" nil))
                             (and (jobject-p machine-contact) (jstring machine-contact "secondary" nil)))))
          (when primary (setf (jget contact "primary") primary))
          (when secondary (setf (jget contact "secondary") secondary)))
        (let ((out (obj
                    "machineId" (machine-id machine)
                    "machineName" (machine-name machine)
                    "sequenceId" sequence-id
                    "ragStatusCode" (or (jstring match "ragStatusCode" nil) +json-null+)
                    "processStatus" (or process-status +json-null+)
                    "ownerTeam" (or (and has-rule-gov (jstring rule-gov "ownerTeam" nil))
                                     (and has-machine-gov (jstring machine-gov "ownerTeam" nil))
                                     "unrouted")
                    "slaSeconds" (or sla-from-rule sla-from-machine +json-null+)
                    "runbook" (or (and has-rule-gov (jstring rule-gov "runbook" nil))
                                  (and has-machine-gov (jstring machine-gov "runbook" nil))
                                  +json-null+)
                    "escalationPolicy" (or (and has-rule-gov (jstring rule-gov "escalationPolicy" nil))
                                           (and has-machine-gov (jstring machine-gov "escalationPolicy" nil))
                                           +json-null+)
                    "contact" contact
                    "source" (cond
                                (has-rule-gov "rule-with-override")
                                (has-machine-gov "rule-only")
                                (t "machine-fallback"))
                    "hasMachineGovernance" (json-bool has-machine-gov))))
          (when (jstring match "description" nil)
            (setf (jget out "description") (jstring match "description" nil)))
          out)))))

(defun days-since-date (value)
  (handler-case
      (when (and (stringp value) (>= (length value) 10))
        (let* ((year (parse-integer value :start 0 :end 4))
               (month (parse-integer value :start 5 :end 7))
               (day (parse-integer value :start 8 :end 10))
               (then (encode-universal-time 0 0 0 day month year 0))
               (now (get-universal-time)))
          (max 0 (floor (- now then) 86400))))
    (error () 0)))

(defun sequence-deprecation-json (sequence)
  (when (sequence-deprecated-at sequence)
    (let ((out (obj "since" (sequence-deprecated-at sequence)
                    "ageDays" (or (days-since-date (sequence-deprecated-at sequence)) 0))))
      (when (sequence-replaced-by sequence)
        (setf (jget out "replacedBy") (sequence-replaced-by sequence)))
      out)))

(defun sorted-merge-operations (operations)
  (sort operations
        (lambda (left right)
          (let ((lm (jstring left "machineId" ""))
                (rm (jstring right "machineId" ""))
                (ls (jstring left "sequenceId" ""))
                (rs (jstring right "sequenceId" ""))
                (li (or (jnumber left "outputIndex" 0) 0))
                (ri (or (jnumber right "outputIndex" 0) 0)))
            (cond
              ((not (string= lm rm)) (string< lm rm))
              ((not (string= ls rs)) (string< ls rs))
              (t (< li ri)))))))

(defun transition-result-json (result)
  (obj "inputVector" (vectorize (transition-result-input-vector result))
       "timestamp" (transition-result-timestamp result)
       "sequenceResults" (transition-result-sequence-results result)
       "machineOutput" (if (transition-result-machine-output result)
                           (output-vector-json (transition-result-machine-output result))
                           +json-null+)
       "arbiterMetadata" (transition-result-arbiter-metadata result)))

(defun extract-region (space region)
  (let ((offset (region-offset region))
        (length (region-length region)))
    (loop for i from offset below (+ offset length)
          collect (if (< i (length space)) (nth i space) 0.0d0))))

(defun merge-region (space region values)
  (let* ((offset (region-offset region))
         (needed (+ offset (length values)))
         (out (copy-list space)))
    (loop while (< (length out) needed) do (setf out (append out (list 0.0d0))))
    (loop for value in values
          for i from offset
          do (setf (nth i out) value))
    out))
