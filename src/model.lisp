(in-package #:reality-engine-lsp)

(defstruct region offset length)
;; OUTPUT-ALPHABET-TOP is k, the top of the ordered chain {0..k} this machine's
;; cells range over. Declared on `perceptualMapping` by the corpus schema and
;; deliberately distinct from BITS-PER-ELEMENT, which gives the REPRESENTABLE
;; range rather than the alphabet: FallDetection declares 4 bits (0..15) and
;; ranges over {0..4}, so deriving k from the width would fold its ladder at
;; k=15 and yield 14 — a value outside the machine's own alphabet
;; (RealityEngine_CI#158). The two live side by side because that is the point
;; at which they are confused.
(defstruct mapping input output bits-per-element output-alphabet-top)
(defstruct vector-element value comparator threshold)
(defstruct output-vector id vector metadata timestamp provenance)
(defstruct reality-vector
  id elements initial-p active-p match-algorithm metadata next-ids output-vectors just-matched-p predecessor-chain)
(defstruct (ces (:constructor make-ces) (:conc-name sequence-))
  id name metadata schema-version deprecated-at replaced-by vectors)
(defstruct machine id name description metadata mapping match-algorithm arbiter-rule
                   output-merge-transformation
                   ;; Interlock on the transformation above. Initialised LOCKED:
                   ;; the transformation is a training variable, and a run that
                   ;; retunes one by accident is a run whose results mean nothing
                   ;; and which nothing distinguishes from a valid one. Changing
                   ;; it requires unlocking first, deliberately and separately.
                   ;;
                   ;; Runtime state, not a corpus property — a machine's declared
                   ;; transformation travels with it, but whether this deployment
                   ;; is currently allowed to change it does not. Every restart
                   ;; comes up locked.
                   (output-merge-locked t)
                   sequences)

(defparameter +boolean-output-merge-transformations+
  '("or" "and" "xor" "nor" "nand")
  "The n-input Boolean gates. Defined over {0,1}; each is a function of K and N.")

(defparameter +mv-output-merge-transformations+
  '("meet" "join" "strong-conjunction" "strong-disjunction" "discrete-median")
  "The multi-valued transformations, defined over an ordered chain {0..k}.

Folding a multi-valued machine's cells with a Boolean gate destroys them: the
gate can only answer \"asserted or not\", so an ordinal severity ladder comes out
as a flag (RealityEngine_CI#158). These five are closed over the chain, symmetric
and deterministic, verified by exhaustion in the reference implementation at
RealityEngine_CI scripts/experiment-mv-transforms.py, which this fold is written
against cell for cell.")

(defparameter +output-merge-transformations+
  (append +boolean-output-merge-transformations+ +mv-output-merge-transformations+)
  "Every transformation the fold accepts. The runtime endpoints advertise and
validate against this list, so a name added here is accepted there.")

(defun output-merge-name (value)
  "Normalise a declared outputMergeTransformation. NIL/absent means \"or\".

The transformation names the n-input gate the Reality Engine folds a machine's
collection of potential outputs with, at the completion boundary of the atomic
matching action. Declared per machine and read when the machine is interned.
See RealityEngine_Machines semantics/ontology/re-core.ttl."
  (string-downcase (or value "or")))

(defun mv-output-merge-p (name)
  "True when NAME is one of the multi-valued transformations."
  (and (member name +mv-output-merge-transformations+ :test #'string=) t))

(defparameter +chain-top-required-transformations+
  '("strong-conjunction" "strong-disjunction")
  "The transformations that are undefined without a chain top.

Both Lukasiewicz strong operations are parameterised by k and move materially
with it; meet and discrete-median take no k at all, and join reads one only as
an early-exit bound and is total without it.")

(defun mv-fold-refuses-p (name chain-top)
  "True when NAME cannot be folded because no chain top CHAIN-TOP was supplied.

THE ONE PLACE the absent-chain-top decision lives. The decision is to REFUSE:
the fold presents no output, exactly as it does for an empty collection.

k is a property of the machine's alphabet, and nothing in the corpus declares
one. It is NOT derivable from `bitsPerElement` — FallDetection declares 4 bits,
representable 0..15, and uses the alphabet {0..4} — so there is no rule to fall
back on, only a guess, and a guess is unsound in both directions:

  - strong disjunction is min(k, sum x), which clamps. Folding FallDetection's
    ladder [0,1,2,3,4,4,0] at k=1 gives 1 — the exact flattening of an ordinal
    ladder into a flag that this vocabulary exists to prevent, reintroduced
    through the parameter instead of through the gate (RealityEngine_CI#158).

  - strong conjunction is max(0, sum x - k(n-1)), which does NOT clamp. Its
    closure bound holds only while every xi <= k, so the same ladder at k=1
    gives 8 — outside the chain {0,1} and outside the machine's own alphabet
    {0..4}. Smallest witness: k=1 over [2,2] gives 3.

Refusing is the only answer that neither flattens the data nor fabricates a
value no contributor asserted. All three runtimes refuse: C++ was changed from
the Boolean-chain fallback to refusal, and Scala refused from the start. Three
runtimes answering differently here would be the divergence class this work
exists to remove."
  (and (null chain-top)
       (member name +chain-top-required-transformations+ :test #'string=)
       t))

(defun mv-cell-integer (cell)
  "Coerce one stored cell to the non-negative integer an MV chain value is.

Cells are stored as double-floats in this engine but an MV value is
conceptually an integer position on an ordered chain, so the conversion is made
here, once, explicitly — rather than letting float representation decide
behaviour somewhere downstream.

Nearest integer with ties rounded AWAY FROM ZERO, via FLOOR of x + 1/2.

DO NOT \"simplify\" this to ROUND. CL's ROUND is round-half-to-EVEN: it answers 2
for 2.5, while C++ `std::llround` and Scala both answer 3. Any arbiter-scaled
cell landing exactly on .5 would then fold to a different value here than in the
other two runtimes — silently, on data that looks unremarkable. That is precisely
the divergence class this fold exists to remove (RealityEngine_CI#158).

The addition is done in exact rationals rather than in floating point: RATIONAL
gives the double's exact value, so nothing rounds on the way to the rounding
decision itself.

NIL — a cell absent from a contribution shorter than the widest one — is the
chain bottom, 0, matching how the Boolean fold treats a missing cell as not
asserting. Anything at or below the bottom clamps to it; the chain has no
negatives."
  (if (null cell)
      0
      (let ((value (coerce cell 'double-float)))
        (if (plusp value)
            (floor (+ (rational value) 1/2))
            0))))

(defun mv-select-nth (buffer size index)
  "Return the INDEX-th smallest of BUFFER[0..SIZE), partitioning BUFFER in place.

Quickselect with Hoare partitioning: O(n) expected, not the O(n log n) of a full
sort, and no allocation — BUFFER is reused across every cell of the fold. The
pivot is the middle element of the live range, chosen positionally rather than
at random: the result of a selection is the same whatever the pivot, but a
deterministic pivot means a deterministic amount of work, and a runtime whose
cost varies run to run is one nothing can be tuned against.

Hoare rather than Lomuto because a column of identical values — the common case
when several sequences of one machine agree — is Lomuto's quadratic worst case
and Hoare's best."
  (let ((lo 0)
        (hi (1- size)))
    (loop
      (when (>= lo hi)
        (return (aref buffer lo)))
      (let ((pivot (aref buffer (+ lo (floor (- hi lo) 2))))
            (i lo)
            (j hi))
        (loop
          (loop while (< (aref buffer i) pivot) do (incf i))
          (loop while (> (aref buffer j) pivot) do (decf j))
          (when (>= i j) (return))
          (rotatef (aref buffer i) (aref buffer j))
          (incf i)
          (decf j))
        ;; J ends strictly below HI, so the live range always shrinks.
        (if (<= index j)
            (setf hi j)
            (setf lo (1+ j)))))))

(defun fold-output-vectors-boolean (vectors n width name)
  "Fold with the n-input Boolean gate NAME. O(n * width).

The gates are functions of K, how many contributions assert a cell, and N, how
many contributions there are:

    or(k,n)  = k >= 1    and(k,n)  = k = n    xor(k,n)  = k odd
    nor(k,n) = k = 0     nand(k,n) = k < n

Chaining two-input gates would not be equivalent — NOR and NAND are commutative
but not associative — which is why this is written over counts.

CONTESTED, not settled: it has been asserted that any transformation added later
must likewise be a function of K and N alone. That generalisation is disputed,
and the multi-valued transformations are the counter-example: they are functions
of the cell values, not of a count of them. What is established is that all of
them are symmetric.

The collection is walked column-major over a vector of list cursors rather than
by NTH. NTH re-walks a contribution from its head for every cell, which made the
fold quadratic in the output width; the cursors make it linear, and cost one
simple-vector of N conses for the whole fold."
  (let ((cursors (make-array n :initial-contents vectors)))
    (loop repeat width
          collect (let ((k 0))
                    (dotimes (index n)
                      (let* ((cell (svref cursors index))
                             (value (car cell)))
                        (setf (svref cursors index) (cdr cell))
                        (when (and value (not (zerop value)))
                          (incf k))))
                    (if (cond ((string= name "or")   (>= k 1))
                              ((string= name "and")  (= k n))
                              ((string= name "xor")  (oddp k))
                              ((string= name "nor")  (= k 0))
                              ((string= name "nand") (< k n))
                              (t (>= k 1)))
                        1.0d0
                        0.0d0)))))

(defun fold-output-vectors-mv (vectors n width name chain-top)
  "Fold with the multi-valued transformation NAME over the chain {0..CHAIN-TOP}.

Per cell, over a collection of N contributions:

    meet                min(x)          O(n), single pass, early exit at the
                                        chain bottom, which absorbs
    join                max(x)          O(n), single pass, early exit at the
                                        chain top when one is known
    strong-disjunction  min(k, sum x)   O(n), single pass sum, early exit once
                                        the chain top is reached
    strong-conjunction  max(0, sum x - k(n-1))
                                        O(n), single pass sum; the threshold
                                        k(n-1) does not vary per cell and is
                                        computed once, before the loop
    discrete-median     lower middle    O(n) selection, integer-only

For an even N the median is floor(median), which over integers is exactly the
lower of the two middle elements — a selection, never a division. That keeps
float arithmetic out of the step, which is a determinism property as much as an
efficiency one: an integer selection cannot differ across runtimes the way a
float division could.

Shape notes for the optimisation review. The transformation name is resolved to
a keyword once, before the cell loop, so the per-cell dispatch is an EQ CASE and
not five string comparisons. Allocation for the whole fold is two arrays: the
cursor vector, and — only for discrete-median, the only transformation that has
to see a whole cell at once — one selection buffer, reused across every cell
rather than rebuilt per cell. An early exit stops the arithmetic, but the
remaining cursors still have to be advanced one cons each to reach the next
cell; DRAIN does that and nothing else."
  (let* ((cursors (make-array n :initial-contents vectors))
         (operation (cond ((string= name "meet")               :meet)
                          ((string= name "join")               :join)
                          ((string= name "strong-disjunction") :strong-disjunction)
                          ((string= name "strong-conjunction") :strong-conjunction)
                          (t                                   :discrete-median)))
         ;; Loop-invariant, so not recomputed per cell. CHAIN-TOP can only be NIL
         ;; for meet, join and discrete-median by the time control reaches here —
         ;; the two strong operations refuse without one, upstream in
         ;; FOLD-OUTPUT-VECTORS — so the guard is about arithmetic on NIL, not
         ;; about a fallback.
         (threshold (if chain-top (* chain-top (1- n)) 0))
         (buffer (when (eq operation :discrete-median)
                   (make-array n :element-type 'fixnum)))
         (median-index (floor (1- n) 2)))
    (macrolet ((next (position)
                 ;; Read the current cell of contribution POSITION and advance
                 ;; that cursor. (CAR NIL) and (CDR NIL) are NIL, so a
                 ;; contribution shorter than the widest needs no guard: it
                 ;; presents the chain bottom for the rest of the fold.
                 `(let* ((%p ,position)
                         (%cell (svref cursors %p)))
                    (setf (svref cursors %p) (cdr %cell))
                    (mv-cell-integer (car %cell))))
               (drain (from)
                 `(loop for %i from ,from below n
                        do (setf (svref cursors %i) (cdr (svref cursors %i))))))
      (loop repeat width
            collect (coerce
                     (case operation
                       (:meet
                        (let ((out (next 0)))
                          (loop for index from 1 below n
                                do (let ((value (next index)))
                                     (when (< value out)
                                       (setf out value)
                                       (when (zerop out)
                                         (drain (1+ index))
                                         (return)))))
                          out))
                       (:join
                        ;; Each read is CLAMPED to CHAIN-TOP when one is known,
                        ;; and the clamp is load-bearing rather than cosmetic.
                        ;; Without it the early exit makes join order-dependent
                        ;; for a contribution above the chain: an earlier
                        ;; out-of-chain value stops the scan at its own
                        ;; magnitude and a later one is never compared, so
                        ;; [1,3,5] at k=3 answered 3 while [5,3,1] answered 5 —
                        ;; the same collection, two results, which is precisely
                        ;; the property this fold exists to guarantee. The
                        ;; on-chain sweep cannot see it, since there the early
                        ;; exit can only fire on a genuine maximum.
                        ;;
                        ;; A contribution above k is a malformed corpus; reading
                        ;; it as k neither fabricates a rung nor lets arrival
                        ;; order decide (RealityEngine_CI#158). C++, Scala and
                        ;; the reference all clamp; a runtime that stopped would
                        ;; diverge from the other two off-chain.
                        (flet ((bounded (value)
                                 (if (and chain-top (> value chain-top)) chain-top value)))
                          (let ((out (bounded (next 0))))
                            (loop for index from 1 below n
                                  do (let ((value (bounded (next index))))
                                       (when (> value out)
                                         (setf out value)
                                         (when (and chain-top (>= out chain-top))
                                           (drain (1+ index))
                                           (return)))))
                            out)))
                       (:strong-disjunction
                        (let ((total 0))
                          (dotimes (index n)
                            (incf total (next index))
                            (when (>= total chain-top)
                              (setf total chain-top)
                              (drain (1+ index))
                              (return)))
                          total))
                       (:strong-conjunction
                        (let ((total 0))
                          (dotimes (index n)
                            (incf total (next index)))
                          (if (> total threshold)
                              (- total threshold)
                              0)))
                       (t
                        (dotimes (index n)
                          (setf (aref buffer index) (next index)))
                        (mv-select-nth buffer n median-index)))
                     'double-float)))))

(defun fold-output-vectors (vectors transformation &key chain-top)
  "Fold VECTORS — a machine's collection of potential outputs — into one.

An n-input transformation applied to the whole collection at once, not a chain
of two-input ones. For the Boolean gates chaining would not even be equivalent —
NOR and NAND are commutative but not associative — and for the multi-valued
transformations the collection carries no order to chain along in the first
place. Every transformation here is symmetric, which is what makes folding a
collection well defined; idempotence is not required and the two strong
operations deliberately lack it.

CHAIN-TOP is k, the top of the ordered chain {0..k} the multi-valued
transformations are defined on. It is a property of the machine's alphabet, so
it is a parameter and not a constant, and it is NOT derivable from
`bitsPerElement`. The Boolean gates, meet and discrete-median ignore it; join
uses it only as an early-exit bound and is total without it; the two strong
operations are undefined without it and refuse — see MV-FOLD-REFUSES-P, which is
the one place that decision lives.

NIL for an empty collection, and NIL for a refusal: a machine that completed no
Reality Event presents no output, and so does one whose declared transformation
cannot be evaluated. Neither is the same as presenting a vector of zeros, which
is a positive claim about every cell."
  (when vectors
    (let ((n (length vectors))
          (width (reduce #'max vectors :key #'length :initial-value 0))
          (name (output-merge-name transformation)))
      (cond
        ((mv-fold-refuses-p name chain-top) nil)
        ((mv-output-merge-p name)
         (fold-output-vectors-mv vectors n width name chain-top))
        (t (fold-output-vectors-boolean vectors n width name))))))

(defun machine-chain-top (machine)
  "k for MACHINE, or NIL when it declares none.

Read from `perceptualMapping.outputAlphabetTop`, which the corpus schema
requires of exactly the machines that select `strong-conjunction` or
`strong-disjunction` and permits on any other. NIL is not a defect: every other
transformation ignores k, and the two that need it refuse without it rather
than guessing (MV-FOLD-REFUSES-P)."
  (let ((mapping (machine-mapping machine)))
    (and mapping (mapping-output-alphabet-top mapping))))

;; One completed Reality Event, carrying the sequence that completed it. The
;; machine's collection of potential outputs is a list of these, and the fold
;; consumes exactly that list — so the value the machine presents and the
;; evidence for it are derived from one enumeration rather than two that could
;; drift. Mirrors C++ `PendingOutput`.
(defstruct pending-output sequence-id values provenance)

(defstruct transition-result input-vector timestamp sequence-results sequence-outputs
                             machine-output merged-output pending-outputs arbiter-metadata)

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
        ;; Round-tripped rather than dropped: several routes rebuild a machine
        ;; with (machine-from-json (machine-json machine :full t)), and a copy
        ;; that lost k would silently refuse the fold its original performed.
        (when (mapping-output-alphabet-top mapping)
          (setf (jget out "outputAlphabetTop") (mapping-output-alphabet-top mapping)))
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
         "outputMergeTransformation" (output-merge-name
                                      (machine-output-merge-transformation machine))
         "outputMergeLocked" (json-bool (machine-output-merge-locked machine))
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
      ;; PENDING-OUTPUTS is the collection: one potential output per completed
      ;; Reality Event, gated by the arbiter rule exactly as the merge batch
      ;; always was. Enumerated in sorted sequence-id order, then output order
      ;; within a sequence, which is the order C++ walks its std::map-keyed
      ;; sequenceResults in — provenance is unioned in this order, so it is
      ;; contract rather than convenience.
      ;;
      ;; MACHINE-OUTPUT above takes the first member of the collection, and
      ;; which member "first" is has differed per runtime — the same corpus
      ;; presented one runtime's pick to its PE and another's to its own
      ;; (RealityEngine_CI#154). It is left as it was so nothing reading
      ;; `outputVector` today changes.
      ;;
      ;; The fold is the machine's actual output: the collection put through the
      ;; n-input gate the machine declares. Computed here, once, where the
      ;; collection is complete and the machine's own work has finished. The
      ;; same value is both what the Perception Engine reads as
      ;; `mergedOutputVector` and what the machine contributes to arbitration —
      ;; deliberately one computation, because two would be free to drift and
      ;; the drift would be invisible in any single step.
      ;;
      ;; The gate matters: previously the fold ran over every asserted output
      ;; whether or not the arbiter rule admitted them, so an `and`-rule machine
      ;; that had not completed every sequence still published a
      ;; `mergedOutputVector`. C++ folds `pendingOutputs`, which is empty unless
      ;; shouldOutput, so this runtime was the outlier.
      (let* ((pending-outputs
               (when should-output
                 (loop for sequence-id in (object-keys-sorted sequence-outputs)
                       append (mapcar (lambda (output)
                                        (make-pending-output
                                         :sequence-id sequence-id
                                         :values (output-vector-vector output)
                                         :provenance (output-vector-provenance output)))
                                      (gethash sequence-id sequence-outputs)))))
             ;; CHAIN-TOP comes from the machine's declared alphabet. It is NOT
             ;; derivable from `bitsPerElement`, so a machine that selects one of
             ;; the two strong operations without declaring k presents no output
             ;; rather than a guessed one (RealityEngine_CI#158). A refusal is
             ;; NIL here, and a NIL merged output means the machine contributes
             ;; no merge operation at all — not a vector of zeros, which would be
             ;; a positive claim about every cell.
             (merged-output (fold-output-vectors
                             (mapcar #'pending-output-values pending-outputs)
                             (machine-output-merge-transformation machine)
                             :chain-top (machine-chain-top machine))))
        (make-transition-result
         :input-vector input
         :timestamp (now-ms)
         :sequence-results sequence-results
         :sequence-outputs sequence-outputs
         :machine-output machine-output
         :merged-output merged-output
         :pending-outputs pending-outputs
         :arbiter-metadata (obj "rule" rule
                                "totalInputs" total
                                "sequencesWithOutput" sequences-with-output
                                "shouldOutput" (json-bool should-output)))))))

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
  "Canonical merge ordering — by `machineId` alone.

`sequenceId` and `outputIndex` were the secondary and tertiary keys while a
machine contributed one operation per asserted output. It now contributes one
per output region, so `machineId` is unique across the batch and orders it
totally on its own. Keeping the old keys would not be wrong, only inert: they
are constant within every comparison the sort can now make."
  (sort operations
        (lambda (left right)
          (string< (jstring left "machineId" "")
                   (jstring right "machineId" "")))))

(defun transition-result-json (result)
  (obj "inputVector" (vectorize (transition-result-input-vector result))
       "timestamp" (transition-result-timestamp result)
       "sequenceResults" (transition-result-sequence-results result)
       "machineOutput" (if (transition-result-machine-output result)
                           (output-vector-json (transition-result-machine-output result))
                           +json-null+)
       "arbiterMetadata" (transition-result-arbiter-metadata result)))

(defun extract-region (space region)
  "Read REGION out of SPACE as a list, zero-filling past the end.

O(region length).  The perceptual space is a vector (see
MAKE-PERCEPTUAL-SPACE), so cells are read with AREF rather than NTH.  This
used to be two O(n) operations *per element*: (nth i space) walked i conses,
and (length space) was loop-invariant but re-evaluated on every iteration,
walking all ~17k conses each time.  With one call per machine per step across
a 1,300-machine corpus that dominated this runtime's CPU (#60).

Lists are still accepted: several callers extract from a plain
NUMBERS-FROM-JSON result (universalInputSpace), which is not the shared space.
That path walks the list once with a cursor instead of re-walking from the
head per element."
  (let* ((offset (region-offset region))
         (len (region-length region)))
    (if (vectorp space)
        (let ((n (length space)))
          (loop for i from offset below (+ offset len)
                collect (if (< i n) (aref space i) 0.0d0)))
        (let ((tail (nthcdr offset space)))
          (loop repeat len
                collect (if tail (or (pop tail) 0.0d0) 0.0d0))))))

(defun merge-region (space region values)
  "Write VALUES into SPACE at REGION and return the space.

Destructive on a vector space, and the caller assigns the result back because
growth may return a different array.  The list version was quadratic three
times over: it copied the whole space, appended one cons at a time to reach
the needed length (re-measuring with an O(n) LENGTH per pass), then wrote each
value with (setf (nth i out) ...) at O(i) apiece.  Committing arbitration
called it once per resolved cell, so a step that resolved k cells copied the
entire ~17k-element space k times (#60)."
  (let* ((offset (region-offset region))
         (needed (+ offset (length values))))
    (if (vectorp space)
        (let ((out (grow-perceptual-space space needed))
              (i offset))
          (map nil (lambda (value)
                     (setf (aref out i) (coerce (or value 0) 'double-float))
                     (incf i))
               values)
          out)
        (let ((out (copy-list space)))
          (loop while (< (length out) needed) do (setf out (append out (list 0.0d0))))
          (loop for value in values
                for i from offset
                do (setf (nth i out) value))
          out))))
