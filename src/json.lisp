(in-package #:reality-engine-lsp)

(defparameter +json-false+ :false)
(defparameter +json-null+ :null)

(defun json-bool (value)
  (if value t +json-false+))

(defun obj (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash (string key) table) value))
    table))

(defun arr (&rest values)
  (coerce values 'vector))

(defun vectorize (values)
  (coerce values 'vector))

(defun hash-table-p* (value)
  (typep value 'hash-table))

(defun jget (object key &optional default)
  (if (hash-table-p* object)
      (multiple-value-bind (value present-p) (gethash (string key) object)
        (if present-p value default))
      default))

(defun (setf jget) (value object key &optional default)
  (declare (ignore default))
  (setf (gethash (string key) object) value))

(defun jget-either (object canonical legacy &optional default)
  "One observation, two spellings, during the corpus schema rename.

A machine file names the same collection EVENTS or VECTORS depending on whether
it has been rewritten yet (RealityEngine_CI#220 layer 1). A loader accepting only
one reads an empty list from the other — with no error, no condition signalled,
and a machine that loads with no sequences while the engine reports success.

CANONICAL is tried first. Absent covers NIL and +JSON-NULL+ both, since a decoded
JSON null arrives as :NULL here.

Deleted in layer 1c, once the corpus carries only the new spelling."
  (let ((value (jget object canonical)))
    (if (or (null value) (eq value +json-null+))
        (jget object legacy default)
        value)))

(defun jobject-p (value)
  (hash-table-p* value))

(defun jarray-p (value)
  (or (vectorp value) (and (listp value) (not (hash-table-p* value)))))

(defun jarray-present-p (object key)
  "True when OBJECT has KEY and its value is an array.

`jarray-p' alone must never be used to test an optional key: a missing key
yields NIL, and NIL satisfies `listp', so `(jarray-p (jget body \"k\"))' is
true for every object.  In a `cond' that makes the first such branch shadow
every later one.  parse-json binds yason:*parse-json-arrays-as-vectors*, so a
present-but-empty array is #() — non-NIL and therefore still matched here,
while an absent key is not."
  (let ((value (jget object key)))
    (and value (jarray-p value))))

(defun jarray-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t nil)))

(defun jstring (object key &optional default)
  (let ((value (jget object key default)))
    (if (stringp value) value default)))

(defun jnumber (object key &optional default)
  (let ((value (jget object key default)))
    (if (numberp value) value default)))

(defun jbool (object key &optional default)
  (let ((value (jget object key :missing)))
    (cond
      ((eq value :missing) default)
      ((eq value +json-false+) nil)
      ((null value) nil)
      (t value))))

(defun normalize-json-booleans (value)
  "Fold YASON:TRUE / YASON:FALSE into this module's vocabulary — T and
+json-false+ — so write-json round-trips them and jbool reads them.

Without *parse-json-booleans-as-symbols* yason maps JSON `false` and JSON
`null` both to NIL, and write-json renders NIL as `null`.  A corpus `false`
therefore came back out as `null`: every boolean false on the LSP surface was
indistinguishable from a missing value, and differed from C++ and Scala.

Arrays parse as vectors here, so NIL is unambiguously JSON null afterwards.
Strings are vectors too and must not be walked."
  (typecase value
    (hash-table
     (maphash (lambda (k v) (setf (gethash k value) (normalize-json-booleans v))) value)
     value)
    (string value)
    (vector
     (dotimes (i (length value) value)
       (setf (aref value i) (normalize-json-booleans (aref value i)))))
    (t
     (cond ((eq value 'yason:true) t)
           ((eq value 'yason:false) +json-false+)
           (t value)))))

(defun parse-json (text)
  (let ((yason:*parse-object-as* :hash-table)
        (yason:*parse-json-arrays-as-vectors* t)
        ;; Keep false distinguishable from null; normalized immediately below.
        (yason:*parse-json-booleans-as-symbols* t))
    (if (or (null text) (string= text ""))
        (obj)
        (normalize-json-booleans (yason:parse text)))))

(defun json-escape (string stream)
  (loop for ch across string
        do (case ch
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             ;; Solidus escaping is optional in JSON (RFC 8259 §7); C++ and Scala
             ;; both emit it bare, so leave it unescaped to keep bytes identical.
             (#\Backspace (write-string "\\b" stream))
             (#\Page (write-string "\\f" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (let ((code (char-code ch)))
                (if (< code 32)
                    (format stream "\\u~4,'0x" code)
                    (write-char ch stream)))))))

(defun %decimal-exponent (exact)
  "Largest e such that 10^e <= EXACT, for a positive rational EXACT."
  (let ((e 0))
    (if (>= exact 1)
        (loop while (>= exact (expt 10 (1+ e))) do (incf e))
        (loop while (< exact (expt 10 e)) do (decf e)))
    e))

(defun %shortest-digits (magnitude)
  "Return (values digit-string n) with MAGNITUDE = 0.<digits> x 10^n.

SBCL's own float printer is not shortest round-trip for subnormals and breaks
some ties differently from ECMAScript, so the digits are derived with exact
rational arithmetic: try 1..17 significant digits and take the first that
reads back as the identical double.  That is the ECMA-262 rule -- smallest k,
and among those the s closest to m."
  (let* ((exact (rational magnitude))
         (e10 (%decimal-exponent exact)))
    (loop for p from 1 to 17
          do (let* ((scale (- (1- p) e10))
                    (s (round (* exact (expt 10 scale))))
                    (candidate (/ s (expt 10 scale))))
               (when (eql (handler-case (coerce candidate 'double-float)
                            (arithmetic-error () nil))
                          magnitude)
                 (let ((str (princ-to-string s))
                       (n (1+ e10)))
                   ;; Rounding up can carry into an extra digit (999 -> 1000).
                   (when (> (length str) p)
                     (incf n)
                     (setf str (subseq str 0 p)))
                   (let ((last (position #\0 str :from-end t :test #'char/=)))
                     (setf str (if last (subseq str 0 (1+ last)) "")))
                   (return (values str n))))))))

(defun write-json-double (value stream)
  "Write VALUE exactly as ECMAScript Number::toString does (ECMA-262
6.1.6.1.20) -- the form JSON.stringify emits, and so the form the TypeScript
runtime already produces.  Integer form when the value is whole and in plain
range, exponential only outside 1e21 / 1e-7.

Replaces `~F`, which always forced a fractional part (\"0.0\" where C++ and
JavaScript both write \"0\") and never used exponential notation, so 1d20 came
out as a 22-character literal (RealityEngine_CI#91)."
  (cond
    ((or (sb-ext:float-nan-p value) (sb-ext:float-infinity-p value))
     (write-string "null" stream))
    ((zerop value) (write-string "0" stream))
    (t
     (multiple-value-bind (digits n) (%shortest-digits (abs value))
       (let ((k (length digits)))
         (when (minusp value) (write-char #\- stream))
         (cond
           ((and (<= k n) (<= n 21))
            (write-string digits stream)
            (dotimes (i (- n k)) (write-char #\0 stream)))
           ((and (< 0 n) (<= n 21))
            (write-string (subseq digits 0 n) stream)
            (write-char #\. stream)
            (write-string (subseq digits n) stream))
           ((and (< -6 n) (<= n 0))
            (write-string "0." stream)
            (dotimes (i (- n)) (write-char #\0 stream))
            (write-string digits stream))
           (t
            (write-char (char digits 0) stream)
            (when (> k 1)
              (write-char #\. stream)
              (write-string (subseq digits 1) stream))
            (write-char #\e stream)
            (write-char (if (>= (1- n) 0) #\+ #\-) stream)
            (format stream "~D" (abs (1- n))))))))))

(defun write-json (value stream)
  (cond
    ((eq value +json-false+) (write-string "false" stream))
    ((eq value +json-null+) (write-string "null" stream))
    ((null value) (write-string "null" stream))
    ((eq value t) (write-string "true" stream))
    ((stringp value)
     (write-char #\" stream)
     (json-escape value stream)
     (write-char #\" stream))
    ((integerp value) (format stream "~D" value))
    ((floatp value)   (write-json-double (float value 1d0) stream))
    ((numberp value)  (princ value stream))
    ((hash-table-p* value)
     ;; Keys are emitted in sorted order.  maphash walks the table in an
     ;; arbitrary order that is not even stable across runs, and the three
     ;; runtimes each produced a different key order for the same object —
     ;; C++ sorts (its Json::Object is a std::map), Scala and LSP did not.
     ;; Sorting is the canonical rule (RealityEngine_CI#91): it is the one
     ;; ordering all four can agree on without declaring a field order for
     ;; every object type.
     (write-char #\{ stream)
     (let ((first t)
           (keys (let (ks)
                   (maphash (lambda (k _) (declare (ignore _)) (push k ks)) value)
                   (sort ks #'string< :key (lambda (k) (if (stringp k) k (princ-to-string k)))))))
       (dolist (key keys)
         (unless first (write-char #\, stream))
         (setf first nil)
         (write-json (if (stringp key) key (princ-to-string key)) stream)
         (write-char #\: stream)
         (write-json (gethash key value) stream)))
     (write-char #\} stream))
    ((vectorp value)
     (write-char #\[ stream)
     (loop for i from 0 below (length value)
           do (progn
                (when (> i 0) (write-char #\, stream))
                (write-json (aref value i) stream)))
     (write-char #\] stream))
    ((listp value)
     (write-json (coerce value 'vector) stream))
    (t
     (write-json (princ-to-string value) stream))))

(defun json-stringify (value)
  (with-output-to-string (stream)
    (write-json value stream)))

(defun numbers-from-json (value)
  (cond
    ((numberp value) (list value))
    ((jarray-p value)
     (mapcar (lambda (item)
               (cond
                 ((numberp item) item)
                 ((jobject-p item) (or (jnumber item "value" 0.0) 0.0))
                 (t 0.0)))
             (jarray-list value)))
    (t nil)))

(defun object-values (object)
  (let (values)
    (when (hash-table-p* object)
      (maphash (lambda (_ value)
                 (declare (ignore _))
                 (push value values))
               object))
    (nreverse values)))

(defun object-keys-sorted (object)
  (let (keys)
    (when (hash-table-p* object)
      (maphash (lambda (key _)
                 (declare (ignore _))
                 (push key keys))
               object))
    (sort keys #'string<)))

(defun object-values-sorted (object)
  (mapcar (lambda (key) (gethash key object))
          (object-keys-sorted object)))
