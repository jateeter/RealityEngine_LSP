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
             (#\/ (write-string "\\/" stream))
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
    ((floatp value)   (format stream "~F" value))
    ((numberp value)  (princ value stream))
    ((hash-table-p* value)
     (write-char #\{ stream)
     (let ((first t))
       (maphash
        (lambda (key item)
          (unless first (write-char #\, stream))
          (setf first nil)
          (write-json (if (stringp key) key (princ-to-string key)) stream)
          (write-char #\: stream)
          (write-json item stream))
        value))
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
