(in-package #:reality-engine-lsp)

(defun env (name &optional default)
  (or (uiop:getenv name) default))

(defun env-set-p (name)
  "True when NAME is present and non-empty in the environment.

Distinguishes \"the operator asked for this\" from \"nothing was set\", which
`env` cannot: a caller reading a default back has no way to tell whether the
default or an identical explicit value produced it. Configuration precedence
needs that distinction."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)))))

(defun env-int (name default)
  (handler-case
      (parse-integer (env name (write-to-string default)))
    (error () default)))

(defun env-bool (name default)
  (let ((value (env name nil)))
    (cond
      ((null value) default)
      ((member (string-downcase value) '("1" "true" "yes" "on") :test #'string=) t)
      ((member (string-downcase value) '("0" "false" "no" "off") :test #'string=) nil)
      (t default))))

(defun now-ms ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000) (floor usec 1000))))

(defun make-id (&optional (prefix "id"))
  (format nil "~a-~36r-~36r" prefix (get-universal-time) (random most-positive-fixnum)))

(defun clamp01 (value)
  (max 0.0d0 (min 1.0d0 (coerce value 'double-float))))

(defun string-prefix-p (prefix value)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defun split-string (value delimiter)
  (let ((parts nil)
        (start 0))
    (loop for pos = (position delimiter value :start start)
          do (if pos
                 (progn
                   (push (subseq value start pos) parts)
                   (setf start (1+ pos)))
                 (progn
                   (push (subseq value start) parts)
                   (return))))
    (nreverse parts)))

(defun path-join (&rest parts)
  (reduce (lambda (left right)
            (merge-pathnames right (uiop:ensure-directory-pathname left)))
          parts))

(defun safe-read-file (path)
  (with-open-file (stream path :direction :input)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun cosine (left right)
  (let ((dot 0.0d0)
        (l2 0.0d0)
        (r2 0.0d0))
    (loop for a in left
          for b in right
          do (let ((ad (coerce a 'double-float))
                   (bd (coerce b 'double-float)))
               (incf dot (* ad bd))
               (incf l2 (* ad ad))
               (incf r2 (* bd bd))))
    (if (or (zerop l2) (zerop r2))
        0.0d0
        (/ dot (* (sqrt l2) (sqrt r2))))))


;; ── Perceptual space representation ─────────────────────────────────────────
;; The shared perceptual space is a growable vector of DOUBLE-FLOAT, matching
;; the C++ std::vector<double> and the Scala indexed array.  It was a Lisp
;; list, which made every random access O(n) against a dimension that is 16,944
;; on the deployment corpus and grows with it — see #60 for the measurements.
;;
;; Adjustable with a fill pointer, so LENGTH is the logical dimension while
;; capacity can run ahead of it and growth amortises to O(1).  Serialization is
;; unaffected: WRITE-JSON-DOUBLE already renders whole doubles in integer form
;; (ECMA-262 Number::toString, RealityEngine_CI#91), so a cell holding 1.0d0
;; emits "1" exactly as the integer 1 did.

(defun make-perceptual-space (dimension)
  "A perceptual space of DIMENSION zeroed double-float cells."
  (let ((n (max 0 dimension)))
    (make-array n :element-type 'double-float
                  :initial-element 0.0d0
                  :adjustable t
                  :fill-pointer n)))

(defun perceptual-space-p (space)
  "True when SPACE is a vector rather than the historical list."
  (and (vectorp space) (not (stringp space))))

(defun grow-perceptual-space (space length)
  "Return SPACE addressable to LENGTH, growing by doubling when needed.

Returns a possibly different array, so callers must assign the result back.
Cells added by growth read as 0.0d0 — including cells inside capacity that a
previous, longer fill pointer had used, which is why the fill region below is
explicit rather than left to ADJUST-ARRAY's :initial-element."
  (let ((capacity (array-dimension space 0)))
    (when (> length capacity)
      (setf space (adjust-array space (max length (* 2 capacity))
                                :initial-element 0.0d0)))
    (let ((filled (fill-pointer space)))
      (when (> length filled)
        (setf (fill-pointer space) length)
        (fill space 0.0d0 :start filled :end length))))
  space)

(defun perceptual-space-snapshot (space)
  "A detached copy of SPACE for serialization or history.

VECTORIZE is (coerce x 'vector), which returns a vector argument *unchanged* —
so handing the live space to a JSON payload would alias it, and the next step's
in-place writes would rewrite a response and every history entry that shared
it.  The list representation could not alias this way because it was rebuilt
each step."
  (if (perceptual-space-p space)
      (coerce space 'simple-vector)
      (coerce space 'vector)))
