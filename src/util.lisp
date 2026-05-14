(in-package #:reality-engine-lsp)

(defun env (name &optional default)
  (or (uiop:getenv name) default))

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

