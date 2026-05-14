(in-package #:reality-engine-lsp)

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

(defun parse-reality-vector (item)
  (let* ((id (or (jstring item "id" nil) (make-id "vector")))
         (initial-p (jbool item "isInitial" nil))
         (elements (mapcar #'parse-vector-element (jarray-list (or (jget item "elements") (arr)))))
         (next-ids (mapcar #'princ-to-string (jarray-list (or (jget item "nextVectorIds") (arr)))))
         (outputs (mapcar #'parse-output-vector (jarray-list (or (jget item "outputVectors") (arr))))))
    (make-reality-vector :id id
                         :elements elements
                         :initial-p initial-p
                         :active-p initial-p
                         :match-algorithm (comparator-name (jstring item "matchAlgorithm" "gte"))
                         :metadata (or (jget item "metadata") (obj))
                         :next-ids next-ids
                         :output-vectors outputs
                         :just-matched-p nil
                         :predecessor-chain nil)))

(defun parse-sequence (item)
  (let ((vectors (make-hash-table :test #'equal))
        (sequence (make-ces :id (or (jstring item "id" nil) (make-id "sequence"))
                            :name (or (jstring item "name" nil) "unnamed")
                            :metadata (or (jget item "metadata") (obj))
                            :schema-version (jstring item "schemaVersion" nil)
                            :deprecated-at (jstring item "deprecatedAt" nil)
                            :replaced-by (jstring item "replacedBy" nil)
                            :vectors nil)))
    (dolist (vector-json (jarray-list (or (jget item "vectors") (arr))))
      (let ((vector (parse-reality-vector vector-json)))
        (setf (gethash (reality-vector-id vector) vectors) vector)))
    (setf (sequence-vectors sequence) vectors)
    sequence))

(defun parse-mapping (item)
  (when (and (jobject-p item) (jobject-p (jget item "input")) (jobject-p (jget item "output")))
    (make-mapping :input (make-region-from-json (jget item "input"))
                  :output (make-region-from-json (jget item "output")))))

(defun machine-from-json (json &optional forced-id)
  (let* ((root (if (jobject-p (jget json "machine")) (jget json "machine") json))
         (sequences (make-hash-table :test #'equal))
         (machine (make-machine
                   :id (or forced-id (jstring root "id" nil) (make-id "machine"))
                   :name (or (jstring root "name" nil) "unnamed")
                   :description (or (jstring root "description" nil) "")
                   :metadata (or (jget root "metadata") (obj))
                   :mapping (parse-mapping (jget root "perceptualMapping"))
                   :match-algorithm (comparator-name (jstring root "matchAlgorithm" "gte"))
                   :arbiter-rule (arbiter-name (jstring root "arbiterRule" "passthrough"))
                   :sequences nil)))
    (dolist (sequence-json (jarray-list (or (jget root "sequences") (arr))))
      (let ((sequence (parse-sequence sequence-json)))
        (setf (gethash (sequence-id sequence) sequences) sequence)))
    (setf (machine-sequences machine) sequences)
    machine))

(defun load-machine-from-file (path)
  (machine-from-json (parse-json (safe-read-file path))
                     (format nil "machine-~a" (pathname-name path))))

(defun load-machines-from-directory (directory)
  (let ((dir (uiop:ensure-directory-pathname directory))
        (machines nil))
    (when (uiop:directory-exists-p dir)
      (dolist (path (uiop:directory-files dir "*.json"))
        (handler-case
            (push (load-machine-from-file path) machines)
          (error (condition)
            (format *error-output* "~&Skipping machine ~a: ~a~%" path condition)))))
    (nreverse machines)))
