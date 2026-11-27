(in-package #:reality-engine-lsp)

;;;; The PE half of a K-line: one ISRE composition record per completed push
;;;; (re-isre-composition/1), kept in the K-line store's format per PE instance
;;;; (RealityEngine_CI#603; SURFACE_SPEC "The PE half: ISRE composition"). The
;;;; same rules as RealityEngine_CPP src/isre.cpp.

(defconstant +isre-taxonomy-version+ 2
  "The source-kind taxonomy version the PEs record (owner, 2026-11-27).")

(defstruct (isre-taxonomy (:conc-name itax-))
  version
  (provider-mapping (make-hash-table :test #'equal))
  (kind-class (make-hash-table :test #'equal))       ; kind -> class
  (provider-class (make-hash-table :test #'equal))   ; kind -> (hash provider -> class)
  (unknown-provider "unattributed")
  (unknown-class "generated"))

(defun isre-taxonomy-dir (machines-dir)
  "<MACHINES_DIR>/../schemas/source-kinds/"
  (merge-pathnames "../schemas/source-kinds/"
                   (uiop:ensure-directory-pathname
                    (or (and machines-dir (plusp (length machines-dir)) machines-dir)
                        "../RealityEngine_Machines/machines"))))

(defun load-isre-taxonomy (dir &optional (version +isre-taxonomy-version+))
  "re-source-kinds-<VERSION>.json from DIR, or NIL when it is not there."
  (let ((file (merge-pathnames (format nil "re-source-kinds-~d.json" version) dir)))
    (when (probe-file file)
      (let ((doc (ignore-errors (parse-json (uiop:read-file-string file)))))
        (when (and (hash-table-p doc) (equal (jstring doc "format" "") "re-source-kinds")
                   (eql (jnumber doc "version" -1) version))
          (let ((tx (make-isre-taxonomy :version version)))
            (let ((pm (jget doc "providerMapping")))
              (when (hash-table-p pm)
                (maphash (lambda (k v) (setf (gethash k (itax-provider-mapping tx)) v)) pm)))
            (loop for k across (or (jget doc "kinds") #())
                  for kind = (jstring k "kind" "")
                  do (if (vectorp (jget k "providers"))
                         (let ((h (make-hash-table :test #'equal)))
                           (loop for p across (jget k "providers")
                                 do (setf (gethash (jstring p "provider" "") h) (jstring p "class" "")))
                           (setf (gethash kind (itax-provider-class tx)) h))
                         (when (stringp (jget k "class"))
                           (setf (gethash kind (itax-kind-class tx)) (jget k "class")))))
            (let ((u (jget doc "unknownProvider")))
              (setf (itax-unknown-provider tx) (jstring u "provider" "unattributed")
                    (itax-unknown-class tx) (jstring u "class" "generated")))
            tx))))))

(defun isre-taxonomy-name (tx) (format nil "re-source-kinds/~d" (itax-version tx)))

(defun isre-source-entry (tx kind origin)
  "(values kind provider class) for a source, as RealityEngine_CPP Taxonomy::source_entry."
  (multiple-value-bind (cls present) (gethash kind (itax-kind-class tx))
    (when present (return-from isre-source-entry (values kind nil cls))))
  (let* ((as-kind (if (nth-value 1 (gethash kind (itax-provider-class tx))) kind "sensor"))
         (origin (or origin ""))
         (dot (position #\. origin))
         (provider (string-downcase (subseq origin 0 (or dot (length origin))))))
    (when (zerop (length provider)) (setf provider "signal"))
    (multiple-value-bind (mapped present) (gethash provider (itax-provider-mapping tx))
      (when present (setf provider mapped)))
    (let* ((h (gethash as-kind (itax-provider-class tx)))
           (cls (and h (gethash provider h))))
      (if cls
          (values as-kind provider cls)
          (values as-kind (itax-unknown-provider tx) (itax-unknown-class tx))))))

(defun isre-feedback-class (tx) (or (gethash "feedback" (itax-kind-class tx)) "deterministic"))

(defun isre-sources-json (cells tx)
  "CELLS: list of (cell feedback-p id kind origin caused-by). Runs of one winner, ascending."
  (let ((runs nil) (cur nil))
    (flet ((flush ()
             (when cur
               (destructuring-bind (first last kind provider cls id caused-by) cur
                 (let ((o (obj "cells" (vector first last) "kind" kind "class" cls
                               "causedBy" (or caused-by +json-null+))))
                   (when provider (setf (gethash "provider" o) provider))
                   (when (plusp (length id)) (setf (gethash "id" o) id))
                   (push o runs)))
               (setf cur nil))))
      (dolist (c (sort (copy-list cells) #'< :key #'first))
        (destructuring-bind (cell feedback-p id kind origin caused-by) c
          (multiple-value-bind (k p cls)
              (if feedback-p (values "feedback" nil (isre-feedback-class tx)) (isre-source-entry tx kind origin))
            (let ((id (if feedback-p "" (or id "")))
                  (cb (if feedback-p nil caused-by)))
              (if (and cur (= (1+ (second cur)) cell) (equal (third cur) k) (equal (fourth cur) p)
                       (equal (fifth cur) cls) (equal (sixth cur) id)
                       (equal (jcs (or (seventh cur) +json-null+)) (jcs (or cb +json-null+))))
                  (setf (second cur) cell)
                  (progn (flush) (setf cur (list cell cell k p cls id cb))))))))
      (flush))
    (vectorize (nreverse runs))))

(defstruct (isre-recorder (:conc-name isr-))
  instance store taxonomy (capture nil) (store-max-bytes (* 2 1024 1024 1024)) (re-instance ""))

(defun isre-record (rec re pe cells folds)
  (when (and (isr-capture rec) (isr-taxonomy rec))
    (when (stringp (jget re "instance")) (setf (isr-re-instance rec) (jget re "instance")))
    (let ((line (obj "format" "re-isre-composition/1" "taxonomy" (isre-taxonomy-name (isr-taxonomy rec))
                     "re" re "pe" pe "sources" (isre-sources-json cells (isr-taxonomy rec))
                     "folds" (if (vectorp folds) folds (vector)) "delta" +json-false+)))
      (kstore-append (isr-store rec) (list line) (isre-taxonomy-name (isr-taxonomy rec))
                     (truncate (jnumber re "commit" 0)))
      (kstore-enforce-budget (isr-store rec) (list :store-max-bytes (isr-store-max-bytes rec))))))

(defun isre-list (rec from to limit)
  (let ((limit (min 1000 (max 1 (if (and limit (plusp limit)) limit 100))))
        (out nil) (count 0) (next +json-null+))
    (loop for m across (kstore-metas (isr-store rec))
          for c = (kmeta-close-commit m)
          when (and (>= c from) (or (minusp to) (<= c to)))
            do (when (>= count limit) (setf next c) (return))
               (push (parse-json (kstore-raw (isr-store rec) m)) out)
               (incf count))
    (obj "records" (vectorize (nreverse out)) "next" next)))

(defun isre-stats (rec)
  (let ((s (kstore-stats (isr-store rec) (list :store-max-bytes (isr-store-max-bytes rec)))))
    (obj "segments" (jget s "segments") "bytes" (jget s "bytes") "budget" (jget s "budget")
         "warnings" (jget s "warnings") "records" (length (kstore-metas (isr-store rec)))
         "isreCapture" (json-bool (isr-capture rec))
         "taxonomy" (if (isr-taxonomy rec) (isre-taxonomy-name (isr-taxonomy rec)) +json-null+))))

(defun isre-export (rec from to)
  (let ((store (isr-store rec)) (count 0) (segments nil))
    (let ((body (with-output-to-string (s)
                  (loop for m across (kstore-metas store)
                        for c = (kmeta-close-commit m)
                        when (and (>= c from) (or (minusp to) (<= c to)))
                          do (write-string (kstore-raw store m) s) (write-char #\Newline s) (incf count)
                             (pushnew (kmeta-segment m) segments :test #'string=)))))
      (dolist (seg (kstore-segments store))
        (when (member (kseg-name seg) segments :test #'string=) (setf (kseg-exported seg) t)))
      (kstore-write-manifest store)
      (format nil "~a~%~a"
              (jcs (obj "format" "re-isre-bundle/1" "source" (obj "instance" (isr-instance rec))
                        "re" (obj "instance" (isr-re-instance rec))
                        "taxonomy" (if (isr-taxonomy rec) (isre-taxonomy-name (isr-taxonomy rec)) +json-null+)
                        "count" count "sha256" (sha256-hex body)))
              body))))

(defun make-engine-isre-recorder (instance)
  "The PE's recorder: its store under <INSTANCE_ISRE_DIR>/<pe_uuid>/ for an allocated
instance (the parent defaulting to the clock directory's sibling), in memory for
a minted one. With RE_ISRE_CAPTURE=1 and the taxonomy missing, the boot is refused."
  (let* ((allocated (string-downcase (or (env "PE_INSTANCE_UUID" nil) "")))
         (capture (member (or (env "RE_ISRE_CAPTURE" nil) "") '("1" "true") :test #'string=))
         (taxonomy (load-isre-taxonomy (isre-taxonomy-dir (env "MACHINES_DIR" nil))))
         (dir (when (string= allocated instance)
                (merge-pathnames (format nil "~a/" instance)
                                 (uiop:ensure-directory-pathname
                                  (or (env "INSTANCE_ISRE_DIR" nil)
                                      (merge-pathnames "../isre/"
                                                       (uiop:ensure-directory-pathname
                                                        (or (env "INSTANCE_CLOCK_DIR" nil)
                                                            (merge-pathnames ".reality-engine/clock/" (user-homedir-pathname)))))))))))
    (when (and capture (null taxonomy))
      (error "isreCapture is on and re-source-kinds/~d is not in the corpus (~a)"
             +isre-taxonomy-version+ (namestring (isre-taxonomy-dir (env "MACHINES_DIR" nil)))))
    (when capture
      (format *error-output* "~&ISRE composition capture on — store ~a~%" (if dir (namestring dir) "in memory")))
    (make-isre-recorder :instance instance :taxonomy taxonomy :capture (and capture t)
                        :store (open-kline-store (and dir (namestring dir))))))
