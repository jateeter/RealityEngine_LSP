(in-package #:reality-engine-lsp)

;;;; Runtime snapshots (RealityEngine_CI#287, phase 2; SURFACE_SPEC "K-lines and
;;;; runtime snapshots": "The snapshot body", "Routes"). The same rules as
;;;; RealityEngine_CPP src/snapshot.cpp: a re-snapshot/1 body over retained
;;;; steps, its digest over JCS without the clocks' instance and lamport, a
;;;; store (memory, file:<dir>, off), and restore.
;;;;
;;;; The retention keeps, for every retained step (historyLimit), the active
;;;; list after it, its contributions and its clock, whether or not K-line
;;;; capture is on (owner decision 4, 2026-11-24).

(defstruct (snap-step (:conc-name sst-)) step lamport isre osre ops after)

(defstruct (snapshot-retention (:conc-name sret-))
  (base nil) (base-set nil) (steps nil))   ; steps oldest first

(defun snapshot-all-actives (machines)
  (let (out)
    (maphash (lambda (_ m) (declare (ignore _)) (setf out (append (machine-actives m) out))) machines)
    out))

(defun snapshot-retain (ret step lamport isre osre merge-batch machines limit)
  (unless (or (sret-steps ret) (sret-base-set ret))
    ;; The reset hook runs before the machines reset, so the base is taken at the
    ;; first step after it: every machine at its initial events.
    (let (base)
      (maphash (lambda (_ m) (declare (ignore _)) (setf base (append (machine-initial-actives m) base))) machines)
      (setf (sret-base ret) base (sret-base-set ret) t)))
  (setf (sret-steps ret)
        (append (sret-steps ret)
                (list (make-snap-step :step step :lamport lamport :isre isre :osre osre
                                      :ops (mapcar (lambda (op)
                                                     (list (jstring op "machineName" "")
                                                           (make-region-from-json (jget op "region"))
                                                           (coerce (or (jget op "values") #()) 'list)))
                                                   merge-batch)
                                      :after (snapshot-all-actives machines)))))
  (loop while (> (length (sret-steps ret)) (max 1 limit))
        do (setf (sret-base ret) (sst-after (first (sret-steps ret)))
                 (sret-steps ret) (rest (sret-steps ret)))))

(defun snapshot-retention-reset (ret)
  (setf (sret-base ret) nil (sret-base-set ret) nil (sret-steps ret) nil))

(defun snapshot-retention-restore (ret base)
  (setf (sret-base ret) base (sret-base-set ret) t (sret-steps ret) nil))

(defun snapshot-at (ret step) (find step (sret-steps ret) :key #'sst-step))

(defun snapshot-before (ret step)
  "(values list found-p): the list after STEP-1, or the base when STEP is the first."
  (let ((prev (snapshot-at ret (1- step))))
    (cond (prev (values (sst-after prev) t))
          ((or (null (sret-steps ret)) (eql (sst-step (first (sret-steps ret))) step))
           (values (sret-base ret) t))
          (t (values nil nil)))))

(defun snapshot-digest (body)
  "sha256:<hex> over BODY without digest and the clocks' instance and lamport."
  (let ((copy (parse-json (jcs body))))
    (remhash "digest" copy)
    (let ((clocks (jget (jget copy "kline") "clocks")))
      (when (hash-table-p clocks)
        (dolist (end '("from" "to"))
          (let ((c (gethash end clocks)))
            (when (hash-table-p c) (remhash "instance" c) (remhash "lamport" c))))))
    (format nil "sha256:~a" (sha256-hex (jcs copy)))))

(defun snapshot-id (body)
  (let ((d (jstring body "digest" "")))
    (if (and (> (length d) 7) (string= "sha256:" d :end2 7)) (subseq d 7) d)))

(defun snapshot-capture (ret &key scope from to instance fingerprint machine-count controls machine-controls
                                   overrides arbitration machines)
  "(values body status error-body). SCOPE nil is the corpus, else a list of names."
  (let ((steps (sret-steps ret)))
    (when (null steps)
      (return-from snapshot-capture (values nil 410 (obj "error" "no step is retained"))))
    (let* ((newest (sst-step (car (last steps))))
           (to (or to from newest))
           (from (or from to)))
      (when (> from to)
        (return-from snapshot-capture (values nil 422 (obj "error" "span.from is after span.to"))))
      (dolist (s (list from to))
        (unless (snapshot-at ret s)
          (return-from snapshot-capture
            (values nil 410 (obj "error" (format nil "step ~d is no longer retained" s))))))
      (multiple-value-bind (before found) (snapshot-before ret from)
        (unless found
          (return-from snapshot-capture
            (values nil 410 (obj "error" (format nil "step ~d is no longer retained" (1- from))))))
        (let ((inputs nil) (outputs nil) (others nil) (shared nil)
              (scope-sorted (and scope (sort (remove-duplicates (copy-list scope) :test #'string=) #'octets<))))
          (when scope
            (maphash (lambda (_ m)
                       (declare (ignore _))
                       (let ((mp (machine-mapping m)))
                         (when mp
                           (if (member (machine-name m) scope :test #'string=)
                               (progn (push (mapping-input mp) inputs) (push (mapping-output mp) outputs))
                               (push (mapping-output mp) others)))))
                     machines)
            (dolist (o outputs)
              (dolist (x others)
                (loop for c from (max (region-offset o) (region-offset x))
                        below (min (+ (region-offset o) (region-length o)) (+ (region-offset x) (region-length x)))
                      do (pushnew c shared)))))
          (flet ((in-scope (list) (if scope (remove-if-not (lambda (e) (member (first e) scope :test #'string=)) list) list))
                 (clock (s) (obj "instance" instance "lamport" (sst-lamport s) "step" (sst-step s))))
            (let* ((pairs
                     (loop for k from from to to
                           for s = (snapshot-at ret k)
                           unless s do (return-from snapshot-capture
                                         (values nil 410 (obj "error" (format nil "step ~d is no longer retained" k))))
                           collect (let ((ops (sort (remove-if-not (lambda (op) (or (null scope) (member (first op) scope :test #'string=)))
                                                                   (copy-list (sst-ops s)))
                                                    (lambda (a b)
                                                      (let ((oa (region-offset (second a))) (ob (region-offset (second b))))
                                                        (if (/= oa ob) (< oa ob) (octets< (first a) (first b))))))))
                                     (obj "stepNumber" k
                                          "isre" (entry-projected (sst-isre s) (and scope (or inputs (list (make-region :offset 0 :length 0)))))
                                          "osre" (entry-projected (sst-osre s) (and scope (or outputs (list (make-region :offset 0 :length 0)))))
                                          "contributions"
                                          (vectorize (mapcar (lambda (op)
                                                               (obj "machine" (first op)
                                                                    "region" (obj "offset" (region-offset (second op))
                                                                                  "length" (region-length (second op)))
                                                                    "values" (vectorize (third op))))
                                                             ops))))))
                   (body (obj "format" "re-snapshot/1"
                              "corpus" (obj "algorithm" "sha256/resident-declared-v1" "fingerprint" fingerprint
                                            "machines" machine-count)
                              "controls" controls "machineControls" machine-controls "machineOverrides" overrides
                              "kline" (obj "scope" (if scope (vectorize scope-sorted) "corpus")
                                           "span" (obj "from" from "to" to)
                                           "clocks" (obj "from" (clock (snapshot-at ret from)) "to" (clock (snapshot-at ret to)))
                                           "activeBefore" (actives-json (in-scope before))
                                           "pairs" (vectorize pairs)
                                           "activeAfter" (actives-json (in-scope (sst-after (snapshot-at ret to))))
                                           "sharedCells" (vectorize (sort shared #'<)))
                              "arbitration" arbitration)))
              (setf (gethash "digest" body) (snapshot-digest body))
              (values body 200 nil))))))))

;;; ── The store ─────────────────────────────────────────────────────────────

(defstruct (snapshot-store (:conc-name snps-)) (mode :memory) dir (memory (make-hash-table :test #'equal)))

(defun snapshot-store-from-environment ()
  "RE_SNAPSHOT_STORE: memory (default), file:<dir>, off. qdrant:<url> is phase 3
and is off until then, never a boot failure."
  (let ((v (or (env "RE_SNAPSHOT_STORE" nil) "memory")))
    (cond ((or (string= v "") (string= v "memory")) (make-snapshot-store :mode :memory))
          ((and (> (length v) 5) (string= "file:" v :end2 5))
           (let ((dir (uiop:ensure-directory-pathname (subseq v 5))))
             (ensure-directories-exist dir)
             (make-snapshot-store :mode :file :dir dir)))
          (t (make-snapshot-store :mode :off)))))

(defun snapshot-store-enabled-p (st) (not (eq (snps-mode st) :off)))

(defun snapshot-id-p (id) (and (plusp (length id)) (every (lambda (c) (find c "0123456789abcdef")) id)))

(defun snapshot-store-put (st id body)
  (if (eq (snps-mode st) :file)
      (write-durable (merge-pathnames (format nil "~a.json" id) (snps-dir st)) (format nil "~a~%" (jcs body)))
      (setf (gethash id (snps-memory st)) body)))

(defun snapshot-store-get (st id)
  (when (snapshot-id-p id)
    (if (eq (snps-mode st) :file)
        (let ((f (merge-pathnames (format nil "~a.json" id) (snps-dir st))))
          (and (probe-file f) (parse-json (uiop:read-file-string f))))
        (gethash id (snps-memory st)))))

(defun snapshot-store-remove (st id)
  (when (snapshot-store-get st id)
    (if (eq (snps-mode st) :file)
        (delete-file (merge-pathnames (format nil "~a.json" id) (snps-dir st)))
        (remhash id (snps-memory st)))
    t))

(defun snapshot-store-list (st)
  (let ((bodies (if (eq (snps-mode st) :file)
                    (loop for f in (uiop:directory-files (snps-dir st) "*.json")
                          collect (parse-json (uiop:read-file-string f)))
                    (loop for b being the hash-values of (snps-memory st) collect b))))
    (setf bodies (stable-sort bodies #'< :key (lambda (b) (jnumber (jget (jget (jget b "kline") "clocks") "to") "lamport" 0))))
    (obj "snapshots" (vectorize (mapcar (lambda (b)
                                          (let ((k (jget b "kline")))
                                            (obj "id" (snapshot-id b) "digest" (jget b "digest") "scope" (jget k "scope")
                                                 "span" (jget k "span") "machines" (jget (jget b "corpus") "machines"))))
                                        bodies)))))
