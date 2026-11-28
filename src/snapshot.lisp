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

;;; ── A snapshot's state, and nearest (SURFACE_SPEC "Nearest", #900) ─────────

(defun snapshot-state-of (body)
  "A snapshot's state, a hash table cell -> value: the ISRE at span.to."
  (let* ((k (jget body "kline"))
         (to (jnumber (jget k "span") "to" -1))
         (out (make-hash-table)))
    (loop for p across (or (jget k "pairs") #())
          when (eql (jnumber p "stepNumber" -2) to)
            do (loop for c across (or (jget (jget p "isre") "nonZero") #())
                     do (setf (gethash (truncate (jnumber c "index" 0)) out) (coerce (jnumber c "value" 0) 'double-float))))
    out))

(defun snapshot-parse-state (isre)
  "{length, nonZero: [{index, value}]} as a state; NIL when malformed."
  (when (and (hash-table-p isre) (vectorp (jget isre "nonZero")) (not (stringp (jget isre "nonZero"))))
    (let ((out (make-hash-table)))
      (loop for c across (jget isre "nonZero")
            do (let ((i (and (hash-table-p c) (jget c "index"))) (v (and (hash-table-p c) (jget c "value"))))
                 (unless (and (realp i) (realp v) (>= i 0)) (return-from snapshot-parse-state nil))
                 (setf (gethash (truncate i) out) (coerce v 'double-float))))
      out)))

(defun snapshot-score (query state)
  "The sparse dot product over the cells both hold, in double, summed in ascending
cell order as every runtime sums it (a hash table's order would move the last bit)."
  (let ((sum 0d0) cells)
    (maphash (lambda (cell v) (push (cons cell v) cells)) query)
    (dolist (c (sort cells #'< :key #'car) sum)
      (let ((w (gethash (car c) state))) (when w (incf sum (* (cdr c) w)))))))

(defun snapshot-summary (b)
  (let ((k (jget b "kline")))
    (obj "id" (snapshot-id b) "digest" (jget b "digest") "scope" (jget k "scope")
         "span" (jget k "span") "machines" (jget (jget b "corpus") "machines"))))

(defun snapshot-lamport (b) (jnumber (jget (jget (jget b "kline") "clocks") "to") "lamport" 0))

(defun bytes< (a b) (string< a b))

(defun snapshot-ranked (scored limit)
  "SCORED is ((score . body) ...): highest first, then id; at most LIMIT."
  (let ((sorted (sort (copy-list scored)
                      (lambda (a b) (if (/= (car a) (car b)) (> (car a) (car b))
                                        (bytes< (snapshot-id (cdr a)) (snapshot-id (cdr b))))))))
    (obj "snapshots" (vectorize (loop for (sc . b) in sorted for i below limit
                                      collect (let ((e (snapshot-summary b))) (setf (gethash "score" e) sc) e))))))

(defun snapshot-sparse (state)
  (let (cells)
    (maphash (lambda (c v) (push (cons c v) cells)) state)
    (setf cells (sort cells #'< :key #'car))
    (obj "indices" (vectorize (mapcar #'car cells)) "values" (vectorize (mapcar #'cdr cells)))))

;;; ── The store ─────────────────────────────────────────────────────────────

(define-condition snapshot-store-unavailable (error)
  ((reason :initarg :reason :reader snapshot-store-unavailable-reason))
  (:report (lambda (c s) (format s "snapshot store unavailable: ~a" (snapshot-store-unavailable-reason c))))
  (:documentation "Qdrant unreachable or answering an error: that request is 503 (#900)."))

(defstruct (snapshot-store (:conc-name snps-))
  (mode :memory) dir (memory (make-hash-table :test #'equal))
  ;; qdrant:<url> (#900): one collection, points filtered by this instance.
  url collection instance off-reason (ready nil))

(defun snapshot-store-from-environment ()
  "RE_SNAPSHOT_STORE: memory (default), file:<dir>, qdrant:<https url>, off. An
unusable value is off, with the reason in the log; never a boot failure."
  (let ((st (snapshot-store-from-value (or (env "RE_SNAPSHOT_STORE" nil) "memory"))))
    (cond ((snps-off-reason st) (format *error-output* "~&~a~%" (snps-off-reason st)))
          ((eq (snps-mode st) :qdrant) (format *error-output* "~&Snapshot store: Qdrant (RE_SNAPSHOT_STORE)~%")))
    st))

(defun snapshot-store-from-value (value)
  (let ((v value))
    (cond ((or (string= v "") (string= v "memory")) (make-snapshot-store :mode :memory))
          ((string= v "off") (make-snapshot-store :mode :off))
          ((and (> (length v) 5) (string= "file:" v :end2 5))
           (let ((dir (uiop:ensure-directory-pathname (subseq v 5))))
             (ensure-directories-exist dir)
             (make-snapshot-store :mode :file :dir dir)))
          ((and (> (length v) 7) (string= "qdrant:" v :end2 7))
           (let ((url (string-right-trim "/" (subseq v 7))))
             (if (and (>= (length url) 8) (string-equal "https://" url :end2 8))
                 (make-snapshot-store :mode :qdrant :url url
                                      :collection (let ((c (env "RE_SNAPSHOT_COLLECTION" nil)))
                                                    (if (and c (plusp (length c))) c "re-snapshots"))
                                      :instance (let ((i (env "INSTANCE_UUID" nil)))
                                                  (if (and i (plusp (length i))) i "unidentified")))
                 ;; Never a plaintext link (ENGINEERING_CONTRACT, data in motion).
                 (make-snapshot-store :mode :off :off-reason
                                      "RE_SNAPSHOT_STORE=qdrant: needs an https URL; the snapshot store is off"))))
          (t (make-snapshot-store
              :mode :off
              :off-reason (format nil "RE_SNAPSHOT_STORE=~a is not memory, file:<dir>, qdrant:<url> or off; the snapshot store is off" v))))))

(defun snapshot-store-enabled-p (st) (not (eq (snps-mode st) :off)))
(defun snapshot-store-qdrant-p (st) (eq (snps-mode st) :qdrant))

(defun snapshot-id-p (id) (and (plusp (length id)) (every (lambda (c) (find c "0123456789abcdef")) id)))

(defun snapshot-qdrant (st method path &optional payload)
  "One Qdrant call: its `result`, or SNAPSHOT-STORE-UNAVAILABLE. Qdrant is not a
clock participant, so the call carries no RE-Clock and never moves the clock."
  (let ((res (handler-case
                 (let ((*client-clock-stamp* nil) (*client-clock-receive* nil))
                   (http-request-json (format nil "~a~a" (snps-url st) path) :method method :payload payload))
               (error (e) (error 'snapshot-store-unavailable :reason (format nil "Qdrant: ~a" e))))))
    (unless (and (hash-table-p res) (equal (jget res "status") "ok"))
      (let ((why (and (hash-table-p res) (hash-table-p (jget res "status")) (jget (jget res "status") "error"))))
        (error 'snapshot-store-unavailable :reason (format nil "Qdrant: ~a" (or why "an error")))))
    (jget res "result")))

(defun snapshot-qdrant-ready (st)
  (unless (snps-ready st)
    (let ((c (snps-collection st)))
      (unless (eq (jget (snapshot-qdrant st :get (format nil "/collections/~a/exists" c)) "exists") t)
        (snapshot-qdrant st :put (format nil "/collections/~a" c) (obj "sparse_vectors" (obj "isre" (obj))))
        (snapshot-qdrant st :put (format nil "/collections/~a/index?wait=true" c)
                         (obj "field_name" "instance" "field_schema" "keyword"))))
    (setf (snps-ready st) t)))

(defun snapshot-point-id (st id)
  (let ((h (sha256-hex (format nil "~a:~a" (snps-instance st) id))))
    (format nil "~a-~a-~a-~a-~a" (subseq h 0 8) (subseq h 8 12) (subseq h 12 16) (subseq h 16 20) (subseq h 20 32))))

(defun snapshot-instance-filter (st)
  (obj "must" (vector (obj "key" "instance" "match" (obj "value" (snps-instance st))))))

(defun snapshot-store-put (st id body)
  (case (snps-mode st)
    (:file (write-durable (merge-pathnames (format nil "~a.json" id) (snps-dir st)) (format nil "~a~%" (jcs body))))
    (:qdrant
     (snapshot-qdrant-ready st)
     (let ((k (jget body "kline")))
       (snapshot-qdrant st :put (format nil "/collections/~a/points?wait=true" (snps-collection st))
                        (obj "points" (vector (obj "id" (snapshot-point-id st id)
                                                   "vector" (obj "isre" (snapshot-sparse (snapshot-state-of body)))
                                                   "payload" (obj "instance" (snps-instance st) "id" id
                                                                  "digest" (jget body "digest") "scope" (jget k "scope")
                                                                  "span" (jget k "span")
                                                                  "machines" (jget (jget body "corpus") "machines")
                                                                  "lamport" (snapshot-lamport body)
                                                                  "body" (jcs body))))))))
    (t (setf (gethash id (snps-memory st)) body))))

(defun snapshot-store-get (st id)
  (when (snapshot-id-p id)
    (case (snps-mode st)
      (:file (let ((f (merge-pathnames (format nil "~a.json" id) (snps-dir st))))
               (and (probe-file f) (parse-json (uiop:read-file-string f)))))
      (:qdrant
       (snapshot-qdrant-ready st)
       (loop for pt across (or (snapshot-qdrant st :post (format nil "/collections/~a/points" (snps-collection st))
                                                (obj "ids" (vector (snapshot-point-id st id)) "with_payload" t))
                               #())
             for pl = (jget pt "payload")
             when (and (equal (jget pl "instance") (snps-instance st)) (equal (jget pl "id") id))
               return (parse-json (jget pl "body"))))
      (t (gethash id (snps-memory st))))))

(defun snapshot-store-remove (st id)
  (when (snapshot-store-get st id)
    (case (snps-mode st)
      (:file (delete-file (merge-pathnames (format nil "~a.json" id) (snps-dir st))))
      (:qdrant (snapshot-qdrant st :post (format nil "/collections/~a/points/delete?wait=true" (snps-collection st))
                                (obj "points" (vector (snapshot-point-id st id)))))
      (t (remhash id (snps-memory st))))
    t))

(defun snapshot-store-bodies (st)
  (case (snps-mode st)
    (:file (loop for f in (uiop:directory-files (snps-dir st) "*.json")
                 collect (parse-json (uiop:read-file-string f))))
    (:qdrant
     (snapshot-qdrant-ready st)
     (let ((offset nil) out)
       (loop
         (let* ((req (obj "filter" (snapshot-instance-filter st) "limit" 256 "with_payload" t "with_vector" +json-false+))
                (res (progn (when offset (setf (gethash "offset" req) offset))
                            (snapshot-qdrant st :post (format nil "/collections/~a/points/scroll" (snps-collection st)) req))))
           (loop for pt across (or (jget res "points") #())
                 do (push (parse-json (jget (jget pt "payload") "body")) out))
           (setf offset (jget res "next_page_offset"))
           (when (or (null offset) (eq offset +json-null+)) (return (nreverse out)))))))
    (t (loop for b being the hash-values of (snps-memory st) collect b))))

(defun snapshot-store-list (st)
  "{snapshots: [summary]}, oldest first by clocks.to.lamport, then id: every store alike."
  (let ((bodies (sort (snapshot-store-bodies st)
                      (lambda (a b) (if (/= (snapshot-lamport a) (snapshot-lamport b))
                                        (< (snapshot-lamport a) (snapshot-lamport b))
                                        (bytes< (snapshot-id a) (snapshot-id b)))))))
    (obj "snapshots" (vectorize (mapcar #'snapshot-summary bodies)))))

(defun snapshot-store-nearest (st query limit)
  "{snapshots: [{id, score, scope, span, machines}]}: score > 0, highest first, then id."
  (if (zerop (hash-table-count query))
      (obj "snapshots" #())
      (let ((candidates
              (if (snapshot-store-qdrant-p st)
                  ;; Qdrant's candidates, scored and ordered here (ties at the
                  ;; limit-th place are Qdrant's to break; SURFACE_SPEC "Nearest").
                  (progn
                    (snapshot-qdrant-ready st)
                    (loop for pt across (or (jget (snapshot-qdrant st :post (format nil "/collections/~a/points/query" (snps-collection st))
                                                                   (obj "query" (snapshot-sparse query) "using" "isre"
                                                                        "filter" (snapshot-instance-filter st) "limit" limit
                                                                        "with_payload" t))
                                                  "points")
                                            #())
                          collect (parse-json (jget (jget pt "payload") "body"))))
                  (snapshot-store-bodies st))))
        (snapshot-ranked (loop for b in candidates
                               for sc = (snapshot-score query (snapshot-state-of b))
                               when (> sc 0) collect (cons sc b))
                         limit))))
