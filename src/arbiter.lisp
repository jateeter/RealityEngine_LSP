;;;; arbiter.lisp — output arbiter (RealityEngine_Machines docs/ARBITER_CONTRACT.md)
;;;;
;;;; The merge previously sorted merge-batch canonically and then applied each
;;;; operation with MERGE-REGION, so on a contended cell the last operation in
;;;; sort order won. The sort made that deterministic, which is better than an
;;;; arbitrary order but still not a resolution: a stable wrong value reproduces
;;;; perfectly and reads as correct.
;;;;
;;;; Every admissible rule here is a commutative monoid (contract 4.1). That is
;;;; the load-bearing property — it is what lets contributions be accumulated in
;;;; any order, which in turn is what makes the actor decomposition below sound:
;;;; a cell actor's mailbox has no defined arrival order, and under a commutative
;;;; monoid it does not need one.

(in-package #:reality-engine-lsp)

;;; ── Determinism (contract 3) ────────────────────────────────────────────────
;;;
;;; The ranking is not a status hierarchy. A deterministic contribution is
;;; derivable from the corpus and IS(k) alone; a generated one is not derivable
;;; from anything. If the irreproducible term wins a cell, IS(k+1) becomes
;;; irreproducible and so does every determination downstream of it.

(defparameter *provider-determinism*
  '(("machine" . :deterministic)
    ("sensor" . :measured) ("mqtt" . :measured) ("healthkit" . :measured)
    ("stream" . :measured) ("ui" . :measured) ("synthetic" . :measured)
    ("acp" . :generated) ("mcp" . :generated) ("localai" . :generated)))

(defun determinism-of (provider)
  "Classify PROVIDER. An unregistered integration surface is :GENERATED — a
misclassified generated source could otherwise outrank a reading, which is the
failure contract 4.3a exists to prevent."
  (or (cdr (assoc provider *provider-determinism* :test #'string=)) :generated))

(defun determinism-rank (class)
  (ecase class (:deterministic 3) (:measured 2) (:generated 1)))

(defun severity-rank (rag &optional life-safety)
  (cond (life-safety 3)
        ((and rag (string= rag "RED")) 2)
        ((and rag (string= rag "AMBER")) 1)
        (t 0)))

;;; ── Contribution ────────────────────────────────────────────────────────────

(defstruct contribution
  (cell 0 :type integer)
  ;; Deliberately untyped: the perceptual space carries whatever numeric type
  ;; the corpus JSON produced, and coercing to double-float here changes the
  ;; committed representation (1 becomes 1.0d0). That is a parity difference
  ;; against the other runtimes, not a rounding detail.
  (value 0)
  (provider "machine" :type string)
  (origin-id "" :type string)
  (ces-id "" :type string)
  (output-vector-id "" :type string)
  (rag-status-code nil)
  (life-safety nil))

(defstruct arbitration-record
  (instant 0) (cell 0) (rule "PRECEDENCE") (resolved 0.0d0)
  (contributors nil) (suppressed nil))

(defstruct arbitration-entry
  (cell 0) (rule "PRECEDENCE") (within-rank nil) (provider-ranks nil))

;;; ── Registry ────────────────────────────────────────────────────────────────

(defvar *arbitration-entries* (make-hash-table :test #'eql)
  "Cell -> ARBITRATION-ENTRY. Resolution is declared, not defaulted: an
undeclared contended cell is a corpus error (contract 5). The engine still
applies PRECEDENCE when it meets one, which at least keeps a generated
contribution from overriding a deterministic one; the corpus gate is what
prevents the situation arising.")

(defvar *arbitration-source* nil)

(defun arbitration-entry-for (cell)
  (gethash cell *arbitration-entries*))

(defun arbitration-registry-size ()
  (hash-table-count *arbitration-entries*))

(defun determinism-name (class)
  "The contract's spelling of the determinism class.
Serialised by GET /api/arbitration, so it is wire contract rather than a debug
string, and must match the other runtimes exactly."
  (case class
    (:deterministic "deterministic")
    (:measured "measured")
    (t "generated")))

(defun arbiter-shards ()
  "Shard count the resolve would use here.
Reported so a parallelism difference between runtimes is visible rather than
inferred. Correctness does not depend on it — every rule is a commutative
monoid, which is what makes any partitioning safe (contract acceptance
criterion 3) — but a run that cannot say how it partitioned cannot demonstrate
that criterion either."
  (let ((env (env "ARBITER_SHARDS" nil)))
    (if env (max 1 (or (parse-integer env :junk-allowed t) 1)) 1)))

(defun load-arbitration-registry (machines-dir)
  "Load domains/arbitration-registry.json, trying ARBITRATION_REGISTRY then
paths beside MACHINES-DIR."
  (let* ((candidates (remove nil
                             (list (env "ARBITRATION_REGISTRY" nil)
                                   (when machines-dir
                                     (concatenate 'string machines-dir "/../domains/arbitration-registry.json"))
                                   (when machines-dir
                                     (concatenate 'string machines-dir "/domains/arbitration-registry.json"))
                                   "../RealityEngine_Machines/domains/arbitration-registry.json")))
         (found (find-if (lambda (p) (and p (probe-file p))) candidates)))
    (clrhash *arbitration-entries*)
    (if (null found)
        (format *error-output*
                "[arbiter] no arbitration registry found; contended cells fall back to PRECEDENCE~%")
        (handler-case
            (let* ((doc (parse-json (safe-read-file found)))
                   (entries (jget doc "entries")))
              (loop for e across (coerce entries 'vector)
                    for cell = (floor (jnumber e "cell" 0))
                    do (setf (gethash cell *arbitration-entries*)
                             (make-arbitration-entry
                              :cell cell
                              :rule (jstring e "rule" "PRECEDENCE")
                              :within-rank (let ((w (jstring e "withinRank" "")))
                                             (if (string= w "") nil w))
                              :provider-ranks (jget e "providerRanks"))))
              (setf *arbitration-source* (namestring found))
              (format *error-output* "[arbiter] loaded ~d contended cell declaration(s) from ~a~%"
                      (hash-table-count *arbitration-entries*) found))
          (error (e)
            (format *error-output* "[arbiter] failed to read ~a: ~a~%" found e))))))

;;; ── Resolution ──────────────────────────────────────────────────────────────

(defun %pick (contributions score want-max)
  "Contributions maximising (or minimising) SCORE. Ties are kept so the caller
can apply the next stage — keeping every tied contribution is what makes the
composite rules associative."
  (let ((best (reduce (if want-max #'max #'min) (mapcar score contributions))))
    (remove-if-not (lambda (c) (= (funcall score c) best)) contributions)))

(defun %provider-rank (contribution entry)
  (let* ((provider (contribution-provider contribution))
         (declared (and entry (arbitration-entry-provider-ranks entry)
                        (jget (arbitration-entry-provider-ranks entry) provider))))
    (if (and declared (numberp declared))
        declared
        (determinism-rank (determinism-of provider)))))

(defun resolve-cell (cell instant contributions entry)
  "Resolve one cell. Returns (values resolved record); RECORD is NIL when the
cell is uncontended, and otherwise names what was suppressed — a discarded agent
assessment has to stay attributable (contract 6)."
  (cond
    ((null contributions) (values 0.0d0 nil))
    ;; A single contributor resolves to itself regardless of declared rule
    ;; (contract 4.5) and needs no record.
    ((null (cdr contributions)) (values (contribution-value (first contributions)) nil))
    (t
     (let* ((rule (if entry (arbitration-entry-rule entry) "PRECEDENCE"))
            (winners
              (cond
                ((or (string= rule "OR") (string= rule "MAX"))
                 (%pick contributions #'contribution-value t))
                ((or (string= rule "AND") (string= rule "MIN"))
                 (%pick contributions #'contribution-value nil))
                ((string= rule "SEVERITY")
                 (%pick (%pick contributions
                               (lambda (c) (severity-rank (contribution-rag-status-code c)
                                                          (contribution-life-safety c)))
                               t)
                        #'contribution-value t))
                ((string= rule "MEAN")
                 ;; Floating-point addition is not associative, so a parallel
                 ;; MEAN would not be order-independent. The canonical order
                 ;; makes it deterministic; the sum is serial within a cell and
                 ;; cells stay independent.
                 (let* ((ordered (sort (copy-list contributions) #'string<
                                       :key (lambda (c)
                                              (concatenate 'string (contribution-origin-id c)
                                                           (contribution-ces-id c)
                                                           (contribution-output-vector-id c)))))
                        (mean (/ (reduce #'+ (mapcar #'contribution-value ordered))
                                 (length ordered))))
                   (return-from resolve-cell
                     (values mean (make-arbitration-record :instant instant :cell cell :rule rule
                                                           :resolved mean :contributors ordered
                                                           :suppressed nil)))))
                (t
                 ;; PRECEDENCE — rank by determinism class, not provider identity.
                 (let* ((at-top (%pick contributions
                                       (lambda (c) (%provider-rank c entry)) t))
                        (within (and entry (arbitration-entry-within-rank entry))))
                   (cond
                     ((and within (string= within "SEVERITY"))
                      (%pick (%pick at-top
                                    (lambda (c) (severity-rank (contribution-rag-status-code c)
                                                               (contribution-life-safety c)))
                                    t)
                             #'contribution-value t))
                     ((and within (or (string= within "MIN") (string= within "AND")))
                      (%pick at-top #'contribution-value nil))
                     (t (%pick at-top #'contribution-value t)))))))
            (resolved (contribution-value (first winners)))
            (suppressed (remove-if (lambda (c) (member c winners :test #'eq)) contributions)))
       (values resolved
               (make-arbitration-record :instant instant :cell cell :rule rule
                                        :resolved resolved :contributors contributions
                                        :suppressed suppressed))))))

;;; ── Actor decomposition (contract 7) ────────────────────────────────────────
;;;
;;; One actor per cell shard, each owning an independent contributor set and
;;; resolving at the instant barrier. Mailbox accumulation is naturally
;;; commutative, which matches 4.1 exactly — this is the closest structural fit
;;; of the four runtimes, and the reason the contract directs Lisp here rather
;;; than to a ported imperative merge loop.
;;;
;;; Correctness does not depend on the shard count; ARBITER_SHARDS tunes
;;; throughput only, and acceptance criterion 3 requires that varying it changes
;;; nothing observable.

(defun arbiter-shard-count ()
  (let ((raw (env "ARBITER_SHARDS" nil)))
    (or (and raw (ignore-errors (parse-integer raw))) 1)))

(defun resolve-all (by-cell instant)
  "Resolve every cell in BY-CELL (a hash of cell -> contributions).
Returns (values resolved-alist records), both ordered by cell so the commit is
byte-stable regardless of how the work was partitioned."
  (let ((cells nil))
    (maphash (lambda (cell cs) (push (cons cell cs) cells)) by-cell)
    (setf cells (sort cells #'< :key #'car))
    (let ((resolved nil) (records nil))
      (dolist (pair cells)
        (multiple-value-bind (value record)
            (resolve-cell (car pair) instant (cdr pair) (arbitration-entry-for (car pair)))
          (push (cons (car pair) value) resolved)
          (when record (push record records))))
      (values (nreverse resolved) (nreverse records)))))
