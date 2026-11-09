(in-package #:reality-engine-lsp)

;;; Cloneable machines and the clone space (RealityEngine_CI SURFACE_SPEC.md,
;;; "Cloneable machines"; RealityEngine_CI#713, localAssetStorageStack#8).
;;;
;;; Recognition is decided here, by the specification's nine-rule table;
;;; nothing in a machine file marks a machine cloneable. The clone space is a
;;; band of 64-cell blocks in the nest layout, allocated lowest free first, each
;;; with a generation the recycle route must present.

;; Fixed by the specification ("The clone space"); RealityEngine_Machines
;; domains/clone-space.json records the same values.
(defconstant +clone-band-offset+ 20480)
(defconstant +clone-band-length+ 65536)
(defconstant +clone-block-cells+ 64)
(defconstant +clone-quarter+ 16)
(defconstant +clone-blocks+ (/ +clone-band-length+ +clone-block-cells+)) ; 1024
(defconstant +clone-bus-cells+ 4)
(defconstant +clone-max-active-default+ 8)

;;; --- recognition ---------------------------------------------------------------

(defun %present-p (value)
  (and value (not (eq value +json-null+))))

(defun machine-edges (machine)
  (jarray-list (jget (machine-metadata machine) "interconnections")))

(defun producer-edge-p (edge)
  (and (jobject-p edge) (equal (jget edge "type") "published-domain-bus-producer")))

(defun machine-file-stem (machine)
  "The corpus file name without .json, or NIL for a machine with no file."
  (let ((file (machine-source-file machine)))
    (when (and file (plusp (length file)))
      (if (and (> (length file) 5) (string= ".json" file :start2 (- (length file) 5)))
          (subseq file 0 (- (length file) 5))
          file))))

(defun outgoing-handoff-p (edge stem)
  (and stem (jobject-p edge)
       (equal (jget edge "type") "document-workflow-handoff")
       (equal (jget edge "sourceMachine") stem)))

(defun bus-target-files (machines)
  "The file names other machines' published-domain-bus-producer edges target."
  (let ((out (make-hash-table :test #'equal)))
    (maphash (lambda (_ machine)
               (declare (ignore _))
               (dolist (edge (machine-edges machine))
                 (let ((target (and (producer-edge-p edge) (jget edge "targetMachineFile"))))
                   (when (and (stringp target) (plusp (length target)))
                     (setf (gethash target out) t)))))
             machines)
    out))

(defun assess-cloneable (machine bus-targets)
  "The nine-rule table, in order. Returns (values cloneable-p reason n folded):
REASON is the first failing rule's code (NIL when cloneable), N the folded
fanout cells plus the bus cells whenever rule 9 was evaluated (else NIL)."
  (let* ((metadata (machine-metadata machine))
         (stem (machine-file-stem machine))
         (producers 0) (handoffs 0) (folded 0))
    (dolist (edge (machine-edges machine))
      (cond ((producer-edge-p edge)
             (incf producers)
             (incf folded (length (jarray-list (jget edge "targetInputElements")))))
            ((outgoing-handoff-p edge stem) (incf handoffs))))
    (flet ((fail (reason) (return-from assess-cloneable (values nil reason nil 0))))
      ;; 1. It dispatches an agentic action.
      (unless (or (%present-p (jget metadata "agentBinding")) (plusp handoffs)) (fail "no-agentic-action"))
      ;; 2. It has fanout.
      (unless (or (plusp producers) (plusp handoffs)) (fail "no-fanout"))
      ;; 3. It is not a bus target.
      (when (and (machine-source-file machine) (gethash (machine-source-file machine) bus-targets))
        (fail "bus-target"))
      ;; 4. Not life-safety.
      (when (equal (jget metadata "severity") "life-safety") (fail "life-safety"))
      ;; 5. Not a fixture (string tags only).
      (when (some (lambda (tag) (and (stringp tag) (search "fixture" tag)))
                  (jarray-list (jget metadata "tags")))
        (fail "fixture"))
      ;; 6. Not opted out.
      (when (eq (jget metadata "cloneable") +json-false+) (fail "opted-out"))
      ;; 7, 8. Its regions fit their quarters; no mapping fails rule 7.
      (let ((mapping (machine-mapping machine)))
        (when (or (null mapping) (> (region-length (mapping-input mapping)) +clone-quarter+))
          (fail "input-too-long"))
        (when (> (region-length (mapping-output mapping)) +clone-quarter+)
          (fail "output-too-long")))
      ;; 9. The cells its fanout folds into fit the fanout quarter.
      (let ((n (+ folded +clone-bus-cells+)))
        (if (> folded +clone-quarter+)
            (values nil "fanout-too-wide" n folded)
            (values t nil n folded))))))

(defun cloneable-listing-json (machines)
  "GET /api/machines/cloneable: every registered machine except clones, sorted
by id, with its verdict."
  (let ((targets (bus-target-files machines))
        (ids nil))
    (maphash (lambda (id machine) (unless (machine-clone-of machine) (push id ids))) machines)
    (obj "machines"
         (vectorize
          (mapcar (lambda (id)
                    (multiple-value-bind (ok reason n) (assess-cloneable (gethash id machines) targets)
                      (obj "id" id
                           "cloneable" (json-bool ok)
                           "reason" (or reason +json-null+)
                           "n" (or n +json-null+))))
                  (sort ids #'string<))))))

;;; --- the clone space -------------------------------------------------------------

(defstruct clone
  clone-id machine-id engine-instance envelope-id asset-id
  block generation input-length output-length fanout-length)

(defstruct (clone-space (:constructor %make-clone-space))
  (generations (make-array +clone-blocks+ :initial-element 0))
  (by-block (make-hash-table :test #'eql))
  (max-active +clone-max-active-default+))

(defun make-clone-space-state () (%make-clone-space))

(defun clone-block-offset (clone)
  (+ +clone-band-offset+ (* +clone-block-cells+ (clone-block clone))))

(defun %region (offset length) (make-region :offset offset :length length))
(defun clone-input-region (c) (%region (clone-block-offset c) (clone-input-length c)))
(defun clone-output-region (c) (%region (+ (clone-block-offset c) +clone-quarter+) (clone-output-length c)))
(defun clone-fanout-region (c) (%region (+ (clone-block-offset c) (* 2 +clone-quarter+)) (clone-fanout-length c)))
(defun clone-bus-region (c) (%region (+ (clone-block-offset c) (* 3 +clone-quarter+)) +clone-bus-cells+))

(defun %region-obj (r) (obj "offset" (region-offset r) "length" (region-length r)))

(defun clone-block-json (c)
  (obj "index" (clone-block c) "offset" (clone-block-offset c) "length" +clone-block-cells+))

(defun clone-json (c)
  "The 201 body, and each entry of GET /api/engine/clones."
  (obj "cloneId" (clone-clone-id c)
       "machineId" (clone-machine-id c)
       "engineInstance" (clone-engine-instance c)
       "envelopeId" (clone-envelope-id c)
       "assetId" (clone-asset-id c)
       "block" (clone-block-json c)
       "input" (%region-obj (clone-input-region c))
       "output" (%region-obj (clone-output-region c))
       "fanout" (%region-obj (clone-fanout-region c))
       "bus" (%region-obj (clone-bus-region c))
       "generation" (clone-generation c)))

(defun clone-space-live (space)
  "Live clones in block order."
  (let ((out nil))
    (maphash (lambda (_ c) (declare (ignore _)) (push c out)) (clone-space-by-block space))
    (sort out #'< :key #'clone-block)))

(defun clone-space-lowest-free (space)
  (loop for k below +clone-blocks+
        unless (gethash k (clone-space-by-block space)) return k))

(defun clone-space-live-of (space machine-id)
  (count machine-id (clone-space-live space) :key #'clone-machine-id :test #'equal))

(defun clone-space-find (space key value)
  (find value (clone-space-live space) :key key :test #'equal))

(defun clone-space-generation (space block)
  (aref (clone-space-generations space) block))

(defun clone-space-add (space clone)
  (when (gethash (clone-block clone) (clone-space-by-block space))
    (error "clone block ~d already held" (clone-block clone)))
  (setf (gethash (clone-block clone) (clone-space-by-block space)) clone))

(defun clone-space-recycle (space clone-id)
  "Frees the clone's block and increments its generation; returns the new one."
  (let ((c (clone-space-find space #'clone-clone-id clone-id)))
    (unless c (error "no live clone ~a" clone-id))
    (remhash (clone-block c) (clone-space-by-block space))
    (incf (aref (clone-space-generations space) (clone-block c)))))

;;; --- making a clone ------------------------------------------------------------------

(defun copy-machine-deep (machine)
  "A machine that shares no mutable state with MACHINE: its own metadata table,
sequences and Reality Events. COPY-MACHINE alone would leave the clone and its
template stepping one set of events."
  (let ((copy (copy-machine machine))
        (sequences (make-hash-table :test #'equal)))
    (when (hash-table-p (machine-metadata machine))
      (let ((meta (make-hash-table :test #'equal)))
        (maphash (lambda (k v) (setf (gethash k meta) v)) (machine-metadata machine))
        (setf (machine-metadata copy) meta)))
    (when (machine-mapping machine)
      (let ((m (machine-mapping machine)))
        (setf (machine-mapping copy)
              (make-mapping :input (copy-region (mapping-input m))
                            :output (copy-region (mapping-output m))
                            :bits-per-element (mapping-bits-per-element m)
                            :output-alphabet-top (mapping-output-alphabet-top m)))))
    (maphash (lambda (id sequence)
               (let ((s (copy-ces sequence))
                     (vectors (make-hash-table :test #'equal)))
                 (maphash (lambda (vid v)
                            (let ((e (copy-reality-event v)))
                              (setf (reality-event-predecessor-chain e)
                                    (copy-list (reality-event-predecessor-chain v)))
                              (setf (gethash vid vectors) e)))
                          (sequence-vectors sequence))
                 (setf (sequence-vectors s) vectors)
                 (setf (gethash id sequences) s)))
             (machine-sequences machine))
    (setf (machine-sequences copy) sequences)
    copy))

(defun make-clone-machine (template clone)
  "The template as declared, at its initial state, with its regions moved into
CLONE's block."
  (let ((m (copy-machine-deep template)))
    (maphash (lambda (_ s) (declare (ignore _)) (reset-sequence s)) (machine-sequences m))
    (setf (machine-id m) (clone-clone-id clone)
          (machine-source-file m) nil
          (machine-clone-of m) (clone-machine-id clone)
          (machine-asset-id m) (clone-asset-id clone)
          (machine-envelope-id m) (clone-envelope-id clone)
          (machine-transitions-inhibited m) nil
          (mapping-input (machine-mapping m)) (clone-input-region clone)
          (mapping-output (machine-mapping m)) (clone-output-region clone))
    m))
