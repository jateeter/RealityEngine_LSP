(in-package #:reality-engine-lsp)

;;; vector-aggregator — PE machine output aggregator
;;;
;;; Merges gated machine CES output vectors from RE's SimulationStep.machineResults
;;; into the base perceptual space vector to produce the next InputSpaceVector.
;;;
;;; Gating:      only machines whose transitionResult.arbiterMetadata.shouldOutput
;;;              is true contribute to the merge.
;;; Merge order: deterministic — records collected via maphash are sorted by
;;;              machineId string before writing (maphash order is not guaranteed).
;;;
;;; This is a thin, stateless function so the aggregation restriction (all machine
;;; outputs must be present before the next input vector is assembled) can be
;;; relaxed in the future without changing call sites.

(defun aggregate-machine-outputs (base-vector machine-results)
  "Merge gated machine CES output vectors from RE's machineResults into BASE-VECTOR.
   MACHINE-RESULTS is the machineResults hash-table from the SimulationStep response.
   Returns the merged nextInputSpaceVector as a list of numbers."
  (unless (jobject-p machine-results)
    (return-from aggregate-machine-outputs base-vector))

  ;; Collect (machineId offset length vec) records where shouldOutput = t
  (let ((records nil))
    (maphash
     (lambda (machine-id result)
       (let* ((transition  (jget result "transitionResult"))
              (arb-meta    (and (jobject-p transition)
                                (jget transition "arbiterMetadata")))
              (should-out  (and (jobject-p arb-meta)
                                (jbool arb-meta "shouldOutput" nil)))
              (out-region  (and should-out (jget result "outputRegion")))
              (out-vec-raw (and (jobject-p out-region)
                                (jget result "outputVector"))))
         (when (and should-out (jobject-p out-region) (jarray-p out-vec-raw))
           (let ((offset (jnumber out-region "offset" nil))
                 (length (jnumber out-region "length" nil))
                 (vec    (numbers-from-json out-vec-raw)))
             (when (and offset length vec (> length 0))
               (push (list machine-id (round offset) (round length) vec) records))))))
     machine-results)

    ;; Sort by machineId for deterministic merge order
    (setf records (sort records #'string< :key #'car))

    ;; Use a simple-vector for O(1) random write access
    (let* ((base-arr  (coerce base-vector 'simple-vector))
           (dim       (length base-arr)))
      (dolist (rec records)
        (destructuring-bind (_ offset length vec) rec
          (declare (ignore _))
          (let* ((vec-arr   (coerce vec 'simple-vector))
                 (write-len (min (length vec-arr) length)))
            (dotimes (i write-len)
              (let ((pos (+ offset i)))
                (when (< pos dim)
                  (setf (svref base-arr pos) (svref vec-arr i))))))))
      ;; Return as list — matches what update-from-perceptual-space expects
      (coerce base-arr 'list))))
