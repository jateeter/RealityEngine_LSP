(in-package #:reality-engine-lsp)

;;; vector-aggregator — PE machine output aggregator
;;;
;;; Merges gated machine CES output vectors from RE's SimulationStep.machineResults
;;; into the base perceptual space vector to produce the next InputSpaceVector.
;;;
;;; Gating:      only machines whose transitionResult.arbiterMetadata.shouldOutput
;;;              is true contribute to the merge.
;;; Merge order: deterministic *and* identical across runtimes — records are
;;;              sorted by machineName, which the corpus declares and which is
;;;              globally unique across it.
;;;
;;;              Sorting by machineId was deterministic within one runtime and
;;;              different between them: the corpus declares no id, so each
;;;              runtime mints its own (machine-1787…, machine-1U358SX…). Where
;;;              two machines' output regions overlap the merge is last-writer-
;;;              wins, so the winner was decided by an id that differs per
;;;              engine, and the merged vector — the next InputSpaceVector —
;;;              diverged. Seen on AgHarvestReadinessAssessor, whose output
;;;              [3967:3971] overlaps AGX055's [3959:3971]: ISRE cell 3968 read
;;;              1.0 on C++/LSP and 0.0 on Scala while every OREV agreed
;;;              (RealityEngine_CI corpus parity sweep, 2026-08-19).
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

  ;; Collect (sort-key offset length vec) records where shouldOutput = t
  (let ((records nil))
    (maphash
     (lambda (machine-id result)
       (let* ((transition  (jget result "transitionResult"))
              (arb-meta    (and (jobject-p transition)
                                (jget transition "arbiterMetadata")))
              (should-out  (and (jobject-p arb-meta)
                                (jbool arb-meta "shouldOutput" nil)))
              (out-region  (and should-out (jget result "outputRegion")))
              ;; Prefer mergedOutputVector: the machine's collection of
              ;; potential outputs folded under its own
              ;; outputMergeTransformation. Presenting that fold is the Reality
              ;; Engine's job and the last thing it does in the step, so it is
              ;; the machine's actual output.
              ;;
              ;; outputVector is one arbitrarily chosen member of that
              ;; collection, and which member differed per runtime — reading it
              ;; here is what carried the RE's disagreement into the perceptual
              ;; space (RealityEngine_CI#154). Falling back to it keeps this
              ;; working against a Reality Engine not yet updated.
              (out-vec-raw (and (jobject-p out-region)
                                (let ((merged (jget result "mergedOutputVector")))
                                  (if (jarray-p merged)
                                      merged
                                      (jget result "outputVector"))))))
         (when (and should-out (jobject-p out-region) (jarray-p out-vec-raw))
           (let ((offset (jnumber out-region "offset" nil))
                 (length (jnumber out-region "length" nil))
                 (vec    (numbers-from-json out-vec-raw)))
             (when (and offset length vec (> length 0))
               ;; machineName is the corpus-declared handle and is stable across
               ;; runtimes; machine-id is minted locally. Fall back to the id
               ;; only if a result carries no name, which keeps a malformed
               ;; payload ordered rather than unordered.
               (let ((sort-key (or (jstring result "machineName" nil) machine-id)))
                 (push (list sort-key (round offset) (round length) vec) records)))))))
     machine-results)

    ;; Sort by machineName — deterministic and identical on every runtime.
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
