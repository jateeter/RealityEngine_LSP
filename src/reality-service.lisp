(in-package #:reality-engine-lsp)

(defstruct reality-state
  dimension machines machine-dir perceptual-space history history-limit include-machine-results-p
  include-perceptual-space-p vector-store sequences qdrant-url collection-name started-at
  event-bus-subscriptions latched-event-bits step-count mapping-version)

(defun compose-key (producer-machine-id producer-sequence-id)
  (format nil "~a|~a" producer-machine-id producer-sequence-id))

(defun ensure-space-length (state length)
  (when (> length (length (reality-state-perceptual-space state)))
    (setf (reality-state-perceptual-space state)
          (append (reality-state-perceptual-space state)
                  (make-list (- length (length (reality-state-perceptual-space state)))
                             :initial-element 0.0d0))))
  (when (> length (reality-state-dimension state))
    (setf (reality-state-dimension state) length)))

(defun register-compose-subscriptions (state machine)
  (let* ((compose (jget (machine-metadata machine) "compose"))
         (subscriptions (and (jobject-p compose) (jget compose "subscriptions"))))
    (when (jarray-p subscriptions)
      (dolist (sub (jarray-list subscriptions))
        (let ((producer-machine-id (jstring sub "producerMachineId" nil))
              (producer-sequence-id (jstring sub "producerSequenceId" nil))
              (bit-offset (jnumber sub "bitOffset" nil)))
          (when (and producer-machine-id producer-sequence-id bit-offset (>= bit-offset 0))
            (let* ((bit (truncate bit-offset))
                   (key (compose-key producer-machine-id producer-sequence-id))
                   (row (obj "producerMachineId" producer-machine-id
                             "producerSequenceId" producer-sequence-id
                             "subscriberMachineId" (machine-id machine)
                             "bitOffset" bit)))
              (push row (gethash key (reality-state-event-bus-subscriptions state)))
              (ensure-space-length state (1+ bit)))))))))

(defun unregister-compose-subscriptions (state machine-id)
  (let ((updates nil)
        (deletes nil))
    (maphash
     (lambda (key rows)
       (let ((remaining (remove-if (lambda (row)
                                     (string= (jstring row "subscriberMachineId" "") machine-id))
                                   rows)))
         (if remaining
             (push (cons key remaining) updates)
             (push key deletes))))
     (reality-state-event-bus-subscriptions state))
    (dolist (update updates)
      (setf (gethash (car update) (reality-state-event-bus-subscriptions state))
            (cdr update)))
    (dolist (key deletes)
      (remhash key (reality-state-event-bus-subscriptions state)))))

(defun event-bus-subscription-count (state)
  (let ((count 0))
    (maphash (lambda (_ rows)
               (declare (ignore _))
               (incf count (length rows)))
             (reality-state-event-bus-subscriptions state))
    count))

(defun put-machine (state machine)
  (unregister-compose-subscriptions state (machine-id machine))
  (setf (gethash (machine-id machine) (reality-state-machines state)) machine)
  (incf (reality-state-mapping-version state))
  (when (machine-mapping machine)
    (ensure-space-length state (max (+ (region-offset (mapping-input (machine-mapping machine)))
                                      (region-length (mapping-input (machine-mapping machine))))
                                   (+ (region-offset (mapping-output (machine-mapping machine)))
                                      (region-length (mapping-output (machine-mapping machine)))))))
  (register-compose-subscriptions state machine)
  machine)

(defun make-reality-state-from-config (&key machine-dir dimension)
  (let ((state
          (make-reality-state
           :dimension dimension
           :machines (make-hash-table :test #'equal)
           :machine-dir machine-dir
           :perceptual-space (make-list dimension :initial-element 0.0d0)
           :history nil
           :history-limit (env-int "RE_HISTORY_LIMIT" 250)
           :include-machine-results-p (env-bool "RE_INCLUDE_MACHINE_RESULTS" t)
           :include-perceptual-space-p (env-bool "RE_INCLUDE_PERCEPTUAL_SPACE" t)
           :vector-store (make-hash-table :test #'equal)
           :sequences (make-hash-table :test #'equal)
           :qdrant-url (env "QDRANT_URL" "http://localhost:4333")
           :collection-name (env "QDRANT_REALITY_COLLECTION" "reality-vectors")
           :started-at (now-ms)
           :event-bus-subscriptions (make-hash-table :test #'equal)
           :latched-event-bits (make-hash-table :test #'equal)
           :step-count 0
           :mapping-version 0)))
    (dolist (machine (load-machines-from-directory machine-dir))
      (put-machine state machine))
    state))

(defun record-history (state item)
  (push item (reality-state-history state))
  (when (> (length (reality-state-history state)) (reality-state-history-limit state))
    (setf (reality-state-history state)
          (subseq (reality-state-history state) 0 (reality-state-history-limit state)))))

(defun required-dimension (state)
  (let ((required (reality-state-dimension state)))
    (maphash
     (lambda (_ machine)
       (declare (ignore _))
       (when (machine-mapping machine)
         (let* ((mapping (machine-mapping machine))
                (input-end (+ (region-offset (mapping-input mapping))
                              (region-length (mapping-input mapping))))
                (output-end (+ (region-offset (mapping-output mapping))
                               (region-length (mapping-output mapping)))))
           (setf required (max required input-end output-end)))))
     (reality-state-machines state))
    required))

(defun stats-json (state)
  (let ((machine-count (hash-table-count (reality-state-machines state)))
        (sequence-count 0)
        (vector-count 0))
    (maphash
     (lambda (_ machine)
       (declare (ignore _))
       (incf sequence-count (hash-table-count (machine-sequences machine)))
       (dolist (sequence (machine-sequence-list machine))
         (incf vector-count (hash-table-count (sequence-vectors sequence)))))
     (reality-state-machines state))
    (obj "machineCount" machine-count
         "sequenceCount" sequence-count
         "vectorCount" vector-count
         "dimension" (reality-state-dimension state)
         "requiredDimension" (required-dimension state)
         "historySize" (length (reality-state-history state))
         "mappingVersion" (reality-state-mapping-version state)
         "eventBusSubscriptionCount" (event-bus-subscription-count state)
         "latchedEventBitCount" (hash-table-count (reality-state-latched-event-bits state)))))

(defun reset-reality-state (state)
  (maphash (lambda (_ machine)
             (declare (ignore _))
             (dolist (sequence (machine-sequence-list machine))
               (reset-sequence sequence)))
           (reality-state-machines state))
  (setf (reality-state-perceptual-space state)
        (make-list (reality-state-dimension state) :initial-element 0.0d0)
        (reality-state-history state) nil
        (reality-state-latched-event-bits state) (make-hash-table :test #'equal)
        (reality-state-step-count state) 0)
  state)

(defun assemble-input-vector (state body)
  (cond
    ((jarray-p (jget body "vector"))
     (numbers-from-json (jget body "vector")))
    ((jarray-p (jget body "sparseVector"))
     (let ((length (reality-state-dimension state)))
       (dolist (entry (jarray-list (jget body "sparseVector")))
         (setf length (max length (1+ (truncate (jnumber entry "index" 0))))))
       (let ((values (make-list length :initial-element 0.0d0)))
         (dolist (entry (jarray-list (jget body "sparseVector")))
           (setf (nth (truncate (jnumber entry "index" 0)) values)
                 (or (jnumber entry "value" 0.0d0) 0.0d0)))
         values)))
    ((jarray-p (jget body "domainVectors"))
     (let ((length (reality-state-dimension state)))
       (dolist (entry (jarray-list (jget body "domainVectors")))
         (let ((offset (truncate (jnumber entry "offset" 0)))
               (values (numbers-from-json (jget entry "values"))))
           (setf length (max length (+ offset (length values))))))
       (let ((out (make-list length :initial-element 0.0d0)))
         (dolist (entry (jarray-list (jget body "domainVectors")))
           (loop for value in (numbers-from-json (jget entry "values"))
                 for i from (truncate (jnumber entry "offset" 0))
                 do (setf (nth i out) value)))
         out)))
    (t nil)))

(defun sequence-by-id (machine sequence-id)
  (gethash sequence-id (machine-sequences machine)))

(defun parse-comma-values (value)
  (remove nil
          (mapcar (lambda (part)
                    (handler-case
                        (let ((parsed (read-from-string part)))
                          (when (numberp parsed) parsed))
                      (error () nil)))
                  (split-string (or value "") #\,))))

(defun compact-query-p (query)
  (let ((value (and (hash-table-p* query) (gethash "compact" query))))
    (and value (member value '("true" "1") :test #'string=))))

(defparameter +cell-packing-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun allowed-bits-per-element-p (bits)
  (member bits '(1 2 4 8)))

(defun cell-integer (value bits index)
  (let ((max-value (1- (ash 1 bits))))
    (unless (and (realp value)
                 (<= 0 value)
                 (<= value max-value)
                 (= value (truncate value)))
      (error "cell[~a]=~a does not fit in ~a-bit cell (range 0..~a)"
             index value bits max-value))
    (truncate value)))

(defun validate-cell-range (values bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (loop for value in values
        for index from 0
        collect (cell-integer value bits index)))

(defun pack-cells (values bits)
  (let* ((cells (validate-cell-range values bits))
         (bytes (make-array (ceiling (* (length cells) bits) 8)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (if (= bits 8)
        (loop for cell in cells
              for index from 0
              do (setf (aref bytes index) cell))
        (let ((mask (1- (ash 1 bits))))
          (loop for cell in cells
                for index from 0
                for bit-index = (* index bits)
                for byte-index = (floor bit-index 8)
                for shift = (- 8 bits (mod bit-index 8))
                do (setf (aref bytes byte-index)
                         (logior (aref bytes byte-index)
                                 (ash (logand cell mask) shift))))))
    bytes))

(defun unpack-cells (bytes length bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (let ((required (ceiling (* length bits) 8)))
    (when (< (length bytes) required)
      (error "bytes too small for packed cell payload")))
  (if (= bits 8)
      (loop for index below length collect (aref bytes index))
      (let ((mask (1- (ash 1 bits))))
        (loop for index below length
              for bit-index = (* index bits)
              for byte-index = (floor bit-index 8)
              for shift = (- 8 bits (mod bit-index 8))
              collect (logand (ash (aref bytes byte-index) (- shift)) mask)))))

(defun encode-packed-base64 (values bits)
  (let ((bytes (pack-cells values bits)))
    (with-output-to-string (stream)
      (loop for index from 0 below (length bytes) by 3
            for b0 = (aref bytes index)
            for b1-present = (< (1+ index) (length bytes))
            for b2-present = (< (+ index 2) (length bytes))
            for b1 = (if b1-present (aref bytes (1+ index)) 0)
            for b2 = (if b2-present (aref bytes (+ index 2)) 0)
            for triple = (logior (ash b0 16) (ash b1 8) b2)
            do (progn
                 (write-char (char +cell-packing-base64-alphabet+
                                   (ldb (byte 6 18) triple))
                             stream)
                 (write-char (char +cell-packing-base64-alphabet+
                                   (ldb (byte 6 12) triple))
                             stream)
                 (write-char (if b1-present
                                 (char +cell-packing-base64-alphabet+
                                       (ldb (byte 6 6) triple))
                                 #\=)
                             stream)
                 (write-char (if b2-present
                                 (char +cell-packing-base64-alphabet+
                                       (ldb (byte 6 0) triple))
                                 #\=)
                             stream))))))

(defun storage-footprint (length bits)
  (unless (allowed-bits-per-element-p bits)
    (error "bitsPerElement must be one of 1, 2, 4, 8"))
  (let* ((float64-bytes (* length 8))
         (packed-bytes (ceiling (* length bits) 8))
         (shrink-factor (if (zerop packed-bytes) 0.0d0 (/ float64-bytes packed-bytes))))
    (obj "float64Bytes" float64-bytes
         "packedBytes" packed-bytes
         "shrinkFactor" shrink-factor)))

(defun machine-bits-per-element (machine)
  (let ((bits (and (machine-mapping machine)
                   (mapping-bits-per-element (machine-mapping machine)))))
    (if (allowed-bits-per-element-p bits) bits 8)))

(defun values-packed-json (values bits)
  (let ((out (obj "bitsPerElement" bits
                  "length" (length values))))
    (handler-case
        (setf (jget out "base64") (encode-packed-base64 values bits))
      (error (e)
        (setf (jget out "error") (princ-to-string e))))
    out))

(defun add-packed-merge-values (state merge-batch)
  (dolist (operation merge-batch)
    (let* ((machine (gethash (jstring operation "machineId" "")
                             (reality-state-machines state)))
           (bits (if machine (machine-bits-per-element machine) 8))
           (values (numbers-from-json (jget operation "values"))))
      (setf (jget operation "valuesPacked") (values-packed-json values bits))))
  merge-batch)

(defun storage-footprint-json (state)
  (let ((per-machine nil)
        (width-histogram (obj "1" 0 "2" 0 "4" 0 "8" 0))
        (total-cells 0)
        (total-float64-bytes 0)
        (total-packed-bytes 0))
    (dolist (machine (object-values-sorted (reality-state-machines state)))
      (when (machine-mapping machine)
        (let* ((mapping (machine-mapping machine))
               (bits (machine-bits-per-element machine))
               (cells (+ (region-length (mapping-input mapping))
                         (region-length (mapping-output mapping))))
               (footprint (storage-footprint cells bits)))
          (incf total-cells cells)
          (incf total-float64-bytes (jnumber footprint "float64Bytes" 0))
          (incf total-packed-bytes (jnumber footprint "packedBytes" 0))
          (setf (jget width-histogram (write-to-string bits))
                (1+ (or (jnumber width-histogram (write-to-string bits) 0) 0)))
          (push (obj "machineId" (machine-id machine)
                     "machineName" (machine-name machine)
                     "bitsPerElement" bits
                     "inputCells" (region-length (mapping-input mapping))
                     "outputCells" (region-length (mapping-output mapping))
                     "float64Bytes" (jnumber footprint "float64Bytes" 0)
                     "packedBytes" (jnumber footprint "packedBytes" 0)
                     "shrinkFactor" (jnumber footprint "shrinkFactor" 0))
                per-machine))))
    (obj "machinesRegistered" (hash-table-count (reality-state-machines state))
         "totalCells" total-cells
         "totalFloat64Bytes" total-float64-bytes
         "totalPackedBytes" total-packed-bytes
         "cumulativeShrinkFactor" (if (zerop total-packed-bytes)
                                      0.0d0
                                      (/ total-float64-bytes total-packed-bytes))
         "widthHistogram" width-histogram
         "perMachine" (vectorize (nreverse per-machine)))))

(defun merge-operation-json (machine sequence output output-index)
  (let* ((values (output-vector-vector output))
         (governance (resolve-governance machine (sequence-id sequence) values))
         (deprecation (sequence-deprecation-json sequence))
         (out (obj "region" (region-json (mapping-output (machine-mapping machine)))
                   "machineId" (machine-id machine)
                   "sequenceId" (sequence-id sequence)
                   "outputIndex" output-index
                   "values" (vectorize values)
                   "provenance" (vectorize (output-vector-provenance output)))))
    (when governance
      (setf (jget out "governance") governance))
    (when deprecation
      (setf (jget out "deprecation") deprecation))
    out))

(defun sorted-event-bus-writes (writes)
  (sort writes
        (lambda (left right)
          (let ((ls (jstring left "subscriberMachineId" ""))
                (rs (jstring right "subscriberMachineId" ""))
                (lb (or (jnumber left "bitOffset" 0) 0))
                (rb (or (jnumber right "bitOffset" 0) 0))
                (lm (jstring left "producerMachineId" ""))
                (rm (jstring right "producerMachineId" ""))
                (lq (jstring left "producerSequenceId" ""))
                (rq (jstring right "producerSequenceId" "")))
            (cond
              ((not (string= ls rs)) (string< ls rs))
              ((/= lb rb) (< lb rb))
              ((not (string= lm rm)) (string< lm rm))
              (t (string< lq rq)))))))

(defun apply-event-bus (state merge-batch)
  (let ((writes nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (operation merge-batch)
      (let* ((key (compose-key (jstring operation "machineId" "")
                               (jstring operation "sequenceId" "")))
             (subscriptions (gethash key (reality-state-event-bus-subscriptions state))))
        (dolist (subscription subscriptions)
          (let* ((bit (truncate (or (jnumber subscription "bitOffset" 0) 0)))
                 (dedup (format nil "~a|~a|~a|~a"
                                (jstring subscription "subscriberMachineId" "")
                                bit
                                (jstring operation "machineId" "")
                                (jstring operation "sequenceId" ""))))
            (unless (gethash dedup seen)
              (setf (gethash dedup seen) t)
              (push (obj "producerMachineId" (jstring operation "machineId" "")
                         "producerSequenceId" (jstring operation "sequenceId" "")
                         "subscriberMachineId" (jstring subscription "subscriberMachineId" "")
                         "bitOffset" bit
                         "value" 1.0d0
                         "provenance" (jget operation "provenance"))
                    writes)
              (setf (gethash bit (reality-state-latched-event-bits state)) t))))))
    (let ((sorted (sorted-event-bus-writes writes)))
      (dolist (write sorted)
        (let ((bit (truncate (or (jnumber write "bitOffset" 0) 0))))
          (ensure-space-length state (1+ bit))
          (setf (nth bit (reality-state-perceptual-space state)) (or (jnumber write "value" 1.0d0) 1.0d0))))
      sorted)))

(defun apply-latched-event-bits (state)
  (maphash (lambda (bit _)
             (declare (ignore _))
             (ensure-space-length state (1+ bit))
             (setf (nth bit (reality-state-perceptual-space state)) 1.0d0))
           (reality-state-latched-event-bits state)))

(defun process-perceptual-input (state input &key override include-machine-results include-perceptual-space compact)
  (ensure-space-length state (max (reality-state-dimension state) (length input)))
  (setf (reality-state-perceptual-space state)
        (append input (make-list (max 0 (- (reality-state-dimension state) (length input)))
                                 :initial-element 0.0d0)))
  (apply-latched-event-bits state)
  (let ((machine-results (make-hash-table :test #'equal))
        (merge-batch nil)
        (active-regions nil))
    (dolist (machine (object-values-sorted (reality-state-machines state)))
      (let ((id (machine-id machine)))
       (when (machine-mapping machine)
         (let* ((mapping (machine-mapping machine))
                (machine-input (extract-region (reality-state-perceptual-space state)
                                               (mapping-input mapping)))
                (result (process-machine-input machine machine-input :override override)))
           (when include-machine-results
             (setf (gethash id machine-results) (transition-result-json result)))
           (push (obj "offset" (region-offset (mapping-input mapping))
                      "length" (region-length (mapping-input mapping))
                      "machineId" id
                      "type" "input")
                 active-regions)
           (when (jbool (transition-result-arbiter-metadata result) "shouldOutput" nil)
             (dolist (sequence-id (object-keys-sorted (transition-result-sequence-outputs result)))
               (let ((sequence (sequence-by-id machine sequence-id))
                     (outputs (gethash sequence-id (transition-result-sequence-outputs result)))
                     (index 0))
                 (dolist (output outputs)
                   (when sequence
                     (push (merge-operation-json machine sequence output index) merge-batch)
                     (incf index))))))))))
    (setf merge-batch (sorted-merge-operations merge-batch))
    (when compact
      (add-packed-merge-values state merge-batch))
    (dolist (operation merge-batch)
      (let ((region (make-region-from-json (jget operation "region")))
            (values (numbers-from-json (jget operation "values"))))
        (setf (reality-state-perceptual-space state)
              (merge-region (reality-state-perceptual-space state) region values))
        (push (obj "offset" (region-offset region)
                   "length" (region-length region)
                   "machineId" (jstring operation "machineId" "")
                   "type" "output")
              active-regions)))
    (let* ((event-bus (apply-event-bus state merge-batch))
           (step-number (reality-state-step-count state))
           (step (obj "success" t
                     "stepNumber" step-number
                     "timestamp" (now-ms)
                     "inputVector" (vectorize input)
                     "machineResults" (if include-machine-results machine-results (obj))
                     "mergeBatch" (vectorize merge-batch)
                     "eventBus" (vectorize event-bus)
                     "activeRegions" (vectorize (nreverse active-regions)))))
      (setf (reality-state-step-count state) (1+ step-number))
      (when include-perceptual-space
        (setf (jget step "perceptualSpace") (vectorize (reality-state-perceptual-space state))
              (jget step "perceptualSpaceIsDebugProjection") t))
      (record-history state step)
      step)))

(defun active-vectors-json (state)
  (let (rows)
    (maphash
     (lambda (machine-id machine)
       (dolist (sequence (machine-sequence-list machine))
         (maphash
          (lambda (_ vector)
            (declare (ignore _))
            (when (reality-vector-active-p vector)
              (push (obj "machineId" machine-id
                         "sequenceId" (sequence-id sequence)
                         "vector" (reality-vector-json vector))
                    rows)))
          (sequence-vectors sequence))))
     (reality-state-machines state))
    (vectorize (nreverse rows))))

(defun machine-graph-json (state)
  (let (nodes edges)
    (maphash
     (lambda (id machine)
       (push (obj "id" id
                  "name" (machine-name machine)
                  "description" (machine-description machine)
                  "inputMapping" (if (machine-mapping machine)
                                      (region-json (mapping-input (machine-mapping machine)))
                                      +json-null+)
                  "outputMapping" (if (machine-mapping machine)
                                       (region-json (mapping-output (machine-mapping machine)))
                                       +json-null+)
                  "metadata" (or (machine-metadata machine) (obj)))
             nodes))
     (reality-state-machines state))
    (obj "nodes" (vectorize (nreverse nodes))
         "edges" (vectorize (nreverse edges)))))

(defun reality-routes (actor)
  (list
   (make-route "GET" "/" (lambda (_ body query)
                           (declare (ignore _ body query))
                           (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
   (make-route "GET" "/api" (lambda (_ body query)
                              (declare (ignore _ body query))
                              (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
   (make-route "GET" "/api/health" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response (obj "status" "healthy" "timestamp" (now-ms) "version" "0.1.0-lsp"))))
   (make-route "GET" "/api/config" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response
                                     (actor-ask actor
                                                (lambda (state)
                                                  (obj "vectorDimension" (reality-state-dimension state)
                                                       "matchThreshold" 0.5d0
                                                       "qdrantUrl" (reality-state-qdrant-url state)
                                                       "collectionName" (reality-state-collection-name state)))))))
   (make-route "PUT" "/api/config/dimension" (lambda (_ body query)
                                             (declare (ignore _ body))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let ((dimension (parse-integer (or (gethash "dimension" query)
                                                                                               (write-to-string (reality-state-dimension state)))
                                                                                           :junk-allowed t)))
                                                             (setf (reality-state-dimension state) dimension)
                                                             (obj "success" t "dimension" dimension)))))))
   (make-route "POST" "/api/vectors/search" (lambda (_ body query)
                                             (declare (ignore _ query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let* ((query-vector (numbers-from-json (jget body "vector")))
                                                                  (limit (truncate (or (jnumber body "limit" 10) 10)))
                                                                  (threshold (jnumber body "threshold" nil))
                                                                  (rows nil))
                                                             (maphash
                                                              (lambda (_ vector-json)
                                                                (declare (ignore _))
                                                                (when (< (length rows) limit)
                                                                  (let ((score (cosine query-vector (numbers-from-json (or (jget vector-json "vector")
                                                                                                                           (jget vector-json "values")
                                                                                                                           (arr))))))
                                                                    (when (or (null threshold) (>= score threshold))
                                                                      (push (obj "vector" vector-json "score" score) rows)))))
                                                              (reality-state-vector-store state))
                                                             (obj "results" (vectorize (nreverse rows)))))))))
   (make-route "POST" "/api/vectors" (lambda (_ body query)
                                      (declare (ignore _ query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (let ((id (or (jstring body "id" nil) (make-id "vector"))))
                                                      (setf (jget body "id") id
                                                            (gethash id (reality-state-vector-store state)) body)
                                                      (obj "success" t "vector" body)))))))
   (make-route "GET" "/api/vectors/:id" (lambda (params body query)
                                        (declare (ignore body query))
                                        (json-response (obj "message" "Vector retrieval endpoint" "id" (gethash "id" params)))))
   (make-route "DELETE" "/api/vectors/:id" (lambda (params body query)
                                           (declare (ignore body query))
                                           (json-response
                                            (actor-ask actor
                                                       (lambda (state)
                                                         (remhash (gethash "id" params) (reality-state-vector-store state))
                                                         (obj "success" t "id" (gethash "id" params)))))))
   (make-route "POST" "/api/sequences/persist" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (json-response (obj "success" t))))
   (make-route "GET" "/api/sequences" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (obj "sequences" (vectorize
                                                                       (mapcar (lambda (s) (sequence-json s :full t))
                                                                               (object-values (reality-state-sequences state))))))))))
   (make-route "POST" "/api/sequences" (lambda (_ body query)
                                        (declare (ignore _ query))
                                        (json-response
                                         (actor-ask actor
                                                    (lambda (state)
                                                      (let ((sequence (parse-sequence body)))
                                                        (setf (gethash (sequence-id sequence) (reality-state-sequences state)) sequence)
                                                        (obj "success" t "sequence" (sequence-json sequence :full t))))))))
   (make-route "GET" "/api/sequences/:id" (lambda (params body query)
                                           (declare (ignore body query))
                                           (let ((result (actor-ask actor
                                                                    (lambda (state)
                                                                      (gethash (gethash "id" params) (reality-state-sequences state))))))
                                             (if result
                                                 (json-response (obj "sequence" (sequence-json result :full t)))
                                                 (error-response "Sequence not found" 404)))))
   (make-route "POST" "/api/engine/reset" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor (lambda (state) (reset-reality-state state) (obj "success" t))))))
   (make-route "GET" "/api/engine/stats" (lambda (_ body query)
                                          (declare (ignore _ body query))
                                          (json-response (actor-ask actor (lambda (state) (obj "stats" (stats-json state)))))))
   (make-route "GET" "/api/runtime/metrics" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (json-response (actor-ask actor (lambda (state) (obj "stats" (stats-json state) "domainWorkerPool" (obj "semantics" "actor-mailbox")))))))
   (make-route "GET" "/api/runtime/vector-space" (lambda (_ body query)
                                                  (declare (ignore _ body query))
                                                  (json-response
                                                   (actor-ask actor
                                                              (lambda (state)
                                                                (obj "dimension" (reality-state-dimension state)
                                                                     "requiredDimension" (required-dimension state)
                                                                     "encoding" "dense-float64-clamped-0-1"
                                                                     "mappingVersion" (reality-state-mapping-version state)
                                                                     "eventBusSubscriptionCount" (event-bus-subscription-count state))))))
   (make-route "GET" "/api/runtime/storage-footprint" (lambda (_ body query)
                                                       (declare (ignore _ body query))
                                                       (json-response
                                                        (actor-ask actor
                                                                   (lambda (state)
                                                                     (storage-footprint-json state))))))
   (make-route "GET" "/api/runtime/options" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (obj "historyLimit" (reality-state-history-limit state)
                                                                "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))))))))
   (make-route "PATCH" "/api/runtime/options" (lambda (_ body query)
                                               (declare (ignore _ query))
                                               (json-response
                                                (actor-ask actor
                                                           (lambda (state)
                                                             (when (jnumber body "historyLimit" nil)
                                                               (setf (reality-state-history-limit state) (truncate (jnumber body "historyLimit"))))
                                                             (when (not (eq (jget body "includeMachineResults" :missing) :missing))
                                                               (setf (reality-state-include-machine-results-p state) (jbool body "includeMachineResults" t)))
                                                             (when (not (eq (jget body "includePerceptualSpace" :missing) :missing))
                                                               (setf (reality-state-include-perceptual-space-p state) (jbool body "includePerceptualSpace" t)))
                                                             (obj "historyLimit" (reality-state-history-limit state)
                                                                  "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                  "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state))))))))
   (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor (lambda (state) (obj "activeVectors" (active-vectors-json state)))))))
   (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                            (declare (ignore _ body))
                                            (let ((limit (parse-integer (or (gethash "limit" query) "0") :junk-allowed t)))
                                              (json-response
                                               (actor-ask actor
                                                          (lambda (state)
                                                            (obj "history" (vectorize (if (and limit (> limit 0))
                                                                                          (subseq (reality-state-history state)
                                                                                                  0 (min limit (length (reality-state-history state))))
                                                                                          (reality-state-history state))))))))))
   (make-route "POST" "/api/engine/process" (lambda (_ body query)
                                             (declare (ignore _ query))
                                             (json-response
                                              (actor-ask actor
                                                         (lambda (state)
                                                           (let ((input (numbers-from-json (jget body "vector")))
                                                                 (outputs nil))
                                                             (maphash
                                                              (lambda (_ machine)
                                                                (declare (ignore _))
                                                                (let ((result (process-machine-input machine input)))
                                                                  (when (transition-result-machine-output result)
                                                                    (push (output-vector-json (transition-result-machine-output result)) outputs))))
                                                              (reality-state-machines state))
                                                             (let ((result (obj "inputVector" (vectorize input)
                                                                                "timestamp" (now-ms)
                                                                                "outputs" (vectorize (nreverse outputs)))))
                                                               (record-history state (obj "type" "engine-process" "result" result))
                                                               (obj "result" result))))))))
   (make-route "GET" "/api/machines" (lambda (_ body query)
                                      (declare (ignore _ body query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (obj "machines" (vectorize
                                                                     (mapcar #'machine-json
                                                                             (object-values (reality-state-machines state))))))))))
   (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                          (declare (ignore body query))
                                          (let ((machine (actor-ask actor (lambda (state) (gethash (gethash "id" params) (reality-state-machines state)))))))
                                            (if machine
                                                (json-response (obj "machine" (machine-json machine :full t)))
                                                (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines" (lambda (_ body query)
                                       (declare (ignore _ query))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (let ((machine (machine-from-json body)))
                                                       (put-machine state machine)
                                                       (obj "success" t "machine" (machine-json machine :full t))))))))
   (make-route "PUT" "/api/machines/:id" (lambda (params body query)
                                          (declare (ignore query))
                                          (json-response
                                           (actor-ask actor
                                                      (lambda (state)
                                                        (let ((machine (machine-from-json body (gethash "id" params))))
                                                          (put-machine state machine)
                                                          (obj "success" t "machine" (machine-json machine :full t)))))))
   (make-route "DELETE" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore body query))
                                             (json-response
                                              (actor-ask actor
                                                           (lambda (state)
                                                             (unregister-compose-subscriptions state (gethash "id" params))
                                                             (let ((removed (remhash (gethash "id" params) (reality-state-machines state))))
                                                               (when removed
                                                                 (incf (reality-state-mapping-version state)))
                                                               (obj "success" (json-bool removed))))))))
   (make-route "POST" "/api/machines/:id/process" (lambda (params body query)
                                                   (declare (ignore query))
                                                   (let ((result (actor-ask actor
                                                                            (lambda (state)
                                                                              (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                                (when machine
                                                                                  (transition-result-json
                                                                                   (process-machine-input machine (numbers-from-json (jget body "inputVector"))))))))))
                                                     (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/:id/whatif" (lambda (params body query)
                                                  (declare (ignore query))
                                                  (let ((result (actor-ask actor
                                                                           (lambda (state)
                                                                             (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                               (when machine
                                                                                 (transition-result-json
                                                                                  (process-machine-input
                                                                                   (machine-from-json (machine-json machine :full t))
                                                                                   (numbers-from-json (jget body "inputVector"))))))))))
                                                    (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/:id/process-universal" (lambda (params body query)
                                                             (declare (ignore query))
                                                             (let ((result (actor-ask actor
                                                                                      (lambda (state)
                                                                                        (let ((machine (gethash (gethash "id" params) (reality-state-machines state))))
                                                                                          (when (and machine (machine-mapping machine))
                                                                                            (let ((input (extract-region (numbers-from-json (jget body "universalInputSpace"))
                                                                                                                         (mapping-input (machine-mapping machine)))))
                                                                                              (transition-result-json (process-machine-input machine input)))))))))
                                                               (if result (json-response result) (error-response "Machine not found" 404)))))
   (make-route "POST" "/api/machines/process-universal/all" (lambda (_ body query)
                                                             (declare (ignore _ query))
                                                             (json-response
                                                              (actor-ask actor
                                                                         (lambda (state)
                                                                           (let ((universal (numbers-from-json (jget body "universalInputSpace")))
                                                                                 (results (make-hash-table :test #'equal)))
                                                                             (maphash
                                                                              (lambda (id machine)
                                                                                (when (machine-mapping machine)
                                                                                  (setf (gethash id results)
                                                                                        (transition-result-json
                                                                                         (process-machine-input machine
                                                                                                                (extract-region universal (mapping-input (machine-mapping machine))))))))
                                                                              (reality-state-machines state))
                                                                             (obj "results" results)))))))
   (make-route "GET" "/api/machines/json/list" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (json-response
                                                 (actor-ask actor
                                                            (lambda (state)
                                                              (let ((rows nil)
                                                                    (dir (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
                                                                (when (uiop:directory-exists-p dir)
                                                                  (dolist (path (uiop:directory-files dir "*.json"))
                                                                    (push (obj "filename" (file-namestring path)
                                                                               "name" (pathname-name path)
                                                                               "description" ""
                                                                               "version" "1.0.0"
                                                                               "metadata" (obj)
                                                                               "sequenceCount" 0)
                                                                          rows)))
                                                                (obj "machines" (vectorize (nreverse rows)))))))))
   (make-route "GET" "/api/machines/json/:name" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (handler-case
                                                     (json-response
                                                      (actor-ask actor
                                                                 (lambda (state)
                                                                   (let* ((name (gethash "name" params))
                                                                          (filename (if (uiop:string-suffix-p ".json" name) name (format nil "~a.json" name)))
                                                                          (path (merge-pathnames filename (uiop:ensure-directory-pathname (reality-state-machine-dir state))))
                                                                     (machine (load-machine-from-file path)))
                                                                     (put-machine state machine)
                                                                     (obj "success" t "machine" (machine-json machine :full t) "message" "Machine loaded successfully")))))
                                                   (error (condition) (error-response (princ-to-string condition) 404)))))
   (make-route "POST" "/api/machines/json/import" (lambda (_ body query)
                                                   (declare (ignore _ query))
                                                   (json-response
                                                    (actor-ask actor
                                                               (lambda (state)
                                                                 (let ((machine (machine-from-json body)))
                                                                   (put-machine state machine)
                                                                   (obj "success" t "machine" (machine-json machine :full t))))))))
   (make-route "GET" "/api/machines/:id/export" (lambda (params body query)
                                                 (declare (ignore body query))
                                                 (let ((machine (actor-ask actor (lambda (state) (gethash (gethash "id" params) (reality-state-machines state)))))))
                                                   (if machine
                                                       (json-response (obj "version" "1.0.0" "machine" (machine-json machine :full t)))
                                                       (error-response "Machine not found" 404)))))
   (make-route "GET" "/api/machine-graph" (lambda (_ body query)
                                           (declare (ignore _ body query))
                                           (json-response (actor-ask actor #'machine-graph-json))))
   (make-route "POST" "/api/perceptual-simulation/step" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor (lambda (state)
                                                                                           (let ((step (process-perceptual-input state (reality-state-perceptual-space state)
                                                                                                                                  :include-machine-results t
                                                                                                                                  :include-perceptual-space t)))
                                                                                             (obj "success" t "step" step)))))))
   (make-route "POST" "/api/perceptual-simulation/reset" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (json-response (actor-ask actor (lambda (state) (reset-reality-state state) (obj "success" t))))))
   (make-route "GET" "/api/perceptual-simulation/state" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor (lambda (state) (obj "running" +json-false+
                                                                                                             "dimension" (reality-state-dimension state)
                                                                                                             "perceptualSpace" (vectorize (reality-state-perceptual-space state))))))))
   (make-route "GET" "/api/perceptual-simulation/history" (lambda (_ body query)
                                                           (declare (ignore _ body query))
                                                           (json-response (actor-ask actor (lambda (state) (obj "history" (vectorize (reality-state-history state))))))))
   (make-route "POST" "/api/preception/diagnostic" (lambda (_ body query)
                                                    (declare (ignore _ query))
                                                    (json-response (obj "universalInputSpace" (jget body "universalInputSpace")
                                                                        "resolvedInputs" (obj)))))
   (make-route "POST" "/api/perceive" (lambda (_ body query)
                                       (declare (ignore _))
                                       (json-response
                                        (actor-ask actor
                                                   (lambda (state)
                                                     (let ((input (assemble-input-vector state body)))
                                                       (if input
                                                           (process-perceptual-input
                                                            state input
                                                            :override (or (jstring body "matchAlgorithmOverride" nil)
                                                                          (jstring body "matchAlgorithm" nil))
                                                            :include-machine-results (jbool body "includeMachineResults"
                                                                                            (if (jbool body "compact" nil)
                                                                                                nil
                                                                                                (reality-state-include-machine-results-p state)))
                                                            :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                             (reality-state-include-perceptual-space-p state))
                                                            :compact (or (jbool body "compact" nil)
                                                                         (compact-query-p query)))
                                                           (obj "error" "Provide exactly one of: vector, sparseVector, domainVectors"))))))))))

(defun reality-routes (actor)
  (labels ((state-json (fn)
             (json-response (actor-ask actor fn))))
    (list
     (make-route "GET" "/" (lambda (_ body query)
                             (declare (ignore _ body query))
                             (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
     (make-route "GET" "/api" (lambda (_ body query)
                                (declare (ignore _ body query))
                                (json-response (obj "name" "Reality Engine" "version" "0.1.0-lsp" "status" "running"))))
     (make-route "GET" "/api/health" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (json-response (obj "status" "healthy" "timestamp" (now-ms) "version" "0.1.0-lsp"))))
     (make-route "GET" "/api/config" (lambda (_ body query)
                                       (declare (ignore _ body query))
                                       (state-json (lambda (state)
                                                     (obj "vectorDimension" (reality-state-dimension state)
                                                          "matchThreshold" 0.5d0
                                                          "qdrantUrl" (reality-state-qdrant-url state)
                                                          "collectionName" (reality-state-collection-name state))))))
     (make-route "GET" "/api/engine/stats" (lambda (_ body query)
                                             (declare (ignore _ body query))
                                             (state-json (lambda (state) (obj "stats" (stats-json state))))))
     (make-route "GET" "/api/runtime/options" (lambda (_ body query)
                                                (declare (ignore _ body query))
                                                (state-json (lambda (state)
                                                              (obj "historyLimit" (reality-state-history-limit state)
                                                                   "includeMachineResults" (json-bool (reality-state-include-machine-results-p state))
                                                                   "includePerceptualSpace" (json-bool (reality-state-include-perceptual-space-p state)))))))
     (make-route "GET" "/api/runtime/vector-space" (lambda (_ body query)
                                                     (declare (ignore _ body query))
                                                     (state-json (lambda (state)
                                                                   (obj "dimension" (reality-state-dimension state)
                                                                        "requiredDimension" (required-dimension state)
                                                                        "encoding" "dense-float64-clamped-0-1"
                                                                        "mappingVersion" (reality-state-mapping-version state)
                                                                        "eventBusSubscriptionCount" (event-bus-subscription-count state)))))
     (make-route "GET" "/api/runtime/storage-footprint" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (state-json #'storage-footprint-json)))
     (make-route "GET" "/api/governance/route" (lambda (_ body query)
                                                 (declare (ignore _ body))
                                                 (let ((machine-id (gethash "machineId" query))
                                                       (sequence-id (gethash "sequenceId" query))
                                                       (values-text (gethash "values" query)))
                                                   (cond
                                                     ((or (null machine-id) (null sequence-id) (null values-text))
                                                      (error-response "machineId, sequenceId, and values query parameters are required" 400))
                                                     (t
                                                      (let ((result (actor-ask actor
                                                                               (lambda (state)
                                                                                 (let ((machine (gethash machine-id (reality-state-machines state))))
                                                                                   (cond
                                                                                     ((null machine) :missing)
                                                                                     (t (resolve-governance machine sequence-id
                                                                                                            (parse-comma-values values-text)))))))))
                                                        (cond
                                                          ((eq result :missing)
                                                           (error-response (format nil "Machine not found: ~a" machine-id) 404))
                                                          ((null result)
                                                           (error-response
                                                            (format nil "No triggerConfig rule matches (sequenceId=~a, values=~a)"
                                                                    sequence-id values-text)
                                                            404))
                                                          (t
                                                           (json-response (obj "success" t "decision" result))))))))))
     (make-route "POST" "/api/engine/reset" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state) (reset-reality-state state) (obj "success" t)))))
     (make-route "GET" "/api/engine/history" (lambda (_ body query)
                                               (declare (ignore _ body query))
                                               (state-json (lambda (state) (obj "history" (vectorize (reality-state-history state)))))))
     (make-route "GET" "/api/engine/active" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json (lambda (state) (obj "activeVectors" (active-vectors-json state))))))
     (make-route "GET" "/api/machines" (lambda (_ body query)
                                         (declare (ignore _ body query))
                                         (state-json (lambda (state)
                                                       (obj "machines" (vectorize
                                                                        (mapcar #'machine-json
                                                                                (object-values-sorted (reality-state-machines state)))))))))
     (make-route "GET" "/api/machines/:id" (lambda (params body query)
                                             (declare (ignore body query))
                                             (let ((machine (actor-ask actor (lambda (state)
                                                                               (gethash (gethash "id" params)
                                                                                        (reality-state-machines state)))))))
                                               (if machine
                                                   (json-response (obj "machine" (machine-json machine :full t)))
                                                   (error-response "Machine not found" 404))))
     (make-route "POST" "/api/machines" (lambda (_ body query)
                                          (declare (ignore _ query))
                                          (state-json (lambda (state)
                                                        (let ((machine (machine-from-json body)))
                                                          (put-machine state machine)
                                                          (obj "success" t "machine" (machine-json machine :full t)))))))
     (make-route "POST" "/api/machines/:id/process" (lambda (params body query)
                                                      (declare (ignore query))
                                                      (let ((result (actor-ask actor
                                                                               (lambda (state)
                                                                                 (let ((machine (gethash (gethash "id" params)
                                                                                                         (reality-state-machines state))))
                                                                                   (when machine
                                                                                     (transition-result-json
                                                                                      (process-machine-input machine (numbers-from-json (jget body "inputVector"))))))))))
                                                        (if result (json-response result) (error-response "Machine not found" 404)))))
     (make-route "GET" "/api/machine-graph" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (state-json #'machine-graph-json)))
     (make-route "POST" "/api/perceive" (lambda (_ body query)
                                          (declare (ignore _))
                                          (state-json (lambda (state)
                                                        (let ((input (assemble-input-vector state body)))
                                                          (if input
                                                              (process-perceptual-input
                                                               state input
                                                               :override (or (jstring body "matchAlgorithmOverride" nil)
                                                                             (jstring body "matchAlgorithm" nil))
                                                               :include-machine-results (jbool body "includeMachineResults"
                                                                                               (if (jbool body "compact" nil)
                                                                                                   nil
                                                                                                   (reality-state-include-machine-results-p state)))
                                                               :include-perceptual-space (jbool body "includePerceptualSpace"
                                                                                                (reality-state-include-perceptual-space-p state))
                                                               :compact (or (jbool body "compact" nil)
                                                                            (compact-query-p query)))
                                                              (obj "error" "Provide exactly one of: vector, sparseVector, domainVectors")))))))))))

(defun start-reality-service (&key (port 3299) (machine-dir "../RealityEngine_AI/examples/machines") (dimension 768))
  (let* ((state (make-reality-state-from-config :machine-dir machine-dir :dimension dimension))
         (actor (state-actor "reality-service" state)))
    (start-http-server port (reality-routes actor) :name "reality-engine-lsp")))

(defun start-reality-from-environment ()
  (start-reality-service :port (env-int "REALITY_ENGINE_PORT" 3299)
                         :machine-dir (env "MACHINES_DIR" "../RealityEngine_AI/examples/machines")
                         :dimension (env-int "VECTOR_DIMENSION" 768)))
