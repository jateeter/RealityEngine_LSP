(in-package #:reality-engine-lsp)

;;;; K-lines: episodes captured at their corpus boundaries while the engine
;;;; steps, kept in a persistent store per engine instance
;;;; (RealityEngine_CI#603, RealityEngine_LSP#172).
;;;;
;;;; The specification is RealityEngine_CI SURFACE_SPEC.md: "Boundaries",
;;;; "Paths that dwell, fork or merge", "Scope", "Identity, clock and parity",
;;;; "Managing K-lines during operation", "The K-line store" and "Record".
;;;; This is the RE half, the same algorithm as RealityEngine_CPP src/kline.cpp:
;;;; the tracker observes each committed step -- per sequence, which events
;;;; matched and which were activated, and which are active afterwards -- and
;;;; never re-derives the transition rule.

;;; ── JCS, SHA-256, the corpus fingerprint ──────────────────────────────────

(defun jcs-escape (string stream)
  (loop for ch across string
        do (case ch
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Backspace (write-string "\\b" stream))
             (#\Page (write-string "\\f" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (let ((code (char-code ch)))
                (if (< code 32)
                    (format stream "\\u~(~4,'0x~)" code)
                    (write-char ch stream)))))))

(defun utf16-key (string)
  "STRING as UTF-16 code units, for JCS key order."
  (let (units)
    (loop for ch across string
          for cp = (char-code ch)
          do (if (>= cp #x10000)
                 (let ((v (- cp #x10000)))
                   (push (+ #xD800 (ash v -10)) units)
                   (push (+ #xDC00 (logand v #x3FF)) units))
                 (push cp units)))
    (nreverse units)))

(defun utf16< (a b)
  (let ((x (utf16-key a)) (y (utf16-key b)))
    (loop
      (cond ((null x) (return (not (null y))))
            ((null y) (return nil))
            ((< (car x) (car y)) (return t))
            ((> (car x) (car y)) (return nil)))
      (pop x) (pop y))))

(defun write-jcs (value stream)
  (cond
    ((eq value +json-false+) (write-string "false" stream))
    ((or (eq value +json-null+) (null value)) (write-string "null" stream))
    ((eq value t) (write-string "true" stream))
    ((stringp value) (write-char #\" stream) (jcs-escape value stream) (write-char #\" stream))
    ((integerp value) (format stream "~D" value))
    ((numberp value) (write-json-double (float value 1d0) stream))
    ((hash-table-p value)
     (write-char #\{ stream)
     (let (keys (first t))
       (maphash (lambda (k v) (declare (ignore v)) (push k keys)) value)
       (dolist (k (sort keys #'utf16<))
         (unless first (write-char #\, stream))
         (setf first nil)
         (write-jcs k stream)
         (write-char #\: stream)
         (write-jcs (gethash k value) stream)))
     (write-char #\} stream))
    ((vectorp value)
     (write-char #\[ stream)
     (loop for i from 0 below (length value)
           do (when (> i 0) (write-char #\, stream))
              (write-jcs (aref value i) stream))
     (write-char #\] stream))
    ((listp value) (write-jcs (coerce value 'vector) stream))
    (t (write-jcs (princ-to-string value) stream))))

(defun jcs (value)
  (with-output-to-string (s) (write-jcs value s)))

(defun sha256-hex (string)
  "Lower-case hex SHA-256 of STRING's UTF-8 bytes, through libcrypto (loaded by cl+ssl)."
  (let* ((octets (babel:string-to-octets string :encoding :utf-8))
         (n (length octets)))
    (cffi:with-foreign-objects ((data :unsigned-char (max 1 n))
                                (md :unsigned-char 64)
                                (len :unsigned-int))
      (dotimes (i n) (setf (cffi:mem-aref data :unsigned-char i) (aref octets i)))
      (let ((ok (cffi:foreign-funcall "EVP_Digest"
                                      :pointer data :size n :pointer md :pointer len
                                      :pointer (cffi:foreign-funcall "EVP_sha256" :pointer)
                                      :pointer (cffi:null-pointer) :int)))
        (unless (= ok 1) (error "SHA-256 failed"))
        (with-output-to-string (s)
          (dotimes (i (cffi:mem-ref len :unsigned-int))
            (format s "~(~2,'0x~)" (cffi:mem-aref md :unsigned-char i))))))))

(defun octets< (a b)
  "A < B comparing UTF-8 bytes (SURFACE_SPEC: collation is declared, not native)."
  (let ((x (babel:string-to-octets a :encoding :utf-8))
        (y (babel:string-to-octets b :encoding :utf-8)))
    (loop for i from 0
          do (cond ((>= i (length x)) (return (< i (length y))))
                   ((>= i (length y)) (return nil))
                   ((< (aref x i) (aref y i)) (return t))
                   ((> (aref x i) (aref y i)) (return nil))))))

(defun corpus-fingerprint (pairs)
  "sha256/resident-declared-v1 over PAIRS of (name . declared object)."
  (let ((lines (mapcar (lambda (p) (cons (car p) (sha256-hex (jcs (cdr p))))) pairs)))
    (setf lines (sort lines (lambda (a b)
                              (if (string= (car a) (car b))
                                  (string< (cdr a) (cdr b))
                                  (octets< (car a) (car b))))))
    (sha256-hex (with-output-to-string (s)
                  (dolist (l lines) (format s "~a~c~a~c" (car l) #\Tab (cdr l) #\Newline))))))

;;; ── The store ─────────────────────────────────────────────────────────────

(defconstant +kline-seal-bytes+ (* 64 1024 1024))

(defstruct (kline-segment (:conc-name kseg-))
  name (bytes 0) sealed exported (sha256 "") (fingerprint "") (first-commit 0) (last-commit 0) archived)

(defstruct (kline-meta (:conc-name kmeta-))
  id machine sequence outcome close-commit close-step open-step segment (offset 0) (line 0))

(defstruct (kline-store (:conc-name kstore-))
  dir                                   ; nil: in memory
  (segments nil)                        ; oldest first
  (metas (make-array 0 :adjustable t :fill-pointer t))
  (by-id (make-hash-table :test #'equal))
  (events (make-hash-table :test #'equal))   ; id -> list of (through . raw), oldest first
  (lines (make-array 0 :adjustable t :fill-pointer t))
  (appends 0))

(defun kstore-path (store name)
  (merge-pathnames name (uiop:ensure-directory-pathname (kstore-dir store))))

(defun write-durable (path string)
  (let ((tmp (format nil "~a.tmp" (namestring path))))
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string string out)
      (finish-output out)
      (sb-posix:fsync (sb-sys:fd-stream-fd out)))
    (sb-posix:rename tmp (namestring path))))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun segment-commit (name)
  (let ((dash (position #\- name)) (dot (position #\. name :from-end t)))
    (or (and dash dot (> dot dash) (ignore-errors (parse-integer name :start (1+ dash) :end dot))) -1)))

(defun open-kline-store (dir)
  "A store under DIR, or in memory when DIR is nil."
  (let ((store (make-kline-store :dir dir)))
    (when dir
      (ensure-directories-exist (uiop:ensure-directory-pathname dir))
      (kstore-load store))
    store))

(defun kstore-index-line (store raw segment offset line)
  (let* ((j (parse-json raw)) (format (jstring j "format" "")))
    (cond ((string= format "re-kline/1")
           (let ((m (make-kline-meta :id (jstring j "id" "")
                                     :machine (jstring (jget j "focal") "machine" "")
                                     :sequence (jstring (jget j "focal") "sequence" "")
                                     :outcome (jstring j "outcome" "")
                                     :close-commit (truncate (jnumber (jget j "close") "commit" 0))
                                     :close-step (truncate (jnumber (jget j "close") "step" 0))
                                     :open-step (truncate (jnumber (jget j "open") "step" 0))
                                     :segment segment :offset offset :line line)))
             (setf (gethash (kmeta-id m) (kstore-by-id store)) (length (kstore-metas store)))
             (vector-push-extend m (kstore-metas store))))
          ((string= format "re-kline-event/1")
           (let ((id (jstring j "kline" "")))
             (setf (gethash id (kstore-events store))
                   (append (gethash id (kstore-events store))
                           (list (cons (truncate (jnumber j "through" 0)) raw)))))))))

(defun kstore-load (store)
  (let ((known (make-hash-table :test #'equal))
        (manifest (kstore-path store "manifest.json")))
    (when (probe-file manifest)
      (loop for s across (or (jget (parse-json (uiop:read-file-string manifest)) "segments") #())
            do (setf (gethash (jstring s "segment" "") known)
                     (make-kline-segment :name (jstring s "segment" "")
                                         :sealed (eq (jget s "sealed") t) :exported (eq (jget s "exported") t)
                                         :sha256 (jstring s "sha256" "") :fingerprint (jstring s "fingerprint" "")
                                         :archived (eq (jget s "archived") t)))))
    (let ((names (sort (remove-if-not (lambda (n) (and (eql 0 (search "segment-" n)) (>= (segment-commit n) 0)))
                                      (mapcar #'file-namestring
                                              (uiop:directory-files (uiop:ensure-directory-pathname (kstore-dir store))
                                                                    "segment-*.jsonl")))
                       #'< :key #'segment-commit)))
      (dolist (name names)
        (let* ((path (kstore-path store name))
               (bytes (read-octets path))
               (good (let ((nl (position 10 bytes :from-end t))) (if nl (1+ nl) 0))))
          ;; A half-written last record is truncated.
          (when (/= good (length bytes))
            (with-open-file (out path :direction :output :if-exists :overwrite :element-type '(unsigned-byte 8))
              (sb-posix:ftruncate (sb-sys:fd-stream-fd out) good))
            (setf bytes (subseq bytes 0 good)))
          (let ((seg (or (gethash name known) (make-kline-segment))))
            (setf (kseg-name seg) name (kseg-first-commit seg) (segment-commit name) (kseg-bytes seg) (length bytes))
            (loop with start = 0
                  for nl = (position 10 bytes :start start)
                  while nl
                  do (ignore-errors
                      (kstore-index-line store (babel:octets-to-string bytes :start start :end nl :encoding :utf-8)
                                         name start 0))
                     (setf start (1+ nl)))
            (setf (kstore-segments store) (append (kstore-segments store) (list seg)))))))
    (kstore-write-index store)))

(defun kstore-write-manifest (store)
  (when (kstore-dir store)
    (write-durable (kstore-path store "manifest.json")
                   (format nil "~a~%"
                           (jcs (obj "segments"
                                     (vectorize
                                      (mapcar (lambda (s)
                                                (obj "segment" (kseg-name s) "sealed" (json-bool (kseg-sealed s))
                                                     "exported" (json-bool (kseg-exported s)) "sha256" (kseg-sha256 s)
                                                     "fingerprint" (kseg-fingerprint s)
                                                     "archived" (json-bool (kseg-archived s)) "bytes" (kseg-bytes s)))
                                              (kstore-segments store)))))))))

(defun kstore-write-index (store)
  (when (kstore-dir store)
    (let ((ids (obj)) (counts (obj)))
      (loop for m across (kstore-metas store)
            do (setf (gethash (kmeta-id m) ids) (obj "segment" (kmeta-segment m) "offset" (kmeta-offset m)))
               (let* ((key (format nil "~a~c~a" (kmeta-machine m) #\Tab (kmeta-sequence m)))
                      (c (or (gethash key counts) (setf (gethash key counts) (obj)))))
                 (setf (gethash (kmeta-outcome m) c) (1+ (or (gethash (kmeta-outcome m) c) 0)))))
      (write-durable (kstore-path store "index.json") (format nil "~a~%" (jcs (obj "ids" ids "counts" counts)))))))

(defun kstore-seal (store seg)
  (setf (kseg-sealed seg) t)
  (when (kstore-dir store)
    (setf (kseg-sha256 seg) (sha256-hex (babel:octets-to-string (read-octets (kstore-path store (kseg-name seg)))
                                                                :encoding :utf-8))))
  (kstore-write-manifest store))

(defun kstore-active-segment (store fingerprint commit)
  (let ((last (car (last (kstore-segments store)))))
    (when (and last (not (kseg-sealed last)))
      (let ((other (and (plusp (length (kseg-fingerprint last)))
                        (string/= (kseg-fingerprint last) fingerprint)
                        (plusp (kseg-bytes last)))))
        (when (and (not other) (< (kseg-bytes last) +kline-seal-bytes+))
          (when (zerop (length (kseg-fingerprint last))) (setf (kseg-fingerprint last) fingerprint))
          (return-from kstore-active-segment last))
        (kstore-seal store last)))
    (let ((seg (make-kline-segment :name (format nil "segment-~d.jsonl" commit)
                                   :first-commit commit :fingerprint fingerprint)))
      (setf (kstore-segments store) (append (kstore-segments store) (list seg)))
      (kstore-write-manifest store)
      seg)))

(defun kstore-append (store lines fingerprint commit)
  (when lines
    (let* ((seg (kstore-active-segment store fingerprint commit))
           (offset (kseg-bytes seg))
           (raws (mapcar (lambda (l)
                           (let* ((raw (jcs l)) (here offset))
                             (incf offset (1+ (babel:string-size-in-octets raw :encoding :utf-8)))
                             (cons raw here)))
                         lines)))
      (when (kstore-dir store)
        (with-open-file (out (kstore-path store (kseg-name seg)) :direction :output :if-exists :append
                                                                 :if-does-not-exist :create :external-format :utf-8)
          (dolist (r raws) (write-string (car r) out) (write-char #\Newline out))
          (finish-output out)
          ;; Durable before the step's completion is published.
          (sb-posix:fsync (sb-sys:fd-stream-fd out))))
      (dolist (r raws)
        (let ((line 0))
          (unless (kstore-dir store)
            (vector-push-extend (car r) (kstore-lines store))
            (setf line (1- (length (kstore-lines store)))))
          (kstore-index-line store (car r) (kseg-name seg) (cdr r) line)))
      (setf (kseg-bytes seg) offset (kseg-last-commit seg) commit)
      ;; index.json is a cache rebuilt at boot: every 64 appends, not every step.
      (when (zerop (mod (incf (kstore-appends store)) 64)) (kstore-write-index store)))))

(defun kstore-checkpoint (store open)
  (when (kstore-dir store)
    (write-durable (kstore-path store "open.json") (format nil "~a~%" (jcs open)))))

(defun kstore-read-checkpoint (store)
  (when (kstore-dir store)
    (let ((path (kstore-path store "open.json")))
      (when (probe-file path) (ignore-errors (parse-json (uiop:read-file-string path)))))))

(defun kstore-raw (store meta)
  (if (kstore-dir store)
      (with-open-file (in (kstore-path store (kmeta-segment meta)) :element-type '(unsigned-byte 8))
        (file-position in (kmeta-offset meta))
        (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
          (loop for b = (read-byte in nil nil)
                while (and b (/= b 10))
                do (vector-push-extend b bytes))
          (babel:octets-to-string (coerce bytes '(simple-array (unsigned-byte 8) (*))) :encoding :utf-8)))
      (aref (kstore-lines store) (kmeta-line meta))))

(defun kstore-record (store meta)
  (let ((j (parse-json (kstore-raw store meta)))
        (events (gethash (kmeta-id meta) (kstore-events store))))
    ;; A sustain-end appended later belongs on the record as served.
    (when events
      (setf (jget j "sustain") (obj "through" (car (car (last events))) "ended" t)))
    j))

(defun kstore-warnings (store controls)
  (let ((sealed 0) (unexported 0))
    (dolist (s (kstore-segments store))
      (when (and (kseg-sealed s) (not (kseg-archived s)))
        (incf sealed (kseg-bytes s))
        (unless (kseg-exported s) (incf unexported))))
    (when (and (> sealed (getf controls :store-max-bytes)) (plusp unexported))
      (list (format nil "klineStoreMaxBytes reached: ~d unexported sealed segment(s) are kept; export them to free space"
                    unexported)))))

(defun kstore-stats (store controls)
  (let ((by-outcome (obj)) (by-machine (obj)) (bytes 0) (live 0))
    (loop for m across (kstore-metas store)
          do (setf (gethash (kmeta-outcome m) by-outcome) (1+ (or (gethash (kmeta-outcome m) by-outcome) 0)))
             (let ((per (or (gethash (kmeta-machine m) by-machine) (setf (gethash (kmeta-machine m) by-machine) (obj)))))
               (setf (gethash (kmeta-outcome m) per) (1+ (or (gethash (kmeta-outcome m) per) 0)))))
    (dolist (s (kstore-segments store))
      (unless (kseg-archived s) (incf bytes (kseg-bytes s)) (incf live)))
    (obj "byOutcome" by-outcome "byMachine" by-machine "segments" live "bytes" bytes
         "budget" (getf controls :store-max-bytes) "warnings" (vectorize (kstore-warnings store controls)))))

(defun kstore-enforce-budget (store controls)
  (flet ((sealed-bytes ()
           (loop for s in (kstore-segments store) when (and (kseg-sealed s) (not (kseg-archived s))) sum (kseg-bytes s))))
    (let (moved)
      (loop while (> (sealed-bytes) (getf controls :store-max-bytes))
            do (let ((s (find-if (lambda (s) (and (kseg-sealed s) (kseg-exported s) (not (kseg-archived s))))
                                 (kstore-segments store))))
                 (unless s (return))     ; never evict an unexported segment
                 (when (kstore-dir store)
                   (let ((archive (merge-pathnames "archive/" (uiop:ensure-directory-pathname (kstore-dir store)))))
                     (ensure-directories-exist archive)
                     (sb-posix:rename (namestring (kstore-path store (kseg-name s)))
                                      (namestring (merge-pathnames (kseg-name s) archive)))))
                 (setf (kseg-archived s) t moved t)))
      (when moved
        (let ((gone (mapcar #'kseg-name (remove-if-not #'kseg-archived (kstore-segments store))))
              (kept (make-array 0 :adjustable t :fill-pointer t)))
          (clrhash (kstore-by-id store))
          (loop for m across (kstore-metas store)
                unless (member (kmeta-segment m) gone :test #'string=)
                  do (setf (gethash (kmeta-id m) (kstore-by-id store)) (length kept))
                     (vector-push-extend m kept))
          (setf (kstore-metas store) kept))
        (kstore-write-manifest store)
        (kstore-write-index store)))))

;;; ── The tracker ───────────────────────────────────────────────────────────

(defstruct (kl-open (:conc-name klo-))
  (id "") machine sequence activator open held path (refused-by "")
  (focal-pairs nil) (focal-active-before #()) input output)

(defstruct (kl-sustain (:conc-name kls-)) (id "") machine sequence event (through 0) (open-step 0))

(defstruct (kl-step (:conc-name kst-))
  step commit isre osre ops
  (changed (make-hash-table :test #'equal))
  (asserted (make-hash-table :test #'equal))
  (regions (make-hash-table :test #'equal)))

(defstruct (kline-tracker (:conc-name klt-))
  instance store tick
  (capture nil) (feedback-hops 3) (store-max-bytes (* 2 1024 1024 1024))
  (longest-path 7) (fingerprint "") (last-step -1) (last-commit 0)
  (open nil) (sustain nil)
  (closed (make-hash-table :test #'equal))   ; id -> (machine sequence open-step close-step)
  (ring nil)                                 ; kl-step, oldest first
  (baseline (make-hash-table :test #'equal))
  (current (make-hash-table :test #'equal)))

(defun klt-controls (tr)
  (list :capture (klt-capture tr) :feedback-hops (klt-feedback-hops tr) :store-max-bytes (klt-store-max-bytes tr)))

(defun kl-stamp (tr lamport step commit)
  (obj "instance" (klt-instance tr) "lamport" lamport "commit" commit "step" step))

(defun active-entry< (a b)
  (destructuring-bind (ma sa ea &rest _) a
    (declare (ignore _))
    (destructuring-bind (mb sb eb &rest _) b
      (declare (ignore _))
      (cond ((string/= ma mb) (octets< ma mb))
            ((string/= sa sb) (octets< sa sb))
            (t (octets< ea eb))))))

(defun actives-json (entries)
  (vectorize (mapcar (lambda (e)
                       (destructuring-bind (m s ev jm) e
                         (obj "machine" m "sequence" s "event" ev "justMatched" (json-bool jm))))
                     (sort (copy-list entries) #'active-entry<))))

(defun machine-actives (machine)
  (let (out)
    (maphash (lambda (sid seq)
               (maphash (lambda (eid ev)
                          (when (reality-event-active-p ev)
                            (push (list (machine-name machine) sid eid (and (reality-event-just-matched-p ev) t)) out)))
                        (sequence-vectors seq)))
             (machine-sequences machine))
    (sort out #'active-entry<)))

(defun machine-initial-actives (machine)
  (let (out)
    (maphash (lambda (sid seq)
               (maphash (lambda (eid ev)
                          (when (reality-event-initial-p ev) (push (list (machine-name machine) sid eid nil) out)))
                        (sequence-vectors seq)))
             (machine-sequences machine))
    (sort out #'active-entry<)))

(defun region-overlap-p (a b)
  (and a b (plusp (region-length a)) (plusp (region-length b))
       (< (region-offset a) (+ (region-offset b) (region-length b)))
       (< (region-offset b) (+ (region-offset a) (region-length a)))))

(defun entry-projected (entry regions)
  "A trajectory ENTRY keeping only cells inside REGIONS (all, when REGIONS is empty)."
  (obj "stepNumber" (jget entry "stepNumber" 0) "length" (jget entry "length" 0)
       "nonZero" (vectorize
                  (remove-if-not (lambda (c)
                                   (let ((i (truncate (jnumber c "index" 0))))
                                     (or (null regions)
                                         (some (lambda (r) (and (>= i (region-offset r))
                                                                (< i (+ (region-offset r) (region-length r)))))
                                               regions))))
                                 (coerce (or (jget entry "nonZero") #()) 'list)))))

(defun region-obj (r) (obj "offset" (region-offset r) "length" (region-length r)))

(defun op-json (op)
  (destructuring-bind (machine region values &rest _) op
    (declare (ignore _))
    (obj "machine" machine "region" (region-obj region) "values" (vectorize values))))

(defun klt-record-for (tr step)
  (find step (klt-ring tr) :key #'kst-step))

(defun klt-active-at (tr machine step)
  (dolist (r (reverse (klt-ring tr)) (gethash machine (klt-baseline tr)))
    (when (<= (kst-step r) step)
      (multiple-value-bind (list present) (gethash machine (kst-changed r))
        (when present (return list))))))

(defun comparator-threshold (type element)
  (let ((th (vector-element-threshold element)))
    (cond ((member type '("equals" "custom") :test #'string=) +json-null+)
          ((string= type "threshold") (or th 0.1d0))
          (t (or th 0.5d0)))))

(defun kline-difference (event input region)
  "The first comparison of EVENT that failed against INPUT."
  (multiple-value-bind (ok score metadata) (match-reality-event event input)
    (declare (ignore ok score))
    (let* ((elements (reality-event-elements event))
           (idx (let ((i (and (hash-table-p metadata) (jget metadata "failedAtIndex"))))
                  (if (and i (< i (length elements))) (truncate i) 0)))
           (element (nth idx elements))
           (type (comparator-name (or (and element (vector-element-comparator element))
                                      (reality-event-match-algorithm event))))
           (canonical (if (string= type "custom") "equals" type)))
      (obj "expected" (vector (reality-event-id event))
           "elementIndex" idx
           "cell" (+ (region-offset region) idx)
           "value" (or (nth idx input) 0)
           "comparator" canonical
           "threshold" (if element (comparator-threshold type element) +json-null+)
           "cells" (vector (region-offset region) (+ (region-offset region) (region-length region) -1))))))

(defun path-entry (event step)
  (obj "event" (reality-event-id event) "step" step
       "milestone" (json-bool (and (reality-event-output-vectors event) (reality-event-next-ids event)))
       "outputs" (vectorize (mapcar (lambda (o) (vectorize (output-vector-vector o)))
                                    (reality-event-output-vectors event)))))

(defun kline-on-step (tr step commit composed merge-batch isre osre machines fingerprint)
  "A committed step: COMPOSED is machine id -> (input . transition-result)."
  (unless (klt-capture tr) (return-from kline-on-step))
  (setf (klt-fingerprint tr) fingerprint)
  (let ((by-name (make-hash-table :test #'equal))
        (results (make-hash-table :test #'equal)))
    (maphash (lambda (id m)
               (setf (gethash (machine-name m) by-name) m)
               (let ((c (gethash id composed))) (when c (setf (gethash (machine-name m) results) c))))
             machines)
    (when (and (null (klt-ring tr)) (zerop (hash-table-count (klt-current tr))))
      (maphash (lambda (name m) (setf (gethash name (klt-baseline tr)) (machine-initial-actives m))) by-name))
    ;; The step record, pushed before closes read it.
    (let ((rec (make-kl-step :step step :commit commit :isre isre :osre osre
                             :ops (mapcar (lambda (op)
                                            (list (jstring op "machineName" "")
                                                  (make-region-from-json (jget op "region"))
                                                  (coerce (or (jget op "values") #()) 'list)
                                                  (coerce (or (jget op "sequenceIds") #()) 'list)))
                                          merge-batch))))
      (maphash (lambda (name m)
                 (when (machine-mapping m)
                   (setf (gethash name (kst-regions rec))
                         (cons (mapping-input (machine-mapping m)) (mapping-output (machine-mapping m)))))
                 (let* ((now (machine-actives m))
                        (before (multiple-value-bind (l present) (gethash name (klt-current tr))
                                  (if present l (or (gethash name (klt-baseline tr)) (machine-initial-actives m))))))
                   (unless (equal now before) (setf (gethash name (kst-changed rec)) now))
                   (setf (gethash name (klt-current tr)) now)))
               by-name)
      (kline-boundaries tr rec by-name results step commit))))

(defun kline-boundaries (tr rec by-name results step commit)
  (let ((touches nil)       ; plists
        (opened nil)
        (holder (make-hash-table :test #'equal))
        (still-open (make-hash-table :test #'eq)))
    (dolist (k (klt-open tr))
      (setf (gethash k still-open) t)
      (dolist (e (klo-held k))
        (setf (gethash (list (klo-machine k) (klo-sequence k) e) holder) (klo-id k))))
    (dolist (s (klt-sustain tr))
      (setf (gethash (list (kls-machine s) (kls-sequence s) (kls-event s)) holder) (kls-id s)))
    (dolist (name (sort (alexandria:hash-table-keys by-name) #'octets<))
      (let* ((m (gethash name by-name))
             (composition (gethash name results)))
        (when composition
          (let* ((input (coerce (car composition) 'list))
                 (seq-results (transition-result-sequence-results (cdr composition)))
                 (in-region (if (machine-mapping m) (mapping-input (machine-mapping m)) (make-region :offset 0 :length 0)))
                 (out-region (if (machine-mapping m) (mapping-output (machine-mapping m)) (make-region :offset 0 :length 0))))
            (dolist (sid (sort (alexandria:hash-table-keys (machine-sequences m)) #'octets<))
              (let ((seq (gethash sid (machine-sequences m)))
                    (sr (gethash sid seq-results)))
                (when sr
                  (let* ((events (sequence-vectors seq))
                         (matched (coerce (or (jget sr "matchedEvents") #()) 'list))
                         (activated (coerce (or (jget sr "activatedEvents") #()) 'list))
                         (claimed (make-hash-table :test #'equal)))
                    (flet ((matched-p (e) (member e matched :test #'string=))
                           (activated-p (e) (member e activated :test #'string=))
                           (active-p (e) (let ((ev (gethash e events))) (and ev (reality-event-active-p ev)))))
                      ;; Existing K-lines of this sequence, older first.
                      (dolist (k (sort (remove-if-not (lambda (k) (and (string= (klo-machine k) name)
                                                                       (string= (klo-sequence k) sid)))
                                                      (klt-open tr))
                                       (lambda (a b)
                                         (let ((sa (truncate (jnumber (klo-open a) "step" 0)))
                                               (sb (truncate (jnumber (klo-open b) "step" 0))))
                                           (if (/= sa sb) (< sa sb) (octets< (klo-activator a) (klo-activator b)))))))
                        (let ((next nil) (completed-by nil) (first-failed nil) (milestones 0))
                          (dolist (e (klo-held k))
                            (let ((ev (gethash e events)))
                              (when ev
                                (cond
                                  ((matched-p e)
                                   (let ((p (path-entry ev step)))
                                     (setf (klo-path k) (append (klo-path k) (list p)))
                                     (when (eq (jget p "milestone") t) (incf milestones)))
                                   (if (null (reality-event-next-ids ev))
                                       (unless completed-by (setf completed-by e))
                                       (progn
                                         (dolist (n (reality-event-next-ids ev))
                                           (cond
                                             ((string= n e) (when (active-p n) (pushnew n next :test #'string=)))
                                             ((activated-p n)
                                              (multiple-value-bind (who present) (gethash n claimed)
                                                (if present
                                                    (when (string= (klo-refused-by k) "") (setf (klo-refused-by k) who))
                                                    (progn (setf (gethash n claimed) (klo-id k))
                                                           (pushnew n next :test #'string=)))))
                                             (t (let ((h (gethash (list name sid n) holder)))
                                                  (cond ((and h (string/= h (klo-id k)))
                                                         (when (string= (klo-refused-by k) "") (setf (klo-refused-by k) h)))
                                                        (h (pushnew n next :test #'string=)))))))
                                         (when (and (reality-event-output-vectors ev) (active-p e))
                                           (pushnew e next :test #'string=)))))
                                  ((null first-failed) (setf first-failed e))))))
                          (let ((touch (list :open k :machine name :sequence sid
                                             :open-step (truncate (jnumber (klo-open k) "step" 0))
                                             :milestones milestones)))
                            (cond
                              (completed-by
                               (setf (getf touch :outcome) "completed" (getf touch :through) step)
                               (push (make-kl-sustain :machine name :sequence sid :event completed-by :through step
                                                      :open-step (getf touch :open-step))
                                     (klt-sustain tr)))
                              ((null next)
                               (if (string/= (klo-refused-by k) "")
                                   (setf (getf touch :outcome) "subsumed" (getf touch :subsumed-by) (klo-refused-by k))
                                   (setf (getf touch :outcome) "abandoned"
                                         (getf touch :difference)
                                         (kline-difference (gethash (or first-failed (first (klo-held k))) events)
                                                           input in-region)))))
                            (setf (klo-held k) (sort next #'octets<))
                            (when (getf touch :outcome) (remhash k still-open))
                            (push touch touches))))
                      ;; Sustains of this sequence.
                      (dolist (s (klt-sustain tr))
                        (when (and (string= (kls-machine s) name) (string= (kls-sequence s) sid)
                                   (string/= (kls-id s) ""))
                          (if (matched-p (kls-event s))
                              (setf (kls-through s) step)
                              (progn
                                (push (list :sustain-end (kls-id s) :through (kls-through s) :machine name
                                            :sequence sid :open-step (kls-open-step s) :milestones 0)
                                      touches)
                                (setf (kls-through s) -1)))))
                      ;; A departure: the initial event matched.
                      (dolist (eid (sort (alexandria:hash-table-keys events) #'octets<))
                        (let ((ev (gethash eid events)))
                          (when (and (reality-event-initial-p ev) (matched-p eid)
                                     (notany (lambda (s) (and (string= (kls-machine s) name) (string= (kls-sequence s) sid)
                                                              (string= (kls-event s) eid) (= (kls-through s) step)))
                                             (klt-sustain tr)))
                            (let* ((p (path-entry ev step))
                                   (k (make-kl-open :machine name :sequence sid :activator eid
                                                    :open (kl-stamp tr 0 step commit) :held nil :path (list p)
                                                    :input in-region :output out-region))
                                   (touch (list :open k :new t :machine name :sequence sid :open-step step
                                                :milestones (if (eq (jget p "milestone") t) 1 0)))
                                   (departs t))
                              (if (null (reality-event-next-ids ev))
                                  (progn
                                    (setf (getf touch :outcome) "completed" (getf touch :through) step)
                                    (push (make-kl-sustain :machine name :sequence sid :event eid :through step :open-step step)
                                          (klt-sustain tr)))
                                  (progn
                                    (dolist (n (reality-event-next-ids ev))
                                      (when (and (string/= n eid) (activated-p n) (not (nth-value 1 (gethash n claimed))))
                                        (setf (gethash n claimed) "#new")
                                        (push n (klo-held k))))
                                    (setf (klo-held k) (sort (klo-held k) #'octets<))
                                    (unless (klo-held k) (setf departs nil))))
                              (when departs
                                (setf (klo-focal-active-before k) (actives-json (klt-active-at tr name (1- step))))
                                (push k opened)
                                (push touch touches)))))))))))))))
    (setf (klt-sustain tr) (remove-if (lambda (s) (minusp (kls-through s))) (klt-sustain tr)))
    (setf (klt-ring tr) (append (klt-ring tr) (list rec)))
    ;; Ticks, in record order.
    (setf touches (sort touches (lambda (a b)
                                  (cond ((string/= (getf a :machine) (getf b :machine)) (octets< (getf a :machine) (getf b :machine)))
                                        ((string/= (getf a :sequence) (getf b :sequence)) (octets< (getf a :sequence) (getf b :sequence)))
                                        ((/= (getf a :open-step) (getf b :open-step)) (< (getf a :open-step) (getf b :open-step)))
                                        (t (and (not (getf a :new)) (getf b :new)))))))
    (let ((lines nil))
      (dolist (touch touches)
        (if (getf touch :sustain-end)
            (push (obj "format" "re-kline-event/1" "kline" (getf touch :sustain-end) "event" "sustain-end"
                       "through" (getf touch :through)
                       "at" (kl-stamp tr (funcall (klt-tick tr)) step commit))
                  lines)
            (let ((k (getf touch :open)))
              (when (getf touch :new)
                (let ((l (funcall (klt-tick tr))))
                  (setf (jget (klo-open k) "lamport") l
                        (klo-id k) (format nil "~a:~d" (klt-instance tr) l))))
              (dotimes (i (getf touch :milestones)) (funcall (klt-tick tr)))
              ;; The pair of this step, projected onto the focal machine (a shutdown record).
              (setf (klo-focal-pairs k)
                    (append (klo-focal-pairs k)
                            (list (obj "stepNumber" step
                                       "isre" (entry-projected (kst-isre rec) (list (klo-input k)))
                                       "osre" (entry-projected (kst-osre rec) (list (klo-output k)))
                                       "contributions" (vectorize (mapcar #'op-json
                                                                          (remove-if-not (lambda (op) (string= (first op) (klo-machine k)))
                                                                                         (kst-ops rec))))))))
              (when (getf touch :outcome)
                (let ((close (kl-stamp tr (funcall (klt-tick tr)) step commit)))
                  (when (string= (getf touch :outcome) "completed")
                    (let ((s (find-if (lambda (s) (and (string= (kls-id s) "") (string= (kls-machine s) (klo-machine k))
                                                       (string= (kls-sequence s) (klo-sequence k))))
                                      (klt-sustain tr))))
                      (when s (setf (kls-id s) (klo-id k)))))
                  (setf (gethash (klo-id k) (klt-closed tr))
                        (list (klo-machine k) (klo-sequence k) (getf touch :open-step) step))
                  (push (kline-record tr k (getf touch :outcome) +json-null+ close (or (getf touch :difference) +json-null+)
                                      (getf touch :subsumed-by) (or (getf touch :through) step) t)
                        lines))))))
      ;; What asserted this step: contributor searches at later steps read it.
      (dolist (touch touches)
        (unless (getf touch :sustain-end)
          (let ((k (getf touch :open)))
            (when (or (plusp (getf touch :milestones)) (equal (getf touch :outcome) "completed"))
              (setf (gethash (list (klo-machine k) (klo-sequence k)) (kst-asserted rec)) (klo-id k))))))
      (dolist (s (klt-sustain tr))
        (when (and (= (kls-through s) step) (string/= (kls-id s) ""))
          (setf (gethash (list (kls-machine s) (kls-sequence s)) (kst-asserted rec)) (kls-id s))))
      (setf (klt-open tr)
            (append (remove-if-not (lambda (k) (gethash k still-open)) (klt-open tr))
                    (loop for touch in touches
                          when (and (getf touch :new) (null (getf touch :outcome))) collect (getf touch :open))))
      (setf (klt-last-step tr) step (klt-last-commit tr) commit)
      (kline-persist tr (nreverse lines))
      (kline-trim-ring tr))))

(defun kline-record (tr k outcome reason close difference subsumed-by sustain-through ring-p)
  (let ((scope (list (klo-machine k)))
        (contributors nil)
        (truncated nil)
        (pairs (vectorize (klo-focal-pairs k)))
        (active-before (klo-focal-active-before k))
        (active-after #())
        (shared nil)
        (open-step (truncate (jnumber (klo-open k) "step" 0)))
        (close-step (truncate (jnumber close "step" 0))))
    (if ring-p
        (let ((frontier (list (list (klo-machine k) open-step close-step)))
              (seen (list (klo-id k)))
              (found nil))
          (loop for hop from 1
                while frontier
                do (let (next)
                     (dolist (f frontier)
                       (destructuring-bind (fm from to) f
                         (let ((region (if (string= fm (klo-machine k))
                                           (klo-input k)
                                           (let ((r (klt-record-for tr to)))
                                             (and r (car (gethash fm (kst-regions r))))))))
                           (loop for s from from to to
                                 do (let ((prev (klt-record-for tr (1- s))))
                                      (if (null prev)
                                          (when (>= (1- s) 0) (setf truncated t))
                                          (dolist (op (kst-ops prev))
                                            (destructuring-bind (om oregion ovalues oseqs) op
                                              (declare (ignore ovalues))
                                              (when (and (string/= om fm) (region-overlap-p oregion region))
                                                (dolist (sq oseqs)
                                                  (let ((id (gethash (list om sq) (kst-asserted prev))))
                                                    (when (and id (not (member id seen :test #'string=)))
                                                      (if (> hop (klt-feedback-hops tr))
                                                          (setf truncated t)
                                                          (let ((c (gethash id (klt-closed tr)))
                                                                (cfrom (1- s)) (cto (1- s)))
                                                            (push id seen)
                                                            (when c (setf cfrom (third c) cto (fourth c)))
                                                            (push (list hop om cfrom id) found)
                                                            (push (list om cfrom cto) next)))))))))))))))
                     (setf frontier next)))
          (setf found (sort found (lambda (a b)
                                    (cond ((/= (first a) (first b)) (< (first a) (first b)))
                                          ((string/= (second a) (second b)) (octets< (second a) (second b)))
                                          ((/= (third a) (third b)) (< (third a) (third b)))
                                          (t (octets< (fourth a) (fourth b)))))))
          (dolist (f found)
            (push (obj "kline" (fourth f) "machine" (second f) "hops" (first f)) contributors)
            (push (second f) scope))
          (setf contributors (nreverse contributors)
                scope (remove-duplicates (sort scope #'octets<) :test #'string=))
          (let* ((last (klt-record-for tr close-step))
                 (inputs nil) (outputs nil))
            (when last
              (dolist (name scope)
                (let ((r (gethash name (kst-regions last))))
                  (when r (push (car r) inputs) (when (cdr r) (push (cdr r) outputs)))))
              (let ((cells nil))
                (dolist (o outputs)
                  (maphash (lambda (name rg)
                             (unless (or (member name scope :test #'string=) (null (cdr rg)))
                               (let ((x (cdr rg)))
                                 (loop for c from (max (region-offset o) (region-offset x))
                                         below (min (+ (region-offset o) (region-length o)) (+ (region-offset x) (region-length x)))
                                       do (pushnew c cells)))))
                           (kst-regions last)))
                (setf shared (sort cells #'<))))
            (let ((ps nil))
              (loop for s from open-step to close-step
                    do (let ((r (klt-record-for tr s)))
                         (when r
                           (push (obj "stepNumber" s
                                      "isre" (entry-projected (kst-isre r) inputs)
                                      "osre" (entry-projected (kst-osre r) outputs)
                                      "contributions"
                                      (vectorize (mapcar #'op-json
                                                         (sort (remove-if-not (lambda (op) (member (first op) scope :test #'string=))
                                                                              (copy-list (kst-ops r)))
                                                               (lambda (a b)
                                                                 (if (/= (region-offset (second a)) (region-offset (second b)))
                                                                     (< (region-offset (second a)) (region-offset (second b)))
                                                                     (octets< (first a) (first b))))))))
                                 ps))))
              (when ps (setf pairs (vectorize (nreverse ps)))))
            (let (before after)
              (dolist (name scope)
                (setf before (append before (klt-active-at tr name (1- open-step)))
                      after (append after (klt-active-at tr name close-step))))
              (setf active-before (actives-json before) active-after (actives-json after)))))
        (setf truncated t))
    (let ((completed (string= outcome "completed")))
      (obj "format" "re-kline/1"
           "id" (klo-id k)
           "corpus" (obj "algorithm" "sha256/resident-declared-v1" "fingerprint" (klt-fingerprint tr))
           "focal" (obj "machine" (klo-machine k) "sequence" (klo-sequence k))
           "outcome" outcome
           "reason" reason
           "open" (klo-open k)
           "close" close
           "path" (vectorize (klo-path k))
           "difference" difference
           "subsumedBy" (if (and subsumed-by (string/= subsumed-by "")) subsumed-by +json-null+)
           "sustain" (obj "through" (if completed sustain-through close-step) "ended" (json-bool (not completed)))
           "scope" (vectorize scope)
           "contributors" (vectorize contributors)
           "feedbackTruncated" (json-bool truncated)
           "sharedCells" (vectorize shared)
           "activeBefore" active-before
           "activeAfter" active-after
           "pairs" pairs
           "outputs" (obj "outputVector" +json-null+ "mergedOutputVector" +json-null+)))))

(defun kline-close-all (tr reason)
  "Close every open K-line `interrupted` at the last committed step, and end every sustain."
  (let ((lines nil)
        (step (max 0 (klt-last-step tr)))
        (commit (klt-last-commit tr)))
    (dolist (k (sort (copy-list (klt-open tr))
                     (lambda (a b) (cond ((string/= (klo-machine a) (klo-machine b)) (octets< (klo-machine a) (klo-machine b)))
                                         ((string/= (klo-sequence a) (klo-sequence b)) (octets< (klo-sequence a) (klo-sequence b)))
                                         (t (< (truncate (jnumber (klo-open a) "step" 0)) (truncate (jnumber (klo-open b) "step" 0))))))))
      (let ((close (kl-stamp tr (funcall (klt-tick tr)) step commit)))
        (push (kline-record tr k "interrupted" reason close +json-null+ nil step t) lines)))
    (dolist (s (sort (copy-list (klt-sustain tr))
                     (lambda (a b) (if (string/= (kls-machine a) (kls-machine b)) (octets< (kls-machine a) (kls-machine b))
                                       (octets< (kls-sequence a) (kls-sequence b))))))
      (unless (string= (kls-id s) "")
        (push (obj "format" "re-kline-event/1" "kline" (kls-id s) "event" "sustain-end" "through" (kls-through s)
                   "at" (kl-stamp tr (funcall (klt-tick tr)) step commit))
              lines)))
    (nreverse lines)))

(defun kline-on-reset (tr)
  (when (klt-capture tr)
    (let ((lines (kline-close-all tr "reset")))
      (setf (klt-open tr) nil (klt-sustain tr) nil)
      (kline-persist tr lines)
      (setf (klt-ring tr) nil (klt-last-step tr) -1)
      (clrhash (klt-baseline tr))
      (clrhash (klt-current tr)))))

(defun kline-on-machine-removed (tr machine-name)
  (when (klt-capture tr)
    (let* ((all-open (klt-open tr)) (all-sustain (klt-sustain tr))
           (lines (progn
                    (setf (klt-open tr) (remove-if-not (lambda (k) (string= (klo-machine k) machine-name)) all-open)
                          (klt-sustain tr) (remove-if-not (lambda (s) (string= (kls-machine s) machine-name)) all-sustain))
                    (kline-close-all tr "machine-removed"))))
      (setf (klt-open tr) (remove-if (lambda (k) (string= (klo-machine k) machine-name)) all-open)
            (klt-sustain tr) (remove-if (lambda (s) (string= (kls-machine s) machine-name)) all-sustain))
      (remhash machine-name (klt-current tr))
      (kline-persist tr lines))))

(defun kline-checkpoint-json (tr)
  (obj "format" "re-kline-open/1" "step" (klt-last-step tr) "commit" (klt-last-commit tr)
       "fingerprint" (klt-fingerprint tr)
       "open" (vectorize
               (mapcar (lambda (k)
                         (obj "id" (klo-id k) "machine" (klo-machine k) "sequence" (klo-sequence k)
                              "activator" (klo-activator k) "open" (klo-open k) "held" (vectorize (klo-held k))
                              "path" (vectorize (klo-path k)) "refusedBy" (klo-refused-by k)
                              "focalPairs" (vectorize (klo-focal-pairs k))
                              "focalActiveBefore" (klo-focal-active-before k)
                              "focalActiveAfter" (actives-json (gethash (klo-machine k) (klt-current tr)))
                              "input" (region-obj (klo-input k)) "output" (region-obj (klo-output k))))
                       (klt-open tr)))
       "sustain" (vectorize
                  (mapcar (lambda (s) (obj "id" (kls-id s) "machine" (kls-machine s) "sequence" (kls-sequence s)
                                           "event" (kls-event s) "through" (kls-through s)))
                          (klt-sustain tr)))))

(defun kline-persist (tr lines)
  (let ((store (klt-store tr)))
    (kstore-append store lines (klt-fingerprint tr) (klt-last-commit tr))
    (kstore-checkpoint store (kline-checkpoint-json tr))
    (kstore-enforce-budget store (klt-controls tr))))

(defun kline-recover (tr)
  "At boot: close every K-line in the checkpoint `interrupted` (shutdown) at its step."
  (let ((cp (kstore-read-checkpoint (klt-store tr))))
    (when (hash-table-p cp)
      (let ((step (truncate (jnumber cp "step" 0)))
            (commit (truncate (jnumber cp "commit" 0)))
            (lines nil))
        (setf (klt-fingerprint tr) (jstring cp "fingerprint" "")
              (klt-last-step tr) step (klt-last-commit tr) commit)
        (loop for o across (or (jget cp "open") #())
              do (let* ((k (make-kl-open :id (jstring o "id" "") :machine (jstring o "machine" "")
                                         :sequence (jstring o "sequence" "") :activator (jstring o "activator" "")
                                         :open (jget o "open") :path (coerce (jget o "path") 'list)
                                         :focal-pairs (coerce (jget o "focalPairs") 'list)
                                         :focal-active-before (jget o "focalActiveBefore")))
                        (rec (kline-record tr k "interrupted" "shutdown" (kl-stamp tr (funcall (klt-tick tr)) step commit)
                                           +json-null+ nil step nil)))
                   (setf (jget rec "activeAfter") (jget o "focalActiveAfter"))
                   (push rec lines)))
        (loop for s across (or (jget cp "sustain") #())
              do (when (plusp (length (jstring s "id" "")))
                   (push (obj "format" "re-kline-event/1" "kline" (jstring s "id" "") "event" "sustain-end"
                              "through" (jget s "through") "at" (kl-stamp tr (funcall (klt-tick tr)) step commit))
                         lines)))
        (setf (klt-open tr) nil (klt-sustain tr) nil)
        (kline-persist tr (nreverse lines))
        (setf (klt-last-step tr) -1)))))

(defun kline-start-capture (tr)
  "klineCapture turned on (at boot or at runtime): capture starts afresh, first
closing what the checkpoint holds, as at boot. Turning it off is setting
KLT-CAPTURE to nil, which leaves the store and its checkpoint untouched."
  (setf (klt-open tr) nil (klt-sustain tr) nil (klt-ring tr) nil (klt-last-step tr) -1
        (klt-capture tr) t)
  (clrhash (klt-closed tr))
  (clrhash (klt-baseline tr))
  (clrhash (klt-current tr))
  (kline-recover tr))

(defun kline-trim-ring (tr)
  (let ((keep-from (- (klt-last-step tr) (max 1 (* (klt-longest-path tr) (1+ (klt-feedback-hops tr)))))))
    (dolist (k (klt-open tr))
      (setf keep-from (min keep-from (1- (truncate (jnumber (klo-open k) "step" 0))))))
    (loop while (and (klt-ring tr) (< (kst-step (first (klt-ring tr))) keep-from))
          do (let ((r (pop (klt-ring tr))))
               (maphash (lambda (name list) (setf (gethash name (klt-baseline tr)) list)) (kst-changed r))))
    (maphash (lambda (id c) (when (< (fourth c) (1- keep-from)) (remhash id (klt-closed tr))))
             (klt-closed tr))))

;;; ── Routes' bodies ────────────────────────────────────────────────────────

(defun kline-open-json (tr)
  (obj "commit" (klt-last-commit tr)
       "open" (vectorize
               (mapcar (lambda (k)
                         (obj "id" (klo-id k) "focal" (obj "machine" (klo-machine k) "sequence" (klo-sequence k))
                              "open" (klo-open k) "path" (vectorize (klo-path k)) "held" (vectorize (klo-held k))))
                       (sort (copy-list (klt-open tr))
                             (lambda (a b)
                               (cond ((string/= (klo-machine a) (klo-machine b)) (octets< (klo-machine a) (klo-machine b)))
                                     ((string/= (klo-sequence a) (klo-sequence b)) (octets< (klo-sequence a) (klo-sequence b)))
                                     (t (< (truncate (jnumber (klo-open a) "step" 0))
                                           (truncate (jnumber (klo-open b) "step" 0)))))))))))

(defun kline-list (tr outcome machine sequence since limit)
  (let* ((limit (min 1000 (max 1 (if (and limit (plusp limit)) limit 100))))
         (sel (loop for m across (kstore-metas (klt-store tr))
                    when (and (>= (kmeta-close-commit m) since)
                              (or (null outcome) (string= (kmeta-outcome m) outcome))
                              (or (null machine) (string= (kmeta-machine m) machine))
                              (or (null sequence) (string= (kmeta-sequence m) sequence)))
                      collect m))
         (out nil) (count 0) (next +json-null+))
    ;; A page holds whole commits.
    (loop while sel
          do (let* ((c (kmeta-close-commit (first sel)))
                    (group (loop while (and sel (= (kmeta-close-commit (first sel)) c)) collect (pop sel))))
               (when (and (plusp count) (> (+ count (length group)) limit))
                 (setf next c)
                 (return))
               (dolist (m group) (push (kstore-record (klt-store tr) m) out) (incf count))
               (when (and (>= count limit) sel)
                 (setf next (kmeta-close-commit (first sel)))
                 (return))))
    (obj "klines" (vectorize (nreverse out)) "next" next)))

(defun kline-get (tr id)
  (let ((i (gethash id (kstore-by-id (klt-store tr)))))
    (when i (kstore-record (klt-store tr) (aref (kstore-metas (klt-store tr)) i)))))

(defun kline-stats (tr) (kstore-stats (klt-store tr) (klt-controls tr)))

(defun kline-export (tr outcomes since machines)
  (let ((store (klt-store tr)) (count 0) (segments nil))
    (let ((body (with-output-to-string (s)
                  (loop for m across (kstore-metas store)
                        when (and (>= (kmeta-close-commit m) since)
                                  (or (null outcomes) (member (kmeta-outcome m) outcomes :test #'string=))
                                  (or (null machines) (member (kmeta-machine m) machines :test #'string=)))
                          do (write-string (kstore-raw store m) s) (write-char #\Newline s) (incf count)
                             (dolist (e (gethash (kmeta-id m) (kstore-events store)))
                               (write-string (cdr e) s) (write-char #\Newline s) (incf count))
                             (pushnew (kmeta-segment m) segments :test #'string=)))))
      (dolist (seg (kstore-segments store))
        (when (member (kseg-name seg) segments :test #'string=) (setf (kseg-exported seg) t)))
      (kstore-write-manifest store)
      (format nil "~a~%~a"
              (jcs (obj "format" "re-kline-bundle/1" "source" (obj "instance" (klt-instance tr))
                        "corpus" (obj "algorithm" "sha256/resident-declared-v1" "fingerprint" (klt-fingerprint tr))
                        "controls" (obj "klineCapture" (json-bool (klt-capture tr))
                                        "klineFeedbackHops" (klt-feedback-hops tr)
                                        "klineStoreMaxBytes" (klt-store-max-bytes tr))
                        "count" count "sha256" (sha256-hex body)))
              body))))

;;; ── The engine's tracker ──────────────────────────────────────────────────

(defun kline-longest-path (machines)
  (let ((longest 1))
    (maphash (lambda (_ m)
               (declare (ignore _))
               (maphash (lambda (_ seq)
                          (declare (ignore _))
                          (let ((events (sequence-vectors seq)))
                            (labels ((walk (id seen)
                                       (let ((ev (gethash id events)))
                                         (if (null ev) 0
                                             (1+ (loop for n in (reality-event-next-ids ev)
                                                       unless (member n seen :test #'string=)
                                                         maximize (walk n (cons id seen))))))))
                              (maphash (lambda (eid ev)
                                         (when (reality-event-initial-p ev)
                                           (setf longest (max longest (walk eid nil)))))
                                       events))))
                        (machine-sequences m)))
             machines)
    longest))

(defun make-engine-kline-tracker (instance tick)
  "The tracker for this engine: its store under <INSTANCE_KLINE_DIR>/<uuid>/ for an
allocated instance (the parent defaulting to the clock directory's sibling), in
memory for a minted one. The store opens whatever the klineCapture control says,
so the routes always answer from it; RE_KLINE_CAPTURE=1 sets capture's initial
value (the control's default is false until the PE half lands)."
  (let* ((allocated (string-downcase (or (env "INSTANCE_UUID" nil) "")))
         (capture (member (or (env "RE_KLINE_CAPTURE" nil) "") '("1" "true") :test #'string=))
         (dir (when (string= allocated instance)
                (merge-pathnames (format nil "~a/" instance)
                                 (uiop:ensure-directory-pathname
                                  (or (env "INSTANCE_KLINE_DIR" nil)
                                      (merge-pathnames "../klines/"
                                                       (uiop:ensure-directory-pathname
                                                        (or (env "INSTANCE_CLOCK_DIR" nil)
                                                            (merge-pathnames ".reality-engine/clock/" (user-homedir-pathname))))))))))
         (tracker (make-kline-tracker :instance instance :tick tick :store (open-kline-store (and dir (namestring dir))))))
    (when capture
      (format *error-output* "~&K-line capture on — store ~a~%" (if dir (namestring dir) "in memory"))
      (kline-start-capture tracker))
    tracker))
