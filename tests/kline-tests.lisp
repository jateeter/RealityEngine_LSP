(in-package #:reality-engine-lsp.tests)

;;; K-line capture (RealityEngine_CI#603, RealityEngine_LSP#172): the same cases
;;; as RealityEngine_CPP tests/kline_tests.cpp, on a state built from fixture
;;; machines and stepped through process-perceptual-input.

(defun kline-event-json (id values initial next output)
  (format nil "{\"id\":\"~a\",\"elements\":[~{{\"value\":~a,\"threshold\":0.5}~^,~}],\"isInitial\":~a~a~a}"
          id values (if initial "true" "false")
          (if next (format nil ",\"nextEventIds\":[~{\"~a\"~^,~}]" next) "")
          (if output (format nil ",\"outputEvents\":[{\"id\":\"~a-out\",\"vector\":[1,1]}]" id) "")))

(defun kline-machine-json (name in out seq events)
  (format nil "{\"version\":\"1.0.0\",\"machine\":{\"name\":\"~a\",\"description\":\"kline test\",\"arbiterRule\":\"PASSTHROUGH\",\"matchAlgorithm\":\"gte\",\"perceptualMapping\":{\"input\":{\"offset\":~d,\"length\":2},\"output\":{\"offset\":~d,\"length\":2}},\"sequences\":[{\"id\":\"~a\",\"name\":\"~a\",\"events\":[~{~a~^,~}]}]}}"
          name in out seq seq events))

(defun kline-state (machines &key dir)
  "A state stepping MACHINES (JSON strings), with K-line capture on."
  (let ((corpus (uiop:ensure-directory-pathname
                 (format nil "~a/kline-corpus-~d-~d/" (uiop:temporary-directory) (sb-posix:getpid) (random 1000000)))))
    (ensure-directories-exist corpus)
    (loop for json in machines for i from 0
          do (with-open-file (out (merge-pathnames (format nil "M~d.json" i) corpus) :direction :output :if-exists :supersede)
               (write-string json out)))
    (let ((st (reality-engine-lsp::make-reality-state-from-config :machine-dir (namestring corpus) :dimension 32))
          (lamport 1000))
      (setf (reality-engine-lsp::reality-state-klines st)
            (reality-engine-lsp::make-kline-tracker
             :instance "01a218ae-9950-76ed-aded-d4cf80228cf4" :capture t
             :tick (lambda () (incf lamport))
             :store (reality-engine-lsp::open-kline-store (and dir (namestring dir)))))
      (uiop:delete-directory-tree corpus :validate t)
      st)))

(defun kline-step (st cells)
  (let ((v (make-list (reality-engine-lsp::reality-state-dimension st) :initial-element 0)))
    (loop for (i . x) in cells do (setf (nth i v) x))
    (reality-engine-lsp::process-perceptual-input st v)))

(defun kline-records (st)
  (coerce (reality-engine-lsp::jget (reality-engine-lsp::kline-list (reality-engine-lsp::reality-state-klines st)
                                                                    nil nil nil 0 1000)
                                    "klines")
          'list))

(defun kline-find (records machine outcome open-step)
  (find-if (lambda (r)
             (and (string= (reality-engine-lsp::jstring (reality-engine-lsp::jget r "focal") "machine" "") machine)
                  (string= (reality-engine-lsp::jstring r "outcome" "") outcome)
                  (= (reality-engine-lsp::jnumber (reality-engine-lsp::jget r "open") "step" -1) open-step)))
           records))

(defun kline-path (record)
  (map 'list (lambda (p) (format nil "~a@~d" (reality-engine-lsp::jstring p "event" "")
                                 (truncate (reality-engine-lsp::jnumber p "step" 0))))
       (reality-engine-lsp::jget record "path")))

(defparameter *kline-linear*
  (kline-machine-json "Linear" 0 10 "lin"
                      (list (kline-event-json "e0" '(1 0) t '("e1") nil)
                            (kline-event-json "e1" '(0 1) nil '("e2") nil)
                            (kline-event-json "e2" '(1 1) nil nil t))))

(defun test-kline ()
  (assert-equal "{\"a\":[true,null,\"x\\u0001\"],\"b\":1,\"c\":1.5e-7}"
                (reality-engine-lsp::jcs (reality-engine-lsp::parse-json "{\"b\":1,\"a\":[true,null,\"x\\u0001\"],\"c\":1.5e-7}"))
                "JCS orders keys and keeps the minimal escapes")
  (assert-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                (reality-engine-lsp::sha256-hex "abc") "SHA-256 of abc")
  ;; linear: completed, sustain, sustain-end, abandoned
  (let ((st (kline-state (list *kline-linear*))))
    (kline-step st '((0 . 1)))
    (kline-step st '((1 . 1)))
    (kline-step st '((0 . 1) (1 . 1)))
    (kline-step st '((0 . 1) (1 . 1)))
    (kline-step st '())
    (kline-step st '((0 . 1)))
    (kline-step st '((0 . 1) (1 . 1)))
    (let* ((recs (kline-records st))
           (c (kline-find recs "Linear" "completed" 0))
           (a (kline-find recs "Linear" "abandoned" 5)))
      (assert-true c "a completed K-line opened at step 0")
      (assert-equal '("e0@0" "e1@1" "e2@2") (kline-path c) "its path is the episode")
      (assert-equal 3 (reality-engine-lsp::jnumber (reality-engine-lsp::jget c "sustain") "through" 0) "sustain through step 3")
      (assert-true a "the second departure is abandoned")
      (let ((d (reality-engine-lsp::jget a "difference")))
        (assert-equal 0 (reality-engine-lsp::jnumber d "elementIndex" -1) "element 0 failed")
        (assert-equal "gte" (reality-engine-lsp::jstring d "comparator" "") "under gte"))))
  ;; one-event: open and complete on one step; consecutive matches are sustain
  (let ((st (kline-state (list (kline-machine-json "One" 2 12 "one" (list (kline-event-json "o0" '(1 1) t nil t)))))))
    (kline-step st '((2 . 1) (3 . 1)))
    (kline-step st '((2 . 1) (3 . 1)))
    (kline-step st '())
    (kline-step st '((2 . 1) (3 . 1)))
    (let ((recs (kline-records st)))
      (assert-equal 2 (length recs) "the sustain did not open a third K-line")
      (assert-true (kline-find recs "One" "completed" 3) "the next opens after a step without a match")))
  ;; merge: the younger K-line is subsumed by the older
  (let ((st (kline-state (list (kline-machine-json "Merge" 6 16 "mg"
                                                   (list (kline-event-json "m0" '(1 0) t '("m1") nil)
                                                         (kline-event-json "m1" '(1 0) nil '("m2") nil)
                                                         (kline-event-json "m2" '(1 0) nil nil t)))))))
    (dotimes (i 3) (kline-step st '((6 . 1))))
    (let* ((recs (kline-records st))
           (older (kline-find recs "Merge" "completed" 0))
           (younger (kline-find recs "Merge" "subsumed" 1)))
      (assert-true older "the older completes")
      (assert-true younger "the younger is subsumed")
      (when (and older younger)
        (assert-equal (reality-engine-lsp::jstring older "id" "") (reality-engine-lsp::jstring younger "subsumedBy" "")
                      "subsumedBy names the older"))))
  ;; reset, and the store across a restart
  (let* ((dir (format nil "~a/kline-store-~d/" (uiop:temporary-directory) (sb-posix:getpid)))
         (st (kline-state (list *kline-linear*) :dir dir)))
    (kline-step st '((0 . 1)))
    (kline-step st '((1 . 1)))
    (kline-step st '((0 . 1) (1 . 1)))
    (kline-step st '((0 . 1)))
    (let* ((lamport 5000)
           (tr (reality-engine-lsp::make-kline-tracker
                :instance "01a218ae-9950-76ed-aded-d4cf80228cf4" :capture t :tick (lambda () (incf lamport))
                :store (reality-engine-lsp::open-kline-store dir))))
      (reality-engine-lsp::kline-recover tr)
      (let ((recs (coerce (reality-engine-lsp::jget (reality-engine-lsp::kline-list tr nil nil nil 0 1000) "klines") 'list)))
        (assert-equal 2 (length recs) "the completed K-line survived and the open one was closed")
        (assert-equal "shutdown" (reality-engine-lsp::jstring (kline-find recs "Linear" "interrupted" 3) "reason" "")
                      "closed interrupted, reason shutdown")))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t))
  (let ((st (kline-state (list *kline-linear*))))
    (kline-step st '((0 . 1)))
    (reality-engine-lsp::reset-reality-state st)
    (assert-equal "reset" (reality-engine-lsp::jstring (kline-find (kline-records st) "Linear" "interrupted" 0) "reason" "")
                  "a reset closes it interrupted")))
