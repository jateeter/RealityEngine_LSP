(in-package #:reality-engine-lsp.tests)

;;; Runtime snapshots (RealityEngine_CI#287): capture, the digest, a round trip
;;; through restore, and the store's modes. The same checks as
;;; RealityEngine_CPP tests/snapshot_tests.cpp.

(defun snap-capture (st body)
  (let ((r (reality-engine-lsp::snapshot-route-capture st body)))
    (if (= 200 (first r))
        (reality-engine-lsp::snapshot-store-get (reality-engine-lsp::reality-state-snapshot-store st)
                                                (reality-engine-lsp::jget (second r) "id"))
        (first r))))

(defun test-snapshot ()
  (let ((st (kline-state (list *kline-linear*))))
    (kline-step st '((0 . 1)))
    (kline-step st '((0 . 1) (1 . 1)))
    (kline-step st '((0 . 1)))
    (let ((s (snap-capture st (reality-engine-lsp::obj))))
      (assert-equal "re-snapshot/1" (reality-engine-lsp::jget s "format") "a re-snapshot/1 body")
      (assert-equal 2 (reality-engine-lsp::jget (reality-engine-lsp::jget (reality-engine-lsp::jget s "kline") "span") "to")
                    "the span defaults to the newest committed step")
      (assert-equal (reality-engine-lsp::jget s "digest") (reality-engine-lsp::snapshot-digest s) "the digest is over the body")
      (assert-equal 410 (snap-capture st (reality-engine-lsp::obj "from" 0 "to" 9)) "a span beyond the retained steps is 410")
      ;; Round trip: reset, restore, and the active list and the step count come back.
      (let ((after (reality-engine-lsp::jcs (reality-engine-lsp::jget (reality-engine-lsp::jget s "kline") "activeAfter"))))
        (reality-engine-lsp::reset-reality-state st)
        (let ((r (reality-engine-lsp::snapshot-route-restore st (reality-engine-lsp::obj "snapshot" s))))
          (assert-equal 200 (first r) "restore answers 200"))
        (assert-equal after
                      (reality-engine-lsp::jcs (reality-engine-lsp::actives-json
                                                (reality-engine-lsp::snapshot-all-actives
                                                 (reality-engine-lsp::reality-state-machines st))))
                      "restore puts the active list back exactly")
        (assert-equal 3 (reality-engine-lsp::reality-state-step-count st) "and the next step is span.to + 1"))
      ;; A tampered body is refused.
      (let ((bad (reality-engine-lsp::parse-json (reality-engine-lsp::jcs s))))
        (setf (gethash "machineOverrides" bad) (reality-engine-lsp::obj))
        (assert-equal 422 (first (reality-engine-lsp::snapshot-route-restore st (reality-engine-lsp::obj "snapshot" bad)))
                      "a digest that does not match the body is 422"))))
  (let ((dir (format nil "~a/snap-~d/" (uiop:temporary-directory) (sb-posix:getpid))))
    (sb-posix:setenv "RE_SNAPSHOT_STORE" "off" 1)
    (assert-true (not (reality-engine-lsp::snapshot-store-enabled-p (reality-engine-lsp::snapshot-store-from-environment)))
                 "RE_SNAPSHOT_STORE=off disables the store")
    (sb-posix:setenv "RE_SNAPSHOT_STORE" "qdrant:https://example.invalid" 1)
    (assert-true (reality-engine-lsp::snapshot-store-qdrant-p (reality-engine-lsp::snapshot-store-from-environment))
                 "qdrant:<https url> is the Qdrant store (#900)")
    (let ((st (reality-engine-lsp::snapshot-store-from-value "qdrant:http://qdrant.invalid:4333"))) ; insecure-transport: negative-test
      (assert-true (and (not (reality-engine-lsp::snapshot-store-enabled-p st)) (reality-engine-lsp::snps-off-reason st))
                   "a plaintext Qdrant URL leaves the store off, with the reason"))
    (assert-true (not (reality-engine-lsp::snapshot-store-enabled-p (reality-engine-lsp::snapshot-store-from-value "redis:x")))
                 "an unknown store is off, never a boot failure")
    (let ((st (reality-engine-lsp::snapshot-store-from-value "qdrant:https://127.0.0.1:1")))
      (assert-true (handler-case (progn (reality-engine-lsp::snapshot-store-list st) nil)
                     (reality-engine-lsp::snapshot-store-unavailable () t))
                   "an unreachable Qdrant is SNAPSHOT-STORE-UNAVAILABLE (503), not a boot failure"))
    (sb-posix:setenv "RE_SNAPSHOT_STORE" (format nil "file:~a" dir) 1)
    (let ((store (reality-engine-lsp::snapshot-store-from-environment))
          (id (make-string 64 :initial-element #\b)))
      (reality-engine-lsp::snapshot-store-put store id (reality-engine-lsp::obj "digest" (format nil "sha256:~a" id)))
      (assert-true (reality-engine-lsp::snapshot-store-get store id) "a file store keeps a snapshot by id")
      (assert-true (null (reality-engine-lsp::snapshot-store-get store "../etc")) "an id is hex, never a path")
      (assert-true (reality-engine-lsp::snapshot-store-remove store id) "and drops it"))
    (sb-posix:unsetenv "RE_SNAPSHOT_STORE")
    (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore))
  ;; Nearest (#900): the state at span.to, the sparse dot product, score > 0,
  ;; highest first, then id.
  (flet ((body (c lamport cells)
           (let ((o #'reality-engine-lsp::obj) (v #'reality-engine-lsp::vectorize))
             (funcall o "digest" (format nil "sha256:~a" (make-string 64 :initial-element c))
                      "corpus" (funcall o "machines" 1)
                      "kline" (funcall o "scope" "corpus" "span" (funcall o "from" 2 "to" 3)
                                       "clocks" (funcall o "to" (funcall o "lamport" lamport))
                                       "pairs" (vector (funcall o "stepNumber" 3
                                                                "isre" (funcall o "length" 8 "nonZero"
                                                                                (funcall v (loop for (i . x) in cells
                                                                                                 collect (funcall o "index" i "value" x)))))
                                                       (funcall o "stepNumber" 2
                                                                "isre" (funcall o "length" 8 "nonZero"
                                                                                (vector (funcall o "index" 7 "value" 9))))))))))
    (let ((st (reality-engine-lsp::snapshot-store-from-value "memory"))
          (a (body #\a 3 '((1 . 1.0) (2 . 1.0)))) (b (body #\b 2 '((1 . 1.0) (2 . 1.0))))
          (c (body #\c 1 '((1 . 0.5)))) (d (body #\d 4 '((5 . 1.0)))))
      (dolist (x (list a b c d)) (reality-engine-lsp::snapshot-store-put st (reality-engine-lsp::snapshot-id x) x))
      (let ((sa (reality-engine-lsp::snapshot-state-of a)))
        (assert-true (and (= 2 (hash-table-count sa)) (null (gethash 7 sa))) "a snapshot's state is its ISRE at span.to"))
      (let* ((q (reality-engine-lsp::snapshot-parse-state
                 (reality-engine-lsp::parse-json "{\"length\":8,\"nonZero\":[{\"index\":1,\"value\":1},{\"index\":2,\"value\":1}]}")))
             (n (gethash "snapshots" (reality-engine-lsp::snapshot-store-nearest st q 5))))
        (assert-true q "a {length, nonZero} query parses")
        (assert-true (null (reality-engine-lsp::snapshot-parse-state (reality-engine-lsp::parse-json "{\"nonZero\":\"x\"}")))
                     "a malformed one does not")
        (assert-equal 3 (length n) "only snapshots scoring above 0")
        (assert-true (and (equal (gethash "id" (aref n 0)) (make-string 64 :initial-element #\a))
                          (equal (gethash "id" (aref n 1)) (make-string 64 :initial-element #\b)))
                     "equal scores ordered by id")
        (assert-true (and (= 2 (gethash "score" (aref n 0))) (= 0.5 (gethash "score" (aref n 2)))) "the dot product")
        (assert-equal 1 (length (gethash "snapshots" (reality-engine-lsp::snapshot-store-nearest st q 1))) "at most limit"))
      (let ((l (gethash "snapshots" (reality-engine-lsp::snapshot-store-list st))))
        (assert-true (and (equal (gethash "id" (aref l 0)) (make-string 64 :initial-element #\c))
                          (equal (gethash "id" (aref l 3)) (make-string 64 :initial-element #\d)))
                     "list: oldest first by clocks.to.lamport")))))
