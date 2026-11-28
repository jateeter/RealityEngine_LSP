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
    (assert-true (not (reality-engine-lsp::snapshot-store-enabled-p (reality-engine-lsp::snapshot-store-from-environment)))
                 "qdrant (phase 3) is off, never a boot failure")
    (sb-posix:setenv "RE_SNAPSHOT_STORE" (format nil "file:~a" dir) 1)
    (let ((store (reality-engine-lsp::snapshot-store-from-environment))
          (id (make-string 64 :initial-element #\b)))
      (reality-engine-lsp::snapshot-store-put store id (reality-engine-lsp::obj "digest" (format nil "sha256:~a" id)))
      (assert-true (reality-engine-lsp::snapshot-store-get store id) "a file store keeps a snapshot by id")
      (assert-true (null (reality-engine-lsp::snapshot-store-get store "../etc")) "an id is hex, never a path")
      (assert-true (reality-engine-lsp::snapshot-store-remove store id) "and drops it"))
    (sb-posix:unsetenv "RE_SNAPSHOT_STORE")
    (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore)))
