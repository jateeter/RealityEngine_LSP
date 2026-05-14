(defpackage #:reality-engine-lsp.tests
  (:use #:cl #:reality-engine-lsp))

(in-package #:reality-engine-lsp.tests)

(defun assert-true (value message)
  (unless value
    (error "Assertion failed: ~a" message)))

(defun run-tests ()
  (let* ((machine-json (reality-engine-lsp::obj
                       "id" "machine-test"
                       "name" "Test"
                       "arbiterRule" "passthrough"
                       "perceptualMapping" (reality-engine-lsp::obj
                                            "input" (reality-engine-lsp::obj "offset" 0 "length" 1)
                                            "output" (reality-engine-lsp::obj "offset" 1 "length" 1))
                       "sequences" (reality-engine-lsp::vectorize
                                    (list
                                     (reality-engine-lsp::obj
                                      "id" "seq"
                                      "name" "Seq"
                                      "vectors" (reality-engine-lsp::vectorize
                                                 (list
                                                  (reality-engine-lsp::obj
                                                   "id" "v1"
                                                   "isInitial" t
                                                   "elements" (reality-engine-lsp::vectorize
                                                               (list (reality-engine-lsp::obj "value" 1)))
                                                   "outputVectors" (reality-engine-lsp::vectorize
                                                                    (list
                                                                     (reality-engine-lsp::obj
                                                                      "id" "out"
                                                                      "vector" (reality-engine-lsp::vectorize (list 1)))))))))))))
         (machine (machine-from-json machine-json))
         (result (process-machine-input machine (list 1))))
    (assert-true (reality-engine-lsp::transition-result-machine-output result)
                 "machine output should fire on matching initial vector"))
  (format t "~&RealityEngine_LSP core tests passed.~%")
  t)

