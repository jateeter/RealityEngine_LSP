(defpackage #:reality-engine-lsp
  (:use #:cl)
  (:export
   #:main
   #:start-reality-from-environment
   #:start-perception-from-environment
   #:start-reality-service
   #:start-perception-service
   #:load-machines-from-directory
   #:machine-from-json
   #:process-machine-input))

(defpackage #:reality-engine-lsp.tests
  (:use #:cl #:reality-engine-lsp)
  (:export #:run-tests))

