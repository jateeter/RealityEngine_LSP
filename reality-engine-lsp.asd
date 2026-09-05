(asdf:defsystem #:reality-engine-lsp
  :description "Common Lisp black-box equivalent runtime for RealityEngine_CPP and RealityEngine_Scala."
  :author "Reality Engine contributors"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:babel
               #:bordeaux-threads
               ;; lparallel layers on bordeaux-threads, already required above,
               ;; so this is not a second concurrency substrate — it is the
               ;; queue, kernel and parallel-map that src/actor.lisp previously
               ;; hand-rolled from locks and condition variables
               ;; (RealityEngine_LSP#92, ARBITER_CONTRACT.md 7.4).
               #:lparallel
               #:cl-base64
               #:hunchentoot
               #:drakma
               #:usocket
               #:yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "json")
                 (:file "util")
                 (:file "actor")
                 (:file "model")
                 (:file "loader")
                 (:file "perception")
                 (:file "http")
                 (:file "ws")           ; SSE broadcast surface (GET /api/events)
                 (:file "arbiter")       ; output arbiter (ARBITER_CONTRACT.md)
                 (:file "reality-service")
                 (:file "vector-aggregator") ; PE machine output aggregator
                 (:file "perception-service")
                 (:file "cli")          ; CLI shim (pe <command> mirrors _CPP reality_engine_cli)
                 (:file "mcp")          ; MCP JSON-RPC shim (POST/GET/DELETE /mcp)
                 (:file "mqtt-mapping")
                 (:file "mqtt-client")
                 (:file "mqtt-bridge")
                 (:file "main")))))

(asdf:defsystem #:reality-engine-lsp/tests
  :description "Smoke tests for RealityEngine_LSP."
  :serial t
  :depends-on (#:reality-engine-lsp)
  :components ((:module "tests"
                :components ((:file "core-tests")
                             (:file "oracle-parity-tests"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call '#:reality-engine-lsp.tests '#:run-tests)))

