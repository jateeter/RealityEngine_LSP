(asdf:defsystem #:reality-engine-lsp
  :description "Common Lisp black-box equivalent runtime for RealityEngine_AI and RealityEngine_CPP."
  :author "Reality Engine contributors"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:babel
               #:bordeaux-threads
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
                 (:file "reality-service")
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
                :components ((:file "core-tests"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call '#:reality-engine-lsp.tests '#:run-tests)))

