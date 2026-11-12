(in-package #:reality-engine-lsp.tests)

;;;; Lane TLS (RealityEngine_CI#668, RealityEngine_LSP#186).
;;;;
;;;; The runtime contract (RealityEngine_CI docs/TRANSPORT_SECURITY.md), the
;;;; same checks RealityEngine_CPP's tests/http_tls_tests.cpp makes: a throwaway
;;;; PKI from the openssl CLI in a temporary directory, an RE-like and a PE-like
;;;; server on loopback, the engine's own client, and s_client for refusals.
;;;; RE_TLS_* is set for the duration and removed afterwards.

(defun tls-openssl ()
  (or (find-if #'probe-file '("/opt/homebrew/opt/openssl@3/bin/openssl" "/usr/local/opt/openssl@3/bin/openssl"))
      "openssl"))

(defun tls-sh (command)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "/bin/sh" "-c" command) :output :string :error-output :string :ignore-error-status t)
    (values (concatenate 'string out err) code)))

(defun tls-free-port ()
  (let* ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
         (port (usocket:get-local-port s)))
    (usocket:socket-close s)
    port))

(defun tls-make-pki (dir)
  "A CA in the profile's shape, one instance leaf (server and client, localhost
and 127.0.0.1), a revoked client and a client from another CA."
  (let ((o (tls-openssl)))
    (flet ((file (name text) (with-open-file (s (format nil "~a/~a" dir name) :direction :output :if-exists :supersede)
                               (write-string text s)))
           (run (fmt &rest args)
             (multiple-value-bind (out code) (tls-sh (apply #'format nil fmt args))
               (unless (zerop code) (error "test PKI: ~a" out)))))
      (file "ca.cnf" (format nil "[ca]~%default_ca=d~%[d]~%dir=~a~%database=$dir/index.txt~%crlnumber=$dir/crlnumber~%default_md=sha384~%default_crl_days=2~%" dir))
      (file "index.txt" "")
      (file "crlnumber" (format nil "01~%"))
      (file "leaf.ext" (format nil "subjectAltName=DNS:localhost,IP:127.0.0.1~%extendedKeyUsage=serverAuth,clientAuth~%keyUsage=critical,digitalSignature~%"))
      (file "client.ext" (format nil "extendedKeyUsage=clientAuth~%keyUsage=critical,digitalSignature~%"))
      (dolist (ca '("ca" "rogue-ca"))
        (run "cd ~a && ~a req -x509 -newkey ec -pkeyopt ec_paramgen_curve:P-384 -nodes -sha384 -days 2 -subj /CN=~a -addext basicConstraints=critical,CA:TRUE -addext keyUsage=critical,keyCertSign,cRLSign -keyout ~a.key -out ~a.crt"
             dir o ca ca ca))
      (loop for (name issuer ext) in '(("lsp-1" "ca" "leaf.ext") ("revoked" "ca" "client.ext") ("rogue" "rogue-ca" "client.ext"))
            do (run "cd ~a && ~a req -newkey ec -pkeyopt ec_paramgen_curve:P-384 -nodes -subj /CN=~a -keyout ~a.key -out ~a.csr && ~a x509 -req -sha384 -days 2 -in ~a.csr -CA ~a.crt -CAkey ~a.key -CAcreateserial -extfile ~a -out ~a.crt && cat ~a.crt ~a.crt > ~a.chain"
                    dir o name name name o name issuer issuer ext name name issuer name))
      (run "cd ~a && ~a ca -config ca.cnf -keyfile ca.key -cert ca.crt -revoke revoked.crt && ~a ca -config ca.cnf -keyfile ca.key -cert ca.crt -gencrl -out ca.crl"
           dir o o))))

(defun tls-s-client (dir port path flags)
  "One request through s_client, bounded (an event stream never closes)."
  (tls-sh (format nil "( printf 'GET ~a HTTP/1.1\\r\\nHost: localhost\\r\\nConnection: close\\r\\n\\r\\n'; sleep 1 ) | perl -e 'alarm shift; exec @ARGV' 5 ~a s_client -quiet -no_ign_eof -connect 127.0.0.1:~a -servername localhost -CAfile ~a/ca.crt ~a"
                  path (tls-openssl) port dir flags)))

(defun tls-answered (out status)
  (search (format nil "HTTP/1.1 ~a" status) out))

(defun test-internal-host-rule ()
  (dolist (h '("127.0.0.1" "localhost" "engine-cpp-re" "cpp-1.localhost" "host.docker.internal" "10.0.0.5"
               "172.16.0.1" "172.31.255.1" "192.168.1.20" "169.254.1.1" "::1" "fd00::1" "fe80::1" "printer.local"))
    (assert-true (reality-engine-lsp::internal-host-p h) (format nil "~a is internal" h)))
  (dolist (h '("api.openai.com" "8.8.8.8" "172.32.0.1" "172.15.0.1" "2001:db8::1" ""))
    (assert-true (not (reality-engine-lsp::internal-host-p h)) (format nil "~a is external" h))))

(defun test-lane-tls ()
  (test-internal-host-rule)
  (let* ((dir (string-right-trim "/" (namestring (uiop:ensure-directory-pathname
                                                   (uiop:run-program "mktemp -d /tmp/re-lsp-tls-XXXXXX" :output '(:string :stripped t))))))
         (own (format nil "-cert ~a/lsp-1.chain -key ~a/lsp-1.key" dir dir))
         (re-port (tls-free-port))
         (pe-port (tls-free-port))
         (acceptors nil))
    (unwind-protect
         (progn
           (tls-make-pki dir)
           (sb-posix:setenv "RE_TLS_CERT" (format nil "~a/lsp-1.chain" dir) 1)
           (sb-posix:setenv "RE_TLS_KEY" (format nil "~a/lsp-1.key" dir) 1)
           (sb-posix:setenv "RE_TLS_CA" (format nil "~a/ca.crt" dir) 1)
           (sb-posix:setenv "RE_TLS_CRL" (format nil "~a/ca.crl" dir) 1)
           ;; An RE-like server: every route requires a client certificate.
           (push (reality-engine-lsp::start-http-server
                  re-port (list (reality-engine-lsp::make-route "GET" "/health"
                                                                (lambda (&rest _) (declare (ignore _))
                                                                  (reality-engine-lsp::json-response (reality-engine-lsp::obj "status" "ok"))))))
                 acceptors)
           (let ((base (format nil "https://127.0.0.1:~a" re-port)))
             (assert-equal "ok" (reality-engine-lsp::jget (reality-engine-lsp::http-get-json (format nil "~a/health" base)) "status")
                           "client: https to an IP SAN with the lane CA and its own certificate")
             (assert-equal "ok" (reality-engine-lsp::jget (reality-engine-lsp::http-get-json (format nil "https://localhost:~a/health" re-port)) "status")
                           "client: https to a DNS SAN")
             (assert-equal 10 (loop repeat 10 count (equal "ok" (reality-engine-lsp::jget (reality-engine-lsp::http-get-json (format nil "~a/health" base)) "status")))
                           "client: ten calls on pooled TLS connections"))
           (assert-true (tls-answered (tls-s-client dir re-port "/health" own) 200) "serve: a lane client certificate is served")
           (assert-true (not (tls-answered (tls-s-client dir re-port "/health" "") 200)) "serve: no client certificate is refused")
           (assert-true (not (tls-answered (tls-s-client dir re-port "/health" (format nil "~a -tls1_2" own)) 200)) "serve: TLS 1.2 is refused")
           (assert-true (not (tls-answered (tls-s-client dir re-port "/health" (format nil "~a -tls1_3 -ciphersuites TLS_AES_128_GCM_SHA256" own)) 200))
                        "serve: a suite outside the profile is refused")
           (assert-true (not (tls-answered (tls-s-client dir re-port "/health" (format nil "-cert ~a/rogue.chain -key ~a/rogue.key" dir dir)) 200))
                        "serve: a certificate from another CA is refused")
           (assert-true (not (tls-answered (tls-s-client dir re-port "/health" (format nil "-cert ~a/revoked.chain -key ~a/revoked.key" dir dir)) 200))
                        "serve: a revoked certificate is refused")
           ;; A resumed session is served: Node's https agent resumes TLS 1.3
           ;; sessions, and without a session id context OpenSSL refused every one
           ;; (RealityEngine_CI#729).
           (flet ((session (io)
                    (tls-sh (format nil "( printf 'GET /health HTTP/1.1\\r\\nHost: localhost\\r\\nConnection: close\\r\\n\\r\\n'; sleep 1 ) | perl -e 'alarm shift; exec @ARGV' 5 ~a s_client -no_ign_eof -connect 127.0.0.1:~a -servername localhost -CAfile ~a/ca.crt ~a ~a ~a/session.pem"
                                    (tls-openssl) re-port dir own io dir))))
             (session "-sess_out")
             (let ((again (session "-sess_in")))
               (assert-true (and (search "Reused, TLSv1.3" again) (tls-answered again 200))
                            "serve: a resumed TLS 1.3 session is served")))
           (let ((brief (tls-s-client dir re-port "/health" (format nil "~a -brief" own))))
             (assert-true (or (search "TLS_AES_256_GCM_SHA384" brief) (search "TLS_CHACHA20_POLY1305_SHA256" brief))
                          "serve: the negotiated suite is in the profile"))
           ;; A PE-like server: one device route open without a certificate.
           (push (reality-engine-lsp::start-http-server
                  pe-port (list (reality-engine-lsp::make-route "GET" "/device" (lambda (&rest _) (declare (ignore _)) (reality-engine-lsp::json-response (reality-engine-lsp::obj "device" t))))
                                (reality-engine-lsp::make-route "GET" "/service" (lambda (&rest _) (declare (ignore _)) (reality-engine-lsp::json-response (reality-engine-lsp::obj "service" t)))))
                  :certificate-optional-routes '("/device"))
                 acceptors)
           (assert-true (tls-answered (tls-s-client dir pe-port "/device" "") 200) "device route: served without a client certificate")
           (let ((out (tls-s-client dir pe-port "/service" "")))
             (assert-true (and (tls-answered out 403) (search "client certificate required" out)) "device route: other routes answer 403 without one"))
           (assert-true (tls-answered (tls-s-client dir pe-port "/service" own) 200) "device route: other routes serve a lane client")
           (assert-true (not (tls-answered (tls-s-client dir pe-port "/device" (format nil "-cert ~a/rogue.chain -key ~a/rogue.key" dir dir)) 200))
                        "device route: a presented untrusted certificate is still refused")
           (format t "~&lane TLS tests passed~%"))
      (dolist (a acceptors) (ignore-errors (hunchentoot:stop a :soft nil)))
      (setf reality-engine-lsp::*tls-certificate-optional-routes* nil)
      (dolist (v '("RE_TLS_CERT" "RE_TLS_KEY" "RE_TLS_CA" "RE_TLS_CRL")) (sb-posix:unsetenv v))
      (clrhash reality-engine-lsp::*tls-client-contexts*)
      (clrhash reality-engine-lsp::*tls-pool*)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t :if-does-not-exist :ignore))))
