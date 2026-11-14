(in-package #:reality-engine-lsp)

;;;; Lane TLS (RealityEngine_CI#668, RealityEngine_LSP#186)
;;;;
;;;; The runtime contract is RealityEngine_CI docs/TRANSPORT_SECURITY.md:
;;;; RE_TLS_CERT, RE_TLS_KEY, RE_TLS_CA, RE_TLS_CRL. With RE_TLS_CERT set the RE
;;;; and PE serve TLS 1.3 only on their own port, with the profile's suites and
;;;; groups and a client certificate from the lane CA, and call internal peers
;;;; over https with the lane CA and their own certificate. With it unset nothing
;;;; here runs and the engine serves and calls exactly as before.
;;;;
;;;; cl+ssl covers the context basics. What it does not expose (the TLS 1.3
;;;; maximum, suites and groups, the CRL, OpenSSL's host and IP check) is set
;;;; through the OpenSSL functions cl+ssl has already loaded. C++ and Scala make
;;;; the same choices; a difference would read as an engine divergence.

(defparameter +tls-profile-suites+ "TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256")
(defparameter +tls-profile-groups-pq+ "X25519MLKEM768:X25519:secp384r1")
(defparameter +tls-profile-groups-fallback+ "X25519:secp384r1")

;; OpenSSL ctrl codes (ssl.h) and flags.
(defconstant +ssl-ctrl-set-groups-list+ 92)
(defconstant +ssl-ctrl-set-max-proto-version+ 124)
(defconstant +ssl-verify-fail-if-no-peer-cert+ 2)
(defconstant +x509-v-flag-crl-check+ 4)
(defconstant +x509-filetype-pem+ 1)

(defun tls-env (name)
  (let ((value (sb-posix:getenv name)))
    (and value (plusp (length value)) value)))

(defun tls-cert () (tls-env "RE_TLS_CERT"))
(defun tls-key () (tls-env "RE_TLS_KEY"))
(defun tls-ca () (tls-env "RE_TLS_CA"))
(defun tls-crl () (tls-env "RE_TLS_CRL"))

(defun tls-serving-p () (and (tls-cert) t))
(defun tls-lane-p () (and (tls-ca) t))

(defun tls-fail (format-control &rest args)
  (error "TLS: ~?" format-control args))

(defun ssl-ctx-ctrl (ctx cmd larg parg)
  (cffi:foreign-funcall "SSL_CTX_ctrl" :pointer ctx :int cmd :long larg :pointer parg :long))

(defun tls-apply-profile (ctx)
  "TLS 1.3 only, the profile's suites and groups. Returns the groups in force."
  (when (zerop (ssl-ctx-ctrl ctx +ssl-ctrl-set-max-proto-version+ cl+ssl::+tls1-3-version+ (cffi:null-pointer)))
    (tls-fail "cannot pin TLS 1.3"))
  (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_set_ciphersuites" :pointer ctx :string +tls-profile-suites+ :int))
    (tls-fail "cannot set the profile's cipher suites"))
  (flet ((try (groups)
           (cffi:with-foreign-string (s groups)
             (eql 1 (ssl-ctx-ctrl ctx +ssl-ctrl-set-groups-list+ 0 s)))))
    (cond ((try +tls-profile-groups-pq+) +tls-profile-groups-pq+)
          (t (cffi:foreign-funcall "ERR_clear_error" :void)
             (unless (try +tls-profile-groups-fallback+)
               (tls-fail "cannot set the profile's key-exchange groups"))
             +tls-profile-groups-fallback+))))

(defun tls-load-crl (ctx crl)
  (let* ((store (cffi:foreign-funcall "SSL_CTX_get_cert_store" :pointer ctx :pointer))
         (lookup (cffi:foreign-funcall "X509_STORE_add_lookup" :pointer store
                                       :pointer (cffi:foreign-funcall "X509_LOOKUP_file" :pointer)
                                       :pointer)))
    (when (or (cffi:null-pointer-p lookup)
              (< (cffi:foreign-funcall "X509_load_crl_file" :pointer lookup :string crl :int +x509-filetype-pem+ :int) 1))
      (tls-fail "cannot load RE_TLS_CRL ~a" crl))
    (cffi:foreign-funcall "X509_STORE_set_flags" :pointer store :unsigned-long +x509-v-flag-crl-check+ :int)))

(defun tls-load-own-certificate (ctx)
  (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_use_certificate_chain_file" :pointer ctx :string (tls-cert) :int))
    (tls-fail "cannot load RE_TLS_CERT ~a" (tls-cert)))
  (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_use_PrivateKey_file" :pointer ctx :string (tls-key)
                                       :int +x509-filetype-pem+ :int))
    (tls-fail "cannot load RE_TLS_KEY ~a" (tls-key)))
  (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_check_private_key" :pointer ctx :int))
    (tls-fail "RE_TLS_KEY does not match RE_TLS_CERT")))

(defun tls-load-lane-trust (ctx)
  (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_load_verify_locations" :pointer ctx :string (tls-ca)
                                       :pointer (cffi:null-pointer) :int))
    (tls-fail "cannot load RE_TLS_CA ~a" (tls-ca)))
  (when (tls-crl) (tls-load-crl ctx (tls-crl))))

(defun tls-base-context (verify-mode verify-location)
  (cl+ssl:make-context :min-proto-version cl+ssl::+tls1-3-version+
                       :verify-mode verify-mode
                       :verify-location verify-location))

(defvar *tls-server-groups* nil "The key-exchange groups the server context uses, for the startup line.")

(defun make-server-tls-context (require-at-handshake)
  "A context for serving: the profile, this instance's certificate, the lane
CA and CRL. A presented client certificate is always verified; one is required
at the handshake unless some routes are open to devices."
  (unless (and (tls-key) (tls-ca))
    (tls-fail "RE_TLS_CERT is set, so RE_TLS_KEY and RE_TLS_CA must be too"))
  (let ((ctx (tls-base-context (logior cl+ssl:+ssl-verify-peer+
                                       (if require-at-handshake +ssl-verify-fail-if-no-peer-cert+ 0))
                               nil)))
    (setf *tls-server-groups* (tls-apply-profile ctx))
    (tls-load-own-certificate ctx)
    (tls-load-lane-trust ctx)
    (tls-set-session-id-context ctx)
    ctx))

(defun tls-set-session-id-context (ctx)
  "A server that verifies client certificates must name a session id context,
or OpenSSL refuses every resumed session (ssl_get_prev_session, alert 80
internal_error). Node's https agent resumes TLS 1.3 sessions, so Manager's
second request to this RE failed while curl's one-shot connections succeeded
(RealityEngine_CI#729, found on the release lane; C++ had the same defect,
RealityEngine_CPP#195)."
  (cffi:with-foreign-string ((name len) "realityengine" :null-terminated-p nil)
    (unless (eql 1 (cffi:foreign-funcall "SSL_CTX_set_session_id_context"
                                         :pointer ctx :pointer name :unsigned-int len :int))
      (tls-fail "cannot set the TLS session id context"))))

;;; Serving -------------------------------------------------------------------

(defvar *tls-peer-verified* nil
  "Within one connection: did the client present a certificate the lane CA verified?")

(defvar *tls-connection* nil
  "Within one connection: did it arrive over TLS? A route open to devices uses it
with *tls-peer-verified* to require a certificate per source.")

(defclass tls-acceptor (hunchentoot:easy-acceptor)
  ((tls-context :initarg :tls-context :reader acceptor-tls-context))
  (:documentation "Hunchentoot over TLS 1.3 with this engine's own context (the
profile, mTLS, the CRL). There is no plaintext listener beside it."))

(defmethod hunchentoot:acceptor-ssl-p ((acceptor tls-acceptor)) t)

(defmethod hunchentoot:process-connection :around ((acceptor tls-acceptor) socket)
  (declare (ignore socket))
  (let ((*tls-peer-verified* nil)
        (*tls-connection* t))
    (call-next-method)))

(defun ssl-peer-verified-p (ssl-stream)
  (let* ((handle (cl+ssl::ssl-stream-handle ssl-stream))
         (cert (cffi:foreign-funcall "SSL_get1_peer_certificate" :pointer handle :pointer)))
    (unless (cffi:null-pointer-p cert)
      (cffi:foreign-funcall "X509_free" :pointer cert :void)
      (zerop (cffi:foreign-funcall "SSL_get_verify_result" :pointer handle :long)))))

(defmethod hunchentoot:initialize-connection-stream ((acceptor tls-acceptor) stream)
  ;; A refused handshake (no or untrusted client certificate, TLS 1.2,
  ;; plaintext) signals here and Hunchentoot closes the connection.
  (let ((ssl (cl+ssl:with-global-context ((acceptor-tls-context acceptor))
               (cl+ssl:make-ssl-server-stream stream))))
    (setf *tls-peer-verified* (ssl-peer-verified-p ssl))
    (call-next-method acceptor ssl)))

(defvar *tls-certificate-optional-routes* nil
  "Path patterns a client may reach without a client certificate (the PE's
device routes). Empty: the handshake requires one.")

(defun tls-requires-client-certificate-p (path)
  (notany (lambda (pattern) (match-pattern pattern path)) *tls-certificate-optional-routes*))

(defun tls-certificate-dispatcher (request)
  "First in the dispatch table on a TLS port with device routes: any other route
reached without a verified client certificate answers 403."
  (when (and (not *tls-peer-verified*)
             (tls-requires-client-certificate-p (hunchentoot:script-name request)))
    (lambda ()
      (error-response "client certificate required" 403))))

;;; Calling -------------------------------------------------------------------

(defun ipv4-octets (host)
  (let ((parts (uiop:split-string host :separator ".")))
    (when (= (length parts) 4)
      (let ((octets (mapcar (lambda (p) (and (plusp (length p)) (every #'digit-char-p p) (parse-integer p))) parts)))
        (when (every (lambda (o) (and o (<= 0 o 255))) octets) octets)))))

(defun ip-literal-p (host)
  (or (ipv4-octets host) (find #\: host)))

(defun internal-host-p (host)
  "An internal peer: loopback, a private or link-local address, a single-label
name (a compose service), or a name under .localhost, .internal or .local.
Internal links trust the lane CA alone and present this instance's certificate;
every other host is an external party, verified against the system store."
  (let ((h (string-downcase (string-trim "[]" (or host "")))))
    (cond ((zerop (length h)) nil)
          ((or (string= h "localhost")
               (some (lambda (suffix) (let ((n (length suffix)))
                                        (and (> (length h) n) (string= suffix h :start2 (- (length h) n)))))
                     '(".localhost" ".internal" ".local")))
           t)
          ((ipv4-octets h)
           (destructuring-bind (a b &rest rest) (ipv4-octets h)
             (declare (ignore rest))
             (or (= a 127) (= a 10) (and (= a 172) (<= 16 b 31)) (and (= a 192) (= b 168)) (and (= a 169) (= b 254)))))
          ((find #\: h)
           (or (string= h "::1")
               (let ((first (subseq h 0 (or (position #\: h) 0))))
                 (and (plusp (length first))
                      (let ((v (parse-integer first :radix 16 :junk-allowed t)))
                        (and v (or (= (logand v #xfe00) #xfc00) (= (logand v #xffc0) #xfe80))))))))
          (t (not (find #\. h))))))

(defvar *tls-client-contexts* (make-hash-table :test #'equal))
(defvar *tls-client-lock* (bt:make-lock "tls-client"))

(defun tls-client-context (host)
  "One client context per peer host, built once: OpenSSL checks the host name or
IP against the peer's certificate (cl+ssl's own check covers names only, and the
instance registry addresses engines by IP)."
  (bt:with-lock-held (*tls-client-lock*)
    (or (gethash host *tls-client-contexts*)
        (setf (gethash host *tls-client-contexts*)
              (let* ((internal (internal-host-p host))
                     (ctx (tls-base-context cl+ssl:+ssl-verify-peer+ (if internal nil :default))))
                (tls-apply-profile ctx)
                (when internal
                  (tls-load-lane-trust ctx)
                  (when (tls-serving-p) (tls-load-own-certificate ctx)))
                (let ((param (cffi:foreign-funcall "SSL_CTX_get0_param" :pointer ctx :pointer)))
                  (unless (eql 1 (if (ip-literal-p host)
                                     (cffi:foreign-funcall "X509_VERIFY_PARAM_set1_ip_asc" :pointer param :string host :int)
                                     (cffi:foreign-funcall "X509_VERIFY_PARAM_set1_host" :pointer param :string host
                                                           :unsigned-long 0 :int)))
                    (tls-fail "cannot verify the peer ~a" host)))
                ctx)))))

;; Kept-alive TLS connections, per host:port. An engine instance's PE calls its
;; RE many times per step; a handshake per call would put one on every step. A
;; stream idle longer than the peer's keep-alive window is dropped, not reused,
;; and only a GET is retried on a fresh connection after a failure on a reused
;; one, since a POST may already have been applied.
(defvar *tls-pool* (make-hash-table :test #'equal))
(defvar *tls-pool-lock* (bt:make-lock "tls-pool"))
(defparameter *tls-pool-idle-seconds* 2)

(defun tls-pool-take (key)
  (bt:with-lock-held (*tls-pool-lock*)
    (loop for entry = (pop (gethash key *tls-pool*))
          while entry
          do (destructuring-bind (stream . at) entry
               (if (< (- (get-universal-time) at) *tls-pool-idle-seconds*)
                   (return stream)
                   (ignore-errors (close stream)))))))

(defun tls-pool-put (key stream)
  (bt:with-lock-held (*tls-pool-lock*)
    (push (cons stream (get-universal-time)) (gethash key *tls-pool*))))

(defun tls-open-stream (host port)
  (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)
                                                    :timeout (http-connect-timeout)))
         (raw (usocket:socket-stream socket)))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (ignore-errors (usocket:socket-close socket)))))
      (cl+ssl:with-global-context ((tls-client-context host))
        ;; :verify nil: the context's SSL_VERIFY_PEER and host/IP parameter make
        ;; OpenSSL refuse the handshake itself, which covers IP SANs.
        ;; Wrapped as Drakma wraps the connections it opens itself, so it can
        ;; take this one with :stream and hand it back for reuse.
        (flexi-streams:make-flexi-stream
         (chunga:make-chunked-stream
          (cl+ssl:make-ssl-client-stream raw :hostname (unless (ip-literal-p host) host) :verify nil))
         :external-format (flexi-streams:make-external-format :latin-1 :eol-style :lf))))))

(defun tls-lane-request (url drakma-args)
  "DRAKMA:HTTP-REQUEST over a pooled lane TLS stream. Returns the body."
  (let* ((uri (puri:parse-uri url))
         (host (puri:uri-host uri))
         (port (or (puri:uri-port uri) 443))
         (key (format nil "~a:~a" host port))
         (method (or (getf drakma-args :method) :get)))
    (flet ((attempt (stream)
             (multiple-value-bind (body status headers uri* stream* must-close)
                 (apply #'drakma:http-request url :stream stream :close nil drakma-args)
               (declare (ignore status headers uri*))
               (if must-close
                   (ignore-errors (close stream*))
                   (tls-pool-put key stream*))
               body)))
      (let ((reused (tls-pool-take key)))
        (if reused
            (handler-case (attempt reused)
              (error (condition)
                (ignore-errors (close reused))
                (if (eq method :get)
                    (attempt (tls-open-stream host port))
                    (error condition))))
            (attempt (tls-open-stream host port)))))))

(defun tls-lane-url-p (url)
  "An https URL while the lane is on: served by the lane client."
  (and (tls-lane-p) (stringp url) (>= (length url) 8) (string-equal "https://" url :end2 8)))
