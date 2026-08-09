(in-package #:reality-engine-lsp)

(defstruct (route (:constructor %make-route)) method pattern handler)

(defun json-response (value &optional (status 200))
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:return-code*) status)
  (json-stringify value))

(defun text-response (value &optional (status 200) (content-type "text/plain; charset=utf-8"))
  (setf (hunchentoot:content-type*) content-type
        (hunchentoot:return-code*) status)
  value)

(defun error-response (message &optional (status 500))
  (json-response (obj "error" message "status" status) status))

(defun request-body-json ()
  (parse-json (or (hunchentoot:raw-post-data :force-text t) "")))

(defun request-bearer-token ()
  "Token from an Authorization: Bearer header on the current request, or NIL.
Must be called on the request thread — the hunchentoot context is dynamic."
  (let ((auth (hunchentoot:header-in* :authorization)))
    (when (and auth
               (> (length auth) 7)
               (string-equal "bearer " (subseq auth 0 7)))
      (let ((token (string-trim " " (subseq auth 7))))
        (when (plusp (length token)) token)))))

(defun query-params ()
  (let ((out (make-hash-table :test #'equal)))
    (dolist (pair (hunchentoot:get-parameters*))
      (setf (gethash (car pair) out) (cdr pair)))
    out))

(defun split-path-components (path)
  (remove "" (split-string (or path "/") #\/) :test #'string=))

(defun match-pattern (pattern path)
  (let ((expected (split-path-components pattern))
        (actual (split-path-components path))
        (params (make-hash-table :test #'equal)))
    (when (= (length expected) (length actual))
      (loop for e in expected
            for a in actual
            always (cond
                     ((and (> (length e) 0) (char= (char e 0) #\:))
                      (setf (gethash (subseq e 1) params) a)
                      t)
                     (t (string= e a)))
            finally (return params)))))

(defun flatten-routes (routes)
  (cond
    ((null routes) nil)
    ((typep routes 'route) (list routes))
    ((listp routes) (mapcan #'flatten-routes routes))
    (t nil)))

;; CORS — byte-identical to the Scala and C++ engines so a single Swagger UI
;; origin can execute against any runtime.  Without these, Swagger served from
;; http://127.0.0.1:8088 could not call LSP endpoints at all: normal responses
;; carried no Access-Control-Allow-Origin and OPTIONS preflight 404'd.
(defparameter +cors-allow-origin+ "*")
(defparameter +cors-allow-methods+ "GET, POST, PUT, PATCH, DELETE, OPTIONS")
(defparameter +cors-allow-headers+ "Content-Type, Accept, Authorization")

(defun set-cors-headers ()
  "Stamp CORS headers on the current response. Must run on the request thread."
  (setf (hunchentoot:header-out :access-control-allow-origin) +cors-allow-origin+
        (hunchentoot:header-out :access-control-allow-methods) +cors-allow-methods+
        (hunchentoot:header-out :access-control-allow-headers) +cors-allow-headers+))

(defun dispatch-route (routes)
  (let* ((method (string-upcase (symbol-name (hunchentoot:request-method*))))
         (path (hunchentoot:script-name*))
         (route (find-if (lambda (route)
                           (and (string= method (route-method route))
                                (match-pattern (route-pattern route) path)))
                         routes)))
    ;; Every response carries CORS headers, including errors — a 404 without
    ;; them surfaces in the browser as an opaque CORS failure rather than the
    ;; 404 it actually is.
    (set-cors-headers)
    (cond
      ;; Preflight is answered for any path, matching the other runtimes:
      ;; the browser asks before it knows whether the route exists.
      ((string= method "OPTIONS")
       (setf (hunchentoot:return-code*) 204)
       (setf (hunchentoot:content-type*) nil)
       "")
      (route
       (let ((params (match-pattern (route-pattern route) path)))
         (handler-case
             (funcall (route-handler route) params (request-body-json) (query-params))
           (error (condition)
             (error-response (princ-to-string condition) 500)))))
      (t (error-response (format nil "No route for ~a ~a" method path) 404)))))

(defun make-route (method pattern handler &rest grouped-routes)
  (let ((route (%make-route :method (string-upcase method) :pattern pattern :handler handler)))
    (if grouped-routes
        (cons route grouped-routes)
        route)))

(defun start-http-server (port routes &key (name "reality-engine-lsp") extra-dispatchers)
  (declare (ignore name))
  (setf routes (flatten-routes routes))
  (setf hunchentoot:*dispatch-table*
        (append extra-dispatchers
                (list (hunchentoot:create-prefix-dispatcher
                       "/"
                       (lambda () (dispatch-route routes))))))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port port)))
    (hunchentoot:start acceptor)
    acceptor))

(defun drakma-body-string (body)
  (if (stringp body)
      body
      (sb-ext:octets-to-string body :external-format :utf-8)))

;; Outbound HTTP must be bounded.
;;
;; Drakma defaults to a 20-second connect timeout and *no* read timeout at
;; all, so a peer that accepts a connection and then goes quiet blocks the
;; caller forever.  That is not hypothetical: with Ollama absent,
;; /api/integrations/ollama/status outlived the regression harness's request
;; budget and the client aborted, while C++ answered the same probe promptly
;; with reachable:false (#40).  A status endpoint must always answer.
;;
;; Applied to every helper rather than just the probe path — the same
;; unbounded wait is one absent service away from stalling any integration
;; call.  Override for genuinely slow providers, e.g. a local model doing a
;; long completion:
;;   RE_HTTP_CONNECT_TIMEOUT_SECONDS (default 3)
;;   RE_HTTP_TOTAL_TIMEOUT_SECONDS   (default 10)

(defun http-connect-timeout () (env-int "RE_HTTP_CONNECT_TIMEOUT_SECONDS" 3))
(defun http-total-timeout   () (env-int "RE_HTTP_TOTAL_TIMEOUT_SECONDS" 10))

;; Drakma's :connection-timeout covers the connect phase on SBCL, but its
;; :read-timeout parameter exists only on LispWorks 7.1 — passing it here is a
;; program error, not a no-op — so the read phase is bounded with
;; sb-ext:with-timeout instead.
;;
;; Two bounds can fire here and both arrive as sb-ext:timeout — SBCL
;; implements Drakma's connection-timeout as a deadline covering the whole
;; request, and sb-sys:deadline-timeout is a subtype of sb-ext:timeout. The
;; message therefore names both limits rather than claiming which one tripped.
;;
;; sb-ext:timeout is a serious-condition, not an error, so it would slip
;; straight through the handler-case (error ...) that every caller uses. It is
;; converted to a plain error here so callers keep working unchanged and a
;; stalled peer reads as an ordinary failed probe.
(defmacro with-bounded-http ((url) &body body)
  (let ((u (gensym "URL")))
    `(let ((,u ,url))
       (handler-case (sb-ext:with-timeout (http-total-timeout) ,@body)
         (sb-ext:timeout ()
           (error "HTTP request to ~a timed out (connect ~a s, total ~a s)"
                  ,u (http-connect-timeout) (http-total-timeout)))))))

(defun http-get-json (url)
  (with-bounded-http (url)
    (parse-json (drakma-body-string
                 (drakma:http-request url :method :get :close t
                                      :connection-timeout (http-connect-timeout))))))

(defun http-post-json (url payload)
  (with-bounded-http (url)
    (parse-json (drakma-body-string
                 (drakma:http-request url
                                      :method :post
                                      :content (json-stringify payload)
                                      :content-type "application/json"
                                      :close t
                                      :connection-timeout (http-connect-timeout))))))

(defun http-request-json (url &key (method :get) payload headers)
  (let ((args (list url :method method)))
    (when payload
      (setf args (append args (list :content (json-stringify payload)
                                    :content-type "application/json"))))
    (when headers
      (setf args (append args (list :additional-headers headers))))
    (with-bounded-http (url)
      (parse-json (drakma-body-string
                   (apply #'drakma:http-request
                          (append args (list :close t
                                             :connection-timeout (http-connect-timeout)))))))))
