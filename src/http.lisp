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

(defun dispatch-route (routes)
  (let* ((method (string-upcase (symbol-name (hunchentoot:request-method*))))
         (path (hunchentoot:script-name*))
         (route (find-if (lambda (route)
                           (and (string= method (route-method route))
                                (match-pattern (route-pattern route) path)))
                         routes)))
    (if route
        (let ((params (match-pattern (route-pattern route) path)))
          (handler-case
              (funcall (route-handler route) params (request-body-json) (query-params))
            (error (condition)
              (error-response (princ-to-string condition) 500))))
        (error-response (format nil "No route for ~a ~a" method path) 404))))

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

(defun http-get-json (url)
  (parse-json (drakma:http-request url :method :get)))

(defun http-post-json (url payload)
  (parse-json (drakma:http-request url
                                   :method :post
                                   :content (json-stringify payload)
                                   :content-type "application/json")))

(defun http-request-json (url &key (method :get) payload headers)
  (let ((args (list url :method method)))
    (when payload
      (setf args (append args (list :content (json-stringify payload)
                                    :content-type "application/json"))))
    (when headers
      (setf args (append args (list :additional-headers headers))))
    (parse-json (apply #'drakma:http-request args))))
