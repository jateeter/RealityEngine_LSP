(in-package #:reality-engine-lsp)

;;; PE CLI shim — mirrors reality_engine_cli pe <command> from _CPP.
;;; Pure HTTP client: every command calls the PE HTTP API.
;;; Entry point: (cli-main args) where args = (rest uiop:*command-line-arguments*).

(defun cli-arg (args flag &optional default)
  "Return the value of a --flag argument from args list."
  (loop for (a b) on args
        when (string= a flag)
          return b
        finally (return default)))

(defun cli-flag-p (args flag)
  "Return t when a boolean --flag is present in args."
  (member flag args :test #'string=))

(defun cli-positional (args index)
  "Return the index-th positional (non-flag) argument."
  (let ((seen 0))
    (loop for (a) on args
          unless (and (> (length a) 2) (string= "--" (subseq a 0 2)))
            do (when (= seen index) (return a))
               (incf seen))
    nil))

(defun cli-parse-csv (csv)
  "Parse comma-separated floats into a JSON array object."
  (let ((nums (mapcar #'parse-float (remove "" (split-string csv #\,) :test #'string=))))
    (vectorize nums)))

(defun parse-float (s)
  (with-standard-io-syntax
    (let ((*read-default-float-format* 'double-float))
      (read-from-string s))))

(defun cli-get (base path)
  "HTTP GET base+path, return raw response string."
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "~a~a" base path)
                           :method :get
                           :want-stream nil)
    (declare (ignore status))
    (if (stringp body) body (babel:octets-to-string body))))

(defun cli-post (base path payload)
  "HTTP POST base+path with JSON payload, return raw response string."
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "~a~a" base path)
                           :method :post
                           :content (json-stringify payload)
                           :content-type "application/json"
                           :want-stream nil)
    (declare (ignore status))
    (if (stringp body) body (babel:octets-to-string body))))

(defun cli-patch (base path payload)
  "HTTP PATCH base+path with JSON payload, return raw response string."
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "~a~a" base path)
                           :method :patch
                           :content (json-stringify payload)
                           :content-type "application/json"
                           :want-stream nil)
    (declare (ignore status))
    (if (stringp body) body (babel:octets-to-string body))))

(defun cli-usage ()
  (format *error-output*
   "Usage:~%~
    pe integrations-status [--pe-url URL]~%~
    pe state [--pe-url URL]~%~
    pe push [--pe-url URL]~%~
    pe sources [--pe-url URL]~%~
    pe dispatch-ledger [--pe-url URL]~%~
    pe dispatch-read ID [--pe-url URL]~%~
    pe dispatch-update ID [--status S] [--adapter A] [--provider P]~%~
       [--external-run-id ID] [--increment-attempts] [--error TEXT] [--pe-url URL]~%~
    pe ollama-status [--pe-url URL]~%~
    pe ollama-dispatch ID [--model M] [--source-mapping-id ID] [--prompt TEXT]~%~
       [--no-commit] [--trigger-push] [--pe-url URL]~%~
    pe openai-status [--pe-url URL]~%~
    pe openai-dispatch ID [--model M] [--source-mapping-id ID] [--prompt TEXT]~%~
       [--instructions TEXT] [--no-commit] [--trigger-push] [--pe-url URL]~%~
    pe acp-status [--pe-url URL]~%~
    pe acp-dispatch ID [--target-agent A] [--session-key KEY]~%~
       [--source-mapping-id ID] [--prompt TEXT] [--external-run-id ID] [--pe-url URL]~%~
    pe healthkit-status [--pe-url URL]~%~
    pe healthkit-ingest --sample-type TYPE --values CSV~%~
       [--source-mapping-id ID] [--bridge-id ID] [--unit UNIT]~%~
       [--trigger-push] [--pe-url URL]~%~
    pe carekit-status [--pe-url URL]~%~
    pe carekit-ingest --sample-type TYPE --values CSV~%~
       [--source-mapping-id ID] [--bridge-id ID] [--task-id ID]~%~
       [--care-plan-id ID] [--trigger-push] [--pe-url URL]~%~
    pe completion --values CSV [--provider P] [--agent A]~%~
       [--source-mapping-id ID] [--correlation-id ID] [--envelope-id ID]~%~
       [--trigger-push] [--pe-url URL]~%"))

(defun cli-run-command (command args base)
  (cond
    ((string= command "integrations-status")
     (cli-get base "/api/integrations/status"))

    ((string= command "state")
     (cli-get base "/api/state"))

    ((string= command "push")
     (cli-post base "/api/push" (obj "includeMachineResults" t)))

    ((string= command "sources")
     (cli-get base "/api/sources"))

    ((string= command "dispatch-ledger")
     (cli-get base "/api/dispatch/ledger"))

    ((string= command "dispatch-read")
     (let ((id (cli-positional args 0)))
       (unless id (error "dispatch-read requires ID"))
       (cli-get base (format nil "/api/dispatch/records/~a" id))))

    ((string= command "dispatch-update")
     (let ((id (cli-positional args 0)))
       (unless id (error "dispatch-update requires ID"))
       (let ((body (obj)))
         (when (cli-flag-p args "--increment-attempts")
           (setf (jget body "incrementAttempts") t))
         (let ((status (cli-arg args "--status"))
               (adapter (cli-arg args "--adapter"))
               (provider (cli-arg args "--provider"))
               (ext-id (cli-arg args "--external-run-id"))
               (err (cli-arg args "--error")))
           (when status (setf (jget body "status") status))
           (when adapter (setf (jget body "adapter") adapter))
           (when provider (setf (jget body "provider") provider))
           (when ext-id (setf (jget body "externalRunId") ext-id))
           (when err (setf (jget body "error") err)))
         (cli-patch base (format nil "/api/dispatch/records/~a" id) body))))

    ((string= command "ollama-status")
     (cli-get base "/api/integrations/ollama/status"))

    ((string= command "ollama-dispatch")
     (let ((id (cli-positional args 0)))
       (unless id (error "ollama-dispatch requires ID"))
       (let ((body (obj "dispatchId" id)))
         (let ((model (cli-arg args "--model"))
               (mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (prompt (cli-arg args "--prompt")))
           (when model (setf (jget body "model") model))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when prompt (setf (jget body "prompt") prompt))
           (when (cli-flag-p args "--no-commit") (setf (jget body "commitCompletion") +json-false+))
           (when (cli-flag-p args "--trigger-push") (setf (jget body "triggerPush") t)))
         (cli-post base "/api/integrations/ollama/dispatch" body))))

    ((string= command "openai-status")
     (cli-get base "/api/integrations/openai/status"))

    ((string= command "openai-dispatch")
     (let ((id (cli-positional args 0)))
       (unless id (error "openai-dispatch requires ID"))
       (let ((body (obj "dispatchId" id)))
         (let ((model (cli-arg args "--model"))
               (mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (prompt (cli-arg args "--prompt"))
               (instr (cli-arg args "--instructions")))
           (when model (setf (jget body "model") model))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when prompt (setf (jget body "prompt") prompt))
           (when instr (setf (jget body "instructions") instr))
           (when (cli-flag-p args "--no-commit") (setf (jget body "commitCompletion") +json-false+))
           (when (cli-flag-p args "--trigger-push") (setf (jget body "triggerPush") t)))
         (cli-post base "/api/integrations/openai/dispatch" body))))

    ((string= command "acp-status")
     (cli-get base "/api/integrations/acp/status"))

    ((string= command "acp-dispatch")
     (let ((id (cli-positional args 0)))
       (unless id (error "acp-dispatch requires ID"))
       (let ((body (obj "dispatchId" id)))
         (let ((agent (cli-arg args "--target-agent" (cli-arg args "--agent")))
               (session (cli-arg args "--session-key" (cli-arg args "--session")))
               (mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (prompt (cli-arg args "--prompt"))
               (external-run-id (cli-arg args "--external-run-id")))
           (when agent (setf (jget body "targetAgent") agent))
           (when session (setf (jget body "sessionKey") session))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when prompt (setf (jget body "prompt") prompt))
           (when external-run-id (setf (jget body "externalRunId") external-run-id)))
         (cli-post base "/api/integrations/acp/dispatch" body))))

    ((string= command "healthkit-status")
     (cli-get base "/api/integrations/healthkit/status"))

    ((string= command "healthkit-ingest")
     (let ((values-csv (cli-arg args "--values"))
           (sample-type (cli-arg args "--sample-type" (cli-arg args "--type"))))
       (unless (and values-csv sample-type)
         (error "healthkit-ingest requires --sample-type TYPE and --values CSV"))
       (let ((body (obj "sampleType" sample-type "values" (cli-parse-csv values-csv))))
         (let ((bridge-id (cli-arg args "--bridge-id"))
               (mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (unit (cli-arg args "--unit")))
           (when bridge-id (setf (jget body "bridgeId") bridge-id))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when unit (setf (jget body "unit") unit))
           (when (cli-flag-p args "--trigger-push") (setf (jget body "triggerPush") t)))
         (cli-post base "/api/integrations/healthkit/ingest" body))))

    ((string= command "carekit-status")
     (cli-get base "/api/integrations/carekit/status"))

    ((string= command "carekit-ingest")
     (let ((values-csv (cli-arg args "--values"))
           (sample-type (cli-arg args "--sample-type" (cli-arg args "--type"))))
       (unless (and values-csv sample-type)
         (error "carekit-ingest requires --sample-type TYPE and --values CSV"))
       (let ((body (obj "sampleType" sample-type "values" (cli-parse-csv values-csv))))
         (let ((bridge-id (cli-arg args "--bridge-id"))
               (mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (task-id (cli-arg args "--task-id"))
               (care-plan (cli-arg args "--care-plan-id")))
           (when bridge-id (setf (jget body "bridgeId") bridge-id))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when task-id (setf (jget body "taskId") task-id))
           (when care-plan (setf (jget body "carePlanId") care-plan))
           (when (cli-flag-p args "--trigger-push") (setf (jget body "triggerPush") t)))
         (cli-post base "/api/integrations/carekit/ingest" body))))

    ((string= command "completion")
     (let ((values-csv (cli-arg args "--values")))
       (unless values-csv (error "completion requires --values CSV"))
       (let ((body (obj "provider" (or (cli-arg args "--provider") "cli")
                        "agent" (or (cli-arg args "--agent") "cli")
                        "values" (cli-parse-csv values-csv)
                        "triggerPush" (json-bool (cli-flag-p args "--trigger-push")))))
         (let ((mapping (cli-arg args "--source-mapping-id" (cli-arg args "--mapping-id")))
               (corr (cli-arg args "--correlation-id"))
               (env (cli-arg args "--envelope-id")))
           (when mapping (setf (jget body "sourceMappingId") mapping))
           (when corr (setf (jget body "correlationId") corr))
           (when env (setf (jget body "envelopeId") env)))
         (cli-post base "/api/integrations/completions" body))))

    (t
     (cli-usage)
     (uiop:quit 2))))

(defun cli-main (args)
  "Entry point for CLI mode.  args = command-line args after the 'pe' subcommand."
  (when (or (null args) (member "--help" args :test #'string=) (member "-h" args :test #'string=))
    (cli-usage)
    (uiop:quit (if args 0 2)))
  (let* ((command (first args))
         (rest-args (rest args))
         (pe-url-raw (or (cli-arg args "--pe-url")
                         (uiop:getenv "PE_URL")
                         "http://localhost:5600"))
         (base (trim-trailing-slashes pe-url-raw)))
    (handler-case
        (let ((result (cli-run-command command rest-args base)))
          (format t "~a~%" result)
          (uiop:quit 0))
      (error (e)
        (format *error-output* "pe: ~a~%" e)
        (uiop:quit 1)))))
