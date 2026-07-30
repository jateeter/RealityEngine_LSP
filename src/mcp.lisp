(in-package #:reality-engine-lsp)

;;; MCP (Model Context Protocol) shim for the Perception Engine.
;;; Wire-compatible with _AI mcp.ts — same tool names, same JSON-RPC envelope.
;;;
;;; Transport:
;;;   POST   /mcp   — initialize session or dispatch JSON-RPC
;;;   GET    /mcp   — SSE notification stream for an existing session
;;;   DELETE /mcp   — close session
;;;
;;; All tools operate in-process against the actor.  Reality-Engine proxy
;;; tools call the RE HTTP API directly (same as _AI's reCall() helper).

;; ── Session store ────────────────────────────────────────────────────────────

(defstruct mcp-session
  id
  (notifications nil)
  (lock (bt:make-lock "mcp-session-lock"))
  (cvar (bt:make-condition-variable "mcp-session-cvar")))

(defvar *mcp-sessions* (make-hash-table :test #'equal))
(defvar *mcp-sessions-lock* (bt:make-lock "mcp-sessions-lock"))

(defun mcp-new-session ()
  (let* ((id (make-id "mcp-session"))
         (session (make-mcp-session :id id)))
    (bt:with-lock-held (*mcp-sessions-lock*)
      (setf (gethash id *mcp-sessions*) session))
    session))

(defun mcp-find-session (id)
  (bt:with-lock-held (*mcp-sessions-lock*)
    (gethash id *mcp-sessions*)))

(defun mcp-close-session (id)
  (bt:with-lock-held (*mcp-sessions-lock*)
    (remhash id *mcp-sessions*)))

(defun mcp-notify (session event)
  (bt:with-lock-held ((mcp-session-lock session))
    (setf (mcp-session-notifications session)
          (nconc (mcp-session-notifications session) (list event)))
    (bt:condition-notify (mcp-session-cvar session))))

(defun mcp-dequeue-notification (session timeout-secs)
  (bt:with-lock-held ((mcp-session-lock session))
    (when (null (mcp-session-notifications session))
      (bt:condition-wait (mcp-session-cvar session) (mcp-session-lock session)
                         :timeout timeout-secs))
    (when (mcp-session-notifications session)
      (let ((ev (first (mcp-session-notifications session))))
        (setf (mcp-session-notifications session)
              (rest (mcp-session-notifications session)))
        ev))))

;; ── JSON-RPC helpers ─────────────────────────────────────────────────────────

(defun mcp-ok (id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(defun mcp-err (id code message)
  (obj "jsonrpc" "2.0" "id" id
       "error" (obj "code" code "message" message)))

(defun mcp-text (text &optional is-error)
  (let ((r (obj "content" (arr (obj "type" "text" "text" (if (stringp text) text
                                                              (json-stringify text)))))))
    (when is-error (setf (jget r "isError") t))
    r))

(defun mcp-re-call (re-url fn)
  (handler-case
      (mcp-text (json-stringify (funcall fn re-url)))
    (error (e) (mcp-text (json-stringify (obj "error" (princ-to-string e))) t))))

(defvar *mcp-allow-mutation-override* nil
  "Test-only override for mutating MCP tool policy.")

(defun mcp-tool-allowlisted-p (tool-name)
  (let ((raw (env "RE_MCP_ALLOWED_TOOLS" "")))
    (and (> (length raw) 0)
         (member tool-name
                 (mapcar (lambda (item)
                           (string-trim '(#\Space #\Tab #\Newline #\Return) item))
                         (split-string raw #\,))
                 :test #'string=))))

(defun mcp-mutating-tool-allowed-p (tool-name)
  (or *mcp-allow-mutation-override*
      (mcp-tool-allowlisted-p tool-name)
      (env-bool "RE_MCP_ALLOW_MUTATION" nil)))

(defun mcp-disabled-by-policy (tool-name)
  (mcp-text
   (json-stringify
    (obj "error"
         (format nil "Tool \"~a\" is disabled by policy. Enable with RE_MCP_ALLOW_MUTATION=true or add it to RE_MCP_ALLOWED_TOOLS."
                 tool-name)))
   t))

(defun mcp-cons-json-text (result)
  (if (consp result)
      (mcp-text (json-stringify (cdr result)) (>= (car result) 400))
      (mcp-text (json-stringify result))))

;; ── Tool registry ────────────────────────────────────────────────────────────

(defun mcp-build-tools (actor re-url)
  "Return the complete tool list as a plist of :name :description :schema :fn."
  (flet ((tool (name description schema fn)
           (list :name name :description description :schema schema :fn fn)))

    ;; ── PE engine control ─────────────────────────────────────────────────

    (list

     (tool "perception_get_state"
           "Return the full perception engine state: sources, globalStep, matchAlgorithm, lastPush."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s)
                                            (perception-state-json (perception-state-engine s))))))))

     (tool "perception_push"
           "Assemble the perceptual vector and push it to the Reality Engine. Returns step result."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s)
                                            (push-perception s t)))))))

     (tool "perception_start_auto"
           "Start automatic periodic pushing of the perceptual vector."
           (obj "type" "object"
                "properties" (obj "interval_ms" (obj "type" "number" "default" 1000)))
           (lambda (args)
             (let ((ms (or (jnumber args "interval_ms" nil) 1000)))
               (actor-tell actor (lambda (s)
                                   (setf (perception-engine-auto-running-p (perception-state-engine s)) t
                                         (perception-engine-auto-interval-ms (perception-state-engine s))
                                         (truncate ms))))
               (mcp-text (json-stringify (obj "success" t "intervalMs" ms))))))

     (tool "perception_stop_auto"
           "Stop automatic periodic pushing."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (actor-tell actor (lambda (s)
                                 (setf (perception-engine-auto-running-p (perception-state-engine s)) nil)))
             (mcp-text (json-stringify (obj "success" t)))))

     (tool "perception_reset"
           "Reset engine globalStep to 0 and restore all test source cursors."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (actor-tell actor (lambda (s)
                                 (setf (perception-state-engine s)
                                       (make-perception-engine-state
                                        (perception-engine-dimension (perception-state-engine s))))))
             (broadcast (obj "type" "state-update"))
             (mcp-text (json-stringify (obj "success" t "globalStep" 0)))))

     (tool "perception_set_match_algorithm"
           "Set the match algorithm: \"gte\" (input >= threshold) or \"equals\" (exact match)."
           (obj "type" "object"
                "properties" (obj "algorithm" (obj "type" "string" "enum" (arr "gte" "equals")))
                "required" (arr "algorithm"))
           (lambda (args)
             (let ((alg (jstring args "algorithm" "gte")))
               (actor-tell actor (lambda (s)
                                   (setf (perception-engine-match-algorithm (perception-state-engine s)) alg)))
               (mcp-text (json-stringify (obj "success" t "matchAlgorithm" alg))))))

     ;; ── Source management ──────────────────────────────────────────────────

     (tool "sources_list"
           "List all perception sources registered with the engine."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s)
                                            (vectorize (mapcar #'source-json
                                                               (object-values
                                                                (perception-engine-sources
                                                                 (perception-state-engine s)))))))))))

     (tool "pe.list_sources"
           "Canonical alias for sources_list."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s)
                                            (vectorize (mapcar #'source-json
                                                               (object-values
                                                                (perception-engine-sources
                                                                 (perception-state-engine s)))))))))))

     (tool "sources_add_simulated"
           "Add a simulated waveform source (sine/sawtooth/square/constant/etc.)."
           (obj "type" "object"
                "properties" (obj "name" (obj "type" "string")
                                  "region_offset" (obj "type" "number")
                                  "region_length" (obj "type" "number")
                                  "pattern" (obj "type" "string")
                                  "frequency" (obj "type" "number")
                                  "amplitude" (obj "type" "number")
                                  "dc_offset" (obj "type" "number")
                                  "active" (obj "type" "boolean"))
                "required" (arr "name" "region_offset" "region_length"))
           (lambda (args)
             (let ((body (obj "type" "simulated"
                              "name" (or (jstring args "name" nil) "source")
                              "active" (json-bool (jbool args "active" t))
                              "region" (obj "offset" (or (jnumber args "region_offset" nil) 0)
                                           "length" (or (jnumber args "region_length" nil) 1))
                              "pattern" (or (jstring args "pattern" nil) "constant")
                              "frequency" (or (jnumber args "frequency" nil) 1.0d0)
                              "amplitude" (or (jnumber args "amplitude" nil) 0.5d0)
                              "dcOffset" (or (jnumber args "dc_offset" nil) 0.5d0))))
               (let ((result (actor-ask actor (lambda (s)
                                                (source-json
                                                 (ensure-source-id (perception-state-engine s)
                                                                   (source-from-json body)))))))
                 (broadcast (obj "type" "state-update"))
                 (mcp-text (json-stringify result))))))

     (tool "sources_add_sensor"
           "Add an HTTP-push sensor source. External systems push values via POST /api/sensors/:sensorId."
           (obj "type" "object"
                "properties" (obj "name" (obj "type" "string")
                                  "sensor_id" (obj "type" "string")
                                  "region_offset" (obj "type" "number")
                                  "region_length" (obj "type" "number")
                                  "ttl_ms" (obj "type" "number")
                                  "active" (obj "type" "boolean"))
                "required" (arr "name" "sensor_id" "region_offset" "region_length"))
           (lambda (args)
             (let ((body (obj "type" "sensor"
                              "name" (or (jstring args "name" nil) "sensor")
                              "active" (json-bool (jbool args "active" t))
                              "sensorId" (jstring args "sensor_id" "")
                              "region" (obj "offset" (or (jnumber args "region_offset" nil) 0)
                                           "length" (or (jnumber args "region_length" nil) 1))
                              "ttlMs" (or (jnumber args "ttl_ms" nil) 30000))))
               (let ((result (actor-ask actor (lambda (s)
                                                (source-json
                                                 (ensure-source-id (perception-state-engine s)
                                                                   (source-from-json body)))))))
                 (broadcast (obj "type" "state-update"))
                 (mcp-text (json-stringify result))))))

     (tool "sources_add_test"
           "Add a test sequence source that replays a fixed array of input vectors step-by-step."
           (obj "type" "object"
                "properties" (obj "name" (obj "type" "string")
                                  "machine_id" (obj "type" "string")
                                  "machine_name" (obj "type" "string")
                                  "sequence_name" (obj "type" "string")
                                  "inputs" (obj "type" "array")
                                  "region_offset" (obj "type" "number")
                                  "region_length" (obj "type" "number")
                                  "loop" (obj "type" "boolean")
                                  "active" (obj "type" "boolean"))
                "required" (arr "name" "inputs" "region_offset" "region_length"))
           (lambda (args)
             (let ((body (obj "type" "test"
                              "name" (or (jstring args "name" nil) "test")
                              "active" (json-bool (jbool args "active" t))
                              "machineId" (or (jstring args "machine_id" nil) "")
                              "machineName" (or (jstring args "machine_name" nil) "")
                              "sequenceName" (or (jstring args "sequence_name" nil) "")
                              "region" (obj "offset" (or (jnumber args "region_offset" nil) 0)
                                           "length" (or (jnumber args "region_length" nil) 1))
                              "inputs" (or (jget args "inputs") (arr))
                              "loop" (json-bool (jbool args "loop" nil)))))
               (let ((result (actor-ask actor (lambda (s)
                                                (source-json
                                                 (ensure-source-id (perception-state-engine s)
                                                                   (source-from-json body)))))))
                 (broadcast (obj "type" "state-update"))
                 (mcp-text (json-stringify result))))))

     (tool "sources_update"
           "Update the name or active state of an existing source by ID."
           (obj "type" "object"
                "properties" (obj "id" (obj "type" "string")
                                  "name" (obj "type" "string")
                                  "active" (obj "type" "boolean"))
                "required" (arr "id"))
           (lambda (args)
             (let ((id (jstring args "id" "")))
               (let ((result (actor-ask actor (lambda (s)
                                                (let ((src (gethash id (perception-engine-sources
                                                                        (perception-state-engine s)))))
                                                  (when src
                                                    (when (jstring args "name" nil)
                                                      (setf (source-name src) (jstring args "name")))
                                                    (unless (eq (jget args "active" :missing) :missing)
                                                      (setf (source-active-p src) (jbool args "active" t)))
                                                    (source-json src)))))))
                 (if result
                     (progn (broadcast (obj "type" "state-update"))
                            (mcp-text (json-stringify result)))
                     (mcp-text (json-stringify (obj "error" (format nil "Source not found: ~a" id))) t))))))

     (tool "sources_delete"
           "Delete a source by ID."
           (obj "type" "object"
                "properties" (obj "id" (obj "type" "string"))
                "required" (arr "id"))
           (lambda (args)
             (let ((id (jstring args "id" "")))
               (actor-tell actor (lambda (s)
                                   (remhash id (perception-engine-sources (perception-state-engine s)))))
               (broadcast (obj "type" "state-update"))
               (mcp-text (json-stringify (obj "success" t "id" id))))))

     (tool "sensor_push_value"
           "Push a live value array to a sensor source identified by sensorId."
           (obj "type" "object"
                "properties" (obj "sensor_id" (obj "type" "string")
                                  "values" (obj "type" "array"))
                "required" (arr "sensor_id" "values"))
           (lambda (args)
             (let* ((sensor-id (jstring args "sensor_id" ""))
                    (values (numbers-from-json (or (jget args "values") (arr))))
                    (result (actor-ask actor (lambda (s)
                                               (let ((src (sensor-exists-p (perception-state-engine s) sensor-id)))
                                                 (when src
                                                   (setf (source-last-value src) values
                                                         (source-last-updated src) (now-ms))
                                                   t))))))
               (if result
                   (mcp-text (json-stringify (obj "success" t "sensorId" sensor-id "timestamp" (now-ms))))
                   (mcp-text (json-stringify (obj "error" (format nil "No sensor source with sensorId ~s" sensor-id))) t)))))

     (tool "pe.push_signal"
           "Canonical alias for sensor_push_value."
           (obj "type" "object"
                "properties" (obj "sensor_id" (obj "type" "string")
                                  "values" (obj "type" "array"))
                "required" (arr "sensor_id" "values"))
           (lambda (args)
             (let* ((sensor-id (jstring args "sensor_id" ""))
                    (values (numbers-from-json (or (jget args "values") (arr))))
                    (result (actor-ask actor (lambda (s)
                                               (let ((src (sensor-exists-p (perception-state-engine s) sensor-id)))
                                                 (when src
                                                   (setf (source-last-value src) values
                                                         (source-last-updated src) (now-ms))
                                                   t))))))
               (if result
                   (mcp-text (json-stringify (obj "success" t "sensorId" sensor-id "timestamp" (now-ms))))
                   (mcp-text (json-stringify (obj "error" (format nil "No sensor source with sensorId ~s" sensor-id))) t))))  )

     (tool "pe.enqueue_push"
           "Canonical alias for perception_push."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s) (push-perception s t)))))))

     ;; ── RE proxy tools ────────────────────────────────────────────────────

     (tool "re.read_state"
           "Canonical alias for perception_get_state. Returns full PE state."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-text (json-stringify
                         (actor-ask actor (lambda (s)
                                            (perception-state-json (perception-state-engine s))))))))

     (tool "reality_engine_health"
           "Check health and stats for the Reality Engine backend."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (let ((health (http-get-json (format nil "~a/api/health" base))))
                   (handler-case
                       (let ((stats (http-get-json (format nil "~a/api/engine/stats" base))))
                         (obj "health" health "stats" stats))
                     (error () (obj "health" health "stats" +json-null+))))))))

     (tool "machines_list"
           "List machines loaded in the Reality Engine plus JSON files on disk."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (let ((loaded (http-get-json (format nil "~a/api/machines" base))))
                   (handler-case
                       (let ((json-files (http-get-json (format nil "~a/api/machines/json/list" base))))
                         (obj "loaded" loaded "jsonFiles" json-files))
                     (error () (obj "loaded" loaded "jsonFiles" +json-null+))))))))

     (tool "re.list_machines"
           "Canonical alias for machines_list."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (let ((loaded (http-get-json (format nil "~a/api/machines" base))))
                   (handler-case
                       (let ((json-files (http-get-json (format nil "~a/api/machines/json/list" base))))
                         (obj "loaded" loaded "jsonFiles" json-files))
                     (error () (obj "loaded" loaded "jsonFiles" +json-null+))))))))

     (tool "re.read_machine"
           "Read one machine record from the Reality Engine by machine id."
           (obj "type" "object"
                "properties" (obj "machine_id" (obj "type" "string"))
                "required" (arr "machine_id"))
           (lambda (args)
             (mcp-re-call re-url
               (lambda (base)
                 (http-get-json (format nil "~a/api/machines/~a/export" base
                                        (jstring args "machine_id" "")))))))

     (tool "machines_load_json"
           "Load a machine from a JSON file into the Reality Engine."
           (obj "type" "object"
                "properties" (obj "filename" (obj "type" "string"))
                "required" (arr "filename"))
           (lambda (args)
             (mcp-re-call re-url
               (lambda (base)
                 (http-get-json (format nil "~a/api/machines/json/~a" base
                                        (jstring args "filename" "")))))))

     (tool "perceptual_sim_state"
           "Get the current state of the Reality Engine perceptual simulation."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (http-get-json (format nil "~a/api/perceptual-simulation/state" base))))))

     (tool "perceptual_sim_step"
           "Advance the Reality Engine perceptual simulation by one step."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (http-post-json (format nil "~a/api/perceptual-simulation/step" base) (obj))))))

     (tool "perceptual_sim_start"
           "Start automatic stepping of the Reality Engine perceptual simulation."
           (obj "type" "object"
                "properties" (obj "step_delay_ms" (obj "type" "number" "default" 500)))
           (lambda (args)
             (mcp-re-call re-url
               (lambda (base)
                 (http-post-json (format nil "~a/api/perceptual-simulation/start" base)
                                 (obj "stepDelayMs" (or (jnumber args "step_delay_ms" nil) 500)))))))

     (tool "perceptual_sim_stop"
           "Stop automatic stepping of the Reality Engine perceptual simulation."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (http-post-json (format nil "~a/api/perceptual-simulation/stop" base) (obj))))))

     (tool "perceptual_sim_reset"
           "Reset the Reality Engine perceptual simulation to its initial state."
           (obj "type" "object" "properties" (obj))
           (lambda (args)
             (declare (ignore args))
             (mcp-re-call re-url
               (lambda (base)
                 (http-post-json (format nil "~a/api/perceptual-simulation/reset" base) (obj))))))

     (tool "perceptual_sim_history"
           "Get the step history of the Reality Engine perceptual simulation."
           (obj "type" "object"
                "properties" (obj "limit" (obj "type" "number" "default" 24)))
           (lambda (args)
             (mcp-re-call re-url
               (lambda (base)
                 (let ((limit (or (jnumber args "limit" nil) 24)))
                   (http-get-json (format nil "~a/api/perceptual-simulation/history?limit=~a" base limit)))))))

     (tool "demo_load"
           "Load a built-in demonstration configuration into the Reality Engine."
           (obj "type" "object"
                "properties" (obj "demo" (obj "type" "string"
                                              "enum" (arr "data-center" "multi-step" "kleene-star")))
                "required" (arr "demo"))
           (lambda (args)
             (mcp-re-call re-url
               (lambda (base)
                 (http-get-json (format nil "~a/api/demo/~a" base (jstring args "demo" "")))))))

     ;; ── Dispatch ledger ───────────────────────────────────────────────────

     (tool "dispatch.read_ledger"
           "Return the current dispatch ledger. Wire-compatible with GET /api/dispatch/ledger."
           (obj "type" "object"
                "properties" (obj "limit" (obj "type" "number")))
           (lambda (args)
             (let* ((limit (jnumber args "limit" nil))
                    (records (actor-ask actor (lambda (s)
                                                (mapcar #'dispatch-record-json
                                                        (perception-state-dispatch-ledger s)))))
                    (trimmed (if limit
                                 (let ((n (truncate limit)))
                                   (if (> (length records) n)
                                       (last records n)
                                       records))
                                 records)))
               (mcp-text (json-stringify (obj "records" (vectorize trimmed)))))))

     (tool "integrations.completion"
           "Commit a provider or agent completion through PE source mappings. Mutating; gated by RE_MCP_ALLOW_MUTATION / RE_MCP_ALLOWED_TOOLS."
           (obj "type" "object"
                "properties" (obj "provider" (obj "type" "string")
                                  "values" (obj "type" "array" "items" (obj "type" "number"))
                                  "agent" (obj "type" "string")
                                  "correlationId" (obj "type" "string")
                                  "envelopeId" (obj "type" "string")
                                  "sourceMappingId" (obj "type" "string")
                                  "mappingId" (obj "type" "string")
                                  "sensorId" (obj "type" "string")
                                  "region" (obj "type" "object")
                                  "ttlMs" (obj "type" "number"))
                "required" (arr "provider" "values"))
           (lambda (args)
             (let ((tool-name "integrations.completion"))
               (if (mcp-mutating-tool-allowed-p tool-name)
                   (mcp-cons-json-text
                    (actor-ask actor (lambda (s)
                                       (ingest-completion s args))))
                   (mcp-disabled-by-policy tool-name)))))

     (tool "integrations.post_completion"
           "Alias for integrations.completion."
           (obj "type" "object"
                "properties" (obj "provider" (obj "type" "string")
                                  "values" (obj "type" "array" "items" (obj "type" "number"))
                                  "agent" (obj "type" "string")
                                  "correlationId" (obj "type" "string")
                                  "envelopeId" (obj "type" "string")
                                  "sourceMappingId" (obj "type" "string")
                                  "mappingId" (obj "type" "string")
                                  "sensorId" (obj "type" "string")
                                  "region" (obj "type" "object")
                                  "ttlMs" (obj "type" "number"))
                "required" (arr "provider" "values"))
           (lambda (args)
             (let ((tool-name "integrations.post_completion"))
               (if (mcp-mutating-tool-allowed-p tool-name)
                   (mcp-cons-json-text
                    (actor-ask actor (lambda (s)
                                       (ingest-completion s args))))
                   (mcp-disabled-by-policy tool-name)))))

     (tool "integrations.dispatch_provider"
           "Dispatch an existing ledger record through the requested provider adapter. Mutating; gated by RE_MCP_ALLOW_MUTATION / RE_MCP_ALLOWED_TOOLS."
           (obj "type" "object"
                "properties" (obj "provider" (obj "type" "string" "enum" (arr "openai" "ollama"))
                                  "dispatch_id" (obj "type" "string")
                                  "dispatchId" (obj "type" "string")
                                  "sourceMappingId" (obj "type" "string")
                                  "model" (obj "type" "string"))
                "required" (arr "provider"))
           (lambda (args)
             (let ((tool-name "integrations.dispatch_provider"))
               (if (mcp-mutating-tool-allowed-p tool-name)
                   (let* ((provider (jstring args "provider" ""))
                          (dispatch-id (or (jstring args "dispatch_id" nil)
                                           (jstring args "dispatchId" nil))))
                     (when dispatch-id (setf (jget args "dispatchId") dispatch-id))
                     (cond
                       ((string= provider "openai")
                        (mcp-cons-json-text
                         (actor-ask actor (lambda (s) (dispatch-openai s args)))))
                       ((string= provider "ollama")
                        (mcp-cons-json-text
                         (actor-ask actor (lambda (s) (dispatch-ollama s args)))))
                       (t (mcp-text (json-stringify (obj "error" "Unsupported provider")) t))))
                   (mcp-disabled-by-policy tool-name)))))

     (tool "integrations.dispatch_openai"
           "Dispatch an existing ledger record through the OpenAI adapter. Mutating; gated by RE_MCP_ALLOW_MUTATION / RE_MCP_ALLOWED_TOOLS."
           (obj "type" "object"
                "properties" (obj "dispatch_id" (obj "type" "string")
                                  "dispatchId" (obj "type" "string")
                                  "sourceMappingId" (obj "type" "string")
                                  "model" (obj "type" "string"))
                "required" (arr "dispatch_id"))
           (lambda (args)
             (let ((tool-name "integrations.dispatch_openai"))
               (if (mcp-mutating-tool-allowed-p tool-name)
                   (progn
                     (when (jstring args "dispatch_id" nil)
                       (setf (jget args "dispatchId") (jstring args "dispatch_id")))
                     (mcp-cons-json-text
                      (actor-ask actor (lambda (s) (dispatch-openai s args)))))
                   (mcp-disabled-by-policy tool-name)))))

     (tool "integrations.dispatch_ollama"
           "Dispatch an existing ledger record through the Ollama adapter. Mutating; gated by RE_MCP_ALLOW_MUTATION / RE_MCP_ALLOWED_TOOLS."
           (obj "type" "object"
                "properties" (obj "dispatch_id" (obj "type" "string")
                                  "dispatchId" (obj "type" "string")
                                  "sourceMappingId" (obj "type" "string")
                                  "model" (obj "type" "string"))
                "required" (arr "dispatch_id"))
           (lambda (args)
             (let ((tool-name "integrations.dispatch_ollama"))
               (if (mcp-mutating-tool-allowed-p tool-name)
                   (progn
                     (when (jstring args "dispatch_id" nil)
                       (setf (jget args "dispatchId") (jstring args "dispatch_id")))
                     (mcp-cons-json-text
                      (actor-ask actor (lambda (s) (dispatch-ollama s args)))))
                   (mcp-disabled-by-policy tool-name)))))

     ;; trigger.replay — re-emit a dispatch record's envelope
     (tool "trigger.replay"
           "Replay an existing dispatch record by id — appends a new ledger entry with mode:\"replay\"."
           (obj "type" "object"
                "properties" (obj "dispatch_id" (obj "type" "string")
                                  "fresh_ids" (obj "type" "boolean" "default" nil))
                "required" (arr "dispatch_id"))
           (lambda (args)
             (let* ((dispatch-id (jstring args "dispatch_id" ""))
                    (result (actor-ask actor (lambda (s)
                                               (replay-dispatch-record s dispatch-id)))))
               (if result
                   (mcp-text (json-stringify (obj "success" t "replayOf" dispatch-id "record" result)))
                   (mcp-text (json-stringify (obj "error" "Dispatch record not found")) t))))))))

;; ── Resource reads ────────────────────────────────────────────────────────────

(defun mcp-read-resource (uri actor)
  (cond
    ((string= uri "perception://state")
     (obj "uri" uri "mimeType" "application/json"
          "text" (json-stringify
                   (actor-ask actor (lambda (s) (perception-state-json (perception-state-engine s)))))))
    ((string= uri "perception://sources")
     (obj "uri" uri "mimeType" "application/json"
          "text" (json-stringify
                   (actor-ask actor (lambda (s)
                                      (vectorize (mapcar #'source-json
                                                         (object-values
                                                          (perception-engine-sources
                                                           (perception-state-engine s))))))))))
    ((string= uri "perception://vector")
     (obj "uri" uri "mimeType" "application/json"
          "text" (json-stringify
                   (actor-ask actor (lambda (s)
                                      (vectorize (assemble-perception-vector
                                                  (perception-state-engine s))))))))
    (t (obj "uri" uri "error" "Unknown resource URI"))))

;; ── JSON-RPC dispatcher ───────────────────────────────────────────────────────

(defun mcp-dispatch (body actor tools)
  "Dispatch one JSON-RPC request. Returns the response object or nil (for notifications)."
  (declare (ignore actor))
  (let* ((method (jstring body "method" ""))
         (params (or (jget body "params") (obj)))
         (id (jget body "id")))

    (cond
      ((string= method "initialize")
       (mcp-ok id
         (obj "protocolVersion" "2024-11-05"
              "capabilities" (obj "tools" (obj "listChanged" nil)
                                  "resources" (obj "listChanged" nil))
              "serverInfo" (obj "name" "reality-engine-perception-lsp" "version" "1.0.0"))))

      ((or (string= method "notifications/initialized")
           (string= method "initialized"))
       nil)

      ((string= method "ping")
       (mcp-ok id (obj)))

      ((string= method "tools/list")
       (mcp-ok id
         (obj "tools"
              (vectorize (mapcar (lambda (tool)
                                   (obj "name" (getf tool :name)
                                        "description" (getf tool :description)
                                        "inputSchema" (getf tool :schema)))
                                 tools)))))

      ((string= method "tools/call")
       (let* ((tool-name (jstring params "name" ""))
              (arguments (or (jget params "arguments") (obj)))
              (tool (find tool-name tools :key (lambda (tool) (getf tool :name)) :test #'string=)))
         (mcp-ok id
           (if tool
               (handler-case
                   (funcall (getf tool :fn) arguments)
                 (error (e) (mcp-text (format nil "Tool error: ~a" e) t)))
               (mcp-text (format nil "Unknown tool: ~a" tool-name) t)))))

      ((string= method "resources/list")
       (mcp-ok id
         (obj "resources"
              (arr (obj "uri" "perception://state"  "name" "perception-state"  "mimeType" "application/json")
                   (obj "uri" "perception://sources" "name" "perception-sources" "mimeType" "application/json")
                   (obj "uri" "perception://vector"  "name" "perception-vector"  "mimeType" "application/json")))))

      ((string= method "resources/read")
       (mcp-ok id
         (obj "contents" (arr (mcp-read-resource (jstring params "uri" "") actor)))))

      (t
       (when id
        (mcp-err id -32601 (format nil "Method not found: ~a" method)))))))

;; ── HTTP transport ────────────────────────────────────────────────────────────

(defun mcp-handle-post (actor tools)
  "Handle POST /mcp — initialize or dispatch JSON-RPC."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (let* ((body (request-body-json))
         (session-id (hunchentoot:header-in* "mcp-session-id")))
    (cond
      ;; Existing session dispatch
      ((and session-id (mcp-find-session session-id))
       (let ((response (mcp-dispatch body actor tools)))
         (if response (json-stringify response) "")))
      ;; New session initialization (no session-id or explicit initialize method)
      ((or (null session-id)
           (string= (jstring body "method" "") "initialize"))
       (let* ((session (mcp-new-session))
              (response (mcp-dispatch body actor tools)))
         (setf (hunchentoot:header-out "mcp-session-id") (mcp-session-id session))
         (if response (json-stringify response) "")))
      ;; Unknown session
      (t
       (setf (hunchentoot:return-code*) 400)
       (json-stringify (obj "error" "Invalid or missing mcp-session-id"))))))

(defun mcp-handle-get ()
  "Handle GET /mcp — SSE notification stream for an existing session."
  (let ((session-id (hunchentoot:header-in* "mcp-session-id")))
    (unless (and session-id (mcp-find-session session-id))
      (setf (hunchentoot:return-code*) 400)
      (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
      (return-from mcp-handle-get
        (json-stringify (obj "error" "Invalid or missing mcp-session-id"))))
    (let* ((session (mcp-find-session session-id))
           (stream (progn
                     (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
                     (setf (hunchentoot:header-out "Cache-Control") "no-cache")
                     (hunchentoot:send-headers))))
      (handler-case
          (progn
            (write-string ": connected\n\n" stream)
            (force-output stream)
            (loop
              (let ((ev (mcp-dequeue-notification session 15)))
                (handler-case
                    (progn
                      (if ev
                          (format stream "data: ~a~%~%" (json-stringify ev))
                          (write-string ": keepalive\n\n" stream))
                      (force-output stream))
                  (error () (return))))))
        (error () nil))
      "")))

(defun mcp-handle-delete ()
  "Handle DELETE /mcp — close session."
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (let ((session-id (hunchentoot:header-in* "mcp-session-id")))
    (when session-id (mcp-close-session session-id)))
  (json-stringify (obj "success" t)))

(defun make-mcp-dispatchers (actor)
  "Build a single hunchentoot dispatcher for the /mcp surface.
Method routing (POST/GET/DELETE) is performed inside the handler."
  (let ((tools (mcp-build-tools actor (actor-ask actor #'perception-state-reality-url))))
    (list
     (hunchentoot:create-prefix-dispatcher
      "/mcp"
      (lambda ()
        (let ((method (symbol-name (hunchentoot:request-method*))))
          (cond
            ((string= method "POST")   (mcp-handle-post actor tools))
            ((string= method "GET")    (mcp-handle-get))
            ((string= method "DELETE") (mcp-handle-delete))
            (t (setf (hunchentoot:return-code*) 405)
               (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
               (json-stringify (obj "error" "Method Not Allowed"))))))))))
