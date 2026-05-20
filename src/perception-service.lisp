(in-package #:reality-engine-lsp)

(defstruct perception-state
  engine reality-url localai-url localai-machine-dir push-records started-at
  integrations-config-path integrations-loaded-p integrations-load-error integrations source-mappings
  triggers-enabled-p trigger-dispatch-mode trigger-graphql-url envelopes-created dispatch-errors
  dropped-no-governance dropped-no-dispatch
  dispatch-ledger dispatch-ledger-limit
  ollama-base-url ollama-model ollama-completion-source-mapping-id
  openai-base-url openai-model openai-completion-source-mapping-id openai-api-key
  healthkit-bridge-id healthkit-default-source-mapping-id healthkit-bridge-token
  carekit-bridge-id carekit-default-source-mapping-id carekit-bridge-token
  ;; MQTT bridge — NIL when the bridge isn't configured (no MQTT_BROKER_HOST).
  ;; When non-NIL, owned by the perception service and torn down on stop.
  mqtt-bridge)

(defun make-perception-state-from-config (&key dimension reality-url localai-url localai-machine-dir)
  (make-perception-state
   :engine (make-perception-engine-state dimension)
   :reality-url reality-url
   :localai-url localai-url
   :localai-machine-dir localai-machine-dir
   :push-records (make-hash-table :test #'equal)
   :started-at (now-ms)
   :integrations-config-path nil
   :integrations-loaded-p nil
   :integrations-load-error nil
   :integrations (arr)
   :source-mappings (make-default-source-mappings)
   :triggers-enabled-p (env-bool "TRIGGERS_ENABLED" nil)
   :trigger-dispatch-mode (env "TRIGGER_DISPATCH_MODE" "dry-run")
   :trigger-graphql-url (env "TRIGGER_GRAPHQL_URL" (format nil "~a/graphql" localai-url))
   :envelopes-created 0
   :dispatch-errors 0
   :dropped-no-governance 0
   :dropped-no-dispatch 0
   :dispatch-ledger nil
   :dispatch-ledger-limit (env-int "TRIGGER_DISPATCH_LEDGER_LIMIT" 100)
   :ollama-base-url (trim-trailing-slashes (env "OLLAMA_BASE_URL" "http://localhost:11434"))
   :ollama-model (env "OLLAMA_MODEL" "gpt-oss:20b")
   :ollama-completion-source-mapping-id (env "OLLAMA_COMPLETION_SOURCE_MAPPING_ID" "agent-completion-risk")
   :openai-base-url (trim-trailing-slashes (env "OPENAI_BASE_URL" "https://api.openai.com/v1"))
   :openai-model (env "OPENAI_MODEL" "gpt-5")
   :openai-completion-source-mapping-id (env "OPENAI_COMPLETION_SOURCE_MAPPING_ID" "agent-completion-risk")
   :openai-api-key (or (env "OPENAI_API_KEY" nil) "")
   :healthkit-bridge-id (env "HEALTHKIT_BRIDGE_ID" "healthkit-ios-bridge")
   :healthkit-default-source-mapping-id (env "HEALTHKIT_DEFAULT_SOURCE_MAPPING_ID" "healthkit-activity")
   :healthkit-bridge-token (env "HEALTHKIT_BRIDGE_TOKEN" nil)
   :carekit-bridge-id (env "CAREKIT_BRIDGE_ID" "carekit-ios-bridge")
   :carekit-default-source-mapping-id (env "CAREKIT_DEFAULT_SOURCE_MAPPING_ID" "carekit-task")
   :carekit-bridge-token (env "CAREKIT_BRIDGE_TOKEN" nil)
   :mqtt-bridge nil))

(defun trim-trailing-slashes (value)
  (let ((end (length value)))
    (loop while (and (> end 0) (char= (char value (1- end)) #\/))
          do (decf end))
    (subseq value 0 end)))

(defun make-default-source-mappings ()
  (let ((mappings (make-hash-table :test #'equal)))
    (setf (gethash "agent-completion-risk" mappings)
          (obj "id" "agent-completion-risk"
               "sensorIdTemplate" "agent.{agent}.completion"
               "region" (obj "offset" 4200 "length" 4)
               "ttlMs" 300000
               "pushMode" "debounced"
               "debounceMs" 250)
          (gethash "healthkit-activity" mappings)
          (obj "id" "healthkit-activity"
               "sensorIdTemplate" "healthkit.{sampleType}"
               "region" (obj "offset" 4300 "length" 4)
               "ttlMs" 900000
               "pushMode" "debounced"
               "debounceMs" 250)
          (gethash "carekit-task" mappings)
          (obj "id" "carekit-task"
               "sensorIdTemplate" "carekit.{sampleType}"
               "region" (obj "offset" 4310 "length" 4)
               "ttlMs" 900000
               "pushMode" "debounced"
               "debounceMs" 250))
    mappings))

(defun localai-sensor-specs ()
  (list (list "localai_rag_retrieval" "localai/rag_retrieval" 52 4 30000)
        (list "localai_rag_grading" "localai/rag_grading" 56 4 30000)
        (list "localai_agent_activity" "localai/agent_activity" 64 4 30000)))

(defun localai-machine-files ()
  '("rag_corrective_cycle.json"
    "session_rag_context.json"
    "session_agent_context.json"
    "ai_load_bridge.json"
    "agent_activity_classifier.json"))

(defun sensor-exists-p (engine sensor-id)
  (find sensor-id (object-values (perception-engine-sources engine))
        :test #'string=
        :key #'source-sensor-id))

(defun bootstrap-localai (state)
  (let ((registered nil)
        (skipped nil)
        (failed nil)
        (imported nil)
        (engine (perception-state-engine state)))
    (dolist (spec (localai-sensor-specs))
      (destructuring-bind (sensor-id name offset length ttl-ms) spec
        (if (sensor-exists-p engine sensor-id)
            (push sensor-id skipped)
            (let ((source (ensure-source-id
                           engine
                           (make-source :id sensor-id
                                        :kind "sensor"
                                        :name name
                                        :active-p t
                                        :region (make-region :offset offset :length length)
                                        :sensor-id sensor-id
                                        :last-value nil
                                        :last-updated 0
                                        :ttl-ms ttl-ms))))
              (push (source-json source) registered)))))
    (dolist (filename (localai-machine-files))
      (let ((path (merge-pathnames filename (uiop:ensure-directory-pathname (perception-state-localai-machine-dir state)))))
        (handler-case
            (let ((raw (safe-read-file path)))
              (push (http-post-json (format nil "~a/api/machines" (perception-state-reality-url state))
                                    (parse-json raw))
                    imported))
          (error (condition)
            (push (obj "file" filename "error" (princ-to-string condition)) failed)))))
    (obj "success" t
         "registeredSensors" (vectorize (nreverse registered))
         "skippedSensors" (vectorize (nreverse skipped))
         "importedMachines" (vectorize (nreverse imported))
         "skippedMachines" (arr)
         "failedMachines" (vectorize (nreverse failed))
         "localAIBaseUrl" (perception-state-localai-url state))))

(defun consolidate-stale-test-sources (engine)
  "Migration: prior bootstrap shapes created one source per sequence;
clear any group of >1 test source for the same machineId so the
consolidated source replaces them.  Idempotent for single-source
machines."
  (let ((sources-table (perception-engine-sources engine))
        (per-machine (make-hash-table :test #'equal)))
    (maphash (lambda (id src)
               (when (and (string= (source-kind src) "test")
                          (source-machine-id src))
                 (push id (gethash (source-machine-id src) per-machine))))
             sources-table)
    (maphash (lambda (_mid ids)
               (declare (ignore _mid))
               (when (> (length ids) 1)
                 (dolist (id ids) (remhash id sources-table))))
             per-machine)))

(defun bootstrap-test-sources-from-machines (state)
  "Fetch /api/machines from the Reality Engine and materialize one
consolidated test source per machine — its `inputs` is the flat
concatenation of every authored inputSequence, so the first sequence
completes before the second begins and `loop=t` wraps the entire set.
Per-sequence boundaries live in metadata.segments for UI display."
  (consolidate-stale-test-sources (perception-state-engine state))
  (let* ((engine (perception-state-engine state))
         (sources-table (perception-engine-sources engine))
         (existing (make-hash-table :test #'equal))
         (created 0) (skipped 0) (machines-seen 0) (errors nil))
    (maphash (lambda (_ src)
               (declare (ignore _))
               (when (and (string= (source-kind src) "test")
                          (source-machine-id src))
                 (setf (gethash (source-machine-id src) existing) t)))
             sources-table)
    (let ((data (handler-case
                    (http-get-json (format nil "~a/api/machines"
                                           (perception-state-reality-url state)))
                  (error (c) (push (princ-to-string c) errors) nil))))
      (when data
        (let ((machines (jarray-list (or (jget data "machines") (arr)))))
          (setf machines-seen (length machines))
          (dolist (machine machines)
            (let* ((mid (jstring machine "id" nil))
                   (mname (or (jstring machine "name" nil) mid))
                   (mapping (jget machine "perceptualMapping"))
                   (input-region (and (jobject-p mapping) (jget mapping "input")))
                   (metadata (jget machine "metadata"))
                   (input-sequences
                    (and (jobject-p metadata)
                         (jarray-list (or (jget metadata "inputSequences") (arr))))))
              (cond
                ((or (null mid) (gethash mid existing)
                     (not (jobject-p input-region))
                     (null input-sequences))
                 (incf skipped))
                (t
                 (let ((concat-inputs nil) (segments nil))
                   (dolist (seq input-sequences)
                     (let ((vectors (jarray-list (or (jget seq "vectors") (arr))))
                           (seq-name (or (jstring seq "name" nil) "Test sequence")))
                       (when vectors
                         (push (obj "name" seq-name "length" (length vectors)) segments)
                         (dolist (v vectors)
                           (push (numbers-from-json v) concat-inputs)))))
                   (if concat-inputs
                       (let* ((seg-list (nreverse segments))
                              (inputs-list (nreverse concat-inputs))
                              (label (if (= 1 (length seg-list))
                                         (jstring (first seg-list) "name" "Test")
                                         (format nil "~a sequences" (length seg-list))))
                              (src (make-source
                                    :id (format nil "test-~a" mid)
                                    :kind "test"
                                    :name (format nil "~a / ~a" mname label)
                                    :active-p nil
                                    :region (make-region
                                             :offset (truncate (or (jnumber input-region "offset" 0) 0))
                                             :length (truncate (or (jnumber input-region "length" 0) 0)))
                                    :machine-id mid
                                    :machine-name mname
                                    :sequence-name label
                                    :sequence-metadata (obj "segments" (vectorize seg-list))
                                    :test-sequence (obj)
                                    :inputs inputs-list
                                    :cursor 0
                                    :loop-p t)))
                         (ensure-source-id engine src)
                         (setf (gethash mid existing) t)
                         (incf created))
                       (incf skipped))))))))))
    (obj "created" created
         "skipped" skipped
         "machinesSeen" machines-seen
         "errors" (vectorize (mapcar #'identity errors))
         "vectorSize" (perception-engine-dimension engine))))

(defun localai-status-json (state)
  (let ((sensors nil))
    (dolist (spec (localai-sensor-specs))
      (destructuring-bind (sensor-id name offset length ttl-ms) spec
        (declare (ignore ttl-ms))
        (push (obj "sensorId" sensor-id
                   "name" name
                   "region" (region-json (make-region :offset offset :length length))
                   "registered" (json-bool (sensor-exists-p (perception-state-engine state) sensor-id)))
              sensors)))
    (obj "localAIBaseUrl" (perception-state-localai-url state)
         "reachable" +json-false+
         "root" +json-null+
         "health" +json-null+
         "sensors" (vectorize (nreverse sensors))
         "machineDirectory" (perception-state-localai-machine-dir state))))

(defun localai-catalog-json (state)
  (obj "success" t
       "status" (localai-status-json state)
       "graphSchema" +json-null+
       "recentGraphQLEvents" +json-null+
       "invokeEndpoint" "/api/integrations/localai/invoke"
       "allowedEndpoints" (vectorize
                           (list (obj "id" "health" "method" "GET" "path" "/health")
                                 (obj "id" "graph_schema" "method" "GET" "path" "/graph/schema")
                                 (obj "id" "graph_rag" "method" "POST" "path" "/graph/rag")
                                 (obj "id" "graph_agent" "method" "POST" "path" "/graph/agent")
                                 (obj "id" "rag_query" "method" "POST" "path" "/rag/query")
                                 (obj "id" "rag_ingest_text" "method" "POST" "path" "/rag/ingest/text")
                                 (obj "id" "chat" "method" "POST" "path" "/chat")
                                 (obj "id" "graphql" "method" "POST" "path" "/graphql")))
       "realityBridge" (obj "sensors" (vectorize '("localai_rag_retrieval"
                                                    "localai_rag_grading"
                                                    "localai_agent_activity"))
                            "bootstrapEndpoint" "/api/integrations/localai/bootstrap"
                            "signalEndpoint" "/api/signals")))

(defun endpoint-allowed-p (endpoint)
  (let ((path (first (split-string endpoint #\?)))
        (allowed '("/" "/health" "/chat" "/rag/query" "/rag/ingest/text"
                   "/graph/schema" "/graph/rag" "/graph/agent" "/graphql")))
    (some (lambda (prefix)
            (or (string= path prefix)
                (and (not (string= prefix "/"))
                     (string-prefix-p (format nil "~a/" prefix) path))))
          allowed)))

(defun invoke-localai (state body)
  (let* ((method (string-upcase (or (jstring body "method" nil) "POST")))
         (endpoint (or (jstring body "endpoint" nil) (jstring body "path" nil))))
    (unless endpoint
      (return-from invoke-localai (obj "success" +json-false+ "error" "localAI invocation requires endpoint or path")))
    (unless (char= (char endpoint 0) #\/)
      (setf endpoint (format nil "/~a" endpoint)))
    (unless (endpoint-allowed-p endpoint)
      (return-from invoke-localai (obj "success" +json-false+ "endpoint" endpoint "method" method "error" "localAI endpoint is not allowed")))
    (handler-case
        (let ((response (if (string= method "GET")
                            (http-get-json (format nil "~a~a" (perception-state-localai-url state) endpoint))
                            (http-post-json (format nil "~a~a" (perception-state-localai-url state) endpoint)
                                            (or (jget body "payload") (obj))))))
          (obj "success" t "endpoint" endpoint "method" method "response" response))
      (error (condition)
        (obj "success" +json-false+ "endpoint" endpoint "method" method "error" (princ-to-string condition))))))

(defun load-integrations-config (state)
  (let* ((configured (env "INTEGRATIONS_CONFIG" nil))
         (default (probe-file "config/integrations.json"))
         (path (or configured (and default (namestring default)))))
    (when path
      (setf (perception-state-integrations-config-path state) path)
      (handler-case
          (let* ((root (parse-json (safe-read-file path)))
                 (integrations (or (jget root "integrations") (arr)))
                 (source-mappings (or (jget root "sourceMappings") (arr))))
            (when (jarray-p source-mappings)
              (dolist (mapping (jarray-list source-mappings))
                (let ((id (jstring mapping "id" nil)))
                  (when id
                    (setf (gethash id (perception-state-source-mappings state)) mapping)))))
            (when (jarray-p integrations)
              (dolist (item (jarray-list integrations))
                (let ((kind (jstring item "kind" "")))
                  (cond
                    ((string= kind "ollama")
                     (when (jstring item "baseUrl" nil)
                       (setf (perception-state-ollama-base-url state)
                             (trim-trailing-slashes (jstring item "baseUrl"))))
                     (when (jstring item "model" nil)
                       (setf (perception-state-ollama-model state) (jstring item "model")))
                     (when (jstring item "completionSourceMappingId" nil)
                       (setf (perception-state-ollama-completion-source-mapping-id state)
                             (jstring item "completionSourceMappingId"))))
                    ((string= kind "openai")
                     (when (jstring item "baseUrl" nil)
                       (setf (perception-state-openai-base-url state)
                             (trim-trailing-slashes (jstring item "baseUrl"))))
                     (when (jstring item "model" nil)
                       (setf (perception-state-openai-model state) (jstring item "model")))
                     (when (jstring item "completionSourceMappingId" nil)
                       (setf (perception-state-openai-completion-source-mapping-id state)
                             (jstring item "completionSourceMappingId"))))
                    ((string= kind "healthkit")
                     (when (jstring item "bridgeId" nil)
                       (setf (perception-state-healthkit-bridge-id state) (jstring item "bridgeId")))
                     (when (jstring item "defaultSourceMappingId" nil)
                       (setf (perception-state-healthkit-default-source-mapping-id state)
                              (jstring item "defaultSourceMappingId"))))
                    ((string= kind "carekit")
                     (when (jstring item "bridgeId" nil)
                       (setf (perception-state-carekit-bridge-id state) (jstring item "bridgeId")))
                     (when (jstring item "defaultSourceMappingId" nil)
                       (setf (perception-state-carekit-default-source-mapping-id state)
                              (jstring item "defaultSourceMappingId"))))))))
            (setf (perception-state-integrations state) integrations
                  (perception-state-integrations-loaded-p state) t
                  (perception-state-integrations-load-error state) nil))
        (error (condition)
          (setf (perception-state-integrations-loaded-p state) nil
                (perception-state-integrations-load-error state) (princ-to-string condition))))))
  state)

(defun source-mapping-by-id (state id)
  (or (and id (gethash id (perception-state-source-mappings state)))
      (gethash "agent-completion-risk" (perception-state-source-mappings state))))

(defun replace-all-substrings (value token replacement)
  (let ((out value)
        (start 0))
    (loop for pos = (search token out :start2 start)
          while pos
          do (setf out (concatenate 'string
                                    (subseq out 0 pos)
                                    replacement
                                    (subseq out (+ pos (length token)))))
             (setf start (+ pos (length replacement))))
    out))

(defun source-id-part (value)
  (let ((text (or value "unknown")))
    (with-output-to-string (out)
      (loop for ch across text
            do (write-char (if (or (alphanumericp ch) (member ch '(#\- #\_) :test #'char=))
                               (char-downcase ch)
                               #\-)
                           out)))))

(defun render-sensor-template (template bindings)
  (let ((out template))
    (dolist (binding bindings)
      (setf out (replace-all-substrings out
                                        (format nil "{~a}" (car binding))
                                        (source-id-part (cdr binding)))))
    out))

(defun commit-signal-source (state sensor-id name region values ttl-ms)
  (let* ((engine (perception-state-engine state))
         (source (sensor-exists-p engine sensor-id)))
    (unless source
      (setf source (ensure-source-id
                    engine
                    (make-source :id sensor-id
                                 :kind "sensor"
                                 :name name
                                 :active-p t
                                 :region region
                                 :sensor-id sensor-id
                                 :last-value nil
                                 :last-updated 0
                                 :ttl-ms ttl-ms))))
    (setf (source-name source) name
          (source-region source) region
          (source-ttl-ms source) ttl-ms
          (source-last-value source) values
          (source-last-updated source) (now-ms))
    source))

(defun signal-body-region (body values)
  (if (jobject-p (jget body "region"))
      (make-region-from-json (jget body "region"))
      (make-region :offset 0 :length (length values))))

(defun ingest-signal-body (state body &key default-sensor-id default-name default-region default-ttl-ms)
  (let* ((values (numbers-from-json (or (jget body "values") (jget body "vector") (arr))))
         (sensor-id (or (jstring body "sensorId" nil)
                        (jstring body "id" nil)
                        default-sensor-id
                        "localai_agent_activity"))
         (name (or (jstring body "name" nil) default-name sensor-id))
         (region (or default-region (signal-body-region body values)))
         (ttl-ms (or (jnumber body "ttlMs" nil) default-ttl-ms 30000))
         (source (commit-signal-source state sensor-id name region values ttl-ms))
         (push-result +json-null+))
    (when (jbool body "triggerPush" nil)
      (setf push-result (push-perception state (not (jbool body "compactPush" nil)))))
    (obj "success" t
         "sensorId" sensor-id
         "source" (source-json source)
         "push" push-result
         "timestamp" (now-ms))))

(defun integrations-status-json (state)
  (obj "loaded" (json-bool (perception-state-integrations-loaded-p state))
       "path" (or (perception-state-integrations-config-path state) +json-null+)
       "error" (or (perception-state-integrations-load-error state) +json-null+)
       "integrationCount" (if (jarray-p (perception-state-integrations state))
                              (length (jarray-list (perception-state-integrations state)))
                              0)
       "integrations" (perception-state-integrations state)
       "sourceMappings" (vectorize (mapcar (lambda (id)
                                             (gethash id (perception-state-source-mappings state)))
                                           (object-keys-sorted (perception-state-source-mappings state))))
       "completionEndpoint" "/api/integrations/completions"
       "ollama" (ollama-status-json state nil)
       "openai" (openai-status-json state nil)
       "healthkit" (healthkit-status-json state)
       "carekit" (carekit-status-json state)))

(defun triggers-status-json (state)
  (obj "enabled" (json-bool (perception-state-triggers-enabled-p state))
       "mode" (perception-state-trigger-dispatch-mode state)
       "graphqlUrl" (perception-state-trigger-graphql-url state)
       "envelopesCreated" (perception-state-envelopes-created state)
       "dispatchErrors" (perception-state-dispatch-errors state)
       "droppedNoGovernance" (perception-state-dropped-no-governance state)
       "droppedNoDispatch" (perception-state-dropped-no-dispatch state)
       "ledgerSize" (length (perception-state-dispatch-ledger state))))

(defun dispatch-record-json (record)
  record)

(defun ledger-json (state)
  (obj "records" (vectorize (mapcar #'dispatch-record-json (perception-state-dispatch-ledger state)))
       "count" (length (perception-state-dispatch-ledger state))
       "triggers" (triggers-status-json state)))

(defun lookup-dispatch-record (state id)
  (find id (perception-state-dispatch-ledger state)
        :test #'string=
        :key (lambda (record) (jstring record "id" ""))))

(defun update-dispatch-record (state id body)
  (let ((record (lookup-dispatch-record state id)))
    (unless record
      (return-from update-dispatch-record nil))
    (dolist (field '("status" "adapter" "provider" "externalRunId" "lastError"))
      (when (jstring body field nil)
        (setf (jget record field) (jstring body field))))
    (when (jobject-p (jget body "metadata"))
      (setf (jget record "metadata") (jget body "metadata")))
    (when (jbool body "incrementAttempts" nil)
      (setf (jget record "attempts") (1+ (or (jnumber record "attempts" nil) 0))))
    (setf (jget record "updatedAt") (now-ms))
    record))

;; ── Dispatch helpers — wire-compatible with CPP dispatch_triggers /
;;    TypeScript Dispatcher.onStep.  Drop rules and full envelope shape
;;    match both reference implementations exactly. ───────────────────────────

(defun fetch-machine-for-dispatch (state machine-id)
  "Fetch the full machine object from RE by id. Returns NIL on error or when
the machine is absent — the caller treats NIL as a drop-no-dispatch signal."
  (handler-case
      (let* ((response (http-get-json (format nil "~a/api/machines/~a"
                                              (perception-state-reality-url state)
                                              machine-id)))
             (machine (jget response "machine")))
        (if (jobject-p machine) machine nil))
    (error () nil)))

(defun ces-semantics-from-values (values)
  "Build outputVector.semantics: [{index, label}…] matching CPP / TS helpers."
  (let ((cells nil) (i 0))
    (dolist (v (if (jarray-p values) (jarray-list values) nil))
      (declare (ignore v))
      (push (obj "index" i "label" (format nil "cell_~a" i)) cells)
      (incf i))
    (vectorize (nreverse cells))))

(defun ces-asserted-label (values)
  "Build assertedLabel: 'cell_i+cell_j' for non-zero cells, or 'none'."
  (let ((labels nil) (i 0))
    (dolist (v (if (jarray-p values) (jarray-list values) nil))
      (when (and (numberp v) (/= v 0))
        (push (format nil "cell_~a" i) labels))
      (incf i))
    (if labels (format nil "~{~a~^+~}" (nreverse labels)) "none")))

(defun ces-first-agent-action (metadata)
  "Return the first string in metadata.agentActions, or empty string."
  (let ((actions (jget metadata "agentActions")))
    (if (and (jarray-p actions) (jarray-list actions))
        (let ((first-elem (first (jarray-list actions))))
          (if (stringp first-elem) first-elem ""))
        "")))

(defun ces-copy-string-array (value)
  "Return a JSON array containing only the string elements of value."
  (if (jarray-p value)
      (vectorize (remove-if-not #'stringp (jarray-list value)))
      (arr)))

(defun build-ces-envelope (op machine envelope-id correlation-id state)
  "Build a full ces.terminal.event envelope.
Wire-compatible with CPP build_trigger_envelope and TS buildTriggerEnvelope."
  (let* ((md (or (jget machine "metadata") (obj)))
         (values (or (jget op "values") (arr)))
         (sequence-id (jstring op "sequenceId" ""))
         (machine-id (jstring op "machineId" ""))
         (mode (perception-state-trigger-dispatch-mode state))
         (graphql-p (string= mode "graphql")))
    (obj "schemaVersion" "1.0.0"
         "envelopeType" "ces.terminal.event"
         "envelopeId" envelope-id
         "correlationId" correlation-id
         "emittedAtMs" (now-ms)
         "source" (obj "engine" "PE"
                       "observedEngine" "RE"
                       "endpoint" (perception-state-reality-url state))
         "ces" (obj "machineId" machine-id
                    "machineName" (or (jstring machine "name" nil) machine-id)
                    "machineCode" (jstring md "machineCode" "")
                    "sequenceId" sequence-id
                    "sequenceName" sequence-id
                    "outputIndex" (or (jnumber op "outputIndex" nil) 0)
                    "stepNumber" 0
                    "perceptualMapping" (obj "output" (or (jget op "region") +json-null+))
                    "provenance" (if (jarray-p (jget op "provenance"))
                                     (jget op "provenance") (arr))
                    "deprecation" (or (jget op "deprecation") +json-null+))
         "outputVector" (obj "values" values
                             "encoding" "vector"
                             "semantics" (ces-semantics-from-values values)
                             "assertedLabel" (ces-asserted-label values))
         "projection" +json-null+
         "governance" (or (jget op "governance") +json-null+)
         "dispatch" (obj "agent" (jstring md "dispatchableAgent" "")
                         "action" (ces-first-agent-action md)
                         "agentActionsCatalog" (ces-copy-string-array (jget md "agentActions"))
                         "trigger" (jstring md "aiTrigger" "")
                         "endpoint" (obj "kind" mode
                                         "url" (if graphql-p
                                                   (perception-state-trigger-graphql-url state)
                                                   "")
                                         "mutation" (if graphql-p "updateProcessState" "")
                                         "schemaRef" (if graphql-p
                                                         "localAIStack/services/api/routers/graphql_endpoint.py"
                                                         ""))))))

(defun record-dispatch-envelope (state operation)
  "Build and ledger one dispatch record for a merge operation.
Returns the record on success, or NIL when the machine lacks
dispatchableAgent / aiTrigger — the drop-no-dispatch signal to the caller.
Wire-compatible with CPP DispatchRecord and TS Dispatcher.recordFromEnvelope."
  (let* ((machine-id (jstring operation "machineId" ""))
         (machine (fetch-machine-for-dispatch state machine-id))
         (md (and machine (jget machine "metadata")))
         (agent (and md (jstring md "dispatchableAgent" nil)))
         (trigger (and md (jstring md "aiTrigger" nil))))
    ;; Drop — no dispatchableAgent or no aiTrigger (matches CPP line 2175-2179).
    (when (or (null machine)
              (null agent) (string= agent "")
              (null trigger) (string= trigger ""))
      (return-from record-dispatch-envelope nil))
    (let* ((envelope-id (make-id "trigger-envelope"))
           (correlation-id (make-id "trigger-correlation"))
           (dispatch-id (make-id "dispatch"))
           (governance (or (jget operation "governance") (obj)))
           (now (now-ms))
           (envelope (build-ces-envelope operation machine envelope-id correlation-id state))
           (record (obj "id" dispatch-id
                        "envelopeId" envelope-id
                        "correlationId" correlation-id
                        "status" "recorded"
                        "mode" (perception-state-trigger-dispatch-mode state)
                        "target" agent
                        "machineId" machine-id
                        "sequenceId" (jstring operation "sequenceId" "")
                        "ragStatusCode" (jstring governance "ragStatusCode" "")
                        "processStatus" (jstring governance "processStatus" "")
                        "adapter" +json-null+
                        "provider" +json-null+
                        "externalRunId" +json-null+
                        "lastError" +json-null+
                        "attempts" 0
                        "createdAt" now
                        "updatedAt" now
                        "envelope" envelope)))
      (push record (perception-state-dispatch-ledger state))
      (when (> (length (perception-state-dispatch-ledger state))
               (perception-state-dispatch-ledger-limit state))
        (setf (perception-state-dispatch-ledger state)
              (subseq (perception-state-dispatch-ledger state)
                      0
                      (perception-state-dispatch-ledger-limit state))))
      (incf (perception-state-envelopes-created state))
      record)))

(defun record-dispatch-envelopes-from-step (state step)
  "Process mergeBatch from a RE step, applying the same drop rules as CPP
dispatch_triggers and TS Dispatcher.onStep: drop ops without governance
(droppedNoGovernance) or without dispatchableAgent+aiTrigger (droppedNoDispatch)."
  (when (perception-state-triggers-enabled-p state)
    (let ((records nil))
      (dolist (operation (jarray-list (or (jget step "mergeBatch") (arr))))
        (cond
          ((not (jobject-p (jget operation "governance")))
           (incf (perception-state-dropped-no-governance state)))
          (t
           (handler-case
               (let ((record (record-dispatch-envelope state operation)))
                 (if record
                     (push record records)
                     (incf (perception-state-dropped-no-dispatch state))))
             (error ()
               (incf (perception-state-dispatch-errors state)))))))
      (vectorize (nreverse records)))))

(defun completion-values-from-content (content)
  (handler-case
      (let ((parsed (parse-json content)))
        (cond
          ((jarray-p (jget parsed "values")) (jget parsed "values"))
          ((and (jobject-p (jget parsed "completion"))
                (jarray-p (jget (jget parsed "completion") "values")))
           (jget (jget parsed "completion") "values"))
          (t nil)))
    (error () nil)))

(defun ingest-completion (state body)
  (let* ((provider (or (jstring body "provider" nil) "agent"))
         (agent (or (jstring body "agent" nil) provider))
         (mapping-id (or (jstring body "sourceMappingId" nil) "agent-completion-risk"))
         (mapping (source-mapping-by-id state mapping-id))
         (values (or (jget body "values")
                     (jget body "vector")
                     (and (jobject-p (jget body "completion"))
                          (jget (jget body "completion") "values"))))
         (numbers (numbers-from-json values))
         (template (or (jstring mapping "sensorIdTemplate" nil) "agent.{agent}.completion"))
         (sensor-id (or (jstring body "sensorId" nil)
                        (render-sensor-template template
                                                (list (cons "provider" provider)
                                                      (cons "agent" agent)))))
         (region (make-region-from-json (or (jget mapping "region")
                                            (obj "offset" 4200 "length" (length numbers)))))
         (ttl-ms (or (jnumber mapping "ttlMs" nil) 300000))
         (source (commit-signal-source state
                                       sensor-id
                                       (or (jstring body "name" nil)
                                           (format nil "agent:~a/~a/completion" provider agent))
                                       region
                                       numbers
                                       ttl-ms)))
    (obj "success" t
         "completion" (obj "provider" provider
                           "agent" agent
                           "sourceMappingId" mapping-id
                           "completionId" (or (jstring body "completionId" nil)
                                              (jstring body "id" nil)
                                              +json-null+)
                           "correlationId" (or (jstring body "correlationId" nil) +json-null+))
         "source" (source-json source)
         "timestamp" (now-ms))))

(defun healthkit-status-json (state)
  (obj "bridgeId" (perception-state-healthkit-bridge-id state)
       "defaultSourceMappingId" (perception-state-healthkit-default-source-mapping-id state)
       "tokenRequired" (json-bool (perception-state-healthkit-bridge-token state))
       "statusEndpoint" "/api/integrations/healthkit/status"
       "ingestEndpoint" "/api/integrations/healthkit/ingest"))

(defun ingest-healthkit-one (state body)
  (let* ((sample-type (or (jstring body "sampleType" nil) "sample"))
         (mapping-id (or (jstring body "sourceMappingId" nil)
                         (perception-state-healthkit-default-source-mapping-id state)))
         (mapping (source-mapping-by-id state mapping-id))
         (values (numbers-from-json (or (jget body "values") (jget body "vector") (arr))))
         (sensor-id (or (jstring body "sensorId" nil)
                        (render-sensor-template
                         (or (jstring mapping "sensorIdTemplate" nil) "healthkit.{sampleType}")
                         (list (cons "sampleType" sample-type)))))
         (region (make-region-from-json (or (jget mapping "region")
                                            (obj "offset" 4300 "length" (length values)))))
         (ttl-ms (or (jnumber mapping "ttlMs" nil) 900000))
         (source (commit-signal-source state
                                       sensor-id
                                       (or (jstring body "name" nil) (format nil "healthkit:~a" sample-type))
                                       region
                                       values
                                       ttl-ms)))
    (obj "sourceMappingId" mapping-id
         "sampleType" sample-type
         "sensorId" sensor-id
         "source" (source-json source))))

(defun ingest-healthkit (state body)
  (let ((required-token (perception-state-healthkit-bridge-token state)))
    (when (and required-token
               (not (string= required-token (or (jstring body "token" nil)
                                                (jstring body "bridgeToken" nil)
                                                ""))))
      (return-from ingest-healthkit (obj "success" +json-false+ "error" "invalid HealthKit bridge token"))))
  (let ((results nil))
    (if (and (not (eq (jget body "samples" :missing) :missing))
             (jarray-p (jget body "samples")))
        (dolist (sample (jarray-list (jget body "samples")))
          (push (ingest-healthkit-one state sample) results))
        (push (ingest-healthkit-one state body) results))
    (obj "success" t
         "bridgeId" (perception-state-healthkit-bridge-id state)
         "results" (vectorize (nreverse results))
         "timestamp" (now-ms))))

(defun carekit-status-json (state)
  (obj "bridgeId" (perception-state-carekit-bridge-id state)
       "defaultSourceMappingId" (perception-state-carekit-default-source-mapping-id state)
       "tokenRequired" (json-bool (perception-state-carekit-bridge-token state))
       "statusEndpoint" "/api/integrations/carekit/status"
       "ingestEndpoint" "/api/integrations/carekit/ingest"))

(defun ingest-carekit-one (state body)
  (let* ((sample-type (or (jstring body "sampleType" nil) "task-event"))
         (mapping-id (or (jstring body "sourceMappingId" nil)
                         (perception-state-carekit-default-source-mapping-id state)))
         (mapping (source-mapping-by-id state mapping-id))
         (values (numbers-from-json (or (jget body "values") (jget body "vector") (arr))))
         (sensor-id (or (jstring body "sensorId" nil)
                        (render-sensor-template
                         (or (jstring mapping "sensorIdTemplate" nil) "carekit.{sampleType}")
                         (list (cons "sampleType" sample-type)
                               (cons "taskId" (or (jstring body "taskId" nil) sample-type))
                               (cons "carePlanId" (or (jstring body "carePlanId" nil) "care-plan"))))))
         (region (make-region-from-json (or (jget mapping "region")
                                            (obj "offset" 4310 "length" (length values)))))
         (ttl-ms (or (jnumber mapping "ttlMs" nil) 900000))
         (source (commit-signal-source state
                                       sensor-id
                                       (or (jstring body "name" nil) (format nil "carekit:~a" sample-type))
                                       region
                                       values
                                       ttl-ms)))
    (obj "sourceMappingId" mapping-id
         "sampleType" sample-type
         "taskId" (or (jstring body "taskId" nil) +json-null+)
         "carePlanId" (or (jstring body "carePlanId" nil) +json-null+)
         "sensorId" sensor-id
         "source" (source-json source))))

(defun ingest-carekit (state body)
  (let ((required-token (perception-state-carekit-bridge-token state)))
    (when (and required-token
               (not (string= required-token (or (jstring body "token" nil)
                                                (jstring body "bridgeToken" nil)
                                                ""))))
      (return-from ingest-carekit (obj "success" +json-false+ "error" "invalid CareKit bridge token"))))
  (let ((results nil))
    (if (and (not (eq (jget body "samples" :missing) :missing))
             (jarray-p (jget body "samples")))
        (dolist (sample (jarray-list (jget body "samples")))
          (push (ingest-carekit-one state sample) results))
        (push (ingest-carekit-one state body) results))
    (obj "success" t
         "bridgeId" (perception-state-carekit-bridge-id state)
         "results" (vectorize (nreverse results))
         "timestamp" (now-ms))))

(defun ollama-status-json (state &optional (probe t))
  (let ((reachable nil)
        (tags +json-null+)
        (error +json-null+))
    (when probe
      (handler-case
          (progn
            (setf tags (http-get-json (format nil "~a/api/tags" (perception-state-ollama-base-url state))))
            (setf reachable t))
        (error (condition)
          (setf error (princ-to-string condition)))))
    (obj "baseUrl" (perception-state-ollama-base-url state)
         "model" (perception-state-ollama-model state)
         "completionSourceMappingId" (perception-state-ollama-completion-source-mapping-id state)
         "reachable" (json-bool reachable)
         "tags" tags
         "error" error
         "statusEndpoint" "/api/integrations/ollama/status"
         "dispatchEndpoint" "/api/integrations/ollama/dispatch")))

(defun openai-status-json (state &optional (probe t))
  (let ((reachable nil)
        (models +json-null+)
        (error +json-null+))
    (when (and probe (> (length (or (perception-state-openai-api-key state) "")) 0))
      (handler-case
          (progn
            (setf models (http-request-json
                          (format nil "~a/models" (perception-state-openai-base-url state))
                          :method :get
                          :headers (list (cons "Authorization"
                                               (format nil "Bearer ~a" (perception-state-openai-api-key state))))))
            (setf reachable t))
        (error (condition)
          (setf error (princ-to-string condition)))))
    (obj "baseUrl" (perception-state-openai-base-url state)
         "model" (perception-state-openai-model state)
         "hasApiKey" (json-bool (> (length (or (perception-state-openai-api-key state) "")) 0))
         "completionSourceMappingId" (perception-state-openai-completion-source-mapping-id state)
         "reachable" (json-bool reachable)
         "models" models
         "error" error
         "statusEndpoint" "/api/integrations/openai/status"
         "dispatchEndpoint" "/api/integrations/openai/dispatch")))

(defun dispatch-record-prompt (record)
  (json-stringify (or (jget record "envelope") record)))

(defun dispatch-ollama (state body)
  (let* ((id (or (jstring body "dispatchId" nil) (jstring body "id" nil)))
         (record (and id (lookup-dispatch-record state id))))
    (unless record
      (return-from dispatch-ollama (obj "success" +json-false+ "error" "dispatch record not found")))
    (update-dispatch-record state id (obj "status" "delivering" "adapter" "ollama" "provider" "ollama" "incrementAttempts" t))
    (handler-case
        (let* ((model (or (jstring body "model" nil) (perception-state-ollama-model state)))
               (mapping-id (or (jstring body "sourceMappingId" nil)
                               (perception-state-ollama-completion-source-mapping-id state)))
               (payload (obj "model" model
                             "stream" +json-false+
                             "messages" (vectorize
                                         (list (obj "role" "system"
                                                    "content" "Return concise JSON. If committing a PE completion, include numeric values.")
                                               (obj "role" "user"
                                                    "content" (dispatch-record-prompt record))))))
               (response (http-request-json (format nil "~a/api/chat" (perception-state-ollama-base-url state))
                                            :method :post
                                            :payload payload))
               (content (or (jstring (jget response "message") "content" nil) ""))
               (values (completion-values-from-content content))
               (completion +json-null+))
          (when values
            (setf completion (ingest-completion
                              state
                              (obj "provider" "ollama"
                                   "agent" (jstring record "target" "ollama")
                                   "sourceMappingId" mapping-id
                                   "correlationId" id
                                   "values" values))))
          (update-dispatch-record state id (obj "status" "delivered"
                                                "adapter" "ollama"
                                                "provider" "ollama"
                                                "externalRunId" (or (jstring response "created_at" nil)
                                                                    (make-id "ollama-run"))))
          (obj "success" t "provider" "ollama" "response" response "completion" completion))
      (error (condition)
        (update-dispatch-record state id (obj "status" "failed"
                                              "adapter" "ollama"
                                              "provider" "ollama"
                                              "lastError" (princ-to-string condition)))
        (obj "success" +json-false+ "provider" "ollama" "error" (princ-to-string condition))))))

(defun openai-response-text (response)
  (or (jstring response "output_text" nil)
      (let ((out nil))
        (dolist (item (jarray-list (or (jget response "output") (arr))))
          (dolist (content (jarray-list (or (jget item "content") (arr))))
            (when (and (null out) (jstring content "text" nil))
              (setf out (jstring content "text")))))
        out)
      ""))

(defun dispatch-openai (state body)
  (let* ((id (or (jstring body "dispatchId" nil) (jstring body "id" nil)))
         (record (and id (lookup-dispatch-record state id))))
    (unless record
      (return-from dispatch-openai (obj "success" +json-false+ "error" "dispatch record not found")))
    (when (zerop (length (or (perception-state-openai-api-key state) "")))
      (return-from dispatch-openai (obj "success" +json-false+ "error" "OPENAI_API_KEY is not configured")))
    (update-dispatch-record state id (obj "status" "delivering" "adapter" "openai" "provider" "openai" "incrementAttempts" t))
    (handler-case
        (let* ((model (or (jstring body "model" nil) (perception-state-openai-model state)))
               (mapping-id (or (jstring body "sourceMappingId" nil)
                               (perception-state-openai-completion-source-mapping-id state)))
               (payload (obj "model" model
                             "instructions" "Return concise JSON. If committing a PE completion, include numeric values."
                             "input" (dispatch-record-prompt record)))
               (response (http-request-json
                          (format nil "~a/responses" (perception-state-openai-base-url state))
                          :method :post
                          :payload payload
                          :headers (list (cons "Authorization"
                                               (format nil "Bearer ~a" (perception-state-openai-api-key state))))))
               (content (openai-response-text response))
               (values (completion-values-from-content content))
               (completion +json-null+))
          (when values
            (setf completion (ingest-completion
                              state
                              (obj "provider" "openai"
                                   "agent" (jstring record "target" "openai")
                                   "sourceMappingId" mapping-id
                                   "correlationId" id
                                   "completionId" (or (jstring response "id" nil) +json-null+)
                                   "values" values))))
          (update-dispatch-record state id (obj "status" "delivered"
                                                "adapter" "openai"
                                                "provider" "openai"
                                                "externalRunId" (or (jstring response "id" nil)
                                                                    (make-id "openai-run"))))
          (obj "success" t "provider" "openai" "response" response "completion" completion))
      (error (condition)
        (update-dispatch-record state id (obj "status" "failed"
                                              "adapter" "openai"
                                              "provider" "openai"
                                              "lastError" (princ-to-string condition)))
        (obj "success" +json-false+ "provider" "openai" "error" (princ-to-string condition))))))

(defun push-perception (state include-machine-results)
  (let* ((engine (perception-state-engine state))
         (vector (assemble-perception-vector engine))
         (payload (obj "vector" (vectorize vector)
                       "includeMachineResults" (json-bool include-machine-results)
                       "includePerceptualSpace" t)))
    (handler-case
        (let ((response (http-post-json (format nil "~a/api/perceive" (perception-state-reality-url state))
                                        payload)))
          (record-dispatch-envelopes-from-step state response)
          (setf (perception-engine-last-push engine) response)
          (obj "success" t
               "vector" (vectorize vector)
               "response" response
               "timestamp" (now-ms)))
      (error (condition)
        (obj "success" +json-false+ "error" (princ-to-string condition) "timestamp" (now-ms))))))

(defun perception-routes (actor)
  (list
   (make-route "GET" "/" (lambda (_ body query)
                           (declare (ignore _ body query))
                           (json-response (obj "service" "Perception Engine (LSP)" "status" "running"))))
   (make-route "GET" "/api/health" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response (obj "status" "healthy" "timestamp" (now-ms)))))
   (make-route "GET" "/api/state" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response (actor-ask actor (lambda (state)
                                                                      (perception-state-json (perception-state-engine state)))))))
   (make-route "GET" "/api/integrations/status" (lambda (_ body query)
                                                  (declare (ignore _ body query))
                                                  (json-response (actor-ask actor #'integrations-status-json))))
   (make-route "POST" "/api/integrations/completions" (lambda (_ body query)
                                                        (declare (ignore _ query))
                                                        (json-response
                                                         (actor-ask actor
                                                                    (lambda (state)
                                                                      (ingest-completion state body))))))
   (make-route "GET" "/api/triggers/status" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (json-response (actor-ask actor #'triggers-status-json))))
   (make-route "GET" "/api/dispatch/ledger" (lambda (_ body query)
                                              (declare (ignore _ body query))
                                              (json-response (actor-ask actor #'ledger-json))))
   (make-route "GET" "/api/dispatch/records/:id" (lambda (params body query)
                                                   (declare (ignore body query))
                                                   (let ((record (actor-ask actor
                                                                            (lambda (state)
                                                                              (lookup-dispatch-record state (gethash "id" params))))))
                                                     (if record
                                                         (json-response record)
                                                         (error-response "Dispatch record not found" 404)))))
   (make-route "PATCH" "/api/dispatch/records/:id" (lambda (params body query)
                                                     (declare (ignore query))
                                                     (let ((record (actor-ask actor
                                                                              (lambda (state)
                                                                                (update-dispatch-record state (gethash "id" params) body)))))
                                                       (if record
                                                           (json-response record)
                                                           (error-response "Dispatch record not found" 404)))))
   (make-route "GET" "/api/integrations/ollama/status" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor #'ollama-status-json))))
   (make-route "POST" "/api/integrations/ollama/dispatch" (lambda (_ body query)
                                                            (declare (ignore _ query))
                                                            (json-response
                                                             (actor-ask actor
                                                                        (lambda (state)
                                                                          (dispatch-ollama state body))))))
   (make-route "GET" "/api/integrations/openai/status" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor #'openai-status-json))))
   (make-route "POST" "/api/integrations/openai/dispatch" (lambda (_ body query)
                                                            (declare (ignore _ query))
                                                            (json-response
                                                             (actor-ask actor
                                                                        (lambda (state)
                                                                          (dispatch-openai state body))))))
   (make-route "GET" "/api/integrations/healthkit/status" (lambda (_ body query)
                                                            (declare (ignore _ body query))
                                                            (json-response (actor-ask actor #'healthkit-status-json))))
   (make-route "POST" "/api/integrations/healthkit/ingest" (lambda (_ body query)
                                                             (declare (ignore _ query))
                                                             (json-response
                                                              (actor-ask actor
                                                                         (lambda (state)
                                                                           (ingest-healthkit state body))))))
   (make-route "GET" "/api/integrations/carekit/status" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (json-response (actor-ask actor #'carekit-status-json))))
   (make-route "POST" "/api/integrations/carekit/ingest" (lambda (_ body query)
                                                           (declare (ignore _ query))
                                                           (json-response
                                                            (actor-ask actor
                                                                       (lambda (state)
                                                                         (ingest-carekit state body))))))
   (make-route "GET" "/api/integrations/localai/status" (lambda (_ body query)
                                                         (declare (ignore _ body query))
                                                         (json-response (actor-ask actor #'localai-status-json))))
   (make-route "GET" "/api/integrations/localai/catalog" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (json-response (actor-ask actor #'localai-catalog-json))))
   (make-route "POST" "/api/integrations/localai/bootstrap" (lambda (_ body query)
                                                             (declare (ignore _ body query))
                                                             (json-response (actor-ask actor #'bootstrap-localai))))
   (make-route "POST" "/api/integrations/localai/invoke" (lambda (_ body query)
                                                          (declare (ignore _ query))
                                                          (json-response (actor-ask actor (lambda (state) (invoke-localai state body))))))
   (make-route "POST" "/api/signals" (lambda (_ body query)
                                      (declare (ignore _ query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (ingest-signal-body
                                                     state body
                                                     :default-sensor-id "localai_agent_activity"
                                                     :default-name "localai_agent_activity"
                                                     :default-ttl-ms 30000))))))
   (make-route "POST" "/api/push" (lambda (_ body query)
                                   (declare (ignore _ query))
                                   (json-response
                                    (actor-ask actor
                                               (lambda (state)
                                                 (push-perception state
                                                                  (jbool body "includeMachineResults"
                                                                         (not (jbool body "compact" nil)))))))))
   (make-route "GET" "/api/push/:id" (lambda (params body query)
                                      (declare (ignore body query))
                                      (json-response (obj "id" (gethash "id" params) "status" "unknown"))))
   (make-route "POST" "/api/auto/start" (lambda (_ body query)
                                         (declare (ignore _ query))
                                         (json-response
                                          (actor-ask actor
                                                     (lambda (state)
                                                       (setf (perception-engine-auto-running-p (perception-state-engine state)) t
                                                             (perception-engine-auto-interval-ms (perception-state-engine state))
                                                             (truncate (or (jnumber body "intervalMs" 1000) 1000)))
                                                       (obj "success" t "intervalMs" (perception-engine-auto-interval-ms (perception-state-engine state))))))))
   (make-route "POST" "/api/auto/stop" (lambda (_ body query)
                                        (declare (ignore _ body query))
                                        (json-response
                                         (actor-ask actor
                                                    (lambda (state)
                                                      (setf (perception-engine-auto-running-p (perception-state-engine state)) nil)
                                                      (obj "success" t))))))
   (make-route "PATCH" "/api/config" (lambda (_ body query)
                                      (declare (ignore _ query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (when (jstring body "matchAlgorithm" nil)
                                                      (setf (perception-engine-match-algorithm (perception-state-engine state))
                                                            (jstring body "matchAlgorithm")))
                                                    (obj "success" t
                                                         "matchAlgorithm" (perception-engine-match-algorithm (perception-state-engine state))))))))
   (make-route "POST" "/api/reset" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response
                                     (actor-ask actor
                                                (lambda (state)
                                                  (setf (perception-state-engine state)
                                                        (make-perception-engine-state
                                                         (perception-engine-dimension (perception-state-engine state))))
                                                  (obj "success" t))))))
   (make-route "GET" "/api/sources" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response
                                      (actor-ask actor
                                                 (lambda (state)
                                                   (obj "sources" (vectorize
                                                                   (mapcar #'source-json
                                                                           (object-values
                                                                            (perception-engine-sources
                                                                             (perception-state-engine state)))))))))))
   (make-route "POST" "/api/sources" (lambda (_ body query)
                                      (declare (ignore _ query))
                                      (json-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (let ((source (ensure-source-id (perception-state-engine state)
                                                                                    (source-from-json body))))
                                                      (obj "source" (source-json source))))))))
   (make-route "PATCH" "/api/sources/:id" (lambda (params body query)
                                           (declare (ignore query))
                                           (let ((result (actor-ask actor
                                                                    (lambda (state)
                                                                      (let* ((engine (perception-state-engine state))
                                                                             (source (gethash (gethash "id" params)
                                                                                              (perception-engine-sources engine))))
                                                                        (when source
                                                                          (when (jstring body "name" nil)
                                                                            (setf (source-name source) (jstring body "name")))
                                                                          (when (not (eq (jget body "active" :missing) :missing))
                                                                            (setf (source-active-p source) (jbool body "active" t)))
                                                                          (source-json source)))))))
                                             (if result (json-response (obj "source" result)) (error-response "Source not found" 404)))))
   (make-route "POST" "/api/sources/bootstrap-from-machines" (lambda (_ body query)
                                                              (declare (ignore _ body query))
                                                              (json-response
                                                               (actor-ask actor #'bootstrap-test-sources-from-machines))))
   (make-route "DELETE" "/api/sources/:id" (lambda (params body query)
                                            (declare (ignore body query))
                                            (json-response
                                             (actor-ask actor
                                                        (lambda (state)
                                                          (obj "success"
                                                               (json-bool
                                                                (remhash (gethash "id" params)
                                                                         (perception-engine-sources
                                                                          (perception-state-engine state))))))))))
   (make-route "POST" "/api/sensors/:sensorId" (lambda (params body query)
                                                (declare (ignore query))
                                                (let ((result (actor-ask actor
                                                                         (lambda (state)
                                                                           (let ((source (sensor-exists-p (perception-state-engine state)
                                                                                                          (gethash "sensorId" params))))
                                                                             (when source
                                                                               (setf (source-last-value source) (numbers-from-json (jget body "values"))
                                                                                     (source-last-updated source) (now-ms))
                                                                               (obj "success" t
                                                                                    "sensorId" (gethash "sensorId" params)
                                                                                    "timestamp" (now-ms))))))))
                                                  (if result
                                                      (json-response result)
                                                      (error-response "No sensor source with requested sensorId" 404)))))
   (make-route "GET" "/api/machines" (lambda (_ body query)
                                      (declare (ignore _ body query))
                                      (handler-case
                                          (json-response (http-get-json (format nil "~a/api/machines"
                                                                                (actor-ask actor #'perception-state-reality-url))))
                                        (error (condition) (error-response (princ-to-string condition) 502)))))
   ;; MQTT bridge — same surface as AI / CPP.  Returns enabled=false when
   ;; MQTT_BROKER_HOST was not set at PE startup; otherwise reports
   ;; connection state + bridge-level counters + the broker config.
   (make-route "GET" "/api/mqtt/status"
               (lambda (_ body query)
                 (declare (ignore _ body query))
                 (json-response
                  (actor-ask actor
                             (lambda (state)
                               (let ((b (perception-state-mqtt-bridge state)))
                                 (if (null b)
                                     (obj "enabled" (json-bool nil))
                                     (let ((stats (mqtt-bridge-stats-snapshot b))
                                           (cfg (reality-engine-lsp::%mqtt-client-config
                                                 (reality-engine-lsp::%mqtt-bridge-client b))))
                                       (let ((bridge-obj (obj)))
                                         (loop for (k . v) in stats
                                               do (setf (jget bridge-obj k) v))
                                         (obj "enabled" (json-bool t)
                                              "connected" (json-bool (mqtt-bridge-connected-p b))
                                              "brokerHost" (mqtt-client-config-broker-host cfg)
                                              "brokerPort" (mqtt-client-config-broker-port cfg)
                                              "clientId" (mqtt-client-config-client-id cfg)
                                              "bridge" bridge-obj
                                              "mappings" (length (mqtt-mapping-registry-rules
                                                                  (mqtt-bridge-registry b)))))))))))))
   ;; Mapping registry — read-only.  Returns the canonical {mappings:[...]}
   ;; shape with per-rule counters.  Enabled=false when bridge is off.
   (make-route "GET" "/api/mqtt/mappings"
               (lambda (_ body query)
                 (declare (ignore _ body query))
                 (json-response
                  (actor-ask actor
                             (lambda (state)
                               (let ((b (perception-state-mqtt-bridge state)))
                                 (if (null b)
                                     (obj "enabled" (json-bool nil) "mappings" (arr))
                                     (let ((body (mqtt-mapping-registry-to-json
                                                  (mqtt-bridge-registry b))))
                                       (setf (jget body "enabled") (json-bool t))
                                       body))))))))
   ;; PUT /api/mqtt/mappings — replace the registry and reload the bridge.
   ;; Validates via mqtt-mapping-registry-from-json (throws on schema
   ;; error → caught + returned as 400); runs overlap validation; rebuilds
   ;; the bridge keeping the existing client config.  Returns the new
   ;; mappings count + warnings.
   (make-route "PUT" "/api/mqtt/mappings"
               (lambda (_ body query)
                 (declare (ignore _ query))
                 (let ((result (actor-ask actor
                                          (lambda (state)
                                            (mqtt-reload-bridge state body)))))
	                   (cond
	                     ((stringp result) (error-response result 400))
	                     (t (json-response result))))))))

;; ── MQTT ingest path ────────────────────────────────────────────────────────
;;
;; Same contract as the AI ingestMqttSignal / CPP feed_mqtt_signal: every
;; accepted broker message resolves to a {sensorId, region, values, ttlMs}
;; tuple and flows through the existing sensor-source path — no special-case
;; downstream of decode.

(defun ingest-mqtt-signal (state sensor-id offset length values ttl-ms topic mapping-id)
  "Apply an MQTT-driven sensor update to the perception engine.  When a
sensor source with sensor-id already exists, update its lastValue +
lastUpdated.  Otherwise auto-create a sensor source covering the
mapping's declared region + TTL.  Mirrors ingestMqttSignal (AI) and
feed_mqtt_signal (CPP)."
  (declare (ignore mapping-id))
  (let* ((engine (perception-state-engine state))
         (existing (sensor-exists-p engine sensor-id)))
    (cond
      (existing
       (setf (source-last-value existing) values
             (source-last-updated existing) (now-ms)))
      (t
       (ensure-source-id engine
                         (make-source :id sensor-id
                                      :kind "sensor"
                                      :name (format nil "mqtt:~a" topic)
                                      :active-p t
                                      :region (make-region :offset offset :length length)
                                      :sensor-id sensor-id
                                      :last-value values
                                      :last-updated (now-ms)
                                      :ttl-ms (if (> ttl-ms 0) ttl-ms 30000)))))))

(defun mqtt-reload-bridge (state body)
  "PUT /api/mqtt/mappings handler — validate the new registry, stop the
old bridge, build a new one preserving the existing client config, and
start it.  Returns a JSON object on success or an error string when the
operator's input is invalid."
  (let ((current (perception-state-mqtt-bridge state)))
    (when (null current)
      (return-from mqtt-reload-bridge
        "no broker config — set MQTT_BROKER_HOST at PE startup before reloading mappings"))
    (let ((new-registry nil))
      (handler-case
          (setf new-registry (mqtt-mapping-registry-from-json body))
        (error (c)
          (return-from mqtt-reload-bridge (format nil "schema: ~a" c))))
      (when (zerop (length (mqtt-mapping-registry-rules new-registry)))
        (return-from mqtt-reload-bridge
          "mappings array is empty — at least one rule is required"))
      (let* ((allow (member (env "MQTT_ALLOW_REGION_OVERLAP" "0")
                            '("1" "true" "TRUE" "yes") :test #'string=))
             (warnings (mqtt-validate-overlaps new-registry allow))
             (config (reality-engine-lsp::%mqtt-client-config
                      (reality-engine-lsp::%mqtt-bridge-client current))))
        (mqtt-bridge-stop current)
        (let ((bridge (make-mqtt-bridge config new-registry
                                        (reality-engine-lsp::%mqtt-bridge-ingest-callback current)
                                        (reality-engine-lsp::%mqtt-bridge-push-trigger current))))
          (mqtt-bridge-start bridge)
          (setf (perception-state-mqtt-bridge state) bridge)
          (obj "success" (json-bool t)
               "enabled" (json-bool t)
               "mappings" (length (mqtt-mapping-registry-rules new-registry))
               "warnings" (vectorize warnings)))))))

(defun maybe-boot-mqtt-bridge (state actor)
  "Build + start the MQTT bridge from the environment, when configured.
Stamps the bridge on STATE so PE routes can read it.  Failure during
mapping-file parse leaves the bridge disabled but doesn't fail the PE
startup — the PE still serves HTTP signals as a pure REST engine."
  (let ((env-pair (mqtt-bridge-from-environment)))
    (when env-pair
      (handler-case
          (let* ((cfg (car env-pair))
                 (reg (cdr env-pair))
                 (ingest (lambda (sensor-id offset length values ttl-ms topic mapping-id)
                           ;; Fire-and-forget into the actor — guarantees the
                           ;; engine mutation runs serialized with every other
                           ;; perception-state operation.
                           (actor-tell
                            actor
                            (lambda (st)
                              (ingest-mqtt-signal st sensor-id offset length values ttl-ms topic mapping-id)))))
                 (trigger (lambda ()
                            (actor-tell actor
                                        (lambda (st)
                                          (handler-case (push-perception st nil)
                                            (error (c)
                                              (format *error-output*
                                                      "[mqtt-bridge] push trigger error: ~a~%" c)))))))
                 (bridge (make-mqtt-bridge cfg reg ingest trigger)))
            (mqtt-bridge-start bridge)
            (setf (perception-state-mqtt-bridge state) bridge)
            (format *standard-output* "[MQTT] bridge enabled — broker=~a:~a mappings=~a~%"
                    (mqtt-client-config-broker-host cfg)
                    (mqtt-client-config-broker-port cfg)
                    (length (mqtt-mapping-registry-rules reg))))
        (error (c)
          (format *error-output* "[MQTT] bridge failed to start: ~a~%" c))))))

(defun start-perception-service (&key (port 3300)
                                      (reality-url "http://localhost:3299")
                                      (localai-url "http://localhost:8000")
                                      (localai-machine-dir "../localAIStack/data/machines")
                                      (dimension 768))
  (let* ((state (make-perception-state-from-config :dimension dimension
                                                   :reality-url reality-url
                                                   :localai-url localai-url
                                                   :localai-machine-dir localai-machine-dir))
         (actor (state-actor "perception-service" state)))
    (load-integrations-config state)
    ;; Boot the bridge after the actor is alive so the ingest closure can
    ;; tell-into it safely.  The bridge boot is fire-and-forget — if MQTT
    ;; isn't configured the PE still serves HTTP signals normally.
    (actor-tell actor (lambda (st) (maybe-boot-mqtt-bridge st actor)))
    (start-http-server port (perception-routes actor) :name "perception-engine-lsp")))

(defun start-perception-from-environment ()
  (start-perception-service :port (env-int "PERCEPTION_ENGINE_PORT" 3300)
                            :reality-url (format nil "http://localhost:~a" (env-int "REALITY_ENGINE_PORT" 3299))
                            :localai-url (env "LOCAL_AI_API_URL" "http://localhost:8000")
                            :localai-machine-dir (env "LOCAL_AI_MACHINES_DIR" "../localAIStack/data/machines")
                            :dimension (env-int "VECTOR_DIMENSION" 768)))
