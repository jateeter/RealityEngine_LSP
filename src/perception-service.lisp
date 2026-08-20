(in-package #:reality-engine-lsp)

;; ── Semantic guardrail metrics (docs/PE_METRICS_CONTRACT.md) ───────────────
;; The semantic_* block must stay byte-identical across PE runtimes after
;; normalizing the runtime label, so the writer below is deliberately literal:
;; fixed HELP strings, labels sorted by key, integer values, label values
;; sorted ascending. Counters are monotonic for the process lifetime and
;; bumped where records are created, so ring-buffer eviction loses no count.

(defparameter +metrics-runtime+ "lsp")
(defvar *semantic-events* (make-hash-table :test #'equal))
(defvar *semantic-events-joined* (make-hash-table :test #'equal))
(defvar *semantic-escalations* (make-hash-table :test #'equal))
(defvar *semantic-dispatch-total* 0)
(defvar *semantic-dispatch-joined* 0)
(defvar *semantic-audit-records* 0)
(defvar *semantics-bases-cache* nil)   ; (mtime . hash-table)

(defun semantics-manifest-file ()
  "Resolve the corpus semantics manifest the same way the RE does."
  (let ((explicit (env "SEMANTICS_MANIFEST" nil)))
    (when (and explicit (> (length explicit) 0) (probe-file explicit))
      (return-from semantics-manifest-file explicit)))
  (let ((cursor (uiop:ensure-directory-pathname
                 (env "MACHINES_DIR" "../RealityEngine_Machines/machines"))))
    (loop repeat 6
          while cursor
          for candidate = (merge-pathnames "semantics/abox-manifest.json" cursor)
          when (probe-file candidate)
            do (return (namestring candidate))
          do (let ((parent (uiop:pathname-parent-directory-pathname cursor)))
               (setf cursor (and parent (not (equal parent cursor)) parent)))
          finally (return nil))))

(defun semantics-manifest-bases ()
  "Machine name -> ABox base IRI, cached on the manifest's write date."
  (let ((path (semantics-manifest-file)))
    (if (null path)
        (make-hash-table :test #'equal)
        (let ((stamp (ignore-errors (file-write-date path))))
          (if (and *semantics-bases-cache* (eql (car *semantics-bases-cache*) stamp))
              (cdr *semantics-bases-cache*)
              (let ((bases (make-hash-table :test #'equal)))
                (ignore-errors
                 (let ((manifest (parse-json (safe-read-file path))))
                   (maphash (lambda (key entry)
                              (declare (ignore key))
                              (let ((name (jstring entry "name" nil))
                                    (iri (jstring entry "iri" nil)))
                                (when (and name iri)
                                  (let ((hash (position #\# iri)))
                                    (when hash
                                      (setf (gethash name bases) (subseq iri 0 hash)))))))
                            (jget manifest "machines"))))
                (setf *semantics-bases-cache* (cons stamp bases))
                bases))))))

(defun record-perception-event (integration joined)
  (incf (gethash integration *semantic-events* 0))
  (incf (gethash integration *semantic-events-joined* 0) (if joined 1 0))
  (setf *semantic-audit-records* (min 1000 (1+ *semantic-audit-records*))))

(defun metric-line (name help kind labels value)
  "One HELP/TYPE/sample triple. LABELS is an alist; runtime is appended and
the whole set sorted by key, per the contract."
  (let ((all (sort (append labels (list (cons "runtime" +metrics-runtime+)))
                   #'string< :key #'car)))
    (format nil "# HELP ~a ~a~%# TYPE ~a ~a~%~a{~{~a~^,~}} ~d~%"
            name help name kind name
            (mapcar (lambda (kv) (format nil "~a=\"~a\"" (car kv) (cdr kv))) all)
            value)))

(defun sorted-keys (table)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) table)
       (sort keys #'string<)))

(defun semantic-metrics-text (sources global-step vector-size last-push-ms)
  (let ((bases (semantics-manifest-bases))
        (out (make-string-output-stream)))
    (write-string (metric-line "perception_engine_sources_total"
                               "Total sensor/test/simulated sources registered." "gauge" nil sources) out)
    (write-string (metric-line "perception_engine_global_step"
                               "Engine globalStep counter (push count since start)." "gauge" nil global-step) out)
    (write-string (metric-line "perception_engine_vector_size"
                               "Configured vector dimension." "gauge" nil vector-size) out)
    (write-string (metric-line "perception_engine_last_push_ms"
                               "Wall-clock timestamp of the last successful push (0 if never)." "gauge" nil last-push-ms) out)
    (write-string (metric-line "semantic_manifest_available"
                               "Corpus OWL semantics manifest resolved (1/0)." "gauge" nil
                               (if (> (hash-table-count bases) 0) 1 0)) out)
    (write-string (metric-line "semantic_manifest_machines"
                               "Machines carrying a semantic identity in the manifest." "gauge" nil
                               (hash-table-count bases)) out)
    (write-string (metric-line "semantic_audit_buffer_records"
                               "re:PerceptionEvent records held in the audit ring buffer." "gauge" nil
                               *semantic-audit-records*) out)
    (dolist (k (sorted-keys *semantic-events*))
      (write-string (metric-line "semantic_perception_events_total"
                                 "re:PerceptionEvent records emitted, by originating integration." "counter"
                                 (list (cons "integration" k)) (gethash k *semantic-events* 0)) out))
    (dolist (k (sorted-keys *semantic-events-joined*))
      (write-string (metric-line "semantic_perception_events_iri_joined_total"
                                 "Perception events whose machine resolved to a corpus ABox IRI." "counter"
                                 (list (cons "integration" k)) (gethash k *semantic-events-joined* 0)) out))
    (write-string (metric-line "semantic_dispatch_records_total"
                               "Dispatch records created with a semantics link." "counter" nil
                               *semantic-dispatch-total*) out)
    (write-string (metric-line "semantic_dispatch_records_iri_joined_total"
                               "Dispatch records whose machine resolved to a corpus ABox IRI." "counter" nil
                               *semantic-dispatch-joined*) out)
    (dolist (k (sorted-keys *semantic-escalations*))
      (write-string (metric-line "semantic_escalation_dispatches_total"
                                 "Escalation-class actions dispatched, by RAG status of the determination." "counter"
                                 (list (cons "rag" k)) (gethash k *semantic-escalations* 0)) out))
    (get-output-stream-string out)))

(defstruct perception-state
  engine reality-url localai-url localai-machine-dir push-records started-at
  integrations-config-path integrations-loaded-p integrations-load-error integrations source-mappings
  triggers-enabled-p trigger-dispatch-mode trigger-graphql-url envelopes-created dispatch-errors
  dropped-no-governance dropped-no-dispatch
  machine-catalog machine-catalog-lock machine-catalog-refreshed-at
  dispatch-ledger dispatch-ledger-limit
  ollama-base-url ollama-model ollama-completion-source-mapping-id
  openai-base-url openai-model openai-completion-source-mapping-id openai-api-key
  acp-enabled-p acp-platform acp-surface acp-command acp-gateway-url
  acp-session-key acp-target-agent acp-completion-source-mapping-id
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
   :machine-catalog (make-hash-table :test #'equal)
   :machine-catalog-lock (bt:make-lock "machine-catalog")
   :machine-catalog-refreshed-at 0
   :dispatch-ledger nil
   :dispatch-ledger-limit (env-int "TRIGGER_DISPATCH_LEDGER_LIMIT" 100)
   :ollama-base-url (trim-trailing-slashes (env "OLLAMA_BASE_URL" "http://localhost:11434"))
   ;; Canonical default shared by every runtime; override per engine with
   ;; OLLAMA_MODEL. See RealityEngine_CI/docs/INTEGRATION_ARCHITECTURE.md.
   :ollama-model (env "OLLAMA_MODEL" "llama3.1:8b")
   :ollama-completion-source-mapping-id (env "OLLAMA_COMPLETION_SOURCE_MAPPING_ID" "agent-completion-risk")
   :openai-base-url (trim-trailing-slashes (env "OPENAI_BASE_URL" "https://api.openai.com/v1"))
   :openai-model (env "OPENAI_MODEL" "gpt-5")
   :openai-completion-source-mapping-id (env "OPENAI_COMPLETION_SOURCE_MAPPING_ID" "agent-completion-risk")
   :openai-api-key (or (env "OPENAI_API_KEY" nil) "")
   :acp-enabled-p (env-bool "ACP_ENABLED" t)
   :acp-platform (env "ACP_PLATFORM" "OpenClaw")
   :acp-surface (env "ACP_SURFACE" "xACP")
   :acp-command (or (env "OPENCLAW_ACP_COMMAND" nil)
                    (env "ACP_COMMAND" "openclaw acp"))
   :acp-gateway-url (or (env "OPENCLAW_GATEWAY_URL" nil)
                        (env "ACP_GATEWAY_URL" "ws://127.0.0.1:18789"))
   :acp-session-key (or (env "OPENCLAW_ACP_SESSION" nil)
                        (env "ACP_SESSION_KEY" "agent:main:main"))
   :acp-target-agent (env "ACP_TARGET_AGENT" "openclaw")
   :acp-completion-source-mapping-id (env "ACP_COMPLETION_SOURCE_MAPPING_ID" "acp-openclaw-completion")
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
               "extract" (obj "type" "json"
                              "pointers" (arr "/completed" "/failed" "/confidence" "/actionClass"))
               "normalize" (obj "mode" "passthrough" "clamp" t)
               "ttlMs" 300000
               "pushMode" "debounced"
               "debounceMs" 250)
          ;; healthkit mappings are keyed by "healthkit:<typeIdentifier>" — no built-in default.
          ;; Operators declare per-type mappings in the integration config.
          (gethash "acp-openclaw-completion" mappings)
          (obj "id" "acp-openclaw-completion"
               "sensorIdTemplate" "acp.openclaw.{agent}.completion"
               "region" (obj "offset" 4210 "length" 4)
               "extract" (obj "type" "json"
                              "pointers" (arr "/completed" "/failed" "/confidence" "/actionClass"))
               "normalize" (obj "mode" "passthrough" "clamp" t)
               "ttlMs" 300000
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
                                        :ttl-ms ttl-ms
                                        :origin "localai"))))
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
                ;; A corpus machine is (re)loaded whether or not a source
                ;; already claims to describe it. `ensure-source-id` keys on
                ;; test-<machineId>, so a reload replaces in place rather than
                ;; duplicating. Nothing in an existing source says whether its
                ;; machine still has the same CESs, interconnections or regions
                ;; — a redefined machine may replace the old one entirely — so
                ;; skipping the rebuild keeps a source describing a machine
                ;; that no longer exists. PE_SOURCE_MERGE=true restores the skip.
                ((or (null mid)
                     (and (env-bool "PE_SOURCE_MERGE" nil) (gethash mid existing))
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
                                    ;; Inactive by default. Activating every
                                    ;; machine source made all three runtimes
                                    ;; replay their input sequences on every
                                    ;; push and the divergence went three-way
                                    ;; rather than away (RealityEngine_Scala#43).
                                    ;; PE_SOURCE_ACTIVATE_ON_LOAD=true turns it
                                    ;; on for a deliberate experiment.
                                    :active-p (env-bool "PE_SOURCE_ACTIVATE_ON_LOAD" nil)
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
         "errors" (vectorize (mapcar #'identity errors))
         "machinesSeen" machines-seen
         "skipped" skipped
         "success" (json-bool t))))

(defun canonical-bootstrap-summary-json (summary)
  (format nil "{\"created\":~d,\"errors\":~a,\"machinesSeen\":~d,\"skipped\":~d,\"success\":~a}"
          (truncate (or (jnumber summary "created" 0) 0))
          (json-stringify (or (jget summary "errors" nil) (arr)))
          (truncate (or (jnumber summary "machinesSeen" 0) 0))
          (truncate (or (jnumber summary "skipped" 0) 0))
          (if (jbool summary "success" nil) "true" "false")))

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
                     ;; The registry supplies defaults; an explicit environment
                     ;; variable outranks them. This applied the file's values
                     ;; unconditionally, so OLLAMA_BASE_URL and OLLAMA_MODEL
                     ;; were silently discarded — including from an
                     ;; integration entry marked "enabled": false. C++ and
                     ;; Scala both let the environment win, so a lane pinning
                     ;; one model across the three runtimes got two of them
                     ;; (#44).
                     (when (and (jstring item "baseUrl" nil)
                                (not (env-set-p "OLLAMA_BASE_URL")))
                       (setf (perception-state-ollama-base-url state)
                             (trim-trailing-slashes (jstring item "baseUrl"))))
                     (when (and (jstring item "model" nil)
                                (not (env-set-p "OLLAMA_MODEL")))
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
                    ((or (string= kind "acp") (string= kind "openclaw-acp"))
                     (setf (perception-state-acp-enabled-p state)
                           (jbool item "enabled" (perception-state-acp-enabled-p state)))
                     (when (jstring item "platform" nil)
                       (setf (perception-state-acp-platform state) (jstring item "platform")))
                     (when (jstring item "surface" nil)
                       (setf (perception-state-acp-surface state) (jstring item "surface")))
                     (when (jstring item "command" nil)
                       (setf (perception-state-acp-command state) (jstring item "command")))
                     (when (jstring item "gatewayUrl" nil)
                       (setf (perception-state-acp-gateway-url state) (jstring item "gatewayUrl")))
                     (when (jstring item "sessionKey" nil)
                       (setf (perception-state-acp-session-key state) (jstring item "sessionKey")))
                     (when (jstring item "targetAgent" nil)
                       (setf (perception-state-acp-target-agent state) (jstring item "targetAgent")))
                     (when (jstring item "completionSourceMappingId" nil)
                       (setf (perception-state-acp-completion-source-mapping-id state)
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
  (when id (gethash id (perception-state-source-mappings state))))

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

(defun commit-signal-source (state sensor-id name region values ttl-ms &optional origin)
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
                                 :ttl-ms ttl-ms
                                 :origin origin))))
    (when origin (setf (source-origin source) origin))
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
         (source (commit-signal-source state sensor-id name region values ttl-ms
                                       (or (jstring body "origin" nil) "signal")))
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
       "acp" (acp-status-json state)
       "healthkit" (healthkit-status-json state)
       "carekit" (carekit-status-json state)))

(defun triggers-status-json (state)
  (let ((catalog-size (bt:with-lock-held ((perception-state-machine-catalog-lock state))
                        (hash-table-count (perception-state-machine-catalog state)))))
    (obj "enabled" (json-bool (perception-state-triggers-enabled-p state))
         "mode" (perception-state-trigger-dispatch-mode state)
         "graphqlUrl" (perception-state-trigger-graphql-url state)
         "envelopesCreated" (perception-state-envelopes-created state)
         "dispatchErrors" (perception-state-dispatch-errors state)
         "droppedNoGovernance" (perception-state-dropped-no-governance state)
         "droppedNoDispatch" (perception-state-dropped-no-dispatch state)
         "machineCatalogSize" catalog-size
         "machineCatalogRefreshedAt" (perception-state-machine-catalog-refreshed-at state)
         "ledgerSize" (length (perception-state-dispatch-ledger state)))))

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

(defun replay-dispatch-record (state dispatch-id)
  "Create a new ledger entry that replays an existing dispatch record.
Wire-compatible with _AI Dispatcher.replay() — same mode:\"replay\" + replayOf fields."
  (let ((original (lookup-dispatch-record state dispatch-id)))
    (when original
      (let* ((now (now-ms))
             (record (obj "id" (make-id "dispatch")
                          "envelopeId" (or (jstring original "envelopeId" nil) (make-id "trigger-envelope"))
                          "correlationId" (or (jstring original "correlationId" nil) (make-id "trigger-correlation"))
                          "status" "recorded"
                          "mode" "replay"
                          "replayOf" dispatch-id
                          "target" (or (jstring original "target" nil) +json-null+)
                          "machineId" (or (jstring original "machineId" nil) "")
                          "sequenceId" (or (jstring original "sequenceId" nil) "")
                          "ragStatusCode" (or (jstring original "ragStatusCode" nil) "")
                          "processStatus" (or (jstring original "processStatus" nil) "")
                          "adapter" +json-null+ "provider" +json-null+
                          "externalRunId" +json-null+ "lastError" +json-null+
                          "attempts" 0 "createdAt" now "updatedAt" now
                          "envelope" (or (jget original "envelope") +json-null+))))
        (push record (perception-state-dispatch-ledger state))
        record))))

;; ── Dispatch helpers — wire-compatible with CPP dispatch_triggers /
;;    TypeScript Dispatcher.onStep.  Drop rules and full envelope shape
;;    match both reference implementations exactly. ───────────────────────────

;; ── Machine catalog cache ─────────────────────────────────────────────────────
;; Mirrors CPP machine_catalog_snapshot / TS machineCatalog + refreshMachineCatalog.
;; The catalog is populated by a background thread; dispatch lookups are O(1)
;; and never block the actor (push cycle) on a RE HTTP round-trip.

(defun refresh-machine-catalog (state)
  "Fetch /api/machines from RE and atomically replace the local catalog.
Intended to be called from the background refresher thread only — never
from the actor thread.  Soft-fails: existing catalog is preserved on error."
  (handler-case
      (let* ((response (http-get-json (format nil "~a/api/machines?summary=true"
                                              (perception-state-reality-url state))))
             (machines (jarray-list (or (jget response "machines") (arr))))
             (new-catalog (make-hash-table :test #'equal)))
        (dolist (machine machines)
          (let ((id (jstring machine "id" nil)))
            (when (and id (not (string= id "")))
              (setf (gethash id new-catalog) machine))))
        (bt:with-lock-held ((perception-state-machine-catalog-lock state))
          (setf (perception-state-machine-catalog state) new-catalog
                (perception-state-machine-catalog-refreshed-at state) (now-ms)))
        (length machines))
    (error (condition)
      (format *error-output* "~&[dispatch] machine catalog refresh failed: ~a~%" condition)
      nil)))

(defun get-cached-machine (state machine-id)
  "O(1) thread-safe lookup of a machine from the local catalog.
Returns NIL when the machine is absent or the catalog is still warming up."
  (bt:with-lock-held ((perception-state-machine-catalog-lock state))
    (gethash machine-id (perception-state-machine-catalog state))))

(defun start-machine-catalog-refresher (state)
  "Spawn a background thread that refreshes the machine catalog every 60 s.
Fires an immediate best-effort fetch on startup so the catalog is warm before
the first push cycle completes — mirrors TS: void refreshMachineCatalog();
setInterval(refreshMachineCatalog, 60_000)."
  (bt:make-thread
   (lambda ()
     (loop
       (refresh-machine-catalog state)
       (sleep 60)))
   :name "machine-catalog-refresher"))

(defun fetch-machine-for-dispatch (state machine-id)
  "Return the cached machine record for machine-id, or NIL when absent.
Never performs a blocking RE call — the catalog is maintained by the
background refresher thread."
  (get-cached-machine state machine-id))

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

(defun ces-copy-string-array (value)
  "Return a JSON array containing only the string elements of value."
  (if (jarray-p value)
      (vectorize (remove-if-not #'stringp (jarray-list value)))
      (arr)))

(defun ces-select-agent-action (actions values)
  "Return the action aligned with the first non-zero output value, or first action."
  (let ((action-list (and (jarray-p actions) (jarray-list actions))))
    (if action-list
        (progn
          (when (jarray-p values)
            (let ((index 0))
              (dolist (value (jarray-list values))
                (when (and (numberp value) (/= value 0) (< index (length action-list)))
                  (let ((action (nth index action-list)))
                    (return-from ces-select-agent-action
                      (if (stringp action) action ""))))
                (incf index))))
          (let ((first-action (first action-list)))
            (if (stringp first-action) first-action "")))
        "")))

(defun ces-dispatch-binding (metadata &optional values)
  "Normalize first-class metadata.agentBinding with legacy dispatch fallback."
  (let* ((binding (jget metadata "agentBinding"))
         (binding-p (jobject-p binding))
         (actions (if binding-p
                      (ces-copy-string-array (jget binding "allowedActions"))
                      (ces-copy-string-array (jget metadata "agentActions")))))
    (when (and binding-p (zerop (length (jarray-list actions))))
      (setf actions (ces-copy-string-array (jget metadata "agentActions"))))
    (obj "agent" (if binding-p
                     (or (jstring binding "agent" nil)
                         (jstring metadata "dispatchableAgent" ""))
                     (jstring metadata "dispatchableAgent" ""))
         "trigger" (if binding-p
                       (or (jstring binding "trigger" nil)
                           (jstring metadata "aiTrigger" ""))
                       (jstring metadata "aiTrigger" ""))
         "action" (ces-select-agent-action actions values)
         "agentActionsCatalog" actions
         "autonomyMode" (if binding-p (jstring binding "mode" "") "")
         "writeBack" (if binding-p
                         (or (jget binding "writeBack") +json-null+)
                         +json-null+))))

(defun build-ces-envelope (op machine envelope-id correlation-id state)
  "Build a full ces.terminal.event envelope.
Wire-compatible with CPP build_trigger_envelope and TS buildTriggerEnvelope."
  (let* ((md (or (jget machine "metadata") (obj)))
         (values (or (jget op "values") (arr)))
         (trigger-config (or (jget md "triggerConfig") (obj)))
         (trigger-dispatch (or (jget trigger-config "dispatch") (obj)))
         (binding (ces-dispatch-binding md values))
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
                    "provenance" (if (jarray-present-p op "provenance")
                                     (jget op "provenance") (arr))
                    "deprecation" (or (jget op "deprecation") +json-null+))
         "outputVector" (obj "values" values
                             "encoding" "vector"
                             "semantics" (ces-semantics-from-values values)
                             "assertedLabel" (ces-asserted-label values))
         "projection" +json-null+
         "governance" (or (jget op "governance") +json-null+)
         "dispatch" (obj "processId" (jstring trigger-config "processId" "")
                         "processName" (jstring trigger-config "processName" "")
                         "agent" (jstring binding "agent" "")
                         "action" (jstring binding "action" "")
                         "agentActionsCatalog" (or (jget binding "agentActionsCatalog") (arr))
                         "trigger" (jstring binding "trigger" "")
                         "autonomyMode" (jstring binding "autonomyMode" "")
                         "writeBack" (or (jget binding "writeBack") +json-null+)
                         "endpoint" (obj "kind" mode
                                         "url" (if graphql-p
                                                   (or (jstring trigger-config "endpoint" nil)
                                                       (perception-state-trigger-graphql-url state))
                                                   "")
                                         "mutation" (if graphql-p
                                                        (or (jstring trigger-dispatch "mutation" nil)
                                                            "updateProcessState")
                                                        "")
                                         "schemaRef" (if graphql-p
                                                         (or (jstring trigger-dispatch "schemaRef" nil)
                                                             "localAIStack/services/api/routers/graphql_endpoint.py")
                                                         ""))))))

(defun record-dispatch-envelope (state operation &optional machine-json-override)
  "Build and ledger one dispatch record for a merge operation.
Returns the record on success, or NIL when the machine lacks a dispatch
agent / trigger — the drop-no-dispatch signal to the caller.
Wire-compatible with CPP DispatchRecord and TS Dispatcher.recordFromEnvelope.
Optional MACHINE-JSON-OVERRIDE bypasses the catalog lookup (used in tests and
corpus-walk helpers that already hold the loaded machine object)."
  (let* ((machine-id (jstring operation "machineId" ""))
         (machine (or machine-json-override (fetch-machine-for-dispatch state machine-id)))
         (md (and machine (jget machine "metadata")))
         (binding (and md (ces-dispatch-binding md (jget operation "values"))))
         (agent (and binding (jstring binding "agent" nil)))
         (trigger (and binding (jstring binding "trigger" nil))))
    ;; Drop — no dispatch agent or no trigger.
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
(droppedNoGovernance) or without dispatch binding (droppedNoDispatch)."
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

(defun completion-scalar-number (value)
  (cond
    ((numberp value) (coerce value 'double-float))
    ((eq value t) 1.0d0)
    ((eq value +json-false+) 0.0d0)
    ((stringp value)
     (handler-case
         (let ((*read-default-float-format* 'double-float))
           (multiple-value-bind (parsed pos) (read-from-string value)
             (if (and (= pos (length value)) (realp parsed))
                 (coerce parsed 'double-float)
                 :invalid)))
       (error () :invalid)))
    (t :invalid)))

(defun validate-completion-values-array (values)
  (unless (jarray-p values)
    (error "provider completion values must be an array"))
  (vectorize
   (mapcar (lambda (value)
             (let ((number (completion-scalar-number value)))
               (when (eq number :invalid)
                 (error "provider completion value is not a finite number"))
               number))
           (jarray-list values))))

(defun response-values-from-content-json (content-json)
  (cond
    ((jarray-present-p content-json "values")
     (validate-completion-values-array (jget content-json "values")))
    ((and (jobject-p (jget content-json "completion"))
          (jarray-present-p (jget content-json "completion") "values"))
     (validate-completion-values-array (jget (jget content-json "completion") "values")))
    (t nil)))

(defun completion-values-from-pointer (content-json pointer)
  (let ((node (mqtt-navigate-pointer content-json pointer)))
    (when (eq node :missing)
      (error "missing required JSON pointer: ~a" pointer))
    (cond
      ((jarray-p node) (validate-completion-values-array node))
      (t
       (let ((number (completion-scalar-number node)))
         (when (eq number :invalid)
           (error "JSON pointer resolved to a non-finite value: ~a" pointer))
         (vectorize (list number)))))))

(defun extract-completion-values-for-mapping (content-json mapping)
  (let ((extract (and mapping (jget mapping "extract"))))
    (if (and (jobject-p extract)
             (string= (jstring extract "type" "") "json"))
        (cond
          ((jarray-present-p extract "pointers")
           (vectorize
            (mapcar (lambda (pointer)
                      (let ((node (mqtt-navigate-pointer content-json pointer)))
                        (when (eq node :missing)
                          (error "missing required JSON pointer: ~a" pointer))
                        (let ((number (completion-scalar-number node)))
                          (when (eq number :invalid)
                            (error "JSON pointer resolved to a non-finite value: ~a" pointer))
                          number)))
                    (jarray-list (jget extract "pointers")))))
          ((jstring extract "pointer" nil)
           (completion-values-from-pointer content-json (jstring extract "pointer")))
          (t
           (or (response-values-from-content-json content-json)
               (error "provider response did not include completion values"))))
        (or (response-values-from-content-json content-json)
            (error "provider response did not include completion values")))))

(defun normalize-completion-values (values mapping)
  (let ((normalize (and mapping (jget mapping "normalize"))))
    (if (not (jobject-p normalize))
        values
        (let ((mode (jstring normalize "mode" "passthrough"))
              (clamp (jbool normalize "clamp" nil)))
          (vectorize
           (mapcar (lambda (value)
                     (let ((n value))
                       (cond
                         ((string= mode "minmax")
                          (let* ((min (or (jnumber normalize "min" nil) 0.0d0))
                                 (max (or (jnumber normalize "max" nil) 1.0d0))
                                 (span (- max min)))
                            (setf n (if (zerop span) 0.0d0 (/ (- n min) span)))))
                         ((string= mode "linear")
                          (setf n (+ (* n (or (jnumber normalize "scale" nil) 1.0d0))
                                      (or (jnumber normalize "offset" nil) 0.0d0)))))
                       (if clamp (clamp01 n) n)))
                   (jarray-list values)))))))

(defun completion-values-from-content (content &optional mapping)
  (let ((content-json (handler-case (parse-json content)
                        (error () (error "provider response content is not valid JSON")))))
    (normalize-completion-values
     (extract-completion-values-for-mapping content-json mapping)
     mapping)))

(defun decode-json-pointer-token (token)
  (let ((out (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (i 0)
        (n (length token)))
    (loop while (< i n) do
      (if (and (char= (aref token i) #\~)
               (< (1+ i) n)
               (member (aref token (1+ i)) '(#\0 #\1)))
          (progn
            (vector-push-extend (if (char= (aref token (1+ i)) #\1) #\/ #\~) out)
            (incf i 2))
          (progn
            (vector-push-extend (aref token i) out)
            (incf i))))
    out))

(defun json-pointer-top-level-key (pointer)
  (if (or (zerop (length pointer)) (char/= (aref pointer 0) #\/))
      "value"
      (let* ((end (or (position #\/ pointer :start 1) (length pointer)))
             (raw (subseq pointer 1 end)))
        (decode-json-pointer-token raw))))

(defun completion-schema-for-mapping (mapping)
  (let* ((extract (and mapping (jget mapping "extract")))
         (pointers (cond
                     ((and (jobject-p extract) (jarray-present-p extract "pointers"))
                      (jarray-list (jget extract "pointers")))
                     ((and (jobject-p extract) (jstring extract "pointer" nil))
                      (list (jstring extract "pointer")))
                     (t nil))))
    (if pointers
        (let ((properties (obj))
              (required nil))
          (dolist (pointer pointers)
            (let ((key (json-pointer-top-level-key pointer)))
              (setf (jget properties key)
                    (obj "type" (arr "number" "boolean")))
              (push key required)))
          (obj "type" "object"
               "additionalProperties" +json-false+
               "properties" properties
               "required" (vectorize (nreverse required))))
        (obj "type" "object"
             "additionalProperties" +json-false+
             "properties" (obj "values" (obj "type" "array"
                                             "items" (obj "type" "number")))
             "required" (arr "values")))))

(defun openai-text-format-for-mapping (mapping)
  (obj "format" (obj "type" "json_schema"
                     "name" "reality_engine_completion"
                     "strict" t
                     "schema" (completion-schema-for-mapping mapping))))

(defun ingest-completion (state body)
  ;; Returns (cons http-status body-hash) — route handler unpacks it.
  (let* ((provider (or (jstring body "provider" nil) "agent"))
         (agent (or (jstring body "agent" nil)
                    (jstring body "agentId" nil)
                    provider))
         ;; sourceMappingId / mappingId alias — empty string means no registry lookup.
         (mapping-id-raw (or (jstring body "sourceMappingId" nil)
                             (jstring body "mappingId" nil)))
         (mapping (when mapping-id-raw (source-mapping-by-id state mapping-id-raw))))
    ;; 404 when an explicit ID was given but is not in the registry — matches CPP / AI.
    (when (and mapping-id-raw (not mapping))
      (return-from ingest-completion
        (cons 404 (obj "error" (format nil "Unknown sourceMappingId \"~a\"" mapping-id-raw)))))
    ;; Inline sourceMapping merged on top of registry entry — matches CPP / AI.
    (when (jobject-p (jget body "sourceMapping"))
      (unless mapping (setf mapping (make-hash-table :test #'equal)))
      (maphash (lambda (k v) (setf (gethash k mapping) v))
               (jget body "sourceMapping")))
    (let* ((mapping-id (or mapping-id-raw ""))
           (values (or (jget body "values")
                       (jget body "vector")
                       (and (jobject-p (jget body "completion"))
                            (jget (jget body "completion") "values"))))
           (numbers (numbers-from-json values))
           (template (or (and mapping (jstring mapping "sensorIdTemplate" nil))
                         "agent.{agent}.completion"))
           (sensor-id (or (jstring body "sensorId" nil)
                          (render-sensor-template
                           template
                           (list (cons "provider" provider)
                                 (cons "agent" agent)
                                 (cons "correlationId" (or (jstring body "correlationId" nil) ""))
                                 (cons "envelopeId"    (or (jstring body "envelopeId"    nil) ""))))))
           (region (make-region-from-json
                    (or (and mapping (jget mapping "region"))
                        (obj "offset" 4200 "length" (length numbers)))))
           (ttl-ms (or (and mapping (jnumber mapping "ttlMs" nil)) 300000))
           (name (or (jstring body "name" nil)
                     (and mapping (jstring mapping "name" nil))
                     (format nil "agent:~a/~a/completion" provider agent)))
           (source (commit-signal-source state sensor-id name region numbers ttl-ms provider))
           (received-at (now-ms))
           ;; Build a signal result matching the shape ingest-signal-body returns,
           ;; so callers see "signal" not "source" — parity with CPP / AI.
           (signal-result (obj "success" t
                               "source" (source-json source)
                               "push" +json-null+
                               "timestamp" received-at))
           (mapping-id (or mapping-id-raw "")))
      (broadcast (obj "type" "agent.completion.received"
                      "provider" provider
                      "agent" agent
                      "sourceMappingId" mapping-id
                      "correlationId" (or (jstring body "correlationId" nil) +json-null+)
                      "envelopeId" (or (jstring body "envelopeId" nil) +json-null+)
                      "timestamp" received-at))
      (obj "success" t
           "completion" (obj "provider" provider
                             "agent" agent
                             "sensorId" sensor-id
                             "sourceMappingId" mapping-id
                             "correlationId" (or (jstring body "correlationId" nil) +json-null+)
                             "envelopeId"    (or (jstring body "envelopeId"    nil) +json-null+)
                             "completionId"  (or (jstring body "completionId" nil)
                                                 (jstring body "id" nil)
                                                 +json-null+)
                             "receivedAt" received-at)
           "signal" signal-result))))

(defun healthkit-status-json (state)
  (obj "bridgeId" (perception-state-healthkit-bridge-id state)
       "enabled" t
       "tokenConfigured" (json-bool (perception-state-healthkit-bridge-token state))
       "nativeAppRequired" t
       "nativeWorkOutsideRepo" t
       "registryKey" "healthkit:<typeIdentifier>"
       "statusEndpoint" "/api/integrations/healthkit/status"
       "ingestEndpoint" "/api/integrations/healthkit/ingest"
       "contract" (obj "transport" "https"
                       "singleSample" (arr "type" "value" "sourceName")
                       "batchSamples" (arr "bridgeId" "samples[]")
                       "auth" (if (perception-state-healthkit-bridge-token state) "bridgeToken|bearer" "none"))))

;; ── HealthKit AI-model helpers ────────────────────────────────────────────

(defun compact-hk-identifier (type)
  "Collapse 'HKQuantityTypeIdentifierHeartRate' → 'heartrate' for sensorId slugs.
Mirrors compactHKIdentifier in AI HealthKitBridge.ts."
  (let* ((prefixes '("HKQuantityTypeIdentifier"
                     "HKCategoryTypeIdentifier"
                     "HKWorkoutTypeIdentifier"
                     "HKCorrelationTypeIdentifier"
                     "HKDocumentTypeIdentifier"
                     "HKClinicalTypeIdentifier"
                     "HKSeriesTypeIdentifier"
                     "HKTypeIdentifier"))
         (stripped (or (loop for p in prefixes
                             when (and (>= (length type) (length p))
                                       (string= (subseq type 0 (length p)) p))
                               return (subseq type (length p)))
                       type))
         (cleaned (with-output-to-string (out)
                    (loop for ch across stripped
                          when (alphanumericp ch) do (write-char (char-downcase ch) out)))))
    (if (string= cleaned "") "unknown" cleaned)))

(defun derive-hk-sensor-id (type source-name mapping)
  "Build a deterministic sensorId for one HK sample.
Priority: mapping.sensorId > mapping.sensorIdTemplate > compact-slug fallback.
Tokens: {type}, {sampleType} (alias), {source}, {provider}, {agent}."
  (let ((fixed (jstring mapping "sensorId" nil)))
    (when (and fixed (not (string= fixed "")))
      (return-from derive-hk-sensor-id fixed)))
  (let ((tpl (jstring mapping "sensorIdTemplate" nil)))
    (when (and tpl (not (string= tpl "")))
      (return-from derive-hk-sensor-id
        (render-sensor-template tpl
                                (list (cons "type"       type)
                                      (cons "sampleType" type)
                                      (cons "source"     (or source-name ""))
                                      (cons "provider"   "healthkit")
                                      (cons "agent"      (or source-name "")))))))
  (let* ((slug   (compact-hk-identifier type))
         (suffix (if (and source-name (not (string= source-name "")))
                     (format nil ".~a" (source-id-part source-name))
                     "")))
    (format nil "hk.~a~a" slug suffix)))

(defun lookup-hk-mapping (state type source-name)
  "Inferred registry lookup: healthkit:<type>:<sourceName> wins over healthkit:<type>."
  (when (and source-name (not (string= source-name "")))
    (let ((specific (source-mapping-by-id state
                                          (format nil "healthkit:~a:~a" type source-name))))
      (when specific (return-from lookup-hk-mapping specific))))
  (source-mapping-by-id state (format nil "healthkit:~a" type)))

(defun explicit-hk-source-mapping-id (body)
  "Explicit HealthKit source mapping id, honoring sourceMappingId before mappingId."
  (let ((source-mapping-id (jstring body "sourceMappingId" nil)))
    (when (and source-mapping-id (not (string= source-mapping-id "")))
      (return-from explicit-hk-source-mapping-id source-mapping-id)))
  (let ((mapping-id (jstring body "mappingId" nil)))
    (when (and mapping-id (not (string= mapping-id "")))
      mapping-id)))

(defun ingest-healthkit-one (state body)
  "Resolve one HK sample against the registry.
Returns an obj with 'resolved' t on success, or 'unmapped' t + 'reason' on failure.
Callers accumulate results into resolved/unmapped lists."
  (let* ((type        (or (jstring body "type" nil) (jstring body "sampleType" nil) ""))
         (source-name (jstring body "sourceName" nil))
         (raw-value   (jnumber body "value" nil))
         (values      (cond
                        ((jarray-present-p body "values") (numbers-from-json (jget body "values")))
                        (raw-value                       (list (coerce raw-value 'double-float)))
                        (t                               nil))))
    (when (string= type "")
      (return-from ingest-healthkit-one
        (obj "unmapped" t "type" "" "sourceName" (or source-name +json-null+)
             "reason" "sample.type is required")))
    (unless values
      (return-from ingest-healthkit-one
        (obj "unmapped" t "type" type "sourceName" (or source-name +json-null+)
             "reason" "sample.value must be a finite number")))
    (let* ((explicit-id (explicit-hk-source-mapping-id body))
           (mapping (if explicit-id
                        (source-mapping-by-id state explicit-id)
                        (lookup-hk-mapping state type source-name))))
      (unless mapping
        (return-from ingest-healthkit-one
          (obj "unmapped" t "type" type "sourceName" (or source-name +json-null+)
               "reason" (if explicit-id
                            (format nil "unknown sourceMappingId \"~a\"" explicit-id)
                            (format nil "no registry mapping (declare healthkit:~a[:<sourceName>])" type)))))
      (let* ((region-json (jget mapping "region"))
             (region (when (and region-json
                                (not (eq region-json +json-null+)))
                       (handler-case (make-region-from-json region-json)
                         (error () nil)))))
        (unless region
          (return-from ingest-healthkit-one
            (obj "unmapped" t "type" type "sourceName" (or source-name +json-null+)
                 "reason" "mapping is missing region.offset/region.length")))
        (let* ((sensor-id (derive-hk-sensor-id type source-name mapping))
               (ttl-ms    (or (jnumber mapping "ttlMs" nil) (* 60 60000)))
               (name      (or (jstring mapping "name" nil)
                              (format nil "healthkit:~a" type)))
               (source    (commit-signal-source state sensor-id name region values ttl-ms "healthkit")))
          (obj "resolved"       t
               "sensorId"       sensor-id
               "name"           name
               "type"           type
               "sourceName"     (or source-name +json-null+)
               "sourceMappingId" (or (jstring mapping "id" nil) +json-null+)
               "region"         (obj "offset" (region-offset region) "length" (region-length region))
               "values"         (vectorize values)
               "ttlMs"          ttl-ms
               "source"         (source-json source)))))))

(defun ingest-healthkit (state body &optional bearer-token)
  "Returns (cons http-status obj) — callers use json-response with both.
BEARER-TOKEN must be captured on the request thread (see request-bearer-token);
it is accepted as an alternative to the body bridgeToken/token fields."
  (let ((required-token (perception-state-healthkit-bridge-token state)))
    (when (and required-token
               (not (string= required-token (or (jstring body "token" nil)
                                                (jstring body "bridgeToken" nil)
                                                "")))
               (not (equal required-token bearer-token)))
      (return-from ingest-healthkit
        (cons 401 (obj "success" +json-false+ "error" "invalid HealthKit bridge token")))))
  (let ((resolved nil) (unmapped nil))
    (if (jget body "samples")
        (dolist (sample (jarray-list (jget body "samples")))
          (let ((r (ingest-healthkit-one state sample)))
            (if (jget r "resolved") (push r resolved) (push r unmapped))))
        (let ((r (ingest-healthkit-one state body)))
          (if (jget r "resolved") (push r resolved) (push r unmapped))))
    (let* ((resolved-list (nreverse resolved))
           (unmapped-list (nreverse unmapped))
           (status (cond ((and unmapped-list (null resolved-list)) 400)
                         (unmapped-list 207)
                         (t 200))))
      (broadcast (obj "type" "healthkit.ingest"
                      "bridgeId" (perception-state-healthkit-bridge-id state)
                      "samples"  (+ (length resolved-list) (length unmapped-list))
                      "resolved" (length resolved-list)
                      "unmapped" (length unmapped-list)
                      "timestamp" (now-ms)))
      (cons status
            (obj "success"  (json-bool (null unmapped-list))
                 "bridgeId" (perception-state-healthkit-bridge-id state)
                 "resolved" (vectorize resolved-list)
                 "unmapped" (vectorize unmapped-list))))))

(defun carekit-status-json (state)
  (let ((token-set (perception-state-carekit-bridge-token state)))
    (obj "bridgeId" (perception-state-carekit-bridge-id state)
         "enabled" t
         "defaultSourceMappingId" (perception-state-carekit-default-source-mapping-id state)
         "tokenConfigured" (json-bool token-set)
         "nativeAppRequired" t
         "nativeWorkOutsideRepo" t
         "registryKey" "carekit:<sampleType>"
         "statusEndpoint" "/api/integrations/carekit/status"
         "ingestEndpoint" "/api/integrations/carekit/ingest"
         "contract" (obj "transport" "https"
                         "singleSample" (arr "bridgeId" "sampleType" "sourceMappingId" "values")
                         "batchSamples" (arr "bridgeId" "samples[]")
                         "auth" (if token-set "bridgeToken" "external-transport")))))

(defun ingest-carekit-one (state body)
  (handler-case
    (let* ((bridge-id (or (jstring body "bridgeId" nil)
                          (perception-state-carekit-bridge-id state)))
           (sample-type (or (jstring body "sampleType" nil)
                            (jstring body "type" nil)
                            "task-event"))
           (mapping-id (or (jstring body "sourceMappingId" nil)
                           (perception-state-carekit-default-source-mapping-id state)))
           (mapping (source-mapping-by-id state mapping-id))
           (values (numbers-from-json (or (jget body "values") (jget body "vector") (arr))))
           (sensor-id (or (jstring body "sensorId" nil)
                          (render-sensor-template
                           (or (jstring mapping "sensorIdTemplate" nil) "carekit.{sampleType}")
                           (list (cons "bridgeId" bridge-id)
                                 (cons "sampleType" sample-type)
                                 (cons "type" sample-type)
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
                                         ttl-ms
                                         "carekit")))
      (obj "success" t
           "sourceMappingId" mapping-id
           "sampleType" sample-type
           "taskId" (or (jstring body "taskId" nil) +json-null+)
           "carePlanId" (or (jstring body "carePlanId" nil) +json-null+)
           "sensorId" sensor-id
           "source" (source-json source)))
    (error (condition)
      (obj "success" +json-false+
           "sampleType" (or (jstring body "sampleType" nil) (jstring body "type" nil) "task-event")
           "taskId" (or (jstring body "taskId" nil) +json-null+)
           "carePlanId" (or (jstring body "carePlanId" nil) +json-null+)
           "reason" (princ-to-string condition)))))

(defun ingest-carekit (state body)
  ;; Returns (cons http-status body-hash) so the route handler can set the correct status.
  (let ((required-token (perception-state-carekit-bridge-token state)))
    (when (and required-token
               (not (string= required-token (or (jstring body "token" nil)
                                                (jstring body "bridgeToken" nil)
                                                ""))))
      (return-from ingest-carekit
        (cons 401 (obj "success" +json-false+ "error" "invalid CareKit bridge token")))))
  (let ((results nil)
        (all-ok t)
        (reserved-keys '("samples" "bridgeToken" "token")))
    (if (jarray-present-p body "samples")
        (dolist (sample (jarray-list (jget body "samples")))
          ;; Merge top-level fields into sample (sample keys win); strip reserved keys.
          (let ((merged (make-hash-table :test #'equal)))
            (maphash (lambda (k v)
                       (unless (member k reserved-keys :test #'string=)
                         (setf (gethash k merged) v)))
                     body)
            (when (hash-table-p* sample)
              (maphash (lambda (k v) (setf (gethash k merged) v)) sample))
            (let ((r (ingest-carekit-one state merged)))
              (unless (eq (gethash "success" r) t) (setf all-ok nil))
              (push r results))))
        (let ((r (ingest-carekit-one state body)))
          (unless (eq (gethash "success" r) t) (setf all-ok nil))
          (push r results)))
    (let ((ts (now-ms)))
      (broadcast (obj "type" "carekit.ingest"
                      "bridgeId" (perception-state-carekit-bridge-id state)
                      "samples" (length results)
                      "success" (json-bool all-ok)
                      "timestamp" ts))
      (cons (if all-ok 200 207)
            (obj "success" (json-bool all-ok)
                 "bridgeId" (perception-state-carekit-bridge-id state)
                 "results" (vectorize (nreverse results))
                 "timestamp" ts)))))

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
               (mapping (and mapping-id (source-mapping-by-id state mapping-id)))
               (payload (obj "model" model
                             "stream" +json-false+
                             "messages" (vectorize
                                         (list (obj "role" "system"
                                                    "content" "Return concise JSON. If committing a PE completion, include numeric values.")
                                               (obj "role" "user"
                                                    "content" (dispatch-record-prompt record))))))
               (response nil)
               (content "")
               (values nil)
               (completion +json-null+))
          (when (and mapping-id (not mapping))
            (error "Unknown sourceMappingId \"~a\"" mapping-id))
          (when mapping
            (setf (jget payload "format") (completion-schema-for-mapping mapping)))
          (setf response (http-request-json (format nil "~a/api/chat" (perception-state-ollama-base-url state))
                                            :method :post
                                            :payload payload))
          (setf content (or (jstring (jget response "message") "content" nil) ""))
          (setf values (completion-values-from-content content mapping))
          (setf completion (ingest-completion
                            state
                            (obj "provider" "ollama"
                                 "agent" (jstring record "target" "ollama")
                                 "sourceMappingId" mapping-id
                                 "correlationId" id
                                 "values" values)))
          (update-dispatch-record state id (obj "status" "delivered"
                                                "adapter" "ollama"
                                                "provider" "ollama"
                                                "externalRunId" (or (jstring response "created_at" nil)
                                                                    (make-id "ollama-run"))
                                                "providerReceipt" (obj "model" model
                                                                       "completionCommitted" t)))
          (obj "success" t
               "dispatchId" id
               "provider" "ollama"
               "model" model
               "response" response
               "completionCommitted" t
               "completion" completion
               "receipt" (obj "provider" "ollama"
                              "adapter" "ollama"
                              "status" "sent"
                              "externalRunId" (or (jstring response "created_at" nil)
                                                  (make-id "ollama-run"))
                              "providerReceipt" (obj "model" model
                                                     "completionCommitted" t))))
      (error (condition)
        (update-dispatch-record state id (obj "status" "failed"
                                              "adapter" "ollama"
                                              "provider" "ollama"
                                              "lastError" (princ-to-string condition)
                                              "providerReceipt" (obj "model" (or (jstring body "model" nil)
                                                                                 (perception-state-ollama-model state)))))
        (obj "success" +json-false+
             "dispatchId" id
             "provider" "ollama"
             "error" (princ-to-string condition)
             "receipt" (obj "provider" "ollama"
                            "adapter" "ollama"
                            "status" "failed"
                            "error" (princ-to-string condition)))))))

(defun openai-response-text (response)
  (or (jstring response "output_text" nil)
      (let ((out nil))
        (dolist (item (jarray-list (or (jget response "output") (arr))))
          (dolist (content (jarray-list (or (jget item "content") (arr))))
            (when (and (null out) (jstring content "text" nil))
              (setf out (jstring content "text")))))
        out)
      ""))

(defun assert-openai-response-ready (response)
  (let ((status (jstring response "status" nil)))
    (when (and status (not (string= status "completed")))
      (error "OpenAI response status is ~a" status)))
  (when (jobject-p (jget response "error"))
    (error "OpenAI response error: ~a"
           (or (jstring (jget response "error") "message" nil)
               (json-stringify (jget response "error")))))
  (dolist (item (jarray-list (or (jget response "output") (arr))))
    (when (string= (jstring item "type" "") "refusal")
      (error "OpenAI response was refused"))
    (dolist (content (jarray-list (or (jget item "content") (arr))))
      (when (or (string= (jstring content "type" "") "refusal")
                (jstring content "refusal" nil))
        (error "OpenAI response was refused")))))

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
               (mapping (and mapping-id (source-mapping-by-id state mapping-id)))
               (payload (obj "model" model
                             "instructions" "Return concise JSON. If committing a PE completion, include numeric values."
                             "input" (dispatch-record-prompt record)))
               (response nil)
               (content "")
               (values nil)
               (completion +json-null+))
          (when (and mapping-id (not mapping))
            (error "Unknown sourceMappingId \"~a\"" mapping-id))
          (unless (jobject-p (jget payload "text"))
            (setf (jget payload "text") (openai-text-format-for-mapping mapping)))
          (setf response (http-request-json
                          (format nil "~a/responses" (perception-state-openai-base-url state))
                          :method :post
                          :payload payload
                          :headers (list (cons "Authorization"
                                               (format nil "Bearer ~a" (perception-state-openai-api-key state))))))
          (assert-openai-response-ready response)
          (setf content (openai-response-text response))
          (setf values (completion-values-from-content content mapping))
          (setf completion (ingest-completion
                            state
                            (obj "provider" "openai"
                                 "agent" (jstring record "target" "openai")
                                 "sourceMappingId" mapping-id
                                 "correlationId" id
                                 "completionId" (or (jstring response "id" nil) +json-null+)
                                 "values" values)))
          (update-dispatch-record state id (obj "status" "delivered"
                                                "adapter" "openai"
                                                "provider" "openai"
                                                "externalRunId" (or (jstring response "id" nil)
                                                                    (make-id "openai-run"))
                                                "providerReceipt" (obj "model" model
                                                                       "completionCommitted" t)))
          (obj "success" t
               "dispatchId" id
               "provider" "openai"
               "model" model
               "response" response
               "completionCommitted" t
               "completion" completion
               "receipt" (obj "provider" "openai"
                              "adapter" "openai"
                              "status" "sent"
                              "externalRunId" (or (jstring response "id" nil)
                                                  (make-id "openai-run"))
                              "providerReceipt" (obj "model" model
                                                     "completionCommitted" t))))
      (error (condition)
        (update-dispatch-record state id (obj "status" "failed"
                                              "adapter" "openai"
                                              "provider" "openai"
                                              "lastError" (princ-to-string condition)
                                              "providerReceipt" (obj "model" (or (jstring body "model" nil)
                                                                                 (perception-state-openai-model state)))))
        (obj "success" +json-false+
             "dispatchId" id
             "provider" "openai"
             "error" (princ-to-string condition)
             "receipt" (obj "provider" "openai"
                            "adapter" "openai"
                            "status" "failed"
                            "error" (princ-to-string condition)))))))

(defun acp-status-json (state)
  (obj "enabled" (json-bool (perception-state-acp-enabled-p state))
       "platform" (perception-state-acp-platform state)
       "surface" (perception-state-acp-surface state)
       "adapter" "openclaw-xacp"
       "command" (perception-state-acp-command state)
       "gatewayUrl" (or (perception-state-acp-gateway-url state) +json-null+)
       "sessionKey" (or (perception-state-acp-session-key state) +json-null+)
       "targetAgent" (perception-state-acp-target-agent state)
       "completionSourceMappingId" (perception-state-acp-completion-source-mapping-id state)
       "dispatchEndpoint" "/api/integrations/acp/dispatch"
       "completionEndpoint" "/api/integrations/completions"
       "noWaitDispatch" t
       "contract" (obj "dispatch" "Record an ACP/OpenClaw handoff receipt only; do not run or wait for the harness in the PE cycle."
                       "completion" "External ACP/OpenClaw adapters commit finished results through /api/integrations/completions.")))

(defun dispatch-acp (state body)
  (let* ((id (or (jstring body "dispatchId" nil) (jstring body "id" nil)))
         (record (and id (lookup-dispatch-record state id))))
    (unless record
      (return-from dispatch-acp (cons 404 (obj "error" "Dispatch record not found"))))
    (let* ((target-agent (or (jstring body "targetAgent" nil)
                             (jstring body "agent" nil)
                             (jstring record "target" nil)
                             (perception-state-acp-target-agent state)))
           (session-key (or (jstring body "sessionKey" nil)
                            (perception-state-acp-session-key state)))
           (mapping-id (or (jstring body "sourceMappingId" nil)
                           (perception-state-acp-completion-source-mapping-id state)))
           (external-run-id (or (jstring body "externalRunId" nil)
                                (make-id "acp-handoff")))
           (prompt (or (jstring body "prompt" nil)
                       "Handle this RealityEngine trigger envelope through the configured OpenClaw ACP session and return a PE completion values array."))
           (handoff (obj "protocol" "ACP"
                         "surface" (perception-state-acp-surface state)
                         "platform" (perception-state-acp-platform state)
                         "adapter" "openclaw-xacp"
                         "command" (or (jstring body "command" nil)
                                       (perception-state-acp-command state))
                         "gatewayUrl" (or (jstring body "gatewayUrl" nil)
                                          (perception-state-acp-gateway-url state))
                         "sessionKey" (or session-key +json-null+)
                         "targetAgent" target-agent
                         "completionEndpoint" "/api/integrations/completions"
                         "completionSourceMappingId" mapping-id
                         "noWaitDispatch" t
                         "prompt" prompt
                         "dispatchId" id
                         "envelopeId" (or (jstring record "envelopeId" nil) +json-null+)
                         "correlationId" (or (jstring record "correlationId" nil) +json-null+))))
      (when (jobject-p (jget body "metadata"))
        (setf (jget handoff "metadata") (jget body "metadata")))
      (update-dispatch-record state id
                              (obj "status" (or (jstring body "status" nil) "accepted")
                                   "adapter" "openclaw-xacp"
                                   "provider" "acp"
                                   "externalRunId" external-run-id
                                   "incrementAttempts" t
                                   "metadata" handoff))
      (cons 202 (obj "success" t
                     "accepted" t
                     "dispatchId" id
                     "provider" "acp"
                     "platform" (perception-state-acp-platform state)
                     "surface" (perception-state-acp-surface state)
                     "externalRunId" external-run-id
                     "noWaitDispatch" t
                     "handoff" handoff)))))

(defun push-perception (state include-machine-results &key compact)
  (let* ((engine (perception-state-engine state))
         (vector (assemble-perception-vector engine))
         ;; Always ask the Reality Engine for the perceptual space and the
         ;; machine results. The PE needs both to compute the next input
         ;; vector, so what the caller wants *reported* must not decide what
         ;; the engine gets to *know*.
         ;;
         ;; Gating the request on these flags made the state update below
         ;; unreachable: with neither field returned, next-ps came out empty,
         ;; the dimension guard rejected it, and the PE's perceptual space
         ;; never advanced. Under compact — which is what the regression
         ;; suite and the MQTT bridge both use — the PE was frozen at its
         ;; initial state while C++ carried its space forward, so the two
         ;; runtimes agreed on the first push and diverged on every one
         ;; after it.  The response is trimmed after the fact instead.
         (payload (obj "vector" (vectorize vector)
                       "matchAlgorithm" (perception-engine-match-algorithm engine)
                       "includeMachineResults" t
                       "includePerceptualSpace" t
                       "compact" (json-bool compact))))
    (handler-case
        (let* ((step      (http-post-json (format nil "~a/api/perceive" (perception-state-reality-url state))
                                          payload))
               (raw-ps    (numbers-from-json (or (jget step "perceptualSpace") (arr))))
               (next-ps   (aggregate-machine-outputs raw-ps (jget step "machineResults")))
               (ts        (now-ms)))
          (when (>= (length next-ps) (perception-engine-dimension engine))
            (update-from-perceptual-space engine next-ps))
          ;; Advance playback once per push, after the vector was assembled and
          ;; sent. This also increments global-step. Cursors used to advance
          ;; inside assembly, which made `/api/state` advance them too — see
          ;; advance-perception-engine.
          (advance-perception-engine engine)
          ;; Semantic audit (SEMANTIC_AUDIT_CONTRACT.md): one re:PerceptionEvent
          ;; per active source region written this push, attributed to the
          ;; integration feeding it and joined to the corpus ABox when the
          ;; source names a machine.
          (let ((bases (semantics-manifest-bases)))
            (maphash (lambda (id source)
                       (declare (ignore id))
                       (when (source-active-p source)
                         (record-perception-event
                          (or (source-origin source) (source-kind source) "unattributed")
                          (and (source-machine-name source)
                               (gethash (source-machine-name source) bases)
                               t))))
                     (perception-engine-sources engine)))
          (record-dispatch-envelopes-from-step state step)
          ;; Trim the reported step to what was asked for.  Done here, after
          ;; the state update and the dispatch pass, so asking for less never
          ;; means the engine does less.
          ;;
          ;; Shape fixed by SURFACE_SPEC.md, "POST /api/push response shape":
          ;; compact omits machineResults and nothing else. This used to empty
          ;; machineResults rather than remove it — an empty object is not an
          ;; absent key to a consumer walking the response — and to drop
          ;; perceptualSpace entirely, so a compact push returned no reality
          ;; vector at all. The engine computed the right answer and did not
          ;; report it, which the cross-runtime parity stage read as engine
          ;; divergence (RealityEngine_Scala#43).
          (when (jobject-p step)
            (when (or compact (not include-machine-results))
              (remhash "machineResults" step)))
          (setf (perception-engine-last-push engine) step)
          (let ((result (obj "success" t
                             "step" step
                             "timestamp" ts
                             "globalStep" (perception-engine-global-step engine))))
            (broadcast (obj "type" "push-result"
                            "success" t
                            "step" step
                            "timestamp" ts
                            "globalStep" (perception-engine-global-step engine)))
            result))
      (error (condition)
        (obj "success" +json-false+ "error" (princ-to-string condition) "timestamp" (now-ms))))))

;; Bundled yuma-agriculture demo mapping registry for GET /api/mqtt/example.
;; Mirrors RealityEngine_CPP/config/mqtt-mappings.yuma-agriculture.json.
;; Band normalization emits 1.0 (in nominal range) or 0.0 (out of range),
;; producing the 4-bit status patterns the AGX001/005/026/032 machines expect.
(defparameter +mqtt-example-mappings+
  (parse-json "{\"version\":\"1.0\",\"defaults\":{\"ttlMs\":60000,\"qos\":0,\"acceptRetained\":true,\"pushMode\":\"debounced\",\"debounceMs\":500},\"mappings\":[
    {\"id\":\"agx001-ph-ok\",        \"topicFilter\":\"LATERAL/WaterSuite/DEV0000001/SensorReadings/v1\",     \"sensorIdTemplate\":\"agx001.water.ph.ok\",        \"region\":{\"offset\":40, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wpH\"},        \"normalize\":{\"mode\":\"band\",\"min\":6.5,  \"max\":8.5}},
    {\"id\":\"agx001-ec-ok\",        \"topicFilter\":\"LATERAL/WaterSuite/DEV0000001/SensorReadings/v1\",     \"sensorIdTemplate\":\"agx001.water.ec.ok\",        \"region\":{\"offset\":41, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wEC\"},         \"normalize\":{\"mode\":\"band\",\"min\":0.5,  \"max\":3.0}},
    {\"id\":\"agx001-orp-ok\",       \"topicFilter\":\"LATERAL/WaterSuite/DEV0000001/SensorReadings/v1\",     \"sensorIdTemplate\":\"agx001.water.orp.ok\",       \"region\":{\"offset\":42, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wORP\"},        \"normalize\":{\"mode\":\"band\",\"min\":200,  \"max\":600}},
    {\"id\":\"agx001-turbidity-ok\", \"topicFilter\":\"LATERAL/WaterSuite/DEV0000001/SensorReadings/v1\",     \"sensorIdTemplate\":\"agx001.water.turbidity.ok\", \"region\":{\"offset\":43, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wTurbidity\"}, \"normalize\":{\"mode\":\"band\",\"min\":0,    \"max\":100}},
    {\"id\":\"agx005-do-ok\",        \"topicFilter\":\"LATERAL/DOSuite/DEV0000017/SensorReadings/v1\",        \"sensorIdTemplate\":\"agx005.do.level.ok\",        \"region\":{\"offset\":84, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wDO\"},         \"normalize\":{\"mode\":\"band\",\"min\":5,    \"max\":25}},
    {\"id\":\"agx005-do-temp-ok\",   \"topicFilter\":\"LATERAL/DOSuite/DEV0000017/SensorReadings/v1\",        \"sensorIdTemplate\":\"agx005.do.temp.ok\",         \"region\":{\"offset\":85, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wDOTemp\"},     \"normalize\":{\"mode\":\"band\",\"min\":60,   \"max\":85}},
    {\"id\":\"agx005-do-watch\",     \"topicFilter\":\"LATERAL/DOSuite/DEV0000017/SensorReadings/v1\",        \"sensorIdTemplate\":\"agx005.do.watch\",           \"region\":{\"offset\":86, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wDO\"},         \"normalize\":{\"mode\":\"band\",\"min\":3,    \"max\":5}},
    {\"id\":\"agx005-temp-watch\",   \"topicFilter\":\"LATERAL/DOSuite/DEV0000017/SensorReadings/v1\",        \"sensorIdTemplate\":\"agx005.do.temp.watch\",      \"region\":{\"offset\":87, \"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/wDOTemp\"},     \"normalize\":{\"mode\":\"band\",\"min\":85,   \"max\":95}},
    {\"id\":\"agx026-temp-ok\",      \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx026.temp.ok\",            \"region\":{\"offset\":184,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aTemp\"},       \"normalize\":{\"mode\":\"band\",\"min\":65,   \"max\":85}},
    {\"id\":\"agx026-humidity-ok\",  \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx026.humidity.ok\",        \"region\":{\"offset\":185,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aHum\"},        \"normalize\":{\"mode\":\"band\",\"min\":40,   \"max\":70}},
    {\"id\":\"agx026-temp-watch\",   \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx026.temp.watch\",         \"region\":{\"offset\":186,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aTemp\"},       \"normalize\":{\"mode\":\"band\",\"min\":85,   \"max\":95}},
    {\"id\":\"agx026-humidity-watch\",\"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",  \"sensorIdTemplate\":\"agx026.humidity.watch\",     \"region\":{\"offset\":187,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aHum\"},        \"normalize\":{\"mode\":\"band\",\"min\":20,   \"max\":40}},
    {\"id\":\"agx032-co2-ok\",       \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx032.co2.ok\",             \"region\":{\"offset\":228,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aCO2\"},        \"normalize\":{\"mode\":\"band\",\"min\":600,  \"max\":1500}},
    {\"id\":\"agx032-co2-watch\",    \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx032.co2.watch\",          \"region\":{\"offset\":229,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aCO2\"},        \"normalize\":{\"mode\":\"band\",\"min\":1500, \"max\":3000}},
    {\"id\":\"agx032-co2-danger\",   \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx032.co2.danger\",         \"region\":{\"offset\":230,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aCO2\"},        \"normalize\":{\"mode\":\"band\",\"min\":3000, \"max\":5000}},
    {\"id\":\"agx032-temp-ok\",      \"topicFilter\":\"LATERAL/AmbientSuite/DEV0000009/SensorReadings/v1\",   \"sensorIdTemplate\":\"agx032.temp.ok\",            \"region\":{\"offset\":231,\"length\":1},\"extract\":{\"type\":\"json\",\"pointer\":\"/data/aTemp\"},       \"normalize\":{\"mode\":\"band\",\"min\":65,   \"max\":85}}
  ]}"))

(defun perception-routes (actor)
  (list
   (make-route "GET" "/" (lambda (_ body query)
                           (declare (ignore _ body query))
                           (json-response (obj "service" "Perception Engine (LSP)" "status" "running"))))
   (make-route "GET" "/api/health" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response (obj "status" "healthy"))))
   (make-route "GET" "/api/state" (lambda (_ body query)
                                    (declare (ignore _ body query))
                                    (json-response (actor-ask actor (lambda (state)
                                                                      (perception-state-json (perception-state-engine state)))))))
   ;; Prometheus exposition — docs/PE_METRICS_CONTRACT.md.
   (make-route "GET" "/api/metrics" (lambda (_ body query)
                                      (declare (ignore _ body query))
                                      (text-response
                                       (actor-ask actor
                                                  (lambda (state)
                                                    (let ((engine (perception-state-engine state)))
                                                      (semantic-metrics-text
                                                       (hash-table-count (perception-engine-sources engine))
                                                       (or (perception-engine-global-step engine) 0)
                                                       (or (perception-engine-dimension engine) 0)
                                                       0)))))))
   (make-route "GET" "/api/integrations/status" (lambda (_ body query)
                                                  (declare (ignore _ body query))
                                                  (json-response (actor-ask actor #'integrations-status-json))))
   (make-route "POST" "/api/integrations/completions" (lambda (_ body query)
                                                        (declare (ignore _ query))
                                                        (let ((result (actor-ask actor
                                                                                 (lambda (state)
                                                                                   (ingest-completion state body)))))
                                                          (if (consp result)
                                                              (json-response (cdr result) (car result))
                                                              (json-response result)))))
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
                                                           (progn
                                                             (broadcast (obj "type" "dispatch-updated" "record" record))
                                                             (json-response record))
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
   (make-route "GET" "/api/integrations/acp/status" (lambda (_ body query)
                                                      (declare (ignore _ body query))
                                                      (json-response (actor-ask actor #'acp-status-json))))
   (make-route "POST" "/api/integrations/acp/dispatch" (lambda (_ body query)
                                                         (declare (ignore _ query))
                                                         (let ((result (actor-ask actor
                                                                                  (lambda (state)
                                                                                    (dispatch-acp state body)))))
                                                           (if (consp result)
                                                               (json-response (cdr result) (car result))
                                                               (json-response result)))))
   (make-route "GET" "/api/integrations/healthkit/status" (lambda (_ body query)
                                                            (declare (ignore _ body query))
                                                            (json-response (actor-ask actor #'healthkit-status-json))))
   (make-route "POST" "/api/integrations/healthkit/ingest" (lambda (_ body query)
                                                             (declare (ignore _ query))
                                                             (let* ((bearer (request-bearer-token))
                                                                    (result (actor-ask actor
                                                                                       (lambda (state)
                                                                                         (ingest-healthkit state body bearer)))))
                                                               (if (consp result)
                                                                   (json-response (cdr result) (car result))
                                                                   (json-response result)))))
   (make-route "GET" "/api/integrations/carekit/status" (lambda (_ body query)
                                                          (declare (ignore _ body query))
                                                          (json-response (actor-ask actor #'carekit-status-json))))
   (make-route "POST" "/api/integrations/carekit/ingest" (lambda (_ body query)
                                                           (declare (ignore _ query))
                                                           (let ((result (actor-ask actor
                                                                                    (lambda (state)
                                                                                      (ingest-carekit state body)))))
                                                             (if (consp result)
                                                                 (json-response (cdr result) (car result))
                                                                 (json-response result)))))
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
                                                                         (not (jbool body "compact" nil)))
                                                                  :compact (jbool body "compact" nil)))
                                               :timeout 120))))
   (make-route "GET" "/api/push/:id" (lambda (params body query)
                                      (declare (ignore body query))
                                      (error-response (format nil "Push record ~a not found" (gethash "id" params)) 404)))
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
                                                  ;; Reset run state in place. Rebuilding the engine
                                                  ;; struct here discarded every registered source,
                                                  ;; which is a different operation — see
                                                  ;; reset-perception-engine.
                                                  (reset-perception-engine
                                                   (perception-state-engine state))
                                                  (obj "success" t))))))
   (make-route "GET" "/api/sources" (lambda (_ body query)
                                     (declare (ignore _ body query))
                                     (json-response
                                      (actor-ask actor
                                                 (lambda (state)
                                                   (obj "sources" (vectorize
                                                                   (mapcar #'source-json
                                                                           (sources-in-canonical-order
                                                                            (perception-state-engine state))))))))))
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
                                                              (text-response
                                                               (canonical-bootstrap-summary-json
                                                                (actor-ask actor #'bootstrap-test-sources-from-machines))
                                                               200
                                                               "application/json; charset=utf-8")))
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
   ;; MQTT bridge — same surface as AI / CPP / Scala.  Returns enabled=false when
   ;; MQTT_BROKER_HOST/MQTT_BROKER_URL was not set at PE startup; otherwise
   ;; reports connection state + bridge-level counters + the broker config.
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
	                     (t (json-response result))))))
   (make-route "POST" "/api/mqtt/enable"
               (lambda (_ body query)
                 (declare (ignore _ query))
                 (let ((result (actor-ask actor
                                          (lambda (state)
                                            (mqtt-enable-bridge state actor body)))))
                   (cond
                     ((stringp result) (error-response result 400))
                     (t (json-response result))))))
   (make-route "POST" "/api/mqtt/disable"
               (lambda (_ body query)
                 (declare (ignore _ body query))
                 (actor-ask actor
                            (lambda (state)
                              (let ((b (perception-state-mqtt-bridge state)))
                                (when b
                                  (mqtt-bridge-stop b)
                                  (setf (perception-state-mqtt-bridge state) nil)
                                  (format *standard-output*
                                          "[MQTT] bridge disabled via API~%")))))
                 (json-response (obj "success" (json-bool t) "enabled" (json-bool nil)))))

   ;; GET /api/mqtt/example — bundled yuma-agriculture demo registry.
   ;; Mirrors RealityEngine_CPP/config/mqtt-mappings.yuma-agriculture.json.
   ;; Uses band normalization so cells emit 1.0/0.0, matching the 4-bit
   ;; status pattern the agriculture machines (AGX001/005/026/032) expect.
   (make-route "GET" "/api/mqtt/example"
               (lambda (_ body query)
                 (declare (ignore _ body query))
                 (json-response +mqtt-example-mappings+)))))

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
                                      :ttl-ms (if (> ttl-ms 0) ttl-ms 30000)
                                      :origin "mqtt"))))
    (broadcast (obj "type" "mqtt-ingest"
                    "payload" (obj "sensorId" sensor-id
                                   "offset" offset
                                   "length" length
                                   "values" (vectorize values)
                                   "ttlMs" ttl-ms
                                   "topic" topic
                                   "mappingId" mapping-id
                                   "timestamp" (now-ms))))))

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

(defun parse-mqtt-broker-url (url)
  "Parse 'mqtt://host:port' or 'host:port' into (values host port).
Port defaults to 1883 when absent or unparseable."
  (let* ((addr (let ((sep (search "://" url)))
                 (if sep (subseq url (+ sep 3)) url)))
         (colon (position #\: addr :from-end t))
         (host  (if colon (subseq addr 0 colon) addr))
         (port  (when colon
                  (parse-integer (subseq addr (1+ colon)) :junk-allowed t))))
    (values host (or port 1883))))

(defun mqtt-enable-bridge (state actor body)
  "POST /api/mqtt/enable handler — parse brokerUrl + mappings, build and
start a new MQTT bridge.  Returns a JSON object on success or an error
string when input is invalid."
  (let* ((broker-url   (jget body "brokerUrl"))
         (mappings-val (jget body "mappings"))
         (body-username (let ((v (jget body "username"))) (when (and v (string/= v "")) v)))
         (body-password (let ((v (jget body "password"))) (when (and v (string/= v "")) v))))
    (when (or (null broker-url) (string= broker-url ""))
      (return-from mqtt-enable-bridge "brokerUrl is required"))
    (multiple-value-bind (broker-host broker-port)
        (parse-mqtt-broker-url broker-url)
      (let ((new-registry nil))
        (handler-case
            (setf new-registry (mqtt-mapping-registry-from-json
                                (or mappings-val (obj "mappings" (arr)))))
          (error (c)
            (return-from mqtt-enable-bridge (format nil "schema: ~a" c))))
        (when (zerop (length (mqtt-mapping-registry-rules new-registry)))
          (return-from mqtt-enable-bridge
            "mappings array is empty — at least one rule is required"))
        (let* ((allow (member (env "MQTT_ALLOW_REGION_OVERLAP" "0")
                              '("1" "true" "TRUE" "yes") :test #'string=))
               (warnings (mqtt-validate-overlaps new-registry allow))
               (current  (perception-state-mqtt-bridge state))
               (existing-client-id (when current
                                     (mqtt-client-config-client-id
                                      (reality-engine-lsp::%mqtt-client-config
                                       (reality-engine-lsp::%mqtt-bridge-client current)))))
               (cfg (make-mqtt-client-config
                     :broker-host broker-host
                     :broker-port broker-port
                     :client-id   (or existing-client-id "reality-engine-pe-lsp")
                     :username    body-username
                     :password    body-password))
               (ingest (lambda (sensor-id offset length values ttl-ms topic mapping-id)
                         (actor-tell actor
                                     (lambda (st)
                                       (ingest-mqtt-signal st sensor-id offset length
                                                           values ttl-ms topic mapping-id)))))
               (trigger (lambda ()
                          (actor-tell actor
                                      (lambda (st)
                                        (handler-case (push-perception st nil)
                                          (error (c)
                                            (format *error-output*
                                                    "[mqtt-bridge] push trigger error: ~a~%" c))))))))
          (when current (mqtt-bridge-stop current))
          (let ((bridge (make-mqtt-bridge cfg new-registry ingest trigger)))
            (handler-case
                (progn
                  (mqtt-bridge-start bridge)
                  (setf (perception-state-mqtt-bridge state) bridge)
                  (format *standard-output*
                          "[MQTT] bridge enabled via API — broker=~a:~a mappings=~a~%"
                          broker-host broker-port
                          (length (mqtt-mapping-registry-rules new-registry)))
                  (obj "success" (json-bool t)
                       "enabled" (json-bool t)
                       "mappings" (length (mqtt-mapping-registry-rules new-registry))
                       "warnings" (vectorize warnings)))
              (error (c)
                (setf (perception-state-mqtt-bridge state) nil)
                (format nil "MQTT bridge failed to start: ~a" c)))))))))

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

(defun start-perception-service (&key (port 5600)
                                      (reality-url "http://localhost:5601")
                                      (localai-url "http://localhost:4000")
                                      (localai-machine-dir "../localAIStack/data/machines")
                                      (dimension 7680))
  (let* ((state (make-perception-state-from-config :dimension dimension
                                                   :reality-url reality-url
                                                   :localai-url localai-url
                                                   :localai-machine-dir localai-machine-dir))
         (actor (state-actor "perception-service" state)))
    (load-integrations-config state)
    ;; Background machine catalog refresher — best-effort initial fetch + 60 s loop.
    ;; Starts before HTTP so the catalog is warm before the first push cycle.
    (start-machine-catalog-refresher state)
    ;; Boot the bridge after the actor is alive so the ingest closure can
    ;; tell-into it safely.  The bridge boot is fire-and-forget — if MQTT
    ;; isn't configured the PE still serves HTTP signals normally.
    (actor-tell actor (lambda (st) (maybe-boot-mqtt-bridge st actor)))
    ;; SSE broadcast at GET /api/events + MCP JSON-RPC at /mcp.
    ;; Both are registered as prefix dispatchers ahead of the main route table
    ;; so they intercept their paths before the catch-all handler claims them.
    (start-http-server port (perception-routes actor)
                       :name "perception-engine-lsp"
                       :extra-dispatchers
                       (append
                        (list (hunchentoot:create-prefix-dispatcher
                               "/api/events"
                               #'sse-events-handler)
                              (hunchentoot:create-prefix-dispatcher
                               "/ws"
                               #'ws-handler))
                        (make-mcp-dispatchers actor)))))

(defun start-perception-from-environment ()
  (start-perception-service :port (env-int "PERCEPTION_ENGINE_PORT" 5600)
                            :reality-url (or (env "REALITY_ENGINE_URL" nil)
                                             (format nil "http://localhost:~a" (env-int "REALITY_ENGINE_PORT" 5601)))
                            :localai-url (env "LOCAL_AI_API_URL" "http://localhost:4000")
                            :localai-machine-dir (env "LOCAL_AI_MACHINES_DIR" "../localAIStack/data/machines")
                            :dimension (env-int "VECTOR_DIMENSION" 7680)))
