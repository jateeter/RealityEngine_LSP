(in-package #:reality-engine-lsp)

(defstruct perception-state
  engine reality-url localai-url localai-machine-dir push-records started-at
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
   :mqtt-bridge nil))

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

(defun push-perception (state include-machine-results)
  (let* ((engine (perception-state-engine state))
         (vector (assemble-perception-vector engine))
         (payload (obj "vector" (vectorize vector)
                       "includeMachineResults" (json-bool include-machine-results)
                       "includePerceptualSpace" t)))
    (handler-case
        (let ((response (http-post-json (format nil "~a/api/perceive" (perception-state-reality-url state))
                                        payload)))
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
                                                    (let* ((sensor-id (or (jstring body "sensorId" nil)
                                                                         (jstring body "id" nil)
                                                                         "localai_agent_activity"))
                                                           (values (numbers-from-json (or (jget body "values")
                                                                                          (jget body "vector")
                                                                                          (arr))))
                                                           (engine (perception-state-engine state))
                                                           (source (sensor-exists-p engine sensor-id)))
                                                      (unless source
                                                        (setf source (ensure-source-id
                                                                      engine
                                                                      (make-source :id sensor-id
                                                                                   :kind "sensor"
                                                                                   :name sensor-id
                                                                                   :active-p t
                                                                                   :region (make-region :offset 0 :length (length values))
                                                                                   :sensor-id sensor-id))))
                                                      (setf (source-last-value source) values
                                                            (source-last-updated source) (now-ms))
                                                      (obj "success" t "sensorId" sensor-id "timestamp" (now-ms))))))))
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
