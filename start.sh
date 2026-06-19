#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

if [ -f .env ]; then
  set -a
  # shellcheck disable=SC1091
  source .env
  set +a
fi

REALITY_ENGINE_PORT="${REALITY_ENGINE_PORT:-5601}"
PERCEPTION_ENGINE_PORT="${PERCEPTION_ENGINE_PORT:-5600}"
VECTOR_DIMENSION="${VECTOR_DIMENSION:-7680}"
MACHINES_DIR="${MACHINES_DIR:-../RealityEngine_Machines/machines}"
RE_LOAD_MACHINES="${RE_LOAD_MACHINES:-1}"
LOCAL_AI_API_URL="${LOCAL_AI_API_URL:-http://localhost:4000}"
LOCAL_AI_MACHINES_DIR="${LOCAL_AI_MACHINES_DIR:-../localAIStack/data/machines}"
QDRANT_URL="${QDRANT_URL:-http://localhost:4333}"
# MQTT bridge env passthrough — currently consumed by the mapping registry
# only (the LSP MQTT client driver is deferred per the rollout plan).  Once
# the driver lands these are picked up unchanged.
MQTT_BROKER_HOST="${MQTT_BROKER_HOST:-}"
MQTT_BROKER_PORT="${MQTT_BROKER_PORT:-1883}"
MQTT_CLIENT_ID="${MQTT_CLIENT_ID:-reality-engine-pe-lsp}"
MQTT_MAPPINGS_FILE="${MQTT_MAPPINGS_FILE:-}"
_DEFAULT_INTEGRATIONS_CONFIG="../RealityEngine_CI/config/integrations.json"
if [ -z "${INTEGRATIONS_CONFIG:-}" ] && [ -f "$_DEFAULT_INTEGRATIONS_CONFIG" ]; then
  INTEGRATIONS_CONFIG="$_DEFAULT_INTEGRATIONS_CONFIG"
else
  INTEGRATIONS_CONFIG="${INTEGRATIONS_CONFIG:-}"
fi
ACP_ENABLED="${ACP_ENABLED:-true}"
ACP_PLATFORM="${ACP_PLATFORM:-OpenClaw}"
ACP_SURFACE="${ACP_SURFACE:-xACP}"
ACP_GATEWAY_URL="${ACP_GATEWAY_URL:-${OPENCLAW_GATEWAY_URL:-ws://127.0.0.1:18789}}"
OPENCLAW_GATEWAY_URL="${OPENCLAW_GATEWAY_URL:-$ACP_GATEWAY_URL}"
ACP_SESSION_KEY="${ACP_SESSION_KEY:-${OPENCLAW_ACP_SESSION:-agent:main:main}}"
OPENCLAW_ACP_SESSION="${OPENCLAW_ACP_SESSION:-$ACP_SESSION_KEY}"
ACP_TARGET_AGENT="${ACP_TARGET_AGENT:-openclaw}"
ACP_COMPLETION_SOURCE_MAPPING_ID="${ACP_COMPLETION_SOURCE_MAPPING_ID:-acp-openclaw-completion}"
# INSTANCE_ID — when set, PID files are suffixed so multiple LSP instances
# can run from the same repo directory simultaneously.
INSTANCE_ID="${INSTANCE_ID:-}"
_INST="${INSTANCE_ID:+-${INSTANCE_ID}}"   # "" or "-<id>"

mkdir -p logs run

# A 0-byte system-index.txt left by a SIGKILL'd SBCL causes ql:register-local-projects
# to fail with a RENAME-AND-DELETE error that enters the SBCL interactive debugger.
# Delete it pre-emptively so Quicklisp regenerates it cleanly.
_SIDX="${ROOT_DIR}/quicklisp/local-projects/system-index.txt"
if [ -f "$_SIDX" ] && [ ! -s "$_SIDX" ]; then
  rm -f "$_SIDX"
fi

if ! command -v sbcl >/dev/null 2>&1; then
  echo "Missing SBCL. Install SBCL and Quicklisp before starting RealityEngine_LSP." >&2
  exit 1
fi

QUICKLISP_SETUP="${QUICKLISP_SETUP:-${ROOT_DIR}/quicklisp/setup.lisp}"
if [ ! -f "$QUICKLISP_SETUP" ]; then
  QUICKLISP_SETUP="${HOME}/quicklisp/setup.lisp"
fi

if [ ! -f "$QUICKLISP_SETUP" ]; then
  echo "Missing Quicklisp. Expected ${ROOT_DIR}/quicklisp/setup.lisp or ${HOME}/quicklisp/setup.lisp." >&2
  exit 1
fi

# When RE_LOAD_MACHINES=0 the RE starts with no corpus (CI will seed separately).
# Use a temp empty directory so the runtime still receives a valid path.
if [ "${RE_LOAD_MACHINES}" = "0" ]; then
    _machines_load_dir=$(mktemp -d)
    trap 'rm -rf "$_machines_load_dir"' EXIT
    MACHINES_DIR="$_machines_load_dir"
fi

export REALITY_ENGINE_PORT PERCEPTION_ENGINE_PORT VECTOR_DIMENSION MACHINES_DIR
export LOCAL_AI_API_URL LOCAL_AI_MACHINES_DIR QDRANT_URL
export MQTT_BROKER_HOST MQTT_BROKER_PORT MQTT_CLIENT_ID MQTT_MAPPINGS_FILE
export INTEGRATIONS_CONFIG
export ACP_ENABLED ACP_PLATFORM ACP_SURFACE ACP_GATEWAY_URL OPENCLAW_GATEWAY_URL
export ACP_SESSION_KEY OPENCLAW_ACP_SESSION ACP_TARGET_AGENT ACP_COMPLETION_SOURCE_MAPPING_ID

sbcl --dynamic-space-size 2048 --noinform --disable-debugger --load "$QUICKLISP_SETUP" \
  --eval "(pushnew (truename \".\") ql:*local-project-directories*)" \
  --eval "(handler-case (ql:register-local-projects) (error (c) (format t \"~&Warning: register-local-projects: ~a~%\" c)))" \
  --eval "(ql:quickload :reality-engine-lsp :force t)" \
  --eval "(reality-engine-lsp:start-reality-from-environment)" \
  --eval "(loop (sleep 3600))" > "logs/reality-engine${_INST}.log" 2>&1 &
echo "$!" > "run/reality-engine${_INST}.pid"

sbcl --dynamic-space-size 2048 --noinform --disable-debugger --load "$QUICKLISP_SETUP" \
  --eval "(pushnew (truename \".\") ql:*local-project-directories*)" \
  --eval "(handler-case (ql:register-local-projects) (error (c) (format t \"~&Warning: register-local-projects: ~a~%\" c)))" \
  --eval "(ql:quickload :reality-engine-lsp :force t)" \
  --eval "(reality-engine-lsp:start-perception-from-environment)" \
  --eval "(loop (sleep 3600))" > "logs/perception-engine${_INST}.log" 2>&1 &
echo "$!" > "run/perception-engine${_INST}.pid"

echo "Reality Engine LSP     : http://localhost:${REALITY_ENGINE_PORT}${INSTANCE_ID:+ [instance: $INSTANCE_ID]}"
echo "Perception Engine LSP  : http://localhost:${PERCEPTION_ENGINE_PORT}${INSTANCE_ID:+ [instance: $INSTANCE_ID]}"
echo "Machines               : ${MACHINES_DIR}"
echo "Vector dimension       : ${VECTOR_DIMENSION}"
echo "Qdrant                 : ${QDRANT_URL}"
[ -n "$MQTT_BROKER_HOST" ] && \
  echo "MQTT broker (mapping)  : ${MQTT_BROKER_HOST}:${MQTT_BROKER_PORT}"
[ -n "$MQTT_MAPPINGS_FILE" ] && \
  echo "MQTT mappings          : ${MQTT_MAPPINGS_FILE}"
[ -n "$INTEGRATIONS_CONFIG" ] && \
  echo "Integrations config    : ${INTEGRATIONS_CONFIG}"
echo "OpenClaw ACP          : enabled=${ACP_ENABLED} gateway=${ACP_GATEWAY_URL}"
echo "OpenClaw mapping      : ${ACP_COMPLETION_SOURCE_MAPPING_ID}"
