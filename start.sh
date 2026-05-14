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

REALITY_ENGINE_PORT="${REALITY_ENGINE_PORT:-3299}"
PERCEPTION_ENGINE_PORT="${PERCEPTION_ENGINE_PORT:-3300}"
VECTOR_DIMENSION="${VECTOR_DIMENSION:-768}"
MACHINES_DIR="${MACHINES_DIR:-../RealityEngine_AI/examples/machines}"
LOCAL_AI_API_URL="${LOCAL_AI_API_URL:-http://localhost:8000}"
LOCAL_AI_MACHINES_DIR="${LOCAL_AI_MACHINES_DIR:-../localAIStack/data/machines}"

mkdir -p logs run

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

export REALITY_ENGINE_PORT PERCEPTION_ENGINE_PORT VECTOR_DIMENSION MACHINES_DIR
export LOCAL_AI_API_URL LOCAL_AI_MACHINES_DIR

sbcl --noinform --load "$QUICKLISP_SETUP" \
  --eval "(pushnew (truename \".\") ql:*local-project-directories*)" \
  --eval "(ql:register-local-projects)" \
  --eval "(ql:quickload :reality-engine-lsp :force t)" \
  --eval "(reality-engine-lsp:start-reality-from-environment)" \
  --eval "(loop (sleep 3600))" > logs/reality-engine.log 2>&1 &
echo "$!" > run/reality-engine.pid

sbcl --noinform --load "$QUICKLISP_SETUP" \
  --eval "(pushnew (truename \".\") ql:*local-project-directories*)" \
  --eval "(ql:register-local-projects)" \
  --eval "(ql:quickload :reality-engine-lsp :force t)" \
  --eval "(reality-engine-lsp:start-perception-from-environment)" \
  --eval "(loop (sleep 3600))" > logs/perception-engine.log 2>&1 &
echo "$!" > run/perception-engine.pid

echo "Reality Engine LSP     : http://localhost:${REALITY_ENGINE_PORT}"
echo "Perception Engine LSP  : http://localhost:${PERCEPTION_ENGINE_PORT}"
echo "Machines               : ${MACHINES_DIR}"
echo "Vector dimension       : ${VECTOR_DIMENSION}"
