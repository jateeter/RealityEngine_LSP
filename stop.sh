#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; NC='\033[0m'
info() { echo -e "${YELLOW}i${NC} $1"; }
ok()   { echo -e "${GREEN}✓${NC} $1"; }
warn() { echo -e "${RED}!${NC} $1"; }

# Accept --instance=<id> to stop a specific named instance
INSTANCE_ID="${INSTANCE_ID:-}"
for _arg in "$@"; do
  case "$_arg" in --instance=*) INSTANCE_ID="${_arg#--instance=}" ;; esac
done
_INST="${INSTANCE_ID:+-${INSTANCE_ID}}"

# SBCL does not exit on SIGTERM (it enters the debugger).  Always escalate to
# SIGKILL after a short wait so the port is reliably freed.
stop_pid_file() {
  local label="$1" pid_file="$2"

  if [ ! -f "$pid_file" ]; then
    info "$label not running (no $(basename "$pid_file"))"
    return 0
  fi

  local pid
  pid="$(cat "$pid_file" 2>/dev/null || true)"
  if [ -z "$pid" ]; then
    rm -f "$pid_file"
    info "$label had empty PID file; removed"
    return 0
  fi

  if ! kill -0 "$pid" 2>/dev/null; then
    rm -f "$pid_file"
    info "$label was not running; removed stale PID file"
    return 0
  fi

  info "Stopping $label (PID $pid)..."
  kill -TERM "$pid" 2>/dev/null || true

  local waited=0
  while kill -0 "$pid" 2>/dev/null && [ "$waited" -lt 8 ]; do
    sleep 1; waited=$((waited + 1))
  done

  if kill -0 "$pid" 2>/dev/null; then
    warn "$label (PID $pid) did not exit after ${waited}s — sending SIGKILL"
    kill -KILL "$pid" 2>/dev/null || true
    sleep 0.5
  fi

  rm -f "$pid_file"
  ok "$label stopped"
}

echo "=================================================="
echo "RealityEngine_LSP - Graceful Shutdown"
echo "=================================================="
echo ""

stop_pid_file "Perception Engine" "run/perception-engine${_INST}.pid"
stop_pid_file "Reality Engine"    "run/reality-engine${_INST}.pid"

echo ""
echo "Qdrant is shared with localAIStack and was not stopped."
echo "Stop with: cd ../localAIStack && ./scripts/stop.sh"
