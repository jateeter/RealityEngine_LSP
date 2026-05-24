#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

# Accept --instance=<id> to stop a specific named instance
INSTANCE_ID="${INSTANCE_ID:-}"
for _arg in "$@"; do
  case "$_arg" in --instance=*) INSTANCE_ID="${_arg#--instance=}" ;; esac
done
_INST="${INSTANCE_ID:+-${INSTANCE_ID}}"

for pidfile in "run/reality-engine${_INST}.pid" "run/perception-engine${_INST}.pid"; do
  if [ -f "$pidfile" ]; then
    pid="$(cat "$pidfile")"
    if kill -0 "$pid" >/dev/null 2>&1; then
      kill "$pid"
    fi
    rm -f "$pidfile"
  fi
done

