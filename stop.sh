#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

for pidfile in run/reality-engine.pid run/perception-engine.pid; do
  if [ -f "$pidfile" ]; then
    pid="$(cat "$pidfile")"
    if kill -0 "$pid" >/dev/null 2>&1; then
      kill "$pid"
    fi
    rm -f "$pidfile"
  fi
done

