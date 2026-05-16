#!/usr/bin/env bash
# =============================================================================
# startUniverse.sh (_LSP) — thin shim that delegates to RealityEngine_AI's
# unified orchestrator with --re-engine=lsp --pe-engine=lsp pre-selected.
# All flags accepted by RealityEngine_AI/startUniverse.sh are forwarded.
# Falls back to ./start.sh when the AI orchestrator isn't available.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AI_ORCHESTRATOR="$SCRIPT_DIR/../RealityEngine_AI/startUniverse.sh"

if [ -x "$AI_ORCHESTRATOR" ]; then
  exec "$AI_ORCHESTRATOR" --re-engine=lsp --pe-engine=lsp "$@"
fi

echo "_AI orchestrator not found at $AI_ORCHESTRATOR — falling back to native start"
exec "$SCRIPT_DIR/start.sh" "$@"
