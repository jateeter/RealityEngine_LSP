#!/usr/bin/env bash
# =============================================================================
# startUniverse.sh (_LSP) — thin shim that delegates to RealityEngine_CI's
# contract-owned orchestrator with --re-engine=lsp --pe-engine=lsp pre-selected.
# All flags accepted by RealityEngine_CI/startUniverse.sh are forwarded.
# Falls back to ./start.sh when the CI orchestrator isn't available.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CI_ORCHESTRATOR="$SCRIPT_DIR/../RealityEngine_CI/startUniverse.sh"

if [ -x "$CI_ORCHESTRATOR" ]; then
  exec "$CI_ORCHESTRATOR" --re-engine=lsp --pe-engine=lsp "$@"
fi

echo "_CI orchestrator not found at $CI_ORCHESTRATOR — falling back to native start"
exec "$SCRIPT_DIR/start.sh" "$@"
