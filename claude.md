# RealityEngine_LSP Guidance

Last reviewed: 2026-06-22

See `/Users/johnt/workspace/GitHub/claude.md` for the integrated application map. Update both this file and the root map when Lisp engine startup, API parity, PE behavior, or integration support changes.

## Role

This repo contains the Common Lisp Reality Engine and Perception Engine implementations. It participates as `lsp-1` in multi-engine runs and is a parity target against C++ and Scala.

## Codebase Map

- `src/main.lisp`, `src/cli.lisp`: entrypoints.
- `src/reality-service.lisp`: RE HTTP/API surface.
- `src/perception-service.lisp`, `src/perception.lisp`: PE behavior.
- `src/loader.lisp`: machine loading.
- `src/model.lisp`, `src/actor.lisp`: runtime model and actor behavior.
- `src/http.lisp`, `src/ws.lisp`, `src/json.lisp`: transport and serialization.
- `src/mqtt-bridge.lisp`, `src/mqtt-client.lisp`, `src/mqtt-mapping.lisp`: MQTT integration.
- `src/mcp.lisp`: MCP support.
- `src/vector-aggregator.lisp`: PE/vector aggregation behavior.
- `tests/`: Lisp unit and e2e coverage.
- `docs/`: docs and API references.
- `quicklisp/`: local dependency environment.

## Key Commands

```bash
make build
make test
make e2e-healthkit-spezi
```

## Startup

`start.sh` selects a launch path via `LSP_LAUNCH_MODE=binary|source|auto`
(default `auto`):

- **binary** — run `bin/reality-engine-lsp <mode>`, built by `make build`. Its
  `main` dispatches `reality` / `perception` and supplies the idle loop.
- **source** — load the system through Quicklisp at launch (the historical path).
- **auto** — binary when it is present, executable and no older than any file
  under `src/` or the `.asd`; otherwise source.

`bin/` is gitignored, so a fresh clone takes the source fallback until someone
runs `make build`. The banner reports which path was taken.

`:force t` belongs to `build` and `test`, where a clean compile is the intent.
It must not appear on a service launch path — there it is a latent full-system
rebuild, three times per launch, the first time a runner comes up with a cold
`~/.cache/common-lisp` (#62).

Note for CI: LSP now has a launched artifact when the binary path is taken, so
`RealityEngine_CI/scripts/verify-build-provenance.py` — which records LSP as
"runs from source" — may need its expectation widened to accept either path.

## Runtime Contract

- Keep RE/PE routes and payloads aligned with C++ and Scala.
- Treat JSON serialization, machine loading, and PE source state as parity-sensitive.
- Use the same ACP/OpenClaw environment defaults as the rest of the application.

## LSP Support

Use SLY/SLIME or Alive with SBCL and Quicklisp. Use markdown LSP for docs and JSON support where config files are present.

## Editing Rules

- Keep generated Quicklisp/runtime state out of commits unless explicitly requested.
- Verify source changes with `make test`; use e2e coverage for endpoint or integration behavior.
