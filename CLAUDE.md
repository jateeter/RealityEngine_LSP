# RealityEngine_LSP Guidance

Last reviewed: 2026-09-25

See `/Users/johnt/workspace/GitHub/CLAUDE.md` for the integrated application map. Update both this file and the root map when Lisp engine startup, API parity, PE behavior, or integration support changes.

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

## Building

**Builds are controlled through `RealityEngine_CI`, not from here.** This repo is
an independent git repository, not a subproject of CI or of any other engine.
Read the contract before building or deploying:

    RealityEngine_CI/docs/BUILD_CONTROL_CONTRACT.md

```bash
cd ../RealityEngine_CI && ./scripts/regression-test.sh --execute --build-only
```

Quicklisp is bootstrapped by the harness, not by the caller's environment:
`quicklisp/` is untracked, so a cold-start worktree never has it, and
`make build` dies with "Missing Quicklisp" without
`scripts/bootstrap-quicklisp.sh --home` having run first. The harness does this
itself so every lane is self-sufficient.

`make build` saves an image to `bin/reality-engine-lsp`, and `start.sh` launches
it when it is current (see Startup). The provenance gate
(`RealityEngine_CI/scripts/verify-build-provenance.py`) treats it as an
*optional* artifact: a source-mode run has none, but a stale image is refused,
because `LSP_LAUNCH_MODE=auto` would prefer it.

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

## Runtime Contract

- Keep RE/PE routes and payloads aligned with C++ and Scala.
- Treat JSON serialization, machine loading, and PE source state as parity-sensitive.
- Use the same ACP/OpenClaw environment defaults as the rest of the application.

## LSP Support

Use SLY/SLIME or Alive with SBCL and Quicklisp. Use markdown LSP for docs and JSON support where config files are present.

## Editing Rules

- Keep generated Quicklisp/runtime state out of commits unless explicitly requested.
- Verify source changes with `make test`; use e2e coverage for endpoint or integration behavior.

## Standing rules — authoritative in `../RealityEngine_CI/docs/ENGINEERING_CONTRACT.md`

These apply here and are **not** restated in this file. The table is an index
to the contract, not a copy of it: it names every rule so you know what to look
up, and the contract's wording governs wherever the two differ.

| Rule | In short |
| --- | --- |
| Qualify every "registry" | Never the bare word — instance / machine / cesgen / arbitration / domain / semantic-bus / tag. |
| Regenerate a stale `<name>` registry, don't fail it | Each `<name>` registry is a view of the running system. A gate regenerates it and fails only on a disagreement that survives regeneration. |
| Verify a merge beyond the hosted checks | A green PR is not a verified PR; the hosted path cannot reach the integration points. Name what you could not exercise, and record what you noticed but did not chase. |
| _CI is the authority | Peripheral repos keep minimal CI that forces local validation; RealityEngine_CI verifies fixes against a live universe. Check its `docs/` before adding CI anywhere else. |
| Name it `CLAUDE.md` | Uppercase, always. On a case-insensitive filesystem `claude.md` is the same inode; dedupe on `st_ino`, never on a resolved path. |
| Never commit to main | Branch from `origin/main`, PR, verify, squash-merge, clean up. |
| Use bash, not zsh | Shell work runs in `/opt/homebrew/bin/bash` (5.x), not zsh or macOS `/bin/bash` 3.2: any loop, unquoted variable, glob or `set --` goes through it with `set -euo pipefail`, and you check the command's exit status, not the pipeline tail. |

Read the contract for the full text, the qualifier table, and the cleanup steps.
