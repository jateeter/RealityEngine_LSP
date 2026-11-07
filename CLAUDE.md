# RealityEngine_LSP Guidance

Last reviewed: 2026-11-04

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

## Lane TLS

With `RE_TLS_CERT` set (RealityEngine_CI#668, this repo's #186), the RE and PE
serve TLS 1.3 with mTLS on their own ports, and call internal peers over `https`
with the lane CA and their own certificate. With it unset, nothing changes.
The contract, including which peers are internal and which PE routes are open
to devices, is `RealityEngine_CI/docs/TRANSPORT_SECURITY.md`, and C++ and
Scala implement the same choices. The code is `src/tls.lisp`, a Hunchentoot
acceptor and a pooled Drakma stream over cl+ssl, with OpenSSL called directly
for what cl+ssl does not expose. `tests/tls-tests.lisp` pins it.

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
- A step composes in parallel and resolves atomically (RealityEngine_CI#375):
  `compose-machines` runs every machine's composition from ISRE(n) through
  `lparallel` (`pmap-machines` is the join); folding, arbitration and the OSRE(n)
  commit follow in the actor, in canonical order. The committed (ISRE, OSRE) pair
  is published on a `bordeaux-threads` condition variable (`step-signal`), which
  `GET /api/engine/steps/:n/pair?timeoutMs=` waits on from the request thread,
  never the actor. Steps are numbered from 0.
- Arbitration retention (RealityEngine_CI#296): `arbitrationRetention` (default
  `false`, the legacy escape) and `arbitrationWindow` (default 1, max 1024) on
  `/api/engine/config`. On, each step's records are retained under its step
  number in `arbitration-steps` before the completion point is signalled, and
  `GET /api/arbitration` answers a list (`?step=N` reads one), each element
  stamped with its clock `{instance, lamport, step}`. `lamport` ticks once
  per committed step and is never reset, so `(instance, lamport)` is unique;
  `step` is the step count, which a reset restarts. A UUID belongs to an
  **instance**, never an engine type or image: the instance registry allocates
  it (`INSTANCE_UUID`), and without one the instance mints a v7 UUID at boot
  (never at image-save time). An allocated instance's clock survives restarts:
  `INSTANCE_CLOCK_DIR` (default `~/.reality-engine/clock/`) holds
  `<uuid>.lamport`, a high-water mark reserved 1024 ticks ahead and written
  (temp + rename) before any tick past it is issued, and `<uuid>.lock`, held
  with `lockf` for the life of the process so a second live process with the
  same UUID refuses to boot. An unreadable or unwritable clock refuses the boot
  too. `GET /api/engine/clock` reports the clock. Off, the legacy object is
  unchanged except that a reset clears it.
- Use the same ACP/OpenClaw environment defaults as the rest of the application.

## LSP Support

Use SLY/SLIME or Alive with SBCL and Quicklisp. Use markdown LSP for docs and JSON support where config files are present.

## Editing Rules

- Keep generated Quicklisp/runtime state out of commits unless explicitly requested.
- Verify source changes with `make test`; use e2e coverage for endpoint or integration behavior.

LSP startup waits for RE health before launching PE in both binary and source modes, so boot source interning observes the loaded corpus.

## Standing rules

The standing rules are in
`../RealityEngine_CI/docs/ENGINEERING_CONTRACT.md`, which is
authoritative. They are not listed or summarised here, so this file never
falls behind the contract. Read it before changing anything here.
