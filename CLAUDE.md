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

## Runtime snapshots (RealityEngine_CI#287)

`src/snapshot.lisp` keeps, for every retained step (`historyLimit`), the active list after it,
its contributions and its clock, whether or not K-line capture is on, and serves
`POST /api/runtime/snapshot`, `GET /api/runtime/snapshots`,
`GET`/`DELETE /api/runtime/snapshot/:id` and `POST /api/runtime/restore`
(SURFACE_SPEC "K-lines and runtime snapshots"). A body is `re-snapshot/1`, its id
the hex of its digest (JCS without the clocks' instance and lamport), so the same
state has the same id on every runtime. Restore re-establishes the controls, the
overrides and `activeAfter`; a corpus scope also replaces the history and the
step count. A restore is a reset for K-line capture. `RE_SNAPSHOT_STORE` is
`memory` (default), `file:<dir>` or `off` (503 on all five); `qdrant:` is phase 3
and off until then.

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
for what cl+ssl does not expose. `tests/tls-tests.lisp` pins it. On the device
routes a source named in `HEALTHKIT_CLIENT_CERTIFICATE_REQUIRED` must still
present a certificate (owner, 2026-11-12; `healthkit-certificate-refusal` in
`src/perception-service.lisp`, with `*tls-connection*` and `*tls-peer-verified*`).

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
- K-lines (RealityEngine_CI#603, #172) are captured in `src/kline.lisp` at the
  step completion point, after the commit tick and arbitration retention, by
  the same algorithm as RealityEngine_CPP `src/kline.cpp`: the tracker reads
  each committed step's sequence results and the machines' active events,
  never re-deriving the transition rule, and appends closed K-lines
  (`re-kline/1`) and `sustain-end` events to `<INSTANCE_KLINE_DIR>/<uuid>/`
  (default the clock dir's sibling `klines/`), fsynced before the completion
  is published, with `open.json` checkpointed. SHA-256 is libcrypto's
  `EVP_Digest` through CFFI (cl+ssl loads it); the fingerprint hashes each
  machine's `declared` form, copied at load before the loader writes into the
  parsed tree. The controls `klineCapture` (default `false` until the PE half
  lands; owner, 2026-11-26), `klineFeedbackHops` and `klineStoreMaxBytes` are
  on `/api/engine/config`, as on C++ and Scala; `RE_KLINE_CAPTURE=1` sets
  capture's initial value. Off leaves the store untouched; on closes what the
  checkpoint holds first (`kline-start-capture`). The routes are in
  `docs/openapi/reality-engine.yaml`. `tests/kline-tests.lisp` covers the boundaries
  and the store.
- The PE half (`src/isre.lisp`) records one `re-isre-composition/1` per
  completed push into `<INSTANCE_ISRE_DIR>/<pe_uuid>/` (the K-line store's
  format, keyed by the RE's commit), as RealityEngine_CPP `src/isre.cpp` does:
  each cell's winner from the push's own assembly (`perception-composition`
  over the writer array `assemble-perception-vector` returns), named under
  `re-source-kinds/2` from `<MACHINES_DIR>/../schemas/source-kinds/`. A source's
  causedBy is `*request-clock*`, bound per request in `dispatch-route` and
  carried into the actor by `actor-ask`. `isreCapture` (default `false`) and
  `isreStoreMaxBytes` are on `PATCH /api/config`; `RE_ISRE_CAPTURE=1` sets the
  initial value. Routes: `GET /api/isre-composition`, `/stats`, `POST /export`;
  `tests/isre-tests.lisp`.
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
