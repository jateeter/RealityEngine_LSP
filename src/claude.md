# RealityEngine_LSP Source Guidance

This directory contains the Common Lisp RE/PE implementation.

- Keep route behavior, JSON serialization, machine loading, and PE state aligned with C++ and Scala.
- Use SBCL/Quicklisp and Lisp-aware editor support.
- Check `reality-service.lisp`, `perception-service.lisp`, `loader.lisp`, and `json.lisp` together for parity issues.

## PE reset semantics

`reset-perception-engine` (`perception.lisp`) resets run state **in place** and
is what `POST /api/reset` and the MCP `perception_reset` tool both call. Do not
reset by rebuilding the engine struct: `make-perception-engine-state` allocates a
fresh `sources` table, which discards every registered source and also clears the
match algorithm and auto-push interval. That divergence made this runtime
contribute all-zero vectors after a reset while C++ and Scala kept replaying
their sequences (#55).

Reset means "start this run again", not "forget what is connected":

- Reset **keeps** sources; it zeroes `global-step`, `last-push`, the persistent
  vector and the `cursor` of **test sources only** — matching
  `PerceptionEngine::reset` (C++) and `PerceptionEngine.reset()` (Scala). Only
  the `"test"` branches of `sample-source` and `advance-perception-engine` read
  or advance `cursor`, so zeroing it on sensor and simulated sources wrote a
  field that is not theirs (#64).
- Reset **validates** activity, it does not assign it (RealityEngine_CI#163
  point 3). `source-validated-active-p` recomputes each flag from the source's
  own state — sensor: holds a value inside its TTL (`sensor-stale-p`); test:
  interned sequence is non-empty; simulated: always. The prior flag is never
  read back, so an operator pause does not survive a reset (#65).
- Reset is **membership-neutral** (contract point 4): it never creates a source
  and never removes one, and must never re-derive from boot config or the
  corpus — that would drop every integration registered since boot.

  Note this is about *reset*, not about *boot*. An earlier revision of this file
  claimed LSP declaring 0 sources under `--pe-source-bootstrap=off` was the
  behaviour the other runtimes should be corrected toward. That was wrong in two
  ways: LSP was declaring 0 sources under **every** setting, because it had no
  boot intern path at all; and the correct default is to intern, not to skip.
  See the section below.
- Reset **leaves** dimension, match algorithm and auto-push settings alone.
  Those are configuration, not run state.
- Removing a source stays a separate operation: `DELETE /api/sources/:id` and the
  corpus path that drops a machine's source when the machine leaves the dynamic
  corpus. Those are the intended way to forget a source.

## Machine ingestion

**Interning a machine's test source is part of ingesting the machine**, and it
happens by default. `start-perception-service` interns the corpus test sources
at boot — a machine's `inputSequences` become a test source over that machine's
own input region — unless `PE_SOURCE_BOOTSTRAP` is `off`/`0`/`false`/`no`.

Those sources are the material the **ISRE seed queue** is composed from: the
seed at step n is the merge of every active test source's n-th vector, each
written into its own machine's region. A runtime holding a corpus but no test
sources has nothing to be presented with, and a parity comparison against it
measures a synthetic stimulus rather than the corpus's own.

This runtime previously had *no* boot intern path and depended on the launcher
calling `POST /api/sources/bootstrap-from-machines` (#68). That POST remains the
dynamic registration path and is unaffected by the flag.

The intern is fire-and-forget through the actor: the RE may still be coming up
when the PE binds, and the machine catalog refresher already retries. A failure
is logged and deferred rather than taking the service down.

Machine-derived test sources are the one source kind that does not wait for an
external integration to register — they arrive with the machines. MQTT, ACP,
MCP, HealthKit and localAI are external and register on their own terms.

Master contract: `RealityEngine_CI/SURFACE_SPEC.md`, "Machine ingestion".

## PE source activity

Activity is **earned by a value, never granted at registration** (contract
2a/2b). Registration declares an integration source completely and INACTIVE;
`record-sensor-value` is the only thing that may originate activity, and every
sensor ingress path funnels through it — `POST /api/sensors/:sensorId`, the
signal/HealthKit/CareKit commit path, MQTT ingest, and both MCP push tools.

Two invariants worth not breaking:

- A source that never received a value reports inactive at **every** observation
  point. `source-json` reports `(stored flag AND validated)`, so this holds even
  when a caller registers with `"active": true`.
- If reset can validate a sensor inactive, ingress **must** re-activate on value
  arrival. `sample-source` gates on the stored flag, so without that a fresh
  reading lands on an inactive source and contributes zeros forever. This is the
  bug that bit the TypeScript PE.

Note: this runtime emits `ageMs` and `stale` on sensor payloads and C++/Scala do
not. Kept deliberately — removing them from a byte-compared payload is a
cross-runtime call (#65).

