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

Governed by the canonical contract, which lives in `RealityEngine_CI` and
nowhere else:

    RealityEngine_CI/SURFACE_SPEC.md  §  Machine ingestion

Do not restate it here. It defines what ingesting a machine interns, how
`PE_SOURCE_BOOTSTRAP` gates it, and how those sources compose `ISRESeed(n)` —
and it governs this repository's implementation of all three.

Implemented in `start-perception-service` (`perception-service.lisp`), which
interns at boot through the actor. Fire-and-forget: the RE may still be coming
up when the PE binds, and the catalog refresher retries (#68).

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

## MUST: every use of the word "registry" carries a qualifier

**The word "registry" MUST NEVER appear unqualified. Every single use of the
word takes a qualifier naming which registry is meant.**

This is a hard requirement, not a style preference. It applies to every
occurrence in every context, with no exceptions: prose, end-of-task summaries,
commit messages, PR bodies, issue titles and bodies, code comments, docstrings,
variable and function names, log lines, and documentation.

Wrong, in every case — these are all violations:

- "the registry"
- "a versioned registry"
- "the registry file" / "update the registry" / "registry-backed"
- "check the registry first"
- "registry drift"

Right — a qualifier every time:

- "the **instance** registry"
- "a versioned **cesgen** registry"
- "the **arbitration** registry"
- "**machine** registry drift"

If you type the word "registry" and the word immediately before it is not a
qualifier, stop and add one. Re-read every summary and every message for the
bare word before sending it — that is where this rule is actually broken, because
the surrounding context makes the referent feel obvious in the moment. That
feeling is exactly the assumption the rule exists to block.

Qualifiers currently in use. **This list is open, not exhaustive** — a registry
added later gets a qualifier too; nothing is ever promoted to being "the
registry" by virtue of being the one under discussion:

- **instance** registry — `/tmp/re-registry/re-registry.json`, served at
  `:5999/re-registry.json`. Running RE/PE instances with `re_url`/`pe_url`/ports,
  plus `services` and `allocation`. What `RE_REGISTRY_URL` points at.
- **machine** registry — the machines a runtime holds in memory, reported by
  `GET /api/machines`. Distinct from `GET /api/machines/json/list`, the on-disk
  corpus catalog.
- **cesgen** registry — `RealityEngine_Machines/domains/ces-contract-registry.json`.
  Which CES output-stream contract shards exist, what corpus each was recorded
  against, whether each is current.
- **arbitration** registry — `machines/domains/arbitration-registry.json`.
- **domain** registry — `machines/domains/domain-registry.json`.
- **semantic-bus** registry — `machines/domains/semantic-bus-registry.json`.
- **tag** registry — `RealityEngine_CI/docs/TAG_REGISTRY.md`.

## MUST: verify a merge beyond the hosted checks

**A green PR is not a verified PR. Never merge on the hosted checks alone.**

The hosted path does not exercise this system's integration points. A PR can show
every check green and still be unverified, because the checks that ran were a
security scan and — at most — a corpus gate. `localAIStack`, `localOpenClawStack`,
Ollama, Qdrant, MQTT, the OpenClaw ACP gateway and the multi-engine universe are
**not** reachable from the hosted runners, so nothing on that path can tell you
whether the change works where it has to work.

Observed repeatedly: RealityEngine_Machines PRs report exactly one check
(GitGuardian). That is not evidence about the corpus, the registries, the
engines, or any bridge.

Before merging, verify **locally**, and say in the PR which of these you ran and
what they returned:

- The repo's own gates — `validate-corpus.sh`, the contract suite,
  `npm test`, `make test`, `sbt test` — whichever the change touches.
- The integration points the change can reach: a live 3-of-3 universe, the
  local AI stack, the OpenClaw gateway, MQTT — whichever the change can affect.
- The specific behaviour the change claims, with the numbers it produced.

If an integration point cannot be exercised, **say so in the PR** and name it.
An unverified area that is named is a known gap; an unverified area that is
silent reads as tested.

A hosted green tells you the change did not break the hosted path. That is worth
having and is not the question being asked at merge time.
