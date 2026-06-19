# RealityEngine_LSP

Common Lisp implementation of the Reality Engine and Perception Engine
service surface.  Black-box equivalent to [`RealityEngine_AI`](../RealityEngine_AI)
(TypeScript, default) and [`RealityEngine_CPP`](../RealityEngine_CPP) (native
C++) on the same machine JSON corpus, governance contracts, MQTT mapping
registry, and Prometheus metrics shape.

This implementation uses Common Lisp actors to mirror the actor mental
model: state is owned by mailbox-driven service actors, messages are
processed serially per actor, `tell` is fire-and-forget, `ask` returns a
future-like reply, and supervised actors can be restarted.

## Integrated Specification

Cross-repository deployment rules are owned by
[`RealityEngine_CI/DEPLOYMENT_CONTRACT.md`](../RealityEngine_CI/DEPLOYMENT_CONTRACT.md)
and [`RealityEngine_CI/INTEGRATED_SPECIFICATION.md`](../RealityEngine_CI/INTEGRATED_SPECIFICATION.md).
The active machine and RE/PE operations contract is described in
[`RealityEngine_Machines/docs/REALITY_PERCEPTION_OPERATIONS.md`](../RealityEngine_Machines/docs/REALITY_PERCEPTION_OPERATIONS.md).

## Requirements

- SBCL
- Quicklisp

The ASDF system depends on:

- `alexandria`
- `babel`             — UTF-8 octets ↔ strings for the MQTT client
- `bordeaux-threads`  — actor mailbox + MQTT I/O thread
- `hunchentoot`       — HTTP server
- `drakma`            — outbound HTTP for PE → RE pushes
- `usocket`           — TCP socket for the MQTT v3.1.1 client
- `yason`             — JSON parsing / serialization

## Build And Test

```bash
make deps-check
make test
```

## Run

The simplest entry point is the unified orchestrator from `RealityEngine_CI`:

```bash
# Either route works — both delegate the same way
./startUniverse.sh --re-engine=lsp --pe-engine=lsp   # from this repo (shim)
                                                      # or from RealityEngine_CI directly
./stopUniverse.sh                                    # tears the LSP engines down
```

Or run the native services directly:

```bash
cp .env.example .env
./start.sh
./stop.sh
```

Defaults match the sibling implementations:

- Reality Engine: `http://localhost:5601`
- Perception Engine: `http://localhost:5600`
- Machine directory: `../RealityEngine_Machines/machines`
- Vector dimension: `7680` (configurable floor; grows elastically to span all machine mappings)

## MQTT Integration

The PE includes a hand-rolled MQTT v3.1.1 client (`src/mqtt-client.lisp`)
running on a `bordeaux-threads` I/O thread + the same mapping registry
schema shared by AI and CPP.  Topics describe the outside world; the
registry alone encodes RE offsets — the design rule honoured uniformly
across all three runtimes.

```bash
# Boot with MQTT enabled
MQTT_BROKER_HOST=broker.example.com \
MQTT_BROKER_PORT=1883 \
MQTT_MAPPINGS_FILE=$PWD/../RealityEngine_CPP/config/mqtt-mappings.yuma-agriculture.json \
./start.sh
```

Endpoints exposed by the PE:

- `GET /api/mqtt/status` — connection state + bridge counters + broker config
- `GET /api/mqtt/mappings` — loaded registry + per-mapping counters
- `PUT /api/mqtt/mappings` — replace registry + reload bridge (validates
  schema + overlap warnings)

Mapping registry schema fields per rule: `id`, `topicFilter` (with `+` / `#`
wildcards), `sensorIdTemplate` (`{1}`, `{2}` captures), `region { offset,
length }`, `extract { type: csv-float | json | raw | single-float,
pointer?, index? }`, `normalize { mode: passthrough | minmax | linear |
band, min, max, scale, offset, clamp }`, `ttlMs`, `qos`, `acceptRetained`,
`pushMode: debounced | manual | immediate`, `debounceMs`.

The bridge fans out: one PUBLISH on a topic shared by N rules drives every
matching rule (e.g. a five-field sensor payload extracted by five
JSON-pointer rules feeds five PE sensor sources from one broker message).

## API Coverage

Implemented endpoints intentionally mirror the AI and CPP services:

- Reality service root, health, config, runtime options, metrics, vector search
- Sequence CRUD and transition reset
- Machine load/list/get/process/what-if/import/export
- Universal perceptual processing and `/api/perceive`
- Perceptual simulation state/history/step/reset
- Perception service health/state/source CRUD/sensor update/push/auto controls
- localAI bridge status/catalog/invoke/bootstrap/signal endpoints
- ACP/OpenClaw xACP status and no-wait dispatch handoff endpoints
- **MQTT bridge** — `/api/mqtt/status`, `/api/mqtt/mappings` (GET + PUT)
- **Cross-runtime Prometheus** — `/api/metrics` text exposition stamped
  with `runtime="lsp"` on every line

See [MACHINE_CONCEPT.md](MACHINE_CONCEPT.md) for the canonical machine model (DFA theory, JSON schema, perceptual mapping, regex equivalences, STA).

See [docs/INTEGRATION_ARCHITECTURE.md](docs/INTEGRATION_ARCHITECTURE.md), [docs/API_EQUIVALENCE.md](docs/API_EQUIVALENCE.md),
[docs/CONTEXT_EQUIVALENCE.md](docs/CONTEXT_EQUIVALENCE.md),
[docs/CONFIGURATION_EQUIVALENCE.md](docs/CONFIGURATION_EQUIVALENCE.md), and
[docs/HEALTHKIT_SPEZI_BRIDGE.md](docs/HEALTHKIT_SPEZI_BRIDGE.md).

## Configuration

Canonical env vars (set in `.env`, on the CLI, or via `startUniverse.sh`
flags):

| Variable | Default | Purpose |
|---|---|---|
| `REALITY_ENGINE_PORT` | `5601` | RE bind port |
| `PERCEPTION_ENGINE_PORT` | `5600` | PE bind port |
| `VECTOR_DIMENSION` | `7680` | Perceptual-space dimension floor |
| `MACHINES_DIR` | `../RealityEngine_Machines/machines` | Source of startup machines |
| `LOCAL_AI_API_URL` | `http://localhost:4000` | localAIStack integration target |
| `INTEGRATIONS_CONFIG` | `config/integrations.json` when present | Provider-neutral startup registry for source mappings |
| `OLLAMA_BASE_URL` | `http://localhost:11434` | Local Ollama adapter base URL |
| `OLLAMA_MODEL` | `gpt-oss:20b` | Default Ollama model for PE-controlled dispatch |
| `OLLAMA_COMPLETION_SOURCE_MAPPING_ID` | `agent-completion-risk` | Default source mapping for Ollama completion commits |
| `OPENAI_API_KEY` | unset | API key for caller-driven OpenAI Responses dispatch |
| `OPENAI_BASE_URL` | `https://api.openai.com/v1` | OpenAI-compatible Responses API base URL |
| `OPENAI_MODEL` | `gpt-5` | Default OpenAI model for PE-controlled dispatch |
| `OPENAI_COMPLETION_SOURCE_MAPPING_ID` | `agent-completion-risk` | Default source mapping for OpenAI completion commits |
| `ACP_ENABLED` | `true` | Enable ACP/OpenClaw adapter metadata in PE status |
| `ACP_COMMAND` / `OPENCLAW_ACP_COMMAND` | `openclaw acp` | External OpenClaw ACP command recorded in no-wait handoff receipts |
| `ACP_GATEWAY_URL` / `OPENCLAW_GATEWAY_URL` | `ws://127.0.0.1:18789` | OpenClaw Gateway URL recorded for xACP handoff |
| `ACP_SESSION_KEY` / `OPENCLAW_ACP_SESSION` | `agent:main:main` | OpenClaw Gateway session key for example xACP handoff |
| `ACP_COMPLETION_SOURCE_MAPPING_ID` | `acp-openclaw-completion` | Default source mapping for ACP/OpenClaw completion commits |
| `HEALTHKIT_BRIDGE_ID` | `healthkit-ios-bridge` | Expected Apple-platform HealthKit bridge identity |
| `HEALTHKIT_DEFAULT_SOURCE_MAPPING_ID` | `healthkit-activity` | Default source mapping for HealthKit bridge ingest |
| `HEALTHKIT_BRIDGE_TOKEN` | unset | Optional shared token required in HealthKit bridge ingest payloads |
| `CAREKIT_BRIDGE_ID` | `carekit-ios-bridge` | Expected Apple-platform CareKit bridge identity |
| `CAREKIT_DEFAULT_SOURCE_MAPPING_ID` | `carekit-task` | Default source mapping for CareKit bridge ingest |
| `CAREKIT_BRIDGE_TOKEN` | unset | Optional shared token required in CareKit bridge ingest payloads |
| `MQTT_BROKER_HOST` | unset | Set to enable the MQTT bridge |
| `MQTT_BROKER_PORT` | `1883` | Broker port |
| `MQTT_CLIENT_ID` | `reality-engine-pe-lsp` | Client identifier |
| `MQTT_MAPPINGS_FILE` | unset | Path to the registry JSON |
| `MQTT_MAPPINGS_JSON` | unset | Inline registry JSON |
| `MQTT_ALLOW_REGION_OVERLAP` | `0` | Suppress overlap warnings when `1` |
| `TRIGGERS_ENABLED` | `false` | Enable PE trigger envelope recording from RE `mergeBatch` results |
| `TRIGGER_DISPATCH_MODE` | `dry-run` | Label for trigger dispatch target (`dry-run`, `graphql`, `openai`, `ollama`, `acp`, `mcp`) |
| `TRIGGER_GRAPHQL_URL` | `${LOCAL_AI_API_URL}/graphql` | localAIStack GraphQL target metadata for trigger envelopes |

## Compatibility Notes

The runtime is designed as a black-box equivalent, not a line-by-line port.
The actor mental model maps cleanly to explicit mailbox actors and
supervised service state.  The HTTP contract and machine semantics are the
compatibility boundary; the same `examples/machines/*.json` corpus runs
unchanged under all three runtimes and produces byte-identical mergeBatch
ordering, provenance chains, and governance decisions.
