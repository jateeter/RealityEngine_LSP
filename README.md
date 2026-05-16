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

The simplest entry point is the unified orchestrator from `RealityEngine_AI`:

```bash
# Either route works — both delegate the same way
./startUniverse.sh --re-engine=lsp --pe-engine=lsp   # from this repo (shim)
                                                      # or from RealityEngine_AI directly
./stopUniverse.sh                                    # tears the LSP engines down
```

Or run the native services directly:

```bash
cp .env.example .env
./start.sh
./stop.sh
```

Defaults match the sibling implementations:

- Reality Engine: `http://localhost:3299`
- Perception Engine: `http://localhost:3300`
- Machine directory: `../RealityEngine_AI/examples/machines`
- Vector dimension: `768`

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
- **MQTT bridge** — `/api/mqtt/status`, `/api/mqtt/mappings` (GET + PUT)
- **Cross-runtime Prometheus** — `/api/metrics` text exposition stamped
  with `runtime="lsp"` on every line

See [docs/API_EQUIVALENCE.md](docs/API_EQUIVALENCE.md),
[docs/CONTEXT_EQUIVALENCE.md](docs/CONTEXT_EQUIVALENCE.md), and
[docs/CONFIGURATION_EQUIVALENCE.md](docs/CONFIGURATION_EQUIVALENCE.md).

## Configuration

Canonical env vars (set in `.env`, on the CLI, or via `startUniverse.sh`
flags):

| Variable | Default | Purpose |
|---|---|---|
| `REALITY_ENGINE_PORT` | `3299` | RE bind port |
| `PERCEPTION_ENGINE_PORT` | `3300` | PE bind port |
| `VECTOR_DIMENSION` | `768` | Perceptual-space dimension floor |
| `MACHINES_DIR` | `../RealityEngine_AI/examples/machines` | Source of startup machines |
| `LOCAL_AI_API_URL` | `http://localhost:8000` | localAIStack integration target |
| `MQTT_BROKER_HOST` | unset | Set to enable the MQTT bridge |
| `MQTT_BROKER_PORT` | `1883` | Broker port |
| `MQTT_CLIENT_ID` | `reality-engine-pe-lsp` | Client identifier |
| `MQTT_MAPPINGS_FILE` | unset | Path to the registry JSON |
| `MQTT_MAPPINGS_JSON` | unset | Inline registry JSON |
| `MQTT_ALLOW_REGION_OVERLAP` | `0` | Suppress overlap warnings when `1` |

## Compatibility Notes

The runtime is designed as a black-box equivalent, not a line-by-line port.
The actor mental model maps cleanly to explicit mailbox actors and
supervised service state.  The HTTP contract and machine semantics are the
compatibility boundary; the same `examples/machines/*.json` corpus runs
unchanged under all three runtimes and produces byte-identical mergeBatch
ordering, provenance chains, and governance decisions.
