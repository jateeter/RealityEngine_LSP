# API Equivalence

`RealityEngine_LSP` implements the same black-box HTTP contract used by
`RealityEngine_AI` and `RealityEngine_CPP`.

## Reality Engine

Core endpoints:

| Method | Path | Status |
| --- | --- | --- |
| `GET` | `/`, `/api`, `/api/health` | Implemented |
| `GET` | `/api/config` | Implemented |
| `PUT` | `/api/config/dimension` | Implemented |
| `POST` | `/api/vectors/search` | Implemented in-memory |
| `POST` | `/api/vectors` | Implemented in-memory |
| `GET`, `DELETE` | `/api/vectors/{id}` | Implemented |
| `GET`, `POST` | `/api/sequences` | Implemented |
| `GET` | `/api/sequences/{id}` | Implemented |
| `POST` | `/api/engine/reset` | Implemented |
| `GET` | `/api/engine/stats` | Implemented |
| `GET` | `/api/runtime/metrics` | Implemented |
| `GET`, `PATCH` | `/api/runtime/options` | Implemented |
| `GET` | `/api/runtime/vector-space` | Implemented |
| `GET` | `/api/engine/active` | Implemented |
| `GET` | `/api/engine/history` | Implemented |
| `POST` | `/api/engine/process` | Implemented |
| `GET`, `POST` | `/api/machines` | Implemented |
| `GET`, `PUT`, `DELETE` | `/api/machines/{id}` | Implemented |
| `POST` | `/api/machines/{id}/process` | Implemented |
| `POST` | `/api/machines/{id}/process-universal` | Implemented |
| `POST` | `/api/machines/{id}/whatif` | Implemented |
| `POST` | `/api/machines/process-universal/all` | Implemented |
| `GET` | `/api/machines/json/list` | Implemented |
| `GET` | `/api/machines/json/{name}` | Implemented |
| `POST` | `/api/machines/json/import` | Implemented |
| `GET` | `/api/machines/{id}/export` | Implemented |
| `GET` | `/api/machine-graph` | Implemented |
| `POST` | `/api/perceive` | Implemented |
| `POST` | `/api/perception/diagnostic` | Implemented placeholder-compatible |
| `POST`, `GET` | `/api/perceptual-simulation/*` | Implemented synchronous subset |

The `/api/perceive` input contract accepts exactly the same transport forms as
the C++ runtime:

- `vector`
- `sparseVector`
- `domainVectors`

It also honors:

- `matchAlgorithmOverride`
- `matchAlgorithm`
- `includeMachineResults`
- `includePerceptualSpace`
- `compact`

## Perception Engine

| Method | Path | Status |
| --- | --- | --- |
| `GET` | `/`, `/api/health`, `/api/state` | Implemented |
| `GET` | `/api/integrations/localai/status` | Implemented |
| `GET` | `/api/integrations/localai/catalog` | Implemented |
| `POST` | `/api/integrations/localai/bootstrap` | Implemented |
| `POST` | `/api/integrations/localai/invoke` | Implemented |
| `POST` | `/api/signals` | Implemented |
| `POST` | `/api/push` | Implemented |
| `GET` | `/api/push/{id}` | Shape-compatible placeholder |
| `POST` | `/api/auto/start`, `/api/auto/stop` | Implemented state controls |
| `PATCH` | `/api/config` | Implemented |
| `POST` | `/api/reset` | Implemented |
| `GET`, `POST` | `/api/sources` | Implemented |
| `PATCH`, `DELETE` | `/api/sources/{id}` | Implemented |
| `POST` | `/api/sensors/{sensorId}` | Implemented |
| `GET` | `/api/machines` | Proxies Reality Engine |

## Known Differences

The implementation is intentionally black-box equivalent, not internally
isomorphic. Persistent Qdrant writes, WebSocket push broadcasts, and the C++
bounded worker-pool instrumentation are represented by compatible response
fields but are not yet backed by the same transport internals.

