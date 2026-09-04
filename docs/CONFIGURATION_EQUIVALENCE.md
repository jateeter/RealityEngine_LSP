# Configuration Equivalence

RealityEngine_LSP uses the same environment variables as the sibling runtimes.

| Variable | Default | Purpose |
| --- | --- | --- |
| `REALITY_ENGINE_PORT` | `5601` | Reality Engine HTTP port |
| `PERCEPTION_ENGINE_PORT` | `5600` | Perception Engine HTTP port |
| `VECTOR_DIMENSION` | `7680` | Dense transport compatibility floor |
| `MACHINES_DIR` | `../RealityEngine_Machines/machines` | Machine JSON corpus |
| `LOCAL_AI_API_URL` | `http://localhost:4000` | localAIStack API base URL |
| `LOCAL_AI_MACHINES_DIR` | `../localAIStack/data/machines` | localAI bridge machine directory |
| `LOCAL_AI_BOOTSTRAP` | `false` | Reserved for startup bootstrap parity |
| `QDRANT_URL` | `http://localhost:4333` | Shared Qdrant REST URL |
| `QDRANT_GRPC_URL` | `http://localhost:4334` | Shared Qdrant gRPC URL |
| `QDRANT_STORAGE_DIR` | `../localAIStack/volumes/qdrant` | Shared Qdrant storage path |
| `QDRANT_LOCALAI_COLLECTION` | `localai_docs` | localAI Qdrant collection |
| `QDRANT_REALITY_COLLECTION` | `reality-events` | Reality Event collection |
| `RE_HISTORY_LIMIT` | `250` | Runtime history entries retained |
| `RE_INCLUDE_MACHINE_RESULTS` | `true` | Default `/api/perceive` machine-result verbosity |
| `RE_INCLUDE_PERCEPTUAL_SPACE` | `true` | Default `/api/perceive` perceptual-space inclusion |

Configuration can be inspected at:

- Reality: `GET /api/config`
- Runtime options: `GET /api/runtime/options`
- Perception: `GET /api/state`

Runtime options can be changed through `PATCH /api/runtime/options`, matching
the C++ API shape.
