# OpenAPI Contracts — RealityEngine_LSP

Two OpenAPI 3.0.3 documents describe the LSP runtime's HTTP surface.

| File | Service | Default URL |
|---|---|---|
| [`reality-engine.yaml`](reality-engine.yaml)     | Reality Engine     | `http://localhost:5601` |
| [`perception-engine.yaml`](perception-engine.yaml) | Perception Engine | `http://localhost:5600` |

Wire-compatible with [`RealityEngine_AI`](../../../RealityEngine_AI/docs/openapi/)
(the default runtime) and [`RealityEngine_CPP`](../../../RealityEngine_CPP/docs/openapi/)
— the same JSON corpus drives byte-identical merge ordering and identical
Prometheus metrics shape; only the `runtime` label differs (this runtime
emits `runtime="lsp"`).

## Quick view

```bash
# Redocly CLI
npx @redocly/cli preview-docs docs/openapi/reality-engine.yaml

# Swagger UI in Docker
docker run -p 8081:8080 \
  -e SWAGGER_JSON=/spec/reality-engine.yaml \
  -v $PWD/docs/openapi:/spec \
  swaggerapi/swagger-ui
# open http://localhost:8081
```

## What's new in 1.1

- `/api/metrics` (Prometheus text, `runtime="lsp"`) on the RE
- `/api/governance/route`, `/api/runtime/vector-space`, `/api/runtime/storage-footprint` on the RE
- `/api/mqtt/status` and `/api/mqtt/mappings` (GET + PUT) on the PE — driven by
  the hand-rolled MQTT v3.1.1 client in `src/mqtt-client.lisp` + the
  `mqtt-mapping-registry` schema in `src/mqtt-mapping.lisp`
- `PagingDecision`, `MqttBridgeStatus`, `MqttMappingRule`, `MqttMappingsResponse` schemas
