# RealityEngine_LSP

Common Lisp implementation of the Reality Engine and Perception Engine service
surface. The goal is black-box equivalence with `RealityEngine_AI` and
`RealityEngine_CPP`: matching JSON APIs, machine JSON loading, perceptual-space
processing, configuration names, and operational context.

This implementation uses Common Lisp actors to mirror the AKKA mental model:
state is owned by mailbox-driven service actors, messages are processed
serially per actor, `tell` is fire-and-forget, `ask` returns a future-like
reply, and supervised actors can be restarted.

## Requirements

- SBCL
- Quicklisp

The ASDF system depends on:

- `hunchentoot`
- `yason`
- `drakma`
- `bordeaux-threads`
- `alexandria`

## Build And Test

```bash
make deps-check
make test
```

## Run

```bash
cp .env.example .env
./start.sh
```

Defaults match the sibling implementations:

- Reality Engine: `http://localhost:3299`
- Perception Engine: `http://localhost:3300`
- Machine directory: `../RealityEngine_AI/examples/machines`
- Vector dimension: `768`

Stop both services:

```bash
./stop.sh
```

## API Coverage

Implemented endpoints intentionally mirror the C++ service:

- Reality service root, health, config, runtime options, metrics, vector search
- Sequence CRUD and transition reset
- Machine load/list/get/process/what-if/import/export
- Universal perceptual processing and `/api/perceive`
- Perceptual simulation state/history/step/reset
- Perception service health/state/source CRUD/sensor update/push/auto controls
- localAI bridge status/catalog/invoke/bootstrap/signal endpoints

See [docs/API_EQUIVALENCE.md](docs/API_EQUIVALENCE.md),
[docs/CONTEXT_EQUIVALENCE.md](docs/CONTEXT_EQUIVALENCE.md), and
[docs/CONFIGURATION_EQUIVALENCE.md](docs/CONFIGURATION_EQUIVALENCE.md).

## Compatibility Notes

The runtime is designed as a black-box equivalent, not a line-by-line port.
Where AKKA concepts exist in the Scala runtime, the Lisp runtime maps them to
explicit mailbox actors and supervised service state. The HTTP contract and
machine semantics are the compatibility boundary.
