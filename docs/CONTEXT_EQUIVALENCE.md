# Context Equivalence

RealityEngine_LSP preserves the deployment context of the existing runtimes.

## Repository Context

Default paths match the C++ startup scripts:

- `MACHINES_DIR=../RealityEngine_AI/examples/machines`
- `LOCAL_AI_MACHINES_DIR=../localAIStack/data/machines`
- `QDRANT_STORAGE_DIR=../localAIStack/volumes/qdrant`

The Lisp runtime can load the authored machine JSON corpus directly. Machine
identity, sequence identity, vector identity, metadata, perceptual mappings,
arbiter rules, and test input-sequence metadata are preserved.

## Runtime Context

The runtime model maps Scala/AKKA concepts into explicit Common Lisp actors:

| AKKA Concept | Lisp Mapping |
| --- | --- |
| Actor | `state-actor` mailbox thread |
| `tell` | `actor-tell` |
| `ask` | `actor-ask` with condition-variable reply |
| Actor state isolation | Service state is only mutated inside actor handlers |
| Supervision | Actor supervisor callback logs failures and replies with error |
| Single-threaded actor execution | One mailbox consumer per actor |

The Reality and Perception services each own state through an actor. HTTP
handlers submit synchronous `ask` messages to the actor, which serializes reads
and writes in the same spirit as AKKA service actors.

## Perceptual Context

`VECTOR_DIMENSION` is retained as a compatibility floor. Runtime processing can
accept shorter or longer vectors; shorter vectors are zero-filled and longer
vectors extend the active perceptual-space list for the current operation.

The following mapping semantics mirror the C++ runtime:

- machine input uses `perceptualMapping.input.offset/length`
- machine output merges into `perceptualMapping.output.offset/length`
- `compact` suppresses per-machine results unless explicitly overridden
- sparse/domain vector forms are assembled into a dense perceptual vector

