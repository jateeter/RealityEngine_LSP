# Codex Guidance: RealityEngine_LSP

Read `claude.md` for the current codebase map and parity context.

## Role

This repo contains the Common Lisp Reality Engine and Perception Engine implementation. It is a parity target against C++ and Scala.

## Development Rules

- Treat JSON serialization, route behavior, machine loading, MQTT mapping, and PE source state as parity-sensitive.
- Prefer small, explicit Lisp changes and keep package boundaries clear.
- Preserve repo-local Quicklisp/SBCL assumptions.
- Keep generated/runtime Quicklisp artifacts out of source commits unless requested.

## Bug Triage

- For route failures, inspect `src/http.lisp`, `src/json.lisp`, and the relevant service file together.
- For machine-loading failures, inspect `src/loader.lisp`, `src/model.lisp`, and runtime path environment.
- For parity drift, compare identity keys and payload shape before byte equality.

## Verification

Common commands:

```bash
make build
make test
make e2e-healthkit-spezi
```

## Artifact Hygiene

Do not commit runtime state, generated binaries, local logs, or dependency cache churn unless explicitly requested.

