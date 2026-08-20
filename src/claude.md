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
  vector and every source `cursor`, and re-arms test sources — matching
  `PerceptionEngine::reset` (C++) and `PerceptionEngine.reset()` (Scala).
- Reset **leaves** dimension, match algorithm and auto-push settings alone.
  Those are configuration, not run state.
- Removing a source stays a separate operation: `DELETE /api/sources/:id` and the
  corpus path that drops a machine's source when the machine leaves the dynamic
  corpus. Those are the intended way to forget a source.

Known open divergence: the `"test"` branch of the assembly function advances a
source's `cursor` as a side effect, so any call that assembles the vector —
including a read of `/api/state` — advances playback. C++ and Scala advance only
in a dedicated per-push step (#55, follow-up).

