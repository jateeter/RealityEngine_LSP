# HealthKit Spezi Bridge — LSP Runtime

Runtime-specific connection guide for the Common Lisp (SBCL / Hunchentoot)
Perception Engine.

Full bridge documentation — iOS setup, HealthKit types, normalization,
payload format, mapping registry, token auth, and response shapes — is in
[`localHealthkitBridge`](https://github.com/jateeter/localHealthkitBridge).

---

## PE endpoint

```
POST http://<mac-lan-ip>:5600/api/integrations/healthkit/ingest
```

Port `5600` is the default LSP PE port (instance lsp-1). Additional instances
use `5700`, `5800`, etc. (`+100` per instance pair).

Do not use `localhost` from a simulator or iPhone unless the LSP PE runs on
the same device. For simulator-to-Mac, `http://127.0.0.1:5600` works. For a
physical iPhone, use the Mac's LAN IP.

---

## App configuration

Use `config/healthkit-spezi-app.example.env` as the bridge app template:

```text
PE_BASE_URL=http://<mac-lan-ip>:5600
PE_HEALTHKIT_ENDPOINT=/api/integrations/healthkit/ingest
HEALTHKIT_BRIDGE_ID=healthkit-ios-bridge
HEALTHKIT_BRIDGE_TOKEN=<dev-token>
```

---

## PE registry

Use `config/integrations.healthkit-spezi.example.json` as the PE source
registry. It maps the three normalized sample families to fixed perceptual
regions:

```
healthkit:HKCorrelationTypeIdentifierBloodPressure → [4320:4324]
healthkit:HKWorkoutTypeIdentifierWorkout           → [4330:4334]
healthkit:HKCategoryTypeIdentifierSleepAnalysis    → [4340:4344]
```

---

## Verify

```bash
make e2e-healthkit-spezi
```

Proves PE config, bridge token, blood pressure / exercise / sleep ingest,
and downstream RE transitions against the LSP PE on port 5600.

---

## Full bridge documentation

See [`localHealthkitBridge`](https://github.com/jateeter/localHealthkitBridge) for:

- iOS app setup (capabilities, privacy strings, authorization)
- HealthKit read types and normalization rules
- Ingest payload format and batch examples
- Mapping registry lookup order
- Token authentication rules
- PE response shapes
- Runtime connection table (all three runtimes)
