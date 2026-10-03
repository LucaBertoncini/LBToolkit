# LBToolkit — Code and Style Conventions

This document describes the programming conventions used in the toolkit's sources. Contributors (both humans and AI assistants) must follow them to keep the codebase homogeneous, maintainable, and predictable.

---

## 1. Name Prefixes

The toolkit uses a rigorous prefix system that makes the nature of every identifier immediately recognizable.

| Prefix | Meaning | Real examples in the repo |
|--------|---------|---------------------------|
| `u` (file name) | Unit | `uLBBaseThread.pas`, `uEventsManager.pas`, `uWebRouteRegistry.pas` |
| `T` | Type (classes, records, arrays, enums, `class of`) | `TLBBaseThread`, `TMultiReferenceObject`, `TPolygon = array`, `TLBLoggerMessageType` |
| `P` / `p` | Typed pointer | `pThread = ^TThread`, `PRTLEvent`, `pSharedMemory`, `pLoggerConfig` |
| `F` | Private class field (**F**ield) | `FWriter`, `FFilePath`, `FMessageList`, `FCSAlternativeLoggers`, `FReferences` |
| `A` or `a` | Formal parameter of a function/procedure | `AValue`, `AMessage`, `AReference`, `aFilename`, `aCount`, `aOffset`, `aBuffer` |
| `_` | **Local** variable of a routine | `_Node: TDOMNode`, `_IniFile: TIniFile`, `_Section: String` (see `uLBSSLConfig.pas`) |
| `c` | Constant | `cDEFAULT_INI_SECTION`, `cAcquireTimeout`, `cToken_Field_Token` |
| `I` | Interface | `IEventInfo = interface(IInterface)` in `uEventsManager.pas` |

Golden rule: **never reuse the same name for a parameter and a local variable**. The three-level scheme `aParameter` / `_LocalVariable` / `FField` eliminates all visual ambiguity inside routine bodies.

---

## 2. Class Structure

- **Visibility**: Use `strict private` and `strict protected` instead of plain `private`/`protected` to prevent "sibling" access between classes in the same unit.
- **Nested types**: Declared inside the class with local `type ... end` blocks (e.g., `TEventsManager.TEventInfo`).
- **Constructors/Destructors**: `constructor Create; virtual;` and `destructor Destroy; override;`. If destruction has ordering constraints (typical of threads), document it with a `{ }` block above the declaration:

```pascal
{
  Warning: derived classes must call inherited Destroy BEFORE
  destroying local resources, because only then is the
  complete termination of the thread guaranteed.
}
destructor Destroy; override;
```

---

## 3. Error and Resource Handling

The error-handling style is based on **return codes**: the toolkit targets software that must stay running (services, web servers), so operational routines return `False` on failure and **do not propagate exceptions**. Exceptions raised internally are converted into a log entry + return code:

```pascal
Result := False;
try
  // ... work
  Result := True;
except
  on E: Exception do
    LBLogger.Write(1, 'RoutineName', lmt_Error, E.Message);
end;
```

> **Note for contributors**: Where existing code contains a `raise Exception.Create(...)` (for example in route registry validation), the direction to follow is replacing it with the return-code + log pattern.

### Resource Management
Every acquisition must have its release in a `finally` block:
- **Locks**: Pattern `if CS.Acquire('Context', Timeout) then try ... finally CS.Release; end;` — **never** a lock without a timeout.
- **Memory**: `FreeAndNil(X)` as the canonical form of release; `try/finally` around created objects.
- **OS handles/sockets**: Closed in `Destroy` and/or `finally`.

---

## 4. Thread Safety

- Every field accessed by multiple threads must be protected by a dedicated `TTimedOutCriticalSection` (often named `FCS...`, e.g., `FCSWriter`, `FCSAlternativeLoggers`).
- The context passed to `Acquire` is a descriptive string (the method acquiring the lock) for deadlock debugging.
- For threads, the `TLBBaseThread` rules apply: never bare threads, always destroy with `FreeAndNil`, share pointers through `TMultiReferenceObject`.

---

## 5. Logging in Code

- Standard call: `LBLogger.Write(LogLevel, '<Context>', lmt_<Type>, '<Format>', [Arguments]);`
  - `LogLevel` (`Byte`): Message verbosity. Errors/warnings use level `1`; debug/info messages use higher levels (`3`, `5`). Written only if `LogLevel <= MaxLogLevel`.
  - `<Context>`: Name of the routine doing the logging (e.g., `'TLBBaseThread.Destroy'`).
  - Types (`lmt_*`): `lmt_Debug` for tracing, `lmt_Info` for state, `lmt_Warning`/`lmt_Error`/`lmt_Critical` for increasing anomalies.
- Logging must be **non-blocking on the critical path**: never synchronous heavy I/O logging inside hot request loops.

---

## 6. Comments and Inline Documentation

- `{ ... }` blocks for contract notes (destruction order, thread-safety assumptions) right above the affected declaration.
- Trailing `//` comments for point explanations.
- **Language**: Comments are in Italian in the core, in English in public/C API interfaces. Keep internal consistency within the modified unit.

---

## 7. Conditional Portability

- `{$IFDEF Linux} ... {$ELSE} ... {$ENDIF}` directives isolated at specific points (calling conventions, System V vs Windows IPC APIs), never at the whole-unit level.
- Business logic must not contain platform branches: abstraction belongs in the primitives under `src/utils/`.

---

## Checklist for Contributors

Before submitting a change, verify:
- [ ] New class fields prefixed `F` and private (`strict private`).
- [ ] New parameters prefixed `a`/`A`, new local variables prefixed `_`.
- [ ] Any new types with `T`/`P` prefix and explicit enum values.
- [ ] Locks always acquired with timeout and released in `finally`.
- [ ] Errors handled with return code + log, without propagating exceptions.
- [ ] Logs added with appropriate context and level.
- [ ] No direct OS-dependent API calls outside primitives in `src/utils/`.
