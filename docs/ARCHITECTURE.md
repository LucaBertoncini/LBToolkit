# 🏛️ LBToolkit System Architecture & Internal Mechanics

This document details the architectural design principles, concurrency model, memory management strategies, and extension points of the **LBToolkit** framework.

---

## 1. Modular Architecture Overview

LBToolkit is structured as an ecosystem of independent, low-coupling Object Pascal modules designed for FreePascal (FPC) and Lazarus:

```
                            +-----------------------------+
                            |     LBToolkit C-API Shared  |
                            |       Library (DLL / .so)   |
                            +--------------+--------------+
                                           |
    +-----------------+--------------------+--------------------+-----------------+
    |                 |                    |                    |                 |
    v                 v                    v                    v                 v
[LBmicroWebServer] [LBWebPrism]   [LBWebSocketClient]      [LBLogger]   [LBVirtualKeyboard]
    |                 |                    |                    |                 |
    +-----------------+--------------------+--------------------+-----------------+
                                           |
                                           v
                            +-----------------------------+
                            |     src/utils Core Engine    |
                            | (Thread, RingBuffer, IPC,CS)|
                            +-----------------------------+
```

---

## 2. Core Primitives (`src/utils/`)

### 🔹 Weak Reference Safety (`TMultiReferenceObject`)
`TMultiReferenceObject` solves dangling pointers across thread and object boundaries. Objects register references to variable addresses (`AddReference(@FWorker)`). Upon destruction, `ClearReferences()` automatically sets all registered owner pointers to `nil`.

### 🔹 Deterministic Thread Lifecycle (`TLBBaseThread`)
- **Single Rule**: Destroying a thread is as simple as `FreeAndNil(aThread)`.
- **`FreeOnTerminate := True`**: Default state on creation.
- **`Terminate`**: Signals internal `FExitFromPauseEvent` to interrupt responsive pauses (`PauseFor`).
- **`Destroy`**: Sets `FreeOnTerminate := False`, calls `Terminate` and `WaitFor`, and runs `ClearReferences()`.
- **Descendant Contract**: Derived `Destroy` overrides **must call `inherited Destroy` FIRST** before releasing local class resources.

### 🔹 Deadlock-Safe Locks (`TTimedOutCriticalSection`)
- `Acquire(aFunctionName, aTimeoutMs)` enforces timeouts (default 3000 ms).
- Logs `aFunctionName` and the last thread owner (`FLastOwner`) on timeout to pinpoint deadlocks.

### 🔹 Event Management (`TEventsManager`)
- **Synchronous Notification**: Handlers run in the calling thread context.
- **Mutual Auto-Deregistration**: Managers register on each other's `EM_Destroy` event, dropping orphaned listeners automatically when either side dies.

---

## 3. Global Logging Engine & Chain of Responsibility Sub-Loggers (`ULBLogger.pas`)

`LBLogger` provides a global logging infrastructure and reactive event/alert system:
- **Global Lifecycle**: `InitLogger(...)` initializes a thread-safe singleton instance `LBLogger`, and `ReleaseLogger()` gracefully stops worker threads and releases resources.
- **Chain of Responsibility**: When `LBLogger.Write()` is invoked, the message traverses a list of sub-loggers (`FAlternativeLoggers`).
- **Dynamic Interception**: Sub-loggers implement `virtualWrite(aLogLevel, aSender, aMsgType, var aMsgText)`. If a sub-logger modifies `aMsgText := ''`, the chain stops (`Exit`), allowing sub-loggers to consume or redirect messages (e.g. Email, Telegram, MQTT, Desktop UI Memo) without modifying application code.

---

## 4. Declarative XML Route Registry & Permission System (`uWebRouteRegistry.pas`)

The core REST routing engine in `LBmicroWebServer` uses a 3-tier conceptual model for each endpoint:

1. **Functional Area (`FunctionalArea.Code`)**: The domain boundary (e.g., `Inventory`, `Auth`, `Users`).
2. **Function (`Endpoint.Function`)**: The unique operation name inside the area mapping to a registered Pascal worker method.
3. **OpType (`Endpoint.OpType`)**: An optional authorization permission code checked against the user's role configuration (`hasPermission(UserConfig, AreaCode, OpType)`).

### Endpoint Execution Families (`TEndpointKind`):
- `ekStandard`: JSON body input parsed into `TJSONObject` and returns `TJSONData`.
- `ekFileDownload`: Invokes worker to retrieve a disk path and streams the file to the client.
- `ekAuth`: Has raw HTTP request/response headers access (used for setting session cookies/tokens).
- `ekUpload`: Processes raw binary files uploaded to temporary disk storage before calling the worker.
- `ekProxy`: Purely declarative transparent proxy that signs requests and forwards them to remote target servers without requiring any local Pascal worker code.

---

## 5. Networking & Sockets

- **Synapse Socket Stack**: Network operations leverage Ararat Synapse (`blcksock`, `TTCPBlockSocket`).
- **OpenSSL 3 Support**: `uLBSSLConfig.pas` initializes multi-threaded locks and callbacks required by OpenSSL 3.0+.
- **SIGPIPE Handling**: Unix systems intercept and ignore `SIGPIPE` to prevent process termination on client socket disconnects.

---

## 6. I/O & Memory Strategy

- **Zero-Allocation Ring Buffers (`TLBCircularBuffer` / `TLBCircularBufferThreaded`)**: Used for HTTP and WebSocket stream parsing. Raw byte blocks are read into the circular buffer and parsed in-place.
- **Streaming Uploads**: Raw uploads bypass memory allocations by piping incoming socket streams directly to temporary files on disk.

---

## 7. C-API Shared Library Interface (`src/shared/LBToolkit/`)

LBToolkit can be compiled into a C-compatible dynamic library (`.so` / `.dll`), allowing foreign languages (C, C++, Python, Rust, Go, C#) to instantiate and control its components:
- `Toolkit_CAPI_microWebServer.pas`: Exported C functions to start/stop the web server, add routes, and handle requests.
- `Toolkit_CAPI_CircularBuffer.pas`: Exported C functions to operate ring buffers.
- `Toolkit_CAPI_Logger.pas`: Exported C functions to dispatch log messages across language boundaries.
- `Toolkit_CAPI_WebPrism.pas`: Exported C functions to configure Python / Node.js worker pools.
