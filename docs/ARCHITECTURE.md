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

## 2. Declarative XML Route Registry & Permission System (`uWebRouteRegistry.pas`)

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

## 3. Concurrency & Threading Model

### 🔹 Thread Management (`TLBBaseThread`)
- All thread instances inherit from `TLBBaseThread` (which extends standard `TThread`).
- Threads execute their main loop inside `InternalExecute`.
- Interruptible sleep `SleepWithCheck(ms)` allows worker threads to pause without blocking `Terminate` signals.
- External reference tracking ensures object references are set to `nil` automatically upon thread termination (`RegisterReference`).

---

## 4. Networking & Sockets

- **Synapse Socket Stack**: Network operations leverage Ararat Synapse (`blcksock`, `TTCPBlockSocket`).
- **OpenSSL 3 Support**: `uLBSSLConfig.pas` initializes multi-threaded locks and callbacks required by OpenSSL 3.0+.
- **SIGPIPE Handling**: Unix systems intercept and ignore `SIGPIPE` to prevent process termination on client socket disconnects.

---

## 5. I/O & Memory Strategy

- **Zero-Allocation Ring Buffers (`TLBCircularBuffer`)**: Used for HTTP and WebSocket stream parsing. Raw byte blocks are read into the circular buffer and parsed in-place.
- **Streaming Uploads**: Raw uploads bypass memory allocations by piping incoming socket streams directly to temporary files on disk.

---

## 6. C-API Shared Library Interface (`src/shared/LBToolkit/`)

LBToolkit can be compiled into a C-compatible dynamic library (`.so` / `.dll`), allowing foreign languages (C, C++, Python, Rust, Go, C#) to instantiate and control its components:
- `Toolkit_CAPI_microWebServer.pas`: Exported C functions to start/stop the web server, add routes, and handle requests.
- `Toolkit_CAPI_CircularBuffer.pas`: Exported C functions to operate ring buffers.
- `Toolkit_CAPI_Logger.pas`: Exported C functions to dispatch log messages across language boundaries.
- `Toolkit_CAPI_WebPrism.pas`: Exported C functions to configure Python / Node.js worker pools.
