# LBToolkit — Utils Module

This folder contains reusable system-level units organized by functional domain.

## 📁 Structure

| Folder | Contents |
|---|---|
| `core/` | Threading primitives, event system, timers, IPC, circular buffer, signal manager |
| `app/` | Application bootstrap, SSL config, external library manager |
| `data/` | SQLite wrapper, PostgreSQL client, DB connection manager |
| `net/` | Mail utilities, HTTP constants, remote connection data, JWT/token manager |
| `ai/` | LLM chat client (OpenAI-compatible), Speech-to-Text wrappers |
| `scripting/` | Lape script engine base class (hot-reload, SIGUSR1, compile/run cycle) |
| `hardware/` | Industrial and field hardware drivers |
| `hardware/PLC/` | Siemens S7 (Snap7), Modbus TCP/RTU, OPC-UA (open62541) |
| `hardware/RFID/` | Impinj R700 (`uR700Reader`) and Zebra FXR90 (`uFXR90Manager`) drivers + common types (`uRFIDCommon`) |
| `hardware/RFID/_legacy/` | Legacy LLRP-based Impinj R700 driver (kept for reference: had RX sensitivity and live power tuning not available via REST API) |
| `hardware/RTK/` | GPS RTK: NTRIP client, RTCM3 distributor, UBX protocol parser, rover management |
| `media/` | FFmpeg/libav* bindings (codec, encoder, format, filter managers) |
| `misc/` | General-purpose utilities: calendar/DST, file utils, geometry, Base64, expression evaluator |

## 🔑 Key Units

### `core/uTimedoutCriticalSection.pas`
Critical section with configurable timeout and automatic diagnostics. On timeout, logs the last owner to aid deadlock analysis without a debugger.

### `core/uEventsManager.pas`
Thread-safe event broadcasting system with 3 callback modes (object method, C-style opaque pointer, single callback with event name). Handles cross-deregistration automatically on destruction.

### `core/uLBBaseThread.pas`
Lifecycle-aware thread class with safe termination, critical section handling, and multi-reference cleanup. Suitable for daemon-like background workers.

### `core/uLBTimers.pas`
Timing tools including interruptable sleep and high-precision delays.

### `core/uIPCUtils.pas`
Shared memory manager (SysV) for fast IPC with external processes (Python, native apps).

### `app/uLBApplicationBoostrap.pas`
Foundational class for orchestrating application initialization: INI/XML config loading, logger setup, optional web server activation.

### `scripting/uLapeScriptContextBase.pas`
Generic Lape script engine base: compile + hot-reload via `mtime` check and POSIX `SIGUSR1`, `BeforeRun`/`AfterRun` hooks, `TEventsManager` integration. POSIX-only (`BaseUnix`). External dependency: Lape library (not included in this repo).

---

## 🛠 Integration

These units are used by other LBToolkit modules such as `LBLogger`, `LBmicroWebServer`, and `LBWebSocketClient`.


### 🔹 uEventsManager.pas
An event broadcasting system supporting multiple listeners.


### 🔹 TLBApplicationBootstrap

`TLBApplicationBootstrap` is a foundational class designed to orchestrate the initialization of Pascal applications.  
It provides a structured approach for loading configuration settings from `.ini` or `.xml` files, setting up the logging system, and activating the integrated web server component (`TLBmicroWebServer`).
This class acts as an extensible entry point for any project, allowing developers to derive custom application managers with specialized logic and behaviors.

#### ✨ Key Features
- 🛠 Configuration loading from INI or XML format  
- 📄 Automatic logger setup with support for file path resolution and size limits  
- 🌐 Optional web server activation with document root configuration  
- 📦 Clean separation of bootstrap logic from application-specific features  
- 🔁 Designed for inheritance and modular expansion  


### 🔹 uLBBaseThread.pas
A lifecycle-aware thread class with safe termination, critical section handling, and multi-reference cleanup.

- Ensures thread closure with `Terminate` + `WaitFor`
- Manages external references via `RegisterReference`
- Suitable for daemon-like background workers

> **Note for inheritance:**  
> Classes deriving from `TLBBaseThread` should call `inherited Destroy` **before** releasing their local resources to ensure safe thread finalization.


### 🔹 uLBCalendar.pas
Module for converting between UTC and local time with full support for historical Daylight Saving Time (DST) rules. It includes:

- Accurate bidirectional conversions: `UTC ↔ LocalTime`
- Year-specific DST activation based on official Italian transitions (1940 onward)
- ISO 8601 formatting for timestamps
- Linux and Windows compatibility
- Extensible architecture for supporting multiple regions


### 🔹 uLBTimers.pas
Timing tools including interruptable sleep and high-precision delays.

### 🔹 uLBFileUtils.pas
Utilities for file operations, folder size calculation and recursive discovery.

### 🔹 uSharedMemoryManagement.pas
A shared memory manager for fast IPC communication with external processes (e.g., Python, other native apps).

**Basic SysV usage:**
```pascal
var
  Shared: pSharedMemory;
begin
  Shared := AllocateSharedMemory(1234, 4096);

  if Assigned(Shared) then
  begin
    // Use Shared^.mem to exchange data with external process
    // ...

    closeSharedMemory(Shared);
    Dispose(Shared);
  end;
end;
```

---

## 🛠 Integration

These units are used by other LBToolkit modules such as `LBLogger` and external projects.
