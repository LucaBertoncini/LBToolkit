# LBToolkit — Utils Module

This folder contains reusable system-level units that support multiple projects within LBToolkit.

## 📦 Included Units

### 🔹 uLBBaseThread.pas
A lifecycle-aware thread class with safe termination, critical section handling, and multi-reference cleanup.

- Ensures thread closure with `Terminate` + `WaitFor`
- Manages external references via `RegisterReference`
- Suitable for daemon-like background workers

> **Note for inheritance:**  
> Classes deriving from `TLBBaseThread` should call `inherited Destroy` **before** releasing their local resources to ensure safe thread finalization.

### 🔹 uEventsManager.pas
An event broadcasting system supporting multiple listeners.

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
