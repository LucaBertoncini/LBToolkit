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

### 🔹 uLBTimers.pas
Timing tools including interruptable sleep and high-precision delays.

### 🔹 uLBFileUtils.pas
Utilities for file operations, folder size calculation and recursive discovery.

---

## 🛠 Integration

These units are used by other LBToolkit modules such as `LBLogger` and external projects.
