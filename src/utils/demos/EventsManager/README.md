# 📡 TEventsManager — Event Dispatch System for FreePascal

**TEventsManager** provides a flexible and thread-safe framework for handling asynchronous events in FreePascal applications.  
It supports multiple invocation styles and enables decoupled communication between objects, modules, and threads.

---

## ✨ Core Features

- ✅ Three operating modes:
  - `emm_Events`: uses standard `TNotifyEvent` callbacks
  - `emm_Callbacks`: C-style function pointers with opaque user data
  - `emm_EventsSingleCallback`: advanced callbacks with event names and senders
- 🔁 Multi-listener support per event
- 🔒 Thread-safe access with `TTimedOutCriticalSection`
- 🛑 Runtime event disabling and conditional dispatching
- 🧹 Automatic cleanup through cross-registered destroy events
