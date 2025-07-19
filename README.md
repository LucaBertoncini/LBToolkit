# LBToolkit
Swiss-army toolkit for FreePascal developers: thread-safe classes, event systems, file utilities and a reactive logging engine UI integration. Lightweight, extensible, and non-intrusive

**LBToolkit** is a modular and extensible collection of units crafted to simplify multithreaded programming, event handling, file operations, and robust logging for desktop, service, and embedded applications.

It includes:

### 🔧 Core Utilities (`src/utils/`)
Reusable components that power multiple projects:
- `uEventsManager.pas`: event dispatching system with flexible signatures
- `uLBBaseThread.pas`: lifecycle-aware thread class with auto-nulling references
- `uLBCalendar.pas`: Module for converting between UTC and local time with full support for historical Daylight Saving Time (DST) rules
- `uLBFileUtils.pas`: advanced file operations, size estimation, search
- `uLBTimers.pas`: timing tools including interruptable sleep and high-precision delays
- `uSharedMemoryManagement.pas`: shared memory manager for fast IPC communication with external processes
- `uTimedOutCriticalSection.pas`: thread-safe access with timeout protection

These utilities form a "Swiss army knife" of functionality across all LBToolkit projects.

---

### 🌐 LBmicroWebServer (`src/LBmicroWebServer/`)
Minimalistic HTTP server with embedded control hooks and dynamic routing support. Designed for fast integration into services and automation pipelines.

- Lightweight HTTP 1.1 engine — zero external dependencies  
- Customizable request handler via `OnRequest` event  
- POST/GET routing with structured URI parsing  
- Ideal for service endpoints, webhooks, remote scripting, or embedded dashboards

---

### 📝 LBLogger System (`src/LBLogger/`)
A reactive, asynchronous and customizable logging platform:
- Queue-based logging with background thread dispatch
- Dynamically attachable sub-loggers (`TLBBaseLogger`) — no changes to existing code
- Built-in visual loggers for desktop apps: `TMemoLogger`, `TLabelLogger`, `TStatusBarLogger`
- External loggers: system `eventlog` via `TfpEventLogger`
- Message filtering by module, content, or severity
- Non-invasive alerting platform with modular reactions to error patterns (`#ERR#`)

### ✅ Architecture Highlights

- **Incremental extension of features**
- **Granular control over message handling**
- **Extreme maintainability and runtime configurability**
