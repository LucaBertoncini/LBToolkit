# LBToolkit
Swiss-army toolkit for FreePascal developers: thread-safe classes, event systems, file utilities and a reactive logging engine UI integration. Lightweight, extensible, and non-intrusive

**LBToolkit** is a modular and extensible collection of units crafted to simplify multithreaded programming, event handling, file operations, and robust logging for desktop, service, and embedded applications.

It includes:

### 🔧 Core Utilities (`src/utils/`)
Reusable components that power multiple projects:
- `uEventsManager.pas`: event dispatching system with flexible signatures
- `uLBBaseThread.pas`: lifecycle-aware thread class with auto-nulling references
- `uLBFileUtils.pas`: advanced file operations, size estimation, search
- `uTimedOutCriticalSection.pas`: thread-safe access with timeout protection

These utilities form a "Swiss army knife" of functionality across all LBToolkit projects.

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

- **No code modification required** — compatible with legacy software
- **Incremental extension of features**
- **Granular control over message handling**
- **Extreme maintainability and runtime configurability**
