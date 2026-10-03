# LBToolkit — Core Utilities (`src/utils/`)

Module `utils` provides essential low-level, thread-safe, and high-performance building blocks for Pascal systems programming, networking, concurrency, and IPC.

---

## 📚 Overview of Key Units

### 🧵 `uLBBaseThread.pas` — Lifecycle-Aware Thread Base Class
`TLBBaseThread` is a robust subclass of `TThread` designed for background daemon threads, worker pools, and asynchronous tasks.
- **Controlled Lifecycle**: Safe `Stop()` method with customizable wait timeout.
- **Reference Tracking**: `RegisterReference()` and `UnregisterReference()` to automatically nullify external object references when the thread terminates.
- **Timeout Management**: Integrated wait logic avoiding infinite deadlocks during teardown.

**Usage Example:**
```pascal
type
  TMyWorker = class(TLBBaseThread)
  protected
    procedure InternalExecute; override;
  end;

procedure TMyWorker.InternalExecute;
begin
  while not Terminated do
  begin
    // Perform task...
    SleepWithCheck(100); // Interruptible sleep
  end;
end;
```

---

### 🔒 `uTimedoutCriticalSection.pas` — Deadlock-Resistant Sincronization
`TTimedOutCriticalSection` wraps system critical sections with timeout-based acquisition (`Enter(TimeoutMs)`).
- Prevents thread deadlocks when acquiring locks.
- Returns `Boolean` indicating whether the lock was acquired before timing out.

**Usage Example:**
```pascal
var
  CS: TTimedOutCriticalSection;
begin
  CS := TTimedOutCriticalSection.Create;
  try
    if CS.Enter(1000) then // Try to acquire lock within 1 second
    begin
      try
        // Critical section logic
      finally
        CS.Leave;
      end;
    end
    else
      // Handle timeout / contention
  finally
    CS.Free;
  end;
end;
```

---

### 📡 `uEventsManager.pas` — Thread-Safe Decoupled Event Dispatcher
`TEventsManager` implements a publish-subscribe pattern allowing multiple listeners to register for named events with flexible parameters.
- Thread-safe listener registration and dispatching.
- Supports generic callback procedures and method pointers.

---

### ⭕ `uLBCircularBuffer.pas` — High-Performance Ring Buffer
`TLBCircularBuffer` is a memory-efficient, fixed-size ring buffer designed for streaming socket I/O without repeated memory reallocations.
- `Write()`, `Read()`, `Peek()`.
- Direct stream dumping via `WriteToStream()`.
- Thread-safe variants and lock-free fast-paths for single-producer/single-consumer setups.

---

### 🛡️ `uLBSSLConfig.pas` — OpenSSL 3 Thread-Safe Initialization
Handles OpenSSL initialization and global thread callbacks.
- Ensures OpenSSL 3 crypto functions are thread-safe when called from multiple web server worker threads.

---

### 💻 `uIPCUtils.pas` — Inter-Process Communication
Provides shared memory (`AllocateSharedMemory`, `AttachSharedMemory`) and named semaphores across Windows and Linux (SysV / POSIX IPC).
- Shared memory buffer structure `TSharedMemory`.
- Cross-platform named semaphore class `TLBNamedSemaphore`.

---

### 🗄️ `SQLiteWrapper.pas` — Lightweight SQLite Object Wrapper
Provides an easy-to-use, zero-overhead Pascal interface for SQLite database interactions.

---

### 📂 `uLBFileUtils.pas` — Extended File System Utilities
- Recursive directory scanning.
- Directory size estimation.
- Cross-platform path normalization and file sanitization against path traversal attacks.

---

### ⏱️ `uLBTimers.pas` — Precision Timers & Interruptible Delays
- `SleepWithCheck()` allows threads to pause while remaining instantly responsive to `Terminated` signals.
- High-precision timestamping utilities.

---

### 🚀 `uLBApplicationBoostrap.pas` — Application Initializer
Standard application lifecycle manager for reading INI/XML configurations, setting up logging, and launching integrated web server instances.
