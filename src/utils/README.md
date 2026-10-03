# LBToolkit — Foundations (`src/utils/`)

Folder: `src/utils/` (including `uMultiReference.pas`, `uLBBaseThread.pas`, `uTimedoutCriticalSection.pas`, `uEventsManager.pas`, `uLBCircularBuffer.pas`, `uLBSplitQueue.pas`, `uLBSignalManager.pas`, `uLBTimers.pas`, `uExternalLibrariesManager.pas`, `uIPCUtils.pas`)

These shared primitives form the core contract across every module of the toolkit, ensuring thread-safety, zero dangling pointers, and deterministic lifecycle management.

---

## 1. `TMultiReferenceObject` — Weak References (`uMultiReference.pas`)

**Purpose.** Eliminates the dangling-pointer problem at its root. An object that may be referenced by several owners registers **references to the owners' fields**; when it is destroyed, each of those fields is automatically set to `nil`.

```pascal
type
  pObject = ^TObject;

  TMultiReferenceObject = class(TObject)
    // FReferences: a TThreadList of pointers to variables pointing at the object
  public
    procedure ClearReferences(); // pObject(List[i])^ := nil for all
    function AddReference(aReference: pObject): Boolean;
    procedure RemoveReference(aReference: pObject);
  end;
```

### Typical Usage
A class field (`FWorker`) receives a reference to the address of the field itself:

```pascal
FWorker := TMyThread.Create;
FWorker.AddReference(@FWorker); // The thread knows how to zero FWorker upon destruction

if FWorker <> nil then          // Test is always reliable across threads
  FWorker.DoSomething;
```

When the object dies (even via `FreeOnTerminate`), its `Destroy` runs `ClearReferences` and the owner's variable becomes `nil`. The reference list uses a thread-safe `TThreadList`. For threads, there is a typed variant `pThread = ^TThread` integrated into `TLBBaseThread`.

---

## 2. `TLBBaseThread` — Thread Lifecycle Management (`uLBBaseThread.pas`)

**Purpose.** Makes thread management deterministic with a single rule: **to destroy a thread, `FreeAndNil(aThread)` is enough**.

```pascal
TLBBaseThread = class(TThread)
  strict protected
    FExitFromPauseEvent : PRTLEvent;     // Interrupts pauses
    FReferences : TMultiReferenceObject; // Zeroes owners' fields
    procedure PauseFor(aMSecs: Integer);  // Reactive alternative to Sleep
    class function getThreadName(): String; virtual;
  public
    constructor Create(); virtual;         // FreeOnTerminate := True
    destructor Destroy(); override;        // Defensive Terminate + WaitFor
    function WaitFor(): Integer; reintroduce;
    procedure Terminate; reintroduce; virtual;
    function AddReference(aReference: pThread): Boolean;
    procedure RemoveReference(aReference: pThread);
    procedure setThreadName(const aValue: AnsiString);
    property OnAsyncTerminate: TNotifyEvent write FOnAsyncTerminate;
end;
```

### Guarantees & Contract:
- **`FreeOnTerminate := True` by default**: The constructor creates the thread suspended with `FreeOnTerminate := True`.
- **`Terminate`**: Overridden to signal `FExitFromPauseEvent`, waking up threads blocked in `PauseFor(aMSecs)` instantly.
- **`WaitFor`**: Resets `FreeOnTerminate := False` before waiting so the caller owns the final instance cleanup.
- **`Destroy` Guarantee**: Sets `FreeOnTerminate := False`, invokes `OnAsyncTerminate`, calls `Terminate` and `WaitFor` if active, and runs `ClearReferences`.
- **Descendant Contract**: A derived class's `Destroy` **must call `inherited Destroy` FIRST** before freeing local resources, ensuring the background thread execution has fully terminated.

---

## 3. `TTimedOutCriticalSection` — Lock with Timeout & Tracing (`uTimedoutCriticalSection.pas`)

**Purpose.** Protects critical sections without blocking processes forever.

```pascal
function Acquire(const aFunctionName: String; aMaxWaitTimeMs: Integer = 3000): Boolean;
procedure Release();
```

- Polling loop trying every 10 ms until timeout; returns `False` on failure.
- On timeout, logs the requesting routine name **and the last owner (`FLastOwner`)** to pinpoint deadlocks immediately.
- **Mandatory Pattern**:
  ```pascal
  if FCS.Acquire('TClass.Method') then
  begin
    try
      // Critical section
    finally
      FCS.Release;
    end;
  end;
  ```

---

## 4. `TEventsManager` — Synchronous Multi-Listener Events (`uEventsManager.pas`)

**Purpose.** Exposes named events to which other objects or C callbacks subscribe, unsubscribing **automatically** when either side dies.

### Key Behavior: Synchronous Notification
`RaiseEvent` invokes listeners *in the thread calling `RaiseEvent`*, holding its own `TTimedOutCriticalSection`.
- Listeners run in the caller's thread context.
- Listener exceptions are caught and logged, never propagated.

### Three Operating Modes:
| Mode | Listener Signature | Usage |
|---|---|---|
| `emm_Events` (default) | `TNotifyEvent(Sender)` | Pure Pascal, multi-linked managers |
| `emm_Callbacks` | `TCallbackProcedure(anOpaquePointer)` | C / external library integration |
| `emm_EventsSingleCallback` | `(Sender, EventName)` | One handler for multiple events |

### Mutual Automatic Deregistration:
Passing a `TEventsManager` as counterparty in `AddEventListener` registers both managers on each other's special `EM_Destroy` event. When either manager dies, `EM_Destroy` fires and drops orphaned listeners automatically.

---

## 5. Buffers and Queues

### `TLBCircularBuffer` (`uLBCircularBuffer.pas`)
Fixed-size ring memory buffer for raw socket/serial streams:
- Writing: `Write`, `WriteFromSocket`, `WriteFromSerial`.
- Reading: `Read` (buffer or `TStream`), `Peek`/`PeekByte` (non-consuming read), `Skip`, `Seek`.
- Searching: `FindPattern`, `FindByte` for protocol framing.

### `TLBCircularBufferThreaded`
Thread-safe wrapper over `TLBCircularBuffer` guarded by `TTimedOutCriticalSection`. Derives from `TMultiReferenceObject`. Default choice across producer/consumer thread boundaries.

### `TLBSplitQueue` (`uLBSplitQueue.pas`)
Linked-list queue where a **tail portion can be detached in O(1)** without copying nodes (`SplitFrom(Node, QueueClass)`). Useful for bulk re-ordering/re-classifying accumulated items.

---

## 6. Signals, Timers & Interruptible Sleep

- **`TSignalManager`** (`uLBSignalManager.pas`): `sigaction` wrapper; provides `BrokenPipeSignalManager` (SIGPIPE) and `TerminateSignalManager` (SIGTERM).
- **`TTimeoutTimer`** (`uLBTimers.pas`): Monotonic tick countdown timer (`Expired()`, `Remain`).
- **`TInterruptableSleep`**: Sleep that wakes up early on `wakeUp()` call (pipe on Linux, event handle on Windows).

---

## 7. Cross-Platform IPC (`uIPCUtils.pas`)

- **Shared Memory**: `TSharedMemory` (Windows `CreateFileMapping` / Linux System V or POSIX `/dev/shm`). `AllocateSharedMemory`, `closeSharedMemory`.
- **`TLBNamedSemaphore`**: Cross-platform named semaphore (`Wait(aTimeoutMs)`, `Signal`).
- **`TLocalSocket` / `TLocalServerSocket`**: Unix domain sockets / named pipes.

---

## 8. Summary Selection Matrix

| Need | Primitive |
|---|---|
| Thread with multiple owners or scattered references | `TLBBaseThread` + `AddReference` |
| Wait for worker without deadlock risks | `TTimedOutCriticalSection` (timeout + owner tracing) |
| Multi-listener event bus with auto-cleanup | `TEventsManager` |
| Producer-Consumer queue across threads | `TLBCircularBufferThreaded` |
| Stream protocol parsing | `TLBCircularBuffer` (`FindPattern`, `Peek`) |
| Bulk queue re-ordering | `TLBSplitQueue.SplitFrom` |
| Operational timeouts / heartbeats | `TTimeoutTimer`, `TInterruptableSleep` |
| Inter-process communication | `uIPCUtils` (shared memory + semaphores / local socket) |
| Dynamic library loading | `TExternalLibraryLoader` (`uExternalLibrariesManager`) |
