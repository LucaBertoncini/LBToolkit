# 🔧 TLBBaseThread — Simplified Thread Management for FreePascal

**TLBBaseThread** provides a robust and developer-friendly abstraction for managing threads in FreePascal.  
It is designed to simplify the lifecycle of `TThread`-based classes—especially in scenarios where safe termination, resource cleanup, and pointer management are essential.

## ✨ Key Features

- ✅ Automatic termination signaling via `RTLEventSetEvent`
- ⏱ Timeout-aware cleanup with `RTLEventWaitFor` during destruction
- 🧹 Safe pointer invalidation via `TMultiReferenceObject`
- 💤 Cooperative pause handling that reacts to `Terminate`
- 📋 Customizable thread naming and structured debug logging

This architecture reduces complexity and eliminates common threading pitfalls like dangling pointers, deadlocks, and unsafe destruction.

---

## 🚀 Typical Usage

```pascal
var
  aThread: TLBBaseThread;
begin
  aThread := TLBBaseThread.Create;
  aThread.AddReference(@aThread); // Ensures pointer safety on shutdown
  aThread.Start;

  // ... thread does its work ...

  FreeAndNil(aThread); // Safe even if the thread has already terminated
end;

