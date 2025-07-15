# 🧪 Unit Tests for TEventsManager

This folder contains comprehensive unit tests for the `TEventsManager` class from `LBToolkit`.
The tests validate core functionality, concurrent event handling, and lifecycle behavior of event listeners.

## 📦 Files Included

| File                             | Description                                                   |
|----------------------------------|---------------------------------------------------------------|
| `uEventsManagerTest.pas`         | Basic functionality and correctness tests                     |
| `uEventsManagerLifecycleTest.pas`| Advanced lifecycle and concurrency stress tests               |

---

## ✅ `uEventsManagerTest.pas`

Unit tests that verify:

- Event registration and dispatch (`emm_Events`)
- Listener activation and deactivation
- Callback-style listener execution (`emm_Callbacks`)
- Single-callback behavior (`emm_EventsSingleCallback`)
- Listener removal and lifecycle safety

### 🔍 Example Tests

- `TestBasicNotify` → Ensures event listener is invoked
- `TestDisabledEvent` → Validates that disabled events do not trigger
- `TestCallbackMode` → Verifies low-level C-style callback execution
- `TestSingleCallback` → Tests named event dispatch to single listener
- `TestRemoveListener` → Confirms correct listener cleanup

---

## 🚦 `uEventsManagerLifecycleTest.pas`

Advanced tests focused on:

- Multi-threaded event dispatch
- Automatic deregistration of listeners upon owner destruction
- Race condition simulation between raising and removing listeners

### 🔍 Example Tests

- `TestConcurrentRaiseEvent` → Launches 40 threads to raise the same event
- `TestRemoveListenerWhileRunning` → Removes listeners during active dispatch
- `TestAddListener` → Verifies deduplication of listener registration
- `TestRemoveListener` → Checks for proper cleanup after owner deletion

---

## 🧠 Testing Philosophy

These units are designed to:

- Validate correctness of `TEventsManager` under stress
- Simulate real-world concurrency and asynchronous teardown
- Ensure safe interaction between objects across lifecycle phases

Built with **FPCUnit**, they are fully integrable into Lazarus using the built-in test runner.

---

## 🚀 How to Run

You can compile and run the tests via Lazarus or command-line:

```bash
./EventsManager_UnitTest
