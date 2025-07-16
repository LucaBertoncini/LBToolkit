# 🧪 Unit Tests for LBLogger

This directory contains unit tests for the `LBLogger` module in LBToolkit.  
The tests are written using [FPCUnit](https://wiki.freepascal.org/fpcunit) and validate the core functionality, concurrency safety, and lifecycle behavior of the asynchronous logging system.

---

## 📄 Included File

| File               | Description                                      |
|--------------------|--------------------------------------------------|
| `uLBLoggerTest.pas`| Test suite for `TLBLogger` and related classes   |

---

## ✅ What’s Covered

| Category               | Test Name                          | Purpose                                                  |
|------------------------|------------------------------------|----------------------------------------------------------|
| Initialization         | `TestInitializeLogger`             | Verifies logger setup via `InitLogger()`                 |
| Basic Logging          | `TestWriteBasicMessage`            | Writes a simple log entry                                |
| Filtering              | `TestLogLevelFiltering`            | Ensures messages above `MaxLogLevel` are ignored         |
| Message Types          | `TestEnabledMessageTypes`          | Filters messages by `MsgType`                            |
| Formatting             | `TestMessageFormatOutput`          | Validates output string from `ElaborateMessageToWrite`   |
| File Rotation          | `TestLogFileRotation`              | Forces rotation via `MaxFileSize` and checks renaming    |
| Alternative Loggers    | `TestAlternativeLoggerIntercept`   | Verifies interception and suppression of messages        |
| Concurrency            | `TestConcurrentWriteCalls`         | Launches 40 threads to write concurrently                |
| Lifecycle & Flush      | `TestFlushOnRelease`               | Ensures messages are flushed before shutdown             |

---

## 🧵 Concurrency

The logger is tested under multi-threaded conditions using `TLogThread`, simulating simultaneous calls to `LBLogger.Write`.  
This validates thread safety and internal queuing behavior.

---

## 🔄 File Rotation

The test suite forces log rotation by setting `MaxFileSize := 1024` and writing hundreds of entries.  
It then checks for the existence of renamed log files using pattern matching.

---

## 🧠 Testing Philosophy

These tests aim to:

- Confirm correct filtering and formatting of log messages
- Validate thread-safe logging under stress
- Ensure proper cleanup and file flushing during shutdown
- Simulate realistic usage scenarios including rotation and interception

---

## ▶️ How to Run

Compile and run the test using Lazarus or command line:

```bash
./LBLoggerTestRunner --format=plain --all

