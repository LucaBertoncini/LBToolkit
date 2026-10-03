# 🪵 LBLogger — Thread-Safe, Reactive Logging Framework

`LBLogger` is a modular, thread-safe logging framework for FreePascal and Lazarus applications. It features asynchronous message queuing, sub-logger delegation, configurable message field formatting, and UI integration.

---

## ✨ Features

- **Thread-Safe & Non-Blocking**: Asynchronous message dispatching ensures worker threads are never blocked by file or I/O operations.
- **Hierarchical Sub-Loggers**: Attach multiple child loggers (e.g., file logger, event log, UI memo logger, notification bridges) dynamically without altering caller code.
- **Granular Log Levels**: Filter by `lmt_Error`, `lmt_Info`, `lmt_Debug`, `lmt_Warning`, `lmt_Critical`, `lmt_Report`, and up to 10 user-customizable types (`lmt_User1`..`lmt_User10`).
- **File Rotation & Size Limits**: `TLBLogger` automatically truncates or rolls log files when `MaxFileSize` limits are reached.
- **Field Formatting**: Configurable inclusion of timestamps, Thread IDs, Process IDs (PID), message types, and calling routines.

---

## 💻 Usage Example

```pascal
uses
  ULBLogger, uLBLoggerEx;

var
  Logger: TLBLogger;
begin
  Logger := TLBLogger.Create('application.log');
  try
    Logger.MaxLogLevel := 3; // Debug
    Logger.SourceMaxLength := 20;

    // Write log messages
    Logger.logWrite(1, 'MainModule', lmt_Info, 'Application started successfully.');
    Logger.logWrite(1, 'Database', lmt_Error, 'Failed to connect to DB.');
  finally
    Logger.Free;
  end;
end;
```

---

## 🧩 Architecture

```
                  +---------------------+
                  |   ILBBaseLogger     |
                  +----------+----------+
                             |
                   +---------+---------+
                   |   TLBBaseLogger   | (Abstract Base)
                   +---------+---------+
                             |
             +---------------+---------------+
             |                               |
     +-------+-------+               +-------+-------+
     |   TLBLogger   |               |  Sub-Loggers  |
     | (File Logger) |               | (UI/EventLog) |
     +---------------+               +---------------+
```
