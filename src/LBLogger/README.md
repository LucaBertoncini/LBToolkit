# 🪵 LBLogger — Thread-Safe, Reactive Logging & Notification Framework

`LBLogger` is a modular, thread-safe logging and notification framework for FreePascal and Lazarus applications. It features asynchronous message queuing, global singleton management, a **Chain of Responsibility** sub-logger pipeline, and dynamic message interception.

---

## 🌟 Key Architectural Features

### 🌐 Global Lifecycle Management (`InitLogger` / `ReleaseLogger`)
`LBLogger` can be initialized globally across an entire application with a single call to `InitLogger(...)` and finalized cleanly with `ReleaseLogger()`. Once initialized, the global `LBLogger` instance is accessible anywhere in your codebase.

```pascal
initialization
  InitLogger(3, 'app.log'); // Level 3 (Debug), writes to app.log

finalization
  ReleaseLogger();
```

---

### ⛓️ Chain of Responsibility & Sub-Logger Interception (`addAlternativeLogger`)
`LBLogger` routes every log message through a chain of sub-loggers (`FAlternativeLoggers`) via `virtualWrite(LogLevel, Sender, MsgType, var MsgText)` before writing to disk.

#### How the Chain Works:
1. When `LBLogger.Write(...)` is called, the message is passed to registered sub-loggers sequentially.
2. **Message Interception / Consumption**: If a sub-logger sets `MsgText := ''` (empty string), **the chain halts immediately** and disk logging for that message is skipped.
3. **Non-Invasive System Expansion**: Because logging calls (`LBLogger.Write`) remain unchanged across your software, you can attach new sub-loggers at any time during development or runtime to route critical errors to **Email**, **Telegram**, **MQTT**, **Database**, or **Desktop UI Memos** without touching existing application logic!

---

## 💻 Custom Sub-Logger Example (e.g. Telegram Alert Logger)

```pascal
unit uTelegramLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger;

type
  TTelegramLogger = class(TLBBaseLogger)
  public
    function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;
  end;

implementation

function TTelegramLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  Result := True;

  // Intercept critical errors and dispatch to Telegram API
  if MsgType = lmt_Critical then
  begin
    SendTelegramNotification('CRITICAL ALERT [' + Sender + ']: ' + MsgText);

    // Optional: Set MsgText := '' if you want to consume the message and prevent it from reaching disk logger
  end;
end;

end.
```

### Attaching Sub-Loggers:
```pascal
var
  TelegramLogger: TTelegramLogger;
begin
  InitLogger(3, 'app.log');

  TelegramLogger := TTelegramLogger.Create;
  LBLogger.addAlternativeLogger(TelegramLogger);
end;
```

---

## 🧩 Architecture Flow

```
   LBLogger.Write('Critical Error!')
                |
                v
     +--------------------+
     | Global LBLogger    |
     +---------+----------+
               |
               v
     [ Sub-Logger Chain ]  <-- (Chain of Responsibility)
     +-------------------------------------------------------+
     | 1. TMemoLogger       (Updates GUI Memo)               |
     | 2. TTelegramLogger   (Sends alert for lmt_Critical)   |
     |    * If MsgText := '' -> STOP CHAIN                   |
     +-------------------------------------------------------+
               |
               v (if MsgText <> '')
     +--------------------+
     | TLBLoggerWriter    | -> Writes to Log File on Disk
     | (Background Thread)|
     +--------------------+
```
