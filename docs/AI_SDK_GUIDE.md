# 🤖 LBToolkit AI SDK & Prompting Context Guide

This guide is designed as an **AI Context Document** for AI coding assistants (such as Claude, ChatGPT, GitHub Copilot, Cursor). When building applications using **LBToolkit**, include or reference this file to provide the AI with accurate unit imports, class interfaces, and idiomatically correct FreePascal code patterns.

---

## 📌 1. Framework Quick Reference

| Feature | Primary Unit | Core Class / Functions |
|---------|--------------|------------------------|
| **Lifecycle Threading** | `uLBBaseThread` | `TLBBaseThread` |
| **Timeout Critical Section**| `uTimedoutCriticalSection` | `TTimedOutCriticalSection` |
| **Ring Buffer** | `uLBCircularBuffer` | `TLBCircularBuffer` |
| **HTTP / REST Server** | `uLBmicroWebServer` | `TLBmicroWebServer`, `THTTPRequestManager` |
| **XML Route & Auth Registry** | `uWebRouteRegistry` | `TWebRouteRegistry`, `TRouteHandlerBase`, `TWebRouteModule` |
| **WebSocket Server** | `uWebSocketManagement` | `TLBWebSocketSession` |
| **WebSocket Client** | `uLBWebSocketClient` | `TLBWebSocketClient` |
| **Global Logging Engine** | `ULBLogger` | `InitLogger`, `ReleaseLogger`, `LBLogger`, `TLBBaseLogger` |
| **Python / JS Gateway** | `uLBWebPrismApplication` | `TLBWebPrismApplication` |
| **IPC Shared Memory** | `uIPCUtils` | `AllocateSharedMemory`, `TLBNamedSemaphore` |
| **SQLite Wrapper** | `SQLiteWrapper` | `TSQLiteDatabase` |
| **OpenSSL 3 Support** | `uLBSSLConfig` | `InitOpenSSL3` |

---

## 💻 2. Idiomatic Code Patterns & Boilerplate

### A. Global Logging & Chain of Responsibility Sub-Loggers (`ULBLogger`)

```pascal
uses
  ULBLogger, SysUtils;

type
  // Custom sub-logger interceptor (e.g. Email / Telegram / UI Memo)
  TCustomNotificationLogger = class(TLBBaseLogger)
  public
    function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;
  end;

function TCustomNotificationLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  Result := True;

  // Intercept critical messages
  if MsgType = lmt_Critical then
  begin
    // Dispatch alert (Telegram, Email, MQTT, UI Memo, etc.)
    // SendEmailAlert(Sender, MsgText);

    // Setting MsgText := '' halts the sublogger chain and skips writing to disk
  end;
end;

procedure ApplicationBootstrap;
var
  Notifier: TCustomNotificationLogger;
begin
  // Initialize global LBLogger singleton instance
  InitLogger(3, 'app.log');

  // Attach sub-logger without changing any log calls across the application
  Notifier := TCustomNotificationLogger.Create;
  LBLogger.addAlternativeLogger(Notifier);

  // Usage anywhere in application:
  LBLogger.Write(1, 'Database', lmt_Info, 'System booted up successfully.');
  LBLogger.Write(1, 'PaymentGate', lmt_Critical, 'Payment provider unreachable!');
end;

procedure ApplicationTeardown;
begin
  ReleaseLogger(); // Safely closes log thread and flushes messages
end;
```

---

### B. XML Route Configuration & REST Handlers (`uWebRouteRegistry`)

#### XML Route Definition (`Routes.xml`)
```xml
<?xml version="1.0" encoding="UTF-8"?>
<RouteRegistry>
  <FunctionalArea Code="Inventory" Name="Stock Management">
    <!-- Public search endpoint -->
    <Endpoint Function="search" Method="POST" URI="/api/v1/inventory/search" Kind="Standard" RequiresAuth="true" />
    <!-- Protected write endpoint requiring 'EDIT_STOCK' OpType permission -->
    <Endpoint Function="update" Method="POST" URI="/api/v1/inventory/update" Kind="Standard" RequiresAuth="true" OpType="EDIT_STOCK" />
  </FunctionalArea>
</RouteRegistry>
```

#### Pascal Worker Class Implementation
```pascal
unit uInventoryRouteHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWebRouteRegistry, fpjson;

type
  TInventoryRouteHandler = class(TRouteHandlerBase)
  public
    constructor Create;
    function SearchWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
    function UpdateWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
  end;

implementation

constructor TInventoryRouteHandler.Create;
begin
  inherited Create('Inventory'); // AreaCode matching XML <FunctionalArea Code="Inventory">

  RegisterStandardWorker('search', @SearchWorker);
  RegisterStandardWorker('update', @UpdateWorker);
end;

function TInventoryRouteHandler.SearchWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  Res: TJSONObject;
begin
  Res := TJSONObject.Create;
  Res.Add('result', 'success');
  aStatusCode := 200;
  Result := Res;
end;

function TInventoryRouteHandler.UpdateWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  Res: TJSONObject;
begin
  Res := TJSONObject.Create;
  Res.Add('updated', True);
  aStatusCode := 200;
  Result := Res;
end;

end.
```

---

### C. Creating a Daemon Worker Thread (`TLBBaseThread`)

```pascal
unit uMyWorkerThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBBaseThread;

type
  TMyWorkerThread = class(TLBBaseThread)
  protected
    procedure InternalExecute; override;
  public
    constructor Create;
  end;

implementation

constructor TMyWorkerThread.Create;
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := True;
end;

procedure TMyWorkerThread.InternalExecute;
begin
  while not Terminated do
  begin
    // Perform periodic work

    // Use interruptible sleep instead of standard Sleep()
    SleepWithCheck(100);
  end;
end;

end.
```

---

### D. Deadlock-Resistant Lock (`TTimedOutCriticalSection`)

```pascal
uses
  uTimedoutCriticalSection;

var
  CS: TTimedOutCriticalSection;
begin
  CS := TTimedOutCriticalSection.Create;
  try
    if CS.Enter(2000) then // Try to acquire lock for max 2000 ms
    begin
      try
        // Critical section logic here
      finally
        CS.Leave;
      end;
    end;
  finally
    CS.Free;
  end;
end;
```

---

## ⚠️ 3. Key Architectural Rules for AI Code Generation

1. **Global Logger Lifecycle**: Use `InitLogger(...)` and `ReleaseLogger()` for global logging management (`LBLogger`).
2. **Sub-Logger Chain of Responsibility**: Sub-loggers derived from `TLBBaseLogger` can inspect, filter, or consume messages (`MsgText := ''`) before disk writing.
3. **REST Worker Signatures**: Pascal REST workers must match the signature defined for their family (`TStandardWorker`, `TFileDownloadWorker`, `TAuthWorker`, `TUploadWorker`).
4. **Thread Lifecycle**: `TLBBaseThread` instances have `FreeOnTerminate := True` set by default.
5. **Interruptible Delays**: Always use `SleepWithCheck(ms)` provided by `uLBTimers` or `uLBBaseThread`.
6. **OpenSSL Multithreading**: Call `InitOpenSSL3()` during app bootstrap when using OpenSSL in multithreaded servers.
7. **No Direct Multipart/Form-Data**: File uploads use raw binary streaming with `X-File-Name` header.
8. **Path Sanitization**: Use `TLBmWsDocumentsFolder` or `SanitizeFileName` from `uLBFileUtils` to protect against path traversal vulnerabilities.
