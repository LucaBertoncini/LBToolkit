# 🤖 LBToolkit AI SDK & Prompting Context Guide

This guide is designed as an **AI Context Document** for AI coding assistants (such as Claude, ChatGPT, GitHub Copilot, Cursor). When building applications using **LBToolkit**, include or reference this file alongside [`docs/STYLE_GUIDE.md`](STYLE_GUIDE.md) to ensure accurate unit imports, class interfaces, and idiomatically compliant FreePascal code patterns.

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

## 💻 2. Naming Conventions & Code Style

All generated code MUST follow [`docs/STYLE_GUIDE.md`](STYLE_GUIDE.md):
- **Parameters**: Prefixed with `a` / `A` (e.g., `aLogLevel`, `aSender`, `aMsgText`).
- **Local Variables**: Prefixed with `_` (e.g., `_Res`, `_Msg`, `_Handler`).
- **Private Fields**: Prefixed with `F` and declared `strict private` or `strict protected`.
- **Types**: Prefixed with `T` or `I` (for interfaces), typed pointers with `P` / `p`.

---

## 💻 3. Idiomatic Code Patterns & Boilerplate

### A. Global Logging & Chain of Responsibility Sub-Loggers (`ULBLogger`)

```pascal
uses
  ULBLogger, SysUtils;

type
  // Custom sub-logger interceptor (e.g. Email / Telegram / UI Memo)
  TCustomNotificationLogger = class(TLBBaseLogger)
  public
    function virtualWrite(aLogLevel: Byte; const aSender: String; aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean; override;
  end;

function TCustomNotificationLogger.virtualWrite(aLogLevel: Byte; const aSender: String; aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean;
begin
  Result := True;

  // Intercept critical messages
  if aMsgType = lmt_Critical then
  begin
    // Dispatch alert (Telegram, Email, MQTT, UI Memo, etc.)
    // SendEmailAlert(aSender, aMsgText);

    // Setting aMsgText := '' halts the sublogger chain and skips writing to disk
  end;
end;

procedure ApplicationBootstrap;
var
  _Notifier: TCustomNotificationLogger;
begin
  // Initialize global LBLogger singleton instance
  InitLogger(3, 'app.log');

  // Attach sub-logger without changing any log calls across the application
  _Notifier := TCustomNotificationLogger.Create;
  LBLogger.addAlternativeLogger(_Notifier);

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
  _Res: TJSONObject;
begin
  _Res := TJSONObject.Create;
  _Res.Add('result', 'success');
  aStatusCode := 200;
  Result := _Res;
end;

function TInventoryRouteHandler.UpdateWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Res: TJSONObject;
begin
  _Res := TJSONObject.Create;
  _Res.Add('updated', True);
  aStatusCode := 200;
  Result := _Res;
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
  _CS: TTimedOutCriticalSection;
begin
  _CS := TTimedOutCriticalSection.Create;
  try
    if _CS.Enter(2000) then // Try to acquire lock for max 2000 ms
    begin
      try
        // Critical section logic here
      finally
        _CS.Leave;
      end;
    end;
  finally
    _CS.Free;
  end;
end;
```

---

## ⚠️ 4. Key Architectural Rules for AI Code Generation

1. **Strict Naming Rules**: Always use `aParameter`, `_LocalVariable`, `FField`, `TType`, `PPointer`.
2. **Global Logger Lifecycle**: Use `InitLogger(...)` and `ReleaseLogger()` for global logging management (`LBLogger`).
3. **Sub-Logger Chain of Responsibility**: Sub-loggers derived from `TLBBaseLogger` can inspect, filter, or consume messages (`aMsgText := ''`) before disk writing.
4. **Error Handling Pattern**: Return boolean / status code + log errors via `LBLogger.Write`. Do not throw raw exceptions across operational boundaries.
5. **Thread Lifecycle**: `TLBBaseThread` instances have `FreeOnTerminate := True` set by default.
6. **Interruptible Delays**: Always use `SleepWithCheck(ms)` provided by `uLBTimers` or `uLBBaseThread`.
7. **OpenSSL Multithreading**: Call `InitOpenSSL3()` during app bootstrap when using OpenSSL in multithreaded servers.
8. **No Direct Multipart/Form-Data**: File uploads use raw binary streaming with `X-File-Name` header.
9. **Path Sanitization**: Use `TLBmWsDocumentsFolder` or `SanitizeFileName` from `uLBFileUtils` to protect against path traversal vulnerabilities.
