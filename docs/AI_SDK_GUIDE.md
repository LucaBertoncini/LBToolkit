# 🤖 LBToolkit AI SDK & Prompting Context Guide

This guide is designed as an **AI Context Document** for AI coding assistants (such as Claude, ChatGPT, GitHub Copilot, Cursor). When building applications using **LBToolkit**, include or reference this file alongside [`docs/STYLE_GUIDE.md`](STYLE_GUIDE.md) to ensure accurate unit imports, class interfaces, and idiomatically compliant FreePascal code patterns.

---

## 📌 1. Framework Quick Reference

| Feature | Primary Unit | Core Class / Functions |
|---------|--------------|------------------------|
| **Weak References** | `uMultiReference` | `TMultiReferenceObject` |
| **Lifecycle Threading** | `uLBBaseThread` | `TLBBaseThread` |
| **Timeout Critical Section**| `uTimedoutCriticalSection` | `TTimedOutCriticalSection` |
| **Ring Buffer** | `uLBCircularBuffer` | `TLBCircularBuffer`, `TLBCircularBufferThreaded` |
| **Multi-Listener Events** | `uEventsManager` | `TEventsManager` |
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

### A. Weak References & Safe Thread Management (`uMultiReference`, `uLBBaseThread`)

```pascal
uses
  Classes, SysUtils, uLBBaseThread;

type
  TMyThreadOwner = class(TObject)
  strict private
    FWorkerThread: TLBBaseThread;
  public
    procedure StartWorker;
    destructor Destroy; override;
  end;

procedure TMyThreadOwner.StartWorker;
begin
  FWorkerThread := TLBBaseThread.Create;
  // Register reference so FWorkerThread is automatically set to nil when destroyed
  FWorkerThread.AddReference(@FWorkerThread);
  FWorkerThread.Start;
end;

destructor TMyThreadOwner.Destroy;
begin
  // FreeAndNil is sufficient to safely terminate and release worker thread
  if FWorkerThread <> nil then
    FreeAndNil(FWorkerThread);
  inherited Destroy;
end;
```

---

### B. Global Logging & Sub-Logger Chain of Responsibility (`ULBLogger`)

```pascal
uses
  ULBLogger, SysUtils;

type
  TAlertLogger = class(TLBBaseLogger)
  public
    function virtualWrite(aLogLevel: Byte; const aSender: String; aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean; override;
  end;

function TAlertLogger.virtualWrite(aLogLevel: Byte; const aSender: String; aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean;
begin
  Result := False;
  if aMsgType in [lmt_Critical, lmt_Error] then
  begin
    // Send external notification (Telegram, Email, etc.)
    // SendAlert(aSender + ': ' + aMsgText);

    // Setting aMsgText := '' consumes the message and halts the sublogger chain
    aMsgText := '';
    Result := True;
  end;
end;

procedure ApplicationBootstrap;
var
  _AlertLogger: TAlertLogger;
begin
  // InitLogger(aMaxLogLevel, aLogFileName, aUseIntf, aUseTmpFolder)
  // Set aUseTmpFolder := False to use exact file path
  if not InitLogger(3, 'app.log', False, False) then
    Halt(1);

  // Sublogger constructor automatically hooks into main logger chain
  _AlertLogger := TAlertLogger.Create('Alerts');

  LBLogger.Write(1, 'Database', lmt_Info, 'System booted up successfully.');
  LBLogger.Write(1, 'PaymentGate', lmt_Critical, 'Payment provider unreachable!');
end;

procedure ApplicationTeardown;
begin
  ReleaseLogger();
end;
```

---

### C. XML Route Configuration & REST Handlers (`uWebRouteRegistry`)

#### XML Route Definition (`Routes.xml`)
```xml
<?xml version="1.0" encoding="UTF-8"?>
<RouteRegistry>
  <FunctionalArea Code="Inventory" Name="Stock Management">
    <Endpoint Function="search" Method="POST" URI="/api/v1/inventory/search" Kind="Standard" RequiresAuth="true" />
    <Endpoint Function="update" Method="POST" URI="/api/v1/inventory/update" Kind="Standard" RequiresAuth="true" OpType="EDIT_STOCK" />
  </FunctionalArea>
</RouteRegistry>
```

#### Pascal Worker Implementation
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
  inherited Create('Inventory');
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

## ⚠️ 4. Key Architectural Rules for AI Code Generation

1. **Strict Naming Rules**: Always use `aParameter`, `_LocalVariable`, `FField`, `TType`, `PPointer`.
2. **Global Logger Lifecycle**: Use `InitLogger(aLogLevel, aFileName, aUseIntf, aUseTmpFolder)` and `ReleaseLogger()`.
3. **Sub-Logger Chain**: Sub-logger constructor `TLBBaseLogger.Create('Name')` automatically registers with `LBLogger`. Setting `aMsgText := ''` in `virtualWrite` consumes the message and halts disk writing.
4. **Error Handling Pattern**: Return boolean / status code + log errors via `LBLogger.Write`. Do not throw raw exceptions across operational boundaries.
5. **Thread Lifecycle**: `TLBBaseThread` instances have `FreeOnTerminate := True` set by default. Destroy with `FreeAndNil(aThread)`. Derived `Destroy` must call `inherited Destroy` FIRST.
6. **Interruptible Delays**: Always use `PauseFor(ms)` or `SleepWithCheck(ms)` in threads instead of `Sleep()`.
7. **No Unused Prototype Classes**: Do not use or reference `TFPHTTPRequestProcessor`.
