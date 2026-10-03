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
| **Logging Engine** | `ULBLogger` | `TLBLogger`, `TLBBaseLogger` |
| **Python / JS Gateway** | `uLBWebPrismApplication` | `TLBWebPrismApplication` |
| **IPC Shared Memory** | `uIPCUtils` | `AllocateSharedMemory`, `TLBNamedSemaphore` |
| **SQLite Wrapper** | `SQLiteWrapper` | `TSQLiteDatabase` |
| **OpenSSL 3 Support** | `uLBSSLConfig` | `InitOpenSSL3` |

---

## 💻 2. Idiomatic Code Patterns & Boilerplate

### A. XML Route Configuration & REST Handlers (`uWebRouteRegistry`)

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

### B. Creating a Daemon Worker Thread (`TLBBaseThread`)

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

### C. Deadlock-Resistant Lock (`TTimedOutCriticalSection`)

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

### D. Setting up `LBmicroWebServer` (HTTP & Raw File Upload)

```pascal
uses
  Classes, SysUtils, uLBmicroWebServer, uHTTPConsts;

procedure StartServer;
var
  Server: TLBmicroWebServer;
begin
  Server := TLBmicroWebServer.Create(nil);
  try
    Server.Port := 8080;
    Server.DocumentsFolder := './www';
    Server.UploadEndpoint := '/api/upload'; // Endpoint for raw binary streaming uploads
    Server.Active := True;

    WriteLn('HTTP Server started on port 8080');
  finally
    // Server lifecycle management
  end;
end;
```

---

## ⚠️ 3. Key Architectural Rules for AI Code Generation

1. **REST Worker Signatures**: Pascal REST workers must match the signature defined for their family (`TStandardWorker`, `TFileDownloadWorker`, `TAuthWorker`, `TUploadWorker`).
2. **Thread Lifecycle**: `TLBBaseThread` instances have `FreeOnTerminate := True` set by default.
3. **Interruptible Delays**: Always use `SleepWithCheck(ms)` provided by `uLBTimers` or `uLBBaseThread`.
4. **OpenSSL Multithreading**: Call `InitOpenSSL3()` during app bootstrap when using OpenSSL in multithreaded servers.
5. **No Direct Multipart/Form-Data**: File uploads use raw binary streaming with `X-File-Name` header.
6. **Path Sanitization**: Use `TLBmWsDocumentsFolder` or `SanitizeFileName` from `uLBFileUtils` to protect against path traversal vulnerabilities.
