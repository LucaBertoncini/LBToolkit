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
| **Route Registry** | `uWebRouteRegistry` | `TWebRouteRegistry` |
| **WebSocket Server** | `uWebSocketManagement` | `TLBWebSocketSession` |
| **WebSocket Client** | `uLBWebSocketClient` | `TLBWebSocketClient` |
| **Logging Engine** | `ULBLogger` | `TLBLogger`, `TLBBaseLogger` |
| **Python / JS Gateway** | `uLBWebPrismApplication` | `TLBWebPrismApplication` |
| **IPC Shared Memory** | `uIPCUtils` | `AllocateSharedMemory`, `TLBNamedSemaphore` |
| **SQLite Wrapper** | `SQLiteWrapper` | `TSQLiteDatabase` |
| **OpenSSL 3 Support** | `uLBSSLConfig` | `InitOpenSSL3` |

---

## 💻 2. Idiomatic Code Patterns & Boilerplate

### A. Creating a Daemon Worker Thread (`TLBBaseThread`)

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

### B. Deadlock-Resistant Lock (`TTimedOutCriticalSection`)

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
    end
    else
    begin
      // Handle lock timeout gracefully
    end;
  finally
    CS.Free;
  end;
end;
```

---

### C. Setting up `LBmicroWebServer` (HTTP & Raw File Upload)

```pascal
uses
  Classes, SysUtils, uLBmicroWebServer, uHTTPConsts;

procedure OnGET(Sender: TObject; const AURI: String; Params: TStringList; ResponseStream: TMemoryStream; var ContentType: String; var Handled: Boolean);
begin
  if AURI = '/api/status' then
  begin
    ContentType := 'application/json';
    WriteStringToStream(ResponseStream, '{"status":"ok"}');
    Handled := True;
  end;
end;

procedure StartServer;
var
  Server: TLBmicroWebServer;
begin
  Server := TLBmicroWebServer.Create(nil);
  try
    Server.Port := 8080;
    Server.DocumentsFolder := './www';
    Server.UploadEndpoint := '/api/upload'; // Endpoint for raw binary streaming uploads
    Server.OnGETRequest := @OnGET;

    Server.Active := True;
    WriteLn('HTTP Server started on port 8080');
  finally
    // Keep running...
  end;
end;
```

---

### D. WebSocket Client Connection (`TLBWebSocketClient`)

```pascal
uses
  uLBWebSocketClient, uHTTPConsts;

type
  TWSHandler = class
    procedure OnTextMessage(Sender: TObject; isLastFrame: Boolean; aDataType: TWebSocketFrameType; aBuffer: pByte; aBufferLen: Int64);
  end;

procedure TWSHandler.OnTextMessage(Sender: TObject; isLastFrame: Boolean; aDataType: TWebSocketFrameType; aBuffer: pByte; aBufferLen: Int64);
var
  Msg: String;
begin
  SetString(Msg, PChar(aBuffer), aBufferLen);
  WriteLn('Received WS Message: ', Msg);
end;

procedure ConnectWS;
var
  WSClient: TLBWebSocketClient;
  Handler: TWSHandler;
begin
  Handler := TWSHandler.Create;
  WSClient := TLBWebSocketClient.Create;

  WSClient.RemoteConnectionData.Host := 'echo.websocket.events';
  WSClient.RemoteConnectionData.Port := 443;
  WSClient.RemoteConnectionData.UseSSL := True;
  WSClient.URI := '/';
  WSClient.OnWebSocketTextMessage := @Handler.OnTextMessage;

  WSClient.Start;
  WSClient.AddWebSocketMessageToSend('Hello from LBToolkit!');
end;
```

---

### E. Ring Buffer Data Stream Handling (`TLBCircularBuffer`)

```pascal
uses
  uLBCircularBuffer, Classes;

var
  RingBuffer: TLBCircularBuffer;
  DataStream: TMemoryStream;
begin
  RingBuffer := TLBCircularBuffer.Create(65536); // 64KB Ring Buffer
  try
    RingBuffer.Write('GET / HTTP/1.1'#13#10, 16);

    // Dump circular buffer content directly to a stream without double allocation
    DataStream := TMemoryStream.Create;
    try
      RingBuffer.WriteToStream(DataStream, RingBuffer.AvailableData);
    finally
      DataStream.Free;
    end;
  finally
    RingBuffer.Free;
  end;
end;
```

---

## ⚠️ 3. Key Architectural Rules for AI Code Generation

1. **Thread Lifecycle**: `TLBBaseThread` instances have `FreeOnTerminate := True` set by default. Do not manually free thread instances unless you explicitly override this behavior.
2. **Interruptible Delays**: Never use `SysUtils.Sleep(ms)` inside worker loops. Always use `SleepWithCheck(ms)` provided by `uLBTimers` or `uLBBaseThread` so threads terminate cleanly.
3. **OpenSSL Multithreading**: When using HTTPS or Secure WebSockets in multithreaded environments, ensure `InitOpenSSL3()` is invoked during app bootstrap.
4. **No Direct Multipart/Form-Data**: File uploads use raw binary streaming (`UploadEndpoint` with `X-File-Name` header) to maintain low memory usage and high streaming performance.
5. **Path Sanitization**: Always use `TLBmWsDocumentsFolder` or `SanitizeFileName` from `uLBFileUtils` when dealing with user-provided path inputs to prevent directory traversal vulnerabilities.
