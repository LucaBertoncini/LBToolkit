# 🌐 LBmicroWebServer — Embedded-Friendly HTTP & WebSocket Web Server

Module: `src/LBmicroWebServer/`

`LBmicroWebServer` is a multithreaded HTTP/1.1 and WebSocket (RFC 6455) server written in Object Pascal (FreePascal). It is engineered for low memory footprint, high stability, and seamless extension into gateways, REST microservices, and file servers.

**Dependencies**: Synapse network transport (`blcksock`); for HTTPS `ssl_openssl3` (OpenSSL 3.0+); FCL `System.NetEncoding`.

**Performance**: Measured at ~1800 requests/second with Apache Bench (5000 requests, Windows, hardware dependent).

| Unit | Contents |
|---|---|
| `uLBmicroWebServer.pas` | `TLBmicroWebServer`, listener (`TLBmWsListener`), request manager (`THTTPRequestManager`) |
| `uHTTPRequestParser.pas` | HTTP request parser using `TLBCircularBuffer` |
| `uWebRouteRegistry.pas` | Declarative XML endpoint registry and REST module (`TWebRouteModule`) |
| `uLBWebServerConfigurationLoader.pas` | Configuration loading (INI, XML, callbacks) |
| `uLBmWsFileManager.pas`, `uLBmWsDocumentsFolder.pas` | File serving and sandboxed document root |
| `uWebSocketManagement.pas` | WebSocket handshake and session management (`TLBWebSocketSession`) |
| `src/shared/LBToolkit/Toolkit_CAPI_microWebServer.pas` | C API interface |

---

## 1. Thread Architecture

```
TLBmicroWebServer           owner: configures, starts (Active := True), stops (Stop)
 └── TLBmWsListener         (TLBBaseThread) opens port and accepts incoming connections
      └── THTTPRequestManager   (TLBBaseThread) one thread per active connection
```

- **Listener (`TLBmWsListener`)**: Opens socket listener; retries binding every 5 seconds on failure. Listens and spawns a `THTTPRequestManager` worker thread for each connection.
- **Request Manager (`THTTPRequestManager`)**: One worker thread per connection. Handles HTTP/1.1 `Connection: keep-alive` (`Keep-Alive: timeout=10, max=100`).
- **Clean Teardown**: `FreeAndNil(WebServer)` gracefully stops listener and connection threads.

---

## 2. Request Dispatch Flow

| HTTP Method | Dispatch Behavior |
|---|---|
| `GET` | 1. WebSocket Upgrade (`Upgrade: websocket`) -> Opens WebSocket session.<br>2. `GET /test` -> Returns built-in test page.<br>3. `ApiPathPrefix` match -> Bypasses filesystem and routes directly to processor chain.<br>4. No Query Parameters -> Looks for static file in `DocumentsFolder`.<br>5. Query Parameters present or File 404 -> Falls through to processing chain. |
| `HEAD` | Headers only for static files in `DocumentsFolder`. |
| `POST`, `PUT`, `DELETE`, `PATCH` | Never static files -> Forwarded directly to processing chain. |
| `OPTIONS` | CORS preflight: `Access-Control-Allow-Origin: *`, allowed headers `Content-Type`. |

> **Security Note**: `DocumentsFolder` enforces strict path traversal protection, rejecting paths with `..` and verifying resolved paths remain within the sandbox.

---

## 3. The Processing Chain (`TRequestChainProcessor`)

```pascal
TRequestChainProcessor = class(TObject) // Must be thread-safe and stateless
strict protected
  function DoProcessRequest(
    aRequestManager: THTTPRequestManager;
    aHTTPParser: THTTPRequestParser;
    aResponseHeaders: TStringList;
    var aResponseData: TMemoryStream;
    out aResponseCode: Integer): Boolean; virtual; abstract;
end;
```

- Processors are shared across connection threads: **must be stateless per-request**.
- `WebServer.addChainProcessor(aProcessor, aAsFirst)`: The server takes ownership of the processor and frees it in `Destroy`.
- Provided Processors:
  - `TWebRouteModule` — Declarative REST routing engine.
  - `TBridgeChainModule` — LBWebPrism Python/Node.js script dispatch gateway.

---

## 4. Declarative XML Route Registry & Permission System

### XML Routes Configuration (`routes.xml`)
```xml
<WebRoutes>
  <Applications>
    <Application Code="gest" Name="Gestionale" Home="/gest/index.html"/>
  </Applications>

  <FunctionalArea Code="Users" Description="Utenti" App="gest">
    <Endpoint Function="GetUser"  URI="/api/users/get"  Method="POST" OpType="Read"/>
    <Endpoint Function="Login"    URI="/api/login"      Kind="Auth"   RequiresAuth="false"/>
    <Endpoint Function="Export"   URI="/api/users/csv"  Method="GET"  Kind="FileDownload" OpType="Read"/>
    <Endpoint Function="Upload"   URI="/api/upload"     Kind="Upload" RequiresAuth="true" />
  </FunctionalArea>
</WebRoutes>
```

### The 5 Endpoint Families:
| `Kind` | Worker Signature | Purpose |
|---|---|---|
| `Standard` | `TStandardWorker` | JSON request body in, JSON response out |
| `FileDownload` | `TFileDownloadWorker` | File path & suggested filename returned for streaming download |
| `Auth` | `TAuthWorker` | Login/logout with direct response headers access |
| `Upload` | `TUploadWorker` | Receives raw binary file uploaded to temporary disk storage |
| `Proxy` | None | Signed forwarding to remote server (`Target` attribute) |

### Worker Signatures:
```pascal
TStandardWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData of object;

TFileDownloadWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
  aHTTPParser: THTTPRequestParser; out aFilePath, aSuggestedFileName: String;
  out aStatusCode: Integer): Boolean of object;

TAuthWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; aHTTPParser: THTTPRequestParser;
  aResponseHeaders: TStringList; out aStatusCode: Integer): TJSONData of object;

TUploadWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestManager: THTTPRequestManager; aHTTPParser: THTTPRequestParser;
  aResponseHeaders: TStringList; var aResponseData: TMemoryStream;
  out aStatusCode: Integer): Boolean of object;
```

---

## 5. Startup Sequence for `TWebRouteModule`

```pascal
var
  _Route: TWebRouteModule;
begin
  _Route := TWebRouteModule.Create;
  _Route.LoadRoutesFromXML('./routes.xml');
  _Route.RegisterHandler(TUserAreaHandler.Create); // Controller derived from TRouteHandlerBase
  _Route.OnAuthenticateRequest := @Self.AuthenticateRequest;
  _Route.OnRetrieveGrantedPermissions := @Self.RetrievePermissions;
  _Route.ValidateRoutes; // Logs warnings for missing registered methods

  WebServer.addChainProcessor(_Route, True);
end;
```

---

## 6. Token Authentication (`TTokenManager`)

`src/utils/uTokenManager.pas` manages session token persistence and validation:
- SQLite persistence with configurable inactivity expiration (`cDefaultTokenExpirationTime` = 1 hour).
- `insertTokenAsCookie` adds a `Set-Cookie` header with attributes `Path=/; HttpOnly; SameSite=Strict; Max-Age=<seconds>`.

---

## 7. C-API Library Export (`Toolkit_CAPI_microWebServer.pas`)

Exports functions for non-Pascal host applications (`cdecl` convention):
`WebServer_Create`, `WebServer_Destroy`, `ReqElab_RequestHeaderName`, `ReqElab_RequestHeaderValue`, `ReqElab_setResponseData`, `ReqElab_Body`.
Buffer-filling functions (`ReqElab_*`) copy up to `bufLen` bytes and return the full string length; if returned length > `bufLen`, repeat with a larger buffer.
