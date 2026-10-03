# 🌐 LBmicroWebServer — Lightweight, High-Performance HTTP & WebSocket Server

`LBmicroWebServer` is a multithreaded HTTP/1.1 and WebSocket (RFC 6455) server written in Object Pascal (FreePascal). It is engineered for low memory footprint, high stability, and seamless extension into gateways, REST microservices, and file servers.

---

## ⚡ Key Features

- **Multithreaded Connection Handling**: Spawns worker threads (`THTTPRequestManager`) for each client connection with automatic lifecycle deallocation (`FreeOnTerminate = True`).
- **Declarative XML Route & Authorization Matrix (`uWebRouteRegistry.pas`)**: Define functional areas, endpoints, HTTP verbs, authentication flags (`RequiresAuth="true|false"`), and operation types (`OpType`) in readable XML configuration files.
- **Worker Execution Families (`ekStandard`, `ekFileDownload`, `ekAuth`, `ekUpload`, `ekProxy`)**: Cleanly dispatches JSON requests, file downloads, authentication tokens, raw streaming uploads, or signed transparent proxy requests.
- **Streaming File Uploads**: Raw binary upload endpoint supporting large files. Body data streams directly from socket to disk, preventing memory overflow. Original filenames are preserved as metadata while files are saved with unique UUIDs.
- **WebSocket Server (RFC 6455)**: Bidirectional streaming with framing, masking/unmasking, automatic ping/pong, and message queues (`uWebSocketManagement.pas`).
- **Security**: Sandboxed document root (`TLBmWsDocumentsFolder`), path-traversal prevention, sanitized file names, OpenSSL 3 thread safety, and SIGPIPE protection on Unix systems.

---

## 📜 XML Route Registry & Authorization Matrix

Routes and permissions are defined in a clean XML file parsed by `TRouteRegistry`.

### 📄 Example XML Route Configuration (`Routes.xml`)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<RouteRegistry>
  <Applications>
    <Application Code="AppWarehouse" Name="Warehouse Management" />
    <Application Code="AppAdmin" Name="System Administration" />
  </Applications>

  <!-- Functional Area for Authentication (Public / No Auth required) -->
  <FunctionalArea Code="Auth" Name="Authentication Module">
    <Endpoint Function="login" Method="POST" URI="/api/v1/auth/login" Kind="Auth" RequiresAuth="false" />
    <Endpoint Function="logout" Method="POST" URI="/api/v1/auth/logout" Kind="Auth" RequiresAuth="true" />
  </FunctionalArea>

  <!-- Protected Warehouse Functional Area -->
  <FunctionalArea Code="Inventory" Name="Stock Inventory" App="AppWarehouse">
    <!-- Public search endpoint for authenticated users -->
    <Endpoint Function="searchItems" Method="POST" URI="/api/v1/inventory/search" Kind="Standard" RequiresAuth="true" />

    <!-- Endpoint requiring specific OpType permission 'EDIT_STOCK' -->
    <Endpoint Function="updateStock" Method="POST" URI="/api/v1/inventory/update" Kind="Standard" RequiresAuth="true" OpType="EDIT_STOCK" />

    <!-- File Download Worker endpoint -->
    <Endpoint Function="downloadReport" Method="GET" URI="/api/v1/inventory/report" Kind="FileDownload" RequiresAuth="true" OpType="VIEW_REPORTS" />

    <!-- Raw File Upload Worker endpoint -->
    <Endpoint Function="uploadDocument" Method="POST" URI="/api/v1/inventory/upload" Kind="Upload" RequiresAuth="true" OpType="EDIT_STOCK" />

    <!-- Transparent Signed Proxy endpoint forwarding to a remote service -->
    <Endpoint Function="externalRates" Method="POST" URI="/api/v1/inventory/rates" Kind="Proxy" Target="rates_service" RequiresAuth="true" />
  </FunctionalArea>
</RouteRegistry>
```

---

## 🛠️ Registering Worker Handlers in Pascal

```pascal
uses
  uWebRouteRegistry, fpjson;

type
  TWarehouseHandler = class(TRouteHandlerBase)
  public
    function SearchItemsWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
    function UpdateStockWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
  end;

function TWarehouseHandler.SearchItemsWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Res: TJSONObject;
begin
  _Res := TJSONObject.Create;
  _Res.Add('status', 'success');
  aStatusCode := 200;
  Result := _Res;
end;

function TWarehouseHandler.UpdateStockWorker(aUserId: Integer; aUserConfig: TJSONObject; aParams: TJSONObject; out aStatusCode: Integer): TJSONData;
begin
  // Handle stock update
end;

// Registration during initialization
procedure RegisterWarehouseRoutes(aRouteModule: TWebRouteModule);
var
  _Handler: TWarehouseHandler;
begin
  _Handler := TWarehouseHandler.Create('Inventory');
  _Handler.RegisterStandardWorker('searchItems', @_Handler.SearchItemsWorker);
  _Handler.RegisterStandardWorker('updateStock', @_Handler.UpdateStockWorker);

  aRouteModule.RegisterHandler(_Handler);
end;
```

---

## 🏗️ Architecture & Component Flow

```
                     +---------------------------+
                     |    TLBmicroWebServer      |
                     +-------------+-------------+
                                   |
                         Listens on TCP Port
                                   v
                     +---------------------------+
                     |    THTTPRequestManager    | (Thread per Connection)
                     +-------------+-------------+
                                   |
                  +----------------+----------------+
                  |                                 |
                  v                                 v
        [HTTP Request Parser]             [WebSocket Handshake]
                  |                                 |
        +---------+---------+                       v
        |                   |             [TLBWebSocketSession]
        v                   v
  [Static File]     [TWebRouteModule / TRouteRegistry]
 (Range downloads)  (Validates Auth & OpType permission matrix)
                            |
           +----------------+----------------+----------------+
           |                |                |                |
           v                v                v                v
     [ekStandard]    [ekFileDownload]   [ekUpload]        [ekProxy]
     (JSON In/Out)    (Disk Streaming) (Disk Stream)   (Signed Remote Forward)
```

---

## 📁 Key Units

| Unit | Purpose |
|------|---------|
| `uLBmicroWebServer.pas` | Core server engine, listener, thread manager |
| `uHTTPRequestParser.pas` | Header & body parsing with `TLBCircularBuffer` |
| `uWebRouteRegistry.pas` | XML-driven REST route registry, permission matrix (`OpType`), and worker dispatchers |
| `uWebSocketManagement.pas` | WebSocket framing, ping/pong, session management |
| `uLBmWsFileManager.pas` | Static file serving & Range HTTP header support |
| `uLBmWsDocumentsFolder.pas` | Document root sandboxing and path verification |
