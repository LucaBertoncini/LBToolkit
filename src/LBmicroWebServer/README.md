# 🌐 LBmicroWebServer — Lightweight, High-Performance HTTP & WebSocket Server

`LBmicroWebServer` is a multithreaded HTTP/1.1 and WebSocket (RFC 6455) server written in Object Pascal (FreePascal). It is engineered for low memory footprint, high stability, and seamless extension into gateways, REST microservices, and file servers.

---

## ⚡ Key Features

- **Multithreaded Connection Handling**: Spawns worker threads (`THTTPRequestManager`) for each client connection with automatic lifecycle deallocation (`FreeOnTerminate = True`).
- **Pipeline Architecture**: Interceptable request processing chain (`TFPHTTPRequestProcessor`).
- **Streaming File Uploads**: Raw binary upload endpoint supporting large files. Body data streams directly from socket to disk, preventing memory overflow. Original filenames are preserved as metadata while files are saved with unique UUIDs.
- **REST Route Registry**: XML-based or programmatic route declaration (`uWebRouteRegistry.pas`) with functional area and permission mapping.
- **WebSocket Server (RFC 6455)**: Bidirectional streaming with framing, masking/unmasking, automatic ping/pong, and message queues (`uWebSocketManagement.pas`).
- **Security**: Sandboxed document root (`TLBmWsDocumentsFolder`), path-traversal prevention, sanitized file names, OpenSSL 3 thread safety, and SIGPIPE protection on Unix systems.

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
        (Header-first parsing)            (RFC 6455 Handshake)
                  |                                 |
        +---------+---------+                       v
        |                   |             [TLBWebSocketSession]
        v                   v             (Frame I/O, Ping/Pong)
  [Static File]    [Forwarded Request]
 (Range downloads) (REST Route Registry /
                    Processors Chain)
```

---

## 📄 Raw File Upload Handling

`LBmicroWebServer` handles high-throughput raw binary file uploads:
1. Client sends HTTP POST/PUT to `UploadEndpoint` with header `X-File-Name: document.pdf`.
2. Header parser reads `Content-Length` and switches strategy to stream directly to disk.
3. The uploaded file is saved under `DocumentsFolder` with a generated unique filename.
4. An `OnUploadCompleted` event triggers for downstream validation.
5. Details are made available in `THTTPRequestParser.UploadedFiles`.

---

## 🚀 Quick Start Example

```pascal
var
  Server: TLBmicroWebServer;
begin
  Server := TLBmicroWebServer.Create(nil);
  try
    Server.Port := 8080;
    Server.DocumentsFolder := '/var/www/html';
    Server.UploadEndpoint := '/api/upload';

    // Hook custom request handler
    Server.OnGETRequest := @HandleGetRequest;

    Server.Active := True;
    WriteLn('Server running on port 8080...');
    ReadLn;
  finally
    Server.Free;
  end;
end;
```

---

## 📁 Key Units

| Unit | Purpose |
|------|---------|
| `uLBmicroWebServer.pas` | Core server engine, listener, thread manager |
| `uHTTPRequestParser.pas` | Header & body parsing with `TLBCircularBuffer` |
| `uWebRouteRegistry.pas` | REST endpoint definition & permission matrix |
| `uWebSocketManagement.pas` | WebSocket framing, ping/pong, session management |
| `uLBmWsFileManager.pas` | Static file serving & Range HTTP header support |
| `uLBmWsDocumentsFolder.pas` | Document root sandboxing and path verification |
| `uFPHTTPRequestProcessor.pas` | Request processor chain base classes |
