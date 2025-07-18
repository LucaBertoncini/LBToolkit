# 🌐 LBmicroWebServer — Embedded-Friendly Web Server in Object Pascal

LBmicroWebServer is a lightweight, modular web server written in Object Pascal (Free Pascal), tailored for embedded systems, educational projects, and microservice deployments. It provides secure file serving, dynamic request handling, and WebSocket support, all built on the [Ararat Synapse](https://github.com/ararat/Synapse) networking stack.

---

## 🧱 Architecture Overview

This project is composed of several clearly defined units:

### 🔹 `uHTTPConst.pas`
Centralized declaration of constants used across the framework.

**Includes:**
- HTTP status lines and headers
- WebSocket protocol constants
- Logging tags and internal timeouts

---

### 📁 `uLBmWsDocumentsFolder.pas`
Handles the mapping between incoming URI requests and actual filesystem paths.

**Responsibilities:**
- Safely decoding and validating subpaths
- Preventing directory traversal (`../`)
- Enforcing sandboxed document root
- Loading configuration via XML nodes

---

### 📦 `uLBmWsFileManager.pas`
Manages static file access and prepares streaming responses.

**Responsibilities:**
- Range support for partial downloads
- MIME type detection
- Data extraction via stream or buffer
- File access abstraction for the HTTP handler

---

### 📡 `uWebSocketManagement.pas`
Implements full RFC 6455 support for WebSocket connections.

**Features:**
- Handshake parsing and response (`Sec-WebSocket-Key`, protocols)
- Frame decoding with masking, fragmentation, and opcode dispatch
- Asynchronous message queue for outbound frames
- Automatic ping/pong with session timeout
- Event-based interface (`OnDataReceived`) for application logic

---

### 🚀 `uLBmicroWebServer.pas`
The main engine of the framework: bootstraps and orchestrates listeners, thread managers, and request routing.

**Features:**
- TCP listener per port (`TLBmWsListener`)
- Configurable threading and critical sections
- Built-in `THTTPRequestManager` to parse and respond to requests
- Event dispatch for `OnGETRequest`, `OnPOSTRequest`, `OnWSUpgradeRequest`
- XML-based server configuration

---

## 🔐 Security by Design

LBmicroWebServer enforces strong filesystem protection and input normalization:

- URI paths are decoded and validated
- `ExpandFileName` ensures paths stay within sandbox
- Handlers verify the origin of requests before dispatch
- WebSocket sessions are protected against protocol violations

---

## 🧪 Testing Support

A full suite of FPCUnit test modules is provided, covering:

- URI path resolution (`uLBmWsDocumentsFolderTests.pas`)
- Static file streaming (`uLBmWsFileManagerTests.pas`)
- HTTP request execution (`uHTTPRequestManagerTests.pas`)
- Integration tests (`uHTTPRequestManagerTests.pas`, `uTestRunner.pas`) with embedded server

WebSocket tests are in progress and planned for the next release.

---

## 💻 Requirements

- Free Pascal Compiler ≥ 3.2.x
- [Ararat Synapse](https://github.com/ararat/Synapse) library
- Lazarus IDE (optional, recommended)


