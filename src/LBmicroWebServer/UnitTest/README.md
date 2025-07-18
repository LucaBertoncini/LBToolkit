# 🧪 Unit Testing Suite — Micro Web Server in Object Pascal

This repository contains a full unit test suite for a lightweight and modular HTTP server written in Object Pascal, designed for embedded-grade performance and strict security. The testing framework relies on the [Ararat Synapse](https://github.com/ararat/Synapse) networking library for TCP/IP communication and HTTP simulation.

---

## 📦 Test Suite Overview

### 🗂 `uLBmWsDocumentsFolderTests.pas`
Validates the `TLBmWsDocumentsFolder` class responsible for URI-to-path mapping and path security enforcement.

**What it tests:**
- URI decoding and sanitization
- Traversal attack protection (`../`)
- Path normalization and expansion
- XML-based configuration loading
- Subpath validation via `isValidSubpath(...)`

---

### 📄 `uLBmWsFileManagerTests.pas`
Covers the `TLBmWsFileManager` class which handles static file access and stream preparation for HTTP responses.

**What it tests:**
- File existence checks and access controls
- Byte-range request support (e.g. `Range: bytes=0-`)
- MIME type resolution based on extension
- Read buffer preparation and stream integrity

---

### 🌐 `uHTTPRequestManagerTests.pas`
Tests the `THTTPRequestManager` class in a full runtime setup using actual TCP socket connections and simulated HTTP clients (via Synapse).

**What it tests:**
- Request method handling: `GET`, `POST`, `HEAD`
- URI parameter parsing and query string separation
- Full response construction with headers and body
- Static file dispatch
- JSON POST handling with dynamic response logic

Client-side interactions are performed using:

```pascal
HttpGetText(...)
HttpPostURL(...)
```
provided by Synapse.

---

### 🔧 `uTestRunner.pas` — Integration Runner

This unit acts as the live harness for full end-to-end tests, instantiating a working instance of `TLBmicroWebServer` used by the other test units.

---

#### ✅ Runtime Setup

- Web server binds to port `10320` (loopback)
- Temporary sandbox directory (`DocumentFolder`) created in `GetTempDir`
- Logger enabled: `Test_mWs.log`
- Dynamic routing for:
  - `OnGETRequest`
  - `OnPOSTRequest`
  - `OnWSUpgradeRequest` (optional WebSocket support)

---

#### 🧪 Test Flow

1. `uTestRunner.pas` creates a web server instance
2. `uHTTPRequestManagerTests.pas` sends actual HTTP requests via TCP
3. Responses are validated for correctness, content, and status
4. Temporary files are used for testing file access and MIME detection
5. JSON and form submissions are tested using real POST bodies


