# 🧰 LBToolkit

> **A modular, thread-safe, and high-performance Object Pascal framework for systems programming, networking, IPC, and scripting integration.**

[![License: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Language: FreePascal](https://img.shields.io/badge/language-FreePascal_3.2+-yellow.svg)](https://www.freepascal.org/)
[![Tested Platforms](https://img.shields.io/badge/platform-Linux_%7C_Windows-lightgrey.svg)](#tested-platforms)

---

## 🌟 Overview

**LBToolkit** is a complete "swiss-army toolkit" designed for FreePascal / Lazarus software engineers building high-concurrency background services, web servers, IPC gateways, and industrial automation tools.

Whether you need a lightweight HTTP/WebSocket server, a reactive logging engine, inter-process communication with Python or Node.js, or thread-safe synchronization primitives, **LBToolkit** provides lightweight, decoupled, zero-bloat building blocks.

---

## 📦 Framework Modules

| Module | Location | Description |
|--------|----------|-------------|
| **Core Utils** | [`src/utils/`](src/utils/) | Threading (`TLBBaseThread`), timeout-safe critical sections, ring buffers (`TLBCircularBuffer`), IPC shared memory, SQLite wrapper, OpenSSL 3 support. |
| **Micro Web Server** | [`src/LBmicroWebServer/`](src/LBmicroWebServer/) | Embedded HTTP/1.1 & WebSocket (RFC 6455) server with streaming file uploads, REST route registry, and range downloads. |
| **WebSocket Client** | [`src/LBWebSocketClient/`](src/LBWebSocketClient/) | Non-blocking WebSocket client with auto ping/pong and reconnect support. |
| **LBWebPrism** | [`src/LBWebPrism/`](src/LBWebPrism/) | Microservice gateway bridging HTTP POST requests to managed pools of Python and Node.js script workers. |
| **LBLogger** | [`src/LBLogger/`](src/LBLogger/) | Asynchronous, hierarchical logging framework with dynamic sub-logger delegation and UI bindings. |
| **LBVirtualKeyboard** | [`src/LBVirtualKeyboard/`](src/LBVirtualKeyboard/) | Touchscreen virtual keyboard engine with XML layouts, themes, and native Win32/X11 input injection. |
| **C-API Shared Lib** | [`src/shared/LBToolkit/`](src/shared/LBToolkit/) | C-compatible shared library wrapper (`.so`/`.dll`) exposing LBToolkit to Python, C++, Go, and Rust. |

---

## 🚀 Quick Starts & Examples

### 1️⃣ Starting an HTTP & WebSocket Server (`LBmicroWebServer`)

```pascal
uses
  uLBmicroWebServer;

var
  Server: TLBmicroWebServer;
begin
  Server := TLBmicroWebServer.Create(nil);
  try
    Server.Port := 8080;
    Server.DocumentsFolder := './www';
    Server.UploadEndpoint := '/api/upload'; // Raw binary streaming upload
    Server.Active := True;

    WriteLn('Server listening on http://localhost:8080');
    ReadLn;
  finally
    Server.Free;
  end;
end;
```

### 2️⃣ Asynchronous Thread-Safe Logging (`LBLogger`)

```pascal
uses
  ULBLogger;

var
  Logger: TLBLogger;
begin
  Logger := TLBLogger.Create('app.log');
  try
    Logger.MaxLogLevel := 3;
    Logger.logWrite(1, 'Database', lmt_Info, 'Connected to SQLite database.');
  finally
    Logger.Free;
  end;
end;
```

---

## 🤖 AI SDK & Developer Documentation

If you use AI coding tools (Claude, ChatGPT, GitHub Copilot, Cursor) to write software with LBToolkit, or if you are looking to contribute to the codebase, check out our comprehensive guides:

- 📖 **[System Architecture Guide](docs/ARCHITECTURE.md)**: Deep dive into the threading model, memory strategy, IPC, and socket layer.
- 🤖 **[AI SDK & Prompting Context Guide](docs/AI_SDK_GUIDE.md)**: Structured reference for AI assistants with ready-to-use boilerplate, class interfaces, and common development patterns.

---

## 🖥️ Tested Platforms

- **Linux** (x86_64, ARM)
- **Windows** (64-bit)

---

## 📄 License

This project is licensed under the **Mozilla Public License 2.0** — see [LICENSE](LICENSE) for details.
