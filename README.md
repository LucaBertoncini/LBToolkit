# LBToolkit

A modular, reactive and thread-safe architecture toolkit for systems programming and automation.

It is a swiss-army toolkit for FreePascal developers: thread-safe classes, event systems, file utilities and a reactive logging engine UI integration. Lightweight, extensible, and non-intrusive.

---

## Features

- **Thread-safe synchronization** via `TTimedOutCriticalSection` (critical section with timeout support)
- **Event dispatching system** with flexible callback signatures
- **Structured logging engine** with optional UI integration
- **File utilities** for recursive operations and size estimation
- **Timers and delays** with interruptable sleep and precision control
- **WebSocket client** with RFC 6455 support and automatic ping/pong
- **Minimal HTTP server** with path-based routing, a customizable pipeline of request handlers (to easily extend it into an HTTP gateway or middleware engine) and built-in support for WebSocket connections
- **Shared memory manager** for IPC with external processes
- **Virtual keyboard component** for UI automation and input simulation (`LBVirtualKeyBoard`)
- **Python bridge**: an HTTP server extension that dispatches JSON POST requests to URI-mapped Python scripts, organized in folders and executed via a managed pool of Python processes (`LBWebPyBridge`)

---

## Tested Platforms

- Linux (x86_64, ARM)
- Windows (64-bit)

Other platforms may be compatible via FreePascal, but are not currently tested.

---

## Design Principles

- **Modularity**: each unit is independent and reusable
- **Thread safety**: concurrency is handled explicitly and predictably
- **Minimal dependencies**: no external libraries required beyond FreePascal, Ararat Synapse and OpenSSL
- **Extensibility**: components are designed to be subclassed or replaced

---

## Directory Overview

| Path                       | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `src/utils/`               | Core reusable units: threading, events, timers, file operations             |
| `src/LBLogger/`            | Reactive logging engine with optional UI integration                        |
| `src/LBmicroWebServer/`    | Lightweight HTTP server with routing and WebSocket server support           |
| `src/LBWebSocketClient/`   | WebSocket client implementation with automatic ping/pong                    |
| `src/LBVirtualKeyBoard/`   | Virtual keyboard component for simulating key input                         |
| `src/LBWebPyBridge/`       | Python bridge for routing HTTP POST requests to URI-mapped Python scripts   |

---

## Practical Examples

These examples demonstrate how to use key components of the library through Lazarus projects and their main source units.

| Lazarus Project | Main Unit | Description |
|-----------------|-----------|-------------|
| [`Test_LBLogger.lpr`](src/LBLogger/LazTest/Test_LBLogger.lpr) | [`ufrmmain.pas`](src/LBLogger/LazTest/ufrmmain.pas) | Demonstrates how to use `TLBBaseLogger` with sub-loggers |
| [`VKb_Demo.lpr`](src/LBVirtualKeyboard/Demo/VKb_Demo.lpr) | [`ufrmmain.pas`](src/LBVirtualKeyboard/Demo/ufrmmain.pas) | Demo of the `LBVirtualKeyboard` component |
| [`LBWebPyBridgeTest.lpr`](src/LBWebPyBridge/UnitTest/LBWebPyBridgeTest.lpr) | [`uLBWebPyBridgeTests.pas`](src/LBWebPyBridge/UnitTest/uLBWebPyBridgeTests.pas) | Unit test showing basic functionality of the Python bridge |
| [`TestWebSocketClient.lpr`](src/LBWebSocketClient/UnitTest/TestWebSocketClient.lpr) | [`uTestWebSocketClient.pas`](src/LBWebSocketClient/UnitTest/uTestWebSocketClient.pas) | Unit test for the `LBWebSocketClient` component |
| [`LBmicroWebServerTests.lpr`](src/LBmicroWebServer/UnitTest/WebServer/LBmicroWebServerTests.lpr) | [`uhttprequestmanagertests.pas`](src/LBmicroWebServer/UnitTest/WebServer/uhttprequestmanagertests.pas) | Unit test for the `LBmicroWebServer` HTTP/WebSocket server |
| [`LBbaseThreadDemo.lpr`](src/utils/demos/LBBaseThread/LBbaseThreadDemo.lpr) | [`ufrmmain.pas`](src/utils/demos/LBBaseThread/ufrmmain.pas) | Example showing how `uLBBaseThread` simplifies thread usage |
| [`EventsManagerExample.lps`](src/utils/demos/EventsManager/EventsManagerExample.lps) | [`ufrmmain.pas`](src/utils/demos/EventsManager/ufrmmain.pas) | Example usage of the `TEventsManager` component |

These examples are designed to be minimal, focused, and directly runnable.

---

## License

This project is licensed under the Mozilla Public License 2.0 — see [LICENSE](LICENSE) for details.

---

![License: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-blue.svg)
![Language: Pascal](https://img.shields.io/badge/language-Pascal-yellow.svg)
