# 🌐 LBWebSocketClient

`LBWebSocketClient` is a standalone WebSocket client, it provides a robust, event-driven interface for establishing and managing WebSocket connections, sending and receiving messages, and handling connection lifecycle events. It supports both secure (`wss://`) and non-secure (`ws://`) protocols and includes automatic ping/pong handling to maintain connection health.

---

## 🚀 Features

- 🔌 WebSocket protocol support (RFC 6455)
- 🔐 SSL/TLS compatibility via OpenSSL
- 📡 Automatic ping/pong for connection keep-alive
- 🧠 Threaded architecture for non-blocking communication
- 🧩 Event-driven callbacks for text, binary, and raw messages
- 📜 XML-based configuration loading
- 🧪 Included unit test for real-world echo servers

---

## 🧪 Example Usage

```pascal
var
  Client: TLBWebSocketClient;
begin
  Client := TLBWebSocketClient.Create;
  Client.RemoteConnectionData.Host := 'echo.websocket.events';
  Client.RemoteConnectionData.Port := 443;
  Client.RemoteConnectionData.UseSSL := True;
  Client.URI := '/';

  Client.OnConnected := @HandleConnected;
  Client.OnDisconnected := @HandleDisconnected;
  Client.OnWebSocketTextMessage := @HandleTextMessage;

  Client.Start;
  Client.AddWebSocketMessageToSend('Hello WebSocket');
end;
```

---

## ⚙️ Configuration

You can configure the WebSocket client either programmatically or by loading settings from an XML file.

### 🔧 XML Configuration Example

```xml
<Connection>
  <Host>echo.websocket.events</Host>
  <Port>443</Port>
  <UseSSL>True</UseSSL>
  <URI>/</URI>
</Connection>
```
