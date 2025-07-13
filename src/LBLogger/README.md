# LBLogger — Lightweight, Modular and Reactive Logging System

## 🚀 Main Features

### 🔄 Asynchronous Logging  
Messages are queued and processed by a dedicated thread, minimizing impact on the main execution flow.

### 🧩 Dynamic and Extensible Subloggers  
You can attach new loggers to the main module — **even after the application has been written** — without modifying existing code.

This enables:
- 📦 Alternative storage backends (e.g. `TEventLogger` for system logs)
- 🖥️ Visual output on desktop interfaces (`TMemoLogger`, `TLabelLogger`, etc.)
- 📤 External notification channels (email, Telegram, MQTT)

### 🧠 Simple Extension via Inheritance  
Just derive from `TLBBaseLogger` to add new handlers.  
➤ This flexibility transforms a **“simple” logger** into a **non-invasive alert and monitoring system**, adaptable to any context.
