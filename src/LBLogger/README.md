LBLogger — Lightweight, modular and reactive logging system
Main features

    Asynchronous logging: messages are queued and processed by a dedicated thread

    Dynamic and extensible subloggers: you can add new loggers to the main module even after the application has been written, without modifying existing code. This enables:

        alternative storage backends (e.g. TEventLogger for system logs),

        visual output on desktop interfaces (TMemoLogger, TLabelLogger, etc.),

        external notification channels (email, Telegram, MQTT),



    Simple extension via inheritance: just derive from TLBBaseLogger to add new handlers. ➤ This flexibility turns a “simple” logger into a non-invasive alert and monitoring system, adaptable to any context.
