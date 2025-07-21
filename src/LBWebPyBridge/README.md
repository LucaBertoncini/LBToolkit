# 💡 LBWebPyBridge  
A modular bridge between Pascal and Python — script-driven microservices with shared memory efficiency.

LBWebPyBridge is a high-performance framework that connects a Pascal web server to Python scripts executed as microservices.
Built on top of LBmicroWebServer, it enables dynamic request handling through a persistent Python thread pool, using shared memory for fast, low-overhead communication.

Compile the server once. Extend functionality with Python.
Perfect for scalable, testable, multi-language architectures.

---



## Why LBWebPyBridge Exists

LBWebPyBridge began as a natural evolution of **LBmicroWebServer (LBmWS)** — a high-performance web engine written entirely in Pascal with just one dependency: the Ararat Synapse library. I've used LBmWS extensively and successfully as the backend for various desktop and web applications over the years.

However, LBmWS had one limitation: every time a new request handler or change in business logic was needed, the entire server had to be recompiled. This created friction in agile development workflows, where flexibility and rapid iteration are essential.

So I envisioned a better solution: compile the server once, extend its functionality dynamically using a scripting language.

I chose **Python**, one of the most popular and widely supported scripting languages.
My first implementation used `TProcess` to invoke Python scripts directly, passing the request data via standard input and reading results from standard output.

It worked… but wasn’t efficient:
each request spawned a new thread and launched a full Python process;
startup time was costly and unsuitable for high-performance applications.

So I rethought the model:
💡 Pre-spawn a pool of Python workers
💡 Keep each process alive and listening for new requests
💡 Exchange data via **shared memory**, avoiding unnecessary data copying and improving speed

And thus **LBWebPyBridge** was born — a robust framework where a Pascal web server communicates seamlessly with long-running Python workers.
Requests are dispatched by an orchestrator, and responses are returned with ultra-low latency through shared memory.

And here’s the beauty of it all: this transformation required only two additional Pascal units, elegantly extending **LBmicroWebServer** into a modular, scriptable, cross-language bridge between **Pascal’s performance** and **Python’s flexibility**.


## Architecture 🏗️

At its core, LBWebPyBridge orchestrates a seamless interaction between a Pascal web server and a pool of Python workers.
Here's how it works from startup to request handling.

### At Start-up:
LBWebPyBridge begins by reading a configuration file. This file dictates the size of the Python worker thread pool, the allocated shared memory segment, and the web server's settings (like the listening port and the document folder for static files). Upon activation, the system dynamically creates the Python threads (TPyBridge), which are centrally managed by an orchestrator (TBridgeOrchestrator).

### Request Flow:
When the WebServer receives an incoming POST request, it doesn't process the business logic itself. Instead, it delegates this responsibility to the TBridgeOrchestrator. Once the request has been fully elaborated by a Python worker, the orchestrator retrieves the answer and sends it back to the client.

### The Orchestrator's Role:
The TBridgeOrchestrator is the brain of the operation. Its primary task is to wait for an available Python thread from its pool and assign the incoming request to it. Each request carries two crucial pieces of information: the name of the Python script to execute and the parameters that script needs to process. Importantly, the script's name is cleverly derived directly from the HTTP request URI. For instance, a request to http://127.0.0.1:8080/tests/AppHello/main will instruct the orchestrator to execute the Python script located at {executableFolder}/pyScripts/tests/AppHello/main.py.

### The worker.py Script:
On the Python side, each worker.py script (launched by a TPyBridge thread) constantly monitors a specific byte in its shared memory segment. When this byte signals a "start" value, the worker knows a new request is waiting. It then proceeds to execute the specified Python script using runpy.run_path. During execution, the Python script receives a request object, which provides a transparent API for reading the incoming parameters and and a bridge object used for sending back the response via shared memory. Once the Python script completes its task, the starting byte in shared memory is updated to a "termination" value, signaling Pascal that the elaboration is finished and the flow can return.

### The worker.py Script:
On the Python side, each worker.py script (launched by a TPyBridge thread) constantly monitors a specific byte in its shared memory segment. When this byte signals a "start" value, the worker knows a new request is waiting. It then proceeds to execute the specified Python script using runpy.run_path. During execution, the Python script receives a request object, which provides a transparent API for reading the incoming parameters, and a bridge object used for sending back the response via shared memory. Once the Python script completes its task, the starting byte in shared memory is updated to a "termination" value, signaling Pascal that the elaboration is finished and the flow can return.


## 🧪 Testing and Usage Examples

The robustness and functionality of LBWebPyBridge are ensured through a comprehensive suite of unit and integration tests. These tests are not merely designed to validate functionality — they serve as living examples of how the framework can be used in real-world scenarios, making them an invaluable resource for developers.

Implemented in Pascal using the fpcunit framework, our test suite demonstrates:

- How to configure and activate the web server and Python bridge.
- How to build and send HTTP requests (e.g., POST with JSON payloads) that interact with the Python backend.
- The precise mapping between HTTP routes and specific Python scripts.
- How to properly verify responses, handle errors, and manage various output formats.
- How the system behaves under special conditions, such as missing scripts or forced exceptions.

This makes the Pascal test suite an invaluable resource for developers. Each test not only ensures the system works as intended, but also acts as a step-by-step usage tutorial. It provides a clear blueprint for integrating LBWebPyBridge into other applications — whether you're building a standalone web server or embedding the bridge inside a larger software system.

In line with the LBToolkit philosophy, the tests are reusable, instructive, and modular, helping you understand not only how LBWebPyBridge performs, but also how you can shape it to meet your specific needs.

### Testing Python Scripts Independently:

Beyond the Pascal test suite, LBWebPyBridge offers a dedicated Python module (`test_launcher.py`) that significantly streamlines the development and debugging of your Python scripts. This tool allows you to:

- **Unit Test Python Logic**: Develop and test your business logic (.py files) in isolation, ensuring their correctness before even involving the Pascal web server.
- **Rapid Debugging**: Quickly identify and fix issues in your Python code by executing it directly, simulating the LBBridge environment.
- **Simulate Bridge Interactions**: The `test_launcher.py` provides a FakeBridge object that replaces the real LBBridge during testing. This FakeBridge effectively simulates shared memory operations by printing outputs to the console, allowing you to verify your script's behavior without the full Pascal stack.

You can simply run your Python script via the launcher with a command like this:

```bash
python3 test_launcher.py tests/AppHello/main.py '{"custom_message": "Test with launcher!"}'


