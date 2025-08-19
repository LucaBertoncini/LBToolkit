# Node.js Wrapper for LBToolkit

This directory contains the Node.js wrapper for the LBToolkit, allowing you to handle web requests in a Node.js environment, orchestrated by the main Pascal application. The architecture is designed to mirror the existing Python bridge.

## Architecture

The wrapper consists of three main parts:

1.  **`worker.js`**: This is the main entry point for the Node.js process launched by the Pascal host. It runs a simple, continuous loop that uses the `Bridge` to wait for and process incoming requests.

2.  **`bridge.js`**: This module provides the core `Bridge` and `Request` classes.
    *   The `Bridge` class is the heart of the wrapper. It handles all the low-level communication over the socket, including reading the binary header, parsing the full request packet, and serializing the response.
    *   The `Request` class is a simple data object that holds the parsed, non-payload information for a request (script name, URI parameters, headers).

3.  **User Script (e.g., `example.js`)**: This is where you write your application logic. The script must export a `handle` function that accepts the `bridge` and `request` objects as arguments.

## Usage

Your user script should be structured as follows:

```javascript
// my_handler.js

/**
 * @param {Bridge} bridge - The bridge object for this request. Use it to get the payload and send a response.
 * @param {Request} request - The request object with headers and other metadata.
 */
function handle(bridge, request) {
    try {
        // 1. Get the payload from the bridge
        const payload = bridge.getPayloadAsJSON();

        // 2. Process the request
        const responseBody = {
            status: 'ok',
            received_payload: payload,
        };

        // 3. Send a response back via the bridge
        bridge.writeResponseAsJSON(responseBody, true);

    } catch (e) {
        // Handle errors and send an error response
        bridge.writeResponseAsJSON({ error: e.message }, false);
    }
}

module.exports = { handle };
```

### How It Works

1.  The Pascal application launches the `worker.js` script, passing it a socket path for communication.
2.  The `worker.js` connects to the socket and enters a loop, calling `bridge.read_request()`.
3.  The `bridge.read_request()` method asynchronously reads from the socket, parses the binary protocol data, and resolves with a `Request` object when a full request is received.
4.  The `worker.js` dynamically loads the user script specified in the request and calls its `handle` function, passing the active `bridge` and the new `request` object.
5.  Your `handle` function uses the `bridge` to get the payload and send a response.
6.  `bridge.signal_completion()` closes the connection for this request, and the `worker.js` loop continues, waiting for the next request.

## Files

-   `worker.js`: The main worker script that runs the processing loop.
-   `bridge.js`: Provides the core `Bridge` and `Request` classes, handling all protocol logic.
-   `example.js`: An example user script demonstrating the correct usage pattern.
-   `README.md`: This file.

