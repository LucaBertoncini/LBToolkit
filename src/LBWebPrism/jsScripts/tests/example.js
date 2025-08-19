/**
 * @file example.js
 * An example user script demonstrating the final, correct architectural pattern.
 */

/**
 * The main handler function for the user script.
 *
 * @param {Bridge} bridge - The bridge object for the current request.
 * @param {Request} request - The request object containing metadata.
 */
function handle(bridge, request) {
    console.log('[User Script] Handling request...');
    console.log('[User Script] Script Name:', request.scriptName);
    console.log('[User Script] URI Params:', request.uriParams);
    console.log('[User Script] Headers:', request.headers);

    try {
        const payload = bridge.getPayloadAsJSON();
        console.log('[User Script] Parsed Payload:', payload);

        const responseBody = {
            status: 'ok',
            message: 'Response from Node.js user script',
            received_payload: payload,
        };

        bridge.writeResponseAsJSON(responseBody, true);
        console.log('[User Script] Response sent successfully.');

    } catch (e) {
        console.error(`[User Script] Error processing request: ${e.message}`);
        bridge.writeResponseAsJSON({ error: 'An error occurred in the user script.' }, false);
    }
}

module.exports = { handle };

