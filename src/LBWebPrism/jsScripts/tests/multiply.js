/**
 * @file multiply.js
 * An example user script demonstrating the final, correct architectural pattern.
 */

/**
 * The main handler function for the user script.
 *
 * @param {Bridge} bridge - The bridge object for the current request.
 * @param {Request} request - The request object containing metadata.
 */
function handle(bridge, request) {

    try {
        const payload = bridge.getPayloadAsJSON();

        const responseBody = {
            status: 'ok',
            message: 'Multiplaying a * b = ' + (parseInt(payload.a) * parseInt(payload.b)).toString(),
        };

        bridge.writeResponseAsJSON(responseBody, true);
        console.log('[User Script] Response sent successfully.');

    } catch (e) {
        console.error(`[User Script] Error processing request: ${e.message}`);
        bridge.writeResponseAsJSON({ error: 'An error occurred in the user script.' }, false);
    }
}

module.exports = { handle };

