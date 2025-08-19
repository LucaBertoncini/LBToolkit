/**
 * @file worker.js
 * Main worker entry point, designed to mirror the logic of `worker.py`.
 */

const net = require('net');
const path = require('path');
const { Bridge } = require('./bridge.js');
const { logger, disable_logger } = require('./logger.js');
disable_logger(false);

function getArg(argName) {
    const arg = process.argv.find(a => a.startsWith(`${argName}=`));
    return arg ? arg.split('=')[1] : null;
}

async function main() {
    const socketPath = getArg('--socket-path');
    if (!socketPath) {
        console.error('FATAL: --socket-path argument is required.');
        process.exit(1);
    }

    // Wait for 200ms to give the Pascal server time to start listening.
    // This prevents a race condition on startup.
    await new Promise(resolve => setTimeout(resolve, 200));

    const client = net.createConnection({ path: socketPath });

    client.on('connect', () => {
        logger.info(`Worker connected to Pascal server on socket: ${socketPath}`);
        // Start the main processing loop once connected
        processLoop(client);
    });

    client.on('error', (err) => {
        console.error(`Socket connection error: ${err.message}`);
        process.exit(1);
    });
}

let request = null;

async function processLoop(client) {
    const bridge = new Bridge(client);

    while (true) {
        request = null;

        try {
            logger.info('[Worker] Waiting for request...');
            request = await bridge.read_request();
            logger.info(`[Worker] Processing script: ${request.scriptName}`);

            const scriptFullPath = path.resolve(request.scriptName);
            if (!require.resolve(scriptFullPath)) {
                throw new Error(`Script file not found: ${scriptFullPath}`);
            }

            const userModule = require(scriptFullPath);
            if (typeof userModule.handle !== 'function') {
                throw new Error(`Script ${scriptFullPath} does not export a 'handle' function.`);
            }

            // Execute the user's script
            userModule.handle(bridge, request);

        } catch (e) {
            logger.error(`[Worker] Execution failed: ${e.message}`);
            bridge.writeResponseAsJSON({ wpbError: `Execution failed: ${e.message}` }, false);
        }
    }
    bridge.close_communication();    
}

main().catch(err => {
    console.error(`Unhandled error in main: ${err.message}`);
    process.exit(1);
});

