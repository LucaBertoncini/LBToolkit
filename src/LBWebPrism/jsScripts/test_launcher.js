/**
 * @file test_launcher.js
 * A standalone script to test user-defined handler scripts without the need for the
 * Pascal host, mirroring the functionality of `test_launcher.py`.
 */

const fs = require('fs');
const path = require('path');

// --- Self-contained mock classes ---

class Request {
    constructor(scriptName, uriParams, headers) {
        this.scriptName = scriptName;
        this.uriParams = uriParams || {};
        this.headers = headers || {};
    }
}

class FakeBridge {
    constructor(mockPayload) {
        this._mockPayload = mockPayload || Buffer.alloc(0);
    }

    getRawPayload() {
        console.log("[Launcher] Script requested Raw Payload.");
        return this._mockPayload;
    }

    getPayloadAsJSON() {
        console.log("[Launcher] Script requested JSON Payload.");
        if (this._mockPayload.length === 0) {
            return null;
        }
        try {
            return JSON.parse(this._mockPayload.toString('utf-8'));
        } catch (e) {
            console.log("[Launcher] Warning: Mock payload is not valid JSON.");
            return null;
        }
    }

    writeResponseAsJSON(obj, success = true) {
        const tag = success ? "✅ SUCCESS" : "❌ ERROR";
        const resp = JSON.stringify(obj, null, 2);
        console.log(`\n[${tag}] Script Response (JSON):\n${resp}`);
    }

    writeRawResponse(data, success = true) {
        const tag = success ? "✅ SUCCESS" : "❌ ERROR";
        try {
            const decodedData = data.toString('utf-8');
            console.log(`\n[${tag}] Script Response (Raw Text):\n${decodedData}`);
        } catch (e) {
            console.log(`\n[${tag}] Script Response (Raw Bytes, hex):\n${data.toString('hex')}`);
        }
    }

    signal_completion() {
        console.log("\n[Launcher] Script signaled completion.");
    }
}

function launch(scriptPath, uriParams, headers, payload) {
    const absoluteScriptPath = path.resolve(scriptPath);
    if (!fs.existsSync(absoluteScriptPath)) {
        console.error(`❌ Error: Script not found at '${absoluteScriptPath}'`);
        return;
    }

    const scriptDir = path.dirname(absoluteScriptPath);
    process.chdir(scriptDir);

    const bridge = new FakeBridge(payload);
    const request = new Request(path.basename(absoluteScriptPath), uriParams, headers);

    console.log(`\n🚀 Executing: ${absoluteScriptPath}`);
    console.log(`   - URI Params: ${JSON.stringify(request.uriParams)}`);
    console.log(`   - Headers: ${JSON.stringify(request.headers)}`);
    console.log(`   - Payload: ${payload ? payload.toString() : 'null'}`);
    console.log("-" .repeat(20));

    try {
        const userModule = require(absoluteScriptPath);
        if (typeof userModule.handle !== 'function') {
            throw new Error(`Script ${absoluteScriptPath} does not export a 'handle' function.`);
        }
        userModule.handle(bridge, request);
    } catch (e) {
        console.error(`\n❌ Unhandled exception during script execution: ${e.message}`);
    }
}

if (require.main === module) {
    if (process.argv.length < 3) {
        console.log("Usage: node test_launcher.js <script_path> [uri_params_json] [headers_json] [payload_string]");
        console.log("\nExample:");
        console.log("node test_launcher.js example.js '{\"id\":\"123\"}' '{\"X-API-KEY\":\"abc\"}' '{\"message\":\"hello\"}'");
        process.exit(1);
    }

    const script = process.argv[2];
    let p, h;

    try {
        p = process.argv.length > 3 ? JSON.parse(process.argv[3]) : {};
    } catch (e) {
        console.warn(`⚠️ Invalid JSON for URI Params: ${e.message}`);
        p = {};
    }

    try {
        h = process.argv.length > 4 ? JSON.parse(process.argv[4]) : {};
    } catch (e) {
        console.warn(`⚠️ Invalid JSON for Headers: ${e.message}`);
        h = {};
    }

    const load = process.argv.length > 5 ? Buffer.from(process.argv[5], 'utf-8') : null;

    launch(script, p, h, load);
}

