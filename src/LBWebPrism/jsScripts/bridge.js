/**
 * @file bridge.js
 * This module provides the core classes for the Node.js bridge, mirroring the Python implementation.
 */

const fs = require('fs');
const { Writable } = require('stream');
const { logger } = require('./logger.js');

const HEADER_SIZE_IN = 9; // ScriptNameLen(1) + URIParamsLen(2) + HeadersLen(2) + PayloadLen(4)
const HEADER_SIZE_OUT = 5; // StatusCode(1) + BodyLen(4)

class Request {
    constructor(scriptName, uriParams, headers) {
        this.scriptName = scriptName;
        this.uriParams = uriParams;
        this.headers = headers;
    }
}

class Bridge {
    constructor(client) {
        this.client = client;
        this._payload_info = null;
        this.dataBuffer = Buffer.alloc(0);
    }

    _parse_key_value(rawData, separator) {
        if (!rawData) return {};
        const params = {};
        const lines = rawData.toString('utf-8').trim().split('\r\n');
        for (const line of lines) {
            if (!line) continue;
            const parts = line.split(separator, 2);
            if (parts.length === 2) {
                params[parts[0].trim()] = parts[1].trim();
            }
        }
        return params;
    }

    async read_request() {
        this._payload_info = null;
        let buffer = Buffer.alloc(0);

        return new Promise((resolve, reject) => {
            const onData = (chunk) => {
                buffer = Buffer.concat([buffer, chunk]);

                if (buffer.length >= HEADER_SIZE_IN) {
                    const scriptLen = buffer.readUInt8(0);
                    const paramsLen = buffer.readUInt16LE(1);
                    const headersLen = buffer.readUInt16LE(3);
                    const payloadLen = buffer.readUInt32LE(5);
                    const totalPacketSize = HEADER_SIZE_IN + scriptLen + paramsLen + headersLen + payloadLen;

                    if (buffer.length >= totalPacketSize) {
                        this.client.removeListener('data', onData);

                        const packet = buffer.slice(0, totalPacketSize);
                        let offset = HEADER_SIZE_IN;

                        const scriptName = packet.toString('utf8', offset, offset + scriptLen);
                        offset += scriptLen;

                        const uriParamsRaw = packet.slice(offset, offset + paramsLen);
                        offset += paramsLen;

                        const headersRaw = packet.slice(offset, offset + headersLen);
                        offset += headersLen;

                        this._payload_info = {
                            offset: offset,
                            length: payloadLen,
                            packet: packet,
                        };

                        const uriParams = this._parse_key_value(uriParamsRaw, '=');
                        const headers = this._parse_key_value(headersRaw, ':');

                        resolve(new Request(scriptName, uriParams, headers));
                    }
                }
            };

            this.client.on('data', onData);
            this.client.once('error', reject);
            this.client.once('close', () => reject(new Error("Connection closed while waiting for request.")));
        });
    }

    getRawPayload() {
        if (!this._payload_info || this._payload_info.length === 0) return Buffer.alloc(0);
        const { offset, length, packet } = this._payload_info;
        return packet.slice(offset, offset + length);
    }

    getPayloadAsJSON() {
        const rawPayload = this.getRawPayload();
        if (rawPayload.length === 0) return null;
        try {
            return JSON.parse(rawPayload.toString('utf-8'));
        } catch (e) {
            return null;
        }
    }

    _write_response(success, payload) {
        const statusCode = success ? 1 : 0;
        const responseBuffer = Buffer.alloc(HEADER_SIZE_OUT + payload.length);
        
        let offset = 0;
        responseBuffer.writeUInt8(statusCode, offset);
        offset += 1;
        responseBuffer.writeUInt32LE(payload.length, offset);
        offset += 4;
        payload.copy(responseBuffer, offset);

        this.client.write(responseBuffer);
    }

    writeResponseAsJSON(dataObj, success = true) {
        try {
            const jsonString = JSON.stringify(dataObj);
            const payload = Buffer.from(jsonString, 'utf-8');

            logger.info(`writeResponseAsJSON → Success: ${success}, Payload: ${jsonString}`);

            this._write_response(success, payload);
        } catch (e) {
            const errorMessage = `JSON serialization failed: ${e}`;
            const errorPayload = Buffer.from(JSON.stringify({ error: errorMessage }), 'utf-8');

            logger.error(`writeResponseAsJSON → Error: ${errorMessage}`);

            this._write_response(false, errorPayload);
        }
    }

    writeRawResponse(data, success = true) {
        const statusCode = success ? 1 : 0;
        if (!Buffer.isBuffer(data)) {
            throw new TypeError("Raw response data must be of type Buffer.");
        }
        this._write_response(statusCode, data);
    }

    close_communication() {
        // This method can be a no-op or could close the client if needed.
        this.client.end();
    }
}

module.exports = { Bridge, Request };

