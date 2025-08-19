/**
 * @file logger.js
 * A simple file-based logger for the Node.js wrapper, mirroring the Python logger.
 */

const fs = require('fs');
const path = require('path');
const util = require('util');

let IS_LOG_DISABLED = true;

class Logger {
    constructor() {
        this.logDir = path.join(__dirname, '..', 'logs');
        this.logFile = path.join(this.logDir, 'WebNodeBridge.log');
        this._ensureLogDirectory();
    }

    _ensureLogDirectory() {
        if (!fs.existsSync(this.logDir)) {
            fs.mkdirSync(this.logDir, { recursive: true });
        }
    }

    _timestamp() {
        return new Date().toISOString().replace('T', ' ').substr(0, 19);
    }


    _writeToLog(level, message) {
        if (IS_LOG_DISABLED) {
            return;
        }
        const logEntry = `[${this._timestamp()}] ${level}: ${message}\n`;
        try {
            fs.appendFileSync(this.logFile, logEntry, 'utf-8');
        } catch (err) {
            // Silently ignore write errors
        }
    }

    info(message) {
        this._writeToLog('#INF#', message);
    }

    warning(message) {
        this._writeToLog('#WRN#', message);
    }

    error(err) {
        if (!(err instanceof Error)) {
            this._writeToLog('#ERR#', util.inspect(err));
            return;
        }
        this._writeToLog('#ERR#', err.message);
        if (err.stack) {
            this._writeToLog('#TRACE#', err.stack);
        }
    }
}

// Global instance
const logger = new Logger();

// Function to disable/enable the logger
function disable_logger(value) {
    IS_LOG_DISABLED = !!value;
}

module.exports = {
    logger,
    disable_logger,
};

