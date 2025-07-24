#!/bin/bash
SCRIPT_FOLDER="$1"
EXE_FOLDER="$2"

cd "$SCRIPT_FOLDER"
zip -r "$EXE_FOLDER"/python_runtime.zip worker.py lb_logger.py LBBridge/ tests/

