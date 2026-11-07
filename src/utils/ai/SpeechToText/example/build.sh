#!/bin/sh
# Compilazione con Ararat Synapse. Adattare SYNAPSE alla propria installazione.
set -eu
cd "$(dirname "$0")"
SRC="${SRC:-../src}"
SYNAPSE="${SYNAPSE:-/path/to/synapse/source/lib}"
OUT="${OUT:-bin}"
OPENSSL3="${OPENSSL3:-0}"
mkdir -p "$OUT/units"
DEFINES=""
if [ "$OPENSSL3" = "1" ]; then
  DEFINES="-dLB_SYNAPSE_OPENSSL3"
fi
fpc -MObjFPC -Sh -g -gl -vewnh $DEFINES \
  -FE"$OUT" -FU"$OUT/units" \
  -Fu"$SRC" -Fu"$SYNAPSE" \
  SpeechToTextTest.lpr
