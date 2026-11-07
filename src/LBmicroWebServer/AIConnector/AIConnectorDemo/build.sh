#!/bin/sh
# Compila l'esempio con Free Pascal.  Uso:   ./build.sh
#
# I percorsi dei TUOI sorgenti si passano come variabili d'ambiente (qui i valori predefiniti: le cartelle
# dei sorgenti accanto a questa):
#
#   LB      cartella LBmicroWebServer  (uLBmicroWebServer.pas, uWebRouteRegistry.pas, ... e le sottocartelle
#                                       AIConnector/ e OAuth/)
#   UTILS   cartella utils             (uHTTPConsts.pas, uLBBaseThread.pas, SQLiteWrapper.pas, uLBFileUtils.pas,
#                                       uLBApplicationBoostrap.pas ... e la sottocartella Token/)
#   LOGGER  cartella LBLogger          (ULBLogger.pas)
#   EXTRA   altri "-Fu<cartella>": le unit di Lazarus (LazUtils: Laz2_DOM, LazFileUtils, LazSysUtils, ...) e di
#           Synapse (blcksock, ssl_openssl3, ...) se il tuo fpc non le trova da solo. Con CodeTyphon/Lazarus sono
#           le stesse che usi per compilare IdroWorld.
#
#   Esempio:
#     LB=~/src/LBmicroWebServer UTILS=~/src/utils LOGGER=~/src/LBLogger \
#     EXTRA="-Fu/usr/share/lazarus/3.0/components/lazutils/lib/x86_64-linux" ./build.sh
#
# Risultato: bin/AIDemo

set -e
cd "$(dirname "$0")"

LB="${LB:-../LBmicroWebServer}"
UTILS="${UTILS:-../utils}"
LOGGER="${LOGGER:-../LBLogger}"
EXTRA="${EXTRA:-}"
OUT="bin"

for d in "$LB" "$LB/AIConnector" "$LB/OAuth" "$UTILS" "$UTILS/Token" "$LOGGER"; do
  if [ ! -d "$d" ]; then
    echo "Cartella non trovata: $d" >&2
    echo "Imposta LB, UTILS e LOGGER (vedi l'intestazione di questo script)." >&2
    exit 1
  fi
done

mkdir -p "$OUT/units"

# -MObjFPC -Sh    : modalita' ObjFPC con stringhe lunghe (le unit lo dichiarano gia' da sole)
# -g -gl          : informazioni di debug e numeri di riga negli errori
# -vewnh          : mostra errori, avvisi, note e suggerimenti
# -FE / -FU       : eseguibile in bin/, file intermedi in bin/units/
set -x
fpc -MObjFPC -Sh -g -gl -vewnh \
    -FE"$OUT" -FU"$OUT/units" \
    -Fu"src" -Fu"$LB" -Fu"$LB/AIConnector" -Fu"$LB/OAuth" \
    -Fu"$UTILS" -Fu"$UTILS/Token" -Fu"$LOGGER" \
    $EXTRA \
    src/AIDemo.lpr
