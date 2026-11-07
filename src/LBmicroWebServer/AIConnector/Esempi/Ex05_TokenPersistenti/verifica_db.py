#!/usr/bin/env python3
"""Mostra le righe della tabella Tokens di Ex05 (solo lettura).

Uso:  python3 verifica_db.py [percorso-del-db]      (predefinito: ex05_tokens.db)

Il token e' mostrato solo nei primi 8 caratteri. LastRequestTime e' un contatore di secondi: conta il valore e non
la data. Dopo 'check <token> --wait' deve essere aumentato rispetto a prima.
"""
import sqlite3
import sys

path = sys.argv[1] if len(sys.argv) > 1 else "ex05_tokens.db"
try:
    con = sqlite3.connect("file:%s?mode=ro" % path, uri=True)
    rows = con.execute("SELECT Token, IdUser, LastRequestTime, Config FROM Tokens ORDER BY LastRequestTime").fetchall()
except sqlite3.Error as e:
    sys.exit("Impossibile leggere %s: %s" % (path, e))

print("%d token in %s" % (len(rows), path))
for token, user, last, config in rows:
    print("  %s...  utente=%s  LastRequestTime=%s" % (token[:8], user, last))
    print("      Config: %s" % config)
