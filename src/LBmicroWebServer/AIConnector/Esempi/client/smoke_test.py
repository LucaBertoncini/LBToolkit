#!/usr/bin/env python3
"""Prove automatiche degli esempi: si avvia l'esempio, poi si lancia questo script.

    python3 smoke_test.py ex01 http://127.0.0.1:8081 [--server-dir CARTELLA]
    python3 smoke_test.py ex02 http://127.0.0.1:8082
    python3 smoke_test.py ex03 http://127.0.0.1:8083
    python3 smoke_test.py ex04 http://127.0.0.1:8084

Ogni riga e' un controllo: PASS o FAIL. Il codice di uscita e' 0 solo se tutti passano.
--server-dir (solo ex01) e' la cartella da cui e' partito il server: serve a creare il file temp.disabled per
provare che il rinnovo del token rilegge l'utente.
"""
import json
import os
import sys
import urllib.parse

from mcp_client import McpClient, LoginFailed, header, http, as_json

results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print("%s  %s%s" % ("PASS" if ok else "FAIL", name, ("   [" + str(detail) + "]") if (detail and not ok) else ""))


def names(tools):
    return sorted(t["name"] for t in tools)


def raises_rpc(client, name, arguments, code):
    """True se la chiamata e' un errore di PROTOCOLLO JSON-RPC con quel codice (es. strumento sconosciuto)."""
    try:
        client.tool_call(name, arguments)
    except RuntimeError as e:
        return ("JSON-RPC %s" % code) in str(e)
    return False


def logged(base, user, password, path="/mcp"):
    client = McpClient(base, path)
    client.login(user, password)
    client.initialize()
    return client


# ------------------------------------------------------------------------------------------------ esempio 1
def ex01(base, opts):
    c = McpClient(base)
    status, headers, _ = c.probe()
    check("senza token il canale risponde 401", status == 401, status)
    check("la risposta dice dove autenticarsi (resource_metadata)", 'resource_metadata="' in header(headers, "WWW-Authenticate"))

    resource, server = c.metadata()
    check("metadati: la risorsa e' indirizzo + /mcp", resource.get("resource") == base + "/mcp", resource)
    check("metadati: PKCE S256 e' l'unico metodo", server.get("code_challenge_methods_supported") == ["S256"], server)

    status, body = c.register("https://evil.example/cb")
    check("un indirizzo di ritorno non ammesso e' rifiutato", status == 400 and body.get("error") == "invalid_redirect_uri", (status, body))

    try:
        c.login("mario", "sbagliata")
        check("password sbagliata: accesso negato", False)
    except LoginFailed as e:
        check("password sbagliata: accesso negato (401, la pagina si ripresenta)", e.status == 401 and "req" in e.page, e.status)

    tokens = c.login("mario", "pw1")
    check("login riuscito: access token di 900 secondi", tokens.get("expires_in") == 900, tokens.get("expires_in"))
    status, body = c.initialize()
    check("initialize risponde con serverInfo", status == 200 and "serverInfo" in body.get("result", {}), body)

    tools = c.tools_list()
    check("mario (lettore) vede 3 strumenti", names(tools) == ["demo_Ping", "demo_Saluta", "demo_Somma"], names(tools))
    somma = next(t for t in tools if t["name"] == "demo_Somma")
    check("lo schema di demo_Somma dichiara a e b interi obbligatori",
          somma["inputSchema"].get("required") == ["a", "b"] and somma["inputSchema"]["properties"]["a"]["type"] == "integer", somma["inputSchema"])
    check("annotazione readOnlyHint", somma.get("annotations", {}).get("readOnlyHint") is True, somma.get("annotations"))

    is_error, _, data = c.tool_call("demo_Ping")
    check("demo_Ping: l'agente lavora come mario (id 1, ruolo lettore)", not is_error and data == {"pong": True, "user_id": 1, "role": "lettore"}, data)
    is_error, _, data = c.tool_call("demo_Somma", {"a": 2, "b": 3})
    check("demo_Somma 2 + 3 = 5", not is_error and data == {"somma": 5}, data)
    is_error, _, data = c.tool_call("demo_Somma", {"a": "2", "b": "3"})
    check('numeri mandati come testo ("2", "3") sono convertiti', not is_error and data == {"somma": 5}, data)
    is_error, text, _ = c.tool_call("demo_Somma", {"a": "x", "b": 1})
    check("un valore non intero e' rifiutato PRIMA del worker, con un messaggio per l'agente", is_error and "must be an integer" in text, text)
    is_error, text, _ = c.tool_call("demo_Somma", {"a": 1})
    check("un parametro obbligatorio mancante e' segnalato", is_error and "required parameter" in text and '"b"' in text, text)
    is_error, text, _ = c.tool_call("demo_Somma", {"a": 1, "b": 2, "c": 3})
    check("un parametro sconosciuto e' segnalato (refuso dell'agente)", is_error and "unknown parameter" in text, text)
    is_error, text, _ = c.tool_call("demo_Saluta", {"nome": "   "})
    check("uno stato >= 400 del worker arriva come errore dello strumento", is_error and "vuoto" in text, text)
    is_error, _, data = c.tool_call("demo_Saluta", {"nome": "Luca"})
    check("demo_Saluta risponde", not is_error and data == {"messaggio": "Ciao Luca"}, data)
    check("uno strumento inesistente e' un errore JSON-RPC -32602", raises_rpc(c, "demo_Inesistente", {}, -32602))

    status, _ = c.rpc("tools/list", token="token-inventato")
    check("un token inventato e' rifiutato (401)", status == 401, status)

    old_refresh = c.refresh_token
    old_access = c.access
    status, body = c.refresh()
    check("il rinnovo restituisce una nuova coppia di token", status == 200 and c.access != old_access and c.refresh_token != old_refresh, (status, body))
    status, body = c.refresh(old_refresh)
    check("il vecchio refresh token non vale piu' (rotazione)", status == 400 and body.get("error") == "invalid_grant", (status, body))

    o = McpClient(base)
    o.login("ospite", "pw4")
    status, body = o.rpc("tools/list")
    check("ospite ha un token valido ma il suo ruolo non ha ai.Uso: il canale e' chiuso (403)", status == 403, (status, body))

    a = logged(base, "admin", "pw3")
    check("admin vede gli stessi 3 strumenti", names(a.tools_list()) == ["demo_Ping", "demo_Saluta", "demo_Somma"])

    if opts.get("server_dir"):
        flag = os.path.join(opts["server_dir"], "temp.disabled")
        t = McpClient(base)
        t.login("temp", "pw5")
        try:
            open(flag, "w").close()
            status, body = t.refresh()
            check("utente disattivato dopo il login: il RINNOVO viene rifiutato", status == 400 and body.get("error") == "invalid_grant", (status, body))
        finally:
            if os.path.exists(flag):
                os.remove(flag)
    else:
        print("SALTATO  revoca al rinnovo (serve --server-dir)")


# ------------------------------------------------------------------------------------------------ esempio 2
def ex02(base, opts):
    c = logged(base, "anna", "pw2")
    tools = c.tools_list()
    check("anna vede Cerca, Crea ed Elimina (Interna e' nascosta, NonDescritta non e' descritta)",
          names(tools) in (["catalogo_Cerca", "catalogo_Crea", "catalogo_Elimina"],
                           ["catalogo_Cerca", "catalogo_Crea", "catalogo_Elimina", "catalogo_NonDescritta"]), names(tools))
    check("catalogo_Interna non e' MAI esposta", "catalogo_Interna" not in names(tools))
    by_name = {t["name"]: t for t in tools}

    cerca = by_name["catalogo_Cerca"]
    props = cerca["inputSchema"]["properties"]
    check("schema: stato e' un'enumerazione", props["stato"].get("enum") == ["aperto", "chiuso"], props.get("stato"))
    check("schema: dal e' una data, alle una data e ora", props["dal"].get("format") == "date" and props["alle"].get("format") == "date-time")
    check("schema: ids e' un array di interi", props["ids"].get("type") == "array" and props["ids"]["items"]["type"] == "integer")
    check("schema: solo testo e' obbligatorio", cerca["inputSchema"].get("required") == ["testo"], cerca["inputSchema"].get("required"))
    check("annotazioni: Cerca readOnly, Crea modifica, Elimina distruttiva",
          by_name["catalogo_Cerca"]["annotations"].get("readOnlyHint") is True and
          by_name["catalogo_Crea"]["annotations"].get("readOnlyHint") is False and
          by_name["catalogo_Elimina"]["annotations"].get("destructiveHint") is True)

    is_error, _, data = c.tool_call("catalogo_Cerca", {
        "testo": "pompa", "stato": "aperto", "limite": "5", "soglia": "1.5", "solo_attivi": "true",
        "dal": "2026-02-28", "alle": "2026-02-28T10:30:00", "ids": [1, "2", 3]})
    got = (data or {}).get("ricevuto", {})
    check("il worker riceve i valori GIA' convertiti (limite 5, soglia 1.5, solo_attivi true, ids numerici)",
          not is_error and got.get("limite") == 5 and got.get("soglia") == 1.5 and got.get("solo_attivi") is True and got.get("ids") == [1, 2, 3], data)

    def rifiutata(arguments, parte):
        err, text, _ = c.tool_call("catalogo_Cerca", arguments)
        return err and parte in text, text

    for label, args, part in [
        ("manca il parametro obbligatorio", {}, "required parameter"),
        ("valore fuori dall'enumerazione", {"testo": "x", "stato": "sospeso"}, "must be one of"),
        ("data inesistente (30 febbraio)", {"testo": "x", "dal": "2026-02-30"}, "valid date"),
        ("data e ora senza l'ora", {"testo": "x", "alle": "2026-02-28"}, "date and time"),
        ("decimale al posto di un intero", {"testo": "x", "limite": 2.5}, "not a decimal"),
        ("elemento di array non numerico", {"testo": "x", "ids": [1, "a"]}, "item 2"),
        ("booleano non valido", {"testo": "x", "solo_attivi": "forse"}, "true or false"),
        ("parametro sconosciuto", {"testo": "x", "tetso": "y"}, "unknown parameter"),
    ]:
        ok, text = rifiutata(args, part)
        check("rifiutata: " + label, ok, text)

    is_error, _, data = c.tool_call("catalogo_Crea", {"titolo": "Nuovo", "priorita": "alta"})
    check("Crea: lo stato 201 e' un successo", not is_error and data and data.get("creato") is True, data)
    is_error, text, _ = c.tool_call("catalogo_Elimina", {"id": 99})
    check("Elimina: lo stato 404 arriva come errore dello strumento, col testo del worker", is_error and "non trovato" in text, text)

    m = logged(base, "mario", "pw1")
    check("mario (lettore) non vede Crea ed Elimina",
          names(m.tools_list()) in (["catalogo_Cerca"], ["catalogo_Cerca", "catalogo_NonDescritta"]), names(m.tools_list()))
    is_error, text, _ = m.tool_call("catalogo_Crea", {"titolo": "x", "priorita": "alta"})
    check("mario non puo' chiamare Crea nemmeno conoscendone il nome (403)", is_error and "not authorised" in text, text)


# ------------------------------------------------------------------------------------------------ esempio 3
def ex03(base, opts):
    o = McpClient(base)
    o.login("ospite", "pw4")
    status, _ = o.rpc("tools/list")
    check("ospite: il canale e' chiuso a tutto il ruolo (403)", status == 403, status)

    m = logged(base, "mario", "pw1")
    check("mario vede solo ListaOrdini", names(m.tools_list()) == ["officina_ListaOrdini"], names(m.tools_list()))
    is_error, text, _ = m.tool_call("officina_ChiudiOrdine", {})
    check("mario non puo' chiamare ChiudiOrdine (403 nel risultato dello strumento)", is_error and "not authorised" in text, text)

    a = logged(base, "admin", "pw3")
    tools = a.tools_list()
    check("admin vede ListaOrdini e ChiudiOrdine, NON Stato (nessun OpType: RequireOpTypeForTools)",
          names(tools) == ["officina_ChiudiOrdine", "officina_ListaOrdini"], names(tools))
    chiudi = next(t for t in tools if t["name"] == "officina_ChiudiOrdine")
    check("ChiudiOrdine non e' descritta: accetta un oggetto libero", chiudi["inputSchema"].get("additionalProperties") is True, chiudi["inputSchema"])
    check("officina_Stato non esiste per nessuno (errore JSON-RPC -32602)", raises_rpc(a, "officina_Stato", {}, -32602))

    status, _ = a.rpc("tools/list", extra_headers={"Origin": "http://evil.example"})
    check("una Origin estranea e' rifiutata (403)", status == 403, status)
    status, _ = a.rpc("tools/list", extra_headers={"Origin": "http://127.0.0.1:8083"})
    check("una Origin elencata in AllowedOrigins e' accettata", status == 200, status)

    t = logged(base, "anna", "pw2")
    check("anna vede ListaOrdini e ChiudiOrdine", names(t.tools_list()) == ["officina_ChiudiOrdine", "officina_ListaOrdini"])
    limited = 0
    for _ in range(12):   # il limite e' 5 al minuto: in 12 chiamate almeno una supera il limite anche a cavallo di due minuti
        is_error, text, _ = t.tool_call("officina_ListaOrdini", {"stato": "aperto"})
        limited += 1 if (is_error and "Too many requests" in text) else 0
    check("oltre 5 chiamate al minuto: 'Too many requests'", limited >= 1, limited)
    print("NOTA     controlla nel registro del server le righe AUDIT (una per ogni chiamata, in qualunque esito)")


# ------------------------------------------------------------------------------------------------ esempio 4
def ex04(base, opts):
    status, headers, _ = http("GET", base + "/whoami")
    check("GET /whoami senza token: 401", status == 401, status)
    challenge = header(headers, "WWW-Authenticate")
    check("la risposta punta ai metadati della risorsa /whoami", "/.well-known/oauth-protected-resource/whoami" in challenge, challenge)

    c = McpClient(base, "/whoami")
    _, _, raw = http("GET", base + "/.well-known/oauth-protected-resource/whoami")
    resource = as_json(raw) or {}
    check("metadati: la risorsa e' indirizzo + /whoami (non /mcp)", resource.get("resource") == base + "/whoami", resource)
    check("metadati: il titolo e' quello del file INI", resource.get("resource_name") == "Portale Manutenzione", resource)
    check("metadati: lo scope e' quello scelto dall'applicazione (portale)", resource.get("scopes_supported") == ["portale"], resource)

    try:
        c.login("mario", "sbagliata")
        check("password sbagliata: accesso negato", False)
    except LoginFailed as e:
        check("dicitura personalizzata per le credenziali errate", e.status == 401 and "Credenziali errate." in e.page, e.page[:200])

    tokens = c.login("mario", "pw1")
    check("la risposta del token riporta lo scope scelto (portale)", tokens.get("scope") == "portale", tokens)
    page = c.last_login_page
    host = urllib.parse.urlparse("http://127.0.0.1:53682/callback").netloc
    check("pagina di accesso: titolo, intestazione, etichette e pulsante personalizzati",
          all(x in page for x in ("Portale Manutenzione", "Accesso al portale", "Utente", "Parola d", "Entra")), page[:300])
    check("pagina di accesso: {host} e {app} sostituiti nel testo",
          ("Per usare Portale Manutenzione da " + host + " inserisci le tue credenziali.") in page and "{host}" not in page, page[:400])

    status, _, raw = http("GET", base + "/whoami", {"Authorization": "Bearer " + c.access})
    data = as_json(raw)
    check("con il token emesso dal login /whoami risponde con l'utente", status == 200 and data == {"user_id": 1, "role": "lettore"}, (status, data))
    status, _, _ = http("GET", base + "/whoami", {"Authorization": "Bearer token-inventato"})
    check("un token inventato e' rifiutato (401)", status == 401, status)
    # Una rotta che nessun processore gestisce non riceve risposta: il server chiude la connessione. Entrambi gli esiti
    # (404 oppure connessione chiusa) significano "il canale /mcp non esiste".
    try:
        status, _, _ = http("POST", base + "/mcp", {"Authorization": "Bearer " + c.access}, {"jsonrpc": "2.0", "id": 1, "method": "ping"})
        absent = status == 404
    except OSError:
        absent = True
    check("senza connettore il canale /mcp non esiste", absent)


SCENARIOS = {"ex01": ex01, "ex02": ex02, "ex03": ex03, "ex04": ex04}


def main(argv):
    if len(argv) < 3 or argv[1] not in SCENARIOS:
        sys.exit(__doc__)
    opts = {}
    if "--server-dir" in argv:
        opts["server_dir"] = argv[argv.index("--server-dir") + 1]
    try:
        SCENARIOS[argv[1]](argv[2].rstrip("/"), opts)
    except Exception as e:   # un errore di rete o di protocollo e' un fallimento, non un'eccezione muta
        check("lo scenario e' arrivato in fondo", False, "%s: %s" % (type(e).__name__, e))
    passed = sum(results)
    print("\n%d controlli, %d superati, %d falliti" % (len(results), passed, len(results) - passed))
    sys.exit(0 if passed == len(results) else 1)


if __name__ == "__main__":
    main(sys.argv)
