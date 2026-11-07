#!/usr/bin/env python3
"""Prove automatiche dell'esempio: avvia l'eseguibile vero, lo prova con un client OAuth + MCP e lo ferma.

    python3 run_tests.py                       # usa ../bin/AIDemo e le configurazioni in ../config
    python3 run_tests.py --bin /percorso/AIDemo --port 8090
    python3 run_tests.py --only base           # solo uno scenario: base | all | persist

Scenari (ognuno avvia il server con una configurazione diversa):
  base      AIDemo.ini          funzionamento normale: OAuth, MCP, permessi per ruolo, errori, rinnovo, revoca
  all       AIDemo_all.ini      ExposeUndescribed, RequireOpTypeForTools, limite di chiamate, Origin
  persist   AIDemo_persist.ini  token su SQLite: sopravvivono a un riavvio (richiede libsqlite3)

Il codice di uscita e' 0 solo se TUTTI i controlli passano. Se un controllo fallisce vengono mostrate le ultime
righe del registro del server (bin/logs/aidemo.log).
"""
import argparse
import json
import os
import re
import shutil
import signal
import socket
import subprocess
import sys
import threading
import time
import urllib.parse

from mcp_client import McpClient, LoginFailed, header, http, as_json

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)


# ---------------------------------------------------------------------------------------------- esito dei controlli
class Checker:
    def __init__(self):
        self.passed = 0
        self.failed = 0

    def check(self, name, ok, detail=""):
        if ok:
            self.passed += 1
            print("PASS  " + name)
        else:
            self.failed += 1
            print("FAIL  " + name + (("   [" + str(detail)[:300] + "]") if detail != "" else ""))
        sys.stdout.flush()
        return bool(ok)

    def section(self, title):
        print("\n== " + title)


def names(tools):
    return sorted(t["name"] for t in tools)


def rpc_error(client, name, arguments, code):
    """True se la chiamata e' un errore di PROTOCOLLO JSON-RPC con quel codice."""
    try:
        client.tool_call(name, arguments)
    except RuntimeError as e:
        return ("JSON-RPC %s" % code) in str(e)
    return False


def logged(base, user, password):
    client = McpClient(base)
    client.login(user, password)
    client.initialize()
    return client


# ---------------------------------------------------------------------------------------------- il server sotto prova
class Server:
    """Avvia e ferma l'eseguibile. Scrive una copia della configurazione con la porta scelta, accanto all'originale."""

    def __init__(self, binary, ini_name, port):
        self.binary = os.path.abspath(binary)
        self.workdir = os.path.dirname(self.binary)
        self.template = os.path.join(ROOT, "config", ini_name)
        self.config_dir = os.path.dirname(self.template)
        self.port = port
        self.base = "http://127.0.0.1:%d" % port
        self.ini = os.path.join(self.config_dir, ".run_%d_%s" % (os.getpid(), ini_name))
        self.proc = None
        self.output = []
        self._thread = None
        self.log_file = os.path.join(self.workdir, "logs", "aidemo.log")
        text = open(self.template, encoding="utf-8").read()
        text = re.sub(r"(?m)^Port=\d+", "Port=%d" % port, text)
        text = text.replace("127.0.0.1:8081", "127.0.0.1:%d" % port)
        with open(self.ini, "w", encoding="utf-8") as f:
            f.write(text)

    def _reader(self, proc):
        for line in proc.stdout:
            self.output.append(line.rstrip("\n"))

    def start(self):
        """True se il server e' pronto e accetta connessioni."""
        self.output = []
        self.proc = subprocess.Popen([self.binary, self.ini], cwd=self.workdir, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT, text=True, bufsize=1)
        self._thread = threading.Thread(target=self._reader, args=(self.proc,), daemon=True)
        self._thread.start()
        deadline = time.time() + 20
        while time.time() < deadline:
            if any(line.startswith("READY") for line in self.output):
                break
            if self.proc.poll() is not None:
                return False
            time.sleep(0.1)
        else:
            return False
        deadline = time.time() + 10
        while time.time() < deadline:
            try:
                socket.create_connection(("127.0.0.1", self.port), timeout=1).close()
                return True
            except OSError:
                time.sleep(0.2)
        return False

    def stop(self):
        """Ferma il server con SIGTERM. Restituisce il codice di uscita (None se ha dovuto essere ucciso)."""
        if self.proc is None:
            return 0
        code = None
        if self.proc.poll() is None:
            self.proc.send_signal(signal.SIGTERM)
            try:
                code = self.proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                self.proc.kill()
                self.proc.wait()
        else:
            code = self.proc.returncode
        self.proc = None
        return code

    def restart(self):
        code = self.stop()
        return self.start(), code

    def cleanup(self):
        self.stop()
        for path in (self.ini, os.path.join(self.config_dir, "temp.disabled")):
            if os.path.exists(path):
                os.remove(path)

    def diagnostics(self):
        if self._thread is not None:
            self._thread.join(timeout=2)   # lascia al lettore il tempo di raccogliere l'ultima uscita del processo
        lines = ["--- uscita del processo:"] + ["    " + l for l in self.output[-15:]]
        if os.path.exists(self.log_file):
            with open(self.log_file, encoding="utf-8", errors="replace") as f:
                tail = f.read().splitlines()[-25:]
            lines += ["--- ultime righe di " + self.log_file + ":"] + ["    " + l for l in tail]
        else:
            lines.append("--- nessun registro in " + self.log_file)
        return "\n".join(lines)


# ---------------------------------------------------------------------------------------------- scenario "base"
def scenario_base(t, server):
    base = server.base

    t.section("Server e canale")
    status, _, body = http("GET", base + "/test")
    t.check("GET /test: il web server risponde", status == 200 and b"Micro-WebServer" in body, status)

    status, _, _ = http("POST", base + "/api/demo/ping", body={})
    t.check("una funzione dal SITO senza cookie di sessione: 401 (l'agente non passa da qui)", status == 401, status)

    c = McpClient(base)
    status, headers, _ = c.probe()
    t.check("/mcp senza token: 401", status == 401, status)
    t.check("la risposta dice dove autenticarsi (resource_metadata)",
            'resource_metadata="' in header(headers, "WWW-Authenticate"), headers)

    status, headers, _ = c.probe(token="token-inventato")
    t.check("/mcp con un token inventato: 401 con error=invalid_token",
            status == 401 and "invalid_token" in header(headers, "WWW-Authenticate"), (status, headers))

    t.section("Scoperta OAuth")
    resource, auth = c.metadata()
    t.check("risorsa protetta = indirizzo pubblico + /mcp", resource.get("resource") == base + "/mcp", resource)
    t.check("la risorsa indica questo stesso server come server di autorizzazione",
            resource.get("authorization_servers") == [base], resource)
    t.check("il titolo della risorsa e' quello del file INI (ServerTitle)", resource.get("resource_name") == "AIDemo", resource)
    t.check("gli indirizzi OAuth sono sotto /oauth",
            auth.get("authorization_endpoint") == base + "/oauth/authorize" and
            auth.get("token_endpoint") == base + "/oauth/token" and
            auth.get("registration_endpoint") == base + "/oauth/register", auth)
    t.check("PKCE S256 e' l'unico metodo ammesso", auth.get("code_challenge_methods_supported") == ["S256"], auth)

    t.section("Registrazione e accesso")
    status, body = McpClient(base).register("https://evil.example/callback")
    t.check("un indirizzo di ritorno non ammesso e' rifiutato (invalid_redirect_uri)",
            status == 400 and body.get("error") == "invalid_redirect_uri", (status, body))

    c = McpClient(base)
    status, body = c.register()
    t.check("registrazione del client con indirizzo loopback: 201 e client_id", status == 201 and body.get("client_id", "").startswith("mcp-"), (status, body))

    _, challenge = McpClient.new_pkce()
    status, headers, _ = http("GET", c.authorize_url(challenge, "s1", redirect_uri="https://evil.example/callback"))
    t.check("authorize con un indirizzo di ritorno non ammesso: errore mostrato all'utente, nessun reindirizzamento",
            status == 400 and not header(headers, "Location"), (status, headers))

    status, headers, _ = http("GET", c.authorize_url(None, "s1"))   # senza code_challenge
    t.check("authorize senza PKCE: reindirizzamento con error=invalid_request",
            status == 302 and "error=invalid_request" in header(headers, "Location"), (status, headers))

    req_id, verifier, state = c.start_authorization()
    page = c.last_login_page
    t.check("pagina di accesso: titolo e testo con {host} e {app} sostituiti",
            "AIDemo" in page and "di usare AIDemo con i tuoi permessi" in page and "{host}" not in page and "{app}" not in page,
            page[:400])

    status, headers, raw = c.submit_login(req_id, "mario", "sbagliata")
    t.check("password sbagliata: 401 e la pagina si ripresenta (la richiesta non e' consumata)",
            status == 401 and 'name="req"' in raw.decode("utf-8", "replace"), status)

    status, headers, raw = c.submit_login(req_id, "mario", "pw1")
    t.check("password giusta sulla stessa richiesta: 302 verso l'indirizzo di ritorno con code e state",
            status == 302 and "code=" in header(headers, "Location") and ("state=" + state) in header(headers, "Location"),
            (status, headers))
    code = urllib.parse.parse_qs(urllib.parse.urlparse(header(headers, "Location")).query)["code"][0]

    status, body = c.exchange_code(code, "verificatore-sbagliato-" + "x" * 30)
    t.check("scambio del codice con un verificatore PKCE sbagliato: invalid_grant",
            status == 400 and body.get("error") == "invalid_grant", (status, body))
    status, body = c.exchange_code(code, verifier)
    t.check("... e il codice e' bruciato: nemmeno quello giusto funziona piu'",
            status == 400 and body.get("error") == "invalid_grant", (status, body))

    tokens = c.login("mario", "pw1")
    t.check("login completo: Bearer, scope mcp, access token di 900 secondi",
            tokens.get("token_type") == "Bearer" and tokens.get("scope") == "mcp" and tokens.get("expires_in") == 900, tokens)
    t.check("access e refresh token sono diversi", tokens["access_token"] != tokens["refresh_token"])

    status, _, raw = http("POST", base + "/oauth/token", {"Content-Type": "application/x-www-form-urlencoded"}, "grant_type=password")
    t.check("grant_type non supportato: unsupported_grant_type",
            status == 400 and (as_json(raw) or {}).get("error") == "unsupported_grant_type", (status, raw[:100]))

    t.section("Protocollo MCP")
    status, body, note = c.initialize()
    result = (body or {}).get("result", {})
    t.check("initialize: serverInfo.name e' ServerName del file INI", result.get("serverInfo", {}).get("name") == "AIDemo", body)
    t.check("initialize: il server dichiara le funzioni 'tools' e le istruzioni dell'INI",
            "tools" in result.get("capabilities", {}) and "demo_Ping" in result.get("instructions", ""), body)
    t.check("notifications/initialized (senza id): HTTP 202, nessuna risposta", note == 202, note)

    status, body = c.rpc("ping")
    t.check("ping: risultato vuoto", status == 200 and body.get("result") == {}, body)
    status, body = c.rpc("metodo/inesistente")
    t.check("metodo sconosciuto: errore JSON-RPC -32601", status == 200 and body.get("error", {}).get("code") == -32601, body)

    status, _, body = c.post_mcp([{"jsonrpc": "2.0", "id": 1, "method": "ping"}, {"jsonrpc": "2.0", "id": 2, "method": "tools/list"}])
    t.check("batch di due richieste: due risposte, nell'ordine", status == 200 and isinstance(body, list) and
            [m.get("id") for m in body] == [1, 2], (status, body))
    status, _, body = c.post_mcp("{questo non e' json")
    t.check("JSON non valido: HTTP 400 e errore -32700", status == 400 and (body or {}).get("error", {}).get("code") == -32700, (status, body))
    status, hdrs, _ = c.post_mcp(None, method="GET")
    t.check("GET su /mcp: 405 con Allow: POST (niente flusso dal server al client)",
            status == 405 and header(hdrs, "Allow") == "POST", (status, hdrs))

    status, _, _ = c.post_mcp({"jsonrpc": "2.0", "id": 1, "method": "ping"}, extra_headers={"Origin": "http://evil.example"})
    t.check("una richiesta con Origin estranea e' rifiutata (403)", status == 403, status)
    status, _, _ = c.post_mcp({"jsonrpc": "2.0", "id": 1, "method": "ping"}, extra_headers={"Origin": base})
    t.check("una richiesta con l'Origin pubblica del server e' accettata", status == 200, status)

    t.section("Strumenti visibili: dipendono dal ruolo dell'utente")
    tools = c.tools_list()
    t.check("mario (lettore): Cerca, Ping, Somma e Stato (Stato non ha OpType)",
            names(tools) == ["demo_Cerca", "demo_Ping", "demo_Somma", "demo_Stato"], names(tools))
    by = {x["name"]: x for x in tools}
    t.check("demo_Interna (Hidden) e demo_NonDescritta (non descritta) non sono strumenti",
            "demo_Interna" not in by and "demo_NonDescritta" not in by)
    somma = by["demo_Somma"]["inputSchema"]
    t.check("schema di demo_Somma: a e b interi obbligatori, nessun altro parametro",
            somma.get("required") == ["a", "b"] and somma["properties"]["a"]["type"] == "integer" and
            somma.get("additionalProperties") is False, somma)
    cerca = by["demo_Cerca"]["inputSchema"]["properties"]
    t.check("schema di demo_Cerca: enumerazione, data, data e ora, array di interi",
            cerca["stato"].get("enum") == ["aperto", "chiuso"] and cerca["dal"].get("format") == "date" and
            cerca["alle"].get("format") == "date-time" and cerca["ids"]["items"]["type"] == "integer", cerca)
    t.check("demo_Cerca ha il titolo dato nel codice; demo_Somma e' di sola lettura",
            by["demo_Cerca"].get("title") == "Ricerca nel catalogo" and by["demo_Somma"]["annotations"]["readOnlyHint"] is True)
    t.check("la descrizione dello strumento e' quella di Routes.xml", "Somma due numeri interi" in by["demo_Somma"]["description"])

    anna = logged(base, "anna", "pw2")
    tools = anna.tools_list()
    t.check("anna (tecnico): in piu' CreaNota ed EliminaNota",
            names(tools) == ["demo_Cerca", "demo_CreaNota", "demo_EliminaNota", "demo_Ping", "demo_Somma", "demo_Stato"], names(tools))
    by = {x["name"]: x for x in tools}
    t.check("annotazioni: CreaNota modifica, EliminaNota e' distruttiva",
            by["demo_CreaNota"]["annotations"]["readOnlyHint"] is False and
            by["demo_CreaNota"]["annotations"]["destructiveHint"] is False and
            by["demo_EliminaNota"]["annotations"]["destructiveHint"] is True)
    admin = logged(base, "admin", "pw3")
    t.check("admin: vede anche Report", "demo_Report" in names(admin.tools_list()))
    t.check("anna non vede Report", "demo_Report" not in names(tools))

    o = McpClient(base)
    o.login("ospite", "pw4")
    status, body = o.rpc("tools/list")
    t.check("ospite ha un token valido ma il suo ruolo non ha ai.Uso: il canale e' chiuso (403)", status == 403, (status, body))

    t.section("Chiamate: l'agente lavora con i permessi dell'utente")
    is_error, _, data = c.tool_call("demo_Ping")
    t.check("demo_Ping: l'agente e' mario (id 1, ruolo lettore, i suoi permessi)",
            not is_error and data == {"pong": True, "user_id": 1, "role": "lettore", "permissions": ["ai.Uso", "demo.Lettura"]}, data)
    is_error, _, data = anna.tool_call("demo_Ping")
    t.check("la stessa funzione, con il token di anna: id 2 e ruolo tecnico", not is_error and data["user_id"] == 2 and data["role"] == "tecnico", data)
    is_error, _, data = c.tool_call("demo_Somma", {"a": 2, "b": 3})
    t.check("demo_Somma 2 + 3 = 5", not is_error and data == {"somma": 5}, data)
    is_error, _, data = c.tool_call("demo_Somma", {"a": "2", "b": "3"})
    t.check('numeri mandati come testo ("2", "3") sono convertiti', not is_error and data == {"somma": 5}, data)
    is_error, _, data = c.tool_call("demo_Stato")
    t.check("demo_Stato (senza OpType) risponde", not is_error and data == {"status": "ok"}, data)

    t.section("Chiamate: gli argomenti sono controllati PRIMA della funzione")
    is_error, _, data = c.tool_call("demo_Cerca", {
        "testo": "pompa", "stato": "aperto", "limite": "5", "soglia": "1.5", "solo_attivi": "true",
        "dal": "2026-02-28", "alle": "2026-02-28T10:30:00", "ids": [1, "2", 3]})
    got = (data or {}).get("ricevuto", {})
    t.check("la funzione riceve i valori GIA' convertiti (limite 5, soglia 1.5, solo_attivi true, ids numerici)",
            not is_error and got.get("limite") == 5 and got.get("soglia") == 1.5 and
            got.get("solo_attivi") is True and got.get("ids") == [1, 2, 3], data)

    rifiutate = [
        ("parametro obbligatorio mancante", "demo_Somma", {"a": 1}, "required parameter"),
        ("valore non intero", "demo_Somma", {"a": "x", "b": 1}, "must be an integer"),
        ("parametro sconosciuto (un refuso dell'agente)", "demo_Somma", {"a": 1, "b": 2, "c": 3}, "unknown parameter"),
        ("valore fuori dall'enumerazione", "demo_Cerca", {"testo": "x", "stato": "sospeso"}, "must be one of"),
        ("data inesistente (30 febbraio)", "demo_Cerca", {"testo": "x", "dal": "2026-02-30"}, "valid date"),
        ("data e ora senza l'ora", "demo_Cerca", {"testo": "x", "alle": "2026-02-28"}, "date and time"),
        ("decimale al posto di un intero", "demo_Cerca", {"testo": "x", "limite": 2.5}, "not a decimal"),
        ("elemento di array non numerico", "demo_Cerca", {"testo": "x", "ids": [1, "a"]}, "item 2"),
        ("booleano non valido", "demo_Cerca", {"testo": "x", "solo_attivi": "forse"}, "true or false"),
    ]
    for label, tool, args, part in rifiutate:
        is_error, text, _ = c.tool_call(tool, args)
        t.check("rifiutata, con un messaggio per l'agente: " + label, is_error and part in text, text)
    status, body = c.rpc("tools/call", {"name": "demo_Ping", "arguments": [1]})
    t.check("argomenti che non sono un oggetto: rifiutati", body["result"]["isError"] is True and "JSON object" in body["result"]["content"][0]["text"], body)

    t.section("Chiamate: permessi, errori e strumenti inesistenti")
    is_error, text, _ = c.tool_call("demo_CreaNota", {"titolo": "x", "priorita": "alta"})
    t.check("mario non puo' chiamare CreaNota nemmeno conoscendone il nome", is_error and "not authorised" in text, text)
    is_error, _, data = anna.tool_call("demo_CreaNota", {"titolo": "Pompa 3", "priorita": "alta"})
    t.check("anna crea una nota: lo stato 201 e' un successo", not is_error and data == {"creata": True, "titolo": "Pompa 3", "priorita": "alta", "autore": 2}, data)
    is_error, text, _ = anna.tool_call("demo_EliminaNota", {"id": 99})
    t.check("lo stato 404 della funzione arriva come errore dello strumento, con il suo testo", is_error and "Nota non trovata: 99" in text, text)
    t.check("uno strumento inesistente e' un errore JSON-RPC -32602", rpc_error(c, "demo_Inesistente", {}, -32602))
    t.check("demo_Interna (Hidden) non esiste per nessun ruolo: -32602 anche per l'amministratore", rpc_error(admin, "demo_Interna", {}, -32602))
    t.check("demo_NonDescritta non e' esposta (ExposeUndescribed=0): -32602", rpc_error(c, "demo_NonDescritta", {}, -32602))

    t.section("Rinnovo e revoca")
    old_refresh, old_access = c.refresh_token, c.access
    status, body = c.refresh()
    t.check("il rinnovo restituisce una nuova coppia di token", status == 200 and c.access != old_access and c.refresh_token != old_refresh, (status, body))
    status, body = c.refresh(old_refresh)
    t.check("il vecchio refresh token non vale piu' (rotazione)", status == 400 and body.get("error") == "invalid_grant", (status, body))
    status, _ = c.rpc("tools/list")
    t.check("il nuovo access token funziona", status == 200, status)

    tmp = McpClient(base)
    tmp.login("temp", "pw5")
    flag = os.path.join(server.config_dir, "temp.disabled")
    try:
        open(flag, "w").close()
        status, body = tmp.refresh()
        t.check("utente disattivato dopo il login: il RINNOVO viene rifiutato (RefreshUser)",
                status == 400 and body.get("error") == "invalid_grant", (status, body))
    finally:
        if os.path.exists(flag):
            os.remove(flag)
    try:
        McpClient(base).login("temp", "pw5")
        t.check("temp.disabled rimosso: il login torna possibile", True)
    except Exception as e:
        t.check("temp.disabled rimosso: il login torna possibile", False, e)

    t.section("Blocco dei tentativi di accesso (ultimo: blocca l'indirizzo fino al riavvio)")
    z = McpClient(base)
    req_id, _, _ = z.start_authorization()
    statuses = [z.submit_login(req_id, "nessuno", "x%d" % i)[0] for i in range(6)]
    t.check("5 password sbagliate: 401; la sesta richiesta riceve 429", statuses == [401] * 5 + [429], statuses)
    status, headers, _ = z.submit_login(req_id, "mario", "pw1")
    t.check("a indirizzo bloccato nemmeno la password giusta passa, e Retry-After lo dice",
            status == 429 and header(headers, "Retry-After") == "600", (status, headers))


# ---------------------------------------------------------------------------------------------- scenario "all"
def scenario_all(t, server):
    base = server.base

    t.section("Opzioni del connettore (AIDemo_all.ini)")
    c = logged(base, "mario", "pw1")
    tools = c.tools_list()
    t.check("ExposeUndescribed=1 e RequireOpTypeForTools=1: NonDescritta si', Stato no, Interna mai",
            names(tools) == ["demo_Cerca", "demo_NonDescritta", "demo_Ping", "demo_Somma"], names(tools))
    by = {x["name"]: x for x in tools}
    t.check("una funzione non descritta accetta un oggetto libero (additionalProperties)",
            by["demo_NonDescritta"]["inputSchema"].get("additionalProperties") is True, by["demo_NonDescritta"]["inputSchema"])
    is_error, _, data = c.tool_call("demo_NonDescritta", {"qualunque": 1, "cosa": "x"})
    t.check("... e riceve gli argomenti cosi' come sono", not is_error and data == {"ricevuto": {"qualunque": 1, "cosa": "x"}}, data)
    t.check("demo_Stato (senza OpType) non e' uno strumento: -32602", rpc_error(c, "demo_Stato", {}, -32602))

    status, _, _ = c.post_mcp({"jsonrpc": "2.0", "id": 1, "method": "ping"}, extra_headers={"Origin": "http://evil.example"})
    t.check("Origin estranea: 403", status == 403, status)
    status, _, _ = c.post_mcp({"jsonrpc": "2.0", "id": 1, "method": "ping"}, extra_headers={"Origin": base})
    t.check("Origin elencata in AllowedOrigins: accettata", status == 200, status)

    results = []
    for _ in range(12):
        is_error, text, _ = c.tool_call("demo_Ping")
        results.append(not is_error or "Too many requests" not in text)
    t.check("MaxCallsPerMinute=5: la prima chiamata passa e, in 12, almeno una riceve 'Too many requests'",
            results[0] is True and False in results, results)


# ---------------------------------------------------------------------------------------------- scenario "persist"
def scenario_persist(t, server):
    t.section("Token su SQLite: sopravvivono a un riavvio (AIDemo_persist.ini)")
    c = logged(server.base, "mario", "pw1")
    old_access, old_refresh, client_id = c.access, c.refresh_token, c.client_id
    t.check("prima del riavvio: il token funziona", len(c.tools_list()) > 0)

    ok, code = server.restart()
    t.check("il server si ferma con SIGTERM (codice 0) e riparte", ok and code == 0, (ok, code))
    if not ok:
        return

    d = McpClient(server.base)
    d.client_id, d.access, d.refresh_token = client_id, old_access, old_refresh
    status, body = d.rpc("tools/list")
    t.check("dopo il riavvio lo STESSO access token e' ancora valido (ricaricato da SQLite)", status == 200 and "result" in (body or {}), (status, body))
    status, body = d.refresh(old_refresh)
    t.check("dopo il riavvio il refresh token si rinnova (ricaricato da SQLite)", status == 200 and d.access != old_access, (status, body))
    new_access, new_refresh = d.access, d.refresh_token

    ok, code = server.restart()
    t.check("secondo riavvio", ok and code == 0, (ok, code))
    if not ok:
        return
    e = McpClient(server.base)
    e.client_id = client_id
    status, body = e.refresh(old_refresh)
    t.check("il refresh token CONSUMATO prima del riavvio non rinasce (e' stato tolto anche da SQLite)",
            status == 400 and body.get("error") == "invalid_grant", (status, body))
    e.access = new_access
    status, _ = e.rpc("tools/list")
    t.check("l'access token emesso dal rinnovo e' valido dopo il secondo riavvio", status == 200, status)
    status, body = e.refresh(new_refresh)
    t.check("e il suo refresh token si puo' ancora usare", status == 200, (status, body))


# ---------------------------------------------------------------------------------------------- esecuzione
SCENARIOS = [
    ("base", "AIDemo.ini", scenario_base),
    ("all", "AIDemo_all.ini", scenario_all),
    ("persist", "AIDemo_persist.ini", scenario_persist),
]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bin", default=os.path.join(ROOT, "bin", "AIDemo"), help="l'eseguibile compilato")
    ap.add_argument("--port", type=int, default=8081, help="porta su cui avviare il server (default 8081)")
    ap.add_argument("--only", choices=[s[0] for s in SCENARIOS], help="esegue un solo scenario")
    args = ap.parse_args()

    if not os.path.isfile(args.bin):
        sys.exit("Eseguibile non trovato: %s\nCompila prima con ./build.sh (oppure usa --bin)." % args.bin)

    t = Checker()
    for key, ini, scenario in SCENARIOS:
        if args.only and args.only != key:
            continue
        print("\n" + "=" * 100 + "\nSCENARIO %s  (%s)\n" % (key, ini) + "=" * 100)
        if key == "persist":
            shutil.rmtree(os.path.join(os.path.dirname(os.path.abspath(args.bin)), "data"), ignore_errors=True)
        server = Server(args.bin, ini, args.port)
        before = t.failed
        started = False
        try:
            started = server.start()
            t.check("il server parte e accetta connessioni", started)
            if started:
                scenario(t, server)
        except Exception as e:   # un errore di rete o di protocollo e' un fallimento, non un'eccezione muta
            t.check("lo scenario e' arrivato in fondo", False, "%s: %s" % (type(e).__name__, e))
        finally:
            code = server.stop()
            if started and key != "persist":
                t.check("il server si ferma con SIGTERM e codice di uscita 0", code == 0, code)
            if t.failed > before:
                print("\n" + server.diagnostics())
            server.cleanup()

    print("\n%d controlli: %d superati, %d falliti" % (t.passed + t.failed, t.passed, t.failed))
    sys.exit(0 if t.failed == 0 else 1)


if __name__ == "__main__":
    main()
