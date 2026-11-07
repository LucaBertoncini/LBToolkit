#!/usr/bin/env python3
"""Client di prova per un server con OAuth + MCP (solo libreria standard di Python).

Fa quello che fa un agente AI, passo per passo:
  1. prova senza token e legge dove autenticarsi        (probe)
  2. legge i metadati OAuth                              (metadata)
  3. registra il client                                  (register)
  4. fa il login come utente, con PKCE                   (login)   <- l'agente non vede la password nel browser:
                                                                      qui la invia noi al posto del browser
  5. usa il token con JSON-RPC su /mcp                   (rpc, tools_list, tool_call)
  6. rinnova il token                                    (refresh)

Da riga di comando:
  python3 mcp_client.py http://127.0.0.1:8081 login mario pw1
  python3 mcp_client.py http://127.0.0.1:8081 tools
  python3 mcp_client.py http://127.0.0.1:8081 call demo_Somma '{"a": 2, "b": 3}'
  python3 mcp_client.py http://127.0.0.1:8081 refresh
I token restano in .mcp_tokens.json nella cartella corrente.
"""
import base64
import hashlib
import json
import os
import re
import secrets
import sys
import urllib.error
import urllib.parse
import urllib.request

REDIRECT_URI = "http://127.0.0.1:53682/callback"   # indirizzo di ritorno "loopback": il client non lo ascolta davvero


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    """Non segue i reindirizzamenti: l'indirizzo di ritorno OAuth contiene il codice e va letto, non aperto."""

    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


_opener = urllib.request.build_opener(_NoRedirect)


def http(method, url, headers=None, body=None, timeout=20):
    """Una richiesta HTTP. Restituisce (stato, intestazioni, corpo come bytes); non solleva per gli stati 4xx/5xx."""
    h = dict(headers or {})
    data = None
    if isinstance(body, (dict, list)):
        data = json.dumps(body).encode("utf-8")
        h.setdefault("Content-Type", "application/json")
    elif isinstance(body, str):
        data = body.encode("utf-8")
    elif body is not None:
        data = body
    request = urllib.request.Request(url, data=data, headers=h, method=method)
    try:
        with _opener.open(request, timeout=timeout) as response:
            return response.status, dict(response.headers.items()), response.read()
    except urllib.error.HTTPError as error:
        return error.code, dict(error.headers.items()), error.read()


def header(headers, name, default=""):
    """Legge un'intestazione senza distinguere maiuscole e minuscole."""
    for key, value in headers.items():
        if key.lower() == name.lower():
            return value
    return default


def as_json(raw):
    try:
        return json.loads(raw.decode("utf-8"))
    except (ValueError, UnicodeDecodeError):
        return None


class LoginFailed(Exception):
    def __init__(self, status, page):
        super().__init__("login rifiutato (HTTP %d)" % status)
        self.status = status
        self.page = page


class McpClient:
    def __init__(self, base_url, mcp_path="/mcp"):
        self.base = base_url.rstrip("/")
        self.mcp_path = mcp_path
        self.client_id = None
        self.access = None
        self.refresh_token = None
        self.expires_in = None
        self._next_id = 0
        self.last_login_page = ""

    # ---- 1 e 2: scoperta ----------------------------------------------------------------------------------
    def probe(self, token=None, extra_headers=None):
        """POST sul canale senza (o con un altro) token. Un server configurato risponde 401 con WWW-Authenticate."""
        headers = dict(extra_headers or {})
        if token:
            headers["Authorization"] = "Bearer " + token
        return http("POST", self.base + self.mcp_path, headers,
                    {"jsonrpc": "2.0", "id": 0, "method": "ping"})

    def metadata(self):
        """Legge i metadati della risorsa protetta e del server di autorizzazione."""
        status, headers, _ = self.probe()
        match = re.search(r'resource_metadata="([^"]+)"', header(headers, "WWW-Authenticate"))
        if status != 401 or not match:
            raise RuntimeError("il server non ha risposto 401 con resource_metadata (stato %d)" % status)
        _, _, raw = http("GET", match.group(1))
        resource = as_json(raw)
        server = resource["authorization_servers"][0].rstrip("/")
        _, _, raw = http("GET", server + "/.well-known/oauth-authorization-server")
        return resource, as_json(raw)

    # ---- 3: registrazione ---------------------------------------------------------------------------------
    def register(self, redirect_uri=REDIRECT_URI, name="Client di prova"):
        status, _, raw = http("POST", self.base + "/oauth/register",
                              body={"client_name": name, "redirect_uris": [redirect_uri]})
        body = as_json(raw) or {}
        if status == 201:
            self.client_id = body["client_id"]
        return status, body

    # ---- 4: login con PKCE --------------------------------------------------------------------------------
    def login(self, username, password, redirect_uri=REDIRECT_URI):
        if not self.client_id:
            status, body = self.register(redirect_uri)
            if status != 201:
                raise RuntimeError("registrazione rifiutata: %s %s" % (status, body))

        # PKCE: un segreto casuale che resta qui; al server va solo la sua impronta SHA-256
        verifier = base64.urlsafe_b64encode(secrets.token_bytes(48)).rstrip(b"=").decode("ascii")
        challenge = base64.urlsafe_b64encode(hashlib.sha256(verifier.encode("ascii")).digest()).rstrip(b"=").decode("ascii")
        state = secrets.token_urlsafe(8)

        query = urllib.parse.urlencode({
            "response_type": "code", "client_id": self.client_id, "redirect_uri": redirect_uri,
            "code_challenge": challenge, "code_challenge_method": "S256", "state": state})
        status, _, raw = http("GET", self.base + "/oauth/authorize?" + query)
        page = raw.decode("utf-8", "replace")
        match = re.search(r'name="req"\s+value="([^"]*)"', page)
        if status != 200 or not match:
            raise RuntimeError("pagina di accesso non ottenuta (HTTP %d)" % status)
        self.last_login_page = page

        # qui il BROWSER dell'utente invierebbe username e password: l'agente non le vedrebbe mai
        form = urllib.parse.urlencode({"req": match.group(1), "username": username, "password": password})
        status, headers, raw = http("POST", self.base + "/oauth/authorize", {"Content-Type": "application/x-www-form-urlencoded"}, form)
        if status != 302:
            raise LoginFailed(status, raw.decode("utf-8", "replace"))
        location = urllib.parse.urlparse(header(headers, "Location"))
        values = urllib.parse.parse_qs(location.query)
        if "code" not in values or values.get("state", [""])[0] != state:
            raise RuntimeError("risposta senza codice o con state diverso: " + header(headers, "Location"))

        status, _, raw = http("POST", self.base + "/oauth/token", {"Content-Type": "application/x-www-form-urlencoded"},
                              urllib.parse.urlencode({
                                  "grant_type": "authorization_code", "code": values["code"][0],
                                  "client_id": self.client_id, "redirect_uri": redirect_uri, "code_verifier": verifier}))
        return self._store_tokens(status, as_json(raw))

    def _store_tokens(self, status, body):
        if status != 200 or not body or "access_token" not in body:
            raise RuntimeError("token non emessi (HTTP %d): %s" % (status, body))
        self.access = body["access_token"]
        self.refresh_token = body["refresh_token"]
        self.expires_in = body.get("expires_in")
        return body

    # ---- 6: rinnovo ---------------------------------------------------------------------------------------
    def refresh(self, refresh_token=None):
        """Restituisce (stato, corpo). Con stato 200 i nuovi token sostituiscono quelli vecchi."""
        status, _, raw = http("POST", self.base + "/oauth/token", {"Content-Type": "application/x-www-form-urlencoded"},
                              urllib.parse.urlencode({
                                  "grant_type": "refresh_token", "refresh_token": refresh_token or self.refresh_token,
                                  "client_id": self.client_id}))
        body = as_json(raw) or {}
        if status == 200:
            self._store_tokens(status, body)
        return status, body

    # ---- 5: JSON-RPC su /mcp ------------------------------------------------------------------------------
    def rpc(self, method, params=None, token=None, extra_headers=None, notification=False):
        """Restituisce (stato HTTP, JSON della risposta o None)."""
        message = {"jsonrpc": "2.0", "method": method}
        if not notification:
            self._next_id += 1
            message["id"] = self._next_id
        if params is not None:
            message["params"] = params
        headers = dict(extra_headers or {})
        headers["Authorization"] = "Bearer " + (token or self.access or "")
        status, _, raw = http("POST", self.base + self.mcp_path, headers, message)
        return status, as_json(raw)

    def initialize(self):
        status, body = self.rpc("initialize", {"protocolVersion": "2025-06-18", "capabilities": {},
                                              "clientInfo": {"name": "mcp_client.py", "version": "1"}})
        self.rpc("notifications/initialized", notification=True)   # senza id: il server risponde 202 e nessun corpo
        return status, body

    def tools_list(self):
        status, body = self.rpc("tools/list")
        if status != 200 or not body or "result" not in body:
            raise RuntimeError("tools/list fallito (HTTP %s): %s" % (status, body))
        return body["result"]["tools"]

    def tool_call(self, name, arguments=None):
        """Restituisce (is_error, testo, json_del_testo_o_None). Un errore di protocollo solleva RuntimeError."""
        status, body = self.rpc("tools/call", {"name": name, "arguments": arguments if arguments is not None else {}})
        if status != 200 or not body:
            raise RuntimeError("tools/call fallito (HTTP %s): %s" % (status, body))
        if "error" in body:
            raise RuntimeError("errore JSON-RPC %s: %s" % (body["error"].get("code"), body["error"].get("message")))
        result = body["result"]
        text = result["content"][0]["text"]
        try:
            parsed = json.loads(text)
        except ValueError:
            parsed = None
        return bool(result.get("isError")), text, parsed


# ---- riga di comando -------------------------------------------------------------------------------------
TOKEN_FILE = ".mcp_tokens.json"


def _save(client):
    with open(TOKEN_FILE, "w", encoding="utf-8") as f:
        json.dump({"client_id": client.client_id, "access": client.access, "refresh": client.refresh_token}, f)


def _load(client):
    try:
        with open(TOKEN_FILE, encoding="utf-8") as f:
            data = json.load(f)
    except OSError:
        sys.exit("Nessun token: esegui prima 'login'")
    client.client_id, client.access, client.refresh_token = data["client_id"], data["access"], data["refresh"]


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    client = McpClient(argv[1])
    command = argv[2]
    if command == "login" and len(argv) == 5:
        body = client.login(argv[3], argv[4])
        _save(client)
        print("Login riuscito. Il token d'accesso vale %s secondi." % body.get("expires_in"))
    elif command == "tools":
        _load(client)
        for tool in client.tools_list():
            print("%-28s %s" % (tool["name"], tool.get("description", "")))
    elif command == "call" and len(argv) >= 4:
        _load(client)
        arguments = json.loads(argv[4]) if len(argv) > 4 else {}
        is_error, text, _ = client.tool_call(argv[3], arguments)
        print(("ERRORE: " if is_error else "") + text)
    elif command == "refresh":
        _load(client)
        status, body = client.refresh()
        _save(client)
        print("HTTP %d %s" % (status, "token rinnovati" if status == 200 else body))
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv)
