# OAuth spiegato in modo semplice

Questo documento spiega la logica di OAuth 2.1 come è realizzata in `uLBOAuthProvider.pas`
(connettore AI di LBmicroWebServer), con esempi di ciò che il client invia e di ciò che il server risponde.
Gli esempi sono quelli reali del test automatico; i valori lunghi sono abbreviati.

---

## 1. Il problema

Claude.ai (o un'altra AI) vuole usare la tua applicazione **a nome di un utente**, ad esempio Mario.

Soluzioni sbagliate:

- Dare a Claude la password di Mario. Claude la conserverebbe, e Mario non potrebbe più revocare l'accesso
  senza cambiare password.
- Dare a Claude una password "di sistema". Non si saprebbe più chi fa cosa, e i permessi sarebbero sbagliati.

OAuth risolve il problema così: **Mario inserisce la password solo sul tuo server**. Claude non la vede mai.
Alla fine Claude riceve un **gettone (token)** che vale per Mario, scade presto ed è revocabile.

> Analogia: la chiave dell'hotel. Alla reception (il tuo server) mostri il documento (la password) e ricevi
> una tessera. Con la tessera apri solo la tua camera, e solo per la durata del soggiorno. Il documento
> resta in tasca.

### In sintesi: a cosa serve OAuth

OAuth serve a far agire **un'applicazione a tuo nome, senza che conosca le tue credenziali**.
Separa due cose:

- **chi sei**: lo verifica solo il tuo server (username e password nella pagina di login);
- **chi agisce per te**: un'altra applicazione (Claude), con un permesso **limitato**, **temporaneo** e **revocabile**.

Rispetto a una password condivisa dà quattro vantaggi:

| Vantaggio | Come si realizza qui |
|---|---|
| Limitato | il token vale solo per ciò che il ruolo dell'utente permette, e per lo scopo `mcp` |
| Temporaneo | access token 15 minuti, refresh token 30 giorni |
| Revocabile | si toglie l'accesso a Claude senza cambiare la password |
| Tracciabile | il server sa quale applicazione (`client_id`) agisce a nome dell'utente |

**Attenzione:** il client non conosce la password, ma **il token è comunque una chiave**. Chi lo possiede
agisce come l'utente finché è valido. Per questo i token durano poco, il refresh token si usa una volta
sola e il server va esposto in HTTPS.

## 2. Gli attori

| Attore | Chi è | Nel nostro caso |
|---|---|---|
| Utente | chi possiede i dati | Mario, con il suo username e password |
| Client | l'applicazione che vuole accedere | Claude.ai |
| Server di autorizzazione | verifica Mario e rilascia i token | il tuo LBmicroWebServer (`/oauth/...`) |
| Risorsa protetta | ciò a cui si accede con il token | l'endpoint `/mcp` |

Nel nostro caso server di autorizzazione e risorsa protetta sono lo stesso programma.

## 3. I due token

- **Access token**: la tessera che si mostra a ogni chiamata (`Authorization: Bearer ...`). Dura poco
  (15 minuti), così se viene rubato il danno è limitato.
- **Refresh token**: serve solo a ottenere un nuovo access token senza far reinserire la password.
  Dura 30 giorni e si usa **una sola volta**: a ogni rinnovo ne viene dato uno nuovo.

Mario inserisce la password una volta; dopo, Claude si rinnova da solo per settimane.

## 4. Le tre idee di sicurezza

1. **La password passa solo dal tuo server.** Il browser di Mario apre la tua pagina di login.
2. **Il codice temporaneo.** Dopo il login il server non consegna subito i token. Consegna un *codice*
   valido 5 minuti e utilizzabile una volta sola. Il client lo scambia con i token in una chiamata diretta
   al server. Così i token non passano mai per la barra degli indirizzi del browser.
3. **PKCE.** Il client inventa un segreto casuale (`code_verifier`) e all'inizio manda solo la sua impronta
   (`code_challenge`, un hash SHA-256). Per scambiare il codice deve mostrare il segreto originale. Se
   qualcuno intercetta il codice, senza il segreto non può usarlo. Con questo server PKCE è obbligatorio.

## 5. Il flusso passo per passo

```
Claude                    Browser di Mario              Tuo server
  |  POST /mcp (senza token) ------------------------------->|
  |<-------------------------------- 401 + dove autenticarsi |   (A)
  |  GET /.well-known/... ----------------------------------->|
  |<-------------------------------- indirizzi del server    |   (B)
  |  POST /oauth/register ----------------------------------->|
  |<-------------------------------- client_id               |   (C)
  |  apre il browser ---->|  GET /oauth/authorize?... ------->|
  |                       |<---------------- pagina di login  |   (D)
  |                       |  POST username+password -------->|
  |                       |<---- 302 verso Claude con code    |   (E)
  |  riceve il code <-----|                                   |
  |  POST /oauth/token (code + code_verifier) --------------->|
  |<------------------------------ access + refresh token    |   (F)
  |  POST /mcp con Authorization: Bearer ... ---------------->|
  |<-------------------------------- risposta                 |   (G)
  |  ... dopo 15 minuti: POST /oauth/token (refresh) -------->|
  |<------------------------------ nuovi token               |   (H)
```

### (A) Claude prova senza token

Richiesta:

```http
POST /mcp HTTP/1.1
Content-Type: application/json

{"jsonrpc":"2.0","id":1,"method":"initialize", ...}
```

Risposta: il server dice "serve un token" e indica dove cercare le istruzioni.

```http
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer resource_metadata="https://app.example.com/.well-known/oauth-protected-resource"
```

### (B) Claude scopre gli indirizzi

```http
GET /.well-known/oauth-protected-resource HTTP/1.1
```

```json
{
  "resource": "https://app.example.com/mcp",
  "authorization_servers": ["https://app.example.com"],
  "bearer_methods_supported": ["header"],
  "scopes_supported": ["mcp"]
}
```

Poi Claude legge `/.well-known/oauth-authorization-server`:

```json
{
  "issuer": "https://app.example.com",
  "authorization_endpoint": "https://app.example.com/oauth/authorize",
  "token_endpoint": "https://app.example.com/oauth/token",
  "registration_endpoint": "https://app.example.com/oauth/register",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "code_challenge_methods_supported": ["S256"],
  "token_endpoint_auth_methods_supported": ["none"]
}
```

È una "rubrica" che permette a Claude di non conoscere in anticipo nessun indirizzo, tranne `/mcp`.

### (C) Claude si registra

Il server non conosce Claude: deve presentarsi e dire dove vuole essere richiamato.

```http
POST /oauth/register HTTP/1.1
Content-Type: application/json

{"client_name":"Claude","redirect_uris":["https://claude.ai/api/mcp/auth_callback"]}
```

```http
HTTP/1.1 201 Created

{"client_id":"mcp-3VWcb9UdTjm6...","client_name":"Claude",
 "redirect_uris":["https://claude.ai/api/mcp/auth_callback"],
 "grant_types":["authorization_code","refresh_token"],
 "token_endpoint_auth_method":"none"}
```

Il server accetta solo indirizzi di ritorno autorizzati (`AllowedRedirectURIs`). Se Claude indica
`https://evil.example/cb` riceve `400 invalid_redirect_uri`. È la protezione che impedisce di
dirottare il codice verso un sito estraneo.

### (D) Mario vede la pagina di login

Claude prepara il segreto PKCE e apre nel browser di Mario:

```
GET /oauth/authorize
    ?response_type=code
    &client_id=mcp-3VWcb9UdTjm6...
    &redirect_uri=https://claude.ai/api/mcp/auth_callback
    &code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM
    &code_challenge_method=S256
    &state=xyz123
```

- `code_challenge` è l'impronta del segreto che Claude tiene per sé.
- `state` è un valore di Claude che il server restituirà identico: serve a Claude per riconoscere
  che la risposta appartiene alla sua richiesta.

Il server risponde con una pagina HTML con username e password. La pagina contiene anche un campo
nascosto `req` che identifica questa richiesta di autorizzazione.

### (E) Mario fa il login

Il browser invia:

```http
POST /oauth/authorize HTTP/1.1
Content-Type: application/x-www-form-urlencoded

req=P0XWjkZ0h7Hl-90...&username=mario&password=********
```

- Se la password è sbagliata: `401` e la pagina si ripresenta. Dopo 5 errori il server risponde
  `429 Too Many Requests` con `Retry-After: 600` (blocco per 10 minuti).
- Se è giusta, il server **non** mostra i token. Rimanda il browser a Claude con il codice:

```http
HTTP/1.1 302 Found
Location: https://claude.ai/api/mcp/auth_callback?code=PZje4bViKcSx...&state=xyz123
```

### (F) Claude scambia il codice con i token

Questa chiamata va direttamente da Claude al server, senza passare dal browser.

```http
POST /oauth/token HTTP/1.1
Content-Type: application/x-www-form-urlencoded

grant_type=authorization_code
&code=PZje4bViKcSx...
&redirect_uri=https://claude.ai/api/mcp/auth_callback
&client_id=mcp-3VWcb9UdTjm6...
&code_verifier=il-segreto-originale
```

Il server controlla che:

- il codice esista, non sia scaduto (5 minuti) e non sia già stato usato;
- `client_id` e `redirect_uri` siano gli stessi della richiesta iniziale;
- `SHA-256(code_verifier)` coincida con il `code_challenge` ricevuto all'inizio.

Se tutto torna:

```http
HTTP/1.1 200 OK
Cache-Control: no-store

{"access_token":"…","token_type":"Bearer","expires_in":900,
 "refresh_token":"…","scope":"mcp"}
```

Se qualcosa non torna, ad esempio il segreto sbagliato:

```json
{"error":"invalid_grant","error_description":"The code does not match the request"}
```

In quel caso il codice viene comunque bruciato: un secondo tentativo con quello stesso codice fallisce.

### (G) Claude usa l'access token

```http
POST /mcp HTTP/1.1
Authorization: Bearer <access_token>
Content-Type: application/json

{"jsonrpc":"2.0","id":2,"method":"tools/list"}
```

Il server cerca il token nel TokenManager, trova che appartiene a Mario e applica il **ruolo di
Mario**. Claude vede ed esegue solo ciò che Mario potrebbe fare nell'applicazione.

### (H) Rinnovo senza password

Dopo 15 minuti l'access token scade e `/mcp` risponde `401`. Claude non disturba Mario:

```http
POST /oauth/token HTTP/1.1
Content-Type: application/x-www-form-urlencoded

grant_type=refresh_token&refresh_token=<vecchio>&client_id=mcp-3VWcb9UdTjm6...
```

```json
{"access_token":"…nuovo…","token_type":"Bearer","expires_in":900,
 "refresh_token":"…nuovo…","scope":"mcp"}
```

Il vecchio refresh token è ora **inutilizzabile**. Se qualcuno prova a riusarlo:

```json
{"error":"invalid_grant","error_description":"The refresh token is invalid or expired"}
```

È la rotazione dei refresh token: un token rubato smette di funzionare appena il legittimo
proprietario si rinnova, o viceversa.

## 6. Riepilogo degli errori

| Situazione | Risposta |
|---|---|
| Nessun token su `/mcp` | `401` + `WWW-Authenticate` con l'indirizzo dei metadati |
| Access token scaduto o falso | `401` |
| Refresh token usato come access token | `401` (i token hanno uno scopo preciso) |
| Indirizzo di ritorno non autorizzato | `400 invalid_redirect_uri` |
| Richiesta senza PKCE | redirect con `error=invalid_request` |
| Codice scaduto, riusato, o `code_verifier` errato | `400 invalid_grant` |
| Refresh token già usato o di un altro client | `400 invalid_grant` |
| Troppi tentativi di login falliti | `429` + `Retry-After: 600` |
| Utente senza il permesso richiesto da un tool | il tool non compare in `tools/list`, e chiamarlo dà errore |

## 7. Dove si trova nel codice

- `uLBOAuthProvider.pas`: metadati, registrazione, pagina di login, `/oauth/token`, limiti anti-abuso.
- `uLBOAuthSetup.pas`: legge la sezione `[OAuth]` del file INI (indirizzo pubblico, durate dei token, indirizzi di ritorno, diciture della pagina di accesso), crea il provider e lo aggancia alla catena del web server. Il provider non dipende dal connettore AI.
- `uTokenManager.pas`: conserva i token (con scopo `ai` per l'accesso e `ai_refresh` per il rinnovo),
  li fa scadere e li associa all'utente. Il resto dell'applicazione non cambia.
- `uLBAIConnector.pas`: chiede al TokenManager di validare il Bearer e filtra gli strumenti in base al ruolo.

## 8. Configurazione minima da ricordare

- `PublicBaseURL`: l'indirizzo pubblico **https** del server. Va impostato sempre. Contiene **solo schema, host e porta** (per esempio `https://cmms.example.com`): i percorsi (`/mcp`, `/oauth/...`, `/.well-known/...`) li aggiunge il server. Con un percorso OAuth non parte.
- `AllowedRedirectURIs`: `https://claude.ai/api/mcp/auth_callback` (aggiungi quelli di altre AI se servono).
- Le diciture della pagina di accesso (titolo, intestazione, testo con `{host}` e `{app}`, etichette) si scelgono nella sezione `[OAuth]`; i valori predefiniti sono neutri.
- Il server va esposto in HTTPS (direttamente o dietro un reverse proxy): senza HTTPS i token e
  la password viaggiano in chiaro.

## 9. In una frase

> Mario dimostra chi è **al tuo server**, il tuo server dà a Claude una tessera a scadenza breve, e ogni
> volta che Claude la mostra il server applica i permessi di Mario.
