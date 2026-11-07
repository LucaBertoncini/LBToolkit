# OAuth e AIConnector in LBmicroWebServer

Guida alla logica di OAuth e alla sua integrazione con il connettore AI (MCP) di LBmicroWebServer.

**Esempi.** Usano l'indirizzo `https://cmms.idrotherm2000.com` e l'utente "Mario". Sono valori d'esempio. I valori lunghi (token, codici) sono abbreviati.

---

# Parte 1 - OAuth spiegato semplice

## 1.1 Il problema

Un agente AI deve usare le funzioni di un sito **per conto di un utente**. Esistono due soluzioni ingenue, entrambe sbagliate:

| Soluzione | Perché non va bene |
|---|---|
| L'utente consegna username e password all'agente | L'agente conosce la password. L'utente non può revocare l'accesso senza cambiarla |
| Si crea un utente "AI" con permessi propri | Non si sa più chi ha fatto cosa, e l'AI potrebbe fare cose che l'utente non potrebbe fare |

OAuth risolve il problema così: **la password viene digitata nel browser dell'utente ed inviato solo al server**. L'agente non la vede mai. Alla fine riceve un **token**, che vale per quell'utente, scade presto e si può revocare.

Analogia: la tessera dell'hotel. Alla reception (il tuo server) mostri il documento (la password) e ricevi una tessera. La tessera apre solo la tua camera e solo per la durata del soggiorno. Il documento resta in tasca.

## 1.2 Tre domande, tre cose diverse

L'idea centrale è che OAuth separa tre domande che altrimenti si confondono.

| Domanda | Risposta in OAuth | Chi la verifica |
|---|---|---|
| Chi è l'**applicazione** che vuole accedere? | `client_id` | Il server, alla registrazione |
| Chi è l'**utente** che la autorizza? | Login con username e password | Solo il server sui dati inviati tramite browser |
| Cosa può fare l'applicazione per conto di quell'utente? | `access_token` | Il server, a ogni chiamata |

La password risponde solo alla seconda domanda. L'agente riceve invece la *conseguenza* dell'autenticazione: "IdroWorld ha verificato l'utente e mi autorizza a operare per suo conto".

**Attenzione.** Il client non conosce la password, ma il token è comunque una chiave. Chi lo possiede agisce come l'utente finché è valido. Per questo i token durano poco, il refresh token si usa una volta sola e il server va raggiunto in HTTPS.

## 1.3 I soggetti

```text
   Utente (Mario) ──── usa ────► Browser
        │                           │
        │ autorizza                 │ login e redirect
        ▼                           ▼
   Client AI  ◄──────────────►  LBmicroWebServer
   (Claude, ecc.)               /mcp   /oauth/*   /.well-known/*
```

| Soggetto | Ruolo |
|---|---|
| Utente | Possiede i dati. Ha un account sul sito, con un ruolo |
| Browser | Serve solo per il login. L'agente lo apre e poi aspetta |
| Client AI | L'applicazione che vuole usare le funzioni |
| Server | Fa da *server di autorizzazione* (`/oauth/*`) e da *risorsa protetta* (`/mcp`). Nel nostro caso sono lo stesso programma |

## 1.4 Il flusso, passo per passo

### Passo 1 - Il client prova senza token

```http
POST /mcp
(nessun header Authorization)
```

Il server risponde:

```http
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer resource_metadata="https://cmms.idrotherm2000.com/.well-known/oauth-protected-resource/mcp"
```

Il `401` non chiede la password. Dice: "serve un token, e qui trovi come ottenerlo". Se il client avesse mandato un token non valido, la risposta aggiungerebbe `error="invalid_token"`.

### Passo 2 - Il client scopre gli indirizzi (discovery)

Il client legge due documenti JSON. Sono **generati dal codice**, non sono file su disco.

`GET /.well-known/oauth-protected-resource/mcp`

```json
{
  "resource": "https://cmms.idrotherm2000.com/mcp",
  "authorization_servers": ["https://cmms.idrotherm2000.com"],
  "bearer_methods_supported": ["header"],
  "scopes_supported": ["mcp"]
}
```

`GET /.well-known/oauth-authorization-server`

```json
{
  "issuer": "https://cmms.idrotherm2000.com",
  "authorization_endpoint": "https://cmms.idrotherm2000.com/oauth/authorize",
  "token_endpoint": "https://cmms.idrotherm2000.com/oauth/token",
  "registration_endpoint": "https://cmms.idrotherm2000.com/oauth/register",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "code_challenge_methods_supported": ["S256"],
  "token_endpoint_auth_methods_supported": ["none"],
  "scopes_supported": ["mcp"]
}
```

È una "rubrica": il client conosce in anticipo solo `/mcp` e da lì ricava tutto il resto. Ne escono tre indirizzi:

| Indirizzo | Risponde alla domanda |
|---|---|
| `/oauth/register` | Chi sono io, applicazione? |
| `/oauth/authorize` | Quale utente mi autorizza? |
| `/oauth/token` | Dammi il token |

### Passo 3 - Registrazione del client

```http
POST /oauth/register
Content-Type: application/json

{"client_name": "Claude", "redirect_uris": ["https://claude.ai/api/mcp/auth_callback"]}
```

```http
HTTP/1.1 201 Created

{"client_id": "mcp-3VWcb9UdTjm6...", "client_name": "Claude",
 "redirect_uris": ["https://claude.ai/api/mcp/auth_callback"],
 "grant_types": ["authorization_code", "refresh_token"],
 "response_types": ["code"], "token_endpoint_auth_method": "none"}
```

Cosa significa:

- **Il `client_id` non è un token.** È solo il "numero di registrazione" dell'applicazione. Non dà accesso a niente.
- **La registrazione non ha memoria.** Il `client_id` è casuale e il server **non lo salva**. Per questo un riavvio non rompe le connessioni già create.
- **Cosa protegge davvero è la lista dei redirect consentiti** (`AllowedRedirectURIs`). Se il client chiede un indirizzo non in lista, la registrazione è rifiutata con `400 invalid_redirect_uri`.

Perché la lista dei redirect conta: un attaccante potrebbe registrare un client con un proprio indirizzo di ritorno per farsi mandare il codice di autorizzazione. La lista lo impedisce. Per default ammette il callback di Claude e, se `AllowLoopbackRedirects` è attivo, gli indirizzi `http://localhost`, `127.0.0.1` e `[::1]` (utili per Claude Code).

### Passo 4 - L'utente si autentica nel browser

Il client prepara il suo segreto PKCE (vedi 1.5) e apre il browser su:

```text
GET /oauth/authorize
    ?response_type=code
    &client_id=mcp-3VWcb9UdTjm6...
    &redirect_uri=https://claude.ai/api/mcp/auth_callback
    &code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM
    &code_challenge_method=S256
    &state=xyz123
```

A questo punto **l'agente si ferma**. Il controllo passa al browser.

Il server controlla che il `redirect_uri` sia consentito (altrimenti mostra una pagina d'errore e **non** reindirizza), che `response_type` sia `code`, e che ci siano `client_id` e `code_challenge` con metodo `S256`. Poi mostra la **pagina di login**:

> *Accedi con il tuo account per consentire a un assistente AI (claude.ai) di usare l'applicazione con i tuoi permessi.*

La pagina contiene un campo nascosto `req` che identifica questa richiesta (valida 10 minuti). Il parametro `state` è un valore del client che il server restituirà identico: serve al client per riconoscere la risposta alla *sua* richiesta.

### Passo 5 - Mario fa il login

Il browser invia:

```http
POST /oauth/authorize
Content-Type: application/x-www-form-urlencoded

req=P0XWjkZ0h7Hl-90...&username=mario&password=********
```

Il server verifica le credenziali tramite l'evento `OnAuthenticateCredentials`, cioè con il controllo dell'applicazione (normalmente lo stesso del login web). Il provider **non conserva** né la password né altri dati sulle credenziali.

| Esito | Risposta |
|---|---|
| Password sbagliata | `401` e la pagina di login si ripresenta. La richiesta non è consumata: si può riprovare |
| 5 errori (per indirizzo o per nome utente) | `429 Too Many Requests` con `Retry-After: 600` |
| Password giusta | `302` verso il client, con il codice di autorizzazione |

```http
HTTP/1.1 302 Found
Location: https://claude.ai/api/mcp/auth_callback?code=PZje4bViKcSx...&state=xyz123
```

Il `code` **non è il token di accesso**. È un codice temporaneo con tre proprietà:

- è **monouso**: viene tolto dalla tabella al primo utilizzo, qualunque ne sia l'esito;
- vale **5 minuti**;
- è **legato** a `client_id`, `redirect_uri` e `code_challenge`.

La pagina di login non può essere inserita in un frame (`X-Frame-Options: DENY`) e non viene mai messa in cache.

### Passo 6 - Il client scambia il codice con i token

Questa chiamata va **direttamente dal client al server**, senza passare dal browser. Per questo i token non compaiono mai nella barra degli indirizzi.

```http
POST /oauth/token
Content-Type: application/x-www-form-urlencoded

grant_type=authorization_code
&code=PZje4bViKcSx...
&client_id=mcp-3VWcb9UdTjm6...
&redirect_uri=https://claude.ai/api/mcp/auth_callback
&code_verifier=dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk
```

Il server controlla che il codice esista, non sia scaduto e non sia già stato usato, che `client_id` e `redirect_uri` siano gli stessi della richiesta iniziale e che il PKCE torni. Se tutto è corretto:

```json
{"access_token": "...", "token_type": "Bearer", "expires_in": 900,
 "refresh_token": "...", "scope": "mcp"}
```

Se qualcosa non torna, per esempio il segreto sbagliato:

```json
{"error": "invalid_grant", "error_description": "The code does not match the request"}
```

In quel caso il codice è comunque **bruciato**: un secondo tentativo con lo stesso codice fallisce.

### Passo 7 - Il client usa l'access token

```http
POST /mcp
Authorization: Bearer <access_token>
Content-Type: application/json

{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}
```

Il server cerca il token, scopre che appartiene a Mario e applica il **ruolo di Mario**. L'agente vede ed esegue solo ciò che Mario potrebbe fare nell'applicazione. Non ha permessi propri.

### Passo 8 - Rinnovo senza password

Dopo 15 minuti l'access token scade e `/mcp` risponde `401`. Il client non disturba Mario:

```http
POST /oauth/token

grant_type=refresh_token&refresh_token=<vecchio>&client_id=mcp-3VWcb9UdTjm6...
```

Il server **consuma** il vecchio refresh token, controlla che l'utente sia ancora valido, rilegge il suo **ruolo attuale** ed emette una **nuova coppia** di token. Il vecchio refresh token non funziona più (*rotazione*). Se qualcuno prova a riusarlo:

```json
{"error": "invalid_grant", "error_description": "The refresh token is invalid or expired"}
```

Una conseguenza pratica: se un utente viene disattivato o cambia ruolo, l'effetto si vede al rinnovo successivo, quindi **entro 15 minuti**.

### Riepilogo del flusso

```text
Client AI                 Browser di Mario            Server
   │ POST /mcp (senza token) ───────────────────────────►│
   │◄──────────────────────────── 401 + dove autenticarsi │  (1)
   │ GET /.well-known/... ──────────────────────────────►│
   │◄──────────────────────────────── indirizzi OAuth     │  (2)
   │ POST /oauth/register ──────────────────────────────►│
   │◄──────────────────────────────────── client_id       │  (3)
   │ apre il browser ──►│ GET /oauth/authorize ─────────►│
   │                    │◄──────────── pagina di login    │  (4)
   │                    │ POST username + password ─────►│
   │                    │◄──── 302 verso il client + code │  (5)
   │◄── code ───────────│                                 │
   │ POST /oauth/token (code + code_verifier) ──────────►│
   │◄─────────────────── access_token + refresh_token     │  (6)
   │ POST /mcp + Bearer ────────────────────────────────►│
   │◄─────────────────────────────────── risposta         │  (7)
   │ ... dopo 15 min: POST /oauth/token (refresh) ──────►│
   │◄─────────────────────────── nuovi token              │  (8)
```

## 1.5 PKCE: perché un codice rubato non serve

Il problema: se un malware intercetta `code=PZje...` nell'indirizzo di ritorno, potrebbe presentarsi a `/oauth/token` prima del client.

La soluzione si chiama **PKCE** (*Proof Key for Code Exchange*):

1. Prima di aprire il browser, il client genera un segreto casuale, il `code_verifier`, che **non trasmette**.
2. Calcola `code_challenge = BASE64URL(SHA-256(code_verifier))` e manda solo quello al passo 4.
3. Al passo 6 presenta il `code_verifier` in chiaro.
4. Il server ricalcola l'hash e lo confronta con il `code_challenge` memorizzato insieme al codice.

Chi ha rubato solo il codice non ha il verificatore, quindi non può usarlo. Il verificatore non è mai passato per il browser.

Nel codice PKCE è **obbligatorio**, solo con metodo `S256`. Il verificatore deve avere da 43 a 128 caratteri dell'insieme ammesso dall'RFC 7636, e il confronto avviene a tempo costante.

## 1.6 I due token

| Token | A cosa serve | Durata predefinita | Uso |
|---|---|---|---|
| **Access token** | Si mostra a ogni chiamata a `/mcp` | **15 minuti** (`AccessTokenLifetime = 900`) | Ripetuto |
| **Refresh token** | Serve solo a ottenere un nuovo access token | **30 giorni** (`RefreshTokenLifetime`) | **Una volta sola** |

Altre durate: il codice di autorizzazione dura 5 minuti, la richiesta di autorizzazione in sospeso 10 minuti.

L'access token è breve perché, se viene rubato, il danno è limitato. Il refresh token permette di non chiedere la password a ogni scadenza: Mario la digita una volta, poi il client si rinnova da solo per settimane.

## 1.7 Chi protegge da cosa

| Tentativo di attacco | Difesa |
|---|---|
| Un client si registra con un proprio indirizzo di ritorno | Lista dei redirect consentiti |
| Qualcuno intercetta il codice di autorizzazione | PKCE, durata di 5 minuti, uso singolo |
| Qualcuno riusa un codice | Il codice è tolto dalla tabella al primo uso, anche in caso di errore |
| Qualcuno prova molte password | 5 errori, poi `429` per 600 secondi (contati per indirizzo e per nome utente) |
| Un refresh token rubato viene usato | Rotazione: dopo il primo uso non esiste più |
| Un token del sito web viene usato come token AI, o viceversa | Gli scope sono separati (`web`, `ai`, `ai_refresh`) |
| La pagina di login viene inserita in un frame altrui | `X-Frame-Options: DENY` e `frame-ancestors 'none'` |
| Un utente viene disattivato o cambia ruolo | Il rinnovo rilegge l'utente e rifiuta se non è più valido |

## 1.8 Risposte di errore da conoscere

| Situazione | Risposta |
|---|---|
| Nessun token su `/mcp` | `401` con `WWW-Authenticate` |
| Access token scaduto, falso o di altro scope | `401` |
| Redirect non autorizzato in registrazione | `400 invalid_redirect_uri` |
| Redirect non autorizzato in `/oauth/authorize` | Pagina d'errore `400`, nessun reindirizzamento |
| Richiesta senza PKCE o con `response_type` diverso da `code` | Reindirizzamento con `error=invalid_request` o `unsupported_response_type` |
| Codice scaduto, riusato o con verificatore errato | `400 invalid_grant` |
| Refresh token già usato, scaduto o di un altro client | `400 invalid_grant` |
| Utente non più valido al rinnovo | `400 invalid_grant` |
| Troppi tentativi di login | `429` con `Retry-After: 600` |
| `PublicBaseURL` non impostato | `500 server_error` su tutti gli endpoint OAuth |
| Ruolo senza il permesso richiesto da uno strumento | Lo strumento non compare in `tools/list`; chiamarlo dà un errore |

## 1.9 OAuth non sostituisce HTTPS

I due meccanismi risolvono problemi diversi.

| | TLS (HTTPS) | OAuth |
|---|---|---|
| Domanda | Sto parlando con il server giusto? La conversazione è cifrata? | Chi è l'utente e cosa ha autorizzato? |
| Protegge da | Intercettazione e dirottamento | Cessione della password a terzi |
| Prova | Certificato firmato da una CA | Token legato a un utente |

La password viaggia nel modulo di login, quindi il server deve essere raggiunto solo in HTTPS. **Il codice non lo impone**: con un `PublicBaseURL` non HTTPS scrive solo un avviso nel log.

---

# Parte 2 - Integrazione in LBmicroWebServer

## 2.1 Una catena di processori

`TLBmicroWebServer` gestisce le richieste con una **catena di processori** (`TRequestChainProcessor`). Ogni processore decide se la richiesta è sua:

- se la gestisce, risponde e la catena si ferma (`DoProcessRequest` restituisce `True`);
- se non è sua, restituisce `False` e la richiesta passa al processore successivo.

```text
Richiesta HTTP
     │
     ▼
┌──────────────────────────────────────────────┐
│ TLBOAuthProvider                             │
│   /.well-known/oauth-protected-resource...   │
│   /.well-known/oauth-authorization-server    │
│   /oauth/register  /oauth/authorize          │
│   /oauth/token                               │
├──────────────────────────────────────────────┤
│ TLBAIConnector                               │
│   /mcp  (il percorso arriva da Routes.xml)   │
├──────────────────────────────────────────────┤
│ TWebRouteModule                              │
│   /api/...  (Routes.xml → worker)            │
└──────────────────────────────────────────────┘
```

I percorsi `/.well-known/...` e `/oauth/...` **non sono file** e **non sono voci di `Routes.xml`**. Sono percorsi virtuali che il provider riconosce e a cui risponde con contenuto generato dal codice.

**Un dettaglio sulle richieste GET.** Per una GET senza parametri (per esempio `/.well-known/oauth-protected-resource/mcp`) il server prova prima a cercarla come file nella cartella dei documenti. Se non esiste, la passa alla catena. Per una GET con parametri (per esempio `/oauth/authorize?...`) salta la ricerca del file. Le POST vanno sempre alla catena. Il risultato è lo stesso, ma la ricerca del file costa un accesso al disco per le GET senza parametri.

**L'ordine nella catena** non influisce sulla correttezza, perché i tre componenti rispondono a percorsi diversi e restituiscono `False` per tutto il resto. Anche `TWebRouteModule` restituisce `False` per una rotta che non conosce, e `/mcp` non è nella sua tabella.

## 2.2 Tre componenti, tre responsabilità

| Componente | Fa | Non fa |
|---|---|---|
| `TLBOAuthProvider` | Discovery, registrazione, pagina di login, codici, emissione e rinnovo dei token | Non esegue funzioni applicative |
| `TLBAIConnector` | Protocollo MCP (`initialize`, `ping`, `tools/list`, `tools/call`), catalogo degli strumenti, controllo dei permessi | Non emette token e non conosce le password |
| `TWebRouteModule` | Funzioni applicative da `Routes.xml` | Non conosce OAuth né MCP |

I token sono conservati da `TTokenManager`, con **tre scope**:

| Scope | Significato |
|---|---|
| `web` (o nessuno) | Sessione del sito, nel cookie |
| `ai` | Access token dell'AI |
| `ai_refresh` | Refresh token dell'AI |

Un token vale solo per il proprio scope: l'access token AI non è accettato come cookie e viceversa. I token AI hanno scadenza **assoluta** (non si allunga con l'uso); quelli web hanno scadenza **per inattività**.

## 2.3 Come il connettore trova gli strumenti

Il connettore **non ha una propria mappa delle funzioni**. Riusa quelle già dichiarate in `Routes.xml`.

1. **Il punto d'ingresso.** Un endpoint con `Kind="AIConnector"` è la porta MCP. Il suo URI è il percorso (per esempio `/mcp`); la sua `Area` e `OpType` dicono quali ruoli possono usare il canale AI. Non entra nella tabella di dispatch e non ha un handler.
2. **I candidati.** Ogni endpoint con `Kind="Standard"` e **autenticazione richiesta** (`RequiresAuth` diverso da `false`), registrato con `RegisterHandler`, diventa candidato.
3. **La descrizione dei parametri.** La classe che implementa `IAIConnectorFunctions` descrive i parametri di ogni funzione. Con `ExposeUndescribed = False` (il valore predefinito) diventano strumenti **solo le funzioni descritte**.
4. **Il nome.** Si ricava da `Area` e `Function`: `assets` + `Search` diventa `assets_Search`. I caratteri non ammessi (solo lettere, cifre, `_` e `-`) diventano `_`, con un massimo di 64 caratteri. Se due funzioni producono lo stesso nome, la seconda **non è esposta** e il log lo segnala.
5. **La descrizione dello strumento.** Viene dall'attributo `Description` dell'endpoint in `Routes.xml`. Se manca, si usa quella del descrittore; se manca anche quella, un testo generico e un avviso nel log. Una buona descrizione è ciò che permette all'AI di scegliere lo strumento giusto.

Esempio di `Routes.xml` (schema, da adattare ai nomi reali):

```xml
<!-- porta MCP: non è una funzione applicativa -->
<FunctionalArea Code="ai" Description="Connettore AI">
  <Endpoint Function="Mcp" Method="POST" URI="/mcp" Kind="AIConnector" OpType="Uso"/>
</FunctionalArea>

<!-- funzione applicativa, condivisa da sito e AI -->
<FunctionalArea Code="assets" Description="Asset">
  <Endpoint Function="Search" Method="POST" URI="/api/assets/search" Kind="Standard"
            OpType="Lettura" Description="Cerca asset per codice, nome o matricola."/>
</FunctionalArea>
```

Il permesso della porta (`ai.Uso` nell'esempio) compare nel catalogo dei permessi come ogni altro `OpType` dichiarato, quindi si assegna ai ruoli con gli stessi strumenti già usati per gli altri.

Esempio di descrizione, nella classe dell'area:

```pascal
function TAssetHandlers.DescribeAIFunction(const aFunctionName: String;
  aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'Search') then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('text', aptString, 'Testo da cercare', True);
    aDescriptor.AddParam('max_rows', aptInteger, 'Numero massimo di risultati (default 50)', False);
  end
  else
    Result := False;   // nessuna descrizione: la funzione non viene esposta
end;
```

**Importante: su quale oggetto va implementata l'interfaccia.** Il connettore chiede `IAIConnectorFunctions` all'oggetto che era stato passato a `RegisterHandler`: la classe dell'**area** (la discendente di `TRouteHandlerBase`). Nel registro quell'oggetto è il campo `Handler` di ogni voce. Non cerca l'interfaccia sull'oggetto a cui appartiene il metodo.

Se in un'area i worker sono metodi di altri oggetti (per esempio `RegisterStandardWorker('GetOne', @FCore.DoGetOne)`), `FCore` non verrà mai interrogato. In quel caso la classe dell'area deve implementare `DescribeAIFunction` e delegare all'oggetto interno.

## 2.4 Chi decide cosa può fare l'AI

Tre livelli, tutti obbligatori:

| Livello | Cosa decide |
|---|---|
| `Routes.xml` e le classi | Quali funzioni sono candidate (Standard, autenticate) e quali sono descritte |
| Il ruolo dell'utente | Quali strumenti vede in `tools/list` e quali può chiamare, in base ad `Area.OpType` |
| La durata dell'access token | Quanto tempo passa prima che una modifica di ruolo o la disattivazione abbia effetto |

Il permesso è sempre `Area.OpType`, lo stesso già usato dal sito. Non esiste un secondo catalogo di permessi per l'AI. Il controllo passa per `UserHasPermission`, che chiama `hasPermission`, il metodo virtuale che l'applicazione può ridefinire. Un `OpType` vuoto significa "nessun permesso specifico": basta essere autenticati.

L'ordine dei controlli a ogni richiesta a `/mcp`:

```text
1. Origin consentita?                         (altrimenti 403)
2. OnAuthenticateRequest assegnato?           (altrimenti 500)
3. Bearer token valido, scope "ai"?           (altrimenti 401 + WWW-Authenticate)
4. Il ruolo ha il permesso della porta MCP?   (altrimenti 403)
5. Metodo POST?                               (altrimenti 405)
        │
        ▼  tools/call, in ordine:
6. Lo strumento esiste?                       (altrimenti errore -32602)
7. Il ruolo ha Area.OpType dello strumento?   (altrimenti risultato d'errore, 403)
8. Limite di chiamate al minuto superato?     (altrimenti risultato d'errore, 429)
9. "arguments" è un oggetto JSON?             (altrimenti risultato d'errore, 400)
10. Argomenti validi per lo schema?           (altrimenti risultato d'errore, 400)
11. Si esegue il worker
12. Risultato più grande di MaxResultBytes?   (altrimenti risultato d'errore, 413)
```

Alla fine di ogni `tools/call`, in qualunque esito, il connettore scrive una riga nel log (`user`, `tool`, `status`, `ms`) e scatta l'evento `OnToolCalled`, utile per audit e statistiche.

Altri aspetti verificati:

- **Origin.** Senza header `Origin` (client server-to-server come Claude.ai) la richiesta è accettata. Con `Origin` (un browser) deve essere in `AllowedOrigins` oppure coincidere con l'origine pubblica del server.
- **Errore del worker.** Un'eccezione nel worker diventa un risultato d'errore `500` generico: il messaggio dell'eccezione va nel log, non all'AI.
- **Indicazioni per il client.** `Effect` (`aeReadOnly`, `aeModifies`, `aeDestructive`) produce le *annotations* MCP. Sono suggerimenti per il client, **non** controlli di sicurezza.

## 2.5 Cosa deve fornire l'applicazione

Il provider non sa nulla di utenti e password. L'applicazione gli passa due funzioni:

| Evento | Quando scatta | Cosa deve fare |
|---|---|---|
| `OnAuthenticateCredentials(user, password, out id, out config)` | Login nella pagina OAuth | Verificare le credenziali (come il login web) e restituire id e `UserConfig` |
| `OnRefreshUser(id, out config)` | Ogni rinnovo | Verificare che l'utente sia ancora valido e restituire un `UserConfig` costruito con i dati **attuali** (ruolo incluso). Se non è assegnato, si riusa la configurazione salvata con il token |

Regole sul `UserConfig`:

- deve essere un **oggetto nuovo**: il provider ne prende la proprietà;
- deve contenere **ciò che `hasPermission` legge**. Nell'implementazione di base è l'array `Permissions` (lo riempie `BuildUserPermissions` del router). Nell'applicazione IdroWorld, secondo la documentazione del provider, sono `is_admin` e `role_id`;
- per il connettore, il `UserConfig` **non viene copiato**: è l'oggetto del token, tenuto vivo da un contesto di autenticazione fino alla fine della richiesta.

Il connettore, da parte sua, riceve `OnAuthenticateRequest`, che nel caso normale è `TokenManager.isValidBearerRequest`: legge `Authorization: Bearer ...`, accetta **solo** token con scope `ai` e restituisce il contesto di autenticazione.

---

## 2.6 La configurazione: la creazione delle classi dipende dal file del server?

**No. Nessuna delle tre unit legge un file di configurazione.** `TLBOAuthProvider` e `TLBAIConnector` espongono *proprietà*; è l'applicazione a creare gli oggetti e a valorizzarle, da qualunque sorgente.

```text
config.ini  ──►  codice dell'applicazione  ──►  proprietà delle classi
Routes.xml  ──►  registro delle rotte      ──►  percorso MCP, ruolo ammesso, elenco funzioni
```

### Cosa legge oggi il bootstrap

`TLBApplicationBoostrap` (`uLBApplicationBoostrap.pas`) legge dal file INI soltanto:

| Sezione | Chiavi |
|---|---|
| `[LBLogger]` | `LogFile`, `LogLevel`, `MaxFileSize` |
| `[LBmicroWebServer]` | `Port`, `DocumentsFolder`, `UploadEndpoint`, `DefaultFile`, `SecureCookie` |
| Sezione SSL (nome definito in `TSSLConnectionData`) | Certificato, CA, chiave privata, password della chiave |

Non c'è nulla su OAuth o AIConnector. E `LBmicroWebServer` non abilita OAuth o MCP perché trova certe chiavi: **abilitarli è una decisione dell'applicazione**.

### Dove si agganciano le classi derivate

Il bootstrap offre due metodi virtuali, chiamati in quest'ordine (percorso INI):

```text
LoadConfiguration(file.ini)
  └─ LoadConfigurationFromINIFile
       ├─ legge [LBLogger]                  → configura il logger
       ├─ legge [LBmicroWebServer] e SSL    → riempie la configurazione del server
       ├─ LoadConfigurationFromINIFileInternal(IniFile)      ◄── (1) virtuale
       │        il TIniFile è disponibile, FWebServer NON esiste ancora
       │        se restituisce False, il server non parte
       └─ se Port > 0: setUpWebServer
             ├─ crea FWebServer, cartella documenti, SSL
             ├─ startingWebServer()                           ◄── (2) virtuale
             │        FWebServer esiste, non è ancora attivo
             └─ imposta la porta e chiama Activate()
```

Il modo naturale di usarli:

| Dove | Cosa fare |
|---|---|
| **(1)** `LoadConfigurationFromINIFileInternal` | Leggere le chiavi di AIConnector e OAuth dal `TIniFile` e **salvarle in campi** della classe derivata |
| **(2)** `startingWebServer` | Creare router, provider e connettore, valorizzarne le proprietà dai campi e aggiungerli alla catena di `FWebServer` |

Il motivo della divisione è che quando si legge il file il server non esiste ancora, e quando il server esiste il `TIniFile` non viene più passato.

Se la sezione `[LBmicroWebServer]` manca o `Port` è 0, il server **non viene creato** e quindi non esiste nessuna catena.

### Il precedente: il TokenManager

`TTokenManager` ha già una sezione propria, `[TokenManager]`, con due chiavi:

| Chiave | Significato |
|---|---|
| `TokenExpirationTime` | Inattività massima della sessione **web**, in secondi (predefinito 3600) |
| `DBFile` | File SQLite dove salvare i token |

Il TokenManager ha un overload `LoadConfigurationFromIniFile(aIniFile: TIniFile)` pensato per ricevere lo stesso `TIniFile`: il punto (1) è il posto naturale per chiamarlo. `TokenExpirationTime` **non** riguarda i token AI, che hanno la durata impostata dal provider.

Altri due fatti sul TokenManager:

- **Persistenza.** I token (AI inclusi) si salvano su disco **solo se `DBFile` è impostato**. Senza, restano in memoria e un riavvio li perde: i client AI dovrebbero rifare il login.
- **`Activate`.** Avvia un thread che ogni 5 minuti salva le modifiche e rimuove i token scaduti.

### Parametri configurabili

`TLBOAuthProvider`:

| Proprietà | Significato | Predefinito |
|---|---|---|
| `PublicBaseURL` | Indirizzo pubblico del server, senza `/` finale, senza query né frammento | vuoto: gli endpoint OAuth rispondono `server_error` |
| `AllowHostFallback` | Se `PublicBaseURL` è vuoto, usa l'header `Host`. Solo sviluppo | `False` |
| `ResourcePath` | Percorso MCP. Lo imposta il connettore | `/mcp` |
| `OAuthPath` | Prefisso degli endpoint OAuth | `/oauth` |
| `AccessTokenLifetime` | Secondi | `900` |
| `RefreshTokenLifetime` | Secondi | 30 giorni |
| `AllowedRedirectURIs` | Lista dei redirect ammessi | callback di Claude |
| `AllowLoopbackRedirects` | Ammette `localhost`, `127.0.0.1`, `[::1]` | `True` |
| `TrustForwardedFor` | Usa `X-Forwarded-For` per l'indirizzo del client. Solo dietro un proxy fidato | `False` |
| `MaxLoginFailures` / `LoginLockSeconds` | Limiti sul login | 5 / 600 |
| `Texts` | Testi della pagina di login (in italiano) | - |
| `OnAuthenticateCredentials` / `OnRefreshUser` | Eventi (vedi 2.5) | - |

`TLBAIConnector`:

| Proprietà | Significato | Predefinito |
|---|---|---|
| `OAuthProvider` | Provider da usare nella risposta `401` | nessuno |
| `OnAuthenticateRequest` | Verifica del bearer token | - |
| `ExposeUndescribed` | Espone anche le funzioni non descritte, con parametri liberi | `False` |
| `RequireOpTypeForTools` | Non espone le funzioni senza `OpType` | `False` |
| `AllowedOrigins` | Origin ammessi per richieste da browser | vuota |
| `MaxCallsPerMinute` | Limite per utente | `0` (nessuno) |
| `MaxResultBytes` | Dimensione massima del risultato | 200.000 |
| `MaxBatchSize` | Messaggi per batch JSON-RPC | 20 |
| `ServerName`, `ServerVersion`, `Instructions` | Dati mostrati al client | `LBmicroWebServer`, `1.0`, vuoto |
| `OnToolCalled` | Evento per audit | - |

### Esempio di sezione INI (proposta)

Le chiavi sotto sono un **esempio di mappatura**. Nelle unit esaminate non esiste codice che le legga: andrebbero lette nel punto (1).

```ini
[AIConnector]
Enabled=1
PublicBaseURL=https://cmms.idrotherm2000.com
AccessTokenLifetime=900
RefreshTokenLifetime=2592000
AllowedRedirectURIs=https://claude.ai/api/mcp/auth_callback
AllowLoopbackRedirects=0
MaxCallsPerMinute=60
MaxResultBytes=200000
```

---

## 2.7 Sequenza di avvio

Schema da adattare. Presuppone che le chiavi siano già state lette nel punto (1) e salvate in campi. I nomi `TMyBootstrap`, `FAuthService`, `RegisterRouteHandlers`, `RoutesFileName` e i campi `FAI...` sono **d'esempio**: non li ho verificati nel codice dell'applicazione.

```pascal
procedure TMyBootstrap.startingWebServer();      // punto (2): FWebServer esiste, non è attivo
begin
  inherited;
  if not FAIEnabled then Exit;

  // 1. router e rotte, con tutti i gestori già registrati
  //    (nell'applicazione, la sottoclasse di TWebRouteModule che ridefinisce hasPermission)
  FRouter := TWebRouteModule.Create;
  FRouter.LoadRoutesFromXML(RoutesFileName);
  RegisterRouteHandlers(FRouter);
  FRouter.ValidateRoutes;

  // 2. provider OAuth
  FOAuthProvider := TLBOAuthProvider.Create(FTokenManager);
  FOAuthProvider.PublicBaseURL := FAIPublicBaseURL;
  FOAuthProvider.AccessTokenLifetime := FAIAccessLifetime;
  FOAuthProvider.OnAuthenticateCredentials := @FAuthService.AuthenticateCredentials;
  FOAuthProvider.OnRefreshUser := @FAuthService.RefreshUser;

  // 3. connettore MCP
  FAIConnector := TLBAIConnector.Create(FRouter);
  FAIConnector.OAuthProvider := FOAuthProvider;
  FAIConnector.OnAuthenticateRequest := @FTokenManager.isValidBearerRequest;
  FAIConnector.MaxCallsPerMinute := FAIMaxCalls;
  FAIConnector.Prepare;                          // catalogo costruito UNA volta

  // 4. catena (il web server ne diventa proprietario: vedi 2.8)
  FWebServer.addChainProcessor(FOAuthProvider, True);
  FWebServer.addChainProcessor(FAIConnector, True);
  FWebServer.addChainProcessor(FRouter, False);
end;
// subito dopo, il bootstrap imposta la porta e chiama FWebServer.Activate()
```

Punti da rispettare:

- **Registrare tutti i gestori prima di `Prepare`.** Il catalogo si costruisce una volta sola e non si ricostruisce.
- **Chiamare `Prepare` prima che il server accetti connessioni.** Se non lo si fa parte alla prima richiesta, ma in modo atomico.
- **`ValidateRoutes` per ultimo**, dopo tutti i `RegisterHandler` e dopo `RegisterProxyEndpoints`.
- **Il secondo parametro di `addChainProcessor` è `asFirst`, cioè la *posizione*** (`True` = in testa). **Non** indica chi possiede l'oggetto.

## 2.8 Proprietà degli oggetti e ordine di distruzione

| Oggetto | Chi lo possiede | Chi lo libera |
|---|---|---|
| `FWebServer` | Il bootstrap | Il bootstrap (e anche `setUpWebServer`, se lo ricrea) |
| Router, provider, connettore, **tutti i processori in catena** | `TLBmicroWebServer`: la lista dei processori è creata con `OwnsObjects = True`, **sempre** | Il web server, nel suo distruttore |
| `TTokenManager` | L'applicazione | L'applicazione |

Conseguenze:

1. **L'applicazione non deve liberare** router, provider e connettore: lo fa il web server. I riferimenti che conserva diventano non validi appena il server viene liberato.
2. Provider e connettore tengono solo un *riferimento* a `TTokenManager` e al router.
3. **Il `TTokenManager` deve restare vivo finché il web server non ha finito le richieste in corso.** Ogni richiesta AI usa un contesto che, a fine richiesta, richiama il TokenManager (`ReleaseTokenLease`). Il distruttore del bootstrap chiama prima l'evento `OnDestroyed` e **poi** libera il web server. Se l'applicazione libera il TokenManager dentro `OnDestroyed`, una richiesta ancora in corso potrebbe usare un oggetto già liberato. Da verificare nel codice dell'applicazione.

## 2.9 Cosa controlla l'avvio

`Prepare` e `ValidateConfiguration` scrivono gli errori nel log. **Non bloccano l'avvio.**

| Situazione | Comportamento |
|---|---|
| `PublicBaseURL` vuoto | Gli endpoint OAuth rispondono `500 server_error` (salvo `AllowHostFallback`) |
| `PublicBaseURL` non HTTPS | Solo un avviso |
| `PublicBaseURL` con query o frammento | Errore nel log |
| Redirect con `*` finale | Avviso: meglio indirizzi esatti |
| **Nessun** endpoint `Kind="AIConnector"` | Avviso, ma il percorso predefinito `/mcp` **resta attivo, senza controllo di ruolo** |
| Più endpoint `Kind="AIConnector"` | Errore nel log, si usa il primo |
| URI del gate che non inizia con `/`, o uguale a un'altra rotta | Errore nel log |
| Funzione senza `Description` in `Routes.xml` | Avviso: l'AI vedrà solo un testo generico |
| Funzione senza `OpType` | Avviso: la usa qualsiasi utente autenticato (a meno di `RequireOpTypeForTools`) |
| Nome strumento duplicato | La seconda funzione non è esposta, errore nel log |

---

# Parte 3 - Punti di attenzione

Sono osservazioni dalla lettura del codice. Non ho compilato né provato nulla.

1. **Dove si cerca `IAIConnectorFunctions`.** Sull'oggetto passato a `RegisterHandler` (la classe dell'area), non sull'oggetto a cui appartiene il metodo. Se i worker stanno in oggetti interni, la classe dell'area deve implementare e delegare (vedi 2.3).

2. **Persistenza dei token dopo un riavvio.** In `TTokenManager.LoadActiveTokens` gli oggetti ricaricati dal database ricevono `IdUser`, `LastUpdate` e `UsrConf`, ma **non il campo `token`**. `UpdateTokens` legge `Elements['token']` per ogni token da aggiornare. Dopo un riavvio, il primo token ricaricato e poi usato dovrebbe far fallire il salvataggio dell'intero gruppo di aggiornamenti. Sospetto fondato sul codice, non verificato: serve un test di riavvio con `DBFile` impostato.

3. **Nessun endpoint `Kind="AIConnector"`.** Il connettore resta raggiungibile su `/mcp` per qualunque utente autenticato. Se si vuole che l'assenza dell'endpoint disattivi il canale, il bootstrap deve creare il connettore solo quando l'endpoint esiste.

4. **HTTPS non imposto.** Un `PublicBaseURL` in HTTP produce un avviso, non un rifiuto.

5. **Revoca non immediata.** Un utente disattivato o con ruolo cambiato è visto solo al rinnovo, quindi fino a 15 minuti dopo. Per una revoca più rapida si può accorciare `AccessTokenLifetime`.

6. **Commento obsoleto.** L'intestazione di `uLBOAuthProvider.pas` parla di un access token di "1 hour", mentre il valore predefinito è 900 secondi. Vale il codice.

7. **Worker che restituisce `nil` con uno stato di errore.** Il testo restituito all'AI è `{"successful":true}`, ma il risultato è segnato come errore. Il contenuto contraddice il flag.

8. **CORS.** Per le richieste `OPTIONS` il server ammette solo `Content-Type` tra gli header (`Access-Control-Allow-Headers`). Un client MCP che gira in un browser e invia `Authorization` o `MCP-Protocol-Version` fallirebbe sul *preflight*. Non riguarda i client server-to-server come Claude.ai.

9. **Strumenti senza `OpType`.** Un endpoint descritto per l'AI ma senza `OpType` è usabile da ogni utente autenticato. `RequireOpTypeForTools` lo evita.

---

# Riassunto in poche righe

- OAuth permette a un'AI di operare **come un utente**, senza conoscerne la password.
- Separa tre cose: chi è l'applicazione (`client_id`), chi è l'utente (login sul tuo server), cosa può fare l'applicazione (`access_token`).
- Il flusso è: `401` → discovery → registrazione → login nel browser → **codice monouso** → scambio con **access e refresh token**. PKCE protegge lo scambio. I token durano 15 minuti e 30 giorni; il refresh è monouso.
- I percorsi `/.well-known/*` e `/oauth/*` sono gestiti da `TLBOAuthProvider`, `/mcp` da `TLBAIConnector`, `/api/*` da `TWebRouteModule`. Nessuno di questi è un file.
- Gli strumenti MCP sono gli **endpoint `Standard` autenticati già presenti in `Routes.xml`**, descritti dalla classe dell'area e filtrati dai permessi `Area.OpType` dell'utente.
- Le classi **non leggono nessun file INI**: le crea e le configura l'applicazione. Nel bootstrap i punti d'aggancio sono `LoadConfigurationFromINIFileInternal` (per leggere i valori) e `startingWebServer` (per creare gli oggetti e aggiungerli alla catena).
- Il web server **possiede** i processori della catena; il `TokenManager` resta dell'applicazione e deve sopravvivere al server.
- OAuth non sostituisce HTTPS: servono entrambi.
