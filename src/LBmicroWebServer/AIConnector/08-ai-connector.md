# 08 · AI Connector: usare il web server come estensione di Claude e di altre AI

`TLBAIConnector` fa del web server un'estensione per Claude.ai e altre AI. Parla il protocollo **MCP** (Model Context Protocol, JSON-RPC 2.0 su HTTP). Funziona con Claude.ai, Claude Code e con qualunque client MCP.

L'idea: **gli endpoint si dichiarano una sola volta**, in `Routes.xml`, come per l'applicazione normale (URI e `OpType`). Il connettore li legge dal registry, chiede alle classi dei gestori di descrivere i parametri delle funzioni che vogliono offrire all'AI, e per ogni utente mostra e permette solo ciò che il suo ruolo consente. L'AI usa le stesse funzioni, con gli stessi permessi del sito.

## 1. Come è fatto

| Unit | Contenuto |
|---|---|
| `uLBAIConnectorTypes.pas` | L'interfaccia `IAIConnectorFunctions` e le classi che descrivono i parametri. |
| `uLBAIConnector.pas` | `TLBAIConnector`: il server MCP (`initialize`, `ping`, `tools/list`, `tools/call`). |
| `uLBAIConnectorSetup.pas` | `TLBAIConnectorSetup`: legge `[AIConnector]` dal file INI, controlla le precondizioni (elencandole tutte nel log), crea il connettore e lo aggancia alla catena del web server. **Richiede OAuth attivo.** |
| `uLBOAuthProvider.pas` | `TLBOAuthProvider`: server di autorizzazione OAuth con username e password dell'utente (necessario per Claude.ai). Indipendente dal connettore: serve anche per altro (vedi `Esempi/Ex04_SoloOAuth`). |
| `uLBOAuthSetup.pas` | `TLBOAuthSetup`: legge `[OAuth]` dal file INI (indirizzo pubblico, durate dei token, indirizzi di ritorno, diciture della pagina di accesso), crea il provider e lo aggancia alla catena. |
| `uWebRouteRegistry.pas` | `Kind="AIConnector"`, attributo `Description`, `AIConnectorEndpoint`, `UserHasPermission`. |
| `uTokenManager.pas` | Token con ambito e scadenza propria (`addNewScopedToken`, `isValidBearerRequest`, `isValidBearerRequestForScope`). In `utils`. |
| `uLBAuthContext.pas` | `TLBAuthenticationContext`: chi è l'utente di una richiesta e la sua configurazione, valida per tutta la richiesta. In `utils`, accanto a chi la definisce: `utils` non dipende dalla cartella del web server (`TAIAuthenticationContext` è lo stesso tipo con il vecchio nome). |
| `uHTTPConsts.pas`, `uLBmicroWebServer.pas` | Costanti e descrizioni di stato mancanti (202, 302, 409, 422, 429, 502), `GetRemoteIP`. |

Nessuna di queste unit legge un file di configurazione per decidere se esistere: lo decide l'applicazione, creando (o no) i due `Setup`. Il bootstrap (`TLBApplicationBoostrap`) non cambia.
## 2. Cosa vede l'AI: quattro filtri

1. **L'utente.** Chiunque acceda con le proprie credenziali (login OAuth) fa lavorare l'AI con i propri diritti: nessun ruolo speciale è necessario. L'endpoint con `Kind="AIConnector"` è solo l'indirizzo MCP (ad esempio `/mcp`). Se vuoi poter escludere certi ruoli dall'AI, dai a quell'endpoint un `OpType` (facoltativo): solo i ruoli con quel permesso potranno usare il connettore, gli altri ricevono 403. Senza `OpType` basta essere autenticati.
2. **Gli endpoint già dichiarati.** Ogni endpoint `Standard` (JSON in, JSON out, con autenticazione) è un candidato. Download, upload, login e proxy non lo sono.
3. **La descrizione dei parametri.** Una classe può descrivere con precisione i parametri di una funzione (`IAIConnectorFunctions`). Con `ExposeUndescribed := True` (`ExposeUndescribed=1` nel file INI) anche le funzioni non descritte sono strumenti, con parametri liberi (un oggetto JSON) e solo la `Description` di `Routes.xml` come guida: è il ruolo a decidere cosa l'AI può eseguire. Con `ExposeUndescribed := False` (predefinito) restano solo le funzioni descritte. Una funzione si può escludere sempre con `aDescriptor.Hidden := True`.
4. **Il ruolo dell'utente.** `tools/list` mostra solo gli strumenti il cui `Area.OpType` è concesso al ruolo, e `tools/call` ricontrolla con lo stesso `hasPermission` del sito (in IdroWorld: amministratore oppure cache dei permessi del ruolo).

Il nome dello strumento visto dall'AI è `<area>_<Function>` (ad esempio `assets_GetOne`).

## 3. Dichiarare e descrivere le funzioni

In `Routes.xml` gli endpoint restano quelli di sempre. Si aggiunge la porta d'ingresso e, dove serve, una `Description` (testo libero: è ciò che l'AI legge per scegliere lo strumento, quindi va scritta bene).

```xml
<FunctionalArea Code="ai" Description="Connettore AI">
  <Endpoint Function="Mcp" URI="/mcp" Kind="AIConnector"
            Description="Ingresso del connettore AI (MCP)."/>
</FunctionalArea>

<FunctionalArea Code="assets" Description="Asset">
  <Endpoint Function="GetOne" URI="/api/assets/getOne" OpType="Lettura"
            Description="Restituisce il dettaglio di un asset dato il suo uuid."/>
</FunctionalArea>
```

Descrivere i parametri è facoltativo se si espongono tutte le funzioni, ma consigliato: l'AI sbaglia meno chiamate e riceve errori chiari prima che il worker venga eseguito. Nel gestore (nessuna registrazione speciale: il worker è lo stesso del sito) si implementa `IAIConnectorFunctions`.

```pascal
TMyHandlers = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function DoSearch(aUserId: Integer; aUserConfig: TJSONObject;
      aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
  public
    constructor Create(...);
    function DescribeAIFunction(const aFunctionName: String;
      aDescriptor: TAIFunctionDescriptor): Boolean;
end;

constructor TMyHandlers.Create(...);
begin
  inherited Create('assets');
  Self.RegisterStandardWorker('Search', @Self.DoSearch);   // come sempre
end;

function TMyHandlers.DescribeAIFunction(const aFunctionName: String;
  aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if aFunctionName = 'Search' then
  begin
    aDescriptor.Effect := aeReadOnly;                 // suggerimento per il client AI
    aDescriptor.AddParam('text', aptString, 'Testo da cercare', True);
    aDescriptor.AddEnumParam('status', 'Stato', 'active,retired');
    aDescriptor.AddParam('limit', aptInteger, 'Numero massimo di righe (default 50)');
  end
  else
    Result := False;                                  // nessuna descrizione precisa (vedi ExposeUndescribed)
end;
```

Tipi disponibili: `aptString, aptInteger, aptNumber, aptBoolean, aptDate, aptDateTime, aptObject, aptArray` (`AddArrayParam`). `Effect`: `aeReadOnly`, `aeModifies`, `aeDestructive`: il client può chiedere conferma all'utente prima di eseguire una funzione che modifica.

Gli argomenti inviati dall'AI sono validati **prima** di chiamare il tuo worker: parametri obbligatori, tipi, valori ammessi, parametri sconosciuti rifiutati. I numeri e i booleani inviati come testo vengono convertiti. Se la validazione fallisce, l'AI riceve un messaggio chiaro e può correggere la chiamata.

Il worker riceve `aUserId`, `aUserConfig` e `aRequestData` come per una richiesta web. Se risponde con stato ≥ 400, l'AI vede `isError`.

Se in `Routes.xml` non c'è nessun endpoint `Kind="AIConnector"`: un `TLBAIConnector` creato a mano risponde su `/mcp` a qualunque utente autenticato e lo segnala nel log (per questo **non** va usato così); `TLBAIConnectorSetup` invece **non attiva il connettore** e scrive il motivo nel log. Il percorso MCP è l'`URI` di quell'endpoint: puoi sceglierlo liberamente (non sotto `/oauth` né `/.well-known`, e non uguale a un'altra rotta).

## 4. Collegarlo all'applicazione

### 4.1 Con i due `Setup` (consigliato)

L'applicazione fa quattro cose, in quest'ordine:

```pascal
// costruttore
FOAuth := TLBOAuthSetup.Create;
FAI    := TLBAIConnectorSetup.Create;

// lettura del file INI (LoadConfigurationFromINIFileInternal)
FOAuth.LoadFromINIFile(aFile);       // [OAuth]
FAI.LoadFromINIFile(aFile);          // [AIConnector]

// ULTIMA cosa di startingWebServer: DOPO ogni RegisterHandler, DOPO ValidateRoutes, DOPO addChainProcessor(router)
FOAuth.Attach(FWebServer, FTokenManager, @FAuth.AuthenticateCredentials, @FAuth.RefreshUser);
FAI.Attach(FWebServer, _WebRouteModule, FOAuth);      // sempre DOPO FOAuth.Attach

// facoltativo, dopo Attach: cio' che il file INI non copre
FAI.Connector.OnToolCalled := @MioRegistro.ToolCalled;
```

`AuthenticateCredentials` verifica username e password come il login web e restituisce id e configurazione di sessione; `RefreshUser` rilegge l'utente a ogni rinnovo (disattivato? ruolo cambiato?). Sono le due cose che solo l'applicazione conosce.

File INI (senza `Enabled=1` nessuna delle due sezioni fa nulla, e non scrive nulla nel log):

```ini
[OAuth]
Enabled=1
PublicBaseURL=https://cmms.example.com     ; SOLO schema, host e porta: niente /mcp, niente "/" finale
; AccessTokenLifetimeSec=900
; RefreshTokenLifetimeSec=2592000
; AllowedRedirectURIs=https://claude.ai/api/mcp/auth_callback
; AllowLoopbackRedirects=0                 ; 1 per Claude Code
; TrustForwardedFor=0                      ; 1 dietro un reverse proxy fidato
; MaxLoginFailures=5
; LoginLockSeconds=600
; ServerTitle=IdroWorld CMMS               ; e le altre diciture della pagina di accesso (LoginIntro con {host} e {app}, ...)

[AIConnector]
Enabled=1
; ExposeUndescribed=0
; RequireOpTypeForTools=0
; AllowedOrigins=
; MaxCallsPerMinute=60
; MaxResultKB=200
; ServerName=IdroWorld CMMS
; Instructions=
```

Elenco completo delle chiavi e dei valori predefiniti: intestazione di `uLBOAuthSetup.pas` e di `uLBAIConnectorSetup.pas`.

Se manca qualcosa, `Attach` **non attiva nulla** e scrive nel log **tutti** i problemi insieme: web server inesistente (`[LBmicroWebServer] Port` assente o 0), token manager o controllo credenziali non consegnati, OAuth non attivo o agganciato dopo il connettore, nessun endpoint `Kind="AIConnector"` in `Routes.xml`, nessun gestore ancora registrato (`Attach` troppo presto: il catalogo resterebbe vuoto per sempre), `PublicBaseURL` mancante, non HTTPS (senza `AllowInsecureHTTP=1`) o con un percorso.

L'ordine delle `Attach` e il fatto che vengano dopo `RegisterHandler` e `ValidateRoutes` sono l'unico vincolo che resta all'applicazione.

### 4.2 A mano

Serve solo per opzioni che i `Setup` non espongono. Il catalogo si costruisce una sola volta, in `Prepare`:

```pascal
FAIConnector := TLBAIConnector.Create(_WebRouteModule);
FAIConnector.OnAuthenticateRequest := @FTokenManager.isValidBearerRequest;
FAIConnector.Instructions := 'Applicazione di manutenzione IdroWorld: ...';   // opzionale
FAIConnector.MaxCallsPerMinute := 60;                                         // opzionale

FOAuth := TLBOAuthProvider.Create(FTokenManager);
FOAuth.PublicBaseURL := 'https://cmms.example.com';     // solo schema, host e porta
FOAuth.OnAuthenticateCredentials := @Self.AIAuthenticate;
FOAuth.OnRefreshUser := @Self.AIRefreshUser;
FOAuth.ValidateConfiguration();                         // scrive nel log gli errori di configurazione (il connettore non lo fa piu')
FAIConnector.OAuthProvider := FOAuth;                   // solo un riferimento: lo possiede il web server (catena)

FAIConnector.Prepare();     // DOPO RegisterHandler e ValidateRoutes: legge l'indirizzo da Routes.xml e segnala nel log i problemi

FWebServer.addChainProcessor(_WebRouteModule, True);
FWebServer.addChainProcessor(FOAuth, True);
FWebServer.addChainProcessor(FAIConnector, True);
```

L'indirizzo da dare all'AI è `PublicBaseURL + /mcp` (il percorso è l'`URI` dell'endpoint `Kind="AIConnector"` in `Routes.xml`). Gli endpoint OAuth stanno sotto `/oauth` e i metadati sotto `/.well-known/`.

**Proprietà degli oggetti.** Una volta aggiunti alla catena con `addChainProcessor`, router, provider e connettore sono del web server, che li libera: l'applicazione non deve liberarli. Il secondo parametro di `addChainProcessor` è la *posizione* (`True` = in testa), non la proprietà. Il `TTokenManager` resta invece dell'applicazione e deve sopravvivere al web server (ogni richiesta AI richiama il token manager a fine richiesta).

### 4.3 In IdroWorld

La cartella `Idroworld/` contiene:

| File | Cosa |
|---|---|
| `uIdroWorld_AuthService.pas` | **Nuovo.** Verifica username e password (la stessa query di `HandleLogin`) e costruisce `{is_admin, role_id}`. Lo usano il login web e il login OAuth: la regola non è scritta due volte. |
| `uCMMS_HandlersAssets.pas` | La classe implementa `IAIConnectorFunctions` e descrive `ListKinds`, `GetRoots`, `GetTree`, `GetOne`. |
| `INTEGRAZIONE.md` | Le righe da aggiungere a `uIdroWorld_WS_Application.pas` e a `uIdroWorld_HandlersAuth.pas`, e le `Description` da mettere in `Routes.xml`. |

La vecchia `uIdroWorld_AIConnector.pas` (una classe che creava provider e connettore da sola e leggeva `PublicBaseURL` da `[AIConnector]`) **non c'è più**: usata insieme ai `Setup` collegherebbe due provider.

Il login web non viene toccato: il token dell'AI porta lo stesso oggetto di sessione (`is_admin`, `role_id`), quindi `TIdroWorldWebRouteModule.hasPermission` e la visibilità degli asset per utente (`AssetTreeVisibleSql`) valgono anche per l'AI. Con `ExposeUndescribed=1` ogni funzione Standard con autenticazione è uno strumento e decide il ruolo: chi non ha `Eliminazione` sugli asset non potrà farli cancellare nemmeno dall'AI. Per migliorare una funzione basta descriverne i parametri nel gestore (come in `uCMMS_HandlersAssets.pas`) e scrivere una buona `Description` in `Routes.xml`. Le funzioni delicate (aree `users` e `roles`, o `Eliminazione`) restano protette dal ruolo; se vuoi escluderle del tutto dall'AI, descrivile con `Hidden := True`.

## 5. Autenticazione

- **Claude.ai (e client OAuth).** Ogni persona fa login con il proprio username e password; l'AI lavora con i diritti di quell'utente. Il server pubblica i metadati OAuth, accetta la registrazione automatica del client, mostra la pagina di login, richiede PKCE S256, emette un token di accesso (15 minuti) e uno di rinnovo (30 giorni, a rotazione). Il ruolo viene letto quando il token è emesso e a ogni rinnovo (`OnRefreshUser`): se l'utente è stato disattivato o ha cambiato ruolo, l'effetto arriva al rinnovo successivo, cioè entro 15 minuti (`AccessTokenLifetime`). Non è istantaneo: per averlo serve revocare i token dell'utente (non ancora presente).
- **Token creato a mano** (Claude Code, API, prove): `FTokenManager.addNewScopedToken(IdUtente, UserConfig, cTokenScope_AIAccess, 0, 'manuale', _Token)`. `0` = senza scadenza. Si revoca con `RemoveToken`.
- Un token AI non vale come cookie del sito e viceversa; un token di rinnovo non vale come token di accesso. Un'applicazione che usa OAuth per altro (senza connettore) sceglie i propri ambiti con `Provider.AccessScope`, `RefreshScope` e `ScopeName` e valida i token con `TTokenManager.isValidBearerRequestForScope` (esempio `Ex04_SoloOAuth`): i suoi token non valgono per il connettore AI e viceversa. `TLBAIConnectorSetup` rifiuta di partire se il provider emette un ambito diverso da quello AI.

Claude Code:

```
claude mcp add --transport http cmms https://cmms.example.com/mcp --header "Authorization: Bearer <token>"
```

### Requisiti per Claude.ai

- **HTTPS pubblico.** La connessione parte dal cloud di Anthropic (indirizzi `160.79.104.0/21`). Un server solo interno non è raggiungibile; si usa un reverse proxy con certificato (nginx, Caddy) oppure, per le organizzazioni Enterprise e su richiesta, un MCP tunnel.
- `PublicBaseURL` deve coincidere con l'indirizzo che l'utente inserisce in Claude.
- Redirect ammesso per default: `https://claude.ai/api/mcp/auth_callback`. Per Claude Code vengono accettati `localhost`/`127.0.0.1` (`AllowLoopbackRedirects`). Altri servizi AI vanno aggiunti a `AllowedRedirectURIs`.
- Dietro un reverse proxy fidato imposta `TrustForwardedFor := True` perché il blocco dei tentativi di login usi l'indirizzo reale.

## 6. Sicurezza: cosa è già fatto e cosa resta a te

Già fatto: PKCE obbligatorio; codici monouso di 5 minuti legati a client, redirect e sfida PKCE; token di rinnovo a rotazione; blocco dopo 5 login falliti (per indirizzo e per utente, risposta 429 per 10 minuti); pagina di login non incorniciabile e senza cache; limite di dimensione del risultato (`MaxResultBytes`, 200 KB); limite di chiamate al minuto per utente; controllo `Origin` che rifiuta per default ogni origine tranne l'indirizzo pubblico del server (le richieste senza `Origin`, tipiche di Claude.ai e Claude Code, passano; altre origini vanno in `AllowedOrigins`); registrazione di ogni chiamata (`OnToolCalled`, impostabile con `Setup.Connector.OnToolCalled` dopo `Attach`: utente, strumento, esito, durata; gli argomenti non vengono registrati).

A tuo carico:

- **Usa solo HTTPS**: la password viaggia nel modulo di login.
- **Decidi cosa esporre.** Quanto restituisce una funzione finisce nel contesto del modello. Non esporre funzioni che restituiscono segreti, e valuta con attenzione quelle che scrivono o cancellano (`aeModifies`, `aeDestructive`). Un testo letto dai dati (ad esempio una nota di un ordine di lavoro) può contenere istruzioni rivolte al modello: i permessi del ruolo sono la vera barriera.
- Le funzioni devono essere **thread-safe**, come ogni worker.
- Con `ExposeUndescribed` ogni funzione è raggiungibile dall'AI nei limiti del ruolo: per `users`, `roles` e simili valuta `Hidden := True`. Gli endpoint senza `OpType` (ricerche, `permission_areas`) sono permessi a ogni utente autenticato: valuta se descriverli.

### Contratti da conoscere

- **Durata della configurazione utente.** `OnAuthenticateRequest` (`TAIAuthenticateEvent`) restituisce un `TAIAuthenticationContext`, non una copia del JSON. `TTokenManager.isValidBearerRequest` incrementa sotto lock il contatore d'uso del token e il contesto lo decrementa a fine richiesta. Se nel frattempo il token viene revocato o scade, sparisce subito dalle ricerche ma l'oggetto viene liberato soltanto quando termina l'ultima richiesta che lo sta usando. La configurazione passata a `hasPermission` e ai worker è quindi l'oggetto originale creato dall'applicazione, senza copie e senza la chiave tecnica `_tm`.
- **Critical section ricorsiva.** Il `TTokenManager` può entrare più volte nella propria critical section dallo stesso thread. `isValidBearerRequest` mantiene volutamente il lock mentre richiama `getScopedToken`: non è un deadlock e permette di avere un solo percorso di validazione del token. Il lock viene comunque rilasciato prima di eseguire il worker; il contesto protegge la durata dell'oggetto senza serializzare le chiamate MCP.
- **`Prepare` una sola volta.** Il catalogo degli strumenti si costruisce una volta sola, all'avvio, prima di accettare connessioni (`Attach` lo fa). Una seconda chiamata non lo ricostruisce.
- **Il worker è il confine applicativo.** Il connettore chiama direttamente il worker e salta il percorso HTTP del modulo di routing: regole di business, transazioni, audit e filtri sui campi sensibili devono stare nel worker, non nel trasporto. L'oggetto con gli argomenti viene liberato a fine chiamata: chi lo vuole conservare ne fa una copia.
- **Le annotazioni (`aeReadOnly`, `aeModifies`, `aeDestructive`) sono suggerimenti** per il client, che può ignorarli. La protezione vera è il permesso del ruolo e il controllo nel worker.
- **`PublicBaseURL` è obbligatorio e contiene solo schema, host e porta.** Senza, gli endpoint OAuth rispondono `server_error` (l'indirizzo non si ricava dall'header `Host`, scelto dal client). Con un percorso (per esempio `.../mcp`) `TLBOAuthSetup` non attiva OAuth e `ValidateConfiguration` segnala l'errore: gli indirizzi risulterebbero `.../mcp/mcp` e `.../mcp/oauth/authorize`. `AllowHostFallback := True` esiste solo per lo sviluppo.
- **Limiti in memoria.** Richieste di login in sospeso, codici e registri dei tentativi hanno un massimo rigido: al raggiungimento si scarta la voce più vecchia, mai l'intera tabella.
- **Generatore casuale.** Se non si riesce a generare un token, l'operazione fallisce (`server_error`): non viene mai creato un identificatore vuoto. La lettura di `/dev/urandom` si ritenta qualche volta.

## 7. Limiti della versione attuale

- Solo risposte JSON: niente flussi SSE e niente sessioni (`GET /mcp` risponde 405). Claude.ai e Claude Code funzionano così.
- Registrazione client senza stato e senza CIMD (Claude passa alla registrazione dinamica).
- Il rinnovo con un client diverso da quello che ha emesso il token invalida anche quel token (scelta prudente).
- La persistenza dei token su SQLite e il flusso con un vero certificato HTTPS non sono ancora coperti da test automatici: `Esempi/Ex05_TokenPersistenti` verifica a mano che i token sopravvivano a un riavvio e che il salvataggio periodico funzioni.
- I token sono salvati in chiaro nella tabella SQLite del `TokenManager` (come quelli del sito): chi copia il database può riusarli finché non scadono. Salvare un digest e proteggere il file è il passo successivo.
- Non c'è la revoca dei token per utente né la "famiglia" dei refresh token (il riuso di un vecchio refresh token rifiuta la richiesta ma non invalida i nuovi).
- Il blocco dopo 5 login falliti è anche per nome utente: chi conosce un nome utente può bloccarlo per 10 minuti. Il blocco per indirizzo protegge dalla forza bruta, quello per utente dalla distribuzione: è un compromesso da rivedere.
- Il descrittore dei parametri non ha ancora minimi/massimi, pattern, valori predefiniti, oggetti annidati e controllo dei nomi duplicati.
- Le funzioni che restituiscono elenchi devono avere un `limit` con massimo lato server: `MaxResultBytes` protegge il contesto del modello, non la memoria del server.
- Nessuna idempotency key né conferma per le operazioni distruttive: se servono, vanno nel worker.

## 8. Esempi e prove

La cartella `Esempi/` contiene cinque programmi, un client Python che fa quello che farebbe un agente AI (scoperta, registrazione, login con PKCE, uso del token, rinnovo) e uno script di prova automatica. Vedi `Esempi/README.md`.

| Esempio | Cosa mostra |
|---|---|
| `Ex01_Minimo` | Il collegamento completo scritto per esteso, in quattro punti numerati. |
| `Ex02_Parametri` | Tutti i tipi di parametro, la validazione prima del worker, `aeModifies`, `aeDestructive`, `Hidden`, `ExposeUndescribed`. |
| `Ex03_PermessiAudit` | Cosa vede ogni ruolo, porta chiusa a un ruolo, `RequireOpTypeForTools`, limite di chiamate, `AllowedOrigins`, registro delle chiamate. |
| `Ex04_SoloOAuth` | OAuth senza connettore AI, con ambiti e diciture proprie. |
| `Ex05_TokenPersistenti` | Token su SQLite che sopravvivono a un riavvio. |

**Stato di verifica.** I programmi Pascal non sono stati compilati né eseguiti da chi li ha scritti: l'ambiente non aveva Free Pascal. Il client Python è stato provato solo contro un simulatore del protocollo, non contro questi server. I controlli di `smoke_test.py` per Ex02, Ex03 ed Ex04 sono scritti leggendo il codice e non sono mai stati eseguiti su un server vero.

## 9. Nota su `TTokenManager.CreateToken` (difetto corretto)

Con FPC 3.2.2 `TNetEncoding.Base64.EncodeBytesToString` sostituisce con `?` ogni byte ≥ `$80` prima di codificare: i token di sessione avevano circa metà dei byte uguali a `?` (stringa `Pz8`) e l'entropia scendeva da 256 a circa 144 bit. Ora `CreateToken` usa l'unit `base64`. **Lo stesso difetto è in `TIdroWorldHandlersAuth.GenResetToken`** (usa `TNetEncoding.Base64.EncodeBytesToString` sui 32 byte casuali): conviene cambiarlo con

```pascal
SetString(_Raw, PAnsiChar(@_Buff[0]), Length(_Buff));   // _Raw: RawByteString
Result := EncodeStringBase64(_Raw);                     // unit base64
```
