# LBmicroWebServer: routing e modulo opzionale LBWebActions

**Documentazione tecnica della nuova architettura**
**Data:** 15 novembre 2026
**Ambito:** `uWebRouteRegistry`, `LBmicroWebServer/extensions/actions` e integrazione IdroWorld

---

## 1. Sintesi

La nuova versione è **più manutenibile di quella iniziale**, soprattutto perché separa tre responsabilità che prima tendevano a sovrapporsi:

1. **LBmicroWebServer core** risolve la rotta, autentica la sessione quando richiesto e invoca un worker.
2. **LBWebActions**, estensione opzionale, interpreta il campo JSON `action`, effettua il binding verso un metodo `DoXxx`, applica un controllo di autorizzazione tramite callback e smista la richiesta.
3. **L’applicazione ospitante**, nel caso concreto IdroWorld, definisce capability, ruoli, implicazioni, esposizione AI e regole di dominio.

Il flusso generale è:

```text
richiesta HTTP
    │
    ▼
LBmicroWebServer core
    ├─ risolve Method + URI
    ├─ controlla RequiresAuth
    └─ invoca il worker registrato
            │
            ▼
LBWebActions, se usato dal worker
    ├─ legge action
    ├─ trova la definizione
    ├─ chiede all’applicazione se è autorizzata
    └─ invoca DoXxx
            │
            ▼
Codice applicativo
    ├─ applica regole dipendenti dai dati
    ├─ esegue l’operazione
    └─ produce JSON e status HTTP
```

La separazione è importante: il core **non conosce** action, capability, ruoli, gruppi, `Implies` o metadati AI. LBWebActions conosce action e token di autorizzazione, ma considera token e metadati come valori opachi. Solo l’applicazione attribuisce loro un significato.

---

## 2. Componenti e responsabilità

### 2.1 LBmicroWebServer core

Unità principale del routing:

```text
LBmicroWebServer/uWebRouteRegistry.pas
```

Responsabilità:

- caricare da XML applicazioni, aree ed endpoint;
- costruire descrittori delle rotte;
- associare ogni endpoint a un worker registrato;
- risolvere le richieste tramite `HTTP method + URI`;
- autenticare la sessione quando `RequiresAuth="true"`;
- invocare il worker della famiglia corretta;
- gestire endpoint Standard, FileDownload, Auth, Upload, Proxy e AIConnector;
- validare che le rotte dichiarate siano collegate.

Non è responsabilità del core:

- leggere i nodi `<Action>`;
- interpretare una capability;
- autorizzare un’operazione applicativa;
- espandere `Implies`;
- leggere ruoli o gruppi;
- decidere quali action esporre all’AI.

### 2.2 Estensione opzionale LBWebActions

Percorso:

```text
LBmicroWebServer/extensions/actions/
```

Unità:

| Unità | Responsabilità |
|---|---|
| `uLBActionTypes.pas` | Definizioni delle action, token e metadati opachi, callback di autorizzazione, raccolta degli errori di configurazione. |
| `uLBActionNames.pas` | Validazione kebab-case e conversione `action` → `DoXxx` / `DescribeXxx`. |
| `uLBActionDispatcher.pas` | Lettura di `action`, ricerca della definizione, 400/403, dispatch al `TStandardWorker`. |
| `uLBActionBinder.pas` | Binding RTTI delle action ai metodi `published DoXxx`. |
| `uLBActionGuard.pas` | Autorizzazione deny-by-default e protezione di action implicite. |

L’estensione è riutilizzabile da altre applicazioni e non dipende da unità IdroWorld.

### 2.3 IdroWorld

IdroWorld fornisce la politica applicativa:

| Unità | Responsabilità |
|---|---|
| `uIdroWorld_Capabilities.pas` | Legge capability e action da `Routes.xml`, valida i riferimenti ed espande `Implies`. |
| `uIdroWorld_ActionCatalog.pas` | Traduce `Action.Capability` nell’`AuthorizationToken` opaco usato da LBWebActions. |
| `uIdroWorld_ActionAuthorization.pas` | Interpreta il token come capability IdroWorld e richiama `UserHasCapability`. |
| `uIdroWorld_ActionHandler.pas` | Integra gli handler IdroWorld con dispatcher, binder e guard generici. |
| `uIdroWorld_AIActionCatalog.pas` | Interpreta il metadato `AI`, valida `DescribeXxx` e autorizza le action AI. |
| `uIdroWorldWebRouteModule.pas` | Espone il controllo centralizzato `UserHasCapability` e i servizi applicativi collegati al routing. |

---

## 3. Modello dichiarativo di Routes.xml

Esempio ridotto:

```xml
<FunctionalArea Code="workorders" Description="Ordini di lavoro" App="cmms">
  <Capability Code="SeguireRichieste" Assignable="true"/>
  <Capability Code="AprireRichiesta" Assignable="true"/>
  <Capability Code="AccettareRichiesta" Assignable="true"/>
  <Capability Code="RespingereRichiesta" Assignable="true"/>

  <Endpoint
      Code="Requests"
      URI="/api/workorders/requests"
      Method="POST"
      RequiresAuth="true">
    <Action Code="list" Capability="SeguireRichieste" AI="read"/>
    <Action Code="create" Capability="AprireRichiesta" AI="none"/>
    <Action Code="accept" Capability="AccettareRichiesta" AI="none"/>
    <Action Code="reject" Capability="RespingereRichiesta" AI="none"/>
  </Endpoint>
</FunctionalArea>
```

### 3.1 Attributi letti dal core

Il registro generico legge, per ogni endpoint:

- area di appartenenza;
- `Code`;
- `URI`;
- `Method`, con valore predefinito `POST`;
- `RequiresAuth`, che è vero salvo valore esplicito `false`;
- `Kind`, con valore predefinito `Standard`;
- `Target` per gli endpoint Proxy;
- `Description`, utilizzabile anche dal connettore AI.

I figli dell’endpoint, compresi i nodi `<Action>`, sono deliberatamente ignorati dal core.

### 3.2 Attributi letti dall’applicazione IdroWorld

Il catalogo applicativo legge dallo stesso XML:

- `<Capability Code Assignable Implies ...>`;
- `<Action Code Capability AI>`;
- `Code`, usato per indicizzare le action dell’endpoint;
- informazioni dell’area, compresa l’applicazione associata.

Questa doppia lettura non duplica la politica: lo stesso file rimane la fonte dichiarativa, ma ogni livello legge soltanto ciò che gli compete.

---

## 4. Ruolo di `Endpoint.Code` nella nuova architettura

### 4.1 Significato effettivo

Nella nuova architettura `Endpoint.Code`:

- **non rappresenta una funzione applicativa da eseguire**;
- **non rappresenta una permission**;
- **non rappresenta una capability**;
- **non sceglie l’action contenuta nel body**.

È la **chiave simbolica stabile dell’endpoint**. Serve, durante la costruzione del routing, ad associare l’endpoint dichiarato in `Routes.xml` al worker o al dispatcher registrato nel codice.

Esempio reale:

```xml
<Endpoint
    Code="Requests"
    URI="/api/workorders/requests"
    Method="POST"
    RequiresAuth="true">
  ...
</Endpoint>
```

corrisponde a:

```pascal
Self.ActionEndpoint('Requests').BindObject(FRequestActions);
```

La corrispondenza è quindi:

```text
Routes.xml                         codice Pascal
------------------------------    ------------------------------------------
Area Code="workorders"             handler AreaCode = "workorders"
Code="Requests"          <->  ActionEndpoint('Requests')
                                      │
                                      └─ RegisterStandardWorker(
                                           'Requests',
                                           @Dispatcher.Execute)
```

### 4.2 Come viene usata nei sorgenti

Il percorso esatto è il seguente:

1. `TRouteRegistry.LoadFromXMLFile` copia `Endpoint.Code` in `TEndpointDescriptor.EndpointCode`.
2. `TActionHandlerBase.ActionEndpoint('Requests')` crea il dispatcher e registra `@Dispatcher.Execute` nel proprio `TRouteHandlerBase` con lo stesso nome simbolico.
3. `TRouteRegistry.RegisterHandler` esamina gli endpoint dell’area.
4. Per ciascun descrittore chiama `aHandler.GetWorkerMethod(Descriptor.EndpointCode, ...)`.
5. Il metodo restituito viene salvato nella `TDispatchEntry` della rotta.
6. La tabella di dispatch runtime viene indicizzata con `HTTP method + URI`.

Ne consegue una distinzione importante:

```text
Code       = chiave di collegamento dichiarazione ↔ worker, usata all’avvio
Method+URI = chiave di risoluzione della richiesta HTTP a runtime
Action    = operazione interna scelta nel body dopo il routing
```

Il core confronta il nome del worker con `SameText`; nel catalogo IdroWorld il `Code` è anche la chiave dell’elenco degli endpoint dell’area. È comunque raccomandato usare sempre la stessa grafia e le stesse maiuscole in XML e codice.

### 4.3 Perché `Code` non va confuso con `Action`

`Code="Requests"` identifica l’endpoint aggregato. Non significa “esegui Requests”. Solo dopo l’invocazione del dispatcher il body seleziona l’operazione:

```json
{
  "action": "reject",
  "request_uuid": "...",
  "reason": "..."
}
```

Il dispatcher traduce `reject` in `DoReject`. Il codice simbolico `Requests` resta invariato per tutte le action dell’endpoint.

### 4.4 Migrazione completata da `Function` a `Code`

L’attributo storico `Function` è stato sostituito con `Code` in `Routes.xml`, nel parser del core, nei cataloghi IdroWorld e nei descrittori runtime. La semantica non cambia: `Code` resta una chiave simbolica stabile e opaca, non il nome di una funzione Pascal.

### 4.5 Si può eliminare?

Non senza modificare il meccanismo di registrazione. Il registro ha bisogno di una chiave per collegare ogni dichiarazione al worker giusto.

Le alternative sarebbero:

- registrare il worker direttamente con `URI + Method`;
- introdurre un altro identificatore interno;
- affidarsi all’ordine degli endpoint, soluzione fragile e da evitare.

Registrare il codice con `URI + Method` è sconsigliato perché accoppia gli handler alla rappresentazione HTTP. Con la chiave simbolica, invece, è possibile cambiare:

```text
/api/workorders/requests
```

in:

```text
/api/v2/work-requests
```

senza rinominare `ActionEndpoint('Requests')` o gli oggetti di dominio. L’URI resta configurazione di trasporto; il codice resta legato a un’identità logica stabile.

**Raccomandazione:** mantenere `Endpoint.Code` come chiave simbolica stabile e non ricavarlo dall’URI.

---

## 5. Funzionamento del modulo di routing

### 5.1 Oggetti principali

#### `TEndpointDescriptor`

Rappresenta un endpoint dichiarato in XML. Contiene area, applicazione, `EndpointCode`, descrizione, URI, metodo, autenticazione, kind e target.

#### `TRouteHandlerBase`

È la base degli handler applicativi. Ogni istanza appartiene a un’area e contiene l’elenco dei worker registrati sotto una chiave `EndpointCode`.

Metodi di registrazione:

```pascal
RegisterStandardWorker
RegisterFileDownloadWorker
RegisterAuthWorker
RegisterUploadWorker
```

Ogni registrazione conserva:

- nome simbolico dell’endpoint;
- metodo Pascal come `TMethod`;
- famiglia del worker.

#### `TDispatchEntry`

È la voce operativa usata a runtime. Contiene:

- area e funzione simbolica;
- `RequiresAuth`;
- `Kind`;
- metodo già risolto;
- esito del controllo di coerenza del kind;
- eventuale target Proxy.

#### `TRouteRegistry`

Possiede:

- applicazioni dichiarate;
- endpoint dichiarati;
- tabella di dispatch;
- candidati AI;
- target remoti.

#### `TWebRouteModule`

È il `TRequestChainProcessor` aggiunto al web server. Usa il registro, autentica la richiesta e invoca il worker corretto.

### 5.2 Chiave della tabella di dispatch

La chiave runtime è costruita come:

```text
UpperCase(HTTP method) + ':' + URI
```

Esempio:

```text
POST:/api/workorders/requests
```

Il metodo HTTP è normalizzato in maiuscolo. L’URI viene mantenuto come dichiarato, dopo `Trim`.

### 5.3 Registrazione di un handler

Quando l’applicazione chiama:

```pascal
WebRouteModule.RegisterHandler(FHWorkOrders);
```

il registro:

1. considera gli endpoint con la stessa area dell’handler;
2. salta Proxy e AIConnector, che hanno un percorso diverso;
3. usa `EndpointCode` per trovare il worker nell’handler;
4. verifica che il `Kind` dichiarato coincida con la famiglia del worker;
5. crea la `TDispatchEntry` indicizzata da `Method + URI`;
6. per gli endpoint Standard autenticati crea anche una voce candidata all’esposizione AI.

Un mismatch tra `Kind` XML e famiglia di registrazione viene memorizzato e produce errore 500 quando la rotta viene invocata.

### 5.4 Sequenza di avvio

La sequenza corretta è:

```text
1. caricare e validare il catalogo capability/action dell’applicazione
2. inizializzare la cache dei permessi
3. creare TWebRouteModule o la sottoclasse applicativa
4. LoadRoutesFromXML
5. configurare l’integrazione action/autorizzazione
6. creare e registrare tutti gli handler
7. registrare gli endpoint Proxy
8. ValidateRoutes
9. validare copertura e binding delle action
10. impostare OnAuthenticateRequest
11. aggiungere il modulo alla chain del web server
```

In IdroWorld i passaggi 8 e 9 sono distinti:

```pascal
_WebRouteModule.ValidateRoutes;
ValidateActionSecurity;
```

Il primo verifica il trasporto; il secondo verifica la sicurezza applicativa.

### 5.5 Elaborazione di una richiesta

Per una richiesta normale, `TWebRouteModule.DoProcessRequest` esegue:

1. separazione di risorsa e parametri URI;
2. lookup tramite `Method + Resource`;
3. controllo di coerenza del kind;
4. autenticazione se richiesta;
5. parsing del body JSON, quando previsto dal kind;
6. invocazione del worker;
7. serializzazione della risposta;
8. applicazione dello status restituito dal worker.

Se la rotta non esiste, il modulo restituisce `False` alla chain e lascia che altri processori o il server producano il 404.

### 5.6 Autenticazione, non autorizzazione

`RequiresAuth="true"` significa soltanto:

> prima di chiamare il worker deve esistere una sessione valida.

Il core chiama `OnAuthenticateRequest`, che restituisce:

- `aUserId`;
- `aUserConfig`;
- esito della sessione.

Il core non decide se l’utente può eseguire `accept`, `reject`, `delete` o qualunque altra operazione. Tale decisione avviene nel dispatcher o nel worker applicativo.

Il vecchio hook virtuale `hasPermission` è conservato solo per compatibilità, restituisce `False` e non viene chiamato dal dispatch.

### 5.7 Famiglie di endpoint

| Kind | Worker | Corpo | Note |
|---|---|---|---|
| `Standard` | `TStandardWorker` | JSON | È la famiglia normalmente usata dagli endpoint con action esplicita. |
| `FileDownload` | `TFileDownloadWorker` | Nessun body action necessario | Il worker risolve file e nome suggerito. In IdroWorld è protetto da action implicita. |
| `Auth` | `TAuthWorker` | JSON | Ha accesso anche a parser e header di risposta. Login pubblico e logout protetto sono casi distinti. |
| `Upload` | `TUploadWorker` | File già ricevuto dal trasporto | In IdroWorld è protetto da action implicita. |
| `Proxy` | Nessun worker locale | Richiesta inoltrata | Il core autentica e inoltra; eventuale autorizzazione fine deve essere applicata dal servizio remoto o da un’architettura dedicata. |
| `AIConnector` | Gestito dal connettore | Specifico del connettore | Non entra nella normale tabella di dispatch. |

---

## 6. Funzionamento di LBWebActions

### 6.1 Definizione di una action

Una `TLBActionDefinition` contiene:

```text
Code
AuthorizationToken
Metadata
```

Esempio concettuale:

```text
Code               = reject
AuthorizationToken = RespingereRichiesta
Metadata['AI']      = none
```

Per LBWebActions il token è solo una stringa. Potrebbe essere una capability IdroWorld, uno scope OAuth, un permesso ERP o un codice deciso da un’altra applicazione.

### 6.2 Codice action e convenzione dei nomi

Il codice deve essere kebab-case stretto:

- primo carattere alfabetico minuscolo;
- caratteri successivi: `a-z`, `0-9` o trattino;
- niente trattino iniziale o finale;
- niente trattini consecutivi;
- dopo un trattino deve comparire una lettera.

Conversioni:

| Action | Metodo di esecuzione | Descrittore AI |
|---|---|---|
| `list` | `DoList` | `DescribeList` |
| `set-result` | `DoSetResult` | `DescribeSetResult` |
| `change-status` | `DoChangeStatus` | `DescribeChangeStatus` |

### 6.3 Dispatcher

`TLBActionDispatcher.Execute`:

1. cerca `action` nel JSON;
2. richiede che sia una stringa non vuota;
3. cerca la definizione esatta e case-sensitive;
4. cerca il worker collegato;
5. autorizza mediante `TLBActionGuard`;
6. invoca il `TStandardWorker`.

Il body completo viene passato al worker; il dispatcher non rimuove `action`.

### 6.4 Autorizzazione tramite callback

Firma:

```pascal
TLBActionAuthorizeEvent = function(
  Sender: TObject;
  aUserId: Integer;
  aUserContext: TJSONObject;
  const anAreaCode: String;
  const anEndpointCode: String;
  const anActionCode: String;
  const anAuthorizationToken: String
): Boolean of object;
```

L’estensione passa tutte le informazioni all’applicazione e si limita a osservare `True` o `False`.

Regola del guard:

```text
definizione assente                 → deny
AuthorizationToken vuoto           → allow generico dell’estensione
callback assente con token presente → deny + errore di configurazione
callback = False                    → deny
callback = True                     → allow
```

IdroWorld impone un vincolo più stretto: per un endpoint autenticato una capability vuota è un errore di avvio. Se serve soltanto una sessione valida usa il token esplicito `@authenticated`.

### 6.5 Registrazione esplicita

Per handler semplici:

```pascal
Self.ActionEndpoint('Read')
  .Add('list', @Self.DoList)
  .Add('get', @Self.DoGetOne);
```

Il catalogo fornisce le definizioni; il codice associa esplicitamente ogni action al worker.

Vantaggi:

- leggibilità immediata;
- nessuna dipendenza da RTTI per il collegamento;
- adatto a endpoint con poche action.

### 6.6 Binding RTTI

Per gruppi più grandi:

```pascal
Self.ActionEndpoint('Requests').BindObject(FRequestActions);
```

Il binder, per ogni action dichiarata:

1. converte il codice in `DoXxx`;
2. usa `MethodAddress` sull’oggetto;
3. richiede quindi un metodo con RTTI, normalmente `published` e classe compilata con `{$M+}`;
4. converte il `TMethod` in `TStandardWorker`;
5. registra il worker nel dispatcher.

Esempio:

```pascal
{$M+}
TRequestActions = class
published
  function DoList(...): TJSONData;
  function DoAccept(...): TJSONData;
  function DoReject(...): TJSONData;
end;
```

Il binding RTTI riduce la duplicazione, ma sposta una parte del controllo dal compilatore alla validazione di avvio. Per questo `ValidateBindings` è obbligatorio.

### 6.7 Action implicite

FileDownload, Upload e alcuni Auth non trasportano una normale action nel body. IdroWorld usa adapter protetti:

```pascal
RegisterProtectedFileDownloadWorker
RegisterProtectedUploadWorker
RegisterProtectedAuthWorker
```

L’endpoint deve dichiarare **esattamente una action**. Il catalogo costruisce una definizione implicita e il `TLBActionGuard` la autorizza prima di invocare il worker della famiglia specifica.

Esempio:

```xml
<Endpoint
    Code="AttachmentDownload"
    URI="/api/workorders/attachments/download"
    Method="GET"
    RequiresAuth="true"
    Kind="FileDownload">
  <Action Code="download" Capability="ConsultareAllegati"/>
</Endpoint>
```

registrato come:

```pascal
Self.RegisterProtectedFileDownloadWorker(
  'AttachmentDownload',
  @FAttachments.ResolveDownload
);
```

### 6.8 Errori aggregati e fail-fast

Gli errori di configurazione vengono accumulati in `TLBActionConfigurationErrors`, registrati nel log e poi trasformati in eccezione all’avvio.

Casi rilevati:

- action non valida;
- definizione duplicata;
- worker duplicato;
- worker nullo;
- action definita senza worker;
- oggetto di binding assente;
- metodo `DoXxx` non trovato;
- token non vuoto senza callback;
- endpoint implicito con zero o più di una action;
- endpoint autenticato non coperto da dispatcher o adapter;
- action AI senza `DescribeXxx`.

Questa strategia evita configurazioni parzialmente funzionanti che fallirebbero soltanto alla prima richiesta reale.

---

## 7. Autorizzazione IdroWorld

### 7.1 Dal nodo XML al guard

Il percorso del token è:

```text
<Action Capability="RespingereRichiesta">
        │
        ▼
TIdroWorldCapabilityCatalog
        │
        ▼
TIdroWorldActionCatalog
        │  Capability → AuthorizationToken
        ▼
TLBActionDefinition
        │
        ▼
TLBActionGuard
        │
        ▼
TIdroWorldActionAuthorizer.Authorize
        │
        ▼
TIdroWorldWebRouteModule.UserHasCapability
```

### 7.2 Decisione di capability

`UserHasCapability` applica:

1. nessun contesto utente → deny;
2. `is_admin=true` → allow;
3. token `@authenticated` → allow, perché la sessione è già stata autenticata dal core;
4. capability vuota o servizi mancanti → deny;
5. verifica che la capability sia dichiarata;
6. lookup nella cache del ruolo usando `role_id`, area e capability.

Le capability del ruolo sono già espanse secondo `Implies`, quindi il controllo per richiesta non deve ricalcolare il grafo.

### 7.3 Regole di dominio

Superare il controllo della capability non significa che l’operazione sia sempre valida. Il metodo `DoXxx` deve continuare a verificare condizioni dipendenti dai dati, per esempio:

- visibilità dell’asset;
- gruppo gestito;
- stato corrente dell’ordine o della richiesta;
- proprietà del record;
- coerenza degli identificativi;
- campi obbligatori;
- transizione di stato ammessa.

Separazione consigliata:

```text
capability → “questo ruolo può tentare questa classe di operazioni”
DoXxx      → “questa specifica operazione è valida su questi dati”
```

---

## 8. Esempio end-to-end: rifiuto di una richiesta

### 8.1 Dichiarazione

```xml
<Endpoint Code="Requests"
          URI="/api/workorders/requests"
          Method="POST"
          RequiresAuth="true">
  <Action Code="reject"
          Capability="RespingereRichiesta"
          AI="none"/>
</Endpoint>
```

### 8.2 Collegamento

```pascal
Self.ActionEndpoint('Requests').BindObject(FRequestActions);
```

Il binder cerca:

```pascal
published
  function DoReject(...): TJSONData;
```

### 8.3 Richiesta

```http
POST /api/workorders/requests
Cookie: ...
Content-Type: application/json

{
  "action": "reject",
  "request_uuid": "...",
  "reason": "Duplicata"
}
```

### 8.4 Sequenza

```text
TRouteRegistry.Resolve("POST", "/api/workorders/requests")
    │
    ├─ nessuna rotta → la chain prosegue verso 404
    │
    ▼
RequiresAuth
    │
    ├─ cookie mancante/non valido → 401
    │
    ▼
TActionEndpoint.Execute
    │
    ├─ action mancante → 400
    ├─ action sconosciuta → 400
    │
    ▼
TLBActionGuard
    │
    ├─ capability negata → 403
    │
    ▼
TRequestActions.DoReject
    │
    ├─ regola di dominio violata → status deciso dal dominio
    └─ operazione valida → aggiornamento e risposta
```

Un utente con `AccettareRichiesta` ma senza `RespingereRichiesta` raggiunge l’endpoint perché possiede una sessione valida, ma riceve 403 quando il dispatcher valuta `reject`. Il router non deve conoscere l’unione delle capability dell’endpoint.

---

## 9. Codici di risposta rilevanti

| Condizione | Status | Livello |
|---|---:|---|
| Rotta non risolta dal modulo | 404 finale, normalmente prodotto dalla chain/server | Routing |
| `Kind` incoerente con la registrazione | 500 | Routing |
| `RequiresAuth=true` senza callback di autenticazione | 500 | Routing |
| Sessione assente o non valida | 401 | Routing |
| `action` assente, vuota o non stringa | 400 | LBWebActions |
| `action` sconosciuta | 400 | LBWebActions |
| Worker non configurato a runtime | 500 | LBWebActions |
| Token negato dalla callback | 403 | LBWebActions/applicazione |
| Regola di dominio violata | definito dal `DoXxx` | Applicazione |
| Operazione riuscita | definito dal `DoXxx`, normalmente 200 | Applicazione |

Le risposte prodotte da `LBActionFailure` hanno la forma:

```json
{
  "successful": false,
  "error": "..."
}
```

---

## 10. Esposizione all’AI

`AI="read"` è una regola IdroWorld, non di LBWebActions.

L’estensione conserva il valore come metadato opaco. `TIdroWorldAIActionCatalog`:

- seleziona soltanto le action `AI="read"`;
- richiede il corrispondente `DescribeXxx`;
- verifica la capability dell’utente;
- accetta un’`action` esplicita negli argomenti;
- se esiste una sola action AI autorizzata, può iniettarla automaticamente negli argomenti.

Le action di scrittura non diventano strumenti AI solo perché esistono nel dispatcher. L’esposizione richiede una dichiarazione esplicita.

---

## 11. Come aggiungere o modificare una action

### 11.1 Aggiungere un’action a un endpoint esistente

1. Dichiarare o riutilizzare una capability nell’area.
2. Aggiungere il nodo `<Action>` all’endpoint corretto.
3. Usare un codice kebab-case.
4. Impostare `AI="none"` salvo esposizione intenzionale in lettura.
5. Collegare il worker:
   - `.Add('codice', @Self.DoXxx)` per registrazione esplicita; oppure
   - aggiungere `published DoXxx` all’oggetto usato da `BindObject`.
6. Se `AI="read"`, aggiungere `published DescribeXxx`.
7. Aggiornare il frontend affinché invii `action`.
8. Aggiungere test 400, 403, successo e regole di dominio.

Esempio:

```xml
<Action Code="archive" Capability="ArchiviareOrdine" AI="none"/>
```

```pascal
published
  function DoArchive(
    aUserId: Integer;
    aUserConfig, aRequestData: TJSONObject;
    out aStatusCode: Integer
  ): TJSONData;
```

### 11.2 Aggiungere un nuovo endpoint

1. Scegliere una `Code` stabile e univoca nell’area.
2. Definire `URI`, `Method`, `RequiresAuth` e `Kind`.
3. Dichiarare le action.
4. Registrare nel codice lo stesso nome simbolico:

```pascal
Self.ActionEndpoint('NewEndpoint').BindObject(FNewActions);
```

5. Registrare l’handler dell’area nel modulo di routing.
6. Verificare che `ValidateRoutes` e `ValidateActionSecurity` completino senza errori.

### 11.3 Cambiare URI senza cambiare identità logica

È sufficiente cambiare `URI` in `Routes.xml` e aggiornare i client. Se `Code` resta invariato, il collegamento al worker non cambia.

### 11.4 Rinominare il valore di `Endpoint.Code`

È necessario aggiornare insieme:

- il valore di `Code` in `Routes.xml`;
- `ActionEndpoint('...')` o la registrazione worker corrispondente;
- eventuali riferimenti applicativi o AI che usano il codice dell’endpoint.

L’URI può restare invariato.

---

## 12. Diagnostica

| Sintomo | Causa probabile | Verifica |
|---|---|---|
| “endpoint senza handler” | Handler non registrato o area non corrispondente | Confrontare `FunctionalArea.Code` con `Handler.AreaCode`. |
| “nessun metodo registrato per l’endpoint” | `Code` XML diverso dal nome passato nel codice | Confrontare `Code="X"` con `ActionEndpoint('X')` o `RegisterXxxWorker('X')`. |
| `Kind` incoerente | Famiglia XML diversa da quella del worker | Verificare `Kind` e metodo `RegisterStandard/FileDownload/Auth/UploadWorker`. |
| “action is required” | Body senza `action` o valore non stringa | Controllare il JSON del client. |
| “unknown action” | Codice non dichiarato o diversa capitalizzazione | Confrontare con `Action.Code`; il match è case-sensitive. |
| “expected method <Do...>” | Metodo RTTI assente o non visibile | Verificare `{$M+}`, sezione `published`, nome e firma. |
| 403 con sessione valida | Capability non concessa o callback non configurata | Verificare role cache, area, token e authorizer. |
| Errore di copertura all’avvio | Endpoint autenticato privo di dispatcher/adapter | Registrare `ActionEndpoint` o un worker protetto implicito. |
| Action implicita non valida | Endpoint FileDownload/Upload/Auth con numero di action diverso da uno | Dichiarare esattamente una action. |
| Action AI non esposta | `AI` non è `read`, manca `DescribeXxx` o capability negata | Verificare catalogo AI, descrittore e ruolo. |

---

## 13. Valutazione di manutenibilità

### 13.1 Miglioramenti rispetto alla struttura iniziale

**Separazione delle responsabilità.** Le modifiche di routing non richiedono di conoscere le capability; le modifiche di capability non richiedono di cambiare il core.

**Meno endpoint puramente operativi.** Più operazioni omogenee possono condividere un endpoint e differenziarsi tramite action, senza perdere l’autorizzazione per singola operazione.

**Riutilizzo.** Dispatch, kebab-case, RTTI, guard e gestione errori sono in un’estensione generica utilizzabile da applicazioni diverse.

**Deny-by-default.** Una configurazione incompleta viene negata o fa fallire l’avvio, invece di aprire involontariamente un’operazione.

**Validazione centralizzata.** Duplicati, action senza worker, metodi mancanti e buchi di copertura vengono rilevati prima del traffico reale.

**URI disaccoppiati dal codice.** `Endpoint.Code` conserva un’identità logica stabile mentre gli URI possono evolvere.

**Coerenza fra browser e AI.** Le stesse action e capability alimentano i due canali; il metadato `AI` limita esplicitamente l’esposizione.

### 13.2 Costi e punti da sorvegliare

**RTTI.** `BindObject` è compatto, ma gli errori di nome emergono all’avvio e non sempre in compilazione. La validazione di startup non deve essere rimossa.

**Due lettori dello stesso XML.** Core e IdroWorld leggono porzioni diverse dello stesso file. È una separazione voluta, ma conviene mantenere test/schema che impediscano divergenze.

**Stato globale dell’integrazione IdroWorld.** `uIdroWorld_ActionHandler` usa variabili globali per catalogo, authorizer, errori e copertura. Funziona nel modello attuale a singola configurazione, ma una futura evoluzione verso più istanze del server dovrebbe introdurre un contesto esplicito posseduto dall’applicazione.

**Durata degli oggetti.** Catalogo, route module e authorizer devono restare vivi almeno quanto i dispatcher che ne usano i riferimenti.

**Proxy.** Poiché non esiste un worker locale, il core può applicare `RequiresAuth` ma non il guard action standard. Le regole fini devono essere applicate dal servizio remoto o da un adapter dedicato.

### 13.3 Conclusione

La nuova architettura è più manutenibile perché rende espliciti i confini:

```text
routing e sessione    → LBmicroWebServer core
action e dispatch     → LBWebActions
capability e dominio  → applicazione
```

Il beneficio non deriva soltanto dal minor numero di endpoint. Deriva soprattutto dal fatto che ogni livello può evolvere senza incorporare le regole degli altri. La condizione per mantenere questo vantaggio è conservare le validazioni di avvio, non spostare nuovamente le capability nel core e trattare `Endpoint.Code` come una chiave simbolica stabile, non come un’operazione o una permission.

---

## 14. Riferimenti ai sorgenti

### Core

- `LBmicroWebServer/uWebRouteRegistry.pas`
  - `TRouteHandlerBase`
  - `TDispatchEntry`
  - `TRouteRegistry.LoadFromXMLFile`
  - `TRouteRegistry.RegisterHandler`
  - `TRouteRegistry.Resolve`
  - `TWebRouteModule.DoProcessRequest`

### Estensione

- `LBmicroWebServer/extensions/actions/uLBActionTypes.pas`
- `LBmicroWebServer/extensions/actions/uLBActionNames.pas`
- `LBmicroWebServer/extensions/actions/uLBActionDispatcher.pas`
- `LBmicroWebServer/extensions/actions/uLBActionBinder.pas`
- `LBmicroWebServer/extensions/actions/uLBActionGuard.pas`

### Integrazione IdroWorld

- `IdroWorld/Backend/src/uIdroWorld_Capabilities.pas`
- `IdroWorld/Backend/src/uIdroWorld_ActionCatalog.pas`
- `IdroWorld/Backend/src/uIdroWorld_ActionAuthorization.pas`
- `IdroWorld/Backend/src/uIdroWorld_ActionHandler.pas`
- `IdroWorld/Backend/src/uIdroWorld_AIActionCatalog.pas`
- `IdroWorld/Backend/src/uIdroWorldWebRouteModule.pas`
- `IdroWorld/Backend/src/uIdroWorld_WS_Application.pas`
- `IdroWorld/Backend/src/Config/Routes.xml`
