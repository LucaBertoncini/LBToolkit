# Integrazione in IdroWorld

Cosa cambia nel backend IdroWorld per dare accesso a un agente AI. Il bootstrap (`TLBApplicationBoostrap`) **non cambia**.

**Questi file non possono essere applicati da qui:** `uIdroWorld_WS_Application.pas`, `uIdroWorld_HandlersAuth.pas` e `Routes.xml` del backend non erano tra i file ricevuti in quest'ultimo giro. Le modifiche sono descritte punto per punto; le otto unit `CMMS/*` con `DescribeAIFunction` sono nel pacchetto `IdroWorld_AI_Connector.zip` consegnato in precedenza e non sono cambiate.

## 1. `uIdroWorld_AuthService.pas` (nuovo, in questa cartella)

Verifica username e password e costruisce `{is_admin, role_id}`. Aggiungerla al progetto.

## 2. `uIdroWorld_HandlersAuth.pas`

- `uses`: aggiungere `uIdroWorld_AuthService`.
- Campo privato `FAuth : TIdroWorldAuthService;` (riferimento in prestito) e `property AuthService : TIdroWorldAuthService write FAuth;`.
- In `HandleLogin`, sostituire il blocco che va da `_RowJSON := FDataAccess.SelectRowToJSON('SELECT id FROM core.app_user ' ...` fino a `_Config.Add('role_id', _RoleId);` con:

```pascal
    if FAuth = nil then
    begin
      LBLogger.Write(1, 'TIdroWorldHandlersAuth.HandleLogin', lmt_Error, 'AuthService non impostato: login impossibile');
      Result := FailAnswer(aStatusCode, HTTP_STATUS_INTERNAL_ERROR, 'Authentication service not available');
      Exit;
    end;

    // Verifica delle credenziali e oggetto di SESSIONE (solo is_admin/role_id): la regola e' in
    // uIdroWorld_AuthService, la stessa usata dal login OAuth dell'agente AI. _Config e' NUOVO e passa a addNewToken.
    if not FAuth.AuthenticateCredentials(_User, _Psw, _IdUser, _Config) then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_UNAUTHORIZED, 'Invalid username or password');
      Exit;
    end;
    _IsAdmin := _Config.Get('is_admin', False);
    _RoleId := _Config.Get('role_id', Int64(0));
```

  Il resto di `HandleLogin` (token, cookie, elenco dei permessi) non cambia. `_RowJSON` e `_RoleRow` restano `nil` e vengono liberati come prima.

## 3. `uIdroWorld_WS_Application.pas`

```pascal
uses ..., uIdroWorld_AuthService, uLBOAuthSetup, uLBAIConnectorSetup;

// campi
FAuthService : TIdroWorldAuthService;
FOAuth       : TLBOAuthSetup;
FAI          : TLBAIConnectorSetup;

// costruttore
FAuthService := nil;
FOAuth := TLBOAuthSetup.Create;
FAI    := TLBAIConnectorSetup.Create;

// subito dopo la creazione di _DataAccess
FAuthService := TIdroWorldAuthService.Create(_DataAccess);
// dopo la creazione di FHAuth
FHAuth.AuthService := FAuthService;

// LoadConfigurationFromINIFileInternal, accanto a FAIChat.LoadFromINIFile(aFile)
FOAuth.LoadFromINIFile(aFile);
FAI.LoadFromINIFile(aFile);

// ULTIMA cosa di startingWebServer: dopo ogni RegisterHandler, dopo ValidateRoutes, dopo addChainProcessor(router)
FOAuth.Attach(FWebServer, FTokenManager, @FAuthService.AuthenticateCredentials, @FAuthService.RefreshUser);
FAI.Attach(FWebServer, _WebRouteModule, FOAuth);           // sempre DOPO FOAuth.Attach

// distruttore
FreeAndNil(FAI);
FreeAndNil(FOAuth);
// destroyObjects: DOPO gli handler, PRIMA del token manager
FreeAndNil(FAuthService);
```

Router, provider OAuth e connettore, una volta nella catena, sono del web server: non vanno liberati dall'applicazione. `FTokenManager` e `FAuthService` restano dell'applicazione e devono sopravvivere al web server.

## 4. `Routes.xml`

Un'area `ai` con la porta MCP (il percorso `URI` è libero, non sotto `/oauth` né `/.well-known`):

```xml
<FunctionalArea Code="ai" Description="Agente AI (MCP)" App="cmms">
  <Endpoint Function="Mcp" URI="/mcp" Method="POST" Kind="AIConnector" OpType="Uso" RequiresAuth="true"
            Description="Canale MCP per agenti AI: l'agente opera con i permessi dell'utente che lo ha autorizzato"/>
</FunctionalArea>
```

Il permesso `ai.Uso` compare da solo nella scheda Permessi dei ruoli: solo gli utenti il cui ruolo lo ha possono collegare un agente (l'amministratore sempre).

Un attributo `Description` sugli endpoint che l'agente deve poter scegliere: è il testo che l'AI legge. Con `ExposeUndescribed=0` diventano strumenti solo le funzioni che la loro classe descrive con `DescribeAIFunction`. La `Description` non decide l'esposizione, ma e' cio' che l'AI legge per scegliere lo strumento: senza, il connettore ne inventa una generica ("Area - Funzione") e lo scrive nel log.

## 5. File INI

```ini
[OAuth]
Enabled=1
PublicBaseURL=https://cmms.idrotherm2000.com     ; solo schema, host e porta
ServerTitle=IdroWorld CMMS

[AIConnector]
Enabled=1
ServerName=IdroWorld CMMS
; ExposeUndescribed=0    ; 1 = ogni endpoint Standard autenticato e' uno strumento (decide il ruolo)
```

`PublicBaseURL` e le durate dei token stanno in `[OAuth]`, non in `[AIConnector]`. Per mettere il registro delle chiamate: `FAI.Connector.OnToolCalled := @...` dopo `Attach`.

## 6. Dopo l'avvio

- Nel log: `OAuth active: ...` e `AI connector active: N tool(s), MCP path /mcp`. Se non compare, il log dice perché (elenco completo dei problemi).
- Dare `Agente AI (MCP) > Uso` ai ruoli che possono usarlo.
- Il token dell'agente dura 15 minuti; un cambio di ruolo o una disattivazione si vede al rinnovo successivo.
- `uTokenManager` corretto: con `[TokenManager] DBFile` impostato, provare un riavvio con `Esempi/Ex05_TokenPersistenti`.
