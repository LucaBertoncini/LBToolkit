# Schema: ciclo di vita di un `TRouteHandlerBase` e dei suoi worker

> Da riallegare insieme al resto della documentazione della migrazione.
> Chiarisce cosa succede QUANDO, distinguendo nettamente due momenti che
> è facile confondere: la costruzione/registrazione (avvio
> dell'applicazione, una sola volta) e la gestione di una richiesta HTTP
> (molte volte, in modo concorrente).

---

## MOMENTO 1 — Avvio dell'applicazione (una sola volta, sequenziale)

```
uCMMS_WS_Application.startingWebServer
│
│  1) Costruisce l'oggetto handler di dominio
│     ────────────────────────────────────────
├─► FAuth := TCMMSHandlersAuth.Create(aDataAccess, aTokenManager)
│         │
│         │  TCMMSHandlersAuth eredita da TRouteHandlerBase.
│         │  Il SUO costruttore (uCMMS_HandlersAuth.pas) fa due cose:
│         │
│         ├─► inherited Create(cAreaAuth)          // TRouteHandlerBase.Create
│         │        │
│         │        └─► FAreaCode := 'auth'
│         │            FWorkers := TWorkerEntryList.Create(True)  // lista VUOTA
│         │
│         └─► Self.RegisterWorker('HandleLogin',
│                 TCMMSLoginEndpoint.Create(aDataAccess, aTokenManager))
│             Self.RegisterWorker('HandleLogout',
│                 TCMMSLogoutEndpoint.Create(aTokenManager))
│             Self.RegisterWorker('HandlePasswordResetRequest',
│                 TCMMSFunctionEndpoint.Create(@Self.DoRequestPasswordReset))
│             Self.RegisterWorker('HandlePasswordResetConfirm',
│                 TCMMSFunctionEndpoint.Create(@Self.DoConfirmPasswordReset))
│
│             ↑ QUI vengono create le 4 istanze worker - UNA VOLTA SOLA,
│               qui, non ad ogni richiesta. RegisterWorker (ereditato da
│               TRouteHandlerBase) si limita ad aggiungerle a FWorkers:
│
│               FWorkers = [ ('HandleLogin',  <istanza TCMMSLoginEndpoint>),
│                            ('HandleLogout', <istanza TCMMSLogoutEndpoint>),
│                            ('HandlePasswordResetRequest', <istanza TCMMSFunctionEndpoint#1>),
│                            ('HandlePasswordResetConfirm', <istanza TCMMSFunctionEndpoint#2>) ]
│
│     A questo punto FAuth esiste, con la propria FWorkers popolata, ma
│     NON è ancora collegato al modulo di routing generico: è solo un
│     oggetto Pascal isolato, che nessuno ha ancora interrogato.
│
│
│  2) Registra l'oggetto handler nel modulo di routing generico
│     ────────────────────────────────────────────────────────
├─► _NewRouter.RegisterHandler(FAuth)
│         │
│         │  TWebRouteModule.RegisterHandler(aHandler: IRouteHandler)
│         └─► Result := aHandler.RegisterToRoute(FRegistrar)
│                   │
│                   │  FAuth eredita RegisterToRoute da TRouteHandlerBase:
│                   └─► Result := aRegistrar.BindFunctionalArea(FAreaCode, Self)
│                             │                                  ('auth', FAuth)
│                             │
│                             │  aRegistrar è un TRouteRegistrarAdapter,
│                             │  che incapsula TRouteRegistry:
│                             └─► FOwner.RegisterHandler('auth', FAuth)
│                                       │
│                                       │  TRouteRegistry.RegisterHandler:
│                                       │  scorre TUTTI gli <Endpoint>
│                                       │  dichiarati in Routes.xml per
│                                       │  l'area "auth" (letti in
│                                       │  precedenza da LoadRoutesFromXML)
│                                       │
│                                       │  Per OGNI endpoint dichiarato
│                                       │  nell'area "auth" (qui: 4 righe,
│                                       │  una per Function):
│                                       └─► _Entry := TDispatchEntry.Create(
│                                               FAuth,        ← l'handler
│                                               'auth',
│                                               'HandleLogin', ← FunctionName
│                                               '',            ← OpType (vuoto: pubblico)
│                                               False,         ← RequiresAuth
│                                               ekAuth)        ← Kind
│                                                 │
│                                                 │  TDispatchEntry.Create:
│                                                 └─► _WorkerRef :=
│                                                       aHandler.GetWorker('HandleLogin')
│                                                             │
│                                                             │  FAuth.GetWorker
│                                                             │  (ereditato da
│                                                             │  TRouteHandlerBase):
│                                                             │  cerca 'HandleLogin'
│                                                             │  DENTRO FWorkers
│                                                             │  (la lista già
│                                                             │  popolata al
│                                                             │  passo 1) e
│                                                             │  restituisce
│                                                             │  L'ISTANZA GIÀ
│                                                             │  ESISTENTE
│                                                             │  (TCMMSLoginEndpoint),
│                                                             │  MAI una nuova.
│                                                             │
│                                                     poi QueryInterface
│                                                     una volta sola:
│                                                     _WorkerRef -> IAuthEndpoint
│                                                     (riuscito: TCMMSLoginEndpoint
│                                                      implementa IAuthEndpoint)
│                                                             │
│                                                     _Entry.AuthWorker :=
│                                                       <quella stessa istanza>
│                                       │
│                                       │  _Entry (TDispatchEntry) viene poi
│                                       │  inserito in FDispatchTable, con
│                                       │  chiave "POST:/api/login"
│                                       │
│                                       │  ... e lo stesso, per le altre 3
│                                       │  righe dichiarate nell'area "auth"
│                                       │  (HandleLogout, HandlePasswordReset*)
│
│  3) Validazione finale (dopo TUTTI i RegisterHandler di TUTTE le aree)
│     ───────────────────────────────────────────────────────────────
└─► _NewRouter.ValidateRoutes()
          │
          └─► Per ogni <Endpoint> dichiarato in Routes.xml: verifica che
              esista una entry in FDispatchTable con quella chiave
              Metodo:URI. Se manca, logga un avviso (endpoint dichiarato
              ma mai collegato ad alcun worker).
```

**Cosa esiste, a questo punto, nella memoria del processo:**

```
FDispatchTable (dentro TRouteRegistry, dentro _NewRouter)
 ├─ "POST:/api/login"                    → TDispatchEntry { Kind=ekAuth, AuthWorker=<TCMMSLoginEndpoint#1> }
 ├─ "POST:/api/logout"                   → TDispatchEntry { Kind=ekAuth, AuthWorker=<TCMMSLogoutEndpoint#1> }
 ├─ "POST:/api/password-reset/request"   → TDispatchEntry { Kind=ekStandard, NativeWorker=<TCMMSFunctionEndpoint#1> }
 ├─ "POST:/api/password-reset/confirm"   → TDispatchEntry { Kind=ekStandard, NativeWorker=<TCMMSFunctionEndpoint#2> }
 ├─ "POST:/api/work-groups/list"         → TDispatchEntry { ... NativeWorker=<TCMMSFunctionEndpoint#3> }
 └─ ... (una entry per ogni endpoint di ogni area già migrata)
```

Ogni istanza worker (`TCMMSLoginEndpoint#1`, ecc.) è stata creata **esattamente una volta** e resta **la stessa, per sempre**, per tutta la vita del processo — non un'istanza per richiesta.

---

## MOMENTO 2 — Gestione di una richiesta HTTP (molte volte, concorrente)

```
Arriva una richiesta:  POST /api/login  {"username":"...", "password":"..."}
(il web server la assegna a un THREAD - un thread diverso per ogni
connessione HTTP attiva contemporaneamente)

TWebRouteModule.DoProcessRequest (eseguito DENTRO quel thread)
│
├─► FRoutes.Resolve('POST', '/api/login', ...)
│         │
│         └─► Cerca "POST:/api/login" in FDispatchTable (GIÀ POPOLATA
│             al Momento 1: nessuna creazione, nessuna chiamata a
│             GetWorker qui - solo una LETTURA della entry già presente)
│             │
│             └─► Restituisce: Kind=ekAuth, AuthWorker=<TCMMSLoginEndpoint#1>
│                                                        (la STESSA istanza
│                                                         creata all'avvio)
│
├─► (verifica RequiresAuth, Area.OpType - qui entrambi "vuoti"/false per
│    login, quindi nessuna verifica bloccante)
│
├─► _Kind = ekAuth  →  chiama:
│     AuthWorker.ExecuteAuth(aUserId, aUserConfig, aRequestData,
│                            HTTPParser, ResponseHeaders, out aStatusCode)
│         │
│         │  Questa chiamata esegue DENTRO IL THREAD CORRENTE, sulla
│         │  STESSA istanza TCMMSLoginEndpoint#1 che ogni altro thread
│         │  concorrente sta eventualmente usando nello stesso istante
│         │  per un'ALTRA richiesta di login.
│         │
│         │  È SICURO perché TCMMSLoginEndpoint.ExecuteAuth:
│         │    - legge FDataAccess/FTokenManager (campi immutabili dal
│         │      momento della costruzione - MAI riscritti da ExecuteAuth)
│         │    - ogni altro dato (_User, _Psw, _RowJSON, _Config, ...) è
│         │      una VARIABILE LOCALE del metodo: vive sullo STACK di
│         │      QUESTO thread, in un'area di memoria che nessun altro
│         │      thread può vedere o toccare
│         │
│         └─► Restituisce un NUOVO TJSONObject (creato qui, per questa
│             sola richiesta) con l'esito del login
│
└─► La risposta viene scritta su ResponseData/ResponseCode - variabili
    locali di DoProcessRequest, quindi anch'esse sul PROPRIO stack di
    questo thread - e inviata al client.
```

**Nello stesso istante, un thread parallelo** (un'altra richiesta di
login, o su un'altra Function) esegue lo stesso percorso: se è
un'ALTRA richiesta di login, invoca lo STESSO `TCMMSLoginEndpoint#1`, ma
con il PROPRIO set di variabili locali (`_User`, `_Psw`, ecc. — nomi
uguali nel codice sorgente, ma celle di memoria fisicamente diverse, una
per ciascun thread): nessuna delle due richieste può leggere o scrivere
le variabili locali dell'altra.

---

## Riepilogo: le due domande da porsi per ogni nuovo worker

1. **"Quante volte viene costruita questa istanza?"** → Sempre una sola
   volta, al Momento 1 (dentro `RegisterWorker`, chiamata dal
   costruttore dell'handler di dominio). Mai al Momento 2.

2. **"Che cosa può scrivere questa istanza, quando risponde a una
   richiesta?"** → Solo variabili locali del metodo (`ExecuteAuth`/
   `BuildResponse`/`ResolveDownload`). MAI un proprio campo (`Fxxx`),
   perché quel campo sarebbe condiviso — e quindi potenzialmente
   sovrascritto — da ogni altra richiesta concorrente sulla stessa
   Function, per tutta la vita del processo.

Si veda anche `PRINCIPIO_thread_safety_worker.md` per il criterio
generale e la verifica sui worker già scritti.
