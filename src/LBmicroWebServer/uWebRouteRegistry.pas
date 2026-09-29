unit uWebRouteRegistry;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// uWebRouteRegistry.pas
//
// Modulo di routing REST dichiarativo per TLBmicroWebServer.
//
// NOMI DEI TIPI: NESSUN RIFERIMENTO A UN APPLICATIVO SPECIFICO
// ----------------------------------------------------------------------
// Questo modulo non conosce alcun dominio applicativo: serve
// indifferentemente la manutenzione, il magazzino o qualunque altro
// applicativo venga costruito sulla stessa piattaforma. I tipi che
// espone portano percio' nomi generici (TStandardWorker,
// TFileDownloadWorker, TAuthWorker, TUploadWorker), non legati
// all'applicativo per cui il modulo fu scritto la prima volta.
//
// TRE LIVELLI CONCETTUALI DI OGNI ENDPOINT DICHIARATO NELL'XML
// ----------------------------------------------------------------------
// Ogni endpoint dichiarato nel file XML delle rotte e' descritto da tre
// informazioni concettualmente distinte:
//   1) AREA (FunctionalArea.Code) - l'ambito funzionale a cui l'endpoint
//      appartiene, primo termine della stringa "Area.OpType" verificata
//      ad ogni richiesta.
//   2) FUNCTION (Endpoint.Function) - il nome, univoco all'interno della
//      propria area, che identifica QUALE CODICE deve rispondere.
//   3) OPTYPE (Endpoint.OpType, facoltativo) - il TIPO di operazione
//      autorizzativa (una parola intera arbitraria, dichiarata
//      liberamente nell'XML). Facoltativo: un endpoint senza OpType
//      richiede solo autenticazione (o nessuna, se RequiresAuth="false").
//
// APPLICAZIONI DELLA PIATTAFORMA: L'ATTRIBUTO App SULL'AREA
// ----------------------------------------------------------------------
// Un unico server puo' ospitare piu' applicativi distinti (es. la
// manutenzione e il magazzino), che condividono autenticazione, utenti,
// ruoli e matrice dei permessi, ma presentano interfacce e aree
// funzionali proprie. Il blocco <Applications> in testa al file XML
// dichiara quali applicazioni esistono; l'attributo App di ciascuna
// <FunctionalArea> indica a quale di esse l'area appartiene.
//
// DUE DOMANDE INDIPENDENTI, DUE ATTRIBUTI DISTINTI
// ----------------------------------------------------------------------
// L'attributo App e l'attributo OpType rispondono a domande diverse e non
// vanno confusi:
//     App    -> "di quale applicazione fa parte quest'area?"
//     OpType -> "serve un permesso specifico per usarla?"
// Ne discendono quattro combinazioni, tutte legittime:
//
//   App presente, OpType presente  Area di un'applicazione, protetta da
//                                  permesso (es. le missioni del
//                                  magazzino).
//   App presente, OpType assente   Area di un'applicazione, liberamente
//                                  accessibile a chiunque usi quella
//                                  applicazione (es. la ricerca).
//   App assente,  OpType presente  Area TRASVERSALE, protetta da permesso
//                                  (es. la gestione di utenti e ruoli).
//   App assente,  OpType assente   Area trasversale e libera (es.
//                                  l'autenticazione, i lookup generici).
//
// UN'AREA SENZA App E' TRASVERSALE: appartiene a tutte le applicazioni e
// resta raggiungibile da ciascuna di esse. Questo NON ne allenta in alcun
// modo l'autorizzazione: se dichiara un OpType, quel permesso resta
// necessario esattamente come per qualunque altra area. L'assenza di App
// significa soltanto che l'area non concorre a stabilire a quali
// applicazioni un utente abbia accesso.
//
// COME SI DERIVANO LE APPLICAZIONI ACCESSIBILI A UN UTENTE
// ----------------------------------------------------------------------
// L'elenco delle applicazioni di un utente NON e' un dato memorizzato: e'
// ricavato dai permessi che gia' possiede, cosi' che non esista un
// secondo elenco da tenere allineato a mano (e che quindi non possa
// divergere). La regola, applicata da GetApplicationsForPermissions, e':
//
//     Un utente accede a un'applicazione se esiste ALMENO UN'AREA con
//     quell'App a cui puo' accedere: o perche' l'area non richiede alcun
//     OpType, o perche' possiede almeno un permesso su quell'area.
//
// Ne discende che un'area dichiarata con App ma senza OpType e'
// sufficiente, da sola, ad abilitare quell'applicazione a qualunque
// utente autenticato - ed e' corretto che sia cosi': dichiarare un'area
// come "di questa applicazione e senza permessi richiesti" significa
// letteralmente "chiunque usi questa applicazione puo' usarla", e sarebbe
// contraddittorio che poi non bastasse ad ESSERE un utente di quella
// applicazione. L'OpType restringe COSA si puo' fare, non SE
// l'applicazione riguardi o meno quell'utente.
//
// Un utente amministratore di sistema accede sempre a tutte le
// applicazioni dichiarate, senza alcuna verifica.
//
// QUESTO CALCOLO NON E' UN CONTROLLO DI SICUREZZA: serve a decidere cosa
// mostrare dopo l'accesso (reindirizzamento diretto se l'applicazione e'
// una sola, selettore se sono piu' d'una). L'autorizzazione vera resta
// interamente in hasPermission, applicata ad OGNI singola richiesta,
// indipendentemente dalla pagina da cui proviene.
//
// CINQUE FAMIGLIE DI ENDPOINT
// ----------------------------------------------------------------------
// Quattro famiglie sono servite da un METODO di un oggetto di dominio
// gia' esistente (una funzione "of object" - si veda la nota "PERCHE' UN
// METODO, NON UN OGGETTO CHE IMPLEMENTA UN'INTERFACCIA" piu' sotto).
// Ciascuna ha una propria firma di metodo, dichiarata come tipo
// "of object":
//   - TStandardWorker  : scambia dati in formato JSON (Kind="Standard",
//     il default se l'attributo Kind e' assente);
//   - TFileDownloadWorker : indica quale file del filesystem inviare
//     (Kind="FileDownload" - si veda TWebRouteModule.StreamFileFromDisk);
//   - TAuthWorker : ha accesso diretto alla richiesta HTTP grezza e
//     alle intestazioni della risposta in costruzione (Kind="Auth"),
//     riservato ai soli casi che ne hanno davvero bisogno: tipicamente
//     l'accesso, che scrive l'intestazione di impostazione del cookie, e
//     la disconnessione, che legge il cookie di sessione da rimuovere;
//   - TUploadWorker : riceve un file gia' scritto su disco dal livello
//     di trasporto (Kind="Upload"). Il corpo binario dell'upload e' gia'
//     stato consumato e salvato in un file temporaneo dal server HTTP
//     (THTTPRequestManager, uLBmicroWebServer.pas) PRIMA che la richiesta
//     raggiunga il routing: l'endpoint riceve quindi la richiesta HTTP
//     grezza (per leggere i metadati dagli header e il percorso del file
//     temporaneo) e scrive direttamente la risposta, senza alcun corpo
//     JSON da interpretare. Autenticazione e verifica di permesso avvengono
//     come per ogni altra famiglia, in base a quanto dichiarato nell'XML.
//
// La quinta famiglia, Kind="Proxy", non ha invece alcun metodo e quindi
// alcuna firma: si veda la nota dedicata qui sotto.
//
// LA FAMIGLIA Kind="Proxy": INTERAMENTE DICHIARATIVA
// ----------------------------------------------------------------------
// Un endpoint Kind="Proxy" non esegue alcun codice applicativo locale: la
// richiesta viene firmata e inoltrata a un server remoto, e la sua
// risposta restituita al chiamante. Tutto cio' che serve per farlo e'
// gia' dichiarato altrove:
//   - l'attributo Target dell'endpoint nomina simbolicamente il bersaglio
//     remoto (es. Target="wms_bridge");
//   - quel nome simbolico risolve in indirizzo, chiave condivisa e timeout
//     leggendo il catalogo dei bersagli (uLBRemoteTargets.pas), popolato
//     dal file di configurazione dell'applicazione.
// Non esiste quindi alcun metodo da registrare, e di conseguenza nessuna
// classe di dominio: un'area funzionale composta da soli endpoint proxy
// non richiede alcun codice Pascal. Le sue voci entrano nella tabella di
// dispatch tramite RegisterProxyEndpoints, non tramite RegisterHandler.
//
// AUTENTICAZIONE E PERMESSI SONO APPLICATI COME PER OGNI ALTRA FAMIGLIA:
// il fatto che l'elaborazione avvenga su un altro server non cambia in
// alcun modo chi puo' chiamare l'endpoint. RequiresAuth e OpType
// dichiarati nell'XML valgono esattamente come altrove, e la verifica
// avviene PRIMA che qualunque cosa venga inoltrata.
//
// LA CHIAVE SEGRETA NON COMPARE MAI NELL'XML: l'XML porta solo il nome
// simbolico del bersaglio. La chiave vive nel file di configurazione
// dell'applicazione, gia' escluso dal controllo di versione perche'
// contiene le credenziali del database.
//
// PERCHE' UN METODO, NON UN OGGETTO CHE IMPLEMENTA UN'INTERFACCIA
// ----------------------------------------------------------------------
// Una CLASSE invita naturalmente ad aggiungerle campi, e nulla nella
// forma "classe che implementa un'interfaccia" segnala a chi la scrive
// che si tratta di un singleton condiviso da richieste concorrenti (si
// veda il capitolo sulla sicurezza a filo nel documento di
// progettazione) - un campo aggiunto per comodita', per passarsi un dato
// fra due metodi privati della stessa classe, introdurrebbe
// silenziosamente un problema di concorrenza. Un METODO, al contrario,
// guida naturalmente verso variabili locali: non esiste "un posto
// comodo" dove infilare per errore un dato specifico di una singola
// richiesta. Questo modulo non persegue nemmeno l'obiettivo di
// endpoint scritti in un linguaggio diverso da Pascal, oltre il confine
// di un processo: nessun contratto a soli PAnsiChar esiste quindi in
// questa unit.
//
// COME UN METODO VIENE MEMORIZZATO IN FORMA GENERICA: TMethod
// ----------------------------------------------------------------------
// Un puntatore a metodo Pascal ("of object") non e' un singolo indirizzo
// di codice: e' internamente una coppia (l'indirizzo del codice,
// l'indirizzo dell'istanza a cui il codice si applica - il "Self" che
// il metodo usera'). Questa coppia ha SEMPRE la stessa forma binaria,
// qualunque sia la firma specifica del metodo (numero e tipo dei
// parametri, tipo del risultato): il linguaggio la espone esplicitamente
// tramite il tipo TMethod (Code: Pointer; Data: Pointer), gia' presente
// nella libreria standard. Questo permette di convertire, con un
// semplice cast, un valore di uno dei tipi "of object" specifici
// sopra in un TMethod generico per la memorizzazione nella tabella di
// dispatch, e di riconvertirlo nel tipo esatto al momento dell'esecuzione
// - senza alcuna verifica a runtime della "forma" del metodo, perche' i
// bit memorizzati sono letteralmente gli stessi. La sicurezza di questa
// conversione e' garantita dalla DISCIPLINA con cui il metodo viene
// registrato: si veda la nota su RegisterStandardWorker/
// RegisterFileDownloadWorker/RegisterAuthWorker/RegisterUploadWorker sotto.
//
// REGISTRAZIONE DI UN'AREA FUNZIONALE: NESSUNA INDIRECTION A INTERFACCIA
// ----------------------------------------------------------------------
// Un'area funzionale (una classe che deriva da TRouteHandlerBase) si
// registra su TWebRouteModule passando SE STESSA - un riferimento
// diretto alla propria istanza, di tipo classe concreto - non
// un'interfaccia: TWebRouteModule.RegisterHandler(aHandler:
// TRouteHandlerBase) chiama direttamente TRouteRegistry.RegisterHandler
// (aHandler.AreaCode, aHandler), senza alcun livello di indirizzamento
// intermedio (nessun IRouteRegistrar, nessun adapter). Per questo stesso
// motivo, TRouteHandlerBase e' oggi una classe TObject ordinaria (non
// TInterfacedObject): nessun conteggio di riferimenti e' necessario,
// perche' ogni istanza di area funzionale e' costruita una sola volta,
// all'avvio, e resta viva per l'intera durata del processo tramite un
// campo di tipo classe concreto nell'applicazione che la possiede - non
// un'interfaccia che ne prolunghi artificialmente la vita.
// CONSEGUENZA PRATICA: chi costruisce un'istanza di un'area funzionale
// (l'applicazione ospitante) ne resta proprietario e deve liberarla
// esplicitamente (FreeAndNil) quando non serve piu': nessuna
// liberazione automatica, a differenza di quanto un conteggio di
// riferimenti avrebbe fornito.
//
// CODICE DI STATO HTTP RESTITUITO DA UN ENDPOINT
// ----------------------------------------------------------------------
// Ogni firma di metodo espone un parametro "out aStatusCode: Integer": un
// endpoint che non ha nulla di speciale da segnalare puo' ignorarlo (il
// chiamante lo inizializza gia' a HTTP_STATUS_OK prima di invocare il
// metodo).
//
// COSTRUZIONE DELLA TABELLA DI DISPATCH: NESSUN ENDPOINT E' MAI "FINTO"
// ----------------------------------------------------------------------
// Le aree funzionali sono dichiarate in un file XML esterno. Ogni
// endpoint dichiarato deve avere un metodo REALE registrato per la
// propria Function: TRouteRegistry.ValidateAllEndpointsRegistered
// segnala un avviso per qualunque endpoint privo di un metodo
// registrato, e un avviso separato se il Kind dichiarato nell'XML non
// coincide con la famiglia con cui quella Function e' stata registrata
// (si veda TDispatchEntry.Create). La stessa procedura verifica inoltre
// la coerenza delle dichiarazioni di applicazione - si veda
// ValidateApplications.
// Gli endpoint Kind="Proxy" fanno eccezione alla regola del metodo,
// perche' non ne hanno alcuno da registrare: entrano comunque nella
// tabella di dispatch tramite RegisterProxyEndpoints, quindi la verifica
// li trova gia' presenti e non segnala nulla - a condizione che
// RegisterProxyEndpoints venga invocato PRIMA di ValidateRoutes.
//
// Nessun identificatore di record compare mai nell'URI di un endpoint
// Kind="Standard": ogni URI e' fissa, un eventuale identificatore viaggia
// dentro il corpo JSON. Per Kind="FileDownload" questo vincolo si applica
// al PATH: un identificatore nella QUERY STRING e' ammesso.
//
// DUE MODI DI REGISTRARE GLI HANDLER: AUTOMATICO O ESPLICITO
// ----------------------------------------------------------------------
// LoadRoutesFromXML si limita a leggere e interpretare il file XML: non
// collega piu' automaticamente ne' gli handler globali ne' effettua la
// validazione finale - si vedano RegisterAllGlobalHandlers e
// ValidateRoutes, entrambi da invocare ESPLICITAMENTE dall'applicazione:
//   (a) REGISTRAZIONE GLOBALE (RegisterRouteHandler, da una sezione
//       "initialization"): comoda quando gli handler non richiedono
//       dipendenze esterne. Gli oggetti registrati con questo idioma
//       sono POSSEDUTI dalla lista globale di questa unit (si veda
//       GlobalRouteHandlers), liberati automaticamente in finalization -
//       nessun conteggio di riferimenti a farlo, ora che TRouteHandlerBase
//       e' un TObject ordinario.
//   (b) REGISTRAZIONE ESPLICITA (TWebRouteModule.RegisterHandler,
//       chiamata dopo aver costruito un handler con le proprie
//       dipendenze, es. un accesso ai dati iniettato): necessaria
//       quando le dipendenze vengono fornite dall'esterno invece di
//       essere costruite dall'handler stesso. In questo caso la
//       proprieta' dell'oggetto resta interamente dell'applicazione che
//       lo ha costruito, mai di questo modulo.
// ValidateRoutes va chiamato per ultimo, dopo ogni RegisterHandler e
// dopo RegisterProxyEndpoints.
//
// TWebRouteModule e' un unico TRequestChainProcessor che possiede l'intera
// tabella di dispatch: va aggiunto UNA SOLA VOLTA a TLBmicroWebServer.
// L'autenticazione e' delegata all'applicazione ospitante tramite il
// callback OnAuthenticateRequest. hasPermission e' protected/virtual,
// per permettere a una sottoclasse di ridefinire la strategia di
// autorizzazione.
//
// IL CATALOGO DELLE AREE FUNZIONALI: L'XML E' L'UNICA FONTE DI VERITA'
// ----------------------------------------------------------------------
// GetAllDeclaredAreas restituisce l'elenco COMPLETO delle aree funzionali
// dichiarate in Routes.xml, ciascuna con il proprio codice, la propria
// descrizione (presa dall'attributo Description di <FunctionalArea>),
// l'applicazione di appartenenza (attributo App, stringa vuota se l'area
// e' trasversale) e l'elenco degli OpType realmente dichiarati per
// quell'area - tutto ricavato UNICAMENTE da FDeclaredEndpoints (la
// struttura gia' popolata da LoadFromXMLFile), MAI da una tabella del
// database. L'XML e' l'UNICA fonte di verita' su quali aree esistono:
// aggiungerne, rimuoverne o rinominarne una si riflette immediatamente
// ovunque nel sistema (incluso il catalogo restituito al client per
// costruire la griglia dei permessi), senza alcuna tabella da tenere
// manualmente allineata. Il campo "app" permette alla griglia di
// raggruppare le aree per applicazione, invece di presentarle tutte in un
// unico elenco.
// ORDINAMENTO: sia le aree (per Description) sia gli OpType di ciascuna
// area (alfabetico) sono restituiti gia' ordinati - decisione esplicita
// per rendere piu' facile l'orientamento visivo per chi amministra i
// ruoli, dato che e' la Description (non il Code tecnico) cio' che viene
// mostrato a schermo.
//
// Sicurezza a filo (thread-safety): DoProcessRequest viene invocato
// concorrentemente da un thread per ogni connessione HTTP attiva. La
// tabella di dispatch e' costruita una sola volta all'avvio e viene
// soltanto letta in seguito. Ogni metodo registrato appartiene sempre
// alla STESSA istanza (creata una volta sola durante la costruzione
// dell'oggetto di dominio): una classe di dominio non deve mai
// mantenere, nei propri campi, alcun dato specifico di una singola
// richiesta - solo dipendenze stabili, impostate una volta per tutte al
// momento della costruzione. Ogni dato di una singola richiesta vive
// esclusivamente in variabili locali del metodo che lo elabora.
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Laz2_DOM, fpjson, jsonparser, fgl,
  uLBmicroWebServer, uHTTPRequestParser,
  uLBRemoteTargets, uLBSignedHttpClient, uLBRequestSignature;

const
  cXML_ROOT_NODENAME     = DOMString('WebRoutes');
  cXML_AREA_NODENAME     = DOMString('FunctionalArea');
  cXML_ENDPOINT_NODENAME = DOMString('Endpoint');

  // Blocco che dichiara le applicazioni della piattaforma, e il singolo
  // elemento al suo interno - si veda la nota "APPLICAZIONI DELLA
  // PIATTAFORMA" in testa alla unit.
  cXML_APPS_NODENAME     = DOMString('Applications');
  cXML_APP_NODENAME      = DOMString('Application');

  cXML_ATTR_CODE         = DOMString('Code');
  cXML_ATTR_DESCRIPTION  = DOMString('Description');
  cXML_ATTR_FUNCTION     = DOMString('Function');
  cXML_ATTR_URI          = DOMString('URI');
  cXML_ATTR_METHOD       = DOMString('Method');
  cXML_ATTR_OPTYPE       = DOMString('OpType');
  cXML_ATTR_REQUIRESAUTH = DOMString('RequiresAuth');
  cXML_ATTR_KIND         = DOMString('Kind');
  // Nome simbolico del bersaglio remoto: significativo per i soli
  // endpoint Kind="Proxy", ignorato altrove.
  cXML_ATTR_TARGET       = DOMString('Target');
  // Applicazione di appartenenza dell'area. Assente = area trasversale.
  cXML_ATTR_APP          = DOMString('App');
  // Attributi del singolo <Application>.
  cXML_ATTR_NAME         = DOMString('Name');
  cXML_ATTR_HOME         = DOMString('Home');

  cEndpointKind_Standard     = String('Standard');
  cEndpointKind_FileDownload = String('FileDownload');
  cEndpointKind_Auth         = String('Auth');
  cEndpointKind_Upload       = String('Upload');
  cEndpointKind_Proxy        = String('Proxy');

  cDefaultHTTPMethod     = String('POST');
  cBooleanFalseStr       = String('false');

  cJSONFieldPermissions  = String('Permissions');
  cEmptyJSONObjectText   = String('{}');

  // Nomi dei campi JSON prodotti da GetAllDeclaredApplications e
  // GetApplicationsForPermissions.
  cJSONFieldAppCode      = String('code');
  cJSONFieldAppName      = String('name');
  cJSONFieldAppHome      = String('home');

  // Restituito quando il server remoto di un endpoint Kind="Proxy" non
  // risponde: non e' questo server ad avere un problema, ma quello a cui
  // si e' rivolto.
  cHTTP_STATUS_BAD_GATEWAY = Integer(502);

type
  // ekProxy: l'endpoint non esegue codice applicativo locale, la
  // richiesta viene firmata e inoltrata al server remoto nominato
  // dall'attributo Target - si veda la nota "LA FAMIGLIA Kind=Proxy" in
  // testa alla unit.
  TEndpointKind = (ekStandard, ekFileDownload, ekAuth, ekUpload, ekProxy);

  {------------------------------------------------------------------------
    Le firme di metodo, una per famiglia - si veda la nota "CINQUE
    FAMIGLIE DI ENDPOINT" in testa alla unit. La famiglia Kind="Proxy"
    non compare qui: non ha alcun metodo, e quindi alcuna firma.

    aUserId/aUserConfig: l'utente gia' autenticato (o 0/nil per un
    endpoint pubblico). aRequestData/aUserConfig sono IN PRESTITO: un
    metodo li legge, non li libera mai. Il valore restituito (quando
    previsto) e' una NUOVA istanza, di cui il chiamante prende in carico
    la proprieta'.
  ------------------------------------------------------------------------}
  TStandardWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
    aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData of object;

  TFileDownloadWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
    HTTPParser: THTTPRequestParser; out aFilePath, aSuggestedFileName: String;
    out aStatusCode: Integer): Boolean of object;

  TAuthWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
    aRequestData: TJSONObject; HTTPParser: THTTPRequestParser;
    aResponseHeaders: TStringList; out aStatusCode: Integer): TJSONData of object;

  // Kind="Upload": il file binario e' gia' stato ricevuto e scritto su
  // disco dal livello di trasporto PRIMA del routing (percorso in
  // HTTPParser.UploadedFiles, metadati negli header della richiesta). Il
  // worker ha accesso alla richiesta HTTP grezza e scrive direttamente la
  // risposta, senza alcun corpo JSON da interpretare.
  TUploadWorker = function(aUserId: Integer; aUserConfig: TJSONObject;
    aRequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
    aResponseHeaders: TStringList; var aResponseData: TMemoryStream;
    out aStatusCode: Integer): Boolean of object;

  TAuthenticateRequestEvent = function(RequestHeaders, ResponseHeaders: TStringList;
    out anUserId: Integer; out anUserConfig: TJSONObject): Boolean of object;

  TRetrieveGrantedPermissionsEvent = function(aUserId: Integer;
    aDeclaredPermissions: TJSONArray): TJSONArray of object;

  {------------------------------------------------------------------------
    TApplicationDescriptor

    Una applicazione della piattaforma, dichiarata nel blocco
    <Applications> del file XML. Code e' il valore che le aree
    referenziano tramite il proprio attributo App; Home e' il percorso
    della pagina iniziale, restituito al client dopo l'accesso perche'
    possa reindirizzarvi l'utente.
  ------------------------------------------------------------------------}
  TApplicationDescriptor = class(TObject)
  public
    Code : String;
    Name : String;
    Home : String;
  end;

  TApplicationDescriptorList = specialize TFPGObjectList<TApplicationDescriptor>;

  TEndpointDescriptor = class(TObject)
  public
    AreaCode        : String;
    AreaDescription : String;
    // Applicazione di appartenenza dell'area (attributo App della
    // <FunctionalArea>). Stringa vuota = area trasversale, appartenente a
    // tutte le applicazioni e non concorrente al calcolo di quali siano
    // accessibili a un utente.
    AreaApp         : String;
    FunctionName    : String;
    URI             : String;
    HTTPMethod      : String;
    OpType          : String;
    RequiresAuth    : Boolean;
    Kind            : TEndpointKind;
    // Nome simbolico del bersaglio remoto, significativo per i soli
    // endpoint Kind="Proxy" (vuoto per tutte le altre famiglie).
    Target          : String;
  end;

  TEndpointDescriptorList = specialize TFPGObjectList<TEndpointDescriptor>;

  {------------------------------------------------------------------------
    TWorkerEntry

    Una Function registrata da un'area funzionale: il nome, il metodo (in
    forma generica TMethod) e la famiglia con cui e' stato registrato.
  ------------------------------------------------------------------------}
  TWorkerEntry = class(TObject)
  public
    FunctionName : String;
    TheMethod    : TMethod;
    WorkerKind   : TEndpointKind;

    constructor Create(const aFunctionName: String; aMethod: TMethod; aKind: TEndpointKind);
  end;

  TWorkerEntryList = specialize TFPGObjectList<TWorkerEntry>;

  {------------------------------------------------------------------------
    TRouteHandlerBase

    Classe base di comodo per un'area funzionale concreta (TObject
    ordinario: nessun conteggio di riferimenti, si veda la nota
    "REGISTRAZIONE DI UN'AREA FUNZIONALE" in testa alla unit). Una classe
    derivata registra i propri metodi nel proprio costruttore, chiamando
    UNA delle RegisterXxxWorker sotto - MAI passando un oggetto che
    implementi un'interfaccia, sempre e solo un puntatore a un proprio
    metodo (@Self.NomeMetodo).

    La scelta di QUALE chiamare e' cio' che determina il WorkerKind
    memorizzato in TWorkerEntry: e' una scelta del programmatore,
    verificata poi contro il Kind dichiarato nell'XML solo al momento del
    collegamento (TDispatchEntry.Create).

    Non esiste alcuna RegisterXxxWorker per la famiglia Kind="Proxy":
    quegli endpoint non hanno alcun metodo da registrare.
  ------------------------------------------------------------------------}
  TRouteHandlerBase = class(TObject)
  strict private
    FAreaCode : String;
    FWorkers  : TWorkerEntryList;

  protected
    procedure RegisterStandardWorker(const aFunctionName: String; aMethod: TStandardWorker);
    procedure RegisterFileDownloadWorker(const aFunctionName: String; aMethod: TFileDownloadWorker);
    procedure RegisterAuthWorker(const aFunctionName: String; aMethod: TAuthWorker);
    procedure RegisterUploadWorker(const aFunctionName: String; aMethod: TUploadWorker);

  public
    constructor Create(const anAreaCode: String); reintroduce;
    destructor Destroy; override;

    function GetWorkerMethod(const aFunctionName: String; out aOutKind: TEndpointKind): TMethod; virtual;

    property AreaCode: String read FAreaCode;
  end;

  {------------------------------------------------------------------------
    TDispatchEntry

    Una voce della tabella di dispatch a runtime: la coppia (Metodo,
    Istanza) gia' risolta in TMethod, insieme a tutto cio' che serve per
    la verifica di autorizzazione e per l'esecuzione corretta.
  ------------------------------------------------------------------------}
  TDispatchEntry = class(TObject)
  public
    AreaCode      : String;
    OpType        : String;
    RequiresAuth  : Boolean;
    Kind          : TEndpointKind;
    TheMethod     : TMethod;   // valido solo se KindMatches = True; sempre nullo per ekProxy
    KindMatches   : Boolean;   // False se il Kind dichiarato nell'XML non coincide con quello di registrazione
    Target        : String;    // nome simbolico del bersaglio remoto, valorizzato solo per ekProxy

    constructor Create(aHandler: TRouteHandlerBase; const anAreaCode, aFunctionName, anOpType: String;
      aRequiresAuth: Boolean; aDeclaredKind: TEndpointKind);

    // Costruttore dedicato agli endpoint Kind="Proxy": non riceve alcun
    // handler perche' non esiste alcun metodo da risolvere. KindMatches
    // e' sempre True, dato che non c'e' alcuna registrazione con cui il
    // Kind dichiarato possa risultare discorde.
    constructor CreateProxy(const anAreaCode, anOpType, aTarget: String; aRequiresAuth: Boolean);
  end;

  TDispatchMap = specialize TFPGMap<String, TDispatchEntry>;

  {------------------------------------------------------------------------
    TRouteRegistry

    Possiede l'elenco delle applicazioni dichiarate, quello degli endpoint
    dichiarati nell'XML e la tabella di dispatch a runtime.
  ------------------------------------------------------------------------}
  TRouteRegistry = class(TObject)
  strict private
    FApplications      : TApplicationDescriptorList;
    FDeclaredEndpoints : TEndpointDescriptorList;
    FDispatchTable     : TDispatchMap;

    class function buildKey(const aHTTPMethod, aURI: String): String;

    // Legge il blocco <Applications>. Invocata da LoadFromXMLFile.
    procedure LoadApplications(aRootNode: TDOMNode);

    // Restituisce la descrizione di un'applicazione dichiarata, o nil.
    // Il riferimento e' IN PRESTITO: resta di proprieta' di questa classe.
    function FindApplication(const aCode: String): TApplicationDescriptor;

    // L'area dichiara un OpType in almeno uno dei propri endpoint?
    function AreaRequiresPermission(const aAreaCode: String): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromXMLFile(const aFilename: String): Boolean;
    function RegisterHandler(const aAreaCode: String; aHandler: TRouteHandlerBase): Boolean;

    // Inserisce nella tabella di dispatch tutti gli endpoint dichiarati
    // con Kind="Proxy". Non richiede alcun handler: quegli endpoint sono
    // interamente dichiarativi. Da invocare UNA SOLA VOLTA dopo
    // LoadFromXMLFile. Restituisce quanti endpoint sono stati collegati.
    function RegisterProxyEndpoints(): Integer;

    function Resolve(const aHTTPMethod, aURIResource: String;
      out anAreaCode, anOpType: String; out aRequiresAuth: Boolean;
      out aKind: TEndpointKind; out aMethod: TMethod; out aKindMatches: Boolean;
      out aTarget: String): Boolean;

    procedure ValidateAllEndpointsRegistered;

    // Verifica la coerenza delle dichiarazioni di applicazione: ogni App
    // referenziata da un'area deve corrispondere a un <Application>
    // dichiarato, e tutti gli endpoint di una stessa area devono
    // concordare sul proprio App (che e' una proprieta' dell'AREA, non
    // del singolo endpoint). Invocata da ValidateAllEndpointsRegistered.
    procedure ValidateApplications;

    function GetAllDeclaredPermissions: TJSONArray;
    function GetDeclaredOpTypesForArea(const aAreaCode: String): TJSONArray;
    function GetAllDeclaredAreas: TJSONArray;

    // Elenco completo delle applicazioni dichiarate, ciascuna come
    // oggetto {code, name, home}. Nuova istanza, di cui il chiamante
    // prende in carico la proprieta'.
    function GetAllDeclaredApplications: TJSONArray;

    // Applicazioni accessibili a un utente, derivate dai suoi permessi -
    // si veda la nota "COME SI DERIVANO LE APPLICAZIONI ACCESSIBILI A UN
    // UTENTE" in testa alla unit.
    //   aGrantedPermissions : le stringhe "Area.OpType" gia' concesse
    //                         all'utente (l'array che finisce in
    //                         UserConfig.Permissions). Puo' essere nil o
    //                         vuoto: in quel caso restano accessibili le
    //                         sole applicazioni che dichiarano almeno
    //                         un'area senza OpType.
    //   aIsAdmin            : se True, restituisce tutte le applicazioni
    //                         dichiarate senza alcuna verifica.
    // Nuova istanza, di cui il chiamante prende in carico la proprieta'.
    function GetApplicationsForPermissions(aGrantedPermissions: TJSONArray;
      aIsAdmin: Boolean): TJSONArray;
  end;

  {------------------------------------------------------------------------
    TWebRouteModule

    L'unico TRequestChainProcessor da aggiungere a TLBmicroWebServer.
    Sequenza d'uso: LoadRoutesFromXML -> [RegisterAllGlobalHandlers] ->
    uno o piu' RegisterHandler -> [RegisterProxyEndpoints] ->
    ValidateRoutes (per ultimo).
  ------------------------------------------------------------------------}
  TWebRouteModule = class(TRequestChainProcessor)
  strict private
    FRoutes                       : TRouteRegistry;
    FOnAuthenticateRequest        : TAuthenticateRequestEvent;
    FOnRetrieveGrantedPermissions : TRetrieveGrantedPermissionsEvent;

    // Riferimenti IN PRESTITO, di proprieta' dell'applicazione ospitante:
    // questo modulo non li crea e non li libera. Restano entrambi nil se
    // nessun endpoint Kind="Proxy" e' in uso, e in quel caso il ramo
    // corrispondente di DoProcessRequest non viene mai raggiunto.
    FRemoteTargets                : TLBRemoteTargets;
    FSignedClient                 : TLBSignedHttpClient;

    function ReadRequestBodyAsString(HTTPParser: THTTPRequestParser): String;
    function WriteJSONResponse(aJSONResponse: TJSONData; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream): Boolean;
    function WriteRawJSONResponse(const aJSONText: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream): Boolean;
    procedure WriteErrorResponse(const aMessage: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream);

    // Firma e inoltra la richiesta al bersaglio remoto, e ne restituisce
    // la risposta al chiamante. Invocato dal ramo ekProxy di
    // DoProcessRequest, quando autenticazione e permessi sono gia' stati
    // verificati.
    function DispatchProxyRequest(HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
      const aTargetName: String; aUserId: Integer; aUserConfig: TJSONObject;
      out ResponseCode: Integer): Boolean;

  strict protected
    function DoProcessRequest(
      RequestManager  : THTTPRequestManager;
      HTTPParser      : THTTPRequestParser;
      ResponseHeaders : TStringList;
      var ResponseData: TMemoryStream;
      out ResponseCode: Integer): Boolean; override;

  protected
    function hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean; virtual;
    function StreamFileFromDisk(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
      const aFilePath, aSuggestedFileName: String; out ResponseCode: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadRoutesFromXML(const aFilename: String): Boolean;
    function RegisterHandler(aHandler: TRouteHandlerBase): Boolean;
    procedure RegisterAllGlobalHandlers;

    // Collega gli endpoint Kind="Proxy" e riceve le due dipendenze
    // necessarie per inoltrarli. Da invocare DOPO LoadRoutesFromXML e
    // PRIMA di ValidateRoutes. Entrambi i parametri restano di
    // proprieta' dell'applicazione ospitante, che deve liberarli.
    // Restituisce quanti endpoint proxy sono stati collegati.
    function RegisterProxyEndpoints(aTargets: TLBRemoteTargets;
      aSignedClient: TLBSignedHttpClient): Integer;

    procedure ValidateRoutes;

    function BuildUserPermissions(aUserId: Integer; aUserConfig: TJSONObject): Boolean;
    function GetDeclaredPermissions: TJSONArray;
    function GetDeclaredOpTypesForArea(const aAreaCode: String): TJSONArray;
    function GetAllDeclaredAreas: TJSONArray;

    // Si vedano le omonime su TRouteRegistry.
    function GetAllDeclaredApplications: TJSONArray;
    function GetApplicationsForPermissions(aGrantedPermissions: TJSONArray;
      aIsAdmin: Boolean): TJSONArray;

    property Routes: TRouteRegistry read FRoutes;

    property OnAuthenticateRequest: TAuthenticateRequestEvent
      read FOnAuthenticateRequest write FOnAuthenticateRequest;
    property OnRetrieveGrantedPermissions: TRetrieveGrantedPermissionsEvent
      read FOnRetrieveGrantedPermissions write FOnRetrieveGrantedPermissions;
  end;

function RegisterRouteHandler(aHandler: TRouteHandlerBase): Boolean;


implementation

uses
  uHTTPConsts, uLBFileUtils, ULBLogger;

type
  // Lista globale usata SOLO dall'idioma di auto-registrazione (a) -
  // possiede gli oggetti che vi vengono aggiunti (OwnsObjects=True in
  // RegisterRouteHandler sotto): senza piu' un conteggio di riferimenti
  // a farlo, e' questa lista stessa a liberarli, in finalization.
  TRouteHandlerBaseList = specialize TFPGObjectList<TRouteHandlerBase>;

var
  GlobalRouteHandlers: TRouteHandlerBaseList = nil;

function RegisterRouteHandler(aHandler: TRouteHandlerBase): Boolean;
begin
  Result := False;

  if aHandler <> nil then
  begin
    if GlobalRouteHandlers = nil then
      GlobalRouteHandlers := TRouteHandlerBaseList.Create(True);

    GlobalRouteHandlers.Add(aHandler);
    Result := True;
  end
  else
    LBLogger.Write(1, 'RegisterRouteHandler', lmt_Warning, 'Handler non impostato!');
end;

function JSONObjectToString(aObject: TJSONObject): String;
begin
  if aObject = nil then
    Result := ''
  else begin
    aObject.CompressedJSON := True;
    Result := aObject.AsJSON;
  end;
end;

function JSONDataToString(aData: TJSONData): String;
begin
  if aData = nil then
    Result := ''
  else begin
    aData.CompressedJSON := True;
    Result := aData.AsJSON;
  end;
end;

function StringToJSONObject(const aText: String): TJSONObject;
var
  _Parser: TJSONParser;
  _Data: TJSONData;
begin
  Result := nil;

  if Trim(aText) = '' then
  begin
    Result := TJSONObject.Create;
    Exit;
  end;

  _Parser := TJSONParser.Create(aText);
  try
    _Data := _Parser.Parse;
    if _Data is TJSONObject then
      Result := TJSONObject(_Data)
    else begin
      LBLogger.Write(1, 'StringToJSONObject', lmt_Warning, 'Il testo non e'' un oggetto JSON!');
      if _Data <> nil then
        _Data.Free;
    end;
  finally
    _Parser.Free;
  end;
end;

function BuildContentDispositionValue(const aSuggestedFileName: String): String;
const
  cUnreserved = ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'];
var
  i: Integer;
  _c: Char;
  _b: Byte;
  _Ascii, _Enc: String;
begin
  _Ascii := '';
  _Enc := '';

  for i := 1 to Length(aSuggestedFileName) do
  begin
    _c := aSuggestedFileName[i];

    if (Byte(_c) < 128) and (_c <> '"') and (_c <> '\') then
      _Ascii := _Ascii + _c
    else
      _Ascii := _Ascii + '_';

    _b := Byte(_c);
    if (_b < 128) and (_c in cUnreserved) then
      _Enc := _Enc + _c
    else
      _Enc := _Enc + '%' + IntToHex(_b, 2);
  end;

  if _Ascii = '' then _Ascii := 'download';
  if _Enc   = '' then _Enc   := 'download';

  Result := 'attachment; filename="' + _Ascii + '"; filename*=UTF-8' + #39 + #39 + _Enc;
end;


{ TWorkerEntry }

constructor TWorkerEntry.Create(const aFunctionName: String; aMethod: TMethod; aKind: TEndpointKind);
begin
  inherited Create;
  FunctionName := aFunctionName;
  TheMethod    := aMethod;
  WorkerKind   := aKind;
end;


{ TRouteHandlerBase }

constructor TRouteHandlerBase.Create(const anAreaCode: String);
begin
  inherited Create;
  FAreaCode := anAreaCode;
  FWorkers := TWorkerEntryList.Create(True);
end;

destructor TRouteHandlerBase.Destroy;
begin
  FreeAndNil(FWorkers);
  inherited Destroy;
end;

procedure TRouteHandlerBase.RegisterStandardWorker(const aFunctionName: String; aMethod: TStandardWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterStandardWorker', lmt_Warning,
      'Metodo non impostato per la funzione <%s> nell''area <%s>', [aFunctionName, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(aFunctionName, TMethod(aMethod), ekStandard));
end;

procedure TRouteHandlerBase.RegisterFileDownloadWorker(const aFunctionName: String; aMethod: TFileDownloadWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterFileDownloadWorker', lmt_Warning,
      'Metodo non impostato per la funzione <%s> nell''area <%s>', [aFunctionName, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(aFunctionName, TMethod(aMethod), ekFileDownload));
end;

procedure TRouteHandlerBase.RegisterAuthWorker(const aFunctionName: String; aMethod: TAuthWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterAuthWorker', lmt_Warning,
      'Metodo non impostato per la funzione <%s> nell''area <%s>', [aFunctionName, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(aFunctionName, TMethod(aMethod), ekAuth));
end;

procedure TRouteHandlerBase.RegisterUploadWorker(const aFunctionName: String; aMethod: TUploadWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterUploadWorker', lmt_Warning,
      'Metodo non impostato per la funzione <%s> nell''area <%s>', [aFunctionName, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(aFunctionName, TMethod(aMethod), ekUpload));
end;

function TRouteHandlerBase.GetWorkerMethod(const aFunctionName: String; out aOutKind: TEndpointKind): TMethod;
var
  i: Integer;
begin
  Result.Code := nil;
  Result.Data := nil;
  aOutKind := ekStandard;

  for i := 0 to FWorkers.Count - 1 do
  begin
    if SameText(FWorkers[i].FunctionName, aFunctionName) then
    begin
      Result := FWorkers[i].TheMethod;
      aOutKind := FWorkers[i].WorkerKind;
      Exit;
    end;
  end;

  LBLogger.Write(1, 'TRouteHandlerBase.GetWorkerMethod', lmt_Warning,
    'Nessun metodo registrato per la funzione <%s> nell''area <%s>', [aFunctionName, FAreaCode]);
end;


{ TDispatchEntry }

constructor TDispatchEntry.Create(aHandler: TRouteHandlerBase; const anAreaCode, aFunctionName, anOpType: String;
  aRequiresAuth: Boolean; aDeclaredKind: TEndpointKind);
var
  _RegisteredKind: TEndpointKind;
begin
  inherited Create;

  AreaCode     := anAreaCode;
  OpType       := anOpType;
  RequiresAuth := aRequiresAuth;
  Kind         := aDeclaredKind;
  KindMatches  := False;
  Target       := '';

  TheMethod.Code := nil;
  TheMethod.Data := nil;

  if aHandler = nil then
  begin
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error, 'Handler non impostato per <%s.%s>!',
      [anAreaCode, aFunctionName]);
    Exit;
  end;

  TheMethod := aHandler.GetWorkerMethod(aFunctionName, _RegisteredKind);
  if TheMethod.Code = nil then
  begin
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error, 'GetWorkerMethod ha restituito un metodo nullo per <%s.%s>!',
      [anAreaCode, aFunctionName]);
    Exit;
  end;

  KindMatches := (_RegisteredKind = aDeclaredKind);
  if not KindMatches then
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error,
      'Endpoint <%s.%s>: il Kind dichiarato nell''XML non coincide con la famiglia con cui il metodo e'' stato registrato!',
      [anAreaCode, aFunctionName]);
end;

constructor TDispatchEntry.CreateProxy(const anAreaCode, anOpType, aTarget: String; aRequiresAuth: Boolean);
begin
  inherited Create;

  AreaCode     := anAreaCode;
  OpType       := anOpType;
  RequiresAuth := aRequiresAuth;
  Kind         := ekProxy;
  Target       := aTarget;

  // Nessun metodo da risolvere: un endpoint Kind="Proxy" non esegue
  // codice applicativo locale. KindMatches resta True perche' non esiste
  // alcuna registrazione con cui il Kind dichiarato possa discordare.
  TheMethod.Code := nil;
  TheMethod.Data := nil;
  KindMatches    := True;
end;


{ TRouteRegistry }

constructor TRouteRegistry.Create;
begin
  inherited Create;
  FApplications := TApplicationDescriptorList.Create(True);
  FDeclaredEndpoints := TEndpointDescriptorList.Create(True);
  FDispatchTable := TDispatchMap.Create;
  FDispatchTable.Sorted := True;
end;

destructor TRouteRegistry.Destroy;
var
  i: Integer;
begin
  try
    if FDispatchTable <> nil then
    begin
      for i := 0 to FDispatchTable.Count - 1 do
        FDispatchTable.Data[i].Free;
      FreeAndNil(FDispatchTable);
    end;

    FreeAndNil(FDeclaredEndpoints);
    FreeAndNil(FApplications);
  except
    on E: Exception do
      LBLogger.Write(1, 'TRouteRegistry.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

class function TRouteRegistry.buildKey(const aHTTPMethod, aURI: String): String;
begin
  Result := UpperCase(Trim(aHTTPMethod)) + ':' + Trim(aURI);
end;

function TRouteRegistry.FindApplication(const aCode: String): TApplicationDescriptor;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FApplications.Count - 1 do
  begin
    if SameText(FApplications[i].Code, aCode) then
    begin
      Result := FApplications[i];
      Exit;
    end;
  end;
end;

function TRouteRegistry.AreaRequiresPermission(const aAreaCode: String): Boolean;
var
  i: Integer;
begin
  // Un'area richiede un permesso se ALMENO UNO dei suoi endpoint dichiara
  // un OpType. Se nessuno lo fa, l'area e' liberamente accessibile e
  // basta da sola ad abilitare la propria applicazione - si veda la nota
  // "COME SI DERIVANO LE APPLICAZIONI ACCESSIBILI" in testa alla unit.
  Result := False;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    if SameText(FDeclaredEndpoints[i].AreaCode, aAreaCode) and
       (FDeclaredEndpoints[i].OpType <> '') then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TRouteRegistry.LoadApplications(aRootNode: TDOMNode);
var
  _Node, _AppNode : TDOMNode;
  _AppElement     : TDOMElement;
  _App            : TApplicationDescriptor;
begin
  FApplications.Clear;

  if aRootNode = nil then Exit;

  _Node := aRootNode.FirstChild;
  while _Node <> nil do
  begin
    if (_Node.NodeType = ELEMENT_NODE) and (_Node.NodeName = cXML_APPS_NODENAME) then
    begin
      _AppNode := _Node.FirstChild;

      while _AppNode <> nil do
      begin
        if (_AppNode.NodeType = ELEMENT_NODE) and (_AppNode.NodeName = cXML_APP_NODENAME) then
        begin
          _AppElement := TDOMElement(_AppNode);

          _App := TApplicationDescriptor.Create;
          _App.Code := Trim(_AppElement.GetAttribute(cXML_ATTR_CODE));
          _App.Name := Trim(_AppElement.GetAttribute(cXML_ATTR_NAME));
          _App.Home := Trim(_AppElement.GetAttribute(cXML_ATTR_HOME));

          if _App.Code <> '' then
          begin
            if Self.FindApplication(_App.Code) <> nil then
            begin
              LBLogger.Write(1, 'TRouteRegistry.LoadApplications', lmt_Warning,
                'Applicazione <%s> dichiarata piu'' di una volta: la ripetizione e'' ignorata', [_App.Code]);
              _App.Free;
            end
            else begin
              if _App.Name = '' then
                _App.Name := _App.Code;

              FApplications.Add(_App);

              LBLogger.Write(5, 'TRouteRegistry.LoadApplications', lmt_Debug,
                'Applicazione dichiarata: <%s> (%s), pagina iniziale <%s>', [_App.Code, _App.Name, _App.Home]);
            end;
          end
          else begin
            LBLogger.Write(1, 'TRouteRegistry.LoadApplications', lmt_Warning,
              '<%s> privo dell''attributo %s: ignorato', [String(cXML_APP_NODENAME), String(cXML_ATTR_CODE)]);
            _App.Free;
          end;
        end;

        _AppNode := _AppNode.NextSibling;
      end;
    end;

    _Node := _Node.NextSibling;
  end;
end;

function TRouteRegistry.LoadFromXMLFile(const aFilename: String): Boolean;
var
  _Doc: TXMLDocument = nil;
  _RootNode, _AreaNode, _EndpointNode: TDOMNode;
  _AreaElement, _EndpointElement: TDOMElement;
  _Descriptor: TEndpointDescriptor;
  _AreaCode, _AreaDescription, _AreaApp, _KindStr: String;
begin
  Result := False;
  FDeclaredEndpoints.Clear;
  FApplications.Clear;

  if aFilename = '' then
  begin
    LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning, 'Nome del file non impostato!');
    Exit;
  end;

  if not FileExists(aFilename) then
  begin
    LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning, 'File <%s> non trovato!', [aFilename]);
    Exit;
  end;

  try
    if OpenXMLFile(aFilename, _Doc) then
    begin
      _RootNode := _Doc.DocumentElement;

      if (_RootNode <> nil) and (_RootNode.NodeName = cXML_ROOT_NODENAME) then
      begin
        // Il blocco delle applicazioni viene letto per primo, cosi' che
        // l'elenco sia gia' completo quando le aree vi fanno riferimento.
        Self.LoadApplications(_RootNode);

        _AreaNode := _RootNode.FirstChild;

        while _AreaNode <> nil do
        begin
          if (_AreaNode.NodeType = ELEMENT_NODE) and (_AreaNode.NodeName = cXML_AREA_NODENAME) then
          begin
            _AreaElement := TDOMElement(_AreaNode);
            _AreaCode := Trim(_AreaElement.GetAttribute(cXML_ATTR_CODE));
            _AreaDescription := Trim(_AreaElement.GetAttribute(cXML_ATTR_DESCRIPTION));
            // Assente = area trasversale, appartenente a tutte le
            // applicazioni - si veda la nota in testa alla unit.
            _AreaApp := Trim(_AreaElement.GetAttribute(cXML_ATTR_APP));

            if _AreaCode <> '' then
            begin
              _EndpointNode := _AreaNode.FirstChild;

              while _EndpointNode <> nil do
              begin
                if (_EndpointNode.NodeType = ELEMENT_NODE) and (_EndpointNode.NodeName = cXML_ENDPOINT_NODENAME) then
                begin
                  _EndpointElement := TDOMElement(_EndpointNode);

                  _Descriptor := TEndpointDescriptor.Create;
                  _Descriptor.AreaCode := _AreaCode;
                  _Descriptor.AreaDescription := _AreaDescription;
                  _Descriptor.AreaApp := _AreaApp;
                  _Descriptor.FunctionName := Trim(_EndpointElement.GetAttribute(cXML_ATTR_FUNCTION));
                  _Descriptor.URI := Trim(_EndpointElement.GetAttribute(cXML_ATTR_URI));
                  _Descriptor.HTTPMethod := UpperCase(Trim(_EndpointElement.GetAttribute(cXML_ATTR_METHOD)));
                  if _Descriptor.HTTPMethod = '' then
                    _Descriptor.HTTPMethod := cDefaultHTTPMethod;

                  _Descriptor.OpType := Trim(_EndpointElement.GetAttribute(cXML_ATTR_OPTYPE));
                  _Descriptor.RequiresAuth :=
                    not SameText(Trim(_EndpointElement.GetAttribute(cXML_ATTR_REQUIRESAUTH)), cBooleanFalseStr);

                  // Letto sempre, ma significativo per i soli endpoint
                  // Kind="Proxy": altrove resta semplicemente vuoto.
                  _Descriptor.Target := Trim(_EndpointElement.GetAttribute(cXML_ATTR_TARGET));

                  _KindStr := Trim(_EndpointElement.GetAttribute(cXML_ATTR_KIND));
                  if SameText(_KindStr, cEndpointKind_FileDownload) then
                    _Descriptor.Kind := ekFileDownload
                  else if SameText(_KindStr, cEndpointKind_Auth) then
                    _Descriptor.Kind := ekAuth
                  else if SameText(_KindStr, cEndpointKind_Upload) then
                    _Descriptor.Kind := ekUpload
                  else if SameText(_KindStr, cEndpointKind_Proxy) then
                  begin
                    _Descriptor.Kind := ekProxy;

                    // Un endpoint proxy senza bersaglio non potrebbe fare
                    // nulla: viene segnalato qui, e piu' avanti
                    // RegisterProxyEndpoints lo salta senza collegarlo.
                    if _Descriptor.Target = '' then
                      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Error,
                        'Endpoint <%s.%s> dichiarato Kind="%s" ma privo dell''attributo %s: non sara'' raggiungibile',
                        [_AreaCode, _Descriptor.FunctionName, cEndpointKind_Proxy, String(cXML_ATTR_TARGET)]);
                  end
                  else begin
                    _Descriptor.Kind := ekStandard;
                    if (_KindStr <> '') and (not SameText(_KindStr, cEndpointKind_Standard)) then
                      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
                        'Endpoint <%s.%s>: valore Kind <%s> non riconosciuto, trattato come <%s>',
                        [_AreaCode, _Descriptor.FunctionName, _KindStr, cEndpointKind_Standard]);
                  end;

                  if (_Descriptor.FunctionName <> '') and (_Descriptor.URI <> '') then
                    FDeclaredEndpoints.Add(_Descriptor)
                  else begin
                    LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
                      'Definizione <Endpoint> incompleta nell''area <%s>: ignorata', [_AreaCode]);
                    _Descriptor.Free;
                  end;
                end;

                _EndpointNode := _EndpointNode.NextSibling;
              end;
            end
            else
              LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
                '<FunctionalArea> priva dell''attributo Code: ignorata', []);
          end;

          _AreaNode := _AreaNode.NextSibling;
        end;

        Result := FDeclaredEndpoints.Count > 0;
        if not Result then
          LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
            'Nessun endpoint dichiarato in <%s>', [aFilename]);
      end
      else
        LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
          'Nodo radice <%s> non trovato in <%s>!', [cXML_ROOT_NODENAME, aFilename]);
    end
    else
      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
        'Impossibile interpretare il file XML <%s>', [aFilename]);

  except
    on E: Exception do
      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Error, E.Message);
  end;

  if _Doc <> nil then
    _Doc.Free;
end;

function TRouteRegistry.RegisterHandler(const aAreaCode: String; aHandler: TRouteHandlerBase): Boolean;
var
  i, _Idx: Integer;
  _Descriptor: TEndpointDescriptor;
  _Key: String;
  _Entry: TDispatchEntry;
begin
  Result := False;

  if aHandler = nil then
  begin
    LBLogger.Write(1, 'TRouteRegistry.RegisterHandler', lmt_Warning, 'Handler non impostato!');
    Exit;
  end;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];

    if SameText(_Descriptor.AreaCode, aAreaCode) then
    begin
      // Un endpoint Kind="Proxy" non ha alcun metodo da risolvere: viene
      // collegato da RegisterProxyEndpoints, non da qui. Questo permette
      // a un'area di contenere endpoint di entrambe le nature senza che
      // la registrazione dell'handler produca voci prive di metodo.
      if _Descriptor.Kind = ekProxy then
        Continue;

      _Key := Self.buildKey(_Descriptor.HTTPMethod, _Descriptor.URI);

      _Entry := TDispatchEntry.Create(aHandler, _Descriptor.AreaCode, _Descriptor.FunctionName,
        _Descriptor.OpType, _Descriptor.RequiresAuth, _Descriptor.Kind);

      if FDispatchTable.Find(_Key, _Idx) then
      begin
        LBLogger.Write(1, 'TRouteRegistry.RegisterHandler', lmt_Warning,
          'Rotta <%s> gia'' registrata: handler precedente sovrascritto', [_Key]);
        FDispatchTable.Data[_Idx].Free;
        FDispatchTable.Data[_Idx] := _Entry;
      end
      else
        FDispatchTable.Add(_Key, _Entry);

      Result := True;

      LBLogger.Write(5, 'TRouteRegistry.RegisterHandler', lmt_Debug,
        'Rotta <%s> collegata all''area <%s>, funzione <%s>, OpType <%s>',
        [_Key, _Descriptor.AreaCode, _Descriptor.FunctionName, _Descriptor.OpType]);
    end;
  end;

  if not Result then
    LBLogger.Write(1, 'TRouteRegistry.RegisterHandler', lmt_Warning,
      'Nessun endpoint dichiarato trovato per l''area funzionale <%s>: handler non utilizzato', [aAreaCode]);
end;

function TRouteRegistry.RegisterProxyEndpoints(): Integer;
var
  i, _Idx: Integer;
  _Descriptor: TEndpointDescriptor;
  _Key: String;
  _Entry: TDispatchEntry;
begin
  Result := 0;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];
    if _Descriptor.Kind <> ekProxy then Continue;

    // Bersaglio assente: gia' segnalato in fase di lettura dell'XML. La
    // rotta non viene collegata, cosi' ValidateAllEndpointsRegistered la
    // riportera' fra quelle prive di handler - dove e' giusto che compaia.
    if _Descriptor.Target = '' then Continue;

    _Key := Self.buildKey(_Descriptor.HTTPMethod, _Descriptor.URI);

    _Entry := TDispatchEntry.CreateProxy(_Descriptor.AreaCode, _Descriptor.OpType,
      _Descriptor.Target, _Descriptor.RequiresAuth);

    if FDispatchTable.Find(_Key, _Idx) then
    begin
      LBLogger.Write(1, 'TRouteRegistry.RegisterProxyEndpoints', lmt_Warning,
        'Rotta <%s> gia'' registrata: voce precedente sovrascritta dall''endpoint proxy', [_Key]);
      FDispatchTable.Data[_Idx].Free;
      FDispatchTable.Data[_Idx] := _Entry;
    end
    else
      FDispatchTable.Add(_Key, _Entry);

    Inc(Result);

    LBLogger.Write(5, 'TRouteRegistry.RegisterProxyEndpoints', lmt_Debug,
      'Rotta <%s> inoltrata al bersaglio remoto <%s> (area <%s>, OpType <%s>)',
      [_Key, _Descriptor.Target, _Descriptor.AreaCode, _Descriptor.OpType]);
  end;
end;

function TRouteRegistry.Resolve(const aHTTPMethod, aURIResource: String;
  out anAreaCode, anOpType: String; out aRequiresAuth: Boolean;
  out aKind: TEndpointKind; out aMethod: TMethod; out aKindMatches: Boolean;
  out aTarget: String): Boolean;
var
  _Key: String;
  _Idx: Integer;
  _Entry: TDispatchEntry;
begin
  anAreaCode := '';
  anOpType := '';
  aRequiresAuth := True;
  aKind := ekStandard;
  aMethod.Code := nil;
  aMethod.Data := nil;
  aKindMatches := False;
  aTarget := '';

  _Key := Self.buildKey(aHTTPMethod, aURIResource);
  Result := FDispatchTable.Find(_Key, _Idx);

  if Result then
  begin
    _Entry := FDispatchTable.Data[_Idx];
    anAreaCode := _Entry.AreaCode;
    anOpType := _Entry.OpType;
    aRequiresAuth := _Entry.RequiresAuth;
    aKind := _Entry.Kind;
    aMethod := _Entry.TheMethod;
    aKindMatches := _Entry.KindMatches;
    aTarget := _Entry.Target;
  end;
end;

procedure TRouteRegistry.ValidateApplications;
var
  i, j            : Integer;
  _Descriptor     : TEndpointDescriptor;
  _Other          : TEndpointDescriptor;
  _Checked        : TStringList;
  _UnknownApp     : Integer;
  _Inconsistent   : Integer;
begin
  _UnknownApp := 0;
  _Inconsistent := 0;

  _Checked := TStringList.Create;
  try
    _Checked.Sorted := True;
    _Checked.Duplicates := dupIgnore;

    for i := 0 to FDeclaredEndpoints.Count - 1 do
    begin
      _Descriptor := FDeclaredEndpoints[i];

      // Ogni area viene verificata una sola volta, alla prima occorrenza.
      if _Checked.IndexOf(LowerCase(_Descriptor.AreaCode)) >= 0 then Continue;
      _Checked.Add(LowerCase(_Descriptor.AreaCode));

      // 1) L'App referenziata deve corrispondere a un <Application>
      //    dichiarato. Un valore che non trova riscontro e' quasi sempre
      //    un refuso, e produrrebbe un'area invisibile a ogni
      //    applicazione.
      if (_Descriptor.AreaApp <> '') and (Self.FindApplication(_Descriptor.AreaApp) = nil) then
      begin
        Inc(_UnknownApp);
        LBLogger.Write(1, 'TRouteRegistry.ValidateApplications', lmt_Warning,
          'L''area <%s> dichiara App <%s>, che non corrisponde ad alcuna applicazione dichiarata: l''area non risultera'' accessibile da alcuna applicazione',
          [_Descriptor.AreaCode, _Descriptor.AreaApp]);
      end;

      // 2) Tutti gli endpoint di una stessa area devono concordare
      //    sull'App: e' una proprieta' dell'AREA, non del singolo
      //    endpoint. Una discordanza indica quasi sempre due
      //    <FunctionalArea> con lo stesso Code e App diverso.
      for j := i + 1 to FDeclaredEndpoints.Count - 1 do
      begin
        _Other := FDeclaredEndpoints[j];
        if not SameText(_Other.AreaCode, _Descriptor.AreaCode) then Continue;

        if not SameText(_Other.AreaApp, _Descriptor.AreaApp) then
        begin
          Inc(_Inconsistent);
          LBLogger.Write(1, 'TRouteRegistry.ValidateApplications', lmt_Warning,
            'L''area <%s> risulta dichiarata con App discordanti (<%s> e <%s>): verra'' considerata la prima incontrata',
            [_Descriptor.AreaCode, _Descriptor.AreaApp, _Other.AreaApp]);
          Break;
        end;
      end;
    end;

  finally
    _Checked.Free;
  end;

  if (_UnknownApp = 0) and (_Inconsistent = 0) then
    LBLogger.Write(5, 'TRouteRegistry.ValidateApplications', lmt_Debug,
      'Dichiarazioni di applicazione coerenti: %d applicazione/i, nessuna area con App sconosciuto o discordante',
      [FApplications.Count]);
end;

procedure TRouteRegistry.ValidateAllEndpointsRegistered;
var
  i, _MissingCount, _Idx: Integer;
  _Descriptor: TEndpointDescriptor;
  _Key: String;
begin
  _MissingCount := 0;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];
    _Key := Self.buildKey(_Descriptor.HTTPMethod, _Descriptor.URI);

    if not FDispatchTable.Find(_Key, _Idx) then
    begin
      Inc(_MissingCount);
      LBLogger.Write(1, 'TRouteRegistry.ValidateAllEndpointsRegistered', lmt_Warning,
        'L''endpoint dichiarato <%s> (%s.%s) non ha alcun handler registrato!',
        [_Key, _Descriptor.AreaCode, _Descriptor.FunctionName]);
    end;
  end;

  if _MissingCount > 0 then
    LBLogger.Write(1, 'TRouteRegistry.ValidateAllEndpointsRegistered', lmt_Warning,
      '%d endpoint dichiarato/i senza handler', [_MissingCount])
  else
    LBLogger.Write(5, 'TRouteRegistry.ValidateAllEndpointsRegistered', lmt_Debug,
      'Ogni endpoint dichiarato e'' collegato a un handler', []);

  // Verifica di coerenza delle dichiarazioni di applicazione, eseguita
  // insieme a quella degli handler: entrambe segnalano errori di
  // configurazione che altrimenti si manifesterebbero come comportamenti
  // inattesi molto piu' tardi.
  Self.ValidateApplications;
end;

function TRouteRegistry.GetAllDeclaredPermissions: TJSONArray;
var
  i, j: Integer;
  _Descriptor: TEndpointDescriptor;
  _Candidate: String;
  _AlreadyPresent: Boolean;
begin
  Result := TJSONArray.Create;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];
    if _Descriptor.OpType = '' then
      Continue;

    _Candidate := _Descriptor.AreaCode + '.' + _Descriptor.OpType;

    _AlreadyPresent := False;
    for j := 0 to Result.Count - 1 do
      if Result.Strings[j] = _Candidate then
      begin
        _AlreadyPresent := True;
        Break;
      end;

    if not _AlreadyPresent then
      Result.Add(_Candidate);
  end;
end;

function TRouteRegistry.GetDeclaredOpTypesForArea(const aAreaCode: String): TJSONArray;
var
  i, j: Integer;
  _Descriptor: TEndpointDescriptor;
  _AlreadyPresent: Boolean;
begin
  Result := TJSONArray.Create;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];
    if _Descriptor.OpType = '' then
      Continue;
    if not SameText(_Descriptor.AreaCode, aAreaCode) then
      Continue;

    _AlreadyPresent := False;
    for j := 0 to Result.Count - 1 do
      if Result.Strings[j] = _Descriptor.OpType then
      begin
        _AlreadyPresent := True;
        Break;
      end;

    if not _AlreadyPresent then
      Result.Add(_Descriptor.OpType);
  end;
end;

function TRouteRegistry.GetAllDeclaredApplications: TJSONArray;
var
  i       : Integer;
  _AppObj : TJSONObject;
begin
  Result := TJSONArray.Create;

  for i := 0 to FApplications.Count - 1 do
  begin
    _AppObj := TJSONObject.Create;
    _AppObj.Add(cJSONFieldAppCode, FApplications[i].Code);
    _AppObj.Add(cJSONFieldAppName, FApplications[i].Name);
    _AppObj.Add(cJSONFieldAppHome, FApplications[i].Home);
    Result.Add(_AppObj);
  end;
end;

function TRouteRegistry.GetApplicationsForPermissions(aGrantedPermissions: TJSONArray;
  aIsAdmin: Boolean): TJSONArray;
{
  Applica la regola descritta nella nota "COME SI DERIVANO LE APPLICAZIONI
  ACCESSIBILI A UN UTENTE" in testa alla unit:

      un utente accede a un'applicazione se esiste ALMENO UN'AREA con
      quell'App a cui puo' accedere - o perche' l'area non richiede alcun
      OpType, o perche' possiede almeno un permesso su quell'area.

  Le aree prive di App non entrano mai nel calcolo: sono trasversali, e
  nessun permesso su di esse abilita alcuna applicazione.

  L'ordine del risultato segue quello di dichiarazione in <Applications>,
  non quello in cui le aree compaiono: cosi' l'elenco presentato
  all'utente resta stabile e prevedibile, e l'ordine e' governato da chi
  scrive il file XML.
}
var
  i, j        : Integer;
  _Descriptor : TEndpointDescriptor;
  _App        : TApplicationDescriptor;
  _Granted    : TStringList;
  _Accessible : TStringList;
  _AppObj     : TJSONObject;
  _AreaPrefix : String;
  _HasPerm    : Boolean;
begin
  Result := TJSONArray.Create;

  // Un amministratore di sistema accede sempre a tutto, senza alcuna
  // verifica: stessa convenzione gia' applicata ovunque nel sistema.
  if aIsAdmin then
  begin
    Result.Free;
    Result := Self.GetAllDeclaredApplications;
    Exit;
  end;

  _Granted := TStringList.Create;
  _Accessible := TStringList.Create;
  try
    _Granted.Sorted := True;
    _Granted.Duplicates := dupIgnore;
    _Accessible.Sorted := True;
    _Accessible.Duplicates := dupIgnore;

    // I permessi concessi vengono raccolti in forma ordinata, cosi' la
    // verifica successiva e' una ricerca e non una scansione.
    if aGrantedPermissions <> nil then
    begin
      for i := 0 to aGrantedPermissions.Count - 1 do
        _Granted.Add(LowerCase(aGrantedPermissions.Strings[i]));
    end;

    // Un giro solo su tutti gli endpoint: per ciascuna area con App,
    // si stabilisce se l'utente vi accede e, in caso affermativo, la
    // propria App entra nell'insieme delle applicazioni accessibili.
    for i := 0 to FDeclaredEndpoints.Count - 1 do
    begin
      _Descriptor := FDeclaredEndpoints[i];

      // Area trasversale: non concorre mai al calcolo.
      if _Descriptor.AreaApp = '' then Continue;

      // App gia' risultata accessibile per un'altra area: nulla da
      // aggiungere.
      if _Accessible.IndexOf(LowerCase(_Descriptor.AreaApp)) >= 0 then Continue;

      if not Self.AreaRequiresPermission(_Descriptor.AreaCode) then
      begin
        // L'area non dichiara alcun OpType in nessuno dei propri
        // endpoint: e' liberamente accessibile a chiunque usi quella
        // applicazione, e quindi basta da sola ad abilitarla.
        _Accessible.Add(LowerCase(_Descriptor.AreaApp));
        Continue;
      end;

      // L'area richiede un permesso: serve almeno uno dei permessi
      // concessi che inizi con "<area>.".
      _AreaPrefix := LowerCase(_Descriptor.AreaCode) + '.';
      _HasPerm := False;

      for j := 0 to _Granted.Count - 1 do
      begin
        if Copy(_Granted[j], 1, Length(_AreaPrefix)) = _AreaPrefix then
        begin
          _HasPerm := True;
          Break;
        end;
      end;

      if _HasPerm then
        _Accessible.Add(LowerCase(_Descriptor.AreaApp));
    end;

    // Il risultato segue l'ordine di dichiarazione delle applicazioni.
    for i := 0 to FApplications.Count - 1 do
    begin
      _App := FApplications[i];
      if _Accessible.IndexOf(LowerCase(_App.Code)) < 0 then Continue;

      _AppObj := TJSONObject.Create;
      _AppObj.Add(cJSONFieldAppCode, _App.Code);
      _AppObj.Add(cJSONFieldAppName, _App.Name);
      _AppObj.Add(cJSONFieldAppHome, _App.Home);
      Result.Add(_AppObj);
    end;

  finally
    _Accessible.Free;
    _Granted.Free;
  end;
end;

function TRouteRegistry.GetAllDeclaredAreas: TJSONArray;
{
  Si veda la nota estesa "IL CATALOGO DELLE AREE FUNZIONALI" in testa
  alla unit: l'XML e' l'unica fonte di verita'.

  Passo 1: un giro su FDeclaredEndpoints, che costruisce un oggetto per
  ciascuna area incontrata la PRIMA volta (code+description+app), e
  accumula in esso l'insieme degli OpType distinti dichiarati per
  quell'area.
  Passo 2: gli OpType di ciascuna area vengono ordinati alfabeticamente.
  Passo 3: l'array delle aree stesso viene ordinato per Description.

  Il campo "app" permette a chi presenta la griglia dei permessi di
  raggrupparle per applicazione; e' stringa vuota per le aree
  trasversali.
}
var
  i, j, k        : Integer;
  _Descriptor    : TEndpointDescriptor;
  _AreaObj       : TJSONObject;
  _OpTypesArr    : TJSONArray;
  _FoundIdx      : Integer;
  _AlreadyHasOp  : Boolean;
  _MinIdx        : Integer;
  _MinDescr      : String;
begin
  Result := TJSONArray.Create;

  // ---- Passo 1: raggruppamento per area. ------------------------------
  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];

    _FoundIdx := -1;
    for j := 0 to Result.Count - 1 do
      if SameText(TJSONObject(Result.Objects[j]).Get('code', ''), _Descriptor.AreaCode) then
      begin
        _FoundIdx := j;
        Break;
      end;

    if _FoundIdx = -1 then
    begin
      _AreaObj := TJSONObject.Create;
      _AreaObj.Add('code', _Descriptor.AreaCode);
      _AreaObj.Add('description', _Descriptor.AreaDescription);
      _AreaObj.Add('app', _Descriptor.AreaApp);
      _OpTypesArr := TJSONArray.Create;
      _AreaObj.Add('op_types', _OpTypesArr);
      Result.Add(_AreaObj);
      _FoundIdx := Result.Count - 1;
    end;

    if _Descriptor.OpType <> '' then
    begin
      _AreaObj := TJSONObject(Result.Objects[_FoundIdx]);
      _OpTypesArr := _AreaObj.Arrays['op_types'];

      _AlreadyHasOp := False;
      for k := 0 to _OpTypesArr.Count - 1 do
        if _OpTypesArr.Strings[k] = _Descriptor.OpType then
        begin
          _AlreadyHasOp := True;
          Break;
        end;

      if not _AlreadyHasOp then
        _OpTypesArr.Add(_Descriptor.OpType);
    end;
  end;

  // ---- Passo 2: ordinamento alfabetico degli OpType di ciascuna area. --
  for i := 0 to Result.Count - 1 do
  begin
    _OpTypesArr := TJSONObject(Result.Objects[i]).Arrays['op_types'];
    for j := 0 to _OpTypesArr.Count - 2 do
      for k := 0 to _OpTypesArr.Count - 2 - j do
        if _OpTypesArr.Strings[k] > _OpTypesArr.Strings[k + 1] then
          _OpTypesArr.Exchange(k, k + 1);
  end;

  // ---- Passo 3: ordinamento delle aree per Description. ----------------
  for i := 0 to Result.Count - 2 do
  begin
    _MinIdx := i;
    _MinDescr := TJSONObject(Result.Objects[i]).Get('description', '');

    for j := i + 1 to Result.Count - 1 do
      if TJSONObject(Result.Objects[j]).Get('description', '') < _MinDescr then
      begin
        _MinIdx := j;
        _MinDescr := TJSONObject(Result.Objects[j]).Get('description', '');
      end;

    if _MinIdx <> i then
      Result.Exchange(i, _MinIdx);
  end;
end;


{ TWebRouteModule }

constructor TWebRouteModule.Create;
begin
  inherited Create;
  FRoutes := TRouteRegistry.Create;
  FRemoteTargets := nil;
  FSignedClient := nil;
end;

destructor TWebRouteModule.Destroy;
begin
  // FRemoteTargets e FSignedClient sono riferimenti in prestito, di
  // proprieta' dell'applicazione ospitante: non vanno liberati qui.
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

function TWebRouteModule.LoadRoutesFromXML(const aFilename: String): Boolean;
begin
  Result := FRoutes.LoadFromXMLFile(aFilename);
end;

function TWebRouteModule.RegisterHandler(aHandler: TRouteHandlerBase): Boolean;
begin
  Result := (aHandler <> nil) and FRoutes.RegisterHandler(aHandler.AreaCode, aHandler);
end;

procedure TWebRouteModule.RegisterAllGlobalHandlers;
var
  i: Integer;
begin
  if GlobalRouteHandlers = nil then
  begin
    LBLogger.Write(3, 'TWebRouteModule.RegisterAllGlobalHandlers', lmt_Debug,
      'Nessun handler registrato tramite RegisterRouteHandler (idioma di auto-registrazione globale non utilizzato)', []);
    Exit;
  end;

  for i := 0 to GlobalRouteHandlers.Count - 1 do
  begin
    if not Self.RegisterHandler(GlobalRouteHandlers[i]) then
      LBLogger.Write(1, 'TWebRouteModule.RegisterAllGlobalHandlers', lmt_Warning,
        'L''handler all''indice %d non ha registrato alcuna rotta', [i]);
  end;
end;

function TWebRouteModule.RegisterProxyEndpoints(aTargets: TLBRemoteTargets;
  aSignedClient: TLBSignedHttpClient): Integer;
begin
  FRemoteTargets := aTargets;
  FSignedClient  := aSignedClient;

  Result := FRoutes.RegisterProxyEndpoints();

  if (Result > 0) and ((FRemoteTargets = nil) or (FSignedClient = nil)) then
    LBLogger.Write(1, 'TWebRouteModule.RegisterProxyEndpoints', lmt_Error,
      '%d endpoint Kind="%s" collegati, ma il catalogo dei bersagli o il client firmato non sono impostati: quelle rotte risponderanno sempre con un errore!',
      [Result, cEndpointKind_Proxy]);
end;

procedure TWebRouteModule.ValidateRoutes;
begin
  FRoutes.ValidateAllEndpointsRegistered;
end;

function TWebRouteModule.BuildUserPermissions(aUserId: Integer; aUserConfig: TJSONObject): Boolean;
var
  _Declared, _Granted: TJSONArray;
  _Idx: Integer;
begin
  Result := False;

  if aUserConfig = nil then
  begin
    LBLogger.Write(1, 'TWebRouteModule.BuildUserPermissions', lmt_Warning, 'UserConfig non impostato!');
    Exit;
  end;

  if not Assigned(FOnRetrieveGrantedPermissions) then
  begin
    LBLogger.Write(1, 'TWebRouteModule.BuildUserPermissions', lmt_Warning,
      'Callback OnRetrieveGrantedPermissions non impostato!', []);
    Exit;
  end;

  _Declared := FRoutes.GetAllDeclaredPermissions;
  try
    _Granted := FOnRetrieveGrantedPermissions(aUserId, _Declared);
  finally
    _Declared.Free;
  end;

  if _Granted <> nil then
  begin
    _Idx := aUserConfig.IndexOfName(cJSONFieldPermissions);
    if _Idx >= 0 then
      aUserConfig.Delete(_Idx);

    aUserConfig.Add(cJSONFieldPermissions, _Granted);
    Result := True;
  end
  else
    LBLogger.Write(1, 'TWebRouteModule.BuildUserPermissions', lmt_Warning,
      'OnRetrieveGrantedPermissions ha restituito nil per l''utente %d', [aUserId]);
end;

function TWebRouteModule.GetDeclaredPermissions: TJSONArray;
begin
  Result := FRoutes.GetAllDeclaredPermissions;
end;

function TWebRouteModule.GetDeclaredOpTypesForArea(const aAreaCode: String): TJSONArray;
begin
  Result := FRoutes.GetDeclaredOpTypesForArea(aAreaCode);
end;

function TWebRouteModule.GetAllDeclaredAreas: TJSONArray;
begin
  Result := FRoutes.GetAllDeclaredAreas;
end;

function TWebRouteModule.GetAllDeclaredApplications: TJSONArray;
begin
  Result := FRoutes.GetAllDeclaredApplications;
end;

function TWebRouteModule.GetApplicationsForPermissions(aGrantedPermissions: TJSONArray;
  aIsAdmin: Boolean): TJSONArray;
begin
  Result := FRoutes.GetApplicationsForPermissions(aGrantedPermissions, aIsAdmin);
end;

function TWebRouteModule.hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean;
var
  _Permissions: TJSONArray;
  _Needed: String;
  i: Integer;
begin
  Result := False;

  if aUserConfig = nil then
  begin
    LBLogger.Write(1, 'TWebRouteModule.hasPermission', lmt_Warning, 'UserConfig not set!');
    Exit;
  end;

  _Needed := anAreaCode + '.' + anOpType;

  _Permissions := aUserConfig.Get(cJSONFieldPermissions, TJSONArray(nil));
  if (_Permissions <> nil) then
  begin
    for i := 0 to _Permissions.Count - 1 do
    begin
      if _Permissions.Strings[i] = _Needed then
      begin
        Result := True;
        Break;
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TWebRouteModule.hasPermission', lmt_Warning, 'No permission found <%s> in UserConfig!', [cJSONFieldPermissions]);
end;

function TWebRouteModule.StreamFileFromDisk(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
  const aFilePath, aSuggestedFileName: String; out ResponseCode: Integer): Boolean;
var
  _Range: String;
begin
  Result := True;

  if not FileExists(aFilePath) then
  begin
    LBLogger.Write(1, 'TWebRouteModule.StreamFileFromDisk', lmt_Error,
      'File non trovato sul filesystem: <%s>', [aFilePath]);
    ResponseCode := HTTP_STATUS_NOT_FOUND;
    Self.WriteErrorResponse('File not found on the filesystem', ResponseHeaders, ResponseData);
    Exit;
  end;

  _Range := Trim(HTTPParser.Headers.Values[HTTP_HEADER_RANGE]);

  if RequestManager.setFileToSendByAbsolutePath(aFilePath, _Range, ResponseCode) then
  begin
    if aSuggestedFileName <> '' then
      ResponseHeaders.Add(HTTP_HEADER_CONTENT_DISPOSITION + ': ' + BuildContentDispositionValue(aSuggestedFileName));
  end
  else begin
    LBLogger.Write(1, 'TWebRouteModule.StreamFileFromDisk', lmt_Error,
      'setFileToSendByAbsolutePath fallito per <%s>', [aFilePath]);
    ResponseCode := HTTP_STATUS_NOT_FOUND;
    Self.WriteErrorResponse('Could not prepare the file for sending', ResponseHeaders, ResponseData);
  end;
end;

function TWebRouteModule.ReadRequestBodyAsString(HTTPParser: THTTPRequestParser): String;
begin
  Result := '';

  try
    if (HTTPParser.Body <> nil) and (HTTPParser.Body.Size > 0) then
    begin
      HTTPParser.Body.Position := 0;
      SetLength(Result, HTTPParser.Body.Size);
      HTTPParser.Body.ReadBuffer(Result[1], HTTPParser.Body.Size);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TWebRouteModule.ReadRequestBodyAsString', lmt_Error, E.Message);
  end;
end;

function TWebRouteModule.WriteJSONResponse(aJSONResponse: TJSONData; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream): Boolean;
begin
  Result := Self.WriteRawJSONResponse(JSONDataToString(aJSONResponse), ResponseHeaders, ResponseData);
end;

function TWebRouteModule.WriteRawJSONResponse(const aJSONText: String; ResponseHeaders: TStringList; var ResponseData: TMemoryStream): Boolean;
var
  _sJSON: String;
begin
  Result := False;

  try
    _sJSON := aJSONText;
    if _sJSON = '' then
      _sJSON := cEmptyJSONObjectText;

    if ResponseData = nil then
      ResponseData := TMemoryStream.Create
    else
      ResponseData.Clear;

    if ResponseHeaders.IndexOfName(HTTP_HEADER_CONTENT_TYPE) = -1 then
      ResponseHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + MIME_TYPE_JSON);

    ResponseData.WriteBuffer(_sJSON[1], Length(_sJSON));
    ResponseData.Position := 0;
    Result := True;
  except
    on E: Exception do
      LBLogger.Write(1, 'TWebRouteModule.WriteRawJSONResponse', lmt_Error, E.Message);
  end;
end;

procedure TWebRouteModule.WriteErrorResponse(const aMessage: String; ResponseHeaders: TStringList; var ResponseData: TMemoryStream);
var
  _Error: TAnswerError;
begin
  _Error := TAnswerError.Create;
  try
    _Error.Error := aMessage;
    Self.WriteJSONResponse(_Error, ResponseHeaders, ResponseData);
  finally
    _Error.Free;
  end;
end;

function TWebRouteModule.DispatchProxyRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
  const aTargetName: String; aUserId: Integer; aUserConfig: TJSONObject;
  out ResponseCode: Integer): Boolean;
var
  _Context  : TLBSignatureContext;
  _Body     : String;
  _Response : TLBSignedResponse;
  _Guid     : TGUID;
begin
  Result := True;
  ResponseCode := HTTP_STATUS_INTERNAL_ERROR;

  if (FSignedClient = nil) or (FRemoteTargets = nil) then
  begin
    LBLogger.Write(1, 'TWebRouteModule.DispatchProxyRequest', lmt_Error,
      'Rotta proxy verso <%s> invocata, ma il client firmato non e'' impostato!', [aTargetName]);
    Self.WriteErrorResponse('Sorgente remota non configurata', ResponseHeaders, ResponseData);
    Exit;
  end;

  // Identita' applicativa propagata al server remoto. Viaggia DENTRO la
  // stringa firmata (si veda uLBRequestSignature.pas): non e' quindi
  // falsificabile da chi intercetta la richiesta, a differenza di quanto
  // accadrebbe con una semplice intestazione non firmata.
  ClearSignatureContext(_Context);
  _Context.UserId := aUserId;
  if aUserConfig <> nil then
  begin
    _Context.RoleId  := aUserConfig.Get('role_id', Int64(0));
    _Context.IsAdmin := aUserConfig.Get('is_admin', False);
  end;

  // Identificativo di correlazione: permette di rintracciare la stessa
  // operazione nei log di entrambi i server. Nessun ruolo di sicurezza.
  CreateGUID(_Guid);
  _Context.CorrelationId := StringReplace(StringReplace(GUIDToString(_Guid), '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll]);

  _Body := Self.ReadRequestBodyAsString(HTTPParser);

  _Response := FSignedClient.SendSigned(aTargetName, HTTPParser.Method,
    HTTPParser.URI, _Body, _Context);

  if _Response.Success then
  begin
    // La risposta del server remoto viene inoltrata cosi' com'e', incluso
    // il suo codice di stato: questo modulo fa da ponte, non reinterpreta
    // il significato di cio' che il remoto ha risposto.
    ResponseCode := _Response.StatusCode;
    Self.WriteRawJSONResponse(_Response.Body, ResponseHeaders, ResponseData);
  end
  else begin
    // Degrado esplicito: il client riceve una risposta strutturata che sa
    // interpretare come "sorgente non disponibile", mai una pagina rotta.
    ResponseCode := cHTTP_STATUS_BAD_GATEWAY;
    LBLogger.Write(1, 'TWebRouteModule.DispatchProxyRequest', lmt_Warning,
      'Inoltro verso <%s> fallito: %s (correlazione %s)',
      [aTargetName, _Response.ErrorMessage, _Context.CorrelationId]);
    Self.WriteErrorResponse('La sorgente dati remota non e'' al momento disponibile', ResponseHeaders, ResponseData);
  end;
end;

function TWebRouteModule.DoProcessRequest(
  RequestManager  : THTTPRequestManager;
  HTTPParser      : THTTPRequestParser;
  ResponseHeaders : TStringList;
  var ResponseData: TMemoryStream;
  out ResponseCode: Integer): Boolean;
var
  _AreaCode, _OpType: String;
  _RequiresAuth: Boolean;
  _Kind: TEndpointKind;
  _Method: TMethod;
  _KindMatches: Boolean;
  _Target: String;
  _UserId: Integer;
  _UserConfig: TJSONObject;
  _RequestDataText: String;
  _RequestDataObj: TJSONObject;
  _WorkerStatusCode: Integer;
  _FilePath, _SuggestedFileName: String;
  _AuthResult: TJSONData;
  _StandardResult: TJSONData;
  _DownloadOk: Boolean;
begin
  Result := False;
  ResponseCode := HTTP_STATUS_NOT_FOUND;
  _UserConfig := nil;

  try
    HTTPParser.SplitURIIntoResourceAndParameters;

    if not FRoutes.Resolve(HTTPParser.Method, HTTPParser.Resource, _AreaCode, _OpType, _RequiresAuth, _Kind, _Method, _KindMatches, _Target) then
      Exit;     // -------------------->>>>

    Result := True;

    if not _KindMatches then
    begin
      LBLogger.Write(1, 'TWebRouteModule.DoProcessRequest', lmt_Error, 'La rotta <%s.%s> ha un Kind dichiarato incoerente con la registrazione del proprio metodo!', [_AreaCode, _OpType]);
      ResponseCode := HTTP_STATUS_INTERNAL_ERROR;
      Self.WriteErrorResponse('Errore interno del server', ResponseHeaders, ResponseData);
      Exit;
    end;

    ResponseCode := HTTP_STATUS_UNAUTHORIZED;
    _UserId := 0;

    if _RequiresAuth then
    begin
      if not Assigned(FOnAuthenticateRequest) then
      begin
        LBLogger.Write(1, 'TWebRouteModule.DoProcessRequest', lmt_Error, 'Callback OnAuthenticateRequest non impostato!');
        ResponseCode := HTTP_STATUS_INTERNAL_ERROR;
        Self.WriteErrorResponse('Errore interno del server', ResponseHeaders, ResponseData);
        Exit;
      end;

      if not FOnAuthenticateRequest(HTTPParser.Headers, ResponseHeaders, _UserId, _UserConfig) then
      begin
        ResponseCode := HTTP_STATUS_UNAUTHORIZED;
        Self.WriteErrorResponse('Autenticazione richiesta', ResponseHeaders, ResponseData);
        Exit;
      end;

      if (_OpType <> '') and (not Self.hasPermission(_UserConfig, _AreaCode, _OpType)) then
      begin
        LBLogger.Write(3, 'TWebRouteModule.DoProcessRequest', lmt_Warning,
          'Utente %d non autorizzato su <%s.%s>', [_UserId, _AreaCode, _OpType]);
        ResponseCode := HTTP_STATUS_FORBIDDEN;
        Self.WriteErrorResponse('Operazione non consentita', ResponseHeaders, ResponseData);
        Exit;
      end;
    end;

    _WorkerStatusCode := HTTP_STATUS_OK;

    case _Kind of
      ekFileDownload:
        begin
          _FilePath := '';
          _SuggestedFileName := '';

          _DownloadOk := TFileDownloadWorker(_Method)(_UserId, _UserConfig, HTTPParser,
            _FilePath, _SuggestedFileName, _WorkerStatusCode);

          if _DownloadOk then
            Self.StreamFileFromDisk(RequestManager, HTTPParser, ResponseHeaders, ResponseData, _FilePath, _SuggestedFileName, ResponseCode)
          else begin
            if _WorkerStatusCode > 0 then
              ResponseCode := _WorkerStatusCode
            else
              ResponseCode := HTTP_STATUS_NOT_FOUND;
            Self.WriteErrorResponse('Download not available', ResponseHeaders, ResponseData);
          end;
        end;

      ekAuth:
        begin
          _RequestDataText := Self.ReadRequestBodyAsString(HTTPParser);
          _RequestDataObj := StringToJSONObject(_RequestDataText);
          try
            _AuthResult := TAuthWorker(_Method)(_UserId, _UserConfig, _RequestDataObj, HTTPParser,
              ResponseHeaders, _WorkerStatusCode);
            try
              Self.WriteJSONResponse(_AuthResult, ResponseHeaders, ResponseData);
            finally
              if _AuthResult <> nil then
                _AuthResult.Free;
            end;
          finally
            _RequestDataObj.Free;
          end;

          if _WorkerStatusCode > 0 then
            ResponseCode := _WorkerStatusCode
          else
            ResponseCode := HTTP_STATUS_OK;
        end;

      ekUpload:
        begin
          // Il file binario e' gia' stato ricevuto e scritto su disco
          // temporaneo dal livello di trasporto (THTTPRequestManager)
          // PRIMA del routing: il percorso e' in HTTPParser.UploadedFiles,
          // i metadati negli header della richiesta. Nessun corpo JSON da
          // leggere: si invoca direttamente il worker, che scrive la
          // risposta. L'autenticazione e hasPermission (per l'area/OpType
          // dichiarati nell'XML) sono gia' state applicate sopra, come
          // per ogni altra famiglia.
          TUploadWorker(_Method)(_UserId, _UserConfig, RequestManager, HTTPParser,
            ResponseHeaders, ResponseData, _WorkerStatusCode);

          if _WorkerStatusCode > 0 then
            ResponseCode := _WorkerStatusCode
          else
            ResponseCode := HTTP_STATUS_OK;
        end;

      ekProxy:
        begin
          // Nessun metodo da invocare: la richiesta viene firmata e
          // inoltrata al server remoto nominato dall'attributo Target.
          // L'autenticazione e hasPermission sono gia' state applicate
          // sopra, esattamente come per ogni altra famiglia: il fatto che
          // l'elaborazione avvenga altrove non cambia in alcun modo chi
          // puo' chiamare questo endpoint.
          Self.DispatchProxyRequest(HTTPParser, ResponseHeaders, ResponseData,
            _Target, _UserId, _UserConfig, ResponseCode);
        end;

    else
      // ekStandard
      _RequestDataText := Self.ReadRequestBodyAsString(HTTPParser);
      _RequestDataObj := StringToJSONObject(_RequestDataText);
      try
        _StandardResult := TStandardWorker(_Method)(_UserId, _UserConfig, _RequestDataObj, _WorkerStatusCode);
        try
          Self.WriteJSONResponse(_StandardResult, ResponseHeaders, ResponseData);
        finally
          if _StandardResult <> nil then
            _StandardResult.Free;
        end;
      finally
        _RequestDataObj.Free;
      end;

      if _WorkerStatusCode > 0 then
        ResponseCode := _WorkerStatusCode
      else begin
        LBLogger.Write(1, 'TWebRouteModule.DoProcessRequest', lmt_Warning,
          'Il worker per <%s.%s> ha restituito un codice di stato non valido (%d): normalizzato a 200',
          [_AreaCode, _OpType, _WorkerStatusCode]);
        ResponseCode := HTTP_STATUS_OK;
      end;
    end;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TWebRouteModule.DoProcessRequest', lmt_Error, E.Message);
      Result := True;
      ResponseCode := HTTP_STATUS_INTERNAL_ERROR;
      Self.WriteErrorResponse('Errore interno del server', ResponseHeaders, ResponseData);
    end;
  end;
end;

initialization

finalization
  if GlobalRouteHandlers <> nil then
    FreeAndNil(GlobalRouteHandlers);

end.
