unit uWebRouteRegistry;

{$mode objfpc}{$H+}

{
  Generic transport router for LBmicroWebServer.

  Responsibilities: match HTTP method + URI, authenticate the session when
  RequiresAuth="true", and invoke the registered worker. Authorization is
  application policy: this unit does not parse or enforce actions,
  capabilities, roles, groups or implication rules. Child nodes of <Endpoint>
  (including <Action>) are deliberately ignored.
}

interface

uses
  Classes, SysUtils, Laz2_DOM, fpjson, jsonparser, fgl,
  uLBmicroWebServer, uHTTPRequestParser,
  uLBSignedProxy, uLBRequestSignature;

const
  cXML_ROOT_NODENAME     = DOMString('WebRoutes');
  cXML_AREA_NODENAME     = DOMString('FunctionalArea');
  cXML_ENDPOINT_NODENAME = DOMString('Endpoint');

  // Blocco che dichiara le applicazioni della piattaforma, e il singolo
  // elemento al suo interno - si veda la nota "APPLICAZIONI DELLA
  // PIATTAFORMA" in testa alla unit.
  cXML_APPS_NODENAME     = DOMString('Applications');
  cXML_APP_NODENAME      = DOMString('Application');

  // Blocco che dichiara il catalogo dei bersagli remoti per gli endpoint
  // Kind="Proxy", e il singolo elemento al suo interno. SOLO indirizzo e
  // timeout vivono qui: la chiave condivisa non compare mai nell'XML -
  // si veda la nota "LA CHIAVE SEGRETA NON COMPARE MAI NELL'XML" piu'
  // sopra - e va impostata a parte con TLBRemoteTargets.SetSharedKey o
  // TLBRemoteTargets.LoadSharedKeysFromJSONFile, da un file di
  // configurazione dell'applicazione escluso dal controllo di versione.
  cXML_TARGETS_NODENAME  = DOMString('RemoteTargets');
  cXML_TARGET_NODENAME   = DOMString('RemoteTarget');
  cXML_ATTR_BASEURL      = DOMString('BaseURL');
  cXML_ATTR_TIMEOUTMS    = DOMString('TimeoutMS');
  cDefaultTargetTimeoutMS = Integer(15000);

  cXML_ATTR_CODE         = DOMString('Code');
  cXML_ATTR_DESCRIPTION  = DOMString('Description');
  cXML_ATTR_URI          = DOMString('URI');
  cXML_ATTR_METHOD       = DOMString('Method');
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
  cEndpointKind_AIConnector  = String('AIConnector');

  cDefaultHTTPMethod     = String('POST');
  cBooleanFalseStr       = String('false');

  cEmptyJSONObjectText   = String('{}');

  // Field names produced by GetAllDeclaredApplications.
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
  TEndpointKind = (ekStandard, ekFileDownload, ekAuth, ekUpload, ekProxy, ekAIConnector);

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

  // Kind="AIConnector" is the single MCP entry point. Tool authorization
  // is delegated to the host application; the alias is only for readability.
  TAIConnectorWorker = TStandardWorker;

  TAuthenticateRequestEvent = function(RequestHeaders, ResponseHeaders: TStringList;
    out anUserId: Integer; out anUserConfig: TJSONObject): Boolean of object;

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
    EndpointCode    : String;
    // Human-readable description of the endpoint (attribute Description). Free text, usable for
    // several purposes. For a Standard endpoint exposed to an AI it is the description of the tool;
    // for the Kind="AIConnector" endpoint it describes the MCP channel itself.
    Description     : String;
    URI             : String;
    HTTPMethod      : String;
    RequiresAuth    : Boolean;
    Kind            : TEndpointKind;
    // Nome simbolico del bersaglio remoto, significativo per i soli
    // endpoint Kind="Proxy" (vuoto per tutte le altre famiglie).
    Target          : String;

    constructor Create;
    destructor Destroy; override;
  end;

  TEndpointDescriptorList = specialize TFPGObjectList<TEndpointDescriptor>;

  {------------------------------------------------------------------------
    TWorkerEntry

    Un endpoint registrato da un'area funzionale: il nome, il metodo (in
    forma generica TMethod) e la famiglia con cui e' stato registrato.
  ------------------------------------------------------------------------}
  TWorkerEntry = class(TObject)
  public
    EndpointCode : String;
    TheMethod    : TMethod;
    WorkerKind   : TEndpointKind;

    constructor Create(const anEndpointCode: String; aMethod: TMethod; aKind: TEndpointKind);
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
    procedure RegisterStandardWorker(const anEndpointCode: String; aMethod: TStandardWorker);
    procedure RegisterFileDownloadWorker(const anEndpointCode: String; aMethod: TFileDownloadWorker);
    procedure RegisterAuthWorker(const anEndpointCode: String; aMethod: TAuthWorker);
    procedure RegisterUploadWorker(const anEndpointCode: String; aMethod: TUploadWorker);

  public
    constructor Create(const anAreaCode: String); reintroduce;
    destructor Destroy; override;

    function GetWorkerMethod(const anEndpointCode: String; out aOutKind: TEndpointKind): TMethod; virtual;

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
    EndpointCode  : String;
    RequiresAuth  : Boolean;
    Kind          : TEndpointKind;
    TheMethod     : TMethod;   // valido solo se KindMatches = True; sempre nullo per ekProxy
    KindMatches   : Boolean;   // False se il Kind dichiarato nell'XML non coincide con quello di registrazione
    Target        : String;    // nome simbolico del bersaglio remoto, valorizzato solo per ekProxy

    constructor Create(aHandler: TRouteHandlerBase; const anAreaCode, anEndpointCode: String;
      aRequiresAuth: Boolean; aDeclaredKind: TEndpointKind);

    // Costruttore dedicato agli endpoint Kind="Proxy": non riceve alcun
    // handler perche' non esiste alcun metodo da risolvere. KindMatches
    // e' sempre True, dato che non c'e' alcuna registrazione con cui il
    // Kind dichiarato possa risultare discorde.
    constructor CreateProxy(const anAreaCode, anEndpointCode, aTarget: String;
      aRequiresAuth: Boolean);
  end;

  TDispatchMap = specialize TFPGMap<String, TDispatchEntry>;

  {------------------------------------------------------------------------
    TAIEndpointEntry

    A Standard endpoint bound to its handler: a candidate AI tool. It stays
    in the dispatch table too (declared once, used by the web app and the AI).
  ------------------------------------------------------------------------}
  TAIEndpointEntry = class(TObject)
  public
    Descriptor  : TEndpointDescriptor;   // borrowed: owned by TRouteRegistry
    Handler     : TRouteHandlerBase;     // borrowed: owned by the host application
    TheMethod   : TMethod;
    KindMatches : Boolean;               // False if the worker could not be resolved
  end;

  TAIEndpointList = specialize TFPGObjectList<TAIEndpointEntry>;

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
    FAIEndpoints       : TAIEndpointList;
    // Catalogo dei bersagli remoti per gli endpoint Kind="Proxy",
    // POSSEDUTO da questa classe (non piu' un riferimento in prestito
    // dall'applicazione ospitante): nasce vuoto in Create e viene
    // popolato da LoadRemoteTargets durante LoadFromXMLFile. La chiave
    // condivisa di ciascun bersaglio resta sempre assente finche'
    // l'applicazione ospitante non la imposta esplicitamente (si veda
    // TLBRemoteTargets.SetSharedKey/LoadSharedKeysFromJSONFile in
    // uLBSignedProxy.pas).
    FRemoteTargets     : TLBRemoteTargets;

    class function buildKey(const aHTTPMethod, aURI: String): String;

    // Legge il blocco <Applications>. Invocata da LoadFromXMLFile.
    procedure LoadApplications(aRootNode: TDOMNode);

    // Legge il blocco <RemoteTargets> (Name/BaseURL/TimeoutMS per ciascun
    // <RemoteTarget>) e popola FRemoteTargets. Invocata da LoadFromXMLFile,
    // subito dopo LoadApplications. Un bersaglio gia' noto (stesso Name)
    // viene aggiornato, non duplicato: la sua eventuale chiave condivisa,
    // se gia' impostata, non viene mai toccata da qui.
    procedure LoadRemoteTargets(aRootNode: TDOMNode);

    // Restituisce la descrizione di un'applicazione dichiarata, o nil.
    // Il riferimento e' IN PRESTITO: resta di proprieta' di questa classe.
    function FindApplication(const aCode: String): TApplicationDescriptor;

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
      out anAreaCode, anEndpointCode: String; out aRequiresAuth: Boolean;
      out aKind: TEndpointKind; out aMethod: TMethod; out aKindMatches: Boolean;
      out aTarget: String): Boolean;

    procedure ValidateAllEndpointsRegistered;

    // The endpoint declared with Kind="AIConnector" (the MCP entry point), or nil.
    function AIConnectorEndpoint: TEndpointDescriptor;
    // Logs every inconsistency of the MCP entry point (more than one, URI not absolute, clash with a
    // route). True if the declaration is usable.
    function ValidateAIConnector: Boolean;
    // Candidate AI tools: the authenticated Standard endpoints bound to a handler by RegisterHandler.
    function AIEndpointCount: Integer;
    function GetAIEndpoint(anIndex: Integer): TAIEndpointEntry;

    // Verifica la coerenza delle dichiarazioni di applicazione: ogni App
    // referenziata da un'area deve corrispondere a un <Application>
    // dichiarato, e tutti gli endpoint di una stessa area devono
    // concordare sul proprio App (che e' una proprieta' dell'AREA, non
    // del singolo endpoint). Invocata da ValidateAllEndpointsRegistered.
    procedure ValidateApplications;

    function GetAllDeclaredAreas: TJSONArray;

    // Elenco completo delle applicazioni dichiarate, ciascuna come
    // oggetto {code, name, home}. Nuova istanza, di cui il chiamante
    // prende in carico la proprieta'.
    function GetAllDeclaredApplications: TJSONArray;

    // Catalogo dei bersagli remoti, popolato dal blocco <RemoteTargets>
    // dell'XML delle rotte. Il riferimento resta di proprieta' di questa
    // classe: l'applicazione ospitante lo usa per impostare le chiavi
    // condivise (SetSharedKey/LoadSharedKeysFromJSONFile) e per costruire
    // il proprio TLBSignedHttpClient, ma non deve liberarlo.
    property RemoteTargets: TLBRemoteTargets read FRemoteTargets;
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

    // Il catalogo dei bersagli remoti NON e' piu' un campo di questo
    // modulo: vive in FRoutes.RemoteTargets (popolato dal blocco
    // <RemoteTargets> dell'XML - si veda TRouteRegistry.LoadRemoteTargets)
    // ed e' esposto qui sotto dalla property RemoteTargets, di sola
    // lettura. FSignedClient resta invece un riferimento IN PRESTITO, di
    // proprieta' dell'applicazione ospitante: questo modulo non lo crea e
    // non lo libera. Resta nil se nessun endpoint Kind="Proxy" e' in uso,
    // e in quel caso il ramo corrispondente di DoProcessRequest non viene
    // mai raggiunto.
    FSignedClient                 : TLBSignedHttpClient;

    // Getter della property RemoteTargets: il catalogo vive in
    // FRoutes.RemoteTargets, popolato dal blocco <RemoteTargets> dell'XML.
    function GetRemoteTargets: TLBRemoteTargets;

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
    // Deprecated compatibility hook. The router never calls it; application
    // authorization must live in the registered worker/dispatcher.
    function hasPermission(aUserConfig: TJSONObject; const anAreaCode,
      anOperationToken: String): Boolean; virtual;
    function StreamFileFromDisk(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
      const aFilePath, aSuggestedFileName: String; out ResponseCode: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadRoutesFromXML(const aFilename: String): Boolean;
    function RegisterHandler(aHandler: TRouteHandlerBase): Boolean;
    procedure RegisterAllGlobalHandlers;

    // Collega gli endpoint Kind="Proxy" e riceve il client firmato da
    // usare per inoltrarli; il catalogo dei bersagli NON si passa piu'
    // qui, perche' e' gia' stato popolato dall'XML (si veda RemoteTargets
    // sotto). Da invocare DOPO LoadRoutesFromXML e PRIMA di ValidateRoutes.
    // aSignedClient resta di proprieta' dell'applicazione ospitante, che
    // deve liberarlo. Restituisce quanti endpoint proxy sono stati
    // collegati.
    function RegisterProxyEndpoints(aSignedClient: TLBSignedHttpClient): Integer;

    procedure ValidateRoutes;

    function GetAllDeclaredAreas: TJSONArray;

    // Si vedano le omonime su TRouteRegistry.
    function GetAllDeclaredApplications: TJSONArray;
    property Routes: TRouteRegistry read FRoutes;

    // Catalogo dei bersagli remoti, popolato dal blocco <RemoteTargets>
    // dell'XML delle rotte (equivalente a FRoutes.RemoteTargets).
    // L'applicazione ospitante lo usa, DOPO LoadRoutesFromXML, per
    // impostare le chiavi condivise (TLBRemoteTargets.SetSharedKey o
    // LoadSharedKeysFromJSONFile) e per costruire il proprio
    // TLBSignedHttpClient da passare a RegisterProxyEndpoints.
    property RemoteTargets: TLBRemoteTargets read GetRemoteTargets;

    property OnAuthenticateRequest: TAuthenticateRequestEvent
      read FOnAuthenticateRequest write FOnAuthenticateRequest;
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

{ TEndpointDescriptor }

constructor TEndpointDescriptor.Create;
begin
  inherited Create;
end;

destructor TEndpointDescriptor.Destroy;
begin
  inherited Destroy;
end;

constructor TWorkerEntry.Create(const anEndpointCode: String; aMethod: TMethod; aKind: TEndpointKind);
begin
  inherited Create;
  EndpointCode := anEndpointCode;
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

procedure TRouteHandlerBase.RegisterStandardWorker(const anEndpointCode: String; aMethod: TStandardWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterStandardWorker', lmt_Warning,
      'Metodo non impostato per l''endpoint <%s> nell''area <%s>', [anEndpointCode, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(anEndpointCode, TMethod(aMethod), ekStandard));
end;

procedure TRouteHandlerBase.RegisterFileDownloadWorker(const anEndpointCode: String; aMethod: TFileDownloadWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterFileDownloadWorker', lmt_Warning,
      'Metodo non impostato per l''endpoint <%s> nell''area <%s>', [anEndpointCode, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(anEndpointCode, TMethod(aMethod), ekFileDownload));
end;

procedure TRouteHandlerBase.RegisterAuthWorker(const anEndpointCode: String; aMethod: TAuthWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterAuthWorker', lmt_Warning,
      'Metodo non impostato per l''endpoint <%s> nell''area <%s>', [anEndpointCode, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(anEndpointCode, TMethod(aMethod), ekAuth));
end;

procedure TRouteHandlerBase.RegisterUploadWorker(const anEndpointCode: String; aMethod: TUploadWorker);
begin
  if not Assigned(aMethod) then
  begin
    LBLogger.Write(1, 'TRouteHandlerBase.RegisterUploadWorker', lmt_Warning,
      'Metodo non impostato per l''endpoint <%s> nell''area <%s>', [anEndpointCode, FAreaCode]);
    Exit;
  end;

  FWorkers.Add(TWorkerEntry.Create(anEndpointCode, TMethod(aMethod), ekUpload));
end;

function TRouteHandlerBase.GetWorkerMethod(const anEndpointCode: String; out aOutKind: TEndpointKind): TMethod;
var
  i: Integer;
begin
  Result.Code := nil;
  Result.Data := nil;
  aOutKind := ekStandard;

  for i := 0 to FWorkers.Count - 1 do
  begin
    if SameText(FWorkers[i].EndpointCode, anEndpointCode) then
    begin
      Result := FWorkers[i].TheMethod;
      aOutKind := FWorkers[i].WorkerKind;
      Exit;
    end;
  end;

  LBLogger.Write(1, 'TRouteHandlerBase.GetWorkerMethod', lmt_Warning,
    'Nessun metodo registrato per l''endpoint <%s> nell''area <%s>', [anEndpointCode, FAreaCode]);
end;


{ TDispatchEntry }

constructor TDispatchEntry.Create(aHandler: TRouteHandlerBase; const anAreaCode, anEndpointCode: String;
  aRequiresAuth: Boolean; aDeclaredKind: TEndpointKind);
var
  _RegisteredKind: TEndpointKind;
begin
  inherited Create;

  AreaCode     := anAreaCode;
  EndpointCode := anEndpointCode;
  RequiresAuth := aRequiresAuth;
  Kind         := aDeclaredKind;
  KindMatches  := False;
  Target       := '';

  TheMethod.Code := nil;
  TheMethod.Data := nil;

  if aHandler = nil then
  begin
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error, 'Handler non impostato per <%s.%s>!',
      [anAreaCode, anEndpointCode]);
    Exit;
  end;

  TheMethod := aHandler.GetWorkerMethod(anEndpointCode, _RegisteredKind);
  if TheMethod.Code = nil then
  begin
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error, 'GetWorkerMethod ha restituito un metodo nullo per <%s.%s>!',
      [anAreaCode, anEndpointCode]);
    Exit;
  end;

  KindMatches := (_RegisteredKind = aDeclaredKind);
  if not KindMatches then
    LBLogger.Write(1, 'TDispatchEntry.Create', lmt_Error,
      'Endpoint <%s.%s>: il Kind dichiarato nell''XML non coincide con la famiglia con cui il metodo e'' stato registrato!',
      [anAreaCode, anEndpointCode]);
end;

constructor TDispatchEntry.CreateProxy(const anAreaCode, anEndpointCode, aTarget: String;
  aRequiresAuth: Boolean);
begin
  inherited Create;

  AreaCode     := anAreaCode;
  EndpointCode := anEndpointCode;
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
  FAIEndpoints := TAIEndpointList.Create(True);
  FRemoteTargets := TLBRemoteTargets.Create;
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

    FreeAndNil(FAIEndpoints);   // before the descriptors it borrows
    FreeAndNil(FDeclaredEndpoints);
    FreeAndNil(FApplications);
    FreeAndNil(FRemoteTargets);
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

procedure TRouteRegistry.LoadRemoteTargets(aRootNode: TDOMNode);
{
  Simmetrica a LoadApplications, ma per il blocco <RemoteTargets>. A
  differenza delle applicazioni, qui NON si fa Clear prima di popolare:
  un bersaglio gia' noto (stesso Name) viene aggiornato sul posto da
  TLBRemoteTargets.Add, che preserva la SharedKey eventualmente gia'
  impostata dall'applicazione ospitante - un ricaricamento a caldo
  dell'XML non deve mai costringere a reinserire le chiavi.
}
var
  _Node, _TargetNode : TDOMNode;
  _TargetElement      : TDOMElement;
  _Name, _BaseURL     : String;
  _TimeoutMS          : Integer;
begin
  if aRootNode = nil then Exit;

  _Node := aRootNode.FirstChild;
  while _Node <> nil do
  begin
    if (_Node.NodeType = ELEMENT_NODE) and (_Node.NodeName = cXML_TARGETS_NODENAME) then
    begin
      _TargetNode := _Node.FirstChild;

      while _TargetNode <> nil do
      begin
        if (_TargetNode.NodeType = ELEMENT_NODE) and (_TargetNode.NodeName = cXML_TARGET_NODENAME) then
        begin
          _TargetElement := TDOMElement(_TargetNode);

          _Name := Trim(_TargetElement.GetAttribute(cXML_ATTR_NAME));
          _BaseURL := Trim(_TargetElement.GetAttribute(cXML_ATTR_BASEURL));
          if not TryStrToInt(Trim(_TargetElement.GetAttribute(cXML_ATTR_TIMEOUTMS)), _TimeoutMS) then
            _TimeoutMS := cDefaultTargetTimeoutMS;

          if (_Name <> '') and (_BaseURL <> '') then
          begin
            FRemoteTargets.Add(_Name, _BaseURL, _TimeoutMS);

            LBLogger.Write(5, 'TRouteRegistry.LoadRemoteTargets', lmt_Debug,
              'Bersaglio remoto dichiarato: <%s> (%s), timeout %d ms - chiave condivisa non ancora impostata',
              [_Name, _BaseURL, _TimeoutMS]);
          end
          else
            LBLogger.Write(1, 'TRouteRegistry.LoadRemoteTargets', lmt_Warning,
              '<%s> privo di %s o %s: ignorato',
              [String(cXML_TARGET_NODENAME), String(cXML_ATTR_NAME), String(cXML_ATTR_BASEURL)]);
        end;

        _TargetNode := _TargetNode.NextSibling;
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
  FAIEndpoints.Clear;   // entries borrow the descriptors: clear them first
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

        // Il catalogo dei bersagli remoti viene letto subito dopo, cosi'
        // da essere gia' completo quando RegisterProxyEndpoints (invocato
        // piu' avanti da chi usa questa classe) collega gli endpoint
        // Kind="Proxy" alla tabella di dispatch.
        Self.LoadRemoteTargets(_RootNode);

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
                  _Descriptor.EndpointCode := Trim(_EndpointElement.GetAttribute(cXML_ATTR_CODE));
                  _Descriptor.Description := Trim(_EndpointElement.GetAttribute(cXML_ATTR_DESCRIPTION));
                  _Descriptor.URI := Trim(_EndpointElement.GetAttribute(cXML_ATTR_URI));
                  _Descriptor.HTTPMethod := UpperCase(Trim(_EndpointElement.GetAttribute(cXML_ATTR_METHOD)));
                  if _Descriptor.HTTPMethod = '' then
                    _Descriptor.HTTPMethod := cDefaultHTTPMethod;

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
                  else if SameText(_KindStr, cEndpointKind_AIConnector) then
                  begin
                    // The MCP entry point has no route worker.  Authorization
                    // of AI tools is delegated to the host application.
                    _Descriptor.Kind := ekAIConnector;
                  end
                  else if SameText(_KindStr, cEndpointKind_Proxy) then
                  begin
                    _Descriptor.Kind := ekProxy;

                    // Un endpoint proxy senza bersaglio non potrebbe fare
                    // nulla: viene segnalato qui, e piu' avanti
                    // RegisterProxyEndpoints lo salta senza collegarlo.
                    if _Descriptor.Target = '' then
                      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Error,
                        'Endpoint <%s.%s> dichiarato Kind="%s" ma privo dell''attributo %s: non sara'' raggiungibile',
                        [_AreaCode, _Descriptor.EndpointCode, cEndpointKind_Proxy, String(cXML_ATTR_TARGET)]);
                  end
                  else begin
                    _Descriptor.Kind := ekStandard;
                    if (_KindStr <> '') and (not SameText(_KindStr, cEndpointKind_Standard)) then
                      LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
                        'Endpoint <%s.%s>: valore Kind <%s> non riconosciuto, trattato come <%s>',
                        [_AreaCode, _Descriptor.EndpointCode, _KindStr, cEndpointKind_Standard]);
                  end;

                  if (_Descriptor.EndpointCode <> '') and
                     (_Descriptor.URI <> '') then
                    FDeclaredEndpoints.Add(_Descriptor)
                  else begin
                    LBLogger.Write(1, 'TRouteRegistry.LoadFromXMLFile', lmt_Warning,
                      'Definizione <Endpoint> incompleta nell''area <%s>: servono gli attributi Code e URI; ignorata',
                      [_AreaCode]);
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
  i, _j, _Idx: Integer;
  _Descriptor: TEndpointDescriptor;
  _Key: String;
  _Entry: TDispatchEntry;
  _AIEntry: TAIEndpointEntry;
  _RegisteredKind: TEndpointKind;
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

      // Kind="AIConnector": the MCP entry point has no handler (like Proxy); see AIConnectorEndpoint.
      if _Descriptor.Kind = ekAIConnector then
        Continue;

      _Key := Self.buildKey(_Descriptor.HTTPMethod, _Descriptor.URI);

      _Entry := TDispatchEntry.Create(aHandler, _Descriptor.AreaCode, _Descriptor.EndpointCode,
        _Descriptor.RequiresAuth, _Descriptor.Kind);

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

      // Every authenticated Standard endpoint is a CANDIDATE AI tool: TLBAIConnector keeps it only if
      // the handler implements IAIConnectorFunctions. No second declaration is needed.
      if (_Descriptor.Kind = ekStandard) and _Descriptor.RequiresAuth then
      begin
        _AIEntry := TAIEndpointEntry.Create;
        _AIEntry.Descriptor := _Descriptor;
        _AIEntry.Handler := aHandler;
        _AIEntry.TheMethod := aHandler.GetWorkerMethod(_Descriptor.EndpointCode, _RegisteredKind);
        _AIEntry.KindMatches := (_AIEntry.TheMethod.Code <> nil) and (_RegisteredKind = ekStandard);
        for _j := FAIEndpoints.Count - 1 downto 0 do
          if FAIEndpoints[_j].Descriptor = _Descriptor then
            FAIEndpoints.Delete(_j);
        FAIEndpoints.Add(_AIEntry);
      end;

      LBLogger.Write(5, 'TRouteRegistry.RegisterHandler', lmt_Debug,
        'Rotta <%s> collegata all''area <%s>, endpoint <%s>',
        [_Key, _Descriptor.AreaCode, _Descriptor.EndpointCode]);
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

    _Entry := TDispatchEntry.CreateProxy(_Descriptor.AreaCode, _Descriptor.EndpointCode,
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
      'Rotta <%s> inoltrata al bersaglio remoto <%s> (area <%s>)',
      [_Key, _Descriptor.Target, _Descriptor.AreaCode]);
  end;
end;

function TRouteRegistry.Resolve(const aHTTPMethod, aURIResource: String;
  out anAreaCode, anEndpointCode: String; out aRequiresAuth: Boolean;
  out aKind: TEndpointKind; out aMethod: TMethod; out aKindMatches: Boolean;
  out aTarget: String): Boolean;
var
  _Key: String;
  _Idx: Integer;
  _Entry: TDispatchEntry;
begin
  anAreaCode := '';
  anEndpointCode := '';
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
    anEndpointCode := _Entry.EndpointCode;
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

function TRouteRegistry.AIConnectorEndpoint: TEndpointDescriptor;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FDeclaredEndpoints.Count - 1 do
    if FDeclaredEndpoints[i].Kind = ekAIConnector then
      Exit(FDeclaredEndpoints[i]);
end;

function TRouteRegistry.ValidateAIConnector: Boolean;
var
  i, _Count : Integer;
  _Gate     : TEndpointDescriptor;
begin
  Result := True;
  _Gate := Self.AIConnectorEndpoint;
  if _Gate = nil then Exit;   // none declared: the connector will warn

  _Count := 0;
  for i := 0 to FDeclaredEndpoints.Count - 1 do
    if FDeclaredEndpoints[i].Kind = ekAIConnector then
      Inc(_Count);
  if _Count > 1 then
  begin
    LBLogger.Write(1, 'TRouteRegistry.ValidateAIConnector', lmt_Error,
      '%d endpoints with Kind="AIConnector": only one MCP entry point is allowed, <%s.%s> is used', [_Count, _Gate.AreaCode, _Gate.EndpointCode]);
    Result := False;
  end;

  if (_Gate.URI = '') or (_Gate.URI[1] <> '/') then
  begin
    LBLogger.Write(1, 'TRouteRegistry.ValidateAIConnector', lmt_Error,
      'The URI of the AIConnector endpoint <%s> must start with "/"', [_Gate.URI]);
    Result := False;
  end;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
    if (FDeclaredEndpoints[i] <> _Gate) and (FDeclaredEndpoints[i].URI = _Gate.URI) then
    begin
      LBLogger.Write(1, 'TRouteRegistry.ValidateAIConnector', lmt_Error,
        'The MCP path <%s> is also the URI of %s.%s: the connector would shadow it', [_Gate.URI,
        FDeclaredEndpoints[i].AreaCode, FDeclaredEndpoints[i].EndpointCode]);
      Result := False;
    end;
end;

function TRouteRegistry.AIEndpointCount: Integer;
begin
  Result := FAIEndpoints.Count;
end;

function TRouteRegistry.GetAIEndpoint(anIndex: Integer): TAIEndpointEntry;
begin
  Result := FAIEndpoints[anIndex];
end;

procedure TRouteRegistry.ValidateAllEndpointsRegistered;
var
  i, j, _MissingCount, _Idx: Integer;
  _Descriptor: TEndpointDescriptor;
  _Key: String;
  _Found: Boolean;
begin
  _MissingCount := 0;

  for i := 0 to FDeclaredEndpoints.Count - 1 do
  begin
    _Descriptor := FDeclaredEndpoints[i];

    if _Descriptor.Kind = ekAIConnector then
      Continue;   // entry point of TLBAIConnector: no handler, not in the dispatch table

    _Key := Self.buildKey(_Descriptor.HTTPMethod, _Descriptor.URI);

    if not FDispatchTable.Find(_Key, _Idx) then
    begin
      Inc(_MissingCount);
      LBLogger.Write(1, 'TRouteRegistry.ValidateAllEndpointsRegistered', lmt_Warning,
        'L''endpoint dichiarato <%s> (%s.%s) non ha alcun handler registrato!',
        [_Key, _Descriptor.AreaCode, _Descriptor.EndpointCode]);
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

function TRouteRegistry.GetAllDeclaredAreas: TJSONArray;
var
  i, j, _FoundIdx, _MinIdx : Integer;
  _Descriptor : TEndpointDescriptor;
  _AreaObj    : TJSONObject;
  _MinDescr   : String;
begin
  Result := TJSONArray.Create;
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
    if _FoundIdx < 0 then
    begin
      _AreaObj := TJSONObject.Create;
      _AreaObj.Add('code', _Descriptor.AreaCode);
      _AreaObj.Add('description', _Descriptor.AreaDescription);
      _AreaObj.Add('app', _Descriptor.AreaApp);
      Result.Add(_AreaObj);
    end;
  end;
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
    if _MinIdx <> i then Result.Exchange(i, _MinIdx);
  end;
end;


{ TWebRouteModule }

constructor TWebRouteModule.Create;
begin
  inherited Create;
  FRoutes := TRouteRegistry.Create;
  FSignedClient := nil;
end;

destructor TWebRouteModule.Destroy;
begin
  // FSignedClient e' un riferimento in prestito, di proprieta'
  // dell'applicazione ospitante: non va liberato qui. Il catalogo dei
  // bersagli remoti (FRoutes.RemoteTargets) e' invece posseduto da
  // FRoutes, e viene gia' liberato dal suo distruttore.
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

function TWebRouteModule.GetRemoteTargets: TLBRemoteTargets;
begin
  Result := FRoutes.RemoteTargets;
end;

function TWebRouteModule.LoadRoutesFromXML(const aFilename: String): Boolean;
begin
  // Popola, fra l'altro, FRoutes.RemoteTargets dal blocco <RemoteTargets>
  // dell'XML: dopo questa chiamata l'applicazione ospitante puo' gia'
  // impostare le chiavi condivise tramite la property RemoteTargets.
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

function TWebRouteModule.RegisterProxyEndpoints(aSignedClient: TLBSignedHttpClient): Integer;
begin
  FSignedClient := aSignedClient;

  Result := FRoutes.RegisterProxyEndpoints();

  if (Result > 0) and (FSignedClient = nil) then
    LBLogger.Write(1, 'TWebRouteModule.RegisterProxyEndpoints', lmt_Error,
      '%d endpoint Kind="%s" collegati, ma il client firmato non e'' impostato: quelle rotte risponderanno sempre con un errore!',
      [Result, cEndpointKind_Proxy]);
end;

procedure TWebRouteModule.ValidateRoutes;
begin
  FRoutes.ValidateAllEndpointsRegistered;
end;

function TWebRouteModule.hasPermission(aUserConfig: TJSONObject;
  const anAreaCode, anOperationToken: String): Boolean;
begin
  // Intentionally unused by route dispatch. Kept only so older subclasses
  // still compile while moving their policy into application workers.
  Result := False;
end;

function TWebRouteModule.GetAllDeclaredAreas: TJSONArray;
begin
  Result := FRoutes.GetAllDeclaredAreas;
end;

function TWebRouteModule.GetAllDeclaredApplications: TJSONArray;
begin
  Result := FRoutes.GetAllDeclaredApplications;
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

  if FSignedClient = nil then
  begin
    LBLogger.Write(1, 'TWebRouteModule.DispatchProxyRequest', lmt_Error,
      'Rotta proxy verso <%s> invocata, ma il client firmato non e'' impostato!', [aTargetName]);
    Self.WriteErrorResponse('Sorgente remota non configurata', ResponseHeaders, ResponseData);
    Exit;
  end;

  // Il bersaglio e' dichiarato nell'XML (indirizzo/timeout), ma la sua
  // chiave condivisa viene impostata a parte dall'applicazione ospitante
  // - si veda la nota "LA CHIAVE SEGRETA NON COMPARE MAI NELL'XML". Se
  // manca ancora, e' un errore di configurazione dell'applicazione, non
  // dell'XML delle rotte: va segnalato qui, non lasciato fallire piu'
  // avanti con un errore di firma poco comprensibile.
  if not FRoutes.RemoteTargets.IsReady(aTargetName) then
  begin
    LBLogger.Write(1, 'TWebRouteModule.DispatchProxyRequest', lmt_Error,
      'Bersaglio remoto <%s> non pronto: indirizzo o chiave condivisa mancanti nella configurazione dell''applicazione!',
      [aTargetName]);
    Self.WriteErrorResponse('Sorgente remota non configurata', ResponseHeaders, ResponseData);
    Exit;
  end;

  // Identita' applicativa propagata al server remoto. Viaggia DENTRO la
  // stringa firmata (si veda uLBSignedProxy.pas): non e' quindi
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
  _AreaCode, _EndpointCode: String;
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

    if not FRoutes.Resolve(HTTPParser.Method, HTTPParser.Resource, _AreaCode, _EndpointCode,
      _RequiresAuth, _Kind, _Method, _KindMatches, _Target) then
      Exit;     // -------------------->>>>

    Result := True;

    if not _KindMatches then
    begin
      LBLogger.Write(1, 'TWebRouteModule.DoProcessRequest', lmt_Error,
        'La rotta <%s.%s> ha un Kind dichiarato incoerente con la registrazione del proprio metodo!',
        [_AreaCode, _EndpointCode]);
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

      // The transport stops here: authentication is the only policy owned
      // by LBmicroWebServer.  Application authorization (actions,
      // capabilities, scopes, roles) belongs to the registered worker.
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
          // risposta. L'autenticazione e' gia' stata applicata sopra.
          TUploadWorker(_Method)(_UserId, _UserConfig, RequestManager, HTTPParser,
            ResponseHeaders, ResponseData, _WorkerStatusCode);

          if _WorkerStatusCode > 0 then
            ResponseCode := _WorkerStatusCode
          else
            ResponseCode := HTTP_STATUS_OK;
        end;

      ekAIConnector:
        begin
          // Unreachable: AIConnector endpoints never enter the dispatch table. Defensive answer.
          ResponseCode := HTTP_STATUS_NOT_FOUND;
          Self.WriteErrorResponse('Not found', ResponseHeaders, ResponseData);
        end;

      ekProxy:
        begin
          // Nessun metodo da invocare: la richiesta viene firmata e
          // inoltrata al server remoto nominato dall'attributo Target.
          // L'autenticazione e' gia' stata applicata sopra: il fatto che
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
          [_AreaCode, _EndpointCode, _WorkerStatusCode]);
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
