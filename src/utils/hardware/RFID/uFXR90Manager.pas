unit uFXR90Manager;

{$mode objfpc}{$H+}

{ ============================================================
  uFXR90Manager — Driver per il lettore RFID Zebra FXR90
  ============================================================

  STRUTTURA
  ─────────────────────────────────────────────────────────────
  TFXR90RFPreset / TFXR90RFPresets
    Preset RF specifici dell'FXR90: Environment, Session, Target,
    TagPopulation. Estendono TRFIDAntennaPreset di uRFIDCommon.

  TFXR90RESTClient
    Client HTTP che gestisce la sessione verso il reader:
    login JWT → PUT /cloud/mode → PUT /cloud/start → PUT /cloud/stop.

  TFXR90Consumer
    Thread separato per il parsing JSON. Non rallenta mai la lettura
    dal WebSocket: accoda i payload e li processa in modo asincrono.

  TFXR90Manager
    Sottoclasse concreta di TRFIDManager. Implementa:
      AddAntenna(port, dBm, preset)
      StartInventory / StopInventory
    Coordina RESTClient, WebSocket e Consumer.

  CONFIGURAZIONE ANTENNE
  ─────────────────────────────────────────────────────────────
  I preset (TFXR90RFPresets) sono una classe a sé: il chiamante la
  istanzia, ne passa i preset ad AddAntenna (che li clona internamente),
  e può liberarla subito dopo — TFXR90Manager non la possiede né la
  espone (non esiste una property "Presets" sul Manager).

    _presets := TFXR90RFPresets.Create;
    try
      { Preset predefinito }
      FReader.AddAntenna(1, 29, _presets.Items[0]);

      { Preset personalizzato: clona, modifica, passa }
      _p := TFXR90RFPreset(_presets.Items[2].Clone);
      try
        _p.TagPopulation := 512;
        FReader.AddAntenna(2, 27, _p);
      finally
        _p.Free;
      end;
    finally
      _presets.Free; { AddAntenna ha già clonato: si può liberare subito }
    end;

    FReader.StartInventory;

  ESEMPIO — CONNESSIONE
  ─────────────────────────────────────────────────────────────
  Il reader fa da server WebSocket: il Manager si collega a lui.
  Nessuna infrastruttura esterna richiesta oltre alla connessione
  di rete verso il reader.

    FReader := TFXR90Manager.Create('169.254.10.1', 443, 'admin', 'password');
    try
      FReader.ConfigureWebSocket('169.254.10.1', 443, '/ws', True, False);
      { host, porta, path (fisso: '/ws', vedi doc IoT Connector),
        AUseSSL=True -> wss://, AVerifyCert=False (certificato self-signed) }

      _presets := TFXR90RFPresets.Create;
      try
        FReader.AddAntenna(1, 29, _presets.Items[0]);
      finally
        _presets.Free;
      end;

      if FReader.Connect then
        FReader.StartInventory
      else
        LBLogger.Write(1, 'Main', lmt_Error, FReader.LastError);
    finally
      { FReader.Free chiude la connessione e ferma i thread interni }
    end;

  NORMALIZZAZIONE RSSI
  ─────────────────────────────────────────────────────────────
  L'FXR90 restituisce peakRssi in dBm interi (es. -52).
  TRFIDAntennaInfo.PeakRSSI è in centesimi di dBm (es. -5200).
  La conversione × 100 avviene in ProcessJSONObject, in modo che
  TRFIDTagData.Antennas sia uniforme tra driver Impinj e Zebra.
  ============================================================ }

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  fphttpclient,
  opensslsockets,
  DateUtils,
  blcksock,
  uTimedoutCriticalSection,
  uLBLogger,
  uRFIDCommon,
  uLBBaseThread,
  uLBWebSocketClient;

type

  TFXR90ConnectionState =
    (
    fxcsDisconnected = 0,
    fxcsConnecting = 1,
    fxcsConnected = 2,
    fxcsError = 3
    );

  TFXR90AntennaStopConditionType =
    (
    fxascDuration,
    fxascGPI,
    fxascTagCount,
    fxascDisabled
    );

  TFXR90DelayType =
    (
    fxdtDisabled,
    fxdtFixed
    );

  TFXR90ReportFilterType =
    (
    fxrfDisabled,
    fxrfPerAntenna
    );

  { TFXR90Environment
    Parametro "environment" del reader.
    Se omesso, il device usa HIGH_INTERFERENCE di default. }

  TFXR90Environment =
    (
    fxeHighInterference,      // Più reader nella stessa area (default device)
    fxeLowInterference,       // Reader singolo, interferenza improbabile
    fxeVeryHighInterference,  // Reader > canali disponibili, o molto vicini
    fxeAutoDetect,            // Il reader valuta da solo l'ambiente
    fxeDemo                   // Massime prestazioni, nessun altro reader
    );

  TFXR90Session = (fxsS0, fxsS1, fxsS2, fxsS3);
  TFXR90Target = (fxtA, fxtB);

  { TFXR90RFPreset
    Preset RF specifico dell'FXR90. Estende TRFIDAntennaPreset con i
    parametri documentati dall'API Zebra ZIoTC.
    Clone() restituisce una copia indipendente dello stesso tipo. }

  TFXR90RFPreset = class(TRFIDAntennaPreset)
  public
    Environment: TFXR90Environment;
    Session: TFXR90Session;
    Target: TFXR90Target;
    TagPopulation: integer;

    { Intervallo di aggregazione e invio all'endpoint in millisecondi.
      Il reader accumula le letture per questo periodo e poi fa POST.
      -1 = disabilitato (POST immediato per ogni tag, massima latenza minima).
      Default: 1000 ms (un POST al secondo). }
    ReportingIntervalMs: integer;

    { Parametri di stop condition dell'antenna — opzionali, raramente
      modificati rispetto ai default. }
    StopConditionType: TFXR90AntennaStopConditionType;
    StopConditionValue: integer;
    LinkProfile: integer;

    function Clone: TRFIDAntennaPreset; override;
  end;

  { TFXR90RFPresets
    Collezione di preset predefiniti per scenari tipici.
    Creata e posseduta da TFXR90Manager (FPresets). }

  TFXR90RFPresets = class(TRFIDFAntennaPresets)
  strict private
    function MakePreset(AEnv: TFXR90Environment; ASession: TFXR90Session; ATarget: TFXR90Target; ATagPop: integer; const ADesc: string): TFXR90RFPreset;
  public
    constructor Create; override;
  end;

  { TFXR90RESTClient }

  TFXR90RESTClient = class(TObject)
  strict private
    FHost: string;
    FPort: word;
    FUseHTTPS: boolean;
    FUsername: string;
    FPassword: string;

    FJWTToken: string;
    FRole: string;
    FUserID: string;

    FHTTPClient: TFPHTTPClient;

    FLoginPath: string;
    FVersionPath: string;
    FModePath: string;
    FStartInventoryPath: string;
    FStopInventoryPath: string;

    FEnvironment: TFXR90Environment;

    function BuildBaseURL: string;
    function BuildURL(const APath: string): string;
    function NewReqToken: string;

    function StopConditionToStr(AValue: TFXR90AntennaStopConditionType): string;
    function DelayTypeToStr(AValue: TFXR90DelayType): string;
    function ReportFilterToStr(AValue: TFXR90ReportFilterType): string;
    function EnvironmentToStr(AValue: TFXR90Environment): string;
    function SessionToStr(AValue: TFXR90Session): string;
    function TargetToStr(AValue: TFXR90Target): string;

    function DoRequest(const AMethod, APath, ABody: string; out AResponse: string): boolean;

    function BuildLoginBody: string;
    function ParseLoginResponse(const AResponse: string; out AError: string): boolean;
    function Login(out AResponse: string): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearSession;

    function Connect(out AResponse: string): boolean;
    function Disconnect(out AResponse: string): boolean;
    function GetVersion(out AResponse: string): boolean;
    function GetMode(out AResponse: string): boolean;

    { Invia PUT /cloud/mode con la configurazione delle antenne.
      APorts, ADBms, APresets sono array paralleli della stessa lunghezza. }
    function ConfigureMode(const APorts: array of integer; const ADBms: array of integer; const APresets: array of TFXR90RFPreset;
      const ATagMetaData: array of string; ADelayType: TFXR90DelayType; ADelayDuration: integer;
      AReportFilter: TFXR90ReportFilterType; AReportFilterDuration: integer; out AResponse: string): boolean;

    function StartInventory(out AResponse: string): boolean;
    function StopInventory(out AResponse: string): boolean;

    property Host: string read FHost write FHost;
    property Port: word read FPort write FPort;
    property UseHTTPS: boolean read FUseHTTPS write FUseHTTPS;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;

    property JWTToken: string read FJWTToken;
    property Role: string read FRole;
    property UserID: string read FUserID;

    property LoginPath: string read FLoginPath write FLoginPath;
    property VersionPath: string read FVersionPath write FVersionPath;
    property ModePath: string read FModePath write FModePath;
    property StartInventoryPath: string read FStartInventoryPath write FStartInventoryPath;
    property StopInventoryPath: string read FStopInventoryPath write FStopInventoryPath;

    { Environment applicato all'intera sessione (non per-antenna). }
    property Environment: TFXR90Environment read FEnvironment write FEnvironment;
  end;

  { TFXR90Consumer }

  TFXR90Consumer = class(TLBBaseThread)
  strict private
    FTagList: TRFIDTagList;
    FMessageQueue: TStringList;
    FMessageCS: TTimedOutCriticalSection;
    FNewDataEvent: PRTLEvent;

    procedure ProcessAssembledMessage(const APayload: ansistring);
    function ProcessJSONPayload(const APayload: ansistring): boolean;
    function ProcessJSONObject(AData: TJSONData): boolean;
    function ParseTimestamp(const S: string; out ADateTime: TDateTime): boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(ATagList: TRFIDTagList); reintroduce;
    destructor Destroy; override;

    procedure Terminate; override;

    { Thread-safe: chiamato dal thread WebSocket, ritorna subito. }
    procedure NewMessageReceived(Sender: TObject; const aPayload: ansistring);
  end;

  { TFXR90Manager }

  TFXR90Manager = class(TRFIDManager)
  strict private
  const
    cMaxAssemblySize = int64(32 * 1024 * 1024);  // 32 MB tetto riassemblaggio

  strict private
    FRESTClient: TFXR90RESTClient;
    FWebSocketClient: TLBWebSocketClient;
    FConsumer: TFXR90Consumer;

    FState: TFXR90ConnectionState;
    FLastError: string;

    { Configurazione antenne: tre array paralleli, stessa lunghezza. }
    FAntennaPort: array of integer;
    FAntennaDBm: array of integer;
    FAntennaPreset: array of TFXR90RFPreset;  // copie interne, possedute dal manager

    { Tag metadata inviato a PUT /cloud/mode. }
    FTagMetaData: array of string;

    { WebSocket }
    FWebSocketHost: string;
    FWebSocketPort: integer;
    FWebSocketUseSSL: boolean;
    FWebSocketVerifyCert: boolean;
    FWebSocketURI: ansistring;

    { Riassemblaggio frame WebSocket (continuation frames). }
    FAssemblyCS: TTimedOutCriticalSection;
    FAssembly: TMemoryStream;
    FAssemblyType: TWebSocketFrameType;

    function get_Connected: boolean;
    function get_WebSocketConnected: boolean;
    procedure SetDefaultTagMetaData;
    procedure ResetAssembly;

    procedure HandleWebSocketConnected(Sender: TObject);
    procedure HandleWebSocketDisconnected(Sender: TObject);
    procedure HandleWebSocketRawMessage(Sender: TObject; isLastFrame: boolean; aDataType: TWebSocketFrameType; aBuffer: pbyte; aBufferLen: int64);

    function StartWebSocketClient: boolean;

    procedure ClearAntennaArrays;

  public
    constructor Create(const AHost: string; APort: word = 443; const AUsername: string = ''; const APassword: string = ''; AUseHTTPS: boolean = True); reintroduce;
    destructor Destroy; override;

    { Aggiunge una porta antenna.
      aDBm   : potenza TX in dBm interi (es. 17, 27, 30).
      aPreset: TRFIDAntennaPreset — deve essere un TFXR90RFPreset;
               viene clonato internamente, il chiamante può liberarlo subito. }
    procedure AddAntenna(aPort: integer; aDBm: integer; aPreset: TRFIDAntennaPreset); override;

    procedure ClearAntennas;

    { Configurazione della connessione WebSocket verso il reader. }
    procedure ConfigureWebSocket(const AHost: string; APort: integer; const AURI: ansistring = '/'; AUseSSL: boolean = False; AVerifyCert: boolean = False);

    function Connect: boolean;
    function Disconnect: boolean;
    function GetVersion(out AResponse: string): boolean;
    function GetMode(out AResponse: string): boolean;
    function ConfigureAntennas: boolean;

    function StartInventory: boolean; override;
    function StopInventory: boolean; override;

    property Connected: boolean read get_Connected;
    property WebSocketConnected: boolean read get_WebSocketConnected;
    property State: TFXR90ConnectionState read FState;
    property LastError: string read FLastError;
    property RESTClient: TFXR90RESTClient read FRESTClient;
    property WebSocketClient: TLBWebSocketClient read FWebSocketClient;
  end;

implementation

uses
  Math, StrUtils;

  { ============================================================ }
  { TFXR90RFPreset                                               }
  { ============================================================ }

function TFXR90RFPreset.Clone: TRFIDAntennaPreset;
var
  _c: TFXR90RFPreset;
begin
  _c := TFXR90RFPreset.Create;
  _c.Description := Self.Description;
  _c.Environment := Self.Environment;
  _c.Session := Self.Session;
  _c.Target := Self.Target;
  _c.TagPopulation := Self.TagPopulation;
  _c.ReportingIntervalMs := Self.ReportingIntervalMs;
  _c.StopConditionType := Self.StopConditionType;
  _c.StopConditionValue := Self.StopConditionValue;
  _c.LinkProfile := Self.LinkProfile;
  Result := _c;
end;

{ ============================================================ }
{ TFXR90RFPresets                                              }
{ ============================================================ }

function TFXR90RFPresets.MakePreset(AEnv: TFXR90Environment; ASession: TFXR90Session; ATarget: TFXR90Target; ATagPop: integer; const ADesc: string): TFXR90RFPreset;
begin
  Result := TFXR90RFPreset.Create;
  Result.Environment := AEnv;
  Result.Session := ASession;
  Result.Target := ATarget;
  Result.TagPopulation := ATagPop;
  Result.ReportingIntervalMs := 1000;
  Result.Description := ADesc;
  Result.StopConditionType := fxascDuration;
  Result.StopConditionValue := 100; // 100;
  Result.LinkProfile := 3;
end;

constructor TFXR90RFPresets.Create;
begin
  inherited Create;

  SetLength(FPresets, 8);
  { 0 } FPresets[0] := MakePreset(fxeLowInterference, fxsS0, fxtA, 32, 'Standard (single reader, general purpose)');
  { 1 } FPresets[1] := MakePreset(fxeLowInterference, fxsS0, fxtA, 64, 'Small Inventory (<100 tag)');
  { 2 } FPresets[2] := MakePreset(fxeHighInterference, fxsS2, fxtA, 600, 'Large Inventory (400-800 tag)');
  { 3 } FPresets[3] := MakePreset(fxeHighInterference, fxsS2, fxtA, 128, 'Dense Reader (multi-reader environment)');
  { 4 } FPresets[4] := MakePreset(fxeLowInterference, fxsS2, fxtA, 32, 'Monitoring (smart shelf / tool tracking)');
  { 5 } FPresets[5] := MakePreset(fxeLowInterference, fxsS1, fxtA, 32, 'High Sensitivity (difficult tags / long range)');
  { 6 } FPresets[6] := MakePreset(fxeDemo, fxsS0, fxtA, 32, 'Fast Scan (tags in motion / conveyor)');
  { 7 } FPresets[7] := MakePreset(fxeVeryHighInterference, fxsS3, fxtA, 128, 'Multi Reader (3+ readers, high tag density)');
end;

{ ============================================================ }
{ TFXR90RESTClient                                             }
{ ============================================================ }

constructor TFXR90RESTClient.Create;
begin
  inherited Create;
  FHost := '';
  FPort := 443;
  FUseHTTPS := True;
  FUsername := '';
  FPassword := '';
  FJWTToken := '';
  FRole := '';
  FUserID := '';

  FHTTPClient := TFPHTTPClient.Create(nil);
  FHTTPClient.AllowRedirect := True;

  FLoginPath := '/auth/login';
  FVersionPath := '/cloud/version';
  FModePath := '/cloud/mode';
  FStartInventoryPath := '/cloud/start';
  FStopInventoryPath := '/cloud/stop';

  FEnvironment := fxeHighInterference;
end;

destructor TFXR90RESTClient.Destroy;
begin
  try
    FreeAndNil(FHTTPClient);
  except
    on E: Exception do
      LBLogger.Write(1, 'TFXR90RESTClient.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

procedure TFXR90RESTClient.ClearSession;
begin
  FJWTToken := '';
  FRole := '';
  FUserID := '';
end;

function TFXR90RESTClient.BuildBaseURL: string;
begin
  if FUseHTTPS then
    Result := Format('https://%s:%d', [FHost, FPort])
  else
    Result := Format('http://%s:%d', [FHost, FPort]);
end;

function TFXR90RESTClient.BuildURL(const APath: string): string;
begin
  if Trim(APath) = '' then Result := BuildBaseURL
  else if APath[1] = '/' then Result := BuildBaseURL + APath
  else
    Result := BuildBaseURL + '/' + APath;
end;

function TFXR90RESTClient.NewReqToken: string;
begin
  Result := IntToStr(GetTickCount64);
end;

function TFXR90RESTClient.StopConditionToStr(AValue: TFXR90AntennaStopConditionType): string;
begin
  case AValue of
    fxascDuration: Result := 'DURATION';
    fxascGPI: Result := 'GPI';
    fxascTagCount: Result := 'TAG_COUNT';
    else
      Result := 'DISABLED';
  end;
end;

function TFXR90RESTClient.DelayTypeToStr(AValue: TFXR90DelayType): string;
begin
  if AValue = fxdtFixed then Result := 'FIXED'
  else
    Result := 'DISABLED';
end;

function TFXR90RESTClient.ReportFilterToStr(AValue: TFXR90ReportFilterType): string;
begin
  if AValue = fxrfPerAntenna then Result := 'PER_ANTENNA'
  else
    Result := 'DISABLED';
end;

function TFXR90RESTClient.EnvironmentToStr(AValue: TFXR90Environment): string;
begin
  case AValue of
    fxeLowInterference: Result := 'LOW_INTERFERENCE';
    fxeVeryHighInterference: Result := 'VERY_HIGH_INTERFERENCE';
    fxeAutoDetect: Result := 'AUTO_DETECT';
    fxeDemo: Result := 'DEMO';
    else
      Result := 'HIGH_INTERFERENCE';
  end;
end;

function TFXR90RESTClient.SessionToStr(AValue: TFXR90Session): string;
begin
  case AValue of
    fxsS0: Result := 'S0';
    fxsS2: Result := 'S2';
    fxsS3: Result := 'S3';
    else
      Result := 'S1';
  end;
end;

function TFXR90RESTClient.TargetToStr(AValue: TFXR90Target): string;
begin
  if AValue = fxtB then Result := 'B'
  else
    Result := 'A';
end;

function TFXR90RESTClient.DoRequest(const AMethod, APath, ABody: string; out AResponse: string): boolean;
var
  RS: TStringStream;
  RQ: TStringStream;
  URL: string;
begin
  Result := False;
  AResponse := '';
  RS := nil;
  RQ := nil;
  URL := BuildURL(APath);
  try
    FHTTPClient.RequestHeaders.Clear;
    FHTTPClient.RequestHeaders.Values['Accept'] := 'application/json';
    if FJWTToken <> '' then
      FHTTPClient.RequestHeaders.Values['Authorization'] := 'Bearer ' + FJWTToken;

    RS := TStringStream.Create('');
    if ABody <> '' then
    begin
      FHTTPClient.RequestHeaders.Values['Content-Type'] := 'application/json';
      RQ := TStringStream.Create(ABody);
      FHTTPClient.RequestBody := RQ;
    end
    else
      FHTTPClient.RequestBody := nil;

    LBLogger.Write(5, 'TFXR90RESTClient.DoRequest', lmt_Debug, '%s %s', [UpperCase(AMethod), URL]);
    if ABody <> '' then
      LBLogger.Write(5, 'TFXR90RESTClient.DoRequest', lmt_Debug, 'REQUEST BODY=%s', [ABody]);

    try
      FHTTPClient.HTTPMethod(UpperCase(AMethod), URL, RS, [200, 201, 202, 204]);
      AResponse := RS.DataString;
      Result := True;
      LBLogger.Write(5, 'TFXR90RESTClient.DoRequest', lmt_Debug,
        '%s %s OK - ResponseLen=%d', [UpperCase(AMethod), URL, Length(AResponse)]);
    except
      on E: EHTTPClient do
      begin
        AResponse := RS.DataString;
        LBLogger.Write(1, 'TFXR90RESTClient.DoRequest', lmt_Error,
          '%s %s failed. HTTP=%d BODY=%s',
          [UpperCase(AMethod), URL, E.StatusCode, AResponse]);
      end;
      on E: Exception do
      begin
        AResponse := E.Message;
        LBLogger.Write(1, 'TFXR90RESTClient.DoRequest', lmt_Error,
          '%s %s failed: %s', [UpperCase(AMethod), URL, E.Message]);
      end;
    end;
  finally
    FHTTPClient.RequestBody := nil;
    if RQ <> nil then
      RQ.Free;

    if RS <> nil then
      RS.Free;
  end;
end;

function TFXR90RESTClient.BuildLoginBody: string;
var
  Root, Hdr, Pay: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Hdr := TJSONObject.Create;
    Hdr.Add('RPC', 'login');
    Hdr.Add('MsgType', 'REQ');
    Hdr.Add('ReqToken', NewReqToken);
    Hdr.Add('Requester', 'web-hmi');
    Hdr.Add('Receiver', 'app-mngr');
    Pay := TJSONObject.Create;
    Pay.Add('user', FUsername);
    Pay.Add('password', FPassword);
    Root.Add('header', Hdr);
    Root.Add('payload', Pay);
    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

function TFXR90RESTClient.ParseLoginResponse(const AResponse: string; out AError: string): boolean;
var
  Data: TJSONData;
  RootObj: TJSONObject;
  HdrObj: TJSONObject;
  PayObj: TJSONObject;
  HdrData: TJSONData;
  PayData: TJSONData;
begin
  Result := False;
  AError := '';
  ClearSession;
  if Trim(AResponse) = '' then
  begin
    AError := 'Empty login response';
    Exit;
  end;
  Data := nil;
  try
    Data := GetJSON(AResponse);
    if (Data = nil) or (Data.JSONType <> jtObject) then
    begin
      AError := 'Login response is not a JSON object';
      Exit;
    end;
    RootObj := TJSONObject(Data);
    HdrData := RootObj.Find('header');
    if (HdrData = nil) or (HdrData.JSONType <> jtObject) then
    begin
      AError := 'Login header not found';
      Exit;
    end;
    HdrObj := TJSONObject(HdrData);
    if UpperCase(HdrObj.Get('Status', '')) <> 'SUCCESS' then
    begin
      AError := 'Login status: ' + HdrObj.Get('Status', '');
      Exit;
    end;
    PayData := RootObj.Find('payload');
    if (PayData = nil) or (PayData.JSONType <> jtObject) then
    begin
      AError := 'Login payload not found';
      Exit;
    end;
    PayObj := TJSONObject(PayData);
    FJWTToken := PayObj.Get('token', '');
    FRole := PayObj.Get('role', '');
    FUserID := PayObj.Get('id', '');
    if FJWTToken = '' then
    begin
      AError := 'JWT token missing';
      Exit;
    end;
    Result := True;
  except
    on E: Exception do AError := 'Parse error: ' + E.Message;
  end;
  if Data <> nil then Data.Free;
end;

function TFXR90RESTClient.Login(out AResponse: string): boolean;
var
  _err: string;
begin
  Result := False;
  AResponse := '';
  Self.ClearSession;
  if Trim(FHost) = '' then
  begin
    AResponse := 'Host not configured';
    Exit;
  end;
  if Trim(FUsername) = '' then
  begin
    AResponse := 'Username not configured';
    Exit;
  end;
  if not DoRequest('POST', FLoginPath, BuildLoginBody, AResponse) then Exit;
  Result := ParseLoginResponse(AResponse, _err);
  if Result then
    LBLogger.Write(5, 'TFXR90RESTClient.Login', lmt_Debug, 'Login SUCCESS. Role=%s UserID=%s TokenLen=%d', [FRole, FUserID, Length(FJWTToken)])
  else begin
    if _err <> '' then AResponse := _err;
    LBLogger.Write(1, 'TFXR90RESTClient.Login', lmt_Error, AResponse);
  end;
end;

function TFXR90RESTClient.Connect(out AResponse: string): boolean;
begin
  Result := Login(AResponse);
end;

function TFXR90RESTClient.Disconnect(out AResponse: string): boolean;
begin
  Self.ClearSession;
  AResponse := 'Local FXR90 session cleared';
  Result := True;
  LBLogger.Write(5, 'TFXR90RESTClient.Disconnect', lmt_Debug, AResponse);
end;

function TFXR90RESTClient.GetVersion(out AResponse: string): boolean;
begin
  if FJWTToken = '' then
  begin
    AResponse := 'Not authenticated';
    Result := False;
    Exit;
  end;
  Result := DoRequest('GET', FVersionPath, '', AResponse);
end;

function TFXR90RESTClient.GetMode(out AResponse: string): boolean;
begin
  if FJWTToken = '' then
  begin
    AResponse := 'Not authenticated';
    Result := False;
    Exit;
  end;
  Result := DoRequest('GET', FModePath, '', AResponse);
end;

function TFXR90RESTClient.ConfigureMode(const APorts: array of integer; const ADBms: array of integer; const APresets: array of TFXR90RFPreset; const ATagMetaData: array of string;
  ADelayType: TFXR90DelayType; ADelayDuration: integer; AReportFilter: TFXR90ReportFilterType; AReportFilterDuration: integer; out AResponse: string): boolean;
var
  Obj: TJSONObject;
  Antennas: TJSONArray;
  MetaData: TJSONArray;
  TxPowers: TJSONArray;
  StopConds: TJSONArray;
  Queries: TJSONArray;
  StopCond: TJSONObject;
  QueryObj: TJSONObject;
  DelayConfig: TJSONObject;
  ReportFilt: TJSONObject;
  Batching: TJSONObject;
  Body: string;
  N, i: integer;
  Preset: TFXR90RFPreset;
begin
  Result := False;
  if FJWTToken = '' then
  begin
    AResponse := 'Not authenticated';
    Exit;
  end;
  N := Length(APorts);
  if N = 0 then
  begin
    AResponse := 'No antenna configured';
    Exit;
  end;

  Obj := TJSONObject.Create;
  try
    Antennas := TJSONArray.Create;
    MetaData := TJSONArray.Create;
    TxPowers := TJSONArray.Create;
    StopConds := TJSONArray.Create;
    Queries := TJSONArray.Create;

    for i := 0 to N - 1 do
    begin
      Antennas.Add(APorts[i]);
      TxPowers.Add(ADBms[i]);
      Preset := APresets[i];
      StopCond := TJSONObject.Create;
      StopCond.Add('type', StopConditionToStr(Preset.StopConditionType));
      StopCond.Add('value', Preset.StopConditionValue);
      StopConds.Add(StopCond);
      QueryObj := TJSONObject.Create;
      QueryObj.Add('sel', 'ALL');
      QueryObj.Add('session', SessionToStr(Preset.Session));
      QueryObj.Add('tagPopulation', Preset.TagPopulation);
      QueryObj.Add('target', TargetToStr(Preset.Target));
      Queries.Add(QueryObj);
    end;

    for i := 0 to High(ATagMetaData) do
      if Trim(ATagMetaData[i]) <> '' then MetaData.Add(ATagMetaData[i]);

    DelayConfig := TJSONObject.Create;
    DelayConfig.Add('duration', ADelayDuration);
    DelayConfig.Add('type', DelayTypeToStr(ADelayType));

    ReportFilt := TJSONObject.Create;
    ReportFilt.Add('duration', AReportFilterDuration);
    ReportFilt.Add('type', ReportFilterToStr(AReportFilter));

    Obj.Add('antennas', Antennas);
    Obj.Add('delayBetweenAntennaCycles', DelayConfig);
    Obj.Add('reportFilter', ReportFilt);
    Obj.Add('tagMetaData', MetaData);
    Obj.Add('type', 'CUSTOM');

    { L'API accetta singolo oggetto o array in base al numero di antenne. }
    if N = 1 then
    begin
      Obj.Add('antennaStopCondition', TJSONObject(StopConds.Items[0]));
      Obj.Add('query', TJSONObject(Queries.Items[0]));
      Obj.Add('transmitPower', TxPowers);
      Obj.Add('linkProfile', APresets[0].LinkProfile);
      { Rimuovi dai rispettivi array per evitare double-free }
      StopConds.Extract(StopConds.Items[0]);
      Queries.Extract(Queries.Items[0]);
      StopConds.Free;
      Queries.Free;
    end
    else
    begin
      Obj.Add('antennaStopCondition', StopConds);
      Obj.Add('query', Queries);
      Obj.Add('transmitPower', TxPowers);
    end;

    { batching: usa il ReportingIntervalMs del primo preset abilitato.
      Tutti i preset nella stessa sessione condividono lo stesso intervallo.
      -1 = nessuna aggregazione: il reader fa POST ad ogni singola lettura,
           minimizzando la latenza a scapito del numero di connessioni HTTP.
      0   = aggregazione senza limite di tempo (POST solo al raggiungimento
           di maxPayloadSizePerReport).
      >0  = intervallo in ms tra un POST e il successivo (default 1000 = 1s). }
    Batching := TJSONObject.Create;
    Batching.Add('reportingInterval', APresets[0].ReportingIntervalMs);
    Batching.Add('maxPayloadSizePerReport', 256000);
    Obj.Add('batching', Batching);

    Body := Obj.AsJSON;
  finally
    Obj.Free;
  end;

  LBLogger.Write(5, 'TFXR90RESTClient.ConfigureMode', lmt_Debug, 'MODE REQUEST=%s', [Body]);
  Result := DoRequest('PUT', FModePath, Body, AResponse);
end;

function TFXR90RESTClient.StartInventory(out AResponse: string): boolean;
begin
  Self.StopInventory(AResponse);

  if FJWTToken = '' then
  begin
    AResponse := 'Not authenticated';
    Exit(False);
  end
  else
    Result := DoRequest('PUT', FStartInventoryPath, '{}', AResponse);
end;

function TFXR90RESTClient.StopInventory(out AResponse: string): boolean;
begin
  if FJWTToken = '' then
  begin
    AResponse := 'Not authenticated';
    Exit(False);
  end
  else
    Result := DoRequest('PUT', FStopInventoryPath, '{}', AResponse);
end;

{ ============================================================ }
{ TFXR90Consumer                                               }
{ ============================================================ }

constructor TFXR90Consumer.Create(ATagList: TRFIDTagList);
begin
  inherited Create;
  FreeOnTerminate := False;
  FTagList := ATagList;
  FMessageCS := TTimedOutCriticalSection.Create;
  FMessageQueue := TStringList.Create;
  FNewDataEvent := RTLEventCreate();
end;

destructor TFXR90Consumer.Destroy;
begin
  inherited Destroy;
  FMessageQueue.Free;
  FMessageCS.Free;
  RTLEventDestroy(FNewDataEvent);
end;

procedure TFXR90Consumer.Terminate;
begin
  inherited Terminate;
  RTLEventSetEvent(FNewDataEvent);
end;

procedure TFXR90Consumer.NewMessageReceived(Sender: TObject; const aPayload: ansistring);
begin
  if aPayload = '' then
    Exit;

  if FMessageCS.Acquire('TFXR90Consumer.NewMessageReceived') then
  begin
    try
      FMessageQueue.Add(aPayload);
    finally
      FMessageCS.Release;
    end;
    RTLEventSetEvent(FNewDataEvent);
  end;
end;

function TFXR90Consumer.ParseTimestamp(const S: string; out ADateTime: TDateTime): boolean;
var
  T: string;
  Y, M, D, HH, NN, SS, MS: word;
begin
  Result := False;
  ADateTime := 0;
  T := Trim(S);
  if Length(T) < 23 then
    Exit;

  try
    Y := StrToInt(Copy(T, 1, 4));
    M := StrToInt(Copy(T, 6, 2));
    D := StrToInt(Copy(T, 9, 2));
    HH := StrToInt(Copy(T, 12, 2));
    NN := StrToInt(Copy(T, 15, 2));
    SS := StrToInt(Copy(T, 18, 2));
    MS := StrToInt(Copy(T, 21, 3));
    ADateTime := EncodeDate(Y, M, D) + EncodeTime(HH, NN, SS, MS);
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LBLogger.Write(1, 'TFXR90Consumer.ParseTimestamp', lmt_Error, E.Message);
    end;
  end;
end;

function TFXR90Consumer.ProcessJSONObject(AData: TJSONData): boolean;
var
  RootObj: TJSONObject;
  DataNode: TJSONData;
  DataObj: TJSONObject;
  EPC: string;
  MsgType: string;
  SeenAt: TDateTime;
  Antenna: integer;
  PeakRSSI: smallint;   // centesimi di dBm dopo conversione
  IsNew: boolean;
  Tag: TRFIDTag;
begin
  Result := False;
  if (AData = nil) or (FTagList = nil) then Exit;
  if AData.JSONType <> jtObject then Exit;
  RootObj := TJSONObject(AData);

  MsgType := UpperCase(RootObj.Get('type', ''));
  if (MsgType <> '') and (MsgType <> 'CUSTOM') then
  begin
    Result := True;  // altri tipi (heartbeat, ecc.) ignorati in modo esplicito
    Exit;
  end;

  DataNode := RootObj.Find('data');
  if (DataNode = nil) or (DataNode.JSONType <> jtObject) then Exit;
  DataObj := TJSONObject(DataNode);

  EPC := UpperCase(Trim(DataObj.Get('idHex', '')));
  if EPC = '' then
  begin
    LBLogger.Write(1, 'TFXR90Consumer.ProcessJSONObject', lmt_Warning, 'Missing idHex: %s', [Copy(AData.AsJSON, 1, 200)]);
    Exit;
  end;

  if not ParseTimestamp(RootObj.Get('timestamp', ''), SeenAt) then
    SeenAt := Now;

  Antenna := DataObj.Get('antenna', 0);
  if Antenna < 0 then Antenna := 0
  else if Antenna > 255 then Antenna := 255;

  { Normalizzazione: FXR90 fornisce dBm interi, TRFIDAntennaInfo usa centesimi }
  PeakRSSI := smallint(DataObj.Get('peakRssi', 0)) * 100;

  Tag := FTagList.GetOrCreate(EPC, byte(Antenna), PeakRSSI, SeenAt, IsNew);
  Result := Tag <> nil;

  if Result then
    LBLogger.Write(6, 'TFXR90Consumer.ProcessJSONObject', lmt_Debug, 'Tag EPC=%s Ant=%d RSSI=%.2fdBm New=%s', [EPC, Antenna, PeakRSSI / 100.0, BoolToStr(IsNew, True)]);
end;

function TFXR90Consumer.ProcessJSONPayload(const APayload: ansistring): boolean;
var
  S: string;
  Data: TJSONData;
  Arr: TJSONArray;
  i: integer;
  Obj: TJSONObject;
  Extracted: integer;
  Scan: string;
  p, Braces: integer;
  ObjStr: string;
begin
  Result := False;
  S := Trim(string(APayload));
  if S = '' then Exit;

  Extracted := 0;

  // Controllo veloce: se contiene '}{' è certamente concatenato
  if Pos('}{', S) = 0 then
  begin
    // --- TENTATIVO 1: parsing JSON diretto (singolo oggetto o array) ---
    try
      Data := GetJSON(S);
      if Data <> nil then
      begin
        try
          case Data.JSONType of
            jtObject:
              begin
                if Self.ProcessJSONObject(Data) then
                  Inc(Extracted);
              end;

            jtArray:
              begin
                Arr := TJSONArray(Data);
                for i := 0 to Arr.Count - 1 do
                begin
                  Obj := Arr.Objects[i];
                  if Obj <> nil then
                    if Self.ProcessJSONObject(Obj) then Inc(Extracted);
                end;
              end;
            else
              LBLogger.Write(1, 'TFXR90Consumer.ProcessJSONPayload', lmt_Warning, 'Unexpected JSON root type: %d', [Ord(Data.JSONType)]);
          end;
        finally
          Data.Free;
        end;
        if Extracted > 0 then
          LBLogger.Write(5, 'TFXR90Consumer.ProcessJSONPayload', lmt_Debug, 'Parsed as JSON: %d object(s)', [Extracted]);
        Result := Extracted > 0;
        Exit;
      end;
    except
      on E: Exception do
        LBLogger.Write(6, 'TFXR90Consumer.ProcessJSONPayload',
          lmt_Debug, 'Direct JSON parsing failed: %s', [E.Message]);
    end;
  end;

  // --- TENTATIVO 2: scansione manuale per oggetti concatenati ---
  Scan := S;
  p := 1;
  while p <= Length(Scan) do
  begin
    // Salta spazi bianchi e caratteri non validi
    while (p <= Length(Scan)) and (Scan[p] in [#32, #9, #10, #13]) do Inc(p);
    if p > Length(Scan) then Break;
    if Scan[p] <> '{' then
    begin
      Inc(p);
      Continue;
    end;

    Braces := 0;
    ObjStr := '';
    while p <= Length(Scan) do
    begin
      ObjStr := ObjStr + Scan[p];
      if Scan[p] = '{' then
        Inc(Braces)
      else if Scan[p] = '}' then
      begin
        Dec(Braces);
        if Braces = 0 then
        begin
          // Oggetto completo
          try
            Data := GetJSON(ObjStr);
            if Data <> nil then
            begin
              try
                if Data.JSONType = jtObject then
                  if Self.ProcessJSONObject(Data) then
                    Inc(Extracted);
              finally
                Data.Free;
              end;
            end;
          except
            on E: Exception do
              LBLogger.Write(1, 'TFXR90Consumer.ProcessJSONPayload',
                lmt_Error, 'Error parsing scanned object: %s', [E.Message]);
          end;
          Inc(p);
          Break;
        end;
      end;
      Inc(p);
    end;
  end;

  if Extracted > 0 then
    LBLogger.Write(5, 'TFXR90Consumer.ProcessJSONPayload', lmt_Debug, 'Extracted %d object(s) via scan from concatenated payload', [Extracted]);

  Result := Extracted > 0;
end;

(*
function TFXR90Consumer.ProcessJSONPayload(const APayload: ansistring): boolean;
var
  Data: TJSONData;
  Arr: TJSONArray;
  i, N: integer;
begin
  Result := False;

  if Trim(string(APayload)) = '' then
    Exit;

  Data := nil;
  N := 0;
  try
    Data := GetJSON(string(APayload));
    if Data = nil then
      Exit;

    case Data.JSONType of
      jtObject:
        if Self.ProcessJSONObject(Data) then Inc(N);

      jtArray:
      begin
        Arr := TJSONArray(Data);
        for i := 0 to Arr.Count - 1 do
          if (Arr.Items[i] <> nil) and Self.ProcessJSONObject(Arr.Items[i]) then
            Inc(N);
      end;
      else
        LBLogger.Write(1, 'TFXR90Consumer.ProcessJSONPayload', lmt_Warning, 'Unsupported JSON root type=%d', [Ord(Data.JSONType)]);
    end;
    LBLogger.Write(5, 'TFXR90Consumer.ProcessConcatenatedJSONObje', lmt_Debug, 'Extracted %d JSON object(s) from assembled message len=%d', [N, Length(APayload)]);
    Result := N > 0;
  except
    on E: Exception do
      LBLogger.Write(1, 'TFXR90Consumer.ProcessJSONPayload', lmt_Error, '%s | DATA=%s', [E.Message, Copy(string(APayload), 1, 200)]);
  end;

  if Data <> nil then
    Data.Free;
end;
*)

procedure TFXR90Consumer.ProcessAssembledMessage(const APayload: ansistring);
begin
  if APayload = '' then Exit;
  LBLogger.Write(5, 'TFXR90Consumer.ProcessAssembledMessage', lmt_Debug, 'Message Len=%d  -  %s', [Length(APayload), APayload]);
  Self.ProcessJSONPayload(APayload);
end;

procedure TFXR90Consumer.Execute;
var
  _payload: ansistring;
  _batch: TStringList;
begin
  Self.setThreadName('FXR90Consumer');
  while not Self.Terminated do
  begin
    _batch := nil;
    if FMessageCS.Acquire('TFXR90Consumer.Execute') then
    try
      if FMessageQueue.Count > 0 then
      begin
        _batch := FMessageQueue;
        FMessageQueue := TStringList.Create;
      end;
    finally
      FMessageCS.Release;
    end;

    if _batch <> nil then
    begin
      try

        for _payload in _batch do
        begin
          if not Self.Terminated then
            Self.ProcessAssembledMessage(_payload);
        end;

      finally
        FreeAndNil(_batch);
      end;
    end
    else
      RTLEventWaitFor(FNewDataEvent, 3000);
  end;
end;

{ ============================================================ }
{ TFXR90Manager                                                }
{ ============================================================ }

constructor TFXR90Manager.Create(const AHost: string; APort: word; const AUsername, APassword: string; AUseHTTPS: boolean);
begin
  inherited Create;

  FReaderType := rrtZebraFXR90;

  FState := fxcsDisconnected;
  FLastError := '';

  FRESTClient := TFXR90RESTClient.Create;
  FRESTClient.Host := AHost;
  FRESTClient.Port := APort;
  FRESTClient.Username := AUsername;
  FRESTClient.Password := APassword;
  FRESTClient.UseHTTPS := AUseHTTPS;

  FWebSocketClient := nil;
  FConsumer := nil;

  SetLength(FAntennaPort, 0);
  SetLength(FAntennaDBm, 0);
  SetLength(FAntennaPreset, 0);

  SetDefaultTagMetaData;

  FWebSocketHost := AHost;
  FWebSocketPort := IfThen(AUseHTTPS, 443, 80);
  FWebSocketUseSSL := AUseHTTPS;
  FWebSocketVerifyCert := False;
  FWebSocketURI := '/ws';

  FAssemblyCS := TTimedOutCriticalSection.Create;
  FAssembly := TMemoryStream.Create;
  FAssemblyType := wsFrame_Text;
end;

destructor TFXR90Manager.Destroy;
begin
  try
    Self.StopInventory;
    Self.Disconnect;
    Self.ClearAntennaArrays;

    if FAssemblyCS.Acquire('TFXR90Manager.Destroy') then
    begin
      try
        FreeAndNil(FAssembly);

      except
        on E: Exception do
          LBLogger.Write(1, 'TFXR90Manager.Destroy', lmt_Error, E.Message);
      end;
      FAssemblyCS.Release;
    end;
    FreeAndNil(FAssemblyCS);

    FreeAndNil(FRESTClient);
  except
    on E: Exception do
      LBLogger.Write(1, 'TFXR90Manager.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

procedure TFXR90Manager.SetDefaultTagMetaData;
begin
  SetLength(FTagMetaData, 8);
  FTagMetaData[0] := 'ANTENNA';
  FTagMetaData[1] := 'RSSI';
  FTagMetaData[2] := 'SEEN_COUNT';
  FTagMetaData[3] := 'CHANNEL';
  FTagMetaData[4] := 'PC';
  FTagMetaData[5] := 'XPC';
  FTagMetaData[6] := 'CRC';
  FTagMetaData[7] := 'PHASE';
end;

procedure TFXR90Manager.ClearAntennaArrays;
var
  i: integer;
begin
  for i := 0 to High(FAntennaPreset) do
  begin
    if FAntennaPreset[i] <> nil then
      FreeAndNil(FAntennaPreset[i]);
  end;

  SetLength(FAntennaPort, 0);
  SetLength(FAntennaDBm, 0);
  SetLength(FAntennaPreset, 0);
end;

procedure TFXR90Manager.ClearAntennas;
begin
  Self.ClearAntennaArrays;
end;

procedure TFXR90Manager.AddAntenna(aPort: integer; aDBm: integer; aPreset: TRFIDAntennaPreset);
var
  _p: TFXR90RFPreset;
  _idx: integer;
  i: integer;
begin
  if not (aPreset is TFXR90RFPreset) then
  begin
    LBLogger.Write(1, 'TFXR90Manager.AddAntenna', lmt_Error, 'Preset must be a TFXR90RFPreset (port=%d)', [aPort]);
    Exit;
  end;

  { Rimuovi eventuale configurazione precedente per la stessa porta }
  for i := High(FAntennaPort) downto 0 do
  begin
    if FAntennaPort[i] = aPort then
    begin
      FreeAndNil(FAntennaPreset[i]);
      Delete(FAntennaPort, i, 1);
      Delete(FAntennaDBm, i, 1);
      Delete(FAntennaPreset, i, 1);
      Break;
    end;
  end;

  _p := TFXR90RFPreset(aPreset.Clone);
  _idx := Length(FAntennaPort);

  SetLength(FAntennaPort, _idx + 1);
  SetLength(FAntennaDBm, _idx + 1);
  SetLength(FAntennaPreset, _idx + 1);

  FAntennaPort[_idx] := aPort;
  FAntennaDBm[_idx] := aDBm;
  FAntennaPreset[_idx] := _p;

  { Environment: l'ultimo preset aggiunto determina l'environment globale }
  if FRESTClient <> nil then
    FRESTClient.Environment := _p.Environment;

  LBLogger.Write(5, 'TFXR90Manager.AddAntenna', lmt_Debug, 'Antenna %d added: %d dBm [%s]', [aPort, aDBm, _p.Description]);
end;

procedure TFXR90Manager.ConfigureWebSocket(const AHost: string; APort: integer; const AURI: ansistring; AUseSSL: boolean; AVerifyCert: boolean);
begin
  FWebSocketHost := AHost;
  FWebSocketPort := APort;
  FWebSocketURI := AURI;
  FWebSocketUseSSL := AUseSSL;
  FWebSocketVerifyCert := AVerifyCert;
end;


function TFXR90Manager.get_Connected: boolean;
begin
  Result := FState = fxcsConnected;
end;

function TFXR90Manager.get_WebSocketConnected: boolean;
begin
  Result := (FWebSocketClient <> nil) and FWebSocketClient.isConnected;
end;

procedure TFXR90Manager.ResetAssembly;
begin
  if FAssembly <> nil then
  begin
    FAssembly.Clear;
    FAssembly.Position := 0;
  end;
end;

procedure TFXR90Manager.HandleWebSocketConnected(Sender: TObject);
begin
  LBLogger.Write(5, 'TFXR90Manager.HandleWebSocketConnected', lmt_Debug, 'FXR90 WebSocket connected');
end;

procedure TFXR90Manager.HandleWebSocketDisconnected(Sender: TObject);
begin
  LBLogger.Write(5, 'TFXR90Manager.HandleWebSocketDisconnected', lmt_Debug, 'FXR90 WebSocket disconnected');
  if FAssemblyCS.Acquire('TFXR90Manager.HandleWebSocketDisconnected') then
  begin
    try
      Self.ResetAssembly;
    except
      on E: Exception do
        LBLogger.Write(1, 'TFXR90Manager.HandleWebSocketDisconnected', lmt_Error, E.Message);
    end;
    FAssemblyCS.Release;
  end;
end;

procedure TFXR90Manager.HandleWebSocketRawMessage(Sender: TObject; isLastFrame: boolean; aDataType: TWebSocketFrameType; aBuffer: pbyte; aBufferLen: int64);
var
  Assembled: ansistring;
  HasFull: boolean;
begin
  if not (aDataType in [wsFrame_Text, wsFrame_Binary]) then Exit;
  Assembled := '';
  HasFull := False;

  if FAssemblyCS.Acquire('TFXR90Manager.HandleWebSocketRawMessage') then
  begin

    try
      try
        if FAssembly = nil then Exit;
        if FAssembly.Size = 0 then FAssemblyType := aDataType;
        if (aBuffer <> nil) and (aBufferLen > 0) then
        begin
          if FAssembly.Size + aBufferLen > cMaxAssemblySize then
          begin
            LBLogger.Write(1, 'TFXR90Manager.HandleWebSocketRawMessage', lmt_Warning, 'Assembly overflow (%d). Dropping.', [FAssembly.Size + aBufferLen]);
            Self.ResetAssembly;
            Exit;
          end;
          FAssembly.Position := FAssembly.Size;
          FAssembly.WriteBuffer(aBuffer^, aBufferLen);
        end;
        if not isLastFrame then Exit;
        SetLength(Assembled, FAssembly.Size);
        if FAssembly.Size > 0 then
          Move(FAssembly.Memory^, Assembled[1], FAssembly.Size);
        Self.ResetAssembly;
        HasFull := True;
      except
        on E: Exception do
        begin
          LBLogger.Write(1, 'TFXR90Manager.HandleWebSocketRawMessage', lmt_Error, E.Message);
          Self.ResetAssembly;
        end;
      end;

    finally
      FAssemblyCS.Release;
    end;

    if HasFull and (FConsumer <> nil) then
      FConsumer.NewMessageReceived(Self, Assembled);

  end;
end;

function TFXR90Manager.StartWebSocketClient: boolean;
begin
  Result := False;
  if Trim(FWebSocketHost) = '' then
  begin
    Result := True;
    Exit;
  end;
  if FWebSocketPort <= 0 then
  begin
    LBLogger.Write(1, 'TFXR90Manager.StartWebSocketClient', lmt_Warning, 'WebSocket port not configured');
    Exit;
  end;

  if FWebSocketClient <> nil then
  begin
    FWebSocketClient.Disconnect;
    FreeAndNil(FWebSocketClient);
  end;

  if FConsumer <> nil then
    FreeAndNil(FConsumer);

  if FAssemblyCS.Acquire('TFXR90Manager.StartWebSocketClient') then
  begin
    try
      Self.ResetAssembly;
    except
      on E: Exception do
        LBLogger.Write(1, 'TFXR90Manager.StartWebSocketClient', lmt_Error, E.Message);
    end;
    FAssemblyCS.Release;
  end;

  FConsumer := TFXR90Consumer.Create(FTagList);
  FConsumer.Start;

  FWebSocketClient := TLBWebSocketClient.Create(128 * 1024, 1000, 30000);
  FWebSocketClient.RemoteConnectionData.Host := FWebSocketHost;
  FWebSocketClient.RemoteConnectionData.Port := FWebSocketPort;
  FWebSocketClient.RemoteConnectionData.UseSSL := FWebSocketUseSSL;
  FWebSocketClient.RemoteConnectionData.VerifyCert := FWebSocketVerifyCert;
  FWebSocketClient.URI := FWebSocketURI;

  FWebSocketClient.OnConnected := @Self.HandleWebSocketConnected;
  FWebSocketClient.OnDisconnected := @Self.HandleWebSocketDisconnected;
  FWebSocketClient.OnWebSocketMessage := @Self.HandleWebSocketRawMessage;
  FWebSocketClient.AutoPingEnabled := True;

  FWebSocketClient.Start;
  Result := True;
end;

function TFXR90Manager.Connect: boolean;
var
  Response: string;
begin
  Result := False;

  if FRESTClient = nil then
  begin
    LBLogger.Write(1, 'TFXR90Manager.Connect', lmt_Warning, 'REST client not created');
    FState := fxcsError;
    Exit;
  end;

  FState := fxcsConnecting;
  Result := FRESTClient.Connect(Response);

  if Result then
  begin
    FState := fxcsConnected;
    LBLogger.Write(5, 'TFXR90Manager.Connect', lmt_Debug,
      'FXR90 login completed. Role=%s UserID=%s TokenLen=%d',
      [FRESTClient.Role, FRESTClient.UserID, Length(FRESTClient.JWTToken)]);

    if not StartWebSocketClient then
    begin
      FState := fxcsError;
      Result := False;
    end;
  end
  else begin
    FState := fxcsError;
    LBLogger.Write(5, 'TFXR90Manager.Connect', lmt_Warning, Response);
  end;
end;

function TFXR90Manager.Disconnect: boolean;
var
  Response: string;
begin
  Result := True;

  if FWebSocketClient <> nil then
  begin
    try
      FWebSocketClient.Disconnect;
      FWebSocketClient.Terminate;
      FWebSocketClient.WaitFor;
    except
      on E: Exception do
        LBLogger.Write(1, 'TFXR90Manager.Disconnect', lmt_Error, E.Message);
    end;
    FreeAndNil(FWebSocketClient);
  end;
  if FConsumer <> nil then
  begin
    try
      FConsumer.Terminate;
      FConsumer.WaitFor;
    except
      on E: Exception do
        LBLogger.Write(1, 'TFXR90Manager.Disconnect', lmt_Error, E.Message);
    end;
    FreeAndNil(FConsumer);
  end;

  if FRESTClient <> nil then
    Result := FRESTClient.Disconnect(Response);

  FState := fxcsDisconnected;
  LBLogger.Write(5, 'TFXR90Manager.Disconnect', lmt_Debug, 'FXR90 disconnected');
end;

function TFXR90Manager.GetVersion(out AResponse: string): boolean;
begin
  Result := False;
  AResponse := '';
  if FState <> fxcsConnected then
  begin
    AResponse := 'FXR90 not connected';
    LBLogger.Write(1, 'TFXR90Manager.GetVersion', lmt_Warning, AResponse);
    Exit;
  end;
  Result := FRESTClient.GetVersion(AResponse);

  if not Result then
    LBLogger.Write(1, 'TFXR90Manager.GetVersion', lmt_Warning, AResponse);
end;

function TFXR90Manager.GetMode(out AResponse: string): boolean;
begin
  Result := False;
  AResponse := '';
  if FState <> fxcsConnected then
  begin
    AResponse := 'FXR90 not connected';
    LBLogger.Write(1, 'TFXR90Manager.GetMode', lmt_Warning, AResponse);
    Exit;
  end;
  Result := FRESTClient.GetMode(AResponse);

  if not Result then
    LBLogger.Write(1, 'TFXR90Manager.GetMode', lmt_Warning, AResponse);
end;

function TFXR90Manager.ConfigureAntennas: boolean;
var
  Response: string;
begin
  Result := False;

  if FState <> fxcsConnected then
  begin
    LBLogger.Write(1, 'TFXR90Manager.ConfigureAntennas', lmt_Warning, 'FXR90 not connected');
    Exit;
  end;
  if Length(FAntennaPort) = 0 then
  begin
    LBLogger.Write(1, 'TFXR90Manager.ConfigureAntennas', lmt_Warning, 'No antenna configured');
    Exit;
  end;

  Result := FRESTClient.ConfigureMode(FAntennaPort, FAntennaDBm, FAntennaPreset, FTagMetaData, fxdtDisabled, 0, fxrfPerAntenna, 0, Response);

  if Result then
    LBLogger.Write(5, 'TFXR90Manager.ConfigureAntennas', lmt_Debug, 'ConfigureAntennas OK (%d antenna/e)', [Length(FAntennaPort)])
  else
    LBLogger.Write(1, 'TFXR90Manager.ConfigureAntennas', lmt_Warning, Response);
end;

function TFXR90Manager.StartInventory: boolean;
var
  Response: string;
begin
  Result := False;


  if FState <> fxcsConnected then
  begin
    if not Self.Connect then
    begin
      LBLogger.Write(1, 'TFXR90Manager.StartInventory', lmt_Warning, 'FXR90 not connected');
      Exit;
    end;
  end;

  FTagList.Clear;

  if not Self.ConfigureAntennas then
    Exit;

  Result := FRESTClient.StartInventory(Response);
  if Result then
    LBLogger.Write(5, 'TFXR90Manager.StartInventory', lmt_Debug, 'StartInventory OK. ResponseLen=%d', [Length(Response)])
  else
    LBLogger.Write(1, 'TFXR90Manager.StartInventory', lmt_Warning, Response);
end;

function TFXR90Manager.StopInventory: boolean;
var
  Response: string;
begin
  Result := False;

  if FRESTClient = nil then
    LBLogger.Write(1, 'TFXR90Manager.StopInventory', lmt_Warning, 'REST client not created')
  else begin
    Result := FRESTClient.StopInventory(Response);
    if Result then
      LBLogger.Write(5, 'TFXR90Manager.StopInventory', lmt_Debug, 'StopInventory OK. ResponseLen=%d', [Length(Response)])
    else
      LBLogger.Write(1, 'TFXR90Manager.StopInventory', lmt_Warning, Response);
  end;
end;

end.
