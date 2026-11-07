unit uIdroWorld_AIConnector;
{
  Collegamento di IdroWorld al connettore AI (TLBAIConnector, uLBAIConnector.pas).

  COSA FA
  ----------------------------------------------------------------------
  Un utente di IdroWorld puo' collegare Claude.ai (o un altro client MCP) al backend: al primo
  collegamento accede con il PROPRIO username e la PROPRIA password, e da quel momento l'AI lavora
  con i diritti del suo ruolo, esattamente come lui dal sito. Questa unit contiene solo il
  collegamento (credenziali, rinnovo, lettura dell'INI): il protocollo MCP e OAuth sono nel toolkit.

  CHE COSA VEDE L'AI
  ----------------------------------------------------------------------
  - Gli endpoint sono quelli di Routes.xml, dichiarati UNA volta sola. Serve in piu' l'endpoint
    Kind="AIConnector" (l'indirizzo MCP, tipicamente /mcp).
  - Ogni endpoint Standard con autenticazione e' uno strumento per l'AI (ExposeAllFunctions=1). Se la
    classe del gestore implementa IAIConnectorFunctions e descrive la funzione in DescribeAIFunction,
    i parametri sono precisi; altrimenti accetta un oggetto JSON libero e l'AI si basa sulla Description
    di Routes.xml. Con ExposeAllFunctions=0 restano solo le funzioni descritte.
  - Per ogni richiesta vale hasPermission di TIdroWorldWebRouteModule (is_admin, poi la cache dei
    permessi del ruolo): lo stesso controllo del sito.

  SESSIONE
  ----------------------------------------------------------------------
  Il token dell'AI porta lo stesso oggetto di sessione del login web ("is_admin", "role_id"), quindi
  nessun gestore deve cambiare. Il token dell'AI e' di un ambito diverso dal cookie web: non vale
  come cookie e viceversa. Al rinnovo (ogni ora) si rilegge l'utente: se e' stato disattivato o ha
  cambiato ruolo, l'AI lo vede subito.

  INI
  ----------------------------------------------------------------------
  [AIConnector]
  Enabled=1
  PublicBaseURL=https://cmms.example.com     ; l'indirizzo pubblico (HTTPS) che l'AI raggiunge
  ServerName=IdroWorld                       ; facoltativo
  Instructions=...                           ; facoltativo: spiega all'AI a cosa serve l'applicazione
  MaxCallsPerMinute=60                       ; facoltativo (per utente)
  TrustForwardedFor=0                        ; 1 se dietro un reverse proxy fidato
  RequireOpType=0                            ; 1: escludi dall'AI le funzioni senza OpType (aperte a ogni utente autenticato)
  ExposeAllFunctions=1                       ; 1 (default): ogni funzione Standard e' uno strumento, decide il ruolo;
                                             ; 0: solo le funzioni descritte con DescribeAIFunction
  ExtraRedirectURIs=                         ; facoltativo: altri redirect OAuth ammessi, separati da virgola
  AllowedOrigins=                            ; facoltativo: controllo dell'header Origin, separati da virgola

  La query delle credenziali e' la stessa di TIdroWorldHandlersAuth.HandleLogin. Se cambia l'una va
  cambiata anche l'altra (le due vivono separate per non toccare il login web).
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson,
  uIdroWorldDataAccess, uTokenManager, uWebRouteRegistry, uLBmicroWebServer,
  uLBAIConnector, uLBOAuthProvider;

type
  { TIdroWorldAIConnectorSetup }
  TIdroWorldAIConnectorSetup = class(TObject)
    strict private
      FEnabled         : Boolean;
      FPublicBaseURL   : String;
      FServerName      : String;
      FInstructions    : String;
      FMaxCallsPerMin  : Integer;
      FTrustForwarded  : Boolean;
      FExposeAll       : Boolean;
      FRequireOpType   : Boolean;
      FExtraRedirects  : TStringList;
      FOrigins         : TStringList;
      FDataAccess      : IIdroWorldDataAccess;   // valido dopo Attach

      function BuildSessionConfig(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
      function AIAuthenticate(const aUsername, aPassword: String; out anUserId: Integer;
        out aUserConfig: TJSONObject): Boolean;
      function AIRefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      // Legge [AIConnector]. False se assente, disattivata o incompleta (il connettore non parte).
      function LoadFromINIFile(aFile: TIniFile): Boolean;

      // Crea connettore e provider OAuth e li aggiunge alla catena del web server, che ne diventa
      // proprietario. Da chiamare in startingWebServer DOPO ValidateRoutes e dopo che il modulo di
      // routing e' gia' stato aggiunto alla catena. Restituisce il numero di strumenti esposti
      // (-1 se il connettore non e' stato attivato).
      function Attach(aDataAccess: IIdroWorldDataAccess; aTokenManager: TTokenManager;
        aRouteModule: TWebRouteModule; aWebServer: TLBmicroWebServer): Integer;

      property Enabled: Boolean read FEnabled;
  end;

implementation

uses
  ULBLogger, StrUtils;

const
  cSection              = 'AIConnector';
  cKey_Enabled          = 'Enabled';
  cKey_PublicBaseURL    = 'PublicBaseURL';
  cKey_ServerName       = 'ServerName';
  cKey_Instructions     = 'Instructions';
  cKey_MaxCalls         = 'MaxCallsPerMinute';
  cKey_TrustForwarded   = 'TrustForwardedFor';
  cKey_ExposeAll        = 'ExposeAllFunctions';
  cKey_RequireOpType    = 'RequireOpType';
  cKey_ExtraRedirects   = 'ExtraRedirectURIs';
  cKey_AllowedOrigins   = 'AllowedOrigins';
  cDefaultMaxCalls      = 60;

procedure SplitInto(const aText: String; aList: TStringList);
var
  _Item : String;
begin
  aList.Clear;
  for _Item in aText.Split([',', ';']) do
    if Trim(_Item) <> '' then
      aList.Add(Trim(_Item));
end;

{ TIdroWorldAIConnectorSetup }

constructor TIdroWorldAIConnectorSetup.Create;
begin
  inherited Create;
  FEnabled := False;
  FMaxCallsPerMin := cDefaultMaxCalls;
  FExtraRedirects := TStringList.Create;
  FOrigins := TStringList.Create;
  FDataAccess := nil;
end;

destructor TIdroWorldAIConnectorSetup.Destroy;
begin
  FDataAccess := nil;
  FreeAndNil(FExtraRedirects);
  FreeAndNil(FOrigins);
  inherited Destroy;
end;

function TIdroWorldAIConnectorSetup.LoadFromINIFile(aFile: TIniFile): Boolean;
const
  cMe = 'TIdroWorldAIConnectorSetup.LoadFromINIFile';
begin
  FEnabled := False;
  Result := False;

  if not aFile.ReadBool(cSection, cKey_Enabled, False) then
  begin
    LBLogger.Write(3, cMe, lmt_Debug, 'AI connector not enabled');
    Exit;
  end;

  FPublicBaseURL := Trim(aFile.ReadString(cSection, cKey_PublicBaseURL, ''));
  if FPublicBaseURL = '' then
  begin
    LBLogger.Write(1, cMe, lmt_Error, '[%s] %s is required when the connector is enabled: connector NOT started', [cSection, cKey_PublicBaseURL]);
    Exit;
  end;
  if not FPublicBaseURL.StartsWith('https://', True) then
    LBLogger.Write(1, cMe, lmt_Warning,
      '[%s] %s is not HTTPS (<%s>): Claude.ai needs a public HTTPS address and the password travels in the login form',
      [cSection, cKey_PublicBaseURL, FPublicBaseURL]);

  FServerName := Trim(aFile.ReadString(cSection, cKey_ServerName, 'IdroWorld'));
  FInstructions := Trim(aFile.ReadString(cSection, cKey_Instructions, ''));
  FMaxCallsPerMin := aFile.ReadInteger(cSection, cKey_MaxCalls, cDefaultMaxCalls);
  FTrustForwarded := aFile.ReadBool(cSection, cKey_TrustForwarded, False);
  FExposeAll := aFile.ReadBool(cSection, cKey_ExposeAll, True);
  FRequireOpType := aFile.ReadBool(cSection, cKey_RequireOpType, False);
  SplitInto(aFile.ReadString(cSection, cKey_ExtraRedirects, ''), FExtraRedirects);
  SplitInto(aFile.ReadString(cSection, cKey_AllowedOrigins, ''), FOrigins);

  FEnabled := True;
  Result := True;
end;

function TIdroWorldAIConnectorSetup.BuildSessionConfig(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
var
  _Row : TJSONObject;
begin
  // Stesso oggetto di sessione di HandleLogin: solo is_admin e role_id.
  Result := False;
  aUserConfig := nil;
  _Row := FDataAccess.SelectRowToJSON(
    'SELECT COALESCE(r.id,0) AS role_id, COALESCE(r.is_system,FALSE) AS is_system ' +
    'FROM core.app_user u LEFT JOIN core.app_role r ON r.id = u.role_id ' +
    'WHERE u.id = :id AND u.is_active = TRUE',
    ['id', Int64(anUserId)], cIdroWorldMainDB);
  if _Row = nil then Exit;
  try
    aUserConfig := TJSONObject.Create;
    aUserConfig.Add('is_admin', _Row.Get('is_system', False));
    aUserConfig.Add('role_id', _Row.Get('role_id', Int64(0)));
    Result := True;
  finally
    _Row.Free;
  end;
end;

function TIdroWorldAIConnectorSetup.AIAuthenticate(const aUsername, aPassword: String; out anUserId: Integer;
  out aUserConfig: TJSONObject): Boolean;
const
  cMe = 'TIdroWorldAIConnectorSetup.AIAuthenticate';
var
  _Row : TJSONObject;
begin
  Result := False;
  anUserId := 0;
  aUserConfig := nil;

  if (Trim(aUsername) = '') or (aPassword = '') then Exit;

  try
    _Row := FDataAccess.SelectRowToJSON(
      'SELECT id FROM core.app_user ' +
      'WHERE is_active = TRUE AND username = :usr AND psw = crypt(:pwd, psw)',
      ['usr', Trim(aUsername), 'pwd', aPassword], cIdroWorldMainDB);
    if _Row = nil then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, 'AI login failed for <%s>', [Trim(aUsername)]);
      Exit;
    end;
    try
      anUserId := _Row.Get('id', 0);
    finally
      _Row.Free;
    end;

    Result := (anUserId > 0) and Self.BuildSessionConfig(anUserId, aUserConfig);
    if not Result then
      anUserId := 0;
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe, lmt_Error, E.Message);
      Result := False;
      anUserId := 0;
      FreeAndNil(aUserConfig);
    end;
  end;
end;

function TIdroWorldAIConnectorSetup.AIRefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
const
  cMe = 'TIdroWorldAIConnectorSetup.AIRefreshUser';
begin
  // Al rinnovo si rilegge l'utente: disattivato = niente nuovi token; ruolo cambiato = effetto immediato.
  aUserConfig := nil;
  try
    Result := Self.BuildSessionConfig(anUserId, aUserConfig);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe, lmt_Error, E.Message);
      Result := False;
      FreeAndNil(aUserConfig);
    end;
  end;
end;

function TIdroWorldAIConnectorSetup.Attach(aDataAccess: IIdroWorldDataAccess; aTokenManager: TTokenManager;
  aRouteModule: TWebRouteModule; aWebServer: TLBmicroWebServer): Integer;
const
  cMe = 'TIdroWorldAIConnectorSetup.Attach';
var
  _OAuth     : TLBOAuthProvider;
  _Connector : TLBAIConnector;
  i          : Integer;
begin
  Result := -1;
  if (not FEnabled) or (aDataAccess = nil) or (aTokenManager = nil) or (aRouteModule = nil) or (aWebServer = nil) then
  begin
    if FEnabled then
      LBLogger.Write(1, cMe, lmt_Error, 'Missing dependency: AI connector not started');
    Exit;
  end;

  FDataAccess := aDataAccess;

  _OAuth := TLBOAuthProvider.Create(aTokenManager);
  _OAuth.PublicBaseURL := FPublicBaseURL;
  _OAuth.TrustForwardedFor := FTrustForwarded;
  for i := 0 to FExtraRedirects.Count - 1 do
    _OAuth.AllowedRedirectURIs.Add(FExtraRedirects[i]);
  _OAuth.OnAuthenticateCredentials := @Self.AIAuthenticate;
  _OAuth.OnRefreshUser := @Self.AIRefreshUser;

  _Connector := TLBAIConnector.Create(aRouteModule);
  _Connector.ServerName := FServerName;
  _Connector.Instructions := FInstructions;
  _Connector.MaxCallsPerMinute := FMaxCallsPerMin;
  // Tutte le funzioni Standard autenticate sono strumenti: decide il ruolo dell'utente. Quelle che una
  // classe descrive (DescribeAIFunction) hanno parametri precisi, le altre accettano un oggetto JSON libero.
  _Connector.ExposeUndescribed := FExposeAll;
  // True: le funzioni senza OpType (ricerche, elenchi di riferimento) non sono strumenti per l'AI.
  _Connector.RequireOpTypeForTools := FRequireOpType;
  _Connector.OnAuthenticateRequest := @aTokenManager.isValidBearerRequest;
  _Connector.AllowedOrigins.AddStrings(FOrigins);
  _Connector.OAuthProvider := _OAuth;   // solo un riferimento: lo possiede la catena del web server

  // Dopo ValidateRoutes e RegisterHandler: legge da Routes.xml l'indirizzo MCP e costruisce l'elenco
  // degli strumenti (e scrive nel log cio' che non puo' esporre).
  Result := _Connector.Prepare();

  // Ordine della catena: ciascuna chiamata con asFirst=True mette il processore in testa. Il
  // connettore e il provider OAuth rispondono solo ai propri indirizzi (/mcp, /oauth, /.well-known) e
  // lasciano passare il resto al modulo di routing, gia' in catena.
  aWebServer.addChainProcessor(_OAuth, True);
  aWebServer.addChainProcessor(_Connector, True);

  LBLogger.Write(3, cMe, lmt_Info, 'AI connector active on <%s%s>: %d tool(s)',
    [FPublicBaseURL, _Connector.EndpointPath, Result]);
end;

end.
