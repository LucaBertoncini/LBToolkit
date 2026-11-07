unit uLBAIConnectorSetup;
(*
  Attivazione del connettore per AGENTI AI (MCP) di LBmicroWebServer, comune a tutte le applicazioni.

  DIPENDE DA OAUTH, NON LO CREA
    L'agente AI e' un utente che si autentica con OAuth e che opera con i permessi del proprio ruolo: servono
    quindi il server di autorizzazione (TLBOAuthSetup, sezione [OAuth]) e il registro delle rotte
    (TWebRouteModule, per sapere qual e' l'endpoint MCP e quali funzioni esporre). Questa unit non li crea: li
    riceve, controlla che ci siano e scrive nel log tutto cio' che manca. OAuth resta cosi' un servizio a se',
    usabile anche senza AI.

  CHI USA QUESTA UNIT
    L'applicazione, in tre punti (nessuna modifica a TLBApplicationBoostrap). Dopo Attach, la proprieta' Connector
    permette di impostare cio' che il file INI non copre (per esempio OnToolCalled, il registro delle chiamate):
      costruttore               FAI := TLBAIConnectorSetup.Create;
      lettura del file INI      FAI.LoadFromINIFile(aFile);        (LoadConfigurationFromINIFileInternal)
      fine di startingWebServer FAI.Attach(FWebServer, Router, FOAuth);   (DOPO FOAuth.Attach)

  ORDINE (l'unico vincolo che resta all'applicazione)
    Attach va chiamato DOPO ogni RegisterHandler e DOPO ValidateRoutes: il catalogo degli strumenti si costruisce
    una volta sola, a questo punto, e non si ricostruisce. Attach lo verifica (nessun gestore registrato = errore
    nel log, connettore non attivato) ma non puo' sapere se ne manchi qualcuno.

  COSA DECIDE COSA PUO' FARE L'AGENTE
    - Routes.xml e le classi: sono candidati gli endpoint Standard autenticati; con ExposeUndescribed=0 solo
      quelli che la loro classe descrive (IAIConnectorFunctions);
    - il ruolo dell'utente che ha autorizzato l'agente (Area.OpType), a ogni chiamata;
    - la porta MCP: l'endpoint Kind="AIConnector" di Routes.xml. Senza, il connettore NON parte (fail-closed):
      da solo risponderebbe a qualunque utente autenticato.

  SEZIONE [AIConnector] (tutte le chiavi facoltative tranne Enabled)
    Enabled=0                      ; 1 = attiva il connettore (richiede [OAuth] attivo)
    ExposeUndescribed=0            ; 1 = ogni endpoint Standard autenticato diventa strumento (decide il ruolo)
    RequireOpTypeForTools=0        ; 1 = non espone gli endpoint senza OpType
    AllowedOrigins=                ; Origin ammesse per richieste da browser (di norma vuoto)
    MaxCallsPerMinute=60           ; per utente; 0 = nessun limite
    MaxResultKB=200
    ServerName=LBmicroWebServer
    Instructions=                  ; testo mostrato all'agente all'avvio della sessione
*)
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uLBmicroWebServer, uWebRouteRegistry, uTokenManager, uLBOAuthSetup, uLBAIConnector;

const
  cAIConnector_INI_Section = 'AIConnector';

type
  { TLBAIConnectorSetup }
  TLBAIConnectorSetup = class(TObject)
  strict private
    FEnabled           : Boolean;
    FAllowedOrigins    : TStringList;
    FExposeUndescribed : Boolean;
    FRequireOpType     : Boolean;
    FMaxCallsPerMinute : Integer;
    FMaxResultKB       : Integer;
    FServerName        : String;
    FInstructions      : String;
    // creato qui e passato alla catena: lo possiede il web server, qui e' solo un riferimento
    FConnector         : TLBAIConnector;
    function GetActive: Boolean;
    procedure SplitList(const aText: String; aList: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    // Legge [AIConnector]. True = richiesto E valido. Non solleva eccezioni.
    function LoadFromINIFile(aFile: TIniFile): Boolean;

    // Controlla le precondizioni (elencandole TUTTE nel log se ne manca qualcuna), poi crea il connettore, ne
    // costruisce il catalogo e lo aggiunge alla catena. True = attivo. Con il connettore spento restituisce
    // False senza scrivere nulla.
    function Attach(aWebServer: TLBmicroWebServer; aRouteModule: TWebRouteModule; anOAuth: TLBOAuthSetup): Boolean;

    property Enabled : Boolean read FEnabled;
    // True quando il connettore e' nella catena del web server.
    property Active  : Boolean read GetActive;
    // Il connettore creato (in prestito: lo possiede il web server), nil finche' Attach non ha avuto successo. Serve a
    // impostare cio' che il file INI non copre, dopo Attach: Setup.Connector.OnToolCalled := @MioRegistro.
    property Connector : TLBAIConnector read FConnector;
  end;

implementation

uses
  ULBLogger;

const
  cMe                    = 'TLBAIConnectorSetup';
  cKey_Enabled           = 'Enabled';
  cKey_ExposeUndescribed = 'ExposeUndescribed';
  cKey_RequireOpType     = 'RequireOpTypeForTools';
  cKey_AllowedOrigins    = 'AllowedOrigins';
  cKey_MaxCalls          = 'MaxCallsPerMinute';
  cKey_MaxResultKB       = 'MaxResultKB';
  cKey_ServerName        = 'ServerName';
  cKey_Instructions      = 'Instructions';

  cDefault_MaxCalls      = Integer(60);
  cDefault_MaxResultKB   = Integer(200);
  cDefault_ServerName    = 'LBmicroWebServer';

constructor TLBAIConnectorSetup.Create;
begin
  inherited Create;
  FEnabled := False;
  FAllowedOrigins := TStringList.Create;
  FConnector := nil;
end;

destructor TLBAIConnectorSetup.Destroy;
begin
  // il connettore NON si libera qui: lo possiede il web server
  FConnector := nil;
  FreeAndNil(FAllowedOrigins);
  inherited Destroy;
end;

function TLBAIConnectorSetup.GetActive: Boolean;
begin
  Result := FConnector <> nil;
end;

procedure TLBAIConnectorSetup.SplitList(const aText: String; aList: TStrings);
var
  _Parts : TStringList;
  _Item  : String;
  i      : Integer;
begin
  aList.Clear;
  _Parts := TStringList.Create;
  try
    _Parts.StrictDelimiter := True;
    _Parts.Delimiter := ',';
    _Parts.DelimitedText := StringReplace(aText, ';', ',', [rfReplaceAll]);
    for i := 0 to _Parts.Count - 1 do
    begin
      _Item := Trim(_Parts[i]);
      if _Item <> '' then aList.Add(_Item);
    end;
  finally
    _Parts.Free;
  end;
end;

function TLBAIConnectorSetup.LoadFromINIFile(aFile: TIniFile): Boolean;
begin
  Result := False;
  FEnabled := False;
  if aFile = nil then Exit;
  try
    // spento (o sezione assente): silenzio assoluto, come per OAuth
    if not aFile.ReadBool(cAIConnector_INI_Section, cKey_Enabled, False) then Exit;

    FExposeUndescribed := aFile.ReadBool(cAIConnector_INI_Section, cKey_ExposeUndescribed, False);
    FRequireOpType := aFile.ReadBool(cAIConnector_INI_Section, cKey_RequireOpType, False);
    Self.SplitList(aFile.ReadString(cAIConnector_INI_Section, cKey_AllowedOrigins, ''), FAllowedOrigins);

    FMaxCallsPerMinute := aFile.ReadInteger(cAIConnector_INI_Section, cKey_MaxCalls, cDefault_MaxCalls);
    if FMaxCallsPerMinute < 0 then FMaxCallsPerMinute := 0;
    FMaxResultKB := aFile.ReadInteger(cAIConnector_INI_Section, cKey_MaxResultKB, cDefault_MaxResultKB);
    if FMaxResultKB <= 0 then FMaxResultKB := cDefault_MaxResultKB;

    FServerName := Trim(aFile.ReadString(cAIConnector_INI_Section, cKey_ServerName, cDefault_ServerName));
    if FServerName = '' then FServerName := cDefault_ServerName;
    FInstructions := Trim(aFile.ReadString(cAIConnector_INI_Section, cKey_Instructions, ''));

    FEnabled := True;
    Result := True;
    LBLogger.Write(3, cMe + '.LoadFromINIFile', lmt_Info,
      'AI connector configured (%d calls/min per user, results up to %d KB)', [FMaxCallsPerMinute, FMaxResultKB]);
  except
    on E: Exception do
    begin
      FEnabled := False;
      Result := False;
      LBLogger.Write(1, cMe + '.LoadFromINIFile', lmt_Error, '%s', [E.Message]);
    end;
  end;
end;

function TLBAIConnectorSetup.Attach(aWebServer: TLBmicroWebServer; aRouteModule: TWebRouteModule;
  anOAuth: TLBOAuthSetup): Boolean;
var
  _Problems  : TStringList;
  _Connector : TLBAIConnector;
  _Added     : Boolean;
  _Tools     : Integer;
  i          : Integer;
begin
  Result := False;
  FConnector := nil;
  if not FEnabled then Exit;

  // TUTTE le precondizioni, elencate insieme: chi configura vede in un colpo solo cosa manca
  _Problems := TStringList.Create;
  try
    if aWebServer = nil then
      _Problems.Add('the web server does not exist ([LBmicroWebServer] Port missing or 0)');
    if (anOAuth = nil) or (not anOAuth.Active) then
      _Problems.Add('OAuth is not active: the AI agent signs in through OAuth ([OAuth] Enabled=1 with a valid configuration, and OAuth attached BEFORE this connector)')
    else if not SameStr(anOAuth.Provider.AccessScope, cTokenScope_AIAccess) then
      // the connector validates tokens of the AI scope only: a provider that issues another scope would produce tokens it refuses
      _Problems.Add('the OAuth provider issues tokens of scope <' + anOAuth.Provider.AccessScope + '>, but the connector accepts only <' + cTokenScope_AIAccess + '>');
    if aRouteModule = nil then
      _Problems.Add('the application did not provide the routing module')
    else if aRouteModule.Routes = nil then
      _Problems.Add('the routing module has no route registry')
    else begin
      // fail-closed: senza la porta dichiarata in Routes.xml nessun controllo di ruolo sarebbe applicato
      if aRouteModule.Routes.AIConnectorEndpoint = nil then
        _Problems.Add('no endpoint with Kind="AIConnector" in Routes.xml: it is the MCP entry point and the role that may use the AI channel');
      // il catalogo si costruisce una volta sola: se i gestori non sono ancora registrati resterebbe vuoto per sempre
      if aRouteModule.Routes.AIEndpointCount = 0 then
        _Problems.Add('no handler registered yet (or no authenticated Standard endpoint): Attach must come after every RegisterHandler and after ValidateRoutes');
    end;

    if _Problems.Count > 0 then
    begin
      for i := 0 to _Problems.Count - 1 do
        LBLogger.Write(1, cMe + '.Attach', lmt_Error, 'AI connector NOT activated: %s', [_Problems[i]]);
      Exit;
    end;
  finally
    _Problems.Free;
  end;

  if FExposeUndescribed then
    LBLogger.Write(1, cMe + '.Attach', lmt_Warning,
      'ExposeUndescribed=1: EVERY authenticated Standard endpoint becomes a tool, writes and administration included; only the role of the user limits what the agent can do');

  _Connector := nil;
  _Added := False;
  try
    _Connector := TLBAIConnector.Create(aRouteModule);
    _Connector.ServerName := FServerName;
    _Connector.Instructions := FInstructions;
    _Connector.ExposeUndescribed := FExposeUndescribed;
    _Connector.RequireOpTypeForTools := FRequireOpType;
    _Connector.MaxCallsPerMinute := FMaxCallsPerMinute;
    _Connector.MaxResultBytes := FMaxResultKB * 1024;
    for i := 0 to FAllowedOrigins.Count - 1 do
      _Connector.AllowedOrigins.Add(FAllowedOrigins[i]);

    // gli stessi del server di autorizzazione: il token emesso dal provider e' validato dallo stesso gestore
    _Connector.OAuthProvider := anOAuth.Provider;
    _Connector.OnAuthenticateRequest := @anOAuth.TokenManager.isValidBearerRequest;

    // catalogo degli strumenti (e controlli di configurazione nel log) all'avvio, non alla prima richiesta
    _Tools := _Connector.Prepare();
    if _Tools = 0 then
      LBLogger.Write(1, cMe + '.Attach', lmt_Warning,
        'The AI connector has no tool: describe the functions (DescribeAIFunction of the handlers) or set ExposeUndescribed=1');

    // Da qui il connettore e' del web server. Il valore restituito da addChainProcessor non si usa: l'esito
    // dell'inserimento e' gia' noto (eccezione o no).
    aWebServer.addChainProcessor(_Connector, True);
    _Added := True;
    FConnector := _Connector;
    Result := True;
    LBLogger.Write(3, cMe + '.Attach', lmt_Info, 'AI connector active: %d tool(s), MCP path %s', [_Tools, FConnector.EndpointPath]);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe + '.Attach', lmt_Error, '%s', [E.Message]);
      // se non e' entrato nella catena si libera qui; se e' entrato e' del web server
      if not _Added then
        FreeAndNil(_Connector);
      Result := FConnector <> nil;
    end;
  end;
end;

end.
