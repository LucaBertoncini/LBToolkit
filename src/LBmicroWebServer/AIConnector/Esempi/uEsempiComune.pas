unit uEsempiComune;
(*
  Parti comuni degli esempi: utenti di prova, permessi per ruolo e un "host" che collega i pezzi.

  NON E' CODICE DA COPIARE IN UN'APPLICAZIONE VERA. Serve a non ripetere in ogni esempio le stesse righe.
  Il collegamento passo per passo, scritto per esteso, e' in Ex01_Minimo.pas.

  UTENTI DI PROVA (nome / password / ruolo)
    mario  / pw1 / lettore   puo' usare l'AI e leggere (OpType Lettura)
    anna   / pw2 / tecnico   come mario, piu' modificare (OpType Modifica)
    admin  / pw3 / admin     puo' tutto (is_admin = true)
    ospite / pw4 / ospite    puo' leggere ma NON ha il permesso ai.Uso: il canale AI gli e' chiuso (403)
    temp   / pw5 / lettore   uguale a mario, ma si disattiva creando il file "temp.disabled" nella cartella
                             da cui parte il server: serve a provare che il rinnovo del token rilegge l'utente

  PERMESSI. Un permesso e' "area.OpType". L'area "ai" e' la porta MCP (OpType "Uso"). Per tutte le altre aree:
  lettore e ospite hanno Lettura, tecnico ha Lettura e Modifica, admin ha tutto.
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson,
  uWebRouteRegistry, uLBmicroWebServer, uTokenManager,
  uLBOAuthSetup, uLBAIConnectorSetup;

type
  { Modulo di routing con la strategia di autorizzazione degli esempi (ruolo -> permessi). }
  TDemoRouteModule = class(TWebRouteModule)
  protected
    function hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean; override;
  end;

  { Utenti di prova. I due metodi hanno la firma richiesta da OnAuthenticateCredentials e OnRefreshUser. }
  TDemoUsers = class(TObject)
  public
    function Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
      out aUserConfig: TJSONObject): Boolean;
    function RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
  end;

  { Collega router, token, OAuth e connettore AI leggendo il file INI. Vedi Ex01_Minimo per il codice esteso. }
  TExampleHost = class(TObject)
  strict private
    FServer : TLBmicroWebServer;
    FRouter : TDemoRouteModule;
    FTokens : TTokenManager;
    FUsers  : TDemoUsers;
    FOAuth  : TLBOAuthSetup;
    FAI     : TLBAIConnectorSetup;
    FPort   : Integer;
  public
    constructor Create(const aIniName, aRoutesName: String);
    // I gestori vanno registrati PRIMA di Wire. L'host non li libera: vivono quanto il processo.
    procedure AddHandler(aHandler: TRouteHandlerBase);
    // ValidateRoutes, autenticazione del sito, catena, OAuth e (se richiesto) connettore AI.
    procedure Wire(aAttachAI: Boolean = True);
    procedure Serve;
    property Server : TLBmicroWebServer read FServer;
    property Router : TDemoRouteModule read FRouter;
    property Tokens : TTokenManager read FTokens;
    property OAuth  : TLBOAuthSetup read FOAuth;
    property AI     : TLBAIConnectorSetup read FAI;
  end;

procedure WaitForever;
// Cerca il file accanto all'eseguibile, poi nella cartella corrente.
function ExampleFile(const aName: String): String;

implementation

type
  TUserRow = record
    Id       : Integer;
    Name     : String;
    Password : String;
    Role     : String;
    Active   : Boolean;
  end;

const
  cUserCount = 5;
  cUsers: array[1..cUserCount] of TUserRow = (
    (Id: 1; Name: 'mario';  Password: 'pw1'; Role: 'lettore'; Active: True),
    (Id: 2; Name: 'anna';   Password: 'pw2'; Role: 'tecnico'; Active: True),
    (Id: 3; Name: 'admin';  Password: 'pw3'; Role: 'admin';   Active: True),
    (Id: 4; Name: 'ospite'; Password: 'pw4'; Role: 'ospite';  Active: True),
    (Id: 5; Name: 'temp';   Password: 'pw5'; Role: 'lettore'; Active: True)
  );

function IsActive(const aRow: TUserRow): Boolean;
begin
  Result := aRow.Active;
  // "temp" si disattiva a caldo creando un file: cosi' si prova il rinnovo del token senza riavviare il server
  if Result and (aRow.Name = 'temp') and FileExists('temp.disabled') then
    Result := False;
end;

// La configurazione utente e' cio' che finisce nel token e che arriva a hasPermission e ai worker.
function BuildConfig(const aRow: TUserRow): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('role', aRow.Role);
  Result.Add('is_admin', aRow.Role = 'admin');
end;

function RoleAllows(const aRole, anArea, anOpType: String): Boolean;
begin
  if anArea = 'ai' then
    Exit((anOpType = 'Uso') and ((aRole = 'lettore') or (aRole = 'tecnico')));
  if aRole = 'tecnico' then
    Exit((anOpType = 'Lettura') or (anOpType = 'Modifica'));
  if (aRole = 'lettore') or (aRole = 'ospite') then
    Exit(anOpType = 'Lettura');
  Result := False;
end;

procedure WaitForever;
begin
  while True do
    Sleep(1000);
end;

function ExampleFile(const aName: String): String;
begin
  Result := ExtractFilePath(ParamStr(0)) + aName;
  if not FileExists(Result) then
    Result := aName;
end;

{ TDemoRouteModule }

function TDemoRouteModule.hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean;
begin
  Result := False;
  if aUserConfig = nil then Exit;
  if aUserConfig.Get('is_admin', False) then Exit(True);
  Result := RoleAllows(aUserConfig.Get('role', ''), anAreaCode, anOpType);
end;

{ TDemoUsers }

function TDemoUsers.Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
  out aUserConfig: TJSONObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  anUserId := 0;
  aUserConfig := nil;
  for i := 1 to cUserCount do
    if (cUsers[i].Name = aUsername) and (cUsers[i].Password = aPassword) and IsActive(cUsers[i]) then
    begin
      anUserId := cUsers[i].Id;
      aUserConfig := BuildConfig(cUsers[i]);   // oggetto NUOVO: lo prende in carico chi chiama
      Exit(True);
    end;
end;

function TDemoUsers.RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
var
  i: Integer;
begin
  // Chiamata a OGNI rinnovo del token: un utente disattivato o con un altro ruolo si nota qui.
  Result := False;
  aUserConfig := nil;
  for i := 1 to cUserCount do
    if (cUsers[i].Id = anUserId) and IsActive(cUsers[i]) then
    begin
      aUserConfig := BuildConfig(cUsers[i]);
      Exit(True);
    end;
end;

{ TExampleHost }

constructor TExampleHost.Create(const aIniName, aRoutesName: String);
var
  _Ini: TIniFile;
begin
  inherited Create;
  FOAuth := TLBOAuthSetup.Create;
  FAI := TLBAIConnectorSetup.Create;
  FTokens := TTokenManager.Create;
  FUsers := TDemoUsers.Create;

  if not FileExists(ExampleFile(aIniName)) then
    WriteLn('Attenzione: file di configurazione <', aIniName, '> non trovato: si usano i valori predefiniti');
  _Ini := TIniFile.Create(ExampleFile(aIniName));
  try
    FPort := _Ini.ReadInteger('LBmicroWebServer', 'Port', 8080);
    FTokens.LoadConfigurationFromIniFile(_Ini);   // [TokenManager]
    FOAuth.LoadFromINIFile(_Ini);                 // [OAuth]
    FAI.LoadFromINIFile(_Ini);                    // [AIConnector]
  finally
    _Ini.Free;
  end;

  FServer := TLBmicroWebServer.Create;
  FServer.ListeningPort := FPort;
  FRouter := TDemoRouteModule.Create;
  if not FRouter.LoadRoutesFromXML(ExampleFile(aRoutesName)) then
  begin
    WriteLn('Impossibile leggere ', aRoutesName);
    Halt(1);
  end;
end;

procedure TExampleHost.AddHandler(aHandler: TRouteHandlerBase);
begin
  FRouter.RegisterHandler(aHandler);
end;

procedure TExampleHost.Wire(aAttachAI: Boolean);
begin
  FRouter.ValidateRoutes;
  // Sessione del SITO (cookie): serve solo se qualcuno chiama gli endpoint /api/... da HTTP. L'AI non passa da qui.
  FRouter.OnAuthenticateRequest := @FTokens.isValidRequest;
  FServer.addChainProcessor(FRouter, True);
  FOAuth.Attach(FServer, FTokens, @FUsers.Authenticate, @FUsers.RefreshUser);
  if aAttachAI then
    FAI.Attach(FServer, FRouter, FOAuth);
  FTokens.Activate;   // thread che rimuove i token scaduti (e salva su SQLite se DBFile e' impostato)
end;

procedure TExampleHost.Serve;
begin
  FServer.Activate;
  WriteLn('Server in ascolto sulla porta ', FPort, '. Ctrl+C per fermare.');
  Flush(Output);
  WaitForever;
end;

end.
