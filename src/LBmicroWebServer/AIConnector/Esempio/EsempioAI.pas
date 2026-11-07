program EsempioAI;
{
  ESEMPIO: LBmicroWebServer come estensione di Claude (MCP + login OAuth).

  Cosa fa
  -------
  - Login con utente "Test" e password "Test" (qualunque altra coppia: "Accesso negato").
  - Espone all'AI quattro strumenti con valori fissi:
        api_countOdL    numero degli ordini di lavoro
        api_countUsers  numero degli utenti
        api_listAssets  elenco di tre asset (codice, nome, stato)
        api_findAsset   un asset dato il codice (esempio di funzione con un parametro)
  - Pagina di prova locale /prova: avvia il login OAuth senza Claude; a login riuscito
    mostra "Tutto OK", se il login viene rifiutato mostra "Accesso negato".

  Uso
  ---
      EsempioAI [porta] [indirizzo-pubblico]
      EsempioAI 8080 https://abcd.trycloudflare.com

  "indirizzo-pubblico" e' l'indirizzo con cui Claude raggiunge il server (HTTPS, senza / finale).
  Senza, si usa http://127.0.0.1:<porta>: va bene per la prova locale, non per Claude.ai.
  Il file Routes_Esempio.xml deve stare accanto all'eseguibile (o nella cartella corrente).
}
{$mode objfpc}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson,
  uLBmicroWebServer, uHTTPRequestParser, uHTTPConsts,
  uWebRouteRegistry, uTokenManager, uLBAIConnectorTypes, uLBAIConnector, uLBOAuthProvider;

const
  cUser     = 'Test';
  cPassword = 'Test';

type
  { Modulo di routing dell'esempio: ogni utente autenticato puo' usare ogni funzione. }
  TDemoModule = class(TWebRouteModule)
  protected
    function hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean; override;
  end;

  { Le funzioni esposte all'AI. }
  TDemoFunctions = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function DoCountOdL(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoCountUsers(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoListAssets(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoFindAsset(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function NewAsset(const aCode, aName, aState: String): TJSONObject;
  public
    constructor Create;
    { Descrive i parametri di ogni funzione: senza questa descrizione la funzione non viene esposta. }
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

  { Collega il login: qui si decide chi puo' entrare. }
  TLoginGlue = class(TObject)
  public
    function Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
      out aUserConfig: TJSONObject): Boolean;
    function RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
  end;

  { Pagine di prova locali: /prova e /prova/callback. }
  TTestPages = class(TRequestChainProcessor)
  strict private
    FPublicBaseURL : String;
    procedure Page(const aTitle, aBody: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream);
  strict protected
    function DoProcessRequest(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean; override;
  public
    constructor Create(const aPublicBaseURL: String);
  end;

{ TDemoModule }

function TDemoModule.hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean;
begin
  // Esempio: chi ha fatto login puo' tutto. In un'applicazione vera qui si guarda il ruolo
  // (in IdroWorld: is_admin e cache dei permessi del ruolo).
  Result := aUserConfig <> nil;
end;

{ TDemoFunctions }

constructor TDemoFunctions.Create;
begin
  inherited Create('api');   // codice dell'area in Routes_Esempio.xml
  Self.RegisterStandardWorker('countOdL', @Self.DoCountOdL);
  Self.RegisterStandardWorker('countUsers', @Self.DoCountUsers);
  Self.RegisterStandardWorker('listAssets', @Self.DoListAssets);
  Self.RegisterStandardWorker('findAsset', @Self.DoFindAsset);
end;

function TDemoFunctions.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if (aFunctionName = 'countOdL') or (aFunctionName = 'countUsers') or (aFunctionName = 'listAssets') then
    aDescriptor.Effect := aeReadOnly            // nessun parametro; sola lettura
  else if aFunctionName = 'findAsset' then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('code', aptString, 'Codice dell''asset, ad esempio A-001', True);
  end
  else
    Result := False;                            // non e' una funzione AI di questa classe
end;

function TDemoFunctions.DoCountOdL(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Answer: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Answer := TJSONObject.Create;
  _Answer.Add('count', 42);
  Result := _Answer;
end;

function TDemoFunctions.DoCountUsers(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Answer: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Answer := TJSONObject.Create;
  _Answer.Add('count', 7);
  Result := _Answer;
end;

function TDemoFunctions.NewAsset(const aCode, aName, aState: String): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('code', aCode);
  Result.Add('name', aName);
  Result.Add('state', aState);
end;

function TDemoFunctions.DoListAssets(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _List: TJSONArray;
begin
  aStatusCode := HTTP_STATUS_OK;
  _List := TJSONArray.Create;
  _List.Add(Self.NewAsset('A-001', 'Pompa di sollevamento', 'attivo'));
  _List.Add(Self.NewAsset('A-002', 'Serbatoio di accumulo', 'attivo'));
  _List.Add(Self.NewAsset('A-003', 'Quadro elettrico principale', 'in manutenzione'));
  Result := _List;
end;

function TDemoFunctions.DoFindAsset(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Code: String;
  _Error: TJSONObject;
begin
  // Il connettore ha gia' verificato che "code" ci sia ed e' una stringa.
  _Code := UpperCase(Trim(aRequestData.Get('code', '')));
  aStatusCode := HTTP_STATUS_OK;
  if _Code = 'A-001' then
    Result := Self.NewAsset('A-001', 'Pompa di sollevamento', 'attivo')
  else if _Code = 'A-002' then
    Result := Self.NewAsset('A-002', 'Serbatoio di accumulo', 'attivo')
  else if _Code = 'A-003' then
    Result := Self.NewAsset('A-003', 'Quadro elettrico principale', 'in manutenzione')
  else begin
    // Uno stato >= 400 arriva all'AI come errore dello strumento: puo' correggere la richiesta.
    aStatusCode := HTTP_STATUS_NOT_FOUND;
    _Error := TJSONObject.Create;
    _Error.Add('error', 'Asset non trovato: ' + _Code);
    Result := _Error;
  end;
end;

{ TLoginGlue }

function TLoginGlue.Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
  out aUserConfig: TJSONObject): Boolean;
begin
  anUserId := 0;
  aUserConfig := nil;
  Result := (aUsername = cUser) and (aPassword = cPassword);
  if Result then
  begin
    anUserId := 1;
    // E' la "configurazione utente" che arrivera' a hasPermission e ai worker.
    aUserConfig := TJSONObject.Create;
    aUserConfig.Add('name', aUsername);
  end;
end;

function TLoginGlue.RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
begin
  // Al rinnovo del token si ricontrolla che l'utente esista ancora (e si rilegge il suo ruolo).
  Result := anUserId = 1;
  aUserConfig := nil;
  if Result then
  begin
    aUserConfig := TJSONObject.Create;
    aUserConfig.Add('name', cUser);
  end;
end;

{ TTestPages }

constructor TTestPages.Create(const aPublicBaseURL: String);
begin
  inherited Create;
  FPublicBaseURL := aPublicBaseURL;
end;

procedure TTestPages.Page(const aTitle, aBody: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream);
var
  _Html: UTF8String;
begin
  _Html := '<!DOCTYPE html><html lang="it"><head><meta charset="utf-8"><title>' + aTitle +
    '</title></head><body style="font-family:sans-serif;margin:3em">' + aBody + '</body></html>';
  if ResponseData = nil then
    ResponseData := TMemoryStream.Create
  else
    ResponseData.Clear;
  ResponseData.WriteBuffer(_Html[1], Length(_Html));
  ResponseData.Position := 0;
  ResponseHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + MIME_TYPE_HTML + '; charset=utf-8');
end;

function TTestPages.DoProcessRequest(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Link: String;
  i: Integer;
  _HasCode, _HasError: Boolean;
begin
  Result := False;
  HTTPParser.SplitURIIntoResourceAndParameters;

  if (HTTPParser.Method = HTTP_METHOD_GET) and (HTTPParser.Resource = '/prova') then
  begin
    // Avvia un login OAuth contro questo stesso server, come farebbe Claude. Il "code_challenge" e'
    // fisso perche' qui il codice non viene scambiato con un token: basta vedere il login.
    _Link := '/oauth/authorize?response_type=code&client_id=prova&code_challenge_method=S256' +
      '&code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM&state=prova&scope=mcp' +
      '&redirect_uri=' + FPublicBaseURL + '/prova/callback';
    Self.Page('Prova login', '<h1>Prova del login</h1><p><a href="' + _Link + '">Accedi (utente Test, password Test)</a></p>',
      ResponseHeaders, ResponseData);
    ResponseCode := HTTP_STATUS_OK;
    Result := True;
  end
  else if (HTTPParser.Method = HTTP_METHOD_GET) and (HTTPParser.Resource = '/prova/callback') then
  begin
    // Qui il server OAuth rimanda il browser dopo il login: con "code" l'accesso e' riuscito,
    // con "error" e' stato rifiutato.
    _HasCode := False;
    _HasError := False;
    for i := 0 to HTTPParser.Params.Count - 1 do
    begin
      if Pos('code=', HTTPParser.Params[i]) = 1 then _HasCode := True;
      if Pos('error=', HTTPParser.Params[i]) = 1 then _HasError := True;
    end;
    if _HasCode and (not _HasError) then
      Self.Page('Tutto OK', '<h1>Tutto OK</h1>', ResponseHeaders, ResponseData)
    else
      Self.Page('Accesso negato', '<h1>Accesso negato</h1>', ResponseHeaders, ResponseData);
    ResponseCode := HTTP_STATUS_OK;
    Result := True;
  end;
end;

{ Programma principale }

var
  gWebServer : TLBmicroWebServer;
  gModule    : TDemoModule;
  gFunctions : TDemoFunctions;
  gTokens    : TTokenManager;
  gConnector : TLBAIConnector;
  gOAuth     : TLBOAuthProvider;
  gGlue      : TLoginGlue;
  gPages     : TTestPages;
  gPort      : Integer;
  gPublicURL : String;
  gRoutes    : String;
  gTokenData : TJSONObject;
  gCount     : Integer;
begin
  gPort := StrToIntDef(ParamStr(1), 8080);
  gPublicURL := ParamStr(2);
  if gPublicURL = '' then
    gPublicURL := 'http://127.0.0.1:' + IntToStr(gPort);
  while (gPublicURL <> '') and (gPublicURL[Length(gPublicURL)] = '/') do
    Delete(gPublicURL, Length(gPublicURL), 1);

  gRoutes := ExtractFilePath(ParamStr(0)) + 'Routes_Esempio.xml';
  if not FileExists(gRoutes) then
    gRoutes := 'Routes_Esempio.xml';

  gWebServer := TLBmicroWebServer.Create;
  gWebServer.ListeningPort := gPort;

  // 1) Rotte e funzioni
  gModule := TDemoModule.Create;
  if not gModule.LoadRoutesFromXML(gRoutes) then
  begin
    WriteLn('Impossibile leggere ', gRoutes);
    Halt(1);
  end;
  gFunctions := TDemoFunctions.Create;
  gModule.RegisterHandler(gFunctions);
  gModule.ValidateRoutes;

  // 2) Token e login
  gTokens := TTokenManager.Create;     // senza database: i token si perdono al riavvio
  gGlue := TLoginGlue.Create;

  gOAuth := TLBOAuthProvider.Create(gTokens);
  gOAuth.PublicBaseURL := gPublicURL;
  gOAuth.OnAuthenticateCredentials := @gGlue.Authenticate;
  gOAuth.OnRefreshUser := @gGlue.RefreshUser;
  gOAuth.AllowedRedirectURIs.Add(gPublicURL + '/prova/callback');   // solo per la pagina di prova
  gOAuth.Texts.ServerTitle := 'Esempio AI Connector';
  gOAuth.Texts.BadCredentials := 'Accesso negato';

  // 3) Il connettore MCP
  gConnector := TLBAIConnector.Create(gModule);
  gConnector.OnAuthenticateRequest := @gTokens.isValidBearerRequest;
  gConnector.ServerName := 'Esempio AI Connector';
  gConnector.Instructions := 'Dati aziendali di esempio: ordini di lavoro, utenti e asset.';
  gConnector.OAuthProvider := gOAuth;     // il connettore ne prende la proprieta'
  gCount := gConnector.Prepare();         // dopo RegisterHandler e ValidateRoutes

  // 4) Una catena di elaborazione: ogni richiesta passa dai moduli finche' uno la gestisce
  gPages := TTestPages.Create(gPublicURL);
  gWebServer.addChainProcessor(gModule, True);
  gWebServer.addChainProcessor(gOAuth, True);
  gWebServer.addChainProcessor(gConnector, True);
  gWebServer.addChainProcessor(gPages, True);

  // Un token fisso per provare con Claude Code (senza OAuth): vale come se Test avesse fatto login.
  gTokenData := nil;
  if gTokens.addNewScopedToken(1, TJSONObject(GetJSON('{"name":"Test"}')), cTokenScope_AIAccess, 0,
       'manuale', gTokenData) then
    WriteLn('Token per Claude Code: ', gTokenData.Get('token', ''));

  WriteLn('Strumenti esposti: ', gCount);
  WriteLn('Indirizzo MCP:      ', gPublicURL, gConnector.EndpointPath);
  WriteLn('Prova del login:    ', gPublicURL, '/prova');
  WriteLn('Ctrl+C per fermare.');
  Flush(Output);

  gWebServer.Activate;
  while True do
    Sleep(1000);
end.
