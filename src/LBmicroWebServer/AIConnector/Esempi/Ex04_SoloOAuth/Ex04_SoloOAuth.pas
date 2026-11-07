program Ex04_SoloOAuth;
(*
  ESEMPIO 4 - OAUTH SENZA IL CONNETTORE AI

  Il server di autorizzazione (TLBOAuthSetup / TLBOAuthProvider) non dipende dal connettore MCP: qui lo usa una
  normale pagina protetta, GET /whoami, che accetta un token "Bearer" emesso dopo il login nel browser.

  COSA MOSTRA
    - TLBOAuthSetup da solo: nessun router, nessun TLBAIConnector, nessuna sezione [AIConnector];
    - le diciture della pagina di accesso scelte nel file INI (titolo, intestazione, testo con {host} e {app}, pulsante);
    - Provider.ResourcePath := '/whoami': e' il percorso che i metadati OAuth indicano come risorsa protetta
      (senza connettore nessuno lo imposta, e resterebbe "/mcp");
    - Provider.AccessScope / RefreshScope / ScopeName: ambiti e nome dello scope propri dell'applicazione;
    - un TRequestChainProcessor che valida il token con TTokenManager.isValidBearerRequestForScope e legge
      l'utente dal contesto di autenticazione (che va liberato a fine richiesta);
    - la risposta 401 con WWW-Authenticate che punta ai metadati, come fa il connettore.

  AMBITO PROPRIO. Il provider emette di default token dell'ambito "ai" (quello che il connettore MCP accetta) e dichiara
  lo scope "mcp". Qui l'applicazione sceglie i propri: AccessScope, RefreshScope e ScopeName. I token sono validati
  con TTokenManager.isValidBearerRequestForScope: un token di questa applicazione NON vale per il connettore AI e
  viceversa, nemmeno se i due convivono nello stesso server.

  Uso:  Ex04_SoloOAuth      (porta 8084)
  Prova: python3 ../client/smoke_test.py ex04 http://127.0.0.1:8084
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, IniFiles, fpjson,
  uHTTPConsts, uHTTPRequestParser, uTokenManager, uLBAuthContext, uLBmicroWebServer,
  uLBOAuthProvider, uLBOAuthSetup,
  uEsempiComune;

const
  // ambiti scelti dall'applicazione (mai 'web': e' la sessione con cookie)
  cWhoAmIAccessScope  = 'portale';
  cWhoAmIRefreshScope = 'portale_refresh';

type
  { Pagina protetta da token Bearer. }
  TWhoAmI = class(TRequestChainProcessor)
  strict private
    FTokens   : TTokenManager;
    FProvider : TLBOAuthProvider;
    procedure Reply(aStatus: Integer; const aBody: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
  strict protected
    function DoProcessRequest(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean; override;
  public
    constructor Create(aTokens: TTokenManager; aProvider: TLBOAuthProvider);
  end;

constructor TWhoAmI.Create(aTokens: TTokenManager; aProvider: TLBOAuthProvider);
begin
  inherited Create;
  FTokens := aTokens;
  FProvider := aProvider;
end;

procedure TWhoAmI.Reply(aStatus: Integer; const aBody: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  if ResponseData = nil then
    ResponseData := TMemoryStream.Create
  else
    ResponseData.Clear;
  ResponseHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + MIME_TYPE_JSON);
  if Length(aBody) > 0 then
    ResponseData.WriteBuffer(aBody[1], Length(aBody));
  ResponseData.Position := 0;
  ResponseCode := aStatus;
end;

function TWhoAmI.DoProcessRequest(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Ctx  : TLBAuthenticationContext;
  _Obj  : TJSONObject;
  _Body : String;
begin
  Result := False;
  ResponseCode := HTTP_STATUS_NOT_FOUND;
  HTTPParser.SplitURIIntoResourceAndParameters;
  if (HTTPParser.Method <> HTTP_METHOD_GET) or (HTTPParser.Resource <> '/whoami') then
    Exit;               // non e' roba nostra: la richiesta passa al processore successivo
  Result := True;

  _Ctx := nil;
  if FTokens.isValidBearerRequestForScope(HTTPParser.Headers, ResponseHeaders, cWhoAmIAccessScope, _Ctx) and
     (_Ctx <> nil) and (_Ctx.UserConfig <> nil) then
  begin
    try
      // UserConfig e' valido fino a quando il contesto non viene liberato (il token e' "bloccato")
      _Obj := TJSONObject.Create;
      try
        _Obj.Add('user_id', _Ctx.UserId);
        _Obj.Add('role', _Ctx.UserConfig.Get('role', ''));
        _Obj.CompressedJSON := True;
        _Body := _Obj.AsJSON;
      finally
        _Obj.Free;
      end;
    finally
      _Ctx.Free;
    end;
    Self.Reply(HTTP_STATUS_OK, _Body, ResponseHeaders, ResponseData, ResponseCode);
  end
  else begin
    FreeAndNil(_Ctx);
    // Come il connettore: dice al client dove sono i metadati OAuth, cosi' sa come ottenere un token.
    ResponseHeaders.Add(HTTP_HEADER_WWW_AUTHENTICATE + ': Bearer resource_metadata="' +
      FProvider.GetResourceMetadataURL(HTTPParser.Headers) + '"');
    Self.Reply(HTTP_STATUS_UNAUTHORIZED, '{"error":"Authentication required"}', ResponseHeaders, ResponseData, ResponseCode);
  end;
end;

var
  gIni    : TIniFile;
  gPort   : Integer;
  gServer : TLBmicroWebServer;
  gTokens : TTokenManager;
  gUsers  : TDemoUsers;
  gOAuth  : TLBOAuthSetup;
begin
  gTokens := TTokenManager.Create;
  gOAuth := TLBOAuthSetup.Create;
  gIni := TIniFile.Create(ExampleFile('Ex04.ini'));
  try
    gPort := gIni.ReadInteger('LBmicroWebServer', 'Port', 8084);
    gTokens.LoadConfigurationFromIniFile(gIni);
    gOAuth.LoadFromINIFile(gIni);
  finally
    gIni.Free;
  end;

  gServer := TLBmicroWebServer.Create;
  gServer.ListeningPort := gPort;
  gUsers := TDemoUsers.Create;

  gOAuth.Attach(gServer, gTokens, @gUsers.Authenticate, @gUsers.RefreshUser);
  if not gOAuth.Active then
  begin
    WriteLn('OAuth non attivo: controlla [OAuth] in Ex04.ini e il log');
    Halt(1);
  end;

  // La risorsa protetta e' /whoami, non /mcp. Senza connettore nessun altro cambia questo valore.
  gOAuth.Provider.ResourcePath := '/whoami';
  gOAuth.Provider.AccessScope := cWhoAmIAccessScope;
  gOAuth.Provider.RefreshScope := cWhoAmIRefreshScope;
  gOAuth.Provider.ScopeName := 'portale';
  gServer.addChainProcessor(TWhoAmI.Create(gTokens, gOAuth.Provider), True);

  gTokens.Activate;
  gServer.Activate;
  WriteLn('Esempio 4 in ascolto sulla porta ', gPort, ': GET /whoami con "Authorization: Bearer <token>".');
  Flush(Output);
  WaitForever;
end.
