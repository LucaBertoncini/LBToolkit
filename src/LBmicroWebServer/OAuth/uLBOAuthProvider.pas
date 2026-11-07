unit uLBOAuthProvider;
{
  TLBOAuthProvider: the OAuth 2.0 "authorization server" that lets an AI client (Claude.ai, Claude
  Code, other MCP clients) obtain a token on behalf of a user of the application.

  WHAT HAPPENS
  ----------------------------------------------------------------------
  1. The AI client calls the MCP endpoint without a token: TLBAIConnector answers 401 and points to
     "/.well-known/oauth-protected-resource" (served here).
  2. The client reads the metadata (here too), registers itself (POST /oauth/register) and sends the
     user's browser to GET /oauth/authorize.
  3. This page shows a login form. The user types HIS username and password; the application checks
     them through OnAuthenticateCredentials (normally the same check as the web login). Nothing about
     the credentials is stored here.
  4. The browser is redirected back to the client with a one-time code; the client exchanges it
     (POST /oauth/token, with PKCE) for an access token (15 minutes by default) and a refresh token (30 days).
  5. From then on the AI works with the rights of that user: the tokens carry the same user
     configuration (user id, role) the web login produces, so the application's own permission check
     (hasPermission) applies unchanged.

  TOKENS ARE KEPT BY TTokenManager (uTokenManager.pas), with their own scope (see addNewScopedToken):
  they are persisted with the web sessions, survive a restart, and an AI token is never valid as a
  web cookie nor vice versa.

  SECURITY DECISIONS
  ----------------------------------------------------------------------
  - PKCE with S256 is mandatory; the only grants are authorization_code and refresh_token.
  - Public clients, no client secret. Dynamic client registration is accepted but not stored (clients
    are stateless, so a restart of the server does not break existing connections); what protects the
    flow is the list of ALLOWED REDIRECT URIs: only Claude's callback (and, if enabled, loopback
    addresses for Claude Code) can ever receive a code. Add other AI services explicitly.
  - Authorization codes: single use, 5 minutes, bound to client, redirect URI and PKCE challenge.
  - Refresh tokens are rotated at every use (the old one stops working). At refresh the user is
    checked again through OnRefreshUser, so a deactivated user or a changed role takes effect.
  - Failed logins are limited (per address and per user name) and answered with 429 for a while.
  - The login page cannot be framed (clickjacking) and is never cached; it shows the host the user
    is being sent back to.
  - Use it only behind HTTPS: the password travels in the form.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, SyncObjs,
  uLBmicroWebServer, uHTTPRequestParser, uTokenManager;

type
  {:Checks the credentials typed by the user. On True, aUserConfig must be a NEW object with the same
    content the web login stores in the session (for IdroWorld: is_admin and role_id); the provider
    takes ownership of it.}
  TAICredentialsEvent = function(const aUsername, aPassword: String; out anUserId: Integer;
    out aUserConfig: TJSONObject): Boolean of object;

  {:Called at every refresh: the user must still be valid. On True, aUserConfig is a NEW object built
    from the CURRENT data of the user (current role); False refuses the refresh (user disabled, deleted).
    If the event is not assigned, the configuration stored with the token is reused.}
  TAIRefreshUserEvent = function(anUserId: Integer; out aUserConfig: TJSONObject): Boolean of object;

  // Texts of the login page. Defaults are in Italian and NEUTRAL (they do not name any kind of client): the
  // application sets its own wording (for IdroWorld, through the [OAuth] section of the INI file).
  // LoginIntro may contain two placeholders, replaced when the page is built: {host} = the host the user is
  // sent back to, {app} = ServerTitle. A plain text replacement is used (not Format), so any other character,
  // "%" included, is safe.
  TLBOAuthTexts = class(TObject)
  public
    ServerTitle        : String;   // name of the application, shown in the page title and in {app}
    LoginHeading       : String;
    LoginIntro         : String;   // {host} = host the user is sent back to, {app} = ServerTitle
    UsernameLabel      : String;
    PasswordLabel      : String;
    SubmitLabel        : String;
    BadCredentials     : String;
    TooManyAttempts    : String;
    RequestExpired     : String;
    InvalidRedirect    : String;
    constructor Create;
    // Copies every text of aSource (all of them, empty ones included).
    procedure Assign(aSource: TLBOAuthTexts);
  end;

  TAuthRequestItem = class(TObject)
  public
    ClientId      : String;
    RedirectURI   : String;
    State         : String;
    CodeChallenge : String;
    Expires       : Int64;
  end;

  TAuthCodeItem = class(TObject)
  public
    ClientId      : String;
    RedirectURI   : String;
    CodeChallenge : String;
    UserId        : Integer;
    UserConfig    : TJSONObject;   // owned
    Expires       : Int64;
    destructor Destroy; override;
  end;

  TLoginFailureItem = class(TObject)
  public
    Count       : Integer;
    WindowStart : Int64;
    LockedUntil : Int64;
  end;

  { TLBOAuthProvider }

  TLBOAuthProvider = class(TRequestChainProcessor)
  strict private
    FTokenManager          : TTokenManager;
    FPublicBaseURL         : String;
    FAllowHostFallback     : Boolean;
    FResourcePath          : String;
    FAccessScope           : String;
    FRefreshScope          : String;
    FScopeName             : String;
    FOAuthPath             : String;
    FAccessTokenLifetime   : Int64;
    FRefreshTokenLifetime  : Int64;
    FAuthRequestLifetime   : Int64;
    FCodeLifetime          : Int64;
    FAllowedRedirectURIs   : TStringList;
    FAllowLoopbackRedirects: Boolean;
    FTrustForwardedFor     : Boolean;
    FMaxLoginFailures      : Integer;
    FLoginLockSeconds      : Int64;
    FTexts                 : TLBOAuthTexts;
    FOnAuthenticateCredentials : TAICredentialsEvent;
    FOnRefreshUser         : TAIRefreshUserEvent;

    FCS                    : TCriticalSection;
    FRequests              : TStringList;   // request id -> TAuthRequestItem
    FCodes                 : TStringList;   // code       -> TAuthCodeItem
    FFailures              : TStringList;   // key        -> TLoginFailureItem

    function BaseURL(aHeaders: TStringList): String;
    function ClientAddress(RequestManager: THTTPRequestManager; aHeaders: TStringList): String;
    function IsAllowedRedirect(const aURI: String): Boolean;
    procedure EvictOldest(aList: TStringList; aKind: Integer);

    procedure PurgeExpired();
    function LoginBlocked(const aKeyAddress, aKeyUser: String): Boolean;
    procedure NoteLoginResult(const aKeyAddress, aKeyUser: String; aSuccess: Boolean);

    procedure Reply(aStatus: Integer; const aContentType, aBody: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure ReplyJSON(aStatus: Integer; aData: TJSONData; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure ReplyOAuthError(aStatus: Integer; const anError, aDescription: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure ReplyHTMLPage(aStatus: Integer; const aBody: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure ReplyRedirect(const aLocation: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure ReplyRedirectError(const aRedirectURI, anError, aState: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);

    function BuildLoginPage(const aRequestId, aRedirectURI, aMessage: String): String;
    function BuildMessagePage(const aMessage: String): String;

    procedure HandleProtectedResourceMetadata(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleServerMetadata(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleRegister(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleAuthorizeGet(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleAuthorizePost(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
      ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleToken(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);

    function IssueTokens(anUserId: Integer; aUserConfig: TJSONObject; const aClientId: String;
      out aResponse: TJSONObject): Boolean;
    procedure HandleGrantAuthorizationCode(aForm: TStringList; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure HandleGrantRefreshToken(aForm: TStringList; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);

  strict protected
    function DoProcessRequest(
      RequestManager  : THTTPRequestManager;
      HTTPParser      : THTTPRequestParser;
      ResponseHeaders : TStringList;
      var ResponseData: TMemoryStream;
      out ResponseCode: Integer): Boolean; override;

  public
    constructor Create(aTokenManager: TTokenManager);
    destructor Destroy; override;

    {:Address of the protected-resource metadata, to put in the 401 answer (used by TLBAIConnector).}
    function GetResourceMetadataURL(aHeaders: TStringList): String;
    {:Origin (scheme://host[:port]) of PublicBaseURL, '' if not set. The connector accepts it as Origin.}
    function PublicOrigin(): String;
    {:Checks the configuration and writes the problems to the log (missing or non-HTTPS PublicBaseURL,
      redirect wildcards). Called by TLBAIConnector.Prepare. False if the provider cannot work.}
    function ValidateConfiguration(): Boolean;

    {:External address of the server, e.g. "https://cmms.example.com" (no trailing slash, no path).
      It must be the address the AI client uses, absolute and HTTPS in production.}
    property PublicBaseURL: String read FPublicBaseURL write FPublicBaseURL;
    {:Development only: when PublicBaseURL is empty, build the address from the Host header. The header is
      chosen by the client, so in production leave False (the OAuth endpoints then answer server_error).}
    property AllowHostFallback: Boolean read FAllowHostFallback write FAllowHostFallback;
    {:Path of the protected resource (set by TLBAIConnector.OAuthProvider; set it yourself without the connector). Default "/mcp".}
    property ResourcePath: String read FResourcePath write FResourcePath;
    {:Scope (in TTokenManager) of the access tokens and of the refresh tokens this provider issues. Defaults:
      cTokenScope_AIAccess and cTokenScope_AIRefresh, which is what TLBAIConnector validates. An application that
      uses OAuth for something else sets its own names (not "web") and validates its tokens with
      TTokenManager.isValidBearerRequestForScope. Set before the first request.}
    property AccessScope: String read FAccessScope write FAccessScope;
    property RefreshScope: String read FRefreshScope write FRefreshScope;
    {:Name of the OAuth scope shown in the metadata and in the token answer. Default "mcp".}
    property ScopeName: String read FScopeName write FScopeName;
    {:Base path of the OAuth endpoints. Default "/oauth".}
    property OAuthPath: String read FOAuthPath write FOAuthPath;

    property AccessTokenLifetime: Int64 read FAccessTokenLifetime write FAccessTokenLifetime;    // seconds, default 900 (the role of the user is re-read at every renewal)
    property RefreshTokenLifetime: Int64 read FRefreshTokenLifetime write FRefreshTokenLifetime; // seconds, default 30 days

    {:Redirect URIs that may receive an authorization code. An entry ending with "*" matches by prefix.
      Default: Claude's callback (https://claude.ai/api/mcp/auth_callback).}
    property AllowedRedirectURIs: TStringList read FAllowedRedirectURIs;
    {:Accept http://localhost, http://127.0.0.1 and http://[::1] (any port) as redirect: needed by Claude Code. Default True.}
    property AllowLoopbackRedirects: Boolean read FAllowLoopbackRedirects write FAllowLoopbackRedirects;
    {:Take the client address from X-Forwarded-For (only when a reverse proxy you control sets it).}
    property TrustForwardedFor: Boolean read FTrustForwardedFor write FTrustForwardedFor;
    {:Failed logins allowed (per address and per user name) before a pause of LoginLockSeconds. Default 5 / 600.}
    property MaxLoginFailures: Integer read FMaxLoginFailures write FMaxLoginFailures;
    property LoginLockSeconds: Int64 read FLoginLockSeconds write FLoginLockSeconds;

    property Texts: TLBOAuthTexts read FTexts;
    property OnAuthenticateCredentials: TAICredentialsEvent read FOnAuthenticateCredentials write FOnAuthenticateCredentials;
    property OnRefreshUser: TAIRefreshUserEvent read FOnRefreshUser write FOnRefreshUser;
  end;

implementation

uses
  ULBLogger, uHTTPConsts, uLBRequestSignature, httpprotocol, base64, jsonparser, jsonscanner, StrUtils;

const
  cMe = 'TLBOAuthProvider';

  cWellKnownResource   = String('/.well-known/oauth-protected-resource');
  cWellKnownServer     = String('/.well-known/oauth-authorization-server');
  cDefaultOAuthPath    = String('/oauth');
  cDefaultResourcePath = String('/mcp');
  cClaudeCallback      = String('https://claude.ai/api/mcp/auth_callback');
  cScope               = String('mcp');

  cMaxFailureEntries   = Integer(5000);
  cMaxPendingEntries   = Integer(2000);
  cMaxCodeEntries      = Integer(2000);
  cFailureWindowSecs   = Int64(600);

{ ---- small helpers ------------------------------------------------------ }

function HTMLEscape(const aText: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(aText) do
    case aText[i] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
    else
      Result := Result + aText[i];
    end;
end;

function URLHost(const aURL: String): String;
var
  _Rest : String;
  p     : Integer;
begin
  Result := '';
  p := Pos('://', aURL);
  if p = 0 then Exit;
  _Rest := Copy(aURL, p + 3, MaxInt);
  p := 1;
  while (p <= Length(_Rest)) and (not (_Rest[p] in ['/', '?', '#'])) do
    Inc(p);
  Result := Copy(_Rest, 1, p - 1);
end;

// Text that comes from the client and goes into the log: no control characters (log forging), bounded length.
function LogSafe(const aText: String): String;
var
  i: Integer;
begin
  Result := Copy(aText, 1, 100);
  for i := 1 to Length(Result) do
    if Result[i] < #32 then
      Result[i] := '?';
end;

function URLScheme(const aURL: String): String;
var
  p: Integer;
begin
  p := Pos('://', aURL);
  if p = 0 then Result := '' else Result := LowerCase(Copy(aURL, 1, p - 1));
end;

function IsPlainURL(const aURL: String): Boolean;
var
  i: Integer;
begin
  // no control characters, spaces or non-ASCII: the value ends up in a Location header
  Result := aURL <> '';
  for i := 1 to Length(aURL) do
    if (aURL[i] <= #32) or (aURL[i] >= #127) then
      Exit(False);
end;

procedure ParseForm(const aText: String; aTarget: TStringList);
var
  _Pairs : TStringArray;
  i, p   : Integer;
  _Name, _Value: String;
begin
  aTarget.Clear;
  if aText = '' then Exit;
  _Pairs := aText.Split(['&']);
  for i := 0 to High(_Pairs) do
  begin
    p := Pos('=', _Pairs[i]);
    if p = 0 then
    begin
      _Name := HTTPDecode(_Pairs[i]);
      _Value := '';
    end
    else begin
      _Name := HTTPDecode(Copy(_Pairs[i], 1, p - 1));
      _Value := HTTPDecode(Copy(_Pairs[i], p + 1, MaxInt));
    end;
    if (_Name <> '') and (aTarget.IndexOfName(_Name) < 0) then
      aTarget.Add(_Name + '=' + _Value);
  end;
end;

function BodyAsString(HTTPParser: THTTPRequestParser): String;
begin
  Result := '';
  if (HTTPParser.Body <> nil) and (HTTPParser.Body.Size > 0) then
  begin
    HTTPParser.Body.Position := 0;
    SetLength(Result, HTTPParser.Body.Size);
    HTTPParser.Body.ReadBuffer(Result[1], HTTPParser.Body.Size);
  end;
end;

function ParseJSONObject(const aText: String): TJSONObject;
var
  _Parser : TJSONParser;
  _Data   : TJSONData;
begin
  Result := nil;
  if Trim(aText) = '' then Exit;
  try
    _Parser := TJSONParser.Create(aText, [joUTF8]);
    try
      _Data := _Parser.Parse;
    finally
      _Parser.Free;
    end;
    if _Data is TJSONObject then
      Result := TJSONObject(_Data)
    else
      _Data.Free;
  except
    Result := nil;
  end;
end;

function Base64URL(const aRaw: RawByteString): String;
begin
  Result := EncodeStringBase64(aRaw);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function HexToRaw(const aHex: String): RawByteString;
var
  i: Integer;
begin
  SetLength(Result, Length(aHex) div 2);
  for i := 1 to Length(Result) do
    Result[i] := AnsiChar(StrToIntDef('$' + Copy(aHex, i * 2 - 1, 2), 0));
end;

function PKCEMatches(const aVerifier, aChallenge: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  // RFC 7636: 43..128 characters from the unreserved set
  if (Length(aVerifier) < 43) or (Length(aVerifier) > 128) then Exit;
  for i := 1 to Length(aVerifier) do
    if not (aVerifier[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~']) then Exit;
  Result := LBConstantTimeSameStr(Base64URL(HexToRaw(LBSHA256Hex(RawByteString(aVerifier)))), aChallenge);
end;

{ TLBOAuthTexts }

constructor TLBOAuthTexts.Create;
begin
  inherited Create;
  ServerTitle := 'Applicazione';
  LoginHeading := 'Accesso';
  LoginIntro := 'Accedi con il tuo account per consentire a {host} di usare {app} con i tuoi permessi.';
  UsernameLabel := 'Nome utente';
  PasswordLabel := 'Password';
  SubmitLabel := 'Accedi';
  BadCredentials := 'Nome utente o password non validi.';
  TooManyAttempts := 'Troppi tentativi falliti. Riprova tra qualche minuto.';
  RequestExpired := 'La richiesta di accesso e'' scaduta. Torna all''applicazione che ti ha inviato qui e riprova.';
  InvalidRedirect := 'Richiesta non valida: l''indirizzo di ritorno non e'' autorizzato.';
end;

procedure TLBOAuthTexts.Assign(aSource: TLBOAuthTexts);
begin
  if aSource = nil then Exit;
  ServerTitle := aSource.ServerTitle;
  LoginHeading := aSource.LoginHeading;
  LoginIntro := aSource.LoginIntro;
  UsernameLabel := aSource.UsernameLabel;
  PasswordLabel := aSource.PasswordLabel;
  SubmitLabel := aSource.SubmitLabel;
  BadCredentials := aSource.BadCredentials;
  TooManyAttempts := aSource.TooManyAttempts;
  RequestExpired := aSource.RequestExpired;
  InvalidRedirect := aSource.InvalidRedirect;
end;

{ TAuthCodeItem }

destructor TAuthCodeItem.Destroy;
begin
  FreeAndNil(UserConfig);
  inherited Destroy;
end;

{ TLBOAuthProvider }

constructor TLBOAuthProvider.Create(aTokenManager: TTokenManager);
begin
  inherited Create;
  FTokenManager := aTokenManager;
  FPublicBaseURL := '';
  FResourcePath := cDefaultResourcePath;
  FAccessScope := cTokenScope_AIAccess;
  FRefreshScope := cTokenScope_AIRefresh;
  FScopeName := cScope;
  FOAuthPath := cDefaultOAuthPath;
  FAllowHostFallback := False;
  FAccessTokenLifetime := 900;   // 15 minutes: the role is re-read at every renewal
  FRefreshTokenLifetime := 30 * 24 * 3600;
  FAuthRequestLifetime := 600;
  FCodeLifetime := 300;
  FAllowLoopbackRedirects := True;
  FTrustForwardedFor := False;
  FMaxLoginFailures := 5;
  FLoginLockSeconds := 600;

  FAllowedRedirectURIs := TStringList.Create;
  FAllowedRedirectURIs.Add(cClaudeCallback);

  FTexts := TLBOAuthTexts.Create;

  FCS := TCriticalSection.Create;
  FRequests := TStringList.Create;
  FRequests.OwnsObjects := True;
  FRequests.Sorted := True;
  FRequests.CaseSensitive := True;
  FCodes := TStringList.Create;
  FCodes.OwnsObjects := True;
  FCodes.Sorted := True;
  FCodes.CaseSensitive := True;
  FFailures := TStringList.Create;
  FFailures.OwnsObjects := True;
  FFailures.Sorted := True;
  FFailures.CaseSensitive := True;
end;

destructor TLBOAuthProvider.Destroy;
begin
  FreeAndNil(FFailures);
  FreeAndNil(FCodes);
  FreeAndNil(FRequests);
  FreeAndNil(FCS);
  FreeAndNil(FTexts);
  FreeAndNil(FAllowedRedirectURIs);
  inherited Destroy;
end;

function TLBOAuthProvider.BaseURL(aHeaders: TStringList): String;
begin
  Result := FPublicBaseURL;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    SetLength(Result, Length(Result) - 1);

  if (Result = '') and FAllowHostFallback then
  begin
    // Development only: whatever host the client used. The header is not trustworthy.
    Result := 'https://' + Trim(aHeaders.Values[HTTP_HEADER_HOST]);
    LBLogger.Write(5, cMe + '.BaseURL', lmt_Warning, 'PublicBaseURL not set: using <%s> (AllowHostFallback)', [Result]);
  end;
end;

function TLBOAuthProvider.PublicOrigin(): String;
var
  _Scheme, _Host: String;
begin
  Result := '';
  if FPublicBaseURL = '' then Exit;
  _Scheme := URLScheme(FPublicBaseURL);
  _Host := URLHost(FPublicBaseURL);
  if (_Scheme <> '') and (_Host <> '') then
    Result := _Scheme + '://' + _Host;
end;

function TLBOAuthProvider.ValidateConfiguration(): Boolean;
var
  i: Integer;
begin
  Result := True;
  if FPublicBaseURL = '' then
  begin
    if FAllowHostFallback then
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Warning,
        'PublicBaseURL not set: the address is taken from the Host header (AllowHostFallback, development only)')
    else begin
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Error,
        'PublicBaseURL not set: the OAuth endpoints will answer server_error. Set the public HTTPS address of the server');
      Result := False;
    end;
  end
  else begin
    if (URLScheme(FPublicBaseURL) <> 'https') then
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Warning,
        'PublicBaseURL <%s> is not HTTPS: Claude.ai needs a public HTTPS address and the password travels in the login form', [FPublicBaseURL]);
    if (Pos('?', FPublicBaseURL) > 0) or (Pos('#', FPublicBaseURL) > 0) then
    begin
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Error, 'PublicBaseURL must not contain a query or a fragment');
      Result := False;
    end;
    // only scheme, host and port: the paths (/mcp, /oauth/..., /.well-known/...) are added by this provider
    if (Pos('://', FPublicBaseURL) > 0) and (Pos('/', Copy(FPublicBaseURL, Pos('://', FPublicBaseURL) + 3, MaxInt)) > 0) then
    begin
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Error,
        'PublicBaseURL <%s> must not contain a path: write only scheme, host and port (the MCP path comes from Routes.xml)', [FPublicBaseURL]);
      Result := False;
    end;
  end;
  for i := 0 to FAllowedRedirectURIs.Count - 1 do
    if (FAllowedRedirectURIs[i] <> '') and (FAllowedRedirectURIs[i][Length(FAllowedRedirectURIs[i])] = '*') then
      LBLogger.Write(1, cMe + '.ValidateConfiguration', lmt_Warning,
        'Redirect URI <%s> is a prefix wildcard: prefer exact addresses', [FAllowedRedirectURIs[i]]);
end;

function TLBOAuthProvider.GetResourceMetadataURL(aHeaders: TStringList): String;
begin
  Result := Self.BaseURL(aHeaders) + cWellKnownResource + FResourcePath;
end;

function TLBOAuthProvider.ClientAddress(RequestManager: THTTPRequestManager; aHeaders: TStringList): String;
var
  _Forwarded : String;
  p          : Integer;
begin
  Result := '';
  if FTrustForwardedFor then
  begin
    // The LAST entry is the one appended by the trusted reverse proxy; the first ones are written by the
    // client and can be changed at will (they would let it dodge the per-address lock).
    _Forwarded := Trim(aHeaders.Values[HTTP_HEADER_X_FORWARDED_FOR]);
    p := LastDelimiter(',', _Forwarded);
    if p > 0 then
      _Forwarded := Trim(Copy(_Forwarded, p + 1, MaxInt));
    Result := _Forwarded;
  end;
  if (Result = '') and (RequestManager <> nil) then
    Result := RequestManager.GetRemoteIP();
end;

function TLBOAuthProvider.IsAllowedRedirect(const aURI: String): Boolean;
var
  i       : Integer;
  _Entry  : String;
  _Host   : String;
  _Bare   : String;
begin
  Result := False;
  if not IsPlainURL(aURI) then Exit;
  if Pos('#', aURI) > 0 then Exit;   // a redirect URI may not carry a fragment (RFC 6749)
  // No user information ("http://localhost:80@evil.com/": the browser goes to evil.com) and no backslash
  // (some browsers read it as "/"): they would let the host checks below be fooled.
  if (Pos('@', URLHost(aURI)) > 0) or (Pos('\', aURI) > 0) then Exit;

  for i := 0 to FAllowedRedirectURIs.Count - 1 do
  begin
    _Entry := FAllowedRedirectURIs[i];
    if _Entry = '' then Continue;
    if _Entry[Length(_Entry)] = '*' then
    begin
      if SameText(Copy(aURI, 1, Length(_Entry) - 1), Copy(_Entry, 1, Length(_Entry) - 1)) then
        Exit(True);
    end
    else if SameStr(aURI, _Entry) then
      Exit(True);
  end;

  if FAllowLoopbackRedirects and (URLScheme(aURI) = 'http') then
  begin
    _Host := LowerCase(URLHost(aURI));
    // strip the port
    if (_Host <> '') and (_Host[1] = '[') then
      _Bare := Copy(_Host, 1, Pos(']', _Host))
    else if Pos(':', _Host) > 0 then
      _Bare := Copy(_Host, 1, Pos(':', _Host) - 1)
    else
      _Bare := _Host;
    Result := (_Bare = 'localhost') or (_Bare = '127.0.0.1') or (_Bare = '[::1]');
  end;
end;

{ ---- state: pending requests, codes, failures -------------------------- }

procedure TLBOAuthProvider.EvictOldest(aList: TStringList; aKind: Integer);
var
  i, _Oldest : Integer;
  _Min, _Cur : Int64;
begin
  // Hard cap reached after purging the expired entries: drop the ONE entry that expires first (never
  // the whole list). aKind: 0 requests, 1 codes, 2 failures (oldest window, never a locked one).
  _Oldest := -1;
  _Min := High(Int64);
  for i := 0 to aList.Count - 1 do
  begin
    case aKind of
      0: _Cur := TAuthRequestItem(aList.Objects[i]).Expires;
      1: _Cur := TAuthCodeItem(aList.Objects[i]).Expires;
    else
      if TLoginFailureItem(aList.Objects[i]).LockedUntil > LBUtcUnixTime then Continue;
      _Cur := TLoginFailureItem(aList.Objects[i]).WindowStart;
    end;
    if _Cur < _Min then
    begin
      _Min := _Cur;
      _Oldest := i;
    end;
  end;
  if _Oldest >= 0 then
    aList.Delete(_Oldest);
end;

procedure TLBOAuthProvider.PurgeExpired();
var
  i    : Integer;
  _Now : Int64;
begin
  // called with FCS held
  _Now := LBUtcUnixTime;
  for i := FRequests.Count - 1 downto 0 do
    if TAuthRequestItem(FRequests.Objects[i]).Expires < _Now then
      FRequests.Delete(i);
  for i := FCodes.Count - 1 downto 0 do
    if TAuthCodeItem(FCodes.Objects[i]).Expires < _Now then
      FCodes.Delete(i);
  // expired failure records are always dropped (the lists are small: the caps below are hard limits)
  for i := FFailures.Count - 1 downto 0 do
    if (TLoginFailureItem(FFailures.Objects[i]).LockedUntil < _Now) and
       (TLoginFailureItem(FFailures.Objects[i]).WindowStart + cFailureWindowSecs < _Now) then
      FFailures.Delete(i);
end;

function TLBOAuthProvider.LoginBlocked(const aKeyAddress, aKeyUser: String): Boolean;
var
  _Keys : array[0..1] of String;
  i, p  : Integer;
begin
  Result := False;
  if FMaxLoginFailures <= 0 then Exit;
  _Keys[0] := 'a:' + aKeyAddress;
  _Keys[1] := 'u:' + LowerCase(aKeyUser);
  FCS.Enter;
  try
    for i := 0 to 1 do
      if FFailures.Find(_Keys[i], p) then
        if TLoginFailureItem(FFailures.Objects[p]).LockedUntil > LBUtcUnixTime then
          Exit(True);
  finally
    FCS.Leave;
  end;
end;

procedure TLBOAuthProvider.NoteLoginResult(const aKeyAddress, aKeyUser: String; aSuccess: Boolean);
var
  _Keys  : array[0..1] of String;
  i, p   : Integer;
  _Item  : TLoginFailureItem;
  _Now   : Int64;
begin
  if FMaxLoginFailures <= 0 then Exit;
  _Keys[0] := 'a:' + aKeyAddress;
  _Keys[1] := 'u:' + LowerCase(aKeyUser);
  _Now := LBUtcUnixTime;
  FCS.Enter;
  try
    for i := 0 to 1 do
    begin
      if aSuccess then
      begin
        if FFailures.Find(_Keys[i], p) then
          FFailures.Delete(p);
        Continue;
      end;

      if FFailures.Find(_Keys[i], p) then
        _Item := TLoginFailureItem(FFailures.Objects[p])
      else begin
        _Item := TLoginFailureItem.Create;
        _Item.WindowStart := _Now;
        while FFailures.Count >= cMaxFailureEntries do
        begin
          p := FFailures.Count;
          Self.EvictOldest(FFailures, 2);
          if FFailures.Count = p then Break;   // only locked entries left: never unlock them to make room
        end;
        FFailures.AddObject(_Keys[i], _Item);
      end;

      if _Item.WindowStart + cFailureWindowSecs < _Now then
      begin
        _Item.WindowStart := _Now;
        _Item.Count := 0;
      end;
      Inc(_Item.Count);
      if _Item.Count >= FMaxLoginFailures then
      begin
        _Item.LockedUntil := _Now + FLoginLockSeconds;
        _Item.Count := 0;
        LBLogger.Write(1, cMe, lmt_Warning, 'Too many failed AI logins for <%s>: paused for %d s', [LogSafe(_Keys[i]), FLoginLockSeconds]);
      end;
    end;
  finally
    FCS.Leave;
  end;
end;

{ ---- replies ------------------------------------------------------------ }

procedure TLBOAuthProvider.Reply(aStatus: Integer; const aContentType, aBody: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  if ResponseData = nil then
    ResponseData := TMemoryStream.Create
  else
    ResponseData.Clear;

  if aContentType <> '' then
    ResponseHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + aContentType);
  ResponseHeaders.Add(HTTP_HEADER_CACHE_CONTROL + ': no-store');
  ResponseHeaders.Add(HTTP_HEADER_PRAGMA + ': no-cache');

  if Length(aBody) > 0 then
    ResponseData.WriteBuffer(aBody[1], Length(aBody));
  ResponseData.Position := 0;
  ResponseCode := aStatus;
end;

procedure TLBOAuthProvider.ReplyJSON(aStatus: Integer; aData: TJSONData; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  aData.CompressedJSON := True;
  Self.Reply(aStatus, MIME_TYPE_JSON, aData.AsJSON, ResponseHeaders, ResponseData, ResponseCode);
end;

procedure TLBOAuthProvider.ReplyOAuthError(aStatus: Integer; const anError, aDescription: String;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Obj: TJSONObject;
begin
  _Obj := TJSONObject.Create;
  try
    _Obj.Add('error', anError);
    if aDescription <> '' then
      _Obj.Add('error_description', aDescription);
    Self.ReplyJSON(aStatus, _Obj, ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Obj.Free;
  end;
end;

procedure TLBOAuthProvider.ReplyHTMLPage(aStatus: Integer; const aBody: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  ResponseHeaders.Add(HTTP_HEADER_X_FRAME_OPTIONS + ': DENY');
  ResponseHeaders.Add(HTTP_HEADER_CONTENT_SECURITY_POLICY + ': default-src ''none''; style-src ''unsafe-inline''; frame-ancestors ''none''');
  Self.Reply(aStatus, MIME_TYPE_HTML + '; charset=utf-8', aBody, ResponseHeaders, ResponseData, ResponseCode);
end;

procedure TLBOAuthProvider.ReplyRedirect(const aLocation: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  ResponseHeaders.Add(HTTP_HEADER_LOCATION + ': ' + aLocation);
  Self.Reply(HTTP_STATUS_FOUND, '', '', ResponseHeaders, ResponseData, ResponseCode);
end;

procedure TLBOAuthProvider.ReplyRedirectError(const aRedirectURI, anError, aState: String;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Sep: String;
begin
  if Pos('?', aRedirectURI) > 0 then _Sep := '&' else _Sep := '?';
  Self.ReplyRedirect(aRedirectURI + _Sep + 'error=' + HTTPEncode(anError) +
    IfThen(aState <> '', '&state=' + HTTPEncode(aState), ''), ResponseHeaders, ResponseData, ResponseCode);
end;

function TLBOAuthProvider.BuildMessagePage(const aMessage: String): String;
begin
  Result :=
    '<!DOCTYPE html><html lang="it"><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width,initial-scale=1">' +
    '<title>' + HTMLEscape(FTexts.ServerTitle) + '</title>' +
    '<style>body{font-family:system-ui,sans-serif;margin:0;display:flex;min-height:100vh;align-items:center;justify-content:center;background:#f4f5f7;color:#1c1e21}' +
    'main{background:#fff;padding:2rem;border-radius:12px;max-width:26rem;width:calc(100% - 2rem);box-shadow:0 2px 12px rgba(0,0,0,.1)}' +
    '@media(prefers-color-scheme:dark){body{background:#17181a;color:#e6e6e6}main{background:#222428}}</style></head>' +
    '<body><main><h1>' + HTMLEscape(FTexts.ServerTitle) + '</h1><p>' + HTMLEscape(aMessage) + '</p></main></body></html>';
end;

function TLBOAuthProvider.BuildLoginPage(const aRequestId, aRedirectURI, aMessage: String): String;
begin
  Result :=
    '<!DOCTYPE html><html lang="it"><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width,initial-scale=1">' +
    '<title>' + HTMLEscape(FTexts.ServerTitle + ' - ' + FTexts.LoginHeading) + '</title>' +
    '<style>' +
    'body{font-family:system-ui,sans-serif;margin:0;display:flex;min-height:100vh;align-items:center;justify-content:center;background:#f4f5f7;color:#1c1e21}' +
    'main{background:#fff;padding:2rem;border-radius:12px;max-width:26rem;width:calc(100% - 2rem);box-shadow:0 2px 12px rgba(0,0,0,.1)}' +
    'h1{font-size:1.4rem;margin:0 0 .5rem}p{line-height:1.45}label{display:block;margin:1rem 0 .25rem;font-weight:600}' +
    'input{width:100%;box-sizing:border-box;padding:.6rem;font-size:1rem;border:1px solid #9aa0a6;border-radius:6px;background:inherit;color:inherit}' +
    'button{margin-top:1.25rem;width:100%;padding:.7rem;font-size:1rem;border:0;border-radius:6px;background:#1a56db;color:#fff;cursor:pointer}' +
    '.err{color:#b3261e;font-weight:600}' +
    '@media(prefers-color-scheme:dark){body{background:#17181a;color:#e6e6e6}main{background:#222428}.err{color:#f2b8b5}}' +
    '</style></head><body><main>' +
    '<h1>' + HTMLEscape(FTexts.ServerTitle) + ' - ' + HTMLEscape(FTexts.LoginHeading) + '</h1>' +
    '<p>' + HTMLEscape(StringReplace(StringReplace(FTexts.LoginIntro, '{app}', FTexts.ServerTitle, [rfReplaceAll, rfIgnoreCase]),
      '{host}', URLHost(aRedirectURI), [rfReplaceAll, rfIgnoreCase])) + '</p>' +
    IfThen(aMessage <> '', '<p class="err">' + HTMLEscape(aMessage) + '</p>', '') +
    '<form method="post" action="' + HTMLEscape(FOAuthPath + '/authorize') + '">' +
    '<input type="hidden" name="req" value="' + HTMLEscape(aRequestId) + '">' +
    '<label for="u">' + HTMLEscape(FTexts.UsernameLabel) + '</label>' +
    '<input id="u" name="username" autocomplete="username" autocapitalize="none" required autofocus>' +
    '<label for="p">' + HTMLEscape(FTexts.PasswordLabel) + '</label>' +
    '<input id="p" name="password" type="password" autocomplete="current-password" required>' +
    '<button type="submit">' + HTMLEscape(FTexts.SubmitLabel) + '</button>' +
    '</form></main></body></html>';
end;

{ ---- metadata ----------------------------------------------------------- }

procedure TLBOAuthProvider.HandleProtectedResourceMetadata(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Obj : TJSONObject;
  _Arr : TJSONArray;
begin
  _Obj := TJSONObject.Create;
  try
    // "resource" must be exactly the URL the user gives to the AI client
    _Obj.Add('resource', Self.BaseURL(HTTPParser.Headers) + FResourcePath);
    _Arr := TJSONArray.Create;
    _Arr.Add(Self.BaseURL(HTTPParser.Headers));
    _Obj.Add('authorization_servers', _Arr);
    _Arr := TJSONArray.Create;
    _Arr.Add('header');
    _Obj.Add('bearer_methods_supported', _Arr);
    _Arr := TJSONArray.Create;
    _Arr.Add(FScopeName);
    _Obj.Add('scopes_supported', _Arr);
    _Obj.Add('resource_name', FTexts.ServerTitle);
    Self.ReplyJSON(HTTP_STATUS_OK, _Obj, ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Obj.Free;
  end;
end;

procedure TLBOAuthProvider.HandleServerMetadata(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Obj  : TJSONObject;
  _Base : String;

  function Arr(const anItems: array of String): TJSONArray;
  var
    i: Integer;
  begin
    Result := TJSONArray.Create;
    for i := Low(anItems) to High(anItems) do
      Result.Add(anItems[i]);
  end;

begin
  _Base := Self.BaseURL(HTTPParser.Headers);
  _Obj := TJSONObject.Create;
  try
    _Obj.Add('issuer', _Base);
    _Obj.Add('authorization_endpoint', _Base + FOAuthPath + '/authorize');
    _Obj.Add('token_endpoint', _Base + FOAuthPath + '/token');
    _Obj.Add('registration_endpoint', _Base + FOAuthPath + '/register');
    _Obj.Add('response_types_supported', Arr(['code']));
    _Obj.Add('grant_types_supported', Arr(['authorization_code', 'refresh_token']));
    _Obj.Add('code_challenge_methods_supported', Arr(['S256']));
    _Obj.Add('token_endpoint_auth_methods_supported', Arr(['none']));
    _Obj.Add('scopes_supported', Arr([FScopeName]));
    Self.ReplyJSON(HTTP_STATUS_OK, _Obj, ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Obj.Free;
  end;
end;

procedure TLBOAuthProvider.HandleRegister(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Req       : TJSONObject;
  _Uris      : TJSONData;
  _Out       : TJSONObject;
  _OutUris   : TJSONArray;
  _GrantTypes, _RespTypes: TJSONArray;
  _ClientId  : String;
  _Name      : String;
  i          : Integer;
begin
  _Req := ParseJSONObject(BodyAsString(HTTPParser));
  if _Req = nil then
  begin
    Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_client_metadata', 'The body must be a JSON object', ResponseHeaders, ResponseData, ResponseCode);
    Exit;
  end;
  try
    _Uris := _Req.Find('redirect_uris');
    if (_Uris = nil) or (_Uris.JSONType <> jtArray) or (TJSONArray(_Uris).Count = 0) then
    begin
      Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_redirect_uri', 'redirect_uris is required', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    _OutUris := TJSONArray.Create;
    for i := 0 to TJSONArray(_Uris).Count - 1 do
    begin
      if (TJSONArray(_Uris).Items[i].JSONType <> jtString) or
         (not Self.IsAllowedRedirect(TJSONArray(_Uris).Items[i].AsString)) then
      begin
        _OutUris.Free;
        LBLogger.Write(1, cMe, lmt_Warning, 'Client registration refused: redirect URI not allowed');
        Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_redirect_uri', 'This redirect URI is not allowed on this server', ResponseHeaders, ResponseData, ResponseCode);
        Exit;
      end;
      _OutUris.Add(TJSONArray(_Uris).Items[i].AsString);
    end;

    // Stateless client: the id is random and is not stored (see the unit header).
    _ClientId := FTokenManager.CreateToken();
    if _ClientId = '' then
    begin
      _OutUris.Free;
      Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'The server could not generate a secure identifier', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;
    _ClientId := 'mcp-' + _ClientId;
    _Name := _Req.Get('client_name', '');

    _Out := TJSONObject.Create;
    try
      _Out.Add('client_id', _ClientId);
      _Out.Add('client_id_issued_at', LBUtcUnixTime);
      if _Name <> '' then
        _Out.Add('client_name', _Name);
      _Out.Add('redirect_uris', _OutUris);
      _GrantTypes := TJSONArray.Create;
      _GrantTypes.Add('authorization_code');
      _GrantTypes.Add('refresh_token');
      _Out.Add('grant_types', _GrantTypes);
      _RespTypes := TJSONArray.Create;
      _RespTypes.Add('code');
      _Out.Add('response_types', _RespTypes);
      _Out.Add('token_endpoint_auth_method', 'none');
      Self.ReplyJSON(HTTP_STATUS_CREATED, _Out, ResponseHeaders, ResponseData, ResponseCode);
    finally
      _Out.Free;
    end;
  finally
    _Req.Free;
  end;
end;

{ ---- authorization ------------------------------------------------------ }

procedure TLBOAuthProvider.HandleAuthorizeGet(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Query    : TStringList;
  _Item     : TAuthRequestItem;
  _Id       : String;
  _State    : String;
  _Redirect : String;
  i         : Integer;
  p         : Integer;
  _Raw      : String;
begin
  // The query string is not decoded by the parser: do it here.
  _Query := TStringList.Create;
  try
    for i := 0 to HTTPParser.Params.Count - 1 do
    begin
      _Raw := HTTPParser.Params[i];
      p := Pos('=', _Raw);
      if p > 0 then
        _Query.Add(HTTPDecode(Copy(_Raw, 1, p - 1)) + '=' + HTTPDecode(Copy(_Raw, p + 1, MaxInt)))
      else
        _Query.Add(HTTPDecode(_Raw) + '=');
    end;

    _Redirect := _Query.Values['redirect_uri'];
    _State := _Query.Values['state'];

    // Without a trusted redirect URI nothing can be sent back: the error is shown to the user.
    if not Self.IsAllowedRedirect(_Redirect) then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, 'Authorization refused: redirect URI not allowed (<%s>)', [LogSafe(_Redirect)]);
      Self.ReplyHTMLPage(HTTP_STATUS_BAD_REQUEST, Self.BuildMessagePage(FTexts.InvalidRedirect), ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    if _Query.Values['response_type'] <> 'code' then
    begin
      Self.ReplyRedirectError(_Redirect, 'unsupported_response_type', _State, ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;
    if (_Query.Values['client_id'] = '') or (_Query.Values['code_challenge'] = '') or
       (_Query.Values['code_challenge_method'] <> 'S256') or (not Assigned(FOnAuthenticateCredentials)) then
    begin
      Self.ReplyRedirectError(_Redirect, 'invalid_request', _State, ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    _Item := TAuthRequestItem.Create;
    _Item.ClientId := _Query.Values['client_id'];
    _Item.RedirectURI := _Redirect;
    _Item.State := _State;
    _Item.CodeChallenge := _Query.Values['code_challenge'];
    _Item.Expires := LBUtcUnixTime + FAuthRequestLifetime;

    _Id := FTokenManager.CreateToken();
    if _Id = '' then
    begin
      _Item.Free;
      Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'The server could not generate a secure identifier', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;
    FCS.Enter;
    try
      Self.PurgeExpired();
      while FRequests.Count >= cMaxPendingEntries do
        Self.EvictOldest(FRequests, 0);   // flooded: drop the oldest pending request, never the whole list
      FRequests.AddObject(_Id, _Item);
    finally
      FCS.Leave;
    end;

    Self.ReplyHTMLPage(HTTP_STATUS_OK, Self.BuildLoginPage(_Id, _Redirect, ''), ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Query.Free;
  end;
end;

procedure TLBOAuthProvider.HandleAuthorizePost(RequestManager: THTTPRequestManager; HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Form        : TStringList;
  _Id          : String;
  _User, _Psw  : String;
  _Idx         : Integer;
  _Req         : TAuthRequestItem;
  _Address     : String;
  _UserId      : Integer;
  _Config      : TJSONObject;
  _Code        : TAuthCodeItem;
  _CodeStr     : String;
  _Ok          : Boolean;
  _Sep         : String;
begin
  _Form := TStringList.Create;
  try
    ParseForm(BodyAsString(HTTPParser), _Form);
    _Id := _Form.Values['req'];
    _User := Trim(_Form.Values['username']);
    _Psw := _Form.Values['password'];

    // look the request up WITHOUT consuming it: a wrong password must allow another attempt
    _Req := nil;
    FCS.Enter;
    try
      Self.PurgeExpired();
      if FRequests.Find(_Id, _Idx) then
      begin
        _Req := TAuthRequestItem.Create;   // private copy, so no lock is needed afterwards
        _Req.ClientId := TAuthRequestItem(FRequests.Objects[_Idx]).ClientId;
        _Req.RedirectURI := TAuthRequestItem(FRequests.Objects[_Idx]).RedirectURI;
        _Req.State := TAuthRequestItem(FRequests.Objects[_Idx]).State;
        _Req.CodeChallenge := TAuthRequestItem(FRequests.Objects[_Idx]).CodeChallenge;
        _Req.Expires := TAuthRequestItem(FRequests.Objects[_Idx]).Expires;
      end;
    finally
      FCS.Leave;
    end;

    if _Req = nil then
    begin
      Self.ReplyHTMLPage(HTTP_STATUS_BAD_REQUEST, Self.BuildMessagePage(FTexts.RequestExpired), ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    try
      _Address := Self.ClientAddress(RequestManager, HTTPParser.Headers);

      if Self.LoginBlocked(_Address, _User) then
      begin
        ResponseHeaders.Add(HTTP_HEADER_RETRY_AFTER + ': ' + IntToStr(FLoginLockSeconds));
        Self.ReplyHTMLPage(HTTP_STATUS_TOO_MANY_REQUESTS, Self.BuildLoginPage(_Id, _Req.RedirectURI, FTexts.TooManyAttempts),
          ResponseHeaders, ResponseData, ResponseCode);
        Exit;
      end;

      _UserId := 0;
      _Config := nil;
      _Ok := False;
      if (_User <> '') and (_Psw <> '') and Assigned(FOnAuthenticateCredentials) then
      try
        _Ok := FOnAuthenticateCredentials(_User, _Psw, _UserId, _Config) and (_Config <> nil) and (_UserId > 0);
      except
        on E: Exception do
        begin
          LBLogger.Write(1, cMe, lmt_Error, 'OnAuthenticateCredentials: %s', [E.Message]);
          _Ok := False;
        end;
      end;

      if not _Ok then
      begin
        FreeAndNil(_Config);
        Self.NoteLoginResult(_Address, _User, False);
        LBLogger.Write(1, cMe, lmt_Warning, 'AI login failed for <%s> from <%s>', [LogSafe(_User), LogSafe(_Address)]);
        Self.ReplyHTMLPage(HTTP_STATUS_UNAUTHORIZED, Self.BuildLoginPage(_Id, _Req.RedirectURI, FTexts.BadCredentials),
          ResponseHeaders, ResponseData, ResponseCode);
        Exit;
      end;

      Self.NoteLoginResult(_Address, _User, True);

      // success: the pending request is consumed and a one-time code is issued
      _Code := TAuthCodeItem.Create;
      _Code.ClientId := _Req.ClientId;
      _Code.RedirectURI := _Req.RedirectURI;
      _Code.CodeChallenge := _Req.CodeChallenge;
      _Code.UserId := _UserId;
      _Code.UserConfig := _Config;   // ownership passes to the item
      _Code.Expires := LBUtcUnixTime + FCodeLifetime;
      _CodeStr := FTokenManager.CreateToken();
      if _CodeStr = '' then
      begin
        _Code.Free;   // frees the user configuration it holds
        Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'The server could not generate a secure code', ResponseHeaders, ResponseData, ResponseCode);
        Exit;
      end;

      FCS.Enter;
      try
        if FRequests.Find(_Id, _Idx) then
          FRequests.Delete(_Idx);
        Self.PurgeExpired();
        while FCodes.Count >= cMaxCodeEntries do
          Self.EvictOldest(FCodes, 1);
        FCodes.AddObject(_CodeStr, _Code);
      finally
        FCS.Leave;
      end;

      LBLogger.Write(3, cMe, lmt_Info, 'AI login ok: user id=%d from <%s>', [_UserId, _Address]);

      if Pos('?', _Req.RedirectURI) > 0 then _Sep := '&' else _Sep := '?';
      Self.ReplyRedirect(_Req.RedirectURI + _Sep + 'code=' + HTTPEncode(_CodeStr) +
        IfThen(_Req.State <> '', '&state=' + HTTPEncode(_Req.State), ''), ResponseHeaders, ResponseData, ResponseCode);
    finally
      _Req.Free;
    end;
  finally
    _Form.Free;
  end;
end;

{ ---- token endpoint ------------------------------------------------------ }

function TLBOAuthProvider.IssueTokens(anUserId: Integer; aUserConfig: TJSONObject; const aClientId: String;
  out aResponse: TJSONObject): Boolean;
var
  _RefreshConfig : TJSONObject;
  _Access        : TJSONObject;
  _Refresh       : TJSONObject;
  _AccessStr     : String;
  _RefreshStr    : String;
begin
  Result := False;
  aResponse := nil;

  // aUserConfig is consumed by the first token; the refresh token gets its own copy.
  _RefreshConfig := TJSONObject(aUserConfig.Clone);

  if not FTokenManager.addNewScopedToken(anUserId, aUserConfig, FAccessScope, FAccessTokenLifetime, aClientId, _Access) then
  begin
    _RefreshConfig.Free;
    Exit;
  end;
  _AccessStr := _Access.Elements[cToken_Field_Token].AsString;

  if not FTokenManager.addNewScopedToken(anUserId, _RefreshConfig, FRefreshScope, FRefreshTokenLifetime, aClientId, _Refresh) then
  begin
    FTokenManager.RemoveToken(_AccessStr);
    Exit;
  end;
  _RefreshStr := _Refresh.Elements[cToken_Field_Token].AsString;

  aResponse := TJSONObject.Create;
  aResponse.Add('access_token', _AccessStr);
  aResponse.Add('token_type', 'Bearer');
  aResponse.Add('expires_in', FAccessTokenLifetime);
  aResponse.Add('refresh_token', _RefreshStr);
  aResponse.Add('scope', FScopeName);
  Result := True;
end;

procedure TLBOAuthProvider.HandleGrantAuthorizationCode(aForm: TStringList; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Idx      : Integer;
  _Code     : TAuthCodeItem;
  _CodeStr  : String;
  _Response : TJSONObject;
  _Valid    : Boolean;
begin
  _CodeStr := aForm.Values['code'];
  _Code := nil;

  // take the code out of the table first: one use only, whatever happens next
  FCS.Enter;
  try
    Self.PurgeExpired();
    if FCodes.Find(_CodeStr, _Idx) then
    begin
      _Code := TAuthCodeItem(FCodes.Objects[_Idx]);
      FCodes.Objects[_Idx] := nil;   // detach the object (the list owns its objects)
      FCodes.Delete(_Idx);           // removes the entry without freeing it
    end;
  finally
    FCS.Leave;
  end;

  if _Code = nil then
  begin
    Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_grant', 'The code is invalid or expired', ResponseHeaders, ResponseData, ResponseCode);
    Exit;
  end;

  try
    _Valid := (_Code.ClientId = aForm.Values['client_id']) and
              (_Code.RedirectURI = aForm.Values['redirect_uri']) and
              PKCEMatches(aForm.Values['code_verifier'], _Code.CodeChallenge);
    if not _Valid then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, 'Token request refused: client, redirect URI or PKCE verifier do not match');
      Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_grant', 'The code does not match the request', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    // IssueTokens consumes the configuration: detach it from the item
    if IssueTokens(_Code.UserId, _Code.UserConfig, _Code.ClientId, _Response) then
    begin
      _Code.UserConfig := nil;
      try
        Self.ReplyJSON(HTTP_STATUS_OK, _Response, ResponseHeaders, ResponseData, ResponseCode);
      finally
        _Response.Free;
      end;
    end
    else begin
      _Code.UserConfig := nil;
      Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'Could not create the tokens', ResponseHeaders, ResponseData, ResponseCode);
    end;
  finally
    _Code.Free;
  end;
end;

procedure TLBOAuthProvider.HandleGrantRefreshToken(aForm: TStringList; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _UserId   : Integer;
  _Config   : TJSONObject;
  _Fresh    : TJSONObject;
  _ClientId : String;
  _Response : TJSONObject;
begin
  // Single use: a refresh token that has been presented is gone, valid or not for this client.
  if not FTokenManager.TakeScopedToken(aForm.Values['refresh_token'], FRefreshScope, _UserId, _Config, _ClientId) then
  begin
    Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_grant', 'The refresh token is invalid or expired', ResponseHeaders, ResponseData, ResponseCode);
    Exit;
  end;

  if (aForm.Values['client_id'] <> '') and (aForm.Values['client_id'] <> _ClientId) then
  begin
    _Config.Free;
    Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_grant', 'The refresh token was issued to another client', ResponseHeaders, ResponseData, ResponseCode);
    Exit;
  end;

  // The user must still be valid, and the role is read again: a change of role applies now.
  if Assigned(FOnRefreshUser) then
  begin
    FreeAndNil(_Config);
    try
      if not FOnRefreshUser(_UserId, _Config) then
        FreeAndNil(_Config);
    except
      on E: Exception do
      begin
        LBLogger.Write(1, cMe, lmt_Error, 'OnRefreshUser: %s', [E.Message]);
        FreeAndNil(_Config);
      end;
    end;
    if _Config = nil then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, 'Refresh refused: user id=%d is no longer valid', [_UserId]);
      Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'invalid_grant', 'The user is no longer allowed', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;
  end;

  if IssueTokens(_UserId, _Config, _ClientId, _Response) then
  begin
    try
      Self.ReplyJSON(HTTP_STATUS_OK, _Response, ResponseHeaders, ResponseData, ResponseCode);
    finally
      _Response.Free;
    end;
  end
  else
    Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'Could not create the tokens', ResponseHeaders, ResponseData, ResponseCode);
end;

procedure TLBOAuthProvider.HandleToken(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Form  : TStringList;
  _Grant : String;
begin
  _Form := TStringList.Create;
  try
    ParseForm(BodyAsString(HTTPParser), _Form);
    _Grant := _Form.Values['grant_type'];

    if _Grant = 'authorization_code' then
      Self.HandleGrantAuthorizationCode(_Form, ResponseHeaders, ResponseData, ResponseCode)
    else if _Grant = 'refresh_token' then
      Self.HandleGrantRefreshToken(_Form, ResponseHeaders, ResponseData, ResponseCode)
    else
      Self.ReplyOAuthError(HTTP_STATUS_BAD_REQUEST, 'unsupported_grant_type', '', ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Form.Free;
  end;
end;

{ ---- dispatch ------------------------------------------------------------ }

function TLBOAuthProvider.DoProcessRequest(
  RequestManager  : THTTPRequestManager;
  HTTPParser      : THTTPRequestParser;
  ResponseHeaders : TStringList;
  var ResponseData: TMemoryStream;
  out ResponseCode: Integer): Boolean;
var
  _Path   : String;
  _IsGet  : Boolean;
  _IsPost : Boolean;
begin
  Result := False;
  ResponseCode := HTTP_STATUS_NOT_FOUND;

  HTTPParser.SplitURIIntoResourceAndParameters;
  _Path := HTTPParser.Resource;
  _IsGet := SameText(HTTPParser.Method, HTTP_METHOD_GET);
  _IsPost := SameText(HTTPParser.Method, HTTP_METHOD_POST);

  try
    if (FPublicBaseURL = '') and (not FAllowHostFallback) and
       (AnsiStartsStr(cWellKnownResource, _Path) or AnsiStartsStr(cWellKnownServer, _Path) or
        AnsiStartsStr(FOAuthPath + '/', _Path)) then
    begin
      // Never build the issuer from a header chosen by the client.
      Result := True;
      LBLogger.Write(1, cMe + '.DoProcessRequest', lmt_Error, 'PublicBaseURL not set: OAuth request refused');
      Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', 'The server is not configured (PublicBaseURL)', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    if AnsiStartsStr(cWellKnownResource, _Path) then
    begin
      Result := True;
      if _IsGet then
        Self.HandleProtectedResourceMetadata(HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else
        Self.ReplyOAuthError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'invalid_request', '', ResponseHeaders, ResponseData, ResponseCode);
    end
    else if AnsiStartsStr(cWellKnownServer, _Path) then
    begin
      Result := True;
      if _IsGet then
        Self.HandleServerMetadata(HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else
        Self.ReplyOAuthError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'invalid_request', '', ResponseHeaders, ResponseData, ResponseCode);
    end
    else if _Path = FOAuthPath + '/register' then
    begin
      Result := True;
      if _IsPost then
        Self.HandleRegister(HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else
        Self.ReplyOAuthError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'invalid_request', '', ResponseHeaders, ResponseData, ResponseCode);
    end
    else if _Path = FOAuthPath + '/authorize' then
    begin
      Result := True;
      if _IsGet then
        Self.HandleAuthorizeGet(HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else if _IsPost then
        Self.HandleAuthorizePost(RequestManager, HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else
        Self.ReplyOAuthError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'invalid_request', '', ResponseHeaders, ResponseData, ResponseCode);
    end
    else if _Path = FOAuthPath + '/token' then
    begin
      Result := True;
      if _IsPost then
        Self.HandleToken(HTTPParser, ResponseHeaders, ResponseData, ResponseCode)
      else
        Self.ReplyOAuthError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'invalid_request', '', ResponseHeaders, ResponseData, ResponseCode);
    end;
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe + '.DoProcessRequest', lmt_Error, E.Message);
      Result := True;
      Self.ReplyOAuthError(HTTP_STATUS_INTERNAL_ERROR, 'server_error', '', ResponseHeaders, ResponseData, ResponseCode);
    end;
  end;
end;

end.
