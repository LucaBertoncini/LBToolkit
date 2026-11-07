unit uLBAIConnector;
{
  TLBAIConnector: exposes the application's endpoints (declared once in Routes.xml, as for the web) to
  an AI client (Claude.ai, Claude Code, any MCP client) as "tools" of an MCP server.

  WHAT IT IS
  ----------------------------------------------------------------------
  A TRequestChainProcessor that speaks MCP (Model Context Protocol, JSON-RPC 2.0 over HTTP,
  "Streamable HTTP" transport in its simplest form: every POST is answered with one JSON body,
  no sessions, no SSE stream). It answers on ONE path (EndpointPath, default "/mcp"); every other
  request is passed on to the next processor of the chain.

  It does not duplicate any logic of the routing module. The ONE endpoint with Kind="AIConnector" is
  the entry point (its URI is the MCP path; an optional OpType restricts the roles that may use the
  AI at all). The tools are the authenticated Standard endpoints of the same registry, called exactly
  like the routing module calls a Standard worker. The class that implements a function may describe
  its parameters (IAIConnectorFunctions); see ExposeUndescribed.

  WHAT AN AI CAN DO IS DECIDED IN THREE PLACES
  ----------------------------------------------------------------------
  1. Routes.xml and the handler classes: only authenticated Standard endpoints are candidates; with
     ExposeUndescribed=False only the functions a class describes are tools.
  2. The role of the user who logged in: tools/list shows only the tools whose Area.OpType the
     user's role grants (TWebRouteModule.UserHasPermission, i.e. the application's own hasPermission
     override), and tools/call checks again.
  3. How long an access token lives: the role is read when a token is issued and renewed (not at
     every request), so a change of role or a deactivated user takes effect at the next renewal,
     i.e. within AccessTokenLifetime of the OAuth provider (15 minutes by default).

  AUTHENTICATION
  ----------------------------------------------------------------------
  Every request needs "Authorization: Bearer <token>". The check is delegated to
  OnAuthenticateRequest (TAIAuthenticateEvent): normally
      Connector.OnAuthenticateRequest := @TokenManager.isValidBearerRequest;
  The returned authentication context keeps the token's user configuration valid until the request
  ends; the configuration itself is not copied.
  A missing or invalid token is answered with 401 and a WWW-Authenticate header that points to the
  OAuth "protected resource metadata" (TLBOAuthProvider), which is what makes Claude.ai start the
  sign-in. Tokens are obtained through TLBOAuthProvider (each user signs in with his own account and
  the AI works with his rights) or created by hand with TTokenManager.addNewScopedToken.

  THREADS
  ----------------------------------------------------------------------
  Called concurrently, one thread per connection. The catalogue is built ONCE (Prepare, single-shot,
  under a lock; call it at start-up, before accepting connections) and is read-only afterwards. The
  functions called must be thread-safe, as for any worker (see "Funzionamento RouteHandler.md").
  The arguments object handed to a worker is freed when the call ends: copy it to keep it.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, SyncObjs,
  uLBmicroWebServer, uHTTPRequestParser, uWebRouteRegistry,
  uLBAIConnectorTypes, uLBOAuthProvider;

type
  TAIToolEntry = class(TObject)
  public
    ToolName     : String;
    Title        : String;
    Description  : String;
    AreaCode     : String;
    FunctionName : String;
    OpType       : String;
    Method       : TMethod;
    Descriptor   : TAIFunctionDescriptor;   // owned

    destructor Destroy; override;
  end;

  TAIToolList = specialize TFPGObjectList<TAIToolEntry>;

  {:Raised after every tools/call (any outcome): audit, statistics.
    aSucceeded = False for refusals, validation errors and worker errors.}
  TAIToolCalledEvent = procedure(Sender: TObject; aUserId: Integer; const aToolName: String;
    aStatusCode: Integer; aSucceeded: Boolean; anElapsedMs: Int64) of object;

  TAIRateEntry = record
    Minute : Int64;
    Count  : Integer;
  end;
  TAIRateMap = specialize TFPGMap<Integer, TAIRateEntry>;

  { TLBAIConnector }

  TLBAIConnector = class(TRequestChainProcessor)
  strict private
    FRouteModule           : TWebRouteModule;
    FEndpointPath          : String;
    FServerName            : String;
    FServerVersion         : String;
    FInstructions          : String;
    FOnAuthenticateRequest : TAIAuthenticateEvent;
    FOnToolCalled          : TAIToolCalledEvent;
    FOAuthProvider         : TLBOAuthProvider;
    FResourceMetadataURL   : String;
    FMaxResultBytes        : Integer;
    FMaxCallsPerMinute     : Integer;
    FAllowedOrigins        : TStringList;

    FTools                 : TAIToolList;
    FToolsCS               : TCriticalSection;
    FPrepared              : Boolean;
    FExposeUndescribed     : Boolean;
    FRequireOpType         : Boolean;
    FMaxBatchSize          : Integer;
    FHasGate               : Boolean;
    FGateArea              : String;
    FGateOpType            : String;

    FRateCS                : TCriticalSection;
    FRate                  : TAIRateMap;

    procedure SetOAuthProvider(aValue: TLBOAuthProvider);
    procedure SetEndpointPath(const aValue: String);

    procedure EnsurePrepared();
    function FindTool(const aName: String): TAIToolEntry;
    function BuildToolName(const anAreaCode, aFunctionName: String): String;
    function RateLimited(aUserId: Integer): Boolean;
    function OriginAllowed(aHeaders: TStringList): Boolean;
    function BuildResourceMetadataURL(aHeaders: TStringList): String;

    procedure WriteText(const aText: String; aStatus: Integer; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure WriteEmpty(aStatus: Integer; var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure WriteHTTPError(aStatus: Integer; const aMessage: String; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);
    procedure WriteUnauthorized(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer);

    function RPCError(anId: TJSONData; aCode: Integer; const aMessage: String): TJSONObject;
    function RPCResult(anId: TJSONData; aResult: TJSONData): TJSONObject;
    function ToolResult(const aText: String; isError: Boolean): TJSONObject;

    function HandleMessage(aMessage: TJSONData; aUserId: Integer; aUserConfig: TJSONObject): TJSONObject;
    function DoInitialize(aParams: TJSONObject): TJSONObject;
    function DoToolsList(aUserConfig: TJSONObject): TJSONObject;
    function DoToolsCall(aParams: TJSONObject; aUserId: Integer; aUserConfig: TJSONObject): TJSONObject;

    procedure HandlePost(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; aUserId: Integer; aUserConfig: TJSONObject; out ResponseCode: Integer);

  strict protected
    function DoProcessRequest(
      RequestManager  : THTTPRequestManager;
      HTTPParser      : THTTPRequestParser;
      ResponseHeaders : TStringList;
      var ResponseData: TMemoryStream;
      out ResponseCode: Integer): Boolean; override;

  public
    {:aRouteModule: the routing module of the application (borrowed). Its registry must have been
      loaded and every handler registered before Prepare / the first request.}
    constructor Create(aRouteModule: TWebRouteModule);
    destructor Destroy; override;

    {:Builds the catalogue of tools from Routes.xml and from the handlers. Call it once, after
      RegisterHandler and ValidateRoutes: problems (function not described, name clash, ...) are logged
      here, at startup. If not called, the first request does it. Returns the number of tools exposed.}
    function Prepare(): Integer;

    {:Number of tools in the catalogue (after Prepare).}
    function ToolCount(): Integer;

    {:The path that answers MCP requests. Default "/mcp". This is the address to give to the AI client
      (PublicBaseURL + EndpointPath).}
    property EndpointPath: String read FEndpointPath write SetEndpointPath;

    property ServerName: String read FServerName write FServerName;
    property ServerVersion: String read FServerVersion write FServerVersion;
    {:Optional text sent to the AI at initialization: what this server is for, how to use the tools.}
    property Instructions: String read FInstructions write FInstructions;

    {:False (default): only the functions a class describes (IAIConnectorFunctions) become tools.
      True: every authenticated Standard endpoint becomes a tool and the user's role decides what the
      AI may run, exactly as on the web. Undescribed functions accept a free-form JSON object, so give
      them a good Description in Routes.xml. Set before Prepare.}
    property ExposeUndescribed: Boolean read FExposeUndescribed write FExposeUndescribed;
    {:True: an endpoint without OpType is never a tool (on the web it is open to every authenticated
      user; for an AI channel you may prefer to leave it out). Default False. Set before Prepare.}
    property RequireOpTypeForTools: Boolean read FRequireOpType write FRequireOpType;
    {:Largest accepted JSON-RPC batch (default 20).}
    property MaxBatchSize: Integer read FMaxBatchSize write FMaxBatchSize;
    property OnAuthenticateRequest: TAIAuthenticateEvent read FOnAuthenticateRequest write FOnAuthenticateRequest;
    property OnToolCalled: TAIToolCalledEvent read FOnToolCalled write FOnToolCalled;

    {:Optional. When assigned, the 401 answer points to its "protected resource metadata", which lets
      Claude.ai (and other OAuth-capable clients) start the sign-in. Assigning it also aligns the
      provider's resource path with EndpointPath.}
    property OAuthProvider: TLBOAuthProvider read FOAuthProvider write SetOAuthProvider;
    {:Used in the 401 answer when there is no OAuthProvider (e.g. the metadata is served elsewhere).}
    property ResourceMetadataURL: String read FResourceMetadataURL write FResourceMetadataURL;

    {:Largest tool result accepted, in bytes. Bigger results are refused with a message that asks the AI
      to narrow the request (a truncated JSON would be useless). Default 200000.}
    property MaxResultBytes: Integer read FMaxResultBytes write FMaxResultBytes;
    {:Calls per user per minute (0 = no limit). Default 0.}
    property MaxCallsPerMinute: Integer read FMaxCallsPerMinute write FMaxCallsPerMinute;
    {:If not empty and the request carries an Origin header, the origin must be in the list (protection
      against DNS rebinding, MCP specification). Requests from an AI service have no Origin: unaffected.}
    property AllowedOrigins: TStringList read FAllowedOrigins;
  end;

implementation

uses
  ULBLogger, uHTTPConsts, jsonparser, jsonscanner, DateUtils;

const
  cMe = 'TLBAIConnector';

  cDefaultEndpointPath  = String('/mcp');
  cDefaultMaxResult     = Integer(200000);

  // JSON-RPC 2.0 error codes
  cRPC_ParseError       = Integer(-32700);
  cRPC_InvalidRequest   = Integer(-32600);
  cRPC_MethodNotFound   = Integer(-32601);
  cRPC_InvalidParams    = Integer(-32602);
  cRPC_InternalError    = Integer(-32603);

  // newest first: the first one is proposed when the client asks for a version we do not know
  cProtocolVersions: array[0..3] of String = ('2025-11-25', '2025-06-18', '2025-03-26', '2024-11-05');

  cDefaultInstructions  = String('');

{ TAIToolEntry }

destructor TAIToolEntry.Destroy;
begin
  FreeAndNil(Descriptor);
  inherited Destroy;
end;

{ TLBAIConnector }

constructor TLBAIConnector.Create(aRouteModule: TWebRouteModule);
begin
  inherited Create;
  FRouteModule := aRouteModule;
  FEndpointPath := cDefaultEndpointPath;
  FServerName := 'LBmicroWebServer';
  FServerVersion := '1.0';
  FInstructions := cDefaultInstructions;
  FMaxResultBytes := cDefaultMaxResult;
  FMaxCallsPerMinute := 0;
  FOAuthProvider := nil;
  FResourceMetadataURL := '';
  FAllowedOrigins := TStringList.Create;
  FAllowedOrigins.CaseSensitive := False;

  FTools := TAIToolList.Create(True);
  FToolsCS := TCriticalSection.Create;
  FPrepared := False;
  FExposeUndescribed := False;
  FRequireOpType := False;
  FMaxBatchSize := 20;

  FRateCS := TCriticalSection.Create;
  FRate := TAIRateMap.Create;
  FRate.Sorted := True;
end;

destructor TLBAIConnector.Destroy;
begin
  FreeAndNil(FRate);
  FreeAndNil(FRateCS);
  FreeAndNil(FTools);
  FreeAndNil(FToolsCS);
  FreeAndNil(FAllowedOrigins);
  inherited Destroy;
end;

procedure TLBAIConnector.SetEndpointPath(const aValue: String);
begin
  FEndpointPath := Trim(aValue);
  if FEndpointPath = '' then
    FEndpointPath := cDefaultEndpointPath;
  if FEndpointPath[1] <> '/' then
    FEndpointPath := '/' + FEndpointPath;
  if FOAuthProvider <> nil then
    FOAuthProvider.ResourcePath := FEndpointPath;
end;

procedure TLBAIConnector.SetOAuthProvider(aValue: TLBOAuthProvider);
begin
  FOAuthProvider := aValue;
  if FOAuthProvider <> nil then
    FOAuthProvider.ResourcePath := FEndpointPath;
end;

{ ---- catalogue ---------------------------------------------------------- }

function TLBAIConnector.BuildToolName(const anAreaCode, aFunctionName: String): String;
var
  i: Integer;
begin
  // MCP tool names: letters, digits, "_" and "-", at most 64 characters.
  Result := anAreaCode + '_' + aFunctionName;
  for i := 1 to Length(Result) do
    if not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) then
      Result[i] := '_';
  if Length(Result) > 64 then
    SetLength(Result, 64);
end;

function TLBAIConnector.Prepare(): Integer;
var
  _Registry   : TRouteRegistry;
  _Gate       : TEndpointDescriptor;
  _Entry      : TAIEndpointEntry;
  _Tool       : TAIToolEntry;
  _Describer  : IAIConnectorFunctions;
  _Descriptor : TAIFunctionDescriptor;
  _Skipped    : Integer;
  _FreeForm   : Integer;
  _Described  : Boolean;
  i, j        : Integer;
  _Duplicate  : Boolean;
begin
  FToolsCS.Enter;
  try
    // Single-shot: the catalogue is read without locks by the request threads, so it is never rebuilt.
    if FPrepared then
      Exit(FTools.Count);

    _Skipped := 0;
    _FreeForm := 0;

    if (FRouteModule = nil) or (FRouteModule.Routes = nil) then
    begin
      LBLogger.Write(1, cMe + '.Prepare', lmt_Error, 'Routing module not set: no AI tool available');
      FPrepared := True;
      Exit(0);
    end;

    _Registry := FRouteModule.Routes;

    // The entry point is declared in Routes.xml (Kind="AIConnector"): its URI is the MCP path and its
    // Area/OpType say which roles may use the AI channel at all.
    _Registry.ValidateAIConnector();   // logs: more than one entry point, bad path, clash with a route
    // The OAuth provider is NOT validated here any more: TLBOAuthSetup.Attach does it (once). Whoever builds the
    // provider by hand calls OAuthProvider.ValidateConfiguration() itself.
    _Gate := _Registry.AIConnectorEndpoint;
    if _Gate <> nil then
    begin
      Self.SetEndpointPath(_Gate.URI);
      FGateArea := _Gate.AreaCode;
      FGateOpType := _Gate.OpType;
      FHasGate := True;
    end
    else begin
      FHasGate := False;
      LBLogger.Write(1, cMe + '.Prepare', lmt_Warning,
        'No endpoint with Kind="AIConnector" in Routes.xml: path <%s> with no role gate (any authenticated user)', [FEndpointPath]);
    end;

    for i := 0 to _Registry.AIEndpointCount - 1 do
    begin
      _Entry := _Registry.GetAIEndpoint(i);

      if not _Entry.KindMatches then
      begin
        Inc(_Skipped);
        Continue;
      end;

      // Not every endpoint is meant for the AI: only the functions a class chose to describe.

      // A class may describe the parameters of a function precisely (IAIConnectorFunctions). With
      // ExposeUndescribed every authenticated Standard endpoint is a tool anyway (the role decides who
      // may use it); without it only the described functions are.
      _Descriptor := TAIFunctionDescriptor.Create(_Entry.Descriptor.FunctionName);
      _Described := Supports(_Entry.Handler, IAIConnectorFunctions, _Describer) and
        _Describer.DescribeAIFunction(_Entry.Descriptor.FunctionName, _Descriptor);
      if _Descriptor.Hidden or ((not _Described) and (not FExposeUndescribed)) then
      begin
        Inc(_Skipped);
        _Descriptor.Free;
        Continue;
      end;
      if not _Described then
      begin
        _Descriptor.FreeForm := True;
        Inc(_FreeForm);
      end;

      _Tool := TAIToolEntry.Create;
      _Tool.ToolName := Self.BuildToolName(_Entry.Descriptor.AreaCode, _Entry.Descriptor.FunctionName);
      _Tool.AreaCode := _Entry.Descriptor.AreaCode;
      _Tool.FunctionName := _Entry.Descriptor.FunctionName;
      _Tool.OpType := _Entry.Descriptor.OpType;
      _Tool.Method := _Entry.TheMethod;
      _Tool.Descriptor := _Descriptor;
      _Tool.Title := _Descriptor.Title;

      _Tool.Description := _Entry.Descriptor.Description;
      if _Tool.Description = '' then
        _Tool.Description := _Descriptor.Description;
      if _Tool.Description = '' then
      begin
        _Tool.Description := Trim(_Entry.Descriptor.AreaDescription + ' - ' + _Entry.Descriptor.FunctionName);
        LBLogger.Write(1, cMe + '.Prepare', lmt_Warning,
          '%s.%s has no Description in Routes.xml: the AI will see only "%s". A good description is what lets the AI choose the right tool',
          [_Entry.Descriptor.AreaCode, _Entry.Descriptor.FunctionName, _Tool.Description]);
      end;

      if _Tool.OpType = '' then
      begin
        if FRequireOpType then
        begin
          LBLogger.Write(1, cMe + '.Prepare', lmt_Warning,
            '%s.%s has no OpType: NOT exposed (RequireOpTypeForTools)', [_Entry.Descriptor.AreaCode, _Entry.Descriptor.FunctionName]);
          Inc(_Skipped);
          _Tool.Free;
          Continue;
        end;
        LBLogger.Write(3, cMe + '.Prepare', lmt_Warning,
          '%s.%s has no OpType: any authenticated user can use it through the AI',
          [_Entry.Descriptor.AreaCode, _Entry.Descriptor.FunctionName]);
      end;

      _Duplicate := False;
      for j := 0 to FTools.Count - 1 do
        if SameText(FTools[j].ToolName, _Tool.ToolName) then
        begin
          _Duplicate := True;
          Break;
        end;
      if _Duplicate then
      begin
        Inc(_Skipped);
        LBLogger.Write(1, cMe + '.Prepare', lmt_Error, 'Tool name <%s> already used: %s.%s NOT exposed (rename the function)',
          [_Tool.ToolName, _Tool.AreaCode, _Tool.FunctionName]);
        _Tool.Free;
        Continue;
      end;

      FTools.Add(_Tool);
    end;

    FPrepared := True;
    Result := FTools.Count;
    LBLogger.Write(3, cMe + '.Prepare', lmt_Info, '%d AI tool(s) exposed on <%s> (%d with free-form parameters), %d not exposed',
      [FTools.Count, FEndpointPath, _FreeForm, _Skipped]);
  finally
    FToolsCS.Leave;
  end;
end;

procedure TLBAIConnector.EnsurePrepared();
begin
  // Normally Prepare has already run at start-up. If not, Prepare is atomic: one thread builds the
  // catalogue, the others wait on the lock and then find it ready.
  if not FPrepared then
    Self.Prepare();
end;

function TLBAIConnector.ToolCount(): Integer;
begin
  Self.EnsurePrepared();
  Result := FTools.Count;
end;

function TLBAIConnector.FindTool(const aName: String): TAIToolEntry;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTools.Count - 1 do
    if SameStr(FTools[i].ToolName, aName) then
      Exit(FTools[i]);
end;

{ ---- helpers ------------------------------------------------------------ }

function TLBAIConnector.RateLimited(aUserId: Integer): Boolean;
var
  _Minute : Int64;
  _Idx    : Integer;
  _Item   : TAIRateEntry;
begin
  Result := False;
  if FMaxCallsPerMinute <= 0 then Exit;

  _Minute := DateTimeToUnix(Now, False) div 60;
  FRateCS.Enter;
  try
    if FRate.Count > 10000 then
      FRate.Clear;   // crude bound on the memory used; counters restart

    if FRate.Find(aUserId, _Idx) then
    begin
      _Item := FRate.Data[_Idx];
      if _Item.Minute <> _Minute then
      begin
        _Item.Minute := _Minute;
        _Item.Count := 0;
      end;
      Inc(_Item.Count);
      FRate.Data[_Idx] := _Item;
    end
    else begin
      _Item.Minute := _Minute;
      _Item.Count := 1;
      FRate.Add(aUserId, _Item);
    end;
    Result := _Item.Count > FMaxCallsPerMinute;
  finally
    FRateCS.Leave;
  end;
end;

function TLBAIConnector.OriginAllowed(aHeaders: TStringList): Boolean;
var
  _Origin: String;
  i: Integer;
begin
  // No Origin header: a server-to-server client (Claude.ai's backend, Claude Code): allowed.
  // With an Origin header (a browser): it must be listed. With no list, only the application's own
  // public origin is accepted; everything else (including "null") is refused.
  Result := True;
  _Origin := Trim(aHeaders.Values[HTTP_HEADER_ORIGIN]);
  if _Origin = '' then Exit;
  while (_Origin <> '') and (_Origin[Length(_Origin)] = '/') do
    SetLength(_Origin, Length(_Origin) - 1);

  for i := 0 to FAllowedOrigins.Count - 1 do
    if SameText(_Origin, FAllowedOrigins[i]) then Exit(True);

  if (FAllowedOrigins.Count = 0) and (FOAuthProvider <> nil) and
     SameText(_Origin, FOAuthProvider.PublicOrigin) and (FOAuthProvider.PublicOrigin <> '') then
    Exit(True);

  Result := False;
end;

function TLBAIConnector.BuildResourceMetadataURL(aHeaders: TStringList): String;
begin
  if FOAuthProvider <> nil then
    Result := FOAuthProvider.GetResourceMetadataURL(aHeaders)
  else
    Result := FResourceMetadataURL;
end;

procedure TLBAIConnector.WriteText(const aText: String; aStatus: Integer; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Text: String;
begin
  _Text := aText;
  if ResponseData = nil then
    ResponseData := TMemoryStream.Create
  else
    ResponseData.Clear;

  if ResponseHeaders.IndexOfName(HTTP_HEADER_CONTENT_TYPE) = -1 then
    ResponseHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + MIME_TYPE_JSON);
  ResponseHeaders.Add(HTTP_HEADER_CACHE_CONTROL + ': no-store');

  if Length(_Text) > 0 then
    ResponseData.WriteBuffer(_Text[1], Length(_Text));
  ResponseData.Position := 0;
  ResponseCode := aStatus;
end;

procedure TLBAIConnector.WriteEmpty(aStatus: Integer; var ResponseData: TMemoryStream; out ResponseCode: Integer);
begin
  // An existing, empty stream makes the transport send "Content-Length: 0": without it a client
  // could wait for a body that never comes.
  if ResponseData = nil then
    ResponseData := TMemoryStream.Create
  else
    ResponseData.Clear;
  ResponseCode := aStatus;
end;

procedure TLBAIConnector.WriteHTTPError(aStatus: Integer; const aMessage: String; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Obj: TJSONObject;
begin
  _Obj := TJSONObject.Create;
  try
    _Obj.Add('error', aMessage);
    _Obj.CompressedJSON := True;
    Self.WriteText(_Obj.AsJSON, aStatus, ResponseHeaders, ResponseData, ResponseCode);
  finally
    _Obj.Free;
  end;
end;

procedure TLBAIConnector.WriteUnauthorized(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer);
var
  _Challenge : String;
  _Meta      : String;
begin
  _Challenge := 'Bearer';
  _Meta := Self.BuildResourceMetadataURL(HTTPParser.Headers);
  if _Meta <> '' then
    _Challenge := _Challenge + ' resource_metadata="' + _Meta + '"';
  if Trim(HTTPParser.Headers.Values[HTTP_HEADER_AUTHORIZATION]) <> '' then
    _Challenge := _Challenge + ', error="invalid_token"';
  ResponseHeaders.Add(HTTP_HEADER_WWW_AUTHENTICATE + ': ' + _Challenge);
  Self.WriteHTTPError(HTTP_STATUS_UNAUTHORIZED, 'Authentication required', ResponseHeaders, ResponseData, ResponseCode);
end;

function TLBAIConnector.RPCError(anId: TJSONData; aCode: Integer; const aMessage: String): TJSONObject;
var
  _Err: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('jsonrpc', '2.0');
  if anId <> nil then
    Result.Add('id', anId.Clone)
  else
    Result.Add('id', TJSONNull.Create);
  _Err := TJSONObject.Create;
  _Err.Add('code', aCode);
  _Err.Add('message', aMessage);
  Result.Add('error', _Err);
end;

function TLBAIConnector.RPCResult(anId: TJSONData; aResult: TJSONData): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('jsonrpc', '2.0');
  if anId <> nil then
    Result.Add('id', anId.Clone)
  else
    Result.Add('id', TJSONNull.Create);
  Result.Add('result', aResult);
end;

function TLBAIConnector.ToolResult(const aText: String; isError: Boolean): TJSONObject;
var
  _Content : TJSONArray;
  _Item    : TJSONObject;
begin
  Result := TJSONObject.Create;
  _Content := TJSONArray.Create;
  _Item := TJSONObject.Create;
  _Item.Add('type', 'text');
  _Item.Add('text', aText);
  _Content.Add(_Item);
  Result.Add('content', _Content);
  Result.Add('isError', isError);
end;

{ ---- protocol ----------------------------------------------------------- }

function TLBAIConnector.DoInitialize(aParams: TJSONObject): TJSONObject;
var
  _Requested : String;
  _Chosen    : String;
  i          : Integer;
  _Caps      : TJSONObject;
  _Tools     : TJSONObject;
  _Info      : TJSONObject;
begin
  _Requested := '';
  if aParams <> nil then
    _Requested := aParams.Get('protocolVersion', '');

  // If we know the version the client asks for we answer with the same, otherwise with our newest.
  _Chosen := cProtocolVersions[0];
  for i := Low(cProtocolVersions) to High(cProtocolVersions) do
    if cProtocolVersions[i] = _Requested then
    begin
      _Chosen := _Requested;
      Break;
    end;

  Result := TJSONObject.Create;
  Result.Add('protocolVersion', _Chosen);

  _Caps := TJSONObject.Create;
  _Tools := TJSONObject.Create;
  _Tools.Add('listChanged', False);
  _Caps.Add('tools', _Tools);
  Result.Add('capabilities', _Caps);

  _Info := TJSONObject.Create;
  _Info.Add('name', FServerName);
  _Info.Add('version', FServerVersion);
  Result.Add('serverInfo', _Info);

  if FInstructions <> '' then
    Result.Add('instructions', FInstructions);
end;

function TLBAIConnector.DoToolsList(aUserConfig: TJSONObject): TJSONObject;
var
  _List  : TJSONArray;
  _Item  : TJSONObject;
  _Ann   : TJSONObject;
  _Tool  : TAIToolEntry;
  i      : Integer;
begin
  Self.EnsurePrepared();

  _List := TJSONArray.Create;
  for i := 0 to FTools.Count - 1 do
  begin
    _Tool := FTools[i];

    // The AI sees only what the user's role allows.
    if not FRouteModule.UserHasPermission(aUserConfig, _Tool.AreaCode, _Tool.OpType) then
      Continue;

    _Item := TJSONObject.Create;
    _Item.Add('name', _Tool.ToolName);
    if _Tool.Title <> '' then
      _Item.Add('title', _Tool.Title);
    _Item.Add('description', _Tool.Description);
    _Item.Add('inputSchema', _Tool.Descriptor.BuildInputSchema());

    if _Tool.Descriptor.Effect <> aeUnspecified then
    begin
      _Ann := TJSONObject.Create;
      _Ann.Add('readOnlyHint', _Tool.Descriptor.Effect = aeReadOnly);
      if _Tool.Descriptor.Effect <> aeReadOnly then
        _Ann.Add('destructiveHint', _Tool.Descriptor.Effect = aeDestructive);
      _Ann.Add('openWorldHint', False);
      _Item.Add('annotations', _Ann);
    end;

    _List.Add(_Item);
  end;

  Result := TJSONObject.Create;
  Result.Add('tools', _List);
end;

function TLBAIConnector.DoToolsCall(aParams: TJSONObject; aUserId: Integer; aUserConfig: TJSONObject): TJSONObject;
var
  _Name       : String;
  _Tool       : TAIToolEntry;
  _ArgsNode   : TJSONData;
  _Args       : TJSONObject;
  _Error      : String;
  _Status     : Integer;
  _Answer     : TJSONData;
  _Text       : String;
  _Start      : QWord;
  _Ok         : Boolean;
begin
  Self.EnsurePrepared();
  _Start := GetTickCount64;
  _Status := HTTP_STATUS_OK;
  _Ok := False;
  _Args := nil;
  _Answer := nil;

  _Name := '';
  if aParams <> nil then
    _Name := aParams.Get('name', '');

  _Tool := Self.FindTool(_Name);
  if _Tool = nil then
    Exit(nil);   // the caller answers with a protocol error (-32602)

  try
    if not FRouteModule.UserHasPermission(aUserConfig, _Tool.AreaCode, _Tool.OpType) then
    begin
      _Status := HTTP_STATUS_FORBIDDEN;
      Result := Self.ToolResult('You are not authorised to use this tool: your role does not grant ' +
        _Tool.AreaCode + '.' + _Tool.OpType + '.', True);
      Exit;
    end;

    if Self.RateLimited(aUserId) then
    begin
      _Status := HTTP_STATUS_TOO_MANY_REQUESTS;
      Result := Self.ToolResult('Too many requests: wait a moment and try again.', True);
      Exit;
    end;

    // The worker receives its own copy (validation may convert values); it is freed after the call, so a worker that wants to keep it must copy it.
    _ArgsNode := aParams.Find('arguments');
    if _ArgsNode = nil then
      _Args := TJSONObject.Create
    else if _ArgsNode.JSONType = jtObject then
      _Args := TJSONObject(_ArgsNode.Clone)
    else begin
      _Status := HTTP_STATUS_BAD_REQUEST;
      Result := Self.ToolResult('Invalid arguments: "arguments" must be a JSON object', True);
      Exit;
    end;

    if not _Tool.Descriptor.ValidateArguments(_Args, _Error) then
    begin
      _Status := HTTP_STATUS_BAD_REQUEST;
      Result := Self.ToolResult('Invalid arguments: ' + _Error, True);
      Exit;
    end;

    try
      _Answer := TAIConnectorWorker(_Tool.Method)(aUserId, aUserConfig, _Args, _Status);
    except
      on E: Exception do
      begin
        LBLogger.Write(1, cMe + '.DoToolsCall', lmt_Error, 'Tool <%s>: %s', [_Tool.ToolName, E.Message]);
        _Status := HTTP_STATUS_INTERNAL_ERROR;
        Result := Self.ToolResult('Internal error while running the tool.', True);
        Exit;
      end;
    end;

    if _Status <= 0 then
      _Status := HTTP_STATUS_OK;

    if _Answer <> nil then
    begin
      _Answer.CompressedJSON := True;
      _Text := _Answer.AsJSON;
    end
    else
      _Text := '{"successful":true}';

    if Length(_Text) > FMaxResultBytes then
    begin
      _Status := 413;   // for the audit: the answer exists but is too large for the AI
      Result := Self.ToolResult(Format('The result is too large (%d bytes, limit %d). Repeat the request with narrower filters or fewer rows.',
        [Length(_Text), FMaxResultBytes]), True);
      Exit;
    end;

    _Ok := _Status < 400;
    Result := Self.ToolResult(_Text, not _Ok);
  finally
    if _Answer <> nil then _Answer.Free;
    if _Args <> nil then _Args.Free;

    LBLogger.Write(3, cMe + '.DoToolsCall', lmt_Info, 'user=%d tool=%s status=%d ms=%d',
      [aUserId, _Tool.ToolName, _Status, Int64(GetTickCount64 - _Start)]);
    if Assigned(FOnToolCalled) then
      try
        FOnToolCalled(Self, aUserId, _Tool.ToolName, _Status, _Ok, Int64(GetTickCount64 - _Start));
      except
        on E: Exception do
          LBLogger.Write(1, cMe + '.DoToolsCall', lmt_Error, 'OnToolCalled: %s', [E.Message]);
      end;
  end;
end;

function TLBAIConnector.HandleMessage(aMessage: TJSONData; aUserId: Integer; aUserConfig: TJSONObject): TJSONObject;
var
  _Msg      : TJSONObject;
  _MethodN  : TJSONData;
  _Method   : String;
  _Id       : TJSONData;
  _ParamsN  : TJSONData;
  _Params   : TJSONObject;
  _CallRes  : TJSONObject;
begin
  Result := nil;

  if (aMessage = nil) or (aMessage.JSONType <> jtObject) then
    Exit(Self.RPCError(nil, cRPC_InvalidRequest, 'Invalid request: a JSON object was expected'));

  _Msg := TJSONObject(aMessage);
  _Id := _Msg.Find('id');
  _MethodN := _Msg.Find('method');

  if (_Id <> nil) and (not (_Id.JSONType in [jtString, jtNumber, jtNull])) then
    Exit(Self.RPCError(nil, cRPC_InvalidRequest, 'Invalid request: "id" must be a string, a number or null'));
  if _Msg.Get('jsonrpc', '') <> '2.0' then
    Exit(Self.RPCError(_Id, cRPC_InvalidRequest, 'Invalid request: "jsonrpc" must be "2.0"'));

  if (_MethodN = nil) or (_MethodN.JSONType <> jtString) then
  begin
    // A response sent by the client to a request of ours: we never send requests, so ignore it.
    if (_Msg.Find('result') <> nil) or (_Msg.Find('error') <> nil) then
      Exit(nil);
    Exit(Self.RPCError(_Id, cRPC_InvalidRequest, 'Invalid request: "method" is missing'));
  end;

  _Method := _MethodN.AsString;

  // No "id" = notification: never answered (notifications/initialized, notifications/cancelled, ...).
  if _Id = nil then
    Exit(nil);

  _ParamsN := _Msg.Find('params');
  if (_ParamsN <> nil) and (_ParamsN.JSONType = jtObject) then
    _Params := TJSONObject(_ParamsN)
  else
    _Params := nil;

  if _Method = 'initialize' then
    Result := Self.RPCResult(_Id, Self.DoInitialize(_Params))
  else if _Method = 'ping' then
    Result := Self.RPCResult(_Id, TJSONObject.Create)
  else if _Method = 'tools/list' then
    Result := Self.RPCResult(_Id, Self.DoToolsList(aUserConfig))
  else if _Method = 'tools/call' then
  begin
    if _Params = nil then
      Result := Self.RPCError(_Id, cRPC_InvalidParams, 'Invalid params: "name" is missing')
    else begin
      _CallRes := Self.DoToolsCall(_Params, aUserId, aUserConfig);
      if _CallRes = nil then
        Result := Self.RPCError(_Id, cRPC_InvalidParams, 'Unknown tool: ' + _Params.Get('name', ''))
      else
        Result := Self.RPCResult(_Id, _CallRes);
    end;
  end
  else
    Result := Self.RPCError(_Id, cRPC_MethodNotFound, 'Method not found: ' + _Method);
end;

procedure TLBAIConnector.HandlePost(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; aUserId: Integer; aUserConfig: TJSONObject; out ResponseCode: Integer);
var
  _Body     : String;
  _Data     : TJSONData;
  _Parser   : TJSONParser;
  _Batch    : TJSONArray;
  _Answers  : TJSONArray;
  _One      : TJSONObject;
  i         : Integer;
begin
  _Body := '';
  if (HTTPParser.Body <> nil) and (HTTPParser.Body.Size > 0) then
  begin
    HTTPParser.Body.Position := 0;
    SetLength(_Body, HTTPParser.Body.Size);
    HTTPParser.Body.ReadBuffer(_Body[1], HTTPParser.Body.Size);
  end;

  _Data := nil;
  try
    _Parser := TJSONParser.Create(_Body, [joUTF8]);
    try
      _Data := _Parser.Parse;
    finally
      _Parser.Free;
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(_Data);
      _One := Self.RPCError(nil, cRPC_ParseError, 'Parse error');
      try
        _One.CompressedJSON := True;
        Self.WriteText(_One.AsJSON, HTTP_STATUS_BAD_REQUEST, ResponseHeaders, ResponseData, ResponseCode);
      finally
        _One.Free;
      end;
      Exit;
    end;
  end;

  try
    if _Data is TJSONArray then
    begin
      // Batch (allowed by older protocol versions): answers only for the requests.
      _Batch := TJSONArray(_Data);
      if (_Batch.Count = 0) or ((FMaxBatchSize > 0) and (_Batch.Count > FMaxBatchSize)) then
      begin
        _One := Self.RPCError(nil, cRPC_InvalidRequest,
          Format('Invalid request: a batch must contain from 1 to %d messages', [FMaxBatchSize]));
        try
          _One.CompressedJSON := True;
          Self.WriteText(_One.AsJSON, HTTP_STATUS_BAD_REQUEST, ResponseHeaders, ResponseData, ResponseCode);
        finally
          _One.Free;
        end;
        Exit;
      end;
      _Answers := TJSONArray.Create;
      try
        for i := 0 to _Batch.Count - 1 do
        begin
          _One := Self.HandleMessage(_Batch.Items[i], aUserId, aUserConfig);
          if _One <> nil then
            _Answers.Add(_One);
        end;
        if _Answers.Count = 0 then
          Self.WriteEmpty(HTTP_STATUS_ACCEPTED, ResponseData, ResponseCode)
        else begin
          _Answers.CompressedJSON := True;
          Self.WriteText(_Answers.AsJSON, HTTP_STATUS_OK, ResponseHeaders, ResponseData, ResponseCode);
        end;
      finally
        _Answers.Free;
      end;
    end
    else begin
      _One := Self.HandleMessage(_Data, aUserId, aUserConfig);
      if _One = nil then
        Self.WriteEmpty(HTTP_STATUS_ACCEPTED, ResponseData, ResponseCode)
      else
        try
          _One.CompressedJSON := True;
          Self.WriteText(_One.AsJSON, HTTP_STATUS_OK, ResponseHeaders, ResponseData, ResponseCode);
        finally
          _One.Free;
        end;
    end;
  finally
    _Data.Free;
  end;
end;

function TLBAIConnector.DoProcessRequest(
  RequestManager  : THTTPRequestManager;
  HTTPParser      : THTTPRequestParser;
  ResponseHeaders : TStringList;
  var ResponseData: TMemoryStream;
  out ResponseCode: Integer): Boolean;
var
  _Authentication : TAIAuthenticationContext;
begin
  Result := False;
  ResponseCode := HTTP_STATUS_NOT_FOUND;
  _Authentication := nil;

  Self.EnsurePrepared();   // the path comes from Routes.xml
  HTTPParser.SplitURIIntoResourceAndParameters;
  if HTTPParser.Resource <> FEndpointPath then
    Exit;

  Result := True;
  try
    if not Self.OriginAllowed(HTTPParser.Headers) then
    begin
      Self.WriteHTTPError(HTTP_STATUS_FORBIDDEN, 'Origin not allowed', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    if not Assigned(FOnAuthenticateRequest) then
    begin
      LBLogger.Write(1, cMe, lmt_Error, 'OnAuthenticateRequest not set: every request is refused');
      Self.WriteHTTPError(HTTP_STATUS_INTERNAL_ERROR, 'Server not configured', ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    if not FOnAuthenticateRequest(HTTPParser.Headers, ResponseHeaders, _Authentication) or
       (_Authentication = nil) or (_Authentication.UserId <= 0) or
       (_Authentication.UserConfig = nil) then
    begin
      // Also for GET: an unauthenticated probe must be told to sign in, as the MCP authorization flow expects.
      FreeAndNil(_Authentication);
      Self.WriteUnauthorized(HTTPParser, ResponseHeaders, ResponseData, ResponseCode);
      Exit;
    end;

    // The context pins the token. UserConfig is borrowed and remains valid until the context is freed.
    try
      if FHasGate and (not FRouteModule.UserHasPermission(_Authentication.UserConfig, FGateArea, FGateOpType)) then
      begin
        Self.WriteHTTPError(HTTP_STATUS_FORBIDDEN, 'Your role does not allow the AI connector', ResponseHeaders, ResponseData, ResponseCode);
        Exit;
      end;

      if SameText(HTTPParser.Method, HTTP_METHOD_POST) then
        Self.HandlePost(HTTPParser, ResponseHeaders, ResponseData, _Authentication.UserId,
          _Authentication.UserConfig, ResponseCode)
      else begin
        // No server-to-client stream (GET) and no sessions to close (DELETE).
        ResponseHeaders.Add(HTTP_HEADER_ALLOW + ': ' + HTTP_METHOD_POST);
        Self.WriteHTTPError(HTTP_STATUS_METHOD_NOT_ALLOWED, 'Method not allowed', ResponseHeaders, ResponseData, ResponseCode);
      end;
    finally
      FreeAndNil(_Authentication);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(_Authentication);
      LBLogger.Write(1, cMe + '.DoProcessRequest', lmt_Error, E.Message);
      Self.WriteHTTPError(HTTP_STATUS_INTERNAL_ERROR, 'Internal error', ResponseHeaders, ResponseData, ResponseCode);
    end;
  end;
end;

end.
