unit Toolkit_CAPI_microWebServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, uWebSocketManagement, uHTTPRequestParser;

type
  TGETCallback = function (RequestElaborator: Pointer; aResource: pChar; aResponseCode: pInteger): Boolean; cdecl;
  TPOSTCallback = function (RequestElaborator: Pointer; aResource: pChar; aPayload: pByte; aPayloadLen: Integer; aResponseCode: pInteger): Boolean; cdecl;
  TWebSocketMessageCallback = procedure (aPayLoad: pByte; aPayloadLen: Integer); cdecl;
  TWebSocketConnectedCallback = procedure (); cdecl;

  pMemoryStream = ^TMemoryStream;

  { TmicroWebServerCallbacks }

  TmicroWebServerCallbacks = class(TRequestChainProcessor)
    strict private
      FGETCallback : TGETCallback;
      FPOSTCallback : TPOSTCallback;
      FWebSocketMessageCallback : TWebSocketMessageCallback;
      FWebSocketConnectedCallback : TWebSocketConnectedCallback;

      FResponseHeaders : TStringList;
      FResponseData : pMemoryStream;

      FHTTPParser : THTTPRequestParser;

      function get_RequestHeaderName(Index: Integer): String;
      function get_RequestHeadersCount: Integer;
      function get_RequetsHeaderValue(Index: Integer): String;
      function get_URIParam(Index: Integer): String;
      function get_URIParamsCount: Integer;

    strict protected
      function DoProcessGETRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function DoProcessPOSTRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

    public
      procedure ProcessWebSocketMessage(Sender: TObject; Opcode: TWebSocketDataType; PayloadBuffer: pByte; PayloadLen: Int64);
      procedure NotifyWebSocketConnectionEstablished(Sender: TObject);

      function addResponseHeader(const aName: String; const aValue: String): Boolean;
      function setResponseData(aBuffer: pByte; aBufferLen: Integer): Boolean;

      property RequestHeadersCount: Integer read get_RequestHeadersCount;
      property RequestHeaderName[Index: Integer]: String read get_RequestHeaderName;
      property RequestHeaderValue[Index: Integer]: String read get_RequetsHeaderValue;

      property URIParamsCount: Integer read get_URIParamsCount;
      property URIParam[Index: Integer]: String read get_URIParam;

      property GETCallback: TGETCallback write FGETCallback;
      property POSTCallback: TPOSTCallback write FPOSTCallback;
      property WebSocketMessageCallback : TWebSocketMessageCallback write FWebSocketMessageCallback;
      property WebSocketConnectedCallback : TWebSocketConnectedCallback write FWebSocketConnectedCallback;

  end;

  function WebServer_Create(aConfigFilename: PChar;
                            GETCallback: TGETCallback;
                            POSTCallback: TPOSTCallback;
                            WebSocketConnectedCallback: TWebSocketConnectedCallback;
                            WebSocketMessageCallback: TWebSocketMessageCallback): Pointer; export; cdecl;

  function WebServer_Destroy(aWebServer: Pointer): Boolean; export; cdecl;

  function ReqElab_RequestHeaderValue(RequestElaborator: Pointer; Index: Integer; aValueBuffer: pByte; aValueBufferLen: Integer): Integer; export; cdecl;
  function ReqElab_RequestHeaderName(RequestElaborator: Pointer; Index: Integer; aNameBuffer: pByte; aNameBufferLen: Integer): Integer; export; cdecl;
  function ReqElab_RequestHeadersCount(RequestElaborator: Pointer): Integer; export; cdecl;
  function ReqElab_setResponseData(RequestElaborator: Pointer; aBuffer: pByte; aBufferLen: Integer): Boolean; export; cdecl;
  function ReqElab_addResponseHeader(RequestElaborator: Pointer; aName: PChar; aValue: PChar): Boolean; export; cdecl;
  function ReqElab_URIParamsCount(RequestElaborator: Pointer): Integer; export; cdecl;
  function ReqElab_URIParam(RequestElaborator: Pointer; Index: Integer; aURIParamBuff: pByte; aURIParamLen: Integer): Integer; export; cdecl;



implementation

uses
  ULBLogger, uLBWebServerConfigurationLoader;

// Return the string length
function insertStringIntoBuffer(const aString: String; aBuffer: pByte; aBufferLen: Integer): Integer;
var
  _Len : Integer;

begin
  Result := 0;

  if aString <> '' then
  begin
    Result := Length(aString);
    if Result > aBufferLen then
      _Len := aBufferLen
    else
      _Len := Result;

    Move(aString[1], aBuffer^, _Len);
  end;

end;

function WebServer_Create(aConfigFilename: PChar;
                          GETCallback: TGETCallback;
                          POSTCallback: TPOSTCallback;
                          WebSocketConnectedCallback: TWebSocketConnectedCallback;
                          WebSocketMessageCallback: TWebSocketMessageCallback): Pointer; export; cdecl;
var
  _CallbacksChainItem : TmicroWebServerCallbacks;
  _WS : TLBmicroWebServer = nil;
  _Loader : TINIConfigLoader = nil;

begin
  Result := nil;

  try
    _Loader := TINIConfigLoader.Create;
    _Loader.Filename := StrPas(aConfigFilename);

    _WS := TLBmicroWebServer.Create();
    if _Loader.LoadConfig(_WS) then
    begin

      _CallbacksChainItem := TmicroWebServerCallbacks.Create();
      _CallbacksChainItem.GETCallback := GETCallback;
      _CallbacksChainItem.POSTCallback := POSTCallback;
      _CallbacksChainItem.WebSocketConnectedCallback := WebSocketConnectedCallback;
      _CallbacksChainItem.WebSocketMessageCallback := WebSocketMessageCallback;

      _WS.addChainProcessor(_CallbacksChainItem, True);

      _WS.OnWebSocketConnectionEstablished := @_CallbacksChainItem.NotifyWebSocketConnectionEstablished;
      _WS.OnElaborateWebSocketMessage := @_CallbacksChainItem.ProcessWebSocketMessage;

      _WS.Activate();

      Result := Pointer(_WS);
      _WS := nil;

    end
    else begin
      _WS.Free;
      LBLogger.Write(1, 'WebServer_Create', lmt_Warning, 'Error loading configuration');
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'WebServer_Create', lmt_Error, E.Message);
  end;

  if _Loader <> nil then
    _Loader.Free;

  if _WS <> nil then
    _WS.Free;
end;

function WebServer_Destroy(aWebServer: Pointer): Boolean; export; cdecl;
var
  _WS : TLBmicroWebServer;

begin
  Result := False;
  if aWebServer <> nil then
  begin
    try

      _WS := TLBmicroWebServer(aWebServer);
      FreeAndNil(_WS);
      Result := True;

    except
      on E: Exception do
        LBLogger.Write(1, 'WebServer_Destroy', lmt_Error, E.Message);
    end;
  end;
end;

function ReqElab_addResponseHeader(RequestElaborator: Pointer; aName: PChar; aValue: PChar): Boolean; export; cdecl;
begin
  Result := False;
  if (RequestElaborator <> nil) and (aName <> nil) and (aValue <> nil) then
    Result := TmicroWebServerCallbacks(RequestElaborator).addResponseHeader(StrPas(aName), StrPas(aValue));
end;

function ReqElab_setResponseData(RequestElaborator: Pointer; aBuffer: pByte; aBufferLen: Integer): Boolean; export; cdecl;
begin
  Result := False;

  if (RequestElaborator <> nil) and (aBuffer <> nil) and (aBufferLen > 0) then
    Result := TmicroWebServerCallbacks(RequestElaborator).setResponseData(aBuffer, aBufferLen);
end;

function ReqElab_RequestHeadersCount(RequestElaborator: Pointer): Integer; export; cdecl;
begin
  Result := 0;
  if RequestElaborator <> nil then
    Result := TmicroWebServerCallbacks(RequestElaborator).RequestHeadersCount;
end;

function ReqElab_RequestHeaderName(RequestElaborator: Pointer; Index: Integer; aNameBuffer: pByte; aNameBufferLen: Integer): Integer; export; cdecl;
begin
  Result := 0;
  if RequestElaborator <> nil then
    Result := insertStringIntoBuffer(TmicroWebServerCallbacks(RequestElaborator).RequestHeaderName[Index], aNameBuffer, aNameBufferLen);
end;

function ReqElab_RequestHeaderValue(RequestElaborator: Pointer; Index: Integer; aValueBuffer: pByte; aValueBufferLen: Integer): Integer; export; cdecl;
begin
  Result := 0;
  if RequestElaborator <> nil then
    Result := insertStringIntoBuffer(TmicroWebServerCallbacks(RequestElaborator).RequestHeaderValue[Index], aValueBuffer, aValueBufferLen);
end;

function ReqElab_URIParamsCount(RequestElaborator: Pointer): Integer; export; cdecl;
begin
  Result := 0;
  if RequestElaborator <> nil then
    Result := TmicroWebServerCallbacks(RequestElaborator).URIParamsCount;
end;

function ReqElab_URIParam(RequestElaborator: Pointer; Index: Integer; aURIParamBuff: pByte; aURIParamLen: Integer): Integer; export; cdecl;
begin
  Result := 0;
  if RequestElaborator <> nil then
    Result := insertStringIntoBuffer(TmicroWebServerCallbacks(RequestElaborator).URIParam[Index], aURIParamBuff, aURIParamLen);
end;

{ TmicroWebServerCallbacks }

function TmicroWebServerCallbacks.get_RequestHeaderName(Index: Integer): String;
begin
  Result := '';
  if (Index >= 0) and (Index < FHTTPParser.Headers.Count) then
    Result := FHTTPParser.Headers.Names[Index];
end;

function TmicroWebServerCallbacks.get_RequestHeadersCount: Integer;
begin
  Result := FHTTPParser.Headers.Count;
end;

function TmicroWebServerCallbacks.get_RequetsHeaderValue(Index: Integer): String;
begin
  Result := '';
  if (Index >= 0) and (Index < FHTTPParser.Headers.Count) then
    Result := FHTTPParser.Headers.ValueFromIndex[Index];
end;

function TmicroWebServerCallbacks.get_URIParam(Index: Integer): String;
begin
  Result := '';
  if (Index >= 0) and (Index < FHTTPParser.Params.Count) then
    Result := FHTTPParser.Params.Strings[Index];
end;

function TmicroWebServerCallbacks.get_URIParamsCount: Integer;
begin
  if FHTTPParser.Params <> nil then
    Result := FHTTPParser.Params.Count
  else
    Result := 0;
end;

function TmicroWebServerCallbacks.DoProcessGETRequest(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
  Result := False;

  FHTTPParser      := HTTPParser;
  FHTTPParser.SplitURIIntoResourceAndParameters();

  FResponseHeaders := ResponseHeaders;
  FResponseData    := @ResponseData;

  if FGETCallback <> nil then
    Result := FGETCallback(Pointer(Self), PChar(FHTTPParser.Resource), @ResponseCode)
end;

function TmicroWebServerCallbacks.DoProcessPOSTRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Data : pByte;
  _DataLen : Integer;

begin
  Result := False;

  FHTTPParser  := HTTPParser;
  FHTTPParser.SplitURIIntoResourceAndParameters();

  FResponseHeaders := ResponseHeaders;
  FResponseData    := @ResponseData;

  if FPOSTCallback <> nil then
  begin
    _DataLen := FHTTPParser.Body.Size;
    if _DataLen > 0 then
      _Data := FHTTPParser.Body.Memory
    else
      _Data := nil;

    Result := FPOSTCallback(Pointer(Self), PChar(FHTTPParser.Resource), _Data, _DataLen, @ResponseCode);
  end;
end;

procedure TmicroWebServerCallbacks.ProcessWebSocketMessage(Sender: TObject; Opcode: TWebSocketDataType; PayloadBuffer: pByte; PayloadLen: Int64);
begin
  if FWebSocketMessageCallback <> nil then
    FWebSocketMessageCallback(PayloadBuffer, PayloadLen);
end;

procedure TmicroWebServerCallbacks.NotifyWebSocketConnectionEstablished(Sender: TObject);
begin
  if FWebSocketConnectedCallback <> nil then
    FWebSocketConnectedCallback();
end;

function TmicroWebServerCallbacks.addResponseHeader(const aName: String; const aValue: String): Boolean;
begin
  Result := False;

  if FResponseHeaders <> nil then
  begin
    FResponseHeaders.AddPair(aName, aValue);
    Result := True;
  end;
end;

function TmicroWebServerCallbacks.setResponseData(aBuffer: pByte; aBufferLen: Integer): Boolean;
begin
  Result := False;

  if FResponseData <> nil then
  begin
    if FResponseData^ = nil then
      FResponseData^ := TMemoryStream.Create;

    FResponseData^.Write(aBuffer^, aBufferLen);
    Result := True;
  end;
end;

end.

