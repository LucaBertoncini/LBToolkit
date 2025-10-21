unit uLBmicroWebServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, contnrs, Laz2_DOM,
  uWebSocketManagement, uLBBaseThread, uTimedoutCriticalSection, uLBTimers,
  fpjson, jsonparser, uLBmWsFileManager, uLBmWsDocumentsFolder, uLBSSLConfig,
  uHTTPRequestParser, uLBCircularBuffer, fgl;

type
  TLBmicroWebServer = class;

  TOnElaborateRequest = function(
    HTTPParser: THTTPRequestParser;
    ResponseHeaders: TStringList;
    var ResponseData: TMemoryStream;
    out ResponseCode: Integer
  ): Boolean of object;

  THTTPRequestManagerClass = class of THTTPRequestManager;

  { THTTPRequestManager }

  THTTPRequestManager = class(TLBBaseThread)
    strict protected
      type
        THTTPRequestManagerState = (rms_Unknown                       = 0,
                                    rms_ReadIncomingHTTPRequest       = 1,
                                    rms_SendHTTPAnswer                = 2,
                                    rms_ManageWebSocketSession        = 3,
                                    rms_CloseSocket                   = 7);
    strict private
      FWebServerOwner: TLBmicroWebServer;
      function ProcessGETRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
      function ProcessHEADRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
      function ProcessOPTIONSRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
      function ProcessPOSTRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;

      procedure DoExecuteTerminated();

      const
        cTestURI = String('/test');


    strict protected
      const
          cWebSocketMaskedHeaderLen   = Int64(14);
          cWebSocketUnmaskedHeaderLen = Int64(10);


      var
        FOnExecuteTerminatedInternal : TNotifyEvent;
        FOnExecuteTerminated : TNotifyEvent;

        FSocket: TTCPBlockSocket;
        FWebSocketManager : TLBWebSocketSession;
        FAllowCrossOrigin : Boolean;

        FDestroying : Boolean;
        FStreamingMode : Boolean;

        FConnectionExecuted : Boolean;

        FAnswerDescription: TFPStringHashTable;

        FOutputHeaders: TStringList;
        FOutputData: TMemoryStream;

        FSendingFile : TLBmWsFileManager;
        FRecvBuffer: TLBCircularBuffer;
        FParser: THTTPRequestParser;

      function ReceiveAndParseRequest(out SocketError: Boolean): Boolean;
      function SendAnswer(aResultCode: Integer): Boolean;
      function SendHeaders(aResultCode: Integer): Boolean;

      procedure setErrorAnswer(ErrorMessage: String);

      function setSSLConnection(aSSLData: TSSLConnectionData): Boolean;

      function SendData(): Boolean; virtual;
      function ProcessHttpRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer; virtual;

    private
      property OnExecuteTerminatedInternal: TNotifyEvent write FOnExecuteTerminatedInternal;

    protected
      procedure InternalExecute; override;

    public
      constructor Create(ASocket: TSocket; AOwner: TLBmicroWebServer); reintroduce; virtual;
      destructor Destroy(); override;

      property OnExecuteTerminated: TNotifyEvent write FOnExecuteTerminated;

      const
        cUploadRequestOnly = String('UpLoad');
        cUploadRequest = String('/' + cUploadRequestOnly);

  end;

{
  TRequestChainProcessor is an abstract class designed to build
  a chain of modules that process HTTP requests (GET or POST).
  Each module can:
    - fully handle the request and stop the chain
    - modify the request parameters and pass it to the next module

  The method ProcessGETRequest or ProcessPOSTRequest must return True
  if the request was fully handled, or False if it should be passed
  to the next module in the chain (FNext).

  This approach allows for building flexible processing pipelines,
  where each component can transform or filter the request before the response.
}

  TRequestChainProcessor = class(TObject)
    strict protected
      FWebServerOwner : TLBmicroWebServer;
      FNext: TRequestChainProcessor;

      function DoProcessRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; virtual; abstract;


    private
      property Owner: TLBmicroWebServer write FWebServerOwner;


    public
      function ProcessRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean;

      property NextStep: TRequestChainProcessor write FNext;
  end;


  TNewConnectionRequestEvent = function (aSocket : TSocket): Boolean of Object;

  { TLBmWsListener }

  TLBmWsListener = class(TLBBaseThread)
    strict private
      type
        TInternalState = (is_Unknown         = 0,
                          is_Bind            = 1,
                          is_WaitConnections = 2);

      var
        FRequestManagerType : THTTPRequestManagerClass;
        FOnNewConnectionRequest : TNewConnectionRequestEvent;
        FMaxActiveConnections : Integer;
        FWebServerOwner: TLBmicroWebServer;

        FSock : TTCPBlockSocket;
        FListeningPort : Integer;
        FActiveConnections : TObjectList;
        FCS : TTimedOutCriticalSection;
        FLastConnectionTimer : TTimeoutTimer;

      const
        cNoConnectionsTimeout = Int64(14400000); // 4 hours

      procedure DestroyAllConnections();
      procedure DestroySocket();
      function BindSocket(): Boolean;
      function AddNewConnections(): Boolean;
      function ResetSocket(): Boolean;
      procedure set_RequestManagerType(AValue: THTTPRequestManagerClass);

      function WaitForDestruction(): Boolean;
      procedure RemoveConnection(Sender: TObject);

    protected
      procedure InternalExecute; override;

    public
      constructor Create(aListeningPort: Integer; aOwner: TLBmicroWebServer); reintroduce;
      destructor Destroy; override;

      property OnNewConnectionRequest: TNewConnectionRequestEvent write FOnNewConnectionRequest;
      property RequestManagerType: THTTPRequestManagerClass write set_RequestManagerType;

      const
        cResetTimeout = Integer(180000);
  end;

  TRequestChainProcessorList = specialize TFPGObjectList<TRequestChainProcessor>;

  { TLBmicroWebServer }

  TLBmicroWebServer = class(TObject)
    strict private
      FOnWebSocketConnectionEstablished : TNotifyEvent;
      FOnElaborateWebSocketMessage      : TWebSocketDataReceivedEvent;

      FCS : TTimedOutCriticalSection;

      FProcessors : TRequestChainProcessorList;

      FListeningPort     : Integer;
      FListener          : TLBmWsListener;
      FSSLData           : TSSLConnectionData;
      FDocumentsFolder   : TLBmWsDocumentsFolder;

      FAdditionalData    : TObject;

      function StartListeningThread(): Boolean;

      procedure UpdateChain();

    private
      function ProcessRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean;


    public
      constructor Create();
      destructor Destroy(); override;

      class function getClassDescription(): String;

      procedure Activate();

      procedure Stop();

      function addChainProcessor(aProcessor: TRequestChainProcessor; asFirst: Boolean): Boolean;

      procedure createDocumentFolder();

      property OnElaborateWebSocketMessage: TWebSocketDataReceivedEvent read FOnElaborateWebSocketMessage write FOnElaborateWebSocketMessage;
      property OnWebSocketConnectionEstablished: TNotifyEvent read FOnWebSocketConnectionEstablished write FOnWebSocketConnectionEstablished;

      property DocumentsFolder  : TLBmWsDocumentsFolder  read FDocumentsFolder;
      property SSLData          : TSSLConnectionData     read FSSLData;
      property ListeningPort    : Integer                read FListeningPort        write FListeningPort;

      property AdditionalData   : TObject                read FAdditionalData       write FAdditionalData;
  end;

  { TAnswerError }

  TAnswerError = class(TJSONObject)
    strict private
      procedure set_Error(AValue: String);

    public
      constructor Create; reintroduce;

      property Error: String write set_Error;

      const
        cErrorItem = String('Err');
  end;


const
  cHTTPHeader_JSONDATALEN = String('JSONLEN');
  cHTTPHeader_JSONRequest = String('JSONREQUEST');
  cHTTPHeader_PlaybackSession = String('X-Playback-Session-Id');


implementation

{$smartlink off}
uses
  uHTTPConsts, System.NetEncoding, ssl_openssl3, synautil, laz2_XMLRead, ULBLogger {$IFDEF Unix}, BaseUnix{$ENDIF};

{$smartlink on}

var
  gv_SSLConnectionCS : TTimedOutCriticalSection = nil;

{ TAnswerError }

procedure TAnswerError.set_Error(AValue: String);
begin
  Self.Elements[cErrorItem].AsString := AValue;
end;

constructor TAnswerError.Create;
begin
  inherited Create();
  Self.Add(cErrorItem, '');
end;

{ TLBmicroWebServer }

function TLBmicroWebServer.StartListeningThread: Boolean;
begin
  Result := False;

  if FListener = nil then
  begin
    if FListeningPort > 0 then
    begin
      FListener := TLBmWsListener.Create(FListeningPort, Self);
      FListener.AddReference(@FListener);
      FListener.Start();
      Result := True;
    end
    else
      LBLogger.Write(1, 'TLBmicroWebServer.StartListeningThread', lmt_Warning, 'Listening port not set!');
  end
  else
    LBLogger.Write(1, 'TLBmicroWebServer.StartListeningThread', lmt_Warning, 'Listener already active!');
end;

procedure TLBmicroWebServer.UpdateChain();
var
  i: Integer;
begin
  for i := 0 to FProcessors.Count - 2 do
    FProcessors[i].NextStep := FProcessors[i+1];

  if FProcessors.Count > 0 then
    FProcessors.Last.NextStep := nil;
end;

function TLBmicroWebServer.ProcessRequest(HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
  Result := False;

  try
    if (FProcessors.Count > 0) then
      Result := FProcessors.First.ProcessRequest(HTTPParser, ResponseHeaders, ResponseData, ResponseCode);

  except
    on E: Exception do
      LBLogger.Write(1, 'LBmicroWebServer.ProcessRequest', lmt_Error, E.Message);
  end;
end;

class function TLBmicroWebServer.getClassDescription(): String;
begin
  Result := 'WebServer';
end;


constructor TLBmicroWebServer.Create;
begin
  inherited Create();

  FDocumentsFolder := nil;
  FListeningPort := 0;

  FSSLData := TSSLConnectionData.Create;

  FProcessors := TRequestChainProcessorList.Create(True);

  FListener := nil;

  FCS := TTimedOutCriticalSection.Create;

  {$IFDEF CodeTyphon}
  InitOpenSSL3();
  {$ENDIF}
end;

destructor TLBmicroWebServer.Destroy;
begin
  try
    Self.Stop();

    FreeAndNil(FCS);

    if FDocumentsFolder <> nil then
      FreeAndNil(FDocumentsFolder);

    FreeAndNil(FProcessors);

    FreeAndNil(FSSLData);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmicroWebServer.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TLBmicroWebServer.Activate;
begin
  Self.StartListeningThread();
end;

procedure TLBmicroWebServer.createDocumentFolder;
begin
  if FCS.Acquire('TLBmicroWebServer.createDocumentFolder') then
  begin
    try
      if FDocumentsFolder = nil then
        FDocumentsFolder := TLBmWsDocumentsFolder.Create;

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBmicroWebServer.createDocumentFolder', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

procedure TLBmicroWebServer.Stop();
begin
  if FListener <> nil then
  begin
    LBLogger.Write(5, 'TLBmicroWebServer.Stop', lmt_Debug, 'Stopping web-server listener ...');
    FreeAndNil(FListener);
  end;
end;

function TLBmicroWebServer.addChainProcessor(aProcessor: TRequestChainProcessor; asFirst: Boolean): Boolean;
begin
  Result := False;
  if (aProcessor <> nil) then
  begin
    if asFirst then
      FProcessors.Insert(0, aProcessor)
    else
      FProcessors.Add(aProcessor);

    aProcessor.Owner := Self;

    Self.UpdateChain();
  end;
end;


{ THTTPRequestManager }

function THTTPRequestManager.SendData(): Boolean;
begin
  Result := False;

  try

    if (not Self.Terminated) then
    begin
      if FSendingFile <> nil then
      begin
        while (not Self.Terminated) do
        begin
          if not FSendingFile.sendData(FSocket) then
            Break;
        end;
        FreeAndNil(FSendingFile);
      end
      else if (FOutputData <> nil) and (FOutputData.Size > 0) then
      begin
        FSocket.SendBuffer(FOutputData.Memory, FOutputData.Size);
        Result := FSocket.LastError = 0;
        if not Result then
          LBLogger.Write(1, 'THTTPRequestManager.SendData', lmt_Warning, 'Error sending document data: %d  -  %s', [FSocket.LastError, FSocket.LastErrorDesc]);
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.SendData', lmt_Error, E.Message);
  end;
end;


function THTTPRequestManager.SendAnswer(aResultCode: Integer): Boolean;
begin
  try

    Result := Self.SendHeaders(aResultCode);
    if Result then
      Result := Self.SendData()
    else
      LBLogger.Write(1, 'THTTPRequestManager.SendAnswer', lmt_Warning, 'Error sending headers to <%s:%d>', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort]);

    if not Result then
      LBLogger.Write(7, 'THTTPRequestManager.SendAnswer', lmt_Warning, 'Error sending answer!');

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.SendAnswer', lmt_Error, E.Message);
  end;
end;

function THTTPRequestManager.SendHeaders(aResultCode: Integer): Boolean;
var
  h : Integer;
  _code_desc: AnsiString;

begin
  Result := False;

  if (not Self.Terminated) then
  begin
    try
      if (FOutputHeaders.Count > 0) and (FOutputHeaders[FOutputHeaders.Count - 1] = '') then
        FOutputHeaders.Delete(FOutputHeaders.Count - 1);

      _code_desc := FAnswerDescription[IntTostr(aResultCode)];
      FSocket.SendString(FParser.HTTPVersion + ' ' + IntTostr(aResultCode) + _code_desc + CRLF);

      if FOutputHeaders.IndexOfName(HTTP_HEADER_DATE) = -1 then
        FOutputHeaders.Add(HTTP_HEADER_DATE + ': ' + Rfc822DateTime(Now));

      if FOutputHeaders.IndexOfName(HTTP_HEADER_SERVER) = -1 then
        FOutputHeaders.Add(HTTP_HEADER_SERVER + ': micro WS by Luca Bertoncini');

      if FAllowCrossOrigin then
      begin
        if FOutputHeaders.IndexOfName(HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN) = -1 then
          FOutputHeaders.Add(HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN + ': *');
      end;

      if FSendingFile <> nil then
        FSendingFile.addResponseHeaders(FOutputHeaders)
      else if FOutputData <> nil then
      begin
        if FOutputHeaders.IndexOfName(HTTP_HEADER_CONTENT_LENGTH) = -1 then
          FOutputHeaders.Add(HTTP_HEADER_CONTENT_LENGTH + ': ' + IntTostr(FOutputData.Size))
        else
          LBLogger.Write(5, 'THTTPRequestManager.SendHeaders', lmt_Debug, 'Header <%s> already present', [HTTP_HEADER_CONTENT_LENGTH]);
      end;

      FOutputHeaders.Add('');

      Result := True;
      for h := 0 to FOutputHeaders.Count - 1 do
      begin
        FSocket.SendString(FOutputHeaders[h] + CRLF);
        if FSocket.LastError <> 0 then
        begin
          LBLogger.Write(1, 'THTTPRequestManager.SendHeaders', lmt_Warning, 'Error sending headers to <%s:%d>: %d  -  %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastError, FSocket.LastErrorDesc]);
          Result := False;
          Break;
        end;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'THTTPRequestManager.SendHeaders', lmt_Error, E.Message);
    end;
  end;
end;

procedure THTTPRequestManager.setErrorAnswer(ErrorMessage: String);
var
  _sError : AnsiString;
  _Error : TAnswerError;

begin
  _Error := TAnswerError.Create;
  _Error.Error := ErrorMessage;
  _Error.CompressedJSON := True;
  _sError := _Error.AsJSON;
  _Error.Free;

  if FOutputData = nil then
    FOutputData := TMemoryStream.Create
  else
    FOutputData.Clear;

  FOutputData.Position := 0;
  FOutputData.Write(_sError[1], Length(_sError));
end;

function THTTPRequestManager.ProcessHttpRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
begin
  Result := HTTP_STATUS_NOT_FOUND;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if Pos('HTTP/', FParser.HTTPVersion) = 1 then
  begin

    try

      case FParser.Method of
        HTTP_METHOD_GET     : Result := Self.ProcessGETRequest(KeepConnection, NextState);
        HTTP_METHOD_HEAD    : Result := Self.ProcessHEADRequest(KeepConnection, NextState);
        HTTP_METHOD_POST    : Result := Self.ProcessPOSTRequest(KeepConnection, NextState);
        HTTP_METHOD_OPTIONS : Result := Self.ProcessOPTIONSRequest(KeepConnection, NextState);

        else
          LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Warning, Format('Unknown request method <%s>', [FParser.Method]));
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Error, E.Message);
    end;

  end
  else begin
    LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Warning, 'Wrong protocol <%s>!', [FParser.HTTPVersion]);
    Self.setErrorAnswer('Unknown request!');
  end;
end;



function THTTPRequestManager.ProcessGETRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
var
  _DocFolder: TLBmWsDocumentsFolder;

const
  cStaticHTML = AnsiString('<!DOCTYPE html><html><head></head><body><br>Micro-WebServer working! ;-)</body></html>');

begin
  Result := HTTP_STATUS_NOT_FOUND;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  try
    // Check for WebSocket Upgrade
    if (Trim(FParser.Headers.Values[HTTP_HEADER_UPGRADE]) = HTTP_UPGRADE_WEBSOCKET) and
       (Pos(HTTP_CONNECTION_UPGRADE, FParser.Headers.Values[HTTP_HEADER_CONNECTION]) > 0) then
    begin
      Result := HTTP_STATUS_OK;
      KeepConnection := True;
      NextState := rms_ManageWebSocketSession;
      Exit;
    end;

    if FParser.URI = cTestURI then
    begin
      LBLogger.Write(5, 'THTTPRequestManager.ProcessGETRequest', lmt_Debug, 'Test request');

      FOutputHeaders.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + MIME_TYPE_HTML);
      if FOutputData = nil then
        FOutputData := TMemoryStream.Create
      else
        FOutputData.Clear;


      FOutputData.Write(cStaticHTML[1], Length(cStaticHTML));

      Result := HTTP_STATUS_OK;
      NextState := rms_SendHTTPAnswer;
      Exit;
    end;

    // Attempt to access static file
    if (FWebServerOwner <> nil) then
    begin
      _DocFolder := FWebServerOwner.DocumentsFolder;
      if (_DocFolder <> nil) and _DocFolder.isValidSubpath(FParser.URI) then
      begin
        FSendingFile := TLBmWsFileManager.Create;
        if FSendingFile.setFile(_DocFolder.RetrieveFilename(FParser.URI), Trim(FParser.Headers.Values[HTTP_HEADER_RANGE])) then
        begin
          Result := FSendingFile.answerStatus;
          NextState := rms_SendHTTPAnswer;
          Exit;
        end
        else
          FreeAndNil(FSendingFile);
      end;

      // Custom GET handler (fallback)
      Result := HTTP_STATUS_NOT_FOUND;
      if FWebServerOwner.ProcessRequest(FParser, FOutputHeaders, FOutputData, Result) then
        NextState := rms_SendHTTPAnswer;
    end
    else
      LBLogger.Write(1, 'THTTPRequestManager.ProcessGETRequest', lmt_Warning, 'WebServer not set!');

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.ProcessGETRequest', lmt_Error, E.Message);
  end;
end;

function THTTPRequestManager.ProcessHEADRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
var
  _DocFolder: TLBmWsDocumentsFolder;

begin
  Result := HTTP_STATUS_NOT_FOUND;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if (FWebServerOwner <> nil) then
  begin
    _DocFolder := FWebServerOwner.DocumentsFolder;
    if (_DocFolder <> nil) and (Length(FParser.URI) > 1) and _DocFolder.isValidSubpath(FParser.URI) then
    begin
      FSendingFile := TLBmWsFileManager.Create;
    FSendingFile.SendHeadersOnly := True;

    if FSendingFile.setFile(_DocFolder.RetrieveFilename(FParser.URI), Trim(FParser.Headers.Values[HTTP_HEADER_RANGE])) then
    begin
      Result := FSendingFile.answerStatus;
      NextState := rms_SendHTTPAnswer;
    end
    else
      FreeAndNil(FSendingFile);
    end
    else
      LBLogger.Write(1, 'THTTPRequestManager.ProcessHEADRequest', lmt_Warning, 'File not found for HEAD <%s>', [FParser.URI]);
  end;
end;

function THTTPRequestManager.ProcessOPTIONSRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
begin
  LBLogger.Write(5, 'THTTPRequestManager.ProcessOPTIONSRequest', lmt_Debug, 'OPTIONS request');

  Result := HTTP_STATUS_NOT_FOUND;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if (FWebServerOwner <> nil) then
  begin
    FOutputHeaders.Add(HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN + ': *');
    FOutputHeaders.Add(HTTP_HEADER_ACCESS_CONTROL_ALLOW_METHODS + ': ' + HTTP_METHOD_POST + ', ' + HTTP_METHOD_OPTIONS);
    FOutputHeaders.Add(HTTP_HEADER_ACCESS_CONTROL_ALLOW_HEADERS + ': ' + HTTP_HEADER_CONTENT_TYPE);

    Result := HTTP_STATUS_OK;
    NextState := rms_SendHTTPAnswer;
    KeepConnection := True;
  end;

end;

function THTTPRequestManager.ProcessPOSTRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
begin
  Result := HTTP_STATUS_NOT_FOUND;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if (FWebServerOwner <> nil) then
  begin
    if FWebServerOwner.ProcessRequest(FParser, FOutputHeaders, FOutputData, Result) then
    begin
      NextState := rms_SendHTTPAnswer;
      KeepConnection := False;
    end;
  end;
end;

procedure THTTPRequestManager.DoExecuteTerminated;
begin
  try

    if Assigned(FOnExecuteTerminated) then
      FOnExecuteTerminated(Self);

    if Assigned(FOnExecuteTerminatedInternal) then
      FOnExecuteTerminatedInternal(Self);

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.DoExecuteTerminated', lmt_Error, E.Message);
  end;

end;

procedure THTTPRequestManager.InternalExecute;
var
  _ResultCode             : Integer;
  _KeepConnection         : Boolean = False;
  _InternalState          : THTTPRequestManagerState = rms_ReadIncomingHTTPRequest;
  _SSLData                : TSSLConnectionData;
  _SocketError            : Boolean;

begin

  _SocketError := False;

  if (FWebServerOwner <> nil) then
  begin
    _SSLData := FWebServerOwner.SSLData;
    if (_SSLData <> nil) and _SSLData.hasValidData then
    begin
      if (not Self.setSSLConnection(_SSLData)) then
      begin
        LBLogger.Write(1, 'THTTPRequestManager.InternalExecute', lmt_Warning, 'SSL connection not set!');
        _SocketError := True;
      end;
    end;
  end;

  if not _SocketError then
  begin
    try

      FConnectionExecuted := True;

      while not Self.Terminated do
      begin

        case _InternalState of
          rms_Unknown : _InternalState := rms_ReadIncomingHTTPRequest;

          rms_ReadIncomingHTTPRequest:
            begin
              if self.ReceiveAndParseRequest(_SocketError) then
              begin
                if FOutputData <> nil then
                  FOutputData.Clear;

                FOutputHeaders.Clear;

                // This function determines the new state: rms_SendHTTPAnswer for a single HTTP request or rms_ManageWebSocketSession for a WebSocket connection request.
                _ResultCode := Self.ProcessHttpRequest(_KeepConnection, _InternalState);
              end
              else
                _InternalState := rms_CloseSocket;
            end;

          rms_SendHTTPAnswer:
            begin
              if (not Self.SendAnswer(_ResultCode)) or (not _KeepConnection) then
                _InternalState := rms_CloseSocket
              else
                _InternalState := rms_ReadIncomingHTTPRequest;
            end;

          rms_ManageWebSocketSession:
            begin
              if FWebSocketManager = nil then
              begin
                FWebSocketManager := TLBWebSocketSession.Create(FSocket, @Self.Terminated);
                if FWebServerOwner <> nil then
                  FWebSocketManager.OnDataReceived := FWebServerOwner.OnElaborateWebSocketMessage;
              end;

              try
                if FWebSocketManager.PerformHandshake(FParser.Headers) then
                begin
                  if (FWebServerOwner <> nil) and Assigned(FWebServerOwner.OnWebSocketConnectionEstablished) then
                    FWebServerOwner.OnWebSocketConnectionEstablished(Self);

                  FWebSocketManager.ExecuteSession();
                end;

              finally
                FreeAndNil(FWebSocketManager);
                _InternalState := rms_CloseSocket;
              end;
            end;

          rms_CloseSocket:
            begin
              FSocket.CloseSocket;
              Break;
            end;

          else begin
            LBLogger.Write(1, 'THTTPRequestManager.InternalExecute', lmt_Warning, 'Wrong internal state: %d', [Integer(_InternalState)]);
            Break;
          end;
        end;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'THTTPRequestManager.InternalExecute', lmt_Error, '<%s>  -  %s', [Self.ClassName, E.Message]);
    end;

  end;

  FDestroying := True;
  Self.DoExecuteTerminated();
end;

function THTTPRequestManager.ReceiveAndParseRequest(out SocketError: Boolean): Boolean;
const
  cReadChunkSize = 4096; // Read in 4KB chunks
  cRequestIdleTimeout = 10000; // 10 seconds idle timeout

var
  parseResult: TParserResult;
  _lastDataTime: TTimeoutTimer;
  bytesRead: Integer;

begin
  SocketError := False;
  Result := False;
  FParser.Reset;
//  FRecvBuffer.Clear;

  _lastDataTime := TTimeoutTimer.Create(cRequestIdleTimeout);

  try
    while not Terminated do
    begin
      // Try to parse what we already have in the buffer
      parseResult := FParser.Parse;

      case parseResult of
        prComplete:
          begin
            Result := True;
            Break;
          end;

        prError:
          begin
            Result := False;
            Break;
          end;

        else begin

          // If more data is needed, wait for it on the socket
          if (FSocket.WaitingData > 0) or FSocket.CanRead(100) then
          begin
            bytesRead := FRecvBuffer.WriteFromSocket(FSocket, cReadChunkSize);
            if bytesRead > 0 then
              _lastDataTime.Reset // Reset idle timer since we got data
            else begin
              // CanRead was true but read failed, socket error
              SocketError := bytesRead < 0;
              Result := False;
              Break;
            end;
          end;

          // Check for idle timeout
          if _lastDataTime.Expired then
          begin
            Result := False;
            Break;
          end;

        end;
      end;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.ReceiveAndParseRequest', lmt_Error, E.Message);
  end;
  _lastDataTime.Free;
end;

constructor THTTPRequestManager.Create(ASocket: TSocket; AOwner: TLBmicroWebServer);
begin
  FDestroying := False;
  inherited Create();

  FWebServerOwner := AOwner;
  FSocket := TTCPBlockSocket.Create;
  FSocket.Socket := ASocket;

  FRecvBuffer := TLBCircularBuffer.Create(16 * 1024); // 16KB buffer
  FParser := THTTPRequestParser.Create(FRecvBuffer);

  FAnswerDescription := TFPStringHashTable.Create;
  FAnswerDescription.Add('200', ' OK');
  FAnswerDescription.Add('206', ' Partial Content');
  FAnswerDescription.Add('403', ' FORBIDDEN');
  FAnswerDescription.Add('404', ' NOT FOUND');

  FAllowCrossOrigin := False;

  FOutputHeaders := TStringList.Create;
  FOutputHeaders.CaseSensitive := False;
  FOutputHeaders.NameValueSeparator := ':';

  FOutputData := nil;

  FConnectionExecuted := False;

  FWebSocketManager := nil;

  FSendingFile := nil;

  FStreamingMode := False;
end;

destructor THTTPRequestManager.Destroy;
begin
  FDestroying := True;

  inherited Destroy;

  try
    if FWebSocketManager <> nil then
      FreeAndNil(FWebSocketManager);

    FreeAndNil(FParser);
    FreeAndNil(FRecvBuffer);
    FreeAndNil(FSendingFile);

    FreeAndNil(FSocket);

    FreeAndNil(FOutputHeaders);

    FreeAndNil(FOutputData);

    FreeAndNil(FAnswerDescription);

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.Destroy', lmt_Error, E.Message);
  end;
end;

function THTTPRequestManager.setSSLConnection(aSSLData: TSSLConnectionData): Boolean;
begin
  Result := False;

  try

    if aSSLData.hasValidData then
    begin

      FSocket.SSL.SSLType := LT_all;
//      FSocket.SSL.VerifyCert := False;

      FSocket.SSL.CertificateFile := aSSLData.CertificateFile;
      FSocket.SSL.PrivateKeyFile := aSSLData.PrivateKeyFile;

      FSocket.SSL.KeyPassword := aSSLData.KeyPassword;

      if gv_SSLConnectionCS.Acquire('THTTPRequestManager.setSSLConnection') then
      begin
        try
          Result := FSocket.SSLAcceptConnection;
        finally
          gv_SSLConnectionCS.Release();
        end;
      end;
      // Result := FSocket.SSL.LastError = 0;

      if not Result then
        LBLogger.Write(1, 'THTTPRequestManager.setSSLConnection', lmt_Warning, 'Error setting ssl connection with client <%s:%d>: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.SSL.LastErrorDesc]);
    end;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'THTTPRequestManager.setSSLConnection', lmt_Error, E.Message);
      Result := False;
    end;
  end;

  if not Result then
    FSocket.SSLDoShutdown;
end;

{ TRequestChainProcessor }

function TRequestChainProcessor.ProcessRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
  try

    Result := Self.DoProcessRequest(HTTPParser, ResponseHeaders, ResponseData, ResponseCode);

    // if Result = True the chain is blocked
    if (not Result) and (FNext <> nil) then
    begin
      LBLogger.Write(5, 'TRequestChainProcessor.ProcessRequest', lmt_Debug, 'Running next processor');
      Result := FNext.ProcessRequest(HTTPParser, ResponseHeaders, ResponseData, ResponseCode);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TRequestChainProcessor.ProcessRequest', lmt_Error, E.Message);
  end;
end;


{ TLBmWsListener }

procedure TLBmWsListener.DestroyAllConnections();
var
  i: Integer;

begin
  if FActiveConnections <> nil then
  begin


    if FCS.Acquire('TLBmWsListener.DestroyAllConnections') then
    begin

      try

        for i := FActiveConnections.Count - 1 downto 0 do
          THTTPRequestManager(FActiveConnections.Items[i]).Terminate;

      except
        on E: Exception do
          LBLogger.Write(1, 'TLBmWsListener.DestroyAllConnections', lmt_Error, E.Message);
      end;
      FCS.Release();

    end;

    if not Self.WaitForDestruction() then
      LBLogger.Write(1, 'TLBmWsListener.DestroyAllConnections', lmt_Warning, 'Timeout reached!')
    else
      LBLogger.Write(1, 'TLBmWsListener.DestroyAllConnections', lmt_Debug, 'All connections destroyed!');
  end;
end;

procedure TLBmWsListener.DestroySocket();
begin
  try

    if FSock <> nil then
      FreeAndNil(FSock);

    Self.DestroyAllConnections();

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.DestroySocket', lmt_Error, E.Message);
  end;
end;

function TLBmWsListener.BindSocket(): Boolean;
{$IFDEF Unix}
var
  _flags : Integer;

const
  FD_CLOEXEC = 1;

{$ENDIF}
begin
  Result := False;

  try

    Self.DestroySocket();

    if FListeningPort > 0 then
    begin
      FSock := TTCPBlockSocket.Create;
      FSock.Bind('0.0.0.0', IntToStr(FListeningPort));

      if FSock.LastError <> 0 then
        LBLogger.Write(1, 'TLBmWsListener.BindSocket', lmt_Warning, 'Error binding port %d: %d  -  %s', [FListeningPort, FSock.LastError, FSock.LastErrorDesc])
      else begin
        FSock.Listen;
        Result :=  FSock.LastError = 0;

        if Result then
        begin
          {$IFDEF Unix}
          _flags := fpFcntl(FSock.Socket, F_GETFD, 0);
          fpFcntl(FSock.Socket, F_SETFD, _flags or FD_CLOEXEC);
          {$ENDIF}
          FLastConnectionTimer.Reset(cNoConnectionsTimeout);
          LBLogger.Write(1, 'TLBmWsListener.BindSocket', lmt_Debug, 'Listening on port %d', [FListeningPort]);
        end
        else
          LBLogger.Write(1, 'TLBmWsListener.BindSocket', lmt_Warning, 'Error listening on port %d: %s', [FListeningPort, FSock.LastErrorDesc]);

      end;
    end
    else
      LBLogger.Write(1, 'TLBmWsListener.BindSocket', lmt_Warning, 'Wrong listening port value (%d)', [FListeningPort]);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.BindSocket', lmt_Error, E.Message);
  end;
end;

function TLBmWsListener.AddNewConnections(): Boolean;
var
  _Child       : THTTPRequestManager;
  _CanAccept   : Boolean;
  _tempSock    : TSocket;
  _Msg         : String;

const
  cWaitForConnection = 1000;

begin
  Result := False;

  try
    if FSock.CanRead(cWaitForConnection) then
    begin
      if FSock.LastError = 0 then
      begin
        FLastConnectionTimer.Reset(cNoConnectionsTimeout);

        // Check active connections limit
        if FCS.Acquire('TLBmWsListener.AddNewConnections (limit check)') then
        begin
          try
            _CanAccept := (FMaxActiveConnections = 0) or (FActiveConnections.Count < FMaxActiveConnections);
          finally
            FCS.Release;
          end;
          if not _CanAccept then
            _Msg := Format('Too many active connections (%d)!', [FMaxActiveConnections]);
        end
        else begin
          _CanAccept := False; // unable to verify state
          _Msg := 'Not enable to verify connection count!'
        end;

        if _CanAccept then
        begin
          _tempSock := FSock.Accept;

          // Verifica che Accept sia andato a buon fine
          if (_tempSock <> INVALID_SOCKET) and (FSock.LastError = 0) then
          begin

            if Assigned(FOnNewConnectionRequest) then
              Result := FOnNewConnectionRequest(_tempSock)
            else begin

              _Child := FRequestManagerType.Create(_tempSock, FWebServerOwner);
              _Child.OnExecuteTerminatedInternal := @Self.RemoveConnection;

              if FCS.Acquire('TLBmWsListener.AddNewConnections') then
              begin
                try
                  if not Self.Terminated then
                  begin
                    FActiveConnections.Add(_Child);
                    Result := True;
                  end;
                except
                  on E: Exception do
                    LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Error, 'Error adding child: %s', [E.Message]);
                end;
                FCS.Release;
              end;

              if Result then
                _Child.Start()
              else begin
                LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Debug, 'Child thread discarded');
                _Child.Free;
              end;
            end;
          end
          else
            LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Accept failed: %s', [FSock.LastErrorDesc]);
        end
        else begin
          LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Connection rejected: %s', [_Msg]);
          _tempSock := FSock.Accept;
          if _tempSock <> INVALID_SOCKET then
          begin
            Self.PauseFor(100);
            synsock.CloseSocket(_tempSock);
          end
          else if FSock.LastError <> 0 then
            LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Accept failed on rejection: %s', [FSock.LastErrorDesc]);
        end;
      end
      else
        LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Error accepting connection: %s', [FSock.LastErrorDesc]);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Error, E.Message);
  end;
end;

function TLBmWsListener.ResetSocket(): Boolean;
begin
  Result := False;

  if FLastConnectionTimer.Expired() then
  begin
    if FCS.Acquire('TLBmWsListener.ResetSocket') then
    begin

      try
        Result := FActiveConnections.Count = 0;

      finally
        FCS.Release();
      end;

    end;
  end;

  if Result then
    LBLogger.Write(1, 'TLBmWsListener.ResetSocket', lmt_Debug, 'Socket needs to be resetted!');
end;

procedure TLBmWsListener.set_RequestManagerType(AValue: THTTPRequestManagerClass);
begin
  if aValue = nil then
    FRequestManagerType := THTTPRequestManager
  else
    FRequestManagerType := AValue;
end;

function TLBmWsListener.WaitForDestruction(): Boolean;
var
  _Counter : Integer;
  i : Integer;

const
  cMaxTimeout: Integer = 8000;
  cSleepTime: Integer = 100;

begin
  _Counter := 0;

  while (FActiveConnections <> nil) and (FActiveConnections.Count > 0) and (_Counter < cMaxTimeout) do
  begin
    Sleep(cSleepTime);
    Inc(_Counter, cSleepTime);
  end;

  Result := _Counter < cMaxTimeout;

  if not Result then
  begin

    if FCS.Acquire('TLBmWsListener.WaitForDestruction') then
    begin

      LBLogger.Write(1, 'TLBmWsListener.WaitForDestruction', lmt_Warning, '%d threads still alive!', [FActiveConnections.Count]);

      try

        for i := 0 to FActiveConnections.Count - 1 do
          (FActiveConnections.Items[i] as THTTPRequestManager).OnExecuteTerminatedInternal := nil;

      except
        on E: Exception do
          LBLogger.Write(1, 'TLBmWsListener.WaitForDestruction', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

  end;

end;


procedure TLBmWsListener.RemoveConnection(Sender: TObject);
begin

  if (FActiveConnections <> nil) and (Sender <> nil) then
  begin

    if FCS.Acquire('TLBmWsListener.RemoveChild') then
    begin

      try

        FActiveConnections.Remove(Sender);

      finally
        FCS.Release();
      end;

    end;
  end;
end;


procedure TLBmWsListener.InternalExecute;
var
  _InternalState : TInternalState;

const
  cWaitBeforeBindAgain = Integer(5000);

begin

  try
    if FListeningPort > 0 then
    begin
      _InternalState := is_Bind;

      while not Self.Terminated do
      begin

        case _InternalState of
          is_Unknown: _InternalState := is_Bind;

          is_Bind:
            begin
              if Self.BindSocket() then
                _InternalState := is_WaitConnections
              else
                Self.PauseFor(cWaitBeforeBindAgain);
            end;

          is_WaitConnections:
            begin
              if not Self.AddNewConnections() then
              begin

                if Self.ResetSocket() then
                  _InternalState := is_Bind;

              end;
            end;
        end;
      end;
    end
    else
      LBLogger.Write(1, 'TLBmWsListener.InternalExecute', lmt_Warning, 'Listening port not set!');

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.InternalExecute', lmt_Error, E.Message);
  end;

  LBLogger.Write(1, 'TLBmWsListener.InternalExecute', lmt_Debug, 'HTTP Listener terminated');
end;

constructor TLBmWsListener.Create(aListeningPort: Integer; aOwner: TLBmicroWebServer);
begin
  inherited Create();

  Self.setThreadName('WSListener');

  FWebServerOwner := aOwner;
  FRequestManagerType := THTTPRequestManager;

  FCS := TTimedOutCriticalSection.Create;

  FActiveConnections := TObjectList.Create(False);

  FLastConnectionTimer := TTimeoutTimer.Create(cNoConnectionsTimeout);
  FListeningPort := aListeningPort;
  FMaxActiveConnections := 0;
end;

destructor TLBmWsListener.Destroy;
begin
  inherited Destroy;

  try
    Self.DestroySocket();


    if FCS.Acquire('TLBmWsListener.Destroy', 10000) then
    begin

      try

        if FActiveConnections <> nil then
          FreeAndNil(FActiveConnections);

      except
        on E1: Exception do
          LBLogger.Write(1, 'TLBmWsListener.Destroy', lmt_Error, '1. %s' , [E1.Message]);
      end;

    end;

    FreeAndNil(FCS);

    if FLastConnectionTimer <> nil then
      FreeAndNil(FLastConnectionTimer);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.Destroy', lmt_Error, E.Message);
  end;
end;

initialization
  gv_SSLConnectionCS := TTimedOutCriticalSection.Create;

finalization
  if gv_SSLConnectionCS <> nil then
    FreeAndNil(gv_SSLConnectionCS);

end.


