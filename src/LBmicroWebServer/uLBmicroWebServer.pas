unit uLBmicroWebServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBUtils, blcksock, synsock, contnrs, Laz2_DOM,
  uWebSocketManagement, uLBBaseThread, uTimedoutCriticalSection, uLBTimers,
  fpjson, jsonparser, uLBmWsFileManager, uLBmWsDocumentsFolder;

type

  TOnGETRequest = function(
    const Resource: String;
    const Headers: TStringList;
    const URIParams: TStringList;
    var ResponseHeaders: TStringList;
    var ResponseData: TMemoryStream;
    out ResponseCode: Integer
  ): Boolean of object;

  TOnPOSTRequest = function(
    const Resource: String;
    const Headers: TStringList;
    const Payload: AnsiString;
    var ResponseHeaders: TStringList;
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
      function ProcessGETRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
      function ProcessHEADRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
      function ProcessPOSTRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;

      procedure DoExecuteTerminated();

      const
        cHeader_Upgrade = 'Upgrade';
        cValue_WebSocketUpgrade = 'websocket';

        cHeader_Connection = 'Connection';
        cValue_ConnectionUpgrade = 'Upgrade';

        cHTTPHeader_FileRange = String('RANGE');

        cGETRequest   = String('GET');
        cHEADRequest   = String('HEAD');
        cPOSTRequest  = String('POST');

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

        FInputHeaders: TStringList;
        FInputData : AnsiString;

        FOutputHeaders: TStringList;
        FOutputData: TMemoryStream;

        FProtocol : String;
        FRequestMethod : String;
        FURI : String;
        FURI_Resource : String;
        FURI_Params   : TStringList;

        FSendingFile : TLBmWsFileManager;

      function ReadIncomingRequest(): Boolean;
      function SendAnswer(aResultCode: Integer): Boolean;
      function SendHeaders(aResultCode: Integer): Boolean;

      procedure setErrorAnswer(ErrorMessage: String);

      function setSSLConnection(aSSLData: TSSLConnectionData): Boolean;

      function SplitURIIntoResourceAndParameters(const anURI: String): Boolean;

      function SendData(): Boolean; virtual;
      function ProcessHttpRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer; virtual;

    private
      property OnExecuteTerminatedInternal: TNotifyEvent write FOnExecuteTerminatedInternal;

    protected
      procedure InternalExecute; override;

    public
      constructor Create(ASocket: TSocket); reintroduce; virtual;
      destructor Destroy; override;

      property OnExecuteTerminated: TNotifyEvent write FOnExecuteTerminated;

      const
        cUploadRequestOnly = String('UpLoad');
        cUploadRequest = String('/' + cUploadRequestOnly);

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

        FSock : TTCPBlockSocket;
        FListeningPort : Integer;
        FActiveChildren : TObjectList;
        FCS : TTimedOutCriticalSection;
        FLastConnectionTimer : TTimeoutTimer;

      const
        cNoConnectionsTimeout = Int64(14400000); // 4 ore

      procedure DestroyAllChildren();
      procedure DestroySocket();
      function BindSocket(): Boolean;
      function AddNewConnections(): Boolean;
      function ResetSocket(): Boolean;
      procedure set_RequestManagerType(AValue: THTTPRequestManagerClass);

      function WaitForDestruction(): Boolean;
      procedure RemoveChild(Sender: TObject);

    protected
      procedure InternalExecute; override;

    public
      constructor Create(aListeningPort: Integer); reintroduce;
      destructor Destroy; override;

      property OnNewConnectionRequest: TNewConnectionRequestEvent write FOnNewConnectionRequest;
      property RequestManagerType: THTTPRequestManagerClass write set_RequestManagerType;

      const
        cResetTimeout = Integer(180000);
  end;


  { TLBmicroWebServer }

  TLBmicroWebServer = class(TObject)
    strict private
      FOnGETRequest                     : TOnGETRequest;
      FOnPOSTRequest                    : TOnPOSTRequest;
      FOnWebSocketConnectionEstablished : TNotifyEvent;
      FOnElaborateWebSocketMessage      : TWebSocketDataReceivedEvent;


      FListener          : TLBmWsListener;
      FSSLData           : TSSLConnectionData;
      FDocumentsFolder   : TLBmWsDocumentsFolder;

      function get_DocumentsFolder: TLBmWsDocumentsFolder;
      procedure set_DocumentsFolder(AValue: TLBmWsDocumentsFolder);
      procedure StartListeningThread(aListeningPort: Integer; aRequestManagerType: THTTPRequestManagerClass);

    public
      constructor Create();
      destructor Destroy; override;

      class function getClassDescription(): String;

      procedure Activate(aListeningPort: Integer; aRequestManagerType: THTTPRequestManagerClass);

      procedure Stop();

      function LoadFromXMLNode(aNode: TDOMNode): Boolean;

      property OnGETRequest : TOnGETRequest read FOnGETRequest write FOnGETRequest;
      property OnPOSTRequest : TOnPOSTRequest read FOnPOSTRequest write FOnPOSTRequest;
      property OnElaborateWebSocketMessage: TWebSocketDataReceivedEvent read FOnElaborateWebSocketMessage write FOnElaborateWebSocketMessage;
      property OnWebSocketConnectionEstablished: TNotifyEvent read FOnWebSocketConnectionEstablished write FOnWebSocketConnectionEstablished;

      property DocumentsFolder  : TLBmWsDocumentsFolder  read get_DocumentsFolder   write set_DocumentsFolder;
      property SSLData          : TSSLConnectionData     read FSSLData;
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

uses
  uHTTPConsts, strutils, synautil, laz2_XMLRead, ULBLogger {$IFDEF Unix}, BaseUnix{$ENDIF};

var
  gv_WebServer : TLBmicroWebServer;

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

procedure TLBmicroWebServer.StartListeningThread(aListeningPort: Integer; aRequestManagerType: THTTPRequestManagerClass);
begin
  if FListener = nil then
  begin
    FListener := TLBmWsListener.Create(aListeningPort);
    FListener.RequestManagerType := aRequestManagerType;
    FListener.AddReference(@FListener);
    FListener.Start();
  end
  else
    LBLogger.Write(1, 'TLBmicroWebServer.StartListeningThread', lmt_Warning, 'Listener already active!');
end;

procedure TLBmicroWebServer.set_DocumentsFolder(AValue: TLBmWsDocumentsFolder);
begin
  if FDocumentsFolder <> AValue then
  begin
    if FDocumentsFolder <> nil then
      FDocumentsFolder.Free;

    FDocumentsFolder := AValue;
  end;
end;

function TLBmicroWebServer.get_DocumentsFolder: TLBmWsDocumentsFolder;
begin
  if FDocumentsFolder = nil then
    FDocumentsFolder := TLBmWsDocumentsFolder.Create;

  Result := FDocumentsFolder;
end;


class function TLBmicroWebServer.getClassDescription(): String;
begin
  Result := 'WebServer';
end;


constructor TLBmicroWebServer.Create;
begin
  inherited Create();

  FSSLData := TSSLConnectionData.Create;

  FListener := nil;

  if gv_WebServer = nil then
    gv_WebServer := Self;
end;

destructor TLBmicroWebServer.Destroy;
begin
  if gv_WebServer = Self then
    gv_WebServer := nil;

  try
    FreeAndNil(FListener);

    if FDocumentsFolder <> nil then
      FreeAndNil(FDocumentsFolder);

    FreeAndNil(FSSLData);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmicroWebServer.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TLBmicroWebServer.Activate(aListeningPort: Integer; aRequestManagerType: THTTPRequestManagerClass);
begin
  Self.StartListeningThread(aListeningPort, aRequestManagerType);
end;

function TLBmicroWebServer.LoadFromXMLNode(aNode: TDOMNode): Boolean;
var
  _Node : TDOMNode;


begin
  Result := False;

  try

    if (aNode <> nil) then
    begin
      if (aNode.NodeName = Self.getClassDescription()) then
      begin

        FSSLData.LoadFromXMLNode(aNode.FindNode(TSSLConnectionData.cRootNodeName));

        _Node := aNode.FindNode(TLBmWsDocumentsFolder.cRootNodeName);
        if _Node <> nil then
        begin
          if FDocumentsFolder = nil then
            FDocumentsFolder := TLBmWsDocumentsFolder.Create;

          FDocumentsFolder.LoadFromXMLNode(aNode.FindNode(TLBmWsDocumentsFolder.cRootNodeName));
        end;

        Result := True;

      end
      else
        LBLogger.Write(1, 'TLBmicroWebServer.LoadFromXMLNode', lmt_Warning, 'Wrong parent node name <%s>', [aNode.NodeName]);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmicroWebServer.LoadFromXMLNode', lmt_Error, E.Message);
  end;
end;

procedure TLBmicroWebServer.Stop();
begin
  FreeAndNil(FListener);
  LBLogger.Write(1, 'TLBmicroWebServer.Stop', lmt_Debug, 'Web-server stopped!');
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

function THTTPRequestManager.ReadIncomingRequest(): Boolean;
var
  _RequestLine: String;
  _HeaderLine: String;
  _ContentLength: Integer = 0;
  _PayloadRead: Integer;
//  _HeaderName, _HeaderValue: String;

const
  cLongTimeout    = 60000;
  cHeaderTimeout  = 10000;
  cPayloadTimeout = 60000;

begin
  Result := False;

  FInputHeaders.Clear;
  SetLength(FInputData, 0);

  try
    _RequestLine := FSocket.RecvString(cLongTimeout);
    if FSocket.LastError = 0 then
    begin
      FRequestMethod := fetch(_RequestLine, ' ');
      FURI := fetch(_RequestLine, ' ');
      FProtocol := fetch(_RequestLine, ' ');

      if (FRequestMethod <> '') and (FURI <> '') and (FProtocol <> '') then
      begin
        Self.SplitURIIntoResourceAndParameters(FURI);

        // Lettura header
        while not Self.Terminated do
        begin
          _HeaderLine := FSocket.RecvString(cHeaderTimeout);
          if (FSocket.LastError = 0) and (_HeaderLine <> '') then
          begin
            FInputHeaders.Add(_HeaderLine);

            if (_ContentLength = 0) and _HeaderLine.Contains('Content-Length') then
              _ContentLength := StrToIntDef(SeparateRight(_HeaderLine, ' '), 0);
          end
          else
            Break;
        end;

        // Lettura payload
        if _ContentLength > 0 then
        begin
          SetLength(FInputData, _ContentLength);
          _PayloadRead := FSocket.RecvBufferEx(@FInputData[1], _ContentLength, cPayloadTimeout);
          if (FSocket.LastError = 0) then
          begin
            if (_PayloadRead < _ContentLength) then
              SetLength(FInputData, _PayloadRead);

            Result := True;
          end;
        end
        else
          Result := True;
      end
      else
        LBLogger.Write(1, 'THTTPRequestManager.ReadIncomingRequest', lmt_Warning, 'Malformed request line from <%s:%d>: <%s>', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, _RequestLine]);
    end
    else
      LBLogger.Write(1, 'THTTPRequestManager.ReadIncomingRequest', lmt_Warning, 'Error receiving request line from <%s:%d>: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.ReadIncomingRequest', lmt_Error, E.Message);
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
      _code_desc := FAnswerDescription[IntTostr(aResultCode)];
      FSocket.SendString(FProtocol + ' ' + IntTostr(aResultCode) + _code_desc + CRLF);
      FOutputHeaders.Add('Date: ' + Rfc822DateTime(Now));
      FOutputHeaders.Add('Server: micro WS by Luca Bertoncini');
      if FAllowCrossOrigin then
        FOutputHeaders.Add('Access-Control-Allow-Origin: *');

      if FSendingFile <> nil then
        FSendingFile.addResponseHeaders(FOutputHeaders)
      else if FOutputData <> nil then
        FOutputHeaders.Add('Content-length: ' + IntTostr(FOutputData.Size));

      // LBLogger.Write(1, 'THTTPRequestManager.SendHeaders', lmt_Debug, 'Headers to send: <%s>', [FOutputHeaders.Text]);

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
  Result := 404;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if Pos('HTTP/', FProtocol) = 1 then
  begin

    try

      case FRequestMethod of
        cGETRequest    : Result := Self.ProcessGETRequest(KeepConnection, NextState);
        cHEADRequest   : Result := Self.ProcessHEADRequest(KeepConnection, NextState);
        cPOSTRequest   : Result := Self.ProcessPOSTRequest(KeepConnection, NextState);
        else
          LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Warning, Format('Unknown request method <%s>', [FRequestMethod]));
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Error, E.Message);
    end;

  end
  else begin
    LBLogger.Write(1, 'THTTPRequestManager.ProcessHttpRequest', lmt_Warning, 'Wrong protocol <%s>!', [FProtocol]);
    Self.setErrorAnswer('Unknown request!');
  end;
end;



function THTTPRequestManager.SplitURIIntoResourceAndParameters(const anURI: String): Boolean;
var
  i: integer;
  _tokens: TStringArray;

begin
  Result := False;

  FURI_Resource := anURI;
  FURI_Params.Clear;

  _tokens := anURI.Split(['?']);

  if High(_tokens) = 0 then Exit(True);

  FURI_Resource := _tokens[0].Trim;

  if Length(_tokens) >= 2 then
  begin
    _tokens := _tokens[1].Split('&');
    for i := 0 to High(_tokens) do
      FURI_Params.Add(_tokens[i].Trim);
  end;

  Result := True;

  LBLogger.Write(5, 'THTTPRequestManager.SplitURIIntoResourceAndParameters', lmt_Debug, 'Resource: <%s>  -  Params: <%s>', [FURI_Resource, FURI_Params.CommaText]);

end;

function THTTPRequestManager.ProcessGETRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
var
  _DocFolder: TLBmWsDocumentsFolder;

const
  cStaticHTML = AnsiString('<!DOCTYPE html><html><head></head><body><br>Micro-WebServer working! ;-)</body></html>');

begin
  Result := 404;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  // Verifica Upgrade WebSocket
  if (Trim(FInputHeaders.Values[cHeader_Upgrade]) = cValue_WebSocketUpgrade) and
     (Pos(cValue_ConnectionUpgrade, FInputHeaders.Values[cHeader_Connection]) > 0) then
  begin
    Result := 200;
    KeepConnection := True;
    NextState := rms_ManageWebSocketSession;
    Exit;
  end;

  if FURI = cTestURI then
  begin
    FOutputHeaders.Add('Content-type: text/html');
    if FOutputData = nil then
      FOutputData := TMemoryStream.Create;


    FOutputData.Write(cStaticHTML[1], Length(cStaticHTML));

    Result := 200;
    NextState := rms_SendHTTPAnswer;
    Exit;
  end;

  // Tentativo di accesso a file statico
  _DocFolder := gv_WebServer.DocumentsFolder;
  if (_DocFolder <> nil) and _DocFolder.isValidSubpath(FURI) then
  begin
    FSendingFile := TLBmWsFileManager.Create;
    if FSendingFile.setFile(_DocFolder.RetrieveFilename(FURI), Trim(FInputHeaders.Values['Range'])) then
    begin
      Result := FSendingFile.answerStatus;
      NextState := rms_SendHTTPAnswer;
      Exit;
    end
    else
      FreeAndNil(FSendingFile);
  end;

  // Custom handler GET (fallback)
  if Assigned(gv_WebServer.OnGETRequest) then
  begin
    Result := 404;
    if gv_WebServer.OnGETRequest(FURI_Resource, FInputHeaders, FURI_Params, FOutputHeaders, FOutputData, Result) then
      NextState := rms_SendHTTPAnswer;
  end;
end;

function THTTPRequestManager.ProcessHEADRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
var
  _DocFolder: TLBmWsDocumentsFolder;

begin
  Result := 404;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  _DocFolder := gv_WebServer.DocumentsFolder;
  if (_DocFolder <> nil) and (Length(FURI) > 1) and _DocFolder.isValidSubpath(FURI) then
  begin
    FSendingFile := TLBmWsFileManager.Create;
    FSendingFile.SendHeadersOnly := True;

    if FSendingFile.setFile(_DocFolder.RetrieveFilename(FURI), Trim(FInputHeaders.Values[cHTTPHeader_FileRange])) then
    begin
      Result := FSendingFile.answerStatus;
      NextState := rms_SendHTTPAnswer;
    end
    else
      FreeAndNil(FSendingFile);
  end
  else
    LBLogger.Write(1, 'THTTPRequestManager.ProcessHEADRequest', lmt_Warning, 'File not found for HEAD <%s>', [FURI]);
end;

function THTTPRequestManager.ProcessPOSTRequest(out KeepConnection: Boolean; out NextState: THTTPRequestManagerState): Integer;
begin
  Result := 404;
  KeepConnection := False;
  NextState := rms_CloseSocket;

  if Assigned(gv_WebServer.OnPOSTRequest) then
  begin
    if gv_WebServer.OnPOSTRequest(FURI_Resource, FInputHeaders, FInputData, FOutputHeaders, FOutputData, Result) then
    begin
      NextState := rms_SendHTTPAnswer;

      // Mantieni la connessione se il client non chiede chiusura
      if LeftStr(FProtocol, 7) = 'HTTP/1.' then
      begin
        // LBLogger.Write(1, 'THTTPRequestManager.ProcessPOSTRequest', lmt_Debug, '%s', [FInputHeaders.Text]);
        FInputHeaders.NameValueSeparator := ':';
        KeepConnection := False; // LowerCase(Trim(FInputHeaders.Values[cHTTPHeader_Connection])) <> cHTTPValue_ConnectionClose;
      end;
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

begin
  _SSLData := gv_WebServer.SSLData;
  if (_SSLData <> nil) and _SSLData.hasValidData then
  begin
    if (not Self.setSSLConnection(_SSLData)) then
    begin
      LBLogger.Write(1, 'THTTPRequestManager.InternalExecute', lmt_Warning, 'SSL connection not set!');
      Exit;
    end;
  end;

  try

    FConnectionExecuted := True;

    while not Self.Terminated do
    begin

      case _InternalState of
        rms_Unknown : _InternalState := rms_ReadIncomingHTTPRequest;

        rms_ReadIncomingHTTPRequest:
          begin
            FInputHeaders.Clear;
            FInputData := '';

            if Self.ReadIncomingRequest() then // Lettura Header, uri, method
            begin
              if FOutputData <> nil then
                FOutputData.Clear;

              FOutputHeaders.Clear;

              _ResultCode := Self.ProcessHttpRequest(_KeepConnection, _InternalState); // La funzione determina il nuovo stato:  rms_SendHTTPAnswer per una singola richiesta HTTP   rms_WebSocket_SendHandshakeAnswer nel caso di richiesta di connessione tramite WebSocket
            end
            else
              _InternalState := rms_CloseSocket;
          end;

        rms_SendHTTPAnswer:  // HTTP Request
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
              FWebSocketManager.OnDataReceived := gv_WebServer.OnElaborateWebSocketMessage;
            end;

            try
              if FWebSocketManager.PerformHandshake(FInputHeaders) then
              begin
                if Assigned(gv_WebServer.OnWebSocketConnectionEstablished) then
                  gv_WebServer.OnWebSocketConnectionEstablished(Self);

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

  FDestroying := True;
  Self.DoExecuteTerminated();
end;

constructor THTTPRequestManager.Create(ASocket: TSocket);
begin
  FDestroying := False;
  inherited Create();

  FSocket := TTCPBlockSocket.Create;
  FSocket.Socket := ASocket;

  FAnswerDescription := TFPStringHashTable.Create;
  FAnswerDescription.Add('200', ' OK');
  FAnswerDescription.Add('206', ' Partial Content');
  FAnswerDescription.Add('403', ' FORBIDDEN');
  FAnswerDescription.Add('404', ' NOT FOUND');

  FAllowCrossOrigin := False;
  FInputHeaders := TStringList.Create;
  FInputHeaders.CaseSensitive := False;
  FInputHeaders.NameValueSeparator := ':';

  FURI_Params := TStringList.Create;
  FURI_Params.NameValueSeparator := '=';

  FOutputHeaders := TStringList.Create;
  FOutputData := nil;

  FConnectionExecuted := False;

  FWebSocketManager := nil;

  FSendingFile := nil;

  FStreamingMode := False;
end;

destructor THTTPRequestManager.Destroy;
begin
  FDestroying := True;

  try
    if FWebSocketManager <> nil then
      FreeAndNil(FWebSocketManager);

    SetLength(FInputData, 0);


    FreeAndNil(FSendingFile);

    FreeAndNil(FSocket);

    FreeAndNil(FInputHeaders);

    FreeAndNil(FURI_Params);


    FreeAndNil(FOutputHeaders);

    FreeAndNil(FOutputData);

    FreeAndNil(FAnswerDescription);

  except
    on E: Exception do
      LBLogger.Write(1, 'THTTPRequestManager.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;

  LBLogger.Write(5, 'THTTPRequestManager.Destroy', lmt_Debug, 'Thread destroyed', []);
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

      Result := FSocket.SSLAcceptConnection;
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

{ TLBmWsListener }

procedure TLBmWsListener.DestroyAllChildren();
var
  i: Integer;

begin
  if FActiveChildren = nil then Exit;


  if FCS.Acquire('TLBmWsListener.DestroyAllChildren') then
  begin

    try

      for i := FActiveChildren.Count - 1 downto 0 do
        THTTPRequestManager(FActiveChildren.Items[i]).Terminate;

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBmWsListener.DestroyAllChildren', lmt_Error, E.Message);
    end;
    FCS.Release();

  end;

  if not Self.WaitForDestruction() then
    LBLogger.Write(1, 'TLBmWsListener.DestroyAllChildren', lmt_Warning, 'Timeout reached!')
  else
    LBLogger.Write(1, 'TLBmWsListener.DestroyAllChildren', lmt_Debug, 'ALL CHILDREN DESTROYED!');

end;

procedure TLBmWsListener.DestroySocket();
begin
  try

    if FSock <> nil then
      FreeAndNil(FSock);

    Self.DestroyAllChildren();

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
  _tempSock    : TTCPBlockSocket;
  _Msg         : String;

const
  cWaitForConnection = 1000;

begin
  Result := False;

  if FSock.CanRead(cWaitForConnection) then
  begin
    if FSock.LastError = 0 then
    begin
      FLastConnectionTimer.Reset(cNoConnectionsTimeout);

      // Verifica limite connessioni attive
      if FCS.Acquire('TLBmWsListener.AddNewConnections (limit check)') then
      begin
        try
          _CanAccept := (FMaxActiveConnections = 0) or (FActiveChildren.Count < FMaxActiveConnections);
        finally
          FCS.Release;
        end;
        if not _CanAccept then
          _Msg := Format('Too many active connections (%d)!', [FMaxActiveConnections]);
      end
      else begin
        _CanAccept := False; // impossibile verificare stato
        _Msg := 'Not enable to verify connection count!'
      end;

      if _CanAccept then
      begin
        if Assigned(FOnNewConnectionRequest) then
          Result := FOnNewConnectionRequest(FSock.Accept)
        else begin
          _Child := FRequestManagerType.Create(FSock.Accept);
          _Child.OnExecuteTerminatedInternal := @Self.RemoveChild;

          if FCS.Acquire('TLBmWsListener.AddNewConnections') then
          begin
            try
              if not Self.Terminated then
              begin
                FActiveChildren.Add(_Child);
                Result := True;
              end;
            except
              on E: Exception do
                LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Error, 'Error adding child: %s', [E.Message]);
            end;
            FCS.Release;
          end;

          if Result then
            _Child.Start
          else begin
            LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Debug, 'Child thread discarded');
            _Child.Free;
          end;
        end;
      end
      else begin
        LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Connection rejected: %s', [_Msg]);
        _tempSock := TTCPBlockSocket.Create;
        _tempSock.Socket := FSock.Accept;

        _tempSock.CloseSocket;
        _tempSock.Free;
      end;
    end
    else
      LBLogger.Write(1, 'TLBmWsListener.AddNewConnections', lmt_Warning, 'Error accepting connection: %s', [FSock.LastErrorDesc]);
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
        Result := FActiveChildren.Count = 0;

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

  while (FActiveChildren <> nil) and (FActiveChildren.Count > 0) and (_Counter < cMaxTimeout) do
  begin
    Sleep(cSleepTime);
    Inc(_Counter, cSleepTime);
  end;

  Result := _Counter < cMaxTimeout;

  if Result then Exit;

  if not FCS.Acquire('TLBmWsListener.WaitForDestruction') then Exit;

  LBLogger.Write(1, 'TLBmWsListener.WaitForDestruction', lmt_Warning, '%d threads still alive!', [FActiveChildren.Count]);

  try

    for i := 0 to FActiveChildren.Count - 1 do
      (FActiveChildren.Items[i] as THTTPRequestManager).OnExecuteTerminatedInternal := nil;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBmWsListener.WaitForDestruction', lmt_Error, E.Message);
  end;

  FCS.Release();

end;


procedure TLBmWsListener.RemoveChild(Sender: TObject);
begin
  if (FActiveChildren = nil) or (Sender = nil) then Exit;

  if not FCS.Acquire('TLBmWsListener.RemoveChild') then Exit;

  try

    FActiveChildren.Remove(Sender);

  finally
    FCS.Release();
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

  LBLogger.Write(1, 'TLBmWsListener.InternalExecute', lmt_Debug, 'HTTP Server closed');
end;

constructor TLBmWsListener.Create(aListeningPort: Integer);
begin
  inherited Create();

  Self.setThreadName('WServerListener');


  FRequestManagerType := THTTPRequestManager;

  FCS := TTimedOutCriticalSection.Create;

  FActiveChildren := TObjectList.Create(False);

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
        if FActiveChildren <> nil then
          FreeAndNil(FActiveChildren);

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

finalization
  if gv_WebServer <> nil then
    FreeAndNil(gv_WebServer);

end.
