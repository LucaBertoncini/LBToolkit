unit uWebSocketManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, uTimedoutCriticalSection, uHTTPConsts;

type
  TWebSocketProtocolElementType = (wse_StartByte        = 0,
                                   wse_MaskLenByte      = 1,
                                   wse_PayloadLenAsWord = 2,
                                   wse_PayloadLenAsIn64 = 3,
                                   wse_MaskValue        = 4,
                                   wse_PayLoad          = 5);



  TWebSocketDataType = (wsd_FrameContinuation = 0,
                        wsd_FrameText         = 1,
                        wsd_FrameBinary       = 2,
                        wsd_Unused1           = 3,
                        wsd_Unused2           = 4,
                        wsd_Unused3           = 5,
                        wsd_Unused4           = 6,
                        wsd_Unused5           = 7,
                        wsd_ConnectionClose   = 8,
                        wsd_Ping              = 9,
                        wsd_Pong              = 10,
                        wsd_Unused9           = 11,
                        wsd_Unused10          = 12,
                        wsd_Unused11          = 13,
                        wsd_Unused12          = 14,
                        wsd_Unused13          = 15);


type
  TWebSocketDataReceivedEvent = procedure(Sender: TObject; Opcode: TWebSocketDataType; PayloadBuffer: pByte; PayloadLen: Int64) of object;


  TWebSocketQueuedMessage = record
    Payload : TBytes;
    Opcode  : TWebSocketDataType;
    Final   : Boolean;
  end;
  pWebSocketQueuedMessage = ^TWebSocketQueuedMessage;

  { TLBWebSocketSession }

  TLBWebSocketSession = class(TObject)
  strict private
    FOnDataReceived: TWebSocketDataReceivedEvent;

    FMessages : TList;
    FCS : TTimedOutCriticalSection;

    FSocket: TTCPBlockSocket;
    FTerminate: PBoolean;
    FNewDataEvent: PRTLEvent;
    FLastActivity: QWord;
    FConnectionError : Boolean;
    FFrameHeader: array[0..9] of Byte; // massimo header size (1 + 1 + 8)

    procedure ClearList();
    function SendWebSocketMessages(): Boolean;

    const
      WS_BASE_HANDSHAKE_RESPONSE = WS_HANDSHAKE_STATUS_LINE + #13#10 +
                                   WS_HANDSHAKE_UPGRADE_HEADER + #13#10 +
                                   WS_HANDSHAKE_CONNECTION_HEADER + #13#10;

  (*
    const
      cBaseWebsocketHeader = cWebSocket_HandshakeStatus +
                             cWebSocket_HandshakeUpgrade +
                             cWebSocket_HandshakeConnect;
*)

    function ReadByte(out Value: Byte; Timeout: Integer): Boolean;

    function SendFrame(const Payload: TBytes; Opcode: TWebSocketDataType = wsd_FrameText; Final: Boolean = True): Boolean;
    function SendPing(const Payload: AnsiString = ''): Boolean;
    function ReceiveWebSocketMessage(out CloseRequested: Boolean): Boolean;
    procedure CloseSession;

  public
    constructor Create(aSocket: TTCPBlockSocket; aTerminateRequest: pBoolean);
    destructor Destroy; override;

    function addMessageToSend(const aMessage: AnsiString; Opcode: TWebSocketDataType = wsd_FrameText; Final: Boolean = True): Boolean; overload;
    function addMessageToSend(aBuffer: pByte; aBufferLen: Integer; Opcode: TWebSocketDataType = wsd_FrameText; Final: Boolean = True): Boolean; overload;

    function PerformHandshake(InputHeaders: TStringList): Boolean;
    function ExecuteSession(): Boolean;

    property OnDataReceived: TWebSocketDataReceivedEvent write FOnDataReceived;
  end;

implementation

uses
  ULBLogger, sha1, uBase64Util, synsock, TypInfo;

constructor TLBWebSocketSession.Create(aSocket: TTCPBlockSocket; aTerminateRequest: pBoolean);
begin
  inherited Create;

  FTerminate := aTerminateRequest;
  FSocket := aSocket;
  FNewDataEvent := RTLEventCreate();
  FConnectionError := False;

  FMessages := TList.Create;
  FCS := TTimedOutCriticalSection.Create;

end;

destructor TLBWebSocketSession.Destroy;
begin
  try

    Self.ClearList();
    FreeAndNil(FCS);
    FreeAndNil(FMessages);

    RTLEventDestroy(FNewDataEvent);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketSession.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

function TLBWebSocketSession.addMessageToSend(const aMessage: AnsiString; Opcode: TWebSocketDataType; Final: Boolean): Boolean;
var
  _Msg : pWebSocketQueuedMessage = nil;

begin
  Result := False;

  if aMessage <> '' then
  begin
    New(_Msg);
    SetLength(_Msg^.Payload, Length(aMessage));
    Move(aMessage[1], _Msg^.Payload[0], Length(_Msg^.Payload));

    _Msg^.Opcode  := Opcode;
    _Msg^.Final   := Final;

    if FCS.Acquire('TLBWebSocketSession.addMessageToSend') then
    begin
      try
        FMessages.Add(_Msg);
        _Msg := nil;
        Result := True;
      except
        on E: Exception do
          LBLogger.Write(1, 'TLBWebSocketSession.addMessageToSend', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

    if _Msg <> nil then
      Dispose(_Msg);

    if Result then
      RTLEventSetEvent(FNewDataEvent);
  end;
end;

function TLBWebSocketSession.addMessageToSend(aBuffer: pByte; aBufferLen: Integer; Opcode: TWebSocketDataType; Final: Boolean): Boolean;
var
  _Msg : pWebSocketQueuedMessage = nil;

begin
  Result := False;

  if (aBufferLen > 0) and (aBuffer <> nil) then
  begin
    New(_Msg);
    SetLength(_Msg^.Payload, aBufferLen);
    Move(aBuffer, _Msg^.Payload[0], aBufferLen);

    _Msg^.Opcode := Opcode;
    _Msg^.Final  := Final;

    if FCS.Acquire('TLBWebSocketSession.addMessageToSend') then
    begin
      try
        FMessages.Add(_Msg);
        _Msg := nil;
        Result := True;
      except
        on E: Exception do
          LBLogger.Write(1, 'TLBWebSocketSession.addMessageToSend', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

    if _Msg <> nil then
      Dispose(_Msg);

    if Result then
      RTLEventSetEvent(FNewDataEvent);
  end;
end;

procedure TLBWebSocketSession.ClearList;
var
  i : Integer;

begin
  if FCS.Acquire('TLBWebSocketSession.ClearList') then
  begin
    try
      for i := 0 to FMessages.Count - 1 do
        Dispose(pWebSocketQueuedMessage(FMessages.Items[i]));

      FMessages.Clear;

    finally
      FCS.Release();
    end;
  end;
end;

function TLBWebSocketSession.SendWebSocketMessages: Boolean;
var
  _List : TList = nil;
  _Msg : pWebSocketQueuedMessage;
  i : Integer;

begin
  Result := True;

  if FCS.Acquire('TLBWebSocketSession.SendWebSocketMessage') then
  begin
    try

      if FMessages.Count > 0 then
      begin
        _List := FMessages;
        FMessages := TList.Create;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBWebSocketSession.SendWebSocketMessage', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;

  if _List <> nil then
  begin
    for i := 0 to _List.Count - 1 do
    begin
      _Msg := _List.Items[0];
      _List.Delete(0);

      Result := Self.SendFrame(_Msg^.Payload, _Msg^.Opcode, _Msg^.Final);
      Dispose(_Msg);
      if not Result then
        Break;
    end;

    for i := 0 to _List.Count - 1 do
      Dispose(pWebSocketQueuedMessage(_List.Items[i]));

    _List.Free;
  end;
end;


function TLBWebSocketSession.PerformHandshake(InputHeaders: TStringList): Boolean;
var
  _RawKey, _EncodedKey, _RawProtocol: AnsiString;
  _Protocols: TStringArray;
  _ErrorOccurred: Boolean;
  _Base64 : TBase64Util = nil;
  _Digest: TSHA1Digest;

begin
  Result := False;
  _ErrorOccurred := False;

  try
    _RawKey := Trim(InputHeaders.Values[WS_HEADER_SEC_KEY]);
    if _RawKey <> '' then
    begin

      // Invio status line e header base
      FSocket.SendString(WS_BASE_HANDSHAKE_RESPONSE);

      if FSocket.LastError = 0 then
      begin
        _ErrorOccurred := False;

        // Subprotocol (se richiesto dal client)
        _RawProtocol := Trim(InputHeaders.Values[WS_HEADER_SEC_PROTOCOL]);
        if _RawProtocol <> '' then
        begin
          _Protocols := _RawProtocol.Split([',']);
          if Length(_Protocols) > 0 then
          begin
            FSocket.SendString(WS_HEADER_SEC_PROTOCOL + ': ' + Trim(_Protocols[0]) + CRLF);
            _ErrorOccurred := FSocket.LastError <> 0;
            if _ErrorOccurred then
              LBLogger.Write(1, 'TLBWebSocketSession.PerformHandshake', lmt_Warning, 'Error sending subprotocol <%s>: %s', [Trim(_Protocols[0]), FSocket.LastErrorDesc]);
          end;
        end;

        if not _ErrorOccurred then
        begin
          _RawKey := _RawKey + WS_GUID;
          _Digest := SHA1Buffer(_RawKey[1], Length(_RawKey));
          _Base64 := TBase64Util.Create;
          if _Base64.EncodeBuffer(@_Digest[0], Length(_Digest), _EncodedKey) then
          begin
            // Invio chiave accettazione handshake
            FSocket.SendString(WS_HEADER_SEC_ACCEPT + ': ' + _EncodedKey + CRLF + CRLF);
            Result := FSocket.LastError = 0;
            if not Result then
              LBLogger.Write(1, 'TLBWebSocketSession.PerformHandshake', lmt_Warning, 'Error sending <%s>: %s', [WS_HEADER_SEC_ACCEPT, FSocket.LastErrorDesc]);
          end;
        end;

      end
      else
        LBLogger.Write(1, 'TLBWebSocketSession.PerformHandshake', lmt_Warning, 'Error sending headers: %s', [FSocket.LastErrorDesc]);

    end
    else
      LBLogger.Write(1, 'TLBWebSocketSession.PerformHandshake', lmt_Warning, 'Sec-WebSocket-Key not found!');

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketSession.PerformHandshake', lmt_Error, E.Message);
  end;

  if _Base64 <> nil then
    _Base64.Free;
end;

function TLBWebSocketSession.ReadByte(out Value: Byte; Timeout: Integer): Boolean;
begin
  Value := 0;
  FSocket.RecvBufferEx(@Value, 1, Timeout);
  Result := FSocket.LastError = 0;
end;

function TLBWebSocketSession.ReceiveWebSocketMessage(out CloseRequested: Boolean): Boolean;
type
  TWSReadState = (wsrs_StartByte, wsrs_LenByte, wsrs_ExtendedLenWord, wsrs_ExtendedLen64,
                  wsrs_MaskKey, wsrs_Payload, wsrs_Done);

var
  State: TWSReadState;
  StartByte, LenByte: Byte;
  Masked: Boolean;
  MaskingKey: array[0..3] of Byte;
  PayloadLen: Int64;
  PayloadPart: TBytes;
  FinalFrame: Boolean;
  Opcode: TWebSocketDataType;
  i: Integer;

  _PayloadWordLen : Word;
  _CompletedData : TMemoryStream = nil;
  _Buffer : pByte = nil;
  _BufferLen : Int64 = 0;

begin
  Result := False;
  CloseRequested := False;
  State := wsrs_StartByte;

  try
    while (not FTerminate^) and (State <> wsrs_Done) do
    begin
      case State of

        wsrs_StartByte:
          begin
            FSocket.RecvBufferEx(@StartByte, SizeOf(StartByte), WS_TIMEOUT_READ_BYTE);
            if FSocket.LastError = 0 then
            begin
              FinalFrame := (StartByte and $80) <> 0;
              Opcode := TWebSocketDataType(StartByte and $0F);

              case OpCode of
                wsd_ConnectionClose:
                  begin
                    CloseRequested := True;
                    State := wsrs_Done;
                    Result := True;
                    LBLogger.Write(5, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Debug, 'Close frame received');
                  end;

                wsd_Ping, wsd_Pong:
                  begin
                    LBLogger.Write(5, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Debug, 'Control frame received: %s', [GetEnumName(TypeInfo(TWebSocketDataType), Ord(Opcode))]);
                    State := wsrs_LenByte; // prosegui per leggere e scartare
                  end;

                else
                  State := wsrs_LenByte;
              end;
            end
            else begin
              FConnectionError := FSocket.LastError <> WSAETIMEDOUT;
              Break;
            end;
          end;

        wsrs_LenByte:
          begin
            FSocket.RecvBufferEx(@LenByte, SizeOf(LenByte), WS_TIMEOUT_READ_BYTE);
            if FSocket.LastError = 0 then
            begin
              Masked := (LenByte and $80) > 0;
              PayloadLen := LenByte and $7F;

              case PayloadLen of
                WS_LEN_EXTENDED_16BIT : State := wsrs_ExtendedLenWord;
                WS_LEN_EXTENDED_64BIT : State := wsrs_ExtendedLen64;
                else                    State := wsrs_MaskKey;
              end;
            end
            else begin
              FConnectionError := True;
              LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error reading mask/len byte: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
              Break;
            end;
          end;

        wsrs_ExtendedLenWord:
          begin
            FSocket.RecvBufferEx(@_PayloadWordLen, SizeOf(_PayloadWordLen), WS_TIMEOUT_READ_BYTE);
            if FSocket.LastError = 0 then
            begin
              PayloadLen := BEtoN(_PayloadWordLen);
              State := wsrs_MaskKey;
            end
            else begin
              FConnectionError := True;
              LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error reading word for length: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
              Break;
            end;
          end;

        wsrs_ExtendedLen64:
          begin
            FSocket.RecvBufferEx(@PayloadLen, SizeOf(PayloadLen), WS_TIMEOUT_READ_BYTE);
            if FSocket.LastError = 0 then
            begin
              PayloadLen := BEtoN(PayloadLen);
              State := wsrs_MaskKey;
            end
            else begin
              FConnectionError := True;
              LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error reading Int64 for length: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
              Break;
            end;
          end;

        wsrs_MaskKey:
          begin
            if Masked then
            begin
              FSocket.RecvBufferEx(@MaskingKey[0], Length(MaskingKey), WS_TIMEOUT_READ_BYTE);
              if FSocket.LastError <> 0 then
              begin
                FConnectionError := True;
                LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error reading masking key: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
                Break;
              end;
            end;
            State := wsrs_Payload;
          end;

        wsrs_Payload:
          begin
            if PayloadLen > 0 then
            begin
              SetLength(PayloadPart, PayloadLen);
              FSocket.RecvBufferEx(@PayloadPart[0], PayloadLen, WS_TIMEOUT_READ_PAYLOAD);
              if FSocket.LastError = 0 then
              begin

                // Applica maschera se necessario
                if Masked then
                  for i := 0 to PayloadLen - 1 do
                    PayloadPart[i] := PayloadPart[i] xor MaskingKey[i mod 4];

                if FinalFrame then
                begin
                  if _CompletedData = nil then
                  begin
                    _Buffer := @PayloadPart[0];
                    _BufferLen := PayloadLen;
                  end
                  else begin
                    _CompletedData.Write(PayloadPart[0], PayloadLen);
                    _Buffer := pByte(_CompletedData.Memory);
                    _BufferLen := _CompletedData.Size;
                  end;
                  State := wsrs_Done
                end
                else begin
                  if _CompletedData = nil then
                    _CompletedData := TMemoryStream.Create;

                  _CompletedData.Write(PayloadPart[0], PayloadLen);
                end;
                State := wsrs_StartByte;
              end
              else begin
                FConnectionError := True;
                LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error reading payload: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
                Break;
              end;
            end
            else begin
              LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Warning, '<%s:%d>  -  Error: payload length = 0!', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort]);
              Break;
            end;
          end;
      end;
    end;

    if (_BufferLen > 0) and (_Buffer <> nil) and Assigned(FOnDataReceived) then
    begin
      FOnDataReceived(Self, Opcode, _Buffer, _BufferLen);
      FLastActivity := GetTickCount64;
      Result := True;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketSession.ReceiveWebSocketMessage', lmt_Error, E.Message);
  end;

  if _CompletedData <> nil then
    _CompletedData.Free;
end;

function TLBWebSocketSession.SendFrame(const Payload: TBytes; Opcode: TWebSocketDataType; Final: Boolean): Boolean;
var
  HeaderLen: Integer;
  PayloadLen: Int64;
begin
  Result := False;

  try
    PayloadLen := Length(Payload);

    // Byte 0: FIN + OPCODE
    FFrameHeader[0] := Ord(Opcode);
    if Final then
      FFrameHeader[0] := FFrameHeader[0] or WS_FLAG_FIN;

    // Byte 1+: lunghezza + eventuale estensione
    if PayloadLen < WS_LEN_EXTENDED_16BIT then
    begin
      FFrameHeader[1] := Byte(PayloadLen);
      HeaderLen := 2;
    end
    else if PayloadLen <= High(Word) then
    begin
      FFrameHeader[1] := WS_LEN_EXTENDED_16BIT;
      pWord(@FFrameHeader[2])^ := NtoBE(Word(PayloadLen));
      HeaderLen := 4;
    end
    else
    begin
      FFrameHeader[1] := WS_LEN_EXTENDED_64BIT;
      PInt64(@FFrameHeader[2])^ := NtoBE(PayloadLen);
      HeaderLen := 10;
    end;

    FSocket.SendBuffer(@FFrameHeader[0], HeaderLen);
    if FSocket.LastError = 0 then
    begin
      // Invio diretto: header + payload
      if PayloadLen > 0 then
      begin

        FSocket.SendBuffer(@Payload[0], PayloadLen);
        if FSocket.LastError = 0 then
        begin
          Result := True;
          FLastActivity := GetTickCount64;
        end
        else
          LBLogger.Write(1, 'TLBWebSocketSession.SendFrame', lmt_Warning, '<%s:%d>  -  Error sending frame payload: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);
      end;
    end
    else
      LBLogger.Write(1, 'TLBWebSocketSession.SendFrame', lmt_Warning, '<%s:%d>  -  Error sending frame header: %s', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort, FSocket.LastErrorDesc]);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketSession.SendFrame', lmt_Error, E.Message);
  end;
end;

procedure TLBWebSocketSession.CloseSession;
begin
  FSocket.SendString(WS_FRAME_CLOSE);
  FSocket.CloseSocket;
  LBLogger.Write(5, 'TLBWebSocketSession.CloseSession', lmt_Debug, 'WebSocket session closed for <%s:%d>', [FSocket.GetRemoteSinIP, FSocket.GetRemoteSinPort]);
end;

function TLBWebSocketSession.SendPing(const Payload: AnsiString): Boolean;
begin
  Result := Self.addMessageToSend(Payload, wsd_Ping, True);
end;

function TLBWebSocketSession.ExecuteSession(): Boolean;
var
  _CloseRequested: Boolean;
  _Now : QWord;

begin
  Result := False;

  try

    repeat
      if Self.SendWebSocketMessages() then
      begin

        Self.ReceiveWebSocketMessage(_CloseRequested);

        if (not _CloseRequested) and (not FConnectionError) then
        begin
          _Now := GetTickCount64;
          if (_Now - FLastActivity) > WS_PING_INTERVAL_DEFAULT then
            Self.SendPing(IntToStr(_Now));

          RTLEventWaitFor(FNewDataEvent, WS_SLEEP_INTERVAL);
        end
        else
          Break;

      end
      else
        Break;

    until FTerminate^  or (_Now - FLastActivity > WS_SESSION_TIMEOUT_DEFAULT);

    Result := True;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketSession.ExecuteSession', lmt_Error, E.Message);
  end;
end;

end.

