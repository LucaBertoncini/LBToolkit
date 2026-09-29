unit uLBWebSocketClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBBaseThread, Laz2_DOM, blcksock, uHTTPConsts,
  uRemoteConnectionData, uLBTimers, uTimedoutCriticalSection, contnrs,
  uLBCircularBuffer;

type
  TWebSocketState = (
    wsState_StartByte,
    wsState_MaskLenByte,
    wsState_PayloadLen16Bit,
    wsState_PayloadLen64Bit,
    wsState_MaskValue,
    wsState_Payload
  );

  TWebSocketFrameType = (
    wsFrame_Continuation = 0,
    wsFrame_Text         = 1,
    wsFrame_Binary       = 2,
    wsFrame_Reserved3    = 3,
    wsFrame_Reserved4    = 4,
    wsFrame_Reserved5    = 5,
    wsFrame_Reserved6    = 6,
    wsFrame_Reserved7    = 7,
    wsFrame_Close        = 8,
    wsFrame_Ping         = 9,
    wsFrame_Pong         = 10,
    wsFrame_Reserved11   = 11,
    wsFrame_Reserved12   = 12,
    wsFrame_Reserved13   = 13,
    wsFrame_Reserved14   = 14,
    wsFrame_Reserved15   = 15
  );

  TWebSocketCloseCode = (
    wsClose_Normal             = 1000,
    wsClose_GoingAway          = 1001,
    wsClose_ProtocolError      = 1002,
    wsClose_UnsupportedData    = 1003,
    wsClose_NoStatus           = 1005,
    wsClose_AbnormalClosure    = 1006,
    wsClose_InvalidFrameData   = 1007,
    wsClose_PolicyViolation    = 1008,
    wsClose_MessageTooBig      = 1009,
    wsClose_MandatoryExtension = 1010,
    wsClose_InternalError      = 1011,
    wsClose_ServiceRestart     = 1012,
    wsClose_TryAgainLater      = 1013,
    wsClose_TLSHandshake       = 1015
  );


  TWebSocketMessageCallback = procedure(Sender: TObject; isLastFrame: Boolean; aDataType: TWebSocketFrameType; aBuffer: pByte; aBufferLen: Int64) of object;

  // Callback alternativa per messaggi testuali
  TWebSocketTextCallback = procedure(Sender: TObject; const Message: AnsiString) of object;

  // Callback alternativa per messaggi binari
  TWebSocketBinaryCallback = procedure(Sender: TObject; aBuffer: pByte; aBufferLen: Int64) of object;

  { TLBWebSocketClient }

  TLBWebSocketClient = class(TLBBaseThread)
    strict private
      type
        TWebSocketClientState = (wscs_Unknown                     = 0,
                                 wscs_VerifyConnectionData        = 1,
                                 wscs_ConnectToServer             = 2,
                                 wscs_StartHandShake              = 3,
                                 wscs_WaitConnectionRequestAnswer = 4,
                                 wscs_ReadIncomingData            = 5,
                                 wscs_SendData                    = 6,
                                 wscs_RestartConnection           = 7,
                                 wscs_CloseSocket                 = 8,
                                 wscs_Suspend                     = 9);

      var
        FRemoteConnectionData : TRemoteConnectionData;
        FInternalState        : TWebSocketClientState;
        FSocket               : TTCPBlockSocket;
        FWebSocketKeyAnswer   : AnsiString;
        FHeartbeatTimer       : TTimeoutTimer;

        FPingTimer        : TTimeoutTimer;
        FPingInterval     : Int64;         // Intervallo tra ping (es. 30 secondi)
        FEnableAutoPing   : Boolean;       // Abilita/disabilita ping automatico

        FRestartConnection : Boolean;
        FDisconnect        : Boolean;

        { True quando DataReceived ha ricevuto un Ping dal server e ha accodato
          il Pong. Il loop di Execute lo controlla dopo ogni chiamata a
          DataReceived e transita immediatamente a wscs_SendData, senza
          aspettare il prossimo timeout di CanRead (cHeaderTimeout = 500 ms).
          Questo garantisce che il Pong venga spedito in pochi ms, evitando
          che il reader FXR90 chiuda la connessione per ping timeout. }
        FPongPending       : Boolean;

        FFragmentationActive : Boolean;         // True se è in corso un messaggio frammentato (FIN=0 su Text/Binary)
        FFragmentedDataType  : TWebSocketFrameType; // Tipo originale (Text/Binary) del messaggio frammentato in corso

        FCSOutputDataList  : TTimedOutCriticalSection;
        FOutputDataList    : TObjectList;

        FRequestedURI      : AnsiString;

        FWaitBeforeReconnect : Integer;
        FConnectionDataTimeout : Integer;

        // Buffer circolare usato come cache di ricezione: accumula in un'unica lettura
        // tutti i byte disponibili sul socket, così i frame WebSocket vengono poi
        // estratti dalla RAM invece di generare una syscall per ogni singolo campo
        // (header, lunghezza estesa, payload). Riduce drasticamente il numero di
        // accessi alla rete quando arrivano molti frame piccoli in sequenza.
        FIncomingBuffer    : TLBCircularBuffer;


        function get_isConnected: Boolean;
        function RemoteHostConnected(): Boolean;
        function SendHTTPConnectionRequest(): Boolean;
        function HandshakeAnswerReceived(out SocketError: Boolean): Boolean;
        function DataReceived(out SocketError: Boolean): Boolean;

        // Legge esattamente aCount byte, prelevandoli prima dal buffer circolare
        // (se già disponibili) e, solo se necessario, rifornendo il buffer con
        // un'unica chiamata di rete. Sostituisce le vecchie letture dirette dal
        // socket, campo per campo, con un accesso bufferizzato più efficiente.
        function ReadBufferedBytes(aDestination: Pointer; aCount: Int64; aTimeoutMS: Integer; out SocketError: Boolean): Boolean;

        function SendData(out SocketError: Boolean): Boolean;
        function SendPingMessage(aPayload: AnsiString = ''): Boolean;

        procedure SetPingInterval(const Value: Int64);


        procedure CloseConnection();


        const
          cHTTPAnswerTimeout = Int64(10000);
          cDefaultPingInterval = Int64(3000); // 3 secondi di default
          cIncomingBufferSize = Cardinal(64 * 1024); // 256 KB: dimensione del buffer di ricezione circolare
          cWaitBeforeReconnect = Integer(5000);
          cConnectionDataTimeout = Integer(20000);

    strict protected
      FOnDisconnected : TNotifyEvent;
      FOnConnected : TNotifyEvent;
      FOnWebSocketMessage : TWebSocketMessageCallback;
      FOnWebSocketTextMessage : TWebSocketTextCallback;
      FOnWebSocketBinaryMessage : TWebSocketBinaryCallback;

    protected
      procedure Execute; override;
      function ElaborateWebSocketMessage(isLastFrame: Boolean; aDataType: TWebSocketFrameType; aBuffer: pByte; aBufferLen: Int64): Boolean; virtual;

    public
      constructor Create(aBufferSize: Integer = cIncomingBufferSize; aWaitBeforeReconnect: Integer = cWaitBeforeReconnect; aReceiveDataTimeout: Integer = cConnectionDataTimeout); reintroduce;
      destructor Destroy(); override;

      function LoadConfiguration(aParentNode: TDOMNode): Boolean;
      function LoadConfigurationFromXMLFile(aFile: String): Boolean;

      function AddWebSocketMessageToSend(aBuffer: pByte; aBufferLen: Int64; aDataType: TWebSocketFrameType = wsFrame_Text; aPriority: Boolean = False): Boolean;
      function AddWebSocketMessageToSend(aBuffer: AnsiString; aDataType: TWebSocketFrameType = wsFrame_Text; aPriority: Boolean = False): Boolean;
      function ClearMessagesToSend(): Boolean;

      procedure Disconnect();
      procedure Reconnect();

      property RemoteConnectionData: TRemoteConnectionData read FRemoteConnectionData;
      property isConnected: Boolean read get_isConnected;
      property URI: AnsiString write FRequestedURI;

      property AutoPingEnabled: Boolean read FEnableAutoPing write FEnableAutoPing;
      property PingInterval: Int64 read FPingInterval write SetPingInterval;

      property OnConnected: TNotifyEvent write FOnConnected;
      property OnDisconnected: TNotifyEvent write FOnDisconnected;

      property OnWebSocketMessage: TWebSocketMessageCallback write FOnWebSocketMessage;
      property OnWebSocketTextMessage: TWebSocketTextCallback write FOnWebSocketTextMessage;
      property OnWebSocketBinaryMessage: TWebSocketBinaryCallback write FOnWebSocketBinaryMessage;

  end;

const
  // 🔹 WebSocket Limits
  WS_MAX_FRAME_SIZE              = Int64(16 * 1024 * 1024); // 16MB


implementation

uses
  ULBLogger, laz2_XMLRead, uBase64Util, synsock, sha1, ssl_openssl3, uLBUtils;

{ TLBWebSocketClient }

function TLBWebSocketClient.RemoteHostConnected(): Boolean;
begin
  Result := False;

  try

    Self.CloseConnection();

    FSocket := TTCPBlockSocket.Create;
    FSocket.Bind(cAnyHost, cAnyPort);
    if FSocket.LastError = 0 then
    begin
      FSocket.Connect(FRemoteConnectionData.Host, IntToStr(FRemoteConnectionData.Port));
      if FSocket.LastError = 0 then
      begin
        if FRemoteConnectionData.UseSSL then
        begin
          FSocket.SSL.SNIHost := FRemoteConnectionData.Host;
          FSocket.SSL.VerifyCert := FRemoteConnectionData.VerifyCert;
          FSocket.SSLDoConnect;
          FSocket.SSL.SNIHost := ''; //don't need it anymore and don't wan't to reuse it in next connection
          Result := FSocket.LastError = 0;
          if not Result then
            LBLogger.Write(1, 'TLBWebSocketClient.RemoteHostConnected', lmt_Warning, 'Error during SSL connection to host [%s:%d] : %s', [FRemoteConnectionData.Host, FRemoteConnectionData.Port, FSocket.LastErrorDesc]);
        end
        else
          Result := True;
      end
      else
        LBLogger.Write(6, 'TLBWebSocketClient.RemoteHostConnected', lmt_Warning, 'Error connecting host [%s:%d] : %s', [FRemoteConnectionData.Host, FRemoteConnectionData.Port, FSocket.LastErrorDesc]);
    end
    else
      LBLogger.Write(1, 'TLBWebSocketClient.RemoteHostConnected', lmt_Warning, 'Error binding socket: %s', [FSocket.LastErrorDesc]);


  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TLBWebSocketClient.RemoteHostConnected', lmt_Error, PChar(E.Message));
      Self.CloseConnection();
    end;
  end;
end;

function TLBWebSocketClient.get_isConnected: Boolean;
begin
  Result := FInternalState in [wscs_ReadIncomingData, wscs_SendData];
end;

function TLBWebSocketClient.SendHTTPConnectionRequest(): Boolean;
const
  cEndLine = AnsiString(#13#10);
  cRequestLineFormat = AnsiString('GET %s HTTP/1.1');
  cWebSocketKeyFormat = AnsiString('Sec-WebSocket-Key: %s');
  cHostFormat = AnsiString('Host: %s:%d');
  cOriginFormat = AnsiString('Origin: http%s://%s:%d'); // o https se SSL

var
  i : Integer;

  _Base64 : TBase64Util = nil;

  _OriginProtocol: AnsiString;

  _SHA1Key     : TSHA1Digest;
  _RandomBytes : array [0 .. 15] of Byte;

  _HTTPRequest : Array [0 .. 7] of AnsiString = ('',
                                                 '',
                                                 '',
                                                 '',
                                                 'Upgrade: websocket' + cEndLine,
                                                 'Connection: Upgrade' + cEndLine,
//                                                 'Sec-WebSocket-Protocol: chat' + cEndLine,
                                                 'Sec-WebSocket-Version: 13' + cEndLine,
                                                 cEndLine);

begin
  Result := False;

  try
    FWebSocketKeyAnswer := '';
    Randomize;

    _Base64 := TBase64Util.Create;

    for i := 0 To High(_RandomBytes) do
      _RandomBytes[i] := Random(256);

    if _Base64.EncodeBuffer(@_RandomBytes[0], Length(_RandomBytes), FWebSocketKeyAnswer) then
    begin
      if FRemoteConnectionData.UseSSL then
        _OriginProtocol := 's'
      else
        _OriginProtocol := '';

      _HTTPRequest[0] := Format(cRequestLineFormat, [FRequestedURI]) + cEndLine;
      _HTTPRequest[1] := Format(cWebSocketKeyFormat, [FWebSocketKeyAnswer]) + cEndLine;
      _HTTPRequest[2] := Format(cOriginFormat, [_OriginProtocol, FRemoteConnectionData.Host, FRemoteConnectionData.Port]) + cEndLine;
      _HTTPRequest[3] := Format(cHostFormat, [FRemoteConnectionData.Host, FRemoteConnectionData.Port]) + cEndLine;

      FWebSocketKeyAnswer += WS_GUID;
      _SHA1Key := SHA1Buffer(FWebSocketKeyAnswer[1], Length(FWebSocketKeyAnswer));
      if _Base64.EncodeBuffer(@_SHA1Key[0], Length(_SHA1Key), FWebSocketKeyAnswer) then
      begin
        Result := True;
        for i := 0 to High(_HTTPRequest) do
        begin
          FSocket.SendBuffer(@_HTTPRequest[i][1], Length(_HTTPRequest[i]));
          Result := FSocket.LastError = 0;
          if not Result then
          begin
            LBLogger.Write(1, 'TLBWebSocketClient.SendHTTPConnectionRequest', lmt_Warning, 'Error sending header <%s> (%d): <%s>', [_HTTPRequest[i], i, FSocket.LastErrorDesc]);
            Break;
          end;
        end;
      end
      else
        LBLogger.Write(1, 'TLBWebSocketClient.SendHTTPConnectionRequest', lmt_Warning, 'Error encoding websocket key answer!');
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketClient.SendHTTPConnectionRequest', lmt_Error, PChar(E.Message));
  end;

  if _Base64 <> nil then
    _Base64.Free;
end;

function TLBWebSocketClient.HandshakeAnswerReceived(out SocketError: Boolean): Boolean;
const
  cReceiveTimeout = Integer(1000);

var
  _IncomingHeaders : TStringList = nil;
  _FirstLine : Boolean = True;
  _s : AnsiString;

begin
  Result := False;
  SocketError := False;

  try
    // Leggo gli headers fino a ricevere una riga vuota
    _IncomingHeaders := TStringList.Create;
    _IncomingHeaders.NameValueSeparator := ':';

    while not Self.Terminated do
    begin
      _s := FSocket.RecvString(cReceiveTimeout);
      if FSocket.LastError = 0 then
      begin
        if _s = EmptyStr then
          Break
        else if _FirstLine then
        begin
          _FirstLine := False;
          if _s <> WS_HANDSHAKE_STATUS_LINE then
          begin
            LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Warning, 'Wrong server status line received: <%s>', [_s]);
            Break;
          end;
        end
        else
          _IncomingHeaders.Add(_s);
      end
      else begin
        SocketError := FSocket.LastError <> WSAETIMEDOUT;
        _IncomingHeaders.Clear;

        if SocketError then
          LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Warning, 'Error reading headers: %d  -  <%s>', [FSocket.LastError, FSocket.LastErrorDesc]);

        Break;
      end;
    end;

    if _IncomingHeaders.Count > 0 then
    begin
      if Trim(_IncomingHeaders.Values[HTTP_HEADER_UPGRADE]) = HTTP_UPGRADE_WEBSOCKET then
      begin
        if LowerCase(Trim(_IncomingHeaders.Values[HTTP_HEADER_CONNECTION])) = HTTP_CONNECTION_UPGRADE_LOWER then
        begin
          _s := Trim(_IncomingHeaders.Values[WS_HEADER_SEC_ACCEPT]);


          Result := _s = FWebSocketKeyAnswer;
          if not Result then
            LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Warning, 'Wrong accept key value: <%s>! Needed: <%s>', [_s, FWebSocketKeyAnswer]);

        end
        else
          LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Warning, 'Wrong <%s> value: <%s>', [HTTP_HEADER_CONNECTION, _IncomingHeaders.Values[HTTP_HEADER_CONNECTION]]);

      end
      else
        LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Warning, 'Wrong <%s> value! Value: <%s>', [HTTP_HEADER_UPGRADE, _IncomingHeaders.Values[HTTP_HEADER_UPGRADE]]);

    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketClient.HandshakeAnswerReceived', lmt_Error, PChar(E.Message));
  end;

  if _IncomingHeaders <> nil then
    _IncomingHeaders.Free;
end;

function TLBWebSocketClient.ReadBufferedBytes(aDestination: Pointer; aCount: Int64; aTimeoutMS: Integer; out SocketError: Boolean): Boolean;
var
  _StartTick    : QWord;      // Istante di partenza, per calcolare il timeout residuo
  _RemainingMS  : Int64;      // Millisecondi ancora disponibili prima di considerare l'operazione scaduta
  _BytesPending : Int64;      // Byte ancora da consegnare al chiamante
  _ChunkSize    : Cardinal;   // Quantità prelevabile dal buffer circolare in un colpo solo
  _Dest         : PByte;      // Puntatore corrente nel buffer di destinazione
  _Refilled     : Integer;    // Byte effettivamente travasati dall'ultimo rifornimento

begin
  Result := False;
  SocketError := False;

  if aCount <= 0 then
    Exit(True); // Nulla da leggere: richiesta banalmente soddisfatta

  _Dest := PByte(aDestination);
  _BytesPending := aCount;
  _StartTick := GetTickCount64;

  while (not Self.Terminated) and (_BytesPending > 0) do
  begin
    // 1. Preleva prima tutto ciò che è già presente nel buffer circolare:
    //    questo passo NON comporta alcun accesso alla rete.
    if FIncomingBuffer.AvailableForRead > 0 then
    begin
      _ChunkSize := FIncomingBuffer.AvailableForRead;
      if Int64(_ChunkSize) > _BytesPending then
        _ChunkSize := Cardinal(_BytesPending);

      if FIncomingBuffer.Read(_Dest, _ChunkSize) then
      begin
        Inc(_Dest, _ChunkSize);
        Dec(_BytesPending, _ChunkSize);
      end;
    end;

    if _BytesPending = 0 then
      Break; // Richiesta interamente soddisfatta dal buffer

    // 2. Tenta SEMPRE un rifornimento diretto, PRIMA di eventualmente attendere
    //    con CanRead. Fondamentale su socket SSL/TLS: il layer SSL può avere
    //    già byte decifrati e pronti in un proprio buffer interno anche quando
    //    il socket grezzo non segnala nulla di nuovo tramite select(); se ci si
    //    affida solo a CanRead, quei byte restano invisibili finché non arriva
    //    altro traffico TCP fresco, con il rischio di uno stallo silenzioso.
    //    WriteFromSocket non ha controindicazioni a essere chiamato "a vuoto":
    //    se non c'è nulla di pronto restituisce 0 senza alcun effetto collaterale.
    _Refilled := FIncomingBuffer.WriteFromSocket(FSocket);
    if _Refilled > 0 then
      Continue; // Trovato qualcosa: si riparte subito dal punto 1, senza attendere

    if (FSocket.LastError <> 0) and (FSocket.LastError <> WSAETIMEDOUT) then
    begin
      SocketError := True;
      LBLogger.Write(1, 'TLBWebSocketClient.ReadBufferedBytes', lmt_Warning,
        'Errore nel rifornire il buffer di ricezione: %d - %s', [FSocket.LastError, FSocket.LastErrorDesc]);
      Exit;
    end;

    // 3. Davvero nulla di pronto: calcola il tempo residuo e attende un evento di rete
    _RemainingMS := aTimeoutMS - Int64(GetTickCount64 - _StartTick);
    if _RemainingMS <= 0 then
    begin
      // Timeout raggiunto. Se non avevamo ancora consumato nulla per QUESTA
      // richiesta, è il caso normale ("nessun dato per ora": nessun errore,
      // decide il chiamante, es. nessun frame in arrivo). Ma se avevamo già
      // consumato una PARTE dei byte richiesti (es. metà di un payload) e ci
      // fermiamo qui, lo stream resta disallineato per sempre da questo punto
      // in poi: i byte successivi verrebbero interpretati come inizio di un
      // nuovo frame quando in realtà sono il resto di quello corrente. Questa
      // non è una condizione recuperabile: va segnalata come errore fatale,
      // così il chiamante forza una riconnessione pulita invece di continuare
      // a leggere da una posizione ormai sbagliata.
      if _BytesPending < aCount then
      begin
        SocketError := True;
        LBLogger.Write(1, 'TLBWebSocketClient.ReadBufferedBytes', lmt_Warning,
          'Lettura parziale andata in timeout (%d di %d byte ricevuti): stream disallineato, forzo la riconnessione',
          [aCount - _BytesPending, aCount]);
      end;
      Exit;
    end;

    if FSocket.CanRead(_RemainingMS) then
    begin
      if FSocket.LastError <> 0 then
      begin
        SocketError := FSocket.LastError <> WSAETIMEDOUT;
        if SocketError then
          LBLogger.Write(1, 'TLBWebSocketClient.ReadBufferedBytes', lmt_Warning,
            'Errore in attesa di dati sul socket: %d - %s', [FSocket.LastError, FSocket.LastErrorDesc]);
        Exit;
      end;
      // Torna in cima al ciclo: punto 1/2 troveranno ora qualcosa da leggere
    end
    else if FSocket.LastError <> 0 then
    begin
      SocketError := FSocket.LastError <> WSAETIMEDOUT;
      if SocketError then
        LBLogger.Write(1, 'TLBWebSocketClient.ReadBufferedBytes', lmt_Warning,
          'Errore sul socket: %d - %s', [FSocket.LastError, FSocket.LastErrorDesc]);
      Exit;
    end;
    // Se CanRead ritorna False senza errore, semplicemente non sono ancora
    // arrivati dati: si ritenta finché resta tempo utile nel timeout.
  end;

  Result := _BytesPending = 0;
end;

function TLBWebSocketClient.DataReceived(out SocketError: Boolean): Boolean;
const
cHeaderTimeout = Integer(500);
cPayloadTimeout = Integer(10000);
cMaxFrameSize = Int64(16 * 1024 * 1024);

var
_Header: array[0..1] of Byte;
_Buffer: TBytes;
_LastFrame: Boolean;
_DataType: TWebSocketFrameType;
_Length64: Int64;
_LengthWord: Word;
_LengthQWord: Int64;
_Len: Byte;

begin
Result := False;
SocketError := False;
SetLength(_Buffer, 0);

try
  if Self.Terminated then
    Exit;

  if not Self.ReadBufferedBytes(@_Header[0], SizeOf(_Header), cHeaderTimeout, SocketError) then
    Exit;

  _LastFrame := (_Header[0] and $80) = $80;
  _DataType := TWebSocketFrameType(_Header[0] and $0F);

  case _DataType of
    wsFrame_Close:
    begin
      FInternalState := wscs_RestartConnection;
      LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Debug, 'Close connection request received!');
      Result := True;
      Exit;
    end;

    wsFrame_Continuation:
    begin
      if FFragmentationActive then
      begin
        _DataType := FFragmentedDataType;
        if _LastFrame then
          FFragmentationActive := False;
      end
      else
      begin
        FInternalState := wscs_RestartConnection;
        LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning,
          'Continuation frame received without a preceding fragmented message!');
        Exit;
      end;
    end;

    wsFrame_Text, wsFrame_Binary:
    begin
      if _LastFrame then
        FFragmentationActive := False
      else
      begin
        FFragmentationActive := True;
        FFragmentedDataType := _DataType;
      end;
    end;

    wsFrame_Ping : LBLogger.Write(6, 'TLBWebSocketClient.DataReceived', lmt_Debug, 'Ping received');

    wsFrame_Pong:
    begin
    end;

  else
    FInternalState := wscs_RestartConnection;
    LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning, 'Wrong data type received: %d!', [Integer(_DataType)]);
    Exit;
  end;

  if (_Header[1] and $80) > 0 then
  begin
    // Un server conforme non maschera mai i propri frame: se il bit MASK è
    // impostato, o il server viola lo standard, o (più probabile con device
    // embedded) c'è un bug nella sua implementazione WebSocket lato server.
    // In ogni caso, a questo punto i 2 byte di header sono già stati
    // consumati dal buffer circolare ma il resto del frame (lunghezza,
    // eventuale mask, payload) NON è stato letto: continuare da qui
    // significa interpretare quei byte residui come l'inizio di un frame
    // nuovo, disallineando lo stream in modo permanente e silenzioso. Va
    // quindi trattato come un errore fatale, non come "nessun dato pronto".
    SocketError := True;
    LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning,
      'Received masked frame from server (protocol violation) - forcing reconnect to avoid stream desync');
    Exit;
  end;

  _Len := _Header[1] and $7F;
  case _Len of
    WS_LEN_EXTENDED_16BIT:
    begin
      if not Self.ReadBufferedBytes(@_LengthWord, SizeOf(_LengthWord), cHeaderTimeout, SocketError) then
        Exit;
      _Length64 := BEtoN(_LengthWord);
    end;

    WS_LEN_EXTENDED_64BIT:
    begin
      if not Self.ReadBufferedBytes(@_LengthQWord, SizeOf(_LengthQWord), cHeaderTimeout, SocketError) then
        Exit;
      _Length64 := BEtoN(_LengthQWord);
    end;

  else
    _Length64 := _Len;
  end;

  if _Length64 > 0 then
  begin
    if _Length64 <= cMaxFrameSize then
    begin
      SetLength(_Buffer, _Length64);
      // Nota: anche se il payload supera la dimensione del buffer circolare,
      // ReadBufferedBytes lo gestisce correttamente: rifornisce il buffer a
      // blocchi e lo svuota progressivamente nel buffer di destinazione.
      if not Self.ReadBufferedBytes(@_Buffer[0], _Length64, cPayloadTimeout, SocketError) then
        Exit;
    end
    else
    begin
      LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning, 'Frame size too large: %d', [_Length64]);
      SocketError := True;
      Exit;
    end;
  end;

  Result := True;

  if not Self.Terminated then
  begin
    case _DataType of
      wsFrame_Ping:
      begin
        if Length(_Buffer) > 0 then
        begin
          LBLogger.Write(6, 'TLBWebSocketClient.DataReceived', lmt_Debug, 'Sending PONG with buffer (%d)', [Length(_Buffer)]);
          if not Self.AddWebSocketMessageToSend(@_Buffer[0], Length(_Buffer), wsFrame_Pong, True) then
            LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning, 'Failed to send Pong response')
          else
            FPongPending := True;
        end
        else
        begin
          LBLogger.Write(6, 'TLBWebSocketClient.DataReceived', lmt_Debug, 'Sending PONG without buffer');
          if not Self.AddWebSocketMessageToSend(nil, 0, wsFrame_Pong, True) then
            LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Warning, 'Failed to send Pong response')
          else
            FPongPending := True;
        end;
      end;

      wsFrame_Pong:
        LBLogger.Write(6, 'TLBWebSocketClient.DataReceived', lmt_Debug, 'Pong confirmed - connection alive');

      wsFrame_Text, wsFrame_Binary:
      begin
        if Length(_Buffer) > 0 then
          Self.ElaborateWebSocketMessage(_LastFrame, _DataType, @_Buffer[0], Length(_Buffer))
        else if _LastFrame then
          Self.ElaborateWebSocketMessage(_LastFrame, _DataType, nil, 0);
      end;
    end;
  end;

except
  on E: Exception do
    LBLogger.Write(1, 'TLBWebSocketClient.DataReceived', lmt_Error, PChar(E.Message));
end;

SetLength(_Buffer, 0);
end;

function TLBWebSocketClient.SendData(out SocketError: Boolean): Boolean;
var
  _MemoryStream : TMemoryStream;
  _MessagesSent : Integer = 0;

begin
  Result := False;
  SocketError := False;

  try
    repeat
      _MemoryStream := nil;

      if FCSOutputDataList.Acquire('TLBWebSocketClient.SendData') then
      begin
        try
          if FOutputDataList.Count > 0 then
            _MemoryStream := TMemoryStream(FOutputDataList.Extract(FOutputDataList.Items[0]));
        except
          on E: Exception do
            LBLogger.Write(1, 'TLBWebSocketClient.SendData', lmt_Error, '1. %s', [E.Message]);
        end;
        FCSOutputDataList.Release();
      end;

      if _MemoryStream <> nil then
      begin
        FSocket.SendBuffer(_MemoryStream.Memory, _MemoryStream.Size);
        if FSocket.LastError = 0 then
        begin
          Inc(_MessagesSent);
          Result := True; // Almeno un messaggio inviato con successo
        end
        else begin
          SocketError := True;
          LBLogger.Write(1, 'TLBWebSocketClient.SendData', lmt_Warning, 'Error sending data <%s>: (%d) <%s>', [HexString(pByte(_MemoryStream.Memory), _MemoryStream.Size), FSocket.LastError, FSocket.LastErrorDesc]);
        end;
        _MemoryStream.Free;
      end
      else
        Break;

    until SocketError; // Continua finché non ci sono errori
  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketClient.SendData', lmt_Error, '2. %s', [E.Message]);
  end;

  if _MessagesSent > 0 then
    LBLogger.Write(6, 'TLBWebSocketClient.SendData', lmt_Debug, 'Sent %d messages', [_MessagesSent]);
end;


function TLBWebSocketClient.AddWebSocketMessageToSend(aBuffer: pByte; aBufferLen: Int64; aDataType: TWebSocketFrameType; aPriority: Boolean): Boolean;
var
  _OutputData : TMemoryStream = nil;
  _MaskValue : array [0..3] of Byte;
  i : Integer;
  _MaskedBuffer : TBytes;
  _FirstByte : Byte;
  _ByteTmp : Byte;
  _WordTmp : Word;
  _PayLoadLen : Int64;
begin
  Result := False;
  if Self.Terminated then Exit;
  if aBufferLen < 0 then Exit; // safety

  try
    _OutputData := TMemoryStream.Create;

    // Primo byte (FIN + opcode)
    case aDataType of
      wsFrame_Text   : _FirstByte := $81;
      wsFrame_Binary : _FirstByte := $82;
      wsFrame_Close  : _FirstByte := $88;
      wsFrame_Ping   : _FirstByte := $89;
      wsFrame_Pong   : _FirstByte := $8A;
      else
        LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Warning, 'Unsupported WebSocket frame type %d', [Integer(aDataType)]);
        Exit;
    end;
    _OutputData.Write(_FirstByte, 1);

    // Byte di lunghezza con bit di maschera sempre attivo (client → server)
    if aBufferLen > High(Word) then
    begin
      _ByteTmp := WS_LEN_EXTENDED_64BIT or $80;   // <-- MASK bit
      _OutputData.Write(_ByteTmp, 1);
      _PayLoadLen := NtoBE(aBufferLen);
      _OutputData.Write(_PayLoadLen, 8);
    end
    else if aBufferLen > 125 then
    begin
      _ByteTmp := WS_LEN_EXTENDED_16BIT or $80;   // <-- MASK bit
      _OutputData.Write(_ByteTmp, 1);
      _WordTmp := NtoBE(Word(aBufferLen));
      _OutputData.Write(_WordTmp, 2);
    end
    else
    begin
      _ByteTmp := (aBufferLen and $7F) or $80;
      _OutputData.Write(_ByteTmp, 1);
    end;

    // Maschera
    Randomize;
    for i := 0 to 3 do _MaskValue[i] := Random(256);
    _OutputData.Write(_MaskValue, 4);

    // Payload (solo se > 0)
    if aBufferLen > 0 then
    begin
      SetLength(_MaskedBuffer, aBufferLen);
      for i := 0 to aBufferLen - 1 do
        _MaskedBuffer[i] := aBuffer[i] xor _MaskValue[i mod 4];
      _OutputData.Write(_MaskedBuffer[0], aBufferLen);
    end;

    // Accoda il frame
    if FCSOutputDataList.Acquire('TLBWebSocketClient.AddWebSocketMessageToSend') then
    begin
      try
        if FOutputDataList <> nil then
        begin
          if aPriority then
            FOutputDataList.Insert(0, _OutputData)
          else
            FOutputDataList.Add(_OutputData);
          _OutputData := nil;
          Result := True;
        end
        else
          LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Warning, 'No output data list!');
      except
        on E: Exception do
          LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Error, 'Error inserting data into list: %s', [E.Message]);
      end;
      FCSOutputDataList.Release;
    end;
  finally
    if _OutputData <> nil then
      _OutputData.Free;
  end;
end;

(*
function TLBWebSocketClient.AddWebSocketMessageToSend(aBuffer: pByte; aBufferLen: Int64; aDataType: TWebSocketFrameType = wsFrame_Text; aPriority: Boolean = False): Boolean;
var
  _OutputData : TMemoryStream = nil;
  _MaskValue : array [0 .. 3] of Byte;
  i : Integer;
  _MaskedBuffer : array of Byte;
  _FirstByte : Byte = 0;

begin
  Result := False;

  if aBufferLen < 0 then Exit;


  if not Self.Terminated then
  begin

    if (aBuffer <> nil) and (aBufferLen > 0) then
    begin

      try
        _OutputData := TMemoryStream.Create();

        // Calcola il primo byte in base al tipo di frame
        case aDataType of
          wsFrame_Text   : _FirstByte := $81;           // FIN + Text
          wsFrame_Binary : _FirstByte := $82;         // FIN + Binary
          wsFrame_Close  : _FirstByte := $88; // FIN + Close
          wsFrame_Ping   : _FirstByte := $89;           // FIN + Ping
          wsFrame_Pong   : _FirstByte := $8A;           // FIN + Pong
          else
            LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Warning, 'Unsupported WebSocket frame type %d', [Integer(aDataType)]);
        end;

        if _FirstByte <> 0 then
        begin
          _OutputData.Write(_FirstByte, SizeOf(_FirstByte));

          if aBufferLen > High(Word) then
          begin
            _ByteTmp := WS_LEN_EXTENDED_64BIT;
            _OutputData.Write(_ByteTmp, SizeOf(_ByteTmp));
            _PayLoadLen := NtoBE(aBufferLen);
            _OutputData.Write(_PayLoadLen, SizeOf(_PayLoadLen));
          end
          else if aBufferLen > 125 then
          begin
            _ByteTmp := WS_LEN_EXTENDED_16BIT;
            _OutputData.Write(_ByteTmp, SizeOf(_ByteTmp));
            _WordTmp := NtoBE(Word(aBufferLen));
            _OutputData.Write(_WordTmp, SizeOf(_WordTmp));
          end
          else begin
            _ByteTmp := aBufferLen or $80;
            _OutputData.Write(_ByteTmp, SizeOf(_ByteTmp));
          end;

          Randomize;
          for i := 0 to High(_MaskValue) do
            _MaskValue[i] := Random(256);

          _OutputData.Write(_MaskValue, Length(_MaskValue));

          SetLength(_MaskedBuffer, aBufferLen);
          for i := 0 to aBufferLen - 1 do
            _MaskedBuffer[i] := aBuffer[i] xor _MaskValue[i mod 4];

          _OutputData.Write(_MaskedBuffer[0], Length(_MaskedBuffer));


          if FCSOutputDataList.Acquire('TLBWebSocketClient.AddWebSocketMessageToSend') then
          begin
            try
              if FOutputDataList <> nil then
              begin
                if aPriority then
                  FOutputDataList.Insert(0, _OutputData)
                else
                  FOutputDataList.Add(_OutputData);

                _OutputData := nil;
                Result := True;
              end
              else
                LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Warning, 'No output data list!');

            except
              on E: Exception do
                LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Error, 'Error inserting data into list: %s', [E.Message]);
            end;
            FCSOutputDataList.Release();
          end;
        end;

      except
        on E: Exception do
          LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Error, PChar(E.Message));
      end;

      if _OutputData <> nil then
        _OutputData.Free;

    end
    else
      LBLogger.Write(1, 'TLBWebSocketClient.AddWebSocketMessageToSend', lmt_Warning, 'No data to send!');

  end;
end;
*)

function TLBWebSocketClient.AddWebSocketMessageToSend(aBuffer: AnsiString; aDataType: TWebSocketFrameType = wsFrame_Text; aPriority: Boolean = False): Boolean;
begin
  if aBuffer <> EmptyStr then
    Result := Self.AddWebSocketMessageToSend(@aBuffer[1], Length(aBuffer), aDataType, aPriority)
  else
    Result := Self.AddWebSocketMessageToSend(nil, 0, aDataType, aPriority);
end;


function TLBWebSocketClient.ClearMessagesToSend(): Boolean;
begin
  Result := False;

  if (FCSOutputDataList.Acquire('TLBWebSocketClient.ClearMessagesToSend')) then
  begin
    try
      if FOutputDataList <> nil then
      begin
        FOutputDataList.Clear();
        Result := True;
      end
      else
        LBLogger.Write(1, 'TLBWebSocketClient.ClearMessagesToSend', lmt_Warning, 'No output data list!');

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBWebSocketClient.ClearMessagesToSend', lmt_Error, PChar(E.Message));
    end;
    FCSOutputDataList.Release();
  end;

end;

procedure TLBWebSocketClient.Disconnect();
begin
  LBLogger.Write(1, 'TLBWebSocketClient.Disconnect', lmt_Debug, 'Request for disconnection ...');
  FDisconnect := True;
end;

procedure TLBWebSocketClient.Reconnect();
begin
  FRestartConnection := True;
  LBLogger.Write(5, 'TLBWebSocketClient.Reconnect', lmt_Debug, 'Request for restart connection');
end;

procedure TLBWebSocketClient.CloseConnection();
const
  cCloseMessage : array [0 .. 1] of Byte = ($88, $00);

begin

  FFragmentationActive := False; // Nessun messaggio frammentato può sopravvivere alla chiusura della connessione

  // Scarta eventuali byte non ancora consumati: appartengono alla connessione
  // che sta per essere chiusa e non devono contaminare quella successiva
  if FIncomingBuffer <> nil then
    FIncomingBuffer.Clear;

  if FSocket <> nil then
  begin
    try
      FSocket.SendBuffer(@cCloseMessage[0], Length(cCloseMessage));
      FreeAndNil(FSocket);

      if (FOnDisconnected <> nil) then
        FOnDisconnected(Self);

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBWebSocketClient.CloseConnection', lmt_Error, PChar(E.Message));
    end;
  end;
end;

procedure TLBWebSocketClient.Execute;
var
  _SocketError : Boolean = False;

begin
  FInternalState := wscs_VerifyConnectionData;
  FRestartConnection := False;
  FDisconnect := False;

  while not Self.Terminated do
  begin

    if FDisconnect then
    begin
      FInternalState := wscs_Suspend;
      LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Debug, 'Suspending web-socket connection ...');
    end
    else if FRestartConnection then
      FInternalState := wscs_RestartConnection;

    case FInternalState of

      wscs_Unknown : FInternalState := wscs_VerifyConnectionData;

      wscs_VerifyConnectionData:
        begin
          Self.CloseConnection();

          if FRemoteConnectionData.hasValidData then
            FInternalState := wscs_ConnectToServer
          else begin
            LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Warning, 'No valid data for connection!');
            Self.PauseFor(FWaitBeforeReconnect);
          end;
        end;

      wscs_ConnectToServer:
        begin
          if Self.RemoteHostConnected() then
          begin
            FInternalState := wscs_StartHandShake;
            LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Remote host connected ... starting handshake');
          end
          else
            FInternalState := wscs_RestartConnection;
        end;

      wscs_StartHandShake:
        begin
          if Self.SendHTTPConnectionRequest() then
          begin
            FHeartbeatTimer.Reset(cHTTPAnswerTimeout);
            FInternalState := wscs_WaitConnectionRequestAnswer;
          end
          else
            FInternalState := wscs_RestartConnection;
        end;

      wscs_WaitConnectionRequestAnswer:
        begin
          if Self.HandshakeAnswerReceived(_SocketError) then
          begin
            FInternalState := wscs_ReadIncomingData;
            if Assigned(FOnConnected) then
              FOnConnected(Self);

            FHeartbeatTimer.Reset(FConnectionDataTimeout);

            if FEnableAutoPing then
              FPingTimer.Reset(FPingInterval);

            LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Connection done!');
          end
          else begin
            if _SocketError or FHeartbeatTimer.Expired() then
            begin
              FInternalState := wscs_RestartConnection;
              LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Warning, 'Error reading socket or handshake timeout reached!');
            end;
          end;
        end;

      wscs_ReadIncomingData:
        begin
          if Self.DataReceived(_SocketError) then
          begin
            FHeartbeatTimer.Reset(FConnectionDataTimeout);

            if FEnableAutoPing then
              FPingTimer.Reset(FPingInterval);
          end
          else begin
            if not _SocketError then
            begin
              if FEnableAutoPing and FPingTimer.Expired() then
              begin
                // LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Sending automatic ping - no data received for %d ms', [FPingInterval]);
                if Self.SendPingMessage() then
                begin
                  FPingTimer.Reset(FPingInterval);
                  // LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Automatic ping sent successfully');
                end
                else
                  LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Warning, 'Failed to send automatic ping');
              end;
              FInternalState := wscs_SendData;
            end
            else begin
              FInternalState := wscs_RestartConnection;
              LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Warning, 'Error reading data! Restarting connection');
            end;
          end;

          { Pong urgente: il server ha inviato un Ping e DataReceived ha
            accodato il Pong con aPriority=True. Transitiamo immediatamente
            a wscs_SendData senza aspettare il prossimo timeout di CanRead,
            così il Pong viene spedito in pochi millisecondi invece di
            aspettare fino a cHeaderTimeout (500 ms) o più. }
          if FPongPending then
          begin
            FPongPending   := False;
            FInternalState := wscs_SendData;
          end;
        end;

      wscs_SendData:
        begin
          if Self.SendData(_SocketError) then
          begin
            FHeartbeatTimer.Reset(FConnectionDataTimeout);
            FInternalState := wscs_ReadIncomingData; // Ora tutti i messaggi sono stati inviati
          end
          else begin
            if _SocketError then
            begin
              FInternalState := wscs_RestartConnection;
              LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Warning, 'Error sending data! Restarting connection!');
            end
            else begin
              if FHeartbeatTimer.Expired() then
              begin
                FInternalState := wscs_RestartConnection;
                LBLogger.Write(1, 'TLBWebSocketClient.Execute', lmt_Warning, 'Timeout! Restarting connection');
              end
              else
                FInternalState := wscs_ReadIncomingData;
            end;
          end;
        end;

      wscs_RestartConnection:
        begin
          if FRestartConnection then
          begin
            LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Restarting connection ...');
            FRestartConnection := False;
          end;
          Self.CloseConnection();
          FInternalState := wscs_VerifyConnectionData;
          Self.PauseFor(FWaitBeforeReconnect);
        end;

      wscs_Suspend:
        begin
          if FDisconnect then
          begin
            LBLogger.Write(5, 'TLBWebSocketClient.Execute', lmt_Debug, 'Connection suspended!');
            FDisconnect := False;
          end;

          Self.CloseConnection();
          Self.PauseFor(FWaitBeforeReconnect);
        end;
    end;
  end;

  if Self.isConnected then
  begin
    Self.SendData(_SocketError); // Invia tutti i messaggi prima di chiudere
    Self.CloseConnection();
  end;
end;

(*
function TLBWebSocketClient.SendPongResponse(aPayload: pByte; aPayloadLen: Int64): Boolean;
begin
  // Usa il metodo generalizzato con priorità alta
  Result := Self.AddWebSocketMessageToSend(aPayload, aPayloadLen, wsd_Pong, True);

  if Result then
    LBLogger.Write(5, 'TLBWebSocketClient.SendPongResponse', lmt_Debug, 'Pong response queued successfully')
  else
    LBLogger.Write(1, 'TLBWebSocketClient.SendPongResponse', lmt_Warning, 'Failed to queue Pong response');
end;
*)

function TLBWebSocketClient.SendPingMessage(aPayload: AnsiString = ''): Boolean;
begin
  Result := Self.AddWebSocketMessageToSend(aPayload, wsFrame_Ping, False);
(*
  if Result then
    LBLogger.Write(5, 'TLBWebSocketClient.SendPingMessage', lmt_Debug, 'Ping message queued successfully')
  else
    LBLogger.Write(1, 'TLBWebSocketClient.SendPingMessage', lmt_Warning, 'Failed to queue Ping message');
*)
end;


function TLBWebSocketClient.ElaborateWebSocketMessage(isLastFrame: Boolean; aDataType: TWebSocketFrameType; aBuffer: pByte; aBufferLen: Int64): Boolean;
var
  _MessageStr: AnsiString;
begin
  Result := True;

  try

    // Priorità a callback per messaggi raw
    if Assigned(FOnWebSocketMessage) then
      FOnWebSocketMessage(Self, isLastFrame, aDataType, aBuffer, aBufferLen)
    else if Assigned(FOnWebSocketTextMessage) and (aDataType = wsFrame_Text) then  // Se c'è una callback per messaggi testuali
    begin
      SetLength(_MessageStr, aBufferLen);
      if aBufferLen > 0 then
        Move(aBuffer^, _MessageStr[1], aBufferLen);

      FOnWebSocketTextMessage(Self, _MessageStr);
    end
    else if Assigned(FOnWebSocketBinaryMessage) and (aDataType = wsFrame_Binary) then
      FOnWebSocketBinaryMessage(Self, aBuffer, aBufferLen);

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TLBWebSocketClient.ElaborateWebSocketMessage', lmt_Error, 'Error in message callback: %s', [E.Message]);
      Result := False;
    end;
  end;
end;

constructor TLBWebSocketClient.Create(aBufferSize: Integer; aWaitBeforeReconnect: Integer; aReceiveDataTimeout: Integer);
begin
  inherited Create();

  FOnConnected := nil;
  FOnDisconnected := nil;

  FRequestedURI := '/';

  FRemoteConnectionData := TRemoteConnectionData.Create;

  FHeartbeatTimer := TTimeoutTimer.Create(cHTTPAnswerTimeout);

  FPingInterval   := cDefaultPingInterval;
  FEnableAutoPing := True;
  FPingTimer      := TTimeoutTimer.Create(FPingInterval);
  FPongPending    := False;

  FRestartConnection := False;
  FDisconnect := False;

  FFragmentationActive := False;
  FFragmentedDataType := wsFrame_Text; // valore neutro, valido solo quando FFragmentationActive = True

  FCSOutputDataList  := TTimedOutCriticalSection.Create();
  FOutputDataList    := TObjectList.Create(True);

  FWaitBeforeReconnect := aWaitBeforeReconnect;
  FConnectionDataTimeout := aReceiveDataTimeout;


  // Buffer di ricezione: creato una sola volta e riutilizzato per tutta la vita
  // dell'oggetto, così da non pagare il costo di allocazione ad ogni riconnessione
  FIncomingBuffer := TLBCircularBuffer.Create(aBufferSize);

  InitOpenSSL3;
end;

destructor TLBWebSocketClient.Destroy;
begin
  inherited Destroy;

  try


    if FCSOutputDataList.Acquire('TLBWebSocketClient.Destroy') then
    begin
      try
        FreeAndNil(FOutputDataList);
      except
        on E1: Exception do
          LBLogger.Write(1, 'TLBWebSocketClient.Destroy', lmt_Error, '2. %s', [E1.Message]);
      end;
      FCSOutputDataList.Release();
    end;

    FreeAndNil(FCSOutputDataList);

    FreeAndNil(FHeartbeatTimer);
    FreeAndNil(FPingTimer);

    FreeAndNil(FRemoteConnectionData);
    FreeAndNil(FIncomingBuffer);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebSocketClient.Destroy', lmt_Error, '3. %s', [E.Message]);
  end;
end;

function TLBWebSocketClient.LoadConfiguration(aParentNode: TDOMNode): Boolean;
var
  _Node : TDOMNode;

const
  cRequestedURI = DOMString('URI');

begin
  Result := False;

  if aParentNode <> nil then
  begin

    if FRemoteConnectionData.LoadFromXMLNode(aParentNode) then
    begin
      Result := FRemoteConnectionData.hasValidData;
      if Result then
      begin
        _Node := aParentNode.FindNode(cRequestedURI);
        if _Node <> nil then
          FRequestedURI := Trim(_Node.TextContent);
      end;
    end
    else
      LBLogger.Write(1, 'TLBWebSocketClient.LoadConfiguration', lmt_Warning, 'Error loading remote connection data!');

  end
  else
    LBLogger.Write(1, 'TLBWebSocketClient.LoadConfigurationFromXMLFile', lmt_Warning, 'No parent node set!');

end;

function TLBWebSocketClient.LoadConfigurationFromXMLFile(aFile: String): Boolean;
var
  _Doc : TXMLDocument = nil;

begin
  Result := False;

  if (aFile <> EmptyStr) and FileExists(aFile) then
  begin

    try

      ReadXMLFile(_Doc, aFile);
      if _Doc <> nil then
        Result := Self.LoadConfiguration(_Doc.DocumentElement)
      else
        LBLogger.Write(1, 'TLBWebSocketClient.LoadConfigurationFromXMLFile', lmt_Warning, 'Error reading file <%s>!', [aFile]);


    except
      on E: Exception do
        LBLogger.Write(1, 'TLBWebSocketClient.LoadConfigurationFromXMLFile', lmt_Error, PChar(E.Message));
    end;

    if _Doc <> nil then
      _Doc.Free;

  end
  else
    LBLogger.Write(1, 'TLBWebSocketClient.LoadConfigurationFromXMLFile', lmt_Warning, 'Configuration file <%s> not found!', [aFile]);

end;

procedure TLBWebSocketClient.SetPingInterval(const Value: Int64);
begin
  FPingInterval := Value;
  if FPingTimer <> nil then
    FPingTimer.Reset(FPingInterval);
end;

end.
