unit RTCMClient;

{
  Client TCP per la ricezione di correzioni RTCM3 da un Caster NTRIP.
  Supporta NTRIP v1 e v2 con rilevamento automatico della versione.
  Gira su thread dedicato. Gestisce riconnessione automatica.

  ── Protocollo NTRIP ────────────────────────────────────────────────────
  NTRIP v1 (2004):
    Richiesta: GET /mountpoint HTTP/1.0 + headers minimali
    Risposta:  "ICY 200 OK" → stream RTCM3 grezzo senza framing

  NTRIP v2 (2009):
    Richiesta: GET /mountpoint HTTP/1.1 + Ntrip-Version: Ntrip/2.0
    Risposta:  "HTTP/1.1 200 OK" + Transfer-Encoding: chunked
    Dati:      <hex_size>\r\n<bytes RTCM3>\r\n ... 0\r\n\r\n

  ── Rilevamento automatico (nvAuto, default) ─────────────────────────────
  Tenta v2. Se il server risponde "ICY" (tipico v1) si riconnette
  immediatamente usando v1. La versione rilevata è in DetectedVersion.

  ── GGA position (per caster VRS) ────────────────────────────────────────
  Alcuni caster richiedono l'invio periodico della posizione NMEA GGA
  per selezionare la stazione virtuale più vicina.
  Imposta GGASentence (usa BuildGGASentence()) e GGAIntervalSec.

  ── Thread-safety di OnCorrections ──────────────────────────────────────
  OnCorrections viene chiamato DIRETTAMENTE dal thread TCP (senza
  Synchronize) per minimizzare la latenza. Il gestore tipico chiama
  PushCorrections() su TCorrectionBuffer (già thread-safe).
}

interface

uses
  SysUtils, Classes, BlckSock, Math, DateUtils, uLBBaseThread, uLBWebSocketClient;

type

  TNTRIPVersion = (nvAuto, nvV1, nvV2);
  TRTCMSourceType = (stNTRIP, stIPDirect, stWebSocket);

  TOnCorrectionsReceived = procedure(Sender: TObject; const Data: TBytes) of object;

  TRTCMClient = class;

  { ── Thread di ricezione ─────────────────────────────────────────────── }

  { TRTCMClientThread }

  TRTCMClientThread = class(TLBBaseThread)
  private
    FClient          : TRTCMClient;
    FSock            : TTCPBlockSocket;
    FTmpData         : TBytes;
    FDetectedVersion : TNTRIPVersion;
    FLastGGASent     : TDateTime;
    FSourceType      : TRTCMSourceType;

    procedure FireCorrections;

    function  Base64Encode(const S: string): string;
    function  ReadLine(TimeoutMs: Integer): string;

    function  DoConnect: Boolean;
    function  SendRequest(Version: TNTRIPVersion): Boolean;

    procedure SendGGAIfDue;
    procedure ReceiveLoopRaw;
    procedure ReceiveLoopChunked;

  protected
    procedure Execute; override;

  public
    constructor Create(AClient: TRTCMClient); reintroduce;
    destructor  Destroy; override;

    property DetectedVersion: TNTRIPVersion read FDetectedVersion;
    property SourceType: TRTCMSourceType write FSourceType;
  end;

  { ── Classe pubblica ─────────────────────────────────────────────────── }

  { TRTCMClient }

  TRTCMClient = class(TObject)
  strict private
    FThread          : TRTCMClientThread;
    FWebSocketClient : TLBWebSocketClient;

    FWebSocketURI    : String;
    FHost            : string;
    FPort            : Integer;
    FMountpoint      : string;
    FUsername        : string;
    FPassword        : string;
    FActive          : Boolean;
    FReconnectDelay  : Integer;
    FNTRIPVersion    : TNTRIPVersion;
    FGGASentence     : string;
    FGGAIntervalSec  : Integer;
    FOnCorrections   : TOnCorrectionsReceived;
    FSourceType      : TRTCMSourceType;

    function GetDetectedVersion: TNTRIPVersion;

    procedure OnWebSocketData(Sender: TObject; aBuffer: pByte; aBufferLen: Int64);

  public
    constructor Create(aSourceType: TRTCMSourceType; aHost: string; APort: Integer = 2101);
    destructor  Destroy; override;

    function Open(): Boolean;
    procedure Close;

    { Aggiorna la stringa GGA inviata al caster (thread-safe). }
    procedure UpdateGGA(const AGGASentence: string);

    property Active          : Boolean         read FActive;
    property Host            : string          read FHost           write FHost;
    property Port            : Integer         read FPort           write FPort;

    { Mountpoint NTRIP obbligatorio (es. 'VEN3', 'MILA1'). }
    property Mountpoint      : string          read FMountpoint     write FMountpoint;
    property Username        : string          read FUsername       write FUsername;
    property Password        : string          read FPassword       write FPassword;

    property WebSocketURI    : string          read FWebSocketURI   write FWebSocketURI;

    { nvAuto (default): tenta v2, ricade su v1 se il server risponde ICY.
      nvV1 / nvV2: forza la versione specificata. }
    property NTRIPVersion    : TNTRIPVersion   read FNTRIPVersion   write FNTRIPVersion;

    { Versione effettivamente negoziata (valida dopo Open). }
    property DetectedVersion : TNTRIPVersion   read GetDetectedVersion;

    property ReconnectDelay  : Integer         read FReconnectDelay write FReconnectDelay;

    { Stringa GGA NMEA da inviare periodicamente al caster (per VRS).
      Usa BuildGGASentence() per costruirla a partire da lat/lon/alt.
      Lasciare vuoto per non inviare. }
    property GGASentence     : string          read FGGASentence    write FGGASentence;

    { Intervallo invio GGA in secondi (0 = disabilitato).
      I caster VRS tipicamente richiedono ogni 5–30 s. }
    property GGAIntervalSec  : Integer         read FGGAIntervalSec write FGGAIntervalSec;

    property SourceType: TRTCMSourceType       read FSourceType write FSourceType;

    property OnCorrections   : TOnCorrectionsReceived read FOnCorrections  write FOnCorrections;
  end;

{ ── Costruisce una stringa GGA NMEA con checksum ─────────────────────────── }
{ Utile per impostare GGASentence quando si conosce la posizione.              }
function BuildGGASentence(Lat, Lon, AltMSL: Double; NumSV: Integer = 8): string;

implementation

uses
  ULBLogger, StrUtils;

const
  RECV_TIMEOUT_MS  = 3000;
  RECV_BUF_SIZE    = 8192;
  USER_AGENT       = 'NTRIP PascalRTK/2.0';

{ ════════════════════════════════════════════════════════════════════════════
  BuildGGASentence
  ════════════════════════════════════════════════════════════════════════════ }

function NMEAChecksum(const Body: string): string;
var
  i, CS: Integer;
begin
  CS := 0;
  for i := 1 to Length(Body) do
    CS := CS xor Ord(Body[i]);
  Result := IntToHex(CS, 2);
end;

function BuildGGASentence(Lat, Lon, AltMSL: Double; NumSV: Integer): string;
var
  FS               : TFormatSettings;
  LatDeg, LonDeg   : Integer;
  LatMin, LonMin   : Double;
  LatH, LonH       : Char;
  TimeStr, Body    : string;
begin
  FS := TFormatSettings.Create('en-US');

  TimeStr := FormatDateTime('hhnnss.zzz',
               TTimeZone.Local.ToUniversalTime(Now), FS);

  if Lat < 0 then begin LatH := 'S'; Lat := -Lat; end else LatH := 'N';
  if Lon < 0 then begin LonH := 'W'; Lon := -Lon; end else LonH := 'E';

  LatDeg := Trunc(Lat); LatMin := (Lat - LatDeg) * 60.0;
  LonDeg := Trunc(Lon); LonMin := (Lon - LonDeg) * 60.0;

  Body := Format(
    'GPGGA,%s,%02d%08.5f,%s,%03d%08.5f,%s,1,%02d,1.0,%.3f,M,0.0,M,,',
    [TimeStr,
     LatDeg, LatMin, LatH,
     LonDeg, LonMin, LonH,
     NumSV, AltMSL], FS);

  Result := '$' + Body + '*' + NMEAChecksum(Body) + #13#10;
end;

{ ══ TRTCMClientThread ════════════════════════════════════════════════════════ }

constructor TRTCMClientThread.Create(AClient: TRTCMClient);
begin
  inherited Create();

  FreeOnTerminate  := False;
  FClient          := AClient;
  FDetectedVersion := nvV1;
  FLastGGASent     := 0;
  FSock            := TTCPBlockSocket.Create;
end;

destructor TRTCMClientThread.Destroy;
begin
  FSock.CloseSocket;
  FSock.Free;
  inherited;
end;

procedure TRTCMClientThread.FireCorrections;
begin
  if Assigned(FClient.OnCorrections) then
    FClient.OnCorrections(FClient, FTmpData);
end;

function TRTCMClientThread.Base64Encode(const S: string): string;
const
  Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, Pad : Integer;
  B      : TBytes;
  Len    : Integer;
  b0, b1, b2 : Byte;

begin
  Result := '';
  Len := Length(S);
  SetLength(B, Len);
  for i := 0 to Len - 1 do
    B[i] := Ord(S[i + 1]);

  i := 0;

  while i < Len do
  begin
    b0 := B[i];
    b1 := 0; if i + 1 < Len then b1 := B[i + 1];
    b2 := 0; if i + 2 < Len then b2 := B[i + 2];
    Result := Result
      + Chars[(b0 shr 2) + 1]
      + Chars[((b0 and 3) shl 4 or (b1 shr 4)) + 1]
      + Chars[((b1 and $0F) shl 2 or (b2 shr 6)) + 1]
      + Chars[(b2 and $3F) + 1];
    Inc(i, 3);
  end;

  Pad := (3 - (Len mod 3)) mod 3;

  for i := 1 to Pad do
    Result[Length(Result) - i + 1] := '=';
end;

function TRTCMClientThread.ReadLine(TimeoutMs: Integer): string;
var
  B: Byte;
begin
  Result := '';
  repeat
    B := FSock.RecvByte(TimeoutMs);
    if FSock.LastError = 0 then
    begin
      if Chr(B) = #10 then
        Break;

      if Chr(B) <> #13 then Result := Result + Chr(B);
    end
    else begin
      LBLogger.Write(1, 'TRTCMClientThread.ReadLine', lmt_Warning, 'Error reading data: <%s>', [FSock.LastErrorDesc]);
      Break;
    end;
  until False;
end;

function TRTCMClientThread.DoConnect: Boolean;
begin
  Result := False;
  FSock.CloseSocket;
  FSock.Connect(FClient.Host, IntToStr(FClient.Port));
  if FSock.LastError <> 0 then
    LBLogger.Write(1, 'TRTCMClientThread.DoConnect', lmt_Warning, 'Connection faile with %s:%d — <%s>', [FClient.Host, FClient.Port, FSock.LastErrorDesc])
  else
    Result := True;
end;

function TRTCMClientThread.SendRequest(Version: TNTRIPVersion): Boolean;
var
  Auth, Req: string;
begin
  if FClient.Username <> '' then
    Auth := 'Authorization: Basic ' +
            Base64Encode(FClient.Username + ':' + FClient.Password) + #13#10
  else
    Auth := '';

  case Version of
    nvV2:
      Req :=
        'GET /' + FClient.Mountpoint + ' HTTP/1.1'    + #13#10 +
        'Host: ' + FClient.Host                        + #13#10 +
        'Ntrip-Version: Ntrip/2.0'                    + #13#10 +
        'User-Agent: ' + USER_AGENT                   + #13#10 +
        'Connection: close'                            + #13#10 +
        Auth + #13#10;
  else
    Req :=
      'GET /' + FClient.Mountpoint + ' HTTP/1.0'      + #13#10 +
      'User-Agent: ' + USER_AGENT                     + #13#10 +
      Auth + #13#10;
  end;

  FSock.SendString(Req);
  Result := FSock.LastError = 0;

  if not Result then
    LBLogger.Write(1, 'TRTCMClientThread.SendRequest', lmt_Warning, 'Error sending request: <%s>', [FSock.LastErrorDesc]);
end;

procedure TRTCMClientThread.SendGGAIfDue;
var
  GGA: string;
begin

  if FClient.GGAIntervalSec <= 0 then Exit;

  GGA := FClient.GGASentence;
  if GGA <> '' then
  begin
    if (FLastGGASent = 0) or
       (SecondsBetween(Now, FLastGGASent) >= FClient.GGAIntervalSec) then
    begin
      FSock.SendString(GGA);
      FLastGGASent := Now;
    end;
  end;
end;

{ ── Loop v1: stream grezzo ── }

procedure TRTCMClientThread.ReceiveLoopRaw;
var
  Buf   : array[0..RECV_BUF_SIZE - 1] of Byte;
  NRead : Integer;
begin
  while not Terminated do
  begin
    if FSourceType = stNTRIP then
      Self.SendGGAIfDue;

    NRead := FSock.RecvBufferEx(@Buf, SizeOf(Buf), RECV_TIMEOUT_MS);
    if Terminated then Break;
    if NRead > 0 then
    begin
      SetLength(FTmpData, NRead);
      Move(Buf[0], FTmpData[0], NRead);
      // FTmpLen := NRead;
      Self.FireCorrections;
    end
    else if FSock.LastError <> 0 then
    begin
      LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopRaw', lmt_Warning, 'Connection error: <%s>', [FSock.LastErrorDesc]);
      Break;
    end;
  end;
end;

{ ── Loop v2: chunked transfer ── }

procedure TRTCMClientThread.ReceiveLoopChunked;
var
  SizeLine  : string;
  ChunkSize : Integer;
  Buf       : array[0..RECV_BUF_SIZE - 1] of Byte;
  NRead     : Integer;
  ToRead    : Integer;
  OldLen    : Integer;

  _SemiPos : Integer;

begin
  while not Terminated do
  begin
    Self.SendGGAIfDue;

    SizeLine := ReadLine(RECV_TIMEOUT_MS);

    if Terminated then Break;
    if FSock.LastError <> 0 then
    begin
      LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopChunked', lmt_Warning, 'Connection error: <%s>', [FSock.LastErrorDesc]);
      Break;
    end;

    SizeLine := Trim(SizeLine);
    if SizeLine = '' then Continue;
    if SizeLine = '0' then Break;  // chunk finale

    { Taglia eventuali chunk-extensions (es. "1A;ext=val") }
    _SemiPos := Pos(';', SizeLine);
    if _SemiPos > 0 then SizeLine := Copy(SizeLine, 1, _SemiPos - 1);

    ChunkSize := StrToIntDef('$' + Trim(SizeLine), 0);
    if ChunkSize <= 0 then Continue;

    { Leggi esattamente ChunkSize byte, eventualmente in più read }
    SetLength(FTmpData, 0);
    ToRead := ChunkSize;
    while (ToRead > 0) and not Terminated do
    begin
      NRead := FSock.RecvBufferEx(@Buf,
                                  Min(ToRead, SizeOf(Buf)),
                                  RECV_TIMEOUT_MS);
      if Terminated then Break;
      if NRead > 0 then
      begin
        OldLen := Length(FTmpData);
        SetLength(FTmpData, OldLen + NRead);
        Move(Buf[0], FTmpData[OldLen], NRead);
        Dec(ToRead, NRead);
      end
      else if FSock.LastError <> 0 then
      begin
        LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopChunked', lmt_Warning, 'Error reading data: <%s>', [FSock.LastErrorDesc]);
        ToRead := 0;
        Break;
      end;
    end;

    if Length(FTmpData) > 0 then
      Self.FireCorrections;

    Self.ReadLine(500);  // consuma \r\n finale del chunk
  end;
end;

{ ── Execute ── }

procedure TRTCMClientThread.Execute;
var
  TryVer    : TNTRIPVersion;
  FirstLine : string;
  IsICY     : Boolean;
  IsChunked : Boolean;
  Line      : string;

begin
  while not Terminated do
  begin
    { ── 1. Connessione TCP ─────────────────────────────────────────────── }
    if not Self.DoConnect then
    begin
      Self.PauseFor(FClient.ReconnectDelay);
      Continue;
    end;

    case FSourceType of
      stIPDirect: Self.ReceiveLoopRaw;

      stNTRIP:
        begin
          { ── 2. Versione da tentare ─────────────────────────────────────────── }
          TryVer := FClient.NTRIPVersion;
          if TryVer = nvAuto then TryVer := nvV2;

          { ── 3. Invia richiesta NTRIP ───────────────────────────────────────── }
          if not Self.SendRequest(TryVer) then
          begin
            FSock.CloseSocket;
            Self.PauseFor(FClient.ReconnectDelay);
            Continue;
          end;

          { ── 4. Leggi prima riga risposta ───────────────────────────────────── }
          FirstLine := Self.ReadLine(3000);
          if FSock.LastError <> 0 then
          begin
            FSock.CloseSocket;
            Self.PauseFor(FClient.ReconnectDelay);
            Continue;
          end;

          IsICY := Pos('ICY', FirstLine) > 0;

          { ── 5. Auto-detection: v2 tentato ma server risponde ICY → riprova v1 }
          if (TryVer = nvV2) and IsICY then
          begin
            LBLogger.Write(1, 'TRTCMClientThread.InternalExecute', lmt_Debug, 'Server is NTRIP v1');
            FSock.CloseSocket;
            if not DoConnect then
            begin
              Self.PauseFor(FClient.ReconnectDelay);
              Continue;
            end;
            if not SendRequest(nvV1) then
            begin
              FSock.CloseSocket;
              Self.PauseFor(FClient.ReconnectDelay);
              Continue;
            end;

            FirstLine := Self.ReadLine(3000);
            if FSock.LastError <> 0 then
            begin
              FSock.CloseSocket;
              Self.PauseFor(FClient.ReconnectDelay);
              Continue;
            end;
            IsICY  := Pos('ICY', FirstLine) > 0;
            TryVer := nvV1;
          end;

          { ── 6. Verifica 200 ────────────────────────────────────────────────── }
          if Pos('200', FirstLine) = 0 then
          begin
            LBLogger.Write(1, 'TRTCMClientThread.InternalExecute', lmt_Warning, 'Wrong answer: <%s>', [FirstLine]);
            FSock.CloseSocket;
            Self.PauseFor(FClient.ReconnectDelay);
            Continue;
          end;

          { ── 7. Leggi header rimanenti (HTTP standard, non ICY) ─────────────── }
          IsChunked := False;
          if not IsICY then
          begin
            Line := Self.ReadLine(2000);
            while (FSock.LastError = 0) and (Trim(Line) <> '') do
            begin
              if LowerCase(Trim(Line)) = 'transfer-encoding: chunked' then
                IsChunked := True;
              Line := Self.ReadLine(2000);
            end;
          end;

          { ── 8. Aggiorna versione rilevata e notifica ────────────────────────── }
          FDetectedVersion := TryVer;
          FLastGGASent     := 0;  // forza GGA immediato

          LBLogger.Write(1, 'TRTCMClientThread.InternalExecute', lmt_Info, 'NTRIP v%s connected to %s/%s%s', [IfThen(TryVer = nvV2, '2', '1'),
             FClient.Host,
             FClient.Mountpoint,
             IfThen(IsChunked, ' (chunked)', '')]);

          { ── 9. Loop ricezione dati ──────────────────────────────────────────── }
          if IsChunked then
            Self.ReceiveLoopChunked
          else
            Self.ReceiveLoopRaw;

        end;
    end;

    FSock.CloseSocket;
    if not Terminated then
      Self.PauseFor(FClient.ReconnectDelay);
  end;
end;

{ ══ TRTCMClient ══════════════════════════════════════════════════════════════ }

constructor TRTCMClient.Create(aSourceType: TRTCMSourceType; aHost: string; APort: Integer);
begin
  inherited Create;

  FHost           := AHost;
  FPort           := APort;
  FWebSocketURI   := '';

  FSourceType     := aSourceType;
  FMountpoint     := '';
  FUsername       := '';
  FPassword       := '';
  FActive         := False;
  FReconnectDelay := 5000;
  FNTRIPVersion   := nvAuto;
  FGGASentence    := '';
  FGGAIntervalSec := 0;
  FThread         := nil;

end;

destructor TRTCMClient.Destroy;
begin
  Self.Close;
  inherited;
end;

function TRTCMClient.GetDetectedVersion: TNTRIPVersion;
begin
  if Assigned(FThread) then
    Result := FThread.DetectedVersion
  else
    Result := nvAuto;
end;

procedure TRTCMClient.OnWebSocketData(Sender: TObject; aBuffer: pByte; aBufferLen: Int64);
var
  _Data : TBytes;

begin
  if Assigned(FOnCorrections) and (aBuffer <> nil) and (aBufferLen > 0) then
  begin
    SetLength(_Data, aBufferLen);
    Move(aBuffer^, _Data[0], aBufferLen);
    FOnCorrections(Self, _Data);
  end;
end;

procedure TRTCMClient.UpdateGGA(const AGGASentence: string);
begin
  FGGASentence := AGGASentence;
end;

function TRTCMClient.Open: Boolean;
begin
  if not FActive then
  begin
    if (FHost <> '') and (FPort > 0) then
    begin
      case FSourceType of
        stNTRIP, stIPDirect:
          begin
            FThread := TRTCMClientThread.Create(Self);
            FThread.SourceType := FSourceType;
            FActive := True;
            FThread.Start;
          end;

        stWebSocket:
          begin
            if FWebSocketClient = nil then
            begin
              FWebSocketClient := TLBWebSocketClient.Create;
              FWebSocketClient.RemoteConnectionData.Host      := FHost;
              FWebSocketClient.RemoteConnectionData.Port      := FPort;
              FWebSocketClient.URI                            := FWebSocketURI;
              FWebSocketClient.OnWebSocketBinaryMessage       := @Self.OnWebSocketData;

              // Facoltativo: abilitare auto-ping per mantenere viva la connessione
              FWebSocketClient.AutoPingEnabled := True;
              FWebSocketClient.PingInterval    := 30000;
            end;
            FWebSocketClient.Start;
            FActive := True;
          end;
      end;
    end
    else
      LBLogger.Write(1, 'TRTCMClient.Open', lmt_Warning, 'No host or port!');
  end;

  Result := FActive;
end;

procedure TRTCMClient.Close;
begin
  if FActive then
  begin
    if FThread <> nil then
      FreeAndNil(FThread);

    if FWebSocketClient <> nil then
      FreeAndNil(FWebSocketClient);

    FActive := False;
  end;
end;

end.
