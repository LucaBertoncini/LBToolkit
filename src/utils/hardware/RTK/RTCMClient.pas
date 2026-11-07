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
  { Modalita' di ricezione dello stream grezzo (IPDirect e NTRIP v1):
      rmFramed : legge l'intestazione dei frame RTCM3, VERIFICA IL CRC-24Q e
                 consegna solo frame completi e validi (si veda
                 ReceiveLoopFramed). Scarta cio' che non e' RTCM3; se manca
                 l'inizio di un frame si risincronizza sul preambolo $D3
                 successivo. Ogni anomalia viene scritta nel log.
      rmTimed  : non interpreta i dati: raggruppa i byte che arrivano ravvicinati
                 (si veda ReceiveLoopTimed). Inoltra qualunque contenuto, ma il
                 confine dei blocchi e' dato solo dai tempi. }
  TRTCMReceiveMode = (rmFramed, rmTimed);

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
    procedure ReceiveLoopFramed;
    procedure ReceiveLoopTimed;
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
    FPassphrase      : String;
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
    FReceiveMode     : TRTCMReceiveMode;
    FBlockGapMs      : Integer;

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

    { Solo stWebSocket: se impostata il client si autentica (header X-LB-*). }
    property Passphrase      : string          read FPassphrase     write FPassphrase;

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
    { Solo IPDirect / NTRIP v1 (stream grezzo). Default rmFramed. }
    property ReceiveMode     : TRTCMReceiveMode read FReceiveMode write FReceiveMode;
    { Solo rmTimed: silenzio (ms) che chiude un blocco. Default 10. }
    property BlockGapMs      : Integer          read FBlockGapMs  write FBlockGapMs;

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
  // Framing RTCM3 usato da ReceiveLoopFramed:
  RTCM3_PREAMBLE   = $D3;
  RTCM3_HEADER_SIZE = 3;     // preambolo + 6 bit riservati + 10 bit lunghezza
  RTCM3_CRC_SIZE   = 3;
  RTCM3_MAX_FRAME  = 1029;   // 3 + 1023 (payload massimo) + 3
  RTCM3_CRC24Q_POLY = $1864CFB;   // x^24 + x^23 + x^18 + x^17 + x^14 + x^11 + x^10 + x^7 + x^6 + x^5 + x^4 + x^3 + x + 1
  RTCM3_MAX_ERR_LOG = 20;    // errori consecutivi loggati prima della sospensione del log
  // Raggruppamento a tempo usato da ReceiveLoopTimed:
  RECV_BLOCK_GAP_MS    = 10;      // silenzio che chiude un blocco (default)
  RECV_MAX_BLOCK       = 32768;   // sicurezza: dimensione massima di un blocco
  RECV_MAX_BLOCK_MS    = 250;     // sicurezza: durata massima di un blocco
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

{ CRC-24Q (Qualcomm) usato da RTCM3: polinomio $1864CFB, valore iniziale 0,
  nessuna riflessione, nessun XOR finale. Calcolato su aCount byte a partire
  da aStart. Valore di controllo: CRC("123456789") = $CDE703. Calcolando il
  CRC su un frame completo, CRC compreso, il risultato e' 0. }
function RTCM3CRC24Q(const aData: array of Byte; aStart, aCount: Integer): Cardinal;
var
  i, b: Integer;
begin
  Result := 0;
  for i := aStart to aStart + aCount - 1 do
  begin
    Result := Result xor (Cardinal(aData[i]) shl 16);
    for b := 1 to 8 do
    begin
      Result := Result shl 1;
      if (Result and $1000000) <> 0 then
        Result := Result xor RTCM3_CRC24Q_POLY;
    end;
  end;
  Result := Result and $FFFFFF;
end;

{ ── Loop v1 / IPDirect, modalita' rmFramed ──
  I byte letti dal socket (CanRead + RecvBuffer: ritorna subito con quanto
  c'e', senza attendere di riempire il buffer) si accodano in un buffer di
  accumulo. Da li' si estraggono i frame RTCM3 COMPLETI E VALIDI: preambolo
  $D3, 6 bit riservati (a 0), lunghezza del payload su 10 bit, payload, CRC-24Q
  su 3 byte (frame = 3 + lunghezza + 3 byte).
  Sincronizzazione: se il CRC non torna (o l'intestazione e' incoerente) si
  scarta SOLO il byte $D3 corrente e si riparte dal preambolo successivo,
  rileggendo gli stessi byte: gli eventuali frame veri contenuti nella finestra
  di un falso frame NON vanno persi, vengono solo consegnati con ritardo (un
  falso frame con lunghezza dichiarata L attende L+6 byte prima di poter essere
  scartato). Un frame incompleto resta nel buffer fino all'arrivo del resto.
  Tutti i frame validi disponibili dopo una lettura sono consegnati con UNA sola
  chiamata a OnCorrections.
  Log: ogni anomalia viene scritta (byte scartati fuori frame, intestazione
  non valida, CRC errato, buffer saturo, connessione persa con un frame
  incompleto pendente). Dopo RTCM3_MAX_ERR_LOG errori consecutivi, senza nessun
  frame valido in mezzo, il log viene sospeso (un solo messaggio lo segnala)
  fino al frame valido successivo. Gli scarti iniziali (primo allineamento
  dopo la connessione, che di norma parte a meta' di un frame) sono solo
  informativi. Silenzio piu' lungo di RECV_TIMEOUT_MS = errore: il ciclo
  termina e il thread si riconnette. }
procedure TRTCMClientThread.ReceiveLoopFramed;
const
  cMe = 'TRTCMClientThread.ReceiveLoopFramed';
var
  _Acc       : array[0..RECV_BUF_SIZE + RTCM3_MAX_FRAME - 1] of Byte;
  _AccLen    : Integer;   // byte validi in Acc
  _NRead     : Integer;
  _Idx       : Integer;
  _Start     : Integer;
  _Skipped   : Integer;
  _PayLen    : Integer;
  _FrameLen  : Integer;
  _Total     : Integer;
  _MsgNum    : Integer;
  _CRCCalc   : Cardinal;
  _CRCRecv   : Cardinal;
  _ErrStreak : Integer;   // errori consecutivi senza frame validi
  _Synced    : Boolean;   // almeno un frame valido dall'apertura della connessione

  function HexDump(aStart, aCount: Integer): string;
  var
    i, n: Integer;
  begin
    Result := '';
    n := aCount;
    if n > 16 then n := 16;
    for i := 0 to n - 1 do
      Result := Result + IntToHex(_Acc[aStart + i], 2) + ' ';
    Result := Trim(Result);
    if aCount > n then
      Result := Result + ' ...';
  end;

  procedure LogError(const aMsg: string);
  begin
    Inc(_ErrStreak);
    if _ErrStreak <= RTCM3_MAX_ERR_LOG then
      LBLogger.Write(1, cMe, lmt_Warning, '%s', [aMsg])
    else if _ErrStreak = RTCM3_MAX_ERR_LOG + 1 then
      LBLogger.Write(1, cMe, lmt_Warning, 'More than %d consecutive RTCM3 errors: further messages suppressed until a valid frame is received', [RTCM3_MAX_ERR_LOG]);
  end;

begin
  _AccLen    := 0;
  _ErrStreak := 0;
  _Synced    := False;
  while not Terminated do
  begin
    if FSourceType = stNTRIP then
      Self.SendGGAIfDue;

    { 1. Attesa dei dati. }
    if not FSock.CanRead(RECV_TIMEOUT_MS) then
    begin
      if Terminated then Break;
      if FSock.LastError <> 0 then
        LBLogger.Write(1, cMe, lmt_Warning, 'Connection error: <%s> (%d byte of incomplete data discarded)', [FSock.LastErrorDesc, _AccLen])
      else
        LBLogger.Write(1, cMe, lmt_Warning, 'No data received for %d ms (%d byte of incomplete frame pending, discarded)', [RECV_TIMEOUT_MS, _AccLen]);
      Break;
    end;
    if Terminated then Break;

    { 2. Lettura in coda a quanto gia' accumulato. }
    if _AccLen >= SizeOf(_Acc) then
    begin
      // Non dovrebbe accadere (a fine ciclo restano meno di RTCM3_MAX_FRAME byte).
      LogError(Format('Accumulation buffer full (%d byte): content discarded', [_AccLen]));
      _AccLen := 0;
    end;
    _NRead := FSock.RecvBuffer(@_Acc[_AccLen], SizeOf(_Acc) - _AccLen);
    if _NRead <= 0 then
    begin
      // CanRead vero ma 0 byte: connessione chiusa dalla base (o errore).
      LBLogger.Write(1, cMe, lmt_Warning, 'Connection closed: <%s> (%d byte of incomplete data discarded)', [FSock.LastErrorDesc, _AccLen]);
      Break;
    end;
    Inc(_AccLen, _NRead);

    { 3. Estrazione dei frame completi e validi. }
    _Idx   := 0;
    _Total := 0;
    while True do
    begin
      { 3a. Scarta i byte che precedono il preambolo. }
      _Start := _Idx;
      while (_Idx < _AccLen) and (_Acc[_Idx] <> RTCM3_PREAMBLE) do
        Inc(_Idx);
      _Skipped := _Idx - _Start;
      if _Skipped > 0 then
      begin
        if _Synced then
          LogError(Format('%d byte outside any RTCM3 frame discarded: %s', [_Skipped, HexDump(_Start, _Skipped)]))
        else
          LBLogger.Write(3, cMe, lmt_Info, 'Initial synchronization: %d byte discarded before the first RTCM3 preamble', [_Skipped]);
      end;

      if _AccLen - _Idx < RTCM3_HEADER_SIZE then Break;        // intestazione incompleta

      { 3b. Intestazione coerente? (6 bit riservati a zero) }
      if (_Acc[_Idx + 1] and $FC) <> 0 then
      begin
        LogError(Format('Invalid RTCM3 header (reserved bits <> 0, bytes %s): false preamble, resynchronizing', [HexDump(_Idx, RTCM3_HEADER_SIZE)]));
        Inc(_Idx);
        Continue;
      end;

      _PayLen   := ((_Acc[_Idx + 1] and $03) shl 8) or _Acc[_Idx + 2];
      _FrameLen := _PayLen + RTCM3_HEADER_SIZE + RTCM3_CRC_SIZE;
      if _AccLen - _Idx < _FrameLen then Break;                 // frame incompleto: attende il resto

      { 3c. Verifica del CRC-24Q. }
      _CRCCalc := RTCM3CRC24Q(_Acc, _Idx, _FrameLen - RTCM3_CRC_SIZE);
      _CRCRecv := (Cardinal(_Acc[_Idx + _FrameLen - 3]) shl 16)
              or (Cardinal(_Acc[_Idx + _FrameLen - 2]) shl 8)
              or  Cardinal(_Acc[_Idx + _FrameLen - 1]);
      if _CRCCalc <> _CRCRecv then
      begin
        if _PayLen >= 2 then
          _MsgNum := (Integer(_Acc[_Idx + 3]) shl 4) or (_Acc[_Idx + 4] shr 4)
        else
          _MsgNum := -1;
        LogError(Format('CRC-24Q mismatch (message %d, payload %d byte, calculated $%s, received $%s): frame discarded, resynchronizing',
          [_MsgNum, _PayLen, IntToHex(_CRCCalc, 6), IntToHex(_CRCRecv, 6)]));
        Inc(_Idx);     // solo il preambolo: i byte successivi vengono riesaminati
        Continue;
      end;

      { 3d. Frame valido. }
      if _ErrStreak > 0 then
      begin
        LBLogger.Write(3, cMe, lmt_Info, 'Valid RTCM3 frame received after %d error(s): synchronized', [_ErrStreak]);
        _ErrStreak := 0;
      end;
      if not _Synced then
      begin
        // Primo frame valido dalla (ri)connessione: scritto una sola volta.
        if _PayLen >= 2 then
          _MsgNum := (Integer(_Acc[_Idx + 3]) shl 4) or (_Acc[_Idx + 4] shr 4)
        else
          _MsgNum := -1;
        LBLogger.Write(5, cMe, lmt_Debug, 'RTCM3 frame recognized, in sync (message %d, payload %d byte)', [_MsgNum, _PayLen]);
        _Synced := True;
      end;
      SetLength(FTmpData, _Total + _FrameLen);
      Move(_Acc[_Idx], FTmpData[_Total], _FrameLen);
      Inc(_Total, _FrameLen);
      Inc(_Idx, _FrameLen);
    end;

    { 4. Si tiene solo cio' che non e' stato consumato (frame incompleto). }
    if _Idx > 0 then
    begin
      Dec(_AccLen, _Idx);
      if _AccLen > 0 then
        Move(_Acc[_Idx], _Acc[0], _AccLen);
    end;

    { 5. Consegna dei frame validi. }
    if _Total > 0 then
      Self.FireCorrections;
  end;
end;

{ ── Loop v1 / IPDirect, modalita' rmTimed ──
  Non interpreta i dati. Legge appena disponibili (CanRead + RecvBuffer:
  ritorna subito con quanto c'e') e raggruppa in un unico blocco i byte
  ravvicinati: dopo ogni lettura attende BlockGapMs ms; se arrivano altri
  dati li legge e attende di nuovo, altrimenti il blocco e' chiuso e
  consegnato con UNA sola chiamata a OnCorrections. Il confine del blocco e'
  quindi dato solo dai tempi: una pausa piu' lunga di BlockGapMs in mezzo a
  una raffica la divide in due blocchi (e puo' tagliare un frame). Limiti di
  sicurezza: RECV_MAX_BLOCK byte o RECV_MAX_BLOCK_MS ms per blocco. Silenzio
  piu' lungo di RECV_TIMEOUT_MS = errore: il ciclo termina e il thread si
  riconnette. }
procedure TRTCMClientThread.ReceiveLoopTimed;
var
  _Buf     : array[0..RECV_BUF_SIZE - 1] of Byte;
  _NRead   : Integer;
  _Total   : Integer;
  _Started : QWord;
  _Failed  : Boolean;
  _GapMs   : Integer;
begin
  _GapMs := FClient.BlockGapMs;
  if _GapMs < 1 then _GapMs := RECV_BLOCK_GAP_MS;
  while not Terminated do
  begin
    if FSourceType = stNTRIP then
      Self.SendGGAIfDue;

    { 1. Attesa del primo dato. }
    if not FSock.CanRead(RECV_TIMEOUT_MS) then
    begin
      if Terminated then Break;
      if FSock.LastError <> 0 then
        LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopTimed', lmt_Warning, 'Connection error: <%s>', [FSock.LastErrorDesc])
      else
        LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopTimed', lmt_Warning, 'No data received for %d ms', [RECV_TIMEOUT_MS]);
      Break;
    end;
    if Terminated then Break;

    { 2. Lettura del blocco: si legge, si attende GapMs e, se sono arrivati
         altri dati, si rilegge. }
    _Total   := 0;
    _Failed  := False;
    _Started := GetTickCount64;
    repeat
      _NRead := FSock.RecvBuffer(@_Buf, SizeOf(_Buf));
      if _NRead <= 0 then
      begin
        // CanRead vero ma 0 byte: connessione chiusa dalla base (o errore).
        _Failed := True;
        Break;
      end;
      SetLength(FTmpData, _Total + _NRead);
      Move(_Buf[0], FTmpData[_Total], _NRead);
      Inc(_Total, _NRead);
    until Terminated
       or (_Total >= RECV_MAX_BLOCK)
       or (GetTickCount64 - _Started >= RECV_MAX_BLOCK_MS)
       or (not FSock.CanRead(_GapMs));

    if Terminated then Break;

    { 3. Consegna dell'intero blocco (anche se la connessione e' caduta
         subito dopo: i byte gia' letti restano validi). }
    if _Total > 0 then
      Self.FireCorrections;

    if _Failed then
    begin
      LBLogger.Write(1, 'TRTCMClientThread.ReceiveLoopTimed', lmt_Warning, 'Connection closed: <%s>', [FSock.LastErrorDesc]);
      Break;
    end;
  end;
end;

{ ── Loop v1 / IPDirect: sceglie la modalita' di ricezione (ReceiveMode) ── }
procedure TRTCMClientThread.ReceiveLoopRaw;
begin
  case FClient.ReceiveMode of
    rmTimed: Self.ReceiveLoopTimed;
  else
    Self.ReceiveLoopFramed;
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
  FPassphrase     := '';

  FSourceType     := aSourceType;
  FMountpoint     := '';
  FUsername       := '';
  FPassword       := '';
  FActive         := False;
  FReconnectDelay := 5000;
  FNTRIPVersion   := nvAuto;
  FGGASentence    := '';
  FGGAIntervalSec := 0;
  FReceiveMode    := rmFramed;
  FBlockGapMs     := RECV_BLOCK_GAP_MS;
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
              FWebSocketClient.AddReference(@FWebSocketClient);
              FWebSocketClient.RemoteConnectionData.Host      := FHost;
              FWebSocketClient.RemoteConnectionData.Port      := FPort;
              FWebSocketClient.URI                            := FWebSocketURI;
              FWebSocketClient.Passphrase                     := FPassphrase;
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
