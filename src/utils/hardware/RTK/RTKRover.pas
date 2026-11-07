unit RTKRover;

{
  Gestione rover simpleRTK2B (ZED-F9P) su thread dedicato.
  Protocollo: NMEA (GGA, RMC, GLL, VTG).

  Il thread è l'unico owner della seriale:
  - legge NMEA tramite buffer circolare TLBCircularBuffer
  - invia correzioni RTCM3 pendenti quando non ci sono dati in arrivo

  PushCorrections() è thread-safe: carica un buffer protetto da CS.
  Il buffer correzioni mantiene solo l'ultimo blocco ricevuto:
  dati vecchi non inviati vengono sovrascritti (semantica last-write-wins).

  Riconoscimento prefissi talker: vengono accettati GN, GP, GA, GL, GQ
  (es. GNGGA, GPGGA, GAGGA…) — confronto sul suffisso a 3 caratteri.
}

interface

uses
  SysUtils, Classes, SynaSer, SyncObjs, uLBCircularBuffer, RTKTypes;

type

  { ── Strutture NMEA ──────────────────────────────────────────────────── }

  TGNSSFixQuality = (
    fqNoFix    = 0,
    fqGPSFix   = 1,
    fqDGPSFix  = 2,
    fqPPSFix   = 3,
    fqRTKFixed = 4,
    fqRTKFloat = 5
  );

  TGNSSStatus = (gsVoid = 0, gsActive = 1);

  TGGAData = record
    Valid         : Boolean;
    UTCTime       : TDateTime;
    Latitude      : Double;
    Longitude     : Double;
    FixQuality    : TGNSSFixQuality;
    NumSatellites : Integer;
    HDOP          : Double;
    Altitude      : Double;
    GeoidSep      : Double;
  end;

  TRMCData = record
    Valid        : Boolean;
    UTCTime      : TDateTime;
    Status       : TGNSSStatus;
    Latitude     : Double;
    Longitude    : Double;
    SpeedKnots   : Double;
    SpeedKmH     : Double;
    CourseDeg    : Double;
  end;

  TGLLData = record
    Valid      : Boolean;
    Latitude   : Double;
    Longitude  : Double;
    UTCTime    : TDateTime;
    Status     : TGNSSStatus;
  end;

  TVTGData = record
    Valid        : Boolean;
    CourseTrueN  : Double;
    CourseMagN   : Double;
    SpeedKnots   : Double;
    SpeedKmH     : Double;
  end;

  { ── Tipi evento ─────────────────────────────────────────────────────── }

  TOnGGAReceived = procedure(Sender: TObject; const Data: TGGAData) of object;
  TOnRMCReceived = procedure(Sender: TObject; const Data: TRMCData) of object;
  TOnGLLReceived = procedure(Sender: TObject; const Data: TGLLData) of object;
  TOnVTGReceived = procedure(Sender: TObject; const Data: TVTGData) of object;
  TOnRawNMEA    = procedure(Sender: TObject; const Sentence: string) of object;

  { ── Forward ─────────────────────────────────────────────────────────── }

  TRTKRover = class;

  { ── Thread seriale ──────────────────────────────────────────────────── }

  TRoverThread = class(TThread)
  private
    FRover      : TRTKRover;
    FSerial     : TBlockSerial;
    FFields     : TStringList;

    { Record temporanei per Synchronize }
    FTmpGGA   : TGGAData;
    FTmpRMC   : TRMCData;
    FTmpGLL   : TGLLData;
    FTmpVTG   : TVTGData;
    FTmpError : string;
    FTmpRaw   : string;

    { Helpers parsing }
    procedure SplitFields(const S: string);
    function  Field(Index: Integer): string; inline;
    function  ParseLatLon(const Value, Hemi: string; IsLat: Boolean): Double;
    function  ParseUTCTime(const HHMMSS: string): TDateTime;
    function  ParseDate(const DDMMYY: string): TDateTime;
    function  ValidChecksum(const S: string): Boolean;

    { Parser NMEA }
    procedure ParseGGA;
    procedure ParseRMC;
    procedure ParseGLL;
    procedure ParseVTG;
    procedure ProcessSentence(const S: string);

    { Synchronize callbacks }
    procedure SyncGGA;
    procedure SyncRMC;
    procedure SyncGLL;
    procedure SyncVTG;
    procedure SyncError;
    procedure SyncRaw;
  protected
    procedure Execute; override;
  public
    constructor Create(ARover: TRTKRover; const APort: string; ABaudRate: Integer);
    destructor  Destroy; override;
    function    SerialOK: Boolean;
    function    SerialErrorDesc: string;
  end;

  { ── Classe pubblica ─────────────────────────────────────────────────── }

  TRTKRover = class
  private
    FThread      : TRoverThread;
    FCorrections : TCorrectionBuffer;
    FPort        : string;
    FBaudRate    : Integer;
    FActive      : Boolean;

    FOnGGA       : TOnGGAReceived;
    FOnRMC       : TOnRMCReceived;
    FOnGLL       : TOnGLLReceived;
    FOnVTG       : TOnVTGReceived;
    FOnRawNMEA   : TOnRawNMEA;
    FOnError     : TOnRTKError;
  public
    constructor Create(const APort: string; ABaudRate: Integer = 38400);
    destructor  Destroy; override;

    procedure Open;
    procedure Close;

    procedure PushCorrections(const Data: array of Byte; Len: Integer); overload;
    procedure PushCorrections(const Data: TBytes); overload;

    property Active      : Boolean           read FActive;
    property Port        : string            read FPort        write FPort;
    property BaudRate    : Integer           read FBaudRate    write FBaudRate;
    property Corrections : TCorrectionBuffer read FCorrections;

    property OnGGA       : TOnGGAReceived    read FOnGGA       write FOnGGA;
    property OnRMC       : TOnRMCReceived    read FOnRMC       write FOnRMC;
    property OnGLL       : TOnGLLReceived    read FOnGLL       write FOnGLL;
    property OnVTG       : TOnVTGReceived    read FOnVTG       write FOnVTG;
    property OnRawNMEA   : TOnRawNMEA        read FOnRawNMEA   write FOnRawNMEA;
    property OnError     : TOnRTKError       read FOnError     write FOnError;
  end;

implementation

const
  KNOTS_TO_KMH = 1.852;
  BYTE_DOLLAR  = Byte(Ord('$'));
  BYTE_LF      = Byte(10);
  BYTE_CR      = Byte(13);

{ ══ TRoverThread — helpers ══════════════════════════════════════════════════ }

procedure TRoverThread.SplitFields(const S: string);
var
  i, StartIdx: Integer;
begin
  FFields.Clear;
  StartIdx := 1;
  for i := 1 to Length(S) do
    if S[i] = ',' then
    begin
      FFields.Add(Copy(S, StartIdx, i - StartIdx));
      StartIdx := i + 1;
    end;
  FFields.Add(Copy(S, StartIdx, Length(S) - StartIdx + 1));
end;

function TRoverThread.Field(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FFields.Count) then
    Result := FFields[Index]
  else
    Result := '';
end;

function TRoverThread.ParseLatLon(const Value, Hemi: string; IsLat: Boolean): Double;
var
  DegLen: Integer;
begin
  Result := 0;
  if Value = '' then Exit;
  if IsLat then DegLen := 2 else DegLen := 3;
  Result := StrToFloatDef(Copy(Value, 1, DegLen), 0) +
            StrToFloatDef(Copy(Value, DegLen + 1, Length(Value)), 0) / 60.0;
  if (Hemi = 'S') or (Hemi = 'W') then Result := -Result;
end;

function TRoverThread.ParseUTCTime(const HHMMSS: string): TDateTime;
var
  H, M, S, MS: Word;
begin
  Result := 0;
  if Length(HHMMSS) < 6 then Exit;
  H  := StrToIntDef(Copy(HHMMSS, 1, 2), 0);
  M  := StrToIntDef(Copy(HHMMSS, 3, 2), 0);
  S  := StrToIntDef(Copy(HHMMSS, 5, 2), 0);
  MS := 0;
  { Gestione decimali: HHMMSS.ss → millisecondi }
  if Length(HHMMSS) > 7 then
    MS := Round(StrToFloatDef('0.' + Copy(HHMMSS, 8, Length(HHMMSS)), 0) * 1000);
  Result := EncodeTime(H, M, S, MS);
end;

function TRoverThread.ParseDate(const DDMMYY: string): TDateTime;
begin
  Result := 0;
  if Length(DDMMYY) < 6 then Exit;
  Result := EncodeDate(
    StrToIntDef(Copy(DDMMYY, 5, 2), 0) + 2000,
    StrToIntDef(Copy(DDMMYY, 3, 2), 1),
    StrToIntDef(Copy(DDMMYY, 1, 2), 1));
end;

function TRoverThread.ValidChecksum(const S: string): Boolean;
var
  i, StarPos : Integer;
  Calc       : Byte;
begin
  Result  := False;
  StarPos := Pos('*', S);
  if StarPos < 2 then Exit;
  Calc := 0;
  for i := 2 to StarPos - 1 do
    Calc := Calc xor Ord(S[i]);
  Result := Calc = StrToIntDef('$' + Copy(S, StarPos + 1, 2), -1);
end;

{ ══ TRoverThread — Synchronize ══════════════════════════════════════════════ }

procedure TRoverThread.SyncGGA;
begin
  if Assigned(FRover.OnGGA) then FRover.OnGGA(FRover, FTmpGGA);
end;

procedure TRoverThread.SyncRMC;
begin
  if Assigned(FRover.OnRMC) then FRover.OnRMC(FRover, FTmpRMC);
end;

procedure TRoverThread.SyncGLL;
begin
  if Assigned(FRover.OnGLL) then FRover.OnGLL(FRover, FTmpGLL);
end;

procedure TRoverThread.SyncVTG;
begin
  if Assigned(FRover.OnVTG) then FRover.OnVTG(FRover, FTmpVTG);
end;

procedure TRoverThread.SyncError;
begin
  if Assigned(FRover.OnError) then FRover.OnError(FRover, FTmpError);
end;

procedure TRoverThread.SyncRaw;
begin
  if Assigned(FRover.OnRawNMEA) then FRover.OnRawNMEA(FRover, FTmpRaw);
end;

{ ══ TRoverThread — parser NMEA ══════════════════════════════════════════════ }

procedure TRoverThread.ParseGGA;
begin
  FillChar(FTmpGGA, SizeOf(FTmpGGA), 0);
  if FFields.Count < 10 then Exit;
  FTmpGGA.UTCTime       := ParseUTCTime(Field(1));
  FTmpGGA.Latitude      := ParseLatLon(Field(2), Field(3), True);
  FTmpGGA.Longitude     := ParseLatLon(Field(4), Field(5), False);
  FTmpGGA.FixQuality    := TGNSSFixQuality(StrToIntDef(Field(6), 0));
  FTmpGGA.NumSatellites := StrToIntDef(Field(7), 0);
  FTmpGGA.HDOP          := StrToFloatDef(Field(8), 0);
  FTmpGGA.Altitude      := StrToFloatDef(Field(9), 0);
  FTmpGGA.GeoidSep      := StrToFloatDef(Field(11), 0);
  FTmpGGA.Valid         := FTmpGGA.FixQuality <> fqNoFix;
  Synchronize(@SyncGGA);
end;

procedure TRoverThread.ParseRMC;
begin
  FillChar(FTmpRMC, SizeOf(FTmpRMC), 0);
  if FFields.Count < 10 then Exit;
  FTmpRMC.Status     := TGNSSStatus(Ord(Field(2) = 'A'));
  FTmpRMC.Latitude   := ParseLatLon(Field(3), Field(4), True);
  FTmpRMC.Longitude  := ParseLatLon(Field(5), Field(6), False);
  FTmpRMC.SpeedKnots := StrToFloatDef(Field(7), 0);
  FTmpRMC.SpeedKmH   := FTmpRMC.SpeedKnots * KNOTS_TO_KMH;
  FTmpRMC.CourseDeg  := StrToFloatDef(Field(8), 0);
  FTmpRMC.UTCTime    := ParseDate(Field(9)) + ParseUTCTime(Field(1));
  FTmpRMC.Valid      := FTmpRMC.Status = gsActive;
  Synchronize(@SyncRMC);
end;

procedure TRoverThread.ParseGLL;
begin
  FillChar(FTmpGLL, SizeOf(FTmpGLL), 0);
  if FFields.Count < 7 then Exit;
  FTmpGLL.Latitude  := ParseLatLon(Field(1), Field(2), True);
  FTmpGLL.Longitude := ParseLatLon(Field(3), Field(4), False);
  FTmpGLL.UTCTime   := ParseUTCTime(Field(5));
  FTmpGLL.Status    := TGNSSStatus(Ord(Field(6) = 'A'));
  FTmpGLL.Valid     := FTmpGLL.Status = gsActive;
  Synchronize(@SyncGLL);
end;

procedure TRoverThread.ParseVTG;
begin
  FillChar(FTmpVTG, SizeOf(FTmpVTG), 0);
  if FFields.Count < 9 then Exit;
  FTmpVTG.CourseTrueN := StrToFloatDef(Field(1), 0);
  FTmpVTG.CourseMagN  := StrToFloatDef(Field(3), 0);
  FTmpVTG.SpeedKnots  := StrToFloatDef(Field(5), 0);
  FTmpVTG.SpeedKmH    := StrToFloatDef(Field(7), 0);
  FTmpVTG.Valid       := True;
  Synchronize(@SyncVTG);
end;

procedure TRoverThread.ProcessSentence(const S: string);
var
  MsgType, MsgSuffix, Clean: string;
  StarPos: Integer;
begin
  if not ValidChecksum(S) then
  begin
    FTmpError := 'Checksum non valido: ' + S;
    Synchronize(@SyncError);
    Exit;
  end;

  if Assigned(FRover.OnRawNMEA) then
  begin
    FTmpRaw := S;
    Synchronize(@SyncRaw);
  end;

  StarPos := Pos('*', S);
  if StarPos > 0 then
    Clean := Copy(S, 2, StarPos - 2)
  else
    Clean := Copy(S, 2, Length(S) - 1);

  SplitFields(Clean);
  if FFields.Count = 0 then Exit;

  MsgType := FFields[0];

  { Confronta sul suffisso a 3 caratteri per accettare qualsiasi prefisso
    talker: GN (GNSS combinato), GP (GPS), GA (Galileo), GL (GLONASS), GQ. }
  if Length(MsgType) >= 5 then
    MsgSuffix := Copy(MsgType, Length(MsgType) - 2, 3)
  else
    MsgSuffix := MsgType;

  if      MsgSuffix = 'GGA' then ParseGGA
  else if MsgSuffix = 'RMC' then ParseRMC
  else if MsgSuffix = 'GLL' then ParseGLL
  else if MsgSuffix = 'VTG' then ParseVTG;
end;

{ ══ TRoverThread — Execute ══════════════════════════════════════════════════ }

procedure TRoverThread.Execute;
var
  DollarPos : Integer;
  LFPos     : Integer;
  LineLen   : Integer;
  LineBuf   : array[0..127] of Byte;
  Line      : string;
  CorrData  : TBytes;
  CorrLen   : Integer;
  RxBuf     : TLBCircularBuffer;
begin
  RxBuf := TLBCircularBuffer.Create(1024);
  try
    while not Terminated do
    begin
      { ── 1. Leggi dalla seriale nel buffer circolare ─────────────────── }
      if FSerial.WaitingData > 0 then
        RxBuf.WriteFromSerial(FSerial)
      else
        Sleep(1);

      { ── 2. Invia correzioni RTCM3 pendenti ──────────────────────────── }
      CorrLen := FRover.Corrections.Flush(CorrData);
      if CorrLen > 0 then
        FSerial.SendBuffer(@CorrData[0], CorrLen);

      { ── 3. Elabora tutte le frasi NMEA complete nel buffer ───────────── }
      while RxBuf.AvailableForRead > 0 do
      begin
        { Trova '$' — inizio frase NMEA }
        DollarPos := RxBuf.FindByte(BYTE_DOLLAR, 0);

        if DollarPos < 0 then
        begin
          RxBuf.Clear;
          Break;
        end;

        if DollarPos > 0 then
          RxBuf.Skip(DollarPos);

        { Cerca LF con offset 1 (offset 0 è il '$' stesso) }
        LFPos := RxBuf.FindByte(BYTE_LF, 1);

        if LFPos < 0 then
          Break;

        LineLen := LFPos + 1;

        if LineLen <= SizeOf(LineBuf) then
        begin
          RxBuf.Read(@LineBuf, LineLen);
          while (LineLen > 0) and (LineBuf[LineLen - 1] in [BYTE_CR, BYTE_LF]) do
            Dec(LineLen);
          if LineLen > 1 then
          begin
            SetString(Line, PAnsiChar(@LineBuf[0]), LineLen);
            ProcessSentence(Line);
          end;
        end
        else
          RxBuf.Skip(LineLen);
      end;

    end;
  finally
    RxBuf.Free;
  end;
end;

{ ══ TRoverThread — lifecycle ════════════════════════════════════════════════ }

constructor TRoverThread.Create(ARover: TRTKRover; const APort: string; ABaudRate: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FRover      := ARover;
  FFields     := TStringList.Create;
  FFields.Capacity := 20;
  FSerial     := TBlockSerial.Create;
  FSerial.Connect(APort);
  FSerial.Config(ABaudRate, 8, 'N', SB1, False, False);
end;

destructor TRoverThread.Destroy;
begin
  FSerial.CloseSocket;
  FSerial.Free;
  FFields.Free;
  inherited;
end;

function TRoverThread.SerialOK: Boolean;
begin
  Result := FSerial.LastError = 0;
end;

function TRoverThread.SerialErrorDesc: string;
begin
  Result := FSerial.LastErrorDesc;
end;

{ ══ TRTKRover ════════════════════════════════════════════════════════════════ }

constructor TRTKRover.Create(const APort: string; ABaudRate: Integer);
begin
  inherited Create;
  FPort        := APort;
  FBaudRate    := ABaudRate;
  FActive      := False;
  FThread      := nil;
  FCorrections := TCorrectionBuffer.Create;
end;

destructor TRTKRover.Destroy;
begin
  Close;
  FCorrections.Free;
  inherited;
end;

procedure TRTKRover.Open;
begin
  if FActive then Exit;
  FThread := TRoverThread.Create(Self, FPort, FBaudRate);
  if FThread.SerialOK then
  begin
    FActive := True;
    FThread.Start;
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, 'Errore apertura porta ' + FPort + ': ' +
                     FThread.SerialErrorDesc);
    FreeAndNil(FThread);
  end;
end;

procedure TRTKRover.Close;
begin
  if not FActive then Exit;
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  FActive := False;
end;

procedure TRTKRover.PushCorrections(const Data: array of Byte; Len: Integer);
begin
  FCorrections.Push(Data, Len);
end;

procedure TRTKRover.PushCorrections(const Data: TBytes);
begin
  FCorrections.Push(Data);
end;

end.
