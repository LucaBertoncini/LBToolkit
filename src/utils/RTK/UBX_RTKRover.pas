unit UBX_RTKRover;

{
  Gestione rover simpleRTK2B (ZED-F9P) su thread dedicato.

  ── Modalità di output posizione ─────────────────────────────────────────
  Selezionabile tramite la property OutputMode prima di chiamare Open:

    romUBX   (default)
      Il modulo emette NAV-PVT binario. Il thread lo decodifica
      internamente. Massima precisione (hAcc/vAcc/sAcc al mm).
      Adatto per uso embedded e logging ad alta frequenza.

    romNMEA
      Il modulo emette GGA + RMC in ASCII NMEA 0183.
      Facile da intercettare con qualsiasi applicazione esterna
      (u-center, QGIS, app mobile) che legge la stessa porta COM.
      Il thread parsifica GGA e RMC e produce lo stesso TPVTData
      dell'altra modalità — l'interfaccia pubblica è identica.
      I campi HAccMm, VAccMm, SpeedAccMms non sono disponibili in
      NMEA e restano a zero.

  In entrambe le modalità le correzioni RTCM3 vengono ricevute
  tramite PushCorrections e scritte sulla stessa porta USB.

  ── Configurazione VALSET ────────────────────────────────────────────────
  Configure(ALayer, AMode) costruisce il payload appropriato:
    romUBX:  OUTPROT UBX=On, NMEA=Off;  NAV-PVT=1, GGA=0, RMC=0
    romNMEA: OUTPROT UBX=Off, NMEA=On;  NAV-PVT=0, GGA=1, RMC=1
  In entrambi: INPROT UBX=On, RTCM3X=On, NMEA=Off.

  ── Consegna dati PVT ────────────────────────────────────────────────────
  OnPVT è chiamato dal thread seriale (non dal thread UI).
  L'handler deve essere thread-safe. Per leggere l'ultimo valore
  in modo sincronizzato usare GetLastPVT.

  ── Dipendenze ───────────────────────────────────────────────────────────
  UBXProtocol, SynaSer, uLBCircularBuffer, RTKTypes, uLBBaseThread,
  uTimedoutCriticalSection, ULBLogger.
}

interface

uses
  SysUtils, Classes, SynaSer, uLBCircularBuffer, uLBUtils,
  RTKTypes, UBXProtocol, uLBBaseThread, uTimedoutCriticalSection;

type

  { ── Modalità output posizione ────────────────────────────────────────── }

  TRoverOutputMode = (
    romUBX,   // NAV-PVT binario — precisione massima
    romNMEA   // GGA + RMC ASCII — interoperabile con app esterne
  );

  { ── Payload UBX-NAV-PVT (92 byte, packed) ───────────────────────────────
    Ref: u-blox F9 HPG 1.51 Interface Description §3.15.13             }

  TFixType = (
    ftNoFix    = 0,
    ftDeadReck = 1,
    ft2D       = 2,
    ft3D       = 3,
    ftGNSSDR   = 4,
    ftTimeOnly = 5
  );

  TUBXNavPVT = packed record
    iTOW      : UInt32;
    year      : UInt16;
    month     : Byte;
    day       : Byte;
    hour      : Byte;
    min       : Byte;
    sec       : Byte;
    valid     : Byte;
    tAcc      : UInt32;
    nano      : Int32;
    fixType   : Byte;
    flags     : Byte;
    flags2    : Byte;
    numSV     : Byte;
    lon       : Int32;
    lat       : Int32;
    height    : Int32;
    hMSL      : Int32;
    hAcc      : UInt32;
    vAcc      : UInt32;
    velN      : Int32;
    velE      : Int32;
    velD      : Int32;
    gSpeed    : Int32;
    headMot   : Int32;
    sAcc      : UInt32;
    headAcc   : UInt32;
    pDOP      : UInt16;
    flags3    : UInt16;
    reserved0 : array[0..3] of Byte;
    headVeh   : Int32;
    magDec    : Int16;
    magAcc    : UInt16;
  end;  // 92 byte ✓

  { ── Dati PVT decodificati ─────────────────────────────────────────────── }

  TPVTData = record
    Valid       : Boolean;
    FixType     : TFixType;
    RTKStatus   : TRTKStatus;
    GNSSFixOK   : Boolean;
    UTCTime     : TDateTime;
    Latitude    : Double;       // gradi decimali, + = Nord
    Longitude   : Double;       // gradi decimali, + = Est
    AltitudeMSL : Double;       // m
    SpeedKmH    : Double;       // km/h
    HeadingDeg  : Double;       // gradi 0..360
    HAccMm      : Cardinal;     // mm — 0 in modalità NMEA
    VAccMm      : Cardinal;     // mm — 0 in modalità NMEA
    SpeedAccMms : Cardinal;     // mm/s — 0 in modalità NMEA
    NumSV       : Byte;
  end;

  { ── Callback ─────────────────────────────────────────────────────────── }

  { Chiamato dal thread seriale — NON dal thread principale.
    L'handler deve essere thread-safe. }
  TOnPVTReceived = procedure(Sender: TObject; const Data: TPVTData) of object;

  { Chiamato dal thread seriale con la stringa NMEA originale (in romNMEA)
    o generata sinteticamente (in romUBX). Include \r\n finale.
    Usato da TNMEAForwarder per scrivere sulla porta virtuale. }
  TOnNMEARawReceived = procedure(Sender: TObject; const Line: string) of object;


  { ── Thread seriale ───────────────────────────────────────────────────── }

  { TUBXRoverThread }

  TUBXRoverThread = class(TLBBaseThread)
  strict private
    type
      TInternalState = (
          is_OpenPort    = 1,   // Connect + Config seriale
          is_Configure   = 2,   // CFG-VALSET + attesa ACK
          is_Running     = 3,   // ExecuteUBXLoop / ExecuteNMEALoop
          is_ResetPort   = 4
        );

    var
      FOnPVT       : TOnPVTReceived;
      FOnNMEARaw   : TOnNMEARawReceived;
      FSerial      : TBlockSerial;
      FOutputMode  : TRoverOutputMode;
      FPort        : string;
      FHz          : Byte;
      FCorrections : TCorrectionBuffer;
      FRxBuf       : TLBCircularBuffer;

      FLastPVT     : TPVTData;
      FPVTLock     : TTimedOutCriticalSection;

    { UBX }
    function  SendUBX(const Msg: TBytes): Boolean;
    function  DecodePVT(const Raw: TUBXNavPVT; out PVT: TPVTData): Boolean;

    { NMEA }
    function  ParseNMEAField(const S: string; Index: Integer): string;
    function  NMEALatLon(const Value, Hemi: string; IsLon: Boolean): Double;
    function  VerifyNMEAChecksum(const Line: string): Boolean;
    function  ParseGGA(const Line: string; out PVT: TPVTData): Boolean;
    function  ParseRMC(const Line: string; var PVT: TPVTData): Boolean;

    { Loop specializzati }
    function ExecuteUBXLoop(): Boolean;
    function ExecuteNMEALoop(): Boolean;

    { Pubblica il PVT verso il rover (lock + callback) }
    procedure PublishPVT(const PVT: TPVTData);

    { Genera GGA + RMC sintetici da TPVTData e chiama OnNMEARaw.
      Usato in romUBX per produrre il flusso NMEA sulla porta virtuale. }
    procedure FireNMEARaw(const PVT: TPVTData);

    function  Configure(ALayer: Byte = VALSET_LAYER_RAM): Boolean;  // use VALSET_LAYER_RAM_FLASH to save into flash (available ~10000 cicles)

    function SendRetrieveData(): Boolean;

    function OpenSerialPort(): Boolean;
    procedure CloseSerialPort();

  protected
    procedure Execute; override;

  public
    constructor Create(const APort: string; AHz: Byte; OutputMode: TRoverOutputMode = romUBX); reintroduce;
    destructor  Destroy; override;

    { Legge l'ultimo PVT in modo thread-safe.
      Restituisce False se nessun dato valido è ancora arrivato. }
    function  GetLastPVT(out PVT: TPVTData): Boolean;

    procedure PushCorrections(const Data: array of Byte; Len: Integer); overload;
    procedure PushCorrections(const Data: TBytes); overload;

    { Chiamato dal thread seriale — l'handler deve essere thread-safe. }
    property OnPVT : TOnPVTReceived     read FOnPVT      write FOnPVT;

    { Stringa NMEA pronta per la porta virtuale (include \r\n finale).
      In romNMEA: righe GGA e RMC originali del modulo.
      In romUBX:  GGA e RMC sintetici generati da TPVTData.
      Chiamato dal thread seriale — l'handler deve essere thread-safe. }
    property OnNMEARaw : TOnNMEARawReceived read FOnNMEARaw  write FOnNMEARaw;

  end;


const
  RTK_QUALITY: array[TRTKStatus] of Byte = (0, 1, 2);

implementation

uses
  ULBLogger, StrUtils;

{ ══ Costanti ═════════════════════════════════════════════════════════════════ }

const
  UBX_ID_NAV_PVT  = $07;
  UBX_PVT_PAYLOAD = 92;
  UBX_PVT_TOTAL   = UBX_HDR_SIZE + UBX_PVT_PAYLOAD + UBX_CRC_SIZE;  // 100

  KEY_USBINPROT_UBX    = $10770001;
  KEY_USBINPROT_NMEA   = $10770002;
  KEY_USBINPROT_RTCM3X = $10770004;

  KEY_USBOUTPROT_UBX    = $10780001;
  KEY_USBOUTPROT_NMEA   = $10780002;
  KEY_USBOUTPROT_RTCM3X = $10780004;

  KEY_RATE_MEAS    = $30210001;
  KEY_RATE_NAV     = $30210002;
  KEY_RATE_TIMEREF = $20210003;

  KEY_MSGOUT_NAVPVT_USB = $20910009;
  KEY_NMEA_GGA_USB      = $209100BD;
  KEY_NMEA_GLL_USB      = $209100CC;
  KEY_NMEA_GSA_USB      = $209100C2;
  KEY_NMEA_GSV_USB      = $209100C7;
  KEY_NMEA_RMC_USB      = $209100AE;
  KEY_NMEA_VTG_USB      = $209100B3;
  KEY_NMEA_ZDA_USB      = $209100DB;
  KEY_NMEA_GNS_USB      = $209100B8;

  { Entrambe le modalità hanno esattamente lo stesso numero di key-value:
      6 bool L × 5 byte =  30   (3 USBINPROT + 3 USBOUTPROT)
      2 U2     × 6 byte =  12   (RATE_MEAS, RATE_NAV)
     10 U1     × 5 byte =  50   (RATE_TIMEREF + NAV-PVT + 8 NMEA MSGOUT)
      header fisso       =   4
      ─────────────────────────────────────
      payload totale     =  96                                                }
  VALSET_PAY_SIZE = VALSET_HDR_SIZE + 92;  // 96

  FIX_TYPE_NAMES   : array[0..5] of string = (
    'NoFix', 'DeadReck', '2D', '3D', 'GNSS+DR', 'TimeOnly');
  RTK_STATUS_NAMES : array[TRTKStatus] of string = ('None', 'Float', 'Fixed');

function FixTypeName(AFixType: Byte): string;
begin
  if AFixType <= 5 then Result := FIX_TYPE_NAMES[AFixType]
  else Result := 'Unknown(' + IntToStr(AFixType) + ')';
end;

{ ══ TUBXRoverThread — UBX ════════════════════════════════════════════════════ }

function TUBXRoverThread.SendUBX(const Msg: TBytes): Boolean;
var
  _Len: Integer;
begin
  Result := False;
  _Len   := Length(Msg);
  if _Len > 0 then
    Result := FSerial.SendBuffer(@Msg[0], _Len) = _Len;
end;

function TUBXRoverThread.DecodePVT(const Raw: TUBXNavPVT;
                                   out PVT: TPVTData): Boolean;
var
  CarrSoln: Byte;
begin
  Result := False;
  FillChar(PVT, SizeOf(PVT), 0);
  PVT.FixType   := TFixType(Raw.fixType);
  PVT.GNSSFixOK := (Raw.flags and $01) <> 0;
  if (not PVT.GNSSFixOK) or (Raw.fixType < 2) then Exit;

  CarrSoln := (Raw.flags shr 6) and $03;
  case CarrSoln of
    1: PVT.RTKStatus := rtkFloat;
    2: PVT.RTKStatus := rtkFixed;
  else PVT.RTKStatus := rtkNone;
  end;

  if (Raw.valid and $03) = $03 then
    PVT.UTCTime := EncodeDate(Raw.year, Raw.month, Raw.day) +
                   EncodeTime(Raw.hour, Raw.min, Raw.sec, 0);

  PVT.Latitude    := Raw.lat     / 1.0e7;
  PVT.Longitude   := Raw.lon     / 1.0e7;
  PVT.AltitudeMSL := Raw.hMSL    / 1000.0;
  PVT.SpeedKmH    := Raw.gSpeed  / 1000.0 * 3.6;
  PVT.HeadingDeg  := Raw.headMot / 1.0e5;
  PVT.HAccMm      := Raw.hAcc;
  PVT.VAccMm      := Raw.vAcc;
  PVT.SpeedAccMms := Raw.sAcc;
  PVT.NumSV       := Raw.numSV;
  PVT.Valid       := True;
  Result          := True;
end;

{ ══ TUBXRoverThread — NMEA ═══════════════════════════════════════════════════ }

function TUBXRoverThread.ParseNMEAField(const S: string;
                                        Index: Integer): string;
{ Restituisce il campo Index (0-based dal primo campo dopo l'ID sentenza).
  Esempio: per $GNGGA,123519,... Index=0 → '123519'. }
var
  i, Cur, _Start: Integer;

begin
  Result := '';
  Cur    := 0;
  _Start  := 2;  // salta '$'
  for i := 2 to Length(S) do
  begin
    if S[i] = ',' then
    begin
      if Cur = Index then
      begin
        Result := Copy(S, _Start, i - _Start);
        Exit;
      end;
      Inc(Cur);
      _Start := i + 1;
    end;
  end;
  if Cur = Index then
    Result := Copy(S, _Start, Length(S) - _Start + 1);
end;

function TUBXRoverThread.NMEALatLon(const Value, Hemi: string;
                                    IsLon: Boolean): Double;
{ Converte DDMM.MMMMM (lat) o DDDMM.MMMMM (lon) + emisferio in gradi decimali. }
var
  FS  : TFormatSettings;
  Deg : Integer;
  Min : Double;
begin
  Result := 0.0;
  if (Value = '') or (Hemi = '') then Exit;
  FS := TFormatSettings.Create('en-US');
  if IsLon then
  begin
    Deg := StrToIntDef(Copy(Value, 1, 3), 0);
    Min := StrToFloatDef(Copy(Value, 4, Length(Value)), 0.0, FS);
  end
  else
  begin
    Deg := StrToIntDef(Copy(Value, 1, 2), 0);
    Min := StrToFloatDef(Copy(Value, 3, Length(Value)), 0.0, FS);
  end;
  Result := Deg + Min / 60.0;
  if (Hemi = 'S') or (Hemi = 'W') then Result := -Result;
end;

function TUBXRoverThread.VerifyNMEAChecksum(const Line: string): Boolean;
{ Verifica checksum XOR tra '$' e '*'. Line deve includere '*XX'. }
var
  i, CS, Got, StarPos: Integer;
begin
  Result  := False;
  StarPos := 0;
  for i := Length(Line) downto 1 do
    if Line[i] = '*' then begin StarPos := i; Break; end;
  if StarPos = 0 then Exit;
  Got := StrToIntDef('$' + Copy(Line, StarPos + 1, 2), -1);
  if Got < 0 then Exit;
  CS := 0;
  for i := 2 to StarPos - 1 do
    CS := CS xor Ord(Line[i]);
  Result := CS = Got;
end;

function TUBXRoverThread.ParseGGA(const Line: string;
                                  out PVT: TPVTData): Boolean;
{
  $GP/GNGGA — campi (0-based dopo talker ID):
    0=UTC  1=Lat  2=N/S  3=Lon  4=E/W  5=Quality  6=NumSV  7=HDOP
    8=AltMSL  9=M  10=Geoid  11=M  12=AgeCorr  13=StaID

  Quality NMEA GGA:
    0 = no fix          → rifiutato
    1 = autonomous GPS  → rtkNone
    2 = DGPS            → rtkNone
    4 = RTK fixed       → rtkFixed
    5 = RTK float       → rtkFloat
    6 = estimated DR    → rtkNone
}
var
  FS      : TFormatSettings;
  Quality : Integer;
  TimeStr : string;
  H, M, S : Integer;
begin
  Result := False;
  FillChar(PVT, SizeOf(PVT), 0);
  FS := TFormatSettings.Create('en-US');

  Quality := StrToIntDef(ParseNMEAField(Line, 5), 0);
  if Quality = 0 then Exit;

  PVT.GNSSFixOK := True;
  PVT.FixType   := ft3D;
  case Quality of
    4: PVT.RTKStatus := rtkFixed;
    5: PVT.RTKStatus := rtkFloat;
  else PVT.RTKStatus := rtkNone;
  end;

  PVT.Latitude    := NMEALatLon(ParseNMEAField(Line, 1),
                                 ParseNMEAField(Line, 2), False);
  PVT.Longitude   := NMEALatLon(ParseNMEAField(Line, 3),
                                 ParseNMEAField(Line, 4), True);
  PVT.AltitudeMSL := StrToFloatDef(ParseNMEAField(Line, 8), 0.0, FS);
  PVT.NumSV       := StrToIntDef(ParseNMEAField(Line, 6), 0);

  { Ora UTC (data assente in GGA: verrà completata da RMC) }
  TimeStr := ParseNMEAField(Line, 0);
  if Length(TimeStr) >= 6 then
  begin
    H := StrToIntDef(Copy(TimeStr, 1, 2), 0);
    M := StrToIntDef(Copy(TimeStr, 3, 2), 0);
    S := StrToIntDef(Copy(TimeStr, 5, 2), 0);
    PVT.UTCTime := EncodeTime(H, M, S, 0);
  end;

  PVT.Valid := True;
  Result    := True;
end;

function TUBXRoverThread.ParseRMC(const Line: string;
                                  var PVT: TPVTData): Boolean;
{
  $GP/GNRMC — campi (0-based dopo talker ID):
    0=UTC  1=Status(A/V)  2=Lat  3=N/S  4=Lon  5=E/W
    6=SpeedKn  7=CourseTrue  8=DateDDMMYY  9=MagVar  10=E/W  11=Mode
}
var
  FS      : TFormatSettings;
  DateStr : string;
  SpeedKn : Double;
  Day, Mon, Yr: Integer;
begin
  Result := False;
  FS := TFormatSettings.Create('en-US');
  if ParseNMEAField(Line, 1) <> 'A' then Exit;

  SpeedKn      := StrToFloatDef(ParseNMEAField(Line, 6), 0.0, FS);
  PVT.SpeedKmH := SpeedKn * 1.852;
  PVT.HeadingDeg := StrToFloatDef(ParseNMEAField(Line, 7), 0.0, FS);

  { Data DDMMYY — completa UTCTime preservando l'ora di GGA }
  DateStr := ParseNMEAField(Line, 8);
  if Length(DateStr) = 6 then
  begin
    Day := StrToIntDef(Copy(DateStr, 1, 2), 1);
    Mon := StrToIntDef(Copy(DateStr, 3, 2), 1);
    Yr  := 2000 + StrToIntDef(Copy(DateStr, 5, 2), 0);
    PVT.UTCTime := EncodeDate(Yr, Mon, Day) + Frac(PVT.UTCTime);
  end;

  Result := True;
end;

{ ══ TUBXRoverThread — Configure ══════════════════════════════════════════════ }

function TUBXRoverThread.Configure(ALayer: Byte): Boolean;
{
  Costruisce un CFG-VALSET con 18 key-value (payload 96 byte).
  Il payload è identico per entrambe le modalità — cambiano solo i valori
  di OUTPROT_UBX, OUTPROT_NMEA, NAV-PVT, GGA, RMC.

  romUBX:
    OUTPROT UBX=On NMEA=Off  |  NAV-PVT=1, GGA=0, RMC=0  (altri off)
  romNMEA:
    OUTPROT UBX=Off NMEA=On  |  NAV-PVT=0, GGA=1, RMC=1  (altri off)
}
var
  Payload    : TBytes;
  Pos        : Integer;
  MeasRateMs : Word;
  IsNMEA     : Boolean;
begin
  Result     := False;
  IsNMEA     := (FOutputMode = romNMEA);
  MeasRateMs := 1000 div FHz;

  SetLength(Payload, VALSET_PAY_SIZE);
  FillChar(Payload[0], VALSET_PAY_SIZE, 0);
  Payload[0] := $00;
  Payload[1] := ALayer;
  Pos := VALSET_HDR_SIZE;

  { ── Input USB: identico in entrambe le modalità ─────────────────────── }
  AddBool(Payload, Pos, KEY_USBINPROT_UBX,    True);
  AddBool(Payload, Pos, KEY_USBINPROT_NMEA,   False);
  AddBool(Payload, Pos, KEY_USBINPROT_RTCM3X, True);

  { ── Output USB: dipende dalla modalità ──────────────────────────────── }
  AddBool(Payload, Pos, KEY_USBOUTPROT_UBX,    not IsNMEA);
  AddBool(Payload, Pos, KEY_USBOUTPROT_NMEA,   IsNMEA);
  AddBool(Payload, Pos, KEY_USBOUTPROT_RTCM3X, False);

  { ── Frequenza ────────────────────────────────────────────────────────── }
  AddU2(Payload, Pos, KEY_RATE_MEAS,    MeasRateMs);
  AddU2(Payload, Pos, KEY_RATE_NAV,     1);
  AddU1(Payload, Pos, KEY_RATE_TIMEREF, 1);

  { ── Output messaggi ─────────────────────────────────────────────────── }
  AddU1(Payload, Pos, KEY_MSGOUT_NAVPVT_USB, Byte(not IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_GGA_USB,      Byte(IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_GLL_USB,      Byte(IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_GSA_USB,      Byte(IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_GSV_USB,      Byte(IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_RMC_USB,      Byte(IsNMEA));
  AddU1(Payload, Pos, KEY_NMEA_VTG_USB,      0);
  AddU1(Payload, Pos, KEY_NMEA_ZDA_USB,      0);
  AddU1(Payload, Pos, KEY_NMEA_GNS_USB,      0);

  if Pos <> VALSET_PAY_SIZE then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Error, 'VALSET byte count mismatch: expected %d, got %d', [VALSET_PAY_SIZE, Pos]);
    Exit;
  end;

  if not SendUBX(BuildUBX(UBX_CLASS_CFG, UBX_ID_CFG_VALSET, Payload)) then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Error,
      'Send error on CFG-VALSET (layer=$%s mode=%s port=<%s>)',
      [IntToHex(ALayer, 2), IfThen(IsNMEA, 'NMEA', 'UBX'), FPort]);
    Exit;
  end;

  Result := WaitACK(FSerial, UBX_CLASS_CFG, UBX_ID_CFG_VALSET, 700);

  if Result then
    LBLogger.Write(5, 'TUBXRoverThread.Configure', lmt_Info,
      'CFG-VALSET ACK (layer=$%s mode=%s rate=%d Hz)',
      [IntToHex(ALayer, 2), IfThen(IsNMEA, 'NMEA', 'UBX'), FHz])
  else
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Warning,
      'CFG-VALSET: no ACK (layer=$%s mode=%s) — check firmware',
      [IntToHex(ALayer, 2), IfThen(IsNMEA, 'NMEA', 'UBX')]);
end;

function TUBXRoverThread.SendRetrieveData(): Boolean;
var
  _CorrLen  : Integer;
  _CorrData : TBytes;

begin
  Result := True;

  case FRxBuf.WriteFromSerial(FSerial) of
    -1 : Exit(False);
    0  : Self.PauseFor(5);
  end;

  _CorrLen := FCorrections.Flush(_CorrData);
  if _CorrLen > 0 then
  begin
    if FSerial.SendBuffer(@_CorrData[0], _CorrLen) <> _CorrLen then
      LBLogger.Write(1, 'TUBXRoverThread.SendRetrieveData', lmt_Warning, 'RTCM3 send incomplete (%d bytes)', [_CorrLen]);
  end;

end;

function TUBXRoverThread.OpenSerialPort(): Boolean;
begin
  Result := False;

  Self.CloseSerialPort();

  if FPort = '' then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.OpenSerialPort', lmt_Warning, 'Serial port not set!');
    Exit;
  end;

  if (FHz = 0) or (FHz > 10) then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.OpenSerialPort', lmt_Warning, 'Wrong frequency value %d!', [FHz]);
    Exit;
  end;

  FSerial := TBlockSerial.Create;
  FSerial.Connect(FPort);
  if FSerial.LastError = 0 then
  begin
    FSerial.Config(115200, 8, 'N', SB1, False, False);
    Result := FSerial.LastError = 0;
  end;

  if not Result then
    LBLogger.Write(1, 'TUBXRoverThread.OpenSerialPort', lmt_Warning, 'Error opening serial port: %s', [FSerial.LastErrorDesc]);
end;

procedure TUBXRoverThread.CloseSerialPort();
begin
  if FSerial <> nil then
  begin
    FSerial.CloseSocket;
    FreeAndNil(FSerial);
  end;
end;

{ ══ TUBXRoverThread — PublishPVT ═════════════════════════════════════════════ }

procedure TUBXRoverThread.PublishPVT(const PVT: TPVTData);
begin
  if FPVTLock.Acquire('TUBXRoverThread.PublishPVT') then
  begin
    try
      FLastPVT := PVT;
    finally
      FPVTLock.Release;
    end;
  end;

  if Assigned(FOnPVT) then
    FOnPVT(Self, PVT);

  { In modalità UBX il callback NMEA richiede la generazione sintetica
    di GGA + RMC a partire dai dati decodificati. In modalità NMEA le
    stringhe originali vengono già passate direttamente da FireNMEARaw. }
  if Assigned(FOnNMEARaw) and (FOutputMode = romUBX) then
    Self.FireNMEARaw(PVT);
end;

procedure TUBXRoverThread.FireNMEARaw(const PVT: TPVTData);
{ Costruisce GGA + RMC sintetici a partire da TPVTData e li consegna
  a OnNMEARaw. Usato in modalità romUBX quando il modulo emette binario
  ma l'applicazione vuole anche il flusso NMEA sulla porta virtuale.  }
var
  FS                   : TFormatSettings;
  LatDeg, LonDeg       : Integer;
  LatMin, LonMin       : Double;
  LatH, LonH           : Char;
  Lat, Lon             : Double;
  TimeStr, DateStr     : string;
  QualityStr           : string;
  GGABody, RMCBody     : string;
  SpeedKn              : Double;

  function CS(const Body: string): string;
  var i, X: Integer;
  begin X := 0; for i := 1 to Length(Body) do X := X xor Ord(Body[i]);
    Result := IntToHex(X, 2);
  end;

begin
  if not Assigned(FOnNMEARaw) then Exit;
  FS := TFormatSettings.Create('en-US');

  TimeStr := FormatDateTime('hhnnss.zzz', Frac(PVT.UTCTime), FS);
  DateStr := FormatDateTime('ddmmyy', Int(PVT.UTCTime), FS);

  Lat := Abs(PVT.Latitude);
  Lon := Abs(PVT.Longitude);
  LatH := IfThen(PVT.Latitude  >= 0, 'N', 'S')[1];
  LonH := IfThen(PVT.Longitude >= 0, 'E', 'W')[1];
  LatDeg := Trunc(Lat); LatMin := (Lat - LatDeg) * 60.0;
  LonDeg := Trunc(Lon); LonMin := (Lon - LonDeg) * 60.0;

  case PVT.RTKStatus of
    rtkFixed: QualityStr := '4';
    rtkFloat: QualityStr := '5';
  else        QualityStr := '1';
  end;

  { ── GGA ─────────────────────────────────────────────────────────────── }
  GGABody := Format(
    'GNGGA,%s,%02d%08.5f,%s,%03d%08.5f,%s,%s,%02d,1.0,%.3f,M,0.000,M,,',
    [TimeStr,
     LatDeg, LatMin, LatH,
     LonDeg, LonMin, LonH,
     QualityStr,
     PVT.NumSV,
     PVT.AltitudeMSL], FS);
  FOnNMEARaw(Self, '$' + GGABody + '*' + CS(GGABody) + #13#10);

  { ── RMC ─────────────────────────────────────────────────────────────── }
  SpeedKn := PVT.SpeedKmH / 1.852;
  RMCBody := Format(
    'GNRMC,%s,A,%02d%08.5f,%s,%03d%08.5f,%s,%.3f,%.2f,%s,,,A',
    [TimeStr,
     LatDeg, LatMin, LatH,
     LonDeg, LonMin, LonH,
     SpeedKn,
     PVT.HeadingDeg,
     DateStr], FS);
  FOnNMEARaw(Self, '$' + RMCBody + '*' + CS(RMCBody) + #13#10);
end;

{ ══ TUBXRoverThread — ExecuteUBXLoop ════════════════════════════════════════ }

function TUBXRoverThread.ExecuteUBXLoop: Boolean;
var
  SyncPat  : array[0..1] of Byte;
  SyncPos  : Integer;
  PayLen   : Word;
  FrameBuf : array[0..UBX_PVT_TOTAL - 1] of Byte;
  PVT      : TUBXNavPVT;
  PVTData  : TPVTData;
  _tmp     : TBytes;

begin
  Result := True;

  SyncPat[0] := UBX_SYNC1;
  SyncPat[1] := UBX_SYNC2;
  FRxBuf.Clear;

  while not Self.Terminated do
  begin
    if not Self.SendRetrieveData() then
      Exit(False);

    while FRxBuf.AvailableForRead >= UBX_PVT_TOTAL do
    begin
      SyncPos := FRxBuf.FindPattern(@SyncPat[0], 2, 0);
      if SyncPos < 0 then
      begin
        SetLength(_tmp, FRxBuf.AvailableForRead);
        FRxBuf.Read(@_tmp[0], Length(_tmp));
        LBLogger.Write(5, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Debug, 'No UBX sync, clearing %d bytes: %s', [Length(_tmp), HexString(@_tmp[0], Length(_tmp))]);
        FRxBuf.Clear;
        Break;
      end;
      if SyncPos > 0 then
      begin
        LBLogger.Write(5, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Debug, 'Skipping %d spurious bytes before sync', [SyncPos]);
        FRxBuf.Skip(SyncPos);
      end;
      if FRxBuf.AvailableForRead < UBX_HDR_SIZE then Break;
      PayLen := FRxBuf.PeekByte(4) or (Word(FRxBuf.PeekByte(5)) shl 8);
      if FRxBuf.AvailableForRead < UBX_HDR_SIZE + PayLen + UBX_CRC_SIZE then
        Break;
      if (FRxBuf.PeekByte(2) = UBX_CLASS_NAV) and
         (FRxBuf.PeekByte(3) = UBX_ID_NAV_PVT) and
         (PayLen = UBX_PVT_PAYLOAD) then
      begin
        FRxBuf.Read(@FrameBuf[0], UBX_PVT_TOTAL);
        if ValidUBXChecksum(FrameBuf, 0, PayLen) then
        begin
          Move(FrameBuf[UBX_HDR_SIZE], PVT, UBX_PVT_PAYLOAD);
          if DecodePVT(PVT, PVTData) then
          begin
            LBLogger.Write(5, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Info,
              'PVT fix=%s rtk=%s sv=%d lat=%.7f lon=%.7f alt=%.1fm hacc=%dmm',
              [FixTypeName(Byte(PVTData.FixType)),
               RTK_STATUS_NAMES[PVTData.RTKStatus],
               PVTData.NumSV, PVTData.Latitude, PVTData.Longitude,
               PVTData.AltitudeMSL, PVTData.HAccMm]);
            PublishPVT(PVTData);
          end
          else
            LBLogger.Write(3, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Debug,
              'PVT discarded: fixType=%s gnssFixOK=%s',
              [FixTypeName(PVT.fixType),
               BoolToStr((PVT.flags and $01) <> 0, True)]);
        end
        else
          LBLogger.Write(1, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Warning,
            'NAV-PVT checksum mismatch — frame discarded');
      end
      else
      begin
        LBLogger.Write(5, 'TUBXRoverThread.ExecuteUBXLoop', lmt_Debug,
          'Non-PVT frame skipped (class=$%s id=$%s len=%d)',
          [IntToHex(FRxBuf.PeekByte(2), 2),
           IntToHex(FRxBuf.PeekByte(3), 2), PayLen]);
        FRxBuf.Skip(UBX_HDR_SIZE + PayLen + UBX_CRC_SIZE);
      end;
    end;
  end;
end;

{ ══ TUBXRoverThread — ExecuteNMEALoop ═══════════════════════════════════════ }

function TUBXRoverThread.ExecuteNMEALoop: Boolean;
{
  Legge righe NMEA ASCII da FSerial carattere per carattere.
  Accumula GGA e RMC: quando arriva RMC valido dopo un GGA valido,
  costruisce TPVTData e chiama PublishPVT.

  Strategia di abbinamento:
  - GGA porta posizione, altitudine, qualità, NumSV, ora UTC.
  - RMC porta velocità, heading, data completa.
  - Si pubblica a ogni RMC valido usando l'ultimo GGA ricevuto.
    Al frame rate tipico (1–5 Hz) GGA e RMC arrivano consecutivi
    nella stessa epoca, quindi l'abbinamento è corretto.
  - Se non è ancora arrivato un GGA, RMC viene ignorato.
}
var
  DollarPos : Integer;
  LFPos     : Integer;
  SentLen   : Cardinal;
  RawBuf    : array[0..255] of Byte;  // max NMEA 82 char + terminatori
  RawLine   : string;
  ParseLine : string;
  GGAData   : TPVTData;
  HaveGGA   : Boolean;
  PVTData   : TPVTData;
  Talker    : string;
  i         : Integer;
begin
  Result := True;
  FillChar(GGAData, SizeOf(GGAData), 0);
  HaveGGA := False;
  FRxBuf.Clear;

  while not Terminated do
  begin
    if not Self.SendRetrieveData() then
      Exit(False);

    { Estrazione sentence complete }
    while True do
    begin
      { 1. Trova '$' — inizio sentence, scarta garbage precedente }
      DollarPos := FRxBuf.FindByte(Ord('$'), 0);
      if DollarPos < 0 then begin FRxBuf.Clear; Break; end;
      if DollarPos > 0 then FRxBuf.Skip(DollarPos);

      { 2. Trova LF — fine sentence }
      LFPos := FRxBuf.FindByte(10, 1);
      if LFPos < 0 then Break; // sentence incompleta, aspetta altri dati

      SentLen := LFPos + 1;
      if SentLen > SizeOf(RawBuf) then
      begin
        { Sentence anomala: salta il '$' e riprova }
        FRxBuf.Skip(1);
        Continue;
      end;

      { 3. Estrae i byte raw e consuma dal buffer }
      FRxBuf.Read(@RawBuf[0], SentLen);

      { 4. Costruisce la stringa raw esattamente com'è nel buffer }
      SetLength(RawLine, SentLen);
      for i := 1 to SentLen do
        RawLine[i] := Chr(RawBuf[i - 1]);

      { 5. FORWARD RAW — nessuna modifica, include i terminatori originali }
      if Assigned(FOnNMEARaw) then
        FOnNMEARaw(Self, RawLine);

      { 6. Normalizza solo per il parser interno }
      // ParseLine := Trim(RawLine);
      if (Length(RawLine) > 6) and (RawLine[1] = '$') then
      begin
        if not VerifyNMEAChecksum(RawLine) then
        begin
          LBLogger.Write(2, 'TUBXRoverThread.ExecuteNMEALoop', lmt_Warning, 'NMEA checksum error: %s', [RawLine]);
          Continue;
        end;
        ParseLine := Copy(RawLine, 1, Pos('*', RawLine) - 1);
        Talker := Copy(RawLine, 3, 3);

        if Talker = 'GGA' then
        begin
          if ParseGGA(ParseLine, GGAData) then
            HaveGGA := True;
        end
        else if Talker = 'RMC' then
        begin
          if HaveGGA then
          begin
            PVTData := GGAData;
            if ParseRMC(ParseLine, PVTData) then
              Self.PublishPVT(PVTData);
          end;
        end;
      end;
    end; // inner while

  end; // outer while
end;

{ ══ TUBXRoverThread — InternalExecute ════════════════════════════════════════ }

procedure TUBXRoverThread.Execute;
var
  _InternalState : TInternalState = is_OpenPort;

begin
  while not Self.Terminated do
  begin
    case _InternalState of
      is_OpenPort:
        begin
          if Self.OpenSerialPort() then
            _InternalState := is_Configure
          else
            Self.PauseFor(5000);
        end;

      is_Configure:
        begin
          if Self.Configure(VALSET_LAYER_RAM) then
            _InternalState := is_Running
          else
            _InternalState := is_ResetPort;
        end;

      is_Running:
        begin
          case FOutputMode of
            romUBX:
              begin
                if not Self.ExecuteUBXLoop then
                  _InternalState := is_ResetPort;
              end;

            romNMEA:
              begin
                if not Self.ExecuteNMEALoop then
                  _InternalState := is_ResetPort;
              end;
          end;
        end;

      is_ResetPort:
        begin
          _InternalState := is_OpenPort;
          Self.CloseSerialPort();
          Self.PauseFor(5000);
        end;
    end;
  end;

  LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Info, 'Thread terminated on port <%s>', [FPort]);
end;

{ ══ TUBXRoverThread — lifecycle ══════════════════════════════════════════════ }

constructor TUBXRoverThread.Create(const APort: string; AHz: Byte; OutputMode: TRoverOutputMode);
begin
  inherited Create();

  FreeOnTerminate := False;

  FPort := APort;
  FHz := AHz;
  FOutputMode := OutputMode;
  FSerial := nil;

  FCorrections := TCorrectionBuffer.Create;

  FRxBuf := TLBCircularBuffer.Create(4096);

  FPVTLock := TTimedOutCriticalSection.Create;
  FillChar(FLastPVT, SizeOf(FLastPVT), 0);
end;

destructor TUBXRoverThread.Destroy;
begin
  inherited Destroy;
  Self.CloseSerialPort();

  FreeAndNil(FPVTLock);
  FreeAndNil(FCorrections);
  FreeAndNil(FRxBuf);
end;

function TUBXRoverThread.GetLastPVT(out PVT: TPVTData): Boolean;
begin
  Result := False;
  if FPVTLock.Acquire('TUBXRoverThread.GetLastPVT') then
  begin
    try
      PVT    := FLastPVT;
      Result := FLastPVT.Valid;
    finally
      FPVTLock.Release;
    end;
  end;
end;

procedure TUBXRoverThread.PushCorrections(const Data: array of Byte; Len: Integer);
begin
  FCorrections.Push(Data, Len);
end;

procedure TUBXRoverThread.PushCorrections(const Data: TBytes);
begin
  FCorrections.Push(Data);
end;


end.
