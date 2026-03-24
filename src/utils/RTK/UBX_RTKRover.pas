unit UBX_RTKRover;

{
  Gestione rover simpleRTK2B (ZED-F9P) su thread dedicato.
  Protocollo: UBX binario (NAV-PVT) — nessun parsing NMEA.

  ── Configurazione automatica all'avvio ──────────────────────────────────
  All'apertura della porta il thread chiama Configure(VALSET_LAYER_RAM):
  in un singolo messaggio CFG-VALSET vengono impostati:
    - Output USB: solo UBX (NMEA e RTCM3X disabilitati)
    - Input  USB: UBX + RTCM3X (per le correzioni), NMEA disabilitato
    - Frequenza di navigazione (Hz configurabile)
    - NAV-PVT abilitato su USB
  La configurazione viene applicata solo alla RAM: è persa al riavvio
  ma non consuma cicli flash. Chiamare SaveConfig per renderla permanente.

  ── Layer di configurazione ───────────────────────────────────────────────
  Configure accetta un parametro ALayer (da UBXProtocol):

    VALSET_LAYER_RAM        Solo RAM — persa al riavvio (default)
    VALSET_LAYER_RAM_FLASH  RAM + Flash — permanente, consuma cicli flash
    VALSET_LAYER_FLASH      Solo Flash — utile per aggiornare la config
                            permanente senza modificare la RAM corrente

  SaveConfig è una scorciatoia che chiama Configure(VALSET_LAYER_RAM_FLASH).

  ── Consegna dati PVT senza Synchronize ──────────────────────────────────
  OnPVT viene chiamato dal thread seriale direttamente, senza rimbalzare
  sul thread principale. Il chiamante deve gestire la thread-safety del
  proprio handler (es. accodare il dato e processarlo nel thread UI).
  Per proteggere la lettura/scrittura del record condiviso (FLastPVT)
  viene usata una CriticalSection: il thread scrive sotto lock, il
  consumer legge con GetLastPVT sotto lo stesso lock.

  ── Correzioni RTCM3 ─────────────────────────────────────────────────────
  Le correzioni ricevute dall'esterno (es. via NTRIP) vengono passate
  tramite PushCorrections e inviate al modulo nel loop del thread.
  Il buffer usa semantica last-write-wins (i dati RTCM3 obsoleti
  sono inutili; si preferisce sempre il blocco più recente).

  ── Dipendenze ───────────────────────────────────────────────────────────
  UBXProtocol (framing e helper VALSET), SynaSer (Synapse),
  uLBCircularBuffer, RTKTypes (TCorrectionBuffer, TRTKStatus),
  uLBBaseThread, ULBLogger.
}

interface

uses
  SysUtils, Classes, SynaSer, uLBCircularBuffer, uLBUtils,
  RTKTypes, UBXProtocol, uLBBaseThread, uTimedoutCriticalSection;

type

  { ── Payload UBX-NAV-PVT (92 byte, packed) ───────────────────────────────
    Ref: u-blox F9 HPG 1.51 Interface Description §3.15.13
         UBXDOC-963802114-13124 — verificato offset per offset.          }

  TFixType = (
    ftNoFix    = 0,
    ftDeadReck = 1,
    ft2D       = 2,
    ft3D       = 3,
    ftGNSSDR   = 4,
    ftTimeOnly = 5
  );

  TUBXNavPVT = packed record
    iTOW      : UInt32;              // offset  0  U4  GPS time of week (ms)
    year      : UInt16;              // offset  4  U2  Year (UTC)
    month     : Byte;                // offset  6  U1  Month 1..12
    day       : Byte;                // offset  7  U1  Day 1..31
    hour      : Byte;                // offset  8  U1  Hour 0..23
    min       : Byte;                // offset  9  U1  Minute 0..59
    sec       : Byte;                // offset 10  U1  Second 0..60
    valid     : Byte;                // offset 11  X1  Validity flags
                                     //                bit0=validDate, bit1=validTime
    tAcc      : UInt32;              // offset 12  U4  Time accuracy estimate (ns)
    nano      : Int32;               // offset 16  I4  Fraction of second (ns)
    fixType   : Byte;                // offset 20  U1  GNSS fix type (0..5)
    flags     : Byte;                // offset 21  X1  Fix flags
                                     //                bit0=gnssFixOK, bits7:6=carrSoln
    flags2    : Byte;                // offset 22  X1  Additional flags
    numSV     : Byte;                // offset 23  U1  Number of satellites used
    lon       : Int32;               // offset 24  I4  Longitude (deg * 1e-7)
    lat       : Int32;               // offset 28  I4  Latitude  (deg * 1e-7)
    height    : Int32;               // offset 32  I4  Height above ellipsoid (mm)
    hMSL      : Int32;               // offset 36  I4  Height above mean sea level (mm)
    hAcc      : UInt32;              // offset 40  U4  Horizontal accuracy estimate (mm)
    vAcc      : UInt32;              // offset 44  U4  Vertical accuracy estimate (mm)
    velN      : Int32;               // offset 48  I4  NED north velocity (mm/s)
    velE      : Int32;               // offset 52  I4  NED east velocity (mm/s)
    velD      : Int32;               // offset 56  I4  NED down velocity (mm/s)
    gSpeed    : Int32;               // offset 60  I4  Ground speed 2-D (mm/s)
    headMot   : Int32;               // offset 64  I4  Heading of motion 2-D (deg * 1e-5)
    sAcc      : UInt32;              // offset 68  U4  Speed accuracy estimate (mm/s)
    headAcc   : UInt32;              // offset 72  U4  Heading accuracy estimate (deg * 1e-5)
    pDOP      : UInt16;              // offset 76  U2  Position DOP (* 0.01)
    flags3    : UInt16;              // offset 78  X2  Additional flags
                                     //                bit0=invalidLlh, bits4:1=lastCorrection
    reserved0 : array[0..3] of Byte; // offset 80  U1[4]  Reserved
    headVeh   : Int32;               // offset 84  I4  Heading of vehicle 2-D (deg * 1e-5)
    magDec    : Int16;               // offset 88  I2  Magnetic declination (deg * 1e-2)
    magAcc    : UInt16;              // offset 90  U2  Magnetic declination accuracy (deg * 1e-2)
  end;                               // totale: 92 byte ✓

  { ── Dati PVT decodificati ───────────────────────────────────────────────  }

  TPVTData = record
    Valid       : Boolean;
    FixType     : TFixType;
    RTKStatus   : TRTKStatus;
    GNSSFixOK   : Boolean;      // fix utilizzabile (gnssFixOK = 1)
    UTCTime     : TDateTime;    // valido solo se validDate e validTime = 1
    Latitude    : Double;       // gradi decimali, + = Nord
    Longitude   : Double;       // gradi decimali, + = Est
    AltitudeMSL : Double;       // altezza sul livello del mare (m)
    SpeedKmH    : Double;       // velocità al suolo (km/h)
    HeadingDeg  : Double;       // direzione di movimento (gradi 0..360)
    HAccMm      : Cardinal;     // accuratezza orizzontale (mm)
    VAccMm      : Cardinal;     // accuratezza verticale (mm)
    SpeedAccMms : Cardinal;     // accuratezza velocità (mm/s)
    NumSV       : Byte;         // satelliti utilizzati
  end;

  { ── Tipo callback ────────────────────────────────────────────────────── }

  { Chiamato dal thread seriale — NON dal thread principale.
    L'handler deve essere thread-safe: accodare il dato e processarlo
    nel thread UI, oppure usare GetLastPVT per leggere l'ultimo valore
    in modo sincronizzato. }
  TOnPVTReceived = procedure(Sender: TObject; const Data: TPVTData) of object;

  { ── Forward ──────────────────────────────────────────────────────────── }

  TUBXRTKRover = class;

  { ── Thread seriale ───────────────────────────────────────────────────── }

  TUBXRoverThread = class(TLBBaseThread)
  strict private
    FRover    : TUBXRTKRover;
    FSerial   : TBlockSerial;

    { Invia un frame UBX già costruito sulla porta seriale.
      Restituisce True se tutti i byte sono stati inviati. }
    function  SendUBX(const Msg: TBytes): Boolean;

    { Decodifica un payload NAV-PVT grezzo in TPVTData.
      Restituisce False se il fix non è utilizzabile. }
    function  DecodePVT(const Raw: TUBXNavPVT; out PVT: TPVTData): Boolean;

  private
    { Configura il modulo via CFG-VALSET.
      ALayer: destinazione della configurazione — usare le costanti
      VALSET_LAYER_* definite in UBXProtocol:
        VALSET_LAYER_RAM        solo RAM, persa al riavvio (default)
        VALSET_LAYER_RAM_FLASH  RAM + Flash, permanente
      Restituisce True se il modulo ha risposto con ACK. }
    function  Configure(ALayer: Byte = VALSET_LAYER_RAM): Boolean;

  protected
    procedure InternalExecute; override;

  public
    constructor Create(ARover: TUBXRTKRover; const APort: string; AHz: Byte); reintroduce;
    destructor  Destroy; override;
    function    SerialOK: Boolean;
    function    SerialErrorDesc: string;
  end;

  { ── Classe pubblica ──────────────────────────────────────────────────── }

  TUBXRTKRover = class(TObject)
  strict private
    FThread      : TUBXRoverThread;
    FCorrections : TCorrectionBuffer;
    FPort        : string;
    FHz          : Byte;
    FActive      : Boolean;
    FOnPVT       : TOnPVTReceived;


  private
    { Protezione di FLastPVT: scritto dal thread seriale, letto
      dal thread chiamante tramite GetLastPVT. }
    FPVTLock     : TTimedOutCriticalSection;
    FLastPVT     : TPVTData;

  public
    constructor Create(const APort: string; AHz: Byte = 5);
    destructor  Destroy; override;

    { Apre la porta e avvia il thread seriale.
      Restituisce True se la porta è stata aperta correttamente. }
    function  Open: Boolean;
    procedure Close;

    { Rende permanente la configurazione corrente scrivendola in Flash.
      Internamente chiama Configure(VALSET_LAYER_RAM_FLASH).
      ATTENZIONE: la flash del ZED-F9P ha ~10.000 cicli di scrittura.
      Chiamare solo quando la configurazione è definitiva e verificata. }
    function SaveConfig: Boolean;

    { Legge l'ultimo record PVT ricevuto in modo thread-safe.
      Restituisce False se non è ancora stato ricevuto nessun dato valido. }
    function  GetLastPVT(out PVT: TPVTData): Boolean;

    procedure PushCorrections(const Data: array of Byte; Len: Integer); overload;
    procedure PushCorrections(const Data: TBytes); overload;

    property Active      : Boolean           read FActive;
    property Port        : string            read FPort        write FPort;
    property Hz          : Byte              read FHz          write FHz;
    property Corrections : TCorrectionBuffer read FCorrections;

    { OnPVT: chiamato dal thread seriale — l'handler deve essere thread-safe.
      Per leggere l'ultimo dato dal thread principale usare GetLastPVT. }
    property OnPVT       : TOnPVTReceived    read FOnPVT       write FOnPVT;
  end;

const
  { Qualità derivata da FixType + RTKStatus, pronta per il client:
    0 = GNSS standalone  (~1–3 m)
    1 = RTK Float        (~0.2–0.5 m)
    2 = RTK Fixed        (~1–2 cm)        }
  RTK_QUALITY: array[TRTKStatus] of Byte = (0, 1, 2);



implementation

uses
  ULBLogger;

{ ══ Costanti specifiche ZED-F9P ══════════════════════════════════════════════

  Key ID CFG-VALSET per USB — Ref: §6.9.38, §6.9.39, §6.9.11, §6.9.18
  Tutti di tipo U1 salvo dove indicato.                                  }

const
  { ── NAV-PVT ─────────────────────────────────────────────────────────── }
  UBX_ID_NAV_PVT  = $07;
  UBX_PVT_PAYLOAD = 92;
  UBX_PVT_TOTAL   = UBX_HDR_SIZE + UBX_PVT_PAYLOAD + UBX_CRC_SIZE;  // 100

  { ── Protocolli input USB (CFG-USBINPROT) — tipo L (bool) ────────────── }
  KEY_USBINPROT_UBX    = $10770001;  // abilita ricezione comandi UBX
  KEY_USBINPROT_NMEA   = $10770002;  // abilita ricezione NMEA
  KEY_USBINPROT_RTCM3X = $10770004;  // abilita ricezione correzioni RTCM3

  { ── Protocolli output USB (CFG-USBOUTPROT) — tipo L (bool) ──────────── }
  KEY_USBOUTPROT_UBX    = $10780001;  // abilita output messaggi UBX
  KEY_USBOUTPROT_NMEA   = $10780002;  // abilita output NMEA
  KEY_USBOUTPROT_RTCM3X = $10780004;  // abilita output RTCM3X

  { ── Frequenza di navigazione (CFG-RATE) ─────────────────────────────── }
  KEY_RATE_MEAS    = $30210001;  // U2 — periodo misure in ms
  KEY_RATE_NAV     = $30210002;  // U2 — soluzioni per misura (1 = ogni misura)
  KEY_RATE_TIMEREF = $20210003;  // E1 — riferimento tempo: 1 = GPS

  { ── Output NAV-PVT su USB (CFG-MSGOUT) ──────────────────────────────── }
  KEY_MSGOUT_NAVPVT_USB = $20910009;  // U1 — rate (0=off, 1=ogni epoch)

  { ── Output NMEA su USB (CFG-MSGOUT) — disabilitare tutti ────────────── }
  { Default factory: GGA, GLL, GSA, GSV, RMC, VTG = 1; ZDA, GNS = 0      }
  KEY_NMEA_GGA_USB = $209100BD;
  KEY_NMEA_GLL_USB = $209100CC;
  KEY_NMEA_GSA_USB = $209100C2;
  KEY_NMEA_GSV_USB = $209100C7;
  KEY_NMEA_RMC_USB = $209100AE;
  KEY_NMEA_VTG_USB = $209100B3;
  KEY_NMEA_ZDA_USB = $209100DB;
  KEY_NMEA_GNS_USB = $209100B8;

  { ── Dimensione payload VALSET ────────────────────────────────────────── }
  {    6 coppie bool (L)  × 5 byte  =  30
      10 coppie U1        × 5 byte  =  50
       2 coppie U2        × 6 byte  =  12
      ────────────────────────────────────
      totale dati                   =  92 byte
      header VALSET fisso           =   4 byte
      ────────────────────────────────────
      payload totale               =  96 byte                           }
  VALSET_DATA_SIZE = 92;
  VALSET_PAY_SIZE  = VALSET_HDR_SIZE + VALSET_DATA_SIZE;  // 96

  { ── Nomi leggibili per il log ────────────────────────────────────────── }
  FIX_TYPE_NAMES   : array[0..5] of string = ('NoFix', 'DeadReck', '2D', '3D', 'GNSS+DR', 'TimeOnly');
  RTK_STATUS_NAMES : array[TRTKStatus] of string = ('None', 'Float', 'Fixed');





{ ══ Helpers interni ══════════════════════════════════════════════════════════ }

function FixTypeName(AFixType: Byte): string;
begin
  if AFixType <= 5 then
    Result := FIX_TYPE_NAMES[AFixType]
  else
    Result := 'Unknown(' + IntToStr(AFixType) + ')';
end;

{ ══ TUBXRoverThread ══════════════════════════════════════════════════════════ }

function TUBXRoverThread.SendUBX(const Msg: TBytes): Boolean;
var
  _MsgLen: Integer;
begin
  Result  := False;
  _MsgLen := Length(Msg);
  if _MsgLen > 0 then
    Result := FSerial.SendBuffer(@Msg[0], _MsgLen) = _MsgLen;
end;

{ ── Configure ───────────────────────────────────────────────────────────── }

function TUBXRoverThread.Configure(ALayer: Byte): Boolean;
{
  Invia un unico CFG-VALSET che:
    1. Imposta i protocolli input/output USB
    2. Disabilita tutti i messaggi NMEA in uscita
    3. Abilita NAV-PVT su USB a ogni epoch
    4. Imposta la frequenza di navigazione

  ALayer controlla dove viene scritta la configurazione:
    VALSET_LAYER_RAM        → solo RAM (default, nessun consumo flash)
    VALSET_LAYER_RAM_FLASH  → RAM + Flash (rende la config permanente)
}
var
  Payload    : TBytes;
  Pos        : Integer;
  MeasRateMs : Word;
begin
  Result     := False;
  MeasRateMs := 1000 div FRover.Hz;

  SetLength(Payload, VALSET_PAY_SIZE);
  FillChar(Payload[0], VALSET_PAY_SIZE, 0);

  { ── Header CFG-VALSET (4 byte fissi) ─────────────────────────────────── }
  Payload[0] := $00;     // version = 0
  Payload[1] := ALayer;  // layer di destinazione
  Payload[2] := $00;     // reserved0[0]
  Payload[3] := $00;     // reserved0[1]
  Pos := VALSET_HDR_SIZE;

  { ── Protocolli input USB ────────────────────────────────────────────── }
  AddBool(Payload, Pos, KEY_USBINPROT_UBX,    True);   // accetta comandi UBX
  AddBool(Payload, Pos, KEY_USBINPROT_NMEA,   False);  // rifiuta NMEA in ingresso
  AddBool(Payload, Pos, KEY_USBINPROT_RTCM3X, True);   // accetta correzioni RTCM3

  { ── Protocolli output USB ───────────────────────────────────────────── }
  AddBool(Payload, Pos, KEY_USBOUTPROT_UBX,    True);  // emette messaggi UBX
  AddBool(Payload, Pos, KEY_USBOUTPROT_NMEA,   False); // non emette NMEA
  AddBool(Payload, Pos, KEY_USBOUTPROT_RTCM3X, False); // non emette RTCM3X

  { ── Frequenza di navigazione ────────────────────────────────────────── }
  AddU2(Payload, Pos, KEY_RATE_MEAS,    MeasRateMs);  // periodo misure (ms)
  AddU2(Payload, Pos, KEY_RATE_NAV,     1);            // 1 fix per misura
  AddU1(Payload, Pos, KEY_RATE_TIMEREF, 1);            // riferimento: GPS

  { ── Disabilita tutti i messaggi NMEA su USB ─────────────────────────── }
  { Anche con USBOUTPROT_NMEA=False i singoli rate vengono azzerati
    per garantire pulizia se la config viene letta da u-center.          }
  AddU1(Payload, Pos, KEY_NMEA_GGA_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_GLL_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_GSA_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_GSV_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_RMC_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_VTG_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_ZDA_USB, 0);
  AddU1(Payload, Pos, KEY_NMEA_GNS_USB, 0);

  { ── Abilita NAV-PVT su USB a ogni epoch ─────────────────────────────── }
  AddU1(Payload, Pos, KEY_MSGOUT_NAVPVT_USB, 1);

  if Pos <> VALSET_PAY_SIZE then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Error, 'VALSET byte count mismatch: expected %d, got %d', [VALSET_PAY_SIZE, Pos]);
    Exit;
  end;

  if not SendUBX(BuildUBX(UBX_CLASS_CFG, UBX_ID_CFG_VALSET, Payload)) then
  begin
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Error, 'Send error on CFG-VALSET (layer=$%s, port=<%s>)', [IntToHex(ALayer, 2), FRover.Port]);
    Exit;
  end;

  Result := WaitACK(FSerial, UBX_CLASS_CFG, UBX_ID_CFG_VALSET, 700);

  if Result then
    LBLogger.Write(2, 'TUBXRoverThread.Configure', lmt_Info, 'CFG-VALSET ACK received (layer=$%s, rate=%d Hz)', [IntToHex(ALayer, 2), FRover.Hz])
  else
    { NAK o timeout: almeno un Key ID è sconosciuto al firmware,
      oppure la config non è valida. Non fatale: la comunicazione
      può continuare se il modulo era già configurato correttamente. }
    LBLogger.Write(1, 'TUBXRoverThread.Configure', lmt_Warning, 'CFG-VALSET: no ACK received (layer=$%s) — check firmware version', [IntToHex(ALayer, 2)]);
end;

{ ── DecodePVT ───────────────────────────────────────────────────────────── }

function TUBXRoverThread.DecodePVT(const Raw: TUBXNavPVT; out PVT: TPVTData): Boolean;
var
  CarrSoln: Byte;
begin
  Result := False;
  FillChar(PVT, SizeOf(PVT), 0);

  PVT.FixType   := TFixType(Raw.fixType);
  PVT.GNSSFixOK := (Raw.flags and $01) <> 0;

  { Valido solo se gnssFixOK e almeno fix 2D }
  if (not PVT.GNSSFixOK) or (Raw.fixType < 2) then Exit;

  { Stato RTK: bit 7:6 di flags — carrSoln }
  CarrSoln := (Raw.flags shr 6) and $03;
  case CarrSoln of
    1: PVT.RTKStatus := rtkFloat;
    2: PVT.RTKStatus := rtkFixed;
  else
    PVT.RTKStatus := rtkNone;
  end;

  { Data e ora UTC — valide solo se validDate (bit0) e validTime (bit1) }
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

{ ── InternalExecute ─────────────────────────────────────────────────────── }

procedure TUBXRoverThread.InternalExecute;
var
  RxBuf    : TLBCircularBuffer;
  SyncPat  : array[0..1] of Byte;
  SyncPos  : Integer;
  PayLen   : Word;
  FrameBuf : array[0..UBX_PVT_TOTAL - 1] of Byte;
  PVT      : TUBXNavPVT;
  PVTData  : TPVTData;
  CorrData : TBytes;
  CorrLen  : Integer;
  _tmp     : TBytes;

begin
  SyncPat[0] := UBX_SYNC1;
  SyncPat[1] := UBX_SYNC2;

  LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Info, 'Thread started on port <%s> at %d Hz', [FRover.Port, FRover.Hz]);

  { Configurazione iniziale — solo RAM, non consuma cicli flash }
  Self.Configure(VALSET_LAYER_RAM);

  RxBuf := TLBCircularBuffer.Create(4096);
  try
    while not Terminated do
    begin
      { ── 1. Leggi dalla seriale ────────────────────────────────────── }
      if FSerial.WaitingData > 0 then
        RxBuf.WriteFromSerial(FSerial)
      else
        Self.PauseFor(5);

      { ── 2. Invia correzioni RTCM3 pendenti ───────────────────────── }
      CorrLen := FRover.Corrections.Flush(CorrData);
      if CorrLen > 0 then
      begin
        if FSerial.SendBuffer(@CorrData[0], CorrLen) <> CorrLen then
          LBLogger.Write(1, 'TUBXRoverThread.InternalExecute', lmt_Warning, 'RTCM3 corrections send incomplete (%d bytes)', [CorrLen]);
      end;

      { ── 3. Elabora frame UBX completi nel buffer ─────────────────── }
      while RxBuf.AvailableForRead >= UBX_PVT_TOTAL do
      begin
        SyncPos := RxBuf.FindPattern(@SyncPat[0], 2, 0);

        if SyncPos < 0 then
        begin
          { Nessun sync nel buffer: scartiamo tutto. Probabile traffico
            residuo di NMEA o protocollo non atteso. }
          SetLength(_tmp, RxBuf.AvailableForRead);
          RxBuf.Read(@_tmp[0], Length(_tmp));
          LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Debug, 'No UBX sync found, clearing %d bytes', [RxBuf.AvailableForRead]);
          LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Debug, '%s', [HexString(@_tmp[0], Length(_tmp))]);
          RxBuf.Clear;
          Break;
        end;

        if SyncPos > 0 then
        begin
          { Byte spuri prima del sync: possibile remnant NMEA o baud
            errato in fase iniziale. }
          LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Debug, 'Skipping %d spurious bytes before UBX sync', [SyncPos]);
          RxBuf.Skip(SyncPos);
        end;

        if RxBuf.AvailableForRead < UBX_HDR_SIZE then
          Break;

        PayLen := RxBuf.PeekByte(4) or (Word(RxBuf.PeekByte(5)) shl 8);

        if RxBuf.AvailableForRead < UBX_HDR_SIZE + PayLen + UBX_CRC_SIZE then
          Break;  // Frame incompleto — aspetta più dati

        if (RxBuf.PeekByte(2) = UBX_CLASS_NAV) and
           (RxBuf.PeekByte(3) = UBX_ID_NAV_PVT) and
           (PayLen = UBX_PVT_PAYLOAD) then
        begin
          RxBuf.Read(@FrameBuf[0], UBX_PVT_TOTAL);

          if ValidUBXChecksum(FrameBuf, 0, PayLen) then
          begin
            Move(FrameBuf[UBX_HDR_SIZE], PVT, UBX_PVT_PAYLOAD);

            if DecodePVT(PVT, PVTData) then
            begin
              { ── Log dato ricevuto ───────────────────────────────────
              LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Info,
                                'PVT fix=%s rtk=%s sv=%d lat=%.7f lon=%.7f ' +
                                'alt=%.1fm hacc=%dmm vacc=%dmm spd=%.1fkm/h',
                [FixTypeName(Byte(PVTData.FixType)),
                 RTK_STATUS_NAMES[PVTData.RTKStatus],
                 PVTData.NumSV,
                 PVTData.Latitude,
                 PVTData.Longitude,
                 PVTData.AltitudeMSL,
                 PVTData.HAccMm,
                 PVTData.VAccMm,
                 PVTData.SpeedKmH]);  }

              { ── Aggiorna FLastPVT thread-safe ─────────────────────── }
              if FRover.FPVTLock.Acquire('TUBXRoverThread.InternalExecute') then
              begin
                try
                  FRover.FLastPVT := PVTData;
                finally
                  FRover.FPVTLock.Release;
                end;

                { ── Notifica il chiamante (da thread seriale) ─────────── }
                if Assigned(FRover.OnPVT) then
                  FRover.OnPVT(FRover, PVTData);
              end;
            end
            else begin
              { Fix non valido o gnssFixOK = 0: normale all'avvio o in
                ambienti con scarsa visibilità satellite. }
              LBLogger.Write(3, 'TUBXRoverThread.InternalExecute', lmt_Debug, 'PVT discarded: no valid fix (fixType=%s gnssFixOK=%s)',
                [FixTypeName(PVT.fixType),
                 BoolToStr((PVT.flags and $01) <> 0, True)]);
              LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Debug, '%s', [HexString(@PVT, SizeOf(PVT))]);
            end;
          end
          else
            LBLogger.Write(1, 'TUBXRoverThread.InternalExecute', lmt_Warning, 'UBX-NAV-PVT: checksum mismatch — frame discarded');
        end
        else begin
          { Frame di tipo diverso (es. ACK residuo): consuma l'intero frame
            per evitare il loop O(n²) che si avrebbe scartando solo 2 byte. }
          LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Debug, 'Non-PVT UBX frame skipped (class=$%s id=$%s len=%d)',
                             [IntToHex(RxBuf.PeekByte(2), 2),
                              IntToHex(RxBuf.PeekByte(3), 2),
                              PayLen]);
          RxBuf.Skip(UBX_HDR_SIZE + PayLen + UBX_CRC_SIZE);
        end;
      end;

    end;  // while not Terminated
  except
    on E: Exception do
      LBLogger.Write(1, 'TUBXRoverThread.InternalExecute', lmt_Error, E.Message);

  end;
  RxBuf.Free;

  LBLogger.Write(5, 'TUBXRoverThread.InternalExecute', lmt_Info, 'Thread terminated on port <%s>', [FRover.Port]);
end;

{ ── Lifecycle thread ────────────────────────────────────────────────────── }

constructor TUBXRoverThread.Create(ARover: TUBXRTKRover;
                                   const APort: string; AHz: Byte);
begin
  inherited Create();
  { TLBBaseThread imposta FreeOnTerminate := True nel suo costruttore.
    Lo sovrascriviamo a False: la vita del thread è gestita da TUBXRTKRover
    tramite Terminate + WaitFor + FreeAndNil, che è incompatibile con
    FreeOnTerminate = True (causa double-free). }
  FreeOnTerminate := False;
  FRover  := ARover;
  FSerial := TBlockSerial.Create;
  FSerial.Connect(APort);
  { USB nativo: il baud rate è irrilevante per la comunicazione, ma
    TBlockSerial richiede un valore — usiamo 115200 come convenzione. }
  FSerial.Config(115200, 8, 'N', SB1, False, False);
end;

destructor TUBXRoverThread.Destroy;
begin
  FSerial.CloseSocket;
  FSerial.Free;
  inherited;
end;

function TUBXRoverThread.SerialOK: Boolean;
begin
  Result := FSerial.LastError = 0;
end;

function TUBXRoverThread.SerialErrorDesc: string;
begin
  Result := FSerial.LastErrorDesc;
end;

{ ══ TUBXRTKRover ══════════════════════════════════════════════════════════════ }

constructor TUBXRTKRover.Create(const APort: string; AHz: Byte);
begin
  inherited Create;
  FPort        := APort;
  FHz          := AHz;
  FActive      := False;
  FThread      := nil;
  FCorrections := TCorrectionBuffer.Create;

  FillChar(FLastPVT, SizeOf(FLastPVT), 0);
  FPVTLock     := TTimedOutCriticalSection.Create;
end;

destructor TUBXRTKRover.Destroy;
begin
  Self.Close;
  FPVTLock.Free;
  FCorrections.Free;
  inherited;
end;

function TUBXRTKRover.Open: Boolean;
begin
  if not FActive then
  begin

    FThread := TUBXRoverThread.Create(Self, FPort, FHz);

    if FThread.SerialOK then
    begin
      FActive := True;
      FThread.Start;
      LBLogger.Write(5, 'TUBXRTKRover.Open', lmt_Info, 'Rover opened on port <%s> at %d Hz', [FPort, FHz]);
    end
    else
    begin
      LBLogger.Write(1, 'TUBXRTKRover.Open', lmt_Warning, 'Cannot open port <%s>: %s', [FPort, FThread.SerialErrorDesc]);
      { AddReference ha impostato FThread^ = @FThread; FreeAndNil lo azzera
        prima che il destructor del thread tenti di accedervi. }
      FreeAndNil(FThread);
    end;
  end;

  Result := FActive;
end;

procedure TUBXRTKRover.Close;
begin
  if FActive then
  begin
    LBLogger.Write(5, 'TUBXRTKRover.Close', lmt_Info, 'Closing rover on port <%s>', [FPort]);
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
    FActive := False;
  end;
end;

function TUBXRTKRover.SaveConfig(): Boolean;
{
  Rende permanente la configurazione corrente scrivendola in Flash.
  Chiama Configure(VALSET_LAYER_RAM_FLASH): il modulo applica la config
  alla RAM (effetto immediato) e la scrive in Flash (effetto permanente)
  in un singolo messaggio — nessun round-trip aggiuntivo.

  ATTENZIONE: la flash del ZED-F9P ha ~10.000 cicli di scrittura.
  Chiamare solo quando la configurazione è definitiva e verificata.
}
begin
  Result := False;

  if FActive then
  begin
    LBLogger.Write(5, 'TUBXRTKRover.SaveConfig', lmt_Info, 'Saving configuration to flash on port <%s>', [FPort]);
    Result := FThread.Configure(VALSET_LAYER_RAM_FLASH);
  end
  else
    LBLogger.Write(1, 'TUBXRTKRover.SaveConfig', lmt_Warning, 'SaveConfig called but rover is not active');
end;

function TUBXRTKRover.GetLastPVT(out PVT: TPVTData): Boolean;
begin
  if FPVTLock.Acquire('TUBXRTKRover.GetLastPVT') then
  begin
    try
      PVT    := FLastPVT;
      Result := FLastPVT.Valid;
    finally
      FPVTLock.Release;
    end;
  end;
end;

procedure TUBXRTKRover.PushCorrections(const Data: array of Byte; Len: Integer);
begin
  FCorrections.Push(Data, Len);
end;

procedure TUBXRTKRover.PushCorrections(const Data: TBytes);
begin
  FCorrections.Push(Data);
end;

end.
