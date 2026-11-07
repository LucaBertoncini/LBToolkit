unit ImpinjR700;

{
  Gestione lettore RFID Impinj R700 via protocollo LLRP su TCP.
  Porta standard: 5084.

  ── Protocollo LLRP ──────────────────────────────────────────────────────
  Binario big-endian. Header fisso 10 byte su ogni messaggio:
    [0-1]  Rsvd(3) + Ver(3) + MsgType(10)  → UInt16 BE
    [2-5]  MessageLength (include i 10 byte di header) → UInt32 BE
    [6-9]  MessageID                        → UInt32 BE

  ── Flusso operativo ─────────────────────────────────────────────────────
  1. Connessione TCP → il reader invia READER_EVENT_NOTIFICATION (connessione)
  2. GET_READER_CAPABILITIES → otteniamo la tabella potenze (opzionale)
  3. SET_READER_CONFIG        → configurazione antenne (potenza, sensibilità)
  4. ADD_ROSPEC               → definisce l'operazione di inventario
  5. ENABLE_ROSPEC            → abilita lo spec (porta in stato "Inactive")
  6. START_ROSPEC             → avvia la lettura continua
  7. RO_ACCESS_REPORT         → report tag (ricevuto in automatico dal reader)
  8. STOP_ROSPEC              → ferma la lettura
  9. DELETE_ROSPEC            → pulizia
  10. CLOSE_CONNECTION        → chiusura pulita

  ── Potenza ──────────────────────────────────────────────────────────────
  L'R700 usa un indice per la potenza TX. Ogni passo = 0.25 dBm.
  Formula: Index = Round((dBm - 10.0) / 0.25) + 1
  Minimo: Index 1  = 10.00 dBm
  Massimo tipico EU: Index 61 = 25.00 dBm (verificare normativa locale)
  Per SetAntennaPower() si passa direttamente il valore in dBm (es. 23.0)
  e la conversione all'indice è automatica.

  ── Note ─────────────────────────────────────────────────────────────────
  - Un solo ROSpec con ID = ROSPEC_ID viene usato per tutta la sessione.
  - Il ROSpec usa start/stop trigger "Null" → lettura continua fino a
    STOP_ROSPEC.
  - Il report include EPC, RSSI, timestamp, antenna ID.
  - Le antenne attive e la potenza per antenna si impostano prima di Open
    o mentre è in pausa (Close→impostazioni→Open).
  - Dipendenze: BlckSock (Synapse), SyncObjs.
}

interface

uses
  SysUtils, Classes, BlckSock, SyncObjs;

const
  LLRP_DEFAULT_PORT = 5084;
  ROSPEC_ID         = 1;
  MAX_ANTENNAS      = 4;

type

  { ── Dati tag letti ───────────────────────────────────────────────────── }

  TTagData = record
    EPC        : string;      // EPC in formato hex (es. 'E28011700000020F4B8C2B65')
    AntennaID  : Word;        // antenna che ha letto il tag (1-based)
    RSSI       : SmallInt;    // picco RSSI in dBm (valore negativo tipico)
    SeenCount  : Word;        // quante volte è stato letto nel report
    Timestamp  : TDateTime;   // ora locale della ricezione del report
  end;

  { ── Configurazione per antenna ───────────────────────────────────────── }

  TAntennaConfig = record
    Enabled      : Boolean;
    PowerDBm     : Double;    // es. 23.0 dBm  (range tipico: 10.0 – 25.0)
    RxSensDBm    : Integer;   // es. -70 dBm   (range R700: -90 .. -30 dBm)
  end;

  { ── Tipi evento ─────────────────────────────────────────────────────── }

  TOnTagRead     = procedure(Sender: TObject; const Tag: TTagData) of object;
  TOnRFIDStatus  = procedure(Sender: TObject; const Msg: string) of object;
  TOnRFIDError   = procedure(Sender: TObject; const Msg: string) of object;

  { ── Forward ─────────────────────────────────────────────────────────── }

  TImpinjR700 = class;

  { ── Thread LLRP ─────────────────────────────────────────────────────── }

  TLLRPThread = class(TThread)
  private
    FReader       : TImpinjR700;
    FSock         : TTCPBlockSocket;
    FMsgIDCounter : UInt32;

    { Tmp per Synchronize }
    FTmpTag    : TTagData;
    FTmpMsg    : string;

    { ── LLRP helpers ───────────────────────────────────────────────── }
    function  NextMsgID: UInt32;
    function  BuildHeader(MsgType: Word; BodyLen: Integer; MsgID: UInt32): TBytes;
    function  BuildMsg(MsgType: Word; const Body: TBytes; MsgID: UInt32 = 0): TBytes;
    function  W16(v: Word): TBytes;    // big-endian 2 byte
    function  W32(v: UInt32): TBytes;  // big-endian 4 byte
    function  Concat(const A, B: TBytes): TBytes; overload;
    function  Concat(const Parts: array of TBytes): TBytes; overload;

    { ── Parametri LLRP encoded ─────────────────────────────────────── }
    function  ParamTLV(ParamType: Word; const Body: TBytes): TBytes;
    function  ParamLLRPStatus: TBytes;

    { ── Comandi LLRP ───────────────────────────────────────────────── }
    procedure SendSetReaderConfig;
    procedure SendAddROSpec;
    procedure SendEnableROSpec;
    procedure SendStartROSpec;
    procedure SendStopROSpec;
    procedure SendDeleteROSpec;
    procedure SendCloseConnection;
    procedure SendKeepAliveAck(MsgID: UInt32);

    function  SendAndReceive(const Msg: TBytes; ExpectedType: Word;
                             TimeoutMs: Integer = 2000): TBytes;
    procedure SendMsg(const Msg: TBytes);

    { ── Parser risposte ────────────────────────────────────────────── }
    function  ReadMessage(TimeoutMs: Integer; out MsgType: Word;
                          out MsgID: UInt32; out Body: TBytes): Boolean;
    procedure ParseROAccessReport(const Body: TBytes);
    procedure ParseTagReportData(const Data: TBytes; Offset: Integer; Len: Integer);
    function  ExtractEPC(const Data: TBytes; Offset, Len: Integer): string;
    function  ExtractParam(const Data: TBytes; StartOffset: Integer;
                           SearchType: Word; out ParamBody: TBytes): Boolean;

    { ── Synchronize callbacks ─────────────────────────────────────── }
    procedure SyncTag;
    procedure SyncStatus;
    procedure SyncError;

    { ── Conversioni potenza ─────────────────────────────────────────── }
    function  PowerDBmToIndex(dBm: Double): Word;
    function  RxSensDBmToIndex(dBm: Integer): Word;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TImpinjR700);
    destructor  Destroy; override;
    function    Connected: Boolean;
  end;

  { ── Classe pubblica ─────────────────────────────────────────────────── }

  TImpinjR700 = class
  private
    FThread          : TLLRPThread;
    FHost            : string;
    FPort            : Integer;
    FActive          : Boolean;
    FScanning        : Boolean;
    FAntennas        : array[1..MAX_ANTENNAS] of TAntennaConfig;
    FConfigLock      : TCriticalSection;  // protegge FAntennas da accessi concorrenti
    FPendingReconfig : Boolean;           // segnala al thread che serve riconfigurazione

    FOnTagRead      : TOnTagRead;
    FOnStatus       : TOnRFIDStatus;
    FOnError        : TOnRFIDError;

    function  GetAntennaConfig(Index: Integer): TAntennaConfig;
    procedure SetAntennaConfig(Index: Integer; const Value: TAntennaConfig);
  public
    constructor Create(const AHost: string; APort: Integer = LLRP_DEFAULT_PORT);
    destructor  Destroy; override;

    { Connette, configura e avvia il loop di ricezione (non inizia la scan) }
    procedure Open;
    { Ferma la scan (se attiva), chiude connessione }
    procedure Close;

    { Avvia inventario continuo }
    procedure StartScan;
    { Ferma inventario }
    procedure StopScan;

    { ── Configurazione statica (prima di Open, o a reader fermo) ──────── }

    { Imposta la potenza TX per una singola antenna (in dBm, es. 23.0). }
    procedure SetAntennaPower(AntennaID: Integer; PowerDBm: Double);
    { Imposta la sensibilità RX per una singola antenna (in dBm, es. -70). }
    procedure SetAntennaRxSens(AntennaID: Integer; SensDBm: Integer);
    { Abilita/disabilita un'antenna nell'inventario. }
    procedure SetAntennaEnabled(AntennaID: Integer; Enabled: Boolean);

    { ── Configurazione live (applicabile anche durante la scansione) ───── }
    {
      Questi metodi applicano la nuova configurazione in modo sicuro anche
      mentre il reader sta scansionando:
        1. Aggiornano FAntennas (protetto da lock)
        2. Se la scan è attiva: Stop → SET_READER_CONFIG → Start
           (ciclo trasparente, tipicamente < 200 ms)
        3. Se la scan non è attiva: aggiornano solo FAntennas, la nuova
           config sarà usata al prossimo StartScan.
    }

    { Cambia la potenza TX di una antenna durante la scansione. }
    procedure SetAntennaPowerLive(AntennaID: Integer; PowerDBm: Double);
    { Cambia la sensibilità RX di una antenna durante la scansione. }
    procedure SetAntennaRxSensLive(AntennaID: Integer; SensDBm: Integer);
    { Cambia la potenza TX di TUTTE le antenne abilitate in un colpo solo. }
    procedure SetAllAntennasPowerLive(PowerDBm: Double);
    { Cambia la sensibilità RX di TUTTE le antenne abilitate in un colpo solo. }
    procedure SetAllAntennasRxSensLive(SensDBm: Integer);

    property Active      : Boolean          read FActive;
    property Scanning    : Boolean          read FScanning;
    property Host        : string           read FHost   write FHost;
    property Port        : Integer          read FPort   write FPort;

    { Leggi/scrivi configurazione completa per antenna (1-based) }
    property AntennaConfig[Index: Integer] : TAntennaConfig
                                             read GetAntennaConfig
                                             write SetAntennaConfig;

    property OnTagRead   : TOnTagRead       read FOnTagRead  write FOnTagRead;
    property OnStatus    : TOnRFIDStatus    read FOnStatus   write FOnStatus;
    property OnError     : TOnRFIDError     read FOnError    write FOnError;
  end;

implementation

{ ── Costanti message type LLRP ─────────────────────────────────────────────── }
const
  LLRP_VER               = 1;   // versione LLRP 1.0.1

  { Client → Reader }
  MT_GET_READER_CAPABILITIES     = 1;
  MT_GET_READER_CONFIG           = 2;
  MT_SET_READER_CONFIG           = 3;
  MT_ADD_ROSPEC                  = 20;
  MT_DELETE_ROSPEC               = 21;
  MT_START_ROSPEC                = 22;
  MT_STOP_ROSPEC                 = 23;
  MT_ENABLE_ROSPEC               = 24;
  MT_DISABLE_ROSPEC              = 25;
  MT_CLOSE_CONNECTION            = 14;
  MT_KEEPALIVE_ACK               = 72;

  { Reader → Client }
  MT_GET_READER_CAPABILITIES_RESP = 11;
  MT_GET_READER_CONFIG_RESP       = 12;
  MT_SET_READER_CONFIG_RESP       = 13;
  MT_ADD_ROSPEC_RESP              = 30;
  MT_DELETE_ROSPEC_RESP           = 31;
  MT_START_ROSPEC_RESP            = 32;
  MT_STOP_ROSPEC_RESP             = 33;
  MT_ENABLE_ROSPEC_RESP           = 34;
  MT_DISABLE_ROSPEC_RESP          = 35;
  MT_CLOSE_CONNECTION_RESP        = 4;
  MT_RO_ACCESS_REPORT             = 61;
  MT_READER_EVENT_NOTIFICATION    = 63;
  MT_KEEPALIVE                    = 62;
  MT_ERROR_MESSAGE                = 100;

  { Parametri TLV (tipi > 128 → TLV con Type a 10 bit nel primo Word) }
  PT_LLRP_STATUS                 = 287;
  PT_ANTENNA_CONFIGURATION       = 222;
  PT_RF_TRANSMITTER              = 225;
  PT_RF_RECEIVER                 = 223;
  PT_TAG_REPORT_DATA             = 240;
  PT_EPC_DATA                    = 241;
  PT_EPC_96                      = 13;    // TV param (tipo < 128, 1 byte type)
  PT_ANTENNA_ID                  = 1;     // TV param
  PT_PEAK_RSSI                   = 6;     // TV param
  PT_TAG_SEEN_COUNT              = 10;    // TV param
  PT_ROSPEC_ID_PARAM             = 9;     // TV param
  PT_ROBOUNDARYSPEC              = 178;
  PT_ROSPEC_START_TRIGGER        = 179;
  PT_ROSPEC_STOP_TRIGGER         = 182;
  PT_AISPEC                      = 183;
  PT_AISPEC_STOP_TRIGGER         = 184;
  PT_INVENTORY_PARAM_SPEC        = 186;
  PT_C1G2LLRP_PARAM              = 330;
  PT_TAG_REPORT_CONTENT_SEL      = 238;
  PT_RO_REPORT_SPEC              = 237;

  LLRP_HDR_SIZE = 10;

{ ══ TLLRPThread — helpers binari ════════════════════════════════════════════ }

function TLLRPThread.NextMsgID: UInt32;
begin
  Inc(FMsgIDCounter);
  if FMsgIDCounter = 0 then FMsgIDCounter := 1;
  Result := FMsgIDCounter;
end;

function TLLRPThread.W16(v: Word): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := (v shr 8) and $FF;
  Result[1] := v and $FF;
end;

function TLLRPThread.W32(v: UInt32): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := (v shr 24) and $FF;
  Result[1] := (v shr 16) and $FF;
  Result[2] := (v shr 8)  and $FF;
  Result[3] := v and $FF;
end;

function TLLRPThread.Concat(const A, B: TBytes): TBytes;
var
  LA, LB: Integer;
begin
  LA := Length(A); LB := Length(B);
  SetLength(Result, LA + LB);
  if LA > 0 then Move(A[0], Result[0], LA);
  if LB > 0 then Move(B[0], Result[LA], LB);
end;

function TLLRPThread.Concat(const Parts: array of TBytes): TBytes;
var
  i, Total, Off: Integer;
begin
  Total := 0;
  for i := 0 to High(Parts) do Inc(Total, Length(Parts[i]));
  SetLength(Result, Total);
  Off := 0;
  for i := 0 to High(Parts) do
  begin
    if Length(Parts[i]) > 0 then
      Move(Parts[i][0], Result[Off], Length(Parts[i]));
    Inc(Off, Length(Parts[i]));
  end;
end;

function TLLRPThread.BuildHeader(MsgType: Word; BodyLen: Integer; MsgID: UInt32): TBytes;
var
  TypeWord : Word;
  TotalLen : UInt32;
begin
  { [Rsvd=0(3) | Ver=1(3) | Type(10)] }
  TypeWord := ((LLRP_VER and $07) shl 10) or (MsgType and $03FF);
  TotalLen := LLRP_HDR_SIZE + BodyLen;
  Result := Concat([W16(TypeWord), W32(TotalLen), W32(MsgID)]);
end;

function TLLRPThread.BuildMsg(MsgType: Word; const Body: TBytes; MsgID: UInt32): TBytes;
var
  ID: UInt32;
begin
  if MsgID = 0 then ID := NextMsgID else ID := MsgID;
  Result := Concat(BuildHeader(MsgType, Length(Body), ID), Body);
end;

{ ── Parametro TLV: [Type(2B BE) | Length(2B BE, include i 4B di header) | Body] }
function TLLRPThread.ParamTLV(ParamType: Word; const Body: TBytes): TBytes;
var
  TotalLen: Word;
begin
  TotalLen := 4 + Length(Body);
  Result := Concat([W16(ParamType), W16(TotalLen), Body]);
end;

function TLLRPThread.ParamLLRPStatus: TBytes;
{ LLRPStatus minimo: StatusCode=0 (M_Success), ErrorDesc length=0 }
var
  Body: TBytes;
begin
  SetLength(Body, 4);  // StatusCode(2) + ErrorDescByteCount(2)
  FillChar(Body[0], 4, 0);
  Result := ParamTLV(PT_LLRP_STATUS, Body);
end;

{ ── Conversioni potenza ─────────────────────────────────────────────────────
  TX Power: ogni step = 0.25 dBm, Index 1 = 10.00 dBm
  RX Sensitivity R700: Index 1 = -90 dBm, Index 2 = -80 dBm (+10 dB),
  poi ogni step = 1 dBm fino a Index 52 = -30 dBm.
}
function TLLRPThread.PowerDBmToIndex(dBm: Double): Word;
var
  Idx: Integer;
begin
  Idx := Round((dBm - 10.0) / 0.25) + 1;
  if Idx < 1  then Idx := 1;
  if Idx > 81 then Idx := 81;  // 30.00 dBm (cap hardware, verificare normativa)
  Result := Idx;
end;

function TLLRPThread.RxSensDBmToIndex(dBm: Integer): Word;
var
  Idx: Integer;
begin
  { -90 dBm → 1, -80 dBm → 2, poi 1 dBm per step fino a -30 dBm → 52 }
  if dBm <= -90 then
    Idx := 1
  else if dBm <= -80 then
    Idx := 2
  else
  begin
    Idx := 2 + (-80 - dBm) * (-1);  // ogni dBm +1
    Idx := 2 + (dBm - (-80));
  end;
  if Idx < 1  then Idx := 1;
  if Idx > 52 then Idx := 52;
  Result := Idx;
end;

{ ══ TLLRPThread — comandi LLRP ══════════════════════════════════════════════ }

procedure TLLRPThread.SendSetReaderConfig;
{
  Configura potenza TX e sensibilità RX per ciascuna antenna abilitata.
  AntennaConfiguration TLV per ogni antenna:
    AntennaID(2) + RFReceiver TLV + RFTransmitter TLV
}
var
  i           : Integer;
  AntCfgBody  : TBytes;
  AllAntCfg   : TBytes;
  RFRx        : TBytes;
  RFTx        : TBytes;
  AntBody     : TBytes;
  Msg         : TBytes;
  Resp        : TBytes;
  Cfg         : TAntennaConfig;
begin
  SetLength(AllAntCfg, 0);

  FReader.FConfigLock.Acquire;
  try
  for i := 1 to MAX_ANTENNAS do
  begin
    Cfg := FReader.FAntennas[i];
    if not Cfg.Enabled then Continue;

    { RFReceiver: ReceiverSensitivity(2) }
    RFRx := ParamTLV(PT_RF_RECEIVER,
              W16(RxSensDBmToIndex(Cfg.RxSensDBm)));

    { RFTransmitter: HopTableID(2) + ChannelIndex(2) + TransmitPower(2) }
    RFTx := ParamTLV(PT_RF_TRANSMITTER,
              Concat([W16(1),   // HopTableID (1 = default)
                      W16(0),   // ChannelIndex (0 = reader sceglie)
                      W16(PowerDBmToIndex(Cfg.PowerDBm))]));

    { AntennaConfiguration body: AntennaID(2) + RFRx + RFTx }
    AntCfgBody := Concat([W16(i), RFRx, RFTx]);
    AntBody    := ParamTLV(PT_ANTENNA_CONFIGURATION, AntCfgBody);
    AllAntCfg  := Concat(AllAntCfg, AntBody);
  end;
  finally
    FReader.FConfigLock.Release;
  end;

  if Length(AllAntCfg) = 0 then Exit;

  { SET_READER_CONFIG: ResetToFactoryDefaults(1) = 0 + antenne }
  var Body: TBytes;
  SetLength(Body, 1);
  Body[0] := 0;  // ResetToFactoryDefaults = false
  Body := Concat(Body, AllAntCfg);

  Msg  := BuildMsg(MT_SET_READER_CONFIG, Body);
  Resp := SendAndReceive(Msg, MT_SET_READER_CONFIG_RESP);

  if Length(Resp) = 0 then
  begin
    FTmpMsg := 'SET_READER_CONFIG: nessuna risposta';
    Synchronize(@SyncError);
  end;
end;

procedure TLLRPThread.SendAddROSpec;
{
  ROSpec per inventario continuo su tutte le antenne abilitate.
  Struttura:
    ROSpec {
      ROSpecID, Priority, CurrentState=Disabled
      ROBoundarySpec {
        ROSpecStartTrigger { Type=Null }
        ROSpecStopTrigger  { Type=Null }
      }
      AISpec {
        AntennaIDs (lista antenne abilitate)
        AISpecStopTrigger { Type=Null }
        InventoryParameterSpec {
          InventoryParameterSpecID=1
          ProtocolID=1 (C1G2)
        }
      }
      ROReportSpec {
        ROReportTrigger=2 (upon N TagReportData),  N=1
        TagReportContentSelector { EPC, AntennaID, RSSI, SeenCount }
      }
    }
}
var
  AntennaList : TBytes;
  i           : Integer;
  StartTrig   : TBytes;
  StopTrig    : TBytes;
  BoundSpec   : TBytes;
  InvSpec     : TBytes;
  AIStopTrig  : TBytes;
  AISpec      : TBytes;
  ContentSel  : TBytes;
  RORepSpec   : TBytes;
  ROSpecBody  : TBytes;
  Msg, Resp   : TBytes;
begin
  { Lista antenne abilitate (2 byte ciascuna) }
  SetLength(AntennaList, 0);
  FReader.FConfigLock.Acquire;
  try
    for i := 1 to MAX_ANTENNAS do
      if FReader.FAntennas[i].Enabled then
        AntennaList := Concat(AntennaList, W16(i));
  finally
    FReader.FConfigLock.Release;
  end;

  { Se nessuna antenna abilitata, usa tutte }
  if Length(AntennaList) = 0 then
    for i := 1 to MAX_ANTENNAS do
      AntennaList := Concat(AntennaList, W16(i));

  { ROSpecStartTrigger: Type=0 (Null/Immediate) }
  StartTrig := ParamTLV(PT_ROSPEC_START_TRIGGER, W16(0));

  { ROSpecStopTrigger: Type=0 (Null) + DurationTriggerValue=0 }
  StopTrig := ParamTLV(PT_ROSPEC_STOP_TRIGGER,
                Concat([W16(0), W32(0)]));

  BoundSpec := ParamTLV(PT_ROBOUNDARYSPEC, Concat(StartTrig, StopTrig));

  { AISpecStopTrigger: Type=0 (Null) + DurationTrigger=0 }
  AIStopTrig := ParamTLV(PT_AISPEC_STOP_TRIGGER, Concat([W16(0), W32(0)]));

  { InventoryParameterSpec: ID=1, ProtocolID=1 (EPCGlobal Class1Gen2) }
  InvSpec := ParamTLV(PT_INVENTORY_PARAM_SPEC,
               Concat([W16(1), W16(1)]));

  { AISpec: AntennaCount(2) + AntennaIDs + StopTrigger + InventorySpec }
  AISpec := ParamTLV(PT_AISPEC,
              Concat([W16(Length(AntennaList) div 2),
                      AntennaList,
                      AIStopTrig,
                      InvSpec]));

  { TagReportContentSelector:
    Bit field (2 byte):
      bit 15 = EnableROSpecID
      bit 14 = EnableSpecIndex
      bit 13 = EnableInventoryParameterSpecID
      bit 12 = EnableAntennaID        ← on
      bit 11 = EnableChannelIndex
      bit 10 = EnablePeakRSSI         ← on
      bit 9  = EnableFirstSeenTimestamp
      bit 8  = EnableLastSeenTimestamp
      bit 7  = EnableTagSeenCount     ← on
      bit 6  = EnableAccessSpecID
    Word: AntennaID(12) + PeakRSSI(10) + TagSeenCount(7) = $1480
  }
  ContentSel := ParamTLV(PT_TAG_REPORT_CONTENT_SEL, W16($14C0));

  { ROReportSpec: ROReportTrigger=2 (report ogni N tag), N=1 }
  RORepSpec := ParamTLV(PT_RO_REPORT_SPEC,
                 Concat([W16(2),   // Trigger: upon N TagReportData
                         W16(1),   // N = 1 (report per ogni tag)
                         ContentSel]));

  { ROSpec completo:
    ROSpecID(4) + Priority(1) + CurrentState(1) + BoundSpec + AISpec + RORepSpec }
  ROSpecBody := Concat([W32(ROSPEC_ID),
                        TBytes.Create(0),   // Priority=0
                        TBytes.Create(0),   // State=Disabled
                        BoundSpec,
                        AISpec,
                        RORepSpec]);

  Msg  := BuildMsg(MT_ADD_ROSPEC, ROSpecBody);
  Resp := SendAndReceive(Msg, MT_ADD_ROSPEC_RESP);

  if Length(Resp) = 0 then
  begin
    FTmpMsg := 'ADD_ROSPEC: nessuna risposta';
    Synchronize(@SyncError);
  end;
end;

procedure TLLRPThread.SendEnableROSpec;
var
  Msg, Resp: TBytes;
begin
  Msg  := BuildMsg(MT_ENABLE_ROSPEC, W32(ROSPEC_ID));
  Resp := SendAndReceive(Msg, MT_ENABLE_ROSPEC_RESP);
  if Length(Resp) = 0 then
  begin
    FTmpMsg := 'ENABLE_ROSPEC: nessuna risposta';
    Synchronize(@SyncError);
  end;
end;

procedure TLLRPThread.SendStartROSpec;
var
  Msg, Resp: TBytes;
begin
  Msg  := BuildMsg(MT_START_ROSPEC, W32(ROSPEC_ID));
  Resp := SendAndReceive(Msg, MT_START_ROSPEC_RESP);
  if Length(Resp) = 0 then
  begin
    FTmpMsg := 'START_ROSPEC: nessuna risposta';
    Synchronize(@SyncError);
  end;
end;

procedure TLLRPThread.SendStopROSpec;
var
  Msg, Resp: TBytes;
begin
  Msg  := BuildMsg(MT_STOP_ROSPEC, W32(ROSPEC_ID));
  Resp := SendAndReceive(Msg, MT_STOP_ROSPEC_RESP, 3000);
  if Length(Resp) = 0 then
  begin
    FTmpMsg := 'STOP_ROSPEC: nessuna risposta';
    Synchronize(@SyncError);
  end;
end;

procedure TLLRPThread.SendDeleteROSpec;
var
  Msg, Resp: TBytes;
begin
  Msg  := BuildMsg(MT_DELETE_ROSPEC, W32(ROSPEC_ID));
  Resp := SendAndReceive(Msg, MT_DELETE_ROSPEC_RESP, 2000);
end;

procedure TLLRPThread.SendCloseConnection;
var
  Msg: TBytes;
begin
  SetLength(Msg, 0);
  Msg := BuildMsg(MT_CLOSE_CONNECTION, Msg);
  SendMsg(Msg);
  Sleep(200);
end;

procedure TLLRPThread.SendKeepAliveAck(MsgID: UInt32);
var
  Msg: TBytes;
begin
  SetLength(Msg, 0);
  Msg := BuildMsg(MT_KEEPALIVE_ACK, Msg, MsgID);
  SendMsg(Msg);
end;

{ ══ TLLRPThread — I/O ══════════════════════════════════════════════════════ }

procedure TLLRPThread.SendMsg(const Msg: TBytes);
begin
  if Length(Msg) > 0 then
    FSock.SendBuffer(@Msg[0], Length(Msg));
end;

function TLLRPThread.ReadMessage(TimeoutMs: Integer; out MsgType: Word;
                                 out MsgID: UInt32; out Body: TBytes): Boolean;
var
  Hdr     : array[0..LLRP_HDR_SIZE - 1] of Byte;
  TypeWord: Word;
  TotalLen: UInt32;
  BodyLen : Integer;
  NRead   : Integer;
begin
  Result  := False;
  MsgType := 0;
  MsgID   := 0;
  SetLength(Body, 0);

  { Leggi header 10 byte }
  NRead := FSock.RecvBufferEx(@Hdr[0], LLRP_HDR_SIZE, TimeoutMs);
  if (NRead <> LLRP_HDR_SIZE) or (FSock.LastError <> 0) then Exit;

  TypeWord := (Word(Hdr[0]) shl 8) or Hdr[1];
  MsgType  := TypeWord and $03FF;
  TotalLen := (UInt32(Hdr[2]) shl 24) or (UInt32(Hdr[3]) shl 16)
            or (UInt32(Hdr[4]) shl 8) or Hdr[5];
  MsgID    := (UInt32(Hdr[6]) shl 24) or (UInt32(Hdr[7]) shl 16)
            or (UInt32(Hdr[8]) shl 8) or Hdr[9];

  BodyLen := Integer(TotalLen) - LLRP_HDR_SIZE;
  if BodyLen < 0 then Exit;

  if BodyLen > 0 then
  begin
    SetLength(Body, BodyLen);
    NRead := FSock.RecvBufferEx(@Body[0], BodyLen, TimeoutMs);
    if NRead <> BodyLen then Exit;
  end;

  Result := True;
end;

function TLLRPThread.SendAndReceive(const Msg: TBytes; ExpectedType: Word;
                                    TimeoutMs: Integer): TBytes;
var
  MsgType : Word;
  MsgID   : UInt32;
  Body    : TBytes;
begin
  SetLength(Result, 0);
  SendMsg(Msg);
  if ReadMessage(TimeoutMs, MsgType, MsgID, Body) then
  begin
    if MsgType = ExpectedType then
      Result := Body
    else if MsgType = MT_ERROR_MESSAGE then
    begin
      FTmpMsg := Format('LLRP ERROR_MESSAGE ricevuto (atteso tipo %d)', [ExpectedType]);
      Synchronize(@SyncError);
    end;
  end;
end;

{ ══ TLLRPThread — parser report tag ══════════════════════════════════════════ }

function TLLRPThread.ExtractParam(const Data: TBytes; StartOffset: Integer;
                                  SearchType: Word; out ParamBody: TBytes): Boolean;
{
  Cerca il primo parametro TLV di tipo SearchType a partire da StartOffset.
  TLV header: Type(2B) | Length(2B, include i 4B header)
  TV params (tipo < 128): Type(1B) | body di lunghezza fissa per tipo
}
var
  Offset  : Integer;
  PType   : Word;
  PLen    : Word;
  DataLen : Integer;
begin
  Result  := False;
  SetLength(ParamBody, 0);
  DataLen := Length(Data);
  Offset  := StartOffset;

  while Offset < DataLen - 3 do
  begin
    PType := (Word(Data[Offset]) shl 8) or Data[Offset + 1];

    if (PType and $8000) <> 0 then
    begin
      { TLV parameter }
      PType := PType and $03FF;
      if Offset + 3 >= DataLen then Break;
      PLen := (Word(Data[Offset + 2]) shl 8) or Data[Offset + 3];
      if (PLen < 4) or (Offset + PLen > DataLen) then Break;

      if PType = SearchType then
      begin
        SetLength(ParamBody, PLen - 4);
        if PLen > 4 then
          Move(Data[Offset + 4], ParamBody[0], PLen - 4);
        Result := True;
        Exit;
      end;
      Inc(Offset, PLen);
    end
    else
    begin
      { TV parameter — lunghezze fisse per tipo }
      PType := Data[Offset] and $7F;
      case PType of
        PT_EPC_96        : begin
                             if PType = SearchType then
                             begin
                               SetLength(ParamBody, 12);
                               if Offset + 13 <= DataLen then
                                 Move(Data[Offset + 1], ParamBody[0], 12);
                               Result := True; Exit;
                             end;
                             Inc(Offset, 13);  // 1 type + 12 EPC bytes
                           end;
        PT_ANTENNA_ID    : begin
                             if PType = SearchType then
                             begin
                               SetLength(ParamBody, 2);
                               if Offset + 3 <= DataLen then
                                 Move(Data[Offset + 1], ParamBody[0], 2);
                               Result := True; Exit;
                             end;
                             Inc(Offset, 3);
                           end;
        PT_PEAK_RSSI     : begin
                             if PType = SearchType then
                             begin
                               SetLength(ParamBody, 1);
                               if Offset + 2 <= DataLen then
                                 ParamBody[0] := Data[Offset + 1];
                               Result := True; Exit;
                             end;
                             Inc(Offset, 2);
                           end;
        PT_TAG_SEEN_COUNT: begin
                             if PType = SearchType then
                             begin
                               SetLength(ParamBody, 2);
                               if Offset + 3 <= DataLen then
                                 Move(Data[Offset + 1], ParamBody[0], 2);
                               Result := True; Exit;
                             end;
                             Inc(Offset, 3);
                           end;
      else
        Inc(Offset, 1);  // tipo TV sconosciuto: avanza di 1 (best effort)
      end;
    end;
  end;
end;

function TLLRPThread.ExtractEPC(const Data: TBytes; Offset, Len: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := Offset to Offset + Len - 1 do
    Result := Result + IntToHex(Data[i], 2);
end;

procedure TLLRPThread.ParseTagReportData(const Data: TBytes; Offset: Integer; Len: Integer);
{
  Estrae EPC, AntennaID, RSSI, SeenCount da un TagReportData TLV.
  EPC può arrivare come:
  - EPC_96 TV param (tipo 13, 12 byte fissi = 96 bit)
  - EPCData TLV  (tipo 241, con BitCount + EPC bytes variabili)
}
var
  Tag      : TTagData;
  Sub      : TBytes;
  SubSlice : TBytes;
  SubOff   : Integer;
  SubLen   : Integer;
  BitCount : Word;
  ByteLen  : Integer;
begin
  FillChar(Tag, SizeOf(Tag), 0);
  Tag.Timestamp := Now;

  SubOff := Offset;
  SubLen := Len;
  SetLength(Sub, SubLen);
  if SubLen > 0 then Move(Data[Offset], Sub[0], SubLen);

  { EPC_96 (TV param, tipo 13) → 12 byte }
  if ExtractParam(Sub, 0, PT_EPC_96, SubSlice) and (Length(SubSlice) = 12) then
    Tag.EPC := ExtractEPC(SubSlice, 0, 12)
  else if ExtractParam(Sub, 0, PT_EPC_DATA, SubSlice) and (Length(SubSlice) >= 2) then
  begin
    { EPCData TLV: BitCount(2) + EPC bytes }
    BitCount := (Word(SubSlice[0]) shl 8) or SubSlice[1];
    ByteLen  := (BitCount + 7) div 8;
    if Length(SubSlice) >= 2 + ByteLen then
      Tag.EPC := ExtractEPC(SubSlice, 2, ByteLen);
  end;

  { AntennaID (TV, tipo 1) → 2 byte }
  if ExtractParam(Sub, 0, PT_ANTENNA_ID, SubSlice) and (Length(SubSlice) >= 2) then
    Tag.AntennaID := (Word(SubSlice[0]) shl 8) or SubSlice[1];

  { PeakRSSI (TV, tipo 6) → 1 byte signed }
  if ExtractParam(Sub, 0, PT_PEAK_RSSI, SubSlice) and (Length(SubSlice) >= 1) then
    Tag.RSSI := ShortInt(SubSlice[0]);

  { TagSeenCount (TV, tipo 10) → 2 byte }
  if ExtractParam(Sub, 0, PT_TAG_SEEN_COUNT, SubSlice) and (Length(SubSlice) >= 2) then
    Tag.SeenCount := (Word(SubSlice[0]) shl 8) or SubSlice[1];

  if Tag.EPC <> '' then
  begin
    FTmpTag := Tag;
    Synchronize(@SyncTag);
  end;
end;

procedure TLLRPThread.ParseROAccessReport(const Body: TBytes);
{
  RO_ACCESS_REPORT body: sequenza di TagReportData TLV.
  Ogni TagReportData TLV: Type=240, Length(2), body.
}
var
  Offset  : Integer;
  PType   : Word;
  PLen    : Word;
  BodyLen : Integer;
begin
  Offset  := 0;
  BodyLen := Length(Body);

  while Offset < BodyLen - 3 do
  begin
    PType := (Word(Body[Offset]) shl 8) or Body[Offset + 1];
    PType := PType and $03FF;
    PLen  := (Word(Body[Offset + 2]) shl 8) or Body[Offset + 3];

    if (PLen < 4) or (Offset + PLen > BodyLen) then Break;

    if PType = PT_TAG_REPORT_DATA then
      ParseTagReportData(Body, Offset + 4, PLen - 4);

    Inc(Offset, PLen);
  end;
end;

{ ══ TLLRPThread — Synchronize ════════════════════════════════════════════════ }

procedure TLLRPThread.SyncTag;
begin
  if Assigned(FReader.OnTagRead) then FReader.OnTagRead(FReader, FTmpTag);
end;

procedure TLLRPThread.SyncStatus;
begin
  if Assigned(FReader.OnStatus) then FReader.OnStatus(FReader, FTmpMsg);
end;

procedure TLLRPThread.SyncError;
begin
  if Assigned(FReader.OnError) then FReader.OnError(FReader, FTmpMsg);
end;

{ ══ TLLRPThread — Execute ═════════════════════════════════════════════════════ }

procedure TLLRPThread.Execute;
var
  MsgType : Word;
  MsgID   : UInt32;
  Body    : TBytes;
begin
  { ── 1. Connessione TCP ───────────────────────────────────────────────── }
  FSock.Connect(FReader.Host, IntToStr(FReader.Port));
  if FSock.LastError <> 0 then
  begin
    FTmpMsg := Format('Connessione fallita a %s:%d — %s',
                      [FReader.Host, FReader.Port, FSock.LastErrorDesc]);
    Synchronize(@SyncError);
    Exit;
  end;

  FTmpMsg := Format('Connesso a %s:%d', [FReader.Host, FReader.Port]);
  Synchronize(@SyncStatus);

  { ── 2. Attendi READER_EVENT_NOTIFICATION di connessione ─────────────── }
  if ReadMessage(3000, MsgType, MsgID, Body) then
  begin
    if MsgType <> MT_READER_EVENT_NOTIFICATION then
    begin
      FTmpMsg := Format('Atteso READER_EVENT_NOTIFICATION, ricevuto tipo %d', [MsgType]);
      Synchronize(@SyncError);
    end;
  end;

  { ── 3. Configura reader (potenza antenne) ────────────────────────────── }
  SendSetReaderConfig;

  { ── 4. Aggiungi ROSpec ────────────────────────────────────────────────── }
  SendAddROSpec;

  { ── 5. Abilita ROSpec ────────────────────────────────────────────────── }
  SendEnableROSpec;

  FTmpMsg := 'Reader configurato — pronto per StartScan';
  Synchronize(@SyncStatus);

  { ── 6. Loop principale ───────────────────────────────────────────────── }
  while not Terminated do
  begin
    if not ReadMessage(500, MsgType, MsgID, Body) then
    begin
      if FSock.LastError <> 0 then
      begin
        FTmpMsg := Format('Connessione persa (err %d)', [FSock.LastError]);
        Synchronize(@SyncError);
        Break;
      end;
      Continue;  // timeout semplice, normale
    end;

    case MsgType of
      MT_RO_ACCESS_REPORT:
        ParseROAccessReport(Body);

      MT_KEEPALIVE:
        SendKeepAliveAck(MsgID);

      MT_READER_EVENT_NOTIFICATION:
        begin
          { Notifiche varie (antenna connect/disconnect, ecc.) — ignoriamo }
        end;

      MT_ERROR_MESSAGE:
        begin
          FTmpMsg := 'LLRP ERROR_MESSAGE ricevuto durante lo scan';
          Synchronize(@SyncError);
        end;
    end;

    { ── Riconfigurazione live richiesta dall'esterno ─────────────────── }
    if FReader.FPendingReconfig then
    begin
      FReader.FPendingReconfig := False;
      if FReader.FScanning then
      begin
        { Stop → riconfigura → restart in modo trasparente }
        SendStopROSpec;
        SendSetReaderConfig;
        SendStartROSpec;
      end
      else
        SendSetReaderConfig;
    end;
  end;

  { ── 7. Pulizia ────────────────────────────────────────────────────────── }
  if FReader.Scanning then
    SendStopROSpec;
  SendDeleteROSpec;
  SendCloseConnection;

  FSock.CloseSocket;
end;

{ ══ TLLRPThread — lifecycle ══════════════════════════════════════════════════ }

constructor TLLRPThread.Create(AReader: TImpinjR700);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FReader       := AReader;
  FMsgIDCounter := 0;
  FSock         := TTCPBlockSocket.Create;
end;

destructor TLLRPThread.Destroy;
begin
  FSock.CloseSocket;
  FSock.Free;
  inherited;
end;

function TLLRPThread.Connected: Boolean;
begin
  Result := FSock.LastError = 0;
end;

{ ══ TImpinjR700 ══════════════════════════════════════════════════════════════ }

constructor TImpinjR700.Create(const AHost: string; APort: Integer);
var
  i: Integer;
begin
  inherited Create;
  FHost     := AHost;
  FPort     := APort;
  FActive   := False;
  FScanning := False;
  FThread   := nil;
  FPendingReconfig := False;
  FConfigLock := TCriticalSection.Create;

  { Default: tutte le antenne abilitate, 23 dBm, -70 dBm sensibilità }
  for i := 1 to MAX_ANTENNAS do
  begin
    FAntennas[i].Enabled   := True;
    FAntennas[i].PowerDBm  := 23.0;
    FAntennas[i].RxSensDBm := -70;
  end;
end;

destructor TImpinjR700.Destroy;
begin
  Close;
  FConfigLock.Free;
  inherited;
end;

function TImpinjR700.GetAntennaConfig(Index: Integer): TAntennaConfig;
begin
  if (Index >= 1) and (Index <= MAX_ANTENNAS) then
    Result := FAntennas[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TImpinjR700.SetAntennaConfig(Index: Integer; const Value: TAntennaConfig);
begin
  if (Index >= 1) and (Index <= MAX_ANTENNAS) then
    FAntennas[Index] := Value;
end;

procedure TImpinjR700.SetAntennaPower(AntennaID: Integer; PowerDBm: Double);
begin
  if (AntennaID >= 1) and (AntennaID <= MAX_ANTENNAS) then
  begin
    FConfigLock.Acquire;
    try
      FAntennas[AntennaID].PowerDBm := PowerDBm;
    finally
      FConfigLock.Release;
    end;
  end;
end;

procedure TImpinjR700.SetAntennaRxSens(AntennaID: Integer; SensDBm: Integer);
begin
  if (AntennaID >= 1) and (AntennaID <= MAX_ANTENNAS) then
  begin
    FConfigLock.Acquire;
    try
      FAntennas[AntennaID].RxSensDBm := SensDBm;
    finally
      FConfigLock.Release;
    end;
  end;
end;

procedure TImpinjR700.SetAntennaEnabled(AntennaID: Integer; Enabled: Boolean);
begin
  if (AntennaID >= 1) and (AntennaID <= MAX_ANTENNAS) then
  begin
    FConfigLock.Acquire;
    try
      FAntennas[AntennaID].Enabled := Enabled;
    finally
      FConfigLock.Release;
    end;
  end;
end;

{ ── Metodi live — aggiornano config e segnalano il thread ─────────────────── }

procedure TImpinjR700.SetAntennaPowerLive(AntennaID: Integer; PowerDBm: Double);
begin
  if (AntennaID >= 1) and (AntennaID <= MAX_ANTENNAS) then
  begin
    FConfigLock.Acquire;
    try
      FAntennas[AntennaID].PowerDBm := PowerDBm;
      FPendingReconfig := True;
    finally
      FConfigLock.Release;
    end;
  end;
end;

procedure TImpinjR700.SetAntennaRxSensLive(AntennaID: Integer; SensDBm: Integer);
begin
  if (AntennaID >= 1) and (AntennaID <= MAX_ANTENNAS) then
  begin
    FConfigLock.Acquire;
    try
      FAntennas[AntennaID].RxSensDBm := SensDBm;
      FPendingReconfig := True;
    finally
      FConfigLock.Release;
    end;
  end;
end;

procedure TImpinjR700.SetAllAntennasPowerLive(PowerDBm: Double);
var
  i: Integer;
begin
  FConfigLock.Acquire;
  try
    for i := 1 to MAX_ANTENNAS do
      if FAntennas[i].Enabled then
        FAntennas[i].PowerDBm := PowerDBm;
    FPendingReconfig := True;
  finally
    FConfigLock.Release;
  end;
end;

procedure TImpinjR700.SetAllAntennasRxSensLive(SensDBm: Integer);
var
  i: Integer;
begin
  FConfigLock.Acquire;
  try
    for i := 1 to MAX_ANTENNAS do
      if FAntennas[i].Enabled then
        FAntennas[i].RxSensDBm := SensDBm;
    FPendingReconfig := True;
  finally
    FConfigLock.Release;
  end;
end;

procedure TImpinjR700.Open;
begin
  if FActive then Exit;
  FThread := TLLRPThread.Create(Self);
  FActive := True;
  FThread.Start;
end;

procedure TImpinjR700.Close;
begin
  if not FActive then Exit;
  FScanning := False;
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  FActive := False;
end;

procedure TImpinjR700.StartScan;
begin
  if not FActive or FScanning then Exit;
  FScanning := True;
  FThread.SendStartROSpec;
end;

procedure TImpinjR700.StopScan;
begin
  if not FActive or not FScanning then Exit;
  FScanning := False;
  FThread.SendStopROSpec;
end;

end.
