unit uR700Reader;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, blcksock, synautil, fpjson, jsonparser,
  uLBBaseThread, uTimedoutCriticalSection, uEventsManager, uLBLogger,
  fgl, DateUtils,
  uRFIDCommon;

type

  { TR700RFMode
    Modalità RF (rfMode) che determina il compromesso tra velocità di lettura
    e sensibilità. I valori statici (0-4) hanno comportamento fisso; le
    modalità AutoSet (1000-1004) commutano automaticamente tra più modalità
    durante l'inventory adattandosi alla popolazione di tag rilevata.

    rmMaxThroughput   (0)  Massima velocità, sensibilità minima. Adatta per
                           tag vicini e facili da leggere. Non legge tag
                           difficili (orientamento sfavorevole, distanza).

    rmHybrid          (1)  Velocità alta, sensibilità media. Buon compromesso
                           per inventory rapidi con tag mediamente accessibili.

    rmDenseReaderM4   (2)  Modalità Dense Reader con Miller M4. Lenta ma molto
                           sensibile. Riduce l'interferenza tra reader adiacenti
                           (DRM). Adatta per ambienti con più reader.

    rmDenseReaderM8   (3)  Come M4 ma con Miller M8: ancora più lenta e ancora
                           più sensibile. Massima immunità alle interferenze.
                           Usare quando M4 non è sufficiente.

    rmMaxMiller       (4)  Massimo Miller, sensibilità estrema a discapito
                           della velocità. Casi edge con tag molto difficili.

    rmAutosetDense   (1000) AutoSet Dense Reader: cicla tra M4 e M8 adattandosi.
                           Raccomandato quando ci sono più reader nella stessa area.

    rmAutosetSingle  (1001) AutoSet Single Reader: cicla tra modalità veloci e
                           sensibili in assenza di interferenze tra reader.

    rmAutosetDeepScan (1002) AutoSet Dense Reader Deep Scan: cicla tra tutte le
                           modalità per massimizzare il numero di tag letti.
                           Ottimale per inventory statici (armadi, scaffali).
                           DEFAULT corrente — buon punto di partenza.

    rmAutosetFast    (1003) AutoSet Static Fast: cicla tra modalità 0 e 1.
                           Raccomandato per popolazioni grandi (>200 tag) dove
                           serve throughput elevato. Con Session 2 o 3 evita
                           di reinventoriare sempre gli stessi tag facili.

    rmAutosetDenseSlow (1004) AutoSet Static Dense Reader: cicla tra modalità
                           2 e 3. Per ambienti con interferenze e tag difficili.
                           Lento ma completo.  }

  TR700RFMode = (
    rmMaxThroughput   = 0,
    rmHybrid          = 1,
    rmDenseReaderM4   = 2,
    rmDenseReaderM8   = 3,
    rmMaxMiller       = 4,
    rmAutosetDense    = 1000,
    rmAutosetSingle   = 1001,
    rmAutosetDeepScan = 1002,
    rmAutosetFast     = 1003,
    rmAutosetDenseSlow = 1004
  );

  { TR700SearchMode
    Modalità di ricerca (inventorySearchMode): controlla il comportamento
    del reader rispetto allo stato A/B dei tag EPC Gen2.

    smSingleTarget          Il reader inventoria i tag in stato A finché tutti
                            non hanno risposto (stato B), poi si ferma.
                            Veloce ma non aggiorna i tag già letti.
                            Utile con Session 0/1 su popolazioni piccole.

    smDualTarget            Il reader alterna tra A→B e B→A continuamente.
                            Ogni tag viene riletto periodicamente.
                            Ideale per inventory continui e monitoraggio
                            della presenza (smart shelf, tool tracking).
                            DEFAULT corrente.

    smDualTargetBtoASelect  Come DualTarget ma con un Select esplicito B→A
                            prima di ogni round. Comportamento più deterministico
                            con Session 2 o 3 su popolazioni grandi. Raccomandato
                            da Impinj come alternativa a Single Target Session 1
                            per popolazioni >200 tag con modalità lente. }

  TR700SearchMode = (
    smSingleTarget         = 0,
    smDualTarget           = 1,
    smDualTargetBtoASelect = 2
  );

  { TR700Session
    La sessione (inventorySession) determina quale coppia di flag A/B il
    reader usa sui tag. Ogni tag mantiene flag indipendenti per sessioni 0-3.

    Session 0  Persistenza brevissima (~500ms senza alimentazione RF).
               Usare solo con un singolo reader e tag sempre in campo.
               Il più veloce ma il meno affidabile con tag che entrano/escono.

    Session 1  Persistenza breve (~5s). Il default più comune per un singolo
               reader. Con Single Target può bloccarsi a leggere sempre i
               tag più facili su popolazioni grandi — preferire Session 2/3
               in quel caso.

    Session 2  Persistenza lunga (~2s in campo, indefinita fuori campo).
               Raccomandato per popolazioni grandi o con più reader.
               Con DualTarget o DualTargetBtoASelect garantisce che ogni
               tag venga riletto regolarmente.

    Session 3  Come Session 2. Usare quando Session 2 mostra interferenze
               tra reader nella stessa area. }

  TR700Session = (
    ssSession0 = 0,
    ssSession1 = 1,
    ssSession2 = 2,
    ssSession3 = 3
  );

  { TR700AntennaConfig
    Configurazione completa di una singola porta antenna.

    Raccomandazioni per scenario con <100 tag:
      RFMode     = rmAutosetDeepScan (1002)  — legge anche i tag difficili
      SearchMode = smDualTarget              — aggiorna continuamente la presenza
      Session    = ssSession1                — sufficiente per pochi tag
      EstimatedTagPopulation = 32..128

    Raccomandazioni per scenario con 400-800 tag:
      RFMode     = rmAutosetFast (1003)      — priorità al throughput
      SearchMode = smDualTargetBtoASelect    — evita di bloccarsi sui tag facili
      Session    = ssSession2                — persistenza lunga, più equo
      EstimatedTagPopulation = 500..800      — va impostato vicino al reale:
                                               sottostimare causa collisioni,
                                               sovrastimare rallenta il Q-round }

  TR700AntennaConfig = record
    Port                  : Byte;
    Enabled               : Boolean;
    TxPowerDbm            : Byte;           // Potenza TX in dBm (10-30)
    RFMode                : TR700RFMode;    // Modalità RF (vedi TR700RFMode)
    SearchMode            : TR700SearchMode;// Modalità ricerca (vedi TR700SearchMode)
    Session               : TR700Session;  // Sessione EPC Gen2 (vedi TR700Session)
    EstimatedTagPopulation: Word;           // Stima popolazione tag nell'area
  end;
  PR700AntennaConfig = ^TR700AntennaConfig;
  TR700AntennaList   = specialize TFPGList<PR700AntennaConfig>;

  { TR700RFPreset
    Preset di configurazione RF pronti all'uso per scenari tipici.
    Contengono i parametri RF (RFMode, SearchMode, Session,
    EstimatedTagPopulation); Port e TxPowerDbm si passano a parte a
    TR700Manager.AddAntenna, che clona internamente il preset ricevuto.

    Uso:
      _presets := TR700RFPresets.Create;
      FMgr.AddAntenna(1, 29, _presets.Items[2]);  // Large Inventory
      _presets.Free;  // AddAntenna ha già clonato il preset
      FMgr.StartInventory;  }

  TR700RFPreset = class(TRFIDAntennaPreset)
  public
    RFMode                : TR700RFMode;
    SearchMode            : TR700SearchMode;
    Session               : TR700Session;
    EstimatedTagPopulation: Word;

    function Clone: TRFIDAntennaPreset; override;
  end;

  { Collezione di preset predefiniti come costanti tipizzate.

    SmallInventory      Inventory statico con <100 tag in campo.
                        Deep scan + dual target: massima accuratezza,
                        legge anche i tag con orientamento sfavorevole.
                        Session 1, popolazione stimata 64.

    LargeInventory      Inventory con 400-800 tag.
                        AutoSet Fast + select B→A: priorità al throughput,
                        evita di bloccarsi sui tag più facili.
                        Session 2, popolazione stimata 600.

    DenseReader         Ambienti con più reader Impinj nella stessa area.
                        Modalità Dense Reader M4 con timing anti-interferenza.
                        Session 2, popolazione stimata 128.

    Monitoring          Monitoraggio continuo della presenza (smart shelf,
                        tool tracking). Dual target con sessione lunga:
                        rileva rapidamente l'aggiunta e la rimozione di tag.
                        Session 2, popolazione stimata 32.

    HighSensitivity     Tag molto difficili da leggere (orientamento fisso
                        sfavorevole, distanza elevata, ostacoli).
                        Dense Reader M8: la modalità più lenta e sensibile.
                        Session 1, popolazione stimata 32.

    FastScan            Tag in movimento rapido (nastro trasportatore,
                        portale). Max throughput, cattura la prima risposta.
                        Session 0 (persistenza brevissima), popolazione 32.

    MultiReader         Come DenseReader ma con Session 3 e M8 per ambienti
                        con 3+ reader adiacenti e alta densità di tag.
                        La combinazione più conservativa disponibile.         }


  { TR700RFPresets }

  TR700RFPresets = class(TRFIDFAntennaPresets)
  strict private
    function Standard:         TR700RFPreset;
    function SmallInventory:   TR700RFPreset;
    function LargeInventory:   TR700RFPreset;
    function DenseReader:      TR700RFPreset;
    function Monitoring:       TR700RFPreset;
    function HighSensitivity:  TR700RFPreset;
    function FastScan:         TR700RFPreset;
    function MultiReader:      TR700RFPreset;
  public
    constructor Create; override;
  end;

  TTagInfoReceivedEvent = procedure(Sender: TObject; const aJSON: String) of object;

  { NOTA: TR700Tag, TR700TagList e i tipi record associati (TAntennaInfo,
    TR700TagData, ecc.) sono stati rimossi: TR700Manager ora usa direttamente
    TRFIDTag/TRFIDTagList/TRFIDTagData/TRFIDAntennasInfo definiti in
    uRFIDCommon.pas, condivisi con il driver Zebra FXR90. Questo rende i due
    driver interscambiabili dietro l'interfaccia comune TRFIDManager. }

  { TR700ReaderClient
    Thread che gestisce la connessione TCP/SSL verso il reader Impinj R700,
    avvia l'inventory in modalità transient e fa streaming dei tag JSON. }

  TR700ReaderClient = class(TLBBaseThread)
  strict private
    type
      TInternalState = (stConnecting  = 0,
                        stConfiguring = 1,
                        stStreaming   = 2,
                        stError       = 3);

  strict private
    FOnReaderConnected : TNotifyEvent;
    FOnReaderDisconnected : TNotifyEvent;
    FOnNewTagInfoReceived : TTagInfoReceivedEvent;
    FState         : TInternalState;
    { Socket di controllo: usato per start/stop inventory. }
    FControlSocket : TTCPBlockSocket;
    { Socket di stream: dedicato a GET /api/v1/data/stream.
      Separato dal control socket per evitare timeout sullo stop. }
    FStreamSocket  : TTCPBlockSocket;
    FHost          : string;
    FPort          : Word;
    FUsername      : string;
    FPassword      : string;
    FAntennas      : TR700AntennaList;

    function  BuildBasicAuth: string;
    function  ConnectToReader: Boolean;
    function  ConnectStreamToReader: Boolean;
    procedure DisconnectFromReader;
    procedure DisconnectStreamFromReader;
    function  SendHTTPRequest(const AMethod, APath: string;
                              const ABody: string = '';
                              const AContentType: string = ''): Boolean;
    function  StartInventory: Boolean;
    function  StreamTags: Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure SetConnection(const AHost: string; APort: Word);
    procedure SetCredentials(const AUsername, APassword: string);

    procedure ClearAntennas;
    procedure AddAntenna(Port: Byte;
                         TxPowerDbm:             Byte            = 29;
                         Enabled:                Boolean         = True;
                         RFMode:                 TR700RFMode     = rmAutosetDeepScan;
                         SearchMode:             TR700SearchMode = smDualTarget;
                         Session:                TR700Session    = ssSession1;
                         EstimatedTagPopulation: Word            = 32);
    procedure RemoveAntenna(Port: Byte);
    procedure SetAntennaEnabled(Port: Byte; Enabled: Boolean);
    procedure SetAntennaPower(Port: Byte; TxPowerDbm: Byte);
    function  GetAntennaConfig(Port: Byte): PR700AntennaConfig;
    function  AntennaCount: Integer;

    { Aggiorna i parametri RF di una porta esistente senza ricreare l'oggetto }
    procedure SetAntennaRFConfig(Port: Byte;
                                 ARFMode:     TR700RFMode;
                                 ASearchMode: TR700SearchMode;
                                 ASession:    TR700Session;
                                 ATagPop:     Word);

    function  BuildInventoryBody(ACfg: PR700AntennaConfig): string;
    function  StopInventory: Boolean;

    property OnNewTagInfoReceived: TTagInfoReceivedEvent write FOnNewTagInfoReceived;
    property OnReaderConnected: TNotifyEvent write FOnReaderConnected;
    property OnReaderDisconnected: TNotifyEvent write FOnReaderDisconnected;
  end;

  TR700Manager = class; // dichiarazione anticipata: serve al Consumer qui sotto

  { TR700Consumer
    Thread che consuma il JSON prodotto da TR700ReaderClient e aggiorna
    direttamente la TRFIDTagList ricevuta alla creazione. Non ha alcun
    riferimento al Manager: stesso principio adottato da TFXR90Consumer
    in uFXR90Manager.pas, per evitare il riferimento incrociato
    Manager↔Consumer. }

  TR700Consumer = class(TLBBaseThread)
  private
    FTagList      : TRFIDTagList;
    FTagQueue     : TStringList;
    FTagCS        : TTimedOutCriticalSection;
    FNewDataEvent : PRTLEvent;

    procedure ProcessJSON(const AJSON: string);
    function ProcessJSONObject(AData: TJSONData): Boolean;
    procedure NewTagReceived(Sender: TObject; const aJSON: String);

  protected
    procedure Execute; override;

  public
    constructor Create(ATagList: TRFIDTagList;
                       aReader: TR700ReaderClient); reintroduce;
    destructor  Destroy; override;

    procedure Terminate; override;
  end;

  { TR700Manager
    Oggetto di alto livello: coordina TR700ReaderClient e TR700Consumer
    ed espone la TRFIDTagList (uRFIDCommon.pas) con i relativi eventi.
    Discendendo da TRFIDManager, è interscambiabile con TFXR90Manager
    dietro la stessa interfaccia comune (TagList, EventsManager, AddAntenna,
    StartInventory, StopInventory).

    Flusso degli eventi dal reader alla GUI:
    ─────────────────────────────────────────────────────────────────────
    TR700ReaderClient ──OnNewTagInfoReceived──► TR700Consumer.ProcessJSON
                                                        │
                                     TR700Consumer.ProcessJSONObject(Data)
                                                        │
                                             TagList.GetOrCreate(EPC,...)
                                              │                   │
                                        [nuovo EPC]          [EPC noto]
                                              │                   │
                                    TRFIDTag.Create        TRFIDTag.Update
                                              │                   │
                                  TagList →  cEvent_TagAdded   cEvent_TagUpdated (sul tag)
                                  (Sender=tag)│               cEvent_TagChanged (sulla lista)
                                              │
                                  TRFIDManager.NewTagAdded (ereditato, automatico)
                                              │
                                  Manager →  cEvent_NewTag (forward, Sender=tag)
    ─────────────────────────────────────────────────────────────────────
    Il forward cEvent_TagAdded → cEvent_NewTag è già gestito da
    TRFIDManager.Create: non serve (anzi, va evitato) registrare qui un
    proprio listener aggiuntivo su cEvent_TagAdded.

    Utilizzo tipico:
      FMgr := TR700Manager.Create('192.168.1.100', 443, 'root', 'impinj');

      FMgr.EventsManager.AddEventListener(
        TRFIDManager.cEvent_NewTag, @Self.OnTagAdded, Self.EventsManager);

      // Qualsiasi aggiornamento → un unico listener su TagList
      FMgr.TagList.EventsManager.AddEventListener(
        TRFIDTagList.cEvent_TagChanged, @Self.OnTagChanged, Self.EventsManager);

      // Oppure registrarsi sul singolo tag quando arriva cEvent_NewTag:
      procedure TMyForm.OnTagAdded(Sender: TObject);
      begin
        (Sender as TRFIDTag).EventsManager.AddEventListener(
          TRFIDTag.cEvent_TagUpdated, @Self.OnTagUpdated, Self.EventsManager);
      end;

      FMgr.AddAntenna(1, 29, TR700RFPresets.Create.Items[0]);
      FMgr.StartInventory;
      ...
      FMgr.StopInventory;
      FMgr.Free;

    ATTENZIONE threading: tutti gli eventi vengono alzati nel thread del
    TR700Consumer. I listener che toccano la UI devono usare
    TThread.Queue / TThread.Synchronize. }

  TR700Manager = class(TRFIDManager)
  strict private
    FReader        : TR700ReaderClient;
    FConsumer      : TR700Consumer;
    FHost          : string;
    FPort          : Word;
    FUsername      : string;
    FPassword      : string;

    { Configurazione antenne: tre array paralleli, stessa lunghezza —
      stesso schema adottato da TFXR90Manager in uFXR90Manager.pas. }
    FAntennaPort   : array of Integer;
    FAntennaDBm    : array of Integer;
    FAntennaPreset : array of TR700RFPreset;  // copie interne, possedute dal manager

    procedure ClearAntennaArrays;
    procedure ReaderConnected(Sender: TObject);

  public
    constructor Create(const AHost: string;
                       APort: Word             = 443;
                       const AUsername: string = 'root';
                       const APassword: string = 'impinj'); reintroduce;
    destructor Destroy; override;

    { Start / Stop sono bloccanti: Stop attende la terminazione dei thread }
    function StartInventory(): Boolean; override;
    function StopInventory(): Boolean; override;

    { aPreset deve essere un TR700RFPreset: viene clonato internamente,
      il chiamante può liberarlo subito dopo la chiamata. }
    procedure AddAntenna(aPort: Integer; aDBm: Integer; aPreset: TRFIDAntennaPreset); override;
    procedure ClearAntennas;
  end;

implementation

uses
  Base64, ssl_openssl3, uLBUtils;

const
  CRLF            = #13#10;
  cMaxAntennaCount = 8;


function Base64ToHex(const ABase64: string): string;
var
  _decoded: string;
  _b64:     string;
  i:        Integer;
begin
  { L'API Impinj usa Base64 URL-safe: - → +  e  _ → / }
  _b64 := StringReplace(ABase64, '-', '+', [rfReplaceAll]);
  _b64 := StringReplace(_b64, '_', '/', [rfReplaceAll]);

  _decoded := DecodeStringBase64(_b64);

  Result := '';
  for i := 1 to Length(_decoded) do
    Result := Result + IntToHex(Ord(_decoded[i]), 2);
end;

{ ============================================================ }
{ TR700RFPresets                                               }
{ ============================================================ }

function TR700RFPreset.Clone: TRFIDAntennaPreset;
var
  _c: TR700RFPreset;
begin
  _c := TR700RFPreset.Create;
  _c.Description             := Self.Description;
  _c.RFMode                  := Self.RFMode;
  _c.SearchMode              := Self.SearchMode;
  _c.Session                 := Self.Session;
  _c.EstimatedTagPopulation  := Self.EstimatedTagPopulation;
  Result := _c;
end;

function TR700RFPresets.Standard: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmAutosetDeepScan; // rmAutosetSingle;
  Result.SearchMode             := smDualTarget;
  Result.Session                := ssSession1;
  Result.EstimatedTagPopulation := 32;
  Result.Description            := 'Standard (single reader, general purpose)';
end;

function TR700RFPresets.SmallInventory: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmAutosetDeepScan;
  Result.SearchMode             := smDualTarget;
  Result.Session                := ssSession1;
  Result.EstimatedTagPopulation := 64;
  Result.Description            := 'Small Inventory (<100 tag)';
end;

function TR700RFPresets.LargeInventory: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmAutosetDeepScan; // rmAutosetFast;
  Result.SearchMode             := smDualTargetBtoASelect;
  Result.Session                := ssSession2;
  Result.EstimatedTagPopulation := 600;
  Result.Description            := 'Large Inventory (400-800 tag)';
end;

function TR700RFPresets.DenseReader: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmAutosetDenseSlow;
  Result.SearchMode             := smDualTargetBtoASelect;
  Result.Session                := ssSession2;
  Result.EstimatedTagPopulation := 128;
  Result.Description            := 'Dense Reader (multi-reader environment)';
end;

function TR700RFPresets.Monitoring: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmAutosetDeepScan;
  Result.SearchMode             := smDualTarget;
  Result.Session                := ssSession2;
  Result.EstimatedTagPopulation := 32;
  Result.Description            := 'Monitoring (smart shelf / tool tracking)';
end;

function TR700RFPresets.HighSensitivity: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmDenseReaderM8;
  Result.SearchMode             := smDualTarget;
  Result.Session                := ssSession1;
  Result.EstimatedTagPopulation := 32;
  Result.Description            := 'High Sensitivity (difficult tags / long range)';
end;

function TR700RFPresets.FastScan: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmMaxThroughput;
  Result.SearchMode             := smSingleTarget;
  Result.Session                := ssSession0;
  Result.EstimatedTagPopulation := 32;
  Result.Description            := 'Fast Scan (tags in motion / conveyor)';
end;

function TR700RFPresets.MultiReader: TR700RFPreset;
begin
  Result := TR700RFPreset.Create;
  Result.RFMode                 := rmDenseReaderM8;
  Result.SearchMode             := smDualTargetBtoASelect;
  Result.Session                := ssSession3;
  Result.EstimatedTagPopulation := 128;
  Result.Description            := 'Multi Reader (3+ readers, high tag density)';
end;

constructor TR700RFPresets.Create;
begin
  inherited Create;

  SetLength(FPresets, 8);
  FPresets[0] := Self.Standard;
  FPresets[1] := Self.SmallInventory;
  FPresets[2] := Self.LargeInventory;
  FPresets[3] := Self.DenseReader;
  FPresets[4] := Self.Monitoring;
  FPresets[5] := Self.HighSensitivity;
  FPresets[6] := Self.FastScan;
  FPresets[7] := self.MultiReader;
end;

{ NOTA: le implementazioni di TAntennasInfoHelper, TR700Tag e TR700TagList
  sono state rimosse da qui: la logica equivalente vive ora in
  uRFIDCommon.pas (TRFIDAntennasInfoHelper, TRFIDTag, TRFIDTagList),
  condivisa con il driver Zebra FXR90. }

{ ============================================================ }
{ TR700ReaderClient                                            }
{ ============================================================ }

constructor TR700ReaderClient.Create;
begin
  inherited Create;

  FreeOnTerminate := False;
  FHost          := '';
  FPort          := 443;
  FUsername      := '';
  FPassword      := '';
  FAntennas      := TR700AntennaList.Create;
  FControlSocket := nil;
  FStreamSocket  := nil;

  if not InitOpenSSL3 then
    LBLogger.Write(1, 'TR700ReaderClient.Create', lmt_Warning, 'OpenSSL not initialized');
end;

destructor TR700ReaderClient.Destroy;
begin
  inherited Destroy;
  Self.DisconnectStreamFromReader;
  Self.DisconnectFromReader;
  Self.ClearAntennas;
  FAntennas.Free;
end;

procedure TR700ReaderClient.SetConnection(const AHost: string; APort: Word);
begin
  FHost := AHost;
  FPort := APort;
end;

procedure TR700ReaderClient.SetCredentials(const AUsername, APassword: string);
begin
  FUsername := AUsername;
  FPassword := APassword;
end;

procedure TR700ReaderClient.ClearAntennas;
var
  i: Integer;
begin
  for i := FAntennas.Count - 1 downto 0 do
    Dispose(FAntennas.Items[i]);
  FAntennas.Clear;
end;

procedure TR700ReaderClient.AddAntenna(Port: Byte; TxPowerDbm: Byte; Enabled: Boolean;
                                       RFMode: TR700RFMode; SearchMode: TR700SearchMode;
                                       Session: TR700Session; EstimatedTagPopulation: Word);
var
  _cfg: PR700AntennaConfig;
begin
  Self.RemoveAntenna(Port);
  New(_cfg);
  _cfg^.Port                   := Port;
  _cfg^.TxPowerDbm             := TxPowerDbm;
  _cfg^.Enabled                := Enabled;
  _cfg^.RFMode                 := RFMode;
  _cfg^.SearchMode             := SearchMode;
  _cfg^.Session                := Session;
  _cfg^.EstimatedTagPopulation := EstimatedTagPopulation;
  FAntennas.Add(_cfg);
end;

procedure TR700ReaderClient.RemoveAntenna(Port: Byte);
var
  i: Integer;
begin
  for i := 0 to FAntennas.Count - 1 do
  begin
    if FAntennas[i]^.Port = Port then
    begin
      Dispose(FAntennas[i]);
      FAntennas.Delete(i);
      Break;
    end;
  end;
end;

procedure TR700ReaderClient.SetAntennaEnabled(Port: Byte; Enabled: Boolean);
var
  _cfg: PR700AntennaConfig;
begin
  _cfg := Self.GetAntennaConfig(Port);

  if _cfg <> nil then
    _cfg^.Enabled := Enabled;
end;

procedure TR700ReaderClient.SetAntennaRFConfig(Port: Byte;
                                              ARFMode:     TR700RFMode;
                                              ASearchMode: TR700SearchMode;
                                              ASession:    TR700Session;
                                              ATagPop:     Word);
var
  _cfg: PR700AntennaConfig;
begin
  _cfg := Self.GetAntennaConfig(Port);
  if _cfg <> nil then
  begin
    _cfg^.RFMode                 := ARFMode;
    _cfg^.SearchMode             := ASearchMode;
    _cfg^.Session                := ASession;
    _cfg^.EstimatedTagPopulation := ATagPop;
  end;
end;

procedure TR700ReaderClient.SetAntennaPower(Port: Byte; TxPowerDbm: Byte);
var
  _cfg: PR700AntennaConfig;
begin
  _cfg := Self.GetAntennaConfig(Port);

  if _cfg <> nil then
    _cfg^.TxPowerDbm := TxPowerDbm;
end;

function TR700ReaderClient.GetAntennaConfig(Port: Byte): PR700AntennaConfig;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FAntennas.Count - 1 do
  begin
    if FAntennas[i]^.Port = Port then
      Exit(FAntennas[i]);
  end;
end;

function TR700ReaderClient.AntennaCount: Integer;
begin
  Result := FAntennas.Count;
end;

function TR700ReaderClient.BuildBasicAuth: string;
begin
  Result := 'Basic ' + EncodeStringBase64(FUsername + ':' + FPassword);
end;

function TR700ReaderClient.ConnectToReader: Boolean;
begin
  Result := False;
  Self.DisconnectFromReader;

  FControlSocket := TTCPBlockSocket.Create;
  FControlSocket.ConnectionTimeout := 5000;
  FControlSocket.SetRecvTimeout(3000);
  FControlSocket.SetSendTimeout(3000);
  FControlSocket.SSL.SSLType := LT_TLSv1_2;
  FControlSocket.Connect(FHost, IntToStr(FPort));

  if FControlSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'R700.ConnectToReader', lmt_Warning, 'TCP connect failed: %s', [FControlSocket.LastErrorDesc]);
    FreeAndNil(FControlSocket);
    Exit;
  end;

  if not FControlSocket.SSL.Connect then
  begin
    LBLogger.Write(1, 'R700.ConnectToReader', lmt_Warning, 'SSL handshake failed: %s', [FControlSocket.SSL.LastErrorDesc]);
    FreeAndNil(FControlSocket);
    Exit;
  end;

  Result := True;

  if Assigned(FOnReaderConnected) then
    FOnReaderConnected(Self);

  LBLogger.Write(5, 'R700.ConnectToReader', lmt_Debug, 'Connected to %s:%d', [FHost, FPort]);
end;

function TR700ReaderClient.ConnectStreamToReader: Boolean;
begin
  Result := False;
  Self.DisconnectStreamFromReader;

  FStreamSocket := TTCPBlockSocket.Create;
  FStreamSocket.ConnectionTimeout := 5000;
  FStreamSocket.SetRecvTimeout(200);
  FStreamSocket.SetSendTimeout(3000);
  FStreamSocket.SSL.SSLType := LT_TLSv1_2;
  FStreamSocket.Connect(FHost, IntToStr(FPort));

  if FStreamSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'R700.ConnectStreamToReader', lmt_Warning, 'TCP stream connect failed: %s', [FStreamSocket.LastErrorDesc]);
    FreeAndNil(FStreamSocket);
    Exit;
  end;

  if not FStreamSocket.SSL.Connect then
  begin
    LBLogger.Write(1, 'R700.ConnectStreamToReader', lmt_Warning, 'SSL stream handshake failed: %s', [FStreamSocket.SSL.LastErrorDesc]);
    FreeAndNil(FStreamSocket);
    Exit;
  end;

  Result := True;
end;

procedure TR700ReaderClient.DisconnectFromReader;
begin
  if FControlSocket <> nil then
  begin
    FControlSocket.CloseSocket;
    FreeAndNil(FControlSocket);

    if Assigned(FOnReaderDisconnected) then
      FOnReaderDisconnected(Self);
  end;
end;

procedure TR700ReaderClient.DisconnectStreamFromReader;
begin
  if FStreamSocket <> nil then
  begin
    FStreamSocket.CloseSocket;
    FreeAndNil(FStreamSocket);
  end;
end;

function TR700ReaderClient.SendHTTPRequest(const AMethod, APath: string;
                                           const ABody: string;
                                           const AContentType: string): Boolean;
var
  _Header     : string;
  _Line       : string;
  _StatusCode : Integer;
  _ContentLen : Integer;
  _IsChunked  : Boolean;
  _ChunkSize  : Integer;
begin
  Result := False;
  if FControlSocket = nil then Exit;

  _Header :=
    AMethod + ' ' + APath + ' HTTP/1.1' + CRLF +
    'Host: '          + FHost + CRLF +
    'Authorization: ' + BuildBasicAuth + CRLF +
    'Connection: keep-alive' + CRLF;

  if ABody <> '' then
    _Header := _Header +
      'Content-Type: '   + AContentType + CRLF +
      'Content-Length: ' + IntToStr(Length(ABody)) + CRLF
  else
    _Header := _Header + 'Content-Length: 0' + CRLF;

  _Header := _Header + CRLF;
  FControlSocket.SendString(_Header + ABody);

  if FControlSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.SendHTTPRequest', lmt_Warning, '%s %s — send error: %s', [AMethod, APath, FControlSocket.LastErrorDesc]);
    Exit;
  end;

  _Line := FControlSocket.RecvString(3000);
  if FControlSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.SendHTTPRequest', lmt_Warning, '%s %s — recv error: %s', [AMethod, APath, FControlSocket.LastErrorDesc]);
    Exit;
  end;

  _StatusCode := 0;
  if Length(_Line) >= 12 then
    _StatusCode := StrToIntDef(Copy(_Line, 10, 3), 0);

  Result := (_StatusCode >= 200) and (_StatusCode < 300);

  if not Result then
    LBLogger.Write(1, 'TR700ReaderClient.SendHTTPRequest', lmt_Warning, '%s %s - HTTP %d (%s)', [AMethod, APath, _StatusCode, _Line])
  else
    LBLogger.Write(5, 'TR700ReaderClient.SendHTTPRequest', lmt_Debug, '%s %s - HTTP %d', [AMethod, APath, _StatusCode]);

  _ContentLen := 0;
  _IsChunked  := False;
  while True do
  begin
    _Line := FControlSocket.RecvString(1000);

    if FControlSocket.LastError <> 0 then Exit;

    if _Line = '' then Break;

    if Pos('Content-Length:', _Line) = 1 then
      _ContentLen := StrToIntDef(Trim(Copy(_Line, 16, MaxInt)), 0);

    if (Pos('Transfer-Encoding:', _Line) > 0) and (Pos('chunked', _Line) > 0) then
      _IsChunked := True;
  end;

  if _IsChunked then
  begin
    while True do
    begin
      _Line := FControlSocket.RecvString(1000);
      if FControlSocket.LastError <> 0 then Break;
      _ChunkSize := StrToIntDef('$' + Trim(_Line), -1);
      if _ChunkSize <= 0 then Break;
      FControlSocket.RecvBufferStr(_ChunkSize, 2000);
      FControlSocket.RecvString(500);
    end;
  end
  else if _ContentLen > 0 then
    FControlSocket.RecvBufferStr(_ContentLen, 2000);
end;

{ Converte TR700SearchMode nella stringa richiesta dall'API Impinj }
function SearchModeToStr(AMode: TR700SearchMode): string;
begin
  case AMode of
    smSingleTarget:         Result := 'single-target';
    smDualTargetBtoASelect: Result := 'dual-target-b-to-a-select';
  else
    Result := 'dual-target';
  end;
end;

{ Costruisce il body JSON per l'inventory di una singola antenna.
  Usato dal TR700AntennaMultiplexer per precalcolare un body per antenna. }
function TR700ReaderClient.BuildInventoryBody(ACfg: PR700AntennaConfig): string;
var
  _JsonObj : TJSONObject;
  _AntArr  : TJSONArray;
  _AntCfg  : TJSONObject;
begin
  Result   := '';
  _JsonObj := TJSONObject.Create;
  try
    _AntArr := TJSONArray.Create;
    _JsonObj.Add('antennaConfigs', _AntArr);

    _AntCfg := TJSONObject.Create;
    _AntCfg.Add('antennaPort',            ACfg^.Port);
    _AntCfg.Add('transmitPowerCdbm',      Integer(ACfg^.TxPowerDbm) * 100);
    _AntCfg.Add('inventorySearchMode',    SearchModeToStr(ACfg^.SearchMode));
    _AntCfg.Add('inventorySession',       Integer(ACfg^.Session));
    _AntCfg.Add('rfMode',                 Integer(ACfg^.RFMode));
    _AntCfg.Add('estimatedTagPopulation', ACfg^.EstimatedTagPopulation);
    _AntArr.Add(_AntCfg);

    Result := _JsonObj.AsJSON;
  except
    on E: Exception do
      LBLogger.Write(1, 'R700.BuildInventoryBody', lmt_Error, 'JSON build error: %s', [E.Message]);
  end;
  _JsonObj.Free;
end;

function TR700ReaderClient.StartInventory: Boolean;
var
  _JsonObj : TJSONObject;
  _AntArr  : TJSONArray;
  _AntCfg  : TJSONObject;
  _Body    : string;
  i        : Integer;
  _cfg     : PR700AntennaConfig;
begin
  Result := False;
  _Body  := '';

  if FAntennas.Count = 0 then
  begin
    LBLogger.Write(1, 'R700.StartInventory', lmt_Error, 'No antennas configured');
    Exit;
  end;

  _JsonObj := TJSONObject.Create;
  try
    _AntArr := TJSONArray.Create;
    _JsonObj.Add('antennaConfigs', _AntArr);
    for i := 0 to FAntennas.Count - 1 do
    begin
      _cfg := FAntennas[i];
      if not _cfg^.Enabled then Continue;
      _AntCfg := TJSONObject.Create;
      _AntCfg.Add('antennaPort',            _cfg^.Port);
      _AntCfg.Add('transmitPowerCdbm',      Integer(_cfg^.TxPowerDbm) * 100);
      _AntCfg.Add('inventorySearchMode',    SearchModeToStr(_cfg^.SearchMode));
      _AntCfg.Add('inventorySession',       Integer(_cfg^.Session));
      _AntCfg.Add('rfMode',                 Integer(_cfg^.RFMode));
      _AntCfg.Add('estimatedTagPopulation', _cfg^.EstimatedTagPopulation);
      _AntArr.Add(_AntCfg);
    end;
    _Body := _JsonObj.AsJSON;
  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'R700.StartInventory', lmt_Error, 'JSON build error: %s', [E.Message]);
      _JsonObj.Free;
      Exit;
    end;
  end;
  _JsonObj.Free;

  Result := Self.SendHTTPRequest('POST', '/api/v1/profiles/inventory/start', _Body, 'application/json');
  if Result then
    LBLogger.Write(5, 'R700.StartInventory', lmt_Debug, 'Inventory started (transient)')
  else
    LBLogger.Write(1, 'R700.StartInventory', lmt_Warning, 'Start inventory failed with body: <%s>', [_Body]);
end;

function TR700ReaderClient.StopInventory: Boolean;
begin
  Result := Self.SendHTTPRequest('POST', '/api/v1/profiles/stop');
  if Result then
    LBLogger.Write(5, 'TR700ReaderClient.StopInventory', lmt_Debug, 'Inventory stopped');
end;

(*
function TR700ReaderClient.StreamTags: Boolean;
var
  _Line       : string;
  _StatusCode : Integer;
  _ChunkSize  : Integer;
  _IsChunked  : Boolean;
  _JsonBuf    : string;
begin
  Result     := False;
  _IsChunked := False;

  FStreamSocket.SendString(
    'GET /api/v1/data/stream HTTP/1.1' + CRLF +
    'Host: '          + FHost           + CRLF +
    'Authorization: ' + BuildBasicAuth  + CRLF +
    'Accept: application/json'          + CRLF +
    'Connection: keep-alive'            + CRLF + CRLF);

  if FStreamSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream GET failed: %s', [FStreamSocket.LastErrorDesc]);
    FState := stError;
    Exit;
  end;

  _Line := FStreamSocket.RecvString(3000);
  if FStreamSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream status read error: %s', [FStreamSocket.LastErrorDesc]);
    FState := stError;
    Exit;
  end;

  _StatusCode := 0;
  if Length(_Line) >= 12 then
    _StatusCode := StrToIntDef(Copy(_Line, 10, 3), 0);

  if (_StatusCode < 200) or (_StatusCode >= 300) then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream HTTP error: %s', [_Line]);
    FState := stError;
    Exit;
  end;

  while not Self.Terminated do
  begin
    _Line := FStreamSocket.RecvString(1000);
    if FStreamSocket.LastError <> 0 then Break;
    if _Line = '' then Break;
    if (Pos('Transfer-Encoding:', _Line) > 0) and (Pos('chunked', _Line) > 0) then
      _IsChunked := True;
  end;

  if Self.Terminated then Exit;

  Result := True;
  LBLogger.Write(5, 'TR700ReaderClient.StreamTags', lmt_Debug, 'Stream open (chunked=%s), reading tags ...', [BoolToStr(_IsChunked, True)]);

  while not Self.Terminated do
  begin
    if _IsChunked then
    begin
      _Line := FStreamSocket.RecvString(200);

      if FStreamSocket.LastError <> 0 then
      begin
        if FStreamSocket.LastError = gc_SocketTimeout then
          Continue
        else begin
          LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream read error: %s', [FStreamSocket.LastErrorDesc]);
          FState := stError;
          Break;
        end;
      end;

      if _Line = '' then Continue;

      _ChunkSize := StrToIntDef('$' + Trim(_Line), -1);

      if _ChunkSize < 0 then
      begin
        if Assigned(FOnNewTagInfoReceived) then
          FOnNewTagInfoReceived(Self, Trim(_Line));
        Continue;
      end;

      if _ChunkSize = 0 then Break;

      _JsonBuf := FStreamSocket.RecvBufferStr(_ChunkSize, 2000);
      FStreamSocket.RecvString(500);

      if FStreamSocket.LastError <> 0 then
      begin
        LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream chunk read error: %s', [FStreamSocket.LastErrorDesc]);
        FState := stError;
        Break;
      end;

      _JsonBuf := Trim(_JsonBuf);
      if (_JsonBuf <> '') and Assigned(FOnNewTagInfoReceived) then
        FOnNewTagInfoReceived(Self, _JsonBuf);
    end
    else begin
      _Line := FStreamSocket.RecvString(200);
      if FStreamSocket.LastError = 0 then
      begin
        if (_Line <> '') and Assigned(FOnNewTagInfoReceived) then
          FOnNewTagInfoReceived(Self, _Line);
      end
      else if FStreamSocket.LastError <> gc_SocketTimeout then
      begin
        LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream read error: %s', [FStreamSocket.LastErrorDesc]);
        FState := stError;
        Break;
      end;
    end;
  end;
end;
*)

function TR700ReaderClient.StreamTags: Boolean;
var
  _Line       : string;
  _StatusCode : Integer;
  _ChunkSize  : Integer;
  _IsChunked  : Boolean;
  _JsonBuf    : string;
  _LineBuffer  : string;          // buffer for incomplete lines
  p           : Integer;
begin
  Result     := False;
  _IsChunked := False;
  _LineBuffer := '';

  FStreamSocket.SendString(
    'GET /api/v1/data/stream HTTP/1.1' + CRLF +
    'Host: '          + FHost           + CRLF +
    'Authorization: ' + BuildBasicAuth  + CRLF +
    'Accept: application/json'          + CRLF +
    'Connection: keep-alive'            + CRLF + CRLF);

  if FStreamSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream GET failed: %s', [FStreamSocket.LastErrorDesc]);
    FState := stError;
    Exit;
  end;

  _Line := FStreamSocket.RecvString(3000);
  if FStreamSocket.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream status read error: %s', [FStreamSocket.LastErrorDesc]);
    FState := stError;
    Exit;
  end;

  _StatusCode := 0;
  if Length(_Line) >= 12 then
    _StatusCode := StrToIntDef(Copy(_Line, 10, 3), 0);

  if (_StatusCode < 200) or (_StatusCode >= 300) then
  begin
    LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream HTTP error: %s', [_Line]);
    FState := stError;
    Exit;
  end;

  while not Self.Terminated do
  begin
    _Line := FStreamSocket.RecvString(1000);
    if FStreamSocket.LastError <> 0 then Break;
    if _Line = '' then Break;
    if (Pos('Transfer-Encoding:', _Line) > 0) and (Pos('chunked', _Line) > 0) then
      _IsChunked := True;
  end;

  if Self.Terminated then Exit;

  Result := True;
  LBLogger.Write(5, 'TR700ReaderClient.StreamTags', lmt_Debug, 'Stream open (chunked=%s), reading tags ...', [BoolToStr(_IsChunked, True)]);

  while not Self.Terminated do
  begin
    if _IsChunked then
    begin
      // Read chunk size line (hex)
      _Line := FStreamSocket.RecvString(200);
      if FStreamSocket.LastError <> 0 then
      begin
        if FStreamSocket.LastError = gc_SocketTimeout then
          Continue
        else
        begin
          LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream read error: %s', [FStreamSocket.LastErrorDesc]);
          FState := stError;
          Break;
        end;
      end;

      if _Line = '' then Continue;

      _ChunkSize := StrToIntDef('$' + Trim(_Line), -1);
      if _ChunkSize < 0 then
      begin
        // This should not happen; but if it does, treat as invalid chunk
        LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Invalid chunk size: %s', [_Line]);
        FState := stError;
        Break;
      end;

      if _ChunkSize = 0 then
      begin
        // End of stream – process any remaining data in buffer
        if _LineBuffer <> '' then
          if Assigned(FOnNewTagInfoReceived) then
            FOnNewTagInfoReceived(Self, _LineBuffer);
        Break;
      end;

      // Read the chunk data
      _JsonBuf := FStreamSocket.RecvBufferStr(_ChunkSize, 2000);
      if FStreamSocket.LastError <> 0 then
      begin
        LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream chunk read error: %s', [FStreamSocket.LastErrorDesc]);
        FState := stError;
        Break;
      end;

      // Read trailing CRLF
      FStreamSocket.RecvString(500);

      // Append to line buffer
      _LineBuffer := _LineBuffer + _JsonBuf;

      // Extract and process complete lines (terminated by CRLF or LF)
      while True do
      begin
        p := Pos(#13#10, _LineBuffer);
        if p = 0 then p := Pos(#10, _LineBuffer);
        if p = 0 then Break;

        // Extract the line without delimiter
        _Line := Copy(_LineBuffer, 1, p - 1);
        // Remove the line and delimiter from the buffer
        if (p <= Length(_LineBuffer)) and (_LineBuffer[p] = #13) then
          Delete(_LineBuffer, 1, p + 1)   // skip CRLF
        else
          Delete(_LineBuffer, 1, p);      // skip LF

        if (_Line <> '') and Assigned(FOnNewTagInfoReceived) then
          FOnNewTagInfoReceived(Self, _Line);
      end;

      // At this point, any remaining data in LineBuffer is a partial line
    end
    else
    begin
      // Non-chunked response (should not happen, but keep fallback)
      _Line := FStreamSocket.RecvString(200);
      if FStreamSocket.LastError = 0 then
      begin
        if (_Line <> '') and Assigned(FOnNewTagInfoReceived) then
          FOnNewTagInfoReceived(Self, _Line);
      end
      else if FStreamSocket.LastError <> gc_SocketTimeout then
      begin
        LBLogger.Write(1, 'TR700ReaderClient.StreamTags', lmt_Warning, 'Stream read error: %s', [FStreamSocket.LastErrorDesc]);
        FState := stError;
        Break;
      end;
    end;
  end;

  // If we exit the loop due to termination, ignore any remaining data
end;

procedure TR700ReaderClient.Execute;
begin
  Self.setThreadName('R700_' + Copy(FHost, 1, 15));
  LBLogger.Write(5, 'TR700ReaderClient.Execute', lmt_Debug, 'Thread started for %s', [FHost]);
  FState := stConnecting;

  while not Self.Terminated do
  begin
    case FState of
      stConnecting:
        if Self.ConnectToReader then
          FState := stConfiguring
        else begin
          FState := stError;
          Self.PauseFor(5000);
        end;

      stConfiguring:
        begin
          Self.StopInventory;
          if Self.ConnectStreamToReader and Self.StartInventory then
            FState := stStreaming
          else
            FState := stError;
        end;

      stStreaming: Self.StreamTags;

      stError:
        begin
          Self.DisconnectStreamFromReader;
          Self.DisconnectFromReader;
          if not Self.Terminated then
            Self.PauseFor(3000);
          FState := stConnecting;
        end;
    end;
  end;

  Self.StopInventory;
  Self.DisconnectStreamFromReader;
  Self.DisconnectFromReader;
  LBLogger.Write(5, 'TR700ReaderClient.Execute', lmt_Debug, 'Thread terminated for <%s>', [FHost]);
end;

{ ============================================================ }
{ TR700Consumer                                                }
{ ============================================================ }

constructor TR700Consumer.Create(ATagList: TRFIDTagList; aReader: TR700ReaderClient);
begin
  inherited Create;
  FreeOnTerminate := False;

  FTagList      := ATagList;
  FTagCS        := TTimedOutCriticalSection.Create;
  FTagQueue     := TStringList.Create;
  FNewDataEvent := RTLEventCreate();

  if aReader <> nil then
    aReader.OnNewTagInfoReceived := @Self.NewTagReceived;
end;

destructor TR700Consumer.Destroy;
begin
  inherited Destroy; // WaitFor prima di liberare le risorse
  FTagQueue.Free;
  FTagCS.Free;
  RTLEventDestroy(FNewDataEvent);
end;

procedure TR700Consumer.Terminate;
begin
  inherited Terminate;
  RTLEventSetEvent(FNewDataEvent);
end;

procedure TR700Consumer.ProcessJSON(const AJSON: string);
var
  _obj  : TJSONData;
  _Data : TStringList;
  i     : Integer;
begin
  _obj  := nil;
  _Data := nil;
  try
    _Data      := TStringList.Create;
    _Data.Text := AJSON;

    for i := 0 to _Data.Count - 1 do
    begin
      if _Data.Strings[i] = '' then Continue;

      try
        _obj := GetJSON(_Data.Strings[i]);
      except
        on E: Exception do
        begin
          _obj := nil;
          LBLogger.Write(1, 'TR700Consumer.ProcessJSON', lmt_Error, 'Error parsing <%s>: %s', [_Data.Strings[i], E.Message]);
          Continue;
        end;
      end;

      if _obj <> nil then
      begin
        Self.ProcessJSONObject(_obj);
      end;

      FreeAndNil(_obj);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TR700Consumer.ProcessJSON', lmt_Error, E.Message);
  end;

  FreeAndNil(_obj);
  FreeAndNil(_Data);
end;

function TR700Consumer.ProcessJSONObject(AData: TJSONData): Boolean;
{ Traduce un singolo oggetto JSON "tagInventoryEvent" del reader Impinj R700
  in un aggiornamento della TRFIDTagList. Logica ex-TR700JSONParser.ProcessJSON,
  spostata qui per eliminare il riferimento incrociato con il Manager (stesso
  principio già adottato da TFXR90Consumer.ProcessJSONObject). }
var
  _InventoryEvent : TJSONObject;
  _epc            : string;
  _rssi           : Integer;
  _ts             : TDateTime;
  _ant            : Byte;
  _isNew          : Boolean;
begin
  Result := False;

  if (AData = nil) or (FTagList = nil) then
    Exit;

  if not (AData is TJSONObject) then
  begin
    LBLogger.Write(1, 'TR700Consumer.ProcessJSONObject', lmt_Warning, 'Wrong JSON object');
    Exit;
  end;

  _InventoryEvent := TJSONObject(AData).Get('tagInventoryEvent', TJSONObject(nil));

  if _InventoryEvent = nil then
  begin
    LBLogger.Write(1, 'TR700Consumer.ProcessJSONObject', lmt_Warning, 'tagInventoryEvent not found in received JSON');
    Exit;
  end;

  _epc := _InventoryEvent.Get('epc', '');
  if _epc = '' then
    Exit;

  _ant  := _InventoryEvent.Get('antennaPort', 0);
  _rssi := _InventoryEvent.Get('peakRssiCdbm', 0);
  if not TryISO8601ToDate(TJSONObject(AData).Get('timestamp', ''), _ts, True) then
    _ts := NowUTC;

  { GetOrCreate aggiorna i dati del tag, notifica gli eventi del tag
    e della lista, tutto fuori dal proprio lock interno. }
  _epc := Base64ToHex(_epc);
  FTagList.GetOrCreate(_epc, _ant, _rssi, _ts, _isNew);

  Result := True;
end;

procedure TR700Consumer.NewTagReceived(Sender: TObject; const aJSON: String);
begin
  if FTagCS.Acquire('R700Consumer.NewTagReceived') then
  begin

    try
      FTagQueue.Add(aJSON);
    finally
      FTagCS.Release;
    end;
    RTLEventSetEvent(FNewDataEvent);

  end;
end;

procedure TR700Consumer.Execute;
var
  _json  : string;
  _batch : TStringList;
begin
  Self.setThreadName('R700Consumer');

  while not Self.Terminated do
  begin
    _batch := nil;

    if FTagCS.Acquire('R700Consumer.Execute') then
    try
      if FTagQueue.Count > 0 then
      begin
        _batch    := FTagQueue;
        FTagQueue := TStringList.Create;
      end;
    finally
      FTagCS.Release;
    end;

    if _batch <> nil then
    begin
      try
        for _json in _batch do
        begin
          if not Self.Terminated then
            Self.ProcessJSON(_json);
        end;
      finally
        FreeAndNil(_batch);
      end;
    end
    else
      RTLEventWaitFor(FNewDataEvent, 3000);
  end;
end;

{ ============================================================ }
{ ============================================================ }
{ TR700Manager                                                 }
{ ============================================================ }

constructor TR700Manager.Create(const AHost: string; APort: Word;
                                const AUsername: string;
                                const APassword: string);
begin
  inherited Create; // TRFIDManager: crea FTagList, FEventsManager, e registra
                    // già il forward automatico cEvent_TagAdded -> cEvent_NewTag

  FReaderType := rrtImpinjR700;

  FHost     := AHost;
  FPort     := APort;
  FUsername := AUsername;
  FPassword := APassword;

  SetLength(FAntennaPort,   0);
  SetLength(FAntennaDBm,    0);
  SetLength(FAntennaPreset, 0);

  FReader   := nil;
  FConsumer := nil;
end;

destructor TR700Manager.Destroy;
begin
  Self.StopInventory();
  ClearAntennaArrays;

  inherited Destroy; // TRFIDManager: libera FTagList e FEventsManager
end;

procedure TR700Manager.ReaderConnected(Sender: TObject);
begin
  FEventsManager.RaiseEvent(cEvent_ReaderConnected);
end;

function TR700Manager.StartInventory: Boolean;
var
  _i : Integer;
  _p : TR700RFPreset;
begin
  Result := False;
  Self.StopInventory();

  if Length(FAntennaPort) = 0 then
  begin
    LBLogger.Write(1, 'TR700Manager.StartInventory', lmt_Warning, 'No antenna configured');
    Exit;
  end;

  { La TagList (ereditata da TRFIDManager) sopravvive al Manager, non al
    singolo Start: la svuotiamo esplicitamente per ripartire da zero ad
    ogni nuova sessione, come faceva la vecchia TR700TagList dedicata. }
  FTagList.Clear;

  FReader := TR700ReaderClient.Create;
  FReader.SetConnection(FHost, FPort);
  FReader.SetCredentials(FUsername, FPassword);
  FReader.ClearAntennas;
  FReader.OnReaderConnected := @Self.ReaderConnected;

  for _i := 0 to High(FAntennaPort) do
  begin
    _p := FAntennaPreset[_i];
    FReader.AddAntenna(Byte(FAntennaPort[_i]), Byte(FAntennaDBm[_i]), True,
                       _p.RFMode, _p.SearchMode, _p.Session, _p.EstimatedTagPopulation);
  end;

  FConsumer := TR700Consumer.Create(FTagList, FReader);

  LBLogger.Write(5, 'TR700Manager.StartInventory', lmt_Debug, 'Starting R700 on %s:%d', [FHost, FPort]);

  FConsumer.Start;
  FReader.Start;

  Result := True;
end;

function TR700Manager.StopInventory: Boolean;
begin
  Result := True;

  if FReader <> nil then
    FreeAndNil(FReader);

  if FConsumer <> nil then
    FreeAndNil(FConsumer);
end;

procedure TR700Manager.AddAntenna(aPort: Integer; aDBm: Integer; aPreset: TRFIDAntennaPreset);
var
  _p   : TR700RFPreset;
  _idx : Integer;
  i    : Integer;
begin
  if not (aPreset is TR700RFPreset) then
  begin
    LBLogger.Write(1, 'TR700Manager.AddAntenna', lmt_Error, 'Preset must be a TR700RFPreset (port=%d)', [aPort]);
    Exit;
  end;

  { Rimuovi eventuale configurazione precedente per la stessa porta }
  for i := High(FAntennaPort) downto 0 do
  begin
    if FAntennaPort[i] = aPort then
    begin
      FreeAndNil(FAntennaPreset[i]);
      Delete(FAntennaPort,   i, 1);
      Delete(FAntennaDBm,    i, 1);
      Delete(FAntennaPreset, i, 1);
      Break;
    end;
  end;

  _p   := TR700RFPreset(aPreset.Clone);
  _idx := Length(FAntennaPort);

  SetLength(FAntennaPort,   _idx + 1);
  SetLength(FAntennaDBm,    _idx + 1);
  SetLength(FAntennaPreset, _idx + 1);

  FAntennaPort[_idx]   := aPort;
  FAntennaDBm[_idx]    := aDBm;
  FAntennaPreset[_idx] := _p;

  LBLogger.Write(5, 'TR700Manager.AddAntenna', lmt_Debug, 'Antenna %d added: %d dBm [%s]', [aPort, aDBm, _p.Description]);
end;

procedure TR700Manager.ClearAntennaArrays;
var
  i: Integer;
begin
  for i := 0 to High(FAntennaPreset) do
    if FAntennaPreset[i] <> nil then FreeAndNil(FAntennaPreset[i]);
  SetLength(FAntennaPort,   0);
  SetLength(FAntennaDBm,    0);
  SetLength(FAntennaPreset, 0);
end;

procedure TR700Manager.ClearAntennas;
begin
  ClearAntennaArrays;
end;

end.
