unit uTimerWheel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBBaseThread, uTimedoutCriticalSection, uPLCBaseStructures, fgl,
  uEventsManager;

type

  { Forward declaration }
  TTimerWheel = class;

  { TXPLCTimerItem }

  TXPLCTimerItem = class(TBasePLCItem)
  strict private
    FDurationMs  : Cardinal;
    FTimedOut    : Boolean;
    FGeneration  : Integer;          // incrementato ad ogni Start/Stop; usato per invalidare timeout pendenti
    FExpireAt    : QWord;            // istante di scadenza (GetTickCount64 + durata)
    FScheduledGeneration: Integer;   // generazione al momento della schedulazione
    FWheel       : TTimerWheel;
    FXPLCName    : String;
    FEventsManager: TEventsManager;

    FDestroying : Boolean;

    function GetRunning: Boolean;


  private
    property ExpireAt: QWord read FExpireAt write FExpireAt;

    { Chiamato dalla TTimerWheel quando scade il tempo.
      Verifica la generazione e, se coincide, imposta il timeout. }
    procedure WheelTimeout;

  public
    constructor Create(AWheel: TTimerWheel; const AXPLCName: String;
                       AId: Integer; const ACode: string;
                       ADefaultDurationMs: Cardinal); reintroduce;
    destructor Destroy; override;

    procedure StartTimer(ADurationMs: Cardinal = 0);
    procedure StopTimer;
    function IsTimedOut: Boolean;

    function ValueAsVariant: Variant; override;
    function ValueAsString: string; override;

    property Running: Boolean read GetRunning;
    property DurationMs: Cardinal read FDurationMs;
    property EventsManager: TEventsManager read FEventsManager;

    const
      cEvent_StatusChanged = String('01');
  end;

  { Lista che contiene i timer attivi. Non possiede gli oggetti (AOwnsObjects=False) }
  TTimerWheelEntryList = specialize TFPGObjectList<TXPLCTimerItem>;

  { TTimerWheel }

  TTimerWheel = class(TLBBaseThread)
  strict private
    FEntries : TTimerWheelEntryList;
    FCS      : TTimedOutCriticalSection;
    FWakeUp  : PRTLEvent;

  protected
    procedure Execute; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    { Schedula il timer item. aDurationMs è la durata in millisecondi.
      L'item verrà aggiunto alla lista e la sua scadenza calcolata. }
    procedure Schedule(aTimerItem: TXPLCTimerItem);

    { Rimuove immediatamente l'item dalla lista (se presente). }
    procedure Cancel(aTimerItem: TXPLCTimerItem);

    procedure Terminate(); override;

    { Ritorna True se l'item è attualmente in attesa di scadenza. }
    function IsScheduled(aTimerItem: TXPLCTimerItem): Boolean;
  end;


implementation

uses
  ULBLogger;

{ TXPLCTimerItem }

constructor TXPLCTimerItem.Create(AWheel: TTimerWheel; const AXPLCName: String;
                                   AId: Integer; const ACode: string;
                                   ADefaultDurationMs: Cardinal);
begin
  inherited Create;

  FDestroying := False;

  FWheel      := AWheel;
  FWheel.AddReference(@FWheel);

  FXPLCName   := AXPLCName;
  Id          := AId;
  Code        := ACode;
  FDurationMs := ADefaultDurationMs;
  FTimedOut   := False;
  FGeneration := 0;
  FExpireAt   := 0;
  FScheduledGeneration := 0;

  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_StatusChanged);
end;

destructor TXPLCTimerItem.Destroy;
begin
  FDestroying := True;

  Self.StopTimer;               // rimuove dalla wheel in modo sicuro

  if FWheel <> nil then
  begin
    FWheel.RemoveReference(@FWheel);
    FWheel := nil;
  end;

  FEventsManager.Free;
  inherited Destroy;
end;

procedure TXPLCTimerItem.StartTimer(ADurationMs: Cardinal);
begin
  if ADurationMs > 0 then
    FDurationMs := ADurationMs;

  Inc(FGeneration);                     // invalida qualsiasi timeout pendente
  FScheduledGeneration := FGeneration;  // memorizza la generazione corrente

  if FTimedOut then
  begin
//    LBLogger.Write(6, 'TXPLCTimerItem.StartTimer', lmt_Debug, '<%s>  -  <%s>  -  Reset timeout value', [FXPLCName, Code]);
    FTimedOut := False;
    FEventsManager.RaiseEvent(cEvent_StatusChanged);
  end;

  // Inserisce (o ri-posiziona) nella wheel con la nuova scadenza
  FWheel.Schedule(Self);
end;

procedure TXPLCTimerItem.StopTimer;
begin
  Inc(FGeneration);                     // invalida timeout pendenti

  if FWheel <> nil then
    FWheel.Cancel(Self);                  // rimuove dalla lista della wheel
end;

function TXPLCTimerItem.IsTimedOut: Boolean;
begin
  Result := FTimedOut;
end;

function TXPLCTimerItem.GetRunning: Boolean;
begin
  if FWheel <> nil then
    Result := FWheel.IsScheduled(Self)
  else
    Result := False;
end;

procedure TXPLCTimerItem.WheelTimeout;
begin
  if FDestroying then Exit;

  // Verifica che la generazione sia ancora quella giusta
  if FScheduledGeneration <> FGeneration then
    Exit;

  // LBLogger.Write(5, 'TXPLCTimerItem.WheelTimeout', lmt_Debug, '<%s>  -  <%s> timed out!', [FXPLCName, Code]);
  FTimedOut := True;
  FEventsManager.RaiseEvent(cEvent_StatusChanged);
end;

function TXPLCTimerItem.ValueAsVariant: Variant;
begin
  if FTimedOut then
    Result := 1
  else
    Result := 0;
end;

function TXPLCTimerItem.ValueAsString: string;
begin
  Result := BoolToStr(FTimedOut, True);
end;


{ TTimerWheel }

constructor TTimerWheel.Create;
begin
  inherited Create;

  { La lista non deve possedere gli oggetti: sono di proprietà della VM.
    La wheel tiene solo riferimenti. }
  FEntries := TTimerWheelEntryList.Create(False);
  FCS      := TTimedOutCriticalSection.Create;
  FWakeUp  := RTLEventCreate;

  Self.setThreadName('TimerWheel');
end;

destructor TTimerWheel.Destroy;
begin
  inherited Destroy;    // Terminate + WaitFor: Execute esce prima di tutto

  FreeAndNil(FEntries);
  FreeAndNil(FCS);
  RTLEventDestroy(FWakeUp);
end;

procedure TTimerWheel.Execute;
var
  _now    : QWord;
  _next   : QWord;
  _wait   : Cardinal;
  _fired  : array of TXPLCTimerItem;
  _timer  : TXPLCTimerItem;
  i       : Integer;
begin
  while not Self.Terminated do
  begin
    SetLength(_fired, 0);
    _now  := GetTickCount64;
    _next := High(QWord);

    if FCS.Acquire('TTimerWheel.Execute') then
    try
      i := 0;
      while i < FEntries.Count do
      begin
        _timer := FEntries[i];
        if _timer.ExpireAt <= _now then
        begin
          // Raccogli i timer scaduti con swap O(1) per non shiftare l'array
          SetLength(_fired, Length(_fired) + 1);
          _fired[High(_fired)] := _timer;
          if i < FEntries.Count - 1 then
            FEntries[i] := FEntries[FEntries.Count - 1];
          FEntries.Delete(FEntries.Count - 1);
          // Non incrementa i: la posizione corrente ora contiene l'elemento spostato
        end
        else begin
          if _timer.ExpireAt < _next then
            _next := _timer.ExpireAt;
          Inc(i);
        end;
      end;
    finally
      FCS.Release;
    end;

    if Self.Terminated then
      Exit;   // ---------------------->>>>>>

    // Notifica FUORI dalla CS per evitare deadlock
    for i := 0 to High(_fired) do
    begin
      try
        _fired[i].WheelTimeout();
      except
        on E: Exception do
          LBLogger.Write(1, 'TTimerWheel.Execute', lmt_Error, 'Error firing timer <%s>: %s', [_fired[i].Code, E.Message]);
      end;
    end;

    // Calcola il tempo di attesa fino al prossimo timer
    _now := GetTickCount64;
    if _next = High(QWord) then
      _wait := 60000              // nessun timer attivo: attesa lunga
    else if _next > _now then
      _wait := Cardinal(_next - _now)
    else
      _wait := 0;

    if (_wait > 0) and (not Self.Terminated) then
      RTLEventWaitFor(FWakeUp, _wait);
  end;
end;

procedure TTimerWheel.Schedule(aTimerItem: TXPLCTimerItem);
var
  _Idx : Integer;

begin
  if FCS.Acquire('TTimerWheel.Schedule') then
  begin
    try
      _Idx := FEntries.IndexOf(aTimerItem);
      if _Idx = -1 then
        FEntries.Add(aTimerItem);

      // Calcola la nuova scadenza
      aTimerItem.ExpireAt := GetTickCount64 + aTimerItem.DurationMs;
    finally
      FCS.Release;
    end;

    // Sveglia il thread per ricalcolare il tempo di attesa
    RTLEventSetEvent(FWakeUp);
  end;
end;

procedure TTimerWheel.Cancel(aTimerItem: TXPLCTimerItem);
begin
  if FCS.Acquire('TTimerWheel.Cancel') then
  try
    FEntries.Remove(aTimerItem);
  finally
    FCS.Release;
  end;
  // Non serve svegliare il thread: il wait corrente è ancora valido
end;

procedure TTimerWheel.Terminate();
begin
  inherited Terminate();
  RTLEventSetEvent(FWakeUp);
end;

function TTimerWheel.IsScheduled(aTimerItem: TXPLCTimerItem): Boolean;
begin
  Result := False;
  if FCS.Acquire('TTimerWheel.IsScheduled') then
  try
    Result := FEntries.IndexOf(aTimerItem) >= 0;
  finally
    FCS.Release;
  end;
end;

end.
