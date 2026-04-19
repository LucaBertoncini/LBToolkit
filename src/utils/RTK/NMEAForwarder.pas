unit NMEAForwarder;

{
  TNMEAForwarder — scrive stringhe NMEA su una porta seriale virtuale.

  ── Scopo ────────────────────────────────────────────────────────────────
  Riceve le stringhe NMEA prodotte dal rover (via OnNMEARaw) e le scrive
  su una seconda porta seriale, tipicamente una porta virtuale creata con
  Free Virtual Serial Port (o equivalente su Linux: socat, tty0tty).

  ── Schema tipico con Free Virtual Serial Port (Windows) ─────────────────

    GPS fisico  ──USB──►  COM3 (rover)
                               │
                           [questo software]
                               │
                          COM8 (TNMEAForwarder) ──FVSP──► COM9 (app terze parti)

    Free Virtual Serial Port crea una coppia COM8↔COM9 collegata: tutto
    quello che il software scrive su COM8 appare come ricevuto su COM9.

  ── Thread-safety ─────────────────────────────────────────────────────────
  Write() è chiamato dal thread seriale del rover (non dal thread UI).
  TBlockSerial non è thread-safe: l'accesso alla porta è protetto da una
  TTimedOutCriticalSection interna.

  ── Buffering ─────────────────────────────────────────────────────────────
  Le stringhe vengono accodate in una TThreadList e scritte da un thread
  dedicato, così il thread del rover non si blocca se la porta è lenta.
  La coda ha dimensione massima configurabile (default 64 righe): se
  piena, le stringhe più vecchie vengono scartate (semantica drop-oldest).

  ── Dipendenze ────────────────────────────────────────────────────────────
  SynaSer (Synapse), uLBBaseThread, ULBLogger.
}

interface

uses
  SysUtils, Classes, SynaSer, uLBBaseThread, uLBCircularBuffer, uTimedoutCriticalSection;

type


  { ── Classe pubblica ──────────────────────────────────────────────────── }

  { TNMEAForwarder }

  TNMEAForwarder = class(TLBBaseThread)
  strict private
    type
      TInternalState = (is_OpenPort = 1,
                        is_ForwardMessages = 2);
    var
      FSerial     : TBlockSerial;
      FRxDrain    : TLBCircularBuffer;
      FCSQueue    : TTimedOutCriticalSection;
      FQueue      : TStringList;
      FPort       : string;
      FMaxQueue   : Integer;
      FEvent      : PRTLEvent;

    function DrainQueue(): Boolean;

    { Apre la porta seriale virtuale e avvia il thread di scrittura.
      Restituisce True se la porta è stata aperta correttamente. }
    function  Open(): Boolean;
    procedure Close;

  protected
    procedure Execute; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    { Accoda una stringa NMEA per la scrittura sulla porta virtuale.
      Thread-safe. Chiamabile dal thread del rover direttamente.
      La stringa deve includere il terminatore \\r\\n. }
    procedure Write(const Line: string);

    property Port     : string  read FPort write FPort;

    { Numero massimo di righe in coda prima di scartare le più vecchie.
      Default: 64. Ridurre se la memoria è critica. }
    property MaxQueue : Integer read FMaxQueue write FMaxQueue;
  end;

implementation

uses
  ULBLogger;


{ ══ TNMEAForwarder ═══════════════════════════════════════════════════════════ }

constructor TNMEAForwarder.Create;
begin
  inherited Create;

  FSerial     := TBlockSerial.Create;
  FRxDrain    := TLBCircularBuffer.Create(1024);
  FQueue      := TStringList.Create;
  FCSQueue    := TTimedOutCriticalSection.Create;
  FEvent      := RTLEventCreate;

  FMaxQueue   := 64;
end;

destructor TNMEAForwarder.Destroy;
begin
  inherited Destroy;

  Self.Close;
  Self.DrainQueue;
  FreeAndNil(FCSQueue);
  RTLEventDestroy(FEvent);
  FQueue.Free;
  FRxDrain.Free;
  FSerial.Free;
end;

function TNMEAForwarder.Open: Boolean;
begin
  Result := False;

  Self.Close;

  if FPort = '' then
  begin
    LBLogger.Write(1, 'TNMEAForwarder.Open', lmt_Warning, 'Virtual port not defined!');
    Exit;
  end;

  FSerial.Connect(FPort);
  if FSerial.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TNMEAForwarder.Open', lmt_Error, 'Cannot open virtual port <%s>: %s', [FPort, FSerial.LastErrorDesc]);
    Exit;
  end;

  { Baud rate standard per porta virtuale — il valore è ignorato dalla
    coppia FVSP ma TBlockSerial richiede una chiamata a Config. }
  FSerial.Config(115200, 8, 'N', SB1, False, False);
  if FSerial.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TNMEAForwarder.Open', lmt_Error, 'Cannot configure virtual port <%s>: %s', [FPort, FSerial.LastErrorDesc]);
    FSerial.CloseSocket;
    Exit;
  end;

  LBLogger.Write(5, 'TNMEAForwarder.Open', lmt_Info, 'NMEA forwarder opened on port <%s>', [FPort]);
  Result := True;
end;

procedure TNMEAForwarder.Close;
begin

  LBLogger.Write(5, 'TNMEAForwarder.Close', lmt_Info, 'Closing NMEA forwarder on port <%s>', [FPort]);

  { Svuota la coda di stringhe rimaste }
  if FCSQueue.Acquire('TNMEAForwarder.Close') then
  begin
    try
      FQueue.Clear;
    finally
      FCSQueue.Release();
    end;
  end;

  FSerial.CloseSocket;
end;

procedure TNMEAForwarder.Execute;
var
  _InternalState : TInternalState = is_OpenPort;

begin
  while not Self.Terminated do
  begin
    case _InternalState of
      is_OpenPort:
        begin
          if Self.Open() then
            _InternalState := is_ForwardMessages
          else
            Self.PauseFor(10000);
        end;

      is_ForwardMessages:
        begin
          RTLEventWaitFor(FEvent, 20);
          RTLEventResetEvent(FEvent);
          if not Self.Terminated then
          begin
            if not Self.DrainQueue() then
            begin
              _InternalState := is_OpenPort;
              Self.Close;
              Self.PauseFor(5000);
            end;
          end;
        end;
    end;
  end;

  { Flush finale prima di uscire }
  Self.DrainQueue;
end;

procedure TNMEAForwarder.Write(const Line: string);
{ Accoda la stringa. Se la coda è piena scarta la riga più vecchia.
  Thread-safe: TThreadList usa il proprio lock interno. }
begin
  if Line = '' then Exit;

  if (Length(Line) < 6) or (Line[1] <> '$') then
  begin
    LBLogger.Write(1, 'TNMEAForwarder.Write', lmt_Warning, 'Wrong NMEA string: <%s>', [Line]);
    Exit;
  end;


  if FCSQueue.Acquire('TNMEAForwarder.Write') then
  begin
    try
      if FQueue.Count >= FMaxQueue then
      begin
        LBLogger.Write(1, 'TNMEAForwarder.Write', lmt_Debug, 'NMEA queue full (%d), oldest line dropped', [FMaxQueue]);
        FQueue.Delete(0);
      end;

      FQueue.Add(Line);
    finally
      FCSQueue.Release();
    end;
  end;

  RTLEventSetEvent(FEvent);
end;

function TNMEAForwarder.DrainQueue: Boolean;
{ Preleva tutte le righe dalla coda e le scrive sulla porta seriale.
  Chiamato dal thread di scrittura. }
var
  _Buf : AnsiString;
begin
  Result := True;

  while (not Self.Terminated) do
  begin
    _Buf := '';
    if FCSQueue.Acquire('TNMEAForwarder.DrainQueue') then
    begin
      try
        if FQueue.Count > 0 then
        begin
          _Buf := AnsiString(FQueue.Strings[0]);
          if not _Buf.EndsWith(#13#10) then
            _Buf += #13#10;

          FQueue.Delete(0);
        end;
      finally
        FCSQueue.Release();
      end;
    end;

    if _Buf = '' then Break;

    if FSerial.SendBuffer(@_Buf[1], Length(_Buf)) <> Length(_Buf) then
    begin
      LBLogger.Write(2, 'TNMEAForwarder.DrainQueue', lmt_Warning, 'Incomplete write on port <%s>: %s', [FPort, FSerial.LastErrorDesc]);
      Result := False;
      Break;
    end;

    if FSerial.WaitingData > 0 then
    begin
      FRxDrain.WriteFromSerial(FSerial);
      FRxDrain.Clear;
    end;
  end;
end;

end.
