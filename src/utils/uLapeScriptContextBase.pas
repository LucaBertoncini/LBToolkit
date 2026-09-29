unit uLapeScriptContextBase;

{$mode ObjFPC}{$H+}

// =============================================================================
// uLapeScriptContextBase — Motore Lape generico, riutilizzabile in qualsiasi
// progetto Free Pascal che voglia eseguire script .lape a caldo.
//
// RESPONSABILITÀ:
//   - Compilazione e ricompilazione a caldo di uno script .lape (Compile,
//     CheckForReload) con backoff su mtime: uno script rotto non viene
//     ritentato ad ogni frame, solo quando il file viene effettivamente
//     modificato su disco.
//   - Esecuzione del bytecode compilato (RunFrame).
//   - Reload via segnale POSIX SIGUSR1 (InstallScriptReloadSignalHandler):
//     un singolo segnale globale sveglia il meccanismo di CheckForReload
//     su tutti gli script attivi, ognuno dei quali verifica autonomamente
//     se il proprio file è cambiato.
//   - Gestione eventi post-esecuzione tramite TEventsManager.
//
// NESSUNA DIPENDENZA DA JACS:
//   Questa unit non conosce telecamere, pipeline video, item PLC, messaggi
//   o qualsiasi altro concetto specifico di JACS. L'API Lape esposta allo
//   script è definita interamente dalla sottoclasse tramite l'override di
//   RegisterAPI.
//
// CICLO DI VITA:
//   { TLapeScriptContextBase — NON ereditare da TInterfacedObject.
//     Il ciclo di vita è gestito dalla classe che crea e possiede le
//     istanze: se si aggiungesse TInterfacedObject, il possessore
//     diventerebbe un puntatore dangling non appena qualsiasi variabile
//     di interfaccia che referenzia questo oggetto uscisse di scope. }
//
// USO TIPICO:
//   1. Sottoclassare TLapeScriptContextBase.
//   2. Fare override di RegisterAPI per aggiungere le funzioni/variabili
//      Lape specifiche del dominio.
//   3. Chiamare Compile dopo la creazione.
//   4. Chiamare RunFrame ad ogni evento/frame in cui lo script deve girare.
//   5. Installare il signal handler una volta sola all'avvio
//      (InstallScriptReloadSignalHandler) e richiedere il reload via
//      kill -USR1 $(pidof <processo>) oppure chiamando RequestScriptReload.
// =============================================================================

interface

uses
  Classes, SysUtils, BaseUnix,
  lptypes, lpvartypes, lpcompiler, lpparser, lpmessages, lpinterpreter, lputils,
  uLBSignalManager, uEventsManager;

type

  { TLapeScriptContextBase }

  TLapeScriptContextBase = class(TObject)
  strict private
    FScriptFile          : string;
    FCompiler            : TLapeCompiler;
    FCompiledOk          : boolean;
    FLastError           : string;
    FSourceMTime         : TDateTime;
    FLastCheckedGeneration: longint;
    FEventsManager       : TEventsManager;

    function TryCompile(out ANewCompiler: TLapeCompiler; out AError: string): boolean;

  protected
    // Override obbligatorio nella sottoclasse: registra le funzioni, le
    // variabili e i tipi Lape specifici del dominio.
    // Chiamato automaticamente da TryCompile a ogni (ri)compilazione.
    // La sottoclasse dovrebbe chiamare inherited per registrare Log().
    procedure RegisterAPI(ACompiler: TLapeCompiler); virtual;

    // Hook chiamato da RunFrame prima di eseguire il bytecode.
    // La sottoclasse può usarlo per preparare i dati frame-specifici
    // (es. copiare i box rilevati in variabili Lape) e per pulire
    // lo stato precedente (es. svuotare la lista degli item cambiati).
    // Default: no-op.
    procedure BeforeRun; virtual;

    // Hook chiamato da RunFrame dopo l'esecuzione del bytecode, solo se
    // l'esecuzione ha avuto successo (Result = True).
    // La sottoclasse può usarlo per propagare i risultati (es. alzare
    // cEvent_ScriptExecuted se ci sono item cambiati).
    // Default: no-op.
    procedure AfterRun; virtual;

  public
    // ==========================================================================
    // Costruttore / Distruttore
    // ==========================================================================

    constructor Create(const AScriptFile: string); virtual;
    destructor Destroy; override;

    // ==========================================================================
    // Compilazione e reload
    // ==========================================================================

    // Compila (o ricompila) lo script. Aggiorna FSourceMTime anche in caso
    // di fallimento, così CheckForReload non ritenta finché il file non cambia.
    // Ritorna True se la compilazione ha avuto successo.
    function Compile: boolean;

    // Controlla se il segnale di reload è arrivato e se il file è cambiato
    // su disco rispetto all'ultima compilazione. Se sì, ricompila.
    // Chiamato automaticamente da RunFrame all'inizio di ogni esecuzione.
    // Ritorna True se la ricompilazione è avvenuta con successo.
//    function CheckForReload: boolean;

    // ==========================================================================
    // Esecuzione
    // ==========================================================================

    // Esegue il bytecode compilato. Chiama CheckForReload, poi BeforeRun,
    // poi il bytecode, poi AfterRun (solo se il bytecode è andato a buon fine).
    // Ritorna True se l'esecuzione ha avuto successo.
    function RunFrame: boolean;

    // ==========================================================================
    // Properties
    // ==========================================================================

    property ScriptFile  : string         read FScriptFile;
    property CompiledOk  : boolean        read FCompiledOk;
    property LastError   : string         read FLastError;
    property EventsManager: TEventsManager read FEventsManager;

  const
    // Alzato da AfterRun (o dall'override di AfterRun nella sottoclasse) dopo
    // ogni esecuzione dello script che ha prodotto risultati significativi.
    // Sender = Self. La definizione di "significativo" è a carico della
    // sottoclasse (es. ChangedItems.Count > 0 in JACS).
    cEvent_ScriptExecuted = string('01');
  end;

// =============================================================================
// Gestione segnale SIGUSR1 per il reload a caldo
// =============================================================================

// Installa l'handler POSIX SIGUSR1 che incrementa il contatore globale
// gScriptReloadGeneration. Tutti i TLapeScriptContextBase attivi rileveranno
// la variazione al prossimo CheckForReload (chiamato da RunFrame).
// Idempotente: se già installato, ritorna True senza reinstallare.
function InstallScriptReloadSignalHandler: boolean;

// Ripristina l'handler SIGUSR1 precedente e libera il TSignalManager.
// Chiamato automaticamente nella finalization di questa unit.
procedure UninstallScriptReloadSignalHandler;

// Incrementa il contatore di reload direttamente, senza segnale POSIX.
// Utile per forzare il reload da codice (es. da un endpoint HTTP).
procedure RequestScriptReload;

implementation

uses
  ULBLogger;

var
  // Contatore atomico incrementato dall'handler SIGUSR1 (o da RequestScriptReload).
  // Ogni TLapeScriptContextBase confronta il proprio FLastCheckedGeneration con
  // questo valore per sapere se deve verificare il mtime del file.
  gScriptReloadGeneration : longint         = 0;
  gSignalManager          : TSignalManager  = nil;

// -----------------------------------------------------------------------------
// Handler segnale — deve essere async-signal-safe
// -----------------------------------------------------------------------------

procedure HandleReloadSignal(signal: longint; info: psiginfo; context: psigcontext); cdecl;
begin
  // ATTENZIONE: questo handler corre nel contesto del segnale POSIX.
  // Deve restare async-signal-safe: nessuna allocazione heap, nessun log,
  // nessuna chiamata che possa rientrare in malloc/printf.
  InterLockedIncrement(gScriptReloadGeneration);
end;

// -----------------------------------------------------------------------------
// API pubblica per il signal handler
// -----------------------------------------------------------------------------

function InstallScriptReloadSignalHandler: boolean;
begin
  Result := False;
  if gSignalManager <> nil then Exit(True);

  gSignalManager := TSignalManager.Create;
  if gSignalManager.SetSignalAction(SIGUSR1, @HandleReloadSignal) then
  begin
    Result := True;
    LBLogger.Write(5, 'InstallScriptReloadSignalHandler', lmt_Debug, 'SIGUSR1 installed');
  end
  else
  begin
    FreeAndNil(gSignalManager);
    LBLogger.Write(1, 'InstallScriptReloadSignalHandler', lmt_Warning, 'SIGUSR1 install failed');
  end;
end;

procedure UninstallScriptReloadSignalHandler;
begin
  if gSignalManager <> nil then
    FreeAndNil(gSignalManager);
end;

procedure RequestScriptReload;
begin
  InterLockedIncrement(gScriptReloadGeneration);
end;

// -----------------------------------------------------------------------------
// TLapeScriptContextBase
// -----------------------------------------------------------------------------

constructor TLapeScriptContextBase.Create(const AScriptFile: string);
begin
  inherited Create;

  FScriptFile            := AScriptFile;
  FCompiler              := nil;
  FCompiledOk            := False;
  FLastError             := '';
  FSourceMTime           := 0;

  FLastCheckedGeneration := -1;  // garantisce che venga fatta la compilazione alla prima chiamata

  FEventsManager         := TEventsManager.Create(Self);

  if gSignalManager = nil then
    InstallScriptReloadSignalHandler;
end;

destructor TLapeScriptContextBase.Destroy;
begin
  FreeAndNil(FCompiler);
  FreeAndNil(FEventsManager);
  inherited Destroy;
end;

procedure TLapeScriptContextBase.RegisterAPI(ACompiler: TLapeCompiler);
begin
  // Base: nessuna funzione aggiuntiva. La sottoclasse chiama inherited e poi
  // aggiunge le proprie funzioni/variabili specifiche del dominio.
end;

procedure TLapeScriptContextBase.BeforeRun;
begin
  // No-op nella base.
end;

procedure TLapeScriptContextBase.AfterRun;
begin
  // No-op nella base. La sottoclasse overrida per propagare i risultati
  // (es. alzare cEvent_ScriptExecuted se ci sono item cambiati).
end;

function TLapeScriptContextBase.TryCompile(out ANewCompiler: TLapeCompiler; out AError: string): boolean;
begin
  Result       := False;
  ANewCompiler := nil;
  AError       := '';

  if not FileExists(FScriptFile) then
  begin
    AError := Format('File <%s> not found', [FScriptFile]);
    Exit;
  end;

  try
    ANewCompiler := TLapeCompiler.Create(TLapeTokenizerFile.Create(FScriptFile));
    InitializePascalScriptBasics(ANewCompiler, [psiTypeAlias]);
    Self.RegisterAPI(ANewCompiler);

    if ANewCompiler.Compile then
      Result := True
    else
      AError := 'Compilation error';

  except
    on E: Exception do
      AError := E.Message;
  end;

  if not Result then
    FreeAndNil(ANewCompiler);
end;

function TLapeScriptContextBase.Compile: boolean;
var
  _currentGen: longint;
  _newMTime: TDateTime;

begin
  Result := False;

  if not FileExists(FScriptFile) then
  begin
    if FSourceMTime = 0 then
    begin
      FLastError := 'File not found';
      LBLogger.Write(1, 'TLapeScriptContextBase.Compile', lmt_Warning, 'File <%s> not found!', [FScriptFile]);
    end;
    Exit;
  end;

  // 1. Verifica se dobbiamo considerare una nuova compilazione
  _currentGen := gScriptReloadGeneration;

  if (FSourceMTime = 0) or (_currentGen <> FLastCheckedGeneration) then
  begin
    // Prima compilazione in assoluto, oppure segnale di reload ricevuto
    if not FileAge(FScriptFile, _newMTime) then Exit;   // sicurezza

    // Se non è la prima volta e l'mtime è invariato, non ricompilare.
    if (FSourceMTime <> 0) and (_newMTime = FSourceMTime) then
    begin
      FLastCheckedGeneration := _currentGen;   // aggiorna per non rientrare subito
      Exit(FCompiledOk);                       // ritorna stato precedente
    end;

    // OK, mtime cambiato (o prima compilazione): aggiorniamo i riferimenti
    FSourceMTime := _newMTime;
    FLastCheckedGeneration := _currentGen;

    // Compila davvero
    FCompiledOk := Self.TryCompile(FCompiler, FLastError);
    Result := FCompiledOk;

    if Result then
      LBLogger.Write(5, 'TLapeScriptContextBase.Compile', lmt_Debug, 'Script compiled/reloaded <%s>', [FScriptFile])
    else
      LBLogger.Write(1, 'TLapeScriptContextBase.Compile', lmt_Warning, 'Compilation failed for <%s>: %s', [FScriptFile, FLastError]);
  end
  else begin
    // Nessun segnale e già compilato in precedenza: restituisci lo stato attuale
    Result := FCompiledOk;
  end;
end;

(*
function TLapeScriptContextBase.Compile: boolean;
var
  _newCompiler: TLapeCompiler;
  _err        : string = '';
  _mtime      : TDateTime;
begin
  FCompiledOk := False;
  FLastError  := '';

  if FileExists(FScriptFile) then
  begin
    // Aggiorna FSourceMTime anche se la compilazione fallisce: così
    // CheckForReload non ritenta finché il file non viene modificato.
    if FileAge(FScriptFile, _mtime) then
      FSourceMTime := _mtime;

    if Self.TryCompile(_newCompiler, _err) then
    begin
      FreeAndNil(FCompiler);
      FCompiler   := _newCompiler;
      FCompiledOk := True;
    end
    else
      FLastError := _err;
  end
  else
    LBLogger.Write(1, 'TLapeScriptContextBase.Compile', lmt_Warning, 'File <%s> not found!', [FScriptFile]);

  Result := FCompiledOk;

  if not FCompiledOk then
    LBLogger.Write(1, 'TLapeScriptContextBase.Compile', lmt_Warning, 'File <%s> not compiled: <%s>', [FScriptFile, _err]);
end;

function TLapeScriptContextBase.CheckForReload: boolean;
var
  _currentGen  : longint;
  _newMTime    : TDateTime;
  _newCompiler : TLapeCompiler;
  _err         : string;
begin
  Result := False;

  _currentGen := gScriptReloadGeneration;
  if _currentGen = FLastCheckedGeneration then Exit;

  FLastCheckedGeneration := _currentGen;

  if not FileAge(FScriptFile, _newMTime) then Exit;
  if _newMTime = FSourceMTime then Exit;

  if Self.TryCompile(_newCompiler, _err) then
  begin
    FreeAndNil(FCompiler);
    FCompiler    := _newCompiler;
    FCompiledOk  := True;
    FLastError   := '';
    FSourceMTime := _newMTime;
    Result       := True;
    LBLogger.Write(5, 'TLapeScriptContextBase.CheckForReload', lmt_Debug, 'Script reloaded: %s', [FScriptFile]);
  end
  else
  begin
    FLastError := _err;
    LBLogger.Write(1, 'TLapeScriptContextBase.CheckForReload', lmt_Warning, 'Reload failed: %s — %s', [FScriptFile, _err]);
  end;
end;
*)

function TLapeScriptContextBase.RunFrame: boolean;
var
  _runner: TLapeCodeRunner;
begin
  Result := False;

  // CheckForReload viene chiamato sempre, anche se FCompiledOk = False:
  // è l'unico modo per accorgersi che uno script rotto è stato corretto.
  // Non comporta lavoro se la generazione non è cambiata o il mtime è uguale.
  Self.Compile; // CheckForReload;

  if not FCompiledOk then
  begin
    LBLogger.Write(1, 'TLapeScriptContextBase.RunFrame', lmt_Warning, 'Not compiled: %s', [FScriptFile]);
    Exit;
  end;

  Self.BeforeRun;

  try
    _runner := TLapeCodeRunner.Create(FCompiler.Emitter);
    try
      _runner.Run;
      Result := True;
    finally
      _runner.Free;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TLapeScriptContextBase.RunFrame', lmt_Error, 'Runtime error in <%s>: %s', [FScriptFile, E.Message]);
  end;

  if Result then
    Self.AfterRun;
end;

{
------------------------------------------------------------------------------
RICARICAMENTO A CALDO DEGLI SCRIPT — USO DA CONSOLE LINUX
------------------------------------------------------------------------------

Per richiedere il ricaricamento di tutti gli script .lape senza riavviare
il processo, inviare il segnale SIGUSR1 al processo:

  kill -USR1 $(pidof <nome-processo>)

oppure, se si conosce già il PID:

  kill -USR1 <pid>

Effetto: l'handler incrementa atomicamente gScriptReloadGeneration.
Al RunFrame successivo, ogni TLapeScriptContextBase rileva la variazione
tramite CheckForReload, verifica il mtime del proprio file .lape e, se
diverso dall'ultima compilazione, ricompila.

Solo gli script effettivamente modificati su disco vengono ricompilati:
uno script invariato che riceve il segnale non fa nulla (mtime uguale).

In alternativa al segnale POSIX, si può chiamare RequestScriptReload()
direttamente dal codice (es. da un endpoint HTTP di gestione).
------------------------------------------------------------------------------
}

initialization

finalization
  UninstallScriptReloadSignalHandler;

end.
