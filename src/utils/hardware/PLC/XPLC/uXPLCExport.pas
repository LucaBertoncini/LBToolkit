unit uXPLCExport;

{
  Esportatore XPLC → PlantUML e Mermaid
  ======================================
  Genera diagrammi di stato da macchine a stati XPLC in due formati:
    - PlantUML  (stateDiagram via @startuml/@enduml)
    - Mermaid   (stateDiagram-v2)

  Funzionalità PlantUML:
    - ExportToPlantUML         : singola macchina da TLogicController
    - ExportToPlantUMLFromFile : singola macchina da file XML
    - ExportManagerToPlantUML  : tutte le macchine del TXPLCManager in un
                                 unico diagramma con stati annidati per
                                 macchina; i canali di comunicazione via
                                 shared variables vengono riportati come
                                 nota e legenda nel diagramma

  Funzionalità Mermaid:
    - ExportToMermaid         : singola macchina da TLogicController
    - ExportToMermaidFromFile : singola macchina da file XML
    - ExportManagerToMermaid  : tutte le macchine in un unico diagramma
                                con stati annidati; i canali via shared
                                variables vengono riportati come commenti %%

  Funzionalità JSON:
    - ExportToJSON         : singola macchina da TLogicController
                             Produce un oggetto JSON con stati, transizioni,
                             azioni OnEnter/OnExit e stato iniziale.
                             Destinato a renderer dinamici (Cytoscape.js,
                             Vis.js) e al WebSocket di supervisione live.
    - ExportToJSONFromFile : singola macchina da file XML
    - ExportManagerToJSON  : tutte le macchine del manager in un array JSON;
                             include shared variables e canali di
                             comunicazione identificati automaticamente.

  Differenze di rendering tra i due formati:
    - PlantUML supporta entry/exit nativi (%sS%d : entry / ...)
    - Mermaid usa "note right of" per le azioni OnEnter/OnExit
    - PlantUML: ':' nelle label va escaped (\:)
    - Mermaid : ':' nelle label non richiede escape
    - Entrambi usano [*] --> StatoIniziale per lo stato di partenza

  PROPRIETÀ RICHIESTE IN uXPLC.pas (già presenti):
  -------------------------------------------------
  TXPLCCondition  : property Expression: string read FExpression;
  TXPLCTransition : property Conditions: TConditionsList read FConditions;
                    property Actions:    TActionsList    read FActions;
  TXPLCState      : property OnEnterActions: TActionsList     read FOnEnterActions;
                    property OnExitActions:  TActionsList     read FOnExitActions;
                    property Transitions:    TTransitionsList read FTransitions;
  TXPLCStateMachine: property States:           TStatesMap read FStates;
                     property InitialStateName: string     read FInitialStateName;
  TLogicController: property XPLCName:      string            read FXPLCName;
                    property StateMachine:  TXPLCStateMachine read FStateMachine;
                    property SharedVars:    TSharedVarsMap    read FSharedVars;
  TXPLCManager    : property XPLCs:      TLogicControllerList read FXPLCs;
                    property SharedVars: TSharedVarsMap       read FSharedVars;
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, uXPLC;

type
  { TXPLCExporter }
  TXPLCExporter = class
  public
    { Singola macchina da controller già caricato }
    class function ExportToPlantUML(AController: TLogicController; AIncludeLegend: Boolean = False): string;

    { Singola macchina da file XML diretto (non .xplci) }
    class function ExportToPlantUMLFromFile(const AFileName: string; AIncludeLegend: Boolean = False): string;

    { Tutte le macchine del manager in un unico diagramma PlantUML.
      Ogni macchina è un sottostate annidato (state "Nome" { ... }).
      Le interazioni via shared variables vengono identificate analizzando
      i SetOutput sulle shared vars e le condizioni che le leggono; vengono
      riportate come nota e legenda nel diagramma finale. }
    class function ExportManagerToPlantUML(AManager: TXPLCManager): string;

    { ── Mermaid ─────────────────────────────────────────────────────────── }

    { Singola macchina da controller gia' caricato — formato Mermaid stateDiagram-v2 }
    class function ExportToMermaid(AController: TLogicController; AIncludeLegend: Boolean = False): string;

    { Singola macchina da file XML — formato Mermaid }
    class function ExportToMermaidFromFile(const AFileName: string; AIncludeLegend: Boolean = False): string;

    { Tutte le macchine del manager in un unico diagramma Mermaid.
      Ogni macchina e' un subgraph annidato. I canali via shared variables
      vengono riportati come commenti annotati alla fine del diagramma. }
    class function ExportManagerToMermaid(AManager: TXPLCManager): string;

    { ── JSON ──────────────────────────────────────────────────────────── }

    { Singola macchina -> oggetto JSON con stati, transizioni, azioni,
      stato iniziale. Destinato a Cytoscape.js / Vis.js e WebSocket live. }
    class function ExportToJSON(AController: TLogicController): string;

    { Singola macchina da file XML -> JSON }
    class function ExportToJSONFromFile(const AFileName: string): string;

    { Tutte le macchine del manager -> array JSON.
      Include shared variables e canali di comunicazione. }
    class function ExportManagerToJSON(AManager: TXPLCManager): string;
  end;

implementation

uses
  ULBLogger, uTimerWheel, StrUtils, fpjson, jsonparser;

{ ---------------------------------------------------------------------------
  EscapePlantUML — escape caratteri speciali per PlantUML
  - % : direttiva preprocessore PlantUML → %%
  - \ : escape backslash → \\
  - " : apice doppio → \"
  - : : separatore etichetta → \:
  --------------------------------------------------------------------------- }
function EscapePlantUML(const S: string): string;
begin
  Result := StringReplace(S,  '\',  '\\',  [rfReplaceAll]);
  Result := StringReplace(Result, '"',  '\"',  [rfReplaceAll]);
  Result := StringReplace(Result, '%',  '%%',  [rfReplaceAll]);  // FIX: evita che PlantUML interpreti %var% come direttiva
  Result := StringReplace(Result, ':',  '\:',  [rfReplaceAll]);
end;

{ ---------------------------------------------------------------------------
  ReverseExpression — trasformazione inversa parziale di PrepareExpression,
  per rendere le condizioni più leggibili nel diagramma.
  Ripristina TIMEOUT(X) dalla forma (X_Timeout = 1).
  --------------------------------------------------------------------------- }
function ReverseExpression(const S: string): string;
var
  i, j: Integer;
  TimerName: string;
  UpperS: string;
  SearchStr: string;
begin
  Result := S;
  // Ripristina (X_Timeout = 1) → TIMEOUT(X)
  UpperS := UpperCase(Result);
  SearchStr := '_TIMEOUT = 1)';
  i := Pos(SearchStr, UpperS);
  while i > 0 do
  begin
    // Cerca il '(' aperto prima
    j := i - 1;
    while (j > 0) and (Result[j] <> '(') do Dec(j);
    if j > 0 then
    begin
      TimerName := Copy(Result, j + 1, i - j - 1);
      Delete(Result, j, i - j + Length(SearchStr));
      Insert('TIMEOUT(' + TimerName + ')', Result, j);
      UpperS := UpperCase(Result);
    end;
    i := Pos(SearchStr, UpperS);
  end;
end;

{ ---------------------------------------------------------------------------
  ActionListToString — converte lista azioni in stringa leggibile
  --------------------------------------------------------------------------- }
function ActionListToString(AList: TActionsList): string;
var
  Act: TXPLCAction;
  Tmp: string;
begin
  Result := '';
  if (AList = nil) or (AList.Count = 0) then Exit;

  for Act in AList do
  begin
    Tmp := '';
    case Act.ActionType of
      atSetOutput:
        Tmp := Format('Set(%s=%s)', [Act.TargetAlias, VarToStr(Act.Value)]);
      atStartTimer:
        if Integer(Act.Value) > 0 then
          Tmp := Format('StartTimer(%s,%dms)', [Act.TargetAlias, Integer(Act.Value)])
        else
          Tmp := Format('StartTimer(%s)', [Act.TargetAlias]);
      atStopTimer:
        Tmp := Format('StopTimer(%s)', [Act.TargetAlias]);
      atIncrementCounter:
        Tmp := Format('Inc(%s)', [Act.TargetAlias]);
      atDecrementCounter:
        Tmp := Format('Dec(%s)', [Act.TargetAlias]);
      atResetCounter:
        Tmp := Format('Reset(%s)', [Act.TargetAlias]);
      atLog:
        Tmp := Format('Log("%s")', [EscapePlantUML(VarToStr(Act.Value))]);
      atApplyPreset:
        Tmp := Format('Preset(%s)', [Act.TargetAlias]);
    end;
    if Tmp <> '' then
    begin
      if Result <> '' then Result := Result + '\n';
      Result := Result + Tmp;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  BuildConditionLabel — etichetta condizione per una transizione
  --------------------------------------------------------------------------- }
function BuildConditionLabel(ATrans: TXPLCTransition): string;
var
  Cond: TXPLCCondition;
  Expr: string;
begin
  Result := '';
  if (ATrans = nil) or (ATrans.Conditions = nil) or (ATrans.Conditions.Count = 0) then
  begin
    Result := 'true';
    Exit;
  end;

  for Cond in ATrans.Conditions do
  begin
    // ReverseExpression prima, EscapePlantUML dopo: l'escape deve avvenire
    // sull'espressione già leggibile per evitare di alterare i pattern di ricerca
    Expr := EscapePlantUML(ReverseExpression(Cond.Expression));
    if Expr = '' then Continue;
    if Result <> '' then Result := Result + ' AND ';
    Result := Result + Expr;
  end;

  if Result = '' then Result := 'true';
end;

{ ---------------------------------------------------------------------------
  GenerateSingleSM — genera il corpo PlantUML per una singola macchina.
  APrefix: prefisso usato per gli ID degli stati (es. 'SM0_').
           Serve per evitare collisioni di nomi quando più macchine
           vengono incluse nello stesso diagramma.
  AIndent: livello di indentazione (spazi) per annidamento in substates.
  --------------------------------------------------------------------------- }
function GenerateSingleSM(AController: TLogicController; const APrefix: string; const AIndent: string): string;
var
  SM: TXPLCStateMachine;
  States: TStatesMap;
  i, j, TargetIdx: Integer;
  State: TXPLCState;
  Trans: TXPLCTransition;
  OnEnterStr, OnExitStr: string;
  CondLabel, ActLabel, TransLabel: string;
begin
  Result := '';
  SM := AController.StateMachine;
  if (SM = nil) then Exit;

  States := SM.States;
  if (States = nil) or (States.Count = 0) then Exit;

  // Stato iniziale
  if SM.InitialStateName <> '' then
  begin
    TargetIdx := States.IndexOf(SM.InitialStateName);
    if TargetIdx >= 0 then
      Result := Result + AIndent + Format('[*] --> %sS%d', [APrefix, TargetIdx]) + LineEnding;
  end;
  Result := Result + LineEnding;

  // Dichiarazione stati
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    Result := Result + AIndent + Format('state "%s" as %sS%d', [State.StateName, APrefix, i]) + LineEnding;
  end;
  Result := Result + LineEnding;

  // Note OnEnter / OnExit (sintassi entry/exit nativa PlantUML)
  // Riga separata per entry e per exit: più leggibile nel diagramma
  // rispetto a concatenarle sulla stessa riga con \n.
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    OnEnterStr := ActionListToString(State.OnEnterActions);
    OnExitStr  := ActionListToString(State.OnExitActions);
    if (OnEnterStr = '') and (OnExitStr = '') then Continue;

    if OnEnterStr <> '' then
      Result := Result + AIndent + Format('%sS%d : entry / %s', [APrefix, i, OnEnterStr]) + LineEnding;
    if OnExitStr <> '' then
      Result := Result + AIndent + Format('%sS%d : exit / %s', [APrefix, i, OnExitStr]) + LineEnding;
  end;
  Result := Result + LineEnding;

  // Transizioni
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    if (State.Transitions = nil) or (State.Transitions.Count = 0) then Continue;

    for j := 0 to State.Transitions.Count - 1 do
    begin
      Trans := State.Transitions[j];
      if Trans.TargetStateName = '' then Continue;

      TargetIdx := States.IndexOf(Trans.TargetStateName);
      if TargetIdx < 0 then
      begin
        LBLogger.Write(1, 'TXPLCExporter', lmt_Warning,
          Format('Target state "%s" not found (from "%s" in "%s")',
          [Trans.TargetStateName, State.StateName, AController.XPLCName]));
        Continue;
      end;

      CondLabel := BuildConditionLabel(Trans);
      ActLabel  := ActionListToString(Trans.Actions);
      TransLabel := CondLabel;
      if ActLabel <> '' then
        TransLabel := TransLabel + ' / ' + ActLabel;

      Result := Result + AIndent +
        Format('%sS%d --> %sS%d : %s', [APrefix, i, APrefix, TargetIdx, TransLabel]) + LineEnding;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToPlantUML
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToPlantUML(AController: TLogicController;
  AIncludeLegend: Boolean): string;
var
  SharedVars: TSharedVarsMap;
  i: Integer;
  VarLine: string;
begin
  Result := '';
  if AController = nil then Exit;

  Result := '@startuml' + LineEnding;
  if AController.XPLCName <> '' then
    Result := Result + 'title ' + AController.XPLCName + LineEnding;
  Result := Result + '!theme plain' + LineEnding;
  Result := Result + 'hide empty description' + LineEnding;
  Result := Result + LineEnding;

  Result := Result + GenerateSingleSM(AController, '', '');

  // Legenda shared variables (opzionale)
  SharedVars := AController.SharedVars;
  if AIncludeLegend and (SharedVars <> nil) and (SharedVars.Count > 0) then
  begin
    Result := Result + 'legend right' + LineEnding;
    Result := Result + '  **Shared Variables**' + LineEnding;
    for i := 0 to SharedVars.Count - 1 do
    begin
      VarLine := Format('  %s = %s',
        [SharedVars.Keys[i], VarToStr(SharedVars.Data[i].GetValue)]);
      Result := Result + VarLine + LineEnding;
    end;
    Result := Result + 'end legend' + LineEnding;
    Result := Result + LineEnding;
  end;

  Result := Result + '@enduml' + LineEnding;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToPlantUMLFromFile
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToPlantUMLFromFile(const AFileName: string;
  AIncludeLegend: Boolean): string;
var
  Ctrl: TLogicController;
  Wheel: TTimerWheel;
begin
  Result := '';
  Wheel := TTimerWheel.Create;
  try
    Ctrl := TLogicController.Create(nil, Wheel);
    try
      if Ctrl.LoadFromXMLFile(AFileName) then
        Result := ExportToPlantUML(Ctrl, AIncludeLegend)
      else
        LBLogger.Write(1, 'TXPLCExporter.ExportToPlantUMLFromFile', lmt_Error,
          'Failed to load: ' + AFileName);
    finally
      Ctrl.Free;
    end;
  finally
    Wheel.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  ExportManagerToPlantUML
  Genera un unico diagramma con:
    - ogni macchina come substate annidato (state "NomeMacchina" { ... })
    - nota finale con le shared variables e i canali di comunicazione
      identificati analizzando le SetOutput su shared vars
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportManagerToPlantUML(AManager: TXPLCManager
  ): string;
var
  XPLCs: TLogicControllerList;
  SharedVars: TSharedVarsMap;
  Ctrl: TLogicController;
  i: Integer;
  Prefix, SMBody: string;

  // Raccolta canali di comunicazione via shared variables
  // Per ogni (produttore, variabile, consumatore) troviamo:
  //   - chi fa SetOutput su una shared var (produttore)
  //   - chi ha una condizione che la legge (consumatore)
  // Li mappiamo come note nel diagramma.
  type
    TSharedVarChannel = record
      Producer: string;    // nome macchina che scrive
      VarName: string;     // nome shared var
      Consumer: string;    // nome macchina che legge
    end;

  var
    Channels: array of TSharedVarChannel;
    ChannelCount: Integer;

  procedure AddChannel(const AProd, AVar, ACons: string);
  var ci: Integer;
  begin
    // Evita duplicati
    for ci := 0 to ChannelCount - 1 do
      if (Channels[ci].Producer = AProd) and
         (Channels[ci].VarName = AVar) and
         (Channels[ci].Consumer = ACons) then Exit;
    SetLength(Channels, ChannelCount + 1);
    Channels[ChannelCount].Producer := AProd;
    Channels[ChannelCount].VarName  := AVar;
    Channels[ChannelCount].Consumer := ACons;
    Inc(ChannelCount);
  end;

  // Controlla se una stringa contiene il nome di una shared var.
  // Usa un controllo di word-boundary per evitare falsi positivi:
  // es. "Stato" non deve matchare dentro "StatoBarraIngresso".
  function ContainsVarName(const AExpr, AVarName: string): Boolean;
  var
    P, VLen: Integer;
    UExpr, UVar: string;
    CharBefore, CharAfter: Char;
  begin
    Result := False;
    UExpr := UpperCase(AExpr);
    UVar  := UpperCase(AVarName);
    VLen  := Length(UVar);
    P     := Pos(UVar, UExpr);
    while P > 0 do
    begin
      // Carattere immediatamente prima del match (o spazio virtuale se P=1)
      if P > 1 then CharBefore := UExpr[P - 1]
               else CharBefore := ' ';
      // Carattere immediatamente dopo il match (o spazio virtuale se a fine stringa)
      if P + VLen <= Length(UExpr) then CharAfter := UExpr[P + VLen]
                                   else CharAfter := ' ';
      // Boundary: il match è valido solo se non è preceduto/seguito da
      // caratteri alfanumerici o underscore (che farebbero parte di un nome più lungo)
      if not (CharBefore in ['A'..'Z', '0'..'9', '_']) and
         not (CharAfter  in ['A'..'Z', '0'..'9', '_']) then
      begin
        Result := True;
        Exit;
      end;
      P := PosEx(UVar, UExpr, P + 1);
    end;
  end;

  // Scansiona azioni di una lista cercando SetOutput su shared vars
  procedure ScanActionsForProducers(AList: TActionsList; const AMachineName: string);
  var
    Act: TXPLCAction;
    vi, k, m, ti, ci2: Integer;
    st : TXPLCState;
    tr : TXPLCTransition;

  begin
    if AList = nil then Exit;
    for Act in AList do
    begin
      if Act.ActionType = atSetOutput then
      begin
        if SharedVars = nil then Continue;
        for vi := 0 to SharedVars.Count - 1 do
          if SameText(Act.TargetAlias, SharedVars.Keys[vi]) then
          begin
            // Trovato un produttore: cerca i consumatori
            for k := 0 to XPLCs.Count - 1 do
            begin
              if SameText(XPLCs[k].XPLCName, AMachineName) then Continue;
              // Scansiona le condizioni di tutte le transizioni di questa macchina
              if XPLCs[k].StateMachine = nil then Continue;
              if XPLCs[k].StateMachine.States = nil then Continue;
              for m := 0 to XPLCs[k].StateMachine.States.Count - 1 do
              begin
                st := XPLCs[k].StateMachine.States.Data[m];
                if (st.Transitions = nil) then Continue;
                for ti := 0 to st.Transitions.Count - 1 do
                begin
                  tr := st.Transitions[ti];
                  if (tr.Conditions = nil) then Continue;

                  for ci2 := 0 to tr.Conditions.Count - 1 do
                    if ContainsVarName(tr.Conditions[ci2].Expression, SharedVars.Keys[vi]) then
                    begin
                      AddChannel(AMachineName, SharedVars.Keys[vi], XPLCs[k].XPLCName);
                      Break;
                    end;
                end;
              end;
            end;
          end;
      end;
    end;
  end;


var
  si, ti : Integer;
  st : TXPLCState;

begin
  Result := '';
  if AManager = nil then Exit;

  XPLCs := AManager.XPLCs;
  SharedVars := AManager.SharedVars;

  if (XPLCs = nil) or (XPLCs.Count = 0) then Exit;

  ChannelCount := 0;
  SetLength(Channels, 0);

  // ── Intestazione ──────────────────────────────────────────────────────────
  Result := '@startuml' + LineEnding;
  Result := Result + 'title XPLC System Overview' + LineEnding;
  Result := Result + '!theme plain' + LineEnding;
  Result := Result + 'hide empty description' + LineEnding;
  Result := Result + LineEnding;

  // ── Una substate box per ogni macchina ────────────────────────────────────
  for i := 0 to XPLCs.Count - 1 do
  begin
    Ctrl := XPLCs[i];
    if (Ctrl = nil) or (Ctrl.StateMachine = nil) then Continue;
    if (Ctrl.StateMachine.States = nil) or (Ctrl.StateMachine.States.Count = 0) then Continue;

    Prefix := Format('SM%d_', [i]);

    Result := Result + Format('state "%s" as SM%d {', [Ctrl.XPLCName, i]) + LineEnding;

    SMBody := GenerateSingleSM(Ctrl, Prefix, '  ');
    Result := Result + SMBody;

    Result := Result + '}' + LineEnding;
    Result := Result + LineEnding;

    // Raccolta produttori per questa macchina
    // Scansiona OnEnter, OnExit e azioni delle transizioni
    for si := 0 to Ctrl.StateMachine.States.Count - 1 do
    begin
      st := Ctrl.StateMachine.States.Data[si];
      ScanActionsForProducers(st.OnEnterActions, Ctrl.XPLCName);
      ScanActionsForProducers(st.OnExitActions,  Ctrl.XPLCName);
      if st.Transitions <> nil then
      begin
        for ti := 0 to st.Transitions.Count - 1 do
          ScanActionsForProducers(st.Transitions[ti].Actions, Ctrl.XPLCName);
      end;
    end;
  end;

  // ── Nota con i canali di comunicazione via shared vars ────────────────────
  if ChannelCount > 0 then
  begin
    Result := Result + 'note as SharedVarChannels' + LineEnding;
    Result := Result + '  **Canali via Shared Variables**' + LineEnding;
    for i := 0 to ChannelCount - 1 do
      Result := Result + Format('  %s → [%s] → %s',
        [Channels[i].Producer, Channels[i].VarName, Channels[i].Consumer]) + LineEnding;
    Result := Result + 'end note' + LineEnding;
    Result := Result + LineEnding;
  end;

  // ── Legenda shared variables ──────────────────────────────────────────────
  if (SharedVars <> nil) and (SharedVars.Count > 0) then
  begin
    Result := Result + 'legend right' + LineEnding;
    Result := Result + '  **Shared Variables**' + LineEnding;
    for i := 0 to SharedVars.Count - 1 do
      Result := Result + Format('  %s', [SharedVars.Keys[i]]) + LineEnding;
    Result := Result + 'end legend' + LineEnding;
    Result := Result + LineEnding;
  end;

  Result := Result + '@enduml' + LineEnding;
end;

{ ===========================================================================
  MERMAID EXPORT
  ===========================================================================

  Mermaid stateDiagram-v2 differisce da PlantUML per:
    - Nomi stati con spazi/caratteri speciali: state "Label" as id
    - Nessun [*] implicito: si usa [*] --> id per lo stato iniziale
    - Entry/exit actions: note right of id : testo  (non c'e' sintassi nativa)
    - Transizioni: id1 --> id2 : label
    - Subgraph con state id { ... }
    - I caratteri ':' nelle label non vanno escaped (Mermaid usa ':' come
      separatore della label ma accetta ':' nel testo della label)
    - Il carattere '"' va escaped come &quot; nell'HTML embed, ma nelle label
      Mermaid viene reso con backslash: "
  =========================================================================== }

{ ---------------------------------------------------------------------------
  EscapeMermaid — escape caratteri speciali per Mermaid stateDiagram-v2
  In Mermaid le label delle transizioni sono tutto cio' che segue il ':'.
  Caratteri problematici:
    - '"' → " (nelle label)
    - '[' e ']' → usati per [*], lasciare inalterati
    - '#' → va bene nelle label (non ha significato speciale in stateDiagram)
  --------------------------------------------------------------------------- }
function EscapeMermaid(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;

{ ---------------------------------------------------------------------------
  ActionListToStringMermaid — come ActionListToString ma con escape Mermaid
  --------------------------------------------------------------------------- }
function ActionListToStringMermaid(AList: TActionsList): string;
var
  Act: TXPLCAction;
  Tmp: string;
begin
  Result := '';
  if (AList = nil) or (AList.Count = 0) then Exit;

  for Act in AList do
  begin
    Tmp := '';
    case Act.ActionType of
      atSetOutput:
        Tmp := Format('Set(%s=%s)', [Act.TargetAlias, VarToStr(Act.Value)]);
      atStartTimer:
        if Integer(Act.Value) > 0 then
          Tmp := Format('StartTimer(%s,%dms)', [Act.TargetAlias, Integer(Act.Value)])
        else
          Tmp := Format('StartTimer(%s)', [Act.TargetAlias]);
      atStopTimer:
        Tmp := Format('StopTimer(%s)', [Act.TargetAlias]);
      atIncrementCounter:
        Tmp := Format('Inc(%s)', [Act.TargetAlias]);
      atDecrementCounter:
        Tmp := Format('Dec(%s)', [Act.TargetAlias]);
      atResetCounter:
        Tmp := Format('Reset(%s)', [Act.TargetAlias]);
      atLog:
        Tmp := Format('Log(%s)', [EscapeMermaid(VarToStr(Act.Value))]);
      atApplyPreset:
        Tmp := Format('Preset(%s)', [Act.TargetAlias]);
    end;
    if Tmp <> '' then
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + Tmp;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  BuildConditionLabelMermaid — etichetta condizione per una transizione
  Usa ReverseExpression (gia' definita) + EscapeMermaid.
  --------------------------------------------------------------------------- }
function BuildConditionLabelMermaid(ATrans: TXPLCTransition): string;
var
  Cond: TXPLCCondition;
  Expr: string;
begin
  Result := '';
  if (ATrans = nil) or (ATrans.Conditions = nil) or (ATrans.Conditions.Count = 0) then
  begin
    Result := 'true';
    Exit;
  end;

  for Cond in ATrans.Conditions do
  begin
    Expr := EscapeMermaid(ReverseExpression(Cond.Expression));
    if Expr = '' then Continue;
    if Result <> '' then Result := Result + ' AND ';
    Result := Result + Expr;
  end;

  if Result = '' then Result := 'true';
end;

{ ---------------------------------------------------------------------------
  GenerateSingleSMMermaid — corpo Mermaid per una singola macchina.
  APrefix: prefisso ID stati (es. 'SM0_') per evitare collisioni.
  AIndent: spazi per annidamento dentro state { }.
  --------------------------------------------------------------------------- }
function GenerateSingleSMMermaid(AController: TLogicController;
  const APrefix: string; const AIndent: string): string;
var
  SM: TXPLCStateMachine;
  States: TStatesMap;
  i, j, TargetIdx: Integer;
  State: TXPLCState;
  Trans: TXPLCTransition;
  OnEnterStr, OnExitStr: string;
  CondLabel, ActLabel, TransLabel: string;
  StateId: string;
begin
  Result := '';
  SM := AController.StateMachine;
  if SM = nil then Exit;

  States := SM.States;
  if (States = nil) or (States.Count = 0) then Exit;

  // Stato iniziale
  if SM.InitialStateName <> '' then
  begin
    TargetIdx := States.IndexOf(SM.InitialStateName);
    if TargetIdx >= 0 then
      Result := Result + AIndent + Format('[*] --> %sS%d', [APrefix, TargetIdx]) + LineEnding;
  end;
  Result := Result + LineEnding;

  // Dichiarazione stati con alias (gestisce nomi con spazi o caratteri speciali)
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    StateId := Format('%sS%d', [APrefix, i]);
    // Se il nome contiene solo caratteri safe ([A-Za-z0-9_]) si potrebbe
    // usare il nome direttamente come ID; per sicurezza usiamo sempre l'alias.
    Result := Result + AIndent + Format('state "%s" as %s', [State.StateName, StateId]) + LineEnding;
  end;
  Result := Result + LineEnding;

  // Note entry/exit: Mermaid non ha sintassi nativa entry/exit in stateDiagram-v2.
  // Usiamo una note right of per ogni stato che ha azioni.
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    StateId := Format('%sS%d', [APrefix, i]);
    OnEnterStr := ActionListToStringMermaid(State.OnEnterActions);
    OnExitStr  := ActionListToStringMermaid(State.OnExitActions);
    if (OnEnterStr = '') and (OnExitStr = '') then Continue;

    Result := Result + AIndent + Format('note right of %s', [StateId]) + LineEnding;
    if OnEnterStr <> '' then
      Result := Result + AIndent + Format('  entry: %s', [OnEnterStr]) + LineEnding;
    if OnExitStr <> '' then
      Result := Result + AIndent + Format('  exit: %s', [OnExitStr]) + LineEnding;
    Result := Result + AIndent + 'end note' + LineEnding;
  end;
  Result := Result + LineEnding;

  // Transizioni
  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    if (State.Transitions = nil) or (State.Transitions.Count = 0) then Continue;

    StateId := Format('%sS%d', [APrefix, i]);

    for j := 0 to State.Transitions.Count - 1 do
    begin
      Trans := State.Transitions[j];
      if Trans.TargetStateName = '' then Continue;

      TargetIdx := States.IndexOf(Trans.TargetStateName);
      if TargetIdx < 0 then
      begin
        LBLogger.Write(1, 'TXPLCExporter(Mermaid)', lmt_Warning,
          Format('Target state "%s" not found (from "%s" in "%s")',
          [Trans.TargetStateName, State.StateName, AController.XPLCName]));
        Continue;
      end;

      CondLabel  := BuildConditionLabelMermaid(Trans);
      ActLabel   := ActionListToStringMermaid(Trans.Actions);
      TransLabel := CondLabel;
      if ActLabel <> '' then
        TransLabel := TransLabel + ' / ' + ActLabel;

      Result := Result + AIndent +
        Format('%s --> %sS%d : %s', [StateId, APrefix, TargetIdx, TransLabel]) + LineEnding;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToMermaid
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToMermaid(AController: TLogicController;
  AIncludeLegend: Boolean): string;
var
  SharedVars: TSharedVarsMap;
  i: Integer;
begin
  Result := '';
  if AController = nil then Exit;

  Result := 'stateDiagram-v2' + LineEnding;

  if AController.XPLCName <> '' then
    Result := Result + Format('  %%{init: {"theme": "default"}}%%' + LineEnding +
                              '  title: %s', [AController.XPLCName]) + LineEnding;
  Result := Result + LineEnding;

  Result := Result + GenerateSingleSMMermaid(AController, '', '  ');

  // Legenda shared variables come commenti (Mermaid non ha legenda nativa)
  SharedVars := AController.SharedVars;
  if AIncludeLegend and (SharedVars <> nil) and (SharedVars.Count > 0) then
  begin
    Result := Result + LineEnding;
    Result := Result + '  %% Shared Variables:' + LineEnding;
    for i := 0 to SharedVars.Count - 1 do
      Result := Result + Format('  %%   %s = %s',
        [SharedVars.Keys[i], VarToStr(SharedVars.Data[i].GetValue)]) + LineEnding;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToMermaidFromFile
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToMermaidFromFile(const AFileName: string;
  AIncludeLegend: Boolean): string;
var
  Ctrl: TLogicController;
  Wheel: TTimerWheel;
begin
  Result := '';
  Wheel := TTimerWheel.Create;
  try
    Ctrl := TLogicController.Create(nil, Wheel);
    try
      if Ctrl.LoadFromXMLFile(AFileName) then
        Result := ExportToMermaid(Ctrl, AIncludeLegend)
      else
        LBLogger.Write(1, 'TXPLCExporter.ExportToMermaidFromFile', lmt_Error,
          'Failed to load: ' + AFileName);
    finally
      Ctrl.Free;
    end;
  finally
    Wheel.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportManagerToMermaid
  Genera un unico diagramma Mermaid con ogni macchina come state { } annidato.
  I canali via shared variables vengono riportati come commenti %% alla fine.
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportManagerToMermaid(AManager: TXPLCManager): string;
var
  XPLCs: TLogicControllerList;
  SharedVars: TSharedVarsMap;
  Ctrl: TLogicController;
  i, si, ti: Integer;
  Prefix, SMBody: string;
  st: TXPLCState;

  type
    TSharedVarChannel = record
      Producer: string;
      VarName: string;
      Consumer: string;
    end;

  var
    Channels: array of TSharedVarChannel;
    ChannelCount: Integer;

  procedure AddChannel(const AProd, AVar, ACons: string);
  var ci: Integer;
  begin
    for ci := 0 to ChannelCount - 1 do
      if (Channels[ci].Producer = AProd) and
         (Channels[ci].VarName  = AVar) and
         (Channels[ci].Consumer = ACons) then Exit;
    SetLength(Channels, ChannelCount + 1);
    Channels[ChannelCount].Producer := AProd;
    Channels[ChannelCount].VarName  := AVar;
    Channels[ChannelCount].Consumer := ACons;
    Inc(ChannelCount);
  end;

  function ContainsVarNameM(const AExpr, AVarName: string): Boolean;
  var
    P, VLen: Integer;
    UExpr, UVar: string;
    CB, CA: Char;
  begin
    Result := False;
    UExpr := UpperCase(AExpr);
    UVar  := UpperCase(AVarName);
    VLen  := Length(UVar);
    P     := Pos(UVar, UExpr);
    while P > 0 do
    begin
      if P > 1 then CB := UExpr[P - 1] else CB := ' ';
      if P + VLen <= Length(UExpr) then CA := UExpr[P + VLen] else CA := ' ';
      if not (CB in ['A'..'Z', '0'..'9', '_']) and
         not (CA in ['A'..'Z', '0'..'9', '_']) then
      begin
        Result := True;
        Exit;
      end;
      P := PosEx(UVar, UExpr, P + 1);
    end;
  end;

  procedure ScanActionsM(AList: TActionsList; const AMachineName: string);
  var
    Act: TXPLCAction;
    vi, k, m, ti2, ci2: Integer;
    st2: TXPLCState;
    tr: TXPLCTransition;
  begin
    if AList = nil then Exit;
    for Act in AList do
    begin
      if Act.ActionType <> atSetOutput then Continue;
      if SharedVars = nil then Continue;
      for vi := 0 to SharedVars.Count - 1 do
        if SameText(Act.TargetAlias, SharedVars.Keys[vi]) then
          for k := 0 to XPLCs.Count - 1 do
          begin
            if SameText(XPLCs[k].XPLCName, AMachineName) then Continue;
            if (XPLCs[k].StateMachine = nil) or
               (XPLCs[k].StateMachine.States = nil) then Continue;
            for m := 0 to XPLCs[k].StateMachine.States.Count - 1 do
            begin
              st2 := XPLCs[k].StateMachine.States.Data[m];
              if st2.Transitions = nil then Continue;
              for ti2 := 0 to st2.Transitions.Count - 1 do
              begin
                tr := st2.Transitions[ti2];
                if tr.Conditions = nil then Continue;
                for ci2 := 0 to tr.Conditions.Count - 1 do
                  if ContainsVarNameM(tr.Conditions[ci2].Expression, SharedVars.Keys[vi]) then
                  begin
                    AddChannel(AMachineName, SharedVars.Keys[vi], XPLCs[k].XPLCName);
                    Break;
                  end;
              end;
            end;
          end;
    end;
  end;

begin
  Result := '';
  if AManager = nil then Exit;

  XPLCs      := AManager.XPLCs;
  SharedVars := AManager.SharedVars;

  if (XPLCs = nil) or (XPLCs.Count = 0) then Exit;

  ChannelCount := 0;
  SetLength(Channels, 0);

  Result := 'stateDiagram-v2' + LineEnding;
  Result := Result + LineEnding;

  // Una state { } box per ogni macchina
  for i := 0 to XPLCs.Count - 1 do
  begin
    Ctrl := XPLCs[i];
    if (Ctrl = nil) or (Ctrl.StateMachine = nil) then Continue;
    if (Ctrl.StateMachine.States = nil) or
       (Ctrl.StateMachine.States.Count = 0) then Continue;

    Prefix := Format('SM%d_', [i]);

    // Mermaid usa state "Label" as ID { } per substates con alias
    Result := Result + Format('  state "%s" as SM%d {', [Ctrl.XPLCName, i]) + LineEnding;
    SMBody := GenerateSingleSMMermaid(Ctrl, Prefix, '    ');
    Result := Result + SMBody;
    Result := Result + '  }' + LineEnding;
    Result := Result + LineEnding;

    // Raccolta canali
    for si := 0 to Ctrl.StateMachine.States.Count - 1 do
    begin
      st := Ctrl.StateMachine.States.Data[si];
      ScanActionsM(st.OnEnterActions, Ctrl.XPLCName);
      ScanActionsM(st.OnExitActions,  Ctrl.XPLCName);
      if st.Transitions <> nil then
        for ti := 0 to st.Transitions.Count - 1 do
          ScanActionsM(st.Transitions[ti].Actions, Ctrl.XPLCName);
    end;
  end;

  // Canali di comunicazione come commenti Mermaid (%%)
  if ChannelCount > 0 then
  begin
    Result := Result + '  %% --- Canali via Shared Variables ---' + LineEnding;
    for i := 0 to ChannelCount - 1 do
      Result := Result + Format('  %%   %s --> [%s] --> %s',
        [Channels[i].Producer, Channels[i].VarName, Channels[i].Consumer]) + LineEnding;
    Result := Result + LineEnding;
  end;

  // Shared variables come commenti
  if (SharedVars <> nil) and (SharedVars.Count > 0) then
  begin
    Result := Result + '  %% --- Shared Variables ---' + LineEnding;
    for i := 0 to SharedVars.Count - 1 do
      Result := Result + Format('  %%   %s', [SharedVars.Keys[i]]) + LineEnding;
  end;
end;


{ ===========================================================================
  JSON EXPORT
  ===========================================================================

  Produce JSON strutturato per renderer dinamici (Cytoscape.js, Vis.js)
  e per il WebSocket di supervisione live.

  Utilizza fpjson (FPC standard) per la costruzione dell'albero JSON:
    - escape automatico di tutti i caratteri speciali inclusi i codici
      di controllo ASCII 0..31
    - nessun rischio di virgole mancanti o in eccesso
    - FormatJSON produce output indentato pronto per il debug

  Formato singola macchina:
  {
    "machine": "NomeMacchina",
    "initial": "StatoIniziale",
    "states": [
      { "id": "NomeStato", "onEnter": "...", "onExit": "..." }, ...
    ],
    "transitions": [
      { "from": "A", "to": "B", "label": "condizione", "actions": "..." }, ...
    ]
  }

  Formato manager (ExportManagerToJSON):
  {
    "machines": [ { ...singola macchina... }, ... ],
    "sharedVariables": [ "VarA", "VarB", ... ],
    "channels": [
      { "producer": "Macchina1", "variable": "VarA", "consumer": "Macchina2" },
      ...
    ]
  }

  Note:
  - Gli ID degli stati sono i nomi originali — stabili, definiti dall'utente.
  - "actions" nelle transizioni e' omesso se vuoto (nessuna azione on-transition).
  - "onEnter" e "onExit" negli stati sono omessi se vuoti.
  =========================================================================== }

{ ---------------------------------------------------------------------------
  ActionListToStringForJSON — converte lista azioni in stringa compatta.
  Non usa escape JSON: fpjson gestisce l'escape automaticamente quando la
  stringa viene assegnata a un TJSONString.
  --------------------------------------------------------------------------- }
function ActionListToStringForJSON(AList: TActionsList): string;
var
  Act: TXPLCAction;
  Tmp: string;
begin
  Result := '';
  if (AList = nil) or (AList.Count = 0) then Exit;

  for Act in AList do
  begin
    Tmp := '';
    case Act.ActionType of
      atSetOutput:
        Tmp := Format('Set(%s=%s)', [Act.TargetAlias, VarToStr(Act.Value)]);
      atStartTimer:
        if Integer(Act.Value) > 0 then
          Tmp := Format('StartTimer(%s,%dms)', [Act.TargetAlias, Integer(Act.Value)])
        else
          Tmp := Format('StartTimer(%s)', [Act.TargetAlias]);
      atStopTimer:
        Tmp := Format('StopTimer(%s)', [Act.TargetAlias]);
      atIncrementCounter:
        Tmp := Format('Inc(%s)', [Act.TargetAlias]);
      atDecrementCounter:
        Tmp := Format('Dec(%s)', [Act.TargetAlias]);
      atResetCounter:
        Tmp := Format('Reset(%s)', [Act.TargetAlias]);
      atLog:
        Tmp := Format('Log(%s)', [VarToStr(Act.Value)]);
      atApplyPreset:
        Tmp := Format('Preset(%s)', [Act.TargetAlias]);
    end;
    if Tmp <> '' then
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + Tmp;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  BuildConditionLabelForJSON — etichetta condizione per JSON.
  Usa ReverseExpression gia' definita; nessun escape manuale (fpjson lo fa).
  --------------------------------------------------------------------------- }
function BuildConditionLabelForJSON(ATrans: TXPLCTransition): string;
var
  Cond: TXPLCCondition;
  Expr: string;
begin
  Result := '';
  if (ATrans = nil) or (ATrans.Conditions = nil) or
     (ATrans.Conditions.Count = 0) then
  begin
    Result := 'true';
    Exit;
  end;
  for Cond in ATrans.Conditions do
  begin
    Expr := ReverseExpression(Cond.Expression);
    if Expr = '' then Continue;
    if Result <> '' then Result := Result + ' AND ';
    Result := Result + Expr;
  end;
  if Result = '' then Result := 'true';
end;

{ ---------------------------------------------------------------------------
  BuildSingleSMJSONObject — costruisce un TJSONObject per una singola
  macchina. Il chiamante e' responsabile della liberazione dell'oggetto.
  --------------------------------------------------------------------------- }
function BuildSingleSMJSONObject(AController: TLogicController): TJSONObject;
var
  SM: TXPLCStateMachine;
  States: TStatesMap;
  JStates, JTransitions: TJSONArray;
  JState, JTrans: TJSONObject;
  State: TXPLCState;
  Trans: TXPLCTransition;
  i, j: Integer;
  OnEnterStr, OnExitStr, CondLabel, ActLabel: string;
begin
  Result := TJSONObject.Create;

  SM := AController.StateMachine;
  if SM = nil then Exit;

  States := SM.States;
  if (States = nil) or (States.Count = 0) then Exit;

  Result.Strings['machine'] := AController.XPLCName;
  Result.Strings['initial'] := SM.InitialStateName;

  // ── Stati ─────────────────────────────────────────────────────────────────
  JStates := TJSONArray.Create;
  Result.Arrays['states'] := JStates;

  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    JState := TJSONObject.Create;
    JState.Strings['id'] := State.StateName;

    OnEnterStr := ActionListToStringForJSON(State.OnEnterActions);
    OnExitStr  := ActionListToStringForJSON(State.OnExitActions);
    if OnEnterStr <> '' then JState.Strings['onEnter'] := OnEnterStr;
    if OnExitStr  <> '' then JState.Strings['onExit']  := OnExitStr;

    JStates.Add(JState);
  end;

  // ── Transizioni ───────────────────────────────────────────────────────────
  JTransitions := TJSONArray.Create;
  Result.Arrays['transitions'] := JTransitions;

  for i := 0 to States.Count - 1 do
  begin
    State := States.Data[i];
    if (State.Transitions = nil) or (State.Transitions.Count = 0) then Continue;

    for j := 0 to State.Transitions.Count - 1 do
    begin
      Trans := State.Transitions[j];
      if Trans.TargetStateName = '' then Continue;

      // Avvisa se il target non esiste ma include la transizione nel JSON
      // cosi' il renderer puo' segnalare l'errore visivamente
      if States.IndexOf(Trans.TargetStateName) < 0 then
        LBLogger.Write(1, 'TXPLCExporter(JSON)', lmt_Warning,
          Format('Target state "%s" not found (from "%s" in "%s")',
          [Trans.TargetStateName, State.StateName, AController.XPLCName]));

      CondLabel := BuildConditionLabelForJSON(Trans);
      ActLabel  := ActionListToStringForJSON(Trans.Actions);

      JTrans := TJSONObject.Create;
      JTrans.Strings['from']  := State.StateName;
      JTrans.Strings['to']    := Trans.TargetStateName;
      JTrans.Strings['label'] := CondLabel;
      if ActLabel <> '' then
        JTrans.Strings['actions'] := ActLabel;

      JTransitions.Add(JTrans);
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToJSON
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToJSON(AController: TLogicController): string;
var
  JObj: TJSONObject;
begin
  Result := '';
  if AController = nil then Exit;

  JObj := BuildSingleSMJSONObject(AController);
  try
    Result := JObj.FormatJSON;
  finally
    JObj.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportToJSONFromFile
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportToJSONFromFile(const AFileName: string): string;
var
  Ctrl: TLogicController;
  Wheel: TTimerWheel;
begin
  Result := '';
  Wheel := TTimerWheel.Create;
  try
    Ctrl := TLogicController.Create(nil, Wheel);
    try
      if Ctrl.LoadFromXMLFile(AFileName) then
        Result := ExportToJSON(Ctrl)
      else
        LBLogger.Write(1, 'TXPLCExporter.ExportToJSONFromFile', lmt_Error,
          'Failed to load: ' + AFileName);
    finally
      Ctrl.Free;
    end;
  finally
    Wheel.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  TXPLCExporter.ExportManagerToJSON

  Raccolta canali via shared variables: stesso algoritmo di
  ExportManagerToPlantUML — cerca SetOutput su shared vars (produttori)
  e condizioni che le leggono (consumatori).
  --------------------------------------------------------------------------- }
class function TXPLCExporter.ExportManagerToJSON(AManager: TXPLCManager): string;
var
  XPLCs: TLogicControllerList;
  SharedVars: TSharedVarsMap;
  Ctrl: TLogicController;
  i, si, ti: Integer;
  st: TXPLCState;
  JRoot: TJSONObject;
  JMachines, JSharedVars, JChannels: TJSONArray;
  JChannel: TJSONObject;
  JMachine: TJSONObject;

  type
    TSharedVarChannel = record
      Producer: string;
      VarName:  string;
      Consumer: string;
    end;

  var
    Channels: array of TSharedVarChannel;
    ChannelCount: Integer;

  procedure AddChannel(const AProd, AVar, ACons: string);
  var ci: Integer;
  begin
    for ci := 0 to ChannelCount - 1 do
      if (Channels[ci].Producer = AProd) and
         (Channels[ci].VarName  = AVar) and
         (Channels[ci].Consumer = ACons) then Exit;
    SetLength(Channels, ChannelCount + 1);
    Channels[ChannelCount].Producer := AProd;
    Channels[ChannelCount].VarName  := AVar;
    Channels[ChannelCount].Consumer := ACons;
    Inc(ChannelCount);
  end;

  function ContainsVarNameJ(const AExpr, AVarName: string): Boolean;
  var
    P, VLen: Integer;
    UExpr, UVar: string;
    CB, CA: Char;
  begin
    Result := False;
    UExpr := UpperCase(AExpr);
    UVar  := UpperCase(AVarName);
    VLen  := Length(UVar);
    P     := Pos(UVar, UExpr);
    while P > 0 do
    begin
      if P > 1 then CB := UExpr[P - 1] else CB := ' ';
      if P + VLen <= Length(UExpr) then CA := UExpr[P + VLen] else CA := ' ';
      if not (CB in ['A'..'Z', '0'..'9', '_']) and
         not (CA in ['A'..'Z', '0'..'9', '_']) then
      begin
        Result := True;
        Exit;
      end;
      P := PosEx(UVar, UExpr, P + 1);
    end;
  end;

  procedure ScanActionsJ(AList: TActionsList; const AMachineName: string);
  var
    Act: TXPLCAction;
    vi, k, m, ti2, ci2: Integer;
    st2: TXPLCState;
    tr: TXPLCTransition;
  begin
    if AList = nil then Exit;
    for Act in AList do
    begin
      if Act.ActionType <> atSetOutput then Continue;
      if SharedVars = nil then Continue;
      for vi := 0 to SharedVars.Count - 1 do
        if SameText(Act.TargetAlias, SharedVars.Keys[vi]) then
          for k := 0 to XPLCs.Count - 1 do
          begin
            if SameText(XPLCs[k].XPLCName, AMachineName) then Continue;
            if (XPLCs[k].StateMachine = nil) or
               (XPLCs[k].StateMachine.States = nil) then Continue;
            for m := 0 to XPLCs[k].StateMachine.States.Count - 1 do
            begin
              st2 := XPLCs[k].StateMachine.States.Data[m];
              if st2.Transitions = nil then Continue;
              for ti2 := 0 to st2.Transitions.Count - 1 do
              begin
                tr := st2.Transitions[ti2];
                if tr.Conditions = nil then Continue;
                for ci2 := 0 to tr.Conditions.Count - 1 do
                  if ContainsVarNameJ(tr.Conditions[ci2].Expression, SharedVars.Keys[vi]) then
                  begin
                    AddChannel(AMachineName, SharedVars.Keys[vi], XPLCs[k].XPLCName);
                    Break;
                  end;
              end;
            end;
          end;
    end;
  end;

begin
  Result := '';
  if AManager = nil then Exit;

  XPLCs      := AManager.XPLCs;
  SharedVars := AManager.SharedVars;
  if (XPLCs = nil) or (XPLCs.Count = 0) then Exit;

  ChannelCount := 0;
  SetLength(Channels, 0);

  JRoot := TJSONObject.Create;
  try
    // ── "machines" ────────────────────────────────────────────────────────────
    JMachines := TJSONArray.Create;
    JRoot.Arrays['machines'] := JMachines;

    for i := 0 to XPLCs.Count - 1 do
    begin
      Ctrl := XPLCs[i];
      if (Ctrl = nil) or (Ctrl.StateMachine = nil) then Continue;
      if (Ctrl.StateMachine.States = nil) or
         (Ctrl.StateMachine.States.Count = 0) then Continue;

      JMachine := BuildSingleSMJSONObject(Ctrl);
      JMachines.Add(JMachine);

      // Raccolta canali per questa macchina
      for si := 0 to Ctrl.StateMachine.States.Count - 1 do
      begin
        st := Ctrl.StateMachine.States.Data[si];
        ScanActionsJ(st.OnEnterActions, Ctrl.XPLCName);
        ScanActionsJ(st.OnExitActions,  Ctrl.XPLCName);
        if st.Transitions <> nil then
          for ti := 0 to st.Transitions.Count - 1 do
            ScanActionsJ(st.Transitions[ti].Actions, Ctrl.XPLCName);
      end;
    end;

    // ── "sharedVariables" ─────────────────────────────────────────────────────
    JSharedVars := TJSONArray.Create;
    JRoot.Arrays['sharedVariables'] := JSharedVars;
    if SharedVars <> nil then
      for i := 0 to SharedVars.Count - 1 do
        JSharedVars.Add(SharedVars.Keys[i]);

    // ── "channels" ────────────────────────────────────────────────────────────
    JChannels := TJSONArray.Create;
    JRoot.Arrays['channels'] := JChannels;
    for i := 0 to ChannelCount - 1 do
    begin
      JChannel := TJSONObject.Create;
      JChannel.Strings['producer'] := Channels[i].Producer;
      JChannel.Strings['variable'] := Channels[i].VarName;
      JChannel.Strings['consumer'] := Channels[i].Consumer;
      JChannels.Add(JChannel);
    end;

    Result := JRoot.FormatJSON;
  finally
    JRoot.Free;
  end;
end;


end.
