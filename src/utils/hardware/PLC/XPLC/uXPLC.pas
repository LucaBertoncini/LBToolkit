unit uXPLC;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, uPLCBaseStructures, uEventsManager, uLBBaseThread, Variants, Laz2_DOM,
fgl, IniFiles, uPLCCollector, ULBLogger, uTimedoutCriticalSection, uExpressionEvaluator,
uTimerWheel, uXPLCTemplates;

type


  { TXPLCCounterItem }
  TXPLCCounterItem = class(TBasePLCItem)
    strict private
      FEventsManager : TEventsManager;
      FValue: Integer;

    public
      constructor Create(AId: Integer; const ACode: string; AInitialValue: Integer = 0); reintroduce;
      destructor Destroy; override;

      procedure Increment;
      procedure Decrement;
      procedure Reset;

      function ValueAsVariant: Variant; override;
      function ValueAsString: string; override;

      property EventsManager: TEventsManager read FEventsManager;
      property CounterValue: Integer read FValue;

      const
        cEvent_ValueChanged = String('cnt_ValChg');
  end;


  TLogicController = class;


  { TXPLCCondition }
  TXPLCCondition = class(TObject)
    strict private
      FExpression : String;

    public
      function Evaluate(AController: TLogicController): Boolean;
      function LoadFromXMLNode(aNode: TDOMNode): Boolean;

      property Expression: String read FExpression;
  end;



  TXPLCState = class;


  TXPLCActionType = (atSetOutput, atStartTimer, atStopTimer, atIncrementCounter, atDecrementCounter,
                     atResetCounter, atLog, atApplyPreset);

  { TXPLCAction }

  TXPLCAction = class
    strict private
      FActionType: TXPLCActionType;
      FTargetAlias: string;
      FValue: Variant;

    public
      procedure Execute(const Sender: String; AController: TLogicController);
      function LoadFromXML(aNode: TDOMNode): Boolean;

      property ActionType: TXPLCActionType read FActionType;
      property TargetAlias: string read FTargetAlias;
      property Value: Variant read FValue;
  end;



  TConditionsList    = specialize TFPGObjectList<TXPLCCondition>;
  TActionsList       = specialize TFPGObjectList<TXPLCAction>;
  TActionsListMap    = specialize TFPGMapObject<String, TActionsList>;


    { TXPLCTransition
      Compatibilità con la versione precedente (nodo <Conditions>):
        - Se il file XML contiene il nodo <Conditions> direttamente sotto <Transition>
          (formato storico), tutte le condizioni vengono trattate in AND come prima.
        - Se il file XML contiene uno o più nodi <ConditionGroup> dentro
          <ConditionGroups>, la transizione è valida se almeno un gruppo è valido
          (OR tra gruppi, AND dentro ogni gruppo).
      I due formati sono mutuamente esclusivi nella stessa transizione. }


  TXPLCTransition = class(TObject)
    strict private
      FTargetStateName: string;

      { Formato legacy: condizioni piatte in AND }
      FConditions: TConditionsList;

      FActions: TActionsList;

    public
      constructor Create;
      destructor Destroy; override;

      function LoadFromXML(aNode: TDOMNode): Boolean;
      function IsValid(AController: TLogicController): Boolean;
      procedure ExecuteActions(AController: TLogicController);

      property Conditions: TConditionsList read FConditions;
      property TargetStateName: string read FTargetStateName;
      property Actions: TActionsList read FActions;
  end;

  TTransitionsList = specialize TFPGObjectList<TXPLCTransition>;



  { TXPLCState }

  TXPLCState = class(TObject)
    strict private
      FStateName: string;

      FOnEnterActions: TActionsList;
      FOnExitActions: TActionsList;
      FTransitions: TTransitionsList;

    public
      constructor Create;
      destructor Destroy; override;

      function LoadFromXML(aNode: TDOMNode): Boolean;

      procedure Enter(AController: TLogicController);
      procedure Leave(AController: TLogicController);
      function EvaluateTransitions(AController: TLogicController): TXPLCTransition;

      property StateName: string read FStateName;
      property OnEnterActions: TActionsList read FOnEnterActions;
      property OnExitActions: TActionsList read FOnExitActions;
      property Transitions: TTransitionsList read FTransitions;
  end;


  TStatesMap = specialize TFPGMapObject<string, TXPLCState>;


  { TXPLCStateMachine }

  TXPLCStateMachine = class(TObject)
    strict private
      FStates: TStatesMap;
      FCurrentState: TXPLCState;
      FInitialStateName: string;

    public
      constructor Create;
      destructor Destroy; override;

      function LoadFromXML(aNode: TDOMNode): Boolean;
      procedure Initialize(AController: TLogicController);

      function Evaluate(AController: TLogicController): Boolean; // restituisce True se c'è stato un cambio
      function GetCurrentStateName: string;

      property States: TStatesMap read FStates;
      property InitialStateName: string read FInitialStateName;
  end;


  { TSharedVariable }

  TSharedVariable = class(TObject)
    strict private
      FVarName: string;
      FValue: Variant;

      FLock: TTimedOutCriticalSection;
      FEventsManager : TEventsManager;

    public
      constructor Create();
      destructor Destroy; override;

      procedure SetValue(AValue: Variant);
      function GetValue: Variant;

      procedure Decrement;
      procedure Increment;
      procedure Reset;

      function loadFromXMLNode(aNode: TDOMNode): Boolean;

      property VarName: string read FVarName;
      property EventsManager: TEventsManager read FEventsManager;

      const
        cEvent_ValueChanged = String('01');
  end;


  TSharedVarsMap = specialize TFPGMapObject<String, TSharedVariable>;

  { TStateMachineLogger }

  TStateMachineLogger = class(TLBLogger)
    public
      constructor Create(const aName: String); reintroduce;
  end;


  TPLCItemsList  = specialize TFPGMapObject<string, TBasePLCItem>;
  TTimersList    = specialize TFPGMapObject<string, TXPLCTimerItem>;
  TCountersList  = specialize TFPGMapObject<string, TXPLCCounterItem>;



  { TLogicController }

  TLogicController = class(TLBBaseThread)
    strict private
      FStateMachine: TXPLCStateMachine;
      FCollector: TPLCCollector; // per accedere alla mappa globale degli item

      FXPLCName : String;
      FLogger : TStateMachineLogger;

      FWheel : TTimerWheel;  // non owned: appartiene a TXPLCManager

      // Mappe alias -> item
      FInputItems: TPLCItemsList; // alias -> item di ingresso (reale o timer/counter)
      FOutputItems: TPLCItemsList; // alias -> item di uscita

      FOutputPresets : TActionsListMap;

      // Timer e contatori (sono anche loro TBasePLCItem, quindi finiscono nella stessa mappa?
      // No, per semplicità li teniamo separati, ma in realtà potrebbero stare in FInputItems.
      // Useremo mappe dedicate per cercarli velocemente.
      FTimerItems: TTimersList; // nome timer -> oggetto timer
      FCounterItems: TCountersList; // nome contatore -> oggetto contatore

      // Sincronizzazione
      FNewDataEvent: PRTLEvent; // svegliato quando un ingresso cambia
      FTerminatedEvent: PRTLEvent; // per uscire dall'attesa

      FEventsManager : TEventsManager;

      FSharedVars : TSharedVarsMap;

      FExpressionEval : TExprEvaluator;

      // Callback per eventi di cambiamento dai client
      function ExpandMessage(const AMessage: string): string;

      procedure loadIO(aParentNode: TDOMNode; isInput: Boolean); // const childNodeName: String; aList: TPLCItemsList);
      procedure loadTimers(aParentNode: TDOMNode);
      procedure loadCounters(aParentNode: TDOMNode);
      procedure loadOutputPresets(aParentNode: TDOMNode);

      procedure OnInputChanged(Sender: TObject);

      procedure CreateLogger();

    private
      procedure updateExpressionVariables();

    protected
      procedure Execute; override;

    public
      constructor Create(ACollector: TPLCCollector; AWheel: TTimerWheel); reintroduce;
      destructor Destroy; override;

      function LoadFromXMLFile(const aFileName: string): Boolean;
      function LoadFromXMLDocument(aDocument: TXMLDocument): Boolean;

      procedure Terminate; override;

      procedure registerAllVariables();
      function evaluateExpression(const anExpr: String): Boolean;

      // Metodi chiamati dalle azioni
      procedure SetOutput(const AAlias: string; const AValue: Variant);
      procedure StartTimer(const ATimerName: string; ADurationMs: Cardinal);
      procedure StopTimer(const ATimerName: string);

      procedure IncrementCounter(const ACounterName: string);
      procedure DecrementCounter(const ACounterName: string);
      procedure ResetCounter(const ACounterName: string);

      procedure LogMessage(const AState, AMessage: string);
      procedure ApplyOutputPreset(const APresetName: String);

      // Metodo chiamato dalle condizioni per ottenere il valore corrente di un input
      function GetInputValue(const AAlias: string): Variant;

      procedure setSharedVariable(aList: TSharedVarsMap);

      property StateMachine: TXPLCStateMachine read FStateMachine;
      property XPLCName: String read FXPLCName;
      property EventsManager: TEventsManager read FEventsManager;

      property SharedVars: TSharedVarsMap read FSharedVars;
  end;


  TLogicControllerList = specialize TFPGObjectList<TLogicController>;



  { TXPLCManager }

  TXPLCManager = class(TObject)
    strict private
      FPLCCollector : TPLCCollector;
      FXPLCs: TLogicControllerList;
      FTemplateEngine : TXPLCTemplateEngine;
      FSharedVars: TSharedVarsMap;
      FTimerWheel : TTimerWheel;   // unico thread timer per tutte le macchine a stati

      procedure loadStateMachines(aFile: TIniFile);
      procedure loadGlobalVariables(aFile: TIniFile);
      procedure loadTemplatesPath(aFile: TIniFile);
      procedure loadSharedVarsFromXML(const aFilename: String);

    public
      constructor Create(ACollector: TPLCCollector);
      destructor Destroy; override;

      function LoadConfigurationFromINIFile(aFile: String): Boolean; overload;
      function LoadConfigurationFromINIFile(aFile: TIniFile): Boolean; overload;

      procedure SetSharedVariable(const AName: String; AValue: Variant);
      function GetSharedVariableValueAsInteger(const AName: String): Integer;

      procedure StartStatesMachines();

      property XPLCs: TLogicControllerList read FXPLCs;
      property SharedVars: TSharedVarsMap read FSharedVars;

      const
        cINI_SECTION_StateMachines = String('StateMachines');
        cINI_KEY_GlobalVar         = String('GlobalVar');
        cINI_KEY_TemplatesPath     = String('TemplatesPath');
  end;

implementation

uses
  uLBFileUtils;


function StringToVariant(const S: string): Variant;
var
  i: Integer;
  f: Double;

begin
  // Prova a interpretare come intero
  if TryStrToInt(S, i) then Exit(i);

  // Prova come floating point
  if TryStrToFloat(S, f) then Exit(f);

  // Booleani
  if SameText(S, 'true') then Exit(True);
  if SameText(S, 'false') then Exit(False);

  // Altrimenti stringa
  Result := S;
end;

function ExpandTimeouts(const Expr: string): string;
var
  i, j: Integer;
  _timerName: string;
  _upperExpr: string;
begin
  Result := Expr;

  _upperExpr := UpperCase(Result);

  i := Pos('TIMEOUT(', _upperExpr);
  while i > 0 do
  begin
    j := i + 8; // posizione dopo '('
    while (j <= Length(Result)) and (Result[j] <> ')') do
      Inc(j);
    if j <= Length(Result) then
    begin
      _timerName := Trim(Copy(Result, i + 8, j - i - 8));
      // Sostituisce TIMEOUT(Nome) con (Nome_Timeout = 1)
      Result := Copy(Result, 1, i - 1) +
                '(' + _timerName + '_Timeout = 1)' +
                Copy(Result, j + 1, MaxInt);
    end
    else
      Break;

    _upperExpr := UpperCase(Result);

    i := Pos('TIMEOUT(', _upperExpr);
  end;
end;


function PrepareExpression(const AExpr: string): string;
begin
  Result := ExpandTimeouts(AExpr);

  // Operatori di confronto
  Result := StringReplace(Result, ' EQ ', ' = ',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ' NE ', ' <> ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ' GT ', ' > ',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ' LT ', ' < ',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ' GE ', ' >= ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ' LE ', ' <= ', [rfReplaceAll, rfIgnoreCase]);
  // Booleani in maiuscolo (richiesto dal parser)
  Result := StringReplace(Result, ' and ', ' AND ', [rfReplaceAll]);
  Result := StringReplace(Result, ' or ',  ' OR ',  [rfReplaceAll]);
  Result := StringReplace(Result, ' not ', ' NOT ', [rfReplaceAll]);
end;


{ TXPLCCounterItem }

constructor TXPLCCounterItem.Create(AId: Integer; const ACode: string; AInitialValue: Integer);
begin
  inherited Create;
  Id := AId;
  Code := ACode;
  FValue := AInitialValue;

  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_ValueChanged);
end;

destructor TXPLCCounterItem.Destroy;
begin
  FreeAndNil(FEventsManager);
  inherited Destroy;
end;

procedure TXPLCCounterItem.Increment;
begin
  Inc(FValue);
  FEventsManager.RaiseEvent(cEvent_ValueChanged);
end;

procedure TXPLCCounterItem.Decrement;
begin
  Dec(FValue);
  FEventsManager.RaiseEvent(cEvent_ValueChanged);
end;

procedure TXPLCCounterItem.Reset;
begin
  FValue := 0;
  FEventsManager.RaiseEvent(cEvent_ValueChanged);
end;

function TXPLCCounterItem.ValueAsVariant: Variant;
begin
  Result := FValue;
end;

function TXPLCCounterItem.ValueAsString: string;
begin
  Result := IntToStr(FValue);
end;



{ TXPLCCondition }

function TXPLCCondition.Evaluate(AController: TLogicController): Boolean;
begin
  Result := AController.evaluateExpression(FExpression);
end;

function TXPLCCondition.LoadFromXMLNode(aNode: TDOMNode): Boolean;
var
  _el: TDOMElement;
  _rawExpr: string;

begin

  Result := False;

  try

    _el := ANode as TDOMElement;

    // Espressione booleana (nuovo formato)
    _rawExpr := _el.AttribStrings['Expression'];
    if _rawExpr <> '' then
    begin
      FExpression := PrepareExpression(_rawExpr);
      Result := True;
    end
    else
      LBLogger.Write(1, 'TXPLCCondition.LoadFromXMLNode', lmt_Warning, 'Missing Expression attribute');

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TXPLCCondition.LoadFromXMLNode', lmt_Error, E.Message);
      Result := False;
    end;
  end;
end;



{ TXPLCAction }

procedure TXPLCAction.Execute(const Sender: String; AController: TLogicController);
begin
  if AController <> nil then
  begin
    case FActionType of
      atSetOutput        : AController.SetOutput(FTargetAlias, FValue);
      atStartTimer       : AController.StartTimer(FTargetAlias, Integer(FValue));
      atStopTimer        : AController.StopTimer(FTargetAlias);
      atIncrementCounter : AController.IncrementCounter(FTargetAlias);
      atDecrementCounter : AController.DecrementCounter(FTargetAlias);
      atResetCounter     : AController.ResetCounter(FTargetAlias);
      atLog              : AController.LogMessage(Sender, VarToStr(FValue));
      atApplyPreset      : AController.ApplyOutputPreset(FTargetAlias);
    end;
  end;
end;


function TXPLCAction.LoadFromXML(aNode: TDOMNode): Boolean;
(*
  <SetOutput Output="SemaforoRosso" Value="1"/>
  <StartTimer Timer="TimerSbarra" DurationMs="5000"/> <!-- DurationMs opzionale -->
  <StopTimer Timer="TimerSbarra"/>
  <IncrementCounter Counter="CntIngressi"/>
  <DecrementCounter Counter="CntIngressi"/>
  <ResetCounter Counter="CntIngressi"/>
  <Log Message="Ingresso nello stato X"/>

*)
var
  _el: TDOMElement;
  _nodeName: string;

begin
  if not (aNode is TDOMElement) then Exit(False);

  Result := True;

  try
    _el := ANode as TDOMElement;
    _nodeName := _el.NodeName;

    case _nodeName of

      'SetOutput':
        begin
          FActionType := atSetOutput;
          FTargetAlias := _el.AttribStrings['Output'];
          FValue := StringToVariant(_el.AttribStrings['Value']);
        end;

      'StartTimer':
        begin
          FActionType := atStartTimer;
          FTargetAlias := _el.AttribStrings['Timer'];
          // DurationMs opzionale: se assente, 0 indica "usa default"
          FValue := StrToIntDef(_el.AttribStrings['DurationMs'], 0);
        end;

      'StopTimer':
        begin
          FActionType := atStopTimer;
          FTargetAlias := _el.AttribStrings['Timer'];
          // FValue non usato
        end;

      'IncrementCounter':
        begin
          FActionType := atIncrementCounter;
          FTargetAlias := _el.AttribStrings['Counter'];
        end;

      'DecrementCounter':
        begin
          FActionType := atDecrementCounter;
          FTargetAlias := _el.AttribStrings['Counter'];
        end;

      'ResetCounter':
        begin
          FActionType := atResetCounter;
          FTargetAlias := _el.AttribStrings['Counter'];
        end;

      'Log':
        begin
          FActionType := atLog;
          FValue := _el.AttribStrings['Message'];
        end;

      'ApplyPreset':
        begin
          FActionType := atApplyPreset;
          FTargetAlias := _el.AttribStrings['Preset'];
          if FTargetAlias = '' then
          begin
            LBLogger.Write(1, 'TXPLCAction.LoadFromXML', lmt_Warning, 'ApplyPreset without Preset attribute');
            Result := False;
            Exit;
          end;
        end

      else begin
        LBLogger.Write(1, 'TXPLCAction.LoadFromXML', lmt_Warning, 'Unknown action <%s>!', [_nodeName]);
        Result := False;
      end;
    end;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TXPLCAction.LoadFromXML', lmt_Error, 'Node <%s> - %s', [aNode.NodeName, E.Message]);
      Result := False;
    end;
  end;
end;



{ TXPLCTransition }

constructor TXPLCTransition.Create;
begin
  inherited Create;

  FConditions      := TConditionsList.Create(True);
  FActions         := TActionsList.Create(True);
end;

destructor TXPLCTransition.Destroy;
begin
  FConditions.Free;
  FActions.Free;
  inherited Destroy;
end;

function TXPLCTransition.LoadFromXML(aNode: TDOMNode): Boolean;
(*
  --- Formato legacy (AND piatto, compatibilità precedente) ---
  <Transition Target="IngressoRilevato">
    <Conditions>
      <Condition Input="Camera01.Vehicle" Op="EQ" Value="1"/>
    </Conditions>
  </Transition>

  --- Formato esteso con ConditionGroups (OR tra gruppi, AND dentro ogni gruppo) ---
  <Transition Target="Apertura">
    <ConditionGroups>
      <!-- Gruppo 1: veicolo E timer scaduto -->
      <ConditionGroup>
        <Condition Input="Camera01.Vehicle" Op="EQ" Value="1"/>
        <Condition Input="TimerIngresso.Timeout" Op="EQ" Value="1"/>
      </ConditionGroup>
      <!-- Gruppo 2: pulsante manuale -->
      <ConditionGroup>
        <Condition Input="PulsanteManuale" Op="EQ" Value="1"/>
      </ConditionGroup>
    </ConditionGroups>
  </Transition>

  I due formati sono mutuamente esclusivi nella stessa transizione.
  Il nodo <Actions> è opzionale in entrambi i formati.
*)
var
  _targetEl: TDOMElement;
  _condNode, _actNode, _item: TDOMNode;
  _cond: TXPLCCondition;
  _action: TXPLCAction;
  i: Integer;
begin
  Result := False;
  if not (aNode is TDOMElement) then Exit;

  _targetEl := TDOMElement(aNode);
  FTargetStateName := _targetEl.AttribStrings['Target'];
  if FTargetStateName = '' then Exit;

  _condNode := aNode.FindNode('Conditions');
  if _condNode <> nil then
  begin
    for i := 0 to _condNode.ChildNodes.Count - 1 do
    begin
      _item := _condNode.ChildNodes.Item[i];
      if _item.NodeName = 'Condition' then
      begin
        _cond := TXPLCCondition.Create;
        if _cond.LoadFromXMLNode(_item) then
          FConditions.Add(_cond)
        else
          _cond.Free;
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TXPLCTransition.LoadFromXML', lmt_Warning,
      'No <Conditions> or <ConditionGroups> found for target <%s>', [FTargetStateName]);

  // --- Nodo <Actions> opzionale (comune a entrambi i formati) ---
  _actNode := aNode.FindNode('Actions');
  if _actNode <> nil then
  begin
    for i := 0 to _actNode.ChildNodes.Count - 1 do
    begin
      _item := _actNode.ChildNodes.Item[i];
      _action := TXPLCAction.Create;
      if _action.LoadFromXML(_item) then
        FActions.Add(_action)
      else
        _action.Free;
    end;
  end;

  Result := FConditions.Count > 0;
end;

function TXPLCTransition.IsValid(AController: TLogicController): Boolean;
var
  _cond: TXPLCCondition;
begin
  Result := True;

  try
    if (FConditions <> nil) and (FConditions.Count > 0) then
    begin
      for _cond in FConditions do
      begin
        if not _cond.Evaluate(AController) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCTransition.IsValid', lmt_Error, E.Message);
  end;
end;
(*
function TXPLCTransition.IsValid(AController: TLogicController): Boolean;
var
  _cond: TXPLCCondition;
  _currentValue: Variant;
begin
  Result := True;
  for _cond in FConditions do
  begin

    _currentValue := AController.GetInputValue(_cond.InputAlias);
    if not _cond.Evaluate(_currentValue, AController) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;
*)

procedure TXPLCTransition.ExecuteActions(AController: TLogicController);
var
  _action: TXPLCAction;
begin
  if (FActions <> nil) and (FActions.Count > 0) then
  begin
    for _action in FActions do
      _action.Execute('>> ' + FTargetStateName, AController);
  end;
end;



{ TXPLCState }

constructor TXPLCState.Create;
begin
  inherited Create;

  FOnEnterActions := TActionsList.Create(True);
  FOnExitActions := TActionsList.Create(True);
  FTransitions := TTransitionsList.Create(True);
end;

destructor TXPLCState.Destroy;
begin
  FOnEnterActions.Free;
  FOnExitActions.Free;
  FTransitions.Free;

  inherited Destroy;
end;

function TXPLCState.LoadFromXML(aNode: TDOMNode): Boolean;
var
  _el: TDOMElement;
  _child, _subNode: TDOMNode;
  _action: TXPLCAction;
  _transition: TXPLCTransition;
  i : Integer;

begin
  Result := False;

  if not (aNode is TDOMElement) then Exit;

  _el := TDOMElement(aNode);
  FStateName := _el.AttribStrings['Name'];
  if FStateName = '' then
  begin
    LBLogger.Write(1, 'TXPLCState.LoadFromXML', lmt_Warning, 'State name needed!');
    Exit;
  end;

  _child := aNode.FindNode('OnEnter');
  if _child <> nil then
  begin
    for i := 0 to _child.ChildNodes.Count - 1 do
    begin
      _action := TXPLCAction.Create;
      if _action.LoadFromXML(_child.ChildNodes.Item[i]) then
      FOnEnterActions.Add(_action)
      else
      _action.Free;
    end;
  end;

  _child := aNode.FindNode('OnExit');
  if _child <> nil then
  begin
    for i := 0 to _child.ChildNodes.Count - 1 do
    begin
      _action := TXPLCAction.Create;
      if _action.LoadFromXML(_child.ChildNodes.Item[i]) then
        FOnExitActions.Add(_action)
      else
        _action.Free;
    end;
  end;

  _child := aNode.FindNode('Transitions');
  if _child <> nil then
  begin
    for i := 0 to _child.ChildNodes.Count - 1 do
    begin
      _transition := TXPLCTransition.Create;
      if _transition.LoadFromXML(_child.ChildNodes.Item[i]) then
      FTransitions.Add(_transition)
      else
      _transition.Free;
    end;
  end;

  Result := FTransitions.Count > 0;  // Ogni stato deve avere almeno una transizione verso un altro stato
  if not Result then
    LBLogger.Write(1, 'TXPLCState.LoadFromXML', lmt_Warning, 'No transition for state <%s>!', [FStateName]);
end;

procedure TXPLCState.Enter(AController: TLogicController);
var
  _action: TXPLCAction;

begin
  for _action in FOnEnterActions do
    _action.Execute(Self.StateName, AController);
end;

procedure TXPLCState.Leave(AController: TLogicController);
var
  _action: TXPLCAction;

begin
  for _action in FOnExitActions do
    _action.Execute(Self.StateName, AController);
end;

function TXPLCState.EvaluateTransitions(AController: TLogicController): TXPLCTransition;
var
  _trans: TXPLCTransition;

begin
  Result := nil;

  for _trans in FTransitions do
  begin
    if _trans.IsValid(AController) then
      Exit(_trans);
  end;

end;



{ TXPLCStateMachine }

constructor TXPLCStateMachine.Create;
begin
  inherited Create;

  FStates := TStatesMap.Create(True);
  FStates.Sorted := True;

  FCurrentState := nil;
end;

destructor TXPLCStateMachine.Destroy;
begin
  FStates.Free;
  inherited Destroy;
end;

function TXPLCStateMachine.LoadFromXML(aNode: TDOMNode): Boolean;
var
  _smNode, _stateNode: TDOMNode;
  _state: TXPLCState;
  _initAttr: string;
  i : Integer;

const
  cStateMachineNodeName = String('StateMachine');
  cStateMachineInitialStateAttrib = String('Initial');

begin
  Result := False;
  if not (aNode is TDOMElement) then Exit;

  try
    // Cerca il nodo <StateMachine>
    _smNode := aNode.FindNode(cStateMachineNodeName);
    if _smNode = nil then
    begin
      LBLogger.Write(1, 'XPLCStateMachine.LoadFromXML', lmt_Warning, 'Needed node <%s> not found!', [cStateMachineNodeName]);
      Exit;
    end;

    FInitialStateName := TDOMElement(_smNode).AttribStrings[cStateMachineInitialStateAttrib];
    if FInitialStateName = '' then
    begin
      LBLogger.Write(1, 'XPLCStateMachine.LoadFromXML', lmt_Warning, 'Intial state needed!');
      Exit;
    end;

    // Per ogni figlio <State>
    for i := 0 to _smNode.ChildNodes.Count - 1 do
    begin
      _stateNode := _smNode.ChildNodes.Item[i];
      if _stateNode.NodeName = 'State' then
      begin
        _state := TXPLCState.Create;
        if _state.LoadFromXML(_stateNode) then
          FStates.Add(_state.StateName, _state)
        else
          _state.Free;
      end;
//      else
//        LBLogger.Write(1, 'XPLCStateMachine.LoadFromXML', lmt_Warning, 'Invalid state <%s>', [_stateNode.NodeName]);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'XPLCStateMachine.LoadFromXML', lmt_Error, E.Message);
  end;

  Result := FStates.Count > 0;
  if Result then
    LBLogger.Write(5, 'XPLCStateMachine.LoadFromXML', lmt_Debug, 'State machine loaded');

end;

procedure TXPLCStateMachine.Initialize(AController: TLogicController);
begin
  FCurrentState := nil;

  if FInitialStateName <> '' then
  begin
    if FStates.TryGetData(FInitialStateName, FCurrentState) then
      FCurrentState.Enter(AController)
    else
      LBLogger.Write(1, 'TXPLCStateMachine.Initialize', lmt_Warning, 'Initial state <%s> not found!', [FInitialStateName]);
  end;
end;

function TXPLCStateMachine.Evaluate(AController: TLogicController): Boolean;
var
  _transition: TXPLCTransition;
  _nextState: TXPLCState;

begin
  Result := False;
  if FCurrentState = nil then Exit;

  AController.updateExpressionVariables();

  // Recupera l'eventaule transizione legata allo stao degli ingressi
  _transition := FCurrentState.EvaluateTransitions(AController);
  if _transition = nil then Exit;

  // Esegui le azioni della transizione (prima di eventuali cambi di stato)
  _transition.ExecuteActions(AController);

  // Trovata transizione valida
  _nextState := FStates.KeyData[_transition.TargetStateName];
  if _nextState <> nil then
  begin
    FCurrentState.Leave(AController);
    FCurrentState := _nextState;
    FCurrentState.Enter(AController);

    Result := True;
  end
  else begin
    // Stato di destinazione non trovato: rimane nello stato corrente
    LBLogger.Write(1, 'TXPLCStateMachine.Evaluate', lmt_Warning, 'Target state <%s> not found!', [_transition.TargetStateName]);
  end;
end;

function TXPLCStateMachine.GetCurrentStateName: string;
begin
  if FCurrentState <> nil then
    Result := FCurrentState.StateName
  else
    Result := '';
end;

{ TSharedVariable }

constructor TSharedVariable.Create;
begin
  inherited Create;

  FVarName := '';
  FValue := Null;

  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_ValueChanged);

  FLock := TTimedOutCriticalSection.Create;
end;

destructor TSharedVariable.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FEventsManager);

  inherited;
end;

procedure TSharedVariable.SetValue(AValue: Variant);
begin
  if FLock.Acquire('TSharedVariable.SetValue') then
  begin
    try
      if AValue <> FValue then
      begin
        FValue := AValue;
        FEventsManager.RaiseEvent(cEvent_ValueChanged);
      end;
    finally
      FLock.Release();
    end;
  end;
end;

function TSharedVariable.GetValue: Variant;
begin
  Result := Null;

  if FLock.Acquire('TSharedVariable.GetValue') then
  begin
    try
      Result := FValue;
    finally
      FLock.Release();
    end;
  end;
end;

function TSharedVariable.loadFromXMLNode(aNode: TDOMNode): Boolean;
begin
  Result := False;

  if aNode.NodeName = 'Var' then
  begin
    FVarName := TDOMElement(aNode).AttribStrings['Name'];
    if FVarName <> '' then
    begin
      FValue := StringToVariant(TDOMElement(aNode).AttribStrings['InitialValue']);
      Result := True;
    end;
  end
  else
    LBLogger.Write(1, 'TSharedVariable.loadFromXMLNode', lmt_Warning, 'Wrong node name <%s>', [aNode.NodeName]);
end;

procedure TSharedVariable.Increment;
begin
  if FLock.Acquire('TSharedVariable.Increment') then
  begin
    try
      if VarIsNumeric(FValue) then
        FValue := FValue + 1
      else
        FValue := 1;  // inizializza a 1 se non numerica
      FEventsManager.RaiseEvent(cEvent_ValueChanged);
    finally
      FLock.Release;
    end;
  end;
end;

procedure TSharedVariable.Decrement;
begin
  if FLock.Acquire('TSharedVariable.Decrement') then
  begin
    try
      if VarIsNumeric(FValue) then
        FValue := FValue - 1
      else
        FValue := -1;
      FEventsManager.RaiseEvent(cEvent_ValueChanged);
    finally
      FLock.Release;
    end;
  end;
end;

procedure TSharedVariable.Reset;
begin
  if FLock.Acquire('TSharedVariable.Reset') then
  begin
    try
      FValue := 0;
      FEventsManager.RaiseEvent(cEvent_ValueChanged);
    finally
      FLock.Release;
    end;
  end;
end;


{ TStateMachineLogger }

constructor TStateMachineLogger.Create(const aName: String);
begin
  inherited Create(aName, False);

  FMessageFormat := '%s | %-15s | %s ';
  SetLength(FMessageFields, 3);
  FMessageFields[0] := lfDateTime;
  FMessageFields[1] := lfSource;
  FMessageFields[2] := lfMessage;
end;

{ TLogicController }

constructor TLogicController.Create(ACollector: TPLCCollector; AWheel: TTimerWheel);
var
  i : Integer;

begin
  inherited Create;

  FXPLCName := '';
  FLogger   := nil;
  FWheel    := AWheel;   // non owned

  FEventsManager := TEventsManager.Create(Self);
  FExpressionEval := TExprEvaluator.Create;

  FCollector := ACollector;

  FInputItems := TPLCItemsList.Create(False); // Oggetti esterni prelevati da FCollector o riferimenti a timer
  FOutputItems := TPLCItemsList.Create(False); // Oggetti esterni prelevati da FCollector
  FTimerItems := TTimersList.Create(True);
  FCounterItems := TCountersList.Create(True);

  FStateMachine := TXPLCStateMachine.Create;

  FOutputPresets := TActionsListMap.Create(True);

  FNewDataEvent := RTLEventCreate;
  FTerminatedEvent := RTLEventCreate;
end;

destructor TLogicController.Destroy;
begin
  try
    LBLogger.Write(5, 'TLogicController.Destroy', lmt_Debug, 'Destroying <%s>', [FXPLCName]);

    // 1. Sblocca il thread e aspetta che Execute termini prima di distruggere
    //    qualsiasi oggetto da cui dipende (timers, state machine, ecc.)
    RTLEventSetEvent(FNewDataEvent);
    inherited Destroy;   // Terminate → Terminated=True, poi WaitFor

    // 2. Execute è terminato: nessuno accede più a questi oggetti
    FreeAndNil(FEventsManager);
    FreeAndNil(FStateMachine);   // StopTimer su tutti i TXPLCTimerItem
    FreeAndNil(FInputItems);
    FreeAndNil(FOutputItems);
    FreeAndNil(FTimerItems);
    FreeAndNil(FCounterItems);

    FreeAndNil(FOutputPresets);

    FreeAndNil(FExpressionEval);

    if FLogger <> nil then
      FreeAndNil(FLogger);

    RTLEventDestroy(FNewDataEvent);
    RTLEventDestroy(FTerminatedEvent);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.Destroy', lmt_Error, E.Message);
  end;
end;

function TLogicController.LoadFromXMLFile(const aFileName: string): Boolean;
var
  _doc: TXMLDocument = nil;

begin
  Result := False;

  FInputItems.Clear;
  FOutputItems.Clear;
  FTimerItems.Clear;
  FCounterItems.Clear;

  if OpenXMLFile(aFileName, _doc) then
    Result := Self.LoadFromXMLDocument(_doc);

  if _doc <> nil then
  _doc.Free;
end;

function TLogicController.LoadFromXMLDocument(aDocument: TXMLDocument): Boolean;
var
  _root, _node, _child: TDOMNode;
  _alias, _aliasId: string;
  _id: Integer;
  _item: TBasePLCItem;
  _timer: TXPLCTimerItem;
  _counter: TXPLCCounterItem;
  _duration: Cardinal;
  _initVal: Integer;
  i, j : Integer;
  _Client : TPLCBaseClient;

begin
  Result := False;

  FInputItems.Clear;
  FOutputItems.Clear;
  FTimerItems.Clear;
  FCounterItems.Clear;

  if aDocument = nil then Exit;

  try
    _root := aDocument.DocumentElement;
    if _root <> nil then
    begin
      FXPLCName := TDOMElement(_root).AttribStrings['Code'];

      if FXPLCName <> '' then
        Self.setThreadName(FXPLCName);

      // --- 1. Inputs ---
      Self.loadIO(_root.FindNode('Inputs'), True);

      // --- 2. Outputs ---
      Self.loadIO(_root.FindNode('Outputs'), False);

      // --- 3. Output Presets ---
      Self.loadOutputPresets(_root.FindNode('OutputPresets'));

      // --- 4. Timers ---
      Self.loadTimers(_root.FindNode('Timers'));

      // --- 5. Counters ---
      Self.loadCounters(_root.FindNode('Counters'));

      // --- 6. StateMachine ---
      if FStateMachine.LoadFromXML(_root) then
      begin
        // Sottoscrizione agli eventi di cambiamento dei client
        if FCollector <> nil then
        begin
          LBLogger.Write(5, 'TLogicController.LoadFromXMLDocument', lmt_Debug, '<%s> - Registering to items changes ...', [FXPLCName]);
          for i := 0 to FCollector.PLCTypes.Count - 1 do
          begin
            for j := 0 to FCollector.PLCTypes[i].Count - 1 do
            begin
              _Client := FCollector.PLCTypes[i].getClient(j);
              LBLogger.Write(5, 'TLogicController.LoadFromXMLDocument', lmt_Debug, '<%s> - Resgistering to <%s> for changes', [FXPLCName, _Client.Code]);
              _Client.EventsManager.AddEventListener(TPLCBaseClient.cEvent_NewValuesRead, @Self.OnInputChanged, FEventsManager);
            end;
          end;

          LBLogger.Write(5, 'TLogicController.LoadFromXMLDocument', lmt_Debug, '<%s> - Timers: %d - Counters: %d - Total inputs: %d - Total outputs: %d', [FXPLCName, FTimerItems.Count, FCounterItems.Count, FInputItems.Count, FOutputItems.Count]);
        end;

        Result := True;
      end
      else
        LBLogger.Write(1, 'TLogicController.LoadFromXMLDocument', lmt_Error, 'Failed to load state machine from XML');

    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.LoadFromXMLDocument', lmt_Error, 'Error: %s', [E.Message]);
  end;
end;

procedure TLogicController.Terminate;
begin
  inherited Terminate;
  Self.OnInputChanged(nil);
end;

procedure TLogicController.OnInputChanged(Sender: TObject);
begin
//  if Sender is TPLCBaseClient then
//    LBLogger.Write(5, 'TLogicController.OnInputChanged', lmt_Debug, '<%s>  -  Received changes from <%s>', [FXPLCName, TPLCBaseClient(Sender).Code]);
//  else
//    LBLogger.Write(5, 'TLogicController.OnInputChanged', lmt_Debug, '<%s>  -  Received changes', [FXPLCName]);
//  if Sender is TXPLCTimerItem then
//    LBLogger.Write(5, 'TLogicController.OnInputChanged', lmt_Debug, '<%s>  -  Timer <%s>', [FXPLCName, TXPLCTimerItem(Sender).Code]);

  RTLEventSetEvent(FNewDataEvent);
end;

procedure TLogicController.CreateLogger();
var
  _Filename : String;

begin
  if (FLogger = nil) and (FXPLCName <> '') then
  begin
    // Crea il logger per questa macchina
    _Filename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'logs' + PathDelim + FXPLCName + '.log';
    FLogger := TStateMachineLogger.Create('Log_' + FXPLCName);
    FLogger.MaxLogLevel := 1;
    FLogger.setLogFile(PChar(_Filename));
  end;
end;

procedure TLogicController.loadIO(aParentNode: TDOMNode; isInput: Boolean);
var
  _child : TDOMNode;
  _alias : String;
  _id : Integer;
  _item: TBasePLCItem;

  _idx : Integer;

  _List : TPLCItemsList;
  _childNodeName : String;

  i : Integer;

begin
  try
    if aParentNode <> nil then
    begin
      if FCollector <> nil then
      begin
        if isInput then
        begin
          _List := FInputItems;
          _childNodeName := 'Input';
        end
        else begin
          _List := FOutputItems;
          _childNodeName := 'Output';
        end;

        for i := 0 to aParentNode.ChildNodes.Count - 1 do
        begin
          _child := aParentNode.ChildNodes.Item[i];
          if _child.NodeType <> ELEMENT_NODE then
            Continue;

          if _child.NodeName = _childNodeName then
          begin
            _alias := TDOMElement(_child).AttribStrings['Alias'];
            _id := StrToIntDef(TDOMElement(_child).AttribStrings['Id'], 0);
            if (_alias <> '') and (_id > 0) then
            begin
              _idx := FCollector.SortedMonitoredItems.IndexOf(_id);
              if _idx >= 0 then
              begin
                _item := FCollector.SortedMonitoredItems.Data[_idx];
                if _item <> nil then
                  _List.Add(_alias, _item)
                else
                  LBLogger.Write(1, 'TLogicController.loadIO', lmt_Warning, '<%s> - %s <%s> id=%d not found in collector', [FXPLCName, _childNodeName, _alias, _id]);
              end
              else
                LBLogger.Write(1, 'TLogicController.loadIO', lmt_Warning, '<%s> - %s <%s> id=%d not found in collector', [FXPLCName, _childNodeName, _alias, _id]);

            end;
          end
          else
            LBLogger.Write(1, 'TLogicController.loadIO', lmt_Warning, '<%s> - <%s> is not an %s node', [FXPLCName, _child.NodeName, _childNodeName]);
        end;
      end
      else
        LBLogger.Write(1, 'TLogicController.loadIO', lmt_Warning, '<%s> - no IO collector!', [FXPLCName]);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.loadIO', lmt_Error, E.Message);
  end;
end;

procedure TLogicController.loadTimers(aParentNode: TDOMNode);
var
  i : Integer;
  _child : TDOMNode;
  _alias : String;
  _duration : Integer;
  _timer : TXPLCTimerItem;

begin
  try
    if aParentNode <> nil then
    begin
      for i := 0 to aParentNode.ChildNodes.Count - 1 do
      begin
        _child := aParentNode.ChildNodes.Item[i];
        if _child.NodeName = 'Timer' then
        begin
          _alias := TDOMElement(_child).AttribStrings['Name'];
          _duration := StrToIntDef(TDOMElement(_child).AttribStrings['DefaultMs'], 0);
          if _alias <> '' then
          begin
            _timer := TXPLCTimerItem.Create(FWheel, FXPLCName, -1, _alias, _duration);
            _timer.EventsManager.AddEventListener(TXPLCTimerItem.cEvent_StatusChanged, @Self.OnInputChanged, FEventsManager);
            FTimerItems.Add(_alias, _timer);
            // Aggiungiamo anche a FInputItems per le condizioni TimerName.Timeout
            FInputItems.Add(_alias + '_Timeout', _timer);
          end;
        end;
//        else
//          LBLogger.Write(1, 'TLogicController.loadTimers', lmt_Warning, '<%s> - <%s> is not a timer node', [FXPLCName, _child.NodeName]);
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.loadTimers', lmt_Error, E.Message);
  end;
end;

procedure TLogicController.loadCounters(aParentNode: TDOMNode);
var
  i : Integer;
  _child : TDOMNode;
  _alias : String;
  _initVal : Integer;
  _counter : TXPLCCounterItem;

begin
  try
    if aParentNode <> nil then
    begin
      for i := 0 to aParentNode.ChildNodes.Count - 1 do
      begin
        _child := aParentNode.ChildNodes.Item[i];
        if _child.NodeName = 'Counter' then
        begin
          _alias := TDOMElement(_child).AttribStrings['Name'];
          _initVal := StrToIntDef(TDOMElement(_child).AttribStrings['InitialValue'], 0);
          if _alias <> '' then
          begin
            _counter := TXPLCCounterItem.Create(-1, _alias, _initVal);
            _counter.EventsManager.AddEventListener(TXPLCCounterItem.cEvent_ValueChanged, @Self.OnInputChanged, FEventsManager);
            FCounterItems.Add(_alias, _counter);
            FInputItems.Add(_alias, _counter);
          end;
        end
        else
          LBLogger.Write(1, 'TLogicController.loadCounters', lmt_Warning, '<%s> - <%s> is not a counter node!', [FXPLCName, _child.NodeName]);
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.loadCounters', lmt_Error, E.Message);
  end;
end;

procedure TLogicController.loadOutputPresets(aParentNode: TDOMNode);
var
  i, j : Integer;
  _child : TDOMNode;
  _actChild : TDOMNode;
  _alias : String;
  _actList : TActionsList;
  _action : TXPLCAction;

begin
  try
    if aParentNode <> nil then
    begin
      for i := 0 to aParentNode.ChildNodes.Count - 1 do
      begin
        _child := aParentNode.ChildNodes.Item[i];
        if _child.NodeName = 'Preset' then
        begin
          _alias := TDOMElement(_child).AttribStrings['Name'];
          if _alias <> '' then
          begin
            _actList := TActionsList.Create(True);
            // Carica tutte le azioni figlie del nodo <Preset>
            for j := 0 to _child.ChildNodes.Count - 1 do
            begin
              _actChild := _child.ChildNodes.Item[j];
              _action := TXPLCAction.Create;
              if _action.LoadFromXML(_actChild) then
              begin
                // Accettiamo solo SetOutput per i preset (per ora)
                if _action.ActionType = atSetOutput then
                  _actList.Add(_action)
                else
                begin
                  LBLogger.Write(1, 'TLogicController.loadOutputPresets', lmt_Warning,
                                 'Preset <%s> contains unsupported action <%s>, ignoring', [_alias, _actChild.NodeName]);
                  _action.Free;
                end;
              end
              else
                _action.Free;
            end;
            if _actList.Count > 0 then
              FOutputPresets.Add(_alias, _actList)
            else begin
              LBLogger.Write(1, 'TLogicController.loadOutputPresets', lmt_Warning, 'Preset <%s> has no valid actions, skipping', [_alias]);
              _actList.Free;
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLogicController.loadOutputPresets', lmt_Error, E.Message);
  end;
end;

procedure TLogicController.registerAllVariables();
var
  i: Integer;
  _alias: string;

begin
  // Input, timer, counter
  for i := 0 to FInputItems.Count - 1 do
  begin
    _alias := FInputItems.Keys[i];
    FExpressionEval.AddVariable(_alias);
  end;

  // Shared vars
  if FSharedVars <> nil then
  begin
    for i := 0 to FSharedVars.Count - 1 do
    begin
      _alias := FSharedVars.Keys[i];
      FExpressionEval.AddVariable(_alias);
    end;
  end;
end;

procedure TLogicController.updateExpressionVariables();
var
  i: Integer;
  _alias: string;

begin
  for i := 0 to FInputItems.Count - 1 do
  begin
    _alias := FInputItems.Keys[i];
//    LBLogger.Write(5, 'TLogicController.updateExpressionVariables', lmt_Debug, 'Input <%s>  -  Value <%s>', [_alias, VarToStr(Self.GetInputValue(_alias))]);
    FExpressionEval.SetVariableValue(_alias, Self.GetInputValue(_alias));
  end;

  if FSharedVars <> nil then
  begin
    for i := 0 to FSharedVars.Count - 1 do
    begin
      _alias := FSharedVars.Keys[i];
      FExpressionEval.SetVariableValue(_alias, Self.GetInputValue(_alias));
    end;
  end;
end;

function TLogicController.evaluateExpression(const anExpr: String): Boolean;
begin
//  Self.updateExpressionVariables();
  Result := FExpressionEval.Evaluate(anExpr);
end;

procedure TLogicController.Execute;
var
  _oldState, _newState: string;
  _stateChanged: Boolean;
begin
  LBLogger.Write(5, 'TLogicController.Execute', lmt_Debug, 'Initializing state machine <%s> ...', [FXPLCName]);
  FStateMachine.Initialize(Self);

  while not Self.Terminated do
  begin
    // Attesa evento o timeout
    RTLEventWaitFor(FNewDataEvent, 5000);
    if Self.Terminated then Break;


    try
      // LBLogger.Write(5, 'TLogicController.Execute', lmt_Debug, '<%s> - Verifing inputs ...', [FXPLCName]);
      // Valuta finché ci sono transizioni che scattano
      repeat
        _oldState := FStateMachine.GetCurrentStateName;
        // LBLogger.Write(5, 'TLogicController.Execute', lmt_Debug, '<%s> - Current state: <%s>', [FXPLCName, _oldState]);
        _stateChanged := FStateMachine.Evaluate(Self);
        if _stateChanged then
        begin
          _newState := FStateMachine.GetCurrentStateName;
          // LBLogger.Write(5, 'TLogicController.Execute', lmt_Info, '<%s> - State change: [%s] → [%s]', [FXPLCName, _oldState, _newState]);
        end
        else
          Break;
      until Self.Terminated;

    except
      on E: Exception do
        LBLogger.Write(1, 'TLogicController.Execute', lmt_Error, 'Evaluation error: %s', [E.Message]);
    end;
  end;

  LBLogger.Write(5, 'TLogicController.Execute', lmt_Debug, 'XPLC terminated');
end;

procedure TLogicController.SetOutput(const AAlias: string; const AValue: Variant);
var
  _item: TBasePLCItem;
  _var: TSharedVariable;
begin
  if FOutputItems.TryGetData(AAlias, _item) then
  begin
    try
      _item.WriteBoolean(AValue);
      // LBLogger.Write(5, 'TLogicController.SetOutput', lmt_Info, 'Output <%s> set to %s', [AAlias, VarToStr(AValue)]);
    except
      on E: Exception do
        LBLogger.Write(1, 'TLogicController.SetOutput', lmt_Error, 'Error setting output <%s>: %s', [AAlias, E.Message]);
    end;
  end
  else if (FSharedVars <> nil) and FSharedVars.TryGetData(AAlias, _var) then
  begin
    _var.SetValue(AValue);
    // LBLogger.Write(5, 'TLogicController.SetOutput', lmt_Info, 'Shared variable <%s> set to %s', [AAlias, VarToStr(AValue)]);
  end
  else
    LBLogger.Write(1, 'TLogicController.SetOutput', lmt_Warning, 'Output / shared variable <%s> not found', [AAlias]);
end;


procedure TLogicController.StartTimer(const ATimerName: string; ADurationMs: Cardinal);
var
  _timer: TXPLCTimerItem;
begin
  if FTimerItems.TryGetData(ATimerName, _timer) then
  begin
    _timer.StartTimer(ADurationMs);
    // LBLogger.Write(5, 'TLogicController.StartTimer', lmt_Info, '<%s>  -  Timer <%s> started (duration %d ms)', [FXPLCName, ATimerName, _timer.DurationMs]);
  end
  else
    LBLogger.Write(1, 'TLogicController.StartTimer', lmt_Warning, '<%s>  -  Timer <%s> not found', [FXPLCName, ATimerName]);
end;

procedure TLogicController.StopTimer(const ATimerName: string);
var
  _timer: TXPLCTimerItem;
begin
  if FTimerItems.TryGetData(ATimerName, _timer) then
  begin
    _timer.StopTimer;
    // LBLogger.Write(5, 'TLogicController.StopTimer', lmt_Info, 'Timer <%s> stopped', [ATimerName]);
  end
  else
    LBLogger.Write(1, 'TLogicController.StopTimer', lmt_Warning, 'Timer <%s> not found', [ATimerName]);
end;

procedure TLogicController.IncrementCounter(const ACounterName: string);
var
  _ctr: TXPLCCounterItem;
  _var: TSharedVariable;
begin
  if FCounterItems.TryGetData(ACounterName, _ctr) then
    _ctr.Increment
  else if (FSharedVars <> nil) and FSharedVars.TryGetData(ACounterName, _var) then
    _var.Increment
  else
    LBLogger.Write(1, 'TLogicController.IncrementCounter', lmt_Warning, 'Counter <%s> not found', [ACounterName]);
end;

procedure TLogicController.DecrementCounter(const ACounterName: string);
var
  _ctr: TXPLCCounterItem;
  _var: TSharedVariable;
begin
  if FCounterItems.TryGetData(ACounterName, _ctr) then
    _ctr.Decrement
  else if (FSharedVars <> nil) and FSharedVars.TryGetData(ACounterName, _var) then
    _var.Decrement
  else
    LBLogger.Write(1, 'TLogicController.DecrementCounter', lmt_Warning, 'Counter <%s> not found', [ACounterName]);
end;

procedure TLogicController.ResetCounter(const ACounterName: string);
var
  _ctr: TXPLCCounterItem;
  _var: TSharedVariable;
begin
  if FCounterItems.TryGetData(ACounterName, _ctr) then
    _ctr.Reset
  else if (FSharedVars <> nil) and FSharedVars.TryGetData(ACounterName, _var) then
    _var.Reset
  else
    LBLogger.Write(1, 'TLogicController.ResetCounter', lmt_Warning, 'Counter <%s> not found', [ACounterName]);
end;

function TLogicController.ExpandMessage(const AMessage: string): string;
var
  i: Integer;
  _alias: string;
  _value: Variant;
  _start, _stop: Integer;

begin
  Result := AMessage;
  _start := Pos('%', Result);
  while _start > 0 do
  begin
    _stop := Pos('%', Result, _start + 1);
    if _stop = 0 then Break;

    _alias := Copy(Result, _start + 1, _stop - _start - 1);
    _value := GetInputValue(_alias);
    Delete(Result, _start, _stop - _start + 1);
    Insert(VarToStr(_value), Result, _start);
    _start := Pos('%', Result, _start + Length(VarToStr(_value)));
  end;
end;

procedure TLogicController.LogMessage(const AState, AMessage: string);
begin
  if FLogger = nil then
    Self.CreateLogger();

  if FLogger <> nil then
    FLogger.Write(1, AState, lmt_Info, Self.ExpandMessage(AMessage));
end;

procedure TLogicController.ApplyOutputPreset(const APresetName: String);
var
  _actList: TActionsList;
  _act: TXPLCAction;
begin
  if FOutputPresets.TryGetData(APresetName, _actList) then
  begin
    for _act in _actList do
      _act.Execute('Preset: ' + APresetName, Self);
  end
  else
    LBLogger.Write(1, 'TLogicController.ApplyOutputPreset', lmt_Warning, 'Output preset <%s> not found', [APresetName]);
end;

function TLogicController.GetInputValue(const AAlias: string): Variant;
var
  _item: TBasePLCItem;
  _var: TSharedVariable;
  _found: Boolean;
begin
  Result := Null;
  _found := False;

  if AAlias <> '' then
  begin
    if FInputItems.TryGetData(AAlias, _item) then
    begin
      Result := _item.ValueAsVariant;
      _found := True;
    end
    else if (FSharedVars <> nil) and FSharedVars.TryGetData(AAlias, _var) then
    begin
      Result := _var.GetValue;
      _found := True;
    end;

    if not _found then
      LBLogger.Write(1, 'TLogicController.GetInputValue', lmt_Warning, 'Input alias <%s> not found', [AAlias]);
  end;

end;

procedure TLogicController.setSharedVariable(aList: TSharedVarsMap);
var
  i : Integer;

begin
  if aList <> FSharedVars then
  begin
    if FSharedVars <> nil then
    begin
      for i := 0 to FSharedVars.Count - 1 do
        FSharedVars.Data[i].EventsManager.RemoveListener(Self);
    end;

    FSharedVars := aList;
    if FSharedVars <> nil then
    begin
      for i := 0 to FSharedVars.Count - 1 do
        FSharedVars.Data[i].EventsManager.AddEventListener(TSharedVariable.cEvent_ValueChanged, @Self.OnInputChanged, FEventsManager);
    end;
  end;
end;


{ TXPLCManager }

procedure TXPLCManager.loadStateMachines(aFile: TIniFile);
var
  _sections  : TStringList;
  _key       : String;
  _file      : String;
  _xplc      : TLogicController;
  _resolved  : TResolvedDocumentList;
  _doc       : TResolvedDocument;
  i          : Integer;

begin
  _sections := TStringList.Create;

  try
    aFile.ReadSection(cINI_SECTION_StateMachines, _sections);

    for _key in _sections do
    begin
      if not _key.StartsWith('SM') then Continue;

      _file := aFile.ReadString(cINI_SECTION_StateMachines, _key, '');

      if _file = '' then
      begin
        LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Warning, 'Empty filename for key <%s>', [_key]);
        Continue;
      end;

      _file := ResolvePathDeterministic(_file);

      if LowerCase(ExtractFileExt(_file)) = TXPLCTemplateEngine.cInstanceExtension then
      begin
        // ── File istanze: risoluzione tramite template engine ──────────────
        if FTemplateEngine = nil then
        begin
          LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Error, 'Key <%s> references .xplci file but TemplatesPath is not set', [_key]);
          Continue;
        end;

        LBLogger.Write(5, 'TXPLCManager.loadStateMachines', lmt_Debug, 'Resolving instance file <%s> for key <%s>', [_file, _key]);

        _resolved := FTemplateEngine.ResolveInstanceFile(_file);
        try
          for i := 0 to _resolved.Count - 1 do
          begin
            _doc  := _resolved[i];
            _xplc := TLogicController.Create(FPLCCollector, FTimerWheel);
            if _xplc.LoadFromXMLDocument(_doc.Document) then
            begin
              _xplc.setSharedVariable(FSharedVars);
              _xplc.registerAllVariables();
              FXPLCs.Add(_xplc);
              _xplc := nil;
              LBLogger.Write(5, 'TXPLCManager.loadStateMachines', lmt_Info, 'XPLC <%s> loaded from template', [_doc.Code]);
            end
            else
              LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Warning, 'Failed to load XPLC <%s> from template', [_doc.Code]);
          end;
        except
          on E: Exception do
          begin
            LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Error, 'Failed loading file <%s>', [_file]);
            _resolved.Free;
          end;
        end;

        if _xplc <> nil then
          FreeAndNil(_xplc);
      end
      else begin
        // ── File XML diretto: comportamento invariato ──────────────────────
        LBLogger.Write(5, 'TXPLCManager.loadStateMachines', lmt_Debug, 'Loading XPLC <%s>: %s', [_key, _file]);
        _xplc := TLogicController.Create(FPLCCollector, FTimerWheel);
        if _xplc.LoadFromXMLFile(_file) then
        begin
          _xplc.setSharedVariable(FSharedVars);
          _xplc.registerAllVariables();
          FXPLCs.Add(_xplc);
          _xplc := nil;
          LBLogger.Write(5, 'TXPLCManager.loadStateMachines', lmt_Info, 'XPLC <%s> started', [_key]);
        end
        else
          LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Warning, 'Failed to load XPLC <%s>', [_key]);
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCManager.loadStateMachines', lmt_Error, E.Message);
  end;

  if _xplc <> nil then
    _xplc.Free;

  _sections.Free;
end;

procedure TXPLCManager.loadGlobalVariables(aFile: TIniFile);
var
  _globalVarFile : String;

begin
  _globalVarFile := aFile.ReadString(cINI_SECTION_StateMachines, cINI_KEY_GlobalVar, '');
  if _globalVarFile <> '' then
  begin
    _globalVarFile := ResolvePathDeterministic(_globalVarFile);
    if FileExists(_globalVarFile) then
    begin
      LBLogger.Write(5, 'TXPLCManager.loadGlobalVariables', lmt_Info,
                        'Loading global variables from <%s>', [_globalVarFile]);
      Self.LoadSharedVarsFromXML(_globalVarFile);
    end
    else
      LBLogger.Write(1, 'TXPLCManager.loadGlobalVariables', lmt_Warning,
                        'Global variables file <%s> not found', [_globalVarFile]);
  end;
end;

procedure TXPLCManager.loadTemplatesPath(aFile: TIniFile);
var
  _path : String;

begin
  _path := aFile.ReadString(cINI_SECTION_StateMachines, cINI_KEY_TemplatesPath, '');

  if _path <> '' then
  begin

    _path := ResolvePathDeterministic(_path);

    if DirectoryExists(_path) then
    begin
      FTemplateEngine := TXPLCTemplateEngine.Create(_path);
      LBLogger.Write(5, 'TXPLCManager.loadTemplatesPath', lmt_Info, 'Template engine initialized with path <%s>', [_path]);
    end
    else
      LBLogger.Write(1, 'TXPLCManager.loadTemplatesPath', lmt_Warning, 'Templates directory <%s> not found', [_path]);

  end
  else
    LBLogger.Write(5, 'TXPLCManager.loadTemplatesPath', lmt_Debug, 'TemplatesPath not set: template engine disabled', []);

end;

procedure TXPLCManager.loadSharedVarsFromXML(const aFilename: String);
var
  _doc: TXMLDocument;
  _root, _node: TDOMNode;
  i: Integer;
  _shrVar : TSharedVariable;

begin
  if not OpenXMLFile(aFileName, _doc) then Exit;

  try
    _root := _doc.DocumentElement;

    if _root <> nil then
    begin
      FSharedVars.Clear;
      for i := 0 to _root.ChildNodes.Count - 1 do
      begin
        _shrVar := TSharedVariable.Create();
        if _shrVar.loadFromXMLNode(_root.ChildNodes[i]) then
          FSharedVars.Add(_shrVar.VarName, _shrVar)
        else
          _shrVar.Free;
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCManager.loadSharedVarsFromXML', lmt_Error, E.Message);
  end;
  _doc.Free;
end;

constructor TXPLCManager.Create(ACollector: TPLCCollector);
begin
  inherited Create;

  FPLCCollector := ACollector;
  FXPLCs := TLogicControllerList.Create(True);

  FSharedVars := TSharedVarsMap.Create(True);
  FSharedVars.Sorted := True;

  // Unico thread timer per tutte le macchine a stati: creato qui, avviato qui,
  // passato ai TLogicController in loadStateMachines (non owned da loro)
  FTimerWheel := TTimerWheel.Create;
  FTimerWheel.Start;
end;

destructor TXPLCManager.Destroy;
begin
  try
    FreeAndNil(FXPLCs);       // prima le macchine: StopTimer su tutti i timer
    FreeAndNil(FSharedVars);
    FreeAndNil(FTimerWheel);  // poi la wheel
    FreeAndNil(FTemplateEngine);

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCManager.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

function TXPLCManager.LoadConfigurationFromINIFile(aFile: String): Boolean;
var
  _IniFile : TIniFile = nil;

begin
  Result := False;

  if aFile <> '' then
  begin
    aFile := ResolvePathDeterministic(aFile);
    if FileExists(aFile) then
    begin
      try
        _IniFile := TIniFile.Create(aFile);
        Result := Self.LoadConfigurationFromINIFile(_IniFile);
      except
        on E: Exception do
          LBLogger.Write(1, 'TXPLCManager.LoadConfigurationFromINIFile', lmt_Error, E.Message);
      end;

      if _IniFile <> nil then
        _IniFile.Free;
    end
    else
      LBLogger.Write(1, 'TXPLCManager.LoadConfigurationFromINIFile', lmt_Warning, 'Configuration file <%s> not found!', [aFile]);
  end;
end;

function TXPLCManager.LoadConfigurationFromINIFile(aFile: TIniFile): Boolean;

(*
[StateMachines]
GlobalVar=./SharedVars.xml
TemplatesPath=./templates

; Macchine a stati dirette
SM01=./Automazione.xml
SM02=./SemaforiPedonali.xml

; Istanze da template (generano N macchine ciascuna)
SM10=./Tornelli.xplci
*)

begin
  Result := False;

  if aFile = nil then Exit;

  try

    if aFile.SectionExists(cINI_SECTION_StateMachines) then
    begin
      Self.loadGlobalVariables(aFile);
      Self.loadTemplatesPath(aFile);    // crea FTemplateEngine se TemplatesPath è presente
      Self.loadStateMachines(aFile);
    end
    else
      LBLogger.Write(1, 'TXPLCManager.Create', lmt_Warning, 'Section <%s> not found in INI file <%s>', [cINI_SECTION_StateMachines, aFile.FileName]);

    Result := FXPLCs.Count > 0;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCManager.Create', lmt_Error, E.Message);
  end;

end;

procedure TXPLCManager.SetSharedVariable(const AName: String; AValue: Variant);
var
  _Idx : Integer;
  _SharedVar : TSharedVariable;

begin
  FSharedVars.Find(AName, _Idx);
  if _Idx >= 0 then
    FSharedVars.Data[_Idx].SetValue(AValue);
end;

function TXPLCManager.GetSharedVariableValueAsInteger(const AName: String): Integer;
var
  _Idx : Integer;
  _SharedVar : TSharedVariable;

begin
  FSharedVars.Find(AName, _Idx);
  if _Idx >= 0 then
    Result := Integer(FSharedVars.Data[_Idx].GetValue);
end;

procedure TXPLCManager.StartStatesMachines();
var
  i : Integer;

begin
  if (FXPLCs <> nil) then
  begin
    for i := 0 to FXPLCs.Count - 1 do
      FXPLCs.Items[i].Start();
  end;
end;

end.
