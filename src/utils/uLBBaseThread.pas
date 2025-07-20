unit uLBBaseThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  pThread = ^TThread;
  pObject = ^TObject;
  
  { TMultiReferenceObject }

  TMultiReferenceObject = class(TObject)
    strict protected
      FReferences : TThreadList;  // Lista thread-safe di puntatori a variabili che puntano a questo oggetto
                                  // Al momento della distruzione, tutti i riferimenti vengono impostati a nil

    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure ClearReferences();

      function AddReference(AReference: pObject): Boolean;
      procedure RemoveReference(AReference: pObject);
  end;


  { TLBBaseThread }

  TLBBaseThread = class(TThread)
    strict private
      FOnAsyncTerminate : TNotifyEvent;

    strict protected
      FTerminatedEvent : PRTLEvent;
      FExitFromPauseEvent : PRTLEvent;  // Evento usato per interrompere le pause
      FReferences : TMultiReferenceObject;  // Lista dei riferimenti al thread

      procedure PauseFor(mSecs: Integer); overload;  // PauseFor è preferibile a Sleep perché reattiva al Terminate

      class function getThreadName(): String; virtual;

    protected
      procedure Execute; override;  // ⚠️ Do NOT override this procedure, use InternalExecute instead
      procedure InternalExecute(); virtual; abstract;

    public
      constructor Create(); virtual;
      {
        Attenzione: le classi derivate devono chiamare inherited Destroy PRIMA
        della distruzione delle risorse locali, perché solo dopo
        è garantita la terminazione completa del thread.
      }
      destructor Destroy(); override;

      procedure setThreadName(const aValue: AnsiString);

      procedure Terminate; reintroduce; virtual;

      function AddReference(AReference: pThread): Boolean;
      procedure RemoveReference(AReference: pThread);

      property OnAsyncTerminate: TNotifyEvent write FOnAsyncTerminate;
  end;

  TLBBaseThreadClassType = class of TLBBaseThread;

implementation

uses
  ULBLogger, uThreadsUtils;

{ TMultiReferenceObject }

constructor TMultiReferenceObject.Create;
begin
  inherited Create;

  FReferences := TThreadList.Create;
end;

destructor TMultiReferenceObject.Destroy;
begin

  if Self <> nil then
  begin

    Self.ClearReferences();

    FreeAndNil(FReferences);

    inherited Destroy;
  end;

end;

procedure TMultiReferenceObject.ClearReferences();
var
  i : Integer;
  _List : TList;

begin

  if FReferences <> nil then
  begin
    _List := FReferences.LockList();

    try

      for i := 0 to _List.Count - 1 do
        pObject(_List.Items[i])^ := nil;

    except
      on E: Exception do
        LBLogger.Write(1, 'TMultiReferenceObject.ClearReferences', lmt_Error, '1. %s - %s', [Self.ClassName, E.Message]);
    end;

    try

      _List.Clear;

    except
      on E: Exception do
        LBLogger.Write(1, 'TMultiReferenceObject.ClearReferences', lmt_Error, '1. %s - %s', [Self.ClassName, E.Message]);
    end;

    FReferences.UnlockList;

  end;
end;

function TMultiReferenceObject.AddReference(AReference: pObject): Boolean;
var
  _Idx : Integer;
  _List : TList;

begin
  Result := False;

  if AReference <> nil then
  begin

    try
      _List := FReferences.LockList();
      _Idx := _List.IndexOf(AReference);
      if _Idx = -1 then
        _List.Add(AReference);

      Result := True;
    finally
      FReferences.UnlockList;
    end;

  end;
end;

procedure TMultiReferenceObject.RemoveReference(AReference: pObject);
var
  _Idx : Integer;
  _List : TList;

begin
  if AReference <> nil then
  begin

    try
      _List := FReferences.LockList();

      _Idx := _List.IndexOf(AReference);
      if _Idx > -1 then
        _List.Delete(_Idx);

    finally
      FReferences.UnlockList();
    end;
  end;
end;


{ TLBBaseThread }

procedure TLBBaseThread.Execute;
begin
  try
    Self.InternalExecute;
  except
    on E: Exception do
      LBLogger.Write(1, 'TLBBaseThread.Execute', lmt_Error, E.Message);
  end;
  RTLEventSetEvent(FTerminatedEvent);
end;

procedure TLBBaseThread.PauseFor(mSecs: Integer);
begin
  if (Self <> nil) and (not Self.Terminated) then
    RTLEventWaitFor(FExitFromPauseEvent, mSecs);
end;

class function TLBBaseThread.getThreadName(): String;
begin
  Result := ClassName;
end;

constructor TLBBaseThread.Create();
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FReferences := TMultiReferenceObject.Create;

  FExitFromPauseEvent := RTLEventCreate();
  FTerminatedEvent := RTLEventCreate();

  Self.setDebugName(Self.getThreadName);
end;

destructor TLBBaseThread.Destroy();
begin
  if Self <> nil then
  begin

    try

      if Assigned(FOnAsyncTerminate) then
        FOnAsyncTerminate(Self);

      FreeOnTerminate := False;

      if (not Self.Suspended) and (not Self.Terminated) then
      begin
        Self.Terminate;
        // Attende la terminazione vera
        LBLogger.Write(5, 'TLBBaseThread.Destroy', lmt_Debug, 'Waiting thread <%s> termination ...', [Self.ClassName]);
        RTLEventWaitFor(FTerminatedEvent, 2000);
      end;

      FreeAndNil(FReferences);
      RTLEventDestroy(FExitFromPauseEvent);
      RTLEventDestroy(FTerminatedEvent);

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBBaseThread.Destroy', lmt_Error, E.Message);
    end;

    inherited Destroy;

  end;
end;

procedure TLBBaseThread.setThreadName(const aValue: AnsiString);
begin
  if Length(aValue) > 0 then
    Self.setDebugName(aValue);
end;

procedure TLBBaseThread.Terminate;
begin
  if Self <> nil then
  begin
    inherited Terminate;
    RTLEventSetEvent(FExitFromPauseEvent);
  end;
end;


function TLBBaseThread.AddReference(AReference: pThread): Boolean;
begin
  if (Self <> nil) and (FReferences <> nil) then
    Result := FReferences.AddReference(AReference);
end;

procedure TLBBaseThread.RemoveReference(AReference: pThread);
begin
  if (Self <> nil) and (FReferences <> nil) then
    FReferences.RemoveReference(AReference);
end;

end.
