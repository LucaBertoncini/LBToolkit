unit uLBBaseThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMultiReference;

type
  pThread = ^TThread;

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
      FreeOnTerminate := False;

      if Assigned(FOnAsyncTerminate) then
        FOnAsyncTerminate(Self);

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
