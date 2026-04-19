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
      FExitFromPauseEvent : PRTLEvent;  // Evento usato per interrompere le pause
      FReferences : TMultiReferenceObject;  // Lista dei riferimenti al thread

      procedure PauseFor(mSecs: Integer); overload;  // PauseFor è preferibile a Sleep perché reattiva al Terminate

      class function getThreadName(): String; virtual;

    public
      constructor Create(); virtual;
      {
        Attenzione: le classi derivate devono chiamare inherited Destroy PRIMA
        della distruzione delle risorse locali, perché solo dopo
        è garantita la terminazione completa del thread.
      }
      destructor Destroy(); override;

      function WaitFor(): Integer; reintroduce;

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

  FreeOnTerminate := True;

  FReferences := TMultiReferenceObject.Create;

  FExitFromPauseEvent := RTLEventCreate();

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
        LBLogger.Write(6, 'TLBBaseThread.Destroy', lmt_Debug, 'Waiting thread <%s> termination ...', [Self.ClassName]);
        Self.WaitFor;
      end;

      FreeAndNil(FReferences);
      RTLEventDestroy(FExitFromPauseEvent);

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBBaseThread.Destroy', lmt_Error, E.Message);
    end;

    inherited Destroy;

  end;
end;

function TLBBaseThread.WaitFor(): Integer;
begin
  FreeOnTerminate := False;
  Result := inherited WaitFor();
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
    Result := FReferences.AddReference(AReference)
  else
    Result := False;
end;

procedure TLBBaseThread.RemoveReference(AReference: pThread);
begin
  if (Self <> nil) and (FReferences <> nil) then
    FReferences.RemoveReference(AReference);
end;

end.
