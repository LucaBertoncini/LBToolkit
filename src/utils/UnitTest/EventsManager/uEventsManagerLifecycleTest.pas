unit uEventsManagerLifecycleTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uEventsManager, SyncObjs;

type

  { TTestEventsManagerLifecycle }

  TTestEventsManagerLifecycle = class(TTestCase)
  private
    FEM1: TEventsManager;
    FEM2: TEventsManager;

    CallCount: Integer;
    Lock: TCriticalSection;

    procedure ConcurrentListener(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestConcurrentRaiseEvent;
    procedure TestRemoveListener;
    procedure TestRemoveListenerWhileRunning;
  end;

  { TRaiseThread }

  TRaiseThread = class(TThread)
    strict private
      FWait : Integer;
      FOwner : TTestEventsManagerLifecycle;

    protected
      procedure Execute; override;

    public
      constructor Create(anOwner: TTestEventsManagerLifecycle; MaxTimeout: Integer); reintroduce;
  end;


implementation

uses
  Math;

procedure TTestEventsManagerLifecycle.SetUp;
begin
  FEM1 := TEventsManager.Create(Self, emm_Events);
  FEM1.addEvent('onStress');

  Lock := TCriticalSection.Create;
  CallCount := 0;
end;

procedure TTestEventsManagerLifecycle.TearDown;
begin
  if Assigned(FEM2) then
    FreeAndNil(FEM2);

  FreeAndNil(FEM1);

  Lock.Free;
end;

procedure TTestEventsManagerLifecycle.ConcurrentListener(Sender: TObject);
begin
  Lock.Enter;
  Inc(CallCount);
  Lock.Leave;
end;

procedure TTestEventsManagerLifecycle.TestConcurrentRaiseEvent;
var
  Threads: array[0..39] of TThread;
  i: Integer;

begin
  CallCount := 0;

  if FEM2 = nil then
    FEM2 := TEventsManager.Create(Self);

  FEM1.AddEventListener('onStress', @Self.ConcurrentListener, FEM2);

  for i := 0 to High(Threads) do
    Threads[i] := TRaiseThread.Create(Self, 0);

  for i := 0 to High(Threads) do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;

  AssertEquals('CallCount must match threads', Length(Threads), CallCount);
end;

procedure TTestEventsManagerLifecycle.TestRemoveListener;
begin
  if FEM2 <> nil then
    FreeAndNil(FEM2);

  AssertTrue('Listeners should be = 0', FEM1.EventListenersCount['onStress'] = 0);
end;

procedure TTestEventsManagerLifecycle.TestRemoveListenerWhileRunning;
var
  Threads: array[0..39] of TThread;
  i: Integer;

begin
  CallCount := 0;

  if FEM2 = nil then
    FEM2 := TEventsManager.Create(Self);

  FEM1.AddEventListener('onStress', @Self.ConcurrentListener, FEM2);

  Threads[0] := TRaiseThread.Create(Self, 0);

  for i := 1 to High(Threads) do
    Threads[i] := TRaiseThread.Create(Self, 20);

  Sleep(10);
  FreeAndNil(FEM2);

  for i := 0 to High(Threads) do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;

  AssertTrue('CallCount is less than threads', (CallCount > 0) and (CallCount < Length(Threads)));
end;

procedure TTestEventsManagerLifecycle.TestAddListener;
begin
  if FEM2 = nil then
    FEM2 := TEventsManager.Create(Self);

  FEM1.AddEventListener('onStress', @Self.ConcurrentListener, FEM2);
  FEM1.AddEventListener('onStress', @Self.ConcurrentListener, FEM2);
  FEM1.AddEventListener('onStress', @Self.ConcurrentListener, FEM2);

  AssertTrue('Listener should be 1', FEM1.EventListenersCount['onStress'] = 1);
end;

{ TRaiseThread }

procedure TRaiseThread.Execute;
begin
  Sleep(FWait);
  FOwner.FEM1.RaiseEvent('onStress');
end;

constructor TRaiseThread.Create(anOwner: TTestEventsManagerLifecycle; MaxTimeout: Integer);
begin
  FOwner := anOwner;
  if MaxTimeout < 5 then
    FWait := MaxTimeout
  else
    FWait := RandomRange(5, MaxTimeout);

  inherited Create(False);
end;

initialization
  RegisterTest(TTestEventsManagerLifecycle);

end.

