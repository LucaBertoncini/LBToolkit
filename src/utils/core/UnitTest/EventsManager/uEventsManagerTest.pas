unit uEventsManagerTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uEventsManager;

type

  TTestEventsManager = class(TTestCase)
  private
    Manager: TEventsManager;
    WasCalled: Boolean;
    EventNameReceived: String;

    procedure DummyNotify(Sender: TObject);
    procedure DummySingle(Sender: TObject; anEventName: String);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestBasicNotify;
    procedure TestDisabledEvent;
    procedure TestCallbackMode;
    procedure TestSingleCallback;
    procedure TestRemoveListener;
  end;

implementation

function DummyCallback(Opaque: Pointer): Integer; {$IFDEF Linux}cdecl{$ELSE}stdcall{$ENDIF};
begin
  PBoolean(Opaque)^ := True;
  Result := 0;
end;

function DummyNotifyNoObj(Opaque: Pointer): Integer; {$IFDEF Linux}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TTestEventsManager(Opaque).WasCalled := True;
  Result := 0;
end;


procedure TTestEventsManager.SetUp;
begin
  WasCalled := False;
  EventNameReceived := '';
end;

procedure TTestEventsManager.TearDown;
begin
  if Assigned(Manager) then
    Manager.Free;
end;

procedure TTestEventsManager.DummyNotify(Sender: TObject);
begin
  WasCalled := True;
end;

procedure TTestEventsManager.DummySingle(Sender: TObject; anEventName: String);
begin
  WasCalled := True;
  EventNameReceived := anEventName;
end;

procedure TTestEventsManager.TestBasicNotify;
begin
  Manager := TEventsManager.Create(Self, emm_Events);
  Manager.AddEvent('onBasic');
  Manager.AddEventListener('onBasic', @Self.DummyNotify, nil);
  Manager.RaiseEvent('onBasic');
  AssertTrue('TNotifyEvent must be called', WasCalled);
end;

procedure TTestEventsManager.TestDisabledEvent;
begin
  Manager := TEventsManager.Create(Self, emm_Events);
  Manager.AddEvent('onDisabled');
  Manager.AddEventListener('onDisabled', @Self.DummyNotify, nil);
  Manager.DisableEvent('onDisabled');
  Manager.RaiseEvent('onDisabled');
  AssertFalse('Disabled event must not trigger', WasCalled);
end;

procedure TTestEventsManager.TestCallbackMode;
begin
  Manager := TEventsManager.Create(Self, emm_Callbacks);
  Manager.AddEvent('onCallback');
  Manager.AddEventListener('onCallback', @WasCalled, @DummyCallback);
  Manager.RaiseEvent('onCallback');
  AssertTrue('Callback must be invoked', WasCalled);
end;

procedure TTestEventsManager.TestSingleCallback;
begin
  Manager := TEventsManager.Create(Self, emm_EventsSingleCallback);
  Manager.AddEvent('onSingle1');
  Manager.AddEvent('onSingle2');
  Manager.AddSingleCallbackListener(@DummySingle, nil);
  Manager.RaiseEvent('onSingle2');
  AssertTrue('SingleCallback must be triggered', WasCalled);
  AssertEquals('Event name should be passed correctly', 'onSingle2', EventNameReceived);
end;

procedure TTestEventsManager.TestRemoveListener;
begin
  Manager := TEventsManager.Create(Self, emm_Events);
  Manager.AddEvent('onRemove');
  Manager.AddEventListener('onRemove', Pointer(Self), @DummyNotifyNoObj);
  Manager.RemoveListener(Self);
  Manager.RaiseEvent('onRemove');
  AssertFalse('Listener should be removed and not called', WasCalled);
end;

initialization
  RegisterTest(TTestEventsManager);

end.

