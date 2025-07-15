program EventsManager_UnitTest;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, uEventsManagerTest, uEventsManagerLifecycleTest, uEventsManager, consoletestrunner;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Events Manager Unit Test';
  Application.Run;
  Application.Free;
end.
