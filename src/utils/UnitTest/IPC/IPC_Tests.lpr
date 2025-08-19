program IPC_Tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}cthreads,{$ENDIF} Classes, consoletestrunner, testlocalipc, ULBLogger;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TMyTestRunner;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitLogger(5, 'Test_IPC.log', False, True);
end;

destructor TMyTestRunner.Destroy;
begin
  ReleaseLogger();
  inherited Destroy;
end;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Test IPC library';
  Application.Run;
  Application.Free;
end.
