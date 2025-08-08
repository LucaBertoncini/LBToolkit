program Test_LBCircularBuffer;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, consoletestrunner, uLBCircularBufferTests, ULBLogger;

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
  InitLogger(5, 'TestCircularBuffer.log');
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
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
