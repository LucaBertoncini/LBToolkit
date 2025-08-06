program TestWebSocketClient;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, ssl_openssl3, uTestWebSocketClient, consoletestrunner,
  ULBLogger;

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
  InitLogger(5, 'Test_WSClient');
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
