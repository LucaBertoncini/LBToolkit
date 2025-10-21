program LBWebPyBridgeTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}cthreads,{$ENDIF} Classes, ssl_openssl3, uLBWebPyBridgeTestRunner,
  uLBWebPyBridgeTests, consoletestrunner;

{$R *.res}

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TLBWebPyBridgeTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'LBWebPyBridge Tests';
  Application.Run;
  Application.Free;
end.
