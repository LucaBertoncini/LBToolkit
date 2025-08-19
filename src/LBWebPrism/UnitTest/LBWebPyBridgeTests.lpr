program LBWebPyBridgeTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}cthreads,{$ENDIF}Classes, uLBWebPyBridgeTestRunner, uLBWebPyBridgeTests, consoletestrunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TLBWebPyBridgeTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'LBWebPyBridge Tests';
  Application.Run;
  Application.Free;
end.
