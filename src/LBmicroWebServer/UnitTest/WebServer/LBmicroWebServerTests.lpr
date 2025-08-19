program LBmicroWebServerTests;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, consoletestrunner, uHTTPRequestManagerTests,
  uLBmicroWebServer, ULBLogger, uLBmWsDocumentsFolder, uTestRunner, uFPHTTPRequestProcessor;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'LB micro WebServer Tests';
  Application.Run;
  Application.Free;
end.
