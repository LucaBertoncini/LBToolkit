program LBmWS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, ULBLogger, uLBmicroWebServer, uLBmWsDocumentsFolder, uLBWebServerConfigurationLoader;

var
  gv_WebServer : TLBmicroWebServer;
  gv_Loader : TINIConfigLoader;

begin
  InitLogger(5, 'LBmWS.log');

  gv_Loader := TINIConfigLoader.Create;
  gv_WebServer := TLBmicroWebServer.Create;

  gv_Loader.Filename := './config.ini';
  if gv_Loader.LoadConfig(gv_WebServer) then
  begin
    gv_WebServer.Activate();
    LBLogger.Write(5, 'LBmWS', lmt_Debug, 'WebServer started');
    while true do
      Sleep(20);
  end;
  FreeAndNil(gv_Loader);
  FreeAndNil(gv_WebServer);

  ReleaseLogger();
end.

