program LBmWS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  Classes, sysutils, ssl_openssl3, ULBLogger, uLBmicroWebServer,
  uLBmWsDocumentsFolder, uLBWebServerConfigurationLoader, uLBSignalManager;

var
  gv_WebServer : TLBmicroWebServer;
  gv_Loader : TINIConfigLoader;
  {$IFDEF Linux}
  gv_PIPESignal : TSignalManager;
  {$ENDIF}

{$IFDEF Linux}
procedure IgnoreBrokenPIPE(signal: longint; info: psiginfo; context: psigcontext); cdecl;
begin
  LBLogger.Write(1, 'LBmWS.IgnoreBrokenPIPE', lmt_Warning, 'Broken PIPE signal ignored');
end;
{$ENDIF}

begin
  InitLogger(5, 'LBmWS.log');

  {$IFDEF Linux}
  gv_PIPESignal := BrokenPipeSignalManager(@IgnoreBrokenPIPE);
  {$ENDIF}

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

  {$IFDEF Linux}
  FreeAndNil(gv_PIPESignal);
  {$ENDIF}

  ReleaseLogger();
end.

