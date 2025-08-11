unit uTestRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, consoletestrunner, uLBmicroWebServer, uLBmWsDocumentsFolder;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  strict private
    FWebServer : TLBmicroWebServer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property WebServer: TLBmicroWebServer read FWebServer;
  end;

var
  Application: TMyTestRunner;

implementation

uses
  ULBLogger;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitLogger(5, 'Test_mWs.log');

  FWebServer := TLBmicroWebServer.Create;
  FWebServer.DocumentsFolder := TLBmWsDocumentsFolder.Create;
  FWebServer.DocumentsFolder.DocumentFolder := GetTempDir;
  FWebServer.Activate(10320, nil);
  Sleep(10);
end;

destructor TMyTestRunner.Destroy;
begin
  FreeAndNil(FWebServer);
  ReleaseLogger();
  inherited Destroy;
end;


end.

