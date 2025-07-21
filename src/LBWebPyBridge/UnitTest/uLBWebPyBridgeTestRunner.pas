unit uLBWebPyBridgeTestRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, consoletestrunner, uLBWebPyBridgeApplication;

type

  { TLBWebPyBridgeTestRunner }

  TLBWebPyBridgeTestRunner = class(TTestRunner)
    strict private
      FConfigFilename : String;
      FWebPyBridge : TLBWebPyBridgeApplication;

      procedure createConfigFile();

      const
        cConfigFilename = String('config.ini');
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property WebPyBridge: TLBWebPyBridgeApplication read FWebPyBridge;
  end;

var
  Application: TLBWebPyBridgeTestRunner;


implementation

uses
  ULBLogger;

{ TLBWebPyBridgeTestRunner }

procedure TLBWebPyBridgeTestRunner.createConfigFile();
var
  _cfg: TStringList;
begin
  if FileExists(FConfigFilename) then
    DeleteFile(FConfigFilename);

  _cfg := TStringList.Create;
  try
    _cfg.Add('[LBmicroWebServer]');
    _cfg.Add('Port=10320');
    _cfg.Add('');
    _cfg.Add('[LBWebPyBridge]');
    _cfg.Add('SharedMemorySize=1048576');
    _cfg.Add('ThreadPoolSize=4');
    _cfg.SaveToFile(FConfigFilename);
  finally
    _cfg.Free;
  end;
end;

constructor TLBWebPyBridgeTestRunner.Create(AOwner: TComponent);
var
  _ErrMsg : String;

begin
  inherited Create(AOwner);
  InitLogger(5, 'LBWebPyBridgeTest.log');
  FConfigFilename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cConfigFilename;
  Self.createConfigFile();

  FWebPyBridge := TLBWebPyBridgeApplication.Create;
  if FWebPyBridge.LoadConfiguration(FConfigFilename, _ErrMsg) then
    FWebPyBridge.Activate()
  else
    LBLogger.Write(1, 'TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal', lmt_Warning,
                      'WebPyBridge configuration not loaded: <%s>', [_ErrMsg]);

end;

destructor TLBWebPyBridgeTestRunner.Destroy;
begin
  FreeAndNil(FWebPyBridge);

  ReleaseLogger();

  inherited Destroy;
end;

end.

