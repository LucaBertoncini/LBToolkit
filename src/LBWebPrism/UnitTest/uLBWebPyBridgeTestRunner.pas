unit uLBWebPyBridgeTestRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, consoletestrunner, uLBWebPrismApplication;

type

  { TLBWebPyBridgeTestRunner }

  TLBWebPyBridgeTestRunner = class(TTestRunner)
    strict private
      FConfigFilename : String;
      FWebPyBridge : TLBWebPrismApplication;

      procedure createConfigFile();

      const
        cConfigFilename = String('config.ini');
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure extractTestFiles();
      property WebPyBridge: TLBWebPrismApplication read FWebPyBridge;
  end;

var
  Application: TLBWebPyBridgeTestRunner;


implementation

uses
  ULBLogger, uLBFileUtils;

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
    _cfg.Add('[PythonBridge]');
    _cfg.Add('SharedMemorySize=1048576');
    _cfg.Add('ThreadPoolSize=1');
    _cfg.Add('WorkerTimeout=5000'); // Use a shorter timeout for tests
    _cfg.Add('ScriptsFolder=./pyScripts');
    _cfg.Add('');
    _cfg.Add('[NodeJSBridge]');
    _cfg.Add('ThreadPoolSize=1');
    _cfg.Add('WorkerTimeout=5000'); // Use a shorter timeout for tests
    _cfg.Add('ScriptsFolder=./jsScripts');
    {$IFDEF WINDOWS}
    _cfg.Add('SocketFilename=\\.\pipe\Prism_NodeJS');
    {$ELSE}
    _cfg.Add('SocketFilename=/tmp/NodeJS.sock');
    {$ENDIF}

    _cfg.SaveToFile(FConfigFilename);
  finally
    _cfg.Free;
  end;
end;

procedure TLBWebPyBridgeTestRunner.extractTestFiles();
const
  cTestFiles : array [0 .. 5] of TResourceFileInfo = (
    (Filename: 'tests/echo_test.py';       Code: 'PY_ECHO_TEST'),
    (Filename: 'tests/error_test.py';      Code: 'PY_ERROR_TEST'),
    (Filename: 'tests/sum_test.py';        Code: 'PY_SUM_TEST'),
    (Filename: 'tests/AppHello/config.py'; Code: 'PY_HELLO_CONFIG'),
    (Filename: 'tests/AppHello/main.py';   Code: 'PY_HELLO_MAIN'),
    (Filename: 'tests/AppHello/utils.py';  Code: 'PY_HELLO_UTILS')
  );

  cJSTestFiles : array [0 .. 1] of TResourceFileInfo = (
    (Filename: 'tests/example.js';       Code: 'JS_EXAMPLE'),
    (Filename: 'tests/multiply.js';       Code: 'JS_MULTIPLY')
  );

var
  _DestFolder : String;

begin
  _DestFolder := ExpandFileName('./pyScripts');
  if not DirectoryExists(_DestFolder) then
    ForceDirectories(_DestFolder);

  uLBFileUtils.extractFilesFormResource(_DestFolder, @cTestFiles[0], Length(cTestFiles));

  _DestFolder := ExpandFileName('./jsScripts');
  if not DirectoryExists(_DestFolder) then
    ForceDirectories(_DestFolder);

  uLBFileUtils.extractFilesFormResource(_DestFolder, @cJSTestFiles[0], Length(cJSTestFiles));
end;

constructor TLBWebPyBridgeTestRunner.Create(AOwner: TComponent);
var
  _ErrMsg : String;

begin
  inherited Create(AOwner);
  InitLogger(5, 'LBWebPrismTest.log');
  FConfigFilename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cConfigFilename;
  Self.createConfigFile();

  FWebPyBridge := TLBWebPrismApplication.Create;
  if FWebPyBridge.LoadConfiguration(FConfigFilename, _ErrMsg) then
  begin
    Self.extractTestFiles();
    FWebPyBridge.Activate();
  end
  else
    LBLogger.Write(1, 'TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal', lmt_Warning,
                      'WebPrism configuration not loaded: <%s>', [_ErrMsg]);

end;

destructor TLBWebPyBridgeTestRunner.Destroy;
begin
  FreeAndNil(FWebPyBridge);

  ReleaseLogger();

  inherited Destroy;
end;

end.


