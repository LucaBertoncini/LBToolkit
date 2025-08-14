unit uLBWebPyBridgeApplication;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBApplicationBoostrap, IniFiles,
  uPyBridgeChainModule;

type

  { TLBWebPyBridgeApplication }

  TLBWebPyBridgeApplication = class(TLBApplicationBoostrap)
  strict private
    type
      TConfigurationInfo = record
        SharedMemorySize    : Cardinal;
        ThreadPoolSize      : Integer;
        WorkerTimeoutMs     : Integer;
        PythonScriptsFolder : String;
      end;

  strict private
    FOrchestratorModule : TPyBridgeChainModule;
    FConfig : TConfigurationInfo;

    const
      INI_SECTION_WEBPYBRIDGE      = 'LBWebPyBridge';
      INI_KEY_SHARED_MEMORY_SIZE   = 'SharedMemorySize';
      INI_KEY_THREAD_POOL_SIZE     = 'ThreadPoolSize';
      INI_KEY_WORKER_TIMEOUT       = 'WorkerTimeout';
      INI_KEY_SCRIPTS_FOLDER       = 'ScriptsFolder';
      DEFAULT_SHARED_MEMORY_SIZE   = 5 * 1024 * 1024; // 5MB
      DEFAULT_THREAD_POOL_SIZE     = 4;
      DEFAULT_WORKER_TIMEOUT_MS    = 20000; // 20 seconds


    strict protected
      procedure startingWebServer(); override; // Used for intercept POST requests and to create processing chain
      function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; override;

    public
      procedure setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
      function extractPythonFilesFromZIP(const aZipFile: String): Boolean;
      function extractPythonFilesFromResources(): Boolean;

      procedure Activate();

  end;


implementation

uses
  ULBLogger, fpjson, Zipper, uPyBridge;


{ TLBWebPyBridgeApplication }

procedure TLBWebPyBridgeApplication.startingWebServer();
begin
  LBLogger.Write(5, 'TLBWebPyBridgeApplication.startingWebServer', lmt_Debug, 'Capturing OnPOSTRequest callback');
  FOrchestratorModule := TPyBridgeChainModule.Create();
  FWebServer.addChainProcessor(FOrchestratorModule, True);
end;

function TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
var
  _DefaultScriptsPath : String;

begin
  Result := False;
  anErrorMsg := '';

  try

    _DefaultScriptsPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder;

    FConfig.SharedMemorySize    := aFile.ReadInt64(INI_SECTION_WEBPYBRIDGE, INI_KEY_SHARED_MEMORY_SIZE, DEFAULT_SHARED_MEMORY_SIZE);
    FConfig.ThreadPoolSize      := aFile.ReadInteger(INI_SECTION_WEBPYBRIDGE, INI_KEY_THREAD_POOL_SIZE, DEFAULT_THREAD_POOL_SIZE);
    FConfig.WorkerTimeoutMs     := aFile.ReadInteger(INI_SECTION_WEBPYBRIDGE, INI_KEY_WORKER_TIMEOUT, DEFAULT_WORKER_TIMEOUT_MS);
    FConfig.PythonScriptsFolder := aFile.ReadString(INI_SECTION_WEBPYBRIDGE, INI_KEY_SCRIPTS_FOLDER, _DefaultScriptsPath);

    if FConfig.PythonScriptsFolder = '' then FConfig.PythonScriptsFolder := _DefaultScriptsPath;
    if not DirectoryExists(FConfig.PythonScriptsFolder) then
      ForceDirectories(FConfig.PythonScriptsFolder);

    Result := True;
  except
    on E: Exception do
    begin
      anErrorMsg := Format('Exception while reading INI: %s', [E.Message]);
      LBLogger.Write(1, 'TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal', lmt_Error, anErrorMsg);
    end;
  end;
end;

procedure TLBWebPyBridgeApplication.setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
begin
  if aThreadPoolSize > 0 then
    FConfig.ThreadPoolSize := aThreadPoolSize;

  if aSharedMemorySize > 0 then
    FConfig.SharedMemorySize := aSharedMemorySize;

  if FOrchestratorModule <> nil then // Restarting
    Self.Activate();
end;

function TLBWebPyBridgeApplication.extractPythonFilesFromZIP(const aZipFile: String): Boolean;
var
  _unzip: TUnZipper;
begin
  Result := False;

  if (aZipFile <> '') and FileExists(aZipFile) then
  begin

    _unzip := TUnZipper.Create;
    try
      _unzip.FileName := aZipFile;
      _unzip.OutputPath := FConfig.PythonScriptsFolder; //IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder;
      _unzip.UnZipAllFiles;

      Result := True;
    except
      on E: Exception do
        LBLogger.Write(5, 'TLBWebPyBridgeApplication.extractPythonFiles', lmt_Error, E.Message);
    end;
    _unzip.Free;

  end
  else
    LBLogger.Write(5, 'TLBWebPyBridgeApplication.extractPythonFiles', lmt_Warning, 'Zip file <%s> not found!', [aZipFile]);

end;

function TLBWebPyBridgeApplication.extractPythonFilesFromResources(): Boolean;
var
  _Stream : TResourceStream;
  _DestFolder : String;
  _Path : String;
  i : Integer;

type
  TFileData = record
    Filename : String;
    Code : String;
  end;

const
  _Files : array [0 .. 9] of TFileData = ( (Filename: 'worker.py';                                      Code: 'WORKER'),
                                           (Filename: 'lb_logger.py';                                   Code: 'LB_LOGGER'),
                                           (Filename: 'test_launcher.py';                               Code: 'TEST_LAUNCHER'),
                                           (Filename: 'readme.txt';                                     Code: 'README'),
                                           (Filename: 'LBBridge' + PathDelim + 'sem_sysv.py';           Code: 'SEM_SYSV'),
                                           (Filename: 'LBBridge' + PathDelim + 'sem_win.py';            Code: 'SEM_WIN'),
                                           (Filename: 'LBBridge' + PathDelim + 'bridge_win.py';         Code: 'BRIDGE_WIN'),
                                           (Filename: 'LBBridge' + PathDelim + 'bridge_sysv.py';        Code: 'BRIDGE_SYSV'),
                                           (Filename: 'LBBridge' + PathDelim + '__init__.py';           Code: '__INIT__'),
                                           (Filename: 'LBBridge' + PathDelim + 'LBBridge_constants.py'; Code: 'LBBRIDGE_CONSTANTS'));

begin
  Result := False;

  try

    _DestFolder := IncludeTrailingPathDelimiter(FConfig.PythonScriptsFolder);

    for i := 0 to High(_Files) do
    begin
      if not FileExists(_DestFolder + _Files[i].Filename) then
      begin
        _Stream := TResourceStream.Create(HINSTANCE, _Files[i].Code, RT_RCDATA);
        if (_Stream <> nil) and (_Stream.Size > 0) then
        begin
          _Path := ExtractFilePath(_DestFolder + _Files[i].Filename);
          if not DirectoryExists(_Path) then
            ForceDirectories(_Path);

          _Stream.SaveToFile(_DestFolder + _Files[i].Filename);
        end;
      end;
    end;

    Result := True;

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebPyBridgeApplication.extractPythonFilesFromResources', lmt_Error, E.Message);
  end;
end;

procedure TLBWebPyBridgeApplication.Activate();
begin
  if FOrchestratorModule <> nil then
    FOrchestratorModule.setOrchestratorParams(FConfig.ThreadPoolSize, FConfig.SharedMemorySize, FConfig.WorkerTimeoutMs, FConfig.PythonScriptsFolder);
end;


end.

