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
        SharedMemorySize : Cardinal;
        ThreadPoolSize   : Integer;
        WorkerTimeoutMs  : Integer;
      end;

  strict private
    FOrchestratorModule : TPyBridgeChainModule;
    FConfig : TConfigurationInfo;

    const
      INI_SECTION_WEBPYBRIDGE      = 'LBWebPyBridge';
      INI_KEY_SHARED_MEMORY_SIZE   = 'SharedMemorySize';
      INI_KEY_THREAD_POOL_SIZE     = 'ThreadPoolSize';
      INI_KEY_WORKER_TIMEOUT       = 'WorkerTimeout';
      DEFAULT_SHARED_MEMORY_SIZE   = 5 * 1024 * 1024; // 5MB
      DEFAULT_THREAD_POOL_SIZE     = 4;
      DEFAULT_WORKER_TIMEOUT_MS    = 20000; // 20 seconds


    strict protected
      procedure startingWebServer(); override; // Used for intercept POST requests and to create processing chain
      function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; override;

    public
      destructor Destroy; override;

      procedure setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
      function extractPythonFiles(const aZipFile: String): Boolean;

      procedure Activate();

  end;


implementation

uses
  ULBLogger, fpjson, Zipper, uPyBridge;


{ TLBWebPyBridgeApplication }

procedure TLBWebPyBridgeApplication.startingWebServer();
begin
  LBLogger.Write(5, 'TLBWebPyBridgeApplication.startingWebServer', lmt_Debug, 'Capturing OnPOSTRequest callback');
  FOrchestratorModule := TPyBridgeChainModule.Create;
  FWebServer.OnPOSTRequest := @FOrchestratorModule.ProcessPOSTRequest;
end;

destructor TLBWebPyBridgeApplication.Destroy;
begin
  if FOrchestratorModule <> nil then
    FreeAndNil(FOrchestratorModule);

  inherited Destroy;
end;


function TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
begin
  Result := False;
  anErrorMsg := '';

  try

    FConfig.SharedMemorySize := aFile.ReadInt64(INI_SECTION_WEBPYBRIDGE, INI_KEY_SHARED_MEMORY_SIZE, DEFAULT_SHARED_MEMORY_SIZE);
    FConfig.ThreadPoolSize   := aFile.ReadInteger(INI_SECTION_WEBPYBRIDGE, INI_KEY_THREAD_POOL_SIZE, DEFAULT_THREAD_POOL_SIZE);
    FConfig.WorkerTimeoutMs  := aFile.ReadInteger(INI_SECTION_WEBPYBRIDGE, INI_KEY_WORKER_TIMEOUT, DEFAULT_WORKER_TIMEOUT_MS);

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

function TLBWebPyBridgeApplication.extractPythonFiles(const aZipFile: String): Boolean;
var
  _unzip: TUnZipper;
begin
  Result := False;

  if (aZipFile <> '') and FileExists(aZipFile) then
  begin

    _unzip := TUnZipper.Create;
    try
      _unzip.FileName := aZipFile;
      _unzip.OutputPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder;
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

procedure TLBWebPyBridgeApplication.Activate();
begin
  if FOrchestratorModule <> nil then
    FOrchestratorModule.setOrchestratorParams(FConfig.ThreadPoolSize, FConfig.SharedMemorySize, FConfig.WorkerTimeoutMs);
end;


end.

