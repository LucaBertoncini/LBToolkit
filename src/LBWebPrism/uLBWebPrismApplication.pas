unit uLBWebPrismApplication;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBApplicationBoostrap, IniFiles,
  uBridgeChainModule, uBaseBridgeManager, uPythonBridge, uNodeJSBridge;

type

  { TLBWebPrismApplication }

  TLBWebPrismApplication = class(TLBApplicationBoostrap)
    strict private
      FPyOrchestratorModule : TBridgeChainModule;
      FJsOrchestratorModule : TBridgeChainModule;
      FPyConfigParams : TBridgeConfigParams;
      FJsConfigParams : TBridgeConfigParams;

    const
      INI_SECTION_PYTHONBRIDGE     = 'PythonBridge';
      INI_SECTION_NODEJSBRIDGE     = 'NodeJSBridge';


    strict protected
      procedure startingWebServer(); override; // Used for intercept requests and to create processing chain
      function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; override;

    public
      destructor Destroy; override;

      procedure setPyOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
      procedure setJsOrchestratorParams(aThreadPoolSize: Integer);

      function extractPythonFilesFromResources(): Boolean;
      function extractNodeJsFilesFromResources(): Boolean;

      procedure Activate();

  end;

(*
   INI File example

   [LBmicroWebServer]
   Port=10320

   [PythonBridge]
   ThreadPoolSize=2
   WorkerTimeout=10000
   ScriptsFolder=./pyScripts
   SharedMemorySize=1048576

   [NodeJSBridge]
   ThreadPoolSize=2
   WorkerTimeout=10000
   ScriptsFolder=./jsScripts

*)

implementation

uses
  ULBLogger, fpjson, uLBFileUtils;


{ TLBWebPrismApplication }

procedure TLBWebPrismApplication.startingWebServer();
begin

  if FPyConfigParams <> nil then
  begin
    LBLogger.Write(5, 'TLBWebPrismApplication.startingWebServer', lmt_Debug, 'Inserting Python orchestrator module');
    FPyOrchestratorModule := TBridgeChainModule.Create(); // Will be destroyed in WebServer
    FWebServer.addChainProcessor(FPyOrchestratorModule, True);
  end;

  if FJsConfigParams <> nil then
  begin
    LBLogger.Write(5, 'TLBWebPrismApplication.startingWebServer', lmt_Debug, 'Inserting JS orchestrator module');
    FJsOrchestratorModule := TBridgeChainModule.Create(); // Will be destroyed in WebServer
    FWebServer.addChainProcessor(FJsOrchestratorModule, False);
  end;
end;

function TLBWebPrismApplication.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
var
  _ConfigOK : Boolean;

begin
  Result := False;
  anErrorMsg := '';

  try
    _ConfigOK := False;

    if aFile.SectionExists(INI_SECTION_PYTHONBRIDGE) then
    begin

      if FPyConfigParams = nil then
      begin
        FPyConfigParams := TBridgeConfigParams.Create;
        FPyConfigParams.BridgeClass := TPythonBridge;
      end;

      if FPyConfigParams.LoadFromINIFile(aFile, INI_SECTION_PYTHONBRIDGE) then
      begin
        if FPyConfigParams.WorkerFilename = '' then
          FPyConfigParams.WorkerFilename := cPythonMainWorker;

        if FPyConfigParams.ScriptsFolder = '' then
          FPyConfigParams.ScriptsFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder;

        if not DirectoryExists(FPyConfigParams.ScriptsFolder) then
          ForceDirectories(FPyConfigParams.ScriptsFolder);

        _ConfigOK := FPyConfigParams.Completed();
      end;
    end;

    if (not _ConfigOK) and (FPyConfigParams <> nil) then
    begin
      LBLogger.Write(1, 'TLBWebPrismApplication.LoadConfigurationFromINIFileInternal', lmt_Warning, 'Error reading Python bridge configuration!');
      FreeAndNil(FPyConfigParams);
    end;

    _ConfigOK := False;
    if aFile.SectionExists(INI_SECTION_NODEJSBRIDGE) then
    begin

      if FJsConfigParams = nil then
      begin
        FJsConfigParams := TBridgeConfigParams.Create;
        FJsConfigParams.BridgeClass := TNodeJSBridge;
      end;

      if FJsConfigParams.LoadFromINIFile(aFile, INI_SECTION_NODEJSBRIDGE) then
      begin
        if FJsConfigParams.WorkerFilename = '' then
          FJsConfigParams.WorkerFilename := cNodeJSMainWorker;

        if FJsConfigParams.ScriptsFolder = '' then
          FJsConfigParams.ScriptsFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cNodeJsScriptsSubfolder;

        if not DirectoryExists(FJsConfigParams.ScriptsFolder) then
          ForceDirectories(FJsConfigParams.ScriptsFolder);

        _ConfigOK := FJsConfigParams.Completed();
      end;
    end;

    if (not _ConfigOK) and (FJsConfigParams <> nil) then
    begin
      LBLogger.Write(1, 'TLBWebPrismApplication.LoadConfigurationFromINIFileInternal', lmt_Warning, 'Error reading Node.js bridge configuration!');
      FreeAndNil(FJsConfigParams);
    end;

    Result := (FPyConfigParams <> nil) or (FJsConfigParams <> nil);
  except
    on E: Exception do
    begin
      anErrorMsg := Format('Exception while reading INI: %s', [E.Message]);
      LBLogger.Write(1, 'TLBWebPrismApplication.LoadConfigurationFromINIFileInternal', lmt_Error, anErrorMsg);
    end;
  end;
end;

destructor TLBWebPrismApplication.Destroy;
begin
  inherited Destroy;

  try
    if FPyConfigParams <> nil then
      FreeAndNil(FPyConfigParams);

    if FJsConfigParams <> nil then
      FreeAndNil(FJsConfigParams);

  except
    on E: Exception do
      LBLogger.Write(1, 'TLBWebPrismApplication.Destroy', lmt_Error, E.Message);
  end;
end;

procedure TLBWebPrismApplication.setPyOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
begin
  if aThreadPoolSize > 0 then
    FPyConfigParams.ThreadPoolSize := aThreadPoolSize;

  if aSharedMemorySize > 0 then
    FPyConfigParams.SharedMemSize := aSharedMemorySize;

  if FPyOrchestratorModule <> nil then // Restarting
    FPyOrchestratorModule.setOrchestratorParams(FPyConfigParams);
end;

procedure TLBWebPrismApplication.setJsOrchestratorParams(aThreadPoolSize: Integer);
begin
  if aThreadPoolSize > 0 then
    FJsConfigParams.ThreadPoolSize := aThreadPoolSize;

  if FJsOrchestratorModule <> nil then // Restarting
    FJsOrchestratorModule.setOrchestratorParams(FJsConfigParams);
end;

function TLBWebPrismApplication.extractNodeJsFilesFromResources: Boolean;
begin
  Result := False;

  if FJsConfigParams <> nil then
    Result := extractFilesFormResource(FJsConfigParams.ScriptsFolder, NodeJsFiles, Length(NodeJsFiles));
end;

function TLBWebPrismApplication.extractPythonFilesFromResources(): Boolean;
begin
  Result := False;

  if FPyConfigParams <> nil then
    Result := extractFilesFormResource(FPyConfigParams.ScriptsFolder, PythonFiles, Length(PythonFiles));
end;

procedure TLBWebPrismApplication.Activate();
begin
  if FPyOrchestratorModule <> nil then
    FPyOrchestratorModule.setOrchestratorParams(FPyConfigParams);

  if FJsOrchestratorModule <> nil then
    FJsOrchestratorModule.setOrchestratorParams(FJsConfigParams);
end;


end.

