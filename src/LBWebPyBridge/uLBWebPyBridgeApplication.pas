unit uLBWebPyBridgeApplication;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBApplicationBoostrap, IniFiles, uPyBridge;

type

  { TLBWebPyBridgeApplication }

  TLBWebPyBridgeApplication = class(TLBApplicationBoostrap)
    strict private
      type
        TConfigurationInfo = record
          SharedMemorySize : Cardinal;
          ThreadPoolSize   : Integer;
        end;

    strict private
      FConfig : TConfigurationInfo;
      FOrchestrator : TBridgeOrchestrator;

      function elaboratePOSTRequest(const Resource: String;
                                    const Headers: TStringList;
                                    const Payload: AnsiString;
                                    var ResponseHeaders: TStringList;
                                    var ResponseData: TMemoryStream;
                                    out ResponseCode: Integer
                                  ): Boolean;

    const
      INI_SECTION_WEBPYBRIDGE      = 'LBWebPyBridge';
      INI_KEY_SHARED_MEMORY_SIZE   = 'SharedMemorySize';
      INI_KEY_THREAD_POOL_SIZE     = 'ThreadPoolSize';
      DEFAULT_SHARED_MEMORY_SIZE   = 5 * 1024 * 1024; // 5MB
      DEFAULT_THREAD_POOL_SIZE     = 4;


    strict protected
      procedure startingWebServer(); override; // Used for intercept POST requests
      function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; override;

    public
      destructor Destroy; override;

      procedure setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer);
      function extractPythonFiles(const aZipFile: String): Boolean;

      procedure Activate();

  end;

const
  cInvalidAnswer = String('wpbError');
  cOperationAborted = String('Request aborted for internal error');

implementation

uses
  ULBLogger, fpjson, Zipper;

{ TLBWebPyBridgeApplication }

function TLBWebPyBridgeApplication.elaboratePOSTRequest(const Resource: String;
  const Headers: TStringList; const Payload: AnsiString;
  var ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out
  ResponseCode: Integer): Boolean;
var
  _Request : TRequestData;
  _ErrMsg : TJSONObject;
  _sErrMsg : String;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeApplication.elaboratePOSTRequest', lmt_Debug, 'Resource <%s>', [Resource]);

  // Called by THTTPRequestManager
  // Used to insert request into orchestrator and waiting for answer
  _Request.initialize();
  if Resource[1] = '/' then
    _Request.Script := Copy(Resource, 2, Length(Resource) - 1)
  else
    _Request.Script := Resource;

  if Payload <> '' then
  begin
    _Request.Params := @Payload[1];
    _Request.ParamsLen := Length(Payload);
  end;

  if FOrchestrator.insertRequest(@_Request) then
    RTLEventWaitFor(_Request.TerminateEvent)
  else
    LBLogger.Write(1, 'TLBWebPyBridgeApplication.elaboratePOSTRequest', lmt_Warning, 'Request <%s> not accepted', [Resource]);


  if not _Request.Aborted then
  begin
    if (_Request.Payload <> nil) and (_Request.PayloadLen > 0) then
    begin
      if ResponseData = nil then
        ResponseData := TMemoryStream.Create
      else
        ResponseData.Clear;

      ResponseData.Write(_Request.Payload^, _Request.PayloadLen);
    end;

  end
  else begin
    _ErrMsg := TJSONObject.Create();

    if (_Request.Payload <> nil) and (_Request.PayloadLen > 0) then
    begin
      SetLength(_sErrMsg, _Request.PayloadLen);
      Move(_Request.Payload^, _sErrMsg[1], _Request.PayloadLen);
      _ErrMsg.Add(cInvalidAnswer, _sErrMsg);
    end
    else
      _ErrMsg.Add(cInvalidAnswer, cOperationAborted);

    _ErrMsg.CompressedJSON := True;
    _sErrMsg := _ErrMsg.AsJSON;
    _ErrMsg.Free;

    if ResponseData = nil then
      ResponseData := TMemoryStream.Create
    else
      ResponseData.Clear;

    ResponseData.Write(_sErrMsg[1], Length(_sErrMsg));
  end;

  ResponseCode := 200;
  Result := True;
end;

procedure TLBWebPyBridgeApplication.startingWebServer();
begin
  LBLogger.Write(5, 'TLBWebPyBridgeApplication.startingWebServer', lmt_Debug, 'Capturing OnPOSTRequest callback');
  FWebServer.OnPOSTRequest := @Self.elaboratePOSTRequest;
end;

destructor TLBWebPyBridgeApplication.Destroy;
begin
  if FOrchestrator <> nil then
    FreeAndNil(FOrchestrator);

  inherited Destroy;
end;


function TLBWebPyBridgeApplication.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
begin
  Result := False;
  anErrorMsg := '';

  try

    FConfig.SharedMemorySize := aFile.ReadInt64(INI_SECTION_WEBPYBRIDGE, INI_KEY_SHARED_MEMORY_SIZE, DEFAULT_SHARED_MEMORY_SIZE);
    FConfig.ThreadPoolSize   := aFile.ReadInteger(INI_SECTION_WEBPYBRIDGE, INI_KEY_THREAD_POOL_SIZE, DEFAULT_THREAD_POOL_SIZE);

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

  if FOrchestrator <> nil then // Restarting
    Self.Activate();
end;

function TLBWebPyBridgeApplication.extractPythonFiles(const aZipFile: String
  ): Boolean;
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
  if FOrchestrator <> nil then
  begin
    LBLogger.Write(5, 'TLBWebPyBridgeApplication.Activate', lmt_Debug, 'Destroying old orchestrator ...');
    FreeAndNil(FOrchestrator);
  end;

  if (FConfig.SharedMemorySize > 0) and (FConfig.ThreadPoolSize > 0) then
    FOrchestrator := TBridgeOrchestrator.Create(FConfig.ThreadPoolSize, FConfig.SharedMemorySize)
  else
    LBLogger.Write(1, 'TLBWebPyBridgeApplication.Activate', lmt_Warning, 'Wrong values for the thread pool size or for the shared memory size!');

end;


end.

