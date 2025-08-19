unit uBaseBridgeManager;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, uLBBaseThread, uTimedoutCriticalSection, process,
  uHTTPRequestParser, IniFiles;

type

  { TRequestData }

  TRequestData = record
    // Input
    Script: String;
    HTTPParser : THTTPRequestParser;

    TerminateEvent : PRTLEvent;

    // Output
    Aborted : Boolean;
    Success : Boolean;
    Payload : pByte;
    PayloadLen : Cardinal;

    procedure initialize();
    procedure finalize();
  end;
  pRequestData = ^TRequestData;

  TBaseBridge = class;
  TBaseBridgeClass = class of TBaseBridge;

  { TBridgeConfigParams }

  TBridgeConfigParams = class(TObject)
    const
      INI_KEY_SHARED_MEMORY_SIZE   = 'SharedMemorySize';
      INI_KEY_THREAD_POOL_SIZE     = 'ThreadPoolSize';
      INI_KEY_WORKER_TIMEOUT       = 'WorkerTimeout';
      INI_KEY_SCRIPTS_FOLDER       = 'ScriptsFolder';
      INI_KEY_WORKER_FILE          = 'WorkerFilename';
      INI_KEY_SOCKET_FILE          = 'SocketFilename';

      DEFAULT_SHARED_MEMORY_SIZE   = 0;
      DEFAULT_THREAD_POOL_SIZE     = 0;
      DEFAULT_WORKER_TIMEOUT_MS    = 0;
      DEFAULT_SCRIPTS_FOLDER       = '';
      DEFAULT_WORKER_FILE          = '';
      DEFAULT_SOCKET_FILE          = '';


    var
      BridgeClass     : TBaseBridgeClass;
      SharedMemSize   : Cardinal;
      WorkerTimeoutMs : Cardinal;
      ScriptsFolder   : String;
      ThreadPoolSize  : Integer;
      WorkerFilename  : String;
      SocketFile      : String;

    function Completed(): Boolean;
    procedure Clear();
    function LoadFromINIFile(aINIFile: TIniFile; aINISection: String): Boolean;
  end;


  { TBaseBridge }

  TBaseBridge = class(TLBBaseThread)
    strict protected
      type
        TRequestHeader = packed record
          ScriptNameLen : Byte;
          URIParamsLen  : Word;
          HeadersLen    : Word;
          PayloadLen    : Cardinal;
        end;
        pRequestHeader = ^TRequestHeader;

        TAnswerHeader = packed record
          Successful : Byte;     // 0 = false, 1 = true
          PayloadLen : Cardinal; // Answer len
        end;
        pAnswerHeader = ^TAnswerHeader;

      var
        FOnElaborationTerminated : TNotifyEvent;
        FConfigParams            : TBridgeConfigParams;
        FCSRequest               : TTimedOutCriticalSection;
        FRequest                 : pRequestData;
        FWorkerCrashed           : Boolean;
        FThreadNum               : Integer;
        FNewRequestAvailable     : PRTLEvent;
        FErrorMessage            : String;

    public
      constructor Create(Params: TBridgeConfigParams); reintroduce; virtual;
      destructor Destroy; override;

      function insertRequest(aRequest: pRequestData): Boolean;

      function prepareIPCChannel(): Boolean; virtual; abstract;

      class function verifyParams(Params: TBridgeConfigParams): Boolean; virtual;


      property OnElaborationTerminated: TNotifyEvent write FOnElaborationTerminated;
  end;

  { TBridgeOrchestrator }

  TBridgeOrchestrator = class(TObject)
    strict private
      FConfigParams : TBridgeConfigParams;
      FCS : TTimedOutCriticalSection;
      FRequests : TList;
      FAvailableElaborators : TList;

      FWorkerScript : String;

      FTerminating : Boolean;

      FBridges : array of TBaseBridge;

      function hasActiveBridges(): Boolean;
      procedure CreateAndStartNewBridge(aBridgeIndex: Integer);

      procedure clearRequests();
      procedure processRequests();
      procedure setBridgeAsAvailable(Sender: TObject);
      procedure BridgeTerminated(Sender: TObject);

    public
      constructor Create(Params: TBridgeConfigParams); reintroduce;  // Params must be destroyed by the caller
      destructor Destroy; override;

      function insertRequest(aRequest: pRequestData): Boolean;
  end;

const
  cError_ScriptNotFound   = String('Script not found!');
  cError_UnmanagedError   = String('Internal error!');

implementation

uses
  ULBLogger {$IFDEF WINDOWS},Windows{$ENDIF};

var
  gv_BridgeCounter : Integer = 0;

{ TRequestData }

procedure TRequestData.initialize();
begin
  HTTPParser := nil;
  TerminateEvent := RTLEventCreate;
  Aborted := False;
  Success := False;
end;

procedure TRequestData.finalize();
begin
  RTLEventDestroy(TerminateEvent);
end;

{ TBaseBridge }

constructor TBaseBridge.Create(Params: TBridgeConfigParams);
begin
  inherited Create();

  FNewRequestAvailable := RTLEventCreate;
  FConfigParams := Params;
  FRequest := nil;
  FWorkerCrashed := False;
  FCSRequest := TTimedOutCriticalSection.Create;
  FThreadNum := InterlockedIncrement(gv_BridgeCounter);
end;

destructor TBaseBridge.Destroy;
begin
  RTLEventSetEvent(FNewRequestAvailable);
  inherited Destroy;
  FreeAndNil(FCSRequest);
  RTLEventDestroy(FNewRequestAvailable);
end;

function TBaseBridge.insertRequest(aRequest: pRequestData): Boolean;
begin
  Result := False;

  if FCSRequest.Acquire('TBaseBridge.insertRequest') then
  begin
    if (FRequest = nil) and (aRequest <> nil) then
    begin
      FRequest := aRequest;
      Result := True;
    end;
    FCSRequest.Release();
    RTLEventSetEvent(FNewRequestAvailable);
  end;
end;

class function TBaseBridge.verifyParams(Params: TBridgeConfigParams): Boolean;
begin
  Result := True;
end;

{ TBridgeConfigParams }

function TBridgeConfigParams.Completed(): Boolean;
begin
  Result := False;
  if BridgeClass <> nil then
  begin
    if WorkerFilename <> '' then
    begin
      if (ScriptsFolder <> '') then
      begin
        if not DirectoryExists(ScriptsFolder) then
          ForceDirectories(ScriptsFolder);

        ScriptsFolder := IncludeTrailingPathDelimiter(ScriptsFolder);
        Result := (WorkerTimeoutMs > 0) and (ThreadPoolSize > 0);
        if Result then
          Result := BridgeClass.VerifyParams(Self)
        else
          LBLogger.Write(1, 'TBridgeConfigParams.Completed', lmt_Warning, 'Wrong parameters! Timeout: %d  -  Bridges count: %d', [WorkerTimeoutMs, ThreadPoolSize]);
      end
      else
        LBLogger.Write(1, 'TBridgeConfigParams.Completed', lmt_Warning, 'Scripts folder not set!');
    end
    else
      LBLogger.Write(1, 'TBridgeConfigParams.Completed', lmt_Warning, 'Worker script not set!');
  end
  else
    LBLogger.Write(1, 'TBridgeConfigParams.Completed', lmt_Warning, 'Bridge class type not set!');
end;

procedure TBridgeConfigParams.Clear();
begin
  SharedMemSize   := 0;
  WorkerTimeoutMs := 0;
  ScriptsFolder   := '';
  ThreadPoolSize  := 0;
  WorkerFilename  := '';
  SocketFile      := '';
end;

function TBridgeConfigParams.LoadFromINIFile(aINIFile: TIniFile; aINISection: String): Boolean;
begin
  Result := False;
  Self.Clear();

  if (aINIFile <> nil) and (aINISection <> '') then
  begin

    SharedMemSize   := aINIFile.ReadInt64   (aINISection, INI_KEY_SHARED_MEMORY_SIZE, DEFAULT_SHARED_MEMORY_SIZE);
    ThreadPoolSize  := aINIFile.ReadInteger (aINISection, INI_KEY_THREAD_POOL_SIZE,   DEFAULT_THREAD_POOL_SIZE);
    WorkerTimeoutMs := aINIFile.ReadInteger (aINISection, INI_KEY_WORKER_TIMEOUT,     DEFAULT_WORKER_TIMEOUT_MS);
    ScriptsFolder   := aINIFile.ReadString  (aINISection, INI_KEY_SCRIPTS_FOLDER,     DEFAULT_SCRIPTS_FOLDER);
    WorkerFilename  := aINIFile.ReadString  (aINISection, INI_KEY_WORKER_FILE,        DEFAULT_WORKER_FILE);
    SocketFile      := aINIFile.ReadString  (aINISection, INI_KEY_SOCKET_FILE,        DEFAULT_SOCKET_FILE);

    if ScriptsFolder <> '' then
      ScriptsFolder := ExpandFileName(ScriptsFolder);

    Result := True;
  end
  else
    LBLogger.Write(1, 'TBridgeConfigParams.LoadFromINIFile', lmt_Warning, 'No INI file or section!');
end;


{ TBridgeOrchestrator }

function TBridgeOrchestrator.hasActiveBridges(): Boolean;
var
  i : Integer;

begin
  Result := False;
  if Length(FBridges) > 0 then
  begin
    for i := 0 to High(FBridges) do
    begin
      if FBridges[i] <> nil then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TBridgeOrchestrator.clearRequests;
var
  i : Integer;
  _Req : pRequestData;

begin
  if FCS.Acquire('TBridgeOrchestrator.freeRequests') then
  begin
    try
      for i := 0 to FRequests.Count - 1 do
      begin
        _Req := pRequestData(FRequests.Items[i]);
        _Req^.Success := False;
        _Req^.Aborted := True;
        _Req^.PayloadLen := 0;
        RTLEventSetEvent(_Req^.TerminateEvent);
      end;

      FRequests.Clear;
    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeOrchestrator.Destroy', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

procedure TBridgeOrchestrator.processRequests();
var
  _Request : pRequestData;
  _Bridge : TBaseBridge;

begin
  if FCS.Acquire('TBridgeOrchestrator.processRequests') then
  begin
    try

      while (FRequests.Count > 0) and (FAvailableElaborators.Count > 0) do
      begin
        _Request := FRequests.Items[0];
        _Bridge := TBaseBridge(FAvailableElaborators.Items[0]);
        FRequests.Delete(0);
        FAvailableElaborators.Delete(0);

        if not _Bridge.insertRequest(_Request) then
        begin
          _Request^.Success := False;
          _Request^.Aborted := True;
          RTLEventSetEvent(_Request^.TerminateEvent);
        end;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeOrchestrator.processRequests', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;
end;

procedure TBridgeOrchestrator.setBridgeAsAvailable(Sender: TObject);
var
  _Idx : Integer;

begin
  if FCS.Acquire('TBridgeOrchestrator.setBridgeAsAvailable') then
  begin
    try

      _Idx := FAvailableElaborators.IndexOf(Sender);
      if _Idx = -1 then
      begin
        FAvailableElaborators.Add(Sender);
        Self.processRequests();
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeOrchestrator.setBridgeAsAvailable', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;
end;

procedure TBridgeOrchestrator.BridgeTerminated(Sender: TObject);
var
  i, _Idx : Integer;
begin
  if FCS.Acquire('TBridgeOrchestrator.BridgeTerminated') then
  begin
    try
      // Remove from available list if it's there
      _Idx := FAvailableElaborators.IndexOf(Sender);
      if _Idx > -1 then
        FAvailableElaborators.Delete(_Idx);

      // Find in main array, nil it out, and restart it
      for i := 0 to High(FBridges) do
      begin
        if FBridges[i] = Sender then
        begin
          LBLogger.Write(1, 'TBridgeOrchestrator.BridgeTerminated', lmt_Warning, 'Bridge %d has terminated. Restarting.', [i]);
          FBridges[i].RemoveReference(@FBridges[i]);
          FBridges[i] := nil; // The TMultiReferenceObject will free the instance
          Self.CreateAndStartNewBridge(i);
          break;
        end;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeOrchestrator.BridgeTerminated', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;
end;

procedure TBridgeOrchestrator.CreateAndStartNewBridge(aBridgeIndex: Integer);
begin
  if not FTerminating then
  begin
    // Start only if worker.py exixts
    if FileExists(FWorkerScript) then
    begin
      if (aBridgeIndex >= 0) and (aBridgeIndex <= High(FBridges)) then
      begin
        FBridges[aBridgeIndex] := FConfigParams.BridgeClass.Create(FConfigParams);
        if FBridges[aBridgeIndex].prepareIPCChannel() then
        begin
          FBridges[aBridgeIndex].AddReference(@FBridges[aBridgeIndex]);
          FBridges[aBridgeIndex].OnElaborationTerminated := @Self.setBridgeAsAvailable;
          FBridges[aBridgeIndex].OnAsyncTerminate := @Self.BridgeTerminated;
          FAvailableElaborators.Add(FBridges[aBridgeIndex]);
          FBridges[aBridgeIndex].Start();
        end
        else
        begin
          LBLogger.Write(1, 'TBridgeOrchestrator.CreateAndStartNewBridge', lmt_Warning, 'Failed to create bridge %d.', [aBridgeIndex]);
          FreeAndNil(FBridges[aBridgeIndex]);
        end;
      end;
    end
    else
      LBLogger.Write(1, 'TBridgeOrchestrator.CreateAndStartNewBridge', lmt_Warning, 'Worker <%s> not found. Bridge %d not created', [FWorkerScript, aBridgeIndex]);
  end;

end;

constructor TBridgeOrchestrator.Create(Params: TBridgeConfigParams);
var
  i : Integer;

begin
  inherited Create;
  FTerminating := False;
  FConfigParams := Params;

  FWorkerScript := IncludeTrailingPathDelimiter(FConfigParams.ScriptsFolder) + FConfigParams.WorkerFilename;

  FRequests := TList.Create;
  FAvailableElaborators := TList.Create;

  SetLength(FBridges, FConfigParams.ThreadPoolSize);
  for i := 0 to High(FBridges) do
    Self.CreateAndStartNewBridge(i);

  FCS := TTimedOutCriticalSection.Create;
end;

destructor TBridgeOrchestrator.Destroy;
var
  i : Integer;

begin
  FTerminating := True;

  try

    for i := 0 to High(FBridges) do
    begin
      if FBridges[i] <> nil then
      begin
        FBridges[i].OnAsyncTerminate := nil;
        FBridges[i].OnElaborationTerminated := nil;
        FreeAndNil(FBridges[i]);
      end;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TBridgeOrchestrator.Destroy', lmt_Error, '1. Error destroying bridges: %s', [E.Message]);
  end;
  SetLength(FBridges, 0);

  try
    Self.clearRequests();
    FreeAndNil(FRequests);

    FreeAndNil(FCS);
    FreeAndNil(FAvailableElaborators);
  except
    on E: Exception do
      LBLogger.Write(1, 'TBridgeOrchestrator.Destroy', lmt_Error, '2. %s', [E.Message]);
  end;


  inherited Destroy;
end;

function TBridgeOrchestrator.insertRequest(aRequest: pRequestData): Boolean;
begin
  Result := False;

  if aRequest <> nil then
  begin
    aRequest^.Success := False;

    if Self.hasActiveBridges() then
    begin

      aRequest^.Aborted := False;

      if FCS.Acquire('TBridgeOrchestrator.insertRequest') then
      begin
        try

          FRequests.Add(aRequest);
          Result := True;

        finally
          FCS.Release();
        end;
      end;

      if Result then
        Self.processRequests;
    end
    else begin
      aRequest^.Aborted := True;
      aRequest^.Payload := @cError_UnmanagedError[1];
      aRequest^.PayloadLen := Length(cError_UnmanagedError);

      LBLogger.Write(1, 'TBridgeOrchestrator.insertRequest', lmt_Warning, 'Aborting: no bridges available!');
    end;
  end;
end;

end.


