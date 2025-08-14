unit uPyBridge;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, uLBBaseThread, uIPCUtils, uTimedoutCriticalSection, process;

type

  { TRequestData }

  TRequestData = record
    // Input
    Script: String;
    Params: pByte;
    ParamsLen : Integer;

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


  { TPyBridge }

  TPyBridge = class(TLBBaseThread)
    strict private
      type
        TRequestHeader = packed record
          Trigger     : Byte;     // 19 = segnala richiesta esecuzione
          FilenameLen : Byte;     // Lunghezza nome del file
          ParamsLen   : Cardinal; // Lunghezza dei parametri
        end;
        pRequestHeader = ^TRequestHeader;

        TAnswerHeader = packed record
          Trigger    : Byte;     // 175 = elaborazione terminata
          Successful : Byte;     // 0 = false, 1 = true
          PayloadLen : Cardinal; // lunghezza risposta
        end;
        pAnswerHeader = ^TAnswerHeader;

    strict private
      FOnElaborationTerminated : TNotifyEvent;
      FScriptsPath : String;
      FSharedMem : pSharedMemory;
      FThreadNum : Integer;
      FCSRequest : TTimedOutCriticalSection;
      FRequest   : pRequestData;
      FProcess   : TProcess;
      FWorkerCrashed: Boolean;
      FRequestSem: TLBNamedSemaphore;
      FResponseSem: TLBNamedSemaphore;
      FWorkerTimeoutMs: Cardinal;

      procedure cleanupSharedMemory();
      function elaborateRequest(): Boolean;

      const
        cStartPythonElaborator = Byte(19);
        cPythonElaborationTerminated = Byte(175);

    protected
      procedure InternalExecute(); override;

    public
      constructor Create(aWorkerTimeoutMs: Cardinal; const ScriptsPath: String); reintroduce;
      destructor Destroy; override;

      function prepareSharedMemory(aSize: Cardinal): Boolean;
      function insertRequest(aRequest: pRequestData): Boolean;

      property OnElaborationTerminated: TNotifyEvent write FOnElaborationTerminated;
  end;

  { TBridgeOrchestrator }

  TBridgeOrchestrator = class(TObject)
    strict private
      FCS : TTimedOutCriticalSection;
      FRequests : TList;
      FAvailableElaborators : TList;
      FInitialBridgesCount: Integer;
      FInitialSharedMemorySize: Cardinal;
      FWorkerTimeoutMs: Integer;

      FWorkerScript : String;
      FScriptsFolder : String;

      FTerminating : Boolean;

      FBridges : array of TPyBridge;

      function hasActiveBridges(): Boolean;
      procedure CreateAndStartNewBridge(aBridgeIndex: Integer);

      procedure clearRequests();
      procedure processRequests();
      procedure setBridgeAsAvailable(Sender: TObject);
      procedure BridgeTerminated(Sender: TObject);

    public
      constructor Create(aBridgesCount: Integer; aSharedMemorySize: Cardinal; aWorkerTimeoutMs: Integer; const ScriptsFolder: String); reintroduce;
      destructor Destroy; override;

      function insertRequest(aRequest: pRequestData): Boolean;
  end;

const
  cPythonScriptsSubfolder = String('pyScripts');
  cPythonMainWorker       = String('worker.py');
  cError_ScriptNotFound   = String('Script not found!');
  cError_UnmanagedError   = String('Internal error!');

implementation

uses
  ULBLogger {$IFDEF WINDOWS},Windows{$ENDIF};

const
  cShmStartingValue = Integer(1975);
  cSemStartingValue = Integer(11975);
  cSleepTime        = Integer(10);


var
  gv_BridgeCounter : Integer = 0;
  gv_SemCounter : Integer = 0;

{ TRequestData }

procedure TRequestData.initialize();
begin
  Params := nil;
  ParamsLen := 0;
  TerminateEvent := RTLEventCreate;
  Aborted := False;
  Success := False;
end;

procedure TRequestData.finalize();
begin
  RTLEventDestroy(TerminateEvent);
end;


{ TPyBridge }

procedure TPyBridge.cleanupSharedMemory();
begin
  if FSharedMem <> nil then
  begin
    closeSharedMemory(FSharedMem);
    Dispose(FSharedMem);
    FSharedMem := nil;
  end;
end;

function TPyBridge.elaborateRequest: Boolean;
var
  _RequestHeader : pRequestHeader;
  _AnswerHeader : pAnswerHeader;
  _Dest : pByte = nil;
  _ScriptFile : String;
begin
  Result := False;
  _RequestHeader := FSharedMem^.mem;
  _AnswerHeader := FSharedMem^.mem;

  FRequest^.Success := False;
  FRequest^.Aborted := False;
  FRequest^.Payload := @cError_ScriptNotFound[1];
  FRequest^.PayloadLen := Length(cError_ScriptNotFound);

  FillChar(_RequestHeader^, SizeOf(TRequestHeader), 0);

  if FRequest^.Script <> '' then
  begin
    try
      _ScriptFile := ChangeFileExt(FRequest^.Script, '.py');
      if FileExists(FScriptsPath + _ScriptFile) then
      begin
        _RequestHeader^.FilenameLen := Byte(Length(_ScriptFile));
        _Dest := pByte(FSharedMem^.mem) + SizeOf(TRequestHeader);
        Move(_ScriptFile[1], _Dest^, _RequestHeader^.FilenameLen);
        Inc(_Dest, _RequestHeader^.FilenameLen);
      end
      else
        FRequest^.Aborted := True;
    except
      on E: Exception do
      begin
        _Dest := nil;
        FRequest^.Aborted := True;
        FRequest^.Payload := @cError_UnmanagedError[1];
        FRequest^.PayloadLen := Length(cError_UnmanagedError);
        LBLogger.Write(1, 'TPyBridge.elaborateRequest', lmt_Error, 'Error writing file name <%s>!', [_ScriptFile]);
      end;
    end;

    if _Dest <> nil then
    begin
      if FRequest^.ParamsLen > 0 then
      begin
        _RequestHeader^.ParamsLen := FRequest^.ParamsLen;
        Move(FRequest^.Params^, _Dest^, FRequest^.ParamsLen);
      end;

      // Signal Python worker that a request is ready
      LBLogger.Write(5, 'TPyBridge.elaborateRequest', lmt_Debug, 'Activating semaphore for elaboration');
      FRequestSem.Signal;

      LBLogger.Write(5, 'TPyBridge.elaborateRequest', lmt_Debug, 'Waiting response');
      // Wait for Python worker to signal completion
      if FResponseSem.Wait(FWorkerTimeoutMs) then
      begin
        LBLogger.Write(5, 'TPyBridge.elaborateRequest', lmt_Debug, 'Answer received');
        // if _AnswerHeader^.Trigger = cPythonElaborationTerminated then
        //begin
          FRequest^.Success := _AnswerHeader^.Successful = 1;
          FRequest^.Aborted := False;
          FRequest^.PayloadLen := _AnswerHeader^.PayloadLen;
          if _AnswerHeader^.PayloadLen = 0 then
            FRequest^.Payload := nil
          else
            FRequest^.Payload := pByte(FSharedMem^.mem) + SizeOf(TAnswerHeader);
          Result := True;
        //end;
      end
      else begin // Timeout
        LBLogger.Write(1, 'TPyBridge.elaborateRequest', lmt_Error, 'Python worker timed out. Terminating process.');
        if (FProcess <> nil) then FProcess.Terminate(1);
        FWorkerCrashed := True;
        FRequest^.Aborted := True;
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TPyBridge.elaborateRequest', lmt_Warning, 'No script file!');

  RTLEventSetEvent(FRequest^.TerminateEvent);
  FRequest := nil;
end;

procedure TPyBridge.InternalExecute;
var
  _Path : String;
begin
  if FSharedMem <> nil then
  begin
    if FScriptsPath = '' then
      FScriptsPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder + PathDelim;

    _Path := FScriptsPath + cPythonMainWorker;
    if FileExists(_Path) then
    begin
      // Starting worker.py and passing it shared memory code and size
      FProcess := TProcess.Create(nil);
      FProcess.Executable := 'python3';
      FProcess.Parameters.Add(_Path);
      FProcess.Parameters.Add(IntToStr(FSharedMem^.key));
      FProcess.Parameters.Add(IntToStr(FSharedMem^.size));
      {$IFDEF WINDOWS}
      FProcess.Parameters.Add(FRequestSem.Name);
      FProcess.Parameters.Add(FResponseSem.Name);
      {$ELSE}
      FProcess.Parameters.Add(IntToStr(FRequestSem.Key));
      FProcess.Parameters.Add(IntToStr(FResponseSem.Key));
      {$ENDIF}
      FProcess.Options := [poNoConsole, poDetached];
      FProcess.CurrentDirectory := FScriptsPath;
      FProcess.Execute;

      while not Self.Terminated do
      begin
        if FCSRequest.Acquire('TPyBridge.InternalExecute') then
        begin
          try
            if FRequest <> nil then
            begin
              Self.elaborateRequest();
              if Assigned(FOnElaborationTerminated) then
                FOnElaborationTerminated(Self);
              if FWorkerCrashed then
                Break; // Exit loop to terminate thread
            end;
          except
            on E: Exception do
              LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Error, 'Error elaborating request: %s', [E.Message]);
          end;
          FCSRequest.Release();
        end;
        Self.PauseFor(cSleepTime);
      end;

      try
        if FProcess <> nil then
          FProcess.Terminate(0);

      except
        on E: Exception do
          LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Error, 'Error terminating process: ' + E.Message);
      end;

      if FProcess <> nil then
        FProcess.Free;

    end
    else
      LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Warning, 'File worker <%s> not found, terminating thread', [_Path]);
  end
  else
    LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Warning, 'Shared memory not initialized!');

end;

constructor TPyBridge.Create(aWorkerTimeoutMs: Cardinal; const ScriptsPath: String);
{$IFDEF Windows}
var
  semNameRequest, semNameResponse: string;
{$ENDIF}
begin
  inherited Create;

  FScriptsPath := IncludeTrailingPathDelimiter(ScriptsPath);

  FreeOnTerminate := True;
  FThreadNum := InterlockedIncrement(gv_BridgeCounter);
  FRequest := nil;
  FProcess := nil;
  FWorkerCrashed := False;
  FWorkerTimeoutMs := aWorkerTimeoutMs;
  FCSRequest := TTimedOutCriticalSection.Create;

  {$IFDEF WINDOWS}
  semNameRequest := 'pybridge_req_sem_' + IntToStr(GetCurrentProcessId()) + '_' + IntToStr(FThreadNum);
  semNameResponse := 'pybridge_res_sem_' + IntToStr(GetCurrentProcessId()) + '_' + IntToStr(FThreadNum);
  FRequestSem := TLBNamedSemaphore.Create(semNameRequest, 0);
  FResponseSem := TLBNamedSemaphore.Create(semNameResponse, 0);
  {$ELSE}
  // On Linux, we need the shared memory to be prepared first to get the key.
  // The semaphores will be created in prepareSharedMemory.
  FRequestSem := nil;
  FResponseSem := nil;
  {$ENDIF}
end;

destructor TPyBridge.Destroy;
begin
  inherited Destroy;

  try
    FreeAndNil(FCSRequest);
    FreeAndNil(FRequestSem);
    FreeAndNil(FResponseSem);
    Self.cleanupSharedMemory();

  except
    on E: Exception do
      LBLogger.Write(1, 'TPyBridge.Destroy', lmt_Error, E.Message);
  end;
end;

function TPyBridge.prepareSharedMemory(aSize: Cardinal): Boolean;
{$IFDEF Unix}
var
  _Sem1, _Sem2 : Integer;
{$ENDIF}

begin
  Result := False;
  Self.cleanupSharedMemory();

  if aSize > 0 then
  begin
    FSharedMem := AllocateSharedMemory(cShmStartingValue + FThreadNum, aSize);
    Result := FSharedMem <> nil;
    {$IFNDEF WINDOWS}
    if Result then
    begin
      // On Linux, create semaphores after shared memory to have a key.
      if FRequest <> nil then
        FreeAndNil(FRequestSem);

      if FResponseSem <> nil then
        FreeAndNil(FResponseSem);

      _Sem1 := cSemStartingValue + InterlockedIncrement(gv_SemCounter);
      _Sem2 := cSemStartingValue + InterlockedIncrement(gv_SemCounter);

      FRequestSem := TLBNamedSemaphore.Create('', _Sem1, True);   // Red semaphore
      FResponseSem := TLBNamedSemaphore.Create('', _Sem2, True);  // Red semaphore
    end;
    {$ENDIF}
  end;
end;

function TPyBridge.insertRequest(aRequest: pRequestData): Boolean;
begin
  Result := False;

  if FCSRequest.Acquire('TPyBridge.insertRequest') then
  begin
    if (FRequest = nil) and (aRequest <> nil) then
    begin
      FRequest := aRequest;
      Result := True;
    end;
    FCSRequest.Release();
  end;
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
  _Bridge : TPyBridge;

begin
  if FCS.Acquire('TBridgeOrchestrator.processRequests') then
  begin
    try

      while (FRequests.Count > 0) and (FAvailableElaborators.Count > 0) do
      begin
        _Request := FRequests.Items[0];
        _Bridge := TPyBridge(FAvailableElaborators.Items[0]);
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
        FBridges[aBridgeIndex] := TPyBridge.Create(FWorkerTimeoutMs, FScriptsFolder);
        if FBridges[aBridgeIndex].prepareSharedMemory(FInitialSharedMemorySize) then
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

constructor TBridgeOrchestrator.Create(aBridgesCount: Integer; aSharedMemorySize: Cardinal; aWorkerTimeoutMs: Integer; const ScriptsFolder: String);
var
  i : Integer;

begin
  inherited Create;
  FTerminating := False;

  FScriptsFolder := ScriptsFolder;
  if FScriptsFolder = '' then
  begin
    FScriptsFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder;
    LBLogger.Write(1, 'TBridgeOrchestrator.Create', lmt_Debug, 'No script folder set, using default <%s>', [FScriptsFolder]);
  end;

  FWorkerScript := IncludeTrailingPathDelimiter(FScriptsFolder) + cPythonMainWorker;

  FInitialBridgesCount := aBridgesCount;
  FInitialSharedMemorySize := aSharedMemorySize;
  FWorkerTimeoutMs := aWorkerTimeoutMs;

  FRequests := TList.Create;
  FAvailableElaborators := TList.Create;

  SetLength(FBridges, FInitialBridgesCount);
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
//        FBridges[i].Terminate;
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


