unit uPyBridge;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, uLBBaseThread, uSharedMemoryManagement, uTimedoutCriticalSection;

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

      procedure cleanupSharedMemory();
      function elaborateRequest(): Boolean;

      const
        cStartPythonElaborator = Byte(19);
        cPythonElaborationTerminated = Byte(175);

    protected
      procedure InternalExecute(); override;

    public
      constructor Create; override;
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

      FBridges : array of TPyBridge;

      function hasActiveBridges(): Boolean;

      procedure clearRequests();
      procedure processRequests();
      procedure setBridgeAsAvailable(Sender: TObject);
      procedure removeFromAvailableBridges(Sender: TObject);

    public
      constructor Create(aBridgesCount: Integer; aSharedMemorySize: Cardinal); reintroduce;
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
  ULBLogger, process;

const
  cStartingValue          = Integer(1975);
  cSleepTime              = Integer(10);


var
  gv_BridgeCounter : Integer = 0;

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

      // LBLogger.Write(5, 'TPyBridge.elaborateRequest', lmt_Debug, 'Start python by trigger (value %d)', [cStartPythonElaborator]);
      _RequestHeader^.Trigger := cStartPythonElaborator;

      while not Self.Terminated do
      begin
        if _AnswerHeader^.Trigger = cPythonElaborationTerminated then
        begin
          FRequest^.Success := _AnswerHeader^.Successful = 1;
          FRequest^.Aborted := False;
          FRequest^.PayloadLen := _AnswerHeader^.PayloadLen;
          if _AnswerHeader^.PayloadLen = 0 then
            FRequest^.Payload := nil
          else
            FRequest^.Payload := pByte(FSharedMem^.mem) + SizeOf(TAnswerHeader);
          Result := True;
          Break;
        end
        else
          Self.PauseFor(cSleepTime);
      end;
    end;

  end
  else
    LBLogger.Write(1, 'TPyBridge.elaborateRequest', lmt_Warning, 'No script file!');


  RTLEventSetEvent(FRequest^.TerminateEvent);
  FRequest := nil;
end;

procedure TPyBridge.InternalExecute();
var
  _Path : String;
  _Process : TProcess;

begin
  if FSharedMem <> nil then
  begin
    FScriptsPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder + PathDelim;

    _Path := FScriptsPath + cPythonMainWorker;
    if FileExists(_Path) then
    begin
      // Starting worker.py and passing it shared memory code and size
      _Process := TProcess.Create(nil);
      _Process.Executable := 'python3';
      _Process.Parameters.Add(_Path);
      _Process.Parameters.Add(IntToStr(FSharedMem^.key));
      _Process.Parameters.Add(IntToStr(FSharedMem^.size));
      _Process.Options := [poNoConsole, poDetached];
      _Process.CurrentDirectory := FScriptsPath;
      _Process.Execute;

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
        if _Process <> nil then
          _Process.Terminate(0);

      except
        on E: Exception do
          LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Error, 'Error terminating process: ' + E.Message);
      end;

      if _Process <> nil then
        _Process.Free;

    end
    else
      LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Warning, 'File worker <%s> not found, terminating thread', [_Path]);
  end
  else
    LBLogger.Write(1, 'TPyBridge.InternalExecute', lmt_Warning, 'Shared memory not initialized!');

end;

constructor TPyBridge.Create;
begin
  inherited Create;

  FreeOnTerminate := True;
  FThreadNum := InterlockedIncrement(gv_BridgeCounter);
  FRequest := nil;
  FCSRequest := TTimedOutCriticalSection.Create;
end;

destructor TPyBridge.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FCSRequest);
  Self.cleanupSharedMemory();
end;

function TPyBridge.prepareSharedMemory(aSize: Cardinal): Boolean;
begin
  Result := False;
  Self.cleanupSharedMemory();

  if aSize > 0 then
  begin
    FSharedMem := AllocateSharedMemory(cStartingValue + FThreadNum, aSize);
    Result := FSharedMem <> nil;
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

procedure TBridgeOrchestrator.removeFromAvailableBridges(Sender: TObject);
var
  _Idx : Integer;

begin
  if FCS.Acquire('TBridgeOrchestrator.removeFromAvailableBridges') then
  begin
    try

      _Idx := FAvailableElaborators.IndexOf(Sender);
      if _Idx = -1 then
        FAvailableElaborators.Delete(_Idx);

    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeOrchestrator.removeFromAvailableBridges', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;
end;

constructor TBridgeOrchestrator.Create(aBridgesCount: Integer; aSharedMemorySize: Cardinal);
var
  i : Integer;

begin
  inherited Create;

  FRequests := TList.Create;
  FAvailableElaborators := TList.Create;

  SetLength(FBridges, aBridgesCount);
  for i := 0 to High(FBridges) do
  begin
    FBridges[i] := TPyBridge.Create;
    if FBridges[i].prepareSharedMemory(aSharedMemorySize) then
    begin
      FBridges[i].AddReference(@FBridges[i]);
      FBridges[i].OnElaborationTerminated := @Self.setBridgeAsAvailable;
      FBridges[i].OnAsyncTerminate := @Self.removeFromAvailableBridges;
      FAvailableElaborators.Add(FBridges[i]);
    end
    else
      FreeAndNil(FBridges[i]);
  end;

  FCS := TTimedOutCriticalSection.Create;

  for i := 0 to High(FBridges) do
  begin
    if FBridges[i] <> nil then
      FBridges[i].Start();
  end;
end;

destructor TBridgeOrchestrator.Destroy;
var
  i : Integer;

begin
  try

    for i := 0 to High(FBridges) do
    begin
      if FBridges[i] <> nil then
      begin
        FBridges[i].OnAsyncTerminate := nil;
        FBridges[i].OnElaborationTerminated := nil;
        FBridges[i].Terminate;
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

