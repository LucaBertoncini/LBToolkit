unit uPythonBridge;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uBaseBridgeManager, uIPCUtils, process, uLBFileUtils;

type
  { TPythonBridge }

  TPythonBridge = class(TBaseBridge)
    strict private
      FSharedMem   : pSharedMemory;
      FProcess     : TProcess;
      FRequestSem  : TLBNamedSemaphore;
      FResponseSem : TLBNamedSemaphore;

      procedure cleanupSharedMemory();
      function elaborateRequest(): Boolean;

    protected
      procedure InternalExecute(); override;

    public
      constructor Create(Params: TBridgeConfigParams); override;
      destructor Destroy; override;

      class function verifyParams(Params: TBridgeConfigParams): Boolean; override;

      function prepareIPCChannel(): Boolean; override;
  end;

const
  cPythonScriptsSubfolder = String('pyScripts');
  cPythonMainWorker       = String('worker.py');


  PythonFiles : array [0 .. 8] of TResourceFileInfo = (
    (Filename: 'worker.py';                               Code: 'PY_WORKER'),
    (Filename: 'lb_logger.py';                            Code: 'PY_LB_LOGGER'),
    (Filename: 'test_launcher.py';                        Code: 'PY_TEST_LAUNCHER'),
    (Filename: 'LBBridge' + PathDelim + 'sem_sysv.py';    Code: 'PY_SEM_SYSV'),
    (Filename: 'LBBridge' + PathDelim + 'sem_win.py';     Code: 'PY_SEM_WIN'),
    (Filename: 'LBBridge' + PathDelim + 'bridge_base.py'; Code: 'PY_BRIDGE_BASE'),
    (Filename: 'LBBridge' + PathDelim + 'bridge_win.py';  Code: 'PY_BRIDGE_WIN'),
    (Filename: 'LBBridge' + PathDelim + 'bridge_sysv.py'; Code: 'PY_BRIDGE_SYSV'),
    (Filename: 'LBBridge' + PathDelim + '__init__.py';    Code: 'PY__INIT__')
  );



implementation

uses
  ULBLogger;

const
  cShmStartingValue = Integer(1975);
  cSemStartingValue = Integer(11975);
  cSleepTime        = Integer(10);


var
  gv_SemCounter : Integer = 0;


{ TPythonBridge }

procedure TPythonBridge.cleanupSharedMemory();
begin
  if FSharedMem <> nil then
  begin
    closeSharedMemory(FSharedMem);
    Dispose(FSharedMem);
    FSharedMem := nil;
  end;
end;

function TPythonBridge.elaborateRequest: Boolean;
var
  _RequestHeader : pRequestHeader;
  _AnswerHeader : pAnswerHeader;
  _Dest : pByte = nil;
  _tmpStr : String;

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

      _tmpStr := ChangeFileExt(FRequest^.Script, '.py');
      if FileExists(FConfigParams.ScriptsFolder + _tmpStr) then
      begin
        _RequestHeader^.ScriptNameLen := Byte(Length(_tmpStr));
        _Dest := pByte(FSharedMem^.mem) + SizeOf(TRequestHeader);
        Move(_tmpStr[1], _Dest^, _RequestHeader^.ScriptNameLen);
        Inc(_Dest, _RequestHeader^.ScriptNameLen);
      end
      else begin
        FRequest^.Aborted := True;

        FErrorMessage := 'Script file <' + FConfigParams.ScriptsFolder + _tmpStr + '> not found!';
        FRequest^.Payload := @FErrorMessage[1];
        FRequest^.PayloadLen := Length(FErrorMessage);
      end;

    except
      on E: Exception do
      begin
        _Dest := nil;
        FRequest^.Aborted := True;
        FRequest^.Payload := @cError_UnmanagedError[1];
        FRequest^.PayloadLen := Length(cError_UnmanagedError);
        LBLogger.Write(1, 'TPythonBridge.elaborateRequest', lmt_Error, 'Error writing file name <%s>!', [_tmpStr]);
      end;
    end;

    if _Dest <> nil then
    begin
      if FRequest^.HTTPParser <> nil then
      begin
        if (FRequest^.HTTPParser.Params <> nil) and (FRequest^.HTTPParser.Params.Count > 0) then
        begin
          FRequest^.HTTPParser.Params.TextLineBreakStyle := tlbsCRLF;
          _tmpStr := FRequest^.HTTPParser.Params.Text;
          _RequestHeader^.URIParamsLen := Word(Length(_tmpStr));
          Move(_tmpStr[1], _Dest^, _RequestHeader^.URIParamsLen);
          Inc(_Dest, _RequestHeader^.URIParamsLen);
        end;

        if (FRequest^.HTTPParser.Headers.Count > 0) then
        begin
          FRequest^.HTTPParser.Headers.TextLineBreakStyle := tlbsCRLF;
          _tmpStr := FRequest^.HTTPParser.Headers.Text;
          _RequestHeader^.HeadersLen := Word(Length(_tmpStr));
          Move(_tmpStr[1], _Dest^, _RequestHeader^.HeadersLen);
          Inc(_Dest, _RequestHeader^.HeadersLen);
        end;

        if (FRequest^.HTTPParser.Body <> nil) and (FRequest^.HTTPParser.Body.Size > 0) then
        begin
          _RequestHeader^.PayloadLen := Cardinal(FRequest^.HTTPParser.Body.Size);
          Move(FRequest^.HTTPParser.Body.Memory^, _Dest^, _RequestHeader^.PayloadLen);
        end;

        // Signal Python worker that a request is ready
        LBLogger.Write(5, 'TPythonBridge.elaborateRequest', lmt_Debug, 'Activating semaphore for elaboration');
        FRequestSem.Signal;

        LBLogger.Write(5, 'TPythonBridge.elaborateRequest', lmt_Debug, 'Waiting response');
        // Wait for Python worker to signal completion
        if FResponseSem.Wait(FConfigParams.WorkerTimeoutMs) then
        begin
          LBLogger.Write(5, 'TPythonBridge.elaborateRequest', lmt_Debug, 'Answer received');
          FRequest^.Success := _AnswerHeader^.Successful = 1;
          FRequest^.Aborted := False;
          FRequest^.PayloadLen := _AnswerHeader^.PayloadLen;
          if _AnswerHeader^.PayloadLen = 0 then
            FRequest^.Payload := nil
          else
            FRequest^.Payload := pByte(FSharedMem^.mem) + SizeOf(TAnswerHeader);
          Result := True;
        end
        else begin // Timeout
          LBLogger.Write(1, 'TPythonBridge.elaborateRequest', lmt_Error, 'Python worker timed out. Terminating process.');
          if (FProcess <> nil) then FProcess.Terminate(1);
          FWorkerCrashed := True;
          FErrorMessage := 'Python worker timed out!';
          FRequest^.Payload := @FErrorMessage[1];
          FRequest^.PayloadLen := Length(FErrorMessage);
          FRequest^.Aborted := True;
        end;
      end
      else begin
        FErrorMessage := 'Missing HTTP parser!';
        FRequest^.Payload := @FErrorMessage[1];
        FRequest^.PayloadLen := Length(FErrorMessage);
        FRequest^.Aborted := True;
        LBLogger.Write(1, 'TPythonBridge.elaborateRequest', lmt_Warning, FErrorMessage);
      end;
    end;
  end
  else begin
    FErrorMessage := 'No script file!';
    FRequest^.Payload := @FErrorMessage[1];
    FRequest^.PayloadLen := Length(FErrorMessage);

    LBLogger.Write(1, 'TPythonBridge.elaborateRequest', lmt_Warning, FErrorMessage);
  end;

  RTLEventSetEvent(FRequest^.TerminateEvent);
  FRequest := nil;
end;

procedure TPythonBridge.InternalExecute;
var
  _Path : String;
begin
  if FSharedMem <> nil then
  begin
    if FConfigParams.ScriptsFolder <> '' then
    begin

      _Path := FConfigParams.ScriptsFolder + cPythonMainWorker;
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
        FProcess.CurrentDirectory := FConfigParams.ScriptsFolder;
        FProcess.Execute;

        while not Self.Terminated do
        begin
          if FCSRequest.Acquire('TPythonBridge.InternalExecute') then
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
                LBLogger.Write(1, 'TPythonBridge.InternalExecute', lmt_Error, 'Error elaborating request: %s', [E.Message]);
            end;
            FCSRequest.Release();
          end;
          RTLEventWaitFor(FNewRequestAvailable, cSleepTime);
        end;

        try
          if FProcess <> nil then
            FProcess.Terminate(0);

        except
          on E: Exception do
            LBLogger.Write(1, 'TPythonBridge.InternalExecute', lmt_Error, 'Error terminating process: ' + E.Message);
        end;

        if FProcess <> nil then
          FreeAndNil(FProcess);

      end
      else
        LBLogger.Write(1, 'TPythonBridge.InternalExecute', lmt_Warning, 'File worker <%s> not found, terminating thread', [_Path]);
    end
    else
      LBLogger.Write(1, 'TPythonBridge.InternalExecute', lmt_Warning, 'Scripts folder not set!');
  end
  else
    LBLogger.Write(1, 'TPythonBridge.InternalExecute', lmt_Warning, 'Shared memory not initialized!');

end;

constructor TPythonBridge.Create(Params: TBridgeConfigParams);
{$IFDEF Windows}
var
  semNameRequest, semNameResponse: string;
{$ENDIF}
begin
  inherited Create(Params);

  FreeOnTerminate := True;
  FProcess := nil;

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

destructor TPythonBridge.Destroy;
begin
  inherited Destroy;

  try
    FreeAndNil(FRequestSem);
    FreeAndNil(FResponseSem);
    Self.cleanupSharedMemory();

  except
    on E: Exception do
      LBLogger.Write(1, 'TPythonBridge.Destroy', lmt_Error, E.Message);
  end;
end;

class function TPythonBridge.verifyParams(Params: TBridgeConfigParams): Boolean;
begin
  Result := Params.SharedMemSize > 0;
  if not Result then
    LBLogger.Write(1, 'TPythonBridge.verifyParams', lmt_Warning, 'Shared memory size not set!');
end;

function TPythonBridge.prepareIPCChannel: Boolean;
{$IFDEF Unix}
var
  _Sem1, _Sem2 : Integer;
{$ENDIF}

begin
  Result := False;
  Self.cleanupSharedMemory();

  if FConfigParams.SharedMemSize > 0 then
  begin
    FSharedMem := AllocateSharedMemory(cShmStartingValue + FThreadNum, FConfigParams.SharedMemSize);
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



end.

