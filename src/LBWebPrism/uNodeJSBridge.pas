unit uNodeJSBridge;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uBaseBridgeManager, uIPCUtils, process, uLBFileUtils;

type

  { TNodeJSBridge }

  TNodeJSBridge = class(TBaseBridge)
    strict private
      FProcess     : TProcess;
      FServerSocket : TLocalServerSocket;
      FPeer : TLocalSocket;
      FIPCFile : String;
      FReceivedBuffer : TBytes;

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
  cNodeJsMainWorker       = String('worker.js');
  cNodeJsScriptsSubfolder = String('jsScripts');

  NodeJsFiles : array [0 .. 3] of TResourceFileInfo = (
    (Filename: 'worker.js';        Code: 'JS_WORKER'),
    (Filename: 'bridge.js';        Code: 'JS_BRIDGE'),
    (Filename: 'logger.js';        Code: 'JS_LOGGER'),
    (Filename: 'test_launcher.js'; Code: 'JS_TEST_LAUNCHER')
  );

implementation

uses
  ULBLogger;

const
  cSleepTime = Integer(10);


{ TNodeJSBridge }

function TNodeJSBridge.elaborateRequest(): Boolean;
var
  _RequestHeader : TRequestHeader;
  _ScriptFilename : String;
  _URIParams : String;
  _Headers : String;
  _Received : Integer;
  _AnswerHeader : TAnswerHeader;
  _TimedOut : Boolean = False;

begin
  Result := False;

  FRequest^.Success := False;
  FRequest^.Aborted := False;
  FRequest^.Payload := @cError_ScriptNotFound[1];
  FRequest^.PayloadLen := Length(cError_ScriptNotFound);

  FillChar(_RequestHeader, SizeOf(TRequestHeader), 0);

  if FRequest^.Script <> '' then
  begin
    try

      _ScriptFilename := ChangeFileExt(FRequest^.Script, '.js');
      if FileExists(FConfigParams.ScriptsFolder + _ScriptFilename) then
      begin
        LBLogger.Write(5, 'TNodeJSBridge.elaborateRequest', lmt_Debug, 'Script needed <%s>', [FConfigParams.ScriptsFolder + _ScriptFilename]);
        _RequestHeader.ScriptNameLen := Byte(Length(_ScriptFilename));

        if (FRequest^.HTTPParser.Params <> nil) and (FRequest^.HTTPParser.Params.Count > 0) then
        begin
          FRequest^.HTTPParser.Params.TextLineBreakStyle := tlbsCRLF;
          _URIParams := FRequest^.HTTPParser.Params.Text;
          _RequestHeader.URIParamsLen := Word(Length(_URIParams));
        end;

        if (FRequest^.HTTPParser.Headers.Count > 0) then
        begin
          FRequest^.HTTPParser.Headers.TextLineBreakStyle := tlbsCRLF;
          _Headers := FRequest^.HTTPParser.Headers.Text;
          _RequestHeader.HeadersLen := Word(Length(_Headers));
        end;

        if (FRequest^.HTTPParser.Body <> nil) then
          _RequestHeader.PayloadLen := FRequest^.HTTPParser.Body.Size;

        FPeer.SendBuffer(@_RequestHeader, SizeOf(TRequestHeader));
        FPeer.SendBuffer(@_ScriptFilename[1], _RequestHeader.ScriptNameLen);
        if _RequestHeader.URIParamsLen > 0 then
          FPeer.SendBuffer(@_URIParams[1], _RequestHeader.URIParamsLen);
        if _RequestHeader.HeadersLen > 0 then
          FPeer.SendBuffer(@_Headers[1], _RequestHeader.HeadersLen);
        if _RequestHeader.PayloadLen > 0 then
          FPeer.SendBuffer(FRequest^.HTTPParser.Body.Memory, _RequestHeader.PayloadLen);

        // All data sent waiting answer
        _Received := FPeer.RecvBuffer(@_AnswerHeader, SizeOf(_AnswerHeader)); // Waiting 6 bytes: status is a word and payload len is a Cardinal
        if _Received = SizeOf(_AnswerHeader) then
        begin
          if _AnswerHeader.PayloadLen > 0 then
          begin
            LBLogger.Write(5, 'TNodeJSBridge.elaborateRequest', lmt_Debug, 'Received payload len: %d', [_AnswerHeader.PayloadLen]);

            SetLength(FReceivedBuffer, _AnswerHeader.PayloadLen);
            _Received := FPeer.RecvBuffer(@FReceivedBuffer[0], _AnswerHeader.PayloadLen);
            _Timedout := _Received < _AnswerHeader.PayloadLen;
          end;
        end
        else
          _TimedOut := True;

        if not _TimedOut then
        begin
          LBLogger.Write(5, 'TNodeJSBridge.elaborateRequest', lmt_Debug, 'Answer received');
          FRequest^.Success := _AnswerHeader.Successful = 1;
          FRequest^.Aborted := False;
          FRequest^.PayloadLen := _AnswerHeader.PayloadLen;
          if _AnswerHeader.PayloadLen = 0 then
            FRequest^.Payload := nil
          else
            FRequest^.Payload := @FReceivedBuffer[0];

          Result := True;
        end
        else begin
          LBLogger.Write(1, 'TNodeJSBridge.elaborateRequest', lmt_Error, 'Node.js worker timed out. Terminating process.');
          if (FProcess <> nil) then FProcess.Terminate(1);
          FWorkerCrashed := True;
          FRequest^.Aborted := True;
        end;
      end
      else begin
        FRequest^.Aborted := True;
        LBLogger.Write(1, 'TNodeJSBridge.elaborateRequest', lmt_Warning, 'Script file <%s> not found!', [FConfigParams.ScriptsFolder + _ScriptFilename]);
      end;

    except
      on E: Exception do
      begin
        FRequest^.Aborted := True;
        FRequest^.Payload := @cError_UnmanagedError[1];
        FRequest^.PayloadLen := Length(cError_UnmanagedError);
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TNodeJSBridge.elaborateRequest', lmt_Warning, 'No script file!');

  RTLEventSetEvent(FRequest^.TerminateEvent);
  FRequest := nil;
end;

procedure TNodeJSBridge.InternalExecute();
var
  _Path : String;

begin
  if FServerSocket <> nil then
  begin
    _Path := FConfigParams.ScriptsFolder + cNodeJSMainWorker;
    if FileExists(_Path) then
    begin
      // Starting worker.js and passing it local socket path
      FProcess := TProcess.Create(nil);
      FProcess.Executable := 'node';
      FProcess.Parameters.Add(_Path);
      FProcess.Parameters.Add(Format('--socket-path=%s', [FIPCFile]));
      FProcess.Options := [poNoConsole, poDetached];
      FProcess.CurrentDirectory := FConfigParams.ScriptsFolder;
      FProcess.Execute;

      FPeer := FServerSocket.Accept;
      if FPeer <> nil then
      begin
        FPeer.Timeout := FConfigParams.WorkerTimeoutMs;

        while not Self.Terminated do
        begin
          if FCSRequest.Acquire('TNodeJSBridge.InternalExecute') then
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
                LBLogger.Write(1, 'TNodeJSBridge.InternalExecute', lmt_Error, 'Error elaborating request: %s', [E.Message]);
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
            LBLogger.Write(1, 'TNodeJSBridge.InternalExecute', lmt_Error, 'Error terminating process: ' + E.Message);
        end;

        if FProcess <> nil then
          FreeAndNil(FProcess);
      end
      else
        LBLogger.Write(1, 'TNodeJSBridge.InternalExecute', lmt_Warning, 'Node.js not connected!');

    end
    else
      LBLogger.Write(1, 'TNodeJSBridge.InternalExecute', lmt_Warning, 'File worker <%s> not found, terminating thread', [_Path]);
  end
  else
    LBLogger.Write(1, 'TNodeJSBridge.InternalExecute', lmt_Warning, 'Socket serever not initialized!');

end;

constructor TNodeJSBridge.Create(Params: TBridgeConfigParams);
begin
  inherited Create(Params);

  FreeOnTerminate := True;
  FProcess := nil;
  FServerSocket := nil;
end;

destructor TNodeJSBridge.Destroy;
begin
  inherited Destroy;

  try

    if FServerSocket <> nil then
      FreeAndNil(FServerSocket);

  except
    on E: Exception do
      LBLogger.Write(1, 'TNodeJSBridge.Destroy', lmt_Error, E.Message);
  end;
end;

class function TNodeJSBridge.verifyParams(Params: TBridgeConfigParams): Boolean;
begin
  if Params.SocketFile <> '' then
    Result := True
  else begin
    Result := False;
    LBLogger.Write(1, 'TNodeJSBridge.verifyParams', lmt_Warning, 'Needed socket filename!');
  end;
end;

function TNodeJSBridge.prepareIPCChannel(): Boolean;
begin
  FIPCFile := ExtractFileName(FConfigParams.SocketFile);
  FIPCFile := ExtractFilePath(FConfigParams.SocketFile) + IntToStr(FThreadNum) + '_' + FIPCFile;
  if FileExists(FIPCFile) then
    DeleteFile(FIPCFile);

  FServerSocket := TLocalServerSocket.Create(FIPCFile);
  Result := FServerSocket.Listen;
end;

end.

