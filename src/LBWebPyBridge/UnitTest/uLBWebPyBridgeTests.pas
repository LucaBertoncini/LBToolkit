unit uLBWebPyBridgeTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uPyBridge, ULBLogger;

type
  // WebServer port = 10320 from config.ini in uLBWebPyBridgeTestRunner

  { TLBWebPyBridgeTests }

  TLBWebPyBridgeTests = class(TTestCase)
  private
    FScriptsPath : String;

    procedure CreateFakeWorkerPy;
    procedure DeleteFakeWorkerPy;
    function PythonWorkerRunning: Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_NoWorkerPy;
    {$IFDEF Unix}
    procedure Test_PythonWorkerRunning;
    {$ENDIF}
    procedure Test_FakeWorker_WrongRequest;
    procedure Test_Echo;
    procedure Test_ScriptException;
    procedure Test_Sum;
    procedure Test_AppHello;
  end;

implementation

uses
  Process, FileUtil, LazFileUtils, uLBWebPyBridgeTestRunner, httpsend, fpjson, jsonparser,
  uLBWebPyBridgeApplication, uPyBridgeChainModule;

procedure TLBWebPyBridgeTests.CreateFakeWorkerPy;
var
  _Wrk: TStringList;
  _WorkerFilename : String;

begin
  _WorkerFilename := FScriptsPath + cPythonMainWorker;

  if not DirectoryExistsUTF8(FScriptsPath) then
    ForceDirectories(FScriptsPath)
  else if FileExists(_WorkerFilename) then
    DeleteFile(_WorkerFilename);

  _Wrk := TStringList.Create;
  _Wrk.Text :=
    'import time' + LineEnding +
    'while True:' + LineEnding +
    '    time.sleep(0.01)';
  _Wrk.SaveToFile(_WorkerFilename);
  _Wrk.Free;
end;

procedure TLBWebPyBridgeTests.DeleteFakeWorkerPy;
var
  _WorkerFilename : String;

begin
  _WorkerFilename := FScriptsPath + cPythonMainWorker;

  if FileExists(_WorkerFilename) then
    DeleteFile(_WorkerFilename);
end;

function TLBWebPyBridgeTests.PythonWorkerRunning: Boolean;
var
  _Process: TProcess;
  _Output: TStringList;

begin
  Result := False;
  _Process := TProcess.Create(nil);
  _Output := TStringList.Create;
  try
    _Process.Executable := 'ps';
    _Process.Parameters.Add('-eo');
    _Process.Parameters.Add('cmd');
    _Process.Options := [poUsePipes, poWaitOnExit];
    _Process.Execute;

    _Output.LoadFromStream(_Process.Output);
    // LBLogger.Write(5, 'TLBWebPyBridgeTests.PythonWorkerRunning', lmt_Debug, '%s', [_Output.Text]);

    Result := _Output.Text.Contains(cPythonMainWorker);
  finally
    _Process.Free;
    _Output.Free;
  end;
end;

procedure TLBWebPyBridgeTests.SetUp;
begin
  FScriptsPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cPythonScriptsSubfolder + DirectorySeparator;
end;


procedure TLBWebPyBridgeTests.TearDown;
begin
//  Self.DeleteFakeWorkerPy;
end;

procedure TLBWebPyBridgeTests.Test_NoWorkerPy;
var
  _String : TStringStream;
  _Obj : TJSONObject;
  _Answer : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_NoWorkerPy', lmt_Debug, '***************************************************************');

  DeleteDirectory(FScriptsPath, False);

  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200);  // wait for restart

  _Obj := TJSONObject.Create();
  _Obj.Add('Code', 'Test');
  _Answer := TJSONObject.Create();
  _Answer.CompressedJSON := True;
  _Answer.Add(cInvalidAnswer, cError_UnmanagedError);
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/testBridge', _Obj.AsJSON, _String));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_NoWorkerPy', lmt_Debug, 'Received: %s', [_String.DataString]);
  AssertTrue('Verifing answer', _String.DataString = _Answer.AsJSON);
  _String.Free;
  _Obj.Free;
  _Answer.Free;
end;

{$IFDEF Unix}
procedure TLBWebPyBridgeTests.Test_PythonWorkerRunning;
begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_FakeWorker_WrongRequest', lmt_Debug, '***************************************************************');

  Self.CreateFakeWorkerPy;
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(5000);
  AssertTrue('Verifing process', Self.PythonWorkerRunning);
end;
{$ENDIF}

procedure TLBWebPyBridgeTests.Test_FakeWorker_WrongRequest;
var
  _String : TStringStream;
  _Obj : TJSONObject;
  _Answer : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_FakeWorker_WrongRequest', lmt_Debug, '***************************************************************');

  Self.CreateFakeWorkerPy;
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200); // wait for restart

  _Obj := TJSONObject.Create();
  _Obj.Add('Code', 'Test');
  _Answer := TJSONObject.Create();
  _Answer.CompressedJSON := True;
  _Answer.Add(cInvalidAnswer, cError_ScriptNotFound);
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/testBridge/test3', _Obj.AsJSON, _String));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_FakeWorker_WrongRequest', lmt_Debug, 'Needed: <%s>  -  Received: <%s>', [_Answer.AsJSON, _String.DataString]);
  AssertTrue('Verifing answer', _String.DataString = _Answer.AsJSON);
  _String.Free;
  _Obj.Free;
  _Answer.Free;
  Sleep(1000);
end;

procedure TLBWebPyBridgeTests.Test_Echo;
var
  _Dummy : TJSONObject;
  _String : TStringStream;
  _Answer : TJSONObject;
  _Received : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_Echo', lmt_Debug, '***************************************************************');
  DeleteDirectory(FScriptsPath, False);

  AssertTrue('Python files extraction', Application.WebPyBridge.extractPythonFiles(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'python_runtime.zip'));
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200); // wait for restart

  _Dummy := TJSONObject.Create();
  _Dummy.Add('Code', 'Test');
  _Answer := TJSONObject.Create;
  _Answer.Add('echo', _Dummy);
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/tests/echo_test', _Dummy.AsJSON, _String));
  _Received := TJSONObject(GetJSON(_String.DataString));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_Echo', lmt_Debug, 'Needed: <%s>  -  Received: <%s>', [_Answer.AsJSON, _Received.AsJSON]);
  AssertTrue('Verifing answer', _Answer.AsJSON = _Received.AsJSON);
  _Received.Free;
  _Answer.Free;
  _String.Free;
end;

procedure TLBWebPyBridgeTests.Test_ScriptException;
var
  _Dummy : TJSONObject;
  _String : TStringStream;
  _Answer : TJSONObject;
  _Received : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_ScriptException', lmt_Debug, '***************************************************************');
  DeleteDirectory(FScriptsPath, False);

  AssertTrue('Python files extraction', Application.WebPyBridge.extractPythonFiles(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'python_runtime.zip'));
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200); // wait for restart

  _Dummy := TJSONObject.Create();
  _Dummy.Add('Code', 'Test');
  _Answer := TJSONObject.Create;
  _Answer.Add('wpbError', 'Execution failed: Intentional test error');
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/tests/error_test', _Dummy.AsJSON, _String));
  _Received := TJSONObject(GetJSON(_String.DataString));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_ScriptException', lmt_Debug, 'Needed: <%s>  -  Received: <%s>', [_Answer.AsJSON, _Received.AsJSON]);
  AssertTrue('Verifing answer', _Answer.AsJSON = _Received.AsJSON);
  _Dummy.Free;
  _Answer.Free;
  _String.Free;

end;

procedure TLBWebPyBridgeTests.Test_Sum;
var
  _Dummy : TJSONObject;
  _String : TStringStream;
  _Answer : TJSONObject;
  _Received : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_Sum', lmt_Debug, '***************************************************************');
  DeleteDirectory(FScriptsPath, False);

  AssertTrue('Python files extraction', Application.WebPyBridge.extractPythonFiles(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'python_runtime.zip'));
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200); // wait for restart

  _Dummy := TJSONObject.Create();
  _Dummy.Add('a', 5);
  _Dummy.Add('b', 7);
  _Answer := TJSONObject.Create;
  _Answer.Add('sum', 12);
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/tests/sum_test', _Dummy.AsJSON, _String));
  _Received := TJSONObject(GetJSON(_String.DataString));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_Sum', lmt_Debug, 'Needed: <%s>  -  Received: <%s>', [_Answer.AsJSON, _Received.AsJSON]);
  AssertTrue('Verifing answer', _Answer.AsJSON = _Received.AsJSON);
  _Dummy.Free;
  _Answer.Free;
  _String.Free;
end;

procedure TLBWebPyBridgeTests.Test_AppHello;
var
  _Dummy : TJSONObject;
  _String : TStringStream;
  _Answer : TJSONObject;
  _Received : TJSONObject;

begin
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_AppHello', lmt_Debug, '***************************************************************');
  DeleteDirectory(FScriptsPath, False);

  AssertTrue('Python files extraction', Application.WebPyBridge.extractPythonFiles(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'python_runtime.zip'));
  Application.WebPyBridge.setOrchestratorParams(3, 8092); // Forcing orchestrator rebuild
  Sleep(200); // wait for restart

  _Dummy := TJSONObject.Create();
  _Dummy.Add('custom_message', 'enjoy the tests');
  _Answer := TJSONObject.Create;
  _Answer.Add('output', 'Risultato elaborato: enjoy the tests');
  _String := TStringStream.Create();
  AssertTrue('Sending POST request', HttpPostURL('http://127.0.0.1:10320/tests/AppHello/main', _Dummy.AsJSON, _String));
  _Received := TJSONObject(GetJSON(_String.DataString));
  LBLogger.Write(5, 'TLBWebPyBridgeTests.Test_AppHello', lmt_Debug, 'Needed: <%s>  -  Received: <%s>', [_Answer.AsJSON, _Received.AsJSON]);
  AssertTrue('Verifing answer', _Answer.AsJSON = _Received.AsJSON);
  _Dummy.Free;
  _Answer.Free;
  _String.Free;
end;

initialization
  RegisterTest(TLBWebPyBridgeTests);

end.

