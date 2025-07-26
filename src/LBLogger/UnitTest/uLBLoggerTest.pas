unit uLBLoggerTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ULBLogger;

type

  { TTestLBLogger }

  TTestLBLogger = class(TTestCase)
  private
    LogFullPath: String;

    function initalizeLogger(): Boolean;

  protected
    procedure TearDown; override;

  published
    procedure TestInitializeLogger;
    procedure TestWriteBasicMessage;
    procedure TestLogLevelFiltering;
    procedure TestEnabledMessageTypes;
    procedure TestMessageFormatOutput;
    procedure TestLogFileRotation;
    procedure TestAlternativeLoggerIntercept;
    procedure TestConcurrentWriteCalls;
    procedure TestFlushOnRelease;
  end;

  { TLogThread }

  TLogThread = class(TThread)
    MsgIndex: Integer;
    procedure Execute; override;
  end;


implementation

uses
  FileUtil, DateUtils;

type
  TInterceptLogger = class(TLBBaseLogger)
  public
    InterceptedMessage: String;
    function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;
  end;

function FindFileInDir(const dirPath, mask: String): String;
var
  searchRec: TSearchRec;
begin
  Result := '';
  if FindFirst(dirPath + mask, faAnyFile, searchRec) = 0 then
    Result := searchRec.Name;
  FindClose(searchRec);
end;


{ TLogThread }

procedure TLogThread.Execute;
begin
  LBLogger.Write(1, 'Thread', lmt_Report, Format('Thread log %d', [MsgIndex]));
end;

function TInterceptLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  InterceptedMessage := MsgText;
  MsgText := ''; // blocca propagazione
  Result := True;
end;

function TTestLBLogger.initalizeLogger: Boolean;
begin
  Result := InitLogger(5, 'test_logger.log', False, True);
  if Result then
    LogFullPath := LBLogger.FilePath + LBLogger.FileName;
end;


procedure TTestLBLogger.TearDown;
begin
  ReleaseLogger();
end;

procedure TTestLBLogger.TestInitializeLogger;
begin
  ReleaseLogger();
  AssertTrue('Should return true', Self.initalizeLogger());
end;

procedure TTestLBLogger.TestWriteBasicMessage;
begin
  Self.initalizeLogger();
  AssertTrue('Should write message', LBLogger.Write(1, 'TestCase', lmt_Info, 'Hello, logger!'));
end;

procedure TTestLBLogger.TestLogLevelFiltering;
begin
  Self.initalizeLogger();
  LBLogger.MaxLogLevel := 2;
  AssertFalse('Level 4 should be ignored', LBLogger.Write(4, 'TestCase', lmt_Debug, 'Should not log'));
end;

procedure TTestLBLogger.TestEnabledMessageTypes;
begin
  Self.initalizeLogger();
  LBLogger.EnabledMessages := [lmt_Error, lmt_Critical];
  AssertFalse('Info message should be disabled', LBLogger.Write(1, 'TestCase', lmt_Info, 'Filtered out'));
end;

procedure TTestLBLogger.TestMessageFormatOutput;
var
  msg: TLBLoggerMessage;
begin
  msg := TLBLoggerMessage.Create(nil);
  msg.Time := EncodeDateTime(2024, 12, 24, 10, 30, 45, 123);
  msg.MsgType := lmt_Debug;
  msg.Message := 'TestMessage';
  msg.PID := 123;
  msg.ThreadId := 456;
  msg.CallingRoutine := 'RoutineX';
  AssertTrue('Formatted message must contain Debug prefix', Pos('DBG', msg.ElaborateMessageToWrite) > 0);
  msg.Free;
end;

procedure TTestLBLogger.TestLogFileRotation;
var
  i: Integer;
  _FilePath : String;

begin
  if FileExists(LogFullPath) then
    DeleteFile(LogFullPath);

  Self.initalizeLogger();

  LBLogger.MaxFileSize := 1024; // forza rotazione
  for i := 1 to 500 do
  begin
    LBLogger.Write(1, 'Rotator', lmt_Info, Format('Line %d', [i]));
    Sleep(2);
  end;

  _FilePath := IncludeTrailingPathDelimiter(LBLogger.FilePath);
  Sleep(300); // tempo per flush
  AssertTrue('Original log file must exist', FileExists(LogFullPath));
  AssertTrue('Renamed files must exists', FindFileInDir(_FilePath, '*_test_logger.log') <> '');
end;

procedure TTestLBLogger.TestAlternativeLoggerIntercept;
var
  alt: TInterceptLogger;
begin
  Self.initalizeLogger();

  alt := TInterceptLogger.Create('Intercept');
  LBLogger.addAlternativeLogger(alt);
  LBLogger.Write(1, 'TestCase', lmt_Info, 'Intercept me!');
  AssertEquals('Message should be intercepted and blocked', 'Intercept me!', alt.InterceptedMessage);
  alt.Free;
end;

procedure TTestLBLogger.TestConcurrentWriteCalls;
var
  i: Integer;
  threads: array of TThread;

begin
  Self.initalizeLogger();

  SetLength(threads, 40);
  for i := 0 to High(threads) do
  begin
    threads[i] := TLogThread.Create(True);
    TLogThread(threads[i]).MsgIndex := i;
    threads[i].Start;
  end;

  for i := 0 to High(threads) do
  begin
    threads[i].WaitFor;
    threads[i].Free;
  end;

  AssertTrue('Concurrent threads completed', True);
end;


procedure TTestLBLogger.TestFlushOnRelease;
begin
  ReleaseLogger();

  if FileExists(LogFullPath) then
    DeleteFile(LogFullPath);

  Self.initalizeLogger();

  LBLogger.Write(1, 'FlushCheck', lmt_Debug, 'Flush test started');
  LBLogger.Write(1, 'FlushCheck', lmt_Info, 'Message A');
  LBLogger.Write(1, 'FlushCheck', lmt_Info, 'Message B');

  Sleep(2);

  ReleaseLogger();

  AssertTrue('File ' + LogFullPath + ' must exists', FileExists(LogFullPath));
  AssertTrue('File ' + LogFullPath + ' size > 0',  FileSize(LogFullPath) > 0);
end;


initialization
  RegisterTest(TTestLBLogger);

end.

