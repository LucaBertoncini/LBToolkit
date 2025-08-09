unit uLBToolkitLibraryTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uLBToolkitLoader;

type

  { TLBToolkitLibraryTest }

  TLBToolkitLibraryTest= class(TTestCase)
  strict private
    FCallbackReceived : Boolean;
    FCallbackLogLevel : Integer;

  protected
    procedure TearDown; override;

  public
    procedure messageReceived(aLevel: Integer; aSender: String; aMessage: String);

  published
    procedure TestLoadLibrary;
    procedure TestCircularBuffer;
    procedure TestSublogger;
  end;

implementation

uses
  ULBLogger;

function SubloggerCallback(aLevel: Integer; aSender: PChar; aMsgType: Byte; aMessage: PChar; aStopPropagation: PBoolean; userData: Pointer): Boolean; cdecl;
begin
  TLBToolkitLibraryTest(userData).messageReceived(aLevel, StrPas(aSender), StrPas(aMessage));
  Result := True;
end;

procedure TLBToolkitLibraryTest.TearDown;
begin
  if isLBToolkitLoaded() then
    Logger_Finalize();
end;

procedure TLBToolkitLibraryTest.messageReceived(aLevel: Integer; aSender: String; aMessage: String);
begin
  FCallbackReceived := True;
  FCallbackLogLevel := aLevel;
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TLBToolkitLibraryTest', lmt_Debug, 'LogLevel: %d  -  Sender: <%s>  -  Message: <%s>', [aLevel, aSender, aMessage]);
end;

procedure TLBToolkitLibraryTest.TestLoadLibrary;
var
  _Res : Boolean;

begin
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestLoadUnloadLibrary', lmt_Debug, '*****************************************************');
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestLoadUnloadLibrary', lmt_Debug, 'Test Load library started ...');
  _Res := LoadLBToolkit('../../sharedLib/liblbtoolkit.so');
  AssertTrue('Toolkit library loaded', _Res);
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestLoadUnloadLibrary', lmt_Debug, 'Test terminated');
end;

procedure TLBToolkitLibraryTest.TestCircularBuffer;
var
  _MsgDebug : Byte;
  _MsgWarning : Byte;
  _BufferHandle : Pointer;
  _DataIn : array [0 .. 15] of Byte;
  _DataOut : array [0 .. 15] of Byte;
  i : Integer;

  _Res : Boolean;

const
  cPattern : array [0 .. 3] of Byte = (5, 6, 7, 8);

begin
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', lmt_Debug, '*****************************************************');
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', lmt_Debug, 'Test Circular Buffer started ...');
  Sleep(20);

  _Res := LoadLBToolkit('../../sharedLib/liblbtoolkit.so');
  AssertTrue('Toolkit library loaded', _Res);
  if _Res then
  begin
    LBLogger.Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', lmt_Debug, 'Toolkit library loaded');

    Logger_Finalize();
    Sleep(20);

    Logger_Initialize('/tmp/TestCircularBuffer.log', 5);
    LBLogger.Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', lmt_Debug, 'Logger initialized');
    Sleep(200);

    _MsgDebug := Logger_GetMsgType_Debug();
    _MsgWarning := Logger_GetMsgType_Debug();
    Logger_Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', _MsgDebug, 'Creating circular buffer ...');

    _BufferHandle := CircularBuffer_Create(64);
    AssertTrue('Buffer created', _BufferHandle <> nil);

    for i := 0 to 15 do _DataIn[i] := i;
    AssertTrue('Wrote 16 bytes', CircularBuffer_Write(_BufferHandle, @_DataIn[0], 16));
    AssertTrue('Available: Correct count (16)', CircularBuffer_GetAvailableForRead(_BufferHandle) = 16);


    i := CircularBuffer_FindPattern(_BufferHandle, @cPattern[0], Length(cPattern), 0);
    Logger_Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', _MsgDebug, PChar('Pattern found @ ' + IntToStr(i)));
    AssertTrue('Pattern index: ' + IntToStr(i), i >= 0);

    AssertTrue('Read 16 bytes', CircularBuffer_Read(_BufferHandle, @_DataOut[0], 16));

    for i := 0 to 15 do
    begin
      Logger_Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', _MsgWarning, PChar('Data Integrity - Index ' + IntToStr(i) + ' - Value: ' + IntToStr(_DataOut[i])));
      AssertFalse('Wrong values for index ' + IntToStr(i) + ': In ' + IntToStr(_DataIn[i]) + ' Out ' + IntToStr(_DataOut[i]) , _DataIn[i] <> _DataOut[i]);
    end;


    CircularBuffer_Destroy(_BufferHandle);
    Logger_Write(1, 'TLBToolkitLibraryTest.TestCircularBuffer', _MsgDebug, 'Buffer destroyed');

    Sleep(200);
  end;
end;

procedure TLBToolkitLibraryTest.TestSublogger;
var
  _Res : Boolean;
  _MsgDebug : Byte;
  _Sublogger : Pointer;

begin
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestSublogger', lmt_Debug, '*****************************************************');
  LBLogger.Write(1, 'TLBToolkitLibraryTest.TestSublogger', lmt_Debug, 'Test Sublogger started ...');

  _Res := LoadLBToolkit('../../sharedLib/liblbtoolkit.so');
  AssertTrue('Toolkit library loaded', _Res);

  Logger_Finalize();
  Sleep(20);

  Logger_Initialize('/tmp/TestSublogger.log', 5);
  _MsgDebug := Logger_GetMsgType_Debug();

  Logger_Write(1, 'TLBToolkitLibraryTest.TestSublogger', _MsgDebug, 'Creating sublogger buffer ...');
  _Sublogger := Logger_CreateCallbackSublogger(@SubloggerCallback, 100, Pointer(Self));

  AssertTrue('Sublogger created', _Sublogger <> nil);

  FCallbackLogLevel := 0;
  FCallbackReceived := False;
  Logger_Write(100, 'TLBToolkitLibraryTest.TestSublogger', _MsgDebug, 'A message with 100 as log level');

  AssertTrue('Callback received successfully', FCallbackReceived and (FCallbackLogLevel = 100));

  FCallbackLogLevel := 0;
  FCallbackReceived := False;
  Logger_Write(101, 'TLBToolkitLibraryTest.TestSublogger', _MsgDebug, 'A message with 101 as log level');

  AssertTrue('Callback not called', (not FCallbackReceived) and (FCallbackLogLevel = 0));

  FCallbackLogLevel := 0;
  FCallbackReceived := False;
  Logger_Write(1, 'TLBToolkitLibraryTest.TestSublogger', _MsgDebug, 'First message with 1 as log level');

  AssertTrue('Regular callback received', FCallbackReceived and (FCallbackLogLevel = 1));

  Logger_DestroySublogger(_Sublogger);

  FCallbackLogLevel := 0;
  FCallbackReceived := False;
  Logger_Write(1, 'TLBToolkitLibraryTest.TestSublogger', _MsgDebug, 'Second message with 1 as log level');

  AssertTrue('Callback not received', (not FCallbackReceived) and (FCallbackLogLevel = 0));
  Sleep(200);
end;


initialization
  RegisterTest(TLBToolkitLibraryTest);
end.

