unit uLBToolkitLibraryTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uLBToolkitLoader;

type

  { TLBToolkitLibraryTest }

  TLBToolkitLibraryTest= class(TTestCase)
  protected
    procedure TearDown; override;

  published
    procedure TestLoadLibrary;
    procedure TestCircularBuffer;
  end;

implementation

uses
  ULBLogger;

procedure TLBToolkitLibraryTest.TearDown;
begin
  if isLBToolkitLoaded() then
    Logger_Finalize();
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

  end;
end;


initialization
  RegisterTest(TLBToolkitLibraryTest);
end.

