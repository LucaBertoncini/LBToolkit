program Tester;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DynLibs;

type
  // Non-Thread-Safe API
  TCircularBuffer_Create = function(aSize: Cardinal): Pointer; cdecl;
  TCircularBuffer_Destroy = procedure(aHandle: Pointer); cdecl;
  TCircularBuffer_Write = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBuffer_Read = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBuffer_GetAvailableForRead = function(aHandle: Pointer): Cardinal; cdecl;

  // Thread-Safe API
  TCircularBufferTS_Create = function(aSize: Cardinal): Pointer; cdecl;
  TCircularBufferTS_Destroy = procedure(aHandle: Pointer); cdecl;
  TCircularBufferTS_Write = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBufferTS_Read = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBufferTS_GetAvailableForRead = function(aHandle: Pointer): Cardinal; cdecl;

  // Logger API
  TLogCallback = function(aLevel: Integer; aSender: PChar; aMessage: PChar): Boolean; cdecl;
  TLogger_Initialize = procedure(aLogFileName: PChar; aLogLevel: Integer); cdecl;
  TLogger_Finalize = procedure(); cdecl;
  TLogger_Write = procedure(aLevel: Integer; aSender: PChar; aMessage: PChar); cdecl;
  TLogger_CreateCallbackSublogger = function(aCallback: TLogCallback; aMaxLogLevel: Integer): Pointer; cdecl;
  TLogger_DestroySublogger = procedure(aHandle: Pointer); cdecl;

var
  // Non-Thread-Safe Pointers
  CircularBuffer_Create: TCircularBuffer_Create;
  CircularBuffer_Destroy: TCircularBuffer_Destroy;
  CircularBuffer_Write: TCircularBuffer_Write;
  CircularBuffer_Read: TCircularBuffer_Read;
  CircularBuffer_GetAvailableForRead: TCircularBuffer_GetAvailableForRead;

  // Thread-Safe Pointers
  CircularBufferTS_Create: TCircularBufferTS_Create;
  CircularBufferTS_Destroy: TCircularBufferTS_Destroy;
  CircularBufferTS_Write: TCircularBufferTS_Write;
  CircularBufferTS_Read: TCircularBufferTS_Read;
  CircularBufferTS_GetAvailableForRead: TCircularBufferTS_GetAvailableForRead;

  // Logger Pointers
  Logger_Initialize: TLogger_Initialize;
  Logger_Finalize: TLogger_Finalize;
  Logger_Write: TLogger_Write;
  Logger_CreateCallbackSublogger: TLogger_CreateCallbackSublogger;
  Logger_DestroySublogger: TLogger_DestroySublogger;

  // Global variables to check if our callback was called
  G_CallbackWasCalled: Boolean = False;
  G_CallbackMessage: String = '';
  G_CallbackSender: String = '';
  G_CallbackLevel: Integer = 0;


// This is the callback function that we will pass to the logger library.
function MyLogCallback(aLevel: Integer; aSender: PChar; aMessage: PChar): Boolean; cdecl;
begin
  WriteLn('[CALLBACK CALLED] Level: ', aLevel, ', Sender: ', StrPas(aSender), ', Message: ', StrPas(aMessage));
  G_CallbackWasCalled := True;
  G_CallbackLevel := aLevel;
  G_CallbackSender := StrPas(aSender);
  G_CallbackMessage := StrPas(aMessage);

  // Stop propagation for messages with level > 50
  Result := (aLevel > 50);
end;

procedure WriteLnSuccess(aMessage: String);
begin
  WriteLn('[ OK ] ', aMessage);
end;

procedure WriteLnFail(aMessage: String);
begin
  WriteLn('[FAIL] ', aMessage);
  Halt(1);
end;

procedure Check(aCondition: Boolean; const aSuccessMsg: String; const aFailMsg: String);
begin
  if aCondition then
    WriteLnSuccess(aSuccessMsg)
  else
    WriteLnFail(aFailMsg);
end;

procedure RunCircularBufferTests;
var
  BufferHandle: Pointer;
  DataIn, DataOut: array[0..15] of Byte;
  i: Integer;
begin
  WriteLn('--- Running Non-Thread-Safe Circular Buffer API Tests ---');
  BufferHandle := CircularBuffer_Create(64);
  Check(BufferHandle <> nil, 'Create: Buffer created.', 'Create: FAILED to create buffer.');

  for i := 0 to 15 do DataIn[i] := i;
  Check(CircularBuffer_Write(BufferHandle, @DataIn[0], 16), 'Write: Wrote 16 bytes.', 'Write: FAILED to write.');
  Check(CircularBuffer_GetAvailableForRead(BufferHandle) = 16, 'Available: Correct count (16).', 'Available: Wrong count.');
  Check(CircularBuffer_Read(BufferHandle, @DataOut[0], 16), 'Read: Read 16 bytes.', 'Read: FAILED to read.');

  for i := 0 to 15 do
  begin
    if DataIn[i] <> DataOut[i] then
      WriteLnFail('Data Integrity: Mismatch at index ' + IntToStr(i));
  end;
  WriteLnSuccess('Data Integrity: All bytes match.');

  CircularBuffer_Destroy(BufferHandle);
  WriteLnSuccess('Destroy: Buffer destroyed.');
  WriteLn('--- Non-Thread-Safe Tests Passed ---');
end;

procedure RunCircularBufferTSTests;
var
  BufferHandle: Pointer;
  DataIn, DataOut: array[0..15] of Byte;
  i: Integer;
begin
  WriteLn('--- Running Thread-Safe Circular Buffer API Tests ---');
  BufferHandle := CircularBufferTS_Create(64);
  Check(BufferHandle <> nil, 'Create: Buffer created.', 'Create: FAILED to create buffer.');

  for i := 0 to 15 do DataIn[i] := i;
  Check(CircularBufferTS_Write(BufferHandle, @DataIn[0], 16), 'Write: Wrote 16 bytes.', 'Write: FAILED to write.');
  Check(CircularBufferTS_GetAvailableForRead(BufferHandle) = 16, 'Available: Correct count (16).', 'Available: Wrong count.');
  Check(CircularBufferTS_Read(BufferHandle, @DataOut[0], 16), 'Read: Read 16 bytes.', 'Read: FAILED to read.');

  for i := 0 to 15 do
  begin
    if DataIn[i] <> DataOut[i] then
      WriteLnFail('Data Integrity: Mismatch at index ' + IntToStr(i));
  end;
  WriteLnSuccess('Data Integrity: All bytes match.');

  CircularBufferTS_Destroy(BufferHandle);
  WriteLnSuccess('Destroy: Buffer destroyed.');
  WriteLn('--- Thread-Safe Tests Passed ---');
end;

procedure RunLoggerTests;
var
  CallbackLoggerHandle: Pointer;
  TestSender, TestMessage: String;
begin
  WriteLn('--- Running Logger API Tests ---');

  // 1. Create Callback Sublogger with a high log level
  CallbackLoggerHandle := Logger_CreateCallbackSublogger(@MyLogCallback, 100);
  Check(CallbackLoggerHandle <> nil, 'CreateCallbackSublogger: Sublogger created.', 'CreateCallbackSublogger: FAILED to create sublogger.');

  // 2. Test the selective log level
  TestSender := 'LogLevelTest';
  TestMessage := 'This message has level 100.';
  G_CallbackWasCalled := False;
  Logger_Write(100, PChar(TestSender), PChar(TestMessage));
  Check(G_CallbackWasCalled, 'Selective Log Level: Callback was invoked for high-level message.', 'Selective Log Level: Callback was NOT invoked.');
  Check(G_CallbackLevel = 100, 'Selective Log Level: Correct level passed to callback.', 'Selective Log Level: Incorrect level.');

  // 3. Test the stop propagation feature
  TestSender := 'StopPropagationTest';
  TestMessage := 'This message has level 101 and should stop propagation.';
  G_CallbackWasCalled := False;
  Logger_Write(101, PChar(TestSender), PChar(TestMessage));
  Check(G_CallbackWasCalled, 'Stop Propagation: Callback was invoked.', 'Stop Propagation: Callback was NOT invoked.');
  // We can't easily check if it was written to the file log, but we trust the implementation.
  WriteLnSuccess('Stop Propagation: Test message sent.');

  // 4. Test a standard message
  TestSender := 'StandardTest';
  TestMessage := 'This is a standard message.';
  G_CallbackWasCalled := False;
  Logger_Write(1, PChar(TestSender), PChar(TestMessage));
  Check(G_CallbackWasCalled, 'Standard Message: Callback was invoked.', 'Standard Message: Callback was NOT invoked.');
  Check(G_CallbackLevel = 1, 'Standard Message: Correct level passed.', 'Standard Message: Incorrect level.');


  // 5. Destroy Sublogger
  Logger_DestroySublogger(CallbackLoggerHandle);
  WriteLnSuccess('DestroySublogger: Callback sublogger destroyed.');

  // 6. Verify callback is no longer called
  G_CallbackWasCalled := False;
  Logger_Write(1, 'APITester', 'This message should NOT trigger the callback.');
  Check(not G_CallbackWasCalled, 'Post-Destroy: Callback was not invoked, as expected.', 'Post-Destroy: FAILED, callback was invoked after being destroyed.');


  WriteLn('--- Logger Tests Passed ---');
end;

var
  LibHandle: TLibHandle;
  LibName: String;
  AllFunctionsLoaded: Boolean;
begin
  {$IFDEF UNIX}
  LibName := './libLBToolkit.so';
  {$ENDIF}
  {$IFDEF WINDOWS}
  LibName := 'LBToolkit.dll';
  {$ENDIF}

  WriteLn('Loading library: ' + LibName);
  LibHandle := LoadLibrary(LibName);

  if LibHandle = NilHandle then
  begin
    WriteLn('FATAL: Could not load shared library: ' + LibName);
    Halt(1);
  end;

  try
    AllFunctionsLoaded := True;
    // Load Non-TS functions
    @CircularBuffer_Create := GetProcedureAddress(LibHandle, 'CircularBuffer_Create');
    if @CircularBuffer_Create = nil then AllFunctionsLoaded := False;
    @CircularBuffer_Destroy := GetProcedureAddress(LibHandle, 'CircularBuffer_Destroy');
    if @CircularBuffer_Destroy = nil then AllFunctionsLoaded := False;
    @CircularBuffer_Write := GetProcedureAddress(LibHandle, 'CircularBuffer_Write');
    if @CircularBuffer_Write = nil then AllFunctionsLoaded := False;
    @CircularBuffer_Read := GetProcedureAddress(LibHandle, 'CircularBuffer_Read');
    if @CircularBuffer_Read = nil then AllFunctionsLoaded := False;
    @CircularBuffer_GetAvailableForRead := GetProcedureAddress(LibHandle, 'CircularBuffer_GetAvailableForRead');
    if @CircularBuffer_GetAvailableForRead = nil then AllFunctionsLoaded := False;

    // Load TS functions
    @CircularBufferTS_Create := GetProcedureAddress(LibHandle, 'CircularBufferTS_Create');
    if @CircularBufferTS_Create = nil then AllFunctionsLoaded := False;
    @CircularBufferTS_Destroy := GetProcedureAddress(LibHandle, 'CircularBufferTS_Destroy');
    if @CircularBufferTS_Destroy = nil then AllFunctionsLoaded := False;
    @CircularBufferTS_Write := GetProcedureAddress(LibHandle, 'CircularBufferTS_Write');
    if @CircularBufferTS_Write = nil then AllFunctionsLoaded := False;
    @CircularBufferTS_Read := GetProcedureAddress(LibHandle, 'CircularBufferTS_Read');
    if @CircularBufferTS_Read = nil then AllFunctionsLoaded := False;
    @CircularBufferTS_GetAvailableForRead := GetProcedureAddress(LibHandle, 'CircularBufferTS_GetAvailableForRead');
    if @CircularBufferTS_GetAvailableForRead = nil then AllFunctionsLoaded := False;

    // Load Logger functions
    @Logger_Initialize := GetProcedureAddress(LibHandle, 'Logger_Initialize');
    if @Logger_Initialize = nil then AllFunctionsLoaded := False;
    @Logger_Finalize := GetProcedureAddress(LibHandle, 'Logger_Finalize');
    if @Logger_Finalize = nil then AllFunctionsLoaded := False;
    @Logger_Write := GetProcedureAddress(LibHandle, 'Logger_Write');
    if @Logger_Write = nil then AllFunctionsLoaded := False;
    @Logger_CreateCallbackSublogger := GetProcedureAddress(LibHandle, 'Logger_CreateCallbackSublogger');
    if @Logger_CreateCallbackSublogger = nil then AllFunctionsLoaded := False;
    @Logger_DestroySublogger := GetProcedureAddress(LibHandle, 'Logger_DestroySublogger');
    if @Logger_DestroySublogger = nil then AllFunctionsLoaded := False;


    if not AllFunctionsLoaded then
    begin
      WriteLn('FATAL: Could not find all required functions in the library.');
      Halt(1);
    end;

    WriteLn('Library loaded and all functions found.');

    // Initialize Logger
    Logger_Initialize('APITester.log', 5);
    WriteLnSuccess('Logger Initialized via API call.');

    RunCircularBufferTests;
    RunCircularBufferTSTests;
    RunLoggerTests;

  finally
    // Finalize Logger
    if @Logger_Finalize <> nil then
    begin
      Logger_Finalize;
      WriteLnSuccess('Logger Finalized via API call.');
    end;

    WriteLn('Unloading library.');
    UnloadLibrary(LibHandle);
  end;
end.
