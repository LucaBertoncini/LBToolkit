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

    if not AllFunctionsLoaded then
    begin
      WriteLn('FATAL: Could not find all required functions in the library.');
      Halt(1);
    end;

    WriteLn('Library loaded and all functions found.');

    RunCircularBufferTests;
    RunCircularBufferTSTests;

  finally
    WriteLn('Unloading library.');
    UnloadLibrary(LibHandle);
  end;
end.
