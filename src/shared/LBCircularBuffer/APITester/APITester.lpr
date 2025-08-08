program APITester;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DynLibs;

type
  // Define function pointer types matching the exported C-API
  TLBCB_Create = function(aSize: Cardinal): Pointer; cdecl;
  TLBCB_Destroy = procedure(aHandle: Pointer); cdecl;
  TLBCB_Write = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TLBCB_Read = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TLBCB_GetAvailableForRead = function(aHandle: Pointer): Cardinal; cdecl;
  TLBCB_GetAvailableForWrite = function(aHandle: Pointer): Cardinal; cdecl;
  TLBCB_Clear = procedure(aHandle: Pointer); cdecl;
  TLBCB_Initialize = procedure(aLogFileName: PChar; aLogLevel: Integer); cdecl;
  TLBCB_Finalize = procedure(); cdecl;

var
  // Variables to hold the function pointers
  LBCB_Create: TLBCB_Create;
  LBCB_Destroy: TLBCB_Destroy;
  LBCB_Write: TLBCB_Write;
  LBCB_Read: TLBCB_Read;
  LBCB_GetAvailableForRead: TLBCB_GetAvailableForRead;
  LBCB_GetAvailableForWrite: TLBCB_GetAvailableForWrite;
  LBCB_Clear: TLBCB_Clear;
  LBCB_Initialize: TLBCB_Initialize;
  LBCB_Finalize: TLBCB_Finalize;

procedure WriteLnSuccess(aMessage: String);
begin
  WriteLn('[ OK ] ', aMessage);
end;

procedure WriteLnFail(aMessage: String);
begin
  WriteLn('[FAIL] ', aMessage);
  Halt(1);
end;

procedure Check(aCondition: Boolean; aSuccessMsg: String; aFailMsg: String);
begin
  if aCondition then
    WriteLnSuccess(aSuccessMsg)
  else
    WriteLnFail(aFailMsg);
end;

procedure RunTests;
var
  BufferHandle: Pointer;
  DataIn, DataOut: array[0..15] of Byte;
  i: Integer;
begin
  WriteLn('--- Running LBCircularBuffer API Tests ---');

  // 1. Create Test
  BufferHandle := LBCB_Create(64);
  Check(BufferHandle <> nil, 'LBCB_Create: Buffer created successfully.', 'LBCB_Create: Failed to create buffer.');

  // 2. Write Test
  for i := 0 to 15 do DataIn[i] := i;
  Check(LBCB_Write(BufferHandle, @DataIn[0], Length(DataIn)),
    'LBCB_Write: Wrote 16 bytes.', 'LBCB_Write: Failed to write data.');

  // 3. AvailableForRead Test
  Check(LBCB_GetAvailableForRead(BufferHandle) = 16,
    'LBCB_GetAvailableForRead: Correct count (16).',
    'LBCB_GetAvailableForRead: Incorrect count. Expected 16, got ' + IntToStr(LBCB_GetAvailableForRead(BufferHandle)));

  // 4. Read Test
  Check(LBCB_Read(BufferHandle, @DataOut[0], Length(DataOut)),
    'LBCB_Read: Read 16 bytes.', 'LBCB_Read: Failed to read data.');

  // 5. Data Integrity Test
  for i := 0 to 15 do
  begin
    if DataIn[i] <> DataOut[i] then
      WriteLnFail('Data Integrity: Mismatch at index ' + IntToStr(i));
  end;
  WriteLnSuccess('Data Integrity: All bytes match.');

  // 6. Final Count Test
  Check(LBCB_GetAvailableForRead(BufferHandle) = 0,
    'LBCB_GetAvailableForRead: Correct final count (0).',
    'LBCB_GetAvailableForRead: Incorrect final count. Expected 0.');

  // 7. Clear Test
  LBCB_Write(BufferHandle, @DataIn[0], 8);
  Check(LBCB_GetAvailableForRead(BufferHandle) = 8, 'LBCB_Write: Wrote 8 bytes for clear test.', 'LBCB_Write: Failed clear test write.');
  LBCB_Clear(BufferHandle);
  Check(LBCB_GetAvailableForRead(BufferHandle) = 0, 'LBCB_Clear: Buffer cleared successfully.', 'LBCB_Clear: Buffer not empty after clear.');

  // 8. Destroy Test
  LBCB_Destroy(BufferHandle);
  WriteLnSuccess('LBCB_Destroy: Buffer destroyed.');

  WriteLn('--- All Tests Passed ---');
end;

var
  LibHandle: TLibHandle;
  LibName: String;
begin
  {$IFDEF UNIX}
  LibName := './libLBCircularBuffer_shared.so';
  {$ENDIF}
  {$IFDEF WINDOWS}
  LibName := 'LBCircularBuffer_shared.dll';
  {$ENDIF}

  WriteLn('Loading library: ' + LibName);
  LibHandle := LoadLibrary(LibName);

  if LibHandle = NilHandle then
  begin
    WriteLn('FATAL: Could not load shared library: ' + LibName);
    Halt(1);
  end;

  try
    // Get function addresses
    @LBCB_Create := GetProcedureAddress(LibHandle, 'LBCB_Create');
    @LBCB_Destroy := GetProcedureAddress(LibHandle, 'LBCB_Destroy');
    @LBCB_Write := GetProcedureAddress(LibHandle, 'LBCB_Write');
    @LBCB_Read := GetProcedureAddress(LibHandle, 'LBCB_Read');
    @LBCB_GetAvailableForRead := GetProcedureAddress(LibHandle, 'LBCB_GetAvailableForRead');
    @LBCB_GetAvailableForWrite := GetProcedureAddress(LibHandle, 'LBCB_GetAvailableForWrite');
    @LBCB_Clear := GetProcedureAddress(LibHandle, 'LBCB_Clear');
    @LBCB_Initialize := GetProcedureAddress(LibHandle, 'LBCB_Initialize');
    @LBCB_Finalize := GetProcedureAddress(LibHandle, 'LBCB_Finalize');

    if (@LBCB_Create = nil) or (@LBCB_Destroy = nil) or (@LBCB_Write = nil) or (@LBCB_Initialize = nil) or (@LBCB_Finalize = nil) then
    begin
      WriteLn('FATAL: Could not find all required functions in the library.');
      Halt(1);
    end;

    WriteLn('Library loaded and all functions found.');

    // Initialize Logger
    LBCB_Initialize('APITester.log', 5);
    WriteLnSuccess('Logger Initialized via API call.');

    // Run the tests
    RunTests;

  finally
    // Finalize Logger
    if @LBCB_Finalize <> nil then
    begin
      LBCB_Finalize;
      WriteLnSuccess('Logger Finalized via API call.');
    end;

    WriteLn('Unloading library.');
    UnloadLibrary(LibHandle);
  end;
end.
