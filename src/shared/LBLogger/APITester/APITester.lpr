program APITester;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DynLibs;

// C-API function pointer types
type
  TLogCallback = procedure(aLevel: Integer; aSender: PChar; aMessage: PChar); cdecl;

  TLBLog_Initialize = procedure(aLogFileName: PChar; aLogLevel: Integer); cdecl;
  TLBLog_Finalize = procedure(); cdecl;
  TLBLog_Write = procedure(aLevel: Integer; aSender: PChar; aMessage: PChar); cdecl;
  TLBLog_CreateCallbackSublogger = function(aCallback: TLogCallback): Pointer; cdecl;
  TLBLog_DestroySublogger = procedure(aHandle: Pointer); cdecl;

var
  // Function pointer variables
  LBLog_Initialize: TLBLog_Initialize;
  LBLog_Finalize: TLBLog_Finalize;
  LBLog_Write: TLBLog_Write;
  LBLog_CreateCallbackSublogger: TLBLog_CreateCallbackSublogger;
  LBLog_DestroySublogger: TLBLog_DestroySublogger;

  // Global variable to check if our callback was called
  G_CallbackWasCalled: Boolean = False;
  G_CallbackMessage: String = '';

// This is the callback function that we will pass to the logger library.
procedure MyLogCallback(aLevel: Integer; aSender: PChar; aMessage: PChar); cdecl;
begin
  WriteLn('[CALLBACK CALLED] Level: ', aLevel, ', Sender: ', StrPas(aSender), ', Message: ', StrPas(aMessage));
  G_CallbackWasCalled := True;
  G_CallbackMessage := StrPas(aMessage);
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

procedure Check(aCondition: Boolean; aSuccessMsg: String; aFailMsg: String);
begin
  if aCondition then
    WriteLnSuccess(aSuccessMsg)
  else
    WriteLnFail(aFailMsg);
end;

procedure RunTests;
var
  CallbackLoggerHandle: Pointer;
  TestMessage: String;
begin
  WriteLn('--- Running LBLogger API Tests ---');

  // 1. Create Callback Sublogger
  CallbackLoggerHandle := LBLog_CreateCallbackSublogger(@MyLogCallback);
  Check(CallbackLoggerHandle <> nil, 'CreateCallbackSublogger: Sublogger created.', 'CreateCallbackSublogger: Failed to create sublogger.');

  // 2. Test the callback
  TestMessage := 'This is a test message for the callback.';
  G_CallbackWasCalled := False; // Reset flag
  LBLog_Write(1, 'APITester', PChar(TestMessage));

  Check(G_CallbackWasCalled, 'LBLog_Write: Callback was invoked.', 'LBLog_Write: Callback was NOT invoked.');
  Check(G_CallbackMessage = TestMessage, 'Callback Message: Message content is correct.', 'Callback Message: Message content is incorrect.');

  // 3. Test writing to file log
  // (This assumes the main logger writes to file, which we can't check here, but we call the function)
  LBLog_Write(1, 'APITester', 'This should go to the file log.');
  WriteLnSuccess('LBLog_Write: Message sent to file logger.');

  // 4. Destroy Sublogger
  LBLog_DestroySublogger(CallbackLoggerHandle);
  WriteLnSuccess('DestroySublogger: Callback sublogger destroyed.');

  // 5. Verify callback is no longer called
  G_CallbackWasCalled := False; // Reset flag
  LBLog_Write(1, 'APITester', 'This message should NOT trigger the callback.');
  Check(not G_CallbackWasCalled, 'Post-Destroy: Callback was not invoked, as expected.', 'Post-Destroy: Callback was invoked after being destroyed.');

  WriteLn('--- All Tests Passed ---');
end;

var
  LibHandle: TLibHandle;
  LibName: String;
begin
  {$IFDEF UNIX}
  LibName := './libLBLogger_shared.so';
  {$ENDIF}
  {$IFDEF WINDOWS}
  LibName := 'LBLogger_shared.dll';
  {$ENDIF}

  WriteLn('Loading library: ' + LibName);
  LibHandle := LoadLibrary(LibName);

  if LibHandle = NilHandle then
  begin
    WriteLn('FATAL: Could not load shared library: ' + LibName);
    Halt(1);
  end;

  try
    // Get function addresses from the library
    @LBLog_Initialize := GetProcedureAddress(LibHandle, 'LBLog_Initialize');
    @LBLog_Finalize := GetProcedureAddress(LibHandle, 'LBLog_Finalize');
    @LBLog_Write := GetProcedureAddress(LibHandle, 'LBLog_Write');
    @LBLog_CreateCallbackSublogger := GetProcedureAddress(LibHandle, 'LBLog_CreateCallbackSublogger');
    @LBLog_DestroySublogger := GetProcedureAddress(LibHandle, 'LBLog_DestroySublogger');

    if (@LBLog_Initialize = nil) or (@LBLog_Finalize = nil) or (@LBLog_Write = nil) or
       (@LBLog_CreateCallbackSublogger = nil) or (@LBLog_DestroySublogger = nil) then
    begin
      WriteLn('FATAL: Could not find all required functions in the library.');
      Halt(1);
    end;

    WriteLn('Library loaded and all functions found.');

    // Initialize the main logger to write to a file
    LBLog_Initialize('APITester_main.log', 5);
    WriteLnSuccess('Main logger initialized via API call.');

    // Run the tests
    RunTests;

  finally
    // Finalize the main logger
    if @LBLog_Finalize <> nil then
      LBLog_Finalize;

    WriteLn('Unloading library.');
    UnloadLibrary(LibHandle);
  end;
end.
