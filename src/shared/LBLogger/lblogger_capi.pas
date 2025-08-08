unit lblogger_capi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger, uCallbackLogger;

procedure LBLog_Initialize(aLogFileName: PChar; aLogLevel: Integer); export; cdecl;
procedure LBLog_Finalize; export; cdecl;
procedure LBLog_Write(aLevel: Integer; aSender: PChar; aMessage: PChar); export; cdecl;
function LBLog_CreateCallbackSublogger(aCallback: TLogCallback): Pointer; export; cdecl;
procedure LBLog_DestroySublogger(aHandle: Pointer); export; cdecl;

implementation

procedure LBLog_Initialize(aLogFileName: PChar; aLogLevel: Integer);
var
  LogFileName: string;
begin
  if LBLogger <> nil then Exit; // Already initialized

  LogFileName := StrPas(aLogFileName);
  InitLogger(aLogLevel, LogFileName, False, False);
end;

procedure LBLog_Finalize;
begin
  ReleaseLogger;
end;

procedure LBLog_Write(aLevel: Integer; aSender: PChar; aMessage: PChar);
begin
  if LBLogger <> nil then
    LBLogger.Write(aLevel, StrPas(aSender), lmt_Info, StrPas(aMessage));
end;

function LBLog_CreateCallbackSublogger(aCallback: TLogCallback): Pointer;
var
  CallbackLogger: TCallbackLogger;
begin
  Result := nil;
  if (LBLogger = nil) or (not Assigned(aCallback)) then Exit;

  try
    CallbackLogger := TCallbackLogger.Create(aCallback);
    if LBLogger.addAlternativeLogger(CallbackLogger) then
      Result := Pointer(CallbackLogger)
    else
      CallbackLogger.Free;
  except
    on E: Exception do
      // In case of error, ensure we don't leak memory
      Result := nil;
  end;
end;

procedure LBLog_DestroySublogger(aHandle: Pointer);
var
  Sublogger: TObject;
begin
  if (LBLogger = nil) or (aHandle = nil) then Exit;

  Sublogger := TObject(aHandle);
  if Sublogger is TCallbackLogger then
  begin
    // removeAlternativeLogger will also free the object if it was the owner,
    // but we free it explicitly to be safe. The list is non-owning.
    LBLogger.removeAlternativeLogger(TCallbackLogger(Sublogger));
    Sublogger.Free;
  end;
end;

end.
