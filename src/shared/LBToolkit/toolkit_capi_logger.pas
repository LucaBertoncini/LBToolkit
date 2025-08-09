unit toolkit_capi_logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger, uCallbackLogger;

procedure Logger_Initialize(aLogFileName: PChar; aLogLevel: Integer); export; cdecl;
procedure Logger_Finalize; export; cdecl;
procedure Logger_Write(aLevel: Integer; aSender: PChar; aMessage: PChar); export; cdecl;
function Logger_CreateCallbackSublogger(aCallback: TLogCallback; aMaxLogLevel: Integer): Pointer; export; cdecl;
procedure Logger_DestroySublogger(aHandle: Pointer); export; cdecl;

implementation

procedure Logger_Initialize(aLogFileName: PChar; aLogLevel: Integer);
var
  LogFileName: string;
begin
  if LBLogger <> nil then Exit;

  LogFileName := StrPas(aLogFileName);
  InitLogger(aLogLevel, LogFileName, False, False);
end;

procedure Logger_Finalize;
begin
  ReleaseLogger;
end;

procedure Logger_Write(aLevel: Integer; aSender: PChar; aMessage: PChar);
begin
  if LBLogger <> nil then
  begin
    // We use lmt_Info as a default, but the level is the important part
    LBLogger.Write(aLevel, StrPas(aSender), lmt_Info, StrPas(aMessage));
  end;
end;

function Logger_CreateCallbackSublogger(aCallback: TLogCallback; aMaxLogLevel: Integer): Pointer;
var
  CallbackLogger: TCallbackLogger;
begin
  Result := nil;
  if (LBLogger = nil) or (not Assigned(aCallback)) then Exit;

  try
    CallbackLogger := TCallbackLogger.Create(aCallback, aMaxLogLevel);
    if LBLogger.addAlternativeLogger(CallbackLogger) then
      Result := Pointer(CallbackLogger)
    else
      CallbackLogger.Free;
  except
    on E: Exception do
      Result := nil;
  end;
end;

procedure Logger_DestroySublogger(aHandle: Pointer);
var
  Sublogger: TObject;
begin
  if (LBLogger = nil) or (aHandle = nil) then Exit;

  Sublogger := TObject(aHandle);
  if Sublogger is TCallbackLogger then
  begin
    LBLogger.removeAlternativeLogger(TCallbackLogger(Sublogger));
    Sublogger.Free;
  end;
end;

end.
