unit Toolkit_CAPI_Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCallbackLogger;

procedure Logger_Initialize(aLogFileName: PChar; aLogLevel: Integer); export; cdecl;
procedure Logger_Finalize; export; cdecl;
procedure Logger_Write(aLevel: Integer; aSender: PChar; aMessageType: Byte; aMessage: PChar); export; cdecl;
function Logger_CreateCallbackSublogger(aCallback: TLogCallback; aMaxLogLevel: Integer): Pointer; export; cdecl;
procedure Logger_DestroySublogger(aHandle: Pointer); export; cdecl;
function Logger_GetMsgType_Error(): byte; export; cdecl;
function Logger_GetMsgType_Warning(): byte; export; cdecl;
function Logger_GetMsgType_Debug(): byte; export; cdecl;
function Logger_GetMsgType_Info(): byte; export; cdecl;

implementation

uses
  ULBLogger,

procedure Logger_Initialize(aLogFileName: PChar; aLogLevel: Integer); cdecl;
var
  _LogFileName: string;
begin
  if LBLogger = nil then
  begin
    _LogFileName := StrPas(aLogFileName);
    InitLogger(aLogLevel, _LogFileName, False, False);
  end;
end;

procedure Logger_Finalize; cdecl;
begin
  ReleaseLogger;
end;

procedure Logger_Write(aLevel: Integer; aSender: PChar; aMessageType: Byte; aMessage: PChar); cdecl;
begin
  LBLogger.Write(aLevel, StrPas(aSender), TLBLoggerMessageType(aMessageType), StrPas(aMessage));
end;

function Logger_CreateCallbackSublogger(aCallback: TLogCallback; aMaxLogLevel: Integer): Pointer; cdecl;
var
  CallbackLogger: TCallbackLogger;
begin
  Result := nil;
  if (LBLogger <> nil) and (Assigned(aCallback)) then
  begin

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
end;

procedure Logger_DestroySublogger(aHandle: Pointer); cdecl;
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

function Logger_GetMsgType_Error(): byte; cdecl;
begin
  Result := Byte(TLBLoggerMessageType.lmt_Error);
end;

function Logger_GetMsgType_Warning(): byte; cdecl;
begin
  Result := Byte(TLBLoggerMessageType.lmt_Warning);
end;

function Logger_GetMsgType_Debug(): byte; cdecl;
begin
  Result := Byte(TLBLoggerMessageType.lmt_Debug);
end;

function Logger_GetMsgType_Info(): byte; cdecl;
begin
  Result := Byte(TLBLoggerMessageType.lmt_Info);
end;

end.
