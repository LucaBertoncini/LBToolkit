unit uEventLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger, eventlog;

type

  { TfpEventLogger }

  TfpEventLogger = class(TLBBaseLogger)
    strict private
      FEventLog : TEventLog;

    strict protected
      function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;

    public
      constructor Create(const aName: String; AppendToMainLogger: Boolean = True); override;
      destructor Destroy(); override;
  end;

implementation

{ TfpEventLogger }

function TfpEventLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
var
  _msgType : String;

begin
  Result := False;

  if (MsgType in FEnabledMessages) and (LogLevel <= FMaxLogLevel) then
  begin
    case MsgType of
      lmt_Info      : FEventLog.Info(MsgText);
      lmt_Warning   : FEventLog.Warning(MsgText);

      lmt_Critical,
      lmt_Error     : FEventLog.Error(MsgText);

      else
        FEventLog.Debug(MsgText);
    end;

    Result := True;
  end;
end;

constructor TfpEventLogger.Create(const aName: String; AppendToMainLogger: Boolean);
begin
  inherited Create(aName, AppendToMainLogger);

  FEventLog := TEventLog.Create(nil);
end;

destructor TfpEventLogger.Destroy;
begin
  FreeAndNil(FEventLog);

  inherited Destroy;
end;

end.

