unit uCallbackLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger;

type
  // Define the C-style callback function pointer type
  TLogCallback = procedure(aLevel: Integer; aSender: PChar; aMessage: PChar); cdecl;

  { TCallbackLogger }
  // A sublogger that forwards log messages to a C-style callback function.
  TCallbackLogger = class(TLBBaseLogger)
  strict private
    FCallback: TLogCallback;
  protected
    function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;
  public
    constructor Create(aCallback: TLogCallback); reintroduce;
  end;

implementation

{ TCallbackLogger }

constructor TCallbackLogger.Create(aCallback: TLogCallback);
begin
  // Call inherited Create, but pass False for AppendToMainLogger
  // because we will add it manually after creation.
  inherited Create('CallbackLogger', False);
  FCallback := aCallback;
end;

function TCallbackLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  Result := False;
  if not Assigned(FCallback) then Exit;
  if not (MsgType in Self.EnabledMessages) then Exit;
  if LogLevel > Self.MaxLogLevel then Exit;

  try
    FCallback(LogLevel, PChar(Sender), PChar(MsgText));
    Result := True;
  except
    // If the callback fails, we can't do much, but we shouldn't crash the logger.
    // A potential improvement could be to log this failure to the main file logger.
  end;
end;

end.
