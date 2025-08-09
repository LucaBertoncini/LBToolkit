unit uCallbackLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger;

type
  // New callback signature that returns a Boolean to stop propagation.
  TLogCallback = function(aLevel: Integer; aSender: PChar; aMessage: PChar): Boolean; cdecl;

  { TCallbackLogger }
  // A sublogger that forwards log messages to a C-style callback function.
  TCallbackLogger = class(TLBBaseLogger)
  strict private
    FCallback: TLogCallback;
  protected
    function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;
  public
    constructor Create(aCallback: TLogCallback; aMaxLogLevel: Integer); reintroduce;
  end;

implementation

{ TCallbackLogger }

constructor TCallbackLogger.Create(aCallback: TLogCallback; aMaxLogLevel: Integer);
begin
  // Call inherited Create, but pass False for AppendToMainLogger
  // because we will add it manually after creation.
  inherited Create('CallbackLogger', False);
  FCallback := aCallback;
  Self.MaxLogLevel := aMaxLogLevel;
  // By default, enable all message types for a callback logger.
  // The filtering should be done by the callback handler itself.
  Self.EnabledMessages := [Low(TLBLoggerMessageType)..High(TLBLoggerMessageType)];
end;

function TCallbackLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
var
  StopPropagation: Boolean;
begin
  Result := False;
  StopPropagation := False;

  if not Assigned(FCallback) then Exit;
  if not (MsgType in Self.EnabledMessages) then Exit;
  if LogLevel > Self.MaxLogLevel then Exit;

  try
    // Call the callback and get the stop propagation flag
    StopPropagation := FCallback(LogLevel, PChar(Sender), PChar(MsgText));
    Result := True;
  except
    // If the callback fails, we can't do much, but we shouldn't crash the logger.
    Result := False;
  end;

  // If the callback requested to stop propagation, we clear the message
  // so the main logger knows not to process it further.
  if StopPropagation then
  begin
    MsgText := '';
  end;
end;

end.
