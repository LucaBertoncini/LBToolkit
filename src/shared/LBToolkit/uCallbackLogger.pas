unit uCallbackLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ULBLogger;

type
  // New callback signature that returns a Boolean to stop propagation.
  TLogCallback = function(aLevel: Integer; aSender: PChar; aMsgType: Byte; aMessage: PChar; aStopPropagation: PBoolean): Boolean; cdecl;

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
  inherited Create('CallbackLogger', True);
  FCallback := aCallback;
  Self.MaxLogLevel := aMaxLogLevel;
  // By default, enable all message types for a callback logger.
  // The filtering should be done by the callback handler itself.
  Self.EnabledMessages := [Low(TLBLoggerMessageType)..High(TLBLoggerMessageType)];
end;

function TCallbackLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
var
  _StopPropagation: Boolean;
begin
  Result := False;
  _StopPropagation := False;

  if Assigned(FCallback) and (MsgType in Self.EnabledMessages) and (LogLevel <= Self.MaxLogLevel) then
  begin

    try
      // Call the callback and get the stop propagation flag
      Result := FCallback(LogLevel, PChar(Sender), Byte(MsgType), PChar(MsgText), @_StopPropagation);
    except
      // If the callback fails, we can't do much, but we shouldn't crash the logger.
      Result := False;
    end;

    // If the callback requested to stop propagation, we clear the message
    // so the main logger knows not to process it further.
    if _StopPropagation then
      MsgText := '';
  end;
end;

end.
