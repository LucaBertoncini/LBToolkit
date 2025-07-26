unit uTimedoutCriticalSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTimedOutCriticalSection }
  // Classe che gestisce l'accesso concorrente a una sezione critica,
  // con timeout configurabile e log degli accessi.

  TTimedOutCriticalSection = class(TObject)
    strict private
      FCriticalSection : TRTLCriticalSection; // struttura low-level per la sincronizzazione
      FLastOwner : String;                     // chi ha acquisito per ultimo la sezione critica
      FLastRelease : String;                   // chi l'ha rilasciata per ultimo

    public
      constructor Create;
      destructor Destroy; override;

      function Acquire(const aFunctionName: String; aMaxWaitTimeMs: Integer = 3000): Boolean;
      procedure Release();
  end;


implementation

uses
  ULBLogger;

{ TTimedOutCriticalSection }

constructor TTimedOutCriticalSection.Create;
begin
  inherited Create;

  FLastRelease := '';
  InitCriticalSection(FCriticalSection); // inizializza la sezione critica
end;

destructor TTimedOutCriticalSection.Destroy;
begin
  try
    DoneCriticalSection(FCriticalSection); // rilascia risorse
  except
    on E: Exception do
      LBLogger.Write(1, 'TTimedOutCriticalSection.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

function TTimedOutCriticalSection.Acquire(const aFunctionName: String; aMaxWaitTimeMs: Integer = 3000): Boolean;
var
  _WaitedTimeMs : Integer = 0;

const
  cSleepTimeout = Integer(10); // intervallo di attesa in millisecondi tra i tentativi

begin
  Result := False;

  if Self <> nil then
  begin
    repeat
      Result := System.TryEnterCriticalSection(FCriticalSection) <> 0;
      if Result then
      begin
        FLastOwner := aFunctionName;
        Break;
      end
      else
      begin
        Sleep(cSleepTimeout);
        Inc(_WaitedTimeMs, cSleepTimeout);
      end;
    until _WaitedTimeMs >= aMaxWaitTimeMs;

    // log in caso di timeout
    if not Result then
      LBLogger.Write(1, 'TTimedOutCriticalSection.Acquire', lmt_Warning, 'Critical section not acquired by <%s> after %d msecs! Last owner: <%s>', [aFunctionName, _WaitedTimeMs, FLastOwner]);
  end
  else
    LBLogger.Write(1, 'TTimedOutCriticalSection.Acquire', lmt_Warning, 'I am nil, <%s> !!! Why are you calling me !?', [aFunctionName]);

end;

procedure TTimedOutCriticalSection.Release();
begin
  try
    if Self <> nil then
    begin
      FLastRelease := FLastOwner;
      FLastOwner := '';
      LeaveCriticalSection(FCriticalSection);
    end
    else
      LBLogger.Write(1, 'TTimedOutCriticalSection.Release', lmt_Warning, 'I''m nil! Why are you calling me?!?!');
  except
    on E: Exception do
      LBLogger.Write(1, 'TTimedOutCriticalSection.Release', lmt_Error, 'Error releasing section for owner <%s>: %s ', [FLastRelease, E.Message]);
  end;
end;

end.
