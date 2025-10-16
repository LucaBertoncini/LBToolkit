unit uLBSignalManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix;

type
  TSignalManager = class(TObject)
  private
    FSignalValue: cint;
    FAction: PSigActionRec;
    FOldAction: PSigActionRec;
    procedure Reset;
  public
    constructor Create;
    destructor Destroy; override;
    function SetSignalAction(aSignal: Integer; aCallback: TSigactionHandler): Boolean;
  end;


  function BrokenPipeSignalManager(aCallback: tsigactionhandler): TSignalManager;

implementation

uses
  ULBLogger;


function BrokenPipeSignalManager(aCallback: tsigactionhandler): TSignalManager;
begin
  Result := TSignalManager.Create;
  if not Result.SetSignalAction(SIGPIPE, aCallback) then
    FreeAndNil(Result);
end;

{ TSignalManager }

procedure TSignalManager.Reset;
begin
  if FOldAction <> nil then
  begin
    if FSignalValue <> 0 then
      fpSigAction(FSignalValue, FOldAction, nil); // ripristina l'handler precedente
    Dispose(FOldAction);
    FOldAction := nil;
  end;

  if FAction <> nil then
  begin
    Dispose(FAction);
    FAction := nil;
  end;

  FSignalValue := 0;
end;

constructor TSignalManager.Create;
begin
  inherited Create;
  FSignalValue := 0;
  FAction := nil;
  FOldAction := nil;
end;

destructor TSignalManager.Destroy;
begin
  Self.Reset();
  inherited Destroy;
end;

function TSignalManager.SetSignalAction(aSignal: Integer; aCallback: TSigactionHandler): Boolean;
begin
  Result := False;
  Self.Reset();

  if aSignal <> 0 then
  begin

    New(FAction);
    try
      // inizializza la struttura correttamente
      FillChar(FAction^, SizeOf(FAction^), 0);
      FAction^.sa_handler := aCallback;
      FAction^.sa_flags := 0;
      FAction^.sa_restorer := nil;

      New(FOldAction);
      if fpSigAction(aSignal, FAction, FOldAction) = 0 then
      begin
        FSignalValue := aSignal;
        Result := True;
      end
      else begin
        LBLogger.Write(1, 'TSignalManager.SetSignalAction', lmt_Warning, 'Error %d - Cannot catch signal %d!', [fpgeterrno, aSignal]);
        Dispose(FOldAction);
        FOldAction := nil;
        Dispose(FAction);
        FAction := nil;
      end;
    except
      on E: Exception do
      begin
        LBLogger.Write(1, 'TSignalManager.SetSignalAction', lmt_Warning, 'Error catching signal %d: %s', [aSignal, E.Message]);

        if FOldAction <> nil then
        begin
          Dispose(FOldAction);
          FOldAction := nil;
        end;
        if FAction <> nil then
        begin
          Dispose(FAction);
          FAction := nil;
        end;
      end;
    end;

  end;
end;

end.

