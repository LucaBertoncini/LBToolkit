unit uPgNotify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fgl,
  uLBBaseThread, uPQConnectionEx;

type
  TOnPgNotify = procedure(
      const Channel : string;
      const Payload : string;
      JSON          : TJSONData) of object;

  TChannelCallbackMap = specialize TFPGMap<string, TOnPgNotify>;

  { TPgNotifyListener }

  TPgNotifyListener = class(TLBBaseThread)
  private
    FConn           : TPQDirectConnection;
    FHost           : string;
    FPort           : string;
    FDB             : string;
    FUser           : string;
    FPass           : string;
    FChannels       : TStringList;
    FOnNotify       : TOnPgNotify;
    FChannelNotify  : TChannelCallbackMap;
    FReconnectDelay : Integer;
    FConnected      : Boolean;

    function  BuildConnInfo: string;
    function SetupConnection(): Boolean;
    procedure DoListenCommands;
    procedure InternalNotify(Sender: TObject; const AName, APayload: string);
  protected
    procedure Execute; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    { Imposta i parametri prima di avviare il thread }
    procedure Connect(const Host, DBName, User, Pass: string; const Port: string = '5432');

    { Registra un canale (opzionale: con callback dedicata) }
    procedure AddChannel(const Channel: string; Callback: TOnPgNotify = nil);

    property OnNotify        : TOnPgNotify         read FOnNotify       write FOnNotify;
    property OnChannelNotify : TChannelCallbackMap read FChannelNotify;
    property Connected       : Boolean             read FConnected;
  end;

implementation

uses
  ULBLogger;

{ TPgNotifyListener }

constructor TPgNotifyListener.Create;
begin
  inherited Create;

  FreeOnTerminate := False;
  FReconnectDelay := 2000;
  FConnected      := False;
  FPort           := '5432';

  FChannels      := TStringList.Create;
  FChannelNotify := TChannelCallbackMap.Create;

  FConn          := TPQDirectConnection.Create;
  FConn.OnNotify := @InternalNotify;
end;

destructor TPgNotifyListener.Destroy;
begin
  try
    FConn.Close;
  except
    on E: Exception do
      LBLogger.Write(1, 'TPgNotifyListener.Destroy', lmt_Error, E.Message);
  end;

  FChannels.Free;
  FChannelNotify.Free;
  FConn.Free;

  inherited Destroy;
end;

procedure TPgNotifyListener.Connect(const Host, DBName, User, Pass: string;
                                    const Port: string);
begin
  FHost := Host;
  FDB   := DBName;
  FUser := User;
  FPass := Pass;
  FPort := Port;
end;

procedure TPgNotifyListener.AddChannel(const Channel: string; Callback: TOnPgNotify);
begin
  if FChannels.IndexOf(Channel) < 0 then
    FChannels.Add(Channel);

  if Assigned(Callback) then
    FChannelNotify[Channel] := Callback;
end;

function TPgNotifyListener.BuildConnInfo: string;
begin
  { Formato conninfo libpq — le virgolette singole proteggono
    le password con caratteri speciali }
  Result := Format(
    'host=%s port=%s dbname=%s user=%s password=''%s'' ' +
    'connect_timeout=10 application_name=PgNotifyListener',
    [FHost, FPort, FDB, FUser, StringReplace(FPass, '''', '''''', [rfReplaceAll])]);
end;

function TPgNotifyListener.SetupConnection: Boolean;
begin
  FConn.Close;
  FConnected := FConn.Open(BuildConnInfo);
  Result := FConnected;
end;

procedure TPgNotifyListener.DoListenCommands;
var
  i: Integer;
begin
  for i := 0 to FChannels.Count - 1 do
    FConn.ExecuteCommand('LISTEN ' + FChannels[i]);
end;

procedure TPgNotifyListener.InternalNotify(Sender: TObject; const AName, APayload: string);
var
  _JSON : TJSONData;
  _CB   : TOnPgNotify;
begin
  _JSON := nil;
  try
    { Il payload potrebbe non essere JSON — proteggiamo il parse }
    try
      if APayload <> '' then
        _JSON := GetJSON(APayload);
    except
      _JSON := nil;
    end;

    if FChannelNotify.TryGetData(AName, _CB) then
      _CB(AName, APayload, _JSON)
    else if Assigned(FOnNotify) then
      FOnNotify(AName, APayload, _JSON);

  except
    on E: Exception do
      LBLogger.Write(1, 'TPgNotifyListener.InternalNotify', lmt_Error, E.Message);
  end;

  _JSON.Free;
end;

procedure TPgNotifyListener.Execute;
begin
  while not Terminated do
  begin
    try
      if not FConnected then
      begin
        if Self.SetupConnection then
          Self.DoListenCommands;
      end;

      { ProcessNotifications ritorna False solo se la connessione è caduta }
      if not FConn.ProcessNotifications then
      begin
        FConnected := False;
        Self.PauseFor(FReconnectDelay);
      end
      else
        Self.PauseFor(100);  { polling leggero — 10 check/sec }

    except
      on E: Exception do
      begin
        LBLogger.Write(1, 'TPgNotifyListener.InternalExecute',  lmt_Error, E.Message);
        FConnected := False;
        Self.PauseFor(FReconnectDelay);
      end;
    end;
  end;
end;

end.
