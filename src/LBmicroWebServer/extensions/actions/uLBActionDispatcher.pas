unit uLBActionDispatcher;
{
  Optional action dispatcher for LBmicroWebServer.
  The host application supplies definitions; this unit does not read Routes.xml
  and does not know capabilities, roles, groups, Implies or AI semantics.

  LOG: configuration errors go through TLBActionConfigurationErrors (one log line
  each, written at once). At run time only the server-side anomaly is logged
  (action declared but without worker: 500). A missing/unknown action (400) is
  a client error and is only answered; the denial (403) is logged by the host's
  authorization callback, which knows the user.
}
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, fpjson, contnrs, uWebRouteRegistry,
  uLBActionTypes, uLBActionGuard;

type
  TLBActionDispatcher = class(TObject)
  strict private
    FAreaCode: String;
    FEndpointCode: String;
    FDefinitions: TStringList; // owns TLBActionDefinition
    FGuards: TObjectList;      // owns TLBActionGuard, same indexes
    FWorkerNames: TStringList;
    FWorkers: array of TStandardWorker;
    FOwnsErrors: Boolean;
    FOnAuthorize: TLBActionAuthorizeEvent;
    function GetDefinition(aIndex: Integer): TLBActionDefinition;
    function GetDefinitionCount: Integer;

  public
    constructor Create(const anAreaCode, anEndpointCode: String; anAuthorize: TLBActionAuthorizeEvent);
    destructor Destroy; override;

    function AddDefinition(const anActionCode, anAuthorizationToken: String): TLBActionDefinition;
    function AddWorker(const anActionCode: String; aWorker: TStandardWorker): TLBActionDispatcher;
    function FindDefinition(const anActionCode: String): TLBActionDefinition;
    procedure ValidateBindings;
    function Execute(aUserId: Integer; aUserContext: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    property AreaCode: String read FAreaCode;
    property EndpointCode: String read FEndpointCode;
    property DefinitionCount: Integer read GetDefinitionCount;
    property Definitions[aIndex: Integer]: TLBActionDefinition read GetDefinition;
  end;

function LBActionFailure(out aStatusCode: Integer; aCode: Integer; const aMessage: String): TJSONObject;

implementation

uses
  uHTTPConsts, ULBLogger, uLBActionNames;

function LBActionFailure(out aStatusCode: Integer; aCode: Integer; const aMessage: String): TJSONObject;
begin
  aStatusCode := aCode;
  Result := TJSONObject.Create;
  Result.Add('successful', False);
  Result.Add('error', aMessage);
end;

constructor TLBActionDispatcher.Create(const anAreaCode, anEndpointCode: String; anAuthorize: TLBActionAuthorizeEvent);
begin
  inherited Create;
  FAreaCode := anAreaCode;
  FEndpointCode := anEndpointCode;
  FOnAuthorize := anAuthorize;
  FDefinitions := TStringList.Create;
  FDefinitions.CaseSensitive := True;
  FDefinitions.OwnsObjects := True;
  FGuards := TObjectList.Create(True);
  FWorkerNames := TStringList.Create;
  FWorkerNames.CaseSensitive := True;
end;

destructor TLBActionDispatcher.Destroy;
begin
  FreeAndNil(FWorkerNames);
  FreeAndNil(FGuards);
  FreeAndNil(FDefinitions);
  inherited Destroy;
end;

function TLBActionDispatcher.GetDefinitionCount: Integer;
begin
  Result := FDefinitions.Count;
end;

function TLBActionDispatcher.GetDefinition(
  aIndex: Integer): TLBActionDefinition;
begin
  Result := TLBActionDefinition(FDefinitions.Objects[aIndex]);
end;

function TLBActionDispatcher.FindDefinition(
  const anActionCode: String): TLBActionDefinition;
var
  i: Integer;
begin
  Result := nil;
  i := FDefinitions.IndexOf(anActionCode);
  if i >= 0 then Result := TLBActionDefinition(FDefinitions.Objects[i]);
end;

function TLBActionDispatcher.AddDefinition(const anActionCode,
  anAuthorizationToken: String): TLBActionDefinition;
const
  cMe = 'TLBActionDispatcher.AddDefinition';
var
  _Guard: TLBActionGuard;
begin
  Result := nil;

  if not LBIsValidActionCode(anActionCode) then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: invalid action code <%s>', [FAreaCode, FEndpointCode, anActionCode]);
    Exit;
  end;

  if FDefinitions.IndexOf(anActionCode) >= 0 then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action <%s> declared twice', [FAreaCode, FEndpointCode, anActionCode]);
    Exit;
  end;

  Result := TLBActionDefinition.Create(anActionCode, anAuthorizationToken);
  FDefinitions.AddObject(anActionCode, Result);
  _Guard := TLBActionGuard.Create(FAreaCode, FEndpointCode, Result, FOnAuthorize);
  FGuards.Add(_Guard);
end;

function TLBActionDispatcher.AddWorker(const anActionCode: String;
  aWorker: TStandardWorker): TLBActionDispatcher;
const
  cMe = 'TLBActionDispatcher.AddWorker';
begin
  Result := Self;

  if FindDefinition(anActionCode) = nil then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action <%s> is not defined by the application', [FAreaCode, FEndpointCode, anActionCode]);
    Exit;
  end;

  if not Assigned(aWorker) then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action <%s> has a nil worker', [FAreaCode, FEndpointCode, anActionCode]);
    Exit;
  end;

  if FWorkerNames.IndexOf(anActionCode) >= 0 then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action <%s> registered twice', [FAreaCode, FEndpointCode, anActionCode]);
    Exit;
  end;
  FWorkerNames.Add(anActionCode);
  SetLength(FWorkers, Length(FWorkers) + 1);
  FWorkers[High(FWorkers)] := aWorker;
end;

procedure TLBActionDispatcher.ValidateBindings;
const
  cMe = 'TLBActionDispatcher.ValidateBindings';
var
  i: Integer;
begin
  if FDefinitions.Count = 0 then
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s> has no action definitions', [FAreaCode, FEndpointCode]);

  for i := 0 to FDefinitions.Count - 1 do
  begin
    if FWorkerNames.IndexOf(FDefinitions[i]) < 0 then
      LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action <%s> has no worker', [FAreaCode, FEndpointCode, FDefinitions[i]]);
  end;
end;

function TLBActionDispatcher.Execute(aUserId: Integer; aUserContext: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
const
  cMe = 'TLBActionDispatcher.Execute';
var
  _Node: TJSONData;
  _Code, _Available: String;
  _DefinitionIndex, _WorkerIndex: Integer;
  _Guard: TLBActionGuard;
begin
  _Code := '';
  if aRequestData <> nil then
  begin
    _Node := aRequestData.Find('action');
    if (_Node <> nil) and (_Node.JSONType = jtString) then
      _Code := Trim(_Node.AsString);
  end;
  if _Code = '' then
    Exit(LBActionFailure(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'action is required'));
  _DefinitionIndex := FDefinitions.IndexOf(_Code);
  if _DefinitionIndex < 0 then
  begin
    _Available := StringReplace(FDefinitions.CommaText, ',', ', ',
      [rfReplaceAll]);
    Exit(LBActionFailure(aStatusCode, HTTP_STATUS_BAD_REQUEST,
      'unknown action <' + _Code + '>: use one of ' + _Available));
  end;
  _WorkerIndex := FWorkerNames.IndexOf(_Code);
  if _WorkerIndex < 0 then
  begin
    // non dovrebbe accadere (ValidateBindings blocca l'avvio): e' un difetto del server
    LBLogger.Write(1, cMe, lmt_Error, 'Endpoint <%s.%s>: action <%s> has no worker',
      [FAreaCode, FEndpointCode, _Code]);
    Exit(LBActionFailure(aStatusCode, HTTP_STATUS_INTERNAL_ERROR,
      'action worker is not configured'));
  end;
  _Guard := TLBActionGuard(FGuards[_DefinitionIndex]);
  if not _Guard.Authorize(aUserId, aUserContext) then
    Exit(LBActionFailure(aStatusCode, HTTP_STATUS_FORBIDDEN,
      'Operation not permitted'));
  Result := FWorkers[_WorkerIndex](aUserId, aUserContext, aRequestData,
    aStatusCode);
end;

end.
