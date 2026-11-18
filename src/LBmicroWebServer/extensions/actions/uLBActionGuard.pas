unit uLBActionGuard;
{
  Generic deny-by-default authorization guard. AuthorizationToken is opaque;
  only the host callback interprets it (and logs the denial, with the user).
}
{$mode ObjFPC}{$H+}
interface

uses
  SysUtils, fpjson, uLBActionTypes;

type
  TLBActionGuard = class(TObject)
  strict private
    FAreaCode: String;
    FEndpointCode: String;
    FDefinition: TLBActionDefinition; // borrowed
    FOnAuthorize: TLBActionAuthorizeEvent;
  public
    constructor Create(const anAreaCode, anEndpointCode: String; aDefinition: TLBActionDefinition; anAuthorize: TLBActionAuthorizeEvent);
    function Authorize(aUserId: Integer; aUserContext: TJSONObject): Boolean;
    property Definition: TLBActionDefinition read FDefinition;
  end;

implementation

uses
  ULBLogger;

constructor TLBActionGuard.Create(const anAreaCode, anEndpointCode: String; aDefinition: TLBActionDefinition; anAuthorize: TLBActionAuthorizeEvent);
const
  cMe = 'TLBActionGuard.Create';
begin
  inherited Create;
  FAreaCode := anAreaCode;
  FEndpointCode := anEndpointCode;
  FDefinition := aDefinition;
  FOnAuthorize := anAuthorize;

  if FDefinition = nil then
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: action definition is missing', [FAreaCode, FEndpointCode])
  else if (FDefinition.AuthorizationToken <> '') and (not Assigned(FOnAuthorize)) then
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>, action <%s>: authorization callback is required', [FAreaCode, FEndpointCode, FDefinition.Code]);
end;

function TLBActionGuard.Authorize(aUserId: Integer;
  aUserContext: TJSONObject): Boolean;
begin
  Result := False;
  if FDefinition = nil then Exit;
  if FDefinition.AuthorizationToken = '' then Exit(True);
  if not Assigned(FOnAuthorize) then Exit;
  Result := FOnAuthorize(Self, aUserId, aUserContext, FAreaCode,
    FEndpointCode, FDefinition.Code, FDefinition.AuthorizationToken);
end;

end.
