unit uLBActionTypes;
{
  Generic types for the optional LBWebActions extension.
  AuthorizationToken and Metadata are opaque to the extension: the host
  application decides what they mean.

  CONFIGURATION ERRORS: every error is written to the log AT ONCE, one line per
  error, with the standard LBLogger format (level 1, source = aWhere,
  lmt_Error, message). The list only counts them and keeps the messages; at the
  end of the startup RaiseIfAny stops the start with a SINGLE-line exception
  (the details are already in the log).
}
{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, fpjson;

type
  TLBActionAuthorizeEvent = function(Sender: TObject; aUserId: Integer;
    aUserContext: TJSONObject; const anAreaCode, anEndpointCode,
    anActionCode, anAuthorizationToken: String): Boolean of object;

  TLBActionDefinition = class(TObject)
  strict private
    FCode: String;
    FAuthorizationToken: String;
    FMetadata: TStringList;
  public
    constructor Create(const aCode, anAuthorizationToken: String);
    destructor Destroy; override;
    function Clone: TLBActionDefinition;
    property Code: String read FCode;
    property AuthorizationToken: String read FAuthorizationToken;
    property Metadata: TStringList read FMetadata;
  end;

implementation

uses
  ULBLogger;

constructor TLBActionDefinition.Create(const aCode, anAuthorizationToken: String);
begin
  inherited Create;
  FCode := aCode;
  FAuthorizationToken := anAuthorizationToken;
  FMetadata := TStringList.Create;
  FMetadata.CaseSensitive := True;
  FMetadata.NameValueSeparator := '=';
end;

destructor TLBActionDefinition.Destroy;
begin
  FreeAndNil(FMetadata);
  inherited Destroy;
end;

function TLBActionDefinition.Clone: TLBActionDefinition;
begin
  Result := TLBActionDefinition.Create(FCode, FAuthorizationToken);
  Result.Metadata.Assign(FMetadata);
end;

end.
