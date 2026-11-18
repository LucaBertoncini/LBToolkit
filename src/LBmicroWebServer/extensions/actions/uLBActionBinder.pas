unit uLBActionBinder;
{
  RTTI binding for the optional LBWebActions extension.
  Each kebab-case action is bound to a published DoPascalCase method.
  A missing method is written to the log at once (one line, through the
  dispatcher's TLBActionConfigurationErrors) and the scan goes on, so a single
  start lists every missing method.
}
{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, uLBActionDispatcher;

type
  TLBActionBinder = class(TObject)
  public
    class procedure BindObject(aDispatcher: TLBActionDispatcher; aObject: TObject);
  end;

implementation

uses
  ULBLogger, uWebRouteRegistry, uLBActionNames;

class procedure TLBActionBinder.BindObject(aDispatcher: TLBActionDispatcher; aObject: TObject);
const
  cMe = 'TLBActionBinder.BindObject';

var
  i: Integer;
  _MethodName: String;
  _Method: TMethod;
  _Worker: TStandardWorker;

begin
  if aDispatcher = nil then Exit;

  if aObject = nil then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'Endpoint <%s.%s>: object is not assigned', [aDispatcher.AreaCode, aDispatcher.EndpointCode]);
    Exit;
  end;

  for i := 0 to aDispatcher.DefinitionCount - 1 do
  begin
    _MethodName := LBActionCodeToMethodName(aDispatcher.Definitions[i].Code);
    _Method.Code := aObject.MethodAddress(_MethodName);
    _Method.Data := aObject;
    if _Method.Code = nil then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, 'Action <%s> not implemented by %s: expected method <%s>', [aDispatcher.Definitions[i].Code, aObject.ClassName, _MethodName]);
      Continue;
    end;
    _Worker := TStandardWorker(_Method);
    aDispatcher.AddWorker(aDispatcher.Definitions[i].Code, _Worker);
  end;
end;

end.
