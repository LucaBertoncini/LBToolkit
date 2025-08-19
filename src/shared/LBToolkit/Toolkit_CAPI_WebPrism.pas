unit Toolkit_CAPI_WebPrism;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBWebPrismApplication;


function WebPrism_Create(aConfigurationFile: PChar): Pointer; export; cdecl;
procedure WebPrism_Destroy(aWebPyBridge: Pointer); export; cdecl;


implementation

uses
  ULBLogger, LResources;

function WebPrism_Create(aConfigurationFile: PChar): Pointer; cdecl;
var
  _ConfigFile : String;
  _Application : TLBWebPrismApplication;
  _ErrMsg : String;

begin
  Result := nil;
  if aConfigurationFile <> nil then
  begin
    _ConfigFile := StrPas(aConfigurationFile);
    if FileExists(_ConfigFile) then
    begin
      _Application := TLBWebPrismApplication.Create;
      if _Application.LoadConfiguration(_ConfigFile, _ErrMsg) then
      begin
        _Application.extractPythonFilesFromResources();
        _Application.extractNodeJsFilesFromResources();
        Result := Pointer(_Application);
        _Application.Activate();
      end
      else
        _Application.Free;
    end
    else
      LBLogger.Write(1, 'WebPyBridge_Create', lmt_Warning, 'Configuration file <%s> not found!', [_ConfigFile]);

  end
  else
    LBLogger.Write(1, 'WebPyBridge_Create', lmt_Warning, 'Configuration file not set!');
end;

procedure WebPrism_Destroy(aWebPyBridge: Pointer); cdecl;
begin
  if aWebPyBridge <> nil then
    TLBWebPrismApplication(aWebPyBridge).Free;
end;

end.

