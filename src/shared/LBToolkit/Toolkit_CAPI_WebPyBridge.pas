unit Toolkit_CAPI_WebPyBridge;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBWebPyBridgeApplication;


function WebPyBridge_Create(aConfigurationFile: PChar): Pointer; export; cdecl;
procedure WebPyBridge_Destroy(aWebPyBridge: Pointer); export; cdecl;


implementation

uses
  ULBLogger, LResources;

function WebPyBridge_Create(aConfigurationFile: PChar): Pointer; cdecl;
var
  _ConfigFile : String;
  _Application : TLBWebPyBridgeApplication;
  _ErrMsg : String;

begin
  Result := nil;
  if aConfigurationFile <> nil then
  begin
    _ConfigFile := StrPas(aConfigurationFile);
    if FileExists(_ConfigFile) then
    begin
      _Application := TLBWebPyBridgeApplication.Create;
      if _Application.LoadConfiguration(_ConfigFile, _ErrMsg) then
      begin
        _Application.extractPythonFilesFromResources();
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

procedure WebPyBridge_Destroy(aWebPyBridge: Pointer); cdecl;
begin
  if aWebPyBridge <> nil then
    TLBWebPyBridgeApplication(aWebPyBridge).Free;
end;

end.

