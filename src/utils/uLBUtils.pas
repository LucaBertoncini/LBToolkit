unit uLBUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


function generateGUID(): String;

implementation

uses
  StrUtils;

function generateGUID(): String;
var
  _GUID : TGuid;
begin
  Result := '';

  if CreateGUID(_GUID) = 0 then
  begin
    Result := GUIDToString(_GUID);
    Result := ReplaceStr(Result, '{', '');
    Result := ReplaceStr(Result, '}', '');
    Result := ReplaceStr(Result, '-', '');
  end;
end;

end.

