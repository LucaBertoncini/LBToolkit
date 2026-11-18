unit uLBActionNames;
{
  Name conventions shared by LBWebActions clients.
  HTTP action codes are strict kebab-case and map to published Pascal methods.
}
{$mode ObjFPC}{$H+}
interface

uses
  SysUtils;

function LBIsValidActionCode(const anActionCode: String): Boolean;
function LBActionCodeToMethodName(const anActionCode: String): String;
function LBActionCodeToDescribeMethodName(const anActionCode: String): String;

implementation

function LBIsValidActionCode(const anActionCode: String): Boolean;
var
  i: Integer;
  _AfterDash: Boolean;
begin
  Result := False;
  if (anActionCode = '') or not (anActionCode[1] in ['a'..'z']) then Exit;
  _AfterDash := False;
  for i := 1 to Length(anActionCode) do
  begin
    if anActionCode[i] = '-' then
    begin
      if (i = 1) or (i = Length(anActionCode)) or _AfterDash then Exit;
      _AfterDash := True;
    end
    else
    begin
      if not (anActionCode[i] in ['a'..'z', '0'..'9']) then Exit;
      if _AfterDash and not (anActionCode[i] in ['a'..'z']) then Exit;
      _AfterDash := False;
    end;
  end;
  Result := not _AfterDash;
end;

function LBActionCodeToMethodName(const anActionCode: String): String;
var
  i: Integer;
  _UpperNext: Boolean;
  _C: Char;
begin
  if not LBIsValidActionCode(anActionCode) then
    raise Exception.CreateFmt('Invalid action code <%s>', [anActionCode]);
  Result := 'Do';
  _UpperNext := True;
  for i := 1 to Length(anActionCode) do
  begin
    _C := anActionCode[i];
    if _C = '-' then
      _UpperNext := True
    else if _UpperNext then
    begin
      Result := Result + UpCase(_C);
      _UpperNext := False;
    end
    else
      Result := Result + _C;
  end;
end;

function LBActionCodeToDescribeMethodName(const anActionCode: String): String;
begin
  Result := 'Describe' + Copy(LBActionCodeToMethodName(anActionCode), 3, MaxInt);
end;

end.
