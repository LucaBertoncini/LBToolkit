unit uLBUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


function generateGUID(): String;
function HexString(ABuff: pByte; Len: Integer): String;

function DateTime2UnixTimestamp(aDateTime: TDateTime): Int64; inline;
function DateTime2UnixTimestampMs(aDateTime: TDateTime): QWord; inline;
function UnixTimestampMs2DateTime(aUnixTimestamp: Int64): TDateTime; inline;
function UnixTimestampSecs2DateTime(aUnixTimestamp: Int64): TDateTime; inline;


type
  TInterfacedItemSortCompare = function (anItem1, anItem2: IInterface): Integer;

  { TInterfaceListHelper }

  TInterfaceListHelper = class helper for TInterfaceList
    procedure Sort(Const Compare : TInterfacedItemSortCompare);
  end;

implementation

uses
  StrUtils, Math;

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

function HexString(ABuff: pByte; Len: Integer): String;
var
   i : Integer;

begin
  Result := '';

  for i := 1 to Len do
  begin
    Result += IntToHex(ABuff^, 2);
    Inc(ABuff);
  end;
end;


function DateTime2UnixTimestampMs(aDateTime: TDateTime): QWord;
begin
  Result := Round((aDateTime - UnixEpoch) * MSecsPerDay);
end;

function UnixTimestampSecs2DateTime(aUnixTimestamp: Int64): TDateTime; inline;
begin
  Result := UnixEpoch + (aUnixTimestamp / SecsPerDay);
end;

function UnixTimestampMs2DateTime(aUnixTimestamp: Int64): TDateTime;
begin
  Result := UnixEpoch + (aUnixTimestamp / MSecsPerDay);
end;

function DateTime2UnixTimestamp(aDateTime: TDateTime): Int64;
begin
  Result := Round((aDateTime - UnixEpoch) * SecsPerDay);
end;

{ TInterfaceListHelper }

procedure TInterfaceListHelper.Sort(const Compare: TInterfacedItemSortCompare);
var
  i, j : Integer;
  _ItemPivot : IInterface;

begin
  for i := 1 to Self.Count - 1 do
  begin
    _ItemPivot := Self.Items[i];
    j := i;
    While ((j >= 1) and (Compare(Self.Items[j - 1], _ItemPivot) = GreaterThanValue)) do
    begin
      Self.Items[j] := Self.Items[j - 1];
      j -= 1;
    end;
    Self.Items[j] := _ItemPivot;
  end;
end;


end.

