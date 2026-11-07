unit uLBUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF Windows}WinSock{$ELSE}BaseUnix{$ENDIF};

type
  TPolygonVertex = record
    X, Y : Integer;
  end;
  TPolygon = array of TPolygonVertex;


function generateGUID(): String;
function HexString(ABuff: pByte; Len: Integer): String;

function DateTime2UnixTimestamp(aDateTime: TDateTime): Int64; inline;
function DateTime2UnixTimestampMs(aDateTime: TDateTime): QWord; inline;
function UnixTimestampMs2DateTime(aUnixTimestamp: Int64): TDateTime; inline;
function UnixTimestampSecs2DateTime(aUnixTimestamp: Int64): TDateTime; inline;

function IsConvexPolygon(const Vertices: TPolygon): Boolean;
function PointInPolygon(const PX, PY: Double; const Polygon: TPolygon): Boolean;


type
  TInterfacedItemSortCompare = function (anItem1, anItem2: IInterface): Integer;

  { TInterfaceListHelper }

  TInterfaceListHelper = class helper for TInterfaceList
    procedure Sort(Const Compare : TInterfacedItemSortCompare);
  end;


const
  gc_SocketTimeout = {$IFDEF Windows}WSAETIMEDOUT{$ELSE}ESysETIMEDOUT{$ENDIF};

implementation

uses
  StrUtils, Math;

function PointInPolygon(const PX, PY: Double; const Polygon: TPolygon): Boolean;
var
  i, j: Integer;
  v1, v2: TPolygonVertex;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;

  j := High(Polygon);
  for i := 0 to High(Polygon) do
  begin
    v1 := Polygon[i];
    v2 := Polygon[j];
    if ((v1.Y > PY) <> (v2.Y > PY)) and
       (PX < (v2.X - v1.X) * (PY - v1.Y) / (v2.Y - v1.Y) + v1.X) then
      Result := not Result;
    j := i;
  end;
end;

function IsConvexPolygon(const Vertices: TPolygon): Boolean;
var
  i, n: Integer;
  Cross, PrevCross: Double;
  ux, uy, vx, vy: Double;
begin
  n := Length(Vertices);
  if n < 3 then Exit(False);
  PrevCross := 0;
  for i := 0 to n - 1 do
  begin
    ux := Vertices[i].X - Vertices[(i - 1 + n) mod n].X;
    uy := Vertices[i].Y - Vertices[(i - 1 + n) mod n].Y;
    vx := Vertices[(i + 1) mod n].X - Vertices[i].X;
    vy := Vertices[(i + 1) mod n].Y - Vertices[i].Y;
    Cross := ux * vy - uy * vx;
    if Cross <> 0 then
    begin
      if PrevCross = 0 then
        PrevCross := Cross
      else if (Cross > 0) <> (PrevCross > 0) then
        Exit(False); // cambio di segno → poligono concavo
    end;
  end;
  Result := True;
end;

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

