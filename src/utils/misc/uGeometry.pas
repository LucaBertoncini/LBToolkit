unit uGeometry;

{$mode ObjFPC}{$H+}

interface

uses
  Types;  // for TPoint, TRect (già definiti in FreePascal)

type
  TPolygon = array of TPoint;

// ----------------------------------------------------------------------------
// Verifica se un punto è all'interno di un poligono (anche concavo)
// ----------------------------------------------------------------------------
// Algoritmo: ray casting (even-odd rule)
// ----------------------------------------------------------------------------
// Si traccia una semiretta orizzontale verso destra a partire dal punto P.
// Si contano le intersezioni con i lati del poligono.
// - Se il numero di intersezioni è dispari → punto interno
// - Se pari → punto esterno
//
// Schema:
//
//                / \
//               /   \   <-- poligono
//              /     \
//             /       \
//            /         \
//      o----P---------->   (semiretta orizzontale)
//        (intersezione con il lato)
//
// Nel punto P la semiretta interseca un solo lato (dispari) → interno.
// ----------------------------------------------------------------------------
function PointInPolygon(const P: TPoint; const Poly: TPolygon): Boolean;

// Calcola l'intersezione tra due rettangoli asse-allineati
function RectIntersect(const R1, R2: TRect; out Intersection: TRect): Boolean;

// Calcola il rapporto area(Intersezione) / area(A) (overlap di A rispetto a B)
function GetOverlapRatio(const A, B: TRect): Double;

// ----------------------------------------------------------------------------
// Verifica se un poligono è convesso
// ----------------------------------------------------------------------------
// Algoritmo: controllo del segno del prodotto vettoriale (cross product)
// ----------------------------------------------------------------------------
// Per ogni tripletta di vertici consecutivi (P[i], P[i+1], P[i+2]) si calcola
// il cross product dei vettori (P[i+1]-P[i]) e (P[i+2]-P[i+1]).
// In un poligono convesso, tutti i cross product hanno lo stesso segno
// (tutti positivi per vertici in ordine antiorario, tutti negativi per orario).
// Appena si trova un cambiamento di segno, il poligono è concavo.
//
// Schema:
//
//   Convesso (segno costante):          Concavo (segno cambia):
//
//        C                               C
//       / \                             / \
//      /   \                           /   \
//     /     \                         /     \
//    A-------B                       A       B
//     \     /                         \     /
//      \   /                           \   /
//       \ /                             \ /
//        D                               D
//   (tutti i cross positivi)        (alcuni negativi)
// ----------------------------------------------------------------------------
function IsConvexPolygon(const Poly: TPolygon): Boolean;

// Calcola il rettangolo minimo che racchiude il poligono (bounding box)
function PolygonToRect(const Poly: TPolygon): TRect;

implementation

uses
  Math;

function PointInPolygon(const P: TPoint; const Poly: TPolygon): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  j := High(Poly);
  for i := 0 to High(Poly) do
  begin
    // Condizione: il segmento (Poly[i], Poly[j]) attraversa la semiretta orizzontale
    // verso destra. L'attraversamento avviene se:
    //  - l'estremo Y del segmento è al di sopra del punto e l'altro al di sotto (o viceversa)
    //  - il punto X è a sinistra dell'intersezione della retta (Poly[i],Poly[j]) con la semiretta.
    if ((Poly[i].Y > P.Y) <> (Poly[j].Y > P.Y)) and
       (P.X < (Poly[j].X - Poly[i].X) * (P.Y - Poly[i].Y) / (Poly[j].Y - Poly[i].Y) + Poly[i].X) then
      Result := not Result;   // inverte lo stato a ogni attraversamento
    j := i;
  end;
end;

function RectIntersect(const R1, R2: TRect; out Intersection: TRect): Boolean;
begin
  Intersection.Left   := Max(R1.Left, R2.Left);
  Intersection.Top    := Max(R1.Top, R2.Top);
  Intersection.Right  := Min(R1.Right, R2.Right);
  Intersection.Bottom := Min(R1.Bottom, R2.Bottom);
  Result := (Intersection.Left < Intersection.Right) and (Intersection.Top < Intersection.Bottom);
end;

function GetOverlapRatio(const A, B: TRect): Double;
var
  inter: TRect;
  areaA, areaInter: Double;
begin
  if RectIntersect(A, B, inter) then
  begin
    areaA := (A.Right - A.Left) * (A.Bottom - A.Top);
    areaInter := (inter.Right - inter.Left) * (inter.Bottom - inter.Top);
    if areaA > 0 then
      Result := areaInter / areaA
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function IsConvexPolygon(const Poly: TPolygon): Boolean;
var
  i, n: Integer;
  cross, prevCross: Integer;
  p1, p2, p3: TPoint;
begin
  n := Length(Poly);
  if n < 3 then Exit(False);
  prevCross := 0;
  for i := 0 to n - 1 do
  begin
    p1 := Poly[i];
    p2 := Poly[(i + 1) mod n];
    p3 := Poly[(i + 2) mod n];
    // prodotto vettoriale (cross product) tra i lati (p2-p1) e (p3-p2)
    cross := (p2.X - p1.X) * (p3.Y - p2.Y) - (p2.Y - p1.Y) * (p3.X - p2.X);
    if cross <> 0 then
    begin
      if prevCross = 0 then
        prevCross := cross
      else if (prevCross > 0) <> (cross > 0) then
        Exit(False);   // segno cambiato: poligono concavo
    end;
  end;
  Result := True;
end;

function PolygonToRect(const Poly: TPolygon): TRect;
var
  i: Integer;
begin
  Result := Rect(0,0,0,0);
  if Length(Poly) = 0 then Exit;
  Result.Left   := Poly[0].X;
  Result.Right  := Result.Left;
  Result.Top    := Poly[0].Y;
  Result.Bottom := Result.Top;
  for i := 1 to High(Poly) do
  begin
    if Poly[i].X < Result.Left   then Result.Left   := Poly[i].X;
    if Poly[i].X > Result.Right  then Result.Right  := Poly[i].X;
    if Poly[i].Y < Result.Top    then Result.Top    := Poly[i].Y;
    if Poly[i].Y > Result.Bottom then Result.Bottom := Poly[i].Y;
  end;
end;

end.
