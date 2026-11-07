unit uExpressionEvaluator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpexprpars;

type
  TExprEvaluator = class(TObject)
  strict private
    FParser: TFPExpressionParser;

  public
    constructor Create;
    destructor Destroy; override;

    // Aggiunge una variabile (se non esiste già) e restituisce il riferimento
    function AddVariable(const AName: string): TFPExprIdentifierDef;

    // Imposta il valore di una variabile già aggiunta
    procedure SetVariableValue(const AName: string; AValue: Variant);

    // Valuta l'espressione senza ripulire gli identificatori
    function Evaluate(const AExpr: string): Boolean;

    // Permette di ripulire tutto esplicitamente
    procedure Clear;
  end;

implementation

uses
  Variants, ULBLogger;

constructor TExprEvaluator.Create;
begin
  inherited Create;

  FParser := TFPExpressionParser.Create(nil);
  FParser.BuiltIns := [bcBoolean]; // per ora solo booleani, eventualmente bcMath se servono funzioni
end;

destructor TExprEvaluator.Destroy;
begin
  FreeAndNil(FParser);
  inherited Destroy;
end;

function TExprEvaluator.AddVariable(const AName: string): TFPExprIdentifierDef;
var
  idx: Integer;

begin
  idx := FParser.Identifiers.IndexOfIdentifier(AName);
  if idx = -1 then
    Result := FParser.Identifiers.AddFloatVariable(AName, 0.0)
  else
    Result := FParser.Identifiers[idx] as TFPExprIdentifierDef;
end;

procedure TExprEvaluator.SetVariableValue(const AName: string; AValue: Variant);
var
  _idx: Integer;
  d: Double;
begin
  // Conversione robusta
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    d := 0.0
  else if VarIsNumeric(AValue) then
    d := AValue
  else if VarIsBool(AValue) then
  begin
    if AValue then d := 1 else d := 0;
  end
  else if VarIsStr(AValue) then
    d := StrToFloatDef(VarToStr(AValue), 0.0)
  else
    d := 0.0;

  _idx := FParser.Identifiers.IndexOfIdentifier(AName);
  if _idx >= 0 then
    FParser.Identifiers.Identifiers[_idx].AsFloat := d
  else
    FParser.Identifiers.AddFloatVariable(AName, d);
end;

function TExprEvaluator.Evaluate(const AExpr: string): Boolean;
begin
  Result := False;

  try
    FParser.Expression := AExpr;
    Result := FParser.Evaluate.ResBoolean;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TExprEvaluator.Evaluate', lmt_Error, 'Expression error <%s>: %s', [AExpr, E.Message]);
      Result := False;
    end;
  end;
end;

procedure TExprEvaluator.Clear;
begin
  FParser.Identifiers.Clear;
end;

end.

