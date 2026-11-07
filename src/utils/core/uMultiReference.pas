unit uMultiReference;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  pObject = ^TObject;

  { TMultiReferenceObject }

  TMultiReferenceObject = class(TObject)
    strict protected
      FReferences : TThreadList;  // Lista thread-safe di puntatori a variabili che puntano a questo oggetto
                                  // Al momento della distruzione, tutti i riferimenti vengono impostati a nil

    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure ClearReferences();

      function AddReference(AReference: pObject): Boolean;
      procedure RemoveReference(AReference: pObject);
  end;


implementation

uses
  ULBLogger;

{ TMultiReferenceObject }

constructor TMultiReferenceObject.Create;
begin
  inherited Create;

  FReferences := TThreadList.Create;
end;

destructor TMultiReferenceObject.Destroy;
begin

  if Self <> nil then
  begin

    Self.ClearReferences();

    FreeAndNil(FReferences);

    inherited Destroy;
  end;

end;

procedure TMultiReferenceObject.ClearReferences();
var
  i : Integer;
  _List : TList;

begin

  if FReferences <> nil then
  begin
    _List := FReferences.LockList();

    try

      for i := 0 to _List.Count - 1 do
        pObject(_List.Items[i])^ := nil;

    except
      on E: Exception do
        LBLogger.Write(1, 'TMultiReferenceObject.ClearReferences', lmt_Error, '1. %s - %s', [Self.ClassName, E.Message]);
    end;

    try

      _List.Clear;

    except
      on E: Exception do
        LBLogger.Write(1, 'TMultiReferenceObject.ClearReferences', lmt_Error, '1. %s - %s', [Self.ClassName, E.Message]);
    end;

    FReferences.UnlockList;

  end;
end;

function TMultiReferenceObject.AddReference(AReference: pObject): Boolean;
var
  _Idx : Integer;
  _List : TList;

begin
  Result := False;

  if AReference <> nil then
  begin

    try
      _List := FReferences.LockList();
      _Idx := _List.IndexOf(AReference);
      if _Idx = -1 then
        _List.Add(AReference);

      Result := True;
    finally
      FReferences.UnlockList;
    end;

  end;
end;

procedure TMultiReferenceObject.RemoveReference(AReference: pObject);
var
  _Idx : Integer;
  _List : TList;

begin
  if AReference <> nil then
  begin

    try
      _List := FReferences.LockList();

      _Idx := _List.IndexOf(AReference);
      if _Idx > -1 then
        _List.Delete(_Idx);

    finally
      FReferences.UnlockList();
    end;
  end;
end;


end.

