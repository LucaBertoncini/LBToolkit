unit uExternalLibrariesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

type

  { TExternalLibraryLoader }

  TExternalLibraryLoader = class(TObject)
    strict private
      type
        TPointerCorrelation = record
          FunctionName : AnsiString;
          FunctionPointer : PPointer;
        end;

        pPointerCorrelation = ^TPointerCorrelation;

      var
        FCorrelations : TList;
        FLibHandle : dynlibs.TLibHandle;

       procedure ClearCorrelations;

    strict protected
      procedure CloseFile();

    public
      constructor Create;
      destructor Destroy; override;

      function LoadLibrary(const aFilename: String; out anErrorMsg: String): Boolean;
      function addFunction(aFunctionName: AnsiString; aDestinationVariable: PPointer): Boolean;
      function isLoaded(): Boolean;

  end;

implementation

uses
  ULBLogger;

{ TExternalLibraryLoader }

procedure TExternalLibraryLoader.ClearCorrelations;
var
  i : Integer;

begin
  try

    for i := FCorrelations.Count - 1 downto 0 do
      Dispose(pPointerCorrelation(FCorrelations.Items[i]));

    FCorrelations.Clear;

  except
    on E: Exception do
      LBLogger.Write(1, 'TExternalLibraryLoader.ClearCorrelations', lmt_Error, PChar(E.Message));
  end;
end;

procedure TExternalLibraryLoader.CloseFile();
begin

  if FLibHandle <> dynlibs.NilHandle then
  begin
    UnloadLibrary(FLibHandle);
    FLibHandle := dynlibs.NilHandle;
  end;

end;

constructor TExternalLibraryLoader.Create;
begin
  inherited Create;

  FCorrelations := TList.Create;
end;

destructor TExternalLibraryLoader.Destroy;
begin
  try

    Self.ClearCorrelations;
    FreeAndNil(FCorrelations);

    Self.CloseFile();


  except
    on E: Exception do
      LBLogger.Write(1, 'TExternalLibraryLoader.Destroy', lmt_Error, PChar(E.Message));
  end;

  inherited Destroy;
end;

function TExternalLibraryLoader.LoadLibrary(const aFilename: String; out anErrorMsg: String): Boolean;
var
  i : Integer;
  _Filename : String;
  _PointerCorrelation : pPointerCorrelation;

begin
  Result := False;
  anErrorMsg := '';

  try
    if aFilename <> '' then
    begin
      _Filename := ExpandFileName(aFilename);
      if FileExists(_Filename) then
      begin
        FLibHandle := dynlibs.LoadLibrary(_Filename);
        if FLibHandle <> dynlibs.NilHandle then
        begin
          Result := True;

          for i := 0 to FCorrelations.Count - 1 do
          begin
            _PointerCorrelation := pPointerCorrelation(FCorrelations.Items[i]);
            _PointerCorrelation^.FunctionPointer^ := dynlibs.GetProcAddress(FLibHandle, _PointerCorrelation^.FunctionName);
            if (_PointerCorrelation^.FunctionPointer^ = nil) then
            begin
              Result := False;
              anErrorMsg := Format('Error retrieving procedure address <%s>', [_PointerCorrelation^.FunctionName]);
              Break;
            end;
          end;
        end
        else
          anErrorMsg := Format('Library <%s> not loaded!', [_Filename]);
      end
      else
        anErrorMsg := Format('Library file <%s> not found!', [_Filename]);

    end
    else
      anErrorMsg := 'No library filename!';


  except
    on E: Exception do
    begin
      Result := False;
      LBLogger.Write(1, 'TExternalLibraryLoader.LoadLibrary', lmt_Error, 'Error loading library <%s>: %s', [aFilename, E.Message]);
    end;
  end;

  if (not Result) then
  begin
    if (FLibHandle <> dynlibs.NilHandle) then
    begin
      UnloadLibrary(FLibHandle);
      FLibHandle := dynlibs.NilHandle;
    end;

    LBLogger.Write(1, 'TExternalLibraryLoader.LoadLibrary', lmt_Warning, 'Library <%s> not loaded: <%s>', [aFilename, anErrorMsg]);
  end;
end;

function TExternalLibraryLoader.addFunction(aFunctionName: AnsiString; aDestinationVariable: PPointer): Boolean;
var
  _PointerCorrelation : pPointerCorrelation;

begin
  Result := False;

  if aFunctionName <> '' then
  begin

    if (aDestinationVariable <> nil) then
    begin

      New(_PointerCorrelation);
      _PointerCorrelation^.FunctionName := aFunctionName;
      _PointerCorrelation^.FunctionPointer := aDestinationVariable;

      FCorrelations.Add(_PointerCorrelation);

    end
    else
      LBLogger.Write(1, 'TExternalLibraryLoader.addFunction', lmt_Warning, 'Destination variable not set!');

  end
  else
    LBLogger.Write(1, 'TExternalLibraryLoader.addFunction', lmt_Warning, 'Function name not set!');
end;

function TExternalLibraryLoader.isLoaded: Boolean;
begin
  Result := FLibHandle <> dynlibs.NilHandle;
end;

end.
