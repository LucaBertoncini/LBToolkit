unit uAVLibraryBaseManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uExternalLibrariesManager;

type

  { TAVLibraryBaseManager }

  TAVLibraryBaseManager = class(TObject)
    strict protected
      FInitialized : Boolean;
      FLibraryLoader : TExternalLibraryLoader;

    public
      constructor Create(aPath: String); virtual;
      destructor Destroy; override;

      property Initialized: Boolean read FInitialized;
  end;

implementation

{ TAVLibraryBaseManager }

constructor TAVLibraryBaseManager.Create(aPath: String);
begin
  inherited Create;

  FLibraryLoader := TExternalLibraryLoader.Create;
  FInitialized := False;
end;

destructor TAVLibraryBaseManager.Destroy;
begin
  FreeAndNil(FLibraryLoader);
  inherited Destroy;
end;

end.
