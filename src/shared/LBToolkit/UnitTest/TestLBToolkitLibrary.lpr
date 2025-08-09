program TestLBToolkitLibrary;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, consoletestrunner, uLBToolkitLibraryTests, uLBToolkitLoader, ULBLogger;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  end;

var
  Application: TMyTestRunner;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitLogger(5, 'TestLBToolkitLib.log');
end;

destructor TMyTestRunner.Destroy;
begin
  ReleaseLogger();
  inherited Destroy;
end;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'Testing LBToolkit library';
  Application.Run;
  Application.Free;
end.
