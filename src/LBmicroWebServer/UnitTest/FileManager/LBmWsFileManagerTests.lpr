program LBmWsFileManagerTests;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, uLBmWsFileManagerTests, consoletestrunner;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'File Manager Tests';
  Application.Run;
  Application.Free;
end.
