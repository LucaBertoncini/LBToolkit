unit uThreadsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{$IFDEF Linux}, cthreads, unixtype, pthreads{$ENDIF};

type

  { TThreadDebugName }

  TThreadDebugName = class helper for TThread
    procedure setDebugName(const aName: String);
  end;



implementation

{$IFDEF Linux}
function pthread_setname_np(thread:pthread_t; const name:PAnsiChar):integer; cdecl; external libthreads name 'pthread_setname_np';
function pthread_getname_np(thread:pthread_t;name: PAnsiChar;len:size_t):integer; cdecl; external libthreads name 'pthread_getname_np';
{$ENDIF}


{ TThreadDebugName }

procedure TThreadDebugName.setDebugName(const aName: String);
{$IFDEF Linux}
var
  _Name : String;
begin
  _Name := LeftStr(aName, 15);
  pthread_setname_np(FThreadID, PAnsiChar(_Name));
end;
{$ELSE}
begin
end;
{$ENDIF}

end.

