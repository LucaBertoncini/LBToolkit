unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uLBBaseThread;

type

  { TSleeper }

  TSleeper = class(TLBBaseThread)
    strict private
      FSleepTime : Integer;

    protected
      procedure InternalExecute(); override;

    public
      constructor Create(aSleepTime: Integer); reintroduce;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnThreads: TButton;
    procedure btnThreadsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLongSleep : TSleeper;
    FShortSleep : TSleeper;

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TSleeper }

procedure TSleeper.InternalExecute();
begin
  Self.PauseFor(FSleepTime);
end;

constructor TSleeper.Create(aSleepTime: Integer);
begin
  inherited Create();

  FSleepTime := aSleepTime;
  FreeOnTerminate := True;
end;

{ TfrmMain }

procedure TfrmMain.btnThreadsClick(Sender: TObject);
begin
  if (FLongSleep = nil) and (FShortSleep = nil) then
  begin
    FLongSleep := TSleeper.Create(120000);  // 2 mins
    FLongSleep.AddReference(@FLongSleep);
    FLongSleep.Start();

    FShortSleep := TSleeper.Create(5000);  // 5 secs
    FShortSleep.AddReference(@FShortSleep);
    FShortSleep.Start();

    btnThreads.Caption := 'Destroy threads';
  end
  else begin
    FreeAndNil(FLongSleep);
    FreeAndNil(FShortSleep);

    btnThreads.Caption := 'Create threads';
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLongSleep);
  FreeAndNil(FShortSleep);
end;

end.

