unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uPgNotify,
  ULBLogger, fpJson;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FNotifier : TPgNotifyListener;

    procedure LogData(const Channel: string; const Payload: string; JSON: TJSONData);

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.frm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitLogger(5, 'Test_PG_Notifier.log');
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  if FNotifier = nil then
  begin
    FNotifier := TPgNotifyListener.Create;
    FNotifier.Connect('192.168.1.108', 'wms', 'postgres', 'Es0rcicc10!');
    FNotifier.OnNotify := @Self.LogData;
    FNotifier.AddChannel('wms_events');
    FNotifier.Start;
  end;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  if FNotifier <> nil then
  begin
    FNotifier.FreeOnTerminate := False;
    FNotifier.Terminate;
    FNotifier.WaitFor;
    FreeAndNil(FNotifier);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ReleaseLogger();
end;

procedure TfrmMain.LogData(const Channel: string; const Payload: string; JSON: TJSONData);
begin
  LBLogger.Write(1, 'TfrmMain.LogData', lmt_Debug, '<%s>  -  <%s>', [Channel, Payload]);
end;

end.

