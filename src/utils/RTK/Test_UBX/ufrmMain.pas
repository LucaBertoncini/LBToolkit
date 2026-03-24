unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UBX_RTKRover;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    edPort: TEdit;
    Label1: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRover : TUBXRTKRover;

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.frm}

uses
  ULBLogger;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitLogger(5, 'UBX_Rover.log');
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  if FRover = nil then
  begin
    FRover := TUBXRTKRover.Create(edPort.Text);
    if not FRover.Open then
      FreeAndNil(FRover);
  end;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  if FRover <> nil then
    FreeAndNil(FRover);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ReleaseLogger();
end;

end.

