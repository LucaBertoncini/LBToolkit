unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ULBLogger, uLBLoggerEx;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCreateLBLogger: TButton;
    btnInsertLog: TButton;
    chkCreateMemoLogger: TCheckBox;
    chkCreateLabelLogger: TCheckBox;
    edLog: TEdit;
    lblLog: TLabel;
    memoLog: TMemo;
    Shape1: TShape;
    procedure btnCreateLBLoggerClick(Sender: TObject);
    procedure btnInsertLogClick(Sender: TObject);
    procedure chkCreateLabelLoggerChange(Sender: TObject);
    procedure chkCreateMemoLoggerChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMemoLogger : TMemoLogger;
    FLabelLogger : TLabelLogger;

    procedure DestroyLoggers();
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnCreateLBLoggerClick(Sender: TObject);
begin
  if LBLogger = nil then
  begin
    if InitLogger(5, 'TestLogger', False, True) then
    begin
      btnCreateLBLogger.Caption := 'Destroy LBLogger';
      chkCreateLabelLogger.Enabled := True;
      chkCreateMemoLogger.Enabled := True;
      ShowMessage('Log file: ' + LBLogger.FilePath + LBLogger.FileName);
    end;
  end
  else begin
    Self.DestroyLoggers();
    chkCreateLabelLogger.Checked := False;
    chkCreateMemoLogger.Checked := False;
    chkCreateLabelLogger.Enabled := False;
    chkCreateMemoLogger.Enabled := False;
  end;
end;

procedure TfrmMain.btnInsertLogClick(Sender: TObject);
var
  _Msg : String;

begin
  _Msg := edLog.Text;
  if _Msg <> '' then
  begin
    LBLogger.Write(1, 'TfrmMain.btnInsertLogClick', lmt_Info, 'Info: <%s>', [_Msg]);
    LBLogger.Write(1, 'TfrmMain.btnInsertLogClick', lmt_Error, 'Error: <%s>', [_Msg]);
    LBLogger.Write(1, 'TfrmMain.btnInsertLogClick', lmt_Warning, 'Warning: <%s>', [_Msg]);
    LBLogger.Write(1, 'TfrmMain.btnInsertLogClick', lmt_Debug, 'Debug: <%s>', [_Msg]);
  end;
end;

procedure TfrmMain.chkCreateLabelLoggerChange(Sender: TObject);
begin
  if chkCreateLabelLogger.Checked then
  begin
    if FLabelLogger = nil then
    begin
      FLabelLogger := TLabelLogger.Create;
      FLabelLogger.ShowMessageType := False;
      FLabelLogger.aLabel := lblLog;
    end;
  end
  else begin
    if FLabelLogger <> nil then
      FreeAndNil(FLabelLogger);
  end;
end;

procedure TfrmMain.chkCreateMemoLoggerChange(Sender: TObject);
begin
  if chkCreateMemoLogger.Checked then
  begin
    if FMemoLogger = nil then
    begin
      FMemoLogger := TMemoLogger.Create;
      FMemoLogger.ShowMessageType := True;
      FMemoLogger.Memo := memoLog;
    end;
  end
  else begin
    if FMemoLogger <> nil then
      FreeAndNil(FMemoLogger);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Self.DestroyLoggers();
end;

procedure TfrmMain.DestroyLoggers();
begin
  if FMemoLogger <> nil then
    FreeAndNil(FMemoLogger);

  if FLabelLogger <> nil then
    FreeAndNil(FLabelLogger);

  ReleaseLogger();
end;

end.

