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
    lblLog1: TLabel;
    lblLog2: TLabel;
    lblLog3: TLabel;
    ListBox1: TListBox;
    memoLog: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Shape1: TShape;
    Shape2: TShape;
    procedure btnCreateLBLoggerClick(Sender: TObject);
    procedure btnInsertLogClick(Sender: TObject);
    procedure chkCreateLabelLoggerChange(Sender: TObject);
    procedure chkCreateMemoLoggerChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
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

procedure TfrmMain.ListBox1Click(Sender: TObject);
var
  _Idx : Integer;
  _Fields : TLBLoggerFieldArray;

begin
  if LBLogger <> nil then
  begin
    _Idx := ListBox1.ItemIndex;
    if _Idx > -1 then
    begin
      LBLogger.MessageFormat := ListBox1.Items[_Idx];
      case _Idx of
        0, 1: begin
                SetLength(_Fields, 6);
                _Fields[0] := lfDateTime;
                _Fields[1] := lfMessagetype;
                _Fields[2] := lfPID;
                _Fields[3] := lfThread;
                _Fields[4] := lfSource;
                _Fields[5] := lfMessage;
              end;
        2 :   begin
                SetLength(_Fields, 5);
                _Fields[0] := lfDateTime;
                _Fields[1] := lfMessagetype;
                _Fields[2] := lfPID;
                _Fields[3] := lfSource;
                _Fields[4] := lfMessage;
              end;
        3 :   begin
                SetLength(_Fields, 3);
                _Fields[0] := lfDateTime;
                _Fields[1] := lfMessagetype;
                _Fields[2] := lfMessage;
              end;
      end;
      LBLogger.MessageFields := _Fields;
    end;
  end;
end;

procedure TfrmMain.RadioButton1Click(Sender: TObject);
begin
  if LBLogger <> nil then
    LBLogger.DateTimeFormat := TRadioButton(Sender).Caption;
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

