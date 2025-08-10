unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCreateKeyboard: TButton;
    btnHideKeyboard: TButton;
    btnShowKeyboard: TButton;
    btnDestroyKeyboard: TButton;
    Edit1: TEdit;
    pnlKeyboard: TPanel;
    procedure btnDestroyKeyboardClick(Sender: TObject);
    procedure btnCreateKeyboardClick(Sender: TObject);
    procedure btnHideKeyboardClick(Sender: TObject);
    procedure btnShowKeyboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FKeyboardHandle : Pointer;

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uLBToolkitLoader, ULBLogger;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitLogger(5, 'TestVirtualKeyboard.log');
  FKeyboardHandle := nil;
end;

procedure TfrmMain.btnCreateKeyboardClick(Sender: TObject);
begin
  LoadLBToolkit('../../sharedLib/liblbtoolkit.so');
  if isLBToolkitLoaded() then
  begin
    Logger_Initialize('/tmp/LBToolkit.log', 5);
    LBLogger.Write(1, 'TfrmMain.btnCreateKeyboardClick', lmt_Debug, 'Setting handle to %d', [LongInt(Application.Handle)]);

    VKeyboard_Initialize(Application.Handle);

    FKeyboardHandle := VKeyboard_Create(THandle(pnlKeyboard.Handle));
    if FKeyboardHandle <> nil then
    begin
      VKeyboard_LoadLayout(FKeyboardHandle, './Numeric_Keyboard.xml');
      VKeyboard_SetPositionAndSize(FKeyboardHandle, 0, 0, pnlKeyboard.ClientWidth, pnlKeyboard.ClientHeight);
      VKeyboard_Show(FKeyboardHandle);
    end
    else
      LBLogger.Write(1, 'TfrmMain.btnCreateKeyboardClick', lmt_Warning, 'Virtual Keyboard not created!');
  end
  else
    LBLogger.Write(1, 'TfrmMain.btnCreateKeyboardClick', lmt_Warning, 'LBToolkit not initialized!');
end;

procedure TfrmMain.btnHideKeyboardClick(Sender: TObject);
begin
  if isLBToolkitLoaded() then
    VKeyboard_Hide(FKeyboardHandle);
end;

procedure TfrmMain.btnShowKeyboardClick(Sender: TObject);
begin
  if isLBToolkitLoaded() then
    VKeyboard_Show(FKeyboardHandle);
end;

procedure TfrmMain.btnDestroyKeyboardClick(Sender: TObject);
begin
  if isLBToolkitLoaded() then
  begin
    VKeyboard_Destroy(FKeyboardHandle);
    FKeyboardHandle := nil;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ReleaseLogger();
end;

end.

