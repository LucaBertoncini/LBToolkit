unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uLBVirtualKeyboard, uLBKeyboardThemes;

{$DEFINE UseBitmap}

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ImageList1: TImageList;
    pnlKeyBoard: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEmptyKey    : TBitmap;
    FBackSpace   : TBitmap;
    FSpaceKey    : TBitmap;
    FReturnKey   : TBitmap;
    FShiftKey    : TBitmap;
    FTabKey      : TBitmap;
    FSwitcherKey : TBitmap;
    FKeyBoard    : TKeyboard;

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  ULBLogger, uLBVKCommons;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitLogger(5, 'LBKeyboard.log');

  FEmptyKey := TBitmap.Create;
  ImageList1.GetBitmap(0, FEmptyKey);

  FBackSpace := TBitmap.Create;
  ImageList1.GetBitmap(1, FBackSpace);

  FSpaceKey := TBitmap.Create;
  ImageList1.GetBitmap(2, FSpaceKey);

  FReturnKey := TBitmap.Create;
  ImageList1.GetBitmap(3, FReturnKey);

  FShiftKey := TBitmap.Create;
  ImageList1.GetBitmap(4, FShiftKey);

  FTabKey := TBitmap.Create;
  ImageList1.GetBitmap(5, FTabKey);

  FSwitcherKey := TBitmap.Create;
  ImageList1.GetBitmap(6, FSwitcherKey);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FKeyBoard);

  FreeAndNil(FEmptyKey);
  FreeAndNil(FBackSpace);
  FreeAndNil(FSpaceKey);
  FreeAndNil(FReturnKey);
  FreeAndNil(FShiftKey);
  FreeAndNil(FTabKey);
  FreeAndNil(FSwitcherKey);

  ReleaseLogger();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  FKeyBoard.DrawKeyboard();
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  _Path : String;
  _Renderer : TKeyRenderer;

begin
//  _Path := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Keyboard_Layout.xml';
  _Path := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Numeric_Keyboard.xml';

  FKeyBoard := TKeyboard.Create(pnlKeyBoard);

  {$IFDEF UseBitmap}
  FKeyBoard.Bitmap[sk_None] := FEmptyKey;
  FKeyBoard.Bitmap[sk_BackSpace] := FBackSpace;
  FKeyBoard.Bitmap[sk_Space] := FSpaceKey;
  FKeyBoard.Bitmap[sk_Return] := FReturnKey;
  FKeyBoard.Bitmap[sk_Shift] := FShiftKey;
  FKeyBoard.Bitmap[sk_Tab] := FTabKey;
  FKeyBoard.Bitmap[sk_Switcher] := FSwitcherKey;

  if FKeyBoard.LoadLayoutFromXMLFile(_Path) then
    FKeyBoard.DrawKeyboard();
  {$ELSE}
  if LBVK_ThemeManager.SetCurrentTheme('Dark Gaming Theme') then
  begin
    _Renderer := TKeyRenderer.Create(LBVK_ThemeManager.CurrentTheme);
    FKeyBoard.SetThemeRenderer(_Renderer);

    if FKeyBoard.LoadLayoutFromXMLFile(_Path) then
      FKeyBoard.DrawKeyboard;
  end;
  {$ENDIF}

end;

end.

