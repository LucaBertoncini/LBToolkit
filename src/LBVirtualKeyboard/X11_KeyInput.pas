unit X11_KeyInput;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  Classes, SysUtils, Controls, Forms, X, XLib, KeySym, uVKExt, Base_KeyInput;

type

  { TXKeyInput }

  TXKeyInput = class(TKeyInput)
    strict private
      FDisplay : PDisplay;

  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Press(Key: Word); override;

  end;

function InitializeKeyInput: TKeyInput;

function XTestFakeKeyEvent(dpy: PDisplay; keycode: dword; is_press: Boolean; delay: dword): longint; cdecl; external;
function XTestGrabControl(dpy: PDisplay; impervious: Boolean): longint; cdecl; external;

var
  KeyInput: TKeyInput;


implementation

uses
  LCLType, ULBLogger;

function InitializeKeyInput: TKeyInput;
begin
  Result := TXKeyInput.Create;
end;

function VirtualKeyToXKeySym(Key: Word): TKeySym;
begin
  case Key of
    VK_BACK: Result := XK_BackSpace;
    VK_TAB: Result := XK_Tab;
    VK_CLEAR: Result := XK_Clear;
    VK_RETURN: Result := XK_Return;
    VK_SHIFT: Result := XK_Shift_L;
    VK_CONTROL: Result := XK_Control_L;
    VK_MENU: Result := XK_VoidSymbol; // alt key crashes app, XK_Alt_R;
    VK_CAPITAL: Result := XK_Caps_Lock;

    VK_ESCAPE: Result := XK_Escape;
    VK_SPACE: Result := XK_space;
    VK_PRIOR: Result := XK_Prior;
    VK_NEXT: Result := XK_Next;
    VK_END: Result := XK_End;
    VK_HOME: Result := XK_Home;
    VK_LEFT: Result := XK_Left;
    VK_UP: Result := XK_Up;
    VK_RIGHT: Result := XK_Right;
    VK_DOWN: Result := XK_Down;
    VK_SELECT: Result := XK_Select;
    VK_PRINT: Result := XK_Print;
    VK_EXECUTE: Result := XK_Execute;

    VK_OEM_COMMA: Result := XK_comma;
    VK_OEM_PERIOD: Result := XK_period;
    VK_OEM_PLUS: Result := XK_plus;
    VK_OEM_MINUS: Result := XK_minus;

    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_HELP: Result := XK_Help;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;

    VK_A: Result := XK_a;
    VK_B: Result := XK_b;
    VK_C: Result := XK_c;
    VK_D: Result := XK_d;
    VK_E: Result := XK_e;
    VK_F: Result := XK_f;
    VK_G: Result := XK_g;
    VK_H: Result := XK_h;
    VK_I: Result := XK_i;
    VK_J: Result := XK_j;
    VK_K: Result := XK_k;
    VK_L: Result := XK_l;
    VK_M: Result := XK_m;
    VK_N: Result := XK_n;
    VK_O: Result := XK_o;
    VK_P: Result := XK_p;
    VK_Q: Result := XK_q;
    VK_R: Result := XK_r;
    VK_S: Result := XK_s;
    VK_T: Result := XK_t;
    VK_U: Result := XK_u;
    VK_V: Result := XK_v;
    VK_W: Result := XK_w;
    VK_X: Result := XK_x;
    VK_Y: Result := XK_y;
    VK_Z: Result := XK_z;

    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_Multiply;
    VK_ADD: Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT: Result := XK_KP_Subtract;
    VK_DECIMAL: Result := XK_KP_Decimal;
    VK_DIVIDE: Result := XK_KP_Divide;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17;
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;
    VK_NUMLOCK: Result := XK_Num_Lock;
    VK_SCROLL: Result := XK_Scroll_Lock;

    VK_Agrave : Result := XK_agrave;
    VK_Egrave : Result := XK_egrave;
    VK_Igrave : Result := XK_igrave;
    VK_Ograve : Result := XK_ograve;
    VK_Ugrave : Result := XK_ugrave;

    VK_QuestionMark : Result := XK_question;
    VK_BackSlash    : Result := XK_backslash;
    VK_Slash        : Result := XK_slash;
    VK_Semicolon    : Result := XK_semicolon;

    VK_Equal        : Result := XK_equal;

  else
    LBLogger.Write(1, 'X11_KeyInput.VirtualKeyToXKeySym', lmt_Warning, 'Virtual key code %d not mapped on X11!', [Key]);
    Result := XK_VoidSymbol;
  end;
end;

{ TXKeyInput }

procedure TXKeyInput.DoDown(Key: Word);
var
  KeySym: TKeySym;

begin
  KeySym := VirtualKeyToXKeySym(Key);
  if KeySym = XK_VoidSymbol then Exit;

  XTestFakeKeyEvent(FDisplay, XKeysymToKeycode(FDisplay, KeySym), True, 0);
end;

procedure TXKeyInput.DoUp(Key: Word);
var
  KeySym: TKeySym;
begin
  KeySym := VirtualKeyToXKeySym(Key);
  if KeySym = XK_VoidSymbol then Exit;

  XTestFakeKeyEvent(FDisplay, XKeysymToKeycode(FDisplay, KeySym), False, 0);
  XSync(FDisplay, False);
end;

constructor TXKeyInput.Create;
begin
  inherited Create;

  FDisplay := XOpenDisplay(nil);
end;

destructor TXKeyInput.Destroy;
begin
  XCloseDisplay(FDisplay);

  inherited Destroy;
end;

procedure TXKeyInput.Press(Key: Word);
var
  KeySym: TKeySym;

  _KeyCode : TKeyCode;

begin

  KeySym := VirtualKeyToXKeySym(Key);
  if KeySym = XK_VoidSymbol then Exit;

  _KeyCode := XKeysymToKeycode(FDisplay, KeySym);

  XTestFakeKeyEvent(FDisplay, _KeyCode, True, 0);
  XTestFakeKeyEvent(FDisplay, _KeyCode, False, 0);

  XSync(FDisplay, False);
end;

initialization
  // Create platform specific object for key input
  KeyInput := InitializeKeyInput;

finalization
  FreeAndNil(KeyInput);


end.

