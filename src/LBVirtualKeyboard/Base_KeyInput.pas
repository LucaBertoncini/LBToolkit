unit Base_KeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType;

type
  // Event types for key input
  TKeyInputEvent = procedure(Key: Word) of object;
  TKeyInputErrorEvent = procedure(const ErrorMsg: string) of object;

  { TKeyInput - Abstract base class for key input handling }
  TKeyInput = class(TObject)
  private
    FOnError: TKeyInputErrorEvent;
    FEnabled: Boolean;
  protected
    procedure DoDown(Key: Word); virtual; abstract;
    procedure DoUp(Key: Word); virtual; abstract;
    procedure DoError(const Sender, ErrorMsg: string); virtual;
  public
    constructor Create; virtual;

    procedure Down(Key: Word);
    procedure Up(Key: Word);
    procedure Press(Key: Word); virtual;
    procedure Press(const StringValue: String);

    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);

    property Enabled: Boolean read FEnabled write FEnabled;
    property OnError: TKeyInputErrorEvent read FOnError write FOnError;
  end;

  TKeyInputClass = class of TKeyInput;


// Utility functions
function Char2VK(C: Char): Word;
function VK2Char(VK: Word): Char;

implementation

uses
  ULBLogger;

{ TKeyInput }

constructor TKeyInput.Create;
begin
  inherited Create;
  FEnabled := True;
end;

procedure TKeyInput.DoError(const Sender, ErrorMsg: string);
begin
  LBLogger.Write(1, Sender, lmt_Error, ErrorMsg);

  if Assigned(FOnError) then
    FOnError(ErrorMsg);
end;

procedure TKeyInput.Down(Key: Word);
begin
  if not FEnabled then Exit;

  try
    DoDown(Key);
    Application.ProcessMessages;
  except
    on E: Exception do
      Self.DoError('TKeyInput.Down', 'Key down failed: ' + E.Message);
  end;
end;

procedure TKeyInput.Up(Key: Word);
begin
  if not FEnabled then Exit;

  try
    DoUp(Key);
    Application.ProcessMessages;
  except
    on E: Exception do
      Self.DoError('TKeyInput.Up', 'Key up failed: ' + E.Message);
  end;
end;

procedure TKeyInput.Press(Key: Word);
begin
  if not FEnabled then Exit;

  Down(Key);
  Up(Key);
end;

procedure TKeyInput.Press(const StringValue: String);
var
  i: Integer;
  VK: Word;
begin
  if not FEnabled then Exit;

  for i := 1 to Length(StringValue) do
  begin
    VK := Char2VK(StringValue[i]);
    if VK <> VK_UNKNOWN then
      Press(VK)
    else
      Self.DoError('TKeyInput.Press', 'Cannot convert character to virtual key: ' + StringValue[i]);
  end;
end;

procedure TKeyInput.Apply(Shift: TShiftState);
begin
  if not FEnabled then Exit;

  if ssCtrl in Shift then Down(VK_CONTROL);
  if ssAlt in Shift then Down(VK_MENU);
  if ssShift in Shift then Down(VK_SHIFT);
end;

procedure TKeyInput.Unapply(Shift: TShiftState);
begin
  if not FEnabled then Exit;

  // Release in reverse order
  if ssShift in Shift then Up(VK_SHIFT);
  if ssAlt in Shift then Up(VK_MENU);
  if ssCtrl in Shift then Up(VK_CONTROL);
end;

// Utility functions implementation
function Char2VK(C: Char): Word;
begin
  case UpCase(C) of
    'A'..'Z': Result := Ord(C) - Ord('A') + VK_A;
    '0'..'9': Result := Ord(C) - Ord('0') + VK_0;
    ' ': Result := VK_SPACE;
    '.': Result := VK_OEM_PERIOD;
    ',': Result := VK_OEM_COMMA;
    '+': Result := VK_OEM_PLUS;
    '-': Result := VK_OEM_MINUS;
    else Result := VK_UNKNOWN;
  end;
end;

function VK2Char(VK: Word): Char;
begin
  case VK of
    VK_A..VK_Z: Result := Chr(VK - VK_A + Ord('A'));
    VK_0..VK_9: Result := Chr(VK - VK_0 + Ord('0'));
    VK_SPACE: Result := ' ';
    VK_OEM_PERIOD: Result := '.';
    VK_OEM_COMMA: Result := ',';
    VK_OEM_PLUS: Result := '+';
    VK_OEM_MINUS: Result := '-';
    else Result := #0;
  end;
end;

end.
