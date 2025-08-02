unit Win_KeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Windows, LCLType, Base_KeyInput;

type
  { TWinKeyInput }

  TWinKeyInput = class(TKeyInput)
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Press(Key: Word); override;
  end;

function InitializeKeyInput: TKeyInput;

var
  KeyInput: TKeyInput;

implementation

function InitializeKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: Word);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.wVk := Key;
  Input.ki.dwFlags := 0; // 0 = key down
  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWinKeyInput.DoUp(Key: Word);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.wVk := Key;
  Input.ki.dwFlags := KEYEVENTF_KEYUP;
  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWinKeyInput.Press(Key: Word);
var
  Inputs: array[0..1] of TInput;
begin
  FillChar(Inputs, SizeOf(Inputs), 0);

  // Key down
  Inputs[0]._Type := INPUT_KEYBOARD;
  Inputs[0].ki.wVk := Key;
  Inputs[0].ki.dwFlags := 0;

  // Key up
  Inputs[1]._Type := INPUT_KEYBOARD;
  Inputs[1].ki.wVk := Key;
  Inputs[1].ki.dwFlags := KEYEVENTF_KEYUP;

  SendInput(2, @Inputs, SizeOf(TInput));
end;

constructor TWinKeyInput.Create;
begin
  inherited Create;
end;

destructor TWinKeyInput.Destroy;
begin
  inherited Destroy;
end;

initialization
  KeyInput := InitializeKeyInput;

finalization
  FreeAndNil(KeyInput);

end.

