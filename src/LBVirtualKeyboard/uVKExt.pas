unit uVKExt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

const
  // Extended Virtual Key Codes for special characters
  VK_Agrave = $FF01;        // à
  VK_Egrave = $FF02;        // è
  VK_Igrave = $FF03;        // ì
  VK_Ograve = $FF05;        // ò
  VK_Ugrave = $FF06;        // ù

  VK_QuestionMark = $FF10;  // ?
  VK_BackSlash = $FF11;     // \
  VK_Slash = $FF12;         // /
  VK_Semicolon = $FF13;     // ;
  VK_Equal = $FF20;         // =

  // Additional special characters
  VK_Apostrophe = $FF21;    // '
  VK_Quote = $FF22;         // "
  VK_Colon = $FF23;         // :
  VK_LessThan = $FF24;      // <
  VK_GreaterThan = $FF25;   // >
  VK_LeftParen = $FF26;     // (
  VK_RightParen = $FF27;    // )
  VK_LeftBracket = $FF28;   // [
  VK_RightBracket = $FF29;  // ]
  VK_LeftBrace = $FF2A;     // {
  VK_RightBrace = $FF2B;    // }

type
  TSpecialChar = record
    VirtualKey: Word;
    Character: String;
    Description: string;
  end;

// Utility functions for extended VK codes
function IsExtendedVK(VK: Word): Boolean;
function ExtendedVKToChar(VK: Word): Char;
function CharToExtendedVK(C: Char): Word;
function GetExtendedVKDescription(VK: Word): string;

implementation

const
  SpecialChars: array[0..16] of TSpecialChar = (
    (VirtualKey: VK_Agrave; Character: 'à'; Description: 'A grave'),
    (VirtualKey: VK_Egrave; Character: 'è'; Description: 'E grave'),
    (VirtualKey: VK_Igrave; Character: 'ì'; Description: 'I grave'),
    (VirtualKey: VK_Ograve; Character: 'ò'; Description: 'O grave'),
    (VirtualKey: VK_Ugrave; Character: 'ù'; Description: 'U grave'),
    (VirtualKey: VK_QuestionMark; Character: '?'; Description: 'Question mark'),
    (VirtualKey: VK_BackSlash; Character: '\'; Description: 'Backslash'),
    (VirtualKey: VK_Slash; Character: '/'; Description: 'Slash'),
    (VirtualKey: VK_Semicolon; Character: ';'; Description: 'Semicolon'),
    (VirtualKey: VK_Equal; Character: '='; Description: 'Equal'),
    (VirtualKey: VK_Apostrophe; Character: ''''; Description: 'Apostrophe'),
    (VirtualKey: VK_Quote; Character: '"'; Description: 'Quote'),
    (VirtualKey: VK_Colon; Character: ':'; Description: 'Colon'),
    (VirtualKey: VK_LessThan; Character: '<'; Description: 'Less than'),
    (VirtualKey: VK_GreaterThan; Character: '>'; Description: 'Greater than'),
    (VirtualKey: VK_LeftParen; Character: '('; Description: 'Left parenthesis'),
    (VirtualKey: VK_RightParen; Character: ')'; Description: 'Right parenthesis')
  );

function IsExtendedVK(VK: Word): Boolean;
begin
  Result := (VK >= $FF00) and (VK <= $FFFF);
end;

function ExtendedVKToChar(VK: Word): Char;
var
  i: Integer;
begin
  Result := #0;
  for i := Low(SpecialChars) to High(SpecialChars) do
  begin
    if SpecialChars[i].VirtualKey = VK then
    begin
      Result := SpecialChars[i].Character[1];
      Break;
    end;
  end;
end;

function CharToExtendedVK(C: Char): Word;
var
  i: Integer;
begin
  Result := VK_UNKNOWN;
  for i := Low(SpecialChars) to High(SpecialChars) do
  begin
    if SpecialChars[i].Character = C then
    begin
      Result := SpecialChars[i].VirtualKey;
      Break;
    end;
  end;
end;

function GetExtendedVKDescription(VK: Word): string;
var
  i: Integer;
begin
  Result := 'Unknown';
  for i := Low(SpecialChars) to High(SpecialChars) do
  begin
    if SpecialChars[i].VirtualKey = VK then
    begin
      Result := SpecialChars[i].Description;
      Break;
    end;
  end;
end;

end.
