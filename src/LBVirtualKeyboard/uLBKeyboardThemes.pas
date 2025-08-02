unit uLBKeyboardThemes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, fpjson, jsonparser, LCLType,
  uLBVKCommons;

type
  // Tipi di animazione per transizioni
  TAnimationType = (atNone, atFade, atSlide, atScale, atBounce);
  
  // Stili di rendering per i tasti
  TKeyRenderStyle = (krsFlat, krsRaised, krsPressed, krsGradient, krsGlass);

  { TKeyThemeData - Definisce l'aspetto di un singolo tasto }
  TKeyThemeData = class(TObject)
  private
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FPressedBackgroundColor: TColor;
    FPressedTextColor: TColor;
    FBorderWidth: Integer;
    FCornerRadius: Integer;
    FFontName: String;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FRenderStyle: TKeyRenderStyle;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FShadowColor: TColor;
    FShadowOffset: TPoint;
    
  public
    constructor Create;
    procedure LoadFromJSON(const JSON: TJSONObject);
    procedure SaveToJSON(const JSON: TJSONObject);
    procedure ApplyToCanvas(Canvas: TCanvas; IsPressed: Boolean = False);
    
    // Properties
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property TextColor: TColor read FTextColor write FTextColor;
    property PressedBackgroundColor: TColor read FPressedBackgroundColor write FPressedBackgroundColor;
    property PressedTextColor: TColor read FPressedTextColor write FPressedTextColor;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property CornerRadius: Integer read FCornerRadius write FCornerRadius;
    property FontName: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property RenderStyle: TKeyRenderStyle read FRenderStyle write FRenderStyle;
    property GradientStartColor: TColor read FGradientStartColor write FGradientStartColor;
    property GradientEndColor: TColor read FGradientEndColor write FGradientEndColor;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property ShadowOffset: TPoint read FShadowOffset write FShadowOffset;
  end;

  { TKeyboardTheme - Tema completo per la tastiera }
  TKeyboardTheme = class(TObject)
  private
    FName: String;
    FDescription: String;
    FAuthor: String;
    FVersion: String;
    FBackgroundColor: TColor;
    FBackgroundImage: String;
    FAnimationType: TAnimationType;
    FAnimationDuration: Integer;
    
    // Temi per diversi tipi di tasti
    FNormalKeyTheme: TKeyThemeData;
    FSpecialKeyTheme: TKeyThemeData;
    FModifierKeyTheme: TKeyThemeData;
    FSpaceKeyTheme: TKeyThemeData;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: String): Boolean;
    function SaveToFile(const FileName: String): Boolean;
    function LoadFromJSON(const JSON: TJSONObject): Boolean;
    function SaveToJSON: TJSONObject;
    
    function GetKeyTheme(KeyType: TSpecialKey): TKeyThemeData;
    
    // Properties
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
    property Author: String read FAuthor write FAuthor;
    property Version: String read FVersion write FVersion;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BackgroundImage: String read FBackgroundImage write FBackgroundImage;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType;
    property AnimationDuration: Integer read FAnimationDuration write FAnimationDuration;
    
    property NormalKeyTheme: TKeyThemeData read FNormalKeyTheme;
    property SpecialKeyTheme: TKeyThemeData read FSpecialKeyTheme;
    property ModifierKeyTheme: TKeyThemeData read FModifierKeyTheme;
    property SpaceKeyTheme: TKeyThemeData read FSpaceKeyTheme;
  end;

  { TThemeManager - Gestisce tutti i temi disponibili }
  TThemeManager = class(TObject)
  private
    FThemes: TStringList;
    FCurrentTheme: TKeyboardTheme;
    FThemesPath: String;
    FOnThemeChanged: TNotifyEvent;
    
    procedure LoadBuiltInThemes;
    
  public
    constructor Create(const ThemesPath: String = '');
    destructor Destroy; override;
    
    procedure LoadThemesFromDirectory;
    function GetThemeCount: Integer;
    function GetThemeName(Index: Integer): String;
    function GetTheme(const Name: String): TKeyboardTheme;
    function SetCurrentTheme(const Name: String): Boolean;
    
    procedure CreateDefaultThemes;
    
    property CurrentTheme: TKeyboardTheme read FCurrentTheme;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

  { TKeyRenderer - Renderizza i tasti secondo il tema }
  TKeyRenderer = class(TObject)
  private
    FTheme: TKeyboardTheme;
    
    procedure DrawFlatKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
    procedure DrawRaisedKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
    procedure DrawGradientKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
    procedure DrawGlassKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
    procedure DrawRoundedRect(Canvas: TCanvas; Rect: TRect; Radius: Integer);
    procedure DrawGradient(Canvas: TCanvas; Rect: TRect; StartColor, EndColor: TColor; Vertical: Boolean = True);
    
  public
    constructor Create(Theme: TKeyboardTheme);
    
    procedure RenderKey(Canvas: TCanvas; Rect: TRect; KeyType: TSpecialKey; 
                       const Text: String; IsPressed: Boolean = False);
    procedure RenderKeyboard(Canvas: TCanvas; Rect: TRect);
    
    property Theme: TKeyboardTheme read FTheme write FTheme;
  end;

// Funzioni utility
function ColorToHex(Color: TColor): String;
function HexToColor(const Hex: String): TColor;

var
  LBVK_ThemeManager: TThemeManager;

implementation

uses
  uLBFileUtils;

function GetRValue(Color: TColor): Byte;
begin
  Result := Color and $FF;
end;

function GetGValue(Color: TColor): Byte;
begin
  Result := (Color shr 8) and $FF;
end;

function GetBValue(Color: TColor): Byte;
begin
  Result := (Color shr 16) and $FF;
end;


{ TKeyThemeData }

constructor TKeyThemeData.Create;
begin
  inherited Create;
  
  // Valori di default
  FBackgroundColor := clWhite;
  FBorderColor := clGray;
  FTextColor := clBlack;
  FPressedBackgroundColor := clSilver;
  FPressedTextColor := clBlack;
  FBorderWidth := 1;
  FCornerRadius := 4;
  FFontName := 'Default';
  FFontSize := 12;
  FFontStyle := [];
  FRenderStyle := krsRaised;
  FGradientStartColor := clWhite;
  FGradientEndColor := clSilver;
  FShadowColor := clGray;
  FShadowOffset := Point(2, 2);
end;

procedure TKeyThemeData.LoadFromJSON(const JSON: TJSONObject);
begin
  if JSON.Find('backgroundColor') <> nil then
    FBackgroundColor := HexToColor(JSON.Get('backgroundColor', ColorToHex(clWhite)));
  if JSON.Find('borderColor') <> nil then
    FBorderColor := HexToColor(JSON.Get('borderColor', ColorToHex(clGray)));
  if JSON.Find('textColor') <> nil then
    FTextColor := HexToColor(JSON.Get('textColor', ColorToHex(clBlack)));
  if JSON.Find('pressedBackgroundColor') <> nil then
    FPressedBackgroundColor := HexToColor(JSON.Get('pressedBackgroundColor', ColorToHex(clSilver)));
  if JSON.Find('pressedTextColor') <> nil then
    FPressedTextColor := HexToColor(JSON.Get('pressedTextColor', ColorToHex(clBlack)));
    
  FBorderWidth := JSON.Get('borderWidth', 1);
  FCornerRadius := JSON.Get('cornerRadius', 4);
  FFontName := JSON.Get('fontName', 'Default');
  FFontSize := JSON.Get('fontSize', 12);
  FRenderStyle := TKeyRenderStyle(JSON.Get('renderStyle', Ord(krsRaised)));
  
  if JSON.Find('gradientStartColor') <> nil then
    FGradientStartColor := HexToColor(JSON.Get('gradientStartColor', ColorToHex(clWhite)));
  if JSON.Find('gradientEndColor') <> nil then
    FGradientEndColor := HexToColor(JSON.Get('gradientEndColor', ColorToHex(clSilver)));
  if JSON.Find('shadowColor') <> nil then
    FShadowColor := HexToColor(JSON.Get('shadowColor', ColorToHex(clGray)));
end;

procedure TKeyThemeData.SaveToJSON(const JSON: TJSONObject);
begin
  JSON.Add('backgroundColor', ColorToHex(FBackgroundColor));
  JSON.Add('borderColor', ColorToHex(FBorderColor));
  JSON.Add('textColor', ColorToHex(FTextColor));
  JSON.Add('pressedBackgroundColor', ColorToHex(FPressedBackgroundColor));
  JSON.Add('pressedTextColor', ColorToHex(FPressedTextColor));
  JSON.Add('borderWidth', FBorderWidth);
  JSON.Add('cornerRadius', FCornerRadius);
  JSON.Add('fontName', FFontName);
  JSON.Add('fontSize', FFontSize);
  JSON.Add('renderStyle', Ord(FRenderStyle));
  JSON.Add('gradientStartColor', ColorToHex(FGradientStartColor));
  JSON.Add('gradientEndColor', ColorToHex(FGradientEndColor));
  JSON.Add('shadowColor', ColorToHex(FShadowColor));
end;

procedure TKeyThemeData.ApplyToCanvas(Canvas: TCanvas; IsPressed: Boolean);
begin
  Canvas.Font.Name := FFontName;
  Canvas.Font.Size := FFontSize;
  Canvas.Font.Style := FFontStyle;
  
  if IsPressed then
  begin
    Canvas.Brush.Color := FPressedBackgroundColor;
    Canvas.Font.Color := FPressedTextColor;
  end
  else begin
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Font.Color := FTextColor;
  end;
  
  Canvas.Pen.Color := FBorderColor;
  Canvas.Pen.Width := FBorderWidth;
end;

{ TKeyboardTheme }

constructor TKeyboardTheme.Create;
begin
  inherited Create;
  
  FNormalKeyTheme := TKeyThemeData.Create;
  FSpecialKeyTheme := TKeyThemeData.Create;
  FModifierKeyTheme := TKeyThemeData.Create;
  FSpaceKeyTheme := TKeyThemeData.Create;
  
  // Impostazioni di default
  FName := 'Default';
  FDescription := 'Default keyboard theme';
  FAuthor := 'System';
  FVersion := '1.0';
  FBackgroundColor := clForm;
  FAnimationType := atNone;
  FAnimationDuration := 200;
end;

destructor TKeyboardTheme.Destroy;
begin
  FreeAndNil(FNormalKeyTheme);
  FreeAndNil(FSpecialKeyTheme);
  FreeAndNil(FModifierKeyTheme);
  FreeAndNil(FSpaceKeyTheme);
  
  inherited Destroy;
end;

function TKeyboardTheme.LoadFromFile(const FileName: String): Boolean;
var
  JSONStr: String;
  JSONData: TJSONData;
  Parser: TJSONParser;
  FileStream: TStringList;
begin
  Result := False;
  
  if not FileExists(FileName) then
    Exit;
    
  try
    FileStream := TStringList.Create;
    try
      FileStream.LoadFromFile(FileName);
      JSONStr := FileStream.Text;
      
      Parser := TJSONParser.Create(JSONStr);
      try
        JSONData := Parser.Parse;
        try
          if JSONData is TJSONObject then
            Result := LoadFromJSON(TJSONObject(JSONData));
        finally
          JSONData.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TKeyboardTheme.SaveToFile(const FileName: String): Boolean;
var
  JSONObj: TJSONObject;
  FileStream: TStringList;
begin
  Result := False;
  
  try
    JSONObj := SaveToJSON;
    try
      FileStream := TStringList.Create;
      try
        FileStream.Text := JSONObj.FormatJSON;
        FileStream.SaveToFile(FileName);
        Result := True;
      finally
        FileStream.Free;
      end;
    finally
      JSONObj.Free;
    end;
  except
    Result := False;
  end;
end;

function TKeyboardTheme.LoadFromJSON(const JSON: TJSONObject): Boolean;
var
  NormalKeyJSON, SpecialKeyJSON, ModifierKeyJSON, SpaceKeyJSON: TJSONObject;
begin
  Result := False;
  
  try
    FName := JSON.Get('name', 'Unnamed Theme');
    FDescription := JSON.Get('description', '');
    FAuthor := JSON.Get('author', '');
    FVersion := JSON.Get('version', '1.0');
    
    if JSON.Find('backgroundColor') <> nil then
      FBackgroundColor := HexToColor(JSON.Get('backgroundColor', ColorToHex(clForm)));
    FBackgroundImage := JSON.Get('backgroundImage', '');
    FAnimationType := TAnimationType(JSON.Get('animationType', Ord(atNone)));
    FAnimationDuration := JSON.Get('animationDuration', 200);
    
    // Carica i temi per i diversi tipi di tasti
    if JSON.Find('normalKey') <> nil then
    begin
      NormalKeyJSON := JSON.Objects['normalKey'];
      FNormalKeyTheme.LoadFromJSON(NormalKeyJSON);
    end;
    
    if JSON.Find('specialKey') <> nil then
    begin
      SpecialKeyJSON := JSON.Objects['specialKey'];
      FSpecialKeyTheme.LoadFromJSON(SpecialKeyJSON);
    end;
    
    if JSON.Find('modifierKey') <> nil then
    begin
      ModifierKeyJSON := JSON.Objects['modifierKey'];
      FModifierKeyTheme.LoadFromJSON(ModifierKeyJSON);
    end;
    
    if JSON.Find('spaceKey') <> nil then
    begin
      SpaceKeyJSON := JSON.Objects['spaceKey'];
      FSpaceKeyTheme.LoadFromJSON(SpaceKeyJSON);
    end;
    
    Result := True;
  except
    Result := False;
  end;
end;

function TKeyboardTheme.SaveToJSON: TJSONObject;
var
  NormalKeyJSON, SpecialKeyJSON, ModifierKeyJSON, SpaceKeyJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  
  Result.Add('name', FName);
  Result.Add('description', FDescription);
  Result.Add('author', FAuthor);
  Result.Add('version', FVersion);
  Result.Add('backgroundColor', ColorToHex(FBackgroundColor));
  Result.Add('backgroundImage', FBackgroundImage);
  Result.Add('animationType', Ord(FAnimationType));
  Result.Add('animationDuration', FAnimationDuration);
  
  // Salva i temi per i diversi tipi di tasti
  NormalKeyJSON := TJSONObject.Create;
  FNormalKeyTheme.SaveToJSON(NormalKeyJSON);
  Result.Add('normalKey', NormalKeyJSON);
  
  SpecialKeyJSON := TJSONObject.Create;
  FSpecialKeyTheme.SaveToJSON(SpecialKeyJSON);
  Result.Add('specialKey', SpecialKeyJSON);
  
  ModifierKeyJSON := TJSONObject.Create;
  FModifierKeyTheme.SaveToJSON(ModifierKeyJSON);
  Result.Add('modifierKey', ModifierKeyJSON);
  
  SpaceKeyJSON := TJSONObject.Create;
  FSpaceKeyTheme.SaveToJSON(SpaceKeyJSON);
  Result.Add('spaceKey', SpaceKeyJSON);
end;

function TKeyboardTheme.GetKeyTheme(KeyType: TSpecialKey): TKeyThemeData;
begin
  case KeyType of
    sk_Shift, sk_Ctrl, sk_Alt:
      Result := FModifierKeyTheme;
    sk_Space:
      Result := FSpaceKeyTheme;
    sk_ArrowLeft, sk_ArrowRight, sk_ArrowUp, sk_ArrowDown,
    sk_Return, sk_BackSpace, sk_Delete, sk_Tab, sk_Switcher:
      Result := FSpecialKeyTheme;
    else
      Result := FNormalKeyTheme;
  end;
end;

{ TThemeManager }

constructor TThemeManager.Create(const ThemesPath: String);
begin
  inherited Create;
  
  FThemes := TStringList.Create;
  FThemes.OwnsObjects := True;
  
  if ThemesPath <> '' then
    FThemesPath := ThemesPath
  else
    FThemesPath := ExtractFilePath(Application.ExeName) + 'themes' + PathDelim;
    
  Self.LoadBuiltInThemes;
  Self.LoadThemesFromDirectory;
  
  // Imposta il tema di default
  if FThemes.Count > 0 then
    FCurrentTheme := TKeyboardTheme(FThemes.Objects[0]);
end;

destructor TThemeManager.Destroy;
begin
  FreeAndNil(FThemes);
  inherited Destroy;
end;

procedure TThemeManager.LoadBuiltInThemes;
begin
  CreateDefaultThemes;
end;

procedure TThemeManager.LoadThemesFromDirectory;
var
  _Files: TStringList;
  _Theme: TKeyboardTheme;
  i : Integer;

begin
  if not DirectoryExists(FThemesPath) then
    Exit;

  _Files := TStringList.Create;

  if FindFilesInFolder(FThemesPath, '*.json', False, _Files) then
  begin
    for i := 0 to _Files.Count - 1 do
    begin
      _Theme := TKeyboardTheme.Create;
      if _Theme.LoadFromFile(_Files.Strings[i]) then
        FThemes.AddObject(_Theme.Name, _Theme)
      else
        _Theme.Free;
    end;
  end;
  _Files.Free;
end;

function TThemeManager.GetThemeCount: Integer;
begin
  Result := FThemes.Count;
end;

function TThemeManager.GetThemeName(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FThemes.Count) then
    Result := FThemes[Index]
  else
    Result := '';
end;

function TThemeManager.GetTheme(const Name: String): TKeyboardTheme;
var
  _Idx: Integer;
begin
  _Idx := FThemes.IndexOf(Name);
  if _Idx >= 0 then
    Result := TKeyboardTheme(FThemes.Objects[_Idx])
  else
    Result := nil;
end;

function TThemeManager.SetCurrentTheme(const Name: String): Boolean;
var
  _Theme: TKeyboardTheme;
begin
  _Theme := Self.GetTheme(Name);
  Result := _Theme <> nil;
  
  if Result then
  begin
    FCurrentTheme := _Theme;
    if Assigned(FOnThemeChanged) then
      FOnThemeChanged(Self);
  end;
end;

procedure TThemeManager.CreateDefaultThemes;
var
  _DefaultTheme, _DarkTheme, _ModernTheme: TKeyboardTheme;
begin
  // Tema Default
  _DefaultTheme := TKeyboardTheme.Create;
  _DefaultTheme.Name := 'Default';
  _DefaultTheme.Description := 'Classic keyboard theme';
  FThemes.AddObject(_DefaultTheme.Name, _DefaultTheme);
  
  // Tema Dark
  _DarkTheme := TKeyboardTheme.Create;
  _DarkTheme.Name := 'Dark';
  _DarkTheme.Description := 'Dark keyboard theme';
  _DarkTheme.BackgroundColor := $2D2D30;
  
  // Tasti normali dark
  _DarkTheme.NormalKeyTheme.BackgroundColor := $3C3C3C;
  _DarkTheme.NormalKeyTheme.TextColor := clWhite;
  _DarkTheme.NormalKeyTheme.BorderColor := $5A5A5A;
  _DarkTheme.NormalKeyTheme.PressedBackgroundColor := $007ACC;
  
  // Tasti speciali dark
  _DarkTheme.SpecialKeyTheme.BackgroundColor := $007ACC;
  _DarkTheme.SpecialKeyTheme.TextColor := clWhite;
  _DarkTheme.SpecialKeyTheme.BorderColor := $005A9E;
  
  FThemes.AddObject(_DarkTheme.Name, _DarkTheme);
  
  // Tema Modern
  _ModernTheme := TKeyboardTheme.Create;
  _ModernTheme.Name := 'Modern';
  _ModernTheme.Description := 'Modern flat keyboard theme';
  _ModernTheme.BackgroundColor := $F0F0F0;
  _ModernTheme.AnimationType := atFade;
  
  // Tasti flat moderni
  _ModernTheme.NormalKeyTheme.RenderStyle := krsFlat;
  _ModernTheme.NormalKeyTheme.BackgroundColor := clWhite;
  _ModernTheme.NormalKeyTheme.BorderColor := $E0E0E0;
  _ModernTheme.NormalKeyTheme.CornerRadius := 8;
  _ModernTheme.NormalKeyTheme.PressedBackgroundColor := $0078D4;
  _ModernTheme.NormalKeyTheme.PressedTextColor := clWhite;
  
  FThemes.AddObject(_ModernTheme.Name, _ModernTheme);
end;

{ TKeyRenderer }

constructor TKeyRenderer.Create(Theme: TKeyboardTheme);
begin
  inherited Create;
  FTheme := Theme;
end;

procedure TKeyRenderer.RenderKey(Canvas: TCanvas; Rect: TRect; KeyType: TSpecialKey; const Text: String; IsPressed: Boolean);
var
  KeyTheme: TKeyThemeData;
  TextRect: TRect;
  TextStyle: TTextStyle;
begin
  KeyTheme := FTheme.GetKeyTheme(KeyType);
  
  case KeyTheme.RenderStyle of
    krsFlat: DrawFlatKey(Canvas, Rect, KeyTheme, IsPressed);
    krsRaised: DrawRaisedKey(Canvas, Rect, KeyTheme, IsPressed);
    krsGradient: DrawGradientKey(Canvas, Rect, KeyTheme, IsPressed);
    krsGlass: DrawGlassKey(Canvas, Rect, KeyTheme, IsPressed);
  end;
  
  // Disegna il testo
  if Text <> '' then
  begin
    KeyTheme.ApplyToCanvas(Canvas, IsPressed);
    
    TextStyle := Canvas.TextStyle;
    TextStyle.Alignment := taCenter;
    TextStyle.Layout := tlCenter;
    
    TextRect := Rect;
    Canvas.TextRect(TextRect, 0, 0, Text, TextStyle);
  end;
end;

procedure TKeyRenderer.DrawFlatKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
begin
  KeyTheme.ApplyToCanvas(Canvas, IsPressed);
  
  if KeyTheme.CornerRadius > 0 then
    DrawRoundedRect(Canvas, Rect, KeyTheme.CornerRadius)
  else
    Canvas.Rectangle(Rect);
end;

procedure TKeyRenderer.DrawRaisedKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
var
  LightColor, DarkColor: TColor;
begin
  KeyTheme.ApplyToCanvas(Canvas, IsPressed);
  
  // Colori per l'effetto 3D
  if IsPressed then
  begin
    LightColor := clGray;
    DarkColor := clWhite;
  end
  else begin
    LightColor := clWhite;
    DarkColor := clGray;
  end;
  
  // Disegna il corpo del tasto
  Canvas.Rectangle(Rect);
  
  // Disegna i bordi 3D
  Canvas.Pen.Color := LightColor;
  Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right - 1, Rect.Top);
  
  Canvas.Pen.Color := DarkColor;
  Canvas.MoveTo(Rect.Right - 1, Rect.Top + 1);
  Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 1);
end;

procedure TKeyRenderer.DrawGradientKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
var
  StartColor, EndColor: TColor;
begin
  if IsPressed then
  begin
    StartColor := KeyTheme.PressedBackgroundColor;
    EndColor := KeyTheme.GradientEndColor;
  end
  else begin
    StartColor := KeyTheme.GradientStartColor;
    EndColor := KeyTheme.GradientEndColor;
  end;
  
  DrawGradient(Canvas, Rect, StartColor, EndColor);
  
  Canvas.Pen.Color := KeyTheme.BorderColor;
  Canvas.Pen.Width := KeyTheme.BorderWidth;
  if KeyTheme.CornerRadius > 0 then
    DrawRoundedRect(Canvas, Rect, KeyTheme.CornerRadius)
  else
    Canvas.Rectangle(Rect);
end;

procedure TKeyRenderer.DrawGlassKey(Canvas: TCanvas; Rect: TRect; KeyTheme: TKeyThemeData; IsPressed: Boolean);
var
  GlossRect: TRect;
begin
  // Disegna il gradiente di base
  DrawGradientKey(Canvas, Rect, KeyTheme, IsPressed);
  
  // Aggiunge l'effetto vetro nella parte superiore
  GlossRect := Rect;
  GlossRect.Bottom := GlossRect.Top + (GlossRect.Bottom - GlossRect.Top) div 3;
  
  Canvas.Pen.Style := psClear;
  DrawGradient(Canvas, GlossRect, clWhite, $00F0F0F0, True);
  Canvas.Pen.Style := psSolid;
end;

procedure TKeyRenderer.DrawRoundedRect(Canvas: TCanvas; Rect: TRect; Radius: Integer);
begin
  Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
end;

procedure TKeyRenderer.DrawGradient(Canvas: TCanvas; Rect: TRect; StartColor, EndColor: TColor; Vertical: Boolean);
var
  i, Steps: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  StepR, StepG, StepB: Double;
  CurrentColor: TColor;
  LineRect: TRect;
begin
  // Estrae i componenti RGB
  R1 := GetRValue(StartColor);
  G1 := GetGValue(StartColor);
  B1 := GetBValue(StartColor);
  R2 := GetRValue(EndColor);
  G2 := GetGValue(EndColor);
  B2 := GetBValue(EndColor);
  
  if Vertical then
    Steps := Rect.Bottom - Rect.Top
  else
    Steps := Rect.Right - Rect.Left;
    
  StepR := (R2 - R1) / Steps;
  StepG := (G2 - G1) / Steps;
  StepB := (B2 - B1) / Steps;
  
  Canvas.Pen.Style := psClear;
  
  for i := 0 to Steps - 1 do
  begin
    CurrentColor := RGBToColor(
      R1 + Round(StepR * i),
      G1 + Round(StepG * i),
      B1 + Round(StepB * i)
    );
    
    Canvas.Brush.Color := CurrentColor;
    
    if Vertical then
    begin
      LineRect := Rect;
      LineRect.Top := Rect.Top + i;
      LineRect.Bottom := LineRect.Top + 1;
    end
    else
    begin
      LineRect := Rect;
      LineRect.Left := Rect.Left + i;
      LineRect.Right := LineRect.Left + 1;
    end;
    
    Canvas.FillRect(LineRect);
  end;
  
  Canvas.Pen.Style := psSolid;
end;

// Aggiungi queste funzioni alla fine dell'implementation di uLBKeyboardThemes.pas

procedure TKeyRenderer.RenderKeyboard(Canvas: TCanvas; Rect: TRect);
var
  KeyRect: TRect;
  KeyWidth, KeyHeight: Integer;
  Row, Col: Integer;
  MaxRows, MaxCols: Integer;
begin
  if FTheme = nil then Exit;
  
  // Disegna lo sfondo della tastiera
  Canvas.Brush.Color := FTheme.BackgroundColor;
  Canvas.FillRect(Rect);
  
  // Per ora renderizza una tastiera di esempio 4x10
  MaxRows := 4;
  MaxCols := 10;
  
  KeyWidth := Rect.Width div MaxCols;
  KeyHeight := Rect.Height div MaxRows;
  
  for Row := 0 to MaxRows - 1 do
  begin
    for Col := 0 to MaxCols - 1 do
    begin
      KeyRect.Left := Rect.Left + (Col * KeyWidth);
      KeyRect.Top := Rect.Top + (Row * KeyHeight);
      KeyRect.Right := KeyRect.Left + KeyWidth - 2; // Spacing between keys
      KeyRect.Bottom := KeyRect.Top + KeyHeight - 2;
      
      // Renderizza un tasto generico
      RenderKey(Canvas, KeyRect, sk_None, '', False);
    end;
  end;
end;

// Funzioni utility per conversione colori
function ColorToHex(Color: TColor): String;
var
  R, G, B: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  Result := Format('#%.2X%.2X%.2X', [R, G, B]);
end;

function HexToColor(const Hex: String): TColor;
var
  R, G, B: Byte;
  HexStr: String;
begin
  Result := clBlack; // Default
  
  HexStr := Hex;
  if Copy(HexStr, 1, 1) = '#' then
    Delete(HexStr, 1, 1);
    
  if Length(HexStr) <> 6 then Exit;
  
  try
    R := StrToInt('$' + Copy(HexStr, 1, 2));
    G := StrToInt('$' + Copy(HexStr, 3, 2));
    B := StrToInt('$' + Copy(HexStr, 5, 2));
    Result := RGBToColor(R, G, B);
  except
    // Mantieni il colore di default
  end;
end;

// Inizializzazione e finalizzazione
initialization
  LBVK_ThemeManager := TThemeManager.Create;

finalization
  if Assigned(LBVK_ThemeManager) then
    FreeAndNil(LBVK_ThemeManager);

end.
