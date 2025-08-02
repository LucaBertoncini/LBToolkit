unit uLBVirtualKeyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Contnrs, Graphics, ExtCtrls, Controls, Laz2_DOM,
  uVKExt, uLBVKCommons, uLBKeyboardThemes;

type
  TKeyboard = class;

    { TKeyDescriptor }

    TKeyDescriptor = class(TObject)
      strict private
        FKeyType : TSpecialKey;
        FKeyBoard : TKeyboard;
        FValues : array [sk_None .. sk_Alt] of String;
        function get_Value(aKeyType: TSpecialKey): String;

       public
         constructor Create(aKeyBoard: TKeyboard);

         function LoadFromXMLNode(aNode: TDOMElement): Boolean;
         function getValue(): String;
         function getDefaultValue(): String;

         function getKeyCode(): Integer;


         procedure isDown1();
         procedure isUP();
         procedure Pressed();

         property KeyType: TSpecialKey read FKeyType;

         const
           cKeyTypeAttributeName = DOMString('kType');
           cDefaultValueAttributeName = DOMString('Default');
           cShiftAttributeName = DOMString('Shift');
           cValuesNodeName = DOMString('Values');

           cKeyType_Alt        = String('Alt');
           cKeyType_Shift      = String('Shift');
           cKeyType_Ctrl       = String('Ctrl');
           cKeyType_Return     = String('Return');
           cKeyType_ArrowUp    = String('Up');
           cKeyType_ArrowDown  = String('Down');
           cKeyType_ArrowLeft  = String('Left');
           cKeyType_ArrowRight = String('Right');
           cKeyType_Switcher   = String('Switcher');
           cKeyType_BackSpace  = String('BackSpace');
           cKeyType_Cancel     = String('Canc');
           cKeyType_Space      = String('Space');
           cKeyType_Tab        = String('Tab');
    end;

    { TKeyDescriptorsInRow }

    TKeyDescriptorsInRow = class(TObject)
      strict private
        FKeyBoard : TKeyboard;
        FKeys : TObjectList;
        function get_KeysCount: Integer;

      public
        constructor Create(aKeyBoard: TKeyboard);
        destructor Destroy; override;

        function LoadFromXMLNode(aNode: TDOMElement): Boolean;
        function getKey(aKeyIdx: Integer): TKeyDescriptor;

        property KeysCount: Integer read get_KeysCount;
    end;

    { TPageDescriptor }

    TPageDescriptor = class(TObject)
      strict private
        FKeyBoard : TKeyboard;
        FRows : TObjectList;
        FMaxColumns : Integer;
        function get_RowCount: Integer;

      public
        constructor Create(aKeyBoard: TKeyboard);
        destructor Destroy; override;

        function getRow(aRowIdx: Integer): TKeyDescriptorsInRow;

        function LoadFromXMLNode(aNode: TDOMElement): Boolean;

        property RowCount: Integer read get_RowCount;
        property MaxColumns: Integer read FMaxColumns;
    end;


    { TPages }

    TPages = class(TObject)
      strict private
        FKeyBoard : TKeyboard;
        FPages : TObjectList;
        FMaxRows : Integer;
        FMaxColumns : Integer;

        function get_Count: Integer;

      public
        constructor Create(aKeyBoard: TKeyboard);
        destructor Destroy; override;

        procedure Clear();

        function LoadFromXMLNode(aNode: TDOMElement): Boolean;
        function getPage(aPageIdx: Integer): TPageDescriptor;

        property Count: Integer read get_Count;
        property MaxRows: Integer read FMaxRows;
        property MaxColumns: Integer read FMaxColumns;

        const
          cPagesNodeName = DOMString('Pages');
    end;

  { TKey }

  TKey = class(TObject)
    strict private
      FPaintBox      : TPaintBox;
      FKeyBoard      : TKeyboard;
      FKeyDescriptor : TKeyDescriptor;
      FKeyPressed    : Boolean;
      FRenderer      : TKeyRenderer;  // Renderer per i temi

      procedure DrawKey(Sender: TObject);
      procedure DrawKeyText(aColor: TColor);
      procedure DrawKeyWithTheme;
      procedure DrawKeyWithBitmap;
      function get_useThemes: Boolean;
      procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    public
      constructor Create(aKeyBoard: TKeyboard; aKeyDescriptor: TKeyDescriptor; aRenderer: TKeyRenderer = nil);
      destructor Destroy; override;

      property PaintBox: TPaintBox read FPaintBox;
      property UseThemes: Boolean read get_useThemes;
  end;

  { TKeyRow }

  TKeyRow = class(TObject)
    strict private
      FKeys : TObjectList;

    public
      constructor Create(aKeyBoard: TKeyboard; aRect: TRect; MaxCols: Integer; aRowDescriptor: TKeyDescriptorsInRow);
      destructor Destroy; override;
  end;


  { TKeyboard }

  TKeyboard = class(TObject)
    strict private
      FOwner         : TCustomControl;
      FRows          : TObjectList;
      FPages         : TPages;
      FActivePageIdx : Integer;
      FShiftPressed  : Boolean;

      FBitmaps       : array [sk_None .. sk_Switcher] of TBitmap;
      FRenderer      : TKeyRenderer;  // Renderer per i temi viene distrutto dall'oggetto KeyBoard
      FUseThemes     : Boolean;       // Aggiunto: flag per abilitare i temi

      FTimer         : TTimer;
      FLastKeyDown   : TKeyDescriptor;

      procedure repeatKey(Sender: TObject);

    private
      function get_Bitmap(aKey: TSpecialKey): TBitmap;
      procedure get_Bitmap(aKey: TSpecialKey; AValue: TBitmap);

      procedure KeyPressed(aKey: TKeyDescriptor);
      procedure KeyPressedSync(Data: PtrInt);
      procedure KeyDown(aKey: TKeyDescriptor);
      procedure KeyUp(aKey: TKeyDescriptor);

    public
      constructor Create(anOwner: TCustomControl);
      destructor Destroy; override;

      function DrawKeyboard(): Boolean;
      function LoadLayoutFromXMLFile(const aFilename: String): Boolean;
      
      // Nuovi metodi per i temi
      procedure SetThemeRenderer(aRenderer: TKeyRenderer);
      procedure EnableThemes(aEnable: Boolean);
      procedure SetTheme(aTheme: TKeyboardTheme);

      property Bitmap[aKey: TSpecialKey]: TBitmap read get_Bitmap write get_Bitmap;
      property ShiftPressed: Boolean read FShiftPressed;
      property Owner: TCustomControl read FOwner;
      property UseThemes: Boolean read FUseThemes write EnableThemes;
      property Renderer: TKeyRenderer read FRenderer;
  end;

implementation

uses
  LCLType, LCLIntf, ULBLogger, uLBFileUtils, {$IFDEF Linux}X11_KeyInput{$ELSE}Win_KeyInput{$ENDIF};


{ TKeyDescriptor }

function TKeyDescriptor.get_Value(aKeyType: TSpecialKey): String;
begin
  Result := '';

  if aKeyType in [sk_None .. sk_Alt] then
    Result := FValues[aKeyType];
end;

constructor TKeyDescriptor.Create(aKeyBoard: TKeyboard);
begin
  inherited Create;

  FKeyBoard := aKeyBoard;
end;

function TKeyDescriptor.LoadFromXMLNode(aNode: TDOMElement): Boolean;
var
  _Attrib : String;
  _ValueNode : TDOMElement;

begin
  Result := False;
  FKeyType := sk_None;

  if aNode <> nil then
  begin
    _Attrib := aNode.GetAttribute(cKeyTypeAttributeName);

    case _Attrib of
      cKeyType_Alt        : FKeyType := sk_Alt;
      cKeyType_Shift      : FKeyType := sk_Shift;
      cKeyType_Ctrl       : FKeyType := sk_Ctrl;
      cKeyType_Return     : FKeyType := sk_Return;
      cKeyType_ArrowUp    : FKeyType := sk_ArrowUp;
      cKeyType_ArrowDown  : FKeyType := sk_ArrowDown;
      cKeyType_ArrowLeft  : FKeyType := sk_ArrowLeft;
      cKeyType_ArrowRight : FKeyType := sk_ArrowRight;
      cKeyType_Switcher   : FKeyType := sk_Switcher;
      cKeyType_BackSpace  : FKeyType := sk_BackSpace;
      cKeyType_Cancel     : FKeyType := sk_Delete;
      cKeyType_Space      : FKeyType := sk_Space;
      cKeyType_Tab        : FKeyType := sk_Tab;


      else begin
        _ValueNode := TDOMElement(aNode.FindNode(cValuesNodeName));
        if _ValueNode <> nil then
        begin
          FValues[sk_None] := _ValueNode.GetAttribute(cDefaultValueAttributeName);
          FValues[sk_Shift] := _ValueNode.GetAttribute(cShiftAttributeName);
          FValues[sk_Ctrl] := '';
          FValues[sk_Alt] := '';
        end;
      end;
    end;

    Result := True;
  end;
end;

function TKeyDescriptor.getValue: String;
begin
  Result := '';

  if FKeyType = sk_None then
  begin
    if (FKeyBoard <> nil) and (FKeyBoard.ShiftPressed) then
      Result := FValues[sk_Shift]
    else
      Result := FValues[sk_None];
  end;
end;

function TKeyDescriptor.getDefaultValue: String;
begin
  Result := '';

  if FKeyType = sk_None then
    Result := FValues[sk_None];
end;

function TKeyDescriptor.getKeyCode: Integer;
var
  _Value : String;

begin
  Result := VK_UNKNOWN;

  case FKeyType of
    sk_Shift      : Result := VK_SHIFT;
    sk_Ctrl       : Result := VK_CONTROL;
    sk_Alt        : Result := VK_MENU;
    sk_ArrowLeft  : Result := VK_LEFT;
    sk_ArrowRight : Result := VK_RIGHT;
    sk_ArrowUp    : Result := VK_UP;
    sk_ArrowDown  : Result := VK_DOWN;
    sk_Return     : Result := VK_RETURN;
    sk_Space      : Result := VK_SPACE;
    sk_Tab        : Result := VK_TAB;
    sk_BackSpace  : Result := VK_BACK;
    sk_Delete     : Result := VK_DELETE;

    sk_None:
      begin
        if FKeyBoard.ShiftPressed and (Length(FValues[sk_Shift]) = 0) then
          Result := VK_UNKNOWN
        else begin
          _Value := FValues[sk_None];
          case _Value of
            '.': Result := VK_OEM_PERIOD;
            ',': Result := VK_OEM_COMMA;
            '+': Result := VK_OEM_PLUS;
            '-': Result := VK_OEM_MINUS;
            'à': Result := VK_Agrave;
            'è': Result := VK_Egrave;
            'ì': Result := VK_Igrave;
            'ò': Result := VK_Ograve;
            'ù': Result := VK_Ugrave;
            '?': Result := VK_QuestionMark;
            '\': Result := VK_BackSlash;
            '/': Result := VK_Slash;
            ';': Result := VK_SEMICOLON;
            '=': Result := VK_Equal;

            else
              if _Value <> '' then
                Result := Char2VK(_Value[1])
              else
                Result := VK_UNKNOWN;
          end;
        end;
      end;
  end;
end;

procedure TKeyDescriptor.isDown1;
begin
  if FKeyBoard <> nil then
    FKeyBoard.KeyDown(Self);
end;

procedure TKeyDescriptor.isUP;
begin
  if FKeyBoard <> nil then
    FKeyBoard.KeyUp(Self);
end;

procedure TKeyDescriptor.Pressed;
begin
  if FKeyBoard <> nil then
    FKeyBoard.KeyPressed(Self);
end;

{ TKeyDescriptorsInRow }

function TKeyDescriptorsInRow.get_KeysCount: Integer;
begin
  Result := FKeys.Count;
end;

constructor TKeyDescriptorsInRow.Create(aKeyBoard: TKeyboard);
begin
  inherited Create;

  FKeyBoard := aKeyBoard;
  FKeys := TObjectList.Create(True);
end;

destructor TKeyDescriptorsInRow.Destroy;
begin
  FreeAndNil(FKeys);

  inherited Destroy;
end;

function TKeyDescriptorsInRow.LoadFromXMLNode(aNode: TDOMElement): Boolean;
var
  k : Integer;
  _Key : TKeyDescriptor;

begin
  Result := False;
  FKeys.Clear;

  for k := 0 to aNode.ChildNodes.Count - 1 do
  begin
    _Key := TKeyDescriptor.Create(FKeyBoard);

    if _Key.LoadFromXMLNode(TDOMElement(aNode.ChildNodes.Item[k])) then
      FKeys.Add(_Key)
    else begin
      LBLogger.Write(1, 'TKeyDescriptorsInRow.LoadFromXMLNode', lmt_Warning, 'Error loading key data for index %d', [k]);
      _Key.Free;
    end;
  end;

  Result := True;
end;

function TKeyDescriptorsInRow.getKey(aKeyIdx: Integer): TKeyDescriptor;
begin
  Result := nil;

  if Self <> nil then
  begin
    if (aKeyIdx >= 0) and (aKeyIdx < FKeys.Count) then
      Result := TKeyDescriptor(FKeys.Items[aKeyIdx]);
  end;
end;


{ TPageDescriptor }

function TPageDescriptor.get_RowCount: Integer;
begin
  Result := FRows.Count;
end;

constructor TPageDescriptor.Create(aKeyBoard: TKeyboard);
begin
  inherited Create;

  FRows := TObjectList.Create(True);
  FKeyBoard := aKeyBoard;
end;

destructor TPageDescriptor.Destroy;
begin
  FreeAndNil(FRows);

  inherited Destroy;
end;

function TPageDescriptor.getRow(aRowIdx: Integer): TKeyDescriptorsInRow;
begin
  Result := nil;

  if Self <> nil then
  begin
    if (aRowIdx >= 0) and (aRowIdx < FRows.Count) then
      Result := TKeyDescriptorsInRow(FRows.Items[aRowIdx]);
  end;
end;

function TPageDescriptor.LoadFromXMLNode(aNode: TDOMElement): Boolean;
var
  r : Integer;
  _Row : TKeyDescriptorsInRow;

begin
  Result := False;
  FRows.Clear;
  FMaxColumns := 0;

  for r := 0 to aNode.ChildNodes.Count - 1 do
  begin
    _Row := TKeyDescriptorsInRow.Create(FKeyBoard);
    if _Row.LoadFromXMLNode(TDOMElement(aNode.ChildNodes.Item[r])) then
    begin
      FRows.Add(_Row);
      if _Row.KeysCount > FMaxColumns then
        FMaxColumns := _Row.KeysCount;
    end
    else begin
      LBLogger.Write(1, 'TPageDescriptor.LoadFromXMLNode', lmt_Warning, 'Error loading row %d in keyboard layout', [r]);
      _Row.Free;
    end;

    Result := True;
  end;
end;

{ TPages }

function TPages.get_Count: Integer;
begin
  Result := FPages.Count;
end;

constructor TPages.Create(aKeyBoard: TKeyboard);
begin
  inherited Create;

  FPages := TObjectList.Create(True);

  FKeyBoard := aKeyBoard;
end;

destructor TPages.Destroy;
begin
  FreeAndNil(FPages);

  inherited Destroy;
end;

procedure TPages.Clear;
begin
  FPages.Clear;
  FMaxColumns := 0;
  FMaxRows := 0;
end;

function TPages.LoadFromXMLNode(aNode: TDOMElement): Boolean;
var
  _Node : TDOMElement;
  _Page : TPageDescriptor;
  i : Integer;

begin
  Result := False;
  Self.Clear();

  if aNode <> nil then
  begin
    if aNode.NodeName = cPagesNodeName then
      _Node := aNode
    else
      _Node := TDOMElement(aNode.FindNode(cPagesNodeName));

    if _Node <> nil then
    begin
      for i := 0 to _Node.ChildNodes.Count - 1 do
      begin
        _Page := TPageDescriptor.Create(FKeyBoard);
        if _Page.LoadFromXMLNode(TDOMElement(_Node.ChildNodes.Item[i])) then
        begin
          FPages.Add(_Page);

          if _Page.RowCount > FMaxRows then
            FMaxRows := _Page.RowCount;

          if _Page.MaxColumns > FMaxColumns then
            FMaxColumns := _Page.MaxColumns;
        end
        else begin
          LBLogger.Write(1, 'TPages.LoadFromXMLNode', lmt_Warning, 'Error loading keyboard page %d', [i]);
          _Page.Free;
        end;
      end;

      Result := True;
    end
    else
      LBLogger.Write(1, 'TPages.LoadFromXMLNode', lmt_Warning, 'Keyboard pages node <%s> not found in node <%s>', [cPagesNodeName, aNode.NodeName]);
  end;
end;

function TPages.getPage(aPageIdx: Integer): TPageDescriptor;
begin
  Result := nil;

  if Self <> nil then
  begin
    if (aPageIdx >= 0) and (aPageIdx < FPages.Count) then
      Result := TPageDescriptor(FPages.Items[aPageIdx]);
  end;
end;

{ TKey }

procedure TKey.DrawKey(Sender: TObject);
begin
  if UseThemes then
    DrawKeyWithTheme
  else
    DrawKeyWithBitmap;
end;

procedure TKey.DrawKeyWithTheme;
var
  KeyText: String;
  KeyType: TSpecialKey;
begin
  if not Assigned(FRenderer) then Exit;
  
  // Determina il testo da mostrare
  if Assigned(FKeyDescriptor) then
  begin
    KeyText := FKeyDescriptor.getValue();
    KeyType := FKeyDescriptor.KeyType;
  end
  else
  begin
    KeyText := '';
    KeyType := sk_None;
  end;
  
  // Renderizza il tasto usando il tema
  FRenderer.RenderKey(FPaintBox.Canvas, FPaintBox.ClientRect, KeyType, KeyText, FKeyPressed);
end;

procedure TKey.DrawKeyWithBitmap;
var
  _X, _Y : Integer;
  _Bitmap : TBitmap;
  _EmptyBitmap : TBitmap;
begin
  _EmptyBitmap := FKeyBoard.Bitmap[sk_None];
  if FKeyDescriptor = nil then
    _Bitmap := _EmptyBitmap
  else begin
    _Bitmap := FKeyBoard.Bitmap[FKeyDescriptor.KeyType];
    if _Bitmap = nil then
      _Bitmap := _EmptyBitmap;
  end;

  _X := (FPaintBox.ClientWidth - _Bitmap.Width) div 2;
  _Y := (FPaintBox.ClientHeight - _Bitmap.Height) div 2;

  FPaintBox.Canvas.Draw(_X, _Y, _Bitmap);

  if _Bitmap = _EmptyBitmap then
  begin
    if FKeyPressed then
      Self.DrawKeyText(clRed)
    else
      Self.DrawKeyText(clBlack);
  end;
end;

function TKey.get_useThemes: Boolean;
begin
  Result := FRenderer <> nil;
end;

constructor TKey.Create(aKeyBoard: TKeyboard; aKeyDescriptor: TKeyDescriptor; aRenderer: TKeyRenderer);
begin
  inherited Create;

  FKeyDescriptor := aKeyDescriptor;
  FKeyBoard := aKeyBoard;
  FRenderer := aRenderer;  // Può essere nil per usare le bitmap
  FKeyPressed := False;

  FPaintBox := TPaintBox.Create(FKeyBoard.Owner);
  FPaintBox.Font.Size := 24;
  FPaintBox.Parent := FKeyBoard.Owner;
  FPaintBox.OnMouseDown := @Self.MouseDown;
  FPaintBox.OnMouseUp := @Self.MouseUp;
  FPaintBox.OnPaint := @Self.DrawKey;
end;


procedure TKey.DrawKeyText(aColor: TColor);
var
  _TextStyle : TTextStyle;
  _Value : String;

begin
  if FKeyDescriptor <> nil then
  begin
    case FKeyDescriptor.KeyType of
      sk_None       : begin
                        _TextStyle := FPaintBox.Canvas.TextStyle;
                        _TextStyle.Alignment := taCenter;
                        _TextStyle.Layout := tlCenter;

                        _Value := FKeyDescriptor.getValue();

                        if Length(_Value) > 0 then
                        begin
                          FPaintBox.Canvas.Font.Color := aColor;

                          FPaintBox.Canvas.TextRect(FPaintBox.ClientRect, 0, 0, _Value, _TextStyle);
                        end;
                      end;

      sk_Shift      : ;
      sk_Space      : ;
      sk_Tab        : ;
      sk_Ctrl       : ;
      sk_Alt        : ;
      sk_ArrowLeft  : ;
      sk_ArrowRight : ;
      sk_ArrowUp    : ;
      sk_ArrowDown  : ;
      sk_Return     : ;
      sk_Switcher   : ;
    end;
  end;
end;

procedure TKey.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FKeyPressed := True;
  FPaintBox.Invalidate;

  if (FKeyBoard <> nil) and (FKeyDescriptor <> nil) then
    FKeyBoard.KeyDown(FKeyDescriptor);
end;

procedure TKey.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FKeyPressed := False;
  FPaintBox.Invalidate;

  if FKeyDescriptor <> nil then
    FKeyDescriptor.Pressed();

  if (FKeyBoard <> nil) and (FKeyDescriptor <> nil) then
    FKeyBoard.KeyUp(FKeyDescriptor);
end;

destructor TKey.Destroy;
begin
  FreeAndNil(FPaintBox);
  inherited Destroy;
end;

{ TKeyRow }

constructor TKeyRow.Create(aKeyBoard: TKeyboard; aRect: TRect; MaxCols: Integer; 
                          aRowDescriptor: TKeyDescriptorsInRow);
var
  i : Integer;
  _Width : Integer;
  _Key : TKey;
  _KeyDescriptor : TKeyDescriptor;
  _EmptyBitmap : TBitmap;
begin
  inherited Create;

  FKeys := TObjectList.Create(True);

  if aKeyBoard.UseThemes then
    _Width := aRect.Width div MaxCols
  else begin
    // Modalità bitmap: dimensioni basate sulla bitmap
    _EmptyBitmap := aKeyBoard.Bitmap[sk_None];
    _Width := aRect.Width div MaxCols;
    if _Width < _EmptyBitmap.Width then
      _Width := _EmptyBitmap.Width;
  end;

  for i := 0 to MaxCols - 1 do
  begin
    _KeyDescriptor := aRowDescriptor.getKey(i);

    if aKeyBoard.UseThemes then
      _Key := TKey.Create(aKeyBoard, _KeyDescriptor, aKeyBoard.Renderer)
    else
      _Key := TKey.Create(aKeyBoard, _KeyDescriptor, nil);
      
    _Key.PaintBox.SetBounds(aRect.Left, aRect.Top, _Width, aRect.Height);
    _Key.PaintBox.Visible := True;
    aRect.Left += _Width;
    FKeys.Add(_Key);
  end;
end;

destructor TKeyRow.Destroy;
begin
  FreeAndNil(FKeys);
  inherited Destroy;
end;

{ TKeyboard }

procedure TKeyboard.repeatKey(Sender: TObject);

const
  cFastRepeatTimeout = Cardinal(200);

begin
  if FLastKeyDown = nil then
    FTimer.Enabled := False
  else begin
    Self.KeyPressedSync(PtrInt(FLastKeyDown));
    if FTimer.Interval <> cFastRepeatTimeout then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := cFastRepeatTimeout;
      FTimer.Enabled := True;
    end;
  end;
end;

function TKeyboard.get_Bitmap(aKey: TSpecialKey): TBitmap;
begin
  Result := FBitmaps[aKey];
end;

procedure TKeyboard.get_Bitmap(aKey: TSpecialKey; AValue: TBitmap);
begin
  FBitmaps[aKey] := AValue;
end;


procedure TKeyboard.KeyPressed(aKey: TKeyDescriptor);
begin
  Application.QueueAsyncCall(@Self.KeyPressedSync, PtrInt(aKey));
end;

procedure TKeyboard.KeyPressedSync(Data: PtrInt);
var
  _Key : TKeyDescriptor;

begin
  _Key := TKeyDescriptor(Data);

  case _Key.KeyType of
    sk_Shift:
      begin
        FShiftPressed := not FShiftPressed;
        if FShiftPressed then
          KeyInput.Apply([ssShift])
        else
          KeyInput.Unapply([ssShift]);

        Self.DrawKeyboard();
      end;

    sk_Switcher:
      begin
        if FPages.Count > 1 then
        begin
          FActivePageIdx += 1;
          if FActivePageIdx >= FPages.Count then
            FActivePageIdx := 0;
          Self.DrawKeyboard();
        end;
      end;

    sk_BackSpace : KeyInput.Press(VK_BACK);

    sk_Delete    : KeyInput.Press(VK_DELETE);

    sk_Return    : KeyInput.Press(VK_RETURN);

    sk_ArrowLeft : KeyInput.Press(VK_LEFT);

    sk_ArrowRight : KeyInput.Press(VK_RIGHT);
    sk_ArrowUp    : KeyInput.Press(VK_UP);
    sk_ArrowDown  : KeyInput.Press(VK_DOWN);

    sk_Space      : KeyInput.Press(VK_SPACE);

    sk_Tab        : KeyInput.Press(VK_TAB);

    sk_None       : KeyInput.Press(_Key.getKeyCode());
  end;
end;

procedure TKeyboard.KeyDown(aKey: TKeyDescriptor);
begin
  FLastKeyDown := aKey;
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;
end;

procedure TKeyboard.KeyUp(aKey: TKeyDescriptor);
begin
  FLastKeyDown := nil;
  FTimer.Enabled := False;
end;

constructor TKeyboard.Create(anOwner: TCustomControl);
begin
  inherited Create();

  FOwner := anOwner;

  FRows := TObjectList.Create(True);

  FPages := TPages.Create(Self);

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := @Self.repeatKey;
end;

destructor TKeyboard.Destroy;
begin
  try

    FreeAndNil(FTimer);

    FreeAndNil(FRows);
    FreeAndNil(FPages);

    if FRenderer <> nil then
      FreeAndNil(FRenderer);

  except
    on E: Exception do
      LBLogger.Write(1, 'TKeyboard.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

function TKeyboard.DrawKeyboard: Boolean;
var
  i : Integer;
  _Row : TKeyRow;
  _Rect : TRect;
  _Height : Integer;
  _Page : TPageDescriptor;
  _RowDescriptor : TKeyDescriptorsInRow;
  _EmptyBitmap : TBitmap;


begin
  Result := False;
  FRows.Clear;

  if FPages.MaxRows > 0 then
  begin
    _Rect := FOwner.ClientRect;

    _Height := _Rect.Height div FPages.MaxRows;

    if FRenderer = nil then
    begin
      _EmptyBitmap := FBitmaps[sk_None];
      if (_EmptyBitmap <> nil) and (_Height < _EmptyBitmap.Height) then
        _Height := _EmptyBitmap.Height;
    end;

    _Rect.Height := _Height;

    _Page := FPages.getPage(FActivePageIdx);

    for i := 0 to FPages.MaxRows - 1 do
    begin
      _RowDescriptor := _Page.getRow(i);

      _Row := TKeyRow.Create(Self, _Rect, FPages.MaxColumns, _RowDescriptor);
      _Rect.Top += _Height;
      _Rect.Height := _Height;
      FRows.Add(_Row);
    end;

    Result := True;

  end;
end;

function TKeyboard.LoadLayoutFromXMLFile(const aFilename: String): Boolean;
var
  _Doc : TXMLDocument = nil;
  _RootNode : TDOMNode;

begin
  Result := False;
  FPages.Clear();
  FActivePageIdx := 0;

  if Length(aFilename) > 0 then
  begin
    if OpenXMLFile(aFilename, _Doc) then
    begin
      _RootNode := _Doc.DocumentElement;
      Result := FPages.LoadFromXMLNode(TDOMElement(_RootNode));
    end
    else
      LBLogger.Write(1, 'TKeyboard.LoadLayoutFromXMLFile', lmt_Warning, 'Keyboard layout file <%s> not opened!', [aFilename]);
  end
  else
    LBLogger.Write(1, 'TKeyboard.LoadLayoutFromXMLFile', lmt_Warning, 'No keyboard layout file!', []);

  if _Doc <> nil then
    _Doc.Free;
end;


procedure TKeyboard.SetThemeRenderer(aRenderer: TKeyRenderer);
begin
  if FRenderer <> nil then
    FreeAndNil(FRenderer);

  FRenderer := aRenderer;
  FUseThemes := Assigned(aRenderer);
  
  // Ridisegna la tastiera con il nuovo renderer
  if FUseThemes then
  begin
    FOwner.Color := FRenderer.Theme.BackgroundColor;
    Self.DrawKeyboard();
  end;
end;

procedure TKeyboard.EnableThemes(aEnable: Boolean);
begin
  FUseThemes := aEnable and Assigned(FRenderer);
  Self.DrawKeyboard();
end;

procedure TKeyboard.SetTheme(aTheme: TKeyboardTheme);
begin
  if Assigned(FRenderer) then
  begin
    FRenderer.Theme := aTheme;
    FOwner.Color := aTheme.BackgroundColor;
    Self.DrawKeyboard();
  end
  else if Assigned(aTheme) then
  begin
    FRenderer := TKeyRenderer.Create(aTheme);
    Self.SetThemeRenderer(FRenderer);
  end;
end;


end.

