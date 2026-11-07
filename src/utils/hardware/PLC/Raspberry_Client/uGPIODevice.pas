unit uGPIODevice;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, uPLCBaseStructures, Variants,
  {$IFDEF SimpleGPIO}uGPIOTrigger{$ELSE}ulibPiGPIO{$ENDIF},
  Laz2_DOM, fgl, uLBUtils, fpjson,
  {$IFDEF Linux}uRPISystemMonitor,{$ENDIF} ULBLogger;

const
  cItemType_Trigger       = 'trigger';
  cItemType_Counter       = 'counter';
  cItemType_EncoderAB     = 'encoderab';

  cItemType_CPUTemp       = 'cputemp';
  cItemType_CPULoad       = 'cpuload';
  cItemType_FreeMem       = 'freemem';
  cItemType_IntError      = 'internalerror';
  cItemType_IntErrorDescr = 'internalerrordescr';

type

  { TGPIO_InternalErrorCatcher }

  TGPIO_InternalErrorCatcher = class(TLBBaseLogger)
    strict private
      FOnErrorEvent : TNotifyEvent;
      FCS : TTimedOutCriticalSection;
      FLastErrorMsg : String;

      function get_LastErrorMsg: String;

    strict protected
      function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;

    public
      constructor Create(); reintroduce;
      destructor Destroy; override;

      property LastErrorMsg: String read get_LastErrorMsg;
      property OnError: TNotifyEvent write FOnErrorEvent;
  end;


  { TGPIOItem }

  TGPIOItem = class(TBasePLCItem)
  private
    FItemType: String;
    FPullType: String;
    FGPIOs: TBytes;
    FOutput: Variant;
    FUpdater: TObject;
    FMicroSecDebounce: Cardinal;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ValueAsVariant: Variant; override;
    function ValueAsString: String; override;

    procedure resetEncoder();

    function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
    function loadFromXMLNode(aParentNode: TDOMNode): Boolean;
    function asJSONObject(const codeRef, valueRef: String): TJSONObject;
    procedure insertValueIntoJSObject(anObj: TJSONObject; aFieldName: String);

    property ItemType: String read FItemType write FItemType;
    property PullType: String read FPullType write FPullType;
    property GPIOs: TBytes read FGPIOs write FGPIOs;
    property Output: Variant read FOutput write FOutput;
    property Updater: TObject read FUpdater write FUpdater;
    property MicroSecDebounce: Cardinal read FMicroSecDebounce write FMicroSecDebounce;
  end;

  TGPIOItemsList = specialize TFPGObjectList<TGPIOItem>;

  TGPIO_EncodersList = specialize TFPGObjectList<TGPIOEncoder>;

  { TGPIODevice }

  TGPIODevice = class(TPLCBaseClient)
    strict private
      FEncoders : TGPIO_EncodersList;
      FChangedOnLastRead : TGPIOItemsList;
      FCSChanges : TTimedOutCriticalSection;
      FNewDataAvailable : PRTLEvent;
      {$IFDEF Linux}
      FSysMon : TRPI_SystemMonitor;
      {$ENDIF}
      FInternalErrorCatcher : TGPIO_InternalErrorCatcher;

      FTemp                     : TGPIOItem;
      FCPULoad                  : TGPIOItem;
      FMemAvailable             : TGPIOItem;
      FInternalError            : TGPIOItem;
      FInternalErrorDescription : TGPIOItem;

      procedure createItemUpdater(anItem: TGPIOItem);
      procedure TriggerValueChanged(Sender: TObject);
      procedure CounterValueChanged(Sender: TObject);
      procedure EncoderValueChanged(Sender: TObject);

      procedure TemperatureChanged(Sender: TObject);
      procedure AvailableMemoryChanged(Sender: TObject);
      procedure CPULoadChanged(Sender: TObject);

      procedure InternalError(Sender: TObject);

    strict protected
      function get_ChangedItem(anIdx: Integer): TBasePLCItem; override;
      function get_ChangedItemsCount: Integer; override;
      function get_ChangedItemValueAsDouble(anIdx: Integer): Double; override;

      function MonitoredItemsAsJSONArray(const codeRef, valueRef: String): TJSONArray; override;
      procedure clearItemsList(); override;
      function CloneItem(anItem: TBasePLCItem): Boolean; override;
      function getValueAsVariant(aItem: TBasePLCItem): Variant; override;

    protected
      procedure Execute; override;

    public
      constructor Create(aOwner: TPLCDevicesCollection); override;
      destructor Destroy; override;

      procedure Terminate; override;

      function loadFromXMLNode(aParentNode: TDOMNode): Boolean; override;

      property ChangedOnLastRead: TGPIOItemsList read FChangedOnLastRead;

      const
        cItemsNodeName = DOMString('Items');
  end;

  { TGPIODevice_ClientsCollection }

  TGPIODevice_ClientsCollection = class(TPLCDevicesCollection)
    public
      constructor Create; override;
      class function getRootNodeName(): String; override;
      const
        cGPIODeviceClientsRootNodeName = DOMString('GPIODevices');
  end;

implementation

uses
  uPLCCollector;

{ TGPIO_InternalErrorCatcher }

function TGPIO_InternalErrorCatcher.get_LastErrorMsg: String;
begin
  if FCS.Acquire('TGPIO_InternalErrorCatcher.get_LastErrorMsg') then
  begin
    Result := FLastErrorMsg;
    FCS.Release();
  end;
end;

function TGPIO_InternalErrorCatcher.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  Result := False;
  if MsgType in [lmt_Error, lmt_Critical] then
  begin
    if FCS.Acquire('TGPIO_InternalErrorCatcher.virtualWrite') then
    begin
      FLastErrorMsg := MsgText;
      FCS.Release();
      Result := True;
    end;
    if Result and Assigned(FOnErrorEvent) then
      FOnErrorEvent(Self);
  end;
end;

constructor TGPIO_InternalErrorCatcher.Create();
begin
  inherited Create('IntErr_Log', True);
  FCS := TTimedOutCriticalSection.Create;
  FLastErrorMsg := '';
end;

destructor TGPIO_InternalErrorCatcher.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

{ TGPIOItem }

constructor TGPIOItem.Create;
begin
  inherited;
  FItemType := '';
  FPullType := '';
  SetLength(FGPIOs, 0);
  FOutput := Null;
  FUpdater := nil;
  FMicroSecDebounce := 20;
end;

destructor TGPIOItem.Destroy;
begin
  if FUpdater <> nil then
    FreeAndNil(FUpdater);
  SetLength(FGPIOs, 0);
  inherited;
end;

function TGPIOItem.ValueAsVariant: Variant;
begin
  Result := FOutput;
end;

function TGPIOItem.ValueAsString: String;
begin
  Result := VarToStr(FOutput);
end;

procedure TGPIOItem.resetEncoder();
begin
  if (FUpdater <> nil) and (FUpdater is TGPIOEncoder) then
    TGPIOEncoder(FUpdater).resetCounter();
end;

function TGPIOItem.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
begin
  Result := False;
  if (aDoc <> nil) and (aParentNode <> nil) then
  begin
    try
      TDOMElement(aParentNode).SetAttribute(cIdAttrib, IntToStr(Id));
      TDOMElement(aParentNode).SetAttribute(cCodeAttrib, Code);
      TDOMElement(aParentNode).SetAttribute(cTypeAttrib, FItemType);
      TDOMElement(aParentNode).SetAttribute(cPullTypeAttrib, FPullType);
      Result := True;
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIOItem.insertIntoXMLNode', lmt_Error, 'Errore: <%s>', [E.Message]);
    end;
  end;
end;

function TGPIOItem.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _el: TDOMElement;
  _GPIO: Integer;
  _GPIO2: Integer;
begin
  Result := False;
  if (aParentNode = nil) or (not (aParentNode is TDOMElement)) then Exit;
  _el := TDOMElement(aParentNode);
  SetLength(FGPIOs, 0);
  try
    Id := StrToIntDef(_el.GetAttribute(cIdAttrib), 0);
    Code := _el.GetAttribute(cCodeAttrib);
    FItemType := LowerCase(_el.GetAttribute(cTypeAttrib));
    FPullType := LowerCase(_el.GetAttribute(cPullTypeAttrib));
    FMicroSecDebounce := StrToInt64Def(_el.GetAttribute(cDebouceAttrib), 20);

    case FItemType of
      cItemType_CPUTemp,
      cItemType_CPULoad,
      cItemType_IntError,
      cItemType_IntErrorDescr,
      cItemType_FreeMem: Result := (Id > 0);

      else begin
        if (FPullType = cPullType_Description[pt_PullDown]) or (FPullType = cPullType_Description[pt_PullUp]) then
        begin
          case FItemType of
            cItemType_Counter,
            cItemType_Trigger:
              begin
                _GPIO := StrToIntDef(_el.GetAttribute(cGPIOAttrib), 0);
                if _GPIO > 0 then
                begin
                  SetLength(FGPIOs, 1);
                  FGPIOs[0] := _GPIO;
                  if FItemType = cItemType_Counter then
                    FOutput := StrToQWordDef(_el.GetAttribute(cStartingOffset), 0);
                end;
              end;
            cItemType_EncoderAB:
              begin
                _GPIO := StrToIntDef(_el.GetAttribute(cGPIO_A_Attrib), 0);
                _GPIO2 := StrToIntDef(_el.GetAttribute(cGPIO_B_Attrib), 0);
                if (_GPIO > 0) and (_GPIO2 > 0) then
                begin
                  SetLength(FGPIOs, 2);
                  FGPIOs[0] := _GPIO;
                  FGPIOs[1] := _GPIO2;
                end;
              end;
          end;
          Result := (Id > 0) and (Code <> '') and (Length(FGPIOs) > 0);
        end;
      end;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TGPIOItem.loadFromXMLNode', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

function TGPIOItem.asJSONObject(const codeRef, valueRef: String): TJSONObject;
var
  _Value: String;
begin
  Result := TJSONObject.Create;
  Result.Add('id', Id);
  if codeRef = '' then Result.Add('Code', Code)
  else Result.Add(codeRef, Code);
  _Value := valueRef;
  if _Value = '' then _Value := cVariableValue;
  insertValueIntoJSObject(Result, _Value);
end;

procedure TGPIOItem.insertValueIntoJSObject(anObj: TJSONObject; aFieldName: String);
begin
  case VarType(FOutput) of
    varNull     : anObj.Add(aFieldName, '');
    varByte     : anObj.Add(aFieldName, Byte(FOutput));
    varWord     : anObj.Add(aFieldName, Word(FOutput));
    varSmallInt : anObj.Add(aFieldName, SmallInt(FOutput));
    varInteger  : anObj.Add(aFieldName, Integer(FOutput));
    varInt64    : anObj.Add(aFieldName, Int64(FOutput));
    varQWord    : anObj.Add(aFieldName, QWord(FOutput));
    varSingle   : anObj.Add(aFieldName, Single(FOutput));
    varDouble   : anObj.Add(aFieldName, Double(FOutput));
    varBoolean  : anObj.Add(aFieldName, Boolean(FOutput));
    varString   : anObj.Add(aFieldName, VarToStr(FOutput));
    else          anObj.Add(aFieldName, '???');
  end;
end;

{ TGPIODevice }

procedure TGPIODevice.createItemUpdater(anItem: TGPIOItem);
var
  _Trigger : TGPIOTrigger;
  _Counter : TGPIOCounter;
  _Encoder : TGPIOEncoder;
begin
  case anItem.FItemType of
    cItemType_CPUTemp:
      begin
        if FCSChanges.Acquire('TGPIODevice.createItemUpdater') then
        begin
          FTemp := anItem;
          {$IFDEF Linux}
          anItem.Output := FSysMon.Temperature * 0.1;
          {$ENDIF}
          FCSChanges.Release();
        end;
      end;
    cItemType_CPULoad:
      begin
        if FCSChanges.Acquire('TGPIODevice.createItemUpdater') then
        begin
          FCPULoad := anItem;
          {$IFDEF Linux}
          anItem.Output := FSysMon.CPULoad;
          {$ENDIF}
          FCSChanges.Release();
        end;
      end;
    cItemType_FreeMem:
      begin
        if FCSChanges.Acquire('TGPIODevice.createItemUpdater') then
        begin
          FMemAvailable := anItem;
          {$IFDEF Linux}
          anItem.Output := FSysMon.AvailableMemory;
          {$ENDIF}
          FCSChanges.Release();
        end;
      end;
    cItemType_IntError:
      begin
        if FCSChanges.Acquire('TGPIODevice.createItemUpdater') then
        begin
          FInternalError := anItem;
          anItem.Output := False;
          FCSChanges.Release();
        end;
      end;
    cItemType_IntErrorDescr:
      begin
        if FCSChanges.Acquire('TGPIODevice.createItemUpdater') then
        begin
          FInternalErrorDescription := anItem;
          anItem.Output := '';
          FCSChanges.Release();
        end;
      end;
    cItemType_Trigger:
      begin
        _Trigger := TGPIOTrigger.Create;
        if _Trigger.setGPIOItemBaseData(anItem.GPIOs[0], getPullTypeEnum(anItem.PullType), anItem.MicroSecDebounce) then
        begin
          _Trigger.UserData := anItem;
          _Trigger.OnValueChanged := @Self.TriggerValueChanged;
          anItem.Updater := _Trigger;
        end
        else FreeAndNil(_Trigger);
      end;
    cItemType_Counter:
      begin
        _Counter := TGPIOCounter.Create;
        if _Counter.setGPIOItemBaseData(anItem.GPIOs[0], getPullTypeEnum(anItem.PullType), anItem.MicroSecDebounce) then
        begin
          _Counter.UserData := anItem;
          _Counter.OnValueChanged := @Self.CounterValueChanged;
          _Counter.setStartingOffset(DWord(anItem.Output));
          anItem.Updater := _Counter;
        end
        else FreeAndNil(_Counter);
      end;
    cItemType_EncoderAB:
      begin
        _Encoder := TGPIOEncoder.Create;
        _Encoder.PinA := anItem.GPIOs[0];
        _Encoder.PinB := anItem.GPIOs[1];
        _Encoder.PullType := getPullTypeEnum(anItem.PullType);
        _Encoder.OnValueChanged := @Self.EncoderValueChanged;
        if _Encoder.Start() then
        begin
          _Encoder.UserData := anItem;
          anItem.Updater := _Encoder;
          FEncoders.Add(_Encoder);
        end
        else FreeAndNil(_Encoder);
      end;
  end;
end;

procedure TGPIODevice.TriggerValueChanged(Sender: TObject);
var
  _Trigger : TGPIOTrigger;
  _Item : TGPIOItem;
  _Encoder : TGPIOEncoder;
  i : Integer;
begin
  if FCSChanges.Acquire('TGPIODevice.TriggerValueChanged') then
  begin
    try
      _Trigger := TGPIOTrigger(Sender);
      _Item := TGPIOItem(_Trigger.UserData);
      _Item.Output := _Trigger.Value;
      FChangedOnLastRead.Add(_Item);
      for i := 0 to FEncoders.Count - 1 do
      begin
        _Encoder := FEncoders.Items[i];
        _Item := TGPIOItem(_Encoder.UserData);
        _Item.Output := _Encoder.Counter;
        FChangedOnLastRead.Add(_Item);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.TriggerValueChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
    RTLEventSetEvent(FNewDataAvailable);
  end;
end;

procedure TGPIODevice.CounterValueChanged(Sender: TObject);
var
  _Counter : TGPIOCounter;
  _Item : TGPIOItem;
begin
  if FCSChanges.Acquire('TGPIODevice.CounterValueChanged') then
  begin
    try
      _Counter := TGPIOCounter(Sender);
      _Item := TGPIOItem(_Counter.UserData);
      _Item.Output := _Counter.Count;
      FChangedOnLastRead.Add(_Item);
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.CounterValueChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
    RTLEventSetEvent(FNewDataAvailable);
  end;
end;

procedure TGPIODevice.EncoderValueChanged(Sender: TObject);
var
  _Item : TGPIOItem;
  _Encoder : TGPIOEncoder;
begin
  if FCSChanges.Acquire('TGPIODevice.EncoderValueChanged') then
  begin
    try
      _Encoder := TGPIOEncoder(Sender);
      _Item := TGPIOItem(_Encoder.UserData);
      _Item.Output := _Encoder.Counter;
      FChangedOnLastRead.Add(_Item);
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.EncoderValueChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
    RTLEventSetEvent(FNewDataAvailable);
  end;
end;

procedure TGPIODevice.TemperatureChanged(Sender: TObject);
begin
  if FCSChanges.Acquire('TGPIODevice.TemperatureChanged') then
  begin
    try
      if FTemp <> nil then
      begin
        {$IFDEF Linux}
        FTemp.Output := FSysMon.Temperature * 0.1;
        {$ENDIF}
        FChangedOnLastRead.Add(FTemp);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.TemperatureChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
  end;
end;

procedure TGPIODevice.AvailableMemoryChanged(Sender: TObject);
begin
  if FCSChanges.Acquire('TGPIODevice.AvailableMemoryChanged') then
  begin
    try
      if FMemAvailable <> nil then
      begin
        {$IFDEF Linux}
        FMemAvailable.Output := FSysMon.AvailableMemory;
        {$ENDIF}
        FChangedOnLastRead.Add(FMemAvailable);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.AvailableMemoryChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
  end;
end;

procedure TGPIODevice.CPULoadChanged(Sender: TObject);
begin
  if FCSChanges.Acquire('TGPIODevice.CPULoadChanged') then
  begin
    try
      if FCPULoad <> nil then
      begin
        {$IFDEF Linux}
        FCPULoad.Output := FSysMon.CPULoad;
        {$ENDIF}
        FChangedOnLastRead.Add(FCPULoad);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.CPULoadChanged', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSChanges.Release();
  end;
end;

procedure TGPIODevice.InternalError(Sender: TObject);
begin
  if FCSChanges.Acquire('TGPIODevice.InternalError') then
  begin
    try
      if FInternalError <> nil then
      begin
        FInternalError.Output := True;
        FChangedOnLastRead.Add(FInternalError);
      end;
      if FInternalErrorDescription <> nil then
      begin
        FInternalErrorDescription.Output := FInternalErrorCatcher.LastErrorMsg;
        FChangedOnLastRead.Add(FInternalErrorDescription);
      end;
    finally
      FCSChanges.Release();
    end;
  end;
end;

function TGPIODevice.get_ChangedItem(anIdx: Integer): TBasePLCItem;
begin
  Result := FChangedOnLastRead[anIdx];
end;

function TGPIODevice.get_ChangedItemsCount: Integer;
begin
  Result := FChangedOnLastRead.Count;
end;

function TGPIODevice.get_ChangedItemValueAsDouble(anIdx: Integer): Double;
begin
  Result := Double(FChangedOnLastRead[anIdx].ValueAsVariant);
end;

function TGPIODevice.MonitoredItemsAsJSONArray(const codeRef, valueRef: String): TJSONArray;
var
  i: Integer;
begin
  Result := TJSONArray.Create;
  if FCSMonitoredItems.Acquire('TGPIODevice.MonitoredItemsAsJSONArray') then
  begin
    try
      for i := 0 to FMonitoredItems.Count - 1 do
        Result.Add(TGPIOItem(FMonitoredItems[i]).asJSONObject(codeRef, valueRef));
    except
      on E: Exception do
        LBLogger.Write(1, 'TGPIODevice.MonitoredItemsAsJSONArray', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSMonitoredItems.Release();
  end;
end;

procedure TGPIODevice.clearItemsList;
begin
  if FCSMonitoredItems.Acquire('TGPIODevice.clearItemsList', 10000) then
  begin
    try
      FMonitoredItems.Clear;   // TObjectList libera gli oggetti
    finally
      FCSMonitoredItems.Release();
    end;
  end;
end;

function TGPIODevice.CloneItem(anItem: TBasePLCItem): Boolean;
var
  _src: TGPIOItem;
  _clone: TGPIOItem;
begin
  Result := False;
  if anItem is TGPIOItem then
  begin
    _src := TGPIOItem(anItem);
    _clone := TGPIOItem.Create;
    _clone.Id := _src.Id;
    _clone.Code := _src.Code;
    _clone.OwnerClient := Self;
    _clone.FItemType := _src.FItemType;
    _clone.FPullType := _src.FPullType;
    SetLength(_clone.FGPIOs, Length(_src.FGPIOs));
    Move(_src.FGPIOs[0], _clone.FGPIOs[0], Length(_src.FGPIOs));
    _clone.FMicroSecDebounce := _src.FMicroSecDebounce;
    _clone.Output := _src.Output; // copia superficiale, ma per i nostri tipi va bene
    if FCSMonitoredItems.Acquire('TGPIODevice.CloneItem') then
    try
      FMonitoredItems.Add(_clone);
      Result := True;
    finally
      FCSMonitoredItems.Release();
    end;
    if not Result then _clone.Free;
  end;
end;

function TGPIODevice.getValueAsVariant(aItem: TBasePLCItem): Variant;
begin
  Result := (aItem as TGPIOItem).ValueAsVariant;
end;

procedure TGPIODevice.Execute;
var
  i: Integer;
  _Encoder: TGPIOEncoder;
  _Item: TGPIOItem;
begin
  {$IFDEF Linux}
  FSysMon.Start();
  {$ENDIF}
  while not Self.Terminated do
  begin
    if FCSChanges.Acquire('TGPIODevice.Execute') then
    begin
      try
        if FChangedOnLastRead.Count = 0 then
        begin
          for i := 0 to FEncoders.Count - 1 do
          begin
            _Encoder := FEncoders.Items[i];
            _Item := TGPIOItem(_Encoder.UserData);
            _Item.Output := _Encoder.Counter;
            FChangedOnLastRead.Add(_Item);
          end;
        end;
        if FChangedOnLastRead.Count > 0 then
        begin
          FEventsManager.RaiseEvent(cEvent_NewValuesRead);
          FChangedOnLastRead.Clear;
        end;
      except
        on E: Exception do
          LBLogger.Write(1, 'TGPIODevice.Execute', lmt_Error, 'Error: <%s>', [E.Message]);
      end;
      FCSChanges.Release();
    end;
    RTLEventWaitFor(FNewDataAvailable, FRefreshInterval);
  end;
end;

constructor TGPIODevice.Create(aOwner: TPLCDevicesCollection);
begin
  inherited Create(aOwner);
  FEncoders := TGPIO_EncodersList.Create(False);
  FChangedOnLastRead := TGPIOItemsList.Create(False);
  FCSChanges := TTimedOutCriticalSection.Create;
  FNewDataAvailable := RTLEventCreate();

  FTemp := nil;
  FCPULoad := nil;
  FMemAvailable := nil;
  FInternalError := nil;
  FInternalErrorDescription := nil;

  {$IFDEF Linux}
  FSysMon := TRPI_SystemMonitor.Create;
  FSysMon.OnTemperatureChanged := @Self.TemperatureChanged;
  FSysMon.OnAvailableMemoryChanged := @Self.AvailableMemoryChanged;
  FSysMon.OnCPULoadChanged := @Self.CPULoadChanged;
  {$ENDIF}

  FInternalErrorCatcher := TGPIO_InternalErrorCatcher.Create();
  FInternalErrorCatcher.OnError := @Self.InternalError;
end;

destructor TGPIODevice.Destroy;
begin
  try
    FreeAndNil(FEncoders);
    RTLEventDestroy(FNewDataAvailable);
    {$IFDEF Linux}
    FreeAndNil(FSysMon);
    {$ENDIF}
  except
    on E: Exception do
      LBLogger.Write(1, 'TGPIODevice.Destroy', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
  inherited Destroy;
end;

procedure TGPIODevice.Terminate;
begin
  inherited Terminate;
  RTLEventSetEvent(FNewDataAvailable);
end;

function TGPIODevice.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _Item: TGPIOItem;
  _Node: TDOMNode;
  i: Integer;
begin
  Self.clearItemsList();
  Result := inherited loadFromXMLNode(aParentNode);
  if Result then
  begin
    _Node := aParentNode.FindNode(cItemsNodeName);
    if _Node <> nil then
    begin
      if FCSMonitoredItems.Acquire('TGPIODevice.loadFromXMLNode') then
      begin
        try
          for i := 0 to _Node.ChildNodes.Count - 1 do
          begin
            _Item := TGPIOItem.Create;
            if _Item.loadFromXMLNode(_Node.ChildNodes.Item[i]) then
            begin
              _Item.OwnerClient := Self;
              Self.createItemUpdater(_Item);
              FMonitoredItems.Add(_Item);
            end
            else
            begin
              _Item.Free;
              Result := False;
              LBLogger.Write(1, 'TGPIODevice.loadFromXMLNode', lmt_Warning, 'Item %d not loaded!', [i]);
              Break;
            end;
          end;
        finally
          FCSMonitoredItems.Release();
        end;
        Result := FMonitoredItems.Count > 0;
        if Result then
          LBLogger.Write(5, 'TGPIODevice.loadFromXMLNode', lmt_Debug, 'Client <%s> items available: %d', [FCode, FMonitoredItems.Count]);
      end;
    end;
  end;
end;

{ TGPIODevice_ClientsCollection }

constructor TGPIODevice_ClientsCollection.Create;
begin
  inherited Create;
  FItemClassType := TGPIODevice;
end;

class function TGPIODevice_ClientsCollection.getRootNodeName(): String;
begin
  Result := cGPIODeviceClientsRootNodeName;
end;

initialization
  RegisterPLCCollection(TGPIODevice_ClientsCollection, 'GPIODevices');

end.
