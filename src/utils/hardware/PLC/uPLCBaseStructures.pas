unit uPLCBaseStructures;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Laz2_DOM, uLBBaseThread, uEventsManager,
  uTimedoutCriticalSection, fpjson, fgl, Contnrs;

type
  TPLCDevicesCollection = class;

  { TBasePLCItem }

  TBasePLCItem = class(TObject)
  private
    FId: Integer;
    FCode: string;
    FOwnerClient: TObject;   // TPLCBaseClient, non posseduto
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ValueAsVariant: Variant; virtual; abstract;
    function ValueAsString: string; virtual; abstract;

    // Scritture di default (sollevano eccezione se non supportate)
    procedure WriteBoolean(Value: Boolean); virtual;
    procedure WriteWord(Value: Word); virtual;
    procedure WriteFloat(Value: Single); virtual;
    procedure WriteString(const S: string); virtual;

    property Id: Integer read FId write FId;
    property Code: string read FCode write FCode;
    property OwnerClient: TObject read FOwnerClient write FOwnerClient;
  end;

  TSortedMonitoredItems = specialize TFPGMap<Integer, TBasePLCItem>;

  TBaseParamValueType = (bpvt_Unknown  = 0,
                         bpvt_Boolean  = 1,
                         bpvt_Integer  = 2,
                         bpvt_Float    = 3,
                         bpvt_String   = 4);


  { TPLCBaseClient }

  TPLCBaseClient = class(TLBBaseThread)
    strict private
      procedure set_Owner(AValue: TPLCDevicesCollection);

    strict protected
      FId   : Integer;
      FRefreshInterval : Integer;
      FCode : string;
      FHost : string;
      FPort : Word;
      FEventsManager : TEventsManager;
      FOwner : TPLCDevicesCollection;

      FMonitoredItems   : TObjectList;          // di TBasePLCItem, OwnsObjects=True
      FCSMonitoredItems : TTimedOutCriticalSection;

      function get_ChangedItem(anIdx: Integer): TBasePLCItem; virtual; abstract;
      function get_ChangedItemsCount: Integer; virtual; abstract;
      function get_ChangedItemValueAsDouble(anIdx: Integer): Double; virtual; abstract;

      function MonitoredItemsAsJSONArray(const codeRef, valueRef: string): TJSONArray; virtual; abstract;
      procedure clearItemsList(); virtual; abstract;
      function CloneItem(anItem: TBasePLCItem): Boolean; virtual; abstract;

      function CloneInternal(aClient: TPLCBaseClient): Boolean; virtual;
      function getValueAsVariant(aItem: TBasePLCItem): Variant; virtual; abstract;

    public
      constructor Create(aOwner: TPLCDevicesCollection); reintroduce; virtual;
      destructor Destroy; override;

      function asJSONObject(const codeRef, valueRef, childrenRef: string): TJSONObject;

      function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean; virtual;
      function loadFromXMLNode(aParentNode: TDOMNode): Boolean; virtual;

      function Clone(aSource: TPLCBaseClient; CloneItems: Boolean): Boolean;
      function CloneMonitoredItems(aSourceList: TObjectList): Boolean; virtual;

      procedure UpdateGlobalIds();
      procedure retrieveMissingIds();

      function getValueType(aItem: TBasePLCItem): TBaseParamValueType;
      function getValueAs(aItem: TBasePLCItem; aValueType: TBaseParamValueType; out aValue: Variant): Boolean;

      function insertItemsIntoList(aList: TSortedMonitoredItems): Boolean;

      function LockItems(): Boolean;
      procedure UnlockItems();

      function Connected(): Boolean; virtual; abstract;

      property ChangedItemsCount: Integer read get_ChangedItemsCount;
      property ChangedItemAsDouble[anIdx: Integer]: Double read get_ChangedItemValueAsDouble;
      property ChangedItem[anIdx: Integer]: TBasePLCItem read get_ChangedItem;

      property Id: Integer read FId;
      property Code: string read FCode write FCode;
      property Host: string read FHost write FHost;
      property Port: Word read FPort write FPort;
      property RefreshInterval: Integer read FRefreshInterval write FRefreshInterval;

      property EventsManager: TEventsManager read FEventsManager;
      property OwnerCollection: TPLCDevicesCollection read FOwner write set_Owner;

      property MonitoredItems: TObjectList read FMonitoredItems;

    const
      cEvent_NewValuesRead = 'NVR';
      cEvent_DeviceConnected = 'ConnON';
      cEvent_DeviceDisconnected = 'ConnOFF';
      cEvent_Terminated = 'Terminated';
      cConnectionStatus = 'Conn';
  end;

  TPLCBaseClientClassType = class of TPLCBaseClient;

  TPLCClientsList = specialize TFPGObjectList<TPLCBaseClient>;

  { TPLCDevicesCollection }

  TPLCDevicesCollection = class(TObject)
    strict private
      FId : Integer;
      FCode : string;
      function get_Count: Integer;

    strict protected
      FClients : TPLCClientsList;
      FCS : TTimedOutCriticalSection;

      FEventsManager : TEventsManager;

      FItemClassType : TPLCBaseClientClassType;

      procedure removeClient(aSender: TObject);
      procedure clearClients();
      function createNewClient(): TPLCBaseClient;

    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure StartReading();

      function isCompatibleClient(aClient: TPLCBaseClient): Boolean;

      function AddEventListener(AnEventName: string; AnEventToRaise: TNotifyEvent; AnEventManager: TEventsManager): Boolean;

      function addClient(aClient: TPLCBaseClient): Boolean;
      function extractClient(aClient: TPLCBaseClient): Boolean;
      function getClient(aIdx: Integer): TPLCBaseClient;

      function loadFromXMLNode(aParentNode: TDOMNode): Boolean;
      function loadFromXMLFile(const aFilename: string): Boolean;

      function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
      function saveXMLFile(const aFilename, aRootNodeName: string): Boolean;

      procedure UpdateGlobalIds();
      procedure retrieveMissingIds();
      function insertItemsIntoList(aList: TSortedMonitoredItems): Boolean;

      function asJSONArray(const codeRef, valueRef, childrenRef: string): TJSONArray;
      function saveAsJSON(const aFilename: string): Boolean;

      property Count: Integer read get_Count;

      property Code: string read FCode write FCode;
      property Id: Integer read FId write FId;

      class function getRootNodeName(): string; virtual; abstract;
      class function getDeviceNodeName(): string; virtual;
  end;

  TPLCDevicesCollectionClass = class of TPLCDevicesCollection;

  procedure insertVariantIntoJSONObject(anObj: TJSONObject; const aField: string; const aValue: Variant);

  function retrieveGlobalId(): Integer;

const
  cIdAttrib = 'Id';
  cCodeAttrib = 'Code';
  cRefreshAttrib = 'Refresh';
  cHostAttrib = 'Host';
  cPortAttrib = 'Port';
  cTypeAttrib = 'Type';
  cPullTypeAttrib = 'PullType';
  cDebouceAttrib = 'Debounce';
  cGPIOAttrib = 'GPIO';
  cGPIO_A_Attrib = 'GPIO_A';
  cGPIO_B_Attrib = 'GPIO_B';

  cStartingOffset = 'Offset';

  cCodeNodeName = 'Code';
  cHostNodeName = 'Host';
  cRefreshIntervalNodeName = 'Refresh';

  cVariableValue = 'value';

implementation

uses
  ULBLogger, uLBFileUtils, Laz2_XMLWrite, LazFileUtils, Variants;

var
  gv_PLCGlobalId : Integer = 0;

procedure insertVariantIntoJSONObject(anObj: TJSONObject; const aField: string; const aValue: Variant);
begin
  if aField <> '' then
  begin
    case VarType(aValue) of
      varNull     : anObj.Add(aField, '');
      varByte     : anObj.Add(aField, Byte(aValue));
      varWord     : anObj.Add(aField, Word(aValue));
      varSmallInt : anObj.Add(aField, SmallInt(aValue));
      varInteger  : anObj.Add(aField, Integer(aValue));
      varInt64    : anObj.Add(aField, Int64(aValue));
      varQWord    : anObj.Add(aField, QWord(aValue));
      varSingle   : anObj.Add(aField, Single(aValue));
      varDouble   : anObj.Add(aField, Double(aValue));
      varBoolean  : anObj.Add(aField, Boolean(aValue));
      varString   : anObj.Add(aField, VarToStr(aValue));

      else begin
        anObj.Add(aField, '???');
        LBLogger.Write(1, 'insertVariantIntoJSONObject', lmt_Warning, 'Variant type unknown');
      end;
    end;
  end;
end;

function retrieveGlobalId(): Integer;
begin
  Result := InterlockedIncrement(gv_PLCGlobalId);
end;

procedure updateGlobalId(aValue: Integer);
var
  _OldValue : Integer;
begin
  repeat
    _OldValue := gv_PLCGlobalId;
    if aValue <= _OldValue then Exit;
  until InterlockedCompareExchange(gv_PLCGlobalId, aValue, _OldValue) = _OldValue;
end;

{ TBasePLCItem }

constructor TBasePLCItem.Create;
begin
  inherited Create;
  FId := 0;
  FCode := '';
  FOwnerClient := nil;
end;

destructor TBasePLCItem.Destroy;
begin
  inherited Destroy;
end;

procedure TBasePLCItem.WriteBoolean(Value: Boolean);
begin
  LBLogger.Write(1, 'TBasePLCItem.WriteBoolean', lmt_Error, 'WriteBoolean not implemented for item %d (%s)', [FId, FCode]);
  raise ENotSupportedException.Create('WriteBoolean non supportato per questo tipo di item');
end;

procedure TBasePLCItem.WriteWord(Value: Word);
begin
  LBLogger.Write(1, 'TBasePLCItem.WriteWord', lmt_Error, 'WriteWord not implemented for item %d (%s)', [FId, FCode]);
  raise ENotSupportedException.Create('WriteWord non supportato per questo tipo di item');
end;

procedure TBasePLCItem.WriteFloat(Value: Single);
begin
  raise ENotSupportedException.Create('WriteFloat non supportato per questo tipo di item');
end;

procedure TBasePLCItem.WriteString(const S: string);
begin
  raise ENotSupportedException.Create('WriteString non supportato per questo tipo di item');
end;

{ TPLCBaseClient }

procedure TPLCBaseClient.set_Owner(AValue: TPLCDevicesCollection);
begin
  if FOwner <> AValue then
  begin
    if FOwner <> nil then
      FOwner.extractClient(Self);
    FOwner := AValue;
    if FOwner <> nil then
      FOwner.addClient(Self);
  end;
end;

function TPLCBaseClient.CloneInternal(aClient: TPLCBaseClient): Boolean;
begin
  Result := True;
end;

function TPLCBaseClient.getValueAs(aItem: TBasePLCItem; aValueType: TBaseParamValueType; out aValue: Variant): Boolean;
var
  _Value : Variant;
begin
  Result := True;
  _Value := Self.getValueAsVariant(aItem);
  if VarIsNull(_Value) then
  begin
    case aValueType of
      bpvt_Boolean : aValue := False;
      bpvt_Integer : aValue := 0;
      bpvt_Float   : aValue := 0.0;
      bpvt_String  : aValue := '';
    end;
  end
  else begin
    case aValueType of
      bpvt_Boolean : aValue := Boolean(_Value);
      bpvt_Integer : aValue := Integer(_Value);
      bpvt_Float   : begin
                       case VarType(_Value) of
                         varBoolean: if Boolean(_Value) then aValue := Double(1.0) else aValue := Double(0.0);
                         else aValue := Double(_Value);
                       end;
                     end;
      bpvt_String  : aValue := VarToStr(_Value);
      else begin
        Result := False;
        aValue := Null;
      end;
    end;
  end;
end;

function TPLCBaseClient.CloneMonitoredItems(aSourceList: TObjectList): Boolean;
var
  i : Integer;
begin
  Result := False;
  if (aSourceList <> nil) and (aSourceList.Count > 0) then
  begin
    for i := 0 to aSourceList.Count - 1 do
      Self.CloneItem(TBasePLCItem(aSourceList.Items[i]));
    Result := True;
  end;
end;

procedure TPLCBaseClient.UpdateGlobalIds();
var
  i : Integer;
  Item : TBasePLCItem;
begin
  if FId > 0 then
    updateGlobalId(FId);
  if FCSMonitoredItems.Acquire('TPLCBaseClient.UpdateGlobalIds') then
  begin
    try
      for i := 0 to FMonitoredItems.Count - 1 do
      begin
        Item := TBasePLCItem(FMonitoredItems.Items[i]);
        if Item.Id > 0 then
          updateGlobalId(Item.Id);
      end;
    finally
      FCSMonitoredItems.Release();
    end;
  end;
end;

constructor TPLCBaseClient.Create(aOwner: TPLCDevicesCollection);
begin
  inherited Create;
  FHost := '';
  FCode := '';
  FRefreshInterval := 1000;
  FMonitoredItems := TObjectList.Create(True);   // OwnsObjects=True

  FCSMonitoredItems := TTimedOutCriticalSection.Create;

  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_NewValuesRead);
  FEventsManager.AddEvent(cEvent_DeviceConnected);
  FEventsManager.AddEvent(cEvent_DeviceDisconnected);
  FEventsManager.AddEvent(cEvent_Terminated);

  FOwner := aOwner;
  if FOwner <> nil then
    FOwner.addClient(Self);
end;

destructor TPLCBaseClient.Destroy;
begin
  LBLogger.Write(1, 'TPLCBaseClient.Destroy', lmt_Debug, 'Destroying <%s> <%s:%d>, id %d', [FCode, FHost, Integer(FPort), FId]);
  inherited Destroy;

  try
    if FEventsManager <> nil then
    begin
      FEventsManager.RaiseEvent(cEvent_Terminated);
      FreeAndNil(FEventsManager);
    end;
    Self.clearItemsList();
    FreeAndNil(FCSMonitoredItems);
    FreeAndNil(FMonitoredItems);
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCBaseClient.Destroy', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

function TPLCBaseClient.asJSONObject(const codeRef, valueRef, childrenRef: string): TJSONObject;
var
  _Items : TJSONArray = nil;
begin
  Result := TJSONObject.Create;
  try
    Result.Add('id', FId);
    if codeRef = '' then Result.Add('Code', FCode)
    else Result.Add(codeRef, FCode);
    Result.Add(cConnectionStatus, Self.Connected);
    _Items := Self.MonitoredItemsAsJSONArray(codeRef, valueRef);
    if _Items <> nil then
    begin
      if childrenRef = '' then Result.Add('Items', _Items)
      else Result.Add(childrenRef, _Items);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCBaseClient.asJSONObject', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

function TPLCBaseClient.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
begin
  Result := False;
  if (aDoc <> nil) and (aParentNode <> nil) then
  begin
    TDOMElement(aParentNode).SetAttribute(cIdAttrib, IntToStr(FId));
    TDOMElement(aParentNode).SetAttribute(cCodeAttrib, FCode);
    TDOMElement(aParentNode).SetAttribute(cHostAttrib, FHost);
    if FPort > 0 then
      TDOMElement(aParentNode).SetAttribute(cPortAttrib, IntToStr(FPort));
    TDOMElement(aParentNode).SetAttribute(cRefreshAttrib, IntToStr(FRefreshInterval));
    Result := True;
  end;
end;

function TPLCBaseClient.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _Node : TDOMNode;
  _Port : Integer;
begin
  Result := False;

  if aParentNode <> nil then
  begin
    FId := StrToIntDef(TDOMElement(aParentNode).GetAttribute(cIdAttrib), 0);
    FCode := TDOMElement(aParentNode).GetAttribute(cCodeAttrib);
    FHost := TDOMElement(aParentNode).GetAttribute(cHostAttrib);
    _Port := StrToIntDef(TDOMElement(aParentNode).GetAttribute(cPortAttrib), 0);
    if _Port > 0 then
      FPort := _Port;

    FRefreshInterval := StrToIntDef(TDOMElement(aParentNode).GetAttribute(cRefreshAttrib), 0);
    if FCode = '' then
    begin
      _Node := aParentNode.FindNode(cCodeNodeName);
      if _Node <> nil then FCode := Trim(_Node.TextContent);
    end;

    if FHost = '' then
    begin
      _Node := aParentNode.FindNode(cHostNodeName);
      if _Node <> nil then FHost := Trim(_Node.TextContent);
    end;

    if FRefreshInterval = 0 then
    begin
      _Node := aParentNode.FindNode(cRefreshIntervalNodeName);
      if _Node <> nil then FRefreshInterval := StrToIntDef(_Node.TextContent, 1000)
      else FRefreshInterval := 1000;
    end;

    Result := True;
  end
  else
    LBLogger.Write(1, 'TPLCBaseClient.loadFromXMLNode', lmt_Warning, 'Parent node not set!');
end;

function TPLCBaseClient.Clone(aSource: TPLCBaseClient; CloneItems: Boolean): Boolean;
begin
  Result := False;
  if aSource <> nil then
  begin
    FId              := aSource.Id;
    FCode            := aSource.Code;
    FHost            := aSource.Host;
    FPort            := aSource.Port;
    FRefreshInterval := aSource.RefreshInterval;
    Result := Self.CloneInternal(aSource);
    if CloneItems then
      Self.CloneMonitoredItems(aSource.MonitoredItems);
  end;
end;

procedure TPLCBaseClient.retrieveMissingIds;
var
  i : Integer;
  Item : TBasePLCItem;
begin
  if FId = 0 then FId := retrieveGlobalId();
  if FCSMonitoredItems.Acquire('TPLCBaseClient.UpdateIds') then
  begin
    try
      for i := 0 to FMonitoredItems.Count - 1 do
      begin
        Item := TBasePLCItem(FMonitoredItems[i]);
        if Item.Id = 0 then Item.Id := retrieveGlobalId();
      end;
    finally
      FCSMonitoredItems.Release();
    end;
  end;
end;

function TPLCBaseClient.getValueType(aItem: TBasePLCItem): TBaseParamValueType;
var
  _Value : Variant;
begin
  _Value := Self.getValueAsVariant(aItem);
  case VarType(_Value) of
    varByte, varWord, varSmallInt, varInteger : Result := bpvt_Integer;
    varSingle, varDouble : Result := bpvt_Float;
    varString  : Result := bpvt_String;
    varBoolean : Result := bpvt_Boolean;
    else Result := bpvt_Unknown;
  end;
end;

function TPLCBaseClient.insertItemsIntoList(aList: TSortedMonitoredItems): Boolean;
var
  i : Integer;
  Item : TBasePLCItem;
begin
  Result := False;
  if FCSMonitoredItems.Acquire('TPLCBaseClient.addItemsToMonitoredItems') then
  begin
    try
      for i := 0 to FMonitoredItems.Count - 1 do
      begin
        Item := TBasePLCItem(FMonitoredItems[i]);
        aList.Add(Item.Id, Item);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TPLCBaseClient.addItemsToMonitoredItems', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCSMonitoredItems.Release();
  end;
end;

function TPLCBaseClient.LockItems(): Boolean;
begin
  Result := FCSMonitoredItems.Acquire('TPLCBaseClient.LockItems');
end;

procedure TPLCBaseClient.UnlockItems();
begin
  FCSMonitoredItems.Release();
end;

{ TPLCDevicesCollection (invariato) }

function TPLCDevicesCollection.get_Count: Integer;
begin
  Result := 0;
  if FCS.Acquire('TPLCDevicesCollection.get_Count') then
  begin
    try
      Result := FClients.Count;
    finally
      FCS.Release();
    end;
  end;
end;

procedure TPLCDevicesCollection.removeClient(aSender: TObject);
var
  _Idx : Integer;
begin
  if FCS.Acquire('TPLCDevicesCollection.removeClient') then
  begin
    try
      _Idx := FClients.IndexOf(TPLCBaseClient(aSender));
      if _Idx >= 0 then FClients.Delete(_Idx);
    except
      on E: Exception do
        LBLogger.Write(1, 'TPLCDevicesCollection.removeClient', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    FCS.Release();
  end;
end;

procedure TPLCDevicesCollection.clearClients();
(*
var
  i : Integer;
  _Client : TPLCBaseClient;
  _Clients : array of TPLCBaseClient;
  _AllNil : Boolean;
  *)

begin
  if FCS.Acquire('TPLCDevicesCollection.clearClients', 10000) then
  begin

    try

      FClients.Clear;

    except
      on E: Exception do
        LBLogger.Write(1, 'TPLCDevicesCollection.clearClients', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;

  (*
  try
    for i := FClients.Count - 1 downto 0 do
    begin
      _Client := FClients.Items[i];
      if _Client.Suspended then
      begin
        FClients.Delete(i);
        _Client.Free;
      end;
    end;
    if FClients.Count > 0 then
    begin
      SetLength(_Clients, FClients.Count);
      for i := 0 to High(_Clients) do
      begin
        _Clients[i] := FClients.Items[i];
        _Clients[i].AddReference(@_Clients[i]);
        _Clients[i].Terminate;
      end;
      _AllNil := False;
      while not _AllNil do
      begin
        _AllNil := True;
        for i := 0 to High(_Clients) do
          if _Clients[i] <> nil then
          begin
            _AllNil := False;
            Break;
          end;
        if not _AllNil then Sleep(10);
      end;
      FClients.Clear;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCDevicesCollection.clearClients', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
  FCS.Release();
  *)
end;

function TPLCDevicesCollection.createNewClient(): TPLCBaseClient;
begin
  if FItemClassType <> nil then
    Result := FItemClassType.Create(Self)
  else
  begin
    Result := nil;
    LBLogger.Write(1, 'TPLCDevicesCollection.createNewClient', lmt_Warning, '<%s>  -  Item class type not set', [Self.ClassName]);
  end;
end;

constructor TPLCDevicesCollection.Create;
begin
  inherited Create;

  FEventsManager := TEventsManager.Create(Self);

  FClients := TPLCClientsList.Create(True);
  FCS := TTimedOutCriticalSection.Create;
end;

destructor TPLCDevicesCollection.Destroy;
begin

  FreeAndNil(FEventsManager);
  Self.clearClients();
  try
    FreeAndNil(FCS);
    FreeAndNil(FClients);
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCDevicesCollection.Destroy', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
  inherited Destroy;
end;

procedure TPLCDevicesCollection.StartReading();
var i: Integer;
begin
  if not FCS.Acquire('TPLCDevicesCollection.StartReading') then Exit;
  try
    for i := 0 to FClients.Count - 1 do
    begin
      if FClients.Items[i].Suspended then
        FClients.Items[i].Start();
    end;
  finally
    FCS.Release();
  end;
end;

function TPLCDevicesCollection.isCompatibleClient(aClient: TPLCBaseClient): Boolean;
begin
  Result := aClient is FItemClassType;
end;

function TPLCDevicesCollection.AddEventListener(AnEventName: string; AnEventToRaise: TNotifyEvent; AnEventManager: TEventsManager): Boolean;
var i: Integer;
begin
  Result := False;
  if (AnEventManager = nil) or (AnEventName = '') or (AnEventToRaise = nil) then Exit;
  if FCS.Acquire('TPLCDevicesCollection.AddEventListener') then
  begin
    try
      for i := 0 to FClients.Count - 1 do
        FClients.Items[i].EventsManager.AddEventListener(AnEventName, AnEventToRaise, AnEventManager);
    finally
      FCS.Release();
    end;
  end;
end;

function TPLCDevicesCollection.addClient(aClient: TPLCBaseClient): Boolean;
begin
  Result := False;
  if (aClient <> nil) and FCS.Acquire('TPLCDevicesCollection.addClient') then
  begin
    try
      if FClients.IndexOf(aClient) = -1 then
      begin
        FClients.Add(aClient);
        aClient.EventsManager.AddEventListener(TPLCBaseClient.cEvent_Terminated, @Self.removeClient, FEventsManager);
      end;
      Result := True;
    finally
      FCS.Release();
    end;
  end;
end;

function TPLCDevicesCollection.extractClient(aClient: TPLCBaseClient): Boolean;
var
    _Idx: Integer;
begin
  Result := False;

  if (aClient <> nil) and FCS.Acquire('TPLCDevicesCollection.extractClient') then
  begin
    try
      _Idx := FClients.IndexOf(aClient);
      if _Idx > -1 then FClients.Delete(_Idx);
      Result := True;
    finally
      FCS.Release();
    end;
  end;
end;

function TPLCDevicesCollection.getClient(aIdx: Integer): TPLCBaseClient;
begin
  Result := nil;
  if FCS.Acquire('TPLCDevicesCollection.get_Count') then
  begin
    try
      if (aIdx >= 0) and (aIdx < FClients.Count) then
        Result := FClients.Items[aIdx];
    finally
      FCS.Release;
    end;
  end;
end;

function TPLCDevicesCollection.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  i: Integer;
  _Node: TDOMNode;
  _Client: TPLCBaseClient;
  _MainNode : TDOMNode;
begin
  Result := False;

  if aParentNode <> nil then
  begin

    if aParentNode.NodeName = Self.getRootNodeName() then
      _MainNode := aParentNode
    else
      _MainNode := aParentNode.FindNode(Self.getRootNodeName());

    if _MainNode <> nil then
    begin

      Self.clearClients;
      FId := StrToIntDef(TDOMElement(_MainNode).GetAttribute(cIdAttrib), 0);
      FCode := TDOMElement(_MainNode).GetAttribute(cCodeAttrib);

      if FCS.Acquire('TPLCDevicesCollection.loadFromXMLNode') then
      begin
        try
          for i := 0 to _MainNode.ChildNodes.Count - 1 do
          begin
            _Node := _MainNode.ChildNodes.Item[i];
            if _Node.NodeName = Self.getDeviceNodeName() then
            begin
              _Client := Self.createNewClient();
              if _Client.loadFromXMLNode(_Node) then
                Self.addClient(_Client)
              else
                FreeAndNil(_Client);
            end;
//            else
//              LBLogger.Write(1, 'TPLCDevicesCollection.loadFromXMLNode', lmt_Warning, 'Found node <%s>  -  Needed <%s>!', [_Node.NodeName, Self.getDeviceNodeName()]);
          end;
          Result := True;
        except
          on E: Exception do
            LBLogger.Write(1, 'TPLCDevicesCollection.loadFromXMLNode', lmt_Error, 'Error: <%s>', [E.Message]);
        end;
        FCS.Release();
      end;

    end
    else
      LBLogger.Write(1, 'TPLCDevicesCollection.loadFromXMLNode', lmt_Warning, 'Node <%s> not found!', [Self.getRootNodeName()]);

  end
  else
    LBLogger.Write(1, 'TPLCDevicesCollection.loadFromXMLNode', lmt_Warning, 'Parent node not set!');

end;

function TPLCDevicesCollection.loadFromXMLFile(const aFilename: string): Boolean;
var
  _Doc : TXMLDocument = nil;
begin
  Result := False;
  if OpenXMLFile(aFilename, _Doc) then
    Result := Self.loadFromXMLNode(_Doc.DocumentElement);

  if _Doc <> nil then _Doc.Free;
end;

function TPLCDevicesCollection.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
var
  i: Integer;
  _ClientNode: TDOMNode;
  _MainNode : TDOMNode;

begin
  Result := False;
  if (aDoc = nil) or (aParentNode = nil) then Exit;

  if aParentNode.NodeName = Self.getRootNodeName() then
    _MainNode := aParentNode
  else begin
    _MainNode := aDoc.CreateElement(Self.getRootNodeName());
    aParentNode.AppendChild(_MainNode);
  end;

  TDOMElement(_MainNode).SetAttribute(cIdAttrib, IntToStr(FId));
  TDOMElement(_MainNode).SetAttribute(cCodeAttrib, FCode);

  if FCS.Acquire('TPLCDevicesCollection.insertIntoXMLNode') then
  begin
    try
      for i := 0 to FClients.Count - 1 do
      begin
        _ClientNode := aDoc.CreateElement(Self.getDeviceNodeName());
        _MainNode.AppendChild(_ClientNode);

        if not FClients.Items[i].insertIntoXMLNode(aDoc, _ClientNode) then Break;
      end;
      Result := True;
    except
      on E: Exception do
        LBLogger.Write(1, 'TPLCDevicesCollection.insertIntoXMLNode', lmt_Error, 'Error serializing clients: <%s>', [E.Message]);
    end;
    FCS.Release();
  end;
end;

function TPLCDevicesCollection.saveXMLFile(const aFilename, aRootNodeName: string): Boolean;
var
  _Doc : TXMLDocument = nil;
  _Path : string;
  _Root : TDOMNode;

begin
  Result := False;
  if aFilename = '' then Exit;
  if FileExistsUTF8(aFilename) then
  begin
    LBLogger.Write(5, 'TPLCDevicesCollection.saveXMLFile', lmt_Debug, 'File <%s> already exists, deleting', [aFilename]);
    DeleteFileUTF8(aFilename);
  end;
  _Path := ExtractFilePath(aFilename);
  if not DirectoryExistsUTF8(_Path) then ForceDirectoriesUTF8(_Path);

  try
    _Doc := TXMLDocument.Create;
    _Root := _Doc.CreateElement(aRootNodeName);
    _Doc.AppendChild(_Root);
    Result := Self.insertIntoXMLNode(_Doc, _Root);
    if Result then
    begin
      WriteXMLFile(_Doc, aFilename);
      Result := FileExistsUTF8(aFilename) and (FileSizeUtf8(aFilename) > 0);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCDevicesCollection.saveXMLFile', lmt_Error, 'Error: <%s>', [E.Message]);
  end;

  if _Doc <> nil then _Doc.Free;
end;

procedure TPLCDevicesCollection.UpdateGlobalIds();
var
  i: Integer;
begin
  updateGlobalId(FId);
  for i := 0 to FClients.Count - 1 do
    FClients.Items[i].UpdateGlobalIds;
end;

procedure TPLCDevicesCollection.retrieveMissingIds;
var
  i: Integer;
begin
  if FId = 0 then FId := retrieveGlobalId();

  if FCS.Acquire('TPLCDevicesCollection.UpdateIds') then
  begin
    try
      for i := 0 to FClients.Count - 1 do
        FClients.Items[i].retrieveMissingIds();
    finally
      FCS.Release();
    end;
  end;
end;

function TPLCDevicesCollection.insertItemsIntoList(aList: TSortedMonitoredItems): Boolean;
var
  i: Integer;
begin
  Result := False;

  if aList = nil then Exit;

  if FCS.Acquire('TPLCDevicesCollection.insertItemsIntoList') then
  begin
    try
      for i := 0 to FClients.Count - 1 do
        FClients.Items[i].insertItemsIntoList(aList);
    finally
      FCS.Release();
    end;
  end;
end;

function TPLCDevicesCollection.asJSONArray(const codeRef, valueRef, childrenRef: string): TJSONArray;
var
  i: Integer;
  _Client : TPLCBaseClient;

begin
  Result := TJSONArray.Create;
  if FCS.Acquire('TPLCDevicesCollection.asJSONArray') then
  begin
    try
      for i := 0 to FClients.Count - 1 do
      begin
        _Client := FClients.Items[i];
        if _Client <> nil then
        begin
          LBLogger.Write(1, 'TPLCDevicesCollection.asJSONArray', lmt_Debug, 'Adding client: <%s> - Index: %d', [_Client.Code, i]);
          Result.Add(_Client.asJSONObject(codeRef, valueRef, childrenRef));
        end
        else
          LBLogger.Write(1, 'TPLCDevicesCollection.asJSONArray', lmt_Debug, 'Client (id=%d, index=%d) is null', [FId, i]);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TPLCDevicesCollection.asJSONArray', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

function TPLCDevicesCollection.saveAsJSON(const aFilename: string): Boolean;
var _Array : TJSONArray = nil;
    _File : TFileStream = nil;
    _JSONString : string;
    _FilePath : string;
begin
  Result := False;
  if aFilename = '' then Exit;
  _FilePath := ExtractFilePath(aFilename);
  if not DirectoryExistsUTF8(_FilePath) then ForceDirectoriesUTF8(_FilePath);
  if FileExistsUTF8(aFilename) then DeleteFileUTF8(aFilename);
  try
    _Array := Self.asJSONArray('', '', '');
    _Array.CompressedJSON := True;
    _JSONString := _Array.AsJSON;
    _File := TFileStream.Create(aFilename, fmCreate);
    _File.Write(_JSONString[1], Length(_JSONString));
    Result := FileExistsUTF8(aFilename) and (FileSizeUtf8(aFilename) > 0);
  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCDevicesCollection.saveAsJSON', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
  if _Array <> nil then _Array.Free;
  if _File <> nil then _File.Free;
end;

class function TPLCDevicesCollection.getDeviceNodeName(): string;
begin
  Result := 'Device';
end;

end.
