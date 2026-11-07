unit uPLCCollector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, uPLCBaseStructures, fpjson,
  Generics.Collections, fgl, uEventsManager;

type
  TDataValueChangedEvent = procedure (Sender: TPLCBaseClient) of object;

  TPLCCollectionsList = specialize TFPGObjectList<TPLCDevicesCollection>;

  { TPLCCollector }

  TPLCCollector = class(TObject)
    strict private
      FOnDataValueChanged : TDataValueChangedEvent;
      FOnDeviceConnected : TDataValueChangedEvent;
      FOnDeviceDisconnected : TDataValueChangedEvent;

      FEventsManager : TEventsManager;

      FPLCTypes : TPLCCollectionsList;

      FSortedMonitoredItems : TSortedMonitoredItems;

      procedure DataValueChanged(Sender: TObject);
      procedure DeviceConnected(Sender: TObject);
      procedure DeviceDisconnected(Sender: TObject);

    public
      constructor Create;
      destructor Destroy; override;

      procedure StartReading();

      function addClient(aClient: TPLCBaseClient): Boolean;
      function getClientById(anId: Integer): TPLCBaseClient;

      function loadFromXMLNode(aNode: TDOMNode): Boolean;
      function loadFromXMLFile(const aFilename: String): Boolean;

      function saveAsXMLFile(const aFilename: String): Boolean;
      function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;

      function asJSONObject(): TJSONObject;
      function fillJSONObject(anObject: TJSONObject): Boolean;

      function asJSONArray(): TJSONArray;
      function fillJSONArray(anArray: TJSONArray): Boolean;

      procedure SortMonitoredItems();

      property SortedMonitoredItems: TSortedMonitoredItems read FSortedMonitoredItems;
      property PLCTypes: TPLCCollectionsList read FPLCTypes;

      property OnDataValueChanged : TDataValueChangedEvent write FOnDataValueChanged;
      property OnDeviceConnected : TDataValueChangedEvent write FOnDeviceConnected;
      property OnDeviceDisconnected : TDataValueChangedEvent write FOnDeviceDisconnected;

      const
        cPLCDevicesRootNodeName = DOMString('PLCDevices');
  end;


function RegisterPLCCollection(aCollectionClass: TPLCDevicesCollectionClass; const aDescription: String): Boolean;
function getManagedPLCList(): TStringList;


implementation

uses
  ULBLogger, uLBFileUtils, LazFileUtils, Laz2_XMLWrite;


type
  TPLCCollectionTypesDictionary = specialize TDictionary<TPLCDevicesCollectionClass, String>; // Associa un tipo di PLC alla sua descrizione
  TPLCCollectionTypesList = specialize TList<TPLCDevicesCollectionClass>;

  TPLCTypesList = specialize TDictionary<TPLCBaseClientClassType, TPLCDevicesCollection>; // Associa un tipo di PLC la relativa collection

  { TPLCTypesCollection }

  TPLCTypesCollection = class(TObject)
    strict private
      FTypesDict : TPLCCollectionTypesDictionary;
      FTypesList : TPLCCollectionTypesList;

    public
      constructor Create;
      destructor Destroy; override;

      function addType(aBaseClientCollectionClass: TPLCDevicesCollectionClass; const aDescription: String): Boolean;
      function createCollectionsList(): TPLCCollectionsList;
      function getCollectionDescription(aCollection: TPLCDevicesCollection): String;
      function getManagedPLCList(): TStringList;
  end;

var
  gv_PLCTypesCollection : TPLCTypesCollection = nil;


function RegisterPLCCollection(aCollectionClass: TPLCDevicesCollectionClass; const aDescription: String): Boolean;
begin
  Result := False;

  if gv_PLCTypesCollection = nil then
    gv_PLCTypesCollection := TPLCTypesCollection.Create;

  Result := gv_PLCTypesCollection.addType(aCollectionClass, aDescription);
end;

function getManagedPLCList(): TStringList;
begin
  REsult := gv_PLCTypesCollection.getManagedPLCList;
end;

{ TPLCTypesCollection }

constructor TPLCTypesCollection.Create;
begin
  inherited Create;

  FTypesDict := TPLCCollectionTypesDictionary.Create();
  FTypesList := TPLCCollectionTypesList.Create();
end;

destructor TPLCTypesCollection.Destroy;
begin
  FreeAndNil(FTypesDict);
  FreeAndNil(FTypesList);

  inherited Destroy;
end;

function TPLCTypesCollection.addType(aBaseClientCollectionClass: TPLCDevicesCollectionClass; const aDescription: String): Boolean;
begin
  Result := False;

  if aBaseClientCollectionClass <> nil then
  begin
    if aDescription <> '' then
    begin
      if not FTypesDict.ContainsKey(aBaseClientCollectionClass) then
      begin
        FTypesDict.Add(aBaseClientCollectionClass, aDescription);
        FTypesList.Add(aBaseClientCollectionClass);
      end
      else
        LBLogger.Write(1, 'TPLCTypesCollection.addType', lmt_Warning, 'Client class <%s> already in!', [aBaseClientCollectionClass.ClassName]);
    end
    else
      LBLogger.Write(1, 'TPLCTypesCollection.addType', lmt_Warning, 'Client class description needed!');

  end
  else
    LBLogger.Write(1, 'TPLCTypesCollection.addType', lmt_Warning, 'Client class type needed!');
end;

function TPLCTypesCollection.createCollectionsList(): TPLCCollectionsList;
var
  i : Integer;

begin
  Result := nil;

  if FTypesList.Count > 0 then
  begin
    Result := TPLCCollectionsList.Create(True);
    LBLogger.Write(5, 'TPLCTypesCollection.createCollectionsList', lmt_Debug, 'Available PLC type %d', [FTypesList.Count]);
    for i := 0 to FTypesList.Count - 1 do
    begin
      LBLogger.Write(5, 'TPLCTypesCollection.createCollectionsList', lmt_Debug, 'PLC type %s', [FTypesList.Items[i].ClassName]);
      Result.Add(FTypesList.Items[i].Create);
    end;
  end;
end;

function TPLCTypesCollection.getCollectionDescription(aCollection: TPLCDevicesCollection): String;
begin
  Result := '';

  if aCollection <> nil then
  begin
    if not FTypesDict.TryGetValue(TPLCDevicesCollectionClass(aCollection.ClassType), Result) then
      Result := '';
  end;
end;

function TPLCTypesCollection.getManagedPLCList(): TStringList;
var
  i : Integer;

begin
  Result := TStringList.Create();
  for i := 0 to FTypesList.Count - 1 do
    Result.Add(FTypesDict.Items[FTypesList.Items[i]]);
end;


{ TPLCCollector }

procedure TPLCCollector.DataValueChanged(Sender: TObject);
begin
  if Assigned(FOnDataValueChanged) then
    FOnDataValueChanged(TPLCBaseClient(Sender));
end;

procedure TPLCCollector.DeviceConnected(Sender: TObject);
begin
  if Assigned(FOnDeviceConnected) then
    FOnDeviceConnected(TPLCBaseClient(Sender));
end;

procedure TPLCCollector.DeviceDisconnected(Sender: TObject);
begin
  if Assigned(FOnDeviceDisconnected) then
    FOnDeviceDisconnected(TPLCBaseClient(Sender));
end;

constructor TPLCCollector.Create;
begin
  inherited Create;

  FEventsManager := TEventsManager.Create(Self);

  FPLCTypes := gv_PLCTypesCollection.createCollectionsList();

  FSortedMonitoredItems := TSortedMonitoredItems.Create;
end;

destructor TPLCCollector.Destroy;
begin
  try
    FreeAndNil(FEventsManager);

    FreeAndNil(FSortedMonitoredItems);

    FreeAndNil(FPLCTypes);

  except
    on E: Exception do
      LBLogger.Write(1, 'TPLCTypesCollection.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TPLCCollector.StartReading();
var
  i : Integer;
begin
  for i := 0 to FPLCTypes.Count - 1 do
    FPLCTypes[i].StartReading();
end;

function TPLCCollector.addClient(aClient: TPLCBaseClient): Boolean;
var
  i : Integer;

begin
  Result := False;

  for i := 0 to FPLCTypes.Count - 1 do
  begin
    if FPLCTypes.Items[i].isCompatibleClient(aClient) then
    begin
      aClient.OwnerCollection := FPLCTypes.Items[i];
      Result := True;
      Break;
    end;
  end;

  if not Result then
    LBLogger.Write(1, 'TPLCCollector.addClient', lmt_Warning, 'Unknown client type <%s>', [aClient.ClassName]);
end;

function TPLCCollector.getClientById(anId: Integer): TPLCBaseClient;
var
  i, d : Integer;
  _Devices : TPLCDevicesCollection;
  _Dev : TPLCBaseClient;

begin
  Result := nil;
  if anId > 0 then
  begin
    for i := 0 to FPLCTypes.Count - 1 do
    begin
      _Devices := FPLCTypes.Items[i];
      if _Devices.Count > 0 then
      begin
        for d := 0 to _Devices.Count - 1 do
        begin
          _Dev := _Devices.getClient(d);
          if _Dev.Id = anId then
          begin
            Result := _Dev;
            Break;
          end;
        end;
      end;
      if Result <> nil then
        Break;
    end;
  end;
end;

function TPLCCollector.loadFromXMLNode(aNode: TDOMNode): Boolean;
var
  i : Integer;
  _MainNode : TDOMNode;

begin
  Result := False;
  if aNode = nil then Exit;

  if aNode.NodeName = cPLCDevicesRootNodeName then
    _MainNode := aNode
  else
    _MainNode := aNode.FindNode(cPLCDevicesRootNodeName);

  if _MainNode <> nil then
  begin
    for i := 0 to FPLCTypes.Count - 1 do
    begin
      LBLogger.Write(5, 'TPLCCollector.loadFromXMLNode', lmt_Info, 'Loading devices <%s> ...', [FPLCTypes.Items[i].ClassName]);
      if FPLCTypes.Items[i].loadFromXMLNode(_MainNode) then
      begin
        if FPLCTypes.Items[i].Code = '' then
          FPLCTypes.Items[i].Code := gv_PLCTypesCollection.getCollectionDescription(FPLCTypes[i]);
      end
      else
        LBLogger.Write(1, 'TPLCCollector.loadFromXMLNode', lmt_Warning, 'Devices <%s> not loaded!', [FPLCTypes.Items[i].ClassName]);


      if (not Result) and (FPLCTypes.Items[i].Count > 0) then
        Result := True;
    end;

    if Result then
    begin
      for i := 0 to FPLCTypes.Count - 1 do
        FPLCTypes.Items[i].UpdateGlobalIds();

      for i := 0 to FPLCTypes.Count - 1 do
      begin
        FPLCTypes.Items[i].retrieveMissingIds();
        FPLCTypes.Items[i].AddEventListener(TPLCBaseClient.cEvent_NewValuesRead, @Self.DataValueChanged, FEventsManager);
        FPLCTypes.Items[i].AddEventListener(TPLCBaseClient.cEvent_DeviceConnected, @Self.DeviceConnected, FEventsManager);
        FPLCTypes.Items[i].AddEventListener(TPLCBaseClient.cEvent_DeviceDisconnected, @Self.DeviceDisconnected, FEventsManager);

        LBLogger.Write(5, 'TPLCCollector.loadFromXMLNode', lmt_Debug, 'Inserting monitored items of <%s>', [FPLCTypes.Items[i].Code]);

        FPLCTypes.Items[i].insertItemsIntoList(FSortedMonitoredItems);
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TPLCCollector.loadFromXMLNode', lmt_Warning, 'Node <%s> not found!', [cPLCDevicesRootNodeName]);

end;

function TPLCCollector.loadFromXMLFile(const aFilename: String): Boolean;
var
  _Doc : TXMLDocument = nil;

begin
  Result := OpenXMLFile(aFilename, _Doc);

  if Result then
    Result := Self.loadFromXMLNode(_Doc.DocumentElement);

  if _Doc <> nil then
    _Doc.Free;
end;

function TPLCCollector.saveAsXMLFile(const aFilename: String): Boolean;
var
  _Doc : TXMLDocument = nil;
  _RootNode : TDOMNode;

begin
  Result := False;

  if aFilename = '' then Exit;

  try

    _Doc := TXMLDocument.Create;
    _RootNode := _Doc.CreateElement(cPLCDevicesRootNodeName);
    _Doc.AppendChild(_RootNode);

    if Self.insertIntoXMLNode(_Doc, _RootNode) then
    begin
      WriteXMLFile(_Doc, aFilename);
      Result := True;
    end;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TPLCCollector.saveAsXMLFile', lmt_Error, 'Error: <%s>', [E.Message]);
      Result := False;
    end;
  end;

  if _Doc <> nil then
    _Doc.Free;
end;

function TPLCCollector.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
var
  i : Integer;

begin
  Result := False;

  for i := 0 to FPLCTypes.Count - 1 do
  begin
    if FPLCTypes.Items[i].Count > 0 then
    begin
      Result := FPLCTypes.Items[i].insertIntoXMLNode(aDoc, aParentNode);
      if not Result then
        Break;
    end;
  end;
end;

function TPLCCollector.asJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Self.fillJSONObject(Result);
end;

function TPLCCollector.fillJSONObject(anObject: TJSONObject): Boolean;
var
  i : Integer;
begin
  Result := False;

  if anObject <> nil then
  begin
    for i := 0 to FPLCTypes.Count - 1 do
      anObject.Add(gv_PLCTypesCollection.getCollectionDescription(FPLCTypes.Items[i]),
                   FPLCTypes.Items[i].asJSONArray('', '', ''));

    Result := True;
  end;
end;

function TPLCCollector.asJSONArray(): TJSONArray;
begin
  Result := TJSONArray.Create;
  if not Self.fillJSONArray(Result) then
    FreeAndNil(Result);
end;

function TPLCCollector.fillJSONArray(anArray: TJSONArray): Boolean;
var
  _Obj : TJSONObject;
  i : Integer;

begin
  Result := False;

  if anArray <> nil then
  begin

    for i := 0 to FPLCTypes.Count - 1 do
    begin
      try
        _Obj := TJSONObject.Create;
        _Obj.Add('id', FPLCTypes.Items[i].Id);
        _Obj.Add('name', FPLCTypes.Items[i].Code);
        _Obj.Add('_children', FPLCTypes.Items[i].asJSONArray('name', 'value', '_children'));

        anArray.Add(_Obj);

      except
        on E: Exception do
          LBLogger.Write(1, 'TPLCCollector.fillJSONArray', lmt_Error, 'Error adding type %d: %s', [i, E.Message]);
      end;
    end;

    Result := True;
    LBLogger.Write(1, 'TPLCCollector.fillJSONArray', lmt_Debug, 'PLCs: <%s>', [anArray.AsJSON]);
  end;
end;

procedure TPLCCollector.SortMonitoredItems();
var
  i : Integer;

begin
  for i := 0 to FPLCTypes.Count - 1 do
    FPLCTypes.Items[i].UpdateGlobalIds();

  for i := 0 to FPLCTypes.Count - 1 do
  begin
    FPLCTypes.Items[i].retrieveMissingIds();
    FPLCTypes.Items[i].insertItemsIntoList(FSortedMonitoredItems);
  end;
end;

finalization
  if gv_PLCTypesCollection <> nil then
    FreeAndNil(gv_PLCTypesCollection);
end.

