unit uModbusTCP;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Blcksock, fgl, uTimedoutCriticalSection, Contnrs,
  Laz2_DOM, fpjson, uPLCBaseStructures, uLBUtils;

const
  cModbusProtocolDataSize = 9;
  cModbus_RequestTypeIdx  = 7;
  cModbus_ReceivedDataLenIdx = 8;
  cModbusMaxPDU = 260;

type
  TEndianType = (etBigEndian, etLittleEndian);
  TWordOrder = (woNormal, woSwapped);

  TModbus_DataReceived = record
    data: PByte;
    size: Integer;
  end;
  pModbus_DataReceived = ^TModbus_DataReceived;

  { TModbusTCPClient }

  TModbusTCPClient = class(TObject)
  strict private
    FSock: TTCPBlockSocket;
    FHost: string;
    FPort: string;
    FEndianType : TEndianType;
    FWordOrder : TWordOrder;

    FConnected : Boolean;

    FTransactionID : Word;


    FRequest    : array[0..cModbusMaxPDU-1] of Byte;
    FRXBuffer   : array[0..cModbusMaxPDU-1] of Byte;
    FDataBuffer : array[0..cModbusMaxPDU-1] of Byte;
    FAnswer: pModbus_DataReceived;

    function sendRequest(aRequestSize: Integer; anExpectedAnswerSize: Integer): Boolean;
    function sendWriteRequest(aRequestSize: Integer): Boolean;
    procedure set_Connected(AValue: Boolean);
    function ValidateResponse(ExpectedDataSize: Integer; aCheckDataLen: Boolean = True): Boolean;
    function MergeWordsToFloat(W1, W2: Word): Single;
    procedure SplitFloatToWords(Value: Single; out W1, W2: Word);
    procedure SwapWords(var A, B: Word);

    procedure FlushRXBuffer();

  private
    function connect(): Boolean;

  public
    constructor Create(AHost: string; APort: string);
    destructor Destroy; override;

    function ReadHoldingRegisters(UnitID: Byte; Address, Quantity: Word): Boolean;
    function ReadInputRegisters(UnitID: Byte; Address, Quantity: Word): Boolean;
    function WriteSingleRegister(UnitID: Byte; Address: Word; Value: Word): Boolean;
    function WriteMultipleRegisters(UnitID: Byte; Address: Word; const Values: array of Word): Boolean;

    function ReadFloat(UnitID: Byte; Address: Word): Single;
    function WriteFloat(UnitID: Byte; Address: Word; Value: Single): Boolean;

    function ReadCoils(UnitID: Byte; Address, Quantity: Word): Boolean;
    function ReadDiscreteInputs(UnitID: Byte; Address, Quantity: Word): Boolean;

    function WriteCoil(UnitID: Byte; Address: Word; Value: Boolean): Boolean;
    function WriteMultipleCoils(UnitID: Byte; Address: Word; const Values: array of Boolean): Boolean;

    function ReadString(UnitID: Byte; Address, Length: Word): string;
    function WriteString(UnitID: Byte; Address: Word; const S: string): Boolean;

    function GetWord(Index: Integer): Word;
    function GetFloat(Index: Integer): Single;
    function GetString(Index, Length: Integer): string;

    property Connected: Boolean read FConnected write set_Connected;

    property DataReceived: pModbus_DataReceived read FAnswer;
    property Endian: TEndianType read FEndianType write FEndianType;
    property WordOrder: TWordOrder read FWordOrder write FWordOrder;
  end;

  TModbusRegisterType = (
    regUnknown = 0,
    regHoldingRegister,
    regInputRegister,
    regCoils,
    regDiscreteInput
  );

  TModbusReqType = (
    rtUnknown = 0,
    rtHR,    // Holding Register (Word)
    rtHRI,   // Holding Register Integer (2 registri)
    rtHRUI,  // Holding Register Unsigned Integer (2 registri)
    rtHRF,   // Holding Register Float (2 registri)
    rtHRS,   // Holding Register String
    rtIR,    // Input Register (Word)
    rtIRI,   // Input Register Integer
    rtIRUI,  // Input Register Unsigned Integer
    rtIRF,   // Input Register Float
    rtCO,    // Coil
    rtDI     // Discrete Input
  );

  TModbus_Client = class;

  { TModbusItem }

  TModbusItem = class(TBasePLCItem)
  private
    FUnitID: Byte;
    FReqType: TModbusReqType;
    FStartAddr: Word;
    FQuantity: Word;
    FByteAmount: Word;
    FLastChange: TDateTime;
    FOutput: TBytes;

    function GetModbusClient: TModbus_Client;
    // Helper per endianness
    function ClientEndian: TEndianType;
    function ClientWordOrder: TWordOrder;
    // Conversioni interne
    function ValueAsWordInternal: Word;
    function ValueAsIntegerInternal: Integer;
    function ValueAsCardinalInternal: Cardinal;
    function ValueAsFloatInternal: Single;
    function ValueAsStringInternal: string;
    procedure InternalSplitFloatToWords(Value: Single; out W1, W2: Word);
    function InternalMergeWordsToFloat(W1, W2: Word): Single;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ValueAsVariant: Variant; override;
    function ValueAsString: string; override;

    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteWord(Value: Word); override;
    procedure WriteFloat(Value: Single); override;

    property UnitID: Byte read FUnitID write FUnitID;
    property ReqType: TModbusReqType read FReqType write FReqType;
    property StartAddr: Word read FStartAddr write FStartAddr;
    property Quantity: Word read FQuantity write FQuantity;
    property ByteAmount: Word read FByteAmount write FByteAmount;
    property LastChange: TDateTime read FLastChange write FLastChange;
    property Output: TBytes read FOutput;

    function GetRegisterType: TModbusRegisterType;
    function BuildAddress: string;
    function fillFromString(const anItemAddress: string): Boolean;
    function asJSONObject(const codeRef, valueRef: string): TJSONObject;
    procedure insertValueIntoJSObject(anObj: TJSONObject; aFieldName: string);
    function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
    function loadFromXMLNode(aParentNode: TDOMNode): Boolean;
  end;

  TModbusItemsList = specialize TFPGObjectList<TModbusItem>;

  { TModbusWriteFCType }
  TModbusWriteFCType = (
    mwfc_Unknown         = 0,
    mwfc_WriteSingleCoil = 5,
    mwfc_WriteSingleReg  = 6,
    mwfc_WriteMultiRegs  = 16,
    mwfc_WriteMultiCoils = 15
  );

  { TModbus_WriteCmd }
  TModbus_WriteCmd = record
    UnitID   : Byte;
    FC       : TModbusWriteFCType;
    Address  : Word;
    Values   : TBytes;
  end;
  PModbus_WriteCmd = ^TModbus_WriteCmd;

  TModbus_WriteCmdList = specialize TFPGList<PModbus_WriteCmd>;

  { TModbus_Client }
  TModbus_Client = class(TPLCBaseClient)
  strict private
    type
      TInternalState = (is_Connecting,
                        is_SendCommand,
                        is_ReceiveData,
                        is_ResetConnection);

    var
      FConnected         : Boolean;
      FModbusClient      : TModbusTCPClient;
      FChangedOnLastRead : TModbusItemsList;
      FEndianType        : TEndianType;
      FWordOrder         : TWordOrder;


    function createRequestLists(): TObjectList;
    function parseItemAddress(const anItemAddress: string; out ItemParams: TModbusItem): Boolean;

    function processWriteQueue: Boolean;
    function updateValues(): Boolean;

    function connectDevice(): Boolean;
    procedure disconnectDevice();

  strict protected
    FWriteQueue        : TModbus_WriteCmdList;
    FCSWriteQueue      : TTimedOutCriticalSection;
    FReconnectEveryTime : Boolean;

    procedure BeforeNotifingChanges(); virtual;

    function get_ChangedItem(anIdx: Integer): TBasePLCItem; override;
    function get_ChangedItemsCount: Integer; override;
    function get_ChangedItemValueAsDouble(anIdx: Integer): Double; override;

    function MonitoredItemsAsJSONArray(const codeRef, valueRef: string): TJSONArray; override;
    procedure clearItemsList(); override;
    function CloneItem(anItem: TBasePLCItem): Boolean; override;
    function CloneInternal(aClient: TPLCBaseClient): Boolean; override;
    function getValueAsVariant(aItem: TBasePLCItem): Variant; override;

    function QueueWriteCoil(UnitID: Byte; Address: Word; Value: Boolean): Boolean; overload;
    function QueueWriteSingleReg(UnitID: Byte; Address: Word; Value: Word): Boolean;
    function QueueWriteMultipleCoils(UnitID: Byte; Address: Word; const Values: array of Boolean): Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(aOwner: TPLCDevicesCollection); override;
    destructor  Destroy; override;

    function addMonitoredItem(const anItemCode, anItemAddress: string): Boolean;
    function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean; override;
    function loadFromXMLNode(aParentNode: TDOMNode): Boolean; override;

    procedure sortItems();

    function Connected(): Boolean; override;

    function QueueWriteCoil(anItem: TModbusItem; Value: Boolean): Boolean; overload; virtual;
    function QueueWriteSingleReg(anItem: TModbusItem; Value: Word): Boolean;
    function QueueWriteMultipleRegs(UnitID: Byte; Address: Word; const Values: array of Word): Boolean;


    property EndianType: TEndianType read FEndianType write FEndianType;
    property WordOrder: TWordOrder read FWordOrder write FWordOrder;

    property ChangedOnLastRead: TModbusItemsList read FChangedOnLastRead;

    const
      cCodeNodeName            = 'Code';
      cHostNodeName            = 'Host';
      cRefreshIntervalNodeName = 'Refresh';
      cItemsNodeName           = 'Items';
      cEndianTypeAttrib      = 'EndianType';
      cWordOrderAttrib       = 'WordOrder';
      cEndianType_BE = 'BE';
      cEndianType_LE = 'LE';
      cWordOrder_Normal = 'Normal';
      cWordOrder_Swap = 'Swap';
  end;

  TModbus_ClientsList = specialize TFPGList<TModbus_Client>;

  { TModbus_ClientsCollection }
  TModbus_ClientsCollection = class(TPLCDevicesCollection)
  public
    constructor Create; override;
    class function getRootNodeName(): string; override;
    const
      cModbusClientsRootNodeName = 'Modbus';
  end;

implementation

uses
  ULBLogger, uLBTimers, LazFileUtils, StrUtils, math, uPLCCollector;

const
  ModbusReqTypeNames: array[TModbusReqType] of string = (
    'UNKNOWN', 'HR', 'HRI', 'HRUI', 'HRF', 'HRS',
    'IR', 'IRI', 'IRUI', 'IRF', 'CO', 'DI'
  );

// --------------------------- TModbusTCPClient ---------------------------

constructor TModbusTCPClient.Create(AHost: string; APort: string);
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FSock := nil;

  New(FAnswer);
  FAnswer^.data := @FDataBuffer[0];
  FAnswer^.size := 0;

  FEndianType := etBigEndian;
  FWordOrder := woNormal;
  FConnected := False;
end;

destructor TModbusTCPClient.Destroy;
begin
  if FSock <> nil then FSock.Free;
  Dispose(FAnswer);
  inherited Destroy;
end;

function TModbusTCPClient.connect(): Boolean;
begin
  Result := False;
  FConnected := False;

  if FSock <> nil then FSock.Free;

  FSock := TTCPBlockSocket.Create;
  FSock.ConnectionTimeout := 3000;
  FSock.SetRecvTimeout(3000);
  FSock.SetSendTimeout(3000);
  FSock.Connect(FHost, FPort);

  if FSock.LastError = 0 then
  begin
    FConnected := True;
    FTransactionID := 0;
    Exit(True);
  end;

  LBLogger.Write(1, 'TModbusTCPClient.connect', lmt_Warning, 'Error connecting <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
  FreeAndNil(FSock);
end;

function TModbusTCPClient.sendRequest(aRequestSize: Integer; anExpectedAnswerSize: Integer): Boolean;
begin
  Result := False;

  if not FConnected then Exit;

  Self.FlushRXBuffer();

  Inc(FTransactionID);
  FRequest[0] := Hi(FTransactionID);
  FRequest[1] := Lo(FTransactionID);

  FSock.SendBuffer(@FRequest[0], aRequestSize);
  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendRequest', lmt_Warning, 'Error sending request to <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
    Exit;
  end;

  FSock.RecvBuffer(@FRXBuffer[0], anExpectedAnswerSize);

  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendRequest', lmt_Warning, 'Error reading answer from <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
//    LBLogger.Write(5, 'TModbusTCPClient.sendRequest', lmt_Debug, 'Request: %s', [HexString(@FRequest[0], aRequestSize)]);
    Exit;
  end;

  if not Self.ValidateResponse(anExpectedAnswerSize - cModbusProtocolDataSize) then
  begin
    Self.FlushRXBuffer();
    Exit;
  end;

  FAnswer^.size := anExpectedAnswerSize - cModbusProtocolDataSize;
  Move(FRXBuffer[cModbusProtocolDataSize], FDataBuffer[0], FAnswer^.size);
  Result := True;
end;

procedure TModbusTCPClient.set_Connected(AValue: Boolean);
begin
  if not aValue then
  begin
    FConnected := False;
    if FSock <> nil then FreeAndNil(FSock);
  end
  else if not FConnected then Self.connect();
end;

function TModbusTCPClient.sendWriteRequest(aRequestSize: Integer): Boolean;
const
  cWriteEchoSize = 12;  // FC05, FC06, FC15, FC16 rispondono sempre con 12 byte

(*
[TID_Hi][TID_Lo][0][0][0][6][UnitID][FC][Addr_Hi][Addr_Lo][Qty_Hi][Qty_Lo]
    0       1    2  3  4  5    6      7     8         9       10      11
= 12 byte fissi
*)

begin
  Result := False;
  if not FConnected then Exit;

  Self.FlushRXBuffer();

  Inc(FTransactionID);
  FRequest[0] := Hi(FTransactionID);
  FRequest[1] := Lo(FTransactionID);

  FSock.SendBuffer(@FRequest[0], aRequestSize);
  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendWriteRequest', lmt_Warning,
                      'Error sending request to <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
    Exit;
  end;

  // La risposta per FC05/FC06/FC15/FC16 è sempre 12 byte, indipendente dalla request
  FSock.RecvBuffer(@FRXBuffer[0], cWriteEchoSize);
  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendWriteRequest', lmt_Warning,
                      'Error reading answer from <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
    Self.FlushRXBuffer();
    Exit;
  end;

  Result := Self.ValidateResponse(0, False);
end;

(*
function TModbusTCPClient.sendWriteRequest(aRequestSize: Integer): Boolean;
var
  _EchoSize : Integer;
begin
  Result := False;
  if not FConnected then Exit;
  
  _EchoSize := aRequestSize;
  if FSock.WaitingData > 0 then
  begin
    LBLogger.Write(5, 'TModbusTCPClient.sendWriteRequest', lmt_Debug,
      'Data %d into socket buffer <%s:%s>  -  Cleaning', [FSock.WaitingData, FHost, FPort]);
    FSock.RecvBuffer(@FDataReceived[0], FSock.WaitingData);
  end;

  Inc(FTransactionID);
  FRequest[0] := Hi(FTransactionID);
  FRequest[1] := Lo(FTransactionID);
    
  FSock.SendBuffer(@FRequest[0], aRequestSize);
  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendWriteRequest', lmt_Warning,
      'Error sending request to <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
    Exit;
  end;
  FSock.RecvBuffer(@FDataReceived[0], _EchoSize);
  if FSock.LastError <> 0 then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.sendWriteRequest', lmt_Warning,
      'Error reading answer from <%s:%s>: %s', [FHost, FPort, FSock.LastErrorDesc]);
    Exit;
  end;
  Result := Self.ValidateResponse(0, False);
end;
*)

function TModbusTCPClient.ValidateResponse(ExpectedDataSize: Integer; aCheckDataLen: Boolean = True): Boolean;
begin
  Result := False;
  if FRXBuffer[cModbus_RequestTypeIdx] = (FRequest[cModbus_RequestTypeIdx] or $80) then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.ValidateResponse', lmt_Warning, 'Error state received from <%s:%s>', [FHost, FPort]);
    Exit;
  end;

  if (FRXBuffer[0] <> FRequest[0]) or (FRXBuffer[1] <> FRequest[1]) then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.ValidateResponse', lmt_Warning,
                      'Transaction ID mismatch  -  Request[0]=%d Received[0]=%d  -  Request[1]=%d Received[1]=%d',
                      [FRequest[0], FRXBuffer[0], FRequest[1], FRXBuffer[1]]);
    Exit;
  end;

  if FRXBuffer[cModbus_RequestTypeIdx] <> FRequest[cModbus_RequestTypeIdx] then
  begin
    LBLogger.Write(1, 'TModbusTCPClient.ValidateResponse', lmt_Warning, 'Wrong answer from <%s:%s>  -  Request %d  -  Answer %d', [FHost, FPort, FRequest[cModbus_RequestTypeIdx], FRXBuffer[cModbus_RequestTypeIdx]]);
    Exit;
  end;
  if aCheckDataLen then
  begin
    Result := FRXBuffer[cModbus_ReceivedDataLenIdx] = ExpectedDataSize;
    if not Result then
      LBLogger.Write(1, 'TModbusTCPClient.ValidateResponse', lmt_Warning,
        'Expected %d bytes from <%s:%s>, received %d', [ExpectedDataSize, FHost, FPort, FRXBuffer[cModbus_ReceivedDataLenIdx]]);
  end
  else Result := True;
end;

function TModbusTCPClient.ReadHoldingRegisters(UnitID: Byte; Address, Quantity: Word): Boolean;
begin
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $03;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Quantity); FRequest[11] := Lo(Quantity);
  Result := Self.sendRequest(12, cModbusProtocolDataSize + Quantity * 2);
end;

function TModbusTCPClient.ReadInputRegisters(UnitID: Byte; Address, Quantity: Word): Boolean;
begin
  if not FConnected then Exit(False);
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $04;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Quantity); FRequest[11] := Lo(Quantity);
  Result := Self.sendRequest(12, cModbusProtocolDataSize + Quantity * 2);
end;

function TModbusTCPClient.WriteSingleRegister(UnitID: Byte; Address: Word; Value: Word): Boolean;
begin
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $06;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Value); FRequest[11] := Lo(Value);
  Result := Self.sendWriteRequest(12);
end;

function TModbusTCPClient.WriteMultipleRegisters(UnitID: Byte; Address: Word; const Values: array of Word): Boolean;
var
  i, Len: Integer;
begin
  Len := Length(Values);
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 7 + Len * 2;
  FRequest[6] := UnitID;
  FRequest[7] := $10;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Len); FRequest[11] := Lo(Len);
  FRequest[12] := Len * 2;
  for i := 0 to Len - 1 do
  begin
    FRequest[13 + i * 2] := Hi(Values[i]);
    FRequest[14 + i * 2] := Lo(Values[i]);
  end;
  Result := Self.sendWriteRequest(13 + (Len * 2));
end;

function TModbusTCPClient.ReadFloat(UnitID: Byte; Address: Word): Single;
var
  W1, W2: Word;
begin
  Result := 0.0;
  if not ReadHoldingRegisters(UnitID, Address, 2) then Exit;
  W1 := GetWord(0); W2 := GetWord(1);
  Result := MergeWordsToFloat(W1, W2);
end;

function TModbusTCPClient.WriteFloat(UnitID: Byte; Address: Word; Value: Single): Boolean;
var
  W1, W2: Word;
begin
  SplitFloatToWords(Value, W1, W2);
  Result := WriteMultipleRegisters(UnitID, Address, [W1, W2]);
end;

function TModbusTCPClient.MergeWordsToFloat(W1, W2: Word): Single;
var
  _Bytes: array[0..3] of Byte;
begin
  Result := 0;
  if FWordOrder = woSwapped then Self.SwapWords(W1, W2);
  if FEndianType = etBigEndian then
  begin
    _Bytes[0] := Hi(W1); _Bytes[1] := Lo(W1);
    _Bytes[2] := Hi(W2); _Bytes[3] := Lo(W2);
  end
  else
  begin
    _Bytes[0] := Lo(W1); _Bytes[1] := Hi(W1);
    _Bytes[2] := Lo(W2); _Bytes[3] := Hi(W2);
  end;
  Move(_Bytes[0], Result, 4);
end;

procedure TModbusTCPClient.SplitFloatToWords(Value: Single; out W1, W2: Word);
var
  Bytes: array[0..3] of Byte;
begin
  Move(Value, Bytes[0], 4);
  if FEndianType = etBigEndian then
  begin
    W1 := (Bytes[0] shl 8) or Bytes[1];
    W2 := (Bytes[2] shl 8) or Bytes[3];
  end
  else
  begin
    W1 := (Bytes[1] shl 8) or Bytes[0];
    W2 := (Bytes[3] shl 8) or Bytes[2];
  end;
  if FWordOrder = woSwapped then Self.SwapWords(W1, W2);
end;

procedure TModbusTCPClient.SwapWords(var A, B: Word);
var
  _tmp: Word;
begin
  _tmp := A; A := B; B := _tmp;
end;

procedure TModbusTCPClient.FlushRXBuffer();
begin
  if (FSock = nil) or (not FConnected) then Exit;

  while FSock.WaitingData > 0 do
    FSock.RecvBuffer(@FRXBuffer[0], Min(FSock.WaitingData, Length(FRXBuffer)));
end;

function TModbusTCPClient.ReadCoils(UnitID: Byte; Address, Quantity: Word): Boolean;
var
  ByteCount: Integer;
begin
  Result := False;
  if not FConnected then Exit;
  ByteCount := (Quantity + 7) div 8;
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $01;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Quantity); FRequest[11] := Lo(Quantity);
  Result := Self.sendRequest(12, cModbusProtocolDataSize + ByteCount);
end;

function TModbusTCPClient.ReadDiscreteInputs(UnitID: Byte; Address, Quantity: Word): Boolean;
var
  ByteCount: Integer;
begin
  Result := False;
  if not FConnected then Exit;
  ByteCount := (Quantity + 7) div 8;
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $02;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(Quantity); FRequest[11] := Lo(Quantity);
  Result := Self.sendRequest(12, cModbusProtocolDataSize + ByteCount);
end;

function TModbusTCPClient.WriteCoil(UnitID: Byte; Address: Word; Value: Boolean): Boolean;
begin
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 6; FRequest[6] := UnitID;
  FRequest[7] := $05;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  if Value then begin FRequest[10] := $FF; FRequest[11] := $00; end
  else          begin FRequest[10] := $00; FRequest[11] := $00; end;
  Result := Self.sendWriteRequest(12);
end;

function TModbusTCPClient.WriteMultipleCoils(UnitID: Byte; Address: Word; const Values: array of Boolean): Boolean;
var
  i, _Qty, _ByteCount : Integer;
  _CoilByte : Byte;
begin
  Result := False;
  _Qty := Length(Values);
  if _Qty = 0 then Exit;
  _ByteCount := (_Qty + 7) div 8;
  FRequest[0] := 0; FRequest[1] := 0; FRequest[2] := 0; FRequest[3] := 0;
  FRequest[4] := 0; FRequest[5] := 7 + _ByteCount;
  FRequest[6] := UnitID;
  FRequest[7] := $0F;
  FRequest[8] := Hi(Address); FRequest[9] := Lo(Address);
  FRequest[10] := Hi(_Qty);   FRequest[11] := Lo(_Qty);
  FRequest[12] := _ByteCount;
  for i := 0 to _ByteCount - 1 do
  begin
    _CoilByte := 0;
    if (i * 8 + 0 < _Qty) and Values[i * 8 + 0] then _CoilByte := _CoilByte or $01;
    if (i * 8 + 1 < _Qty) and Values[i * 8 + 1] then _CoilByte := _CoilByte or $02;
    if (i * 8 + 2 < _Qty) and Values[i * 8 + 2] then _CoilByte := _CoilByte or $04;
    if (i * 8 + 3 < _Qty) and Values[i * 8 + 3] then _CoilByte := _CoilByte or $08;
    if (i * 8 + 4 < _Qty) and Values[i * 8 + 4] then _CoilByte := _CoilByte or $10;
    if (i * 8 + 5 < _Qty) and Values[i * 8 + 5] then _CoilByte := _CoilByte or $20;
    if (i * 8 + 6 < _Qty) and Values[i * 8 + 6] then _CoilByte := _CoilByte or $40;
    if (i * 8 + 7 < _Qty) and Values[i * 8 + 7] then _CoilByte := _CoilByte or $80;
    FRequest[13 + i] := _CoilByte;
  end;
  Result := Self.sendWriteRequest(13 + _ByteCount);
end;

function TModbusTCPClient.ReadString(UnitID: Byte; Address, Length: Word): string;
begin
  Result := '';
  if not Self.ReadHoldingRegisters(UnitID, Address, (Length + 1) div 2) then Exit;
  Result := GetString(0, Length);
end;

function TModbusTCPClient.WriteString(UnitID: Byte; Address: Word; const S: string): Boolean;
var
  i, NumRegs: Integer;
  W: array of Word;
begin
  NumRegs := (Length(S) + 1) div 2;
  SetLength(W, NumRegs);
  for i := 0 to NumRegs - 1 do
  begin
    W[i] := Ord(S[i * 2 + 1]) shl 8;
    if i * 2 + 2 <= Length(S) then W[i] := W[i] or Ord(S[i * 2 + 2]);
  end;
  Result := WriteMultipleRegisters(UnitID, Address, W);
end;

function TModbusTCPClient.GetWord(Index: Integer): Word;
begin
  if FAnswer^.size < (Index + 1) * 2 then Exit(0);
  Result := (FAnswer^.data[Index * 2] shl 8) or FAnswer^.data[Index * 2 + 1];
end;

function TModbusTCPClient.GetFloat(Index: Integer): Single;
var
  W1, W2: Word;
begin
  W1 := GetWord(Index);
  W2 := GetWord(Index + 1);
  Result := MergeWordsToFloat(W1, W2);
end;

function TModbusTCPClient.GetString(Index, Length: Integer): string;
var
  i: Integer;
  Offset: Integer;
begin
  Result := '';
  Offset := Index * 2;
  for i := 0 to Length - 1 do
    if Offset + i < FAnswer^.size then
      Result := Result + Chr(FAnswer^.data[Offset + i]);
end;


// ============================= TModbusItem =============================

function TModbusItem.GetModbusClient: TModbus_Client;
begin
  Result := Self.OwnerClient as TModbus_Client;
end;

function TModbusItem.ClientEndian: TEndianType;
begin
  Result := GetModbusClient.EndianType;
end;

function TModbusItem.ClientWordOrder: TWordOrder;
begin
  Result := GetModbusClient.WordOrder;
end;

constructor TModbusItem.Create;
begin
  inherited;
  FUnitID := 0;
  FReqType := rtUnknown;
  FStartAddr := 0;
  FQuantity := 0;
  FByteAmount := 0;
  FLastChange := 0;
  SetLength(FOutput, 0);
end;

destructor TModbusItem.Destroy;
begin
  SetLength(FOutput, 0);
  inherited;
end;

function TModbusItem.ValueAsWordInternal: Word;
begin
  if Length(FOutput) < 2 then Exit(0);
  Result := PWord(@FOutput[0])^;
  case ClientEndian of
    etBigEndian:    Result := BEtoN(Result);
    etLittleEndian: Result := LEtoN(Result);
  end;
end;

function TModbusItem.ValueAsIntegerInternal: Integer;
begin
  Result := Integer(ValueAsCardinalInternal);
end;

function TModbusItem.ValueAsCardinalInternal: Cardinal;
var
  w1, w2: Word;
begin
  if Length(FOutput) >= 4 then
  begin
    w1 := PWord(@FOutput[0])^;
    w2 := PWord(@FOutput[2])^;
    case ClientEndian of
      etBigEndian:
        begin
          w1 := BEtoN(w1);
          w2 := BEtoN(w2);
        end;
      etLittleEndian:
        begin
          w1 := LEtoN(w1);
          w2 := LEtoN(w2);
        end;
    end;
    case ClientWordOrder of
      woNormal:  Result := (Cardinal(w1) shl 16) or w2;
      woSwapped: Result := (Cardinal(w2) shl 16) or w1;
    end;
  end
  else
    Result := 0;
end;

function TModbusItem.ValueAsFloatInternal: Single;
var
  raw: Cardinal;
begin
  raw := ValueAsCardinalInternal;
  Result := PSingle(@raw)^;
end;

function TModbusItem.ValueAsStringInternal: string;
var
  _tmpBuff: array[0..cModbusMaxPDU-1] of Byte;
begin
  if Length(FOutput) = 0 then Exit('');
  StrLCopy(@_tmpBuff[0], @FOutput[0], Length(FOutput));
  Result := StrPas(@_tmpBuff[0]);
end;

procedure TModbusItem.InternalSplitFloatToWords(Value: Single; out W1, W2: Word);
var
  Bytes: array[0..3] of Byte;
begin
  Move(Value, Bytes[0], 4);
  if ClientEndian = etBigEndian then
  begin
    W1 := (Bytes[0] shl 8) or Bytes[1];
    W2 := (Bytes[2] shl 8) or Bytes[3];
  end
  else
  begin
    W1 := (Bytes[1] shl 8) or Bytes[0];
    W2 := (Bytes[3] shl 8) or Bytes[2];
  end;
  if ClientWordOrder = woSwapped then
  begin
    // swap W1 e W2
    W1 := W1 xor W2;
    W2 := W1 xor W2;
    W1 := W1 xor W2;
  end;
end;

function TModbusItem.InternalMergeWordsToFloat(W1, W2: Word): Single;
var
  _Bytes: array[0..3] of Byte;
begin
  Result := 0;
  if ClientWordOrder = woSwapped then
  begin
    W1 := W1 xor W2;
    W2 := W1 xor W2;
    W1 := W1 xor W2;
  end;
  if ClientEndian = etBigEndian then
  begin
    _Bytes[0] := Hi(W1); _Bytes[1] := Lo(W1);
    _Bytes[2] := Hi(W2); _Bytes[3] := Lo(W2);
  end
  else
  begin
    _Bytes[0] := Lo(W1); _Bytes[1] := Hi(W1);
    _Bytes[2] := Lo(W2); _Bytes[3] := Hi(W2);
  end;
  Move(_Bytes[0], Result, 4);
end;

function TModbusItem.ValueAsVariant: Variant;
begin
  case FReqType of
    rtHR, rtIR     : Result := ValueAsWordInternal;
    rtHRI, rtIRI   : Result := ValueAsIntegerInternal;
    rtHRUI, rtIRUI : Result := ValueAsCardinalInternal;
    rtHRF, rtIRF   : Result := ValueAsFloatInternal;
    rtHRS          : Result := ValueAsStringInternal;
    rtCO, rtDI     : Result := IfThen((Length(FOutput) > 0) and (FOutput[0] <> 0), 1, 0);
    else             Result := Null;
  end;
end;

function TModbusItem.ValueAsString: string;
var
  _fs: TFormatSettings;
begin
  if Length(FOutput) = 0 then Exit('');
  case FReqType of
    rtHR, rtIR     : Result := IntToStr(ValueAsWordInternal);
    rtHRI, rtIRI   : Result := IntToStr(ValueAsIntegerInternal);
    rtHRUI, rtIRUI : Result := IntToStr(ValueAsCardinalInternal);
    rtHRF, rtIRF   : begin
                       _fs.ThousandSeparator := #0;
                       _fs.DecimalSeparator := '.';
                       Result := Format('%g', [ValueAsFloatInternal], _fs);
                     end;
    rtHRS          : Result := ValueAsStringInternal;
    rtCO, rtDI     : Result := BoolToStr((Length(FOutput) > 0) and (FOutput[0] <> 0), True);
    else             Result := '';
  end;
end;

procedure TModbusItem.WriteBoolean(Value: Boolean);
begin

  if FReqType = rtCO then
  begin
    if Self.OwnerClient <> nil then
      TModbus_Client(Self.OwnerClient).QueueWriteCoil(Self, Value) // FUnitID, FStartAddr, Value)
    else
      LBLogger.Write(1, 'TModbusItem.WriteBoolean', lmt_Warning, 'Owner client not found!');
  end
  else
    LBLogger.Write(1, 'TModbusItem.WriteBoolean', lmt_Warning, 'WriteBoolean not supported for this Modbus register');
end;

procedure TModbusItem.WriteWord(Value: Word);
begin
  if FReqType in [rtHR, rtHRI, rtHRUI, rtHRF, rtHRS] then
  begin
    if Self.OwnerClient <> nil then
      TModbus_Client(Self.OwnerClient).QueueWriteSingleReg(Self, Value)
    else
      LBLogger.Write(1, 'TModbusItem.WriteWord', lmt_Warning, 'Owner client not found!');
  end
  else
    LBLogger.Write(1, 'TModbusItem.WriteWord', lmt_Warning, 'WriteWord not supported for this Modbus register');
end;

procedure TModbusItem.WriteFloat(Value: Single);
var
  W1, W2: Word;
begin
  if FReqType = rtHRF then
  begin
    InternalSplitFloatToWords(Value, W1, W2);
    GetModbusClient.QueueWriteMultipleRegs(FUnitID, FStartAddr, [W1, W2]);
  end
  else
    raise ENotSupportedException.Create('WriteFloat non supportato per questo tipo di item');
end;

function TModbusItem.GetRegisterType: TModbusRegisterType;
begin
  case FReqType of
    rtHR, rtHRI, rtHRUI, rtHRF, rtHRS : Result := regHoldingRegister;
    rtIR, rtIRI, rtIRUI, rtIRF        : Result := regInputRegister;
    rtCO                               : Result := regCoils;
    rtDI                               : Result := regDiscreteInput;
    else                                 Result := regUnknown;
  end;
end;

function TModbusItem.BuildAddress: string;
begin
  Result := Format('%d.%s%d', [FUnitID, ModbusReqTypeNames[FReqType], FStartAddr]);
  if FReqType = rtHRS then
    if FQuantity > 1 then
      Result := Result + ':' + IntToStr(FQuantity);
end;

function TModbusItem.fillFromString(const anItemAddress: string): Boolean;
var
  _parts: TStringArray;
  _addrPart, _reqTypeStr, _numberStr: string;
  i, _posDigit: Integer;
  _registersPerValue: Integer;
  _logicalQuantity: Integer;

  function getRequestType(const aRequest: string): TModbusReqType;
  var
    j: TModbusReqType;
  begin
    Result := rtUnknown;
    for j in TModbusReqType do
      if ModbusReqTypeNames[j] = aRequest then Exit(j);
  end;

begin
  Result := False;

  _parts := anItemAddress.Split(['.']);
  if Length(_parts) <> 2 then Exit;

  FUnitID := StrToIntDef(_parts[0], 0);
  _addrPart := _parts[1];
  _posDigit := 1;

  while (_posDigit <= Length(_addrPart)) and not (_addrPart[_posDigit] in ['0'..'9']) do
    Inc(_posDigit);

  if _posDigit = 1 then Exit;

  _reqTypeStr := Copy(_addrPart, 1, _posDigit - 1);
  _numberStr := Copy(_addrPart, _posDigit, Length(_addrPart));
  FReqType := getRequestType(UpperCase(_reqTypeStr));

  if FReqType = rtUnknown then
  begin
    LBLogger.Write(1, 'TModbusItem.fillFromString', lmt_Warning, 'Unknown request type: <%s>', [anItemAddress]);
    Exit;
  end;

  _logicalQuantity := 1;
  i := Pos(':', _numberStr);
  if i > 0 then
  begin
    FStartAddr := StrToIntDef(Copy(_numberStr, 1, i - 1), 0);
    _logicalQuantity := StrToIntDef(Copy(_numberStr, i + 1, Length(_numberStr)), 1);
  end
  else
    FStartAddr := StrToIntDef(_numberStr, 0);

  case FReqType of
    rtHR, rtIR, rtCO, rtDI: _registersPerValue := 1;
    rtHRI, rtHRUI, rtIRI, rtIRUI, rtHRF, rtIRF: _registersPerValue := 2;
    else _registersPerValue := 1;
  end;

  FQuantity := _logicalQuantity * _registersPerValue;

  if FReqType in [rtCO, rtDI] then
    FByteAmount := (FQuantity + 7) div 8
  else
    FByteAmount := FQuantity * 2;

  SetLength(FOutput, FByteAmount);
  FillChar(FOutput[0], FByteAmount, 0);

  Result := FQuantity > 0;
end;

function TModbusItem.asJSONObject(const codeRef, valueRef: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.Add('id', Id);
    if codeRef = '' then Result.Add('Code', Code)
    else Result.Add(codeRef, Code);
    insertValueIntoJSObject(Result, IfThen(valueRef = '', cVariableValue, valueRef));
  except
    on E: Exception do
      LBLogger.Write(1, 'TModbusItem.asJSONObject', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

procedure TModbusItem.insertValueIntoJSObject(anObj: TJSONObject; aFieldName: string);
const
  cIOType = 'IO_Type';
begin
  try
    anObj.Add(cIOType, ModbusReqTypeNames[FReqType]);
    case FReqType of
      rtHR, rtIR     : anObj.Add(aFieldName, ValueAsWordInternal);
      rtHRI, rtIRI   : anObj.Add(aFieldName, ValueAsIntegerInternal);
      rtHRUI, rtIRUI : anObj.Add(aFieldName, ValueAsCardinalInternal);
      rtHRF, rtIRF   : anObj.Add(aFieldName, ValueAsFloatInternal);
      rtHRS          : anObj.Add(aFieldName, ValueAsStringInternal);
      rtCO, rtDI     : anObj.Add(aFieldName, (Length(FOutput) > 0) and (FOutput[0] <> 0));
      else             anObj.Add(aFieldName, '');
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TModbusItem.insertValueIntoJSObject', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

function TModbusItem.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
var
  _Node: TDOMElement;
begin
  Result := Assigned(aDoc) and Assigned(aParentNode);
  if not Result then Exit;
  _Node := aDoc.CreateElement('Item');
  _Node.SetAttribute('UnitID', IntToStr(FUnitID));
  _Node.SetAttribute('StartAddr', IntToStr(FStartAddr));
  _Node.SetAttribute('Quantity', IntToStr(FQuantity));
  _Node.SetAttribute('ByteAmount', IntToStr(FByteAmount));
  _Node.SetAttribute('Type', IntToStr(Integer(FReqType)));
  _Node.SetAttribute('Code', Code);
  _Node.SetAttribute('Address', BuildAddress());
  _Node.SetAttribute('Id', IntToStr(Id));
  aParentNode.AppendChild(_Node);
  Result := True;
end;

function TModbusItem.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
begin
  Result := False;
  if not Assigned(aParentNode) then Exit;
  if aParentNode.NodeType <> ELEMENT_NODE then Exit;

  try
    Id          := StrToIntDef(TDOMElement(aParentNode).GetAttribute('Id'), 0);
    Code        := TDOMElement(aParentNode).GetAttribute('Code');
    FUnitID     := StrToIntDef(TDOMElement(aParentNode).GetAttribute('UnitID'), 0);
    FStartAddr  := StrToIntDef(TDOMElement(aParentNode).GetAttribute('StartAddr'), 0);
    FQuantity   := StrToIntDef(TDOMElement(aParentNode).GetAttribute('Quantity'), 0);
    FByteAmount := StrToIntDef(TDOMElement(aParentNode).GetAttribute('ByteAmount'), 0);
    FReqType    := TModbusReqType(StrToIntDef(TDOMElement(aParentNode).GetAttribute('Type'), 0));
    if (FQuantity = 0) or (FReqType = rtUnknown) then
      Self.fillFromString(TDOMElement(aParentNode).GetAttribute('Address'))
    else begin
      if FByteAmount = 0 then
      begin
        if FReqType in [rtCO, rtDI] then
          FByteAmount := (FQuantity + 7) div 8
        else FByteAmount := FQuantity * 2;
      end;

      if Length(FOutput) <> FByteAmount then
        SetLength(FOutput, FByteAmount);

      FillChar(FOutput[0], Length(FOutput), 0);

    end;
    Result := (FQuantity > 0) and (FReqType <> rtUnknown);

//    LBLogger.Write(5, 'TModbusItem.loadFromXMLNode', lmt_Debug,
//                      'Item <%s>: UnitID=%d ReqType=%d StartAddr=%d Quantity=%d ByteAmount=%d - Value size: %d',
//                      [Code, FUnitID, Integer(FReqType), FStartAddr, FQuantity, FByteAmount, Length(FOutput)]);
  except
    on E: Exception do
      LBLogger.Write(1, 'TModbusItem.loadFromXMLNode', lmt_Error, 'Error: <%s>', [E.Message]);
  end;
end;

// ============================= TModbus_Client =============================

function TModbus_Client.createRequestLists(): TObjectList;
var
  _tmpList: TList = nil;
  _Item, _pOther: TModbusItem;
  _Grouped: TList;
  i, j: Integer;
const
  MaxGroupDistance = 40;
begin
  Result := TObjectList.Create(True);
  if FCSMonitoredItems.Acquire('TModbus_Client.createRequestLists') then
  begin
    try
      if FMonitoredItems.Count = 0 then Exit;
      _tmpList := TList.Create;
      _tmpList.Assign(FMonitoredItems);
      i := 0;
      while i < _tmpList.Count do
      begin
        _Item := TModbusItem(_tmpList[i]);
        _Grouped := TList.Create;
        _Grouped.Add(_Item);
        Result.Add(_Grouped);
        // if not (_Item.ReqType in [rtCO, rtDI]) then
        begin
          j := i + 1;
          while j < _tmpList.Count do
          begin
            _pOther := TModbusItem(_tmpList[j]);
            if (_Item.UnitID = _pOther.UnitID) and
               (_Item.ReqType = _pOther.ReqType) and
               (Abs(_Item.StartAddr - _pOther.StartAddr) < MaxGroupDistance) then
            begin
              _Grouped.Add(_pOther);
              _tmpList.Delete(j);
            end
            else Inc(j);
          end;
        end;
        Inc(i);
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TModbus_Client.createRequestLists', lmt_Error, E.Message);
    end;
    FCSMonitoredItems.Release;
  end;

  if _tmpList <> nil then
    _tmpList.Free;
end;

function TModbus_Client.parseItemAddress(const anItemAddress: string; out ItemParams: TModbusItem): Boolean;
begin
  ItemParams := TModbusItem.Create;
  Result := ItemParams.fillFromString(anItemAddress);
  if Result then
    ItemParams.OwnerClient := Self
  else
    FreeAndNil(ItemParams);
end;

function TModbus_Client.Connected: Boolean;
begin
  Result := (FModbusClient <> nil) and FModbusClient.Connected;
end;

function TModbus_Client.get_ChangedItem(anIdx: Integer): TBasePLCItem;
begin
  Result := FChangedOnLastRead[anIdx];
end;

function TModbus_Client.get_ChangedItemsCount: Integer;
begin
  Result := FChangedOnLastRead.Count;
end;

function TModbus_Client.get_ChangedItemValueAsDouble(anIdx: Integer): Double;
var
  _v: Variant;
begin
  _v := FChangedOnLastRead[anIdx].ValueAsVariant;
  Result := Double(_v);
end;

procedure TModbus_Client.clearItemsList;
begin
  if FCSMonitoredItems.Acquire('TModbus_Client.clearItemsList', 10000) then
  try
    FMonitoredItems.Clear;
  finally
    FCSMonitoredItems.Release;
  end;
end;

function TModbus_Client.CloneItem(anItem: TBasePLCItem): Boolean;
var
  _src: TModbusItem;
  _clone: TModbusItem;
begin
  Result := False;
  if anItem is TModbusItem then
  begin
    _src := TModbusItem(anItem);
    _clone := TModbusItem.Create;
    _clone.Id := _src.Id;
    _clone.Code := _src.Code;
    _clone.OwnerClient := Self;
    _clone.FUnitID := _src.FUnitID;
    _clone.FReqType := _src.FReqType;
    _clone.FStartAddr := _src.FStartAddr;
    _clone.FQuantity := _src.FQuantity;
    _clone.FByteAmount := _src.FByteAmount;
    _clone.FLastChange := _src.FLastChange;
    SetLength(_clone.FOutput, Length(_src.FOutput));
    if Length(_clone.FOutput) > 0 then
      Move(_src.FOutput[0], _clone.FOutput[0], Length(_src.FOutput));
    if FCSMonitoredItems.Acquire('TModbus_Client.CloneItem') then
    try
      FMonitoredItems.Add(_clone);
      Result := True;
    finally
      FCSMonitoredItems.Release;
    end;
    if not Result then _clone.Free;
  end;
end;

function TModbus_Client.CloneInternal(aClient: TPLCBaseClient): Boolean;
begin
  if aClient is TModbus_Client then
  begin
    FEndianType := TModbus_Client(aClient).EndianType;
    FWordOrder  := TModbus_Client(aClient).WordOrder;
    Result := True;
  end
  else Result := False;
end;

function TModbus_Client.getValueAsVariant(aItem: TBasePLCItem): Variant;
begin
  Result := (aItem as TModbusItem).ValueAsVariant;
end;

function TModbus_Client.MonitoredItemsAsJSONArray(const codeRef, valueRef: string): TJSONArray;
var
  i: Integer;
begin
  Result := TJSONArray.Create;
  if FCSMonitoredItems.Acquire('TModbus_Client.MonitoredItemsAsJSONArray') then
  try
    for i := 0 to FMonitoredItems.Count - 1 do
      Result.Add(TModbusItem(FMonitoredItems[i]).asJSONObject(codeRef, valueRef));
  finally
    FCSMonitoredItems.Release;
  end;
end;

function TModbus_Client.processWriteQueue(): Boolean;
var
  _Local  : TModbus_WriteCmdList = nil;
  _Cmd    : PModbus_WriteCmd;
  i, w, b : Integer;
  _ok     : Boolean;
  _words  : array of Word;
  _wCount : Integer;
  _bools  : array of Boolean;
  _bCount : Integer;
begin
  Result := False;

  if FModbusClient = nil then Exit;

  try
    if FCSWriteQueue.Acquire('TModbus_Client.processWriteQueue') then
    begin
      try
        _Local := FWriteQueue;
        FWriteQueue := TModbus_WriteCmdList.Create;
      finally
        FCSWriteQueue.Release;
      end;
    end;
    if _Local = nil then Exit;

    if _Local.Count > 0 then
    begin
      Result := True;
      // LBLogger.Write(5, 'TModbus_Client.processWriteQueue', lmt_Debug, '<%s> Command to elaborate %d', [FCode, _Local.Count]);
    end;

    for i := 0 to _Local.Count - 1 do
    begin
      _Cmd := _Local[i];
      if _Cmd = nil then Continue;

      case _Cmd^.FC of
        mwfc_WriteSingleCoil:
          begin
            _ok := FModbusClient.WriteCoil(_Cmd^.UnitID, _Cmd^.Address, _Cmd^.Values[0] <> 0);
            // LBLogger.Write(5, 'TModbus_Client.processWriteQueue', lmt_Debug, '<%s> FC05 UnitID=%d Addr=%d Val=%d -> %s', [FCode, _Cmd^.UnitID, _Cmd^.Address, _Cmd^.Values[0], IfThen(_ok, 'OK', 'FAIL')]);
          end;

        mwfc_WriteSingleReg:
          begin
            _ok := FModbusClient.WriteSingleRegister(_Cmd^.UnitID, _Cmd^.Address, (Word(_Cmd^.Values[0]) shl 8) or Word(_Cmd^.Values[1]));
            // LBLogger.Write(5, 'TModbus_Client.processWriteQueue', lmt_Debug, '<%s> FC06 UnitID=%d Addr=%d -> %s', [FCode, _Cmd^.UnitID, _Cmd^.Address, IfThen(_ok, 'OK', 'FAIL')]);
          end;

        mwfc_WriteMultiRegs:
          begin
            _wCount := Length(_Cmd^.Values) div 2;
            SetLength(_words, _wCount);
            for w := 0 to _wCount - 1 do
              _words[w] := (Word(_Cmd^.Values[w*2]) shl 8) or Word(_Cmd^.Values[w*2+1]);
            _ok := FModbusClient.WriteMultipleRegisters(_Cmd^.UnitID, _Cmd^.Address, _words);
            // LBLogger.Write(5, 'TModbus_Client.processWriteQueue', lmt_Debug, '<%s> FC16 UnitID=%d Addr=%d Count=%d -> %s', [FCode, _Cmd^.UnitID, _Cmd^.Address, _wCount, IfThen(_ok, 'OK', 'FAIL')]);
          end;

        mwfc_WriteMultiCoils:
          begin
            _bCount := Length(_Cmd^.Values);
            SetLength(_bools, _bCount);
            for b := 0 to _bCount - 1 do
              _bools[b] := _Cmd^.Values[b] <> 0;
            _ok := FModbusClient.WriteMultipleCoils(_Cmd^.UnitID, _Cmd^.Address, _bools);
            // LBLogger.Write(5, 'TModbus_Client.processWriteQueue', lmt_Debug, '<%s> FC15 UnitID=%d Addr=%d Count=%d -> %s', [FCode, _Cmd^.UnitID, _Cmd^.Address, _bCount, IfThen(_ok, 'OK', 'FAIL')]);
          end;

        else
          LBLogger.Write(1, 'TModbus_Client.processWriteQueue', lmt_Warning, '<%s> Unknown FC %d, command discarded', [FCode, Integer(_Cmd^.FC)]);
      end;
      SetLength(_Cmd^.Values, 0);
      Dispose(_Cmd);
    end;
  finally
    _Local.Free;
  end;
end;

function TModbus_Client.updateValues(): Boolean;
var
  _GroupedItems: TObjectList;
  _List: TList;
  i, j: Integer;
  _Item: TModbusItem;
  _StartAddr, _EndAddr, _TotalQty: Word;
  _ReadBuffer: pModbus_DataReceived;
  _UnitID: Byte;
  _ReqType: TModbusReqType;
  _Offset, _LengthInBytes, _ExpectedSize: Integer;
  _BitIdx, _ByteIdx, _BitMask, _BitVal: Byte;

begin
  Result := False;
  try
    _GroupedItems := Self.createRequestLists();
    FChangedOnLastRead.Clear;
    if _GroupedItems.Count > 0 then
    begin
      for i := 0 to _GroupedItems.Count - 1 do
      begin
        _List := TList(_GroupedItems[i]);
        if _List.Count = 0 then Continue;
        _StartAddr := High(Word); _EndAddr := 0;
        _UnitID := TModbusItem(_List[0]).UnitID;
        _ReqType := TModbusItem(_List[0]).ReqType;
        for j := 0 to _List.Count - 1 do
        begin
          _Item := TModbusItem(_List[j]);
          if _Item.StartAddr < _StartAddr then _StartAddr := _Item.StartAddr;
          if (_Item.StartAddr + _Item.Quantity) > _EndAddr then
            _EndAddr := _Item.StartAddr + _Item.Quantity;
        end;
        _TotalQty := _EndAddr - _StartAddr;
        _ReadBuffer := nil;

        case _ReqType of
          rtHR, rtHRI, rtHRUI, rtHRS:
            begin
              if FModbusClient.ReadHoldingRegisters(_UnitID, _StartAddr, _TotalQty) then
                _ReadBuffer := FModbusClient.DataReceived;
            end;

          rtIR, rtIRI, rtIRUI, rtIRF:
            begin
              if FModbusClient.ReadInputRegisters(_UnitID, _StartAddr, _TotalQty) then
                _ReadBuffer := FModbusClient.DataReceived;
            end;

          rtDI:
            begin
              if FModbusClient.ReadDiscreteInputs(_UnitID, _StartAddr, _TotalQty) then
                _ReadBuffer := FModbusClient.DataReceived;
            end;

          rtCO:
            begin
              if FModbusClient.ReadCoils(_UnitID, _StartAddr, _TotalQty) then
                _ReadBuffer := FModbusClient.DataReceived;
            end

          else
            Continue;
        end;

        if _ReadBuffer <> nil then
        begin
          if _ReqType in [rtCO, rtDI] then
            _ExpectedSize := (_TotalQty + 7) div 8
          else
            _ExpectedSize := _TotalQty * 2;

          if _ReadBuffer^.size = _ExpectedSize then
          begin
            Result := True;
            for j := 0 to _List.Count - 1 do
            begin
              _Item := TModbusItem(_List[j]);
              if _Item.ReqType in [rtCO, rtDI] then
              begin
                _BitIdx := _Item.StartAddr - _StartAddr;
                _ByteIdx := _BitIdx div 8;
                _BitMask := 1 shl (_BitIdx mod 8);

                if (_ReadBuffer^.data[_ByteIdx] and _BitMask) <> 0 then
                  _BitVal := 1
                else
                  _BitVal := 0;

                if (Length(_Item.FOutput) > 0) then
                begin
                  if (_Item.FOutput[0] <> _BitVal) then
                  begin
                    // LBLogger.Write(5, 'TModbus_Client.updateValues', lmt_Debug, 'Item <%s> changed to %d', [_Item.Code, _BitVal]);
                    _Item.FOutput[0] := _BitVal;
                    _Item.LastChange := NowUTC;
                    FChangedOnLastRead.Add(_Item);
                  end
                  else if _Item.LastChange = 0 then
                    _Item.LastChange := NowUTC;
                end;
              end
              else begin
                _Offset := (_Item.StartAddr - _StartAddr) * 2;
                _LengthInBytes := _Item.Quantity * 2;
                if (_Offset + _LengthInBytes) <= _ReadBuffer^.size then
                begin
                  if not CompareMem(@_ReadBuffer^.data[_Offset], @_Item.FOutput[0], _LengthInBytes) then
                  begin
                    Move(_ReadBuffer^.data[_Offset], _Item.FOutput[0], _LengthInBytes);
                    FChangedOnLastRead.Add(_Item);
                  end;
                end;
              end;
            end;
          end
          else
            LBLogger.Write(1, 'TModbus_Client.updateValues', lmt_Warning, '<%s> - Wrong buffer size! Expected: %d  -  Received: %d', [FCode, _ExpectedSize, _ReadBuffer^.size]);

        end;
      end;
      if Result and (FChangedOnLastRead.Count > 0) and (not Self.Terminated) then
      begin
        Self.BeforeNotifingChanges();
//        LBLogger.Write(5, 'TModbus_Client.updateValues', lmt_Debug, '<%s> - Updated values: %d', [FCode, FChangedOnLastRead.Count]);
        FEventsManager.RaiseEvent(cEvent_NewValuesRead);
      end;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TModbus_Client.updateValues', lmt_Error, E.Message);
  end;
  _GroupedItems.Free;
end;

function TModbus_Client.connectDevice(): Boolean;
begin
  Self.disconnectDevice;
  FModbusClient := TModbusTCPClient.Create(FHost, IntToStr(FPort));
  Result := FModbusClient.connect();
  if Result then
  begin
    FConnected := True;
    FEventsManager.RaiseEvent(cEvent_DeviceConnected);
    LBLogger.Write(5, 'TModbus_Client.connected', lmt_Debug, 'Host <%s><%s> connected', [FCode, FHost]);
  end;
end;

procedure TModbus_Client.disconnectDevice();
begin
  if FConnected then
    FEventsManager.RaiseEvent(cEvent_DeviceDisconnected);

  if FModbusClient <> nil then
    FreeAndNil(FModbusClient);

  FConnected := False;
end;

procedure TModbus_Client.BeforeNotifingChanges();
begin
  //
end;

function TModbus_Client.QueueWriteCoil(UnitID: Byte; Address: Word; Value: Boolean): Boolean;
var
  _Cmd: PModbus_WriteCmd = nil;
begin
  Result := False;
  New(_Cmd);
  _Cmd^.UnitID := UnitID;
  _Cmd^.FC := mwfc_WriteSingleCoil;
  _Cmd^.Address := Address;
  SetLength(_Cmd^.Values, 1);
  _Cmd^.Values[0] := Byte(IfThen(Value, $FF, $00));
  if FCSWriteQueue.Acquire('TModbus_Client.QueueWriteCoil') then
  try
    FWriteQueue.Add(_Cmd);
    _Cmd := nil;
    Result := True;
  finally
    FCSWriteQueue.Release;
  end;
  if _Cmd <> nil then
  begin
    SetLength(_Cmd^.Values, 0);
    Dispose(_Cmd);
  end;
end;

function TModbus_Client.QueueWriteCoil(anItem: TModbusItem; Value: Boolean): Boolean;
begin
  Result := False;
  
  if anItem <> nil then
    Result := QueueWriteCoil(anItem.UnitID, anItem.StartAddr, Value)
  else
    LBLogger.Write(1, 'TModbus_Client.QueueWriteCoil', lmt_Warning, 'No item set!');
end;

function TModbus_Client.QueueWriteSingleReg(anItem: TModbusItem; Value: Word): Boolean;
begin
  if anItem <> nil then
    Result := Self.QueueWriteSingleReg(anItem.UnitID, anItem.StartAddr, Value)
  else
    Result := False;
end;

function TModbus_Client.QueueWriteSingleReg(UnitID: Byte; Address: Word; Value: Word): Boolean;
var
  _Cmd: PModbus_WriteCmd;
begin
  Result := False;
  New(_Cmd);
  _Cmd^.UnitID := UnitID;
  _Cmd^.FC := mwfc_WriteSingleReg;
  _Cmd^.Address := Address;
  SetLength(_Cmd^.Values, 2);
  _Cmd^.Values[0] := Hi(Value);
  _Cmd^.Values[1] := Lo(Value);
  if FCSWriteQueue.Acquire('TModbus_Client.QueueWriteSingleReg') then
  begin
    try
      FWriteQueue.Add(_Cmd);
      Result := True;
    except
      on E: Exception do
      begin
        SetLength(_Cmd^.Values, 0);
        Dispose(_Cmd);
        LBLogger.Write(1, 'TModbus_Client.QueueWriteSingleReg', lmt_Error, '<%s> %s', [FCode, E.Message]);
      end;
    end;
    FCSWriteQueue.Release;
  end
  else begin
    SetLength(_Cmd^.Values, 0);
    Dispose(_Cmd);
  end;
end;

function TModbus_Client.QueueWriteMultipleRegs(UnitID: Byte; Address: Word; const Values: array of Word): Boolean;
var
  _Cmd: PModbus_WriteCmd;
  i: Integer;
begin
  Result := False;
  if Length(Values) = 0 then Exit;
  New(_Cmd);
  _Cmd^.UnitID := UnitID;
  _Cmd^.FC := mwfc_WriteMultiRegs;
  _Cmd^.Address := Address;
  SetLength(_Cmd^.Values, Length(Values) * 2);
  for i := 0 to Length(Values) - 1 do
  begin
    _Cmd^.Values[i * 2] := Hi(Values[i]);
    _Cmd^.Values[i * 2 + 1] := Lo(Values[i]);
  end;
  if FCSWriteQueue.Acquire('TModbus_Client.QueueWriteMultipleRegs') then
  begin
    try
      FWriteQueue.Add(_Cmd);
      Result := True;
    except
      on E: Exception do
      begin
        SetLength(_Cmd^.Values, 0);
        Dispose(_Cmd);
        LBLogger.Write(1, 'TModbus_Client.QueueWriteMultipleRegs', lmt_Error, '<%s> %s', [FCode, E.Message]);
      end;
    end;

    FCSWriteQueue.Release();
  end
  else begin
    SetLength(_Cmd^.Values, 0);
    Dispose(_Cmd);
  end;
end;

function TModbus_Client.QueueWriteMultipleCoils(UnitID: Byte; Address: Word; const Values: array of Boolean): Boolean;
var
  _Cmd: PModbus_WriteCmd;
  i: Integer;
begin
  Result := False;
  if Length(Values) = 0 then Exit;
  New(_Cmd);
  _Cmd^.UnitID := UnitID;
  _Cmd^.FC := mwfc_WriteMultiCoils;
  _Cmd^.Address := Address;
  SetLength(_Cmd^.Values, Length(Values));
  for i := 0 to Length(Values) - 1 do
    _Cmd^.Values[i] := Byte(IfThen(Values[i], 1, 0));
  if FCSWriteQueue.Acquire('TModbus_Client.QueueWriteMultipleCoils') then
  begin
    try
      FWriteQueue.Add(_Cmd);
      Result := True;
    except
      on E: Exception do
      begin
        SetLength(_Cmd^.Values, 0);
        Dispose(_Cmd);
        LBLogger.Write(1, 'TModbus_Client.QueueWriteMultipleCoils', lmt_Error, '<%s> %s', [FCode, E.Message]);
      end;
    end;
    FCSWriteQueue.Release();
  end
  else begin
    SetLength(_Cmd^.Values, 0);
    Dispose(_Cmd);
  end;
end;

procedure TModbus_Client.Execute;
var
  _InternalState: TInternalState = is_Connecting;
const
  cWaitBeforeReconnect = 5000;

begin
  Self.setThreadName('MB_' + RightStr(FHost, 7));

  LBLogger.Write(1, 'TModbus_Client.Execute', lmt_Debug, 'Modbus <%s> started <%s>', [FCode, FHost]);

  while not Self.Terminated do
  begin
    case _InternalState of
      is_Connecting:
        begin
          if Self.connectDevice() then _InternalState := is_SendCommand
          else Self.PauseFor(cWaitBeforeReconnect);
        end;

      is_SendCommand:
        begin
          if Self.processWriteQueue() then   // nel caso in cui vengano cambiati gli ingressi
            PreciseSleep(100);

          _InternalState := is_ReceiveData;
        end;

      is_ReceiveData:
        begin
          if Self.updateValues() then
          begin
            if FReconnectEveryTime then
            begin
              Self.disconnectDevice();
              _InternalState := is_Connecting;
            end
            else
              _InternalState := is_SendCommand;

            PreciseSleep(FRefreshInterval);
          end
          else _InternalState := is_ResetConnection;
        end;

      is_ResetConnection:
        begin
          LBLogger.Write(5, 'TModbus_Client.Execute', lmt_Debug, '<%s> - Resetting connection with host <%s:%d>', [FCode, FHost, FPort]);
          Self.disconnectDevice();
          Self.PauseFor(cWaitBeforeReconnect);
          _InternalState := is_Connecting;
        end;
    end;
  end;
  LBLogger.Write(5, 'TModbus_Client.Execute', lmt_Debug, '<%s> terminated', [FCode]);
end;

constructor TModbus_Client.Create(aOwner: TPLCDevicesCollection);
begin
  inherited Create(aOwner);

  FChangedOnLastRead := TModbusItemsList.Create(False);
  FRefreshInterval := 1000;
  FPort := 502;
  FConnected := False;
  FEndianType := etBigEndian;
  FWordOrder := woNormal;
  FWriteQueue := TModbus_WriteCmdList.Create;
  FCSWriteQueue := TTimedOutCriticalSection.Create;
  FReconnectEveryTime := False;
end;

destructor TModbus_Client.Destroy;
var
  i: Integer;
  _Cmd: PModbus_WriteCmd;
begin
  if Self <> nil then
  begin
    try
      FreeAndNil(FEventsManager);
      FreeAndNil(FModbusClient);
      FreeAndNil(FChangedOnLastRead);
      if FWriteQueue <> nil then
      begin
        for i := 0 to FWriteQueue.Count - 1 do
        begin
          _Cmd := FWriteQueue[i];
          if _Cmd <> nil then
          begin
            SetLength(_Cmd^.Values, 0);
            Dispose(_Cmd);
          end;
        end;
        FreeAndNil(FWriteQueue);
      end;
      FreeAndNil(FCSWriteQueue);
    except
      on E: Exception do
        LBLogger.Write(1, 'TModbus_Client.Destroy', lmt_Error, 'Error: <%s>', [E.Message]);
    end;
    inherited Destroy;
  end
  else
    LBLogger.Write(1, 'TModbus_Client.Destroy', lmt_Warning, 'I''m nil');
end;

function TModbus_Client.addMonitoredItem(const anItemCode, anItemAddress: string): Boolean;
var
  _Item: TModbusItem;
begin
  if not Self.parseItemAddress(anItemAddress, _Item) then Exit(False);
  _Item.Code := anItemCode;
  if FCSMonitoredItems.Acquire('TModbus_Client.addMonitoredItem') then
  try
    FMonitoredItems.Add(_Item);
    Result := True;
  finally
    FCSMonitoredItems.Release;
  end;
end;

function TModbus_Client.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
var
  _Items: TDOMNode;
  i: Integer;
begin
  Result := False;
  if FCSMonitoredItems.Acquire('TModbus_Client.insertIntoXMLNode') then
  try
    Result := inherited insertIntoXMLNode(aDoc, aParentNode);
    if Result then
    begin
      TDOMElement(aParentNode).SetAttribute(cEndianTypeAttrib, IfThen(FEndianType = etBigEndian, cEndianType_BE, cEndianType_LE));
      TDOMElement(aParentNode).SetAttribute(cWordOrderAttrib, IfThen(FWordOrder = woNormal, cWordOrder_Normal, cWordOrder_Swap));
      _Items := aDoc.CreateElement(cItemsNodeName);
      aParentNode.AppendChild(_Items);
      for i := 0 to FMonitoredItems.Count - 1 do
        if not TModbusItem(FMonitoredItems[i]).insertIntoXMLNode(aDoc, _Items) then
          Break;
    end;
  finally
    FCSMonitoredItems.Release;
  end;
end;

function TModbus_Client.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _Item: TModbusItem;
  _Node: TDOMNode;
  i: Integer;
  _str, _Addr: string;
  _Items: TStringList;
  _DisposeItem: Boolean;
begin
  Self.clearItemsList;
  Result := inherited loadFromXMLNode(aParentNode);
  if Result then
  begin
    _str := TDOMElement(aParentNode).GetAttribute(cEndianTypeAttrib);
    if _str = cEndianType_LE then FEndianType := etLittleEndian else FEndianType := etBigEndian;
    _str := TDOMElement(aParentNode).GetAttribute(cWordOrderAttrib);
    if _str = cWordOrder_Swap then FWordOrder := woSwapped else FWordOrder := woNormal;
    _Node := aParentNode.FindNode(cItemsNodeName);
    if _Node <> nil then
    begin
      if FPort <= 0 then
        FPort := 502;

      _Items := TStringList.Create;
      _Items.Sorted := True;
      _Items.CaseSensitive := True;
      try
        if not FCSMonitoredItems.Acquire('TModbus_Client.loadFromXMLNode') then Exit;
        try
          for i := 0 to _Node.ChildNodes.Count - 1 do
          begin
            _DisposeItem := True;
            _Item := TModbusItem.Create;
            if _Item.loadFromXMLNode(_Node.ChildNodes.Item[i]) then
            begin
              _Addr := _Item.BuildAddress;
              if _Items.IndexOf(_Addr) = -1 then
              begin
                _Item.OwnerClient := Self;
                FMonitoredItems.Add(_Item);
                _DisposeItem := False;
                _Items.Add(_Addr);
              end
              else
                LBLogger.Write(1, 'TModbus_Client.loadFromXMLNode', lmt_Warning, 'Address <%s> already used, item <%s> not added', [_Addr, _Item.Code]);
            end;
            if _DisposeItem then _Item.Free;
          end;
        finally
          FCSMonitoredItems.Release;
        end;
        Result := FMonitoredItems.Count > 0;
        LBLogger.Write(5, 'TModbus_Client.loadFromXMLNode', lmt_Debug, 'Client <%s> items available: %d', [FCode, FMonitoredItems.Count]);
      finally
        _Items.Free;
      end;
    end;
  end;
end;

function sortMonitoredItems(Item1, Item2: Pointer): Integer;
var
  _I1, _I2: TModbusItem;
begin
  _I1 := TModbusItem(Item1);
  _I2 := TModbusItem(Item2);
  Result := CompareValue(_I1.UnitID, _I2.UnitID);
  if Result <> 0 then Exit;
  Result := CompareValue(Integer(_I1.GetRegisterType), Integer(_I2.GetRegisterType));
  if Result <> 0 then Exit;
  Result := CompareValue(_I1.StartAddr, _I2.StartAddr);
end;

procedure TModbus_Client.sortItems();
begin
  if FCSMonitoredItems.Acquire('TModbus_Client.sortItems') then
  try
    FMonitoredItems.Sort(@sortMonitoredItems);
  finally
    FCSMonitoredItems.Release;
  end;
end;

{ TModbus_ClientsCollection }

function sortItemsByCode(const Item1, Item2: TPLCBaseClient): Integer;
begin
  Result := CompareStr(Item1.Code, Item2.Code);
end;

constructor TModbus_ClientsCollection.Create;
begin
  inherited Create;
  FItemClassType := TModbus_Client;
end;

class function TModbus_ClientsCollection.getRootNodeName(): string;
begin
  Result := cModbusClientsRootNodeName;
end;

initialization
  RegisterPLCCollection(TModbus_ClientsCollection, 'Modbus');

end.
