unit uCIEH10A;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

{ ============================================================================
  uCIEH10A.pas

  Supporto per il controllore remoto I/O Sollae Systems CIE-H10A.

  Il dispositivo espone via Modbus/TCP (porta 502, UnitID 1 di default):
    - 8 ingressi digitali  DI0..DI7  (FC02 Discrete Inputs,  addr 0..7)
    - 8 uscite  digitali   DO0..DO7  (FC01 Coil,             addr 8..15)

  Scrittura uscite:
    FC05 Write Single Coil    -> SetOutput(portIndex, True/False)
    FC15 Write Multiple Coils -> SetOutputs([v0,v1,...,v7])
  Entrambi delegano alla coda di TModbus_Client (QueueWriteCoil /
  QueueWriteMultipleCoils), quindi sono non-bloccanti e thread-safe.

  La connessione, il ciclo di lettura e la coda di scrittura sono
  interamente delegate a TModbus_Client (uModbusTCP). Questa unit aggiunge:
    - TCIEH10A_IOState     : snapshot tipizzato degli 8 DI e 8 DO
    - TCIEH10A_Client      : specializzazione con item standard e helper
    - TCIEH10A_Collection  : contenitore auto-registrato in TPLCCollector
  ============================================================================ }

interface

uses
  Classes, SysUtils, Laz2_DOM, fpjson,
  uTimedoutCriticalSection,
  uPLCBaseStructures,
  uModbusTCP;

const
  // Indirizzi Modbus default CIE-H10A
  cCIEH10A_DI_BaseAddr       = 0;
  cCIEH10A_DO_BaseAddr       = 8;
  cCIEH10A_PortCount         = 8;
  cCIEH10A_DefaultUnitID     = 1;
  cCIEH10A_DefaultModbusPort = 502;

  // Attributi XML aggiuntivi rispetto a TModbus_Client
  cCIEH10A_UnitIDAttrib = DOMString('UnitID');
  cCIEH10A_DIBaseAttrib = DOMString('DIBase');
  cCIEH10A_DOBaseAttrib = DOMString('DOBase');

  cCIEH10A_DI_Prefix = 'DI';
  cCIEH10A_DO_Prefix = 'DO';

type

  { TCIEH10A_IOState
    Snapshot degli ultimi valori letti per DI e DO.
    Ottenuto tramite TCIEH10A_Client.GetIOState(). }

  TCIEH10A_IOState = record
    DI      : array[0..cCIEH10A_PortCount - 1] of Boolean;
    DO_     : array[0..cCIEH10A_PortCount - 1] of Boolean;
    DIValid : Boolean;
    DOValid : Boolean;

    procedure Clear;
    function  DIAsByte: Byte;
    function  DOAsByte: Byte;
    function  DIToString: String;
    function  DOToString: String;
  end;

  pCIEH10A_IOState = ^TCIEH10A_IOState;

  { TCIEH10A_Client
    Estende TModbus_Client con:
      - Costruzione automatica degli item DI/DO standard al primo avvio
      - GetIOState: lettura tipizzata dello stato DI/DO aggiornato a ogni ciclo
      - SetOutput / SetOutputs: helper di scrittura che delegano alla coda
        ereditata da TModbus_Client
      - Attributi XML propri (UnitID, DIBase, DOBase)
  }

  TCIEH10A_Client = class(TModbus_Client)
  strict private
    FUnitID     : Byte;
    FDIBaseAddr : Word;
    FDOBaseAddr : Word;

    FPendingCoilCmd : PModbus_WriteCmd;

    FInputs : TList;   // di TModbusItem (riferimenti, non possiede)
    FOutputs : TList;  // di TModbusItem (riferimenti, non possiede)

  strict protected
    procedure BeforeNotifingChanges(); override;

  public
    constructor Create(aOwner: TPLCDevicesCollection); override;
    destructor Destroy; override;

    function QueueWriteCoil(anItem: TModbusItem; Value: Boolean): Boolean; override;

    { Copia thread-safe dell'ultimo stato I/O letto.
      Restituisce True se almeno un ciclo di lettura e' andato a buon fine. }
    function GetIOState(aState: pCIEH10A_IOState): Boolean;

    function SetOutput(anItem: TModbusItem; value: Boolean): Boolean;
    function SetOutputs(const items: array of TModbusItem; const values: array of Boolean): Boolean;

    function getOutput(anIdx: Integer): TModbusItem;

(*
    { Imposta un singolo relè (DO portIndex 0..7).
      Accoda FC05 Write Single Coil. Non bloccante. }
    function SetOutput(portIndex: Byte; value: Boolean): Boolean;

    { Imposta tutti gli 8 relè in un colpo solo.
      Accoda FC15 Write Multiple Coils. Non bloccante. }
    function SetOutputs(const values: array of Boolean): Boolean;
*)

    function insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean; override;
    function loadFromXMLNode(aParentNode: TDOMNode): Boolean; override;

    property UnitID     : Byte read FUnitID     write FUnitID;
    property DIBaseAddr : Word read FDIBaseAddr  write FDIBaseAddr;
    property DOBaseAddr : Word read FDOBaseAddr  write FDOBaseAddr;
  end;

  { TCIEH10A_Collection }

  TCIEH10A_Collection = class(TPLCDevicesCollection)
  public
    constructor Create; override;
    class function getRootNodeName(): String; override;

    const
      cRootNodeName = DOMString('CIEH10A_Devices');
  end;


implementation

uses
  ULBLogger, uPLCCollector, StrUtils, Math;

{ ============================================================================
  TCIEH10A_IOState
  ============================================================================ }

procedure TCIEH10A_IOState.Clear;
var
  i : Integer;
begin
  for i := 0 to cCIEH10A_PortCount - 1 do
  begin
    DI[i]  := False;
    DO_[i] := False;
  end;
  DIValid := False;
  DOValid := False;
end;

function TCIEH10A_IOState.DIAsByte: Byte;
var i: Integer;
begin
  Result := 0;
  for i := 0 to cCIEH10A_PortCount - 1 do
    if DI[i] then Result := Result or (1 shl i);
end;

function TCIEH10A_IOState.DOAsByte: Byte;
var i: Integer;
begin
  Result := 0;
  for i := 0 to cCIEH10A_PortCount - 1 do
    if DO_[i] then Result := Result or (1 shl i);
end;

function TCIEH10A_IOState.DIToString: String;
var i: Integer;
begin
  Result := 'DI:0b';
  for i := cCIEH10A_PortCount - 1 downto 0 do
    Result := Result + IfThen(DI[i], '1', '0');
end;

function TCIEH10A_IOState.DOToString: String;
var i: Integer;
begin
  Result := 'DO:0b';
  for i := cCIEH10A_PortCount - 1 downto 0 do
    Result := Result + IfThen(DO_[i], '1', '0');
end;

{ ============================================================================
  TCIEH10A_Client
  ============================================================================ }

procedure TCIEH10A_Client.BeforeNotifingChanges();
//var
//  _State : TCIEH10A_IOState;
begin
//  if Self.GetIOState(@_State) then
//    LBLogger.Write(5, 'TCIEH10A_Client.BeforeNotifingChanges', lmt_Debug, '<%s> - New state: %s  %s', [Self.Code, _State.DIToString, _State.DOToString]);
end;

constructor TCIEH10A_Client.Create(aOwner: TPLCDevicesCollection);
begin
  inherited Create(aOwner);

  FUnitID     := cCIEH10A_DefaultUnitID;
  FDIBaseAddr := cCIEH10A_DI_BaseAddr;
  FDOBaseAddr := cCIEH10A_DO_BaseAddr;
  FPort       := cCIEH10A_DefaultModbusPort;

  FInputs  := TList.Create;
  FOutputs := TList.Create;

  FPendingCoilCmd := nil;
//  FReconnectEveryTime := True;
end;

destructor TCIEH10A_Client.Destroy;
begin
  // FPendingCoilCmd è già in coda a FWriteQueue (ereditato) oppure nil:
  // in entrambi i casi non va liberato qui perché processWriteQueue
  // o il distruttore della classe base ci pensano.

  FreeAndNil(FInputs);
  FreeAndNil(FOutputs);

  inherited Destroy;
end;

{ Crea e accoda il comando FC15 iniziale copiando lo stato corrente di tutti
  gli FOutputs come valore di partenza per tutti gli 8 coil.
  Non più usata direttamente: la logica è inline in QueueWriteCoil. }
{ Override di QueueWriteCoil:
  - Prima chiamata: crea FPendingCoilCmd copiando lo stato corrente dei coil,
    lo accoda in FWriteQueue e salva il puntatore in FPendingCoilCmd.
  - Chiamate successive: aggiorna direttamente il bit in FPendingCoilCmd
    senza aggiungere nuovi comandi in coda.
  - Quando processWriteQueue svuota la coda, FPendingCoilCmd viene liberato
    insieme al comando → lo azzeriamo dopo l'esecuzione. }

function TCIEH10A_Client.QueueWriteCoil(anItem: TModbusItem; Value: Boolean): Boolean;
var
  _offset : Integer;
  _off_i  : Integer;
  _item   : TModbusItem;
  _Cmd    : PModbus_WriteCmd;
  i       : Integer;
begin
  Result := False;

  if (anItem.LastChange = 0) then
  begin
    LBLogger.Write(1, 'TCIEH10A_Client.QueueWriteCoil', lmt_Warning, '<%s> Write blocked on item <%s>: no read cycle completed yet', [FCode, anItem.Code]);
    Exit;
  end;

  if anItem.ReqType <> rtCO then
  begin
    LBLogger.Write(1, 'TCIEH10A_Client.QueueWriteCoil', lmt_Warning, '<%s> Item is not a Coil', [FCode]);
    Exit;
  end;

//  if FOutputs.Count < 8 then
//  begin
    Result := inherited QueueWriteCoil(anItem, Value);
    Exit;
//  end;

  if (anItem.StartAddr < FDOBaseAddr) or
     (anItem.StartAddr >= FDOBaseAddr + cCIEH10A_PortCount) then
  begin
    LBLogger.Write(1, 'TCIEH10A_Client.QueueWriteCoil', lmt_Warning, '<%s> Address %d out of DO range [%d..%d]',
                      [FCode, anItem.StartAddr, FDOBaseAddr, FDOBaseAddr + cCIEH10A_PortCount - 1]);
    Exit;
  end;


  // Blocca la scrittura finché non c'è stata almeno una lettura
  if FCSWriteQueue.Acquire('TCIEH10A_Client.QueueWriteCoil') then
  begin
    try

      _offset := anItem.StartAddr - FDOBaseAddr;  // 0..7

      if (FPendingCoilCmd <> nil) and (FWriteQueue.IndexOf(FPendingCoilCmd) < 0) then
        FPendingCoilCmd := nil;

      if FPendingCoilCmd = nil then
      begin
        New(_Cmd);
        _Cmd^.UnitID  := anItem.UnitID;
        _Cmd^.FC      := mwfc_WriteMultiCoils;
        _Cmd^.Address := FDOBaseAddr;
        SetLength(_Cmd^.Values, cCIEH10A_PortCount);

        // Copia lo stato attuale degli FOutputs usando l'indirizzo reale dell'item
        for i := 0 to FOutputs.Count - 1 do
        begin
          _item := TModbusItem(FOutputs[i]);
          _off_i := _item.StartAddr - FDOBaseAddr;
          if (_off_i >= 0) and (_off_i < cCIEH10A_PortCount) then
            _Cmd^.Values[_off_i] := Byte(IfThen(
              (Length(_item.Output) > 0) and (_item.Output[0] <> 0), 1, 0));
        end;

        _Cmd^.Values[_offset] := Byte(IfThen(Value, 1, 0));

        FWriteQueue.Add(_Cmd);
        FPendingCoilCmd := _Cmd;
      end
      else
        FPendingCoilCmd^.Values[_offset] := Byte(IfThen(Value, 1, 0));

      Result := True;
    finally
      FCSWriteQueue.Release;
    end;

  end;
end;


{ ============================================================================
  GetIOState
  ============================================================================ }

function TCIEH10A_Client.GetIOState(aState: pCIEH10A_IOState): Boolean;
var
  i    : Integer;
  _item : TModbusItem;

begin
  Result := False;

  if FCSMonitoredItems.Acquire('TCIEH10A_Client.GetIOState') then
  begin
    try

      for i := 0 to FInputs.Count - 1 do
      begin
        _item := TModbusItem(FInputs[i]);
        aState^.DI[i] := (Length(_item.Output) > 0) and (_item.Output[0] <> 0);
      end;

      for i := 0 to FOutputs.Count - 1 do
      begin
        _item := TModbusItem(FOutputs[i]);
        aState^.DO_[i] := (Length(_item.Output) > 0) and (_item.Output[0] <> 0);
      end;

      Result := True;

    finally
      FCSMonitoredItems.Release();
    end;
  end;
end;

{ ============================================================================
  SetOutput / SetOutputs
  Wrapper leggibili che delegano alla coda di TModbus_Client.
  ============================================================================ }

function TCIEH10A_Client.SetOutput(anItem: TModbusItem; value: Boolean): Boolean;
begin
  if anItem <> nil then
    Result := Self.QueueWriteCoil(anItem, value);

(*
  if portIndex >= cCIEH10A_PortCount then
  begin
    LBLogger.Write(1, 'TCIEH10A_Client.SetOutput', lmt_Warning,
      '<%s> portIndex %d out of range [0..%d]',
      [FCode, portIndex, cCIEH10A_PortCount - 1]);
    Exit(False);
  end;

  // Delega alla coda ereditata: FC05
  Result := Self.QueueWriteCoil(FUnitID, FDOBaseAddr + portIndex, value);
*)
end;

function TCIEH10A_Client.SetOutputs(const items: array of TModbusItem;
                                    const values: array of Boolean): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Length(items) <> Length(values) then
  begin
    LBLogger.Write(1, 'TCIEH10A_Client.SetOutputs', lmt_Warning, '<%s> items count %d <> values count %d', [FCode, Length(items), Length(values)]);
    Exit;
  end;

  for i := 0 to Length(items) - 1 do
    Result := Self.QueueWriteCoil(items[i], values[i]);
end;

function TCIEH10A_Client.getOutput(anIdx: Integer): TModbusItem;
begin
  Result := nil;

  if (anIdx >= 0) and (anIdx < FOutputs.Count) then
    Result := TModbusItem(FOutputs.Items[anIdx]);
end;

(*
function TCIEH10A_Client.SetOutputs(const items: array of TModbusItem; const values: array of Boolean): Boolean;
var
  i : Integer;
begin
  Result := False;

  // Passa per QueueWriteCoil così tutti i bit confluiscono nello stesso FC15
  for i := 0 to Min(Length(values), cCIEH10A_PortCount) - 1 do
    Result := Self.QueueWriteCoil(FUnitID, FDOBaseAddr + i, values[i]);
end;
*)

{ ============================================================================
  XML
  ============================================================================ }

function TCIEH10A_Client.insertIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;
begin
  Result := inherited insertIntoXMLNode(aDoc, aParentNode);
  if Result then
  begin
    TDOMElement(aParentNode).SetAttribute(cCIEH10A_UnitIDAttrib, IntToStr(FUnitID));
    TDOMElement(aParentNode).SetAttribute(cCIEH10A_DIBaseAttrib, IntToStr(FDIBaseAddr));
    TDOMElement(aParentNode).SetAttribute(cCIEH10A_DOBaseAttrib, IntToStr(FDOBaseAddr));
  end;
end;

function TCIEH10A_Client.loadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _str : String;
  _item : TModbusItem;
  i : Integer;

begin
  FInputs.Clear;
  FOutputs.Clear;

  Result := inherited loadFromXMLNode(aParentNode);

  if Result then
  begin
    _str    := TDOMElement(aParentNode).GetAttribute(cCIEH10A_UnitIDAttrib);
    FUnitID := Byte(StrToIntDef(_str, cCIEH10A_DefaultUnitID));

    _str        := TDOMElement(aParentNode).GetAttribute(cCIEH10A_DIBaseAttrib);
    FDIBaseAddr := Word(StrToIntDef(_str, cCIEH10A_DI_BaseAddr));

    _str        := TDOMElement(aParentNode).GetAttribute(cCIEH10A_DOBaseAttrib);
    FDOBaseAddr := Word(StrToIntDef(_str, cCIEH10A_DO_BaseAddr));

    for i := 0 to FMonitoredItems.Count - 1 do
    begin
      _item := TModbusItem(FMonitoredItems[i]);

      case _item.ReqType of
        rtDI : FInputs.Add(_item);
        rtCO : FOutputs.Add(_item);
      end;
    end;

//    LBLogger.Write(5, 'TCIEH10A_Client.loadFromXMLNode', lmt_Debug,
//      '<%s> UnitID=%d DI start addr=%d DO start addr=%d items=%d inputs=%d outputs=%d',
//      [FCode, FUnitID, FDIBaseAddr, FDOBaseAddr, FMonitoredItems.Count, FInputs.Count, FOutputs.Count]);
  end;
end;

{ ============================================================================
  TCIEH10A_Collection
  ============================================================================ }

constructor TCIEH10A_Collection.Create;
begin
  inherited Create;
  FItemClassType := TCIEH10A_Client;
end;

class function TCIEH10A_Collection.getRootNodeName(): String;
begin
  Result := cRootNodeName;
end;

initialization
  RegisterPLCCollection(TCIEH10A_Collection, 'CIEH10A');

end.
