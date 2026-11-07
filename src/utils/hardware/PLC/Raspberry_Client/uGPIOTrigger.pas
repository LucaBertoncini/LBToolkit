unit uGPIOTrigger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBUtils;

type
  TPullType = (pt_Unknown  = 0,
               pt_PullDown = 1,
               pt_PullUp   = 2);

(*
  TGPIO_Item = record
    Id    : Integer;
    Code  : String;
    Owner : TObject;
    Value : Byte;
  end;
  pGPIO_Item = ^TGPIO_Item;
*)

  { TGPIOEncoder }

  TGPIOEncoder = class(TObject)
    strict private
      FPIN_A: Byte;
      FPIN_B: Byte;
      FPullType: TPullType;
      FUserData: Pointer;
      function get_CounterValue: Int64;
      procedure set_OnValueChanged(AValue: TNotifyEvent);

    public
      procedure resetCounter();

      function Start(): Boolean;

      property Counter: Int64 read get_CounterValue;

      property PinA: Byte write FPIN_A;
      property PinB: Byte write FPIN_B;
      property PullType: TPullType write FPullType;
      property UserData: Pointer read FUserData write FUserData;

      property OnValueChanged: TNotifyEvent write set_OnValueChanged;
  end;

  { TGPIOTrigger }

  TGPIOTrigger = class(TLBBaseThread)
    strict private
      FChip: Pointer;
      FLine: Pointer;
      FDebounceMS: Cardinal;
      FLineNum : Byte;
      FLastEventTime: QWord;
      FUserData : Pointer;

      procedure DoValueChanged();
//      function get_GPIOItem: pGPIO_Item;
      
      procedure clearGPIO();

    strict protected
      FOnValueChanged : TNotifyEvent;
      FPullType : TPullType;
      FValue : Byte;

      function normalizeLevel(aLevel: Integer): Integer;
      function get_OnValueChanged: TNotifyEvent; virtual;
      procedure set_OnValueChanged(AValue: TNotifyEvent); virtual;

    protected
      procedure Execute; override;

    public
      constructor Create; override;
      destructor Destroy; override;
      

      function setGPIOItemBaseData(GPIONum: Byte; PullType: TPullType; aDebounceMicroSecs: Cardinal): Boolean;

      property UserData: Pointer read FUserData write FUserData;
      property Value: Byte read FValue;

      property OnValueChanged: TNotifyEvent read get_OnValueChanged write set_OnValueChanged;
  end;

  { TGPIOCounter }

  TGPIOCounter = class(TGPIOTrigger)
    strict private
      FCount : QWord;
      FOnCountChanged : TNotifyEvent;
      FCountEnable : Boolean;

      procedure ValueChanged(Sender: TObject);

    strict protected
      function get_OnValueChanged: TNotifyEvent; override;
      procedure set_OnValueChanged(AValue: TNotifyEvent); override;

    public
      constructor Create; override;

      procedure setStartingOffset(aValue: QWord);

      property Count: QWord read FCount;
  end;

  function getPullTypeEnum(const aPullType: String): TPullType;


const
  cPullType_Description : array [TPullType] of String = ('-', 'down', 'up');


implementation

uses
  ULBLogger, dynlibs,{$IFDEF Unix}Unix{$ELSE}ctypes{$ENDIF};



type
  Pgpiod_chip = Pointer;
  Pgpiod_line = Pointer;

var
  gpiod_chip_open_by_name: function(name: PChar): Pgpiod_chip; cdecl;
  gpiod_chip_close: procedure(chip: Pgpiod_chip); cdecl;
  gpiod_chip_get_line: function(chip: Pgpiod_chip; offset: CInt): Pgpiod_line; cdecl;
  gpiod_line_request_input: function(line: Pgpiod_line; consumer: PChar): CInt; cdecl;
  gpiod_line_get_value: function(line: Pgpiod_line): CInt; cdecl;
  gpiod_line_release: procedure(line: Pgpiod_line); cdecl;

  hGpiod: TLibHandle = 0;

const
  cGPIOdLibname = String('libgpiod.so.2.2.2');
  cGPIOChipName = String('gpiochip0');

procedure FinalizeGPIODLibrary;
begin
  if hGpiod <> 0 then
  begin
    FreeLibrary(hGpiod);
    hGpiod := 0;
  end;
end;

function InitGPIODLibrary: Boolean;
begin
  Result := hGpiod <> 0;

  if not Result then
  begin

    hGpiod := LoadLibrary(cGPIOdLibname);

    if hGpiod = 0 then
    begin
      LBLogger.Write(1, 'InitGPIODLibrary', lmt_Warning, 'Library <%s> not loaded!', [cGPIOdLibname]);
      Exit;
    end;

    Pointer(gpiod_chip_open_by_name) := GetProcAddress(hGpiod, 'gpiod_chip_open_by_name');
    Pointer(gpiod_chip_close) := GetProcAddress(hGpiod, 'gpiod_chip_close');
    Pointer(gpiod_chip_get_line) := GetProcAddress(hGpiod, 'gpiod_chip_get_line');
    Pointer(gpiod_line_request_input) := GetProcAddress(hGpiod, 'gpiod_line_request_input');
    Pointer(gpiod_line_get_value) := GetProcAddress(hGpiod, 'gpiod_line_get_value');
    Pointer(gpiod_line_release) := GetProcAddress(hGpiod, 'gpiod_line_release');

    Result := Assigned(gpiod_chip_open_by_name)
          and Assigned(gpiod_chip_close)
          and Assigned(gpiod_chip_get_line)
          and Assigned(gpiod_line_request_input)
          and Assigned(gpiod_line_get_value)
          and Assigned(gpiod_line_release);

    if not Result then
    begin
      LBLogger.Write(1, 'InitGPIODLibrary', lmt_Warning, 'Functions not completely loaded from library <%s>!', [cGPIOdLibname]);
      FinalizeGPIODLibrary();
    end;
  end;
end;

function getPullTypeEnum(const aPullType: String): TPullType;
var
  _PullType : String;

begin
  _PullType := LowerCase(aPullType);
  if _PullType = cPullType_Description[pt_PullDown] then
    Result := pt_PullDown
  else if _PullType = cPullType_Description[pt_PullUp] then
    Result := pt_PullUp
  else
    Result := pt_Unknown;
end;



{ TGPIOEncoder }

procedure TGPIOEncoder.set_OnValueChanged(AValue: TNotifyEvent);
begin
  //
end;

function TGPIOEncoder.get_CounterValue: Int64;
begin
  Result := 0;
  LBLogger.Write(1, 'TGPIOEncoder.get_CounterValue', lmt_Warning, 'Invalid encoder!');
end;

procedure TGPIOEncoder.resetCounter();
begin
  LBLogger.Write(1, 'TGPIOEncoder.resetCounter', lmt_Warning, 'Invalid encoder!');
end;

function TGPIOEncoder.Start(): Boolean;
begin
  Result := False;
  LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Invalid encoder!');
end;




{ TGPIOTrigger }

(*
function TGPIOTrigger.get_GPIOItem: pGPIO_Item;
begin
  Result := @FGPIO_Item;
end;
*)

destructor TGPIOTrigger.Destroy;
begin
  Self.clearGPIO();
  
  inherited Destroy;
end;

procedure TGPIOTrigger.Execute;
var
  _Now : QWord;
  _Value : Integer;
  _PreviousValue : Integer = 0;

begin
  while not Self.Terminated do
  begin

    _Value := Self.normalizeLevel(gpiod_line_get_value(Fline));
    if ((_Value = 1) and (_PreviousValue = 0)) or
       ((_Value = 0) and (_PreviousValue = 1)) then
    begin
      FValue := _Value;
      _Now := GetTickCount64;
      if (_Now - FLastEventTime) >= FDebounceMS then
      begin
        LBLogger.Write(5, 'TGPIOTrigger.Execute', lmt_Debug, 'Value changed');
        FLastEventTime := _Now;
        Self.DoValueChanged();
      end;
    end;

    _PreviousValue := _Value;
    PreciseSleep(50);
  end;
end;

constructor TGPIOTrigger.Create;
begin
  inherited Create;

  InitGPIODLibrary;
  FLastEventTime := 0;
end;


procedure TGPIOTrigger.clearGPIO();
begin
  try
    if FLine <> nil then
      gpiod_line_release(FLine);
	  
    if FChip <> nil then
      gpiod_chip_close(FChip);
  
  except
    on E: Exception do
      LBLogger.Write(1, 'TGPIOTrigger.clearGPIO', lmt_Error, E.Message);    
  end;
  
  FLine := nil;
  FChip := nil;
end;

function TGPIOTrigger.get_OnValueChanged: TNotifyEvent;
begin
  Result := FOnValueChanged;
end;

procedure TGPIOTrigger.set_OnValueChanged(AValue: TNotifyEvent);
begin
  FOnValueChanged := AValue;
end;


function TGPIOTrigger.setGPIOItemBaseData(GPIONum: Byte; PullType: TPullType; aDebounceMicroSecs: Cardinal): Boolean;
var
  _cmd : String;
  _ThreadName : AnsiString;
  _pt : String;

begin
  Result := False;
  LBLogger.Write(5, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Debug, 'Initializing');

  if GPIONum > 0 then
  begin
    {$IFDEF Linux}
    case PullType of
      pt_PullDown : _pt := 'pd';
      pt_PullUp   : _pt := 'pu';
      else _pt := 'pu';
    end;
    FPullType := PullType;
    _cmd := Format('pinctrl set %d ip %s', [GPIONum, _pt]);
    LBLogger.Write(5, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Debug, 'Executing: <%s>', [_cmd]);
//    _cmd := Format('raspi-gpio set %d ip %s', [FLineNum, _pt]);
    fpSystem(_cmd);
    {$ENDIF}

    FLineNum := GPIONum;
    FDebounceMS := aDebounceMicroSecs div 1000;

    FChip := gpiod_chip_open_by_name(cGPIOChipName);
    if FChip <> nil then
    begin
      FLine := gpiod_chip_get_line(FChip, GPIONum);
      if FLine <> nil then
      begin
        _ThreadName := 'GPIOTrigger_' + IntToStr(GPIONum);
        Self.setThreadName(_ThreadName);

        if gpiod_line_request_input(FLine, PChar(_ThreadName)) = 0 then
        begin
          FValue := 0;
          Result := True;
          Self.Start();
        end;
      end;
    end;
  end
  else
    LBLogger.Write(1, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Warning, 'Wrong GPIO number!');
end;

procedure TGPIOTrigger.DoValueChanged();
begin
  try
    if Assigned(FOnValueChanged) then
    begin
      LBLogger.Write(5, 'TGPIOTrigger.DoValueChanged', lmt_Debug, 'Event triggered');
      FOnValueChanged(Self);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TGPIOTrigger.DoValueChanged', lmt_Error, E.Message);
  end;
end;

function TGPIOTrigger.normalizeLevel(aLevel: cInt): cInt;
begin
  // Converto il valore in 1 o 0 logico a seconda della configurazione del pull
  // Se PullDown => 1 se Level = 1
  // Se PullUp => 1 se Level = 0
  case FPullType of
    pt_PullDown : Result := aLevel;
    pt_PullUp   : begin
                    If aLevel = 0 then
                      Result := 1
                    else
                      Result := 0;
                  end;
    else begin
      LBLogger.Write(1, 'TGPIOTrigger.normalizeLevel', lmt_Warning, 'Pull type not set for GPIO %d', [FLineNum]);
      Result := aLevel;
    end;
  end;
end;

{ TGPIOCounter }

procedure TGPIOCounter.ValueChanged(Sender: TObject);
begin
  if FValue = 1 then
  begin
    if FCountEnable then
    begin
      Inc(FCount);
      if Assigned(FOnCountChanged) then
        FOnCountChanged(Self);
    end;
    FCountEnable := not FCountEnable;
  end
  else
    LBLogger.Write(5, 'TGPIOCounter.ValueChanged', lmt_Debug, 'Nothing to do');
end;

function TGPIOCounter.get_OnValueChanged: TNotifyEvent;
begin
  Result := FOnCountChanged;
end;

procedure TGPIOCounter.set_OnValueChanged(AValue: TNotifyEvent);
begin
  FOnCountChanged := AValue;
end;

constructor TGPIOCounter.Create;
begin
  inherited Create;

  FOnValueChanged := @Self.ValueChanged;
  FCountEnable := True; {$WARNING DA TOGLIERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
end;

procedure TGPIOCounter.setStartingOffset(aValue: QWord);
begin
  FCount := aValue;
end;

finalization
  FinalizeGPIODLibrary();

end.

