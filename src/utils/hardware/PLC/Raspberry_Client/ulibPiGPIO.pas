unit ulibPiGPIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ctypes, uLBUtils, dynlibs;

const
  LIBPIGPIO = 'libpigpio.so';

type
  TPigpioCallback = procedure(gpio: cuint; level: cuint; tick: cuint); cdecl;
  TPigpioCallbackEx = procedure(gpio: cuint; level: cuint; tick: cuint; userdata: Pointer); cdecl;

var
  gpioInitialise: function(): cint; cdecl;
  gpioTerminate: procedure(); cdecl;
  gpioSetMode: function(gpio, mode: cuint): cint; cdecl;
  gpioSetAlertFunc: function(gpio: cuint; f: TPigpioCallback): cint; cdecl;
  gpioRead: function(gpio: cuint): cint; cdecl;
  gpioSetAlertFuncEx: function(gpio: cuint; f: TPigpioCallbackEx; userdata: Pointer): cint; cdecl;
  gpioRead_Bits_0_31: function(): cuint; cdecl;
//  gpioSetGlitchFilter: function(gpio: cuint; steady: cuint): cint; cdecl;
  gpioSetPullUpDown: function(gpio: cuint; pud: cuint): cint; cdecl;


const
  PI_INPUT = 0;

  PI_BAD_GPIO = -2;
  PI_BAD_MODE = -3;
  PI_NOT_PERMITTED = -41;


type
  TPullType = (pt_Unknown  = 0,
               pt_PullDown = 1,
               pt_PullUp   = 2);


  TGPIOEncoder = class;


  { TGPIOEncoderUpdater }

  TGPIOEncoderUpdater = class(TLBBaseThread)
    strict private
      FOnValueChanged : TNotifyEvent;

      FLastChange : QWord;
      FNeedUpdate : Boolean;
      FOwner : TGPIOEncoder;

    protected
      procedure Execute; override;

    public
      constructor Create(Owner: TGPIOEncoder); reintroduce;

      property NeedUpdate: Boolean write FNeedUpdate;
      property OnValueChanged: TNotifyEvent write FOnValueChanged;
  end;

  { TGPIOEncoder }

  TGPIOEncoder = class(TObject)
    (*
      ============================================
      Quadrature Encoder State Machine Reference
      ============================================

      1. Binary Encoding of A and B:
         ---------------------------
         | A | B | State  |
         |---|---|--------|
         | 0 | 0 |   0    |
         | 0 | 1 |   1    |
         | 1 | 1 |   3    |
         | 1 | 0 |   2    |

      2. Valid State Transitions:
         -------------------------
         | From | To  | Direction |
         |------|-----|-----------|
         |  0   |  1  |   +1      |
         |  1   |  3  |   +1      |
         |  3   |  2  |   +1      |
         |  2   |  0  |   +1      |
         |  0   |  2  |   -1      |
         |  2   |  3  |   -1      |
         |  3   |  1  |   -1      |
         |  1   |  0  |   -1      |



      3. State Transition Diagram:
         --------------------------

             +-----+      +-----+
             |  00 | ---> | 01  |
             +-----+      +-----+
               ^            |
               |            v
             +-----+      +-----+
             | 10  | <--- | 11  |
             +-----+      +-----+

         Clockwise: 00 → 01 → 11 → 10 → 00
         Counter-Clockwise: 00 → 10 → 11 → 01 → 00
    *)
  strict private
    FUpdater : TGPIOEncoderUpdater;

    FPIN_A : Byte;
    FPIN_B : Byte;
    FLastValB : Byte;
    FLock: TTimedOutCriticalSection;
    FLastTick: Int64;
    FPulseCounter: Int64;
    FLastPulseCount : Int64;
    FPulsesPerSec: Double;
    FPullType : TPullType;
    FDebounceMicroSec : Int64;
    FUserData : Pointer;

    FLastChange : QWord;

    function get_CounterValue: Int64;

    procedure releaseGPIOs();
    procedure set_OnValueChanged(aValue: TNotifyEvent);
    procedure destroyUpdater();

  public
    constructor Create;
    destructor Destroy; override;

    function Start(): Boolean;
    procedure resetCounter();

    procedure updateCounter(aGPIO: cuint; level: cint; tick: cuint);
    procedure updateBValue(aValue: cint);

    property PinA: Byte write FPIN_A;
    property PinB: Byte write FPIN_B;
    property PullType: TPullType write FPullType;
    property DebounceMicroSec: Int64 write FDebounceMicroSec;
    property UserData: Pointer read FUserData write FUserData;

    property Counter: Int64 read get_CounterValue;
    property OnValueChanged: TNotifyEvent write set_OnValueChanged;
  end;

  { TGPIOTrigger }

  TGPIOTrigger = class(TObject)
    strict private
      FDebounceMicroSecs: Cardinal;
      FLastTick: QWord;

      FUserData : Pointer;

      procedure clearGPIO();

    strict protected
      FOnValueChanged : TNotifyEvent;
      FGPIONum : Byte;
      FPullType : TPullType;
      FValue : Byte;

      function normalizeLevel(aLevel: cInt): cInt;
      function canEvaluate(aTick: cuint): Boolean;

    public
      constructor Create;
      destructor Destroy; override;

      function setGPIOItemBaseData(GPIONum: Byte; PullType: TPullType; aDebounceMicroSecs: Cardinal): Boolean;
      procedure statusChanged(Level: cInt; tick: cuint); virtual;

      property UserData: Pointer read FUserData write FUserData;

      property Value: Byte read FValue;

      property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;

  { TGPIOCounter }

  TGPIOCounter = class(TGPIOTrigger)
    strict private
      FCount : QWord;

    public
      procedure statusChanged(Level: cInt; tick: cuint); override;

      procedure setStartingOffset(aValue: QWord);

      property Count: QWord read FCount;
  end;

  function getPullTypeEnum(const aPullType: String): TPullType;

const
  cPullType_Description : array [TPullType] of String = ('-', 'down', 'up');

implementation

uses
  ULBLogger;

var
  gv_LibPiGPIOHandle: TLibHandle = NilHandle;

procedure UnloadPigpioLib;
begin
  if gv_LibPiGPIOHandle <> NilHandle then
  begin
    if Assigned(gpioTerminate) then
      gpioTerminate();

    UnloadLibrary(gv_LibPiGPIOHandle);
    gv_LibPiGPIOHandle := NilHandle;
  end;
end;

function LoadPigpioLib: Boolean;
begin
  Result := gv_LibPiGPIOHandle <> NilHandle;

  if not Result then
  begin
    gv_LibPiGPIOHandle := LoadLibrary(LIBPIGPIO);
    if gv_LibPiGPIOHandle <> NilHandle then
    begin
      Pointer(gpioInitialise)      := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioInitialise');
      Pointer(gpioTerminate)       := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioTerminate');
      Pointer(gpioSetMode)         := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioSetMode');
      Pointer(gpioSetAlertFunc)    := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioSetAlertFunc');
      Pointer(gpioRead)            := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioRead');
      Pointer(gpioSetAlertFuncEx)  := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioSetAlertFuncEx');
      Pointer(gpioRead_Bits_0_31)  := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioRead_Bits_0_31');
      Pointer(gpioSetPullUpDown)   := GetProcedureAddress(gv_LibPiGPIOHandle, 'gpioSetPullUpDown');


      Result := Assigned(gpioInitialise) and Assigned(gpioTerminate) and
                Assigned(gpioSetMode) and Assigned(gpioSetAlertFunc) and
                Assigned(gpioRead) and Assigned(gpioSetAlertFuncEx) and
                Assigned(gpioRead_Bits_0_31) and Assigned(gpioSetPullUpDown);

      if Result then
      begin
        Result := gpioInitialise() >= 0;
        if Result then
          LBLogger.Write(1, 'LoadPigpioLib', lmt_Debug, 'Library <%s> successfully initialized', [LIBPIGPIO]);
      end
      else
        LBLogger.Write(1, 'LoadPigpioLib', lmt_Warning, 'Function not loaded from <%s>!', [LIBPIGPIO]);

      if not Result then
        UnloadPigpioLib();
    end
    else
      LBLogger.Write(1, 'LoadPigpioLib', lmt_Warning, '<%s> not loaded!', [LIBPIGPIO]);
  end;
end;

procedure OnChangeEx(gpio: cuint; level: cuint; tick: cuint; userdata: Pointer); cdecl;
begin
  if level <> 2 then
    TGPIOEncoder(userdata).updateCounter(gpio, level, tick);
end;

procedure OnUpdateBValue(gpio: cuint; level: cuint; tick: cuint; userdata: Pointer); cdecl;
begin
  if level <> 2 then
    TGPIOEncoder(userdata).updateBValue(level);
end;

procedure OnTriggerStatusChanged(gpio: cuint; level: cuint; tick: cuint; userdata: Pointer); cdecl;
begin
  if level <> 2 then
    TGPIOTrigger(userdata).statusChanged(level, tick);
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

{ TGPIOEncoderUpdater }

procedure TGPIOEncoderUpdater.Execute;
var
  _Now : QWord;

const
  cTimeout = QWord(1000);

begin
  while not Self.Terminated do
  begin
    if FNeedUpdate and Assigned(FOnValueChanged) then
    begin
      _Now := GetTickCount64;
      if _Now - FLastChange >= cTimeout then
      begin
        FOnValueChanged(FOwner);
        FLastChange := _Now;
        FNeedUpdate := False;
      end;
    end;
    Self.PauseFor(40);
  end;
end;

constructor TGPIOEncoderUpdater.Create(Owner: TGPIOEncoder);
begin
  inherited Create();

  FOwner := Owner;
  FOnValueChanged := nil;
  FLastChange := 0;
  FNeedUpdate := False;
end;



{ TGPIOEncoder }

constructor TGPIOEncoder.Create;
begin
  inherited Create();

  LoadPigpioLib();

  FLock := TTimedOutCriticalSection.Create;
  FLastTick := 0;
  FPulseCounter := 0;
  FLastPulseCount := 0;
  FPulsesPerSec := 0;
  
  FPullType := pt_Unknown;

  FLastValB := 0;
  FDebounceMicroSec := 20;

  FLastChange := 0;
end;

destructor TGPIOEncoder.Destroy;
begin
  Self.destroyUpdater();

  try
    FreeAndNil(FLock);
    Self.releaseGPIOs();

  except
    on E: Exception do
      LBLogger.Write(1, 'TGPIOEncoder.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TGPIOEncoder.updateBValue(aValue: cint);
begin
  FLastValB := aValue;
end;

procedure TGPIOEncoder.resetCounter;
begin
  if FLock.Acquire('TEncoderThread.resetCounter') then
  begin
    try
      LBLogger.Write(5, 'TGPIOEncoder.resetCounter', lmt_Debug, 'Resetting counter ... Previous value %d', [Int64(FPulseCounter)]);

      FPulseCounter := 0;
    finally
      FLock.Release;
    end;
  end;
end;


procedure TGPIOEncoder.updateCounter(aGPIO: cuint; level: cint; tick: cuint);
var
  _Delta : Integer;
  _Elapsed : Int64;
  _Now : QWord;

const
  cUpdateTime = QWord(1000);


begin
  // Devo incrementare il contatore sul fronte in salita o in discesa a seconda
  // di come è settato il PIN dell'encoder. Se è in PullDown vuol dire che "a riposo"
  // il valore logico è 0, quindi incremento quanto c'è la transizione a 1.
  // Nel caso di PullUp "a riposo" il valore logico è 1, la transizione che mi interessa è verso lo 0

  if ((FPullType = pt_PullDown) and (Level = 1)) or
     ((FPullType = pt_PullUp) and (Level = 0)) then
  begin

    if tick < FLastTick then
      _Elapsed := (Int64(tick) + (Int64(High(Cardinal)) - FLastTick))
    else
      _Elapsed := tick - FLastTick;

   if _Elapsed > FDebounceMicroSec then
    begin
      if ((FLastValB = 0) and (Level = 1)) or
         ((FLastValB = 1) and (Level = 0)) then
        _Delta := 1
      else
        _Delta := -1;

      if FLock.Acquire('TEncoderThread.updateCounter') then
      begin
        try
          Inc(FPulseCounter, _Delta);

        finally
          FLock.Release();
        end;
      end;
    end;
  end;
end;


function TGPIOEncoder.get_CounterValue: Int64;
begin
  if FLock.Acquire('TEncoderThread.GetPosition') then
  begin
    try
      Result := FPulseCounter;
    finally
      FLock.Release;
    end;
  end;
end;

procedure TGPIOEncoder.releaseGPIOs();
begin
  if FPIN_A > 0 then
  begin
    gpioSetAlertFuncEx(FPIN_A, nil, nil);
    gpioSetMode(FPIN_A, PI_INPUT);         // Imposta il pin come input
    gpioSetPullUpDown(FPIN_A, cint(pt_Unknown)); // Disattiva pull-up/down
  end;

  if FPIN_B > 0 then
  begin
    gpioSetAlertFuncEx(FPIN_B, nil, nil);
    gpioSetMode(FPIN_B, PI_INPUT);         // Imposta il pin come input
    gpioSetPullUpDown(FPIN_B, cint(pt_Unknown)); // Disattiva pull-up/down
  end;
end;

procedure TGPIOEncoder.set_OnValueChanged(aValue: TNotifyEvent);
begin
  Self.destroyUpdater();
  if aValue <> nil then
  begin
    FUpdater := TGPIOEncoderUpdater.Create(Self);
    FUpdater.OnValueChanged := aValue;
    FUpdater.Start();
  end;
end;

procedure TGPIOEncoder.destroyUpdater();
begin
  if FUpdater <> nil then
  begin
    if FUpdater.Suspended then
      FreeAndNil(FUpdater)
    else begin
      FUpdater.AddReference(@FUpdater);
      FUpdater.Terminate;
      while FUpdater <> nil do
        Sleep(10);
    end;
  end;
end;

function TGPIOEncoder.Start: Boolean;
var
  _res : cInt;

begin
  Result := False;

  if gv_LibPiGPIOHandle = NilHandle then
  begin
    LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Library <%s> not initialized!', [LIBPIGPIO]);
    Exit;
  end;

  if (FPIN_A = 0) or (FPIN_B = 0) then
  begin
    LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Wrong pins! A: %d  -  B: %d', [FPIN_A, FPIN_B]);
    Exit;
  end;

  _res := gpioSetMode(FPIN_A, PI_INPUT);
  if _res <> 0 then
  begin
    LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Error setting mode for pin A %d', [_res]);
    Exit;
  end;

  _res := gpioSetMode(FPIN_B, PI_INPUT);
  if _res <> 0 then
  begin
    LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Error setting mode for pin B %d', [_res]);
    Exit;
  end;

  gpioSetPullUpDown(FPIN_A, cInt(FPullType));
  gpioSetPullUpDown(FPIN_B, cInt(FPullType));

  // Attacca callback per entrambi i pin
  if gpioSetAlertFuncEx(FPIN_A, @OnChangeEx, Self) = 0 then
  begin
    if gpioSetAlertFuncEx(FPIN_B, @OnUpdateBValue, Self) = 0 then
      Result := True
    else
      LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Error setting callback for pin B %d', [FPIN_B]);
  end
  else
    LBLogger.Write(1, 'TGPIOEncoder.Start', lmt_Warning, 'Error setting callback for pin A %d', [FPIN_A]);
end;



{ TGPIOTrigger }

constructor TGPIOTrigger.Create;
begin
  inherited Create;

  LoadPigpioLib();
  FGPIONum := 0;
  FLastTick := 0;
  FDebounceMicroSecs := 100;
  FUserData := nil;
end;

destructor TGPIOTrigger.Destroy;
begin
  Self.clearGPIO();
  inherited Destroy;
end;

procedure TGPIOTrigger.clearGPIO();
begin
  if FGPIONum > 0 then
  begin
    gpioSetAlertFuncEx(FGPIONum, nil, nil);
    gpioSetMode(FGPIONum, PI_INPUT);         // Imposta il pin come input
    gpioSetPullUpDown(FGPIONum, cint(pt_Unknown)); // Disattiva pull-up/down
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
      LBLogger.Write(1, 'TGPIOTrigger.normalizeLevel', lmt_Warning, 'Pull type not set for GPIO %d', [FGPIONum]);
      Result := aLevel;
    end;
  end;
end;

function TGPIOTrigger.canEvaluate(aTick: cuInt): Boolean;
var
  _Elapsed : QWord;

begin
  if aTick < FLastTick then
    _Elapsed := (Int64(aTick) + (Int64(High(Cardinal)) - FLastTick))
  else
    _Elapsed := aTick - FLastTick;

  Result := _Elapsed > FDebounceMicroSecs;
  if Result then
    FLastTick := aTick;
//  else
//    LBLogger.Write(1, 'TGPIOTrigger.canEvaluate', lmt_Warning, 'Elapsed only %d ms!', [_Elapsed]);
end;


function TGPIOTrigger.setGPIOItemBaseData(GPIONum: Byte; PullType: TPullType; aDebounceMicroSecs: Cardinal): Boolean;
var
  _res : cint;

begin
  Result := False;

  Self.clearGPIO();

  if gv_LibPiGPIOHandle = NilHandle then
  begin
    LBLogger.Write(1, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Warning, 'Library <%s> not initialized!', [LIBPIGPIO]);
    Exit;
  end;

  if (GPIONum = 0) then
  begin
    LBLogger.Write(1, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Warning, 'Wrong pin %d', [GPIONum]);
    Exit;
  end;

  _res := gpioSetMode(GPIONum, PI_INPUT);
  if _res <> 0 then
  begin
    LBLogger.Write(1, 'TGPIOTrigger.setGPIOItemBaseData', lmt_Warning, 'Error setting mode for pin %d', [GPIONum]);
    Exit;
  end;

  FGPIONum := GPIONum;
  gpioSetPullUpDown(FGPIONum, cInt(PullType));
  FPullType := PullType;

  FDebounceMicroSecs := aDebounceMicroSecs;

  Result := gpioSetAlertFuncEx(FGPIONum, @OnTriggerStatusChanged, Self) = 0;
end;

procedure TGPIOTrigger.statusChanged(Level: cInt; tick: cuint);
var
//  _Elapsed : Int64;
  _NewValue : Byte;

begin
  (*
  if tick < FLastTick then
    _Elapsed := (Int64(tick) + (Int64(High(Cardinal)) - FLastTick))
  else
    _Elapsed := tick - FLastTick;

  FLastTick := tick;
  *)
  _NewValue := Self.normalizeLevel(Level);

  if Self.canEvaluate(tick) then
//  if _Elapsed > FDebounceMicroSecs then
  begin
(*
    // Converto il valore in 1 o 0 logico a seconda della configurazione del pull
    // Se PullDown => 1 se Level = 1
    // Se PullUp => 1 se Level = 0
    case FPullType of
      pt_PullDown : _NewValue := Level;
      pt_PullUp   : begin
                      If Level = 0 then
                        _NewValue := 1
                      else
                        _NewValue := 0;
                    end;
      else begin
        LBLogger.Write(1, 'TGPIOTrigger.statusChanged', lmt_Warning, 'Pull type not set for GPIO %d', [FGPIONum]);
        _NewValue := Level;
      end;
    end;
*)
    if _NewValue <> FValue then
    begin
      LBLogger.Write(5, 'TGPIOTrigger.statusChanged', lmt_Debug, 'New value GPIO %d: old value %d  -  new value %d', [FGPIONum, FValue, _NewValue]);

      if Assigned(FOnValueChanged) then
        FOnValueChanged(Self);
    end;
  end;

  FValue := _NewValue;

end;

{ TGPIOCounter }

procedure TGPIOCounter.statusChanged(Level: cInt; tick: cuint);
var
  _NewValue : Byte;

begin
  _NewValue := Self.normalizeLevel(Level);

  if Self.canEvaluate(tick) then
  begin
    if _NewValue <> FValue then
    begin
      if _NewValue = 1 then
      begin
        Inc(FCount);

        LBLogger.Write(5, 'TGPIOCounter.statusChanged', lmt_Debug, 'GPIO %d: old value %d  new value %d  -  Counter: %d', [FGPIONum, FValue, _NewValue, FCount]);

        if Assigned(FOnValueChanged) then
          FOnValueChanged(Self);
      end
      else
        LBLogger.Write(5, 'TGPIOCounter.statusChanged', lmt_Debug, 'GPIO %d: %d  -  Down', [FGPIONum, FValue]);
    end;
  end;

  FValue := _NewValue;
end;

procedure TGPIOCounter.setStartingOffset(aValue: QWord);
begin

end;



finalization
  UnloadPigpioLib;

end.




