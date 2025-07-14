unit uLBTimers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uTimedoutCriticalSection {$IFDEF Linux}, BaseUnix{$ENDIF};

type
  { TTimeoutTimer }

  TTimeoutTimer = class(TObject)
    strict private
      FOnDestroyEvent  : TNotifyEvent;

      FLogExpireStatus : Boolean;
      FTicks           : QWord;
      FTimeout         : QWord;
      function get_Remain: Int64;

    public
      constructor Create(msTimeout: QWord); reintroduce;
      destructor Destroy; override;

      procedure Reset();
      procedure Reset(aNewTimeout: QWord); overload;

      function Expired(): Boolean;

      property LogExpireStatus : Boolean              read FLogExpireStatus    write FLogExpireStatus;
      property msTimeout       : QWord                read FTimeout;
      property Remain          : Int64                read get_Remain;
      property OnDestroyEvent  : TNotifyEvent                                  write FOnDestroyEvent;
  end;


  { TInterruptableSleep }

  TInterruptableSleep = class(TObject)
    strict private
      FSleeping : Boolean;
      FCS : TTimedOutCriticalSection;
      {$IFDEF Linux}
      FPipe: array[0..1] of cint;
      procedure CleanupPipe();
      function initialize(): Boolean;
      {$ELSE IFDEF Windows}
      FEvent : THandle;
      {$ENDIF}

    public
      constructor Create;
      destructor Destroy; override;

      procedure sleep(aTimeoutMs: Cardinal);
      procedure wakeUp();
  end;


  procedure PreciseSleep(ms: LongInt);

implementation

uses
  ULBLogger, {$IFDEF Linux}LazSysUtils, Unix{$ENDIF}{$IFDEF MSWINDOWS}windows{$ENDIF};

{$IFDEF Linux}
function clock_gettime(clock_id: LongInt; tp: PTimeSpec): LongInt; cdecl; external 'c';
function clock_nanosleep(clock_id: LongInt; flags: LongInt; const request: PTimeSpec; remain: PTimeSpec): LongInt; cdecl; external 'c';
{$ENDIF}


procedure PreciseSleep(ms: LongInt);
{$IFDEF Linux}
var
  now, target: timespec;

const
  CLOCK_MONOTONIC = 1;
  TIMER_ABSTIME = 1;

{$ENDIF}
{$IFDEF Windows}
var
  hTimer: THandle;
  dueTime: Int64;
{$ENDIF}
begin
  {$IFDEF Linux}
  clock_gettime(CLOCK_MONOTONIC, @now);
  target.tv_sec := now.tv_sec + ms div 1000;
  target.tv_nsec := now.tv_nsec + (ms mod 1000) * 1000000;
  if target.tv_nsec >= 1000000000 then
  begin
    Inc(target.tv_sec);
    Dec(target.tv_nsec, 1000000000);
  end;
  clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, @target, nil);
  {$ENDIF}

  {$IFDEF Windows}
  // timeBeginPeriod(1); // Uncomment if system timer resolution is too coarse
  hTimer := CreateWaitableTimer(nil, True, nil);
  if hTimer <> 0 then
  begin
    dueTime := -Int64(ms) * 10000;
    if SetWaitableTimer(hTimer, dueTime, 0, nil, nil, False) then
      WaitForSingleObject(hTimer, INFINITE);
    CloseHandle(hTimer);
  end;
  // timeEndPeriod(1);
  {$ENDIF}
end;

{ TInterruptableSleep }


{$IFDEF Unix}
procedure TInterruptableSleep.CleanupPipe();
begin
  if FPipe[0] <> -1 then
  begin
    fpClose(FPipe[0]);
    FPipe[0] := -1;
  end;

  if FPipe[1] <> -1 then
  begin
    fpClose(FPipe[1]);
    FPipe[1] := -1;
  end;
end;
{$ENDIF}

constructor TInterruptableSleep.Create;
begin
  inherited Create;

  FSleeping := False;
  FCS := TTimedOutCriticalSection.Create;

  {$IFDEF Unix}
  FPipe[0] := -1;
  FPipe[1] := -1;
  Self.initialize();
  {$ELSE}
  FEvent := CreateEvent(nil, True, False, nil); // manual reset, initially non-signaled
  {$ENDIF}
end;

destructor TInterruptableSleep.Destroy;
begin
  try
    FreeAndNil(FCS);
    {$IFDEF Unix}
    Self.CleanupPipe();
    {$ELSE}
    if FEvent <> 0 then
      CloseHandle(FEvent);
    {$ENDIF}

  except
    on E: Exception do
      LBLogger.Write(1, 'TInterruptableSleep.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

{$IFDEF Unix}
function TInterruptableSleep.initialize(): Boolean;
var
  _flags: cint;

begin
  if (FPipe[0] = -1) and (FPipe[1] = -1) then
  begin
    Result := fpPipe(FPipe) = 0;
    if Result then
    begin

      _flags := fpfcntl(FPipe[0], F_GETFL, 0);
      fpfcntl(FPipe[0], F_SETFL, _flags or O_NONBLOCK);

    end;
  end
  else
    Result := True;
end;
{$ENDIF}

procedure TInterruptableSleep.sleep(aTimeoutMs: Cardinal);
{$IFDEF Unix}
var
  _fds: TFDSet;
  _timeout: TTimeVal;
  _res: cint;
  _dummy: char;
{$ENDIF}
begin
  {$IFDEF Unix}
  if (FPipe[0] = -1) then
    Exit;

  fpFD_ZERO(_fds);
  fpFD_SET(FPipe[0], _fds);

  _timeout.tv_sec := aTimeoutMs div 1000;
  _timeout.tv_usec := (aTimeoutMs mod 1000) * 1000;

  if FCS.Acquire('TInterruptableSleep.sleep') then
  begin
    FSleeping := True;
    FCS.Release();
  end;

  _res := fpSelect(FPipe[0] + 1, @_fds, nil, nil, @_timeout);

  if FCS.Acquire('TInterruptableSleep.sleep') then
  begin
    try

      if _res > 0 then
        while fpRead(FPipe[0], _dummy, 1) = 1 do ; // clear the pipe

    except
      on E: Exception do
        LBLogger.Write(1, 'TInterruptableSleep.sleep', lmt_Error, E.Message);
    end;
    FSleeping := False;

    FCS.Release();
  end;
  {$ELSE}
  if FCS.Acquire('TInterruptableSleep.sleep') then
  begin
    FSleeping := True;
    ResetEvent(FEvent); // reset prima di attendere
    FCS.Release();
  end;

  case WaitForSingleObject(FEvent, aTimeoutMs) of
    WAIT_OBJECT_0: ; // svegliato da WakeUp
    WAIT_TIMEOUT: ;  // timeout naturale
    WAIT_FAILED: LBLogger.Write(1, 'TInterruptableSleep.sleep', lmt_Warning, 'Wait failed');
  end;

  if FCS.Acquire('TInterruptableSleep.sleep') then
  begin
    FSleeping := False;
    FCS.Release();
  end;
  {$ENDIF}
end;

procedure TInterruptableSleep.wakeUp();
begin
  if FCS.Acquire('TInterruptableSleep.wakeUp') then
  begin
    try

      {$IFDEF Unix}
      if FSleeping and (FPipe[1] <> -1) then
        fpWrite(FPipe[1], '1', 1);
      {$ELSE}
      if FSleeping then
        SetEvent(FEvent);
      {$ENDIF}

    except
      on E: Exception do
        LBLogger.Write(1, 'TInterruptableSleep.wakeUp', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;
end;

{ TTimeoutTimer }

function TTimeoutTimer.get_Remain: Int64;
var
  _Arrival : QWord;
  _Now : QWord;

begin
  _Arrival := FTicks + FTimeout;
  _Now := SysUtils.GetTickCount64;

  if _Arrival >= _Now then
    Result := _Arrival - _Now
  else
    Result := Int64(-1) * (_Now - _Arrival);
end;

constructor TTimeoutTimer.Create(msTimeout: QWord);
begin
  inherited Create;

  FTimeout := msTimeout;
  FLogExpireStatus := False;
  FOnDestroyEvent := nil;
  FTicks := SysUtils.GetTickCount64();
end;

destructor TTimeoutTimer.Destroy;
begin
  try

    if Assigned(FOnDestroyEvent) then
      FOnDestroyEvent(Self);

  except
    on E: Exception do
      LBLogger.Write(1, 'TTimeoutTimer.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TTimeoutTimer.Reset();
begin
  if Self <> nil then
  begin
    FTicks := SysUtils.GetTickCount64();
    FLogExpireStatus := False;
  end;
end;

procedure TTimeoutTimer.Reset(aNewTimeout: QWord);
begin
  if Self <> nil then
  begin
    FTimeout := aNewTimeout;
    Self.Reset();
  end;
end;

function TTimeoutTimer.Expired(): Boolean;
var
  _Now : QWord;

begin
  Result := False;

  if Self <> nil then
  begin
    _Now := SysUtils.GetTickCount64();
    if _Now >= FTicks then
      Result := (_Now - FTicks) > FTimeout
    else
      Result := (High(QWord) - FTicks) + _Now > FTimeout;
  end;
end;

end.

