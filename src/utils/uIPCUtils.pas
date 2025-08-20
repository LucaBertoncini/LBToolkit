unit uIPCUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF Windows} Windows{$ELSE}BaseUnix, Unix, UnixType, Sockets, ipc{$ENDIF};

type
  {$IFDEF Windows}
  TSharedMemory = record
    shmHandle : THandle;       // Windows handle from CreateFileMapping
    mem       : Pointer;
    size      : Integer;
    name      : String;       // identifier passed to CreateFileMapping
  end;
  {$ELSE}
    {$IFDEF shmPOSIX}
    TSharedMemory = record
    shmHandle: Integer;       // file descriptor or segment ID
      mem       : Pointer;
      size      : Integer;
      name      : String;       // /dev/shm/<name>
    end;
    {$ELSE}
    TSharedMemory = record      // SysV
      shmHandle: Integer;       // file descriptor or segment ID
      mem       : Pointer;
      size      : Integer;
      key       : TKey;         // IPC SysV key
    end;
    {$ENDIF}
  {$ENDIF}

  pSharedMemory = ^TSharedMemory;


{$IFDEF Windows}
function createSharedMemory(aShmInfo: pSharedMemory): boolean;
function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
function AllocateSharedMemory(aName: String; aSize: Integer): pSharedMemory;
{$ELSE}
  {$IFDEF shmPOSIX}
  function createSharedMemory(aShmInfo: pSharedMemory): boolean;
  function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
  {$ELSE}
  function createSharedMemory(aShmInfo: pSharedMemory): Boolean;
  function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
  function AllocateSharedMemory(aKey: TKey; aSize: Integer): pSharedMemory;
  {$ENDIF}
{$ENDIF}

type
  { TLBNamedSemaphore }

  TLBNamedSemaphore = class(TObject)
  strict private
    {$IFDEF WINDOWS}
    FSemHandle: THandle;
    {$ELSE}
    FSemId: cint;
    FKey: TKey;
    {$ENDIF}
    FName: string;
  public
    constructor Create(const AName: string;{$IFDEF Linux} const AKey: TKey;{$ENDIF} Locked: Boolean);
    destructor Destroy; override;

    function Wait(aTimeoutMs: Cardinal): Boolean;
    procedure Signal;

    {$IFDEF Linux}
    property Key: TKey read FKey;
    {$ENDIF}
    property Name: String read FName;
  end;

  { TLocalSocket }

  TLocalSocket = class(TObject)
  private
    FHandle: THandle;
    FLastError: Integer;
    FTimeout: Integer;
    function GetLastErrorDescr: string;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const Path: string): Boolean;
    procedure Close;

    procedure FlushRecvBuffer;
    function SendBuffer(const Buffer: Pointer; Size: Integer): Integer;
    function RecvBuffer(Buffer: Pointer; Size: Integer): Integer;

    {
      Sends a string over the socket or pipe without appending any termination characters.
      The string is transmitted as a binary block, preceded by its length encoded
      as a 32-bit integer. This ensures that the receiver knows exactly how many
      bytes to read, regardless of the string content.

      [4 bytes: string length] + [N bytes: string content]
    }
    function SendString(const S: string): Boolean;

    {
      Receives a string that was previously sent using the SendString format.
      It first reads a 32-bit integer representing the length of the incoming string,
      then reads exactly that number of bytes to reconstruct the original string.
      No termination characters are expected or processed.
    }
    function RecvString: string;

    property LastError: Integer read FLastError;
    property LastErrorDescr: string read GetLastErrorDescr;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TLocalServerSocket = class(TObject)
  private
    FHandle: THandle;
    FPath: string;
    FLastError: Integer;
    FTimeout: Integer;
  public
    constructor Create(const Path: string);
    destructor Destroy; override;

    function Listen: Boolean;
    function Accept: TLocalSocket;

    property LastError: Integer read FLastError;
    property Timeout: Integer read FTimeout write FTimeout;
  end;



implementation

uses
  ULBLogger, LazFileUtils;

{$IFDEF Windows}
function createSharedMemory(aShmInfo: pSharedMemory): Boolean;
begin
  Result := False;
  aShmInfo^.mem := nil;

  if Length(aShmInfo^.name) = 0 then
  begin
    LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'Shared memory name is empty!');
    Exit;
  end;

  // Create named file mapping
  aShmInfo^.shmHandle := Windows.CreateFileMapping(
    INVALID_HANDLE_VALUE, nil,
    PAGE_READWRITE, 0,
    aShmInfo^.size,
    PChar(aShmInfo^.name)
  );

  if aShmInfo^.shmHandle = 0 then
  begin
    LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'CreateFileMapping failed.');
    Exit;
  end;

  // Map the memory
  aShmInfo^.mem := Windows.MapViewOfFile(
    aShmInfo^.shmHandle,
    FILE_MAP_ALL_ACCESS,
    0, 0, aShmInfo^.size
  );

  if aShmInfo^.mem = nil then
  begin
    LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'MapViewOfFile failed.');
    Windows.CloseHandle(aShmInfo^.shmHandle);
    aShmInfo^.shmHandle := 0;
    Exit;
  end;

  Result := True;
end;

function AllocateSharedMemory(aName: String; aSize: Integer): pSharedMemory;
var
  _shm: pSharedMemory;
begin
  Result := nil;

  if aName <> '' then
  begin
    New(_shm);
    _shm^.name := aName;
    _shm^.size := aSize;

    if createSharedMemory(_shm) then
      Result := _shm
    else
    begin
      Dispose(_shm);
      Result := nil;
    end;
  end
  else
    LBLogger.Write(1, 'AllocateSharedMemory', lmt_Warning, 'Shared memory name cannot be empty');
end;

function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
begin
  Result := False;

  if aShmInfo^.mem <> nil then
  begin
    Windows.UnmapViewOfFile(aShmInfo^.mem);
    aShmInfo^.mem := nil;
  end;

  if aShmInfo^.shmHandle <> 0 then
  begin
    Windows.CloseHandle(aShmInfo^.shmHandle);
    aShmInfo^.shmHandle := 0;
  end;

  Result := True;
end;
{$ELSE}

{$IFDEF shmPOSIX}
const
  shmFolder = String('/dev/shm/');

  rtlib = 'rt';
  clib = 'c';

  function shm_open(name: PChar; oflag: LongInt; mode: mode_t): cint; cdecl; external rtlib name 'shm_open';
  function shm_unlink(name: PChar): cint; cdecl; external rtlib name 'shm_unlink';
  function ftruncate(fd: cint; length: off_t): cint; cdecl; external clib name 'ftruncate';
{$ELSE}
const
  SHM_MODE = &666; // S_IRUSR or S_IWUSR; // Read and write permissions
{$ENDIF}

{$IFDEF Unix}
const
  GETVAL   = 12;
  SETVAL   = 16;
  IPC_RMID = 0;
{$ENDIF}



{$IFDEF shmPOSIX}
function createSharedMemory(aShmInfo: pSharedMemory): boolean;
var
  _CompleteFile : String;

begin
  Result := False;

  aShmInfo^.mem := nil;
  aShmInfo^.shmHandle := -1;

  if Length(aShmInfo^.name) > 0 then
  begin

    _CompleteFile := shmFolder + aShmInfo^.name;
    aShmInfo^.shmHandle := shm_open(PChar(_CompleteFile), O_CREAT or O_RDWR, S_IRUSR or S_IWUSR);
    if aShmInfo^.shmHandle <> -1 then
    begin
      if ftruncate(aShmInfo^.shmHandle, aShmInfo^.Size) = 0 then
      begin
        aShmInfo^.mem := Fpmmap(nil, aShmInfo^.Size, PROT_READ or PROT_WRITE, MAP_SHARED, aShmInfo^.shmHandle, 0);
        if aShmInfo^.mem <> Pointer(-1) then
          Result := True
        else begin
          LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'Error mapping file', []);
          aShmInfo^.mem := nil;
        end;
      end
      else
        LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'Error executing Ftruncate on file <%s> size %d', [_CompleteFile, aShmInfo^.size]);
    end
    else
      LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'Error opening file <%s>', [_CompleteFile]);

    if not Result then
      closeSharedMemory(aShmInfo);

  end
  else
    LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'No shared memory name!', []);
end;

function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
var
  _CompleteFile : String;
begin
  Result := False;

  if aShmInfo^.mem <> nil then
  begin
    Fpmunmap(aShmInfo^.mem, aShmInfo^.size);
    aShmInfo^.mem := nil;
  end;

  if aShmInfo^.shmHandle <> -1 then
  begin
    FpClose(aShmInfo^.shmHandle);
    aShmInfo^.shmHandle := -1
  end;

  if Length(aShmInfo^.name) > 0 then
  begin
    _CompleteFile := shmFolder + aShmInfo^.name;

    shm_unlink(PChar(_CompleteFile));
  end;

  Result := True;
end;
{$ELSE}
function createSharedMemory(aShmInfo: pSharedMemory): Boolean;
begin
  Result := False;

  aShmInfo^.mem := nil;
  aShmInfo^.shmHandle := -1;

  if aShmInfo^.key > 0 then
  begin

    aShmInfo^.shmHandle := shmget(aShmInfo^.key, aShmInfo^.size, IPC_CREAT or SHM_MODE);
    if aShmInfo^.shmHandle <> -1 then
    begin
      // Attach shared memory to our address space
      aShmInfo^.mem := shmat(aShmInfo^.shmHandle, nil, 0);
      if aShmInfo^.mem <> Pointer(-1) then
        Result := True
      else begin
        LBLogger.Write(1, 'createSharedMemoryPosix', lmt_Warning, 'Error attaching shared memory!');
        aShmInfo^.mem := nil;
      end;
    end
    else
      LBLogger.Write(1, 'createSharedMemoryPosix', lmt_Warning, 'Error creating shared memory!', []);

    if not Result then
      closeSharedMemory(aShmInfo);

  end
  else
    LBLogger.Write(1, 'createSharedMemory', lmt_Warning, 'No shared memory key!', []);
end;

function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
begin
  if aShmInfo^.mem <> nil then
  begin
    if shmdt(aShmInfo^.mem) = -1 then
      LBLogger.Write(1, 'closeSharedMemory', lmt_Warning, 'Error detaching shared memory');

    aShmInfo^.mem := nil;
  end;

  if aShmInfo^.shmHandle <> -1 then
  begin
    // Remove shared memory
    if shmctl(aShmInfo^.shmHandle, IPC_RMID, nil) = -1 then
      LBLogger.Write(1, 'closeSharedMemory', lmt_Warning, 'Error removing shared memory!');
    aShmInfo^.shmHandle := -1;
  end;

  Result := True;
end;

function AllocateSharedMemory(aKey: TKey; aSize: Integer): pSharedMemory;
var
  _shm: pSharedMemory;

begin
  New(_shm);
  _shm^.key := aKey;
  _shm^.size := aSize;

  if createSharedMemory(_shm) then
    Result := _shm
  else begin
    Dispose(_shm);
    Result := nil;
  end;
end;

{$ENDIF}
{$ENDIF}


{ TLBNamedSemaphore }

constructor TLBNamedSemaphore.Create(const AName: string; {$IFDEF Linux} const AKey: TKey;{$ENDIF} Locked: Boolean);
{$IFDEF Unix}
var
  _arg: TSEMun;
{$ENDIF}
begin
  inherited Create;
  FName := AName;
  {$IFDEF WINDOWS}
  FSemHandle := CreateSemaphore(nil, 0, 1, PChar(FName));
  {$ELSE}
  FKey := AKey;
  LBLogger.Write(5, 'TLBNamedSemaphore.Create', lmt_Debug, 'Creating semaphore with key: %d', [Integer(FKey)]);

  FSemId := semget(FKey, 1, IPC_CREAT or IPC_EXCL or 438); // 438 = 0o666
  if FSemId = -1 then // Already exists, get it
  begin
    LBLogger.Write(5, 'TLBNamedSemaphore.Create', lmt_Debug, 'Semaphore %d already exists, getting it', [Integer(FKey)]);
    FSemId := semget(FKey, 1, 438);
  end;

  if Locked then
    _arg.val := 0  // Red semaphore
  else
    _arg.val := 1;

  // Initialize semaphore
  semctl(FSemId, 0, SETVAL, _arg);
  {$ENDIF}
end;

destructor TLBNamedSemaphore.Destroy;
{$IFDEF Unix}
var
  _dummy : TSEMun;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if FSemHandle <> 0 then
    CloseHandle(FSemHandle);
  {$ELSE}
  if FSemId <> -1 then
  begin
    FillChar(_dummy, SizeOf(_dummy), 0);
    semctl(FSemId, 0, IPC_RMID, _dummy);
  end;
  {$ENDIF}
  inherited Destroy;
end;

function TLBNamedSemaphore.Wait(aTimeoutMs: Cardinal): Boolean;
{$IFDEF WINDOWS}
begin
  Result := WaitForSingleObject(FSemHandle, aTimeoutMs) = WAIT_OBJECT_0;
end;
{$ELSE}
var
  timeout_spec: TTimeSpec;
  sops: TSEMbuf;
begin
  timeout_spec.tv_sec := aTimeoutMs div 1000;
  timeout_spec.tv_nsec := (aTimeoutMs mod 1000) * 1000000;

  sops.sem_num := 0;
  sops.sem_op := -1; // Wait operation
  sops.sem_flg := 0;
  Result := semtimedop(FSemId, @sops, 1, @timeout_spec) <> -1;
end;
{$ENDIF}

procedure TLBNamedSemaphore.Signal;
{$IFDEF WINDOWS}
begin
  ReleaseSemaphore(FSemHandle, 1, nil);
end;
{$ELSE}
var
  sops: TSEMbuf;
begin
  sops.sem_num := 0;
  sops.sem_op := 1; // Signal operation
  sops.sem_flg := 0;

  semop(FSemId, @sops, 1);
end;
{$ENDIF}

{ TLocalSocket }

constructor TLocalSocket.Create;
begin
  inherited Create;
  FHandle := -1;
  FLastError := 0;
  FTimeout := 5000;
end;

destructor TLocalSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TLocalSocket.Close;
begin
  {$IFDEF UNIX}
  if FHandle >= 0 then fpClose(FHandle);
  {$ENDIF}
  {$IFDEF WINDOWS}
  if FHandle <> INVALID_HANDLE_VALUE then CloseHandle(FHandle);
  {$ENDIF}
  FHandle := -1;
end;

function TLocalSocket.Connect(const Path: string): Boolean;
{$IFDEF UNIX}
var
  Addr: TUnixSockAddr;
begin
  FHandle := fpSocket(AF_UNIX, SOCK_STREAM, 0);
  if FHandle < 0 then Exit(False);

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.family := AF_UNIX;
  StrPCopy(Addr.path, Path);

  Result := fpConnect(FHandle, @Addr, SizeOf(Addr)) = 0;
  if not Result then
    FLastError := fpGetErrno;
end;
{$ENDIF}

{$IFDEF WINDOWS}
begin
  FHandle := CreateFile(PChar(Path), GENERIC_READ or GENERIC_WRITE,
                        0, nil, OPEN_EXISTING, 0, 0);
  Result := FHandle <> INVALID_HANDLE_VALUE;
  if not Result then
    FLastError := GetLastError;
end;
{$ENDIF}

function TLocalSocket.SendBuffer(const Buffer: Pointer; Size: Integer): Integer;
{$IFDEF UNIX}
begin
  Result := fpSend(FHandle, Buffer, Size, 0);
  if Result < 0 then FLastError := fpGetErrno;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  Written: DWORD;
begin
  if not WriteFile(FHandle, Buffer^, Size, Written, nil) then
  begin
    FLastError := GetLastError;
    Result := -1;
  end
  else
    Result := Written;
end;
{$ENDIF}

function TLocalSocket.RecvBuffer(Buffer: Pointer; Size: Integer): Integer;
{$IFDEF UNIX}
var
  FDSet: TFDSet;
  TV: TTimeVal;
  StartTime, Elapsed: QWord;
  P: PByte;
  Received, R: Integer;
begin
  Result := -1;
  Received := 0;
  P := Buffer;
  StartTime := GetTickCount64;

  repeat
    fpFD_ZERO(FDSet);
    fpFD_SET(FHandle, FDSet);
    TV.tv_sec := 0;
    TV.tv_usec := 100000;

    if fpSelect(FHandle + 1, @FDSet, nil, nil, @TV) > 0 then
    begin
      R := fpRecv(FHandle, P, Size - Received, 0);
      if R <= 0 then
      begin
        FLastError := fpGetErrno;
        Exit(-1);
      end;
      Inc(Received, R);
      Inc(P, R);
    end;

    Elapsed := GetTickCount64 - StartTime;
  until (Received >= Size) or (Elapsed >= QWord(FTimeout));

  if Received = Size then
    Result := Received
  else
  begin
    FLastError := -2;
    Result := -1;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  Read: DWORD;
begin
  if not ReadFile(FHandle, Buffer^, Size, Read, nil) then
  begin
    FLastError := GetLastError;
    Result := -1;
  end
  else
    Result := Read;
end;
{$ENDIF}


procedure TLocalSocket.FlushRecvBuffer;
{$IFDEF UNIX}
var
  FDSet: TFDSet;
  TV: TTimeVal;
  TempBuf: array[0..1023] of Byte;
  R: Integer;
begin
  repeat
    fpFD_ZERO(FDSet);
    fpFD_SET(FHandle, FDSet);
    TV.tv_sec := 0;
    TV.tv_usec := 0;

    if fpSelect(FHandle + 1, @FDSet, nil, nil, @TV) > 0 then
    begin
      R := fpRecv(FHandle, @TempBuf, SizeOf(TempBuf), 0);
      if R <= 0 then Break;
    end
    else
      Break;
  until False;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  BytesAvailable: DWORD;
  TempBuf: array[0..1023] of Byte;
  Read: DWORD;
begin
  repeat
    if not PeekNamedPipe(FHandle, nil, 0, nil, @BytesAvailable, nil) then
      Break;

    if BytesAvailable = 0 then
      Break;

    if not ReadFile(FHandle, TempBuf[0], Min(BytesAvailable, SizeOf(TempBuf)), Read, nil) then
      Break;
  until False;
end;
{$ENDIF}


function TLocalSocket.SendString(const S: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (SendBuffer(@Len, SizeOf(Len)) = SizeOf(Len)) and
            (SendBuffer(PChar(S), Len) = Len);
end;

function TLocalSocket.RecvString: string;
var
  Len: Integer;
  Buf: PChar;
begin
  Result := '';
  if RecvBuffer(@Len, SizeOf(Len)) <> SizeOf(Len) then Exit;

  GetMem(Buf, Len + 1);
  try
    if RecvBuffer(Buf, Len) = Len then
    begin
      Buf[Len] := #0;
      Result := Buf;
    end;
  finally
    FreeMem(Buf);
  end;
end;

function TLocalSocket.GetLastErrorDescr: string;
{$IFDEF UNIX}
begin
  Result := SysErrorMessage(FLastError);
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  Msg: array[0..255] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
                nil, FLastError, 0, Msg, Length(Msg), nil);
  Result := Msg;
end;
{$ENDIF}

{ TLocalServerSocket }

constructor TLocalServerSocket.Create(const Path: string);
begin
  inherited Create;
  FPath := Path;
  FHandle := NilHandle;
  FTimeout := 5000;
end;

destructor TLocalServerSocket.Destroy;
begin
  {$IFDEF UNIX}
  if FHandle >= 0 then fpClose(FHandle);
  fpUnlink(FPath);
  {$ENDIF}
  {$IFDEF WINDOWS}
  if FHandle <> INVALID_HANDLE_VALUE then CloseHandle(FHandle);
  {$ENDIF}
  inherited Destroy;
end;

function TLocalServerSocket.Listen: Boolean;
{$IFDEF UNIX}
var
  Addr: TUnixSockAddr;
begin
  FHandle := fpSocket(AF_UNIX, SOCK_STREAM, 0);
  if FHandle < 0 then Exit(False);

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.family := AF_UNIX;
  StrPCopy(Addr.path, FPath);
  fpUnlink(FPath);

  if fpBind(FHandle, @Addr, SizeOf(Addr)) <> 0 then
  begin
    FLastError := fpGetErrno;
    Exit(False);
  end;

  Result := fpListen(FHandle, 5) = 0;
end;
{$ENDIF}

{$IFDEF WINDOWS}
begin
  Result := True; // pipe created in Accept
end;
{$ENDIF}

function TLocalServerSocket.Accept: TLocalSocket;
{$IFDEF UNIX}
var
  ClientHandle: THandle;
  FDSet: TFDSet;
  TV: TTimeVal;
begin
  Result := nil;

  fpFD_ZERO(FDSet);
  fpFD_SET(FHandle, FDSet);
  TV.tv_sec := FTimeout div 1000;
  TV.tv_usec := (FTimeout mod 1000) * 1000;

  if fpSelect(FHandle + 1, @FDSet, nil, nil, @TV) > 0 then
  begin
    ClientHandle := fpAccept(FHandle, nil, nil);
    if ClientHandle >= 0 then
    begin
      Result := TLocalSocket.Create;
      Result.FHandle := ClientHandle;
      Exit;
    end;
  end;

  FLastError := fpGetErrno;
  Result := nil;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  Overlapped: TOverlapped;
  Event: THandle;
  WaitResult: DWORD;

const
  PIPE_UNLIMITED_INSTANCES = 255;

begin
  Result := nil;
  FHandle := CreateNamedPipe(PChar(FPath), // es: '\\.\pipe\MyPipe'
              PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,
              PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,
              PIPE_UNLIMITED_INSTANCES,
              4096, 4096, 0, nil);

  if FHandle = NilHandle then // INVALID_HANDLE_VALUE then
  begin
    FLastError := GetLastError;
    Exit;
  end;

  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Event := CreateEvent(nil, True, False, nil);
  Overlapped.hEvent := Event;

  if not ConnectNamedPipe(FHandle, @Overlapped) then
  begin
    if GetLastError <> ERROR_IO_PENDING then
    begin
      FLastError := GetLastError;
      CloseHandle(FHandle);
      CloseHandle(Event);
      Exit;
    end;
  end;

  WaitResult := WaitForSingleObject(Event, FTimeout);
  CloseHandle(Event);

  if WaitResult = WAIT_OBJECT_0 then
  begin
    Result := TLocalSocket.Create;
    Result.FHandle := FHandle;
  end
  else
  begin
    FLastError := ERROR_TIMEOUT;
    CloseHandle(FHandle);
    Result := nil;
  end;
end;
{$ENDIF}

end.


