unit uSharedMemoryManagement;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF shmPOSIX}BaseUnix,{$ENDIF}{$IFDEF Windows} Windows{$ELSE} Unix{$ENDIF}, ipc;

type
  {$IFDEF Windows}
  TSharedMemory = record
    shmHandle: THandle;       // Windows handle from CreateFileMapping
    mem       : Pointer;
    size      : Integer;
    name      : String;       // identifier passed to CreateFileMapping
  end;
  {$ELSE}
    {$IFDEF shmPOSIX}
    TSharedMemory = record
      shmHandle: Integer;       // file descriptor o ID segment
      mem       : Pointer;
      size      : Integer;
      name      : String;       // /dev/shm/<name>
    end;
    {$ELSE}
    TSharedMemory = record      // SysV
      shmHandle: Integer;       // file descriptor o ID segment
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

implementation

uses
  {$IFDEF Windows}Windows,{$ENDIF}ULBLogger, LazFileUtils;

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
  SHM_MODE = &666; // S_IRUSR or S_IWUSR; // Permessi di lettura e scrittura
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
      // Attacco della memoria condivisa al nostro spazio di indirizzi
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
    // Rimozione della memoria condivisa
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

end.

