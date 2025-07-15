unit uSharedMemoryManagement;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF shmPOSIX}BaseUnix,{$ENDIF} Unix, ipc;

type
  TSharedMemory = record
    shmHandle: Integer;
    mem : Pointer;
    size : Integer;
    {$IFDEF shmPOSIX}
    name : String;
    {$ELSE}
    key : TKey;
    {$ENDIF}
  end;
  pSharedMemory = ^TSharedMemory;


{$IFDEF shmPOSIX}
function createSharedMemory(aShmInfo: pSharedMemory): boolean;
function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
{$ELSE}
function createSharedMemory(aShmInfo: pSharedMemory): Boolean;
function closeSharedMemory(aShmInfo: pSharedMemory): Boolean;
function AllocateSharedMemory(aKey: TKey; aSize: Integer): pSharedMemory;
{$ENDIF}


implementation

uses
  ULBLogger, LazFileUtils;


const
  SHM_MODE = &666; // S_IRUSR or S_IWUSR; // Permessi di lettura e scrittura

  {$IFDEF shmPOSIX}
  shmFolder = String('/dev/shm/');

  rtlib = 'rt';
  clib = 'c';

  function shm_open(name: PChar; oflag: LongInt; mode: mode_t): cint; cdecl; external rtlib name 'shm_open';
  function shm_unlink(name: PChar): cint; cdecl; external rtlib name 'shm_unlink';
  function ftruncate(fd: cint; length: off_t): cint; cdecl; external clib name 'ftruncate';
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
    // Rimozione della memoria condivisa (facoltativo, dipende dai requisiti)
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

end.

