unit Toolkit_CAPI_CircularBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBCircularBuffer;

// --- Non-Thread-Safe Circular Buffer API ---
function CircularBuffer_Create(aSize: Cardinal): Pointer; export; cdecl;
procedure CircularBuffer_Destroy(aHandle: Pointer); export; cdecl;
function CircularBuffer_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function CircularBuffer_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function CircularBuffer_GetAvailableForRead(aHandle: Pointer): Cardinal; export; cdecl;
function CircularBuffer_GetAvailableForWrite(aHandle: Pointer): Cardinal; export; cdecl;
function CircularBuffer_FindPattern(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; export; cdecl;
function CircularBuffer_Peek(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean; export; cdecl;
function CircularBuffer_Seek(aHandle: Pointer; aOffset: Cardinal): Boolean; export; cdecl;
procedure CircularBuffer_Clear(aHandle: Pointer); export; cdecl;



// --- Thread-Safe Circular Buffer API ---
function CircularBufferTS_Create(aSize: Cardinal): Pointer; export; cdecl;
procedure CircularBufferTS_Destroy(aHandle: Pointer); export; cdecl;
function CircularBufferTS_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function CircularBufferTS_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function CircularBufferTS_GetAvailableForRead(aHandle: Pointer): Cardinal; export; cdecl;
function CircularBufferTS_GetAvailableForWrite(aHandle: Pointer): Cardinal; export; cdecl;
function CircularBufferTS_FindPattern(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; export; cdecl;
function CircularBufferTS_Peek(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean; export; cdecl;
function CircularBufferTS_Seek(aHandle: Pointer; aOffset: Cardinal): Boolean; export; cdecl;
procedure CircularBufferTS_Clear(aHandle: Pointer); export; cdecl;

implementation

// --- Non-Thread-Safe Implementation ---

function CircularBuffer_Create(aSize: Cardinal): Pointer; cdecl;
begin
  Result := nil;
  try
    Result := Pointer(TLBCircularBuffer.Create(aSize));
  except
    Result := nil;
  end;
end;

procedure CircularBuffer_Destroy(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    TLBCircularBuffer(aHandle).Free;
end;

function CircularBuffer_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).Write(aData, aCount);
end;

function CircularBuffer_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).Read(aData, aCount);
end;

function CircularBuffer_GetAvailableForRead(aHandle: Pointer): Cardinal; cdecl;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).AvailableForRead;
end;

function CircularBuffer_GetAvailableForWrite(aHandle: Pointer): Cardinal; cdecl;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).AvailableForWrite;
end;

function CircularBuffer_FindPattern(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; cdecl;
begin
  Result := -1;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).FindPattern(aPattern, aPatternSize, aOffset);
end;

function CircularBuffer_Peek(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal): Boolean; cdecl;
begin
  Result := False;

  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).Peek(aBuffer, aCount, aOffset);
end;

function CircularBuffer_Seek(aHandle: Pointer; aOffset: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBuffer(aHandle).Seek(aOffset);
end;

procedure CircularBuffer_Clear(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    TLBCircularBuffer(aHandle).Clear;
end;

// --- Thread-Safe Implementation ---

function CircularBufferTS_Create(aSize: Cardinal): Pointer; cdecl;
begin
  Result := nil;
  try
    Result := Pointer(TLBCircularBufferThreaded.Create(aSize));
  except
    Result := nil;
  end;
end;

procedure CircularBufferTS_Destroy(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    TLBCircularBufferThreaded(aHandle).Free;
end;

function CircularBufferTS_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Write(aData, aCount);
end;

function CircularBufferTS_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Read(aData, aCount);
end;

function CircularBufferTS_GetAvailableForRead(aHandle: Pointer): Cardinal; cdecl;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Buffer.AvailableForRead;
end;

function CircularBufferTS_GetAvailableForWrite(aHandle: Pointer): Cardinal; cdecl;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Buffer.AvailableForWrite;
end;

function CircularBufferTS_FindPattern(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; cdecl;
begin
  Result := -1;

  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).FindPattern(aPattern, aPatternSize, aOffset);
end;

function CircularBufferTS_Peek(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Peek(aBuffer, aCount, aOffset);
end;

function CircularBufferTS_Seek(aHandle: Pointer; aOffset: Cardinal): Boolean; cdecl;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Seek(aOffset);
end;

procedure CircularBufferTS_Clear(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    TLBCircularBufferThreaded(aHandle).Clear();
end;

end.
