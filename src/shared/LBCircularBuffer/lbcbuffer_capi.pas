unit lbcbuffer_capi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBCircularBuffer, ULBLogger;

procedure LBCB_Initialize(aLogFileName: PChar; aLogLevel: Integer); export; cdecl;
procedure LBCB_Finalize; export; cdecl;
function LBCB_Create(aSize: Cardinal): Pointer; export; cdecl;
procedure LBCB_Destroy(aHandle: Pointer); export; cdecl;
function LBCB_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function LBCB_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; export; cdecl;
function LBCB_GetAvailableForRead(aHandle: Pointer): Cardinal; export; cdecl;
function LBCB_GetAvailableForWrite(aHandle: Pointer): Cardinal; export; cdecl;
procedure LBCB_Clear(aHandle: Pointer); export; cdecl;

implementation

procedure LBCB_Initialize(aLogFileName: PChar; aLogLevel: Integer);
var
  LogFileName: string;
begin
  LogFileName := StrPas(aLogFileName);
  if (LogFileName <> '') and (aLogLevel > 0) then
  begin
    InitLogger(aLogLevel, LogFileName, False, False);
  end;
end;

procedure LBCB_Finalize;
begin
  ReleaseLogger;
end;

function LBCB_Create(aSize: Cardinal): Pointer;
var
  Buffer: TLBCircularBufferThreaded;
begin
  Result := nil;
  try
    Buffer := TLBCircularBufferThreaded.Create(aSize);
    Result := Pointer(Buffer);
  except
    on E: Exception do
      // In a real library, you might want to log this or handle it differently
      Result := nil;
  end;
end;

procedure LBCB_Destroy(aHandle: Pointer);
begin
  if aHandle <> nil then
    TLBCircularBufferThreaded(aHandle).Free;
end;

function LBCB_Write(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Write(aData, aCount);
end;

function LBCB_Read(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean;
begin
  Result := False;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Read(aData, aCount);
end;

function LBCB_GetAvailableForRead(aHandle: Pointer): Cardinal;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Buffer.AvailableForRead;
end;

function LBCB_GetAvailableForWrite(aHandle: Pointer): Cardinal;
begin
  Result := 0;
  if aHandle <> nil then
    Result := TLBCircularBufferThreaded(aHandle).Buffer.AvailableForWrite;
end;

procedure LBCB_Clear(aHandle: Pointer);
begin
  if aHandle <> nil then
    TLBCircularBufferThreaded(aHandle).BufferCS.Acquire('LBCB_Clear');
    try
      TLBCircularBufferThreaded(aHandle).Buffer.Clear;
    finally
      TLBCircularBufferThreaded(aHandle).BufferCS.Release;
    end;
end;

end.
