unit uLBCircularBuffer;

interface

uses
  SysUtils, Classes, uMultiReference, uTimedoutCriticalSection, blcksock;

type
  { TLBCircularBuffer }

  TLBCircularBuffer = class(TMultiReferenceObject)
  private
    FData: PByte;
    FMemorySize: Cardinal;
    FReadPos: Cardinal;   // Posizione di lettura
    FWritePos: Cardinal;  // Posizione di scrittura
    FCount: Cardinal;     // Numero di bytes attualmente nel buffer
    function FindDWord(aDWord: Cardinal; aOffset: Cardinal): Integer;
    function FindWord(aWord: Word; aOffset: Cardinal): Integer;
    function FindQWord(aQWord: UInt64; aOffset: Cardinal): Integer;

  protected
    function GetAvailableForRead: Cardinal;
    function GetAvailableForWrite: Cardinal;
    function WrapPosition(aPos: Cardinal): Cardinal; inline;

  public
    constructor Create(aSize: Cardinal); reintroduce; virtual;
    destructor Destroy; override;

    { Gestione buffer }
    procedure Clear;
    function ResizeBuffer(aNewSize: Cardinal; aPreserveData: Boolean = True): Boolean;

    { Lettura }
    function Read(aBuffer: Pointer; aCount: Cardinal): Boolean;
    function Peek(aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean;
    function PeekByte(aOffset: Cardinal): Byte;
    function Skip(aCount: Cardinal): Boolean;
    function Seek(aOffset: Cardinal): Boolean;

    { Scrittura }
    function Write(aBuffer: Pointer; aCount: Cardinal): Boolean;
    function WriteFromSocket(aSocket: TTCPBlockSocket; aMaxCount: Cardinal): Integer;

    { Utilità }
    function FindPattern(aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer;
    function FindByte(aByte: Byte; aOffset: Cardinal): Integer;

    { Proprietà }
    property MemorySize: Cardinal read FMemorySize;
    property AvailableForRead: Cardinal read GetAvailableForRead;
    property AvailableForWrite: Cardinal read GetAvailableForWrite;
  end;



type

  { TLBCircularBufferThreaded }

  TLBCircularBufferThreaded = class(TMultiReferenceObject)
  private
    FBuffer: TLBCircularBuffer;
    FBufferCS: TTimedOutCriticalSection;
  public
    constructor Create(aSize: Cardinal); reintroduce;
    destructor Destroy; override;

    function Read(aBuffer: Pointer; aCount: Cardinal): Boolean;
    function Write(aBuffer: Pointer; aCount: Cardinal): Boolean;
    function Peek(aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean;
    function PeekByte(aOffset: Cardinal): Byte;
    function Skip(aCount: Cardinal): Boolean;
    function Seek(aOffset: Cardinal): Boolean;
    function FindPattern(aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer;
    function WriteFromSocket(aSocket: TTCPBlockSocket; aMaxCount: Cardinal): Integer;

    function Clear(): Boolean;

    property Buffer: TLBCircularBuffer read FBuffer;
    property BufferCS: TTimedOutCriticalSection read FBufferCS;
  end;


implementation

uses
  ULBLogger;

{ TLBCircularBuffer }

constructor TLBCircularBuffer.Create(aSize: Cardinal);
begin
  inherited Create;

  if aSize = 0 then
  begin
    LBLogger.Write(6, 'TLBCircularBuffer.Create', lmt_Debug, 'Buffer size cannot be zero');
    raise Exception.Create('Buffer size cannot be zero');
  end;

  FMemorySize := aSize;
  FData := AllocMem(FMemorySize);
  FReadPos := 0;
  FWritePos := 0;
  FCount := 0;

end;

destructor TLBCircularBuffer.Destroy;
begin
  LBLogger.Write(6, 'TLBCircularBuffer.Destroy', lmt_Debug, Format('Destroying buffer of size %d', [FMemorySize]));

  Self.Clear;
  FreeMemAndNil(FData);

  inherited Destroy;
end;

function TLBCircularBuffer.WrapPosition(aPos: Cardinal): Cardinal; inline;
begin
  // Se il buffer è una potenza di 2, usa AND per efficienza
  if (FMemorySize and (FMemorySize - 1)) = 0 then
    Result := aPos and (FMemorySize - 1)
  else
  begin
    if aPos >= FMemorySize then
      Result := aPos mod FMemorySize
    else
      Result := aPos;
  end;
end;

function TLBCircularBuffer.GetAvailableForRead: Cardinal;
begin
  Result := FCount;
end;

function TLBCircularBuffer.GetAvailableForWrite: Cardinal;
begin
  Result := FMemorySize - GetAvailableForRead;
end;

procedure TLBCircularBuffer.Clear;
begin
  FReadPos := 0;
  FWritePos := 0;
  FCount := 0;
  FillChar(FData^, FMemorySize, 0);
end;

function TLBCircularBuffer.ResizeBuffer(aNewSize: Cardinal; aPreserveData: Boolean): Boolean;
var
  _NewData: PByte = nil;
  _BytesToCopy: Cardinal;
begin
  Result := False;

  if aNewSize > 0 then
  begin
    _NewData := AllocMem(aNewSize);

    if aPreserveData and (FCount > 0) then
    begin
      _BytesToCopy := FCount;
      if _BytesToCopy > aNewSize then
        _BytesToCopy := aNewSize;

      Self.Read(_NewData, _BytesToCopy);
    end;

    FreeMem(FData);
    FData := _NewData;
    _NewData := nil;
    FMemorySize := aNewSize;
    FReadPos := 0;

    if aPreserveData then
    begin
      FWritePos := _BytesToCopy;
      FCount := _BytesToCopy;
    end
    else begin
      FWritePos := 0;
      FCount := 0;
    end;

    Result := True;
  end
  else
    LBLogger.Write(1, 'TLBCircularBuffer.ResizeBuffer', lmt_Error, 'New size cannot be zero');

end;

function TLBCircularBuffer.Read(aBuffer: Pointer; aCount: Cardinal): Boolean;
var
  _FirstChunk, _SecondChunk: Cardinal;
  _Dest: PByte;
begin
  Result := False;

  if (aCount > 0) and (aBuffer <> nil) and (FCount >= aCount) then
  begin
    _Dest := PByte(aBuffer);

    // Calcola quanto leggere prima del wrap
    if FReadPos + aCount <= FMemorySize then // Lettura in un solo blocco
      Move(FData[FReadPos], _Dest^, aCount)
    else begin
      // Lettura in due blocchi (wrap around)
      _FirstChunk := FMemorySize - FReadPos;
      _SecondChunk := aCount - _FirstChunk;

      Move(FData[FReadPos], _Dest^, _FirstChunk);
      Move(FData[0], (_Dest + _FirstChunk)^, _SecondChunk);
    end;

    FReadPos := WrapPosition(FReadPos + aCount);
    Dec(FCount, aCount);
    Result := True;

    if FCount = 0 then
    begin
      FReadPos := 0;
      FWritePos := 0;
    end;
  end;
end;

function TLBCircularBuffer.Write(aBuffer: Pointer; aCount: Cardinal): Boolean;
var
  FirstChunk, SecondChunk: Cardinal;
  Src: PByte;
begin
  Result := False;

  if (aCount > 0) and (aBuffer <> nil) and (Self.GetAvailableForWrite >= aCount) then
  begin

    Src := PByte(aBuffer);

    // Calcola quanto scrivere prima del wrap
    if FWritePos + aCount <= FMemorySize then // Scrittura in un solo blocco
      Move(Src^, FData[FWritePos], aCount)
    else begin
      // Scrittura in due blocchi (wrap around)
      FirstChunk := FMemorySize - FWritePos;
      SecondChunk := aCount - FirstChunk;

      Move(Src^, FData[FWritePos], FirstChunk);
      Move((Src + FirstChunk)^, FData[0], SecondChunk);
    end;

    FWritePos := Self.WrapPosition(FWritePos + aCount);
    Inc(FCount, aCount);
    Result := True;
  end;
end;

function TLBCircularBuffer.Peek(aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal): Boolean;
var
  _OrigReadPos: Cardinal;
begin
  Result := False;

  if FCount >= (aOffset + aCount) then
  begin
    _OrigReadPos := FReadPos;
    FReadPos := WrapPosition(FReadPos + aOffset);
    Result := Self.Read(aBuffer, aCount);

    // Ripristina posizione originale
    FReadPos := _OrigReadPos;
    Inc(FCount, aCount); // Ripristina il count
  end;
end;

function TLBCircularBuffer.PeekByte(aOffset: Cardinal): Byte;
var
  _pos: Cardinal;
begin
  Result := 0;
  if aOffset < FCount then
  begin
    _pos := WrapPosition(FReadPos + aOffset);
    Result := FData[_pos];
  end;
end;


function TLBCircularBuffer.Skip(aCount: Cardinal): Boolean;
begin
  Result := False;

  if FCount >= aCount then
  begin
    FReadPos := WrapPosition(FReadPos + aCount);
    Dec(FCount, aCount);
    Result := True;

    if FCount = 0 then
    begin
      FReadPos := 0;
      FWritePos := 0;
    end;
  end;
end;

function TLBCircularBuffer.Seek(aOffset: Cardinal): Boolean;
begin
  Result := False;
  if aOffset < FCount then
  begin
    FReadPos := WrapPosition(FReadPos + aOffset);
    Dec(FCount, aOffset);
    Result := True;

    if FCount = 0 then
    begin
      FReadPos := 0;
      FWritePos := 0;
    end;
  end;
end;

function TLBCircularBuffer.FindPattern(aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer;
var
  SearchPos: Cardinal;
  i, SearchCount: Cardinal;
  CurrentPos: Cardinal;

  function ComparePattern(ABufferPos: Cardinal): Boolean;
  var
    BufPtr, PatPtr: PByte;
    j: Cardinal;
  begin
    // Se il pattern è tutto in un blocco contiguo, usa confronti ottimizzati
    if ABufferPos + aPatternSize <= FMemorySize then
    begin
      BufPtr := @FData[ABufferPos];
      PatPtr := aPattern;

      // Ottimizzazioni inline per casi comuni
      case aPatternSize of
        1: Result := BufPtr^ = PatPtr^;
        2: Result := PWord(BufPtr)^ = PWord(PatPtr)^;
        4: Result := PCardinal(BufPtr)^ = PCardinal(PatPtr)^;
        8: Result := PUInt64(BufPtr)^ = PUInt64(PatPtr)^;
      else
        // Per pattern più grandi, confronti a blocchi
        Result := CompareMem(BufPtr, PatPtr, aPatternSize);
      end;
    end
    else begin
      // Pattern attraversa il boundary - confronto byte per byte
      Result := True;
      for j := 0 to aPatternSize - 1 do
      begin
        if FData[WrapPosition(ABufferPos + j)] <> aPattern[j] then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

begin
  Result := -1;

  if (aPattern <> nil) and (aPatternSize > 0) and (aOffset + aPatternSize <= FCount) then
  begin

    SearchPos := WrapPosition(FReadPos + aOffset);
    SearchCount := FCount - aOffset - aPatternSize + 1;

    for i := 0 to SearchCount - 1 do
    begin
      CurrentPos := WrapPosition(SearchPos + i);
      if ComparePattern(CurrentPos) then
      begin
        Result := Integer(aOffset + i);
        Exit;
      end;
    end;
  end;
end;

// Metodi di convenienza per casi comuni
function TLBCircularBuffer.FindByte(aByte: Byte; aOffset: Cardinal): Integer;
begin
  Result := Self.FindPattern(@aByte, 1, aOffset);
end;

function TLBCircularBuffer.FindWord(aWord: Word; aOffset: Cardinal): Integer;
begin
  Result := Self.FindPattern(@aWord, 2, aOffset);
end;

function TLBCircularBuffer.FindDWord(aDWord: Cardinal; aOffset: Cardinal): Integer;
begin
  Result := Self.FindPattern(@aDWord, 4, aOffset);
end;

function TLBCircularBuffer.FindQWord(aQWord: UInt64; aOffset: Cardinal): Integer;
begin
  Result := Self.FindPattern(@aQWord, 8, aOffset);
end;

function TLBCircularBuffer.WriteFromSocket(aSocket: TTCPBlockSocket; aMaxCount: Cardinal): Integer;
var
  _BytesToRead, _AvailableToWrite, _FirstChunk, _SecondChunk, _BytesRead: Cardinal;
begin
  Result := 0;

  if Assigned(aSocket) then
  begin

    _AvailableToWrite := Self.GetAvailableForWrite;
    if _AvailableToWrite > 0 then
    begin
      _BytesToRead := aMaxCount;
      if _BytesToRead > _AvailableToWrite then
        _BytesToRead := _AvailableToWrite;

      if (FWritePos + _BytesToRead <= FMemorySize) then
      begin
        // Contiguous write
        _BytesRead := aSocket.RecvBuffer(@FData[FWritePos], _BytesToRead);
        if _BytesRead > 0 then
        begin
          FWritePos := Self.WrapPosition(FWritePos + _BytesRead);
          Inc(FCount, _BytesRead);
          Result := _BytesRead;
        end;
      end
      else begin
        // Wrap-around write
        _FirstChunk := FMemorySize - FWritePos;
        _BytesRead := aSocket.RecvBuffer(@FData[FWritePos], _FirstChunk);

        if _BytesRead > 0 then
        begin
          Inc(FCount, _BytesRead);
          Result := _BytesRead;
          FWritePos := self.WrapPosition(FWritePos + _BytesRead);

          if _BytesRead = _FirstChunk then // We filled the first chunk, try for the second
          begin
            _SecondChunk := _BytesToRead - _FirstChunk;
            if _SecondChunk > 0 then
            begin
              _BytesRead := aSocket.RecvBuffer(@FData[FWritePos], _SecondChunk);
              if _BytesRead > 0 then
              begin
                Inc(FCount, _BytesRead);
                Inc(Result, _BytesRead);
                FWritePos := self.WrapPosition(FWritePos + _BytesRead);
              end;
            end;
          end;
        end;
      end;
    end
    else
      LBLogger.Write(1, 'TLBCircularBuffer.WriteFromSocket', lmt_Warning, 'Buffer is full!');
  end
  else
    LBLogger.Write(1, 'TLBCircularBuffer.WriteFromSocket', lmt_Warning, 'No socket available!');
end;

{ TLBCircularBufferThreaded }

constructor TLBCircularBufferThreaded.Create(aSize: Cardinal);
begin
  inherited Create;
  FBuffer := TLBCircularBuffer.Create(aSize);
  FBufferCS := TTimedOutCriticalSection.Create;
end;

destructor TLBCircularBufferThreaded.Destroy;
begin
  FreeAndNil(FBufferCS);
  FreeAndNIl(FBuffer);
  inherited Destroy;
end;

function TLBCircularBufferThreaded.Read(aBuffer: Pointer; aCount: Cardinal): Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Read') then
  begin
    try
      Result := FBuffer.Read(aBuffer, aCount);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.Write(aBuffer: Pointer; aCount: Cardinal): Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Write') then
  begin
    try
      Result := FBuffer.Write(aBuffer, aCount);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.Peek(aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal): Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Peek') then
  begin
    try
      Result := FBuffer.Peek(aBuffer, aCount, aOffset);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.PeekByte(aOffset: Cardinal): Byte;
begin
  Result := 0;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.PeekByte') then
  begin
    try
      Result := FBuffer.PeekByte(aOffset);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.Skip(aCount: Cardinal): Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Skip') then
  begin
    try
      Result := FBuffer.Skip(aCount);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.Seek(aOffset: Cardinal): Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Seek') then
  begin
    try
      Result := FBuffer.Seek(aOffset);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.FindPattern(aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer;
begin
  Result := -1;

  if FBufferCS.Acquire('TLBCircularBufferThreaded.FindPattern') then
  begin
    try
      Result := FBuffer.FindPattern(aPattern, aPatternSize, aOffset);
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.Clear: Boolean;
begin
  Result := False;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.Clear') then
  begin
    try
      FBuffer.Clear();
      Result := True;
    finally
      FBufferCS.Release;
    end;
  end;
end;

function TLBCircularBufferThreaded.WriteFromSocket(aSocket: TTCPBlockSocket; aMaxCount: Cardinal): Integer;
begin
  Result := -1;
  if FBufferCS.Acquire('TLBCircularBufferThreaded.WriteFromSocket') then
  begin
    try
      Result := FBuffer.WriteFromSocket(aSocket, aMaxCount);
    finally
      FBufferCS.Release;
    end;
  end;
end;

end.

