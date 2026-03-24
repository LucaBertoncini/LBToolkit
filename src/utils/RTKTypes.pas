unit RTKTypes;

{
  Unit condivisa per i moduli RTK.
  Contiene:
  - TCorrectionBuffer : buffer RTCM3 thread-safe usato da tutti i rover
  - Tipi comuni (TRTKStatus, ecc.) condivisi tra rover NMEA e UBX
}

interface

uses
  SysUtils, SyncObjs;

type

  { ── Stato RTK ────────────────────────────────────────────────────────── }

  TRTKStatus = (rtkNone, rtkFloat, rtkFixed);

  { ── Callback generici ────────────────────────────────────────────────── }

  TOnRTKError = procedure(Sender: TObject; const Msg: string) of object;

  { ── Buffer correzioni RTCM3 thread-safe ─────────────────────────────── }
  {                                                                         }
  { Semantica "last-write-wins": se arrivano correzioni più velocemente di  }
  { quanto il rover le consumi, viene mantenuto solo il blocco più recente. }
  { Questo è il comportamento corretto per RTCM3 (i messaggi vecchi sono    }
  { inutili; meglio mandare i nuovi).                                       }

  TCorrectionBuffer = class
  private
    FLock  : TCriticalSection;
    FData  : TBytes;
    FLen   : Integer;
    FDirty : Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    { Carica un nuovo blocco di correzioni (sovrascrive eventuale blocco
      precedente non ancora consumato). Thread-safe. }
    procedure Push(const Data: array of Byte; Len: Integer); overload;
    procedure Push(const Data: TBytes); overload;

    { Preleva il blocco pendente in Dest e azzera il flag dirty.
      Restituisce il numero di byte copiati (0 se nessun dato pendente).
      Thread-safe. }
    function  Flush(out Dest: TBytes): Integer;
  end;

implementation

{ ══ TCorrectionBuffer ═══════════════════════════════════════════════════════ }

constructor TCorrectionBuffer.Create;
begin
  inherited Create;
  FLock  := TCriticalSection.Create;
  FLen   := 0;
  FDirty := False;
end;

destructor TCorrectionBuffer.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TCorrectionBuffer.Push(const Data: array of Byte; Len: Integer);
begin
  if Len <= 0 then Exit;
  FLock.Acquire;
  try
    if Len > Length(FData) then
      SetLength(FData, Len);
    Move(Data[0], FData[0], Len);
    FLen   := Len;
    FDirty := True;
  finally
    FLock.Release;
  end;
end;

procedure TCorrectionBuffer.Push(const Data: TBytes);
var
  Len: Integer;
begin
  Len := Length(Data);
  if Len <= 0 then Exit;
  FLock.Acquire;
  try
    if Len > Length(FData) then
      SetLength(FData, Len);
    Move(Data[0], FData[0], Len);
    FLen   := Len;
    FDirty := True;
  finally
    FLock.Release;
  end;
end;

function TCorrectionBuffer.Flush(out Dest: TBytes): Integer;
begin
  Result := 0;
  { Controllo rapido senza lock — FDirty è scritto solo da Push (owner esterno)
    e letto qui. Il doppio-check dopo il lock previene race condition. }
  if not FDirty then Exit;
  FLock.Acquire;
  try
    if not FDirty then Exit;
    SetLength(Dest, FLen);
    Move(FData[0], Dest[0], FLen);
    Result := FLen;
    FDirty := False;
  finally
    FLock.Release;
  end;
end;

end.
