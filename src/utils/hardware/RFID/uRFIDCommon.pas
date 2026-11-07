unit uRFIDCommon;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  uTimedoutCriticalSection,
  uEventsManager,
  uLBLogger;

type

  TRFIDReaderType =
  (
    rrtUnknown      = 0,
    rrtImpinjR700   = 1,
    rrtZebraFXR90   = 2
  );

  TRFIDTagEvent = procedure(Sender: TObject) of object;

  TRFIDAntennaInfo = record
    Port     : Byte;
    PeakRSSI : SmallInt;   // centesimi di dBm oppure RSSI normalizzato dal driver specifico
    Count    : Integer;
  end;

  TRFIDAntennaPreset = class(TObject)
    Description : string;  // etichetta leggibile per log e UI

    function Clone: TRFIDAntennaPreset; virtual; abstract;
  end;

  TRFIDAntennaPresetArray = array of TRFIDAntennaPreset;

  { TRFIDFAntennaPresets }

  TRFIDFAntennaPresets = class(TObject)
    strict protected
      FPresets : TRFIDAntennaPresetArray;

    public
      constructor Create; virtual;
      destructor Destroy; override;

      property Items: TRFIDAntennaPresetArray read FPresets;
  end;
  TRFIDAntennaPresetsClass = class of TRFIDFAntennaPresets;

  TRFIDAntennasInfo = array of TRFIDAntennaInfo;

  TRFIDAntennasInfoHelper = type helper for TRFIDAntennasInfo
    function UpdateValues(anAntenna: Byte; aRSSI: SmallInt): Boolean;
    function AsString: String;
    function GetRSSIPeak(out aPeak: SmallInt; out anAntenna: Byte): Boolean;
  end;

  TRFIDTagData = record
    EPC       : String;
    FirstSeen : TDateTime;
    LastSeen  : TDateTime;
    Count     : Cardinal;
    Antennas  : TRFIDAntennasInfo;
  end;

  PRFIDTagData = ^TRFIDTagData;

  TRFIDTag = class(TObject)
  strict private
    FData          : TRFIDTagData;
    FCS            : TTimedOutCriticalSection;
    FEventsManager : TEventsManager;

  public
    constructor Create(const AEPC: String;
                       AFirstSeen: TDateTime;
                       AAntenna: Byte;
                       ARSSI: SmallInt);

    destructor Destroy; override;

    procedure Update(AAntenna: Byte;
                     ARSSI: SmallInt;
                     ALastSeen: TDateTime);

    function GetDataCopy: TRFIDTagData;

    function Lock: Boolean;
    procedure Unlock;
    function InternalData: PRFIDTagData;

    property EPC: String read FData.EPC;
    property EventsManager: TEventsManager read FEventsManager;

    const
      cEvent_TagUpdated = String('01');
  end;

  TRFIDTagList = class(TObject)
  strict private
    FIndex         : TStringList;
    FCS            : TTimedOutCriticalSection;
    FEventsManager : TEventsManager;

    function get_Count: Integer;
    function get_Tag(anIdx: Integer): TRFIDTag;

  public
    constructor Create;
    destructor Destroy; override;

    function GetOrCreate(const AEPC: String;
                         AAntenna: Byte;
                         ARSSI: SmallInt;
                         ATime: TDateTime;
                         out AIsNew: Boolean): TRFIDTag;

    function Find(const AEPC: String): TRFIDTag;

    function Lock: Boolean;
    procedure Unlock;

    procedure Clear;

    property Count: Integer read get_Count;
    property Tags[anIdx: Integer]: TRFIDTag read get_Tag;

    property EventsManager: TEventsManager read FEventsManager;

    const
      cEvent_TagAdded   = String('01');
      cEvent_TagChanged = String('02');
  end;

  { TRFIDManager }

  TRFIDManager = class(TObject)
  strict protected
    FReaderType    : TRFIDReaderType;
    FTagList       : TRFIDTagList;
    FEventsManager : TEventsManager;


    procedure NewTagAdded(Sender: TObject);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure addAntenna(aPort: Integer; aDBm: Integer; aPreset: TRFIDAntennaPreset); virtual; abstract;

    function StartInventory(): Boolean; virtual; abstract;
    function StopInventory(): Boolean; virtual; abstract;

    property ReaderType: TRFIDReaderType read FReaderType;
    property TagList: TRFIDTagList read FTagList;
    property EventsManager: TEventsManager read FEventsManager;

    const
      cEvent_NewTag          = String('01');  // forward di TRFIDTagList.cEvent_TagAdded
      cEvent_ReaderConnected = String('02');

  end;

implementation

{ TRFIDFAntennaPresets }

constructor TRFIDFAntennaPresets.Create;
begin
  inherited Create;
end;

destructor TRFIDFAntennaPresets.Destroy;
var
  i : Integer;

begin
  for i := 0 to High(FPresets) do
    FPresets[i].Free;

  SetLength(FPresets, 0);

  inherited Destroy;
end;

{ TRFIDAntennasInfoHelper }

function TRFIDAntennasInfoHelper.UpdateValues(anAntenna: Byte;
  aRSSI: SmallInt): Boolean;
var
  i : Integer;
  L : Integer;
begin
  Result := False;

  for i := 0 to High(Self) do
  begin
    if Self[i].Port = anAntenna then
    begin
      Inc(Self[i].Count);

      if (Self[i].Count = 1) or (aRSSI > Self[i].PeakRSSI) then
        Self[i].PeakRSSI := aRSSI;

      Result := True;
      Exit;
    end;
  end;

  L := Length(Self);
  SetLength(Self, L + 1);

  Self[L].Port     := anAntenna;
  Self[L].PeakRSSI := aRSSI;
  Self[L].Count    := 1;

  Result := True;
end;

function TRFIDAntennasInfoHelper.AsString: String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to High(Self) do
  begin
    if Result <> '' then
      Result := Result + ', ';

    Result := Result + Format('A%d (%d)', [Self[i].Port, Self[i].Count]);
  end;
end;

function TRFIDAntennasInfoHelper.GetRSSIPeak(out aPeak: SmallInt;
  out anAntenna: Byte): Boolean;
var
  i: Integer;
begin
  Result := False;
  aPeak := Low(SmallInt);
  anAntenna := 0;

  for i := 0 to High(Self) do
  begin
    if (not Result) or (Self[i].PeakRSSI > aPeak) then
    begin
      aPeak := Self[i].PeakRSSI;
      anAntenna := Self[i].Port;
      Result := True;
    end;
  end;
end;

{ TRFIDTag }

constructor TRFIDTag.Create(const AEPC: String; AFirstSeen: TDateTime; AAntenna: Byte; ARSSI: SmallInt);
begin
  inherited Create;

  FCS := TTimedOutCriticalSection.Create;
  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(TRFIDTag.cEvent_TagUpdated);

  FillChar(FData, SizeOf(FData), 0);

  FData.EPC       := AEPC;
  FData.FirstSeen := AFirstSeen;
  FData.LastSeen  := AFirstSeen;
  FData.Count     := 1;

  SetLength(FData.Antennas, 0);
  FData.Antennas.UpdateValues(AAntenna, ARSSI);
end;

destructor TRFIDTag.Destroy;
begin
  try
    SetLength(FData.Antennas, 0);
    FreeAndNil(FEventsManager);
    FreeAndNil(FCS);
  except
    on E: Exception do
      LBLogger.Write(1, 'TRFIDTag.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

procedure TRFIDTag.Update(AAntenna: Byte; ARSSI: SmallInt;
  ALastSeen: TDateTime);
begin
  if FCS.Acquire('TRFIDTag.Update') then
  begin
    try
      FData.LastSeen := ALastSeen;
      Inc(FData.Count);
      FData.Antennas.UpdateValues(AAntenna, ARSSI);
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTag.Update', lmt_Error, E.Message);
    end;

    FCS.Release;

    FEventsManager.RaiseEvent(cEvent_TagUpdated, Self);
  end;
end;

function TRFIDTag.GetDataCopy: TRFIDTagData;
begin
  FillChar(Result, SizeOf(Result), 0);

  if FCS.Acquire('TRFIDTag.GetDataCopy') then
  begin
    try
      Result.EPC       := FData.EPC;
      Result.FirstSeen := FData.FirstSeen;
      Result.LastSeen  := FData.LastSeen;
      Result.Count     := FData.Count;
      Result.Antennas  := Copy(FData.Antennas, 0, Length(FData.Antennas));
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTag.GetDataCopy', lmt_Error, E.Message);
    end;

    FCS.Release;
  end;
end;

function TRFIDTag.Lock: Boolean;
begin
  Result := FCS.Acquire('TRFIDTag.Lock');
end;

procedure TRFIDTag.Unlock;
begin
  FCS.Release;
end;

function TRFIDTag.InternalData: PRFIDTagData;
begin
  Result := @FData;
end;

{ TRFIDTagList }

constructor TRFIDTagList.Create;
begin
  inherited Create;

  FCS := TTimedOutCriticalSection.Create;
  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_TagAdded);
  FEventsManager.AddEvent(cEvent_TagChanged);

  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupError;
  FIndex.CaseSensitive := False;
end;

destructor TRFIDTagList.Destroy;
begin
  try
    Self.Clear;
    FreeAndNil(FIndex);
    FreeAndNil(FEventsManager);
    FreeAndNil(FCS);
  except
    on E: Exception do
      LBLogger.Write(1, 'TRFIDTagList.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

function TRFIDTagList.get_Count: Integer;
begin
  Result := 0;

  if FCS.Acquire('TRFIDTagList.get_Count') then
  begin
    try
      Result := FIndex.Count;
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTagList.get_Count', lmt_Error, E.Message);
    end;

    FCS.Release;
  end;
end;

function TRFIDTagList.get_Tag(anIdx: Integer): TRFIDTag;
begin
  Result := nil;

  if FCS.Acquire('TRFIDTagList.get_Tag') then
  begin
    try
      if (anIdx >= 0) and (anIdx < FIndex.Count) then
        Result := TRFIDTag(FIndex.Objects[anIdx]);
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTagList.get_Tag', lmt_Error, E.Message);
    end;

    FCS.Release;
  end;
end;

function TRFIDTagList.GetOrCreate(const AEPC: String;
  AAntenna: Byte; ARSSI: SmallInt; ATime: TDateTime;
  out AIsNew: Boolean): TRFIDTag;
var
  Idx : Integer;
begin
  Result := nil;
  AIsNew := False;

  if Trim(AEPC) = '' then
    Exit;

  if FCS.Acquire('TRFIDTagList.GetOrCreate') then
  begin
    try
      Idx := FIndex.IndexOf(AEPC);

      if Idx < 0 then
      begin
        Result := TRFIDTag.Create(AEPC, NowUTC(), AAntenna, ARSSI);
        FIndex.AddObject(AEPC, Result);
        AIsNew := True;
      end
      else
        Result := TRFIDTag(FIndex.Objects[Idx]);

    except
      on E: Exception do
      begin
        Result := nil;
        LBLogger.Write(1, 'TRFIDTagList.GetOrCreate', lmt_Error, E.Message);
      end;
    end;

    FCS.Release;
  end;

  if Result <> nil then
  begin
    if AIsNew then
      FEventsManager.RaiseEvent(cEvent_TagAdded, Result)
    else begin
      Result.Update(AAntenna, ARSSI, NowUTC());
      FEventsManager.RaiseEvent(cEvent_TagChanged, Result);
    end;
  end;
end;

function TRFIDTagList.Find(const AEPC: String): TRFIDTag;
var
  Idx: Integer;
begin
  Result := nil;

  if FCS.Acquire('TRFIDTagList.Find') then
  begin
    try
      Idx := FIndex.IndexOf(AEPC);
      if Idx >= 0 then
        Result := TRFIDTag(FIndex.Objects[Idx]);
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTagList.Find', lmt_Error, E.Message);
    end;

    FCS.Release;
  end;
end;

function TRFIDTagList.Lock: Boolean;
begin
  Result := FCS.Acquire('TRFIDTagList.Lock');
end;

procedure TRFIDTagList.Unlock;
begin
  FCS.Release;
end;

procedure TRFIDTagList.Clear;
var
  i: Integer;
begin
  if FCS.Acquire('TRFIDTagList.Clear') then
  begin
    try
      for i := 0 to FIndex.Count - 1 do
        FIndex.Objects[i].Free;

      FIndex.Clear;
    except
      on E: Exception do
        LBLogger.Write(1, 'TRFIDTagList.Clear', lmt_Error, E.Message);
    end;

    FCS.Release;
  end;
end;

{ TRFIDManager }

procedure TRFIDManager.NewTagAdded(Sender: TObject);
begin
  FEventsManager.RaiseEvent(cEvent_NewTag, Sender);
end;

constructor TRFIDManager.Create;
begin
  inherited Create;

  FReaderType := rrtUnknown;

  FEventsManager := TEventsManager.Create(Self);
  FEventsManager.AddEvent(cEvent_NewTag);
  FEventsManager.AddEvent(cEvent_ReaderConnected);

  FTagList := TRFIDTagList.Create;
  FTagList.EventsManager.AddEventListener(TRFIDTagList.cEvent_TagAdded, @Self.NewTagAdded, FEventsManager);

end;

destructor TRFIDManager.Destroy;
begin
  try
    FreeAndNil(FEventsManager);
    FreeAndNil(FTagList);
  except
    on E: Exception do
      LBLogger.Write(1, 'TRFIDManager.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

end.
