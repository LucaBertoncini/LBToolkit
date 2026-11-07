unit uLBCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fgl;


type
  // Classe base astratta per i calendari DST (Daylight Saving Time)

  { TDSTCalendar }

  TDSTCalendar = class(TObject)
  protected
    function GetLastSundayInMonth(aYear, aMonth: Word): Word;
    function GetFirstSundayInMonth(aYear, aMonth: Word): Word;

  public
    // Verifica se l'ora legale è attiva per una data specifica
    function IsLegalTimeActive(aDateTime: TDateTime; IsUTC: Boolean): Boolean; virtual; abstract;

    // Ritorna l'offset (es. +1, +2) da applicare alla data
    function GetZoneOffset(aDateTime: TDateTime; IsUTC: Boolean): Integer; virtual; abstract;

    // Nome leggibile del calendario (opzionale)
    function GetRegionName: String; virtual;

    // Conversioni
    function UTCToLocalTime(anUTCTime: TDateTime): TDateTime;
    function LocalTimeToUTC(aLocalTime: TDateTime): TDateTime;
    function ISO8601DateTime(aLocalTime: TDateTime): String;
    function ISO8601DateTimeFromUTC(anUTCTime: TDateTime): String;
  end;


  TMinTimeInfo = record
    DateTime: TDateTime;
    Always: Boolean;
  end;

  // Implementazione specifica per l'Italia
  TItalianDSTCalendar = class(TDSTCalendar)
  strict private
    function GetDSTDatesForYear(aYear: Word; IsUTC: Boolean; out aStart, anEnd: TMinTimeInfo): Boolean;
    function IsInDSTRange(aDateTime: TDateTime; aStart, anEnd: TMinTimeInfo): Boolean;

  public
    function IsLegalTimeActive(aDateTime: TDateTime; IsUTC: Boolean): Boolean; override;
    function GetZoneOffset(aDateTime: TDateTime; IsUTC: Boolean): Integer; override;
    function GetRegionName: String; override;
  end;


  // Factory per creare calendari DST

  TDSTCalendarMapper = specialize TFPGMapObject<String, TDSTCalendar>;

  { TDSTCalendarRegistry }

  TDSTCalendarRegistry = class(TObject)
  strict private
    FMap: TDSTCalendarMapper;

  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterCalendar(aRegion: string; aCalendar: TDSTCalendar);
    function GetCalendar(aRegion: string): TDSTCalendar;
  end;


// Funzioni di compatibilità con l'API precedente
function UTCToLocalTime(anUTCTime: TDateTime; aCountry: string = 'IT'): TDateTime;
function LocalTimeToUTC(aLocalTime: TDateTime; aCountry: String = 'IT'): TDateTime;
function ISO8601DateTime(aLocalTime: TDateTime; aCountry: String = 'IT'): String;
function ISO8601DateTimeFromUTC(anUTCTime: TDateTime; aCountry: String = 'IT'): String;
function decodeStringDatetime(const aDateTimeString: String; out UTCDateTime, LocalDateTime: TDateTime): Boolean;

procedure ReReadLocalTime();

var
  gv_CalendarRegistry : TDSTCalendarRegistry = nil;


implementation

uses
  LazSysUtils, strutils, ULBLogger, uTimedoutCriticalSection, {$IFDEF WINDOWS}Windows{$ELSE}Unix{$ENDIF};

{$IFDEF Linux}
type
  TTimeUpdater = class(TObject)
    strict private
      FCS : TTimedOutCriticalSection;
      FLastUpdateDay : Integer;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Update();
  end;

var
  gv_TimeUpdater : TTimeUpdater = nil;


{ TTimeUpdater }

constructor TTimeUpdater.Create;
begin
  inherited Create;
  FLastUpdateDay := 0;
  FCS := TTimedOutCriticalSection.Create;
end;

destructor TTimeUpdater.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TTimeUpdater.Update();
var
  _ActualDate : TDateTime;
  _Hour : Integer;

begin
  if FCS.Acquire('TTimeUpdater.Update', 500) then
  begin

    _ActualDate := NowUTC();
    if FLastUpdateDay < Trunc(_ActualDate) then
    begin
      _Hour := HourOf(_ActualDate);
      if (_Hour > 1) or ((_Hour = 1) and (MinuteOf(_ActualDate) >= 1)) then
      begin
        unix.ReReadLocalTime;
        FLastUpdateDay := Trunc(_ActualDate);
        LBLogger.Write(5, 'TTimeUpdater.Update', lmt_Debug, 'Local time updated!');
      end;
    end;

    FCS.Release;
  end;
end;
{$ENDIF}


procedure ReReadLocalTime();
begin
  {$IFDEF Linux}
  if gv_TimeUpdater <> nil then
    gv_TimeUpdater.Update();
  {$ENDIF}
end;


{ TDSTCalendarRegistry }

constructor TDSTCalendarRegistry.Create;
begin
  inherited Create;

  FMap := TDSTCalendarMapper.Create(True);
end;

destructor TDSTCalendarRegistry.Destroy;
begin
  FMap.Free;

  inherited Destroy;
end;

procedure TDSTCalendarRegistry.RegisterCalendar(aRegion: string; aCalendar: TDSTCalendar);
begin
  FMap.Add(UpperCase(aRegion), aCalendar);
end;

function TDSTCalendarRegistry.GetCalendar(aRegion: string): TDSTCalendar;
var
  _Idx : Integer;

begin
  Result := nil;
  _Idx := FMap.IndexOf(UpperCase(aRegion));
  if _Idx > -1 then
    Result := FMap.Data[_Idx];
end;


{$IFDEF WINDOWS}
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external kernel32 name 'TzSpecificLocalTimeToSystemTime';

function LocalTimeToUTC_Windows(aLocalTime: TDateTime): TDateTime;
var
  _TimeZoneInf: _TIME_ZONE_INFORMATION;
  _SysTime, _LocalTime: TSystemTime;
begin
  if GetTimeZoneInformation(_TimeZoneInf) < $FFFFFFFF then
  begin
    DateTimeToSystemTime(aLocalTime, _SysTime);
    if TzSpecificLocalTimeToSystemTime(@_TimeZoneInf, _SysTime, _LocalTime) then
      Result := SystemTimeToDateTime(_LocalTime)
    else
      Result := aLocalTime;
  end
  else
    Result := aLocalTime;
end;

function UTCToLocalTime_Windows(anUTCTime: TDateTime): TDateTime;
var
  _TimeZoneInf: _TIME_ZONE_INFORMATION;
  _SysTime, _LocalTime: TSystemTime;
begin
  if GetTimeZoneInformation(_TimeZoneInf) < $FFFFFFFF then
  begin
    DateTimeToSystemTime(anUTCTime, _SysTime);
    if SystemTimeToTzSpecificLocalTime(@_TimeZoneInf, _SysTime, _LocalTime) then
      Result := SystemTimeToDateTime(_LocalTime)
    else
      Result := anUTCTime;
  end
  else
    Result := anUTCTime;
end;
{$ENDIF}

function TDSTCalendar.GetLastSundayInMonth(aYear, aMonth: Word): Word;
var
  d: TDateTime;
begin
  d := EndOfTheMonth(EncodeDate(aYear, aMonth, 1)); // ottieni l’ultimo giorno del mese

  while DayOfTheWeek(d) <> DaySunday do
    d := d - 1; // scorri indietro fino alla domenica

  Result := DayOf(d);
end;

function TDSTCalendar.GetFirstSundayInMonth(aYear, aMonth: Word): Word;
var
  d: TDateTime;
begin
  d := EncodeDate(aYear, aMonth, 1); // primo giorno del mese

  while DayOfTheWeek(d) <> DaySunday do
    d := d + 1; // scorri avanti fino alla domenica

  Result := DayOf(d);
end;

// Implementazione TDSTCalendar
function TDSTCalendar.GetRegionName: String;
begin
  Result := 'Unknown';
end;

function TDSTCalendar.LocalTimeToUTC(aLocalTime: TDateTime): TDateTime;
{$IFDEF WINDOWS}
begin
  Result := LocalTimeToUTC_Windows(aLocalTime);
end;
{$ELSE}
var
  _Offset: Integer;
begin
  _Offset := GetZoneOffset(aLocalTime, False);
  Result := IncHour(aLocalTime, -_Offset);
end;
{$ENDIF}

function TDSTCalendar.UTCToLocalTime(anUTCTime: TDateTime): TDateTime;
{$IFDEF WINDOWS}
begin
  Result := UTCToLocalTime_Windows(anUTCTime);
end;
{$ELSE}
var
  _Offset: Integer;
begin
  _Offset := GetZoneOffset(anUTCTime, True);
  Result := IncHour(anUTCTime, _Offset);
end;
{$ENDIF}

function TDSTCalendar.ISO8601DateTime(aLocalTime: TDateTime): String;
var
  _Minutes: TDateTime;
  _UTC: TDateTime;
  _sSign: String;
begin
  _UTC := LocalTimeToUTC(aLocalTime);
  _Minutes := aLocalTime - _UTC;
  if _Minutes < 0 then
  begin
    _sSign := '-';
    _Minutes := -_Minutes;
  end
  else
    _sSign := '+';

  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', aLocalTime) + _sSign + FormatDateTime('hh":"nn', _Minutes);
end;

function TDSTCalendar.ISO8601DateTimeFromUTC(anUTCTime: TDateTime): String;
var
  _Minutes: TDateTime;
  _LocalTime: TDateTime;
  _sSign: String;
begin
  _LocalTime := UTCToLocalTime(anUTCTime);
  _Minutes := _LocalTime - anUTCTime;
  if _Minutes < 0 then
  begin
    _sSign := '-';
    _Minutes := -_Minutes;
  end
  else
    _sSign := '+';

  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', _LocalTime) + _sSign + FormatDateTime('hh":"nn', _Minutes);
end;

// Implementazione TItalianDSTCalendar
function TItalianDSTCalendar.GetRegionName: String;
begin
  Result := 'Italy';
end;

function TItalianDSTCalendar.IsLegalTimeActive(aDateTime: TDateTime; IsUTC: Boolean): Boolean;
var
  _Year: Word;
  _Start, _End: TMinTimeInfo;
begin
  Result := False;
  _Year := YearOf(aDateTime);

  if GetDSTDatesForYear(_Year, IsUTC, _Start, _End) then
    Result := IsInDSTRange(aDateTime, _Start, _End);
end;

function TItalianDSTCalendar.GetZoneOffset(aDateTime: TDateTime; IsUTC: Boolean): Integer;
begin
  if IsLegalTimeActive(aDateTime, IsUTC) then
    Result := 2
  else
    Result := 1;
end;

function TItalianDSTCalendar.IsInDSTRange(aDateTime: TDateTime; aStart, anEnd: TMinTimeInfo): Boolean;
begin
  if aStart.Always and anEnd.Always then
    Exit(True)
  else if aStart.Always then
    Exit(aDateTime < anEnd.DateTime)
  else if anEnd.Always then
    Exit(aDateTime >= aStart.DateTime)
  else
    Exit((aDateTime >= aStart.DateTime) and (aDateTime < anEnd.DateTime));
end;

function TItalianDSTCalendar.GetDSTDatesForYear(aYear: Word; IsUTC: Boolean; out aStart, anEnd: TMinTimeInfo): Boolean;
var
  startDay, endDay: Word;
begin
  Result := True;
  aStart.Always := False;
  anEnd.Always := False;

  case aYear of
    1996..3000:
      begin
        startDay := GetLastSundayInMonth(aYear, 3);
        endDay := GetLastSundayInMonth(aYear, 10);

        if IsUTC then
        begin
          aStart.DateTime := EncodeDateTime(aYear, 3, startDay, 1, 0, 0, 0); // 2:00 locali → 1:00 UTC
          anEnd.DateTime  := EncodeDateTime(aYear, 10, endDay, 1, 0, 0, 0); // 3:00 locali → 1:00 UTC
        end
        else begin
          aStart.DateTime := EncodeDateTime(aYear, 3, startDay, 2, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(aYear, 10, endDay, 3, 0, 0, 0);
        end;
      end;

    1981..1995:
      begin
        startDay := GetLastSundayInMonth(aYear, 3);
        endDay := GetLastSundayInMonth(aYear, 9);

        if IsUTC then
        begin
          aStart.DateTime := EncodeDateTime(aYear, 3, startDay, 0, 0, 0, 0); // 1:00 locali
          anEnd.DateTime  := EncodeDateTime(aYear, 9, endDay, 0, 0, 0, 0);
        end
        else begin
          aStart.DateTime := EncodeDateTime(aYear, 3, startDay, 1, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(aYear, 9, endDay, 1, 0, 0, 0);
        end;
      end;

    1980:
      begin
        if IsUTC then
        begin
          aStart.DateTime := EncodeDateTime(1980, 4, 6, 0, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(1980, 9, 28, 1, 0, 0, 0);
        end
        else begin
          aStart.DateTime := EncodeDateTime(1980, 4, 6, 2, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(1980, 9, 28, 3, 0, 0, 0);
        end;
      end;

    1979:
      begin
        startDay := GetLastSundayInMonth(aYear, 5) - 1;
        endDay := GetLastSundayInMonth(aYear, 9) - 1;

        if IsUTC then
        begin
          aStart.DateTime := EncodeDateTime(aYear, 5, startDay, 21, 0, 0, 0); // 23:00 locali
          anEnd.DateTime  := EncodeDateTime(aYear, 9, endDay, 21, 0, 0, 0);
        end
        else begin
          aStart.DateTime := EncodeDateTime(aYear, 5, startDay, 23, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(aYear, 9, endDay, 23, 0, 0, 0);
        end;
      end;

    1978:
      begin
        // Fonte storica: dal 28 maggio ore 00:00 locali → 1 ottobre ore 01:00 locali
        if IsUTC then
        begin
          aStart.DateTime := EncodeDateTime(aYear, 5, 27, 22, 0, 0, 0); // 00:00 locali
          anEnd.DateTime  := EncodeDateTime(aYear, 9, 30, 23, 0, 0, 0);  // 01:00 locali
        end
        else begin
          aStart.DateTime := EncodeDateTime(aYear, 5, 28, 0, 0, 0, 0);
          anEnd.DateTime  := EncodeDateTime(aYear, 10, 1, 1, 0, 0, 0);
        end;
      end;

    1942:
      begin
        aStart.Always := True;
        if IsUTC then
          anEnd.DateTime := EncodeDateTime(aYear, 11, 2, 0, 0, 0, 0) // 1:00 locali
        else
          anEnd.DateTime := EncodeDateTime(aYear, 11, 2, 1, 0, 0, 0);
      end;

    1941:
      begin
        aStart.Always := True;
        anEnd.Always := True;
      end;

    1940:
      begin
        if IsUTC then
          aStart.DateTime := EncodeDateTime(aYear, 6, 14, 21, 0, 0, 0) // 23:00 locali
        else
          aStart.DateTime := EncodeDateTime(aYear, 6, 14, 23, 0, 0, 0);
        anEnd.Always := True;
      end;

  else
    Result := False;
  end;
end;


function UTCToLocalTime(anUTCTime: TDateTime; aCountry: string = 'IT'): TDateTime;
var
  _Calendar: TDSTCalendar;
begin
  Result := 0;
  _Calendar := gv_CalendarRegistry.GetCalendar(aCountry);
  if _Calendar <> nil then
    Result := _Calendar.UTCToLocalTime(anUTCTime);
end;

function LocalTimeToUTC(aLocalTime: TDateTime; aCountry: String = 'IT'): TDateTime;
var
  _Calendar: TDSTCalendar;
begin
  Result := 0;

  _Calendar := gv_CalendarRegistry.GetCalendar(aCountry);
  if _Calendar <> nil then
    Result := _Calendar.LocalTimeToUTC(aLocalTime);
end;


function ISO8601DateTime(aLocalTime: TDateTime; aCountry: String = 'IT'): String;
var
  _Calendar: TDSTCalendar;
begin
  Result := '';
  _Calendar := gv_CalendarRegistry.GetCalendar(aCountry);
  if _Calendar <> nil then
    Result := _Calendar.ISO8601DateTime(aLocalTime);
end;

function ISO8601DateTimeFromUTC(anUTCTime: TDateTime; aCountry: String = 'IT'): String;
var
  _Calendar: TDSTCalendar;
begin
  Result := '';
  _Calendar := gv_CalendarRegistry.GetCalendar(aCountry);
  if _Calendar <> nil then
    Result := _Calendar.ISO8601DateTimeFromUTC(anUTCTime);
end;

function decodeStringDatetime(const aDateTimeString: String; out UTCDateTime, LocalDateTime: TDateTime): Boolean;
var
  _sYear, _sMonth, _sDay: String;
  _sHours, _sMinutes, _sSeconds: String;
  _isUTC: Boolean;
begin
  Result := False;
  UTCDateTime := 0;
  LocalDateTime := 0;

  try
    if Length(aDateTimeString) >= 19 then
    begin
      _sDay := LeftStr(aDateTimeString, 2);
      _sMonth := MidStr(aDateTimeString, 4, 2);
      _sYear := MidStr(aDateTimeString, 7, 4);
      _sHours := MidStr(aDateTimeString, 12, 2);
      _sMinutes := MidStr(aDateTimeString, 15, 2);
      _sSeconds := MidStr(aDateTimeString, 18, 2);
      _isUTC := aDateTimeString.EndsWith('(UTC)') or aDateTimeString.EndsWith('UTC');

      ReReadLocalTime();

      if _isUTC then
      begin
        UTCDateTime := EncodeDateTime(StrToInt(_sYear), StrToInt(_sMonth), StrToInt(_sDay), 
                                     StrToInt(_sHours), StrToInt(_sMinutes), StrToInt(_sSeconds), 0);
        LocalDateTime := UTCToLocalTime(UTCDateTime);
      end
      else
      begin
        LocalDateTime := EncodeDateTime(StrToInt(_sYear), StrToInt(_sMonth), StrToInt(_sDay), 
                                       StrToInt(_sHours), StrToInt(_sMinutes), StrToInt(_sSeconds), 0);
        UTCDateTime := LocalTimeToUTC(LocalDateTime);
      end;

      Result := True;
    end
    else
      LBLogger.Write(1, 'decodeStringDatetime', lmt_Warning, 'Wrong date string: %s', [aDateTimeString]);

  except
    on E: Exception do
      LBLogger.Write(1, 'decodeStringDatetime', lmt_Error, PChar(E.Message));
  end;
end;

initialization
  gv_CalendarRegistry := TDSTCalendarRegistry.Create;
  gv_CalendarRegistry.RegisterCalendar('IT', TItalianDSTCalendar.Create);
  {$IFDEF Linux}
  gv_TimeUpdater := TTimeUpdater.Create;
  {$ENDIF}

finalization
  if gv_CalendarRegistry <> nil then
    FreeAndNil(gv_CalendarRegistry);

  {$IFDEF Linux}
  if gv_TimeUpdater <> nil then
    FreeAndNil(gv_TimeUpdater);
  {$ENDIF}

end.
