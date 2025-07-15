unit uLBCalendarTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, uLBCalendar;

type

  { TTestDSTCalendar }

  TTestDSTCalendar = class(TTestCase)
  private
    Calendar: TDSTCalendar;
  protected
    procedure SetUp; override;
  published
    // Test base recenti
    procedure TestIsLegalTime_2024_Start;
    procedure TestIsLegalTime_2024_End;
    procedure TestUTCToLocal_2024_Summer;
    procedure TestLocalToUTC_2024_Winter;
    procedure TestAmbiguousTime_UTC2Local_AfterChange_2024;
    procedure TestAmbiguousTime_UTC2Local_BeforeChange_2024;

    // Test storici
    procedure TestIsLegalTime_1980_Start;
    procedure TestIsLegalTime_1979_Summer;
    procedure TestLocalToUTC_1980_Spring;

    // Test edge: confine di ambiguità
    procedure TestAmbiguousTime_Local_2024;
  end;

implementation

procedure TTestDSTCalendar.SetUp;
begin
  Calendar := gv_CalendarRegistry.GetCalendar('IT');
  AssertNotNull('Calendario italiano non disponibile', Calendar);
end;

{-----------------------------}
{ Anno recente: 2024 }
{-----------------------------}

procedure TTestDSTCalendar.TestIsLegalTime_2024_Start;
var
  dt: TDateTime;
begin
  dt := EncodeDateTime(2024, 3, 31, 1, 0, 0, 0); // UTC
  AssertTrue('Ora legale attiva all’inizio 2024', Calendar.IsLegalTimeActive(dt, True));
end;

procedure TTestDSTCalendar.TestIsLegalTime_2024_End;
var
  dt: TDateTime;
begin
  dt := EncodeDateTime(2024, 10, 27, 1, 0, 0, 0); // UTC
  AssertFalse('Ora legale terminata a fine 2024', Calendar.IsLegalTimeActive(dt, True));
end;

procedure TTestDSTCalendar.TestUTCToLocal_2024_Summer;
var
  UTC, Local: TDateTime;
begin
  UTC := EncodeDateTime(2024, 6, 15, 12, 0, 0, 0); // estate, UTC+2
  Local := Calendar.UTCToLocalTime(UTC);
  AssertEquals('UTC→Local estate 2024', EncodeDateTime(2024, 6, 15, 14, 0, 0, 0), Local);
end;

procedure TTestDSTCalendar.TestLocalToUTC_2024_Winter;
var
  Local, UTC: TDateTime;
begin
  Local := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0); // inverno, UTC+1
  UTC := Calendar.LocalTimeToUTC(Local);
  AssertEquals('Local→UTC inverno 2024', EncodeDateTime(2024, 1, 15, 11, 0, 0, 0), UTC);
end;

procedure TTestDSTCalendar.TestAmbiguousTime_Local_2024;
var
  Local, UTC: TDateTime;
  _sDate : String;

begin
  // Ora ambigua: 2:30 locali nel giorno del passaggio (27 ottobre 2024)
  Local := EncodeDateTime(2024, 10, 27, 2, 30, 0, 0);
  UTC := Calendar.LocalTimeToUTC(Local);
  _sDate := FormatDateTime('dd"-"mm"-"yyyy hh:nn:ss', UTC);

  // Interpretato come ora solare → UTC = 1:30
  AssertTrue('Caso ambiguità ora locale UTC deve essere tra 00:30 e 01:30  -  Locale: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Local) + '  -  Ottengo ' + _sDate,
    (_sDate = '27-10-2024 00:30:00') or
    (_sDate = '27-10-2024 01:30:00'));
end;


procedure TTestDSTCalendar.TestAmbiguousTime_UTC2Local_BeforeChange_2024;
var
  Local, UTC: TDateTime;

begin
  // Ore 1.30 nel giorno del passaggio (27 ottobre 2024) => 2.30 locali
  UTC := EncodeDateTime(2024, 10, 27, 0, 30, 0, 0);
  Local := Calendar.UTCToLocalTime(UTC);

  AssertEquals('UTC2Local_2024_BeforeChange', + EncodeDateTime(2024, 10, 27, 2, 30, 0, 0), Local);
end;


procedure TTestDSTCalendar.TestAmbiguousTime_UTC2Local_AfterChange_2024;
var
  Local, UTC: TDateTime;

begin
  // Ore 1.30 nel giorno del passaggio (27 ottobre 2024) => 2.30 locali
  UTC := EncodeDateTime(2024, 10, 27, 1, 30, 0, 0);
  Local := Calendar.UTCToLocalTime(UTC);

  AssertEquals('UTC2Local_2024_AfterChange', + EncodeDateTime(2024, 10, 27, 2, 30, 0, 0), Local);
end;



{-----------------------------}
{ Anni storici }
{-----------------------------}

procedure TTestDSTCalendar.TestIsLegalTime_1980_Start;
var
  dt: TDateTime;
begin
  dt := EncodeDateTime(1980, 4, 6, 1, 0, 0, 0); // ora legale
  AssertTrue('Ora legale attiva in 1980', Calendar.IsLegalTimeActive(dt, True));
end;

procedure TTestDSTCalendar.TestIsLegalTime_1979_Summer;
var
  dt: TDateTime;
begin
  dt := EncodeDateTime(1979, 7, 15, 22, 0, 0, 0); // estate
  AssertTrue('Ora legale attiva in 1979', Calendar.IsLegalTimeActive(dt, True));
end;


procedure TTestDSTCalendar.TestLocalToUTC_1980_Spring;
var
  Local, UTC: TDateTime;
begin
  Local := EncodeDateTime(1980, 4, 7, 12, 0, 0, 0); // primavera, UTC+2
  UTC := Calendar.LocalTimeToUTC(Local);
  AssertEquals('Local→UTC primavera 1980', EncodeDateTime(1980, 4, 7, 10, 0, 0, 0), UTC);
end;

initialization
  RegisterTest(TTestDSTCalendar);

end.

