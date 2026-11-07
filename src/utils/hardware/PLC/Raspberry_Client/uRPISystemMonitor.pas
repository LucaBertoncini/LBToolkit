unit uRPISystemMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBUtils, ULBLogger;


type

  { TRPI_TemperatureMsg }

  TRPI_TemperatureMsg = class(TLBLoggerMessage)
    public
      function ElaborateMessageToWrite(): String; override;
  end;

  { TRPI_TemperatureLogger }

  TRPI_TemperatureLogger = class(TLBLogger)
    public
      constructor Create(); reintroduce;

      function writeTemp(Values: String): Boolean;
  end;

  { TRPI_SystemMonitor }

  TRPI_SystemMonitor = class(TLBBaseThread)
    strict private
      FOnTemperatureChanged : TNotifyEvent;
      FOnSpaceChanged : TNotifyEvent;
      FOnAvailableMemoryChanged : TNotifyEvent;
      FOnCPULoadChanged : TNotifyEvent;

      FTemperature : Integer;
      FMountpointSpace : Int64;
      FAvailableMem : Int64;
      FTotalMem : Int64;
      FCPULoad : Double;
      FMountpoint : String;

      FTempLogger : TRPI_TemperatureLogger;

      function GetFreeSpace(const MountPoint: String): Int64;
      function GetLoadAverage(): Double;
      function GetCPUTemperature(): Integer; // precisione decimo di grado
      function GetMemInfo(out TotalMem, FreeMem: Int64): Boolean; // la quantita di memoria è espressa in Kb

    protected
      procedure Execute(); override;

    public
      constructor Create; override;
      destructor Destroy; override;

      procedure createTemperatureLogger();
      procedure destroyTemperatureLogger();

      property Mountpoint: String write FMountpoint;

      property Temperature: Integer read FTemperature;
      property AvailableMemory: Int64 read FAvailableMem;
      property CPULoad: Double read FCPULoad;

      property OnTemperatureChanged: TNotifyEvent write FOnTemperatureChanged;
      property OnAvailableMemoryChanged: TNotifyEvent write FOnAvailableMemoryChanged;
      property OnCPULoadChanged: TNotifyEvent write FOnCPULoadChanged;

      property OnSpaceChanged: TNotifyEvent write FOnSpaceChanged;
  end;

implementation

uses
  process, LazFileUtils, Unix, StrUtils;

{ TRPI_SystemMonitor }

function TRPI_SystemMonitor.GetFreeSpace(const MountPoint: String): Int64;
var
  _Stat: TStatfs;

begin
  if fpStatFS(MountPoint, @_Stat) = 0 then
    Result := _Stat.bavail * _Stat.bsize  // blocchi disponibili * dimensione blocco
  else
    Result := -1; // errore
end;

function TRPI_SystemMonitor.GetLoadAverage(): Double;
var
  _File: Text;

begin
  Assign(_File, '/proc/loadavg');
  Reset(_File);
  Read(_File, Result); // legge solo il primo valore (1 minuto)
  Close(_File);
end;

function TRPI_SystemMonitor.GetCPUTemperature(): Integer;
var
  _File: Text;

begin
  Assign(_File, '/sys/class/thermal/thermal_zone0/temp');
  Reset(_File);
  Read(_File, Result);
  Close(_File);
  Result := Round(Result * 0.01);
end;

function TRPI_SystemMonitor.GetMemInfo(out TotalMem, FreeMem: Int64): Boolean;
var
  _File  : Text;
  _Line  : string;

const
  cMemTotal = String('MemTotal:');
  cMemAvailable = String('MemAvailable:');

begin
  TotalMem := 0;
  FreeMem := 0;

  Assign(_File, '/proc/meminfo');
  Reset(_File);

  while not EOF(_File) do
  begin
    ReadLn(_File, _Line);
    if _Line.StartsWith(cMemTotal) then
    begin
      _Line := Trim(MidStr(_Line, Length(cMemTotal) + 1, Length(_Line) - Length(cMemTotal) - 3));
      TotalMem := StrToIntDef(_Line, 0);
    end
    else if _Line.StartsWith(cMemAvailable) then
    begin
      _Line := Trim(MidStr(_Line, Length(cMemAvailable) + 1, Length(_Line) - Length(cMemAvailable) - 3));
      FreeMem := StrToIntDef(_Line, 0);
    end;
    if (TotalMem > 0) and (FreeMem > 0) then
      Break;
  end;
  Close(_File);
  Result := True;
end;

procedure TRPI_SystemMonitor.Execute;
var
  _FreeMem : Int64;
  _TotalMem : Int64;
  _CPULoad : Double;
  _CPUTemp : Integer;
  _FreeSpace : Int64;


begin
  try

      while not Self.Terminated do
      begin
        if FMountpoint <> '' then
        begin
          _FreeSpace := Self.GetFreeSpace(FMountpoint);
          if _FreeSpace <> FMountpointSpace then
          begin
            FMountpointSpace := _FreeSpace;
            if Assigned(FOnSpaceChanged) then
              FOnSpaceChanged(Self);
          end;
        end;

        if Self.GetMemInfo(_TotalMem, _FreeMem) then
        begin
          if (_FreeMem <> FAvailableMem) or (_TotalMem <> FTotalMem) then
          begin
            FAvailableMem := _FreeMem;
            FTotalMem := _TotalMem;

            if Assigned(FOnAvailableMemoryChanged) then
              FOnAvailableMemoryChanged(Self);
          end;
        end;

        _CPULoad := Self.GetLoadAverage();
        if _CPULoad <> FCPULoad then
        begin

          FCPULoad := _CPULoad;
          if Assigned(FOnCPULoadChanged) then
            FOnCPULoadChanged(Self);
        end;

        _CPUTemp := Self.GetCPUTemperature();
        if _CPUTemp <> FTemperature then
        begin
          FTemperature := _CPUTemp;
          if Assigned(FOnTemperatureChanged) then
            FOnTemperatureChanged(Self);
        end;

        if not Self.Terminated then
          Self.PauseFor(30000);
      end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TRPI_SystemMonitor.Execute', lmt_Error, E.Message);
  end;
end;

constructor TRPI_SystemMonitor.Create;
begin
  inherited Create;

  Self.setThreadName('SysMonitor');

  FMountpoint      := '';
  FMountPointSpace := -1;
  FTemperature     := 0;
  FAvailableMem    := 0;
  FTotalMem        := 0;

  FCPULoad         := 0;
end;

destructor TRPI_SystemMonitor.Destroy;
begin
  Self.destroyTemperatureLogger();

  inherited Destroy;
end;

procedure TRPI_SystemMonitor.createTemperatureLogger;
begin
  if LBLogger <> nil then
    FTempLogger := TRPI_TemperatureLogger.Create();
end;

procedure TRPI_SystemMonitor.destroyTemperatureLogger;
begin
  if FTempLogger <> nil then
    FreeAndNil(FTempLogger);
end;


{ TRPI_TemperatureLogger }

constructor TRPI_TemperatureLogger.Create;
var
  _LogFile : String;

begin
  inherited Create('TempLogger', False);

  FMessageClass := TRPI_TemperatureMsg;
  Self.MaxLogLevel := 1;

  if LBLogger <> nil then
  begin
    _LogFile := LBLogger.FilePath;
    if Length(_LogFile) > 0 then
    begin
      _LogFile := IncludeTrailingPathDelimiter(_LogFile) + 'Temperature' + PathDelim;
      if not DirectoryExistsUTF8(_LogFile) then
        ForceDirectoriesUTF8(_LogFile);

      Self.setLogFile(PChar(_LogFile + 'TempValues.csv'));
    end;
  end;
end;

function TRPI_TemperatureLogger.writeTemp(Values: String): Boolean;
begin
  Result := Self.Write(1, 'TempLog', lmt_Info, Values);
end;

{ TRPI_TemperatureMsg }

function TRPI_TemperatureMsg.ElaborateMessageToWrite: String;
begin
  Result := '"' + FormatDateTime('dd/mm/yyyy hh:nn:ss', FTime) + '";' + FMessage;
end;

end.

