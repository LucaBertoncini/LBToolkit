unit uPQConnectionEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, postgres3dyn;

const
  { Massimo numero di parametri per query preparata }
  MAX_PQ_PARAMS = 64;

type
  TPGNotifyEvent = procedure(Sender: TObject; const Channel, Payload: string) of object;

  { Rappresenta un singolo valore di parametro da passare a PQexecParams }
  TPQParamValue = record
    Data    : AnsiString;  // valore serializzato come stringa (testo)
    IsNull  : Boolean;     // se True, passa NULL indipendentemente da Data
  end;

  { TPQResultSet
    Incapsula un PPGresult e offre un cursore riga-per-riga,
    speculare a prepareSQLStatement/executeQuery/nextRow/closeQuery di SQLite. }

  TPQResultSet = class
  private
    FResult     : PPGresult;
    FRowCount   : Integer;
    FCurrentRow : Integer;    // -1 = prima di fetchare, >= 0 riga corrente
    FEmpty      : Boolean;
    FEOF        : Boolean;
  public
    constructor Create(aResult: PPGresult);
    destructor Destroy; override;

    { Avanza alla riga successiva; ritorna False quando non ci sono più righe }
    function NextRow: Boolean;

    { Chiude e libera il result-set }
    procedure Close;

    function ColumnIsNull(aColIdx: Integer): Boolean;
    function GetColumnAsText(aColIdx: Integer): string;
    function GetColumnAsInteger(aColIdx: Integer): Integer;
    function GetColumnAsInt64(aColIdx: Integer): Int64;
    function GetColumnAsDouble(aColIdx: Integer): Double;
    function GetColumnAsBool(aColIdx: Integer): Boolean;

    { Numero di colonne e righe nel result-set }
    function ColumnCount: Integer;
    function RowCount: Integer;

    property IsEmpty : Boolean read FEmpty;
    property EOF     : Boolean read FEOF;
  end;

  { TPQDirectConnection
    Connessione libpq nativa senza TPQConnection/SQLDB.
    Supporta:
      - LISTEN/NOTIFY (ProcessNotifications)
      - Comandi senza risultato (ExecuteCommand)
      - Query parametrizzate con result-set (prepare/setParam/executeQuery)
      - Transazioni esplicite (StartTransaction/Commit/Rollback)           }

  TPQDirectConnection = class
  private
    FHandle            : PPGconn;
    FOnNotify          : TPGNotifyEvent;
    FConnected         : Boolean;
    FTransactionActive : Boolean;

    { Stato per la query preparata corrente }
    FPreparedSQL   : AnsiString;
    FParamCount    : Integer;
    FParams        : array[0..MAX_PQ_PARAMS - 1] of TPQParamValue;

    { Array temporanei ricostruiti a ogni executeQuery }
    FParamValues   : array[0..MAX_PQ_PARAMS - 1] of PAnsiChar;
    FParamLengths  : array[0..MAX_PQ_PARAMS - 1] of Integer;
    FParamFormats  : array[0..MAX_PQ_PARAMS - 1] of Integer;

    function  GetConnected: Boolean;
    function  CheckHandle: Boolean;
    procedure ClearParams;
    procedure BuildParamArrays;

  public
    constructor Create;
    destructor Destroy; override;

    { ------------------------------------------------------------------ }
    { Connessione                                                          }
    { ------------------------------------------------------------------ }

    { ConnInfo: stringa libpq standard
      Es.: 'host=localhost port=5432 dbname=mydb user=foo password=bar' }
    function Open(const ConnInfo: string): Boolean;
    procedure Close;

    { ------------------------------------------------------------------ }
    { Comandi senza result-set (LISTEN, DDL, DML senza parametri)         }
    { ------------------------------------------------------------------ }
    function ExecuteCommand(const SQL: string): Boolean;

    { ------------------------------------------------------------------ }
    { Transazioni                                                          }
    { ------------------------------------------------------------------ }
    function StartTransaction: Boolean;
    function Commit: Boolean;
    function Rollback(LogError: Boolean = True): Boolean;

    { ------------------------------------------------------------------ }
    { Query parametrizzate — stile SQLiteWrapper                          }
    {                                                                      }
    { Uso tipico:                                                          }
    {   Conn.PrepareSQLStatement('SELECT * FROM t WHERE id=$1');           }
    {   Conn.SetParam(1, 42);                                              }
    {   RS := Conn.ExecuteQuery;                                           }
    {   if Assigned(RS) then                                               }
    {   try                                                                }
    {     while RS.NextRow do                                              }
    {       Writeln(RS.GetColumnAsText(0));                                }
    {   finally                                                            }
    {     RS.Free;                                                         }
    {   end;                                                               }
    { ------------------------------------------------------------------ }

    { Memorizza l'SQL e azzera i parametri. Non invia nulla al server. }
    function PrepareSQLStatement(const SQL: string): Boolean;

    { Indice 1-based, come sqlite3_bind_* }
    function SetParam(anIdx: Integer; const AValue: string):   Boolean; overload;
    function SetParam(anIdx: Integer; AValue: Integer):        Boolean; overload;
    function SetParam(anIdx: Integer; AValue: Int64):          Boolean; overload;
    function SetParam(anIdx: Integer; AValue: Double):         Boolean; overload;
    function SetParam(anIdx: Integer; AValue: Boolean):        Boolean; overload;
    function SetParamNull(anIdx: Integer):                     Boolean;

    { Esegue la query preparata e restituisce un TPQResultSet.
      Il chiamante è responsabile del Free del result-set restituito.
      Ritorna nil in caso di errore. }
    function ExecuteQuery: TPQResultSet;

    { Versione senza result-set (INSERT/UPDATE/DELETE con parametri) }
    function ExecuteNonQuery: Boolean;

    { ------------------------------------------------------------------ }
    { LISTEN / NOTIFY                                                      }
    { ------------------------------------------------------------------ }
    function ProcessNotifications: Boolean;

    { ------------------------------------------------------------------ }
    { Proprietà                                                            }
    { ------------------------------------------------------------------ }
    property Connected         : Boolean        read GetConnected;
    property TransactionActive : Boolean        read FTransactionActive;
    property OnNotify          : TPGNotifyEvent read FOnNotify write FOnNotify;
    property Handle            : PPGconn        read FHandle;
  end;

implementation

uses
  ULBLogger;

var
  gv_FormatSettings : TFormatSettings;

{ --------------------------------------------------------------------------- }
{ TPQResultSet                                                                 }
{ --------------------------------------------------------------------------- }

constructor TPQResultSet.Create(aResult: PPGresult);
begin
  inherited Create;
  FResult     := aResult;
  FCurrentRow := -1;
  FEOF        := False;

  if FResult <> nil then
  begin
    FRowCount := PQntuples(FResult);
    FEmpty    := FRowCount = 0;
    FEOF      := FEmpty;
  end
  else begin
    FRowCount := 0;
    FEmpty    := True;
    FEOF      := True;
  end;
end;

destructor TPQResultSet.Destroy;
begin
  Self.Close;
  inherited Destroy;
end;

function TPQResultSet.NextRow: Boolean;
begin

  if FResult <> nil then
  begin

    Inc(FCurrentRow);
    Result := FCurrentRow < FRowCount;
    FEOF   := not Result;

  end
  else
    Result := False;
end;

procedure TPQResultSet.Close;
begin
  if FResult <> nil then
  begin
    PQclear(FResult);
    FResult := nil;
  end;
end;

function TPQResultSet.ColumnIsNull(aColIdx: Integer): Boolean;
begin
  Result := (FResult = nil) or
            (PQgetisnull(FResult, FCurrentRow, aColIdx) <> 0);
end;

function TPQResultSet.GetColumnAsText(aColIdx: Integer): string;
begin
  if ColumnIsNull(aColIdx) then
    Result := ''
  else
    Result := StrPas(PQgetvalue(FResult, FCurrentRow, aColIdx));
end;

function TPQResultSet.GetColumnAsInteger(aColIdx: Integer): Integer;
begin
  if ColumnIsNull(aColIdx) then
    Result := 0
  else
    Result := StrToIntDef(Self.GetColumnAsText(aColIdx), 0);
end;

function TPQResultSet.GetColumnAsInt64(aColIdx: Integer): Int64;
begin
  if ColumnIsNull(aColIdx) then
    Result := 0
  else
    Result := StrToInt64Def(Self.GetColumnAsText(aColIdx), 0);
end;

function TPQResultSet.GetColumnAsDouble(aColIdx: Integer): Double;
begin
  if not ColumnIsNull(aColIdx) then
    Result := StrToFloatDef(Self.GetColumnAsText(aColIdx), 0.0, gv_FormatSettings)
  else
    Result := 0.0;
end;

function TPQResultSet.GetColumnAsBool(aColIdx: Integer): Boolean;
var
  S: string;
begin
  S := LowerCase(GetColumnAsText(aColIdx));
  Result := (S = 't') or (S = 'true') or (S = '1') or (S = 'yes') or (S = 'on');
end;

function TPQResultSet.ColumnCount: Integer;
begin
  if FResult <> nil then
    Result := PQnfields(FResult)
  else
    Result := 0;
end;

function TPQResultSet.RowCount: Integer;
begin
  Result := FRowCount;
end;

{ --------------------------------------------------------------------------- }
{ TPQDirectConnection                                                          }
{ --------------------------------------------------------------------------- }

constructor TPQDirectConnection.Create;
begin
  inherited Create;
  FHandle            := nil;
  FConnected         := False;
  FTransactionActive := False;
  FPreparedSQL       := '';
  FParamCount        := 0;
end;

destructor TPQDirectConnection.Destroy;
begin
  if FTransactionActive then
    Self.Rollback(False);

  Close;
  inherited Destroy;
end;

function TPQDirectConnection.GetConnected: Boolean;
begin
  Result := (FHandle <> nil) and (PQstatus(FHandle) = CONNECTION_OK);
end;

function TPQDirectConnection.CheckHandle: Boolean;
begin
  Result := (FHandle <> nil) and (PQstatus(FHandle) <> CONNECTION_BAD);
end;

procedure TPQDirectConnection.ClearParams;
var
  i: Integer;
begin
  for i := 0 to FParamCount - 1 do
  begin
    FParams[i].Data   := '';
    FParams[i].IsNull := False;
  end;
  FParamCount := 0;
end;

procedure TPQDirectConnection.BuildParamArrays;
var
  i: Integer;
begin
  for i := 0 to FParamCount - 1 do
  begin
    if FParams[i].IsNull then
      FParamValues[i] := nil
    else
      FParamValues[i] := PAnsiChar(FParams[i].Data);

    FParamLengths[i] := 0;   // 0 = trattato come stringa C null-terminated
    FParamFormats[i] := 0;   // 0 = formato testo
  end;
end;

{ ------------------------------------------------------------------ }
{ Connessione                                                          }
{ ------------------------------------------------------------------ }

function TPQDirectConnection.Open(const ConnInfo: string): Boolean;
begin
  Result := False;

  InitialisePostgres3;
  if Postgres3LibraryHandle <> NilHandle then
  begin

    if FHandle <> nil then
      Self.Close;

    FHandle := PQconnectdb(PChar(ConnInfo));

    if (FHandle = nil) or (PQstatus(FHandle) <> CONNECTION_OK) then
    begin
      LBLogger.Write(1, 'TPQDirectConnection.Open', lmt_Warning, 'PostgreSQL not connected: %s', [StrPas(PQerrorMessage(FHandle))]);
      if FHandle <> nil then
      begin
        PQfinish(FHandle);
        FHandle := nil;
      end;
    end
    else begin
      FConnected := True;
      Result     := True;
      LBLogger.Write(5, 'TPQDirectConnection.Open', lmt_Info, 'PostgreSQL connected.');
    end;

  end
  else
    LBLogger.Write(1, 'TPQDirectConnection.Open', lmt_Warning, 'Library libpq not loaded!');

end;

procedure TPQDirectConnection.Close;
begin
  if FHandle <> nil then
  begin
    PQfinish(FHandle);
    FHandle            := nil;
    FConnected         := False;
    FTransactionActive := False;
  end;
end;

{ ------------------------------------------------------------------ }
{ Comandi senza result-set                                             }
{ ------------------------------------------------------------------ }

function TPQDirectConnection.ExecuteCommand(const SQL: string): Boolean;
var
  Res    : PPGresult;
  Status : TExecStatusType;
begin
  Result := False;

  if Self.CheckHandle then
  begin
    Res    := PQexec(FHandle, PChar(SQL));
    Status := PQresultStatus(Res);

    if not (Status in [PGRES_COMMAND_OK, PGRES_TUPLES_OK]) then
      LBLogger.Write(1, 'TPQDirectConnection.ExecuteCommand', lmt_Warning, 'SQL error [%s]: %s', [SQL, StrPas(PQresultErrorMessage(Res))])
    else
      Result := True;

    PQclear(Res);

  end;
end;

{ ------------------------------------------------------------------ }
{ Transazioni                                                          }
{ ------------------------------------------------------------------ }

function TPQDirectConnection.StartTransaction: Boolean;
begin
  Result := False;

  if not FTransactionActive then
  begin

    Result := ExecuteCommand('BEGIN');
    if Result then
      FTransactionActive := True;

  end
  else
    LBLogger.Write(1, 'TPQDirectConnection.StartTransaction', lmt_Warning, 'Transaction already active!');

end;

function TPQDirectConnection.Commit: Boolean;
begin
  Result := False;

  if FTransactionActive then
  begin
    Result := ExecuteCommand('COMMIT');
    if Result then
      FTransactionActive := False;
  end
  else
    LBLogger.Write(1, 'TPQDirectConnection.Commit', lmt_Warning, 'No active transaction to commit.');
end;

function TPQDirectConnection.Rollback(LogError: Boolean): Boolean;
begin
  Result := ExecuteCommand('ROLLBACK');
  FTransactionActive := False;

  if LogError and (not Result) then
    LBLogger.Write(1, 'TPQDirectConnection.Rollback', lmt_Warning, 'Rollback failed.');
end;


{ ------------------------------------------------------------------ }
{ Query parametrizzate                                                 }
{ ------------------------------------------------------------------ }

function TPQDirectConnection.PrepareSQLStatement(const SQL: string): Boolean;
begin
  FPreparedSQL := SQL;
  ClearParams;
  Result := Length(SQL) > 0;
end;

{ Indice 1-based: il parametro $1 corrisponde a anIdx=1 }

function TPQDirectConnection.SetParam(anIdx: Integer; const AValue: string): Boolean;
begin
  Result := (anIdx >= 1) and (anIdx <= MAX_PQ_PARAMS);
  if Result then
  begin
    FParams[anIdx - 1].Data   := AValue;
    FParams[anIdx - 1].IsNull := False;
    if anIdx > FParamCount then
      FParamCount := anIdx;
  end
  else
    LBLogger.Write(1, 'TPQDirectConnection.SetParam', lmt_Warning, 'Param index %d out of range.', [anIdx]);
end;

function TPQDirectConnection.SetParam(anIdx: Integer; AValue: Integer): Boolean;
begin
  Result := SetParam(anIdx, IntToStr(AValue));
end;

function TPQDirectConnection.SetParam(anIdx: Integer; AValue: Int64): Boolean;
begin
  Result := SetParam(anIdx, IntToStr(AValue));
end;

function TPQDirectConnection.SetParam(anIdx: Integer; AValue: Double): Boolean;
var
  S: string;
begin
  S := FloatToStr(AValue, gv_FormatSettings);
  Result := SetParam(anIdx, S);
end;

function TPQDirectConnection.SetParam(anIdx: Integer; AValue: Boolean): Boolean;
begin
  if AValue then
    Result := SetParam(anIdx, 'true')
  else
    Result := SetParam(anIdx, 'false');
end;

function TPQDirectConnection.SetParamNull(anIdx: Integer): Boolean;
begin
  Result := (anIdx >= 1) and (anIdx <= MAX_PQ_PARAMS);
  if Result then
  begin
    FParams[anIdx - 1].Data   := '';
    FParams[anIdx - 1].IsNull := True;
    if anIdx > FParamCount then
      FParamCount := anIdx;
  end
  else
    LBLogger.Write(1, 'TPQDirectConnection.SetParamNull', lmt_Warning, 'Param index %d out of range.', [anIdx]);
end;

function TPQDirectConnection.ExecuteQuery: TPQResultSet;
var
  Res        : PPGresult;
  Status     : TExecStatusType;
  _SavedSQL  : AnsiString;

begin
  Result := nil;

  if Self.CheckHandle then
  begin
    if FPreparedSQL <> '' then
    begin

      Self.BuildParamArrays;

      if FParamCount > 0 then
        Res := PQexecParams(
          FHandle,
          PChar(FPreparedSQL),
          FParamCount,
          nil,           // paramTypes: nil = inferisce il tipo dal contesto
          @FParamValues,
          @FParamLengths,
          @FParamFormats,
          0)             // resultFormat: 0 = testo
      else
        Res := PQexec(FHandle, PChar(FPreparedSQL));

      { Salva l'SQL per il log, poi resetta sempre lo stato }
      _SavedSQL    := FPreparedSQL;
      FPreparedSQL := '';
      Self.ClearParams;

      if Res <> nil then
      begin

        Status := PQresultStatus(Res);

        if Status in [PGRES_TUPLES_OK, PGRES_COMMAND_OK] then
          Result := TPQResultSet.Create(Res)
        else begin
          LBLogger.Write(1, 'TPQDirectConnection.ExecuteQuery', lmt_Warning, 'Query error [%s]: %s', [_SavedSQL, StrPas(PQresultErrorMessage(Res))]);
          PQclear(Res);
        end;

      end
      else
        LBLogger.Write(1, 'TPQDirectConnection.ExecuteQuery', lmt_Warning, 'PQexecParams returned nil: %s', [StrPas(PQerrorMessage(FHandle))]);

    end
    else
      LBLogger.Write(1, 'TPQDirectConnection.ExecuteQuery', lmt_Warning, 'No SQL statement prepared.');

  end;
end;

function TPQDirectConnection.ExecuteNonQuery: Boolean;
var
  RS: TPQResultSet;
begin
  RS := ExecuteQuery;
  Result := Assigned(RS);
  if Assigned(RS) then
    RS.Free;
end;

{ ------------------------------------------------------------------ }
{ LISTEN / NOTIFY                                                      }
{ ------------------------------------------------------------------ }

function TPQDirectConnection.ProcessNotifications: Boolean;
var
  Notify: PPGnotify;
begin
  Result := False;

  if FHandle <> nil then
  begin

    if PQstatus(FHandle) <> CONNECTION_BAD then
    begin

      if PQconsumeInput(FHandle) <> 0 then
      begin

        Result := True;

        Notify := PQnotifies(FHandle);
        while Notify <> nil do
        begin
          if Assigned(FOnNotify) then
            FOnNotify(Self, StrPas(Notify^.relname), StrPas(Notify^.extra));
          PQfreemem(Notify);
          Notify := PQnotifies(FHandle);
        end;

      end
      else begin
        LBLogger.Write(1, 'TPQDirectConnection.ProcessNotifications', lmt_Warning, 'PQconsumeInput failed: %s', [StrPas(PQerrorMessage(FHandle))]);
        FConnected := False;
      end;

    end
    else begin
      LBLogger.Write(1, 'TPQDirectConnection.ProcessNotifications', lmt_Warning, 'PostgreSQL connection lost!');
      FConnected := False;
    end;
  end;
end;

initialization
  { libpq restituisce sempre il punto come separatore decimale }
  gv_FormatSettings.ThousandSeparator := #0;
  gv_FormatSettings.DecimalSeparator := '.';

end.
