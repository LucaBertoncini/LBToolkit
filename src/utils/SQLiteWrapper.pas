unit SQLiteWrapper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Dyn, uLBUtils;

type



  { TSQLiteDBManager }

  TSQLiteDBManager = class(TObject)
    public
      type
        TNewDBEvent = procedure (Sender: TObject; anUTCCreationTime: TDateTime) of object;

    strict private
      FOnNewDBEvent : TNewDBEvent;
      FDBHandle : psqlite3;
      FStatement : psqlite3_stmt;

      FEmptyRecordset : Boolean;
      FEOF : Boolean;

      FCSLock : TTimedOutCriticalSection;

      FStatementResetNeeded : Boolean;

      FCreationDate : TDateTime;

      FTransactionActive : Boolean;

      procedure resetStatementIfNeeded();

      function ClearSQLStatement(): Boolean;

      function Rollback(logError: Boolean = True): Boolean;

    public
      constructor Create();
      destructor Destroy; override;

      function ConnectDB(const aDBName: String; const aPassword: String; anUTCCreationTime: TDateTime): Boolean;
      function CloseConnection(): Boolean;

      function Lock(const anOwner: String): Boolean;
      procedure Unlock();

      function Commit(): Boolean;
      function startTransaction(): Boolean;

      function executeSQL(aSQLStatements: TStringList): Boolean; overload;
      function executeSQL(const aSingleStatement: String): Boolean; overload;

      function prepareSQLStatement(const aStatement: String): Boolean;

      function setParam(anIdx: Integer; AValue: Byte): Boolean; overload;
      function setParam(anIdx: Integer; AValue: Integer): Boolean; overload;
      function setParam(anIdx: Integer; AValue: Int64): Boolean; overload;
      function setParam(anIdx: Integer; AValue: Cardinal): Boolean; overload;
      function setParam(anIdx: Integer; AValue: Double): Boolean; overload;
      function setParam(anIdx: Integer; const AValue: String; isStaticString: Boolean): Boolean; overload;
      function setParam(anIdx: Integer; aBuffer: pByte; aBufferLen: Integer): Boolean; overload;


      function executeQuery(): Boolean;
      function nextRow(): Boolean;
      procedure closeQuery();

      function ColumnValueIsNull(aColIdx: Integer): Boolean;

      function getColumnAsDouble(aColIdx: Integer): Double;
      function getColumnAsInteger(aColIdx: Integer): Integer;
      function getColumnAsInt64(aColIdx: Integer): Int64;
      function getColumnAsText(aColIdx: Integer): String;
      function getColumnAsBlob(aColIdx: Integer; out aBuffer: pByte; out aBufferLen: Integer): Boolean;

      property isEmpty: Boolean read FEmptyRecordset;
      property EOF: Boolean read FEOF;
      property OnNewDB: TNewDBEvent write FOnNewDBEvent;
      property CreationDate: TDateTime read FCreationDate write FCreationDate;
  end;

  function initializeSQLiteWrapper(useCipher: Boolean): Boolean;
  function isDatabaseAttached(aDB: TSQLiteDBManager; const aDBAlias: String): Boolean;

implementation

uses
  ULBLogger, LazFileUtils;

var
  gv_SQLiteInitialized  : Boolean = False;

const
  cCommit           : PChar = 'COMMIT'; // null terminated string
  cRollBack         : PChar = 'ROLLBACK';
  cBeginTransaction : PChar = 'BEGIN TRANSACTION';

function isDatabaseAttached(aDB: TSQLiteDBManager; const aDBAlias: String): Boolean;
begin
  Result := False;

  aDB.prepareSQLStatement('PRAGMA DATABASE_LIST;');
  aDB.executeQuery();
  if not aDB.isEmpty then
  begin
    while not aDB.EOF do
    begin
      if aDB.getColumnAsText(1) = aDBAlias then
      begin
        Result := True;
        Break;
      end
      else
        aDB.nextRow();
    end;
  end;
  aDB.closeQuery();

end;

function initializeSQLiteWrapper(useCipher: Boolean): Boolean;
var
  _res : Integer;

begin
  Result := False;

  if not gv_SQLiteInitialized then
  begin
    gv_SQLiteInitialized := True;
    if useCipher then
      _res := InitializeSqliteANSI('libsqlcipher.so')
    else
      _res := InitializeSqliteANSI();

    if _res <= 0 then
    begin
      LBLogger.Write(1, 'initialiseSQLiteWrapper', lmt_Warning, 'SQLite not loaded!');
      gv_SQLiteInitialized := False;
    end
    else begin
      LBLogger.Write(5, 'initialiseSQLiteWrapper', lmt_Debug, 'SQLite library loaded!');
      Result := True;
    end;
  end;
end;

{ TSQLiteDBManager }

function TSQLiteDBManager.startTransaction: Boolean;
var
  _errmsg : pAnsiChar = nil;
  _sError : String;

begin
  if not FTransactionActive then
  begin

    Result := sqlite3_exec(FDBHandle, cBeginTransaction, nil, nil, @_errmsg) = SQLITE_OK;
    if Result then
      FTransactionActive := True
    else begin
      _sError := StrPas(_errmsg);
      LBLogger.Write(1, 'TSQLiteDBManager.startTransaction', lmt_Warning, 'Not enable to start transaction: <%s>', [_sError {AnsiString(sqlite3_errmsg(FDBHandle))}]);
    end;

    if _errmsg <> nil then
      sqlite3_free(_errmsg);

  end
  else begin
    Result := False;
    LBLogger.Write(1, 'TSQLiteDBManager.startTransaction', lmt_Warning, 'Transaction already active!');
  end;

end;

function TSQLiteDBManager.Commit: Boolean;
var
  _errmsg : pAnsiChar = nil;
  _sError : String;

begin
  Result := False;

  if FDBHandle <> nil then
  begin
    if FTransactionActive then
    begin
      Self.ClearSQLStatement();

      Result := sqlite3_exec(FDBHandle, cCommit, nil, nil, @_errmsg) = SQLITE_OK;
      if Result then
        FTransactionActive := False
      else begin
        _sError := StrPas(_errmsg);
        LBLogger.Write(1, 'TSQLiteDBManager.Commit', lmt_Warning, 'Error during commit: <%s>', [_sError]);
        raise Exception.Create(Format('Commit not executed: <%s>',[_sError]));
      end;

      if _errmsg <> nil then
        sqlite3_free(_errmsg);
    end
    else
      LBLogger.Write(1, 'TSQLiteDBManager.Commit', lmt_Warning, 'Transaction not active!');
  end
  else
    LBLogger.Write(1, 'TSQLiteDBManager.Commit', lmt_Warning, 'Database not connected!');
end;

function TSQLiteDBManager.Rollback(logError: Boolean): Boolean;
var
  _errmsg : PAnsiChar = nil;
  _sError : String;

begin
  Result := False;

  if FDBHandle <> nil then
  begin
    Result := sqlite3_exec(FDBHandle, cRollBack, nil, nil, @_errmsg) = SQLITE_OK;
    FTransactionActive := False;

    if logError and (not Result) then
    begin
      _sError := StrPas(_errmsg);
      LBLogger.Write(1, 'TSQLiteDBManager.Rollback', lmt_Warning, 'Error during rollback: <%s>', [_sError]);
    end;

    if _errmsg <> nil then
      sqlite3_free(_errmsg);
  end
  else
    LBLogger.Write(1, 'TSQLiteDBManager.Rollback', lmt_Warning, 'Database not connected!');
end;

function TSQLiteDBManager.executeSQL(aSQLStatements: TStringList): Boolean;
var
  _errmsg : PAnsiChar = nil;
  _sError : String;
  i : Integer;
  _Statement : PChar = nil;


begin
  Result := False;

  if (aSQLStatements <> nil) and (aSQLStatements.Count > 0) then
  begin
    Self.ClearSQLStatement();
    FEOF := True;
    FEmptyRecordset := True;

    for i := 0 to aSQLStatements.Count - 1 do
    begin
      _Statement := uLBUtils.getNullTerminatedString(aSQLStatements.Strings[i]);
      Result := sqlite3_exec(FDBHandle, _Statement{PAnsiChar(aSQLStatements.Strings[i])}, nil, nil, @_errmsg) = SQLITE_OK;
      if not Result then
      begin
        _sError := StrPas(_errmsg);
        LBLogger.Write(1, 'TSQLiteDBManager.executeSQL', lmt_Warning, 'Statement %d not executed: <%s>', [i + 1, _sError {AnsiString(sqlite3_errstr(FDBHandle))}]);
        raise Exception.Create(Format('Statement <%s> not executed: <%s>',[aSQLStatements.Strings[i], _sError {AnsiString(sqlite3_errstr(FDBHandle))}]));
      end;

      uLBUtils.disposeNullTerminatedString(_Statement);

      if _errmsg <> nil then
      begin
        sqlite3_free(_errmsg);
        _errmsg := nil;
      end;
    end;
  end;
end;

function TSQLiteDBManager.executeSQL(const aSingleStatement: String): Boolean;
var
  _errmsg : PAnsiChar = nil;
  _Statement : PChar = nil;
  _sError : String;

begin
  Result := False;

  if Length(aSingleStatement) > 0 then
  begin
    Self.ClearSQLStatement();

    FEOF := True;
    FEmptyRecordset := True;

    _Statement := uLBUtils.getNullTerminatedString(aSingleStatement);

    Result := sqlite3_exec(FDBHandle, _Statement, nil, nil, @_errmsg) = SQLITE_OK;
    if not Result then
    begin
      _sError := StrPas(_errmsg);
      LBLogger.Write(1, 'TSQLiteDBManager.executeSQL', lmt_Warning, 'Statement <%s> not executed: <%s>', [aSingleStatement, _sError]);
      raise Exception.Create(Format('Statement <%s> not executed: <%s>',[aSingleStatement, _sError]));
    end;

    uLBUtils.disposeNullTerminatedString(_Statement);

    if _errmsg <> nil then
      sqlite3_free(_errmsg);
  end;
end;

function TSQLiteDBManager.prepareSQLStatement(const aStatement: String): Boolean;
begin
  Result := False;

  if Length(aStatement) > 0 then
  begin
    Self.ClearSQLStatement();


    Result := sqlite3_prepare_v2(FDBHandle, @aStatement[1], Length(aStatement), @FStatement, nil) = SQLITE_OK;
    if not Result then
    begin
      LBLogger.Write(1, 'TSQLiteDBManager.prepareSQLStatement', lmt_Warning, 'Cannot prepare query <%s>: <%s>', [aStatement, AnsiString(sqlite3_errmsg(FDBHandle))]);
      Self.ClearSQLStatement();
      raise Exception.Create('Query not prepared!');
    end;

  end;
end;

function TSQLiteDBManager.setParam(anIdx: Integer; AValue: Byte): Boolean;
begin
  Self.resetStatementIfNeeded;

  Result := sqlite3_bind_int(FStatement, anIdx, Integer(AValue)) = SQLITE_OK;
  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.set_Param', lmt_Warning, 'Byte param %d not set to value %d', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; AValue: Integer): Boolean;
begin
  Self.resetStatementIfNeeded;

  Result := sqlite3_bind_int(FStatement, anIdx, AValue) = SQLITE_OK;
  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.set_Param', lmt_Warning, 'Integer param %d not set to value %d', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; AValue: Int64): Boolean;
begin
  Self.resetStatementIfNeeded;

  Result := sqlite3_bind_int64(FStatement, anIdx, AValue) = SQLITE_OK;
  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.set_Param', lmt_Warning, 'Int64 param %d not set to value %d', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; AValue: Cardinal): Boolean;
begin
  Self.resetStatementIfNeeded;

  Result := sqlite3_bind_int64(FStatement, anIdx, Int64(AValue)) = SQLITE_OK;
  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.set_Param', lmt_Warning, 'Cardinal param %d not set to value %d', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; AValue: Double): Boolean;
begin
  Self.resetStatementIfNeeded;

  Result := sqlite3_bind_double(FStatement, anIdx, AValue) = SQLITE_OK;

  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.setParam', lmt_Warning, 'Double param %d not set to value <%f>', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; const AValue: String; isStaticString: Boolean): Boolean;
begin
  Self.resetStatementIfNeeded;

  if Length(AValue) = 0 then
    Result := sqlite3_bind_null(FStatement, anIdx) = SQLITE_OK
  else begin
    if isStaticString then
      Result := sqlite3_bind_text(FStatement, anIdx, PAnsiChar(@AValue[1]), Length(AValue), SQLITE_STATIC) = SQLITE_OK
    else
      Result := sqlite3_bind_text(FStatement, anIdx, PAnsiChar(@AValue[1]), Length(AValue), sqlite3_destructor_type(SQLITE_TRANSIENT)) = SQLITE_OK;
  end;

  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.setParam', lmt_Warning, 'Text param %d not set to value <%s>', [anIdx, AValue]);
end;

function TSQLiteDBManager.setParam(anIdx: Integer; aBuffer: pByte; aBufferLen: Integer): Boolean;
begin
  Self.resetStatementIfNeeded;

  if (aBuffer = nil) or (aBufferLen = 0) then
    Result := sqlite3_bind_null(FStatement, anIdx) = SQLITE_OK
  else
    Result := sqlite3_bind_blob(FStatement, anIdx, aBuffer, aBufferLen, SQLITE_STATIC) = SQLITE_OK;

  if not Result then
    LBLogger.Write(1, 'TSQLiteDBManager.setParam', lmt_Warning, 'Buffer param %d not set!', [anIdx]);
end;

function TSQLiteDBManager.executeQuery: Boolean;
begin
  Result := False;

  if FStatement <> nil then
  begin
    case sqlite3_step(FStatement) of

      SQLITE_DONE:
        begin
          Result := True;
          FEmptyRecordset := True;
          FEOF := True;
        end;

      SQLITE_ROW:
        begin
          Result := True;
          FEmptyRecordset := False;
          FEOF := False;
        end;

      else begin
        FEmptyRecordset := True;
        FEOF := True;
        raise Exception.Create(Format('Query not executed: <%s>', [AnsiString(sqlite3_errmsg(FDBHandle))]));
      end;
    end;

    FStatementResetNeeded := True;
  end;
end;

function TSQLiteDBManager.nextRow: Boolean;
begin
  Result := sqlite3_step(FStatement) = SQLITE_ROW;
  FEOF := not Result;
end;

procedure TSQLiteDBManager.closeQuery;
begin
  if FStatement <> nil then
  begin
    sqlite3_clear_bindings(FStatement);
    sqlite3_reset(FStatement);
    sqlite3_finalize(FStatement);

    FStatement := nil;
   end;
end;

function TSQLiteDBManager.ColumnValueIsNull(aColIdx: Integer): Boolean;
begin
  Result := sqlite3_column_type(FStatement, aColIdx) = SQLITE_NULL;
end;

function TSQLiteDBManager.getColumnAsDouble(aColIdx: Integer): Double;
begin
  Result := sqlite3_column_double(FStatement, aColIdx);
end;

function TSQLiteDBManager.getColumnAsInteger(aColIdx: Integer): Integer;
begin
  Result := sqlite3_column_int(FStatement, aColIdx)
end;

function TSQLiteDBManager.getColumnAsInt64(aColIdx: Integer): Int64;
begin
  Result := sqlite3_column_int64(FStatement, aColIdx)
end;

function TSQLiteDBManager.getColumnAsText(aColIdx: Integer): String;
begin
  Result := StrPas(sqlite3_column_text(FStatement, aColIdx));
end;

function TSQLiteDBManager.getColumnAsBlob(aColIdx: Integer; out aBuffer: pByte; out aBufferLen: Integer): Boolean;
begin
  aBuffer := sqlite3_column_blob(FStatement, aColIdx);
  aBufferLen := sqlite3_column_bytes(FStatement, aColIdx);

  Result := (aBuffer <> nil) and (aBufferLen > 0);
end;

procedure TSQLiteDBManager.resetStatementIfNeeded;
begin
  if (FStatement <> nil) and FStatementResetNeeded then
  begin
    sqlite3_clear_bindings(FStatement);
    sqlite3_reset(FStatement);
    FStatementResetNeeded := False;
  end;
end;

function TSQLiteDBManager.ClearSQLStatement: Boolean;
begin
  if FStatement <> nil then
  begin
    sqlite3_clear_bindings(FStatement);
    Result := sqlite3_finalize(FStatement) = SQLITE_OK;
    FStatement := nil;
    FStatementResetNeeded := False;
    if not Result then
      LBLogger.Write(1, 'TSQLiteDBManager.cleanSQLStatement', lmt_Warning, 'Error finalizing statement: <%s>', [AnsiString(sqlite3_errmsg(FDBHandle))]);
  end
  else
    Result := True;
end;

constructor TSQLiteDBManager.Create;
begin
  inherited Create;

  FOnNewDBEvent := nil;
  FDBHandle     := nil;
  FStatement    := nil;

  FCSLock       := TTimedOutCriticalSection.Create;

  FCreationDate := 0;

  FTransactionActive := False;

//  initialiseSQLiteWrapper(useCipher);

(*

  if useCipher then
    _libName := 'libsqlcipher.so';

  if InitializeSqliteANSI(_libName) <= 0 then
    LBLogger.Write(1, 'TSQLiteDBManager.Create', lmt_Warning, 'SQLite not loaded!');

*)

end;

destructor TSQLiteDBManager.Destroy;
begin
  try

    FreeAndNil(FCSLock);

    Self.ClearSQLStatement();
    Self.CloseConnection();

  except
    on E: Exception do
      LBLogger.Write(1, 'TSQLiteDBManager.Destroy', lmt_Error, PChar(E.Message));
  end;

//  ReleaseSqlite;
  inherited Destroy;
end;

function TSQLiteDBManager.ConnectDB(const aDBName: String; const aPassword: String; anUTCCreationTime: TDateTime): Boolean;
var
  _NewDB : Boolean;
  _DBName : PChar = nil;
  _Error : Boolean;

begin
  Result := False;

  _Error := False;

  if gv_SQLiteInitialized then
  begin

    _DBName := uLBUtils.getNullTerminatedString(aDBName);

    if _DBName <> nil then
    begin
      Self.CloseConnection();
      if FileExists(aDBName) then
      begin
        if FileSizeUtf8(aDBName) = 0 then
        begin
          LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'File <%s> already exists but size = 0. Deleting file ...', [aDBName]);
          if not DeleteFileUTF8(aDBName) then
          begin
            LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'File <%s> not deleted!', [aDBName]);
            _Error := True;
          end;
        end;
      end;

      if not _Error then
      begin
        _NewDB := not FileExists(aDBName);


        if sqlite3_open_v2(_DBName, @FDBHandle, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE, nil) = SQLITE_OK then
        begin
          if Length(aPassword) > 0 then
          begin
            Result := sqlite3_key(FDBHandle, @aPassWord[1], Length(aPassword)) = SQLITE_OK;
            if not Result then
              LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'Not enable to set password for DB <%s>: <%s>', [aDBName, AnsiString(sqlite3_errmsg(FDBHandle))]);
          end
          else
            Result := True;

          if Result then
          begin
            Self.prepareSQLStatement('PRAGMA locking_mode=EXCLUSIVE;');
            Self.executeQuery();
            Self.prepareSQLStatement('PRAGMA synchronous=FULL;');
            Self.executeQuery();
            Self.prepareSQLStatement('PRAGMA journal_mode=WAL;');
            Self.executeQuery();
          end;

          if Result and _NewDB then
          begin
            try
              if Assigned(FOnNewDBEvent) then
              begin
                if anUTCCreationTime = 0 then
                  anUTCCreationTime := NowUTC();

                FOnNewDBEvent(Self, anUTCCreationTime);

                FCreationDate := anUTCCreationTime;
              end;
            except
              on E: Exception do
                LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Error, PChar(E.Message));
            end;
          end;
        end
        else begin
          LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'DB <%s> not opened: <%s>', [aDBName, AnsiString(sqlite3_errmsg(FDBHandle))]);
          Self.CloseConnection();
        end;

        uLBUtils.disposeNullTerminatedString(_DBName);
      end;
    end
    else
      LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'No database name!');

  end
  else
    LBLogger.Write(1, 'TSQLiteDBManager.ConnectDB', lmt_Warning, 'SQLite not initialized');
end;

function TSQLiteDBManager.CloseConnection: Boolean;
begin
  if FDBHandle <> nil then
  begin
    Self.Rollback(False);

    Result := sqlite3_close_v2(FDBHandle) = SQLITE_OK;
    if Result then
      FDBHandle := nil
    else
      LBLogger.Write(1, 'TSQLiteDBManager.CloseConnection', lmt_Warning, 'Error closing DB connection!');
  end
  else
    Result := True;
end;

function TSQLiteDBManager.Lock(const anOwner: String): Boolean;
begin
  Result := False;

  if FDBHandle <> nil then
  begin
    if FCSLock.Acquire(anOwner) then
    begin
      Result := Self.startTransaction();
      if not Result then
        FCSLock.Release();
    end;
  end
  else
    LBLogger.Write(1, 'TSQLiteDBManager.Lock', lmt_Warning, 'Database not connected!');
end;

procedure TSQLiteDBManager.Unlock;
begin
  if FDBHandle <> nil then
  begin
    Self.Rollback(False);
    FCSLock.Release();
  end;
end;


finalization
  if gv_SQLiteInitialized then
    ReleaseSqlite;

end.

