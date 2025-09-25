unit uTokenManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, contnrs, SQLiteWrapper, uTimedoutCriticalSection,
  uLBBaseThread, fpjson, IniFiles;

type

  TDBTokenUpdater = class;

  { TTokenManager }

  TTokenManager = class(TObject)
    strict private
      FCS        : TTimedOutCriticalSection;
      FTokens    : TStringList;
      FDBManager : TSQLiteDBManager;
      FUpdater   : TDBTokenUpdater;

      FTokensLoaded     : Boolean;

      FRemoveTokens : Boolean;
      FInactiveTime : Int64;



      function CreateToken(): String;
      procedure StopUpdater();
      function CreateTokenTable(Sender: TObject; anUTCCreationTime: TDateTime): Boolean;

      function LoadActiveTokens(): Boolean;

      const
        cTimelessTokenNodeName = DOMString('TimelessToken');
        cTokenManagerSessionName = String('TokenManager');
        cTokenExpirationTime = String('TokenExpirationTime');
        cDBFile = String('DBFile');
        cDefaultTokenExpirationTime = Int64(20 * 60); // 20 min

    private
      function isUnNeededToken(aToken: TJSONObject): Boolean;
      function UpdateTokens(aTokenList: TObjectList): Boolean;
      function RemoveUnneededTokens(TokensToRemove: TStringList): Boolean;

      property DBManager: TSQLiteDBManager read FDBManager;

    public
      constructor Create();
      destructor Destroy; override;

      function Activate(): Boolean;

      procedure Clear();

      function LoadConfigurationFromIniFile(const aFilename: String): Boolean; overload;
      function LoadConfigurationFromIniFile(aIniFile: TIniFile): Boolean; overload;

      class function GenerateSecureRandomBytes(aLength: Integer; out aBuffer: TBytes): Boolean;

      function LockTokens(): TStringList;
      procedure UnlockTokens();

      function isValidRequest(aRequest: TJSONObject; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;

      function getToken(const aToken: String): TJSONObject;
      function addNewToken(IdUser: Integer; ConfigData: TJSONObject; out aTokenData: TJSONObject): Boolean;
      function RemoveToken(aToken: String): Boolean;

      property RemoveTokens: Boolean write FRemoveTokens;
  end;

  { TDBTokenUpdater }

  TDBTokenUpdater = class(TLBBaseThread)
    strict private
      FTokenManager : TTokenManager;

    protected
      procedure Execute; override;

    public
      constructor Create(aTokenManager: TTokenManager); reintroduce;
  end;


const
  cToken_Field_Updated    = String('updated');
  cToken_Field_Token      = String('token');
  cToken_Field_IdUser     = String('IdUser');
  cToken_Field_LastUpdate = String('LastUpdate');
  cToken_Field_UserConfig = String('UsrConf');


implementation

uses
  strutils, System.NetEncoding, ULBLogger, LazSysUtils, DateUtils, jsonparser, jsonscanner
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;


{ TDBTokenUpdater }

procedure TDBTokenUpdater.Execute;
var
  _TokensToUpdate: TObjectList;
  _TokensToRemove: TStringList;
  _aToken: TJSONObject;
  i: Integer;
  _Tokens : TStringList;

const
  cWaitTime = 5 * 60 * 1000; // 5 minutes
  cWaitBeforeStart = 60 * 1000; // 1 minute
begin
  if FTokenManager = nil then Exit;

  Self.PauseFor(cWaitBeforeStart);

  _TokensToUpdate := TObjectList.Create(False);
  _TokensToRemove := TStringList.Create;
  try
    while not Self.Terminated do
    begin
      _TokensToUpdate.Clear;
      _TokensToRemove.Clear;

      _Tokens := FTokenManager.LockTokens();

      // Acquire the single lock for all operations
      if _Tokens <> nil then
      begin
        try
          // 1. Build lists of work to do
          for i := _Tokens.Count - 1 downto 0 do
          begin
            _aToken := TJSONObject(_Tokens.Objects[i]);
            if (_aToken.Find(cToken_Field_Updated) <> nil) and (_aToken.Elements[cToken_Field_Updated].AsBoolean) then
              _TokensToUpdate.Add(_aToken)
            else if FTokenManager.isUnNeededToken(_aToken) then
              _TokensToRemove.Add(_Tokens.Strings[i]);
          end;

          // 2. Perform DB operations inside the lock if there's work to do
          if (_TokensToUpdate.Count > 0) or (_TokensToRemove.Count > 0) then
          begin
            FTokenManager.UpdateTokens(_TokensToUpdate);
            FTokenManager.RemoveUnneededTokens(_TokensToRemove);
          end;

        except
          on E: Exception do
            LBLogger.Write(1, 'TDBTokenUpdater.Execute', lmt_Error, '2. %s', [E.Message]);
        end;
        FTokenManager.UnlockTokens();
      end;

      Self.PauseFor(cWaitTime);
    end;
  finally
    _TokensToUpdate.Free;
    _TokensToRemove.Free;
  end;
end;

constructor TDBTokenUpdater.Create(aTokenManager: TTokenManager);
begin
  inherited Create();

  FTokenManager := aTokenManager;
end;



{ TTokenManager }

constructor TTokenManager.Create();
begin
  inherited Create();

  FInactiveTime := cDefaultTokenExpirationTime;

  FCS := TTimedOutCriticalSection.Create;

  FTokens := TStringList.Create;
  FTokens.OwnsObjects := True;
  FTokens.Sorted := True;
  FTokens.CaseSensitive := True;

  FDBManager := nil;
  FUpdater := nil;

  FTokensLoaded := False;

  FRemoveTokens := True;
end;

destructor TTokenManager.Destroy;
begin

  try

    Self.StopUpdater();

    FreeAndNil(FCS);
    FreeAndNil(FTokens);

    if FDBManager <> nil then
      FreeAndNil(FDBManager);

  except
    on E: Exception do
      LBLogger.Write(1, 'TTokenManager.Destroy', lmt_Error, PChar(E.Message));
  end;

  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
type
  HCRYPTPROV = Pointer;

function CryptAcquireContextA(out phProv: HCRYPTPROV; pszContainer: PAnsiChar; pszProvider: PAnsiChar; dwProvType: DWord; dwFlags: DWord): Boolean; stdcall; external 'advapi32.dll' name 'CryptAcquireContextA';
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWord; pbBuffer: PByte): Boolean; stdcall; external 'advapi32.dll' name 'CryptGenRandom';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWord): Boolean; stdcall; external 'advapi32.dll' name 'CryptReleaseContext';

const
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT = $F0000000;
{$ENDIF}

class function TTokenManager.GenerateSecureRandomBytes(aLength: Integer; out aBuffer: TBytes): Boolean;
{$IFDEF MSWINDOWS}
var
  hProv: HCRYPTPROV;
begin
  Result := False;
  SetLength(aBuffer, 0);
  hProv := 0;
  if not CryptAcquireContextA(hProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
  begin
    LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Error, 'CryptAcquireContext failed');
    Exit;
  end;
  try
    SetLength(aBuffer, aLength);
    Result := CryptGenRandom(hProv, aLength, @aBuffer[0]);
    if not Result then
      LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Error, 'CryptGenRandom failed');
  finally
    CryptReleaseContext(hProv, 0);
  end;
end;
{$ELSE}
var
  f: TFileStream;
begin
  Result := False;
  SetLength(aBuffer, 0);
  try
    f := TFileStream.Create('/dev/urandom', fmOpenRead);
    SetLength(aBuffer, aLength);
    if f.Read(aBuffer[0], aLength) = aLength then
      Result := True
    else
      LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Warning, 'Failed to read from /dev/urandom');
  except
    on E: Exception do
      LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Error, 'Error accessing /dev/urandom: ' + E.Message);
  end;
  f.Free;
end;
{$ENDIF}


function TTokenManager.CreateToken(): String;
var
  RandomBytes: TBytes;
begin
  Result := '';
  // Generate 32 secure random bytes (will result in a ~43 char Base64 string)
  if Self.GenerateSecureRandomBytes(32, RandomBytes) then
  begin
    Result := TNetEncoding.Base64.EncodeBytesToString(RandomBytes);

    // Make the token URL-safe
    Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
    Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
    Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  end
  else
    LBLogger.Write(1, 'TTokenManager.CreateToken', lmt_Warning, 'Failed to generate a secure token');
end;


function TTokenManager.Activate(): Boolean;
begin
  Result := False;

  if FUpdater = nil then
  begin
    FUpdater := TDBTokenUpdater.Create(Self);
    FUpdater.AddReference(@FUpdater);
    FUpdater.Start();

    Result := True;
  end;

end;

procedure TTokenManager.Clear();
begin

  if FCS.Acquire('TTokenManager.Clear') then
  begin

    try
      FTokens.Clear;

    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.Clear', lmt_Error, E.Message);
    end;

    FCS.Release();
  end;

end;

function TTokenManager.LoadConfigurationFromIniFile(const aFilename: String): Boolean;
var
  _IniFile : TIniFile = nil;

begin
  Result := False;

  if aFilename <> '' then
  begin
    if FileExists(aFilename) then
    begin
      try

        _IniFile := TIniFile.Create(aFilename);
        Result := Self.LoadConfigurationFromIniFile(_IniFile);

      except
        on E: Exception do
          LBLogger.Write(1, 'TTokenManager.LoadConfigurationFromIniFile', lmt_Error, E.Message);
      end;

      if _IniFile <> nil then
        _IniFile.Free;
    end
    else
      LBLogger.Write(1, 'TTokenManager.LoadConfigurationFromIniFile', lmt_Warning, 'File <%s> not found!', [aFilename]);

  end
  else
    LBLogger.Write(1, 'TTokenManager.LoadConfigurationFromIniFile', lmt_Warning, 'No file name!');

end;

function TTokenManager.LoadConfigurationFromIniFile(aIniFile: TIniFile): Boolean;
var
  _DBFilename : String;
  _Path : String;

begin
  Result := False;

  if FDBManager <> nil then
    FreeAndNil(FDBManager);

  if aIniFile <> nil then
  begin
    try

      if aIniFile.SectionExists(cTokenManagerSessionName) then
      begin
        _DBFilename := aIniFile.ReadString(cTokenManagerSessionName, cDBFile, '');
        if _DBFilename <> '' then
        begin
          _DBFilename := ExpandFileName(_DBFilename);

          _Path := ExtractFilePath(_DBFilename);
          if not DirectoryExists(_Path) then
            ForceDirectories(_Path);

          FInactiveTime := aIniFile.ReadInteger(cTokenManagerSessionName, cTokenExpirationTime, cDefaultTokenExpirationTime);

          FDBManager := TSQLiteDBManager.Create();
          FDBManager.OnNewDB := @Self.CreateTokenTable;
          Result := FDBManager.ConnectDB(_DBFilename, '', NowUTC());

        end;

      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.LoadConfigurationFromIniFile', lmt_Error, E.Message);
    end;
  end;
end;


function TTokenManager.LockTokens(): TStringList;
begin
  Result := nil;

  if FCS.Acquire('TTokenManager.LockList') then
    Result := FTokens;
end;

procedure TTokenManager.UnlockTokens();
begin
  FCS.Release();
end;

function TTokenManager.isValidRequest(aRequest: TJSONObject; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;
var
  _sToken : TJSONString;
  _Token : TJSONObject;

begin
  Result := False;
  anUserId := 0;
  anUserConfig := nil;

  if aRequest <> nil then
  begin
    if aRequest.Find(cToken_Field_Token, _sToken) then
    begin
      _Token := Self.getToken(_sToken.AsString);
      if _Token <> nil then
      begin
        anUserId := _Token.Elements[cToken_Field_IdUser].AsInteger;
        anUserConfig := TJSONObject(_Token.Elements[cToken_Field_UserConfig]);
        Result := True;
      end
      else
        LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Invalid token <%s>!', [_sToken]);

    end
    else
      LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Token field not found: <%s>', [aRequest.AsJSON]);
  end;
end;

function TTokenManager.getToken(const aToken: String): TJSONObject;
var
  _Idx : Integer;
begin
  Result := nil;
  if Length(aToken) = 0 then Exit;

  // Thread-safe lazy loading with double-checked locking pattern
  if not FTokensLoaded then
  begin
    if FCS.Acquire('TTokenManager.getToken.LazyLoad') then
    try
      // Check again inside the lock
      if not FTokensLoaded then
        Self.LoadActiveTokens();
    finally
      FCS.Release();
    end;
  end;

  if FCS.Acquire('TTokenManager.getToken') then
  begin
    try
      if FTokens.Find(aToken, _Idx) then
      begin
        Result := TJSONObject(FTokens.Objects[_Idx]);
        Result.Elements[cToken_Field_LastUpdate].AsInt64 := Trunc(NowUTC() * SecsPerDay);
        if Result.Find(cToken_Field_Updated) = nil then
          Result.Add(cToken_Field_Updated, False);
        Result.Elements[cToken_Field_Updated].AsBoolean := True;
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.getToken', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

function TTokenManager.addNewToken(IdUser: Integer; ConfigData: TJSONObject; out aTokenData: TJSONObject): Boolean;
var
  _Index: Integer;
//  _DBSuccess: Boolean;
begin
  Result := False;
  aTokenData := nil;
  _Index := -1;

  if (IdUser <= 0) or (ConfigData = nil) then
    Exit;

  // Create the token object first
  aTokenData := TJSONObject.Create();
  aTokenData.Add(cToken_Field_Token, Self.CreateToken());
  aTokenData.Add(cToken_Field_IdUser, IdUser);
  aTokenData.Add(cToken_Field_LastUpdate, Int64(Trunc(NowUTC() * SecsPerDay)));
  aTokenData.Add(cToken_Field_UserConfig, ConfigData);

  // Acquire a single lock to ensure atomicity for memory and DB operations
  if FCS.Acquire('TTokenManager.addNewToken') then
  begin

    try
      // 1. Add to in-memory cache
      _Index := FTokens.AddObject(aTokenData.Elements[cToken_Field_Token].AsString, aTokenData);
      Result := True;

      // 2. Add to database (if available)
      if FDBManager <> nil then
      begin
        Result := False;

        if FDBManager.Lock('TTokenManager.addNewToken') then
        begin
          try
            // It's better to remove unneeded tokens in the background thread,
            // but if we must do it here, it should be inside the transaction.
            // Self.RemoveUnneededTokens(nil);
            FDBManager.prepareSQLStatement('INSERT INTO Tokens (Token, IdUser, LastRequestTime, Config) ' +
                                           'VALUES (:aTk, :IdUsr, :LastReq, :Cnf);');
            FDBManager.setParam(1, aTokenData.Elements[cToken_Field_Token].AsString, False);
            FDBManager.setParam(2, IdUser);
            FDBManager.setParam(3, aTokenData.Elements[cToken_Field_LastUpdate].AsInt64);
            ConfigData.CompressedJSON := True;
            FDBManager.setParam(4, ConfigData.AsJSON, False);

            FDBManager.executeQuery();
            FDBManager.Commit();

            Result := True;
          except
            on E: Exception do
              LBLogger.Write(1, 'TTokenManager.addNewToken', lmt_Error, '1. %s', [E.Message]);
          end;
          FDBManager.Unlock();
        end;
      end;


      // If anything failed, undo the in-memory add.
      if not Result then
      begin
        if _Index > -1 then
          FTokens.Delete(_Index); // This will free aTokenData as OwnsObjects is true
        aTokenData := nil; // The object is freed by FTokens.Delete
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.addNewToken', lmt_Error, '2. %s', [E.Message]);
    end;
    FCS.Release();
  end;

  // If we failed to acquire the lock in the first place
  if (not Result) and (aTokenData <> nil) then
    FreeAndNil(aTokenData);

end;

function TTokenManager.UpdateTokens(aTokenList: TObjectList): Boolean;
var
  i: Integer;
  _Token, _Config: TJSONObject;
begin
  Result := False;
  if (aTokenList <> nil) and (aTokenList.Count > 0) then
  begin
    if FDBManager.Lock('TTokenManager.UpdateTokens') then
    begin
      try
        FDBManager.prepareSQLStatement('UPDATE Tokens SET LastRequestTime = :LRT, Config = :Cnf WHERE (Token = :TK);');

        for i := 0 to aTokenList.Count - 1 do
        begin
          _Token := TJSONObject(aTokenList.Items[i]);
          _Config := TJSONObject(_Token.Elements[cToken_Field_UserConfig]);
          _Config.CompressedJSON := True;

          FDBManager.setParam(1, _Token.Elements[cToken_Field_LastUpdate].AsInt64);
          FDBManager.setParam(2, _Config.AsJSON, False);
          FDBManager.setParam(3, _Token.Elements[cToken_Field_Token].AsString, False);
          FDBManager.executeQuery();
        end;

        FDBManager.Commit();

        // After successful execution, mark objects as no longer needing an update.
        for i := 0 to aTokenList.Count - 1 do
          TJSONObject(aTokenList.Items[i]).Elements[cToken_Field_Updated].AsBoolean := False;

        Result := True;

      except
        on E: Exception do
          LBLogger.Write(1, 'TTokenManager.UpdateTokens', lmt_Error, E.Message);
      end;
      FDBManager.Unlock();
    end;
  end;
end;

function TTokenManager.RemoveUnneededTokens(TokensToRemove: TStringList): Boolean;
var
  i: Integer;
  _sQuery: String;
  _localList: TStringList = nil;
  _Token: TJSONObject;
begin
  Result := False;
  if FRemoveTokens then
  begin

    // This method can be called with a list (from RemoveToken) or without (from background thread).
    // If no list is provided, we build one.
    if TokensToRemove = nil then
    begin
      _localList := TStringList.Create;
      if FCS.Acquire('TTokenManager.RemoveUnneededTokens') then
      try
        for i := FTokens.Count - 1 downto 0 do
        begin
          _Token := TJSONObject(FTokens.Objects[i]);
          if Self.isUnNeededToken(_Token) then
            _localList.Add(FTokens.Strings[i]);
        end;
      except
        on E: Exception do
          LBLogger.Write(1, 'TTokenManager.RemoveUnneededTokens', lmt_Error, E.Message);
      end;
      FCS.Release();
      TokensToRemove := _localList;
    end;

    try
      if (TokensToRemove <> nil) and (TokensToRemove.Count > 0) then
      begin

        // Now, perform the atomic removal from memory and DB
        if FCS.Acquire('TTokenManager.RemoveUnneededTokens') then
        begin
          try
            // 1. Remove from in-memory list
            for i := 0 to TokensToRemove.Count - 1 do
            begin
              if FTokens.IndexOf(TokensToRemove[i]) > -1 then
                FTokens.Delete(FTokens.IndexOf(TokensToRemove[i]));
            end;

            // 2. Remove from DB
            if (FDBManager <> nil) and FDBManager.Lock('TTokenManager.RemoveUnneededTokens') then
            begin
              try
                _sQuery := 'DELETE FROM Tokens WHERE Token IN (:TK0';
                for i := 1 to TokensToRemove.Count - 1 do
                  _sQuery := _sQuery + ',:TK' + IntToStr(i);
                FDBManager.prepareSQLStatement(_sQuery + ');');

                for i := 0 to TokensToRemove.Count - 1 do
                  FDBManager.setParam(i + 1, TokensToRemove.Strings[i], True);

                FDBManager.executeQuery();
                FDBManager.Commit();
                Result := True;
              except
                on E: Exception do
                begin
                  LBLogger.Write(1, 'TTokenManager.RemoveUnneededTokens', lmt_Error, E.Message);
                  Result := False;
                  // NOTE: A DB error here leaves memory and DB out of sync.
                  // For a robust system, we should probably reload the state from DB.
                end;
              end;
              FDBManager.Unlock();
            end
            else if FDBManager = nil then
              Result := True // No DB, so memory removal is success
            else
              Result := False; // Failed to get DB lock
          except
            on E: Exception do
              LBLogger.Write(1, 'TTokenManager.RemoveUnneededTokens', lmt_Error, E.Message);
          end;
          FCS.Release();
        end;

      end
      else
        Result := True;

    finally
      if _localList <> nil then
        _localList.Free;
    end;

  end
  else
    LBLogger.Write(5, 'TTokenManager.RemoveUnneededTokens', lmt_Debug, 'Token cleaner disabled', []);

end;

function TTokenManager.RemoveToken(aToken: String): Boolean;
var
  _TokenList : TStringList = nil;

begin
  Result := False;

  if FRemoveTokens then
  begin
    if aToken <> '' then
    begin
      try

        _TokenList := TStringList.Create;
        _TokenList.Add(aToken);

        Result := Self.RemoveUnneededTokens(_TokenList);

      except
        on E: Exception do
          LBLogger.Write(1, 'TTokenManager.RemoveToken', lmt_Error, PChar(E.Message));
      end;
    end;

    if _TokenList <> nil then
      _TokenList.Free;

  end
  else
    LBLogger.Write(5, 'TTokenManager.RemoveToken', lmt_Debug, 'Token cleaner disabled', []);
end;

procedure TTokenManager.StopUpdater();
begin
  if FUpdater <> nil then
    FreeAndNil(FUpdater);
end;

function TTokenManager.CreateTokenTable(Sender: TObject; anUTCCreationTime: TDateTime): Boolean;
var
  _Statements : TStringList = nil;
  _SQLiteDB : TSQLiteDBManager;
begin
  Result := False;

  if Sender is TSQLiteDBManager then
  begin
    try
      _SQLiteDB := TSQLiteDBManager(Sender);

      _Statements := TStringList.Create;
      _Statements.Add('CREATE TABLE Tokens (' +
                        'Token           TEXT PRIMARY KEY,' +
                        'IdUser          INTEGER NOT NULL,' +
                        'LastRequestTime INTEGER NOT NULL,' +
                        'Config          TEXT);');

      if _SQLiteDB.Lock('TTokenManager.CreateTokenTable') then
      begin

        try

          if _SQLiteDB.executeSQL(_Statements) then
          begin
            _SQLiteDB.Commit();
            Result := True;
          end;

        except
          on E: Exception do
            LBLogger.Write(1, 'TTokenManager.CreateTokenTable', lmt_Error, '1. %s', [E.Message]);
        end;

        _SQLiteDB.Unlock();

      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.CreateTokenTable', lmt_Error, '1. %s', [E.Message]);
    end;

    if _Statements <> nil then
      _Statements.Free;
  end;
end;

function TTokenManager.LoadActiveTokens(): Boolean;
var
  _TokenData : TJSONObject;
  _Parser : TJSONParser = nil;

begin
  Result := FTokensLoaded;

  if (not Result) and (FDBManager <> nil) then
  begin
    Self.Clear();

    if FDBManager.Lock('TTokenManager.LoadActiveTokens') then
    begin
      try
        FDBManager.prepareSQLStatement('SELECT Token, IdUser, LastRequestTime, Config FROM Tokens;');
        FDBManager.executeQuery();
        if not FDBManager.isEmpty then
        begin
          if FCS.Acquire('TTokenManager.LoadActiveTokens') then
          begin
            try

              while not FDBManager.EOF do
              begin
                _TokenData := TJSONObject.Create();
                try
                  _TokenData.Add(cToken_Field_IdUser, FDBManager.getColumnAsInteger(1));
                  _TokenData.Add(cToken_Field_LastUpdate, FDBManager.getColumnAsInt64(2));
                  _Parser := TJSONParser.Create(FDBManager.getColumnAsText(3), [joUTF8]);
                  _TokenData.Add(cToken_Field_UserConfig, _Parser.Parse);
                  FreeAndNil(_Parser);
                  FTokens.AddObject(FDBManager.getColumnAsText(0), _TokenData);
                  _TokenData := nil; // Ownership transferred to FTokens
                except
                  on E: Exception do
                  begin
                    LBLogger.Write(1, 'TTokenManager.LoadActiveTokens', lmt_Error, 'Error parsing token config: ' + E.Message);
                    FreeAndNil(_TokenData); // Clean up on error
                  end;
                end;

                FDBManager.nextRow();
              end;

              Result := True;

            except
              on E: Exception do
                LBLogger.Write(1, 'TTokenManager.LoadActiveTokens', lmt_Error, '1. %s', [E.Message]);
            end;

            FCS.Release();

            if _TokenData <> nil then
              FreeAndNil(_TokenData);
          end;
        end;
        FDBManager.closeQuery();

      except
        on E: Exception do
          LBLogger.Write(1, 'TTokenManager.LoadActiveTokens', lmt_Error, '2. %s', [E.Message]);
      end;
      FDBManager.Unlock();
    end;

    FTokensLoaded := Result;
  end;

end;

function TTokenManager.isUnNeededToken(aToken: TJSONObject): Boolean;
var
  _Time : Int64;

begin
  Result := False;

  if aToken <> nil then
  begin
    _Time := Trunc(NowUTC() * SecsPerDay);
    Result := (_Time - aToken.Elements[cToken_Field_LastUpdate].AsInt64) > FInactiveTime;
  end;
end;


end.

