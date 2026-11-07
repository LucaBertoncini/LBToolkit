unit uTokenManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, contnrs, SQLiteWrapper, uTimedoutCriticalSection,
  uLBBaseThread, fpjson, IniFiles, uLBAuthContext;

type

  TDBTokenUpdater = class;

  { TTokenManager }

  TTokenManager = class(TObject)
    strict private
      FCS        : TTimedOutCriticalSection;
      FTokens    : TStringList;
      FRetiredTokens : TObjectList;
      FDBManager : TSQLiteDBManager;
      FUpdater   : TDBTokenUpdater;

      FTokensLoaded : Boolean;

      FRemoveTokens : Boolean;
      FInactiveTime : Int64;
      FCookieSecure : Boolean;



      procedure StopUpdater();
      function CreateTokenTable(Sender: TObject; anUTCCreationTime: TDateTime): Boolean;

      function LoadActiveTokens(): Boolean;

      const
        cTimelessTokenNodeName      = DOMString('TimelessToken');
        cTokenManagerSessionName    = String('TokenManager');
        cTokenExpirationTime        = String('TokenExpirationTime');
        cDBFile                     = String('DBFile');
        cDefaultTokenExpirationTime = Int64(60 * 60); // 1 h

    private
      function isUnNeededToken(aToken: TJSONObject): Boolean;
      function TokenMeta(aToken: TJSONObject): TJSONObject;
      function TokenScope(aToken: TJSONObject): String;
      procedure EnsureTokensLoaded();
      function getScopedToken(const aToken, aScope: String): TJSONObject;
      function TokenLeaseCount(aToken: TJSONObject): Integer;
      procedure RetireTokenAt(aIndex: Integer);
      procedure ReleaseTokenLease(aToken: TJSONObject);
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

      function isValidRequest(const aToken: String; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean; overload;
      function isValidRequest(aRequest: TJSONObject; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean; overload;
      function isValidRequest(requestHeaders, responseHeaders: TStringList; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean; overload;

      function insertTokenAsCookie(const aToken: String; responseHeaders: TStringList): Boolean;

      {:****** for the AI connector (TLBAIConnector), same shape as TAIAuthenticateEvent (uLBAIConnectorTypes):
          _AIConnector.OnAuthenticateRequest := @FTokenManager.isValidBearerRequest;
        Reads "Authorization: ******" and accepts ONLY tokens of scope cTokenScope_AIAccess:
        a web session cookie is never valid here, and an AI token is never valid as a cookie.
        No user configuration is copied: the returned authentication context pins the token object
        until the connector frees the context at the end of the request. No cookie is written and the
        token value is never logged.}
      function isValidBearerRequest(requestHeaders, responseHeaders: TStringList;
        out anAuthentication: TLBAuthenticationContext): Boolean;

      {:Same as isValidBearerRequest for a token of ANY scope: used by an application that issues its own tokens
        through TLBOAuthProvider with AccessScope set (OAuth without the AI connector). The scope must not be empty
        and must not be cTokenScope_Web (that one is the cookie session: use isValidRequest).}
      function isValidBearerRequestForScope(requestHeaders, responseHeaders: TStringList; const aScope: String;
        out anAuthentication: TLBAuthenticationContext): Boolean;

      {:Adds a token with a scope and its own lifetime (instead of the sliding web-session timeout).
        aLifetimeSecs > 0: absolute expiry, not extended by use. 0: no expiry (revoke with RemoveToken).
        Scope, expiry and client id are kept beside UsrConf in memory; when persisted they are written
        under the reserved key "_tm" in the existing Config column, so the SQLite table is unchanged.
        OWNERSHIP: ConfigData is ALWAYS consumed (it becomes part of aTokenData, or is freed on failure):
        the caller must not free it, whatever the result.}
      function addNewScopedToken(IdUser: Integer; ConfigData: TJSONObject; const aScope: String;
        aLifetimeSecs: Int64; const aClientId: String; out aTokenData: TJSONObject): Boolean;

      {:Validates a scoped token and CONSUMES it (single use: refresh-token rotation). Ownership of the
        configuration is transferred to the caller; it is not copied. The token is removed from memory
        atomically with the lookup, so two concurrent uses cannot both succeed.}
      function TakeScopedToken(const aToken, aScope: String; out anUserId: Integer;
        out aConfig: TJSONObject; out aClientId: String): Boolean;

      {:When True the session cookie carries the "Secure" flag, so browsers send it only over HTTPS.
        Default False: with plain HTTP a "Secure" cookie would never be sent back. The value is read
        every time a cookie is created, so it can be set (or changed) at any moment.}
      property CookieSecure: Boolean read FCookieSecure write FCookieSecure;

      function CreateToken(): String;

      function getToken(const aToken: String): TJSONObject;
      function addNewToken(IdUser: Integer; ConfigData: TJSONObject; out aTokenData: TJSONObject): Boolean;
      function RemoveToken(aToken: String): Boolean;

      property RemoveTokens: Boolean write FRemoveTokens;

      {:Durata della sessione in secondi (inattivita' massima): ogni richiesta autenticata la rinnova.
        E' anche il valore che insertTokenAsCookie comunica al client (Max-Age e X-Session-Expires-In).}
      property SessionTimeout: Int64 read FInactiveTime;
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
  {:Token scopes. A token created by addNewToken (web login) has no "_tm" key and counts as cTokenScope_Web.}
  cTokenScope_Web         = String('web');
  cTokenScope_AIAccess    = String('ai');
  cTokenScope_AIRefresh   = String('ai_refresh');

  cToken_Field_Updated    = String('updated');
  cToken_Field_Token      = String('token');
  cToken_Field_IdUser     = String('IdUser');
  cToken_Field_LastUpdate = String('LastUpdate');
  cToken_Field_UserConfig = String('UsrConf');

  {:Header di risposta con i secondi di validita' rimanenti della sessione. Lo aggiunge
    insertTokenAsCookie a ogni rinnovo: il cookie e' HttpOnly, quindi il client non puo' leggerne
    Max-Age e usa questo valore per avvisare l'utente prima della scadenza.}
  cSessionExpiresInHeader = String('X-Session-Expires-In');

  {:Reserved metadata key of a scoped token: {"scope":..,"exp":..,"client":..}. It is beside
    UsrConf in memory and embedded in the persisted Config JSON only for database compatibility.}
  cToken_Config_Meta      = String('_tm');


implementation

uses
  strutils, base64, ULBLogger, LazSysUtils, DateUtils, jsonparser, jsonscanner, uHTTPConsts
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;

type
  { Keeps one token object alive while the connector uses its UserConfig. The context does not own
    UserConfig: releasing the last context frees a retired token, or simply drops the lease on an
    active one. }
  TTokenAuthenticationContext = class(TLBAuthenticationContext)
  strict private
    FTokenManager : TTokenManager;
    FTokenData    : TJSONObject;
  public
    constructor Create(aTokenManager: TTokenManager; aTokenData: TJSONObject;
      anUserId: Integer; aUserConfig: TJSONObject);
    destructor Destroy; override;
  end;

const
  cToken_Field_LeaseCount = String('_tm_leases');
  cToken_Field_Retired    = String('_tm_retired');

{ TTokenAuthenticationContext }

constructor TTokenAuthenticationContext.Create(aTokenManager: TTokenManager;
  aTokenData: TJSONObject; anUserId: Integer; aUserConfig: TJSONObject);
begin
  inherited Create(anUserId, aUserConfig, False);
  FTokenManager := aTokenManager;
  FTokenData := aTokenData;
end;

destructor TTokenAuthenticationContext.Destroy;
begin
  if (FTokenManager <> nil) and (FTokenData <> nil) then
    FTokenManager.ReleaseTokenLease(FTokenData);
  FTokenManager := nil;
  FTokenData := nil;
  inherited Destroy;
end;


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
  FCookieSecure := False;

  FCS := TTimedOutCriticalSection.Create;

  FTokens := TStringList.Create;
  FTokens.OwnsObjects := True;
  FTokens.Sorted := True;
  FTokens.CaseSensitive := True;

  // Tokens removed while a request is still using their configuration stay here until the last
  // authentication context is released. They are no longer valid for new requests.
  FRetiredTokens := TObjectList.Create(True);

  FDBManager := nil;
  FUpdater := nil;

  FTokensLoaded := False;

  FRemoveTokens := True;
end;

destructor TTokenManager.Destroy;
begin

  try

    Self.StopUpdater();

    FreeAndNil(FTokens);
    FreeAndNil(FRetiredTokens);
    FreeAndNil(FCS);

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
  hProv := nil;
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
  f: TFileStream = nil;   // nil: it is freed also when Create raises
  _Attempt: Integer;
begin
  Result := False;
  SetLength(aBuffer, 0);
  // A momentary failure to open the device (for instance EAGAIN under a burst of connections) must not
  // become an empty token: retry a few times before giving up.
  for _Attempt := 1 to 5 do
  begin
    try
      f := TFileStream.Create('/dev/urandom', fmOpenRead);
      SetLength(aBuffer, aLength);
      if f.Read(aBuffer[0], aLength) = aLength then
        Result := True
      else
        LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Warning, 'Failed to read from /dev/urandom');
    except
      on E: Exception do
        LBLogger.Write(1, 'GenerateSecureRandomBytes', lmt_Error, 'Error accessing /dev/urandom (attempt %d): %s', [_Attempt, E.Message]);
    end;
    FreeAndNil(f);
    if Result then Break;
    Sleep(5 * _Attempt);
  end;
  if not Result then
    SetLength(aBuffer, 0);
end;
{$ENDIF}


function TTokenManager.CreateToken(): String;
var
  RandomBytes: TBytes;
  _Raw: RawByteString;
begin
  Result := '';
  // Generate 32 secure random bytes (will result in a ~43 char Base64 string)
  if Self.GenerateSecureRandomBytes(32, RandomBytes) then
  begin
    // The bytes go through the "base64" unit as raw bytes. TNetEncoding.Base64.EncodeBytesToString of
    // FPC 3.2.x (vcl-compat) turns every byte >= $80 into '?' before encoding: about half of the token
    // would be the constant "Pz8" and the entropy would drop from 256 to roughly 144 bits.
    SetString(_Raw, PAnsiChar(@RandomBytes[0]), Length(RandomBytes));
    Result := EncodeStringBase64(_Raw);

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
var
  i: Integer;
begin

  if FCS.Acquire('TTokenManager.Clear') then
  begin

    try
      // A token can be revoked while another thread is already processing a request authenticated
      // with it. RetireTokenAt removes it from the lookup immediately and postpones only the Free.
      for i := FTokens.Count - 1 downto 0 do
        Self.RetireTokenAt(i);

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
  Result := True;

  if FDBManager <> nil then
    FreeAndNil(FDBManager);

  if aIniFile <> nil then
  begin
    try

      if aIniFile.SectionExists(cTokenManagerSessionName) then
      begin
        // la durata vale anche senza database dei token (prima si leggeva solo con DBFile)
        FInactiveTime := aIniFile.ReadInteger(cTokenManagerSessionName, cTokenExpirationTime, cDefaultTokenExpirationTime);
        if FInactiveTime <= 0 then
          FInactiveTime := cDefaultTokenExpirationTime;
        _DBFilename := aIniFile.ReadString(cTokenManagerSessionName, cDBFile, '');
        if _DBFilename <> '' then
        begin
          _DBFilename := ExpandFileName(_DBFilename);

          _Path := ExtractFilePath(_DBFilename);
          if not DirectoryExists(_Path) then
            ForceDirectories(_Path);

          FDBManager := TSQLiteDBManager.Create();
          FDBManager.OnNewDB := @Self.CreateTokenTable;
          Result := FDBManager.ConnectDB(_DBFilename, '', NowUTC());

        end;

      end;

    except
      on E: Exception do
      begin
        LBLogger.Write(1, 'TTokenManager.LoadConfigurationFromIniFile', lmt_Error, E.Message);
        Result := False;
      end;
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
      Result := Self.isValidRequest(_sToken.AsString, anUserId, anUserConfig)
    else
      LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Token field not found: <%s>', [aRequest.AsJSON]);
  end;
end;

function TTokenManager.isValidRequest(const aToken: String; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;
var
  _Token : TJSONObject;

begin
  Result := False;
  anUserId := 0;
  anUserConfig := nil;

  if aToken <> '' then
  begin
    _Token := Self.getToken(aToken);
    if _Token <> nil then
    begin
      anUserId := _Token.Elements[cToken_Field_IdUser].AsInteger;
      anUserConfig := TJSONObject(_Token.Elements[cToken_Field_UserConfig]);
      Result := True;
    end
    else
      LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Invalid token <%s>!', [aToken]);
  end
  else
    LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'No token!', []);
end;

function TTokenManager.isValidRequest(requestHeaders, responseHeaders: TStringList; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;
var
  _Cookies : String;
  _Items : TStringArray;
  _Token : String;
  i : Integer;

begin
  Result := False;
  anUserId := 0;
  anUserConfig := nil;

  if requestHeaders <> nil then
  begin
    _Cookies := requestHeaders.Values[HTTP_HEADER_COOKIE];
    if _Cookies <> '' then
    begin
      _Items := _Cookies.Split(';');
      if Length(_Items) > 0 then
      begin
        _Token := '';
        for i := 0 to High(_Items) do
        begin
          _Items[i] := Trim(_Items[i]);
          if _Items[i].StartsWith(cToken_Field_Token + '=') then
          begin
            _Token := RightStr(_Items[i], Length(_Items[i]) - Length(cToken_Field_Token) - 1);  // Tolgo la parte di sinistra token=
            Break;
          end;
        end;

        Result := Self.isValidRequest(_Token, anUserId, anUserConfig);
        if Result then   // Update token timeout
          Self.insertTokenAsCookie(_Token, responseHeaders)
        else
          LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Token <%s> not found!', [_Token]);
      end;
    end
    else
      LBLogger.Write(1, 'TTokenManager.isValidRequest', lmt_Warning, 'Cookies not found!');

  end;
end;

function TTokenManager.insertTokenAsCookie(const aToken: String; responseHeaders: TStringList): Boolean;
const
  cCookieData       = cToken_Field_Token + '=%s; Path=/; HttpOnly; SameSite=Strict; Max-Age=%d;';
  // "Secure": the browser sends the cookie only over HTTPS (see CookieSecure)
  cCookieDataSecure = cToken_Field_Token + '=%s; Path=/; HttpOnly; Secure; SameSite=Strict; Max-Age=%d;';

begin
  Result := True;
  if FCookieSecure then
    responseHeaders.Add(HTTP_HEADER_SET_COOKIE + ': ' + Format(cCookieDataSecure, [aToken, FInactiveTime]))
  else
    responseHeaders.Add(HTTP_HEADER_SET_COOKIE + ': ' + Format(cCookieData, [aToken, FInactiveTime]));
  // il cookie e' HttpOnly: la scadenza (rinnovata a ogni richiesta) si comunica al client anche qui
  responseHeaders.Add(cSessionExpiresInHeader + ': ' + IntToStr(FInactiveTime));
end;

procedure TTokenManager.EnsureTokensLoaded();
begin
  // Thread-safe lazy loading with double-checked locking pattern
  if not FTokensLoaded then
  begin
    if FCS.Acquire('TTokenManager.EnsureTokensLoaded') then
    try
      // Check again inside the lock
      if not FTokensLoaded then
        Self.LoadActiveTokens();
    finally
      FCS.Release();
    end;
  end;
end;

function TTokenManager.TokenMeta(aToken: TJSONObject): TJSONObject;
var
  _Config : TJSONData;
  _Meta   : TJSONData;
begin
  Result := nil;
  if aToken = nil then Exit;

  // Current in-memory format: token metadata lives beside UsrConf, so the configuration seen by
  // permissions and workers is exactly the one produced by the application.
  _Meta := aToken.Find(cToken_Config_Meta);
  if _Meta is TJSONObject then
    Exit(TJSONObject(_Meta));

  // Backward compatibility while loading tokens written by earlier versions: in the SQLite Config
  // column the metadata is stored under _tm together with the user configuration.
  _Config := aToken.Find(cToken_Field_UserConfig);
  if (_Config is TJSONObject) then
  begin
    _Meta := TJSONObject(_Config).Find(cToken_Config_Meta);
    if _Meta is TJSONObject then
      Result := TJSONObject(_Meta);
  end;
end;

function TTokenManager.TokenScope(aToken: TJSONObject): String;
var
  _Meta : TJSONObject;
begin
  _Meta := Self.TokenMeta(aToken);
  if _Meta = nil then
    Result := cTokenScope_Web
  else
    Result := _Meta.Get('scope', '');
end;

function TTokenManager.TokenLeaseCount(aToken: TJSONObject): Integer;
begin
  if aToken = nil then
    Result := 0
  else
    Result := aToken.Get(cToken_Field_LeaseCount, 0);
end;

procedure TTokenManager.RetireTokenAt(aIndex: Integer);
var
  _Token : TJSONObject;
begin
  // Called with FCS held. Removing the item from FTokens invalidates it immediately for new lookups.
  // If an in-flight request has pinned it, ownership moves to FRetiredTokens until the last lease is
  // released; otherwise TStringList.Delete frees it now.
  if (aIndex < 0) or (aIndex >= FTokens.Count) then Exit;

  _Token := TJSONObject(FTokens.Objects[aIndex]);
  if Self.TokenLeaseCount(_Token) > 0 then
  begin
    if _Token.Find(cToken_Field_Retired) = nil then
      _Token.Add(cToken_Field_Retired, True)
    else
      _Token.Elements[cToken_Field_Retired].AsBoolean := True;

    FTokens.Objects[aIndex] := nil;
    FTokens.Delete(aIndex);
    FRetiredTokens.Add(_Token);
  end
  else
    FTokens.Delete(aIndex);
end;

procedure TTokenManager.ReleaseTokenLease(aToken: TJSONObject);
var
  _Count : Integer;
  _Idx   : Integer;
begin
  if aToken = nil then Exit;

  if FCS.Acquire('TTokenManager.ReleaseTokenLease') then
  begin
    try
      _Count := Self.TokenLeaseCount(aToken);
      if _Count <= 1 then
      begin
        _Idx := aToken.IndexOfName(cToken_Field_LeaseCount);
        if _Idx >= 0 then
          aToken.Delete(_Idx);

        if aToken.Get(cToken_Field_Retired, False) then
        begin
          _Idx := FRetiredTokens.IndexOf(aToken);
          if _Idx >= 0 then
            FRetiredTokens.Delete(_Idx);   // last user: now the retired token can be freed
        end;
      end
      else
        aToken.Elements[cToken_Field_LeaseCount].AsInteger := _Count - 1;
    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.ReleaseTokenLease', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

function TTokenManager.getToken(const aToken: String): TJSONObject;
begin
  // The classic web-session lookup: only tokens WITHOUT a scope (or scope "web") are valid here.
  Result := Self.getScopedToken(aToken, cTokenScope_Web);
end;

function TTokenManager.getScopedToken(const aToken, aScope: String): TJSONObject;
var
  _Idx : Integer;
begin
  Result := nil;
  if Length(aToken) = 0 then Exit;

  Self.EnsureTokensLoaded();

  if FCS.Acquire('TTokenManager.getToken') then
  begin
    try
      if FTokens.Find(aToken, _Idx) and Self.isUnNeededToken(TJSONObject(FTokens.Objects[_Idx])) then
      begin
        // SCADUTO per inattivita': non vale piu', anche se il thread di pulizia (ogni 5 minuti) non l'ha
        // ancora rimosso. Senza questo controllo un token scaduto restava utilizzabile fino alla pulizia.
        LBLogger.Write(3, 'TTokenManager.getToken', lmt_Warning, 'Expired token rejected');
      end
      else if FTokens.Find(aToken, _Idx) then
      begin
        if Self.TokenScope(TJSONObject(FTokens.Objects[_Idx])) <> aScope then
          LBLogger.Write(3, 'TTokenManager.getToken', lmt_Warning, 'Token rejected: wrong scope (expected <%s>)', [aScope])
        else begin
          Result := TJSONObject(FTokens.Objects[_Idx]);
          Result.Elements[cToken_Field_LastUpdate].AsInt64 := Trunc(NowUTC() * SecsPerDay);
          if Result.Find(cToken_Field_Updated) = nil then
            Result.Add(cToken_Field_Updated, False);
          Result.Elements[cToken_Field_Updated].AsBoolean := True;
        end;
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TTokenManager.getToken', lmt_Error, E.Message);
    end;
    FCS.Release();
  end;
end;

function TTokenManager.isValidBearerRequest(requestHeaders, responseHeaders: TStringList;
  out anAuthentication: TLBAuthenticationContext): Boolean;
begin
  Result := Self.isValidBearerRequestForScope(requestHeaders, responseHeaders, cTokenScope_AIAccess, anAuthentication);
end;

function TTokenManager.isValidBearerRequestForScope(requestHeaders, responseHeaders: TStringList; const aScope: String;
  out anAuthentication: TLBAuthenticationContext): Boolean;
const
  cBearerPrefix = String('bearer ');
var
  _Auth   : String;
  _Token  : TJSONObject;
  _Config : TJSONData;
  _Count  : Integer;
begin
  Result := False;
  anAuthentication := nil;

  if requestHeaders = nil then Exit;
  // a bearer token never belongs to the cookie session: refuse an empty or "web" scope instead of quietly
  // accepting a web session token presented as a bearer
  if (aScope = '') or SameStr(aScope, cTokenScope_Web) then Exit;

  _Auth := Trim(requestHeaders.Values[HTTP_HEADER_AUTHORIZATION]);
  if (Length(_Auth) <= Length(cBearerPrefix)) or (LowerCase(Copy(_Auth, 1, Length(cBearerPrefix))) <> cBearerPrefix) then Exit;

  _Auth := Trim(Copy(_Auth, Length(cBearerPrefix) + 1, MaxInt));
  if _Auth = '' then Exit;

  Self.EnsureTokensLoaded();

  if FCS.Acquire('TTokenManager.isValidBearerRequest') then
  begin
    try
      // getScopedToken acquires FCS again. This is intentional: Free Pascal critical sections are
      // recursive for the owning thread, and keeping the lookup in one place avoids two subtly
      // different validation paths.
      _Token := Self.getScopedToken(_Auth, aScope);
      if _Token <> nil then
      begin
        _Config := _Token.Find(cToken_Field_UserConfig);
        if _Config is TJSONObject then
        begin
          anAuthentication := TTokenAuthenticationContext.Create(Self, _Token,
            _Token.Elements[cToken_Field_IdUser].AsInteger, TJSONObject(_Config));

          _Count := Self.TokenLeaseCount(_Token);
          if _Token.Find(cToken_Field_LeaseCount) = nil then
            _Token.Add(cToken_Field_LeaseCount, _Count + 1)
          else
            _Token.Elements[cToken_Field_LeaseCount].AsInteger := _Count + 1;
          Result := True;
        end;
      end;
    except
      on E: Exception do
      begin
        LBLogger.Write(1, 'TTokenManager.isValidBearerRequest', lmt_Error, E.Message);
        FreeAndNil(anAuthentication);
        Result := False;
      end;
    end;
    FCS.Release();
  end;
end;

function TTokenManager.addNewScopedToken(IdUser: Integer; ConfigData: TJSONObject; const aScope: String;
  aLifetimeSecs: Int64; const aClientId: String; out aTokenData: TJSONObject): Boolean;
var
  _Meta : TJSONObject;
  _Idx  : Integer;
begin
  Result := False;
  aTokenData := nil;

  if ConfigData = nil then Exit;

  if (IdUser <= 0) or (aScope = '') then
  begin
    ConfigData.Free;   // consumed, as documented
    Exit;
  end;

  _Idx := ConfigData.IndexOfName(cToken_Config_Meta);
  if _Idx >= 0 then
    ConfigData.Delete(_Idx);

  _Meta := TJSONObject.Create;
  _Meta.Add('scope', aScope);
  if aLifetimeSecs > 0 then
    _Meta.Add('exp', Int64(Trunc(NowUTC() * SecsPerDay)) + aLifetimeSecs);
  if aClientId <> '' then
    _Meta.Add('client', aClientId);
  ConfigData.Add(cToken_Config_Meta, _Meta);

  Result := Self.addNewToken(IdUser, ConfigData, aTokenData);
  if Result and (aTokenData <> nil) and FCS.Acquire('TTokenManager.addNewScopedToken') then
  begin
    try
      // addNewToken has already persisted the complete Config JSON. In memory keep bookkeeping out
      // of UsrConf, so the application receives its original object without a copy or extra fields.
      _Idx := ConfigData.IndexOfName(cToken_Config_Meta);
      if _Idx >= 0 then
        aTokenData.Add(cToken_Config_Meta, ConfigData.Extract(_Idx));
    finally
      FCS.Release();
    end;
  end;
end;

function TTokenManager.TakeScopedToken(const aToken, aScope: String; out anUserId: Integer;
  out aConfig: TJSONObject; out aClientId: String): Boolean;
var
  _Idx    : Integer;
  _Token  : TJSONObject;
  _Meta   : TJSONObject;
  _Cfg    : TJSONData;
  _CfgIdx : Integer;
  _List   : TStringList;
  _Consumed : Boolean;
begin
  Result := False;
  _Consumed := False;
  anUserId := 0;
  aConfig := nil;
  aClientId := '';

  if aToken = '' then Exit;
  Self.EnsureTokensLoaded();

  if FCS.Acquire('TTokenManager.TakeScopedToken') then
  begin
    try
      if FTokens.Find(aToken, _Idx) then
      begin
        _Token := TJSONObject(FTokens.Objects[_Idx]);
        _Cfg := _Token.Find(cToken_Field_UserConfig);
        if (not Self.isUnNeededToken(_Token)) and (Self.TokenScope(_Token) = aScope) and
           (_Cfg is TJSONObject) and (Self.TokenLeaseCount(_Token) = 0) then
        begin
          anUserId := _Token.Elements[cToken_Field_IdUser].AsInteger;
          _Meta := Self.TokenMeta(_Token);
          if _Meta <> nil then
            aClientId := _Meta.Get('client', '');

          // This token is single-use: transfer its configuration instead of cloning it. Extract
          // detaches the child from the token object, then RetireTokenAt can free the remaining token.
          _CfgIdx := _Token.IndexOfName(cToken_Field_UserConfig);
          aConfig := TJSONObject(_Token.Extract(_CfgIdx));
          Self.RetireTokenAt(_Idx);
          _Consumed := True;
          _CfgIdx := aConfig.IndexOfName(cToken_Config_Meta);
          if _CfgIdx >= 0 then
            aConfig.Delete(_CfgIdx);
          Result := True;
        end;
      end;
    except
      on E: Exception do
      begin
        LBLogger.Write(1, 'TTokenManager.TakeScopedToken', lmt_Error, E.Message);
        FreeAndNil(aConfig);
      end;
    end;
    FCS.Release();
  end;

  if _Consumed then
  begin
    // The token is already gone from memory: remove it from the database too.
    _List := TStringList.Create;
    try
      _List.Add(aToken);
      Self.RemoveUnneededTokens(_List);
    finally
      _List.Free;
    end;
  end;
end;

function TTokenManager.addNewToken(IdUser: Integer; ConfigData: TJSONObject; out aTokenData: TJSONObject): Boolean;
var
  _Index: Integer;
  _NewToken: String;
begin
  Result := False;
  aTokenData := nil;
  _Index := -1;

  // I token salvati si caricano PRIMA di aggiungerne uno nuovo: il caricamento svuota l'elenco in memoria e lo
  // riempie dal database, quindi fatto dopo scarterebbe il token appena creato.
  Self.EnsureTokensLoaded();

  if (IdUser > 0) and (ConfigData <> nil) then
  begin

    aTokenData := TJSONObject.Create();
    _NewToken := Self.CreateToken();
    if _NewToken = '' then
    begin
      // No secure random source: never create a token with an empty (guessable) name.
      LBLogger.Write(1, 'TTokenManager.addNewToken', lmt_Error, 'Token not created: secure random generator failed');
      FreeAndNil(aTokenData);
      // As in every other failure path, ConfigData is consumed (here it was not yet owned by aTokenData)
      ConfigData.Free;
      Exit;
    end;
    aTokenData.Add(cToken_Field_Token, _NewToken);
    aTokenData.Add(cToken_Field_IdUser, IdUser);
    aTokenData.Add(cToken_Field_LastUpdate, Int64(Trunc(NowUTC() * SecsPerDay)));
    aTokenData.Add(cToken_Field_UserConfig, ConfigData);

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
  end;

  // If we failed to acquire the lock in the first place
  if (not Result) and (aTokenData <> nil) then
    FreeAndNil(aTokenData);

end;

function TTokenManager.UpdateTokens(aTokenList: TObjectList): Boolean;
var
  i: Integer;
  _Token, _Config: TJSONObject;
  _Meta: TJSONData;
  _ConfigJSON: String;
begin
  Result := False;

  if (FDBManager <> nil) and (aTokenList <> nil) and (aTokenList.Count > 0) then
  begin
    if FDBManager.Lock('TTokenManager.UpdateTokens') then
    begin
      try
        FDBManager.prepareSQLStatement('UPDATE Tokens SET LastRequestTime = :LRT, Config = :Cnf WHERE (Token = :TK);');

        for i := 0 to aTokenList.Count - 1 do
        begin
          _Token := TJSONObject(aTokenList.Items[i]);
          _Config := TJSONObject(_Token.Elements[cToken_Field_UserConfig]);

          // SQLite keeps token metadata in the Config column for backward compatibility. Build that
          // representation as text: do not clone and, importantly, do not modify the UserConfig that
          // request threads may be reading through an authentication context.
          _ConfigJSON := _Config.AsJSON;
          _Meta := _Token.Find(cToken_Config_Meta);
          if _Meta <> nil then
          begin
            if _Config.Count = 0 then
              _ConfigJSON := '{"' + cToken_Config_Meta + '":' + _Meta.AsJSON + '}'
            else begin
              Delete(_ConfigJSON, Length(_ConfigJSON), 1);   // final "}"
              _ConfigJSON := _ConfigJSON + ',"' + cToken_Config_Meta + '":' + _Meta.AsJSON + '}';
            end;
          end;

          FDBManager.setParam(1, _Token.Elements[cToken_Field_LastUpdate].AsInt64);
          FDBManager.setParam(2, _ConfigJSON, False);
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
                Self.RetireTokenAt(FTokens.IndexOf(TokensToRemove[i]));
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
  _ConfigData : TJSONData;
  _MetaIdx : Integer;

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
                  // Il campo "token" DEVE stare nell'oggetto, come in quelli creati da addNewToken: UpdateTokens lo
                  // legge per sapere quale riga del database aggiornare. Prima si usava solo come chiave dell'elenco.
                  _TokenData.Add(cToken_Field_Token, FDBManager.getColumnAsText(0));
                  _TokenData.Add(cToken_Field_IdUser, FDBManager.getColumnAsInteger(1));
                  _TokenData.Add(cToken_Field_LastUpdate, FDBManager.getColumnAsInt64(2));
                  _Parser := TJSONParser.Create(FDBManager.getColumnAsText(3), [joUTF8]);
                  try
                    _ConfigData := _Parser.Parse;
                  finally
                    FreeAndNil(_Parser);   // anche se Parse solleva un'eccezione
                  end;
                  // la configurazione utente deve essere un oggetto JSON: gli altri la leggono con un cast diretto
                  if not (_ConfigData is TJSONObject) then
                  begin
                    FreeAndNil(_ConfigData);
                    raise Exception.Create('the stored configuration is not a JSON object');
                  end;
                  _TokenData.Add(cToken_Field_UserConfig, _ConfigData);

                  // Persisted scoped-token metadata used to live inside the configuration. Keep the
                  // database format compatible, but separate it in memory without cloning anything.
                  _MetaIdx := TJSONObject(_ConfigData).IndexOfName(cToken_Config_Meta);
                  if _MetaIdx >= 0 then
                    _TokenData.Add(cToken_Config_Meta, TJSONObject(_ConfigData).Extract(_MetaIdx));
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
        end
        else
          // Database senza token: il caricamento e' comunque riuscito. Prima restava "non caricato" e ogni richiesta
          // lo ripeteva; peggio, alla prima richiesta DOPO un login (che crea il token in memoria e nel database)
          // il caricamento scartava il token appena creato e lo rileggeva dal database.
          Result := True;
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
  _Exp  : Int64;
  _Meta : TJSONObject;

begin
  Result := False;

  if aToken <> nil then
  begin
    _Time := Trunc(NowUTC() * SecsPerDay);

    _Meta := Self.TokenMeta(aToken);
    if _Meta <> nil then
    begin
      // Scoped token: absolute expiry (no sliding), or none at all when "exp" is absent.
      _Exp := _Meta.Get('exp', Int64(0));
      Result := (_Exp > 0) and (_Time > _Exp);
    end
    else
      Result := (_Time - aToken.Elements[cToken_Field_LastUpdate].AsInt64) > FInactiveTime;
  end;
end;


end.

