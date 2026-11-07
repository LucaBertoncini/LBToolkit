unit uIdroWorld_AuthService;
{
  Verifica delle credenziali di un utente IdroWorld e costruzione della sua "configurazione di sessione".

  UN SOLO PUNTO per due ingressi diversi:
    - il login del sito web  (uIdroWorld_HandlersAuth.HandleLogin);
    - il login OAuth dell'agente AI (TLBOAuthProvider, collegato con TLBOAuthSetup.Attach).
  La query sulla password e la regola "quale ruolo, con quali privilegi" non sono scritte due volte: un utente
  si autentica allo stesso modo in tutti e due i casi e con la stessa configurazione.

  LA CONFIGURAZIONE DI SESSIONE contiene SOLO is_admin e role_id (come prima): ogni verifica di permesso legge
  poi la cache dei permessi del ruolo (TIdroWorldRolePermissionsCache). L'agente AI non ha quindi permessi
  propri: ha quelli dell'utente che l'ha autorizzato.

  RINNOVO (RefreshUser): ad ogni rinnovo del token AI il provider richiama RefreshUser. Un utente disattivato o
  cancellato non rinnova piu'; un cambio di ruolo si vede al rinnovo successivo (al piu' AccessTokenLifetime
  secondi dopo, 15 minuti di default). Il LOGIN WEB invece non rilegge l'utente finche' dura la sessione: la
  differenza e' voluta, i token dell'AI sono brevi.

  PROPRIETA': l'oggetto restituito in aUserConfig e' NUOVO e passa a chi chiama (il token manager lo incorpora).

  I due metodi pubblici hanno la firma di TAICredentialsEvent e TAIRefreshUserEvent (uLBOAuthProvider):
      FOAuth.Attach(FWebServer, FTokenManager, @FAuthService.AuthenticateCredentials, @FAuthService.RefreshUser);

  Il servizio e' dell'applicazione e deve sopravvivere al web server (le richieste in corso lo usano).
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, uIdroWorldDataAccess;

type
  { TIdroWorldAuthService }
  TIdroWorldAuthService = class(TObject)
  strict private
    FDataAccess : IIdroWorldDataAccess;
    // {is_admin, role_id} dell'utente ATTIVO; nil se non esiste o non e' attivo
    function LoadUserConfig(anUserId: Integer): TJSONObject;
  public
    constructor Create(aDataAccess: IIdroWorldDataAccess);

    // Compatibile con TAICredentialsEvent (TLBOAuthProvider.OnAuthenticateCredentials) e usato da HandleLogin
    function AuthenticateCredentials(const aUsername, aPassword: String; out anUserId: Integer;
      out aUserConfig: TJSONObject): Boolean;

    // Compatibile con TAIRefreshUserEvent (TLBOAuthProvider.OnRefreshUser)
    function RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
  end;

implementation

uses
  ULBLogger;

constructor TIdroWorldAuthService.Create(aDataAccess: IIdroWorldDataAccess);
begin
  inherited Create;
  FDataAccess := aDataAccess;
end;

function TIdroWorldAuthService.LoadUserConfig(anUserId: Integer): TJSONObject;
var
  _Row : TJSONObject;
begin
  Result := nil;
  _Row := FDataAccess.SelectRowToJSON(
    'SELECT COALESCE(r.id, 0) AS role_id, COALESCE(r.is_system, FALSE) AS is_system ' +
    'FROM core.app_user u LEFT JOIN core.app_role r ON r.id = u.role_id ' +
    'WHERE u.id = :id AND u.is_active = TRUE',
    ['id', Int64(anUserId)], cIdroWorldMainDB);
  if _Row = nil then Exit;
  try
    Result := TJSONObject.Create;
    Result.Add('is_admin', _Row.Get('is_system', False));
    Result.Add('role_id', _Row.Get('role_id', Int64(0)));
  finally
    _Row.Free;
  end;
end;

function TIdroWorldAuthService.AuthenticateCredentials(const aUsername, aPassword: String;
  out anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
const
  cMe = 'TIdroWorldAuthService.AuthenticateCredentials';
var
  _User : String;
begin
  Result := False;
  anUserId := 0;
  aUserConfig := nil;
  _User := Trim(aUsername);
  if (_User = '') or (aPassword = '') then Exit;
  try
    anUserId := Integer(FDataAccess.ScalarInt(
      'SELECT id FROM core.app_user WHERE is_active = TRUE AND username = :usr AND psw = crypt(:pwd, psw)',
      ['usr', _User, 'pwd', aPassword], 0, cIdroWorldMainDB));
    if anUserId <= 0 then
    begin
      anUserId := 0;
      LBLogger.Write(1, cMe, lmt_Warning, 'Login failed for <%s>', [_User]);
      Exit;
    end;
    aUserConfig := Self.LoadUserConfig(anUserId);
    if aUserConfig = nil then
    begin
      anUserId := 0;
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe, lmt_Error, '%s', [E.Message]);
      anUserId := 0;
      FreeAndNil(aUserConfig);
      Result := False;
    end;
  end;
end;

function TIdroWorldAuthService.RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
const
  cMe = 'TIdroWorldAuthService.RefreshUser';
begin
  aUserConfig := nil;
  try
    aUserConfig := Self.LoadUserConfig(anUserId);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe, lmt_Error, '%s', [E.Message]);
      aUserConfig := nil;
    end;
  end;
  Result := aUserConfig <> nil;
  if not Result then
    LBLogger.Write(3, cMe, lmt_Info, 'Renewal refused for user id=%d (not active or not found)', [anUserId]);
end;

end.
