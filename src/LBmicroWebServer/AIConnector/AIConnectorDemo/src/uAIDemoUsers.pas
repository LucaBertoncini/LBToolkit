unit uAIDemoUsers;
{
  Utenti di prova e controllo delle credenziali, nella forma richiesta da TLBOAuthSetup.Attach:

    Authenticate  = TAICredentialsEvent  (login nella pagina di accesso OAuth)
    RefreshUser   = TAIRefreshUserEvent  (ogni rinnovo del token: l'utente e' ancora valido? con quale ruolo?)

  In un'applicazione vera qui c'e' il database (come TIdroWorldAuthService in IdroWorld). Qui gli utenti sono una
  tabella nel codice, con le password in chiaro: serve solo a provare il connettore.

  LA CONFIGURAZIONE UTENTE che il token si porta dietro contiene il ruolo e l'elenco dei permessi "area.OpType".
  Il modulo di routing predefinito (TWebRouteModule.hasPermission) legge proprio quell'elenco (campo "Permissions"):
  per questo l'esempio non ha bisogno di derivare alcuna classe.

  UTENTE      PASSWORD  RUOLO    PERMESSI
  mario       pw1       lettore  ai.Uso, demo.Lettura
  anna        pw2       tecnico  ai.Uso, demo.Lettura, demo.Modifica
  admin       pw3       admin    ai.Uso, demo.Lettura, demo.Modifica, demo.Amministrazione
  ospite      pw4       ospite   demo.Lettura                (NON ha ai.Uso: il canale AI gli e' chiuso)
  temp        pw5       lettore  come mario; si disattiva creando il file "temp.disabled" nella cartella di
                                 configurazione: serve a provare che il rinnovo del token rilegge l'utente
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  { TDemoUsers }
  TDemoUsers = class(TObject)
  strict private
    FFlagFolder : String;
    function IsDisabled(const aUserName: String): Boolean;
    function BuildConfig(anIndex: Integer): TJSONObject;
  public
    constructor Create;

    // Compatibile con TAICredentialsEvent. Su True aUserConfig e' un oggetto NUOVO: lo prende in carico il chiamante.
    function Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
      out aUserConfig: TJSONObject): Boolean;

    // Compatibile con TAIRefreshUserEvent. Su True aUserConfig e' un oggetto NUOVO costruito con i dati ATTUALI.
    function RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;

    // Cartella in cui si cerca il file "temp.disabled" (con il separatore finale). Vuota = cartella corrente.
    property FlagFolder: String read FFlagFolder write FFlagFolder;
  end;

implementation

uses
  uWebRouteRegistry, ULBLogger;

type
  TDemoUserRow = record
    Id       : Integer;
    Name     : String;
    Password : String;
    Role     : String;
  end;

const
  cDemoUserCount = 5;
  cDemoUsers : array[1..cDemoUserCount] of TDemoUserRow = (
    (Id: 1; Name: 'mario';  Password: 'pw1'; Role: 'lettore'),
    (Id: 2; Name: 'anna';   Password: 'pw2'; Role: 'tecnico'),
    (Id: 3; Name: 'admin';  Password: 'pw3'; Role: 'admin'),
    (Id: 4; Name: 'ospite'; Password: 'pw4'; Role: 'ospite'),
    (Id: 5; Name: 'temp';   Password: 'pw5'; Role: 'lettore')
  );
  cDisabledFlagFile = 'temp.disabled';

constructor TDemoUsers.Create;
begin
  inherited Create;
  FFlagFolder := '';
end;

function TDemoUsers.IsDisabled(const aUserName: String): Boolean;
begin
  // "temp" si disattiva a caldo creando un file: cosi' si prova il rinnovo del token senza riavviare il server
  Result := (aUserName = 'temp') and FileExists(FFlagFolder + cDisabledFlagFile);
end;

function TDemoUsers.BuildConfig(anIndex: Integer): TJSONObject;
var
  _Role  : String;
  _Perms : TJSONArray;
begin
  _Role := cDemoUsers[anIndex].Role;
  _Perms := TJSONArray.Create;
  // il permesso della porta MCP: senza, il connettore risponde 403 a tutto il ruolo
  if (_Role = 'lettore') or (_Role = 'tecnico') or (_Role = 'admin') then
    _Perms.Add('ai.Uso');
  _Perms.Add('demo.Lettura');
  if (_Role = 'tecnico') or (_Role = 'admin') then
    _Perms.Add('demo.Modifica');
  if _Role = 'admin' then
    _Perms.Add('demo.Amministrazione');

  Result := TJSONObject.Create;
  Result.Add('role', _Role);
  Result.Add(cJSONFieldPermissions, _Perms);
end;

function TDemoUsers.Authenticate(const aUsername, aPassword: String; out anUserId: Integer;
  out aUserConfig: TJSONObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  anUserId := 0;
  aUserConfig := nil;
  for i := Low(cDemoUsers) to High(cDemoUsers) do
    if (cDemoUsers[i].Name = aUsername) and (cDemoUsers[i].Password = aPassword) and
       (not Self.IsDisabled(cDemoUsers[i].Name)) then
    begin
      anUserId := cDemoUsers[i].Id;
      aUserConfig := Self.BuildConfig(i);
      Result := True;
      Break;
    end;
  if not Result then
    LBLogger.Write(1, 'TDemoUsers.Authenticate', lmt_Warning, 'Login refused for <%s>', [aUsername]);
end;

function TDemoUsers.RefreshUser(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  aUserConfig := nil;
  for i := Low(cDemoUsers) to High(cDemoUsers) do
    if cDemoUsers[i].Id = anUserId then
    begin
      if not Self.IsDisabled(cDemoUsers[i].Name) then
      begin
        aUserConfig := Self.BuildConfig(i);
        Result := True;
      end;
      Break;
    end;
  if not Result then
    LBLogger.Write(3, 'TDemoUsers.RefreshUser', lmt_Info, 'Renewal refused for user id=%d (disabled or unknown)', [anUserId]);
end;

end.
