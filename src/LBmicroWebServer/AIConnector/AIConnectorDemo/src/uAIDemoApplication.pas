unit uAIDemoApplication;
{
  L'applicazione dell'esempio: deriva da TLBApplicationBoostrap, come le applicazioni vere (IdroWorld).

  IL BOOTSTRAP FA GIA' (non si riscrive):
    - legge il file INI: [LBLogger] avvia il registro, [LBmicroWebServer] Port, DocumentsFolder, SecureCookie, [SSLData];
    - crea il web server, lo configura e lo attiva.

  QUESTA CLASSE AGGIUNGE SOLO COSA SERVE ALL'ESEMPIO, in due punti d'aggancio del bootstrap:

    LoadConfigurationFromINIFileInternal   (il web server NON esiste ancora; il registro si')
        legge [TokenManager], [OAuth], [AIConnector] e [AIDemo]. Il file INI non attiva nulla da solo: le chiavi
        configurano oggetti che l'applicazione crea.

    startingWebServer                      (il web server esiste, NON e' ancora attivo)
        1. rotte (Routes.xml) e gestori;
        2. router in catena;
        3. OAuth.Attach   (server di autorizzazione: /.well-known/*, /oauth/*)
        4. AI.Attach      (connettore MCP: /mcp)  <- sempre DOPO OAuth e DOPO ogni RegisterHandler
        5. gestore dei token: avvio del thread che salva e ripulisce

  PROPRIETA' DEGLI OGGETTI
    Router, provider OAuth e connettore, una volta nella catena, sono del WEB SERVER, che li libera.
    Token manager, utenti e gestori restano nostri e vanno liberati DOPO il web server (le richieste in corso li
    usano): per questo il distruttore libera per primo il web server.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uLBApplicationBoostrap, uTokenManager, uWebRouteRegistry, uLBOAuthSetup, uLBAIConnectorSetup,
  uAIDemoUsers, uAIDemoHandlers;

type
  { TAIDemoApplication }
  TAIDemoApplication = class(TLBApplicationBoostrap)
  strict private
    FConfigDir  : String;
    FRoutesFile : String;
    FPort       : Integer;
    FPrepared   : Boolean;
    FTokens     : TTokenManager;
    FUsers      : TDemoUsers;
    FHandlers   : TDemoHandlers;
    FOAuth      : TLBOAuthSetup;
    FAI         : TLBAIConnectorSetup;
    function get_Ready: Boolean;
  strict protected
    function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; override;
    procedure startingWebServer(); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Una riga di riepilogo, per la console.
    function StatusLine(): String;

    // True quando il web server e' stato preparato (rotte lette, OAuth e connettore agganciati se richiesti).
    property Ready: Boolean read get_Ready;
  end;

implementation

uses
  uLBFileUtils, SQLiteWrapper, ULBLogger;

const
  cSection_App      = 'AIDemo';
  cSection_Web      = 'LBmicroWebServer';
  cSection_Tokens   = 'TokenManager';
  cKey_RoutesFile   = 'RoutesFile';
  cKey_Port         = 'Port';
  cKey_DBFile       = 'DBFile';

constructor TAIDemoApplication.Create;
begin
  inherited Create;
  FConfigDir := '';
  FRoutesFile := '';
  FPort := 0;
  FPrepared := False;
  FTokens := TTokenManager.Create;
  FUsers := TDemoUsers.Create;
  FHandlers := TDemoHandlers.Create;
  FOAuth := TLBOAuthSetup.Create;
  FAI := TLBAIConnectorSetup.Create;
end;

destructor TAIDemoApplication.Destroy;
begin
  // Il web server per PRIMO: possiede router, provider OAuth e connettore, e le sue richieste in corso usano ancora
  // token manager, utenti e gestori. Il distruttore della base libera poi il registro.
  FreeAndNil(FWebServer);
  FreeAndNil(FAI);          // solo configurazione: gli oggetti creati sono gia' stati liberati dal web server
  FreeAndNil(FOAuth);
  FreeAndNil(FHandlers);
  FreeAndNil(FUsers);
  FreeAndNil(FTokens);
  inherited Destroy;
end;

function TAIDemoApplication.get_Ready: Boolean;
begin
  Result := FPrepared and (FWebServer <> nil);
end;

function TAIDemoApplication.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
begin
  Result := True;
  anErrorMsg := '';

  // cartella del file INI: ci stanno Routes.xml e il file temp.disabled
  FConfigDir := ExtractFilePath(aFile.FileName);
  FRoutesFile := ResolvePathDeterministic(aFile.ReadString(cSection_App, cKey_RoutesFile, 'Routes.xml'), FConfigDir);
  FUsers.FlagFolder := FConfigDir;
  FPort := aFile.ReadInteger(cSection_Web, cKey_Port, 0);

  // I token si salvano su SQLite solo se [TokenManager] DBFile e' impostato. La libreria SQLite va caricata PRIMA
  // di leggere la configurazione del token manager: senza, ConnectDB rifiuta ("SQLite not initialized").
  if aFile.ReadString(cSection_Tokens, cKey_DBFile, '') <> '' then
    if not initializeSQLiteWrapper(False) then
    begin
      anErrorMsg := 'Cannot load the SQLite library (libsqlite3): needed because [TokenManager] DBFile is set';
      Result := False;
      Exit;
    end;

  if not FTokens.LoadConfigurationFromIniFile(aFile) then
  begin
    anErrorMsg := 'Cannot open the tokens database ([TokenManager] DBFile): see the log';
    Result := False;
    Exit;
  end;

  // Sezioni facoltative: con Enabled assente o 0 non fanno nulla e non scrivono nulla nel registro.
  FOAuth.LoadFromINIFile(aFile);       // [OAuth]
  FAI.LoadFromINIFile(aFile);          // [AIConnector]
end;

procedure TAIDemoApplication.startingWebServer();
var
  _Router: TWebRouteModule;
begin
  inherited startingWebServer();
  FPrepared := False;

  // ---- 1. rotte e gestori -------------------------------------------------------------------------------------
  _Router := TWebRouteModule.Create;
  if not _Router.LoadRoutesFromXML(FRoutesFile) then
  begin
    FreeAndNil(_Router);
    raise Exception.CreateFmt('Cannot read the routes file <%s>', [FRoutesFile]);
  end;
  _Router.RegisterHandler(FHandlers);
  _Router.ValidateRoutes;                                    // dopo OGNI RegisterHandler

  // ---- 2. router in catena ------------------------------------------------------------------------------------
  // Sessione del SITO (cookie): serve solo a chi chiama /api/... da HTTP. L'agente AI non passa da qui.
  _Router.OnAuthenticateRequest := @FTokens.isValidRequest;
  FWebServer.addChainProcessor(_Router, True);               // da qui il router e' del web server

  // ---- 3. OAuth e 4. connettore AI (sempre in quest'ordine, sempre per ULTIMI) -----------------------------------
  FOAuth.Attach(FWebServer, FTokens, @FUsers.Authenticate, @FUsers.RefreshUser);
  FAI.Attach(FWebServer, _Router, FOAuth);

  // ---- 5. thread del token manager (salvataggio su SQLite e pulizia dei token scaduti) ---------------------------
  FTokens.Activate;

  FPrepared := True;
end;

function TAIDemoApplication.StatusLine(): String;
begin
  Result := Format('port=%d oauth=%d ai=%d', [FPort, Ord(FOAuth.Active), Ord(FAI.Active)]);
end;

end.
