unit uLBOAuthSetup;
(*
  Attivazione del server di autorizzazione OAuth (TLBOAuthProvider) di LBmicroWebServer, indipendente da
  qualunque altro servizio: serve a tutto cio' che deve autenticare un client a nome di un utente (oggi il
  connettore per agenti AI, domani altro).

  CHI USA QUESTA UNIT
    L'applicazione, in tre punti (nessuna modifica a TLBApplicationBoostrap):
      costruttore               FOAuth := TLBOAuthSetup.Create;
      lettura del file INI      FOAuth.LoadFromINIFile(aFile);      (LoadConfigurationFromINIFileInternal)
      fine di startingWebServer FOAuth.Attach(FWebServer, FTokenManager, @Auth.AuthenticateCredentials, @Auth.RefreshUser);

  COSA FA
    - LoadFromINIFile  legge la sezione [OAuth]. Non solleva eccezioni: con una configurazione non valida scrive
                       il motivo nel log e il servizio resta spento. Con la sezione assente o Enabled diverso
                       da 1 NON scrive nulla (e' il caso normale di chi non usa OAuth).
    - Attach           controlla TUTTE le precondizioni, le elenca nel log se ne manca qualcuna, altrimenti crea
                       il provider, lo configura e lo aggiunge alla catena del web server. Da quel momento il
                       provider e' del WEB SERVER, che lo libera: qui resta solo un riferimento.

  CIO' CHE L'APPLICAZIONE DEVE FORNIRE (la unit non puo' conoscerlo)
    - il TTokenManager, lo stesso che usa per le sessioni del sito;
    - OnCredentials: verifica username e password, come il login web, e restituisce id e configurazione di
      sessione dell'utente;
    - OnRefreshUser: a ogni rinnovo del token rilegge l'utente (disattivato? ruolo cambiato?). Facoltativo, ma
      senza di esso un utente disattivato o con ruolo cambiato resta valido fino alla scadenza del refresh token
      (30 giorni): Attach lo segnala nel log.

  SEZIONE [OAuth] (tutte le chiavi sono facoltative tranne Enabled e PublicBaseURL)
    Enabled=0                      ; 1 = attiva il server di autorizzazione
    PublicBaseURL=                 ; indirizzo pubblico: SOLO schema, host e porta, https, senza percorso (es. https://cmms.idrotherm2000.com)
    AllowInsecureHTTP=0            ; 1 SOLO per prove: la password dell'utente viaggia nel modulo di login
    AccessTokenLifetimeSec=900
    RefreshTokenLifetimeSec=2592000
    AllowedRedirectURIs=           ; elenco separato da virgola; chiave assente = quello predefinito del provider
    AllowLoopbackRedirects=0       ; 1 = ammette http://localhost e 127.0.0.1
    TrustForwardedFor=0            ; 1 solo dietro un reverse proxy fidato
    MaxLoginFailures=5
    LoginLockSeconds=600

    Diciture della pagina di accesso (facoltative: chiave assente o vuota = dicitura predefinita, neutra):
    ServerTitle=                   ; nome dell'applicazione: titolo della pagina e segnaposto app
    LoginHeading=
    LoginIntro=                    ; puo' contenere host (indirizzo a cui si torna) e app (ServerTitle) fra graffe
    UsernameLabel=
    PasswordLabel=
    SubmitLabel=
    BadCredentials=
    TooManyAttempts=
    RequestExpired=
    InvalidRedirect=
*)
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uLBmicroWebServer, uTokenManager, uLBOAuthProvider;

const
  cOAuth_INI_Section = 'OAuth';

type
  { TLBOAuthSetup }
  TLBOAuthSetup = class(TObject)
  strict private
    FEnabled               : Boolean;
    FPublicBaseURL         : String;
    FAccessTokenLifetime   : Integer;
    FRefreshTokenLifetime  : Integer;
    FHasRedirects          : Boolean;
    FAllowedRedirects      : TStringList;
    FAllowLoopback         : Boolean;
    FTrustForwardedFor     : Boolean;
    FMaxLoginFailures      : Integer;
    FLoginLockSeconds      : Integer;
    FTexts                 : TLBOAuthTexts;
    // creati qui e passati alla catena: li possiede il web server, qui sono solo riferimenti
    FProvider              : TLBOAuthProvider;
    FTokenManager          : TTokenManager;
    function GetActive: Boolean;
    procedure SplitList(const aText: String; aList: TStrings);
    procedure ReadText(aFile: TIniFile; const aKey: String; var aTarget: String);
  public
    constructor Create;
    destructor Destroy; override;

    // Legge [OAuth]. True = richiesto E valido. Non solleva eccezioni.
    function LoadFromINIFile(aFile: TIniFile): Boolean;

    // Crea il provider e lo aggiunge alla catena di aWebServer. Da chiamare dopo la creazione del web server
    // (di norma alla fine di startingWebServer). True = attivo. Con il servizio spento restituisce False senza
    // scrivere nulla.
    function Attach(aWebServer: TLBmicroWebServer; aTokenManager: TTokenManager;
      anOnCredentials: TAICredentialsEvent; anOnRefreshUser: TAIRefreshUserEvent): Boolean;

    property Enabled      : Boolean read FEnabled;
    // True quando il provider e' nella catena del web server.
    property Active       : Boolean read GetActive;
    // Il provider creato (in prestito: lo possiede il web server) e il token manager con cui lavora: chi vi si
    // appoggia (il connettore AI) deve usare gli STESSI.
    property Provider     : TLBOAuthProvider read FProvider;
    property TokenManager : TTokenManager read FTokenManager;
  end;

implementation

uses
  ULBLogger;

const
  cMe                    = 'TLBOAuthSetup';
  cKey_Enabled           = 'Enabled';
  cKey_PublicBaseURL     = 'PublicBaseURL';
  cKey_AllowInsecureHTTP = 'AllowInsecureHTTP';
  cKey_AccessLifetime    = 'AccessTokenLifetimeSec';
  cKey_RefreshLifetime   = 'RefreshTokenLifetimeSec';
  cKey_RedirectURIs      = 'AllowedRedirectURIs';
  cKey_AllowLoopback     = 'AllowLoopbackRedirects';
  cKey_TrustForwardedFor = 'TrustForwardedFor';
  cKey_MaxLoginFailures  = 'MaxLoginFailures';
  cKey_LoginLockSeconds  = 'LoginLockSeconds';

  cDefault_AccessLifetime  = Integer(900);
  cDefault_RefreshLifetime = Integer(30 * 24 * 3600);
  cDefault_MaxFailures     = Integer(5);
  cDefault_LockSeconds     = Integer(600);

constructor TLBOAuthSetup.Create;
begin
  inherited Create;
  FEnabled := False;
  FAllowedRedirects := TStringList.Create;
  FTexts := TLBOAuthTexts.Create;       // i valori predefiniti (neutri) del provider
  FProvider := nil;
  FTokenManager := nil;
end;

destructor TLBOAuthSetup.Destroy;
begin
  // il provider NON si libera qui: lo possiede il web server
  FProvider := nil;
  FTokenManager := nil;
  FreeAndNil(FTexts);
  FreeAndNil(FAllowedRedirects);
  inherited Destroy;
end;

function TLBOAuthSetup.GetActive: Boolean;
begin
  Result := FProvider <> nil;
end;

procedure TLBOAuthSetup.SplitList(const aText: String; aList: TStrings);
var
  _Parts : TStringList;
  _Item  : String;
  i      : Integer;
begin
  aList.Clear;
  _Parts := TStringList.Create;
  try
    _Parts.StrictDelimiter := True;
    _Parts.Delimiter := ',';
    _Parts.DelimitedText := StringReplace(aText, ';', ',', [rfReplaceAll]);
    for i := 0 to _Parts.Count - 1 do
    begin
      _Item := Trim(_Parts[i]);
      if _Item <> '' then aList.Add(_Item);
    end;
  finally
    _Parts.Free;
  end;
end;

// Chiave assente o vuota: la dicitura predefinita resta com'e'.
procedure TLBOAuthSetup.ReadText(aFile: TIniFile; const aKey: String; var aTarget: String);
var
  _Value: String;
begin
  _Value := Trim(aFile.ReadString(cOAuth_INI_Section, aKey, ''));
  if _Value <> '' then
    aTarget := _Value;
end;

function TLBOAuthSetup.LoadFromINIFile(aFile: TIniFile): Boolean;
var
  _AllowInsecure: Boolean;
begin
  Result := False;
  FEnabled := False;
  if aFile = nil then Exit;
  try
    // spento (o sezione assente): silenzio assoluto. E' il caso di ogni applicazione che non usa OAuth, e il
    // logger puo' non essere stato inizializzato.
    if not aFile.ReadBool(cOAuth_INI_Section, cKey_Enabled, False) then Exit;

    FPublicBaseURL := Trim(aFile.ReadString(cOAuth_INI_Section, cKey_PublicBaseURL, ''));
    while (FPublicBaseURL <> '') and (FPublicBaseURL[Length(FPublicBaseURL)] = '/') do
      Delete(FPublicBaseURL, Length(FPublicBaseURL), 1);
    if FPublicBaseURL = '' then
    begin
      LBLogger.Write(1, cMe + '.LoadFromINIFile', lmt_Error, '[%s] %s is required when OAuth is enabled: OAuth NOT activated', [cOAuth_INI_Section, cKey_PublicBaseURL]);
      Exit;
    end;

    // solo schema, host e porta: i percorsi (/mcp, /oauth/..., /.well-known/...) li aggiunge il provider. Un "/mcp"
    // scritto qui produrrebbe indirizzi sbagliati (.../mcp/mcp, .../mcp/oauth/authorize) e il login non partirebbe.
    if (Pos('://', FPublicBaseURL) > 0) and (Pos('/', Copy(FPublicBaseURL, Pos('://', FPublicBaseURL) + 3, MaxInt)) > 0) then
    begin
      LBLogger.Write(1, cMe + '.LoadFromINIFile', lmt_Error,
        '[%s] %s <%s> must not contain a path: write only scheme, host and port: OAuth NOT activated', [cOAuth_INI_Section, cKey_PublicBaseURL, FPublicBaseURL]);
      Exit;
    end;

    // il modulo di login riceve la password dell'utente: senza HTTPS solo se lo si chiede in modo esplicito
    _AllowInsecure := aFile.ReadBool(cOAuth_INI_Section, cKey_AllowInsecureHTTP, False);
    if (not SameText(Copy(FPublicBaseURL, 1, 8), 'https://')) and (not _AllowInsecure) then
    begin
      LBLogger.Write(1, cMe + '.LoadFromINIFile', lmt_Error, '[%s] %s <%s> is not HTTPS: OAuth NOT activated (for tests only: %s=1)', [cOAuth_INI_Section, cKey_PublicBaseURL, FPublicBaseURL, cKey_AllowInsecureHTTP]);
      Exit;
    end;

    FAccessTokenLifetime := aFile.ReadInteger(cOAuth_INI_Section, cKey_AccessLifetime, cDefault_AccessLifetime);
    if FAccessTokenLifetime <= 0 then FAccessTokenLifetime := cDefault_AccessLifetime;
    FRefreshTokenLifetime := aFile.ReadInteger(cOAuth_INI_Section, cKey_RefreshLifetime, cDefault_RefreshLifetime);
    if FRefreshTokenLifetime <= 0 then FRefreshTokenLifetime := cDefault_RefreshLifetime;

    FHasRedirects := aFile.ValueExists(cOAuth_INI_Section, cKey_RedirectURIs);
    if FHasRedirects then
      Self.SplitList(aFile.ReadString(cOAuth_INI_Section, cKey_RedirectURIs, ''), FAllowedRedirects)
    else
      FAllowedRedirects.Clear;
    FAllowLoopback := aFile.ReadBool(cOAuth_INI_Section, cKey_AllowLoopback, False);
    FTrustForwardedFor := aFile.ReadBool(cOAuth_INI_Section, cKey_TrustForwardedFor, False);
    FMaxLoginFailures := aFile.ReadInteger(cOAuth_INI_Section, cKey_MaxLoginFailures, cDefault_MaxFailures);
    if FMaxLoginFailures < 0 then FMaxLoginFailures := cDefault_MaxFailures;
    FLoginLockSeconds := aFile.ReadInteger(cOAuth_INI_Section, cKey_LoginLockSeconds, cDefault_LockSeconds);
    if FLoginLockSeconds <= 0 then FLoginLockSeconds := cDefault_LockSeconds;

    // diciture: ripartono dai valori predefiniti ad ogni lettura
    FreeAndNil(FTexts);
    FTexts := TLBOAuthTexts.Create;
    Self.ReadText(aFile, 'ServerTitle', FTexts.ServerTitle);
    Self.ReadText(aFile, 'LoginHeading', FTexts.LoginHeading);
    Self.ReadText(aFile, 'LoginIntro', FTexts.LoginIntro);
    Self.ReadText(aFile, 'UsernameLabel', FTexts.UsernameLabel);
    Self.ReadText(aFile, 'PasswordLabel', FTexts.PasswordLabel);
    Self.ReadText(aFile, 'SubmitLabel', FTexts.SubmitLabel);
    Self.ReadText(aFile, 'BadCredentials', FTexts.BadCredentials);
    Self.ReadText(aFile, 'TooManyAttempts', FTexts.TooManyAttempts);
    Self.ReadText(aFile, 'RequestExpired', FTexts.RequestExpired);
    Self.ReadText(aFile, 'InvalidRedirect', FTexts.InvalidRedirect);

    FEnabled := True;
    Result := True;
    LBLogger.Write(3, cMe + '.LoadFromINIFile', lmt_Info,
      'OAuth configured (public URL=%s, access token %d s, refresh token %d s)',
      [FPublicBaseURL, FAccessTokenLifetime, FRefreshTokenLifetime]);
  except
    on E: Exception do
    begin
      FEnabled := False;
      Result := False;
      LBLogger.Write(1, cMe + '.LoadFromINIFile', lmt_Error, '%s', [E.Message]);
    end;
  end;
end;

function TLBOAuthSetup.Attach(aWebServer: TLBmicroWebServer; aTokenManager: TTokenManager;
  anOnCredentials: TAICredentialsEvent; anOnRefreshUser: TAIRefreshUserEvent): Boolean;
var
  _Problems : TStringList;
  _Provider : TLBOAuthProvider;
  _Added    : Boolean;
  i         : Integer;
begin
  Result := False;
  FProvider := nil;
  FTokenManager := nil;
  if not FEnabled then Exit;

  // TUTTE le precondizioni, elencate insieme: chi configura vede in un colpo solo cosa manca
  _Problems := TStringList.Create;
  try
    if aWebServer = nil then
      _Problems.Add('the web server does not exist ([LBmicroWebServer] Port missing or 0)');
    if aTokenManager = nil then
      _Problems.Add('the application did not provide the token manager');
    if not Assigned(anOnCredentials) then
      _Problems.Add('the application did not provide the credentials check (OnCredentials)');
    if _Problems.Count > 0 then
    begin
      for i := 0 to _Problems.Count - 1 do
        LBLogger.Write(1, cMe + '.Attach', lmt_Error, 'OAuth NOT activated: %s', [_Problems[i]]);
      Exit;
    end;
  finally
    _Problems.Free;
  end;

  if not Assigned(anOnRefreshUser) then
    LBLogger.Write(1, cMe + '.Attach', lmt_Warning,
      'OnRefreshUser not provided: a deactivated user or a changed role is NOT noticed when a token is renewed (the refresh token stays valid until it expires)');

  _Provider := nil;
  _Added := False;
  try
    _Provider := TLBOAuthProvider.Create(aTokenManager);
    _Provider.PublicBaseURL := FPublicBaseURL;
    _Provider.AccessTokenLifetime := FAccessTokenLifetime;
    _Provider.RefreshTokenLifetime := FRefreshTokenLifetime;
    if FHasRedirects then
    begin
      _Provider.AllowedRedirectURIs.Clear;
      for i := 0 to FAllowedRedirects.Count - 1 do
        _Provider.AllowedRedirectURIs.Add(FAllowedRedirects[i]);
    end;
    _Provider.AllowLoopbackRedirects := FAllowLoopback;
    _Provider.TrustForwardedFor := FTrustForwardedFor;
    _Provider.MaxLoginFailures := FMaxLoginFailures;
    _Provider.LoginLockSeconds := FLoginLockSeconds;
    _Provider.Texts.Assign(FTexts);
    _Provider.OnAuthenticateCredentials := anOnCredentials;
    _Provider.OnRefreshUser := anOnRefreshUser;

    // Controlli di configurazione del provider nel log, subito all'avvio.
    _Provider.ValidateConfiguration();

    // Da qui il provider e' del web server. Il valore restituito da addChainProcessor non si usa: l'esito
    // dell'inserimento e' gia' noto (eccezione o no).
    aWebServer.addChainProcessor(_Provider, True);
    _Added := True;
    FProvider := _Provider;
    FTokenManager := aTokenManager;
    Result := True;
    LBLogger.Write(3, cMe + '.Attach', lmt_Info, 'OAuth active: %s/oauth', [FPublicBaseURL]);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe + '.Attach', lmt_Error, '%s', [E.Message]);
      // se non e' entrato nella catena si libera qui; se e' entrato e' del web server
      if not _Added then
        FreeAndNil(_Provider);
      Result := FProvider <> nil;
    end;
  end;
end;

end.
