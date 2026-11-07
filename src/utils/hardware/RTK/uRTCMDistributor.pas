unit uRTCMDistributor;

{
  Distribuisce correzioni RTCM3 ai client WebSocket di un TLBmicroWebServer.
  NON modifica LBmicroWebServer: usa solo eventi gia' esistenti.

  ── Uso con configurazione da file INI (consigliato) ─────────────────────
  1. FDistributor := TRTCMDistributor.Create;
     if not FDistributor.LoadConfigFromINIFile(aIniFile) then
       FreeAndNil(FDistributor);            // sezione [RTCM] assente/disattivata
  2. Prima di attivare il web server (es. in startingWebServer):
       FWebServer.OnWebSocketConnectionEstablished := @MioGestore;
       FDistributor.StartSource;            // crea e apre TRTCMClient
     MioGestore chiama FDistributor.TryRegister(THTTPRequestManager(Sender)).
  3. In chiusura: scollegare l'evento dal web server, poi liberare il
     distributore (il distruttore ferma la sorgente).

  Il distributore possiede la sorgente (TRTCMClient) e collega da solo
  OnCorrections a CorrectionsReceived: l'applicazione non deve conoscere
  ne' la sezione [RTCM] ne' i parametri della base.

  ── Sezione [RTCM] del file INI ──────────────────────────────────────────
    Enabled=1                  ; 0/assente = funzione disattivata
    URI=/getRTCM               ; percorso WebSocket offerto ai client
    Passphrase=...             ; chiave dei client (vuota = canale aperto)
    Source=IPDirect            ; NTRIP | IPDirect | WebSocket
    Host=192.168.x.x           ; sorgente
    Port=2101                  ; porta della sorgente
    Mountpoint=VEN3            ; solo NTRIP (obbligatorio)
    Username= / Password=      ; solo NTRIP
    NTRIPVersion=Auto          ; solo NTRIP: Auto | V1 | V2
    ReconnectDelayMs=5000
    ReceiveMode=Framed         ; solo IPDirect/NTRIP v1: Framed | Timed
    BlockGapMs=10              ; solo ReceiveMode=Timed: silenzio che chiude un blocco
    WebSocketURI=/getRTCM      ; solo Source=WebSocket (obbligatorio)
    SourcePassphrase=...       ; solo Source=WebSocket
    GGAIntervalSec=0           ; solo NTRIP/VRS: 0 = non inviare il GGA
    GGALat= / GGALon= / GGAAlt=

  ── Esempi di configurazione [RTCM] ──────────────────────────────────────
  A) Base RTK raggiunta direttamente via IP (stream RTCM3 grezzo su TCP):
       [RTCM]
       Enabled=1
       URI=/getRTCM
       Passphrase=chiave-condivisa-con-i-client
       Source=IPDirect
       Host=192.168.1.50
       Port=5000
       ReconnectDelayMs=5000

  B) Caster NTRIP (mountpoint con credenziali, versione rilevata da sola):
       [RTCM]
       Enabled=1
       URI=/getRTCM
       Passphrase=chiave-condivisa-con-i-client
       Source=NTRIP
       Host=caster.esempio.it
       Port=2101
       Mountpoint=VEN3
       Username=utente
       Password=password
       NTRIPVersion=Auto

  C) Caster NTRIP VRS (richiede la posizione del rover, inviata ogni 10 s;
     decimali con punto o virgola):
       [RTCM]
       Enabled=1
       Passphrase=chiave-condivisa-con-i-client
       Source=NTRIP
       Host=caster.esempio.it
       Port=2101
       Mountpoint=VRS_RTCM3
       Username=utente
       Password=password
       GGAIntervalSec=10
       GGALat=45.4642
       GGALon=9.1900
       GGAAlt=120

  D) Relay: ricevere da un altro server IdroWorld (che offre /getRTCM) e
     ridistribuire. Passphrase = chiave per i client di QUESTO server;
     SourcePassphrase = chiave richiesta dal server a monte:
       [RTCM]
       Enabled=1
       URI=/getRTCM
       Passphrase=chiave-per-i-client-locali
       Source=WebSocket
       Host=idroworld-centrale.esempio.it
       Port=8080
       WebSocketURI=/getRTCM
       SourcePassphrase=chiave-del-server-a-monte

  E) Funzione disattivata (equivale ad assente la sezione):
       [RTCM]
       Enabled=0

  Notare: Passphrase vuota = canale aperto a chiunque (warning nel log);
  Mountpoint e' obbligatorio con NTRIP, WebSocketURI con WebSocket; con
  IPDirect Mountpoint/Username/Password/NTRIPVersion/GGA* sono ignorati.

  ── Uso manuale (senza INI) ──────────────────────────────────────────────
  1. FDistributor := TRTCMDistributor.Create('/getRTCM');
     FDistributor.Passphrase := '...';      // chiave condivisa (vuota = aperto)
  2. Nel gestore di TLBmicroWebServer.OnWebSocketConnectionEstablished:
       if FDistributor.TryRegister(THTTPRequestManager(Sender)) then Exit;
       ... gestione degli altri WebSocket ...
  3. Nel gestore OnCorrections della sorgente (TRTCMClient: base RTK / NTRIP):
       FDistributor.CorrectionsReceived(Sender, Data);
     (stessa firma di TOnCorrectionsReceived)

  ── Comportamento ────────────────────────────────────────────────────────
  - TryRegister controlla l'URI della richiesta (senza query, case-insensitive).
    Se coincide, verifica la firma della richiesta (header X-LB-*, lo stesso
    metodo di uLBSignedProxy, chiave = Passphrase). Se la firma e' valida il
    THTTPRequestManager entra nella lista dei destinatari e si registra
    OnExecuteTerminated per essere rimosso quando il suo thread termina.
    Se la firma non e' valida: non viene registrato e il suo thread viene
    terminato (Terminate). Altre URI: restituisce False e non tocca nulla.
  - Protezione dal replay: timestamp entro MaxClockSkewSeconds (default 300 s)
    e nonce mai visto prima (elenco in memoria, ripulito automaticamente).
  - CorrectionsReceived invia il blocco, come frame WebSocket binario, a ogni
    destinatario in lista.
  - La lista e' protetta da TTimedOutCriticalSection. Il lock resta preso
    durante l'invio: un destinatario non puo' essere rimosso mentre gli si
    sta scrivendo.

  Nota: OnExecuteTerminated e' chiamato da THTTPRequestManager DOPO aver
  liberato il suo WebSocketManager (che a quel punto e' nil). Tra la
  liberazione e la rimozione dalla lista resta una finestra di pochi
  microsecondi in cui un invio potrebbe trovare la sessione gia' liberata;
  la si chiude del tutto solo con un evento in LBmicroWebServer prima della
  liberazione (non fatto, su indicazione).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, uLBmicroWebServer, uTimedoutCriticalSection,
  uLBSignedRequestVerifier, RTCMClient;

type

  { TRTCMDistributor }

  TRTCMDistributor = class(TObject)
    strict private
      FURI : String;
      FSharedKey : String;
      FVerifier : TLBSignedRequestVerifier;

      FRecipients : TList;
      FCS : TTimedOutCriticalSection;

      FNonces : TStringList;
      FNonceTimes : array of Int64;
      FCSNonces : TTimedOutCriticalSection;

      // Configurazione della sorgente (da LoadConfigFromINIFile) e sorgente.
      FEnabled : Boolean;
      FSourceType : TRTCMSourceType;
      FSourceHost : String;
      FSourcePort : Integer;
      FMountpoint : String;
      FUsername : String;
      FPassword : String;
      FNTRIPVersion : TNTRIPVersion;
      FReconnectDelayMs : Integer;
      FReceiveMode : TRTCMReceiveMode;
      FBlockGapMs : Integer;
      FWebSocketURI : String;
      FSourcePassphrase : String;
      FGGAIntervalSec : Integer;
      FGGALat : Double;
      FGGALon : Double;
      FGGAAlt : Double;
      FSource : TRTCMClient;

      function NormalizeURI(const aURI: String): String;
      function IsRTCMRequest(aManager: THTTPRequestManager): Boolean;
      function IsAuthorized(aManager: THTTPRequestManager): Boolean;
      function AcceptNonce(const aNonce: String; aTimestamp: Int64): Boolean;
      procedure RecipientTerminated(Sender: TObject);

      function get_MaxClockSkewSeconds: Integer;
      procedure set_MaxClockSkewSeconds(AValue: Integer);

    public
      constructor Create(const aURI: String = '/getRTCM'); reintroduce;
      destructor Destroy; override;

      function TryRegister(aManager: THTTPRequestManager): Boolean;
      procedure CorrectionsReceived(Sender: TObject; const Data: TBytes);
      function ClientCount(): Integer;

      { Legge la sezione [RTCM] (vedi nota in testa alla unit): URI e
        Passphrase del distributore piu' i parametri della sorgente.
        Restituisce True solo se la funzione e' abilitata e la configurazione
        e' valida. Non solleva eccezioni: in caso di errore scrive nel log e
        restituisce False (il chiamante puo' liberare il distributore). }
      function LoadConfigFromINIFile(aFile: TIniFile): Boolean;
      { Crea la sorgente secondo la configurazione, collega OnCorrections a
        CorrectionsReceived e la apre. Richiede LoadConfigFromINIFile = True. }
      function StartSource(): Boolean;
      { Ferma e libera la sorgente (il suo thread non chiamera' piu'
        CorrectionsReceived). Richiamato anche dal distruttore. }
      procedure StopSource();

      property Enabled : Boolean read FEnabled;
      property URI : String read FURI;
      { Chiave condivisa (la "passphrase"). Impostarla prima di attivare il web
        server. Vuota = nessuna autenticazione. }
      property Passphrase : String write FSharedKey;
      property MaxClockSkewSeconds : Integer read get_MaxClockSkewSeconds write set_MaxClockSkewSeconds;
  end;

implementation

uses
  uWebSocketManagement, ULBLogger, uLBRequestSignature;

const
  cRTCMSection = 'RTCM';
  cRTCM_Enabled = 'Enabled';
  cRTCM_URI = 'URI';
  cRTCM_Passphrase = 'Passphrase';
  cRTCM_Source = 'Source';
  cRTCM_Host = 'Host';
  cRTCM_Port = 'Port';
  cRTCM_Mountpoint = 'Mountpoint';
  cRTCM_Username = 'Username';
  cRTCM_Password = 'Password';
  cRTCM_NTRIPVersion = 'NTRIPVersion';
  cRTCM_ReconnectDelayMs = 'ReconnectDelayMs';
  cRTCM_ReceiveMode = 'ReceiveMode';
  cRTCM_BlockGapMs = 'BlockGapMs';
  cRTCM_WebSocketURI = 'WebSocketURI';
  cRTCM_SourcePassphrase = 'SourcePassphrase';
  cRTCM_GGAIntervalSec = 'GGAIntervalSec';
  cRTCM_GGALat = 'GGALat';
  cRTCM_GGALon = 'GGALon';
  cRTCM_GGAAlt = 'GGAAlt';
  cRTCM_DefaultURI = '/getRTCM';
  cRTCM_DefaultPort = Integer(2101);
  cRTCM_DefaultReconnectDelayMs = Integer(5000);
  cRTCM_DefaultBlockGapMs = Integer(10);

function ParseDecimal(const aText: String; aDefault: Double): Double;
{ Accetta sia il punto sia la virgola decimale, indipendentemente dal locale. }
var
  _FS : TFormatSettings;
  _Text : String;
begin
  _FS := DefaultFormatSettings;
  _FS.DecimalSeparator := '.';
  _Text := StringReplace(Trim(aText), ',', '.', [rfReplaceAll]);
  if (_Text = '') or (not TryStrToFloat(_Text, Result, _FS)) then
    Result := aDefault;
end;

{ TRTCMDistributor }

constructor TRTCMDistributor.Create(const aURI: String);
begin
  inherited Create;

  FURI := Self.NormalizeURI(aURI);

  FSharedKey := '';

  FEnabled := False;
  FSourceType := stNTRIP;
  FSourceHost := '';
  FSourcePort := cRTCM_DefaultPort;
  FMountpoint := '';
  FUsername := '';
  FPassword := '';
  FNTRIPVersion := nvAuto;
  FReconnectDelayMs := cRTCM_DefaultReconnectDelayMs;
  FWebSocketURI := '';
  FSourcePassphrase := '';
  FGGAIntervalSec := 0;
  FGGALat := 0;
  FGGALon := 0;
  FGGAAlt := 0;
  FReceiveMode := rmFramed;
  FBlockGapMs := cRTCM_DefaultBlockGapMs;
  FSource := nil;

  FVerifier := TLBSignedRequestVerifier.Create;
  FVerifier.OnAcceptNonce := @Self.AcceptNonce;

  FRecipients := TList.Create;
  FCS := TTimedOutCriticalSection.Create;

  FNonces := TStringList.Create;
  FNonces.CaseSensitive := True;
  SetLength(FNonceTimes, 0);
  FCSNonces := TTimedOutCriticalSection.Create;
end;

destructor TRTCMDistributor.Destroy;
var
  i : Integer;

begin
  // La sorgente per prima: il suo thread chiama CorrectionsReceived.
  Self.StopSource();
  if FCS.Acquire('TRTCMDistributor.Destroy') then
  begin
    try
      for i := 0 to FRecipients.Count - 1 do
        THTTPRequestManager(FRecipients.Items[i]).OnExecuteTerminated := nil;
      FRecipients.Clear;
    finally
      FCS.Release();
    end;
  end;

  FreeAndNil(FRecipients);
  FreeAndNil(FCS);
  FreeAndNil(FNonces);
  FreeAndNil(FCSNonces);
  FreeAndNil(FVerifier);
  inherited Destroy;
end;

function TRTCMDistributor.NormalizeURI(const aURI: String): String;
begin
  Result := Trim(aURI);
  if (Result = '') or (Result[1] <> '/') then
    Result := '/' + Result;
end;

function TRTCMDistributor.LoadConfigFromINIFile(aFile: TIniFile): Boolean;
const
  cMe = 'TRTCMDistributor.LoadConfigFromINIFile';
var
  _Source : String;
  _Version : String;
  _Mode : String;
begin
  Result := False;
  FEnabled := False;
  try
    if (aFile = nil) or (not aFile.SectionExists(cRTCMSection)) then
    begin
      LBLogger.Write(3, cMe, lmt_Debug, 'RTCM distribution not configured');
      Exit;
    end;
    if not aFile.ReadBool(cRTCMSection, cRTCM_Enabled, False) then
    begin
      LBLogger.Write(3, cMe, lmt_Debug, 'RTCM distribution disabled');
      Exit;
    end;

    FURI := Self.NormalizeURI(aFile.ReadString(cRTCMSection, cRTCM_URI, cRTCM_DefaultURI));
    FSharedKey := aFile.ReadString(cRTCMSection, cRTCM_Passphrase, '');

    _Source := LowerCase(Trim(aFile.ReadString(cRTCMSection, cRTCM_Source, 'NTRIP')));
    if _Source = 'ntrip' then
      FSourceType := stNTRIP
    else if _Source = 'ipdirect' then
      FSourceType := stIPDirect
    else if _Source = 'websocket' then
      FSourceType := stWebSocket
    else
    begin
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s=<%s> non valido (NTRIP, IPDirect o WebSocket): distribuzione RTCM disattivata', [cRTCMSection, cRTCM_Source, _Source]);
      Exit;
    end;

    FSourceHost := Trim(aFile.ReadString(cRTCMSection, cRTCM_Host, ''));
    FSourcePort := aFile.ReadInteger(cRTCMSection, cRTCM_Port, cRTCM_DefaultPort);
    FMountpoint := Trim(aFile.ReadString(cRTCMSection, cRTCM_Mountpoint, ''));
    while (FMountpoint <> '') and (FMountpoint[1] = '/') do
      Delete(FMountpoint, 1, 1);          // TRTCMClient antepone gia' lo '/'
    FUsername := aFile.ReadString(cRTCMSection, cRTCM_Username, '');
    FPassword := aFile.ReadString(cRTCMSection, cRTCM_Password, '');
    FReconnectDelayMs := aFile.ReadInteger(cRTCMSection, cRTCM_ReconnectDelayMs, cRTCM_DefaultReconnectDelayMs);
    if FReconnectDelayMs <= 0 then
      FReconnectDelayMs := cRTCM_DefaultReconnectDelayMs;
    FWebSocketURI := Trim(aFile.ReadString(cRTCMSection, cRTCM_WebSocketURI, ''));
    FSourcePassphrase := aFile.ReadString(cRTCMSection, cRTCM_SourcePassphrase, '');

    _Version := LowerCase(Trim(aFile.ReadString(cRTCMSection, cRTCM_NTRIPVersion, 'Auto')));
    if _Version = 'v1' then
      FNTRIPVersion := nvV1
    else if _Version = 'v2' then
      FNTRIPVersion := nvV2
    else
      FNTRIPVersion := nvAuto;

    // Solo per IPDirect / NTRIP v1 (stream grezzo).
    _Mode := LowerCase(Trim(aFile.ReadString(cRTCMSection, cRTCM_ReceiveMode, 'Framed')));
    if _Mode = 'timed' then
      FReceiveMode := rmTimed
    else
    begin
      FReceiveMode := rmFramed;
      if (_Mode <> 'framed') and (_Mode <> '') then
        LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s=<%s> non valido (Framed o Timed): uso Framed', [cRTCMSection, cRTCM_ReceiveMode, _Mode]);
    end;
    FBlockGapMs := aFile.ReadInteger(cRTCMSection, cRTCM_BlockGapMs, cRTCM_DefaultBlockGapMs);
    if FBlockGapMs <= 0 then
      FBlockGapMs := cRTCM_DefaultBlockGapMs;

    FGGAIntervalSec := aFile.ReadInteger(cRTCMSection, cRTCM_GGAIntervalSec, 0);
    FGGALat := ParseDecimal(aFile.ReadString(cRTCMSection, cRTCM_GGALat, ''), 0);
    FGGALon := ParseDecimal(aFile.ReadString(cRTCMSection, cRTCM_GGALon, ''), 0);
    FGGAAlt := ParseDecimal(aFile.ReadString(cRTCMSection, cRTCM_GGAAlt, ''), 0);
    if (FGGAIntervalSec > 0) and (FGGALat = 0) and (FGGALon = 0) then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s impostato ma %s/%s assenti: invio GGA disattivato', [cRTCMSection, cRTCM_GGAIntervalSec, cRTCM_GGALat, cRTCM_GGALon]);
      FGGAIntervalSec := 0;
    end;

    if (FSourceHost = '') or (FSourcePort <= 0) then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s/%s mancanti o non validi: distribuzione RTCM disattivata', [cRTCMSection, cRTCM_Host, cRTCM_Port]);
      Exit;
    end;
    if (FSourceType = stNTRIP) and (FMountpoint = '') then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s obbligatorio con Source=NTRIP: distribuzione RTCM disattivata', [cRTCMSection, cRTCM_Mountpoint]);
      Exit;
    end;
    if (FSourceType = stWebSocket) and (FWebSocketURI = '') then
    begin
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s obbligatorio con Source=WebSocket: distribuzione RTCM disattivata', [cRTCMSection, cRTCM_WebSocketURI]);
      Exit;
    end;
    if FSharedKey = '' then
      LBLogger.Write(1, cMe, lmt_Warning, '[%s] %s vuota: il canale %s e'' aperto a chiunque', [cRTCMSection, cRTCM_Passphrase, FURI]);

    FEnabled := True;
    Result := True;
    LBLogger.Write(3, cMe, lmt_Info, 'RTCM distribution configured (uri=%s, source=%s, host=%s:%d)', [FURI, _Source, FSourceHost, FSourcePort]);
  except
    on E: Exception do
    begin
      FEnabled := False;
      Result := False;
      LBLogger.Write(1, cMe, lmt_Error, E.Message);
    end;
  end;
end;

function TRTCMDistributor.StartSource(): Boolean;
const
  cMe = 'TRTCMDistributor.StartSource';
begin
  Result := False;
  if not FEnabled then
  begin
    LBLogger.Write(1, cMe, lmt_Warning, 'RTCM distribution not configured: source not started');
    Exit;
  end;
  try
    Self.StopSource();   // per sicurezza, se richiamato due volte

    FSource := TRTCMClient.Create(FSourceType, FSourceHost, FSourcePort);
    FSource.Mountpoint := FMountpoint;
    FSource.Username := FUsername;
    FSource.Password := FPassword;
    FSource.NTRIPVersion := FNTRIPVersion;
    FSource.ReconnectDelay := FReconnectDelayMs;
    FSource.ReceiveMode := FReceiveMode;
    FSource.BlockGapMs := FBlockGapMs;
    FSource.WebSocketURI := FWebSocketURI;
    FSource.Passphrase := FSourcePassphrase;
    if (FSourceType = stNTRIP) and (FGGAIntervalSec > 0) then
    begin
      FSource.GGASentence := BuildGGASentence(FGGALat, FGGALon, FGGAAlt);
      FSource.GGAIntervalSec := FGGAIntervalSec;
    end;
    // Chiamato direttamente dal thread della sorgente.
    FSource.OnCorrections := @Self.CorrectionsReceived;

    Result := FSource.Open();
    if Result then
      LBLogger.Write(3, cMe, lmt_Info, 'RTCM source opened (%s:%d), serving <%s>', [FSourceHost, FSourcePort, FURI])
    else
      LBLogger.Write(1, cMe, lmt_Warning, 'RTCM source not opened (%s:%d)', [FSourceHost, FSourcePort]);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, cMe, lmt_Error, E.Message);
      Self.StopSource();
      Result := False;
    end;
  end;
end;

procedure TRTCMDistributor.StopSource();
begin
  if FSource <> nil then
  begin
    try
      // TRTCMClient.Destroy chiama Close, che ferma e attende il thread.
      FSource.OnCorrections := nil;
      FreeAndNil(FSource);
    except
      on E: Exception do
        LBLogger.Write(1, 'TRTCMDistributor.StopSource', lmt_Error, E.Message);
    end;
  end;
end;

function TRTCMDistributor.get_MaxClockSkewSeconds: Integer;
begin
  Result := FVerifier.MaxClockSkewSeconds;
end;

procedure TRTCMDistributor.set_MaxClockSkewSeconds(AValue: Integer);
begin
  FVerifier.MaxClockSkewSeconds := AValue;
end;

function TRTCMDistributor.AcceptNonce(const aNonce: String; aTimestamp: Int64): Boolean;
{ Chiamato dal verificatore, nel thread della connessione, solo dopo che la
  firma e' risultata valida. False = nonce gia' visto (replay). }
var
  _Limit : Int64;
  i : Integer;

begin
  Result := False;

  if FCSNonces.Acquire('TRTCMDistributor.AcceptNonce') then
  begin
    try
      // Un nonce va ricordato finche' la sua richiesta potrebbe ancora essere
      // accettata (timestamp +/- skew): oltre il doppio dello skew si scarta.
      _Limit := LBUtcUnixTime - (2 * Int64(FVerifier.MaxClockSkewSeconds));
      for i := FNonces.Count - 1 downto 0 do
      begin
        if FNonceTimes[i] < _Limit then
        begin
          FNonces.Delete(i);
          if i < High(FNonceTimes) then
            Move(FNonceTimes[i + 1], FNonceTimes[i], (High(FNonceTimes) - i) * SizeOf(Int64));
          SetLength(FNonceTimes, Length(FNonceTimes) - 1);
        end;
      end;

      if FNonces.IndexOf(aNonce) < 0 then
      begin
        FNonces.Add(aNonce);
        SetLength(FNonceTimes, FNonces.Count);
        FNonceTimes[FNonces.Count - 1] := aTimestamp;
        Result := True;
      end;
    finally
      FCSNonces.Release();
    end;
  end;
end;

function TRTCMDistributor.IsRTCMRequest(aManager: THTTPRequestManager): Boolean;
var
  _Res : String;
  p : Integer;

begin
  Result := False;
  if (aManager <> nil) and (aManager.Parser <> nil) then
  begin
    _Res := aManager.Parser.URI;
    p := Pos('?', _Res);
    if p > 0 then
      _Res := Copy(_Res, 1, p - 1);

    if (Length(_Res) > 1) and (_Res[Length(_Res)] = '/') then
      SetLength(_Res, Length(_Res) - 1);

    Result := SameText(_Res, FURI);
  end;
end;

function TRTCMDistributor.IsAuthorized(aManager: THTTPRequestManager): Boolean;
var
  _Res : TLBVerificationResult;

begin
  Result := FSharedKey = '';   // nessuna chiave configurata: aperto

  if not Result then
  begin
    _Res := FVerifier.Verify(aManager.Parser.Headers, aManager.Parser.Method, aManager.Parser.URI, '', FSharedKey);
    Result := _Res.Valid;

    if not Result then
      LBLogger.Write(1, 'TRTCMDistributor.IsAuthorized', lmt_Warning, 'RTCM client rejected: %s', [_Res.ErrorMessage]);
  end;
end;

function TRTCMDistributor.TryRegister(aManager: THTTPRequestManager): Boolean;
begin
  Result := False;

  if Self.IsRTCMRequest(aManager) then
  begin
    // True anche se la richiesta viene rifiutata: era per /getRTCM e non deve
    // finire tra gli altri listener.
    Result := True;

    if not Self.IsAuthorized(aManager) then
    begin
      // L'handshake e' gia' avvenuto: non si registra e si chiude la sessione.
      aManager.Terminate;
      Exit;
    end;

    if FCS.Acquire('TRTCMDistributor.TryRegister') then
    begin
      try
        aManager.OnExecuteTerminated := @Self.RecipientTerminated;
        FRecipients.Add(aManager);
        LBLogger.Write(5, 'TRTCMDistributor.TryRegister', lmt_Info, 'RTCM client added (%d active)', [FRecipients.Count]);
      finally
        FCS.Release();
      end;
    end
    else
      LBLogger.Write(1, 'TRTCMDistributor.TryRegister', lmt_Warning, 'Unable to register RTCM client!');
  end;
end;

procedure TRTCMDistributor.RecipientTerminated(Sender: TObject);
begin
  if FCS.Acquire('TRTCMDistributor.RecipientTerminated') then
  begin
    try
      FRecipients.Remove(Sender);
      LBLogger.Write(5, 'TRTCMDistributor.RecipientTerminated', lmt_Info, 'RTCM client removed (%d active)', [FRecipients.Count]);
    finally
      FCS.Release();
    end;
  end
  else
    LBLogger.Write(1, 'TRTCMDistributor.RecipientTerminated', lmt_Warning, 'Unable to unregister RTCM client!');
end;

procedure TRTCMDistributor.CorrectionsReceived(Sender: TObject; const Data: TBytes);
var
  i : Integer;
  _WS : TLBWebSocketSession;

begin
  if Length(Data) = 0 then
    Exit;

  if FCS.Acquire('TRTCMDistributor.CorrectionsReceived') then
  begin
    try
      for i := 0 to FRecipients.Count - 1 do
      begin
        _WS := THTTPRequestManager(FRecipients.Items[i]).WebSocketManager;
        if _WS <> nil then
          _WS.addMessageToSend(@Data[0], Length(Data), wsd_FrameBinary, True);
      end;
    finally
      FCS.Release();
    end;
  end;
end;

function TRTCMDistributor.ClientCount(): Integer;
begin
  Result := 0;
  if FCS.Acquire('TRTCMDistributor.ClientCount') then
  begin
    try
      Result := FRecipients.Count;
    finally
      FCS.Release();
    end;
  end;
end;

end.
