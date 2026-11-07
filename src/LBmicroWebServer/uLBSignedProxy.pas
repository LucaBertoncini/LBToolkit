unit uLBSignedProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpjson, jsonparser, jsonscanner,
  blcksock, synsock, httpsend, ssl_openssl3, uLBRequestSignature;

type
  TLBRemoteTarget = class
  public
    Name      : String;
    BaseURL   : String;
    SharedKey : String;
    TimeoutMS : Integer;
  end;

  TLBRemoteTargets = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(aIndex: Integer): TLBRemoteTarget;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // Registra (o aggiorna) un bersaglio: Name/BaseURL/TimeoutMS vengono
    // tipicamente letti dal blocco <RemoteTargets> del file XML delle
    // rotte durante TRouteRegistry.LoadFromXMLFile. aSharedKey e'
    // OPZIONALE qui (default ''): la chiave non deve mai provenire
    // dall'XML - si veda la nota "LA CHIAVE SEGRETA NON COMPARE MAI
    // NELL'XML" in uWebRouteRegistry.pas - e va impostata in un secondo
    // momento con SetSharedKey, da un file di configurazione
    // dell'applicazione escluso dal controllo di versione.
    function Add(const aName, aBaseURL: String; aTimeoutMS: Integer = 15000;
      const aSharedKey: String = ''): TLBRemoteTarget;
    function Find(const aName: String): TLBRemoteTarget;
    // Imposta (o sovrascrive) la sola chiave condivisa di un bersaglio
    // gia' dichiarato (per nome). Restituisce False se il bersaglio non
    // esiste ancora: la chiave presuppone che l'XML sia gia' stato letto.
    function SetSharedKey(const aName, aSharedKey: String): Boolean;
    // Un bersaglio e' utilizzabile solo se possiede gia' indirizzo E
    // chiave: utile per diagnosticare in avvio un bersaglio dichiarato
    // nell'XML ma privo della propria chiave in configurazione.
    function IsReady(const aName: String): Boolean;
    // Carica le SOLE chiavi condivise da un file di configurazione JSON
    // minimale, nella forma {"shared_keys": {"<nome_target>": "<chiave>"}}.
    // Non porta ne' indirizzi ne' timeout: quelli vengono sempre e solo
    // dall'XML delle rotte. Restituisce quante chiavi sono state
    // associate con successo a un bersaglio gia' noto.
    function LoadSharedKeysFromJSONFile(const aFileName: String): Integer;
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TLBRemoteTarget read GetItem; default;
  end;

  TLBSignedResponse = record
    Success      : Boolean;
    StatusCode   : Integer;
    Body         : String;
    ErrorMessage : String;
  end;

  TLBSignedHttpClient = class(TObject)
  private
    FTargets: TLBRemoteTargets; // riferimento in prestito
    FVerifyCertificate: Boolean;
    FCACertificateFile: String;
    function JoinURL(const aBaseURL, aURI: String): String;
  public
    constructor Create(aTargets: TLBRemoteTargets);
    function SendSigned(const aTargetName, aMethod, aURI, aBody: String; const aContext: TLBSignatureContext): TLBSignedResponse;
    // Verifica della catena del certificato per i target https:// (default True).
    property VerifyCertificate: Boolean read FVerifyCertificate write FVerifyCertificate;
    // Bundle PEM delle CA attendibili usato per quella verifica. Con un
    // certificato autofirmato o di una CA privata va indicato qui.
    property CACertificateFile: String read FCACertificateFile write FCACertificateFile;
  end;

implementation

uses
  ULBLogger;

constructor TLBRemoteTargets.Create;
begin
  inherited Create;

  FItems := TList.Create;
end;

destructor TLBRemoteTargets.Destroy;
begin
  Self.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLBRemoteTargets.Clear;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    TObject(FItems[I]).Free;
  FItems.Clear;
end;

function TLBRemoteTargets.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TLBRemoteTargets.GetItem(aIndex: Integer): TLBRemoteTarget;
begin
  Result := TLBRemoteTarget(FItems[aIndex]);
end;

function TLBRemoteTargets.Find(const aName: String): TLBRemoteTarget;
var
  I: Integer;
begin
  Result:=nil;

  for I:=0 to FItems.Count-1 do
  begin
    if SameText(TLBRemoteTarget(FItems[I]).Name, Trim(aName)) then
      Exit(TLBRemoteTarget(FItems[I]));
  end;
end;

function TLBRemoteTargets.Add(const aName, aBaseURL: String; aTimeoutMS: Integer; const aSharedKey: String): TLBRemoteTarget;
begin
  Result:=Self.Find(aName);

  if Result = nil then
  begin
    Result := TLBRemoteTarget.Create;
    FItems.Add(Result);
  end;

  Result.Name := Trim(aName);
  Result.BaseURL := Trim(aBaseURL);

  // La chiave non viene mai azzerata da una ri-registrazione proveniente
  // dall'XML (es. un ricaricamento a caldo delle rotte): se aSharedKey e'
  // vuota e il bersaglio aveva gia' una chiave impostata via SetSharedKey,
  // quella chiave resta intatta.
  if aSharedKey <> '' then
    Result.SharedKey := aSharedKey;

  if aTimeoutMS>0 then
    Result.TimeoutMS := aTimeoutMS
  else
    Result.TimeoutMS := 15000;
end;

function TLBRemoteTargets.SetSharedKey(const aName, aSharedKey: String): Boolean;
var
  T: TLBRemoteTarget;
begin
  T := Find(aName);
  Result := T <> nil;
  if Result then
    T.SharedKey := aSharedKey;
end;

function TLBRemoteTargets.IsReady(const aName: String): Boolean;
var
  T: TLBRemoteTarget;
begin
  T := Self.Find(aName);
  Result := (T <> nil) and (T.BaseURL <> '') and (T.SharedKey <> '');
end;

function TLBRemoteTargets.LoadSharedKeysFromJSONFile(const aFileName: String): Integer;
var
  _FS: TFileStream = nil;
  P: TJSONParser = nil;
  D: TJSONData = nil;
  O, Keys: TJSONObject;
  I: Integer;
begin
  Result := 0;
  if not FileExists(aFileName) then Exit;

  _FS := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try

    P := TJSONParser.Create(_FS, [joUTF8]);
    D := P.Parse;

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TLBRemoteTargets.LoadSharedKeysFromJSONFile', lmt_Error, '1. %s', [E.Message]);
      if D <> nil then
        FreeAndNil(D);
    end;
  end;

  if P <> nil then
    FreeAndNil(P);

  FreeAndNil(_FS);

  try
    if (D is TJSONObject) then
    begin
      O := TJSONObject(D);
      if (O.Find('shared_keys') is TJSONObject) then
      begin
        Keys := TJSONObject(O.Find('shared_keys'));
        for I := 0 to Keys.Count - 1 do
        begin
          if Self.SetSharedKey(Keys.Names[I], Keys.Items[I].AsString) then
            Inc(Result);
        end;
      end;
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TLBRemoteTargets.LoadSharedKeysFromJSONFile', lmt_Error, '2. %s', [E.Message]);
  end;

  if D <> nil then
    D.Free;
end;

constructor TLBSignedHttpClient.Create(aTargets: TLBRemoteTargets);
begin
  inherited Create;
  FTargets := aTargets;
  FVerifyCertificate := True;
  FCACertificateFile := '';
end;

function TLBSignedHttpClient.JoinURL(const aBaseURL, aURI: String): String;
begin
  Result := Trim(aBaseURL);

  while (Result<>'') and (Result[Length(Result)]='/') do
    Delete(Result,Length(Result),1);

  Result += LBNormalizeURI(aURI);
end;

function TLBSignedHttpClient.SendSigned(const aTargetName, aMethod, aURI, aBody: String; const aContext: TLBSignatureContext): TLBSignedResponse;
{
  Implementazione basata su Ararat Synapse (unit httpsend/blcksock), per
  uniformita' con il resto dello stack TLB che gia' usa blcksock a livello
  di socket (si veda uLBmicroWebServer.pas). ssl_openssl3 e' incluso in
  interface per registrare il gestore SSL/TLS di Synapse: e' sufficiente
  che l'unit compaia nel progetto perche' THTTPSend sappia gestire
  bersagli "https://" tramite OpenSSL; nessuna chiamata esplicita e'
  richiesta qui.
}
var
  _Target    : TLBRemoteTarget;
  _HTTP      : THTTPSend = nil;
  _URL       : String;
  _BodyBytes : RawByteString;

begin
  FillChar(Result, SizeOf(Result), 0);
  Result.StatusCode := 0;

  if FTargets = nil then
  begin
    LBLogger.Write(1, 'TLBRemoteTargets.SendSigned', lmt_Warning, 'Targets list not set!');
    Exit;
  end;

  try
    _Target := FTargets.Find(aTargetName);

    if _Target = nil then
    begin
      LBLogger.Write(1, 'TLBRemoteTargets.SendSigned', lmt_Warning, 'Target <%s> not found!', [aTargetName]);
      Exit;
    end;

    if (_Target.BaseURL = '') or (_Target.SharedKey = '') then
    begin
      LBLogger.Write(1, 'TLBRemoteTargets.SendSigned', lmt_Warning, 'Incomplete remote target <%s>!', [aTargetName]);
      Exit;
    end;

    _URL := JoinURL(_Target.BaseURL, aURI);

    _HTTP := THTTPSend.Create;
    _HTTP.Timeout := _Target.TimeoutMS;

    // Synapse espone i timeout di connessione/IO separatamente sul socket
    // sottostante (Sock), oltre al Timeout complessivo gia' impostato sopra.
    _HTTP.Sock.ConnectionTimeout := _Target.TimeoutMS;

    if SameText(Copy(_URL, 1, 8), 'https://') then
    begin
      _HTTP.Sock.SSL.VerifyCert := FVerifyCertificate;
      if FCACertificateFile <> '' then
        _HTTP.Sock.SSL.CertCAFile := FCACertificateFile;
    end;

    _HTTP.MimeType := 'application/json; charset=utf-8';
    LBBuildSignatureHeaders(_HTTP.Headers, aMethod, aURI, aBody, _Target.SharedKey, aContext);

    _BodyBytes := RawByteString(aBody);
    if Length(_BodyBytes) > 0 then
      _HTTP.Document.WriteBuffer(_BodyBytes[1], Length(_BodyBytes));

    if _HTTP.HTTPMethod(UpperCase(Trim(aMethod)), _URL) then
    begin
      Result.StatusCode := _HTTP.ResultCode;
      SetLength(Result.Body, _HTTP.Document.Size);
      if _HTTP.Document.Size > 0 then
      begin
        _HTTP.Document.Position := 0;
        _HTTP.Document.ReadBuffer(Result.Body[1], _HTTP.Document.Size);
      end;

      Result.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 500);

      if not Result.Success then
        Result.ErrorMessage := _HTTP.ResultString;
    end
    else begin
      Result.Success := False;
      Result.StatusCode := _HTTP.ResultCode;
      Result.ErrorMessage := Trim(_HTTP.Sock.LastErrorDesc);
      if Result.ErrorMessage = '' then
        Result.ErrorMessage := 'Connessione al server remoto fallita';
    end;
  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TLBRemoteTargets.SendSigned', lmt_Error, 'Target: <%s>  -  %s', [aTargetName, E.Message]);
      Result.Success := False;
      Result.ErrorMessage := E.Message;
    end;
  end;

  if _HTTP <> nil then
    _HTTP.Free;
end;

end.
