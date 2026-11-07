program Ex01_Minimo;
(*
  ESEMPIO 1 - IL COLLEGAMENTO MINIMO, SCRITTO PER ESTESO

  Mostra tutto cio' che un'applicazione deve fare per dare accesso a un agente AI (MCP + OAuth).
  Non usa il bootstrap: ogni passo e' qui, numerato.

    PUNTO 1  Leggere il file INI:        OAuth.LoadFromINIFile, AI.LoadFromINIFile
    PUNTO 2  Rotte e gestori:            LoadRoutesFromXML, RegisterHandler, ValidateRoutes
    PUNTO 3  Identita':                  TTokenManager + controllo delle credenziali + rinnovo
    PUNTO 4  Catena del web server:      router, poi OAuth.Attach, poi AI.Attach
                                         (ULTIMO, dopo OGNI RegisterHandler e dopo ValidateRoutes)

  Strumenti che l'agente vede (nome = area_Funzione):
    demo_Ping     chi sono (id e ruolo dell'utente che ha autorizzato l'agente)
    demo_Saluta   un parametro di testo obbligatorio
    demo_Somma    due parametri interi obbligatori

  Uso (dalla cartella dell'esempio, con Ex01.ini e Routes_Ex01.xml accanto):
      Ex01_Minimo
  Prova automatica:
      python3 ../client/smoke_test.py ex01 http://127.0.0.1:8081
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, IniFiles, fpjson,
  uHTTPConsts, uTokenManager, uWebRouteRegistry, uLBmicroWebServer,
  uLBAIConnectorTypes, uLBOAuthSetup, uLBAIConnectorSetup,
  uEsempiComune;

type
  { Un gestore normale (RegisterStandardWorker) che in piu' DESCRIVE i parametri per l'AI. }
  TEx01Handler = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function DoPing(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoSaluta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoSomma(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
  public
    constructor Create;
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

constructor TEx01Handler.Create;
begin
  inherited Create('demo');   // codice dell'area in Routes_Ex01.xml
  // Registrazione normale: lo stesso metodo serve il sito (POST /api/demo/...) e l'agente AI.
  Self.RegisterStandardWorker('Ping', @Self.DoPing);
  Self.RegisterStandardWorker('Saluta', @Self.DoSaluta);
  Self.RegisterStandardWorker('Somma', @Self.DoSomma);
end;

// Senza questa descrizione una funzione NON diventa uno strumento (a meno di ExposeUndescribed=1 nel file INI).
function TEx01Handler.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'Ping') then
    aDescriptor.Effect := aeReadOnly                       // nessun parametro
  else if SameText(aFunctionName, 'Saluta') then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('nome', aptString, 'Nome della persona da salutare', True);
  end
  else if SameText(aFunctionName, 'Somma') then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('a', aptInteger, 'Primo addendo', True);
    aDescriptor.AddParam('b', aptInteger, 'Secondo addendo', True);
  end
  else
    Result := False;   // funzione non descritta: non e' uno strumento
end;

function TEx01Handler.DoPing(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('pong', True);
  _Obj.Add('user_id', aUserId);                       // l'utente che ha autorizzato l'agente
  _Obj.Add('role', aUserConfig.Get('role', ''));      // la sua configurazione di sessione
  Result := _Obj;
end;

function TEx01Handler.DoSaluta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj  : TJSONObject;
  _Name : String;
begin
  _Name := Trim(aRequestData.Get('nome', ''));
  _Obj := TJSONObject.Create;
  if _Name = '' then
  begin
    // uno stato >= 400 arriva all'agente come errore dello strumento, con questo testo
    aStatusCode := HTTP_STATUS_BAD_REQUEST;
    _Obj.Add('error', 'Il nome non puo'' essere vuoto');
  end
  else begin
    aStatusCode := HTTP_STATUS_OK;
    _Obj.Add('messaggio', 'Ciao ' + _Name);
  end;
  Result := _Obj;
end;

function TEx01Handler.DoSomma(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  // il connettore ha gia' controllato che a e b ci siano e siano interi (e convertito "2" in 2)
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('somma', aRequestData.Get('a', Int64(0)) + aRequestData.Get('b', Int64(0)));
  Result := _Obj;
end;

var
  gIni     : TIniFile;
  gPort    : Integer;
  gServer  : TLBmicroWebServer;
  gRouter  : TDemoRouteModule;
  gHandler : TEx01Handler;
  gTokens  : TTokenManager;
  gUsers   : TDemoUsers;
  gOAuth   : TLBOAuthSetup;
  gAI      : TLBAIConnectorSetup;
begin
  // ---- PUNTO 1: file INI (sezioni [LBmicroWebServer], [TokenManager], [OAuth], [AIConnector]) ----
  gOAuth := TLBOAuthSetup.Create;
  gAI := TLBAIConnectorSetup.Create;
  gTokens := TTokenManager.Create;
  gIni := TIniFile.Create(ExampleFile('Ex01.ini'));
  try
    gPort := gIni.ReadInteger('LBmicroWebServer', 'Port', 8081);
    gTokens.LoadConfigurationFromIniFile(gIni);
    gOAuth.LoadFromINIFile(gIni);   // con Enabled assente o 0 non scrive nulla e non fa nulla
    gAI.LoadFromINIFile(gIni);
  finally
    gIni.Free;
  end;

  gServer := TLBmicroWebServer.Create;
  gServer.ListeningPort := gPort;

  // ---- PUNTO 2: rotte (Routes_Ex01.xml) e gestori ----
  gRouter := TDemoRouteModule.Create;
  if not gRouter.LoadRoutesFromXML(ExampleFile('Routes_Ex01.xml')) then
  begin
    WriteLn('Impossibile leggere Routes_Ex01.xml');
    Halt(1);
  end;
  gHandler := TEx01Handler.Create;
  gRouter.RegisterHandler(gHandler);
  gRouter.ValidateRoutes;

  // ---- PUNTO 3: identita' ----
  // gTokens: lo stesso gestore dei token vale per il sito e per l'AI (con "ambiti" diversi).
  // gUsers : in un'applicazione vera qui c'e' il database (stessa query del login web).
  gUsers := TDemoUsers.Create;
  gRouter.OnAuthenticateRequest := @gTokens.isValidRequest;   // sessione del SITO, non dell'AI

  // ---- PUNTO 4: catena del web server. L'ORDINE conta: AI.Attach per ultimo ----
  gServer.addChainProcessor(gRouter, True);
  gOAuth.Attach(gServer, gTokens, @gUsers.Authenticate, @gUsers.RefreshUser);
  gAI.Attach(gServer, gRouter, gOAuth);   // chiede che OAuth sia gia' attivo e che l'endpoint "ai" esista
  // Da qui router, provider OAuth e connettore sono del WEB SERVER (lo liberera' lui).
  // gTokens, gUsers e gHandler restano nostri: in un'applicazione vera si liberano DOPO il web server.

  gTokens.Activate;
  gServer.Activate;
  WriteLn('Esempio 1 in ascolto sulla porta ', gPort, '. Ctrl+C per fermare.');
  WriteLn('Utenti: mario/pw1 (lettore), anna/pw2 (tecnico), admin/pw3, ospite/pw4 (niente AI), temp/pw5');
  Flush(Output);
  WaitForever;
end.
