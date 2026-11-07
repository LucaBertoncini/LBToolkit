unit uAIDemoHandlers;
{
  Le funzioni dell'esempio (area "demo" di Routes.xml).

  UNA funzione e' scritta UNA volta sola e serve due ingressi:
    - il sito web:      POST /api/demo/<funzione>  (con il cookie di sessione)
    - l'agente AI:      strumento MCP "demo_<Funzione>"  (con il token Bearer ottenuto via OAuth)
  Il metodo Pascal e' lo stesso. Il permesso e' lo stesso: "demo.<OpType>" dichiarato in Routes.xml.

  PER ESSERE UNO STRUMENTO una funzione deve
    1. avere un endpoint Standard con autenticazione in Routes.xml (qui: tutte);
    2. essere DESCRITTA da DescribeAIFunction (nomi, tipi e obbligatorieta' dei parametri), salvo ExposeUndescribed=1.
  Il connettore controlla gli argomenti PRIMA di chiamare il metodo: obbligatori, tipo, valori ammessi, date reali,
  parametri sconosciuti; converte "5" in 5 e "true" in true. Il metodo riceve valori gia' validi.

  FUNZIONI
    Ping          demo.Lettura         chi sono (id, ruolo, permessi di chi ha autorizzato l'agente)
    Somma         demo.Lettura         due interi obbligatori
    Cerca         demo.Lettura         tutti i tipi di parametro; risponde con cio' che ha RICEVUTO
    Stato         (nessun OpType)      aperta a ogni utente autenticato: RequireOpTypeForTools=1 la esclude
    CreaNota      demo.Modifica        aeModifies; risponde 201
    EliminaNota   demo.Modifica        aeDestructive; risponde 404: mostra come arriva un errore dello strumento
    Report        demo.Amministrazione solo l'amministratore la vede
    Interna       demo.Lettura         descritta ma Hidden: non e' MAI uno strumento
    NonDescritta  demo.Lettura         nessuna descrizione: e' uno strumento solo con ExposeUndescribed=1
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, uWebRouteRegistry, uLBAIConnectorTypes;

type
  { TDemoHandlers }
  TDemoHandlers = class(TRouteHandlerBase, IAIConnectorFunctions)
  strict private
    function EchoArguments(aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoPing(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoSomma(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoCerca(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoStato(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoCreaNota(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoEliminaNota(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoReport(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoInterna(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoNonDescritta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
  public
    constructor Create;

    // IAIConnectorFunctions: i parametri delle funzioni che l'agente puo' usare.
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

implementation

uses
  uHTTPConsts;

constructor TDemoHandlers.Create;
begin
  inherited Create('demo');   // codice dell'area in Routes.xml
  // Registrazione normale: lo stesso metodo serve il sito e l'agente AI.
  Self.RegisterStandardWorker('Ping', @Self.DoPing);
  Self.RegisterStandardWorker('Somma', @Self.DoSomma);
  Self.RegisterStandardWorker('Cerca', @Self.DoCerca);
  Self.RegisterStandardWorker('Stato', @Self.DoStato);
  Self.RegisterStandardWorker('CreaNota', @Self.DoCreaNota);
  Self.RegisterStandardWorker('EliminaNota', @Self.DoEliminaNota);
  Self.RegisterStandardWorker('Report', @Self.DoReport);
  Self.RegisterStandardWorker('Interna', @Self.DoInterna);
  Self.RegisterStandardWorker('NonDescritta', @Self.DoNonDescritta);
end;

{ ---- descrizione per l'agente AI ---------------------------------------------------------------------------- }

function TDemoHandlers.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'Ping') then
    aDescriptor.Effect := aeReadOnly                          // nessun parametro
  else if SameText(aFunctionName, 'Somma') then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('a', aptInteger, 'Primo addendo', True);
    aDescriptor.AddParam('b', aptInteger, 'Secondo addendo', True);
  end
  else if SameText(aFunctionName, 'Cerca') then
  begin
    aDescriptor.Title := 'Ricerca nel catalogo';
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('testo', aptString, 'Parole da cercare', True);                       // obbligatorio
    aDescriptor.AddEnumParam('stato', 'Stato degli elementi', 'aperto,chiuso');                // solo questi valori
    aDescriptor.AddParam('limite', aptInteger, 'Numero massimo di righe');
    aDescriptor.AddParam('soglia', aptNumber, 'Valore minimo, anche decimale');
    aDescriptor.AddParam('solo_attivi', aptBoolean, 'true = escludi gli elementi non attivi');
    aDescriptor.AddParam('dal', aptDate, 'Dal giorno, nel formato AAAA-MM-GG');                // data reale
    aDescriptor.AddParam('alle', aptDateTime, 'Fino a questo istante, ISO 8601 (AAAA-MM-GGTHH:MM:SS)');
    aDescriptor.AddArrayParam('ids', aptInteger, 'Elenco di identificativi');                  // ogni elemento controllato
  end
  else if SameText(aFunctionName, 'Stato') then
    aDescriptor.Effect := aeReadOnly
  else if SameText(aFunctionName, 'CreaNota') then
  begin
    aDescriptor.Title := 'Crea una nota';
    aDescriptor.Effect := aeModifies;                          // il client puo' chiedere conferma all'utente
    aDescriptor.AddParam('titolo', aptString, 'Titolo della nota', True);
    aDescriptor.AddEnumParam('priorita', 'Priorita della nota', 'bassa,media,alta', True);
  end
  else if SameText(aFunctionName, 'EliminaNota') then
  begin
    aDescriptor.Title := 'Elimina una nota';
    aDescriptor.Effect := aeDestructive;
    aDescriptor.AddParam('id', aptInteger, 'Identificativo della nota da eliminare', True);
  end
  else if SameText(aFunctionName, 'Report') then
    aDescriptor.Effect := aeReadOnly
  else if SameText(aFunctionName, 'Interna') then
    aDescriptor.Hidden := True                                 // descritta, ma mai esposta a nessun ruolo
  else
    Result := False;                                           // NonDescritta: nessuna descrizione precisa
end;

{ ---- le funzioni -------------------------------------------------------------------------------------------- }

// Risponde con una copia di cio' che il metodo ha ricevuto (l'oggetto degli argomenti e' del chiamante).
function TDemoHandlers.EchoArguments(aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('ricevuto', aRequestData.Clone);
  Result := _Obj;
end;

function TDemoHandlers.DoPing(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj   : TJSONObject;
  _Perms : TJSONData;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('pong', True);
  _Obj.Add('user_id', aUserId);                         // l'utente che ha autorizzato l'agente
  _Obj.Add('role', aUserConfig.Get('role', ''));
  _Perms := aUserConfig.Find(cJSONFieldPermissions);
  if _Perms <> nil then
    _Obj.Add('permissions', _Perms.Clone);
  Result := _Obj;
end;

function TDemoHandlers.DoSomma(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj : TJSONObject;
  _Sum : Int64;
begin
  // il connettore ha gia' controllato che a e b ci siano e siano interi (e convertito "2" in 2)
  aStatusCode := HTTP_STATUS_OK;
  _Sum := aRequestData.Get('a', Int64(0)) + aRequestData.Get('b', Int64(0));
  _Obj := TJSONObject.Create;
  _Obj.Add('somma', _Sum);
  Result := _Obj;
end;

function TDemoHandlers.DoCerca(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.EchoArguments(aRequestData, aStatusCode);
end;

function TDemoHandlers.DoStato(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('status', 'ok');
  Result := _Obj;
end;

function TDemoHandlers.DoCreaNota(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_CREATED;                   // 201: per l'agente e' un successo (solo >= 400 e' un errore)
  _Obj := TJSONObject.Create;
  _Obj.Add('creata', True);
  _Obj.Add('titolo', aRequestData.Get('titolo', ''));
  _Obj.Add('priorita', aRequestData.Get('priorita', ''));
  _Obj.Add('autore', aUserId);
  Result := _Obj;
end;

function TDemoHandlers.DoEliminaNota(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_NOT_FOUND;                 // >= 400: l'agente vede isError e questo testo
  _Obj := TJSONObject.Create;
  _Obj.Add('error', 'Nota non trovata: ' + IntToStr(aRequestData.Get('id', Int64(0))));
  Result := _Obj;
end;

function TDemoHandlers.DoReport(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('righe', 3);
  _Obj.Add('riservato', True);
  Result := _Obj;
end;

function TDemoHandlers.DoInterna(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.EchoArguments(aRequestData, aStatusCode);
end;

function TDemoHandlers.DoNonDescritta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.EchoArguments(aRequestData, aStatusCode);
end;

end.
