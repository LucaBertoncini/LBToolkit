program Ex02_Parametri;
(*
  ESEMPIO 2 - COME SI DESCRIVONO I PARAMETRI (E COSA CONTROLLA IL CONNETTORE PRIMA DEL WORKER)

  Una funzione descritta con DescribeAIFunction ha uno "schema" dei parametri. Il connettore:
    - lo pubblica all'agente (inputSchema di tools/list), cosi' l'agente sa cosa mandare;
    - controlla gli argomenti PRIMA di chiamare il worker: obbligatori, tipo, valori ammessi, date reali,
      parametri sconosciuti; converte "5" in 5 e "true" in true. L'errore e' scritto per l'agente, che puo' correggere.

  Funzioni (area "catalogo"):
    Cerca         tutti i tipi di parametro; risponde con quello che il worker ha RICEVUTO (dopo i controlli)
    Crea          Effect = aeModifies: l'agente puo' chiedere conferma all'utente. Risponde 201.
    Elimina       Effect = aeDestructive. Risponde 404: mostra come arriva un errore dello strumento.
    Interna       descritta ma Hidden = True: non e' mai uno strumento, per nessun ruolo
    NonDescritta  nessuna descrizione: e' uno strumento SOLO con ExposeUndescribed=1 nel file INI,
                  e accetta un oggetto JSON libero

  Tipi disponibili: aptString, aptInteger, aptNumber, aptBoolean, aptDate, aptDateTime, aptObject, aptArray.

  Uso:  Ex02_Parametri      (porta 8082)
  Prova: python3 ../client/smoke_test.py ex02 http://127.0.0.1:8082
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson,
  uHTTPConsts, uWebRouteRegistry, uLBAIConnectorTypes,
  uEsempiComune;

type
  TEx02Handler = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function Echo(aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoCerca(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoCrea(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoElimina(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoInterna(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoNonDescritta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
  public
    constructor Create;
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

constructor TEx02Handler.Create;
begin
  inherited Create('catalogo');
  Self.RegisterStandardWorker('Cerca', @Self.DoCerca);
  Self.RegisterStandardWorker('Crea', @Self.DoCrea);
  Self.RegisterStandardWorker('Elimina', @Self.DoElimina);
  Self.RegisterStandardWorker('Interna', @Self.DoInterna);
  Self.RegisterStandardWorker('NonDescritta', @Self.DoNonDescritta);
end;

function TEx02Handler.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'Cerca') then
  begin
    aDescriptor.Title := 'Cerca nel catalogo';
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('testo', aptString, 'Parole da cercare', True);                          // obbligatorio
    aDescriptor.AddEnumParam('stato', 'Stato degli elementi', 'aperto,chiuso');                   // solo questi valori
    aDescriptor.AddParam('limite', aptInteger, 'Numero massimo di righe (default 50)');
    aDescriptor.AddParam('soglia', aptNumber, 'Valore minimo, anche decimale');
    aDescriptor.AddParam('solo_attivi', aptBoolean, 'true = escludi gli elementi non attivi');
    aDescriptor.AddParam('dal', aptDate, 'Dal giorno, nel formato AAAA-MM-GG');                   // data reale
    aDescriptor.AddParam('alle', aptDateTime, 'Fino a questo istante, ISO 8601 (AAAA-MM-GGTHH:MM:SS)');
    aDescriptor.AddArrayParam('ids', aptInteger, 'Elenco di identificativi');                     // ogni elemento controllato
  end
  else if SameText(aFunctionName, 'Crea') then
  begin
    aDescriptor.Title := 'Crea un elemento';
    aDescriptor.Effect := aeModifies;
    aDescriptor.AddParam('titolo', aptString, 'Titolo dell''elemento', True);
    aDescriptor.AddEnumParam('priorita', 'Priorita''', 'bassa,media,alta', True);
  end
  else if SameText(aFunctionName, 'Elimina') then
  begin
    aDescriptor.Title := 'Elimina un elemento';
    aDescriptor.Effect := aeDestructive;
    aDescriptor.AddParam('id', aptInteger, 'Identificativo dell''elemento da eliminare', True);
  end
  else if SameText(aFunctionName, 'Interna') then
    aDescriptor.Hidden := True        // descritta, ma mai esposta
  else
    Result := False;                  // NonDescritta: nessuna descrizione precisa
end;

// Risponde con una copia di quanto il worker ha ricevuto: l'oggetto degli argomenti lo libera il connettore.
function TEx02Handler.Echo(aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('ricevuto', aRequestData.Clone);
  Result := _Obj;
end;

function TEx02Handler.DoCerca(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.Echo(aRequestData, aStatusCode);
end;

function TEx02Handler.DoCrea(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_CREATED;        // 201: per l'agente e' un successo (solo >= 400 e' un errore)
  _Obj := TJSONObject.Create;
  _Obj.Add('creato', True);
  _Obj.Add('titolo', aRequestData.Get('titolo', ''));
  _Obj.Add('priorita', aRequestData.Get('priorita', ''));
  Result := _Obj;
end;

function TEx02Handler.DoElimina(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_NOT_FOUND;      // >= 400: l'agente vede isError e questo testo, e puo' correggersi
  _Obj := TJSONObject.Create;
  _Obj.Add('error', 'Elemento non trovato: ' + IntToStr(aRequestData.Get('id', Int64(0))));
  Result := _Obj;
end;

function TEx02Handler.DoInterna(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.Echo(aRequestData, aStatusCode);
end;

function TEx02Handler.DoNonDescritta(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  Result := Self.Echo(aRequestData, aStatusCode);
end;

var
  gHost: TExampleHost;
begin
  gHost := TExampleHost.Create('Ex02.ini', 'Routes_Ex02.xml');
  gHost.AddHandler(TEx02Handler.Create);
  gHost.Wire;
  gHost.Serve;
end.
