program Ex03_PermessiAudit;
(*
  ESEMPIO 3 - PERMESSI PER RUOLO, REGISTRO DELLE CHIAMATE E LIMITI

  Le opzioni del connettore stanno nel file INI ([AIConnector]); nel codice resta solo cio' che l'INI non puo'
  esprimere: il registro delle chiamate (OnToolCalled), che si imposta DOPO Attach tramite Setup.Connector.

  COSA VEDE E COSA PUO' FARE OGNI RUOLO
    Il filtro e' sempre Area.OpType del ruolo, applicato due volte: in tools/list (lo strumento non compare)
    e in tools/call (rifiuto con 403). L'agente non ha permessi propri.

      ospite  non ha ai.Uso: il canale /mcp gli risponde 403 (porta chiusa a tutto il ruolo)
      mario   (lettore)  vede   officina_ListaOrdini
      anna    (tecnico)  vede   officina_ListaOrdini, officina_ChiudiOrdine
      admin              vede gli stessi due: "Stato" non ha OpType e RequireOpTypeForTools lo esclude

  OPZIONI MOSTRATE (tutte in Ex03.ini, sezione [AIConnector])
    RequireOpTypeForTools=1  uno strumento senza OpType (aperto a ogni utente autenticato) non viene esposto
    ExposeUndescribed=1      ChiudiOrdine non e' descritta ma e' esposta, con parametri liberi
    MaxCallsPerMinute=5      5 chiamate al minuto per utente: la sesta riceve "Too many requests" (429)
    AllowedOrigins           una richiesta con header Origin va accettata esplicitamente (i client MCP server-to-server
                             non lo mandano); qualunque altra Origin riceve 403
  NEL CODICE
    OnToolCalled             una riga di registro per ogni chiamata, in qualunque esito (si stampa qui)

  Uso:  Ex03_PermessiAudit      (porta 8083)
  Prova: python3 ../client/smoke_test.py ex03 http://127.0.0.1:8083
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson,
  uHTTPConsts, uWebRouteRegistry, uLBAIConnectorTypes,
  uEsempiComune;

type
  TEx03Handler = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function DoListaOrdini(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoChiudiOrdine(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
    function DoStato(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
      out aStatusCode: Integer): TJSONData;
  public
    constructor Create;
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

  { Registro delle chiamate. Si chiama a ogni tools/call: se serve scrivere su file o database, qui. }
  TAudit = class(TObject)
  public
    procedure ToolCalled(Sender: TObject; aUserId: Integer; const aToolName: String;
      aStatusCode: Integer; aSucceeded: Boolean; anElapsedMs: Int64);
  end;

constructor TEx03Handler.Create;
begin
  inherited Create('officina');
  Self.RegisterStandardWorker('ListaOrdini', @Self.DoListaOrdini);
  Self.RegisterStandardWorker('ChiudiOrdine', @Self.DoChiudiOrdine);
  Self.RegisterStandardWorker('Stato', @Self.DoStato);
end;

function TEx03Handler.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'ListaOrdini') then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddEnumParam('stato', 'Stato degli ordini', 'aperto,chiuso');
  end
  else
    Result := False;   // ChiudiOrdine e Stato: non descritte
end;

function TEx03Handler.DoListaOrdini(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _List  : TJSONArray;
  _Order : TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _List := TJSONArray.Create;
  _Order := TJSONObject.Create;
  _Order.Add('numero', 101);
  _Order.Add('stato', aRequestData.Get('stato', 'aperto'));
  _List.Add(_Order);
  Result := _List;
end;

function TEx03Handler.DoChiudiOrdine(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
var
  _Obj: TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Obj := TJSONObject.Create;
  _Obj.Add('chiuso_da', aUserId);
  Result := _Obj;
end;

function TEx03Handler.DoStato(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject;
  out aStatusCode: Integer): TJSONData;
begin
  aStatusCode := HTTP_STATUS_OK;
  Result := TJSONObject.Create;
end;

procedure TAudit.ToolCalled(Sender: TObject; aUserId: Integer; const aToolName: String;
  aStatusCode: Integer; aSucceeded: Boolean; anElapsedMs: Int64);
begin
  // Gli argomenti NON arrivano qui (possono contenere dati riservati): solo chi, cosa, esito, durata.
  WriteLn(Format('AUDIT utente=%d strumento=%s stato=%d esito=%s ms=%d',
    [aUserId, aToolName, aStatusCode, BoolToStr(aSucceeded, 'ok', 'errore'), anElapsedMs]));
  Flush(Output);
end;

var
  gHost  : TExampleHost;
  gAudit : TAudit;
begin
  gHost := TExampleHost.Create('Ex03.ini', 'Routes_Ex03.xml');
  gHost.AddHandler(TEx03Handler.Create);
  gHost.Wire;                            // router, token, OAuth e connettore (le opzioni vengono da Ex03.ini)
  if not gHost.AI.Active then
  begin
    WriteLn('Connettore AI non attivo: controlla [OAuth] e [AIConnector] in Ex03.ini e il log');
    Halt(1);
  end;

  // Il file INI non copre gli eventi: dopo Attach il connettore e' raggiungibile da Setup.Connector.
  gAudit := TAudit.Create;
  gHost.AI.Connector.OnToolCalled := @gAudit.ToolCalled;
  WriteLn('Strumenti nel catalogo: ', gHost.AI.Connector.ToolCount);

  gHost.Serve;
end.
