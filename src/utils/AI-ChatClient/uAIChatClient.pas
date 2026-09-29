unit uAIChatClient;
{
  Generic, PROJECT-AGNOSTIC client for an OpenAI-COMPATIBLE "chat
  completions" HTTP API, built on Ararat Synapse (httpsend/synautil).

  AGGIORNAMENTO (QUESTO GIRO): property Config (SOLA LETTURA)
  ----------------------------------------------------------------------
  Aggiunta una property pubblica Config (read FConfig, nessun setter) che
  espone in SOLA LETTURA la configurazione immutabile del client,
  restituita PER VALORE (il chiamante ottiene una COPIA; FConfig non e' mai
  modificabile dall'esterno). Serve al livello REST (uCMMS_HandlersSearch.
  DoConsult) per costruire, quando arrivano override per-richiesta
  (max_tokens/temperature/timeout/num_ctx via ExtraParams), un client
  TEMPORANEO con config derivata da questa - senza introdurre alcuno stato
  per-richiesta in questa classe. Essendo sola lettura, non intacca in alcun
  modo la thread-safety gia' documentata sotto (concorrenti solo in
  lettura su FConfig).

  DELIBERATELY GENERIC - NOT TIED TO ANY SPECIFIC APPLICATION
  ----------------------------------------------------------------------
  This unit has no knowledge of, and no dependency on, any specific
  project it might be used from. It knows only "a system prompt, a user
  message, a JSON-speaking HTTP endpoint, a typed answer". Any
  project-specific naming, default system prompt content, or convenience
  wrapper belongs in a thin layer the CALLING project writes around this
  unit - never inside it.

  GESTIONE DEL TAG <think> (ragionamento interno del modello)
  ----------------------------------------------------------------------
  Alcuni modelli "reasoning" restituiscono il proprio ragionamento interno
  racchiuso tra <think> e </think>. TryStripThinkingBlock gestisce tre casi:
  nessun tag (testo invariato), tag chiuso correttamente (ragionamento
  scartato, resta il testo dopo), tag aperto ma mai chiuso (budget MaxTokens
  esaurito durante il ragionamento -> risposta trattata come FALLITA). Questa
  e' una protezione lato RISPOSTA, indipendente dal provider - INVARIATA in
  questo aggiornamento.

  ExtraParamsJSON (parametri specifici del provider)
  ----------------------------------------------------------------------
  TAIChatProviderConfig.ExtraParamsJSON ('' di default) permette al
  chiamante di iniettare campi proprietari del proprio provider (es.
  "think" o "num_ctx" di Ollama) nel corpo della richiesta, uniti da
  BuildRequestBody DOPO i campi standard (quindi possono sovrascriverli).
  Questa libreria generica non conosce ne' fissa alcun nome di campo
  specifico del provider.

  CRITICAL DEPLOYMENT REQUIREMENT: Synapse's SSL plugin
  ----------------------------------------------------------------------
  Il programma finale che collega questa unit DEVE includere anche un'unit
  di plugin SSL di Synapse (tipicamente ssl_openssl) in una propria
  clausola uses, anche se nessun codice qui la referenzia per nome: senza,
  ogni chiamata verso un Endpoint https:// fallisce a livello di rete.

  VERIFICATION STATUS: questa unit NON e' stata test-compilata (nessun
  compilatore Free Pascal disponibile nell'ambiente). PLEASE COMPILE e
  segnala eventuali errori - in particolare intorno alla nuova property
  Config e al blocco di merge di ExtraParamsJSON in BuildRequestBody.
}
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, fpjson;
type
  { Configuration for a single AI provider endpoint. }
  TAIChatProviderConfig = record
    Endpoint     : String;
    APIKey       : String;
    Model        : String;
    Temperature  : Double;
    MaxTokens    : Integer;
    TimeoutMs    : Integer;
    SystemPrompt : String;
    MaxRetries   : Integer;
    RetryDelayMs : Integer;
    // '' (default) = nessun parametro extra, comportamento identico a prima
    // che questo campo esistesse. Altrimenti DEVE essere un OGGETTO JSON
    // i cui membri di primo livello vengono uniti al corpo della richiesta
    // costruito da BuildRequestBody, DOPO ogni campo che questa libreria
    // imposta di suo (un membro con la stessa chiave SOVRASCRIVE quello
    // impostato sopra). Via di fuga generica per parametri proprietari del
    // provider - es. '{"think":false,"num_ctx":16384}' per Ollama.
    ExtraParamsJSON : String;
  end;
  { A single labelled piece of reference material to ground a request in. }
  TAIChatContextItem = record
    Label_  : String; // trailing underscore: 'Label' collides with a reserved word in some contexts
    Content : String;
  end;
  TAIChatContextItemArray = array of TAIChatContextItem;
  { Optional logging callback - a plain (level, message) signature. }
  TAIChatLogEvent = procedure(const aLevel, aMessage: String) of object;
  { TAIChatAnswer - always returned, always owned by the CALLER (Free it). }
  TAIChatAnswer = class(TObject)
  public
    Success         : Boolean;
    NetworkError    : Boolean;
    HTTPStatusCode  : Integer;
    Answer          : String;
    ErrorMessage    : String;
    RawResponseBody : String;
    constructor Create;
  end;
  TAIChatClient = class(TObject)
  strict private
    FConfig : TAIChatProviderConfig;
    FOnLog  : TAIChatLogEvent;
    procedure Log(const aLevel, aMessage: String);
    function  BuildRequestBody(const aSystemPrompt, aUserMessage: String): String;
    function  ParseSuccessAnswer(aRoot: TJSONObject; out aAnswerText: String): Boolean;
    function  TryStripThinkingBlock(const aRawText: String; out aCleanText: String): Boolean;
    function  ExtractProviderErrorMessage(aRoot: TJSONObject): String;
    function  IsTransientFailure(anAnswer: TAIChatAnswer): Boolean;
    function  DoAsk(const aSystemPrompt, aUserMessage: String): TAIChatAnswer;
    function  DoAskWithRetry(const aSystemPrompt, aUserMessage: String): TAIChatAnswer;
  public
    constructor Create(const aConfig: TAIChatProviderConfig);
    function Ask(const aUserMessage: String): TAIChatAnswer; overload;
    function Ask(const aUserMessage, aSystemPromptOverride: String): TAIChatAnswer; overload;
    function AskWithContext(const aQuestion, aContext: String): TAIChatAnswer;
    function AskWithReferenceItems(const aQuestion: String; const aItems: TAIChatContextItemArray): TAIChatAnswer;
    class function BuildContextFromItems(const aItems: TAIChatContextItemArray): String;
    property OnLog: TAIChatLogEvent read FOnLog write FOnLog;
    // NUOVO (QUESTO GIRO): configurazione in SOLA LETTURA (per valore -> il
    // chiamante ottiene una copia; FConfig resta immutabile dall'esterno) -
    // vedi la nota "AGGIORNAMENTO (QUESTO GIRO)" in testa alla unit.
    property Config: TAIChatProviderConfig read FConfig;
  end;
function AIChatDefaultSystemPrompt: String;
implementation
uses
  httpsend, jsonparser;
const
  cAIChat_TransientHTTPCodes: array[0..4] of Integer = (429, 500, 502, 503, 504);
  cAIChat_DefaultTimeoutMs  = 120000;
  cAIChat_DefaultRetryDelay = 1000;
  cThink_OpenTag  = '<think>';
  cThink_CloseTag = '</think>';
function AIChatDefaultSystemPrompt: String;
begin
  Result :=
    'Sei un assistente utile e preciso. Se ti viene fornito del materiale di riferimento, ' +
    'basa la tua risposta ESCLUSIVAMENTE su di esso e dichiara esplicitamente se non contiene ' +
    'quanto richiesto, invece di inventare. Rispondi nella stessa lingua della richiesta.';
end;
constructor TAIChatAnswer.Create;
begin
  inherited Create;
  Success         := False;
  NetworkError    := False;
  HTTPStatusCode  := 0;
  Answer          := '';
  ErrorMessage    := '';
  RawResponseBody := '';
end;
constructor TAIChatClient.Create(const aConfig: TAIChatProviderConfig);
begin
  inherited Create;
  FConfig := aConfig;
  if Trim(FConfig.SystemPrompt) = '' then
    FConfig.SystemPrompt := AIChatDefaultSystemPrompt;
  if FConfig.TimeoutMs <= 0 then
    FConfig.TimeoutMs := cAIChat_DefaultTimeoutMs;
  if FConfig.RetryDelayMs <= 0 then
    FConfig.RetryDelayMs := cAIChat_DefaultRetryDelay;
  if FConfig.MaxRetries < 0 then
    FConfig.MaxRetries := 0;
  // ExtraParamsJSON: nessuna inizializzazione necessaria (String gestita,
  // gia' '' di default). E' compito del chiamante decidere se e cosa
  // includervi, in base ai campi realmente supportati dal proprio provider.
  FOnLog := nil;
end;
procedure TAIChatClient.Log(const aLevel, aMessage: String);
begin
  if Assigned(FOnLog) then
    FOnLog(aLevel, aMessage);
end;
function TAIChatClient.BuildRequestBody(const aSystemPrompt, aUserMessage: String): String;
{
  In fondo: fusione generica di FConfig.ExtraParamsJSON. Se non vuota, ogni
  membro di primo livello dell'oggetto JSON che contiene viene aggiunto a
  _Root, SOVRASCRIVENDO (Delete + Add) un campo omonimo gia' impostato sopra
  (model/temperature/stream/max_tokens/messages). .Clone() su ogni valore
  estratto -> _Extra resta proprietario dei propri elementi, _Root riceve
  una copia indipendente (nessuna doppia liberazione). Qualunque problema di
  parsing viene loggato e ignorato: la richiesta parte comunque coi soli
  campi standard.
}
var
  _Root        : TJSONObject;
  _Messages    : TJSONArray;
  _SystemMsg   : TJSONObject;
  _UserMsg     : TJSONObject;
  _Extra       : TJSONData;
  _ExtraObj    : TJSONObject;
  i            : Integer;
  _Name        : String;
  _ExistingIdx : Integer;
begin
  _Root := TJSONObject.Create;
  try
    _Root.Add('model', FConfig.Model);
    _Root.Add('temperature', FConfig.Temperature);
    _Root.Add('stream', False);
    if FConfig.MaxTokens > 0 then
      _Root.Add('max_tokens', FConfig.MaxTokens);
    _Messages := TJSONArray.Create;
    _SystemMsg := TJSONObject.Create;
    _SystemMsg.Add('role', 'system');
    _SystemMsg.Add('content', aSystemPrompt);
    _Messages.Add(_SystemMsg);
    _UserMsg := TJSONObject.Create;
    _UserMsg.Add('role', 'user');
    _UserMsg.Add('content', aUserMessage);
    _Messages.Add(_UserMsg);
    _Root.Add('messages', _Messages);
    if Trim(FConfig.ExtraParamsJSON) <> '' then
    begin
      _Extra := nil;
      try
        try
          _Extra := GetJSON(FConfig.ExtraParamsJSON);
          if (_Extra <> nil) and (_Extra.JSONType = jtObject) then
          begin
            _ExtraObj := TJSONObject(_Extra);
            for i := 0 to _ExtraObj.Count - 1 do
            begin
              _Name := _ExtraObj.Names[i];
              _ExistingIdx := _Root.IndexOfName(_Name);
              if _ExistingIdx >= 0 then
                _Root.Delete(_ExistingIdx);
              _Root.Add(_Name, _ExtraObj.Items[i].Clone);
            end;
          end
          else
            Self.Log('warning', 'ExtraParamsJSON non e'' un oggetto JSON valido, ignorato: ' + FConfig.ExtraParamsJSON);
        except
          on E: Exception do
            Self.Log('error', 'Impossibile interpretare ExtraParamsJSON, ignorato: ' + E.Message + ' - valore: ' + FConfig.ExtraParamsJSON);
        end;
      finally
        if _Extra <> nil then
          _Extra.Free;
      end;
    end;
    Result := _Root.AsJSON;
  finally
    _Root.Free;
  end;
end;
function TAIChatClient.ParseSuccessAnswer(aRoot: TJSONObject; out aAnswerText: String): Boolean;
var
  _Data : TJSONData;
begin
  Result := False;
  aAnswerText := '';
  if aRoot = nil then Exit;
  _Data := aRoot.FindPath('choices[0].message.content');
  if (_Data <> nil) and (_Data.JSONType = jtString) then
  begin
    aAnswerText := _Data.AsString;
    Result := True;
  end;
end;
function TAIChatClient.TryStripThinkingBlock(const aRawText: String; out aCleanText: String): Boolean;
{
  Tre casi:
  1) Nessun tag <think>: aCleanText = aRawText, Result = True.
  2) <think>...</think> chiuso: aCleanText = testo DOPO </think>
     (ragionamento scartato), Result = True. Se il testo dopo e' vuoto,
     Result resta True ma aCleanText = '' (il chiamante decide).
  3) <think> aperto ma MAI chiuso (caso reale in produzione): aCleanText = '',
     Result = False - il chiamante deve trattarlo come fallimento.
}
var
  _OpenPos  : Integer;
  _ClosePos : Integer;
begin
  aCleanText := '';
  _OpenPos := Pos(cThink_OpenTag, aRawText);
  if _OpenPos = 0 then
  begin
    aCleanText := aRawText;
    Result := True;
    Exit;
  end;
  _ClosePos := Pos(cThink_CloseTag, aRawText);
  if (_ClosePos = 0) or (_ClosePos < _OpenPos) then
  begin
    Result := False;
    Exit;
  end;
  aCleanText := Trim(Copy(aRawText, _ClosePos + Length(cThink_CloseTag), MaxInt));
  Result := True;
end;
function TAIChatClient.ExtractProviderErrorMessage(aRoot: TJSONObject): String;
var
  _Data : TJSONData;
begin
  Result := '';
  if aRoot = nil then Exit;
  _Data := aRoot.FindPath('error.message');
  if (_Data <> nil) and (_Data.JSONType = jtString) then
    Result := _Data.AsString;
end;
function TAIChatClient.IsTransientFailure(anAnswer: TAIChatAnswer): Boolean;
var
  i : Integer;
begin
  Result := anAnswer.NetworkError;
  if Result then Exit;
  for i := Low(cAIChat_TransientHTTPCodes) to High(cAIChat_TransientHTTPCodes) do
    if anAnswer.HTTPStatusCode = cAIChat_TransientHTTPCodes[i] then
      Exit(True);
end;
function TAIChatClient.DoAsk(const aSystemPrompt, aUserMessage: String): TAIChatAnswer;
var
  _HTTP        : THTTPSend;
  _RequestBody : String;
  _ReqStream   : TStringStream;
  _RespStream  : TStringStream;
  _ResponseStr : String;
  _ParsedRoot  : TJSONData;
  _ProviderErr : String;
  _AnswerText  : String;
  _CleanAnswer : String;
begin
  Result := TAIChatAnswer.Create;
  if (Trim(FConfig.Endpoint) = '') or (Trim(FConfig.Model) = '') then
  begin
    Result.ErrorMessage := 'TAIChatClient misconfigured: Endpoint and Model are both required.';
    Self.Log('error', Result.ErrorMessage);
    Exit;
  end;
  _RequestBody := Self.BuildRequestBody(aSystemPrompt, aUserMessage);
  _HTTP := THTTPSend.Create;
  try
    _HTTP.Timeout := FConfig.TimeoutMs;
    if FConfig.APIKey <> '' then
      _HTTP.Headers.Add('Authorization: Bearer ' + FConfig.APIKey);
    _HTTP.MimeType := 'application/json; charset=utf-8';
    _ReqStream := TStringStream.Create(_RequestBody, TEncoding.UTF8);
    try
      _HTTP.Document.LoadFromStream(_ReqStream);
    finally
      _ReqStream.Free;
    end;
    if not _HTTP.HTTPMethod('POST', FConfig.Endpoint) then
    begin
      Result.NetworkError := True;
      Result.ErrorMessage := 'Network error contacting the AI provider: ' + _HTTP.Sock.LastErrorDesc;
      Self.Log('error', Result.ErrorMessage);
      Exit;
    end;
    _RespStream := TStringStream.Create('', TEncoding.UTF8);
    try
      _RespStream.CopyFrom(_HTTP.Document, 0);
      _ResponseStr := _RespStream.DataString;
    finally
      _RespStream.Free;
    end;
    Result.HTTPStatusCode  := _HTTP.ResultCode;
    Result.RawResponseBody := _ResponseStr;
    if Trim(_ResponseStr) = '' then
    begin
      Result.ErrorMessage := Format('Empty response body from AI provider (HTTP %d).', [_HTTP.ResultCode]);
      Self.Log('error', Result.ErrorMessage);
      Exit;
    end;
    try
      _ParsedRoot := GetJSON(_ResponseStr);
    except
      on E: Exception do
      begin
        Result.ErrorMessage := 'Malformed JSON in AI provider response: ' + E.Message;
        Self.Log('error', Result.ErrorMessage + ' Raw: ' + _ResponseStr);
        Exit;
      end;
    end;
    try
      if (_ParsedRoot = nil) or (_ParsedRoot.JSONType <> jtObject) then
      begin
        Result.ErrorMessage := 'AI provider response is not a JSON object.';
        Self.Log('error', Result.ErrorMessage + ' Raw: ' + _ResponseStr);
        Exit;
      end;
      if _HTTP.ResultCode <> 200 then
      begin
        _ProviderErr := Self.ExtractProviderErrorMessage(TJSONObject(_ParsedRoot));
        if _ProviderErr <> '' then
          Result.ErrorMessage := Format('AI provider error (HTTP %d): %s', [_HTTP.ResultCode, _ProviderErr])
        else
          Result.ErrorMessage := Format('AI provider returned HTTP %d.', [_HTTP.ResultCode]);
        Self.Log('warning', Result.ErrorMessage);
        Exit;
      end;
      if Self.ParseSuccessAnswer(TJSONObject(_ParsedRoot), _AnswerText) then
      begin
        if Self.TryStripThinkingBlock(_AnswerText, _CleanAnswer) then
        begin
          if Trim(_CleanAnswer) = '' then
          begin
            Result.ErrorMessage := 'Il modello ha prodotto solo il proprio ragionamento interno (<think>...</think>), senza alcuna risposta finale dopo di esso.';
            Self.Log('warning', Result.ErrorMessage + ' Raw: ' + _AnswerText);
          end
          else begin
            Result.Answer  := _CleanAnswer;
            Result.Success := True;
          end;
        end
        else begin
          Result.ErrorMessage :=
            'Il modello ha esaurito il budget di token (MaxTokens) durante il proprio ragionamento interno, ' +
            'senza produrre alcuna risposta finale. Aumentare MaxTokens nella configurazione, oppure, se il ' +
            'proprio provider lo supporta VERIFICATAMENTE, disattivare il ragionamento a monte tramite il ' +
            'campo che il proprio provider realmente usa a questo scopo (es. "think" per Ollama), impostabile ' +
            'tramite ExtraParamsJSON - attenzione: alcuni provider rifiutano con un errore l''intera richiesta ' +
            'se un campo non riconosciuto viene inviato - verificare prima di impostarlo.';
          Self.Log('warning', Result.ErrorMessage + ' Raw (troncato): ' + Copy(_AnswerText, 1, 500));
        end;
      end
      else
      begin
        Result.ErrorMessage := 'Unexpected JSON structure in AI provider response (missing choices[0].message.content).';
        Self.Log('error', Result.ErrorMessage + ' Raw: ' + _ResponseStr);
      end;
    finally
      _ParsedRoot.Free;
    end;
  finally
    _HTTP.Free;
  end;
end;
function TAIChatClient.DoAskWithRetry(const aSystemPrompt, aUserMessage: String): TAIChatAnswer;
var
  _Attempt : Integer;
begin
  _Attempt := 0;
  repeat
    Result := Self.DoAsk(aSystemPrompt, aUserMessage);
    if Result.Success or (not Self.IsTransientFailure(Result)) or (_Attempt >= FConfig.MaxRetries) then
      Exit;
    Self.Log('warning', Format('Transient AI provider failure (attempt %d of %d), retrying in %dms: %s',
      [_Attempt + 1, FConfig.MaxRetries, FConfig.RetryDelayMs, Result.ErrorMessage]));
    Result.Free;
    Inc(_Attempt);
    Sleep(FConfig.RetryDelayMs);
  until False;
end;
function TAIChatClient.Ask(const aUserMessage: String): TAIChatAnswer;
begin
  Result := Self.DoAskWithRetry(FConfig.SystemPrompt, aUserMessage);
end;
function TAIChatClient.Ask(const aUserMessage, aSystemPromptOverride: String): TAIChatAnswer;
begin
  Result := Self.DoAskWithRetry(aSystemPromptOverride, aUserMessage);
end;
function TAIChatClient.AskWithContext(const aQuestion, aContext: String): TAIChatAnswer;
var
  _UserMessage : String;
begin
  _UserMessage := 'Materiale di riferimento:' + LineEnding + aContext + LineEnding + LineEnding + 'Richiesta: ' + aQuestion;
  Result := Self.DoAskWithRetry(FConfig.SystemPrompt, _UserMessage);
end;
function TAIChatClient.AskWithReferenceItems(const aQuestion: String; const aItems: TAIChatContextItemArray): TAIChatAnswer;
var
  _Context : String;
begin
  _Context := TAIChatClient.BuildContextFromItems(aItems);
  Result := Self.AskWithContext(aQuestion, _Context);
end;
class function TAIChatClient.BuildContextFromItems(const aItems: TAIChatContextItemArray): String;
var
  i      : Integer;
  _Lines : TStringList;
begin
  _Lines := TStringList.Create;
  try
    for i := 0 to High(aItems) do
    begin
      if aItems[i].Label_ <> '' then
        _Lines.Add('[' + IntToStr(i + 1) + '] ' + aItems[i].Label_)
      else
        _Lines.Add('[' + IntToStr(i + 1) + ']');
      _Lines.Add(aItems[i].Content);
      _Lines.Add('');
    end;
    Result := Trim(_Lines.Text);
  finally
    _Lines.Free;
  end;
end;
end.
