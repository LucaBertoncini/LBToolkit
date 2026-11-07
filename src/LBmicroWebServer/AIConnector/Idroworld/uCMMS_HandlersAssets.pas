unit uCMMS_HandlersAssets;
{
  CONVERSIONE AL NUOVO MECCANISMO DI DISPATCH (TMethod/RegisterStandardWorker)
  ----------------------------------------------------------------------
  Questa unit è stata convertita al nuovo modulo di routing generico
  (uWebRouteRegistry.pas): la classe deriva ora da TRouteHandlerBase, e
  ogni Function è registrata con RegisterStandardWorker invece di essere
  chiamata direttamente dal vecchio router con un TCMMSRequestContext.

  IDENTIFICAZIONE DIRETTA PER UUID: NESSUN RESOLVER, NESSUNA CONVERSIONE
  PRELIMINARE A ID NUMERICO (invariato - asset non e' toccato da questa
  migrazione: resta identificato per UUID, deciso esplicitamente)
  ----------------------------------------------------------------------
  L'UUID arriva come campo del corpo della richiesta ("uuid" per il nodo
  su cui si opera, "parent_uuid" per il padre in CreateAsset) e viene
  usato DIRETTAMENTE nella clausola WHERE di ciascuna query (uuid =
  CAST(:uuid AS uuid)), senza alcun passo di risoluzione preliminare.
  L'id numerico resta usato SOLO dove serve davvero (le JOIN interne a
  una query, es. asset.parent_id, asset.model_id): non attraversa mai il
  confine con il corpo della richiesta.

  IDENTIFICAZIONE DI ASSET MODEL: RESTA NUMERICA (INVARIATA)
  ----------------------------------------------------------------------
  asset_model non e' identificato per UUID: ModelsGetOne/ModelsUpdate/
  ModelsDelete/ModelsListDocuments continuano a identificare il modello
  con il proprio campo "id" numerico nel corpo della richiesta - e'
  un'entita' amministrata solo da chi ha permesso sull'area assets,
  senza una cascata di visibilita' per singolo modello (si veda
  PRINCIPIO_IDENTIFICATORI.md).

  DOCUMENTI DI UN MODELLO (ModelsListDocuments): ORA ESPOSTI PER UUID
  ----------------------------------------------------------------------
  document e' identificato ESTERNAMENTE per UUID (si veda
  uCMMS_HandlersDocuments.pas): ModelsListDocuments espone quindi
  document.uuid al posto di document.id, cosi' che il client possa
  operare direttamente su quel documento (modifica, validazione,
  download) con lo stesso identificatore usato ovunque altrove nel
  sistema per un documento.
}
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, fpjson,
  uIdroWorldDataAccess, uWebRouteRegistry, uLBAIConnectorTypes;
type
  { TCMMSHandlersAssets }
  TCMMSHandlersAssets = class(TRouteHandlerBase, IAIConnectorFunctions)
    strict private
      FDataAccess : IIdroWorldDataAccess;
      function ResolveAssetModelId(aBody: TJSONObject): Int64;
      { ASSET KINDS }
      function DoListKinds(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoCreateKind(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoUpdateKind(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoDeleteKind(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      { ASSETS }
      function DoGetRoots(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoGetTree(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoGetOne(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoCreateAsset(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoUpdate(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoDelete(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoSetUsable(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      { ASSET MODELS }
      function DoModelsList(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoModelsCreate(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoModelsGetOne(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoModelsUpdate(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoModelsDelete(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
      function DoModelsListDocuments(aUserId: Integer; aUserConfig: TJSONObject;
        aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    public
      constructor Create(aDataAccess: IIdroWorldDataAccess);
      // Descrizione precisa dei parametri per l'AI (connettore MCP). Le funzioni non elencate qui sono
      // comunque strumenti, con parametri liberi: il ruolo dell'utente decide cosa puo' eseguire.
      function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;
implementation
uses
  ULBLogger, uHTTPConsts, uIdroWorld_HandlerUtils, uIdroWorld_Permissions;

// Condizione SQL di VISIBILITA' dell'asset a.id per l'utente del parametro :user_id. Non amministratore:
// semi-join sugli asset precalcolati del ruolo (cmms.role_visible_asset), non una funzione per riga.
// Amministratore: condizione sempre vera, senza alcun accesso alla tabella; il parametro resta citato
// (un parametro non usato nel testo SQL e' un errore del driver).
function AssetTreeVisibleSql(aUserConfig: TJSONObject): String;
begin
  if (aUserConfig <> nil) and aUserConfig.Get('is_admin', False) then
    Result := '(CAST(:user_id AS bigint) > 0)'
  else
    Result := 'EXISTS (SELECT 1 FROM core.app_user rvu JOIN cmms.role_visible_asset rv ON rv.role_id = rvu.role_id ' +
              'WHERE rvu.id = :user_id AND rv.asset_id = a.id)';
end;

// Legge "cost_center_id" dal corpo di una creazione/modifica. aPresent = il campo c'e' (anche con valore 0 = "toglilo");
// risultato: 0 = nessun centro di costo proprio (eredita dal padre), > 0 = id valido, -1 = id che non esiste (400).
// Un campo ASSENTE in una modifica lascia il centro di costo com'e' (il dialogo lo invia solo se l'utente puo' vederlo).
function ReadCostCenterId(aData: IIdroWorldDataAccess; aBody: TJSONObject; out aPresent: Boolean): Int64;
var
  _D : TJSONData;
begin
  Result := 0;
  _D := aBody.Find('cost_center_id');
  aPresent := (_D <> nil) and (_D.JSONType = jtNumber);
  if not aPresent then Exit;
  Result := _D.AsInt64;
  if Result < 0 then Result := 0;
  if (Result > 0) and (aData.ScalarInt('SELECT id FROM cmms.cost_center WHERE id = :id', ['id', Result], 0, cIdroWorldMainDB) <= 0) then
    Result := -1;
end;

{ TCMMSHandlersAssets }

function TCMMSHandlersAssets.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  // L'elenco degli asset visibili dipende dall'utente (visibilita' del ruolo): vale anche per l'AI.
  Result := True;
  if aFunctionName = 'ListKinds' then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('all', aptBoolean, 'true per includere anche le tipologie non attive (default: solo attive)');
  end
  else if aFunctionName = 'GetRoots' then
    aDescriptor.Effect := aeReadOnly     // nessun parametro
  else if aFunctionName = 'GetTree' then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('uuid', aptString, 'UUID dell''asset radice del sottoalbero (si ottiene da GetRoots o da GetOne)', True);
  end
  else if aFunctionName = 'GetOne' then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddParam('uuid', aptString, 'UUID dell''asset', True);
  end
  else
    Result := False;     // nessuna descrizione precisa: l'AI usa la Description di Routes.xml
end;

constructor TCMMSHandlersAssets.Create(aDataAccess: IIdroWorldDataAccess);
begin
  inherited Create(cArea_Assets);
  FDataAccess := aDataAccess;
  Self.RegisterStandardWorker('ListKinds', @Self.DoListKinds);
  Self.RegisterStandardWorker('CreateKind', @Self.DoCreateKind);
  Self.RegisterStandardWorker('UpdateKind', @Self.DoUpdateKind);
  Self.RegisterStandardWorker('DeleteKind', @Self.DoDeleteKind);
  Self.RegisterStandardWorker('GetRoots', @Self.DoGetRoots);
  Self.RegisterStandardWorker('GetTree', @Self.DoGetTree);
  Self.RegisterStandardWorker('GetOne', @Self.DoGetOne);
  Self.RegisterStandardWorker('CreateAsset', @Self.DoCreateAsset);
  Self.RegisterStandardWorker('Update', @Self.DoUpdate);
  Self.RegisterStandardWorker('Delete', @Self.DoDelete);
  Self.RegisterStandardWorker('SetUsable', @Self.DoSetUsable);
  Self.RegisterStandardWorker('ModelsList', @Self.DoModelsList);
  Self.RegisterStandardWorker('ModelsCreate', @Self.DoModelsCreate);
  Self.RegisterStandardWorker('ModelsGetOne', @Self.DoModelsGetOne);
  Self.RegisterStandardWorker('ModelsUpdate', @Self.DoModelsUpdate);
  Self.RegisterStandardWorker('ModelsDelete', @Self.DoModelsDelete);
  Self.RegisterStandardWorker('ModelsListDocuments', @Self.DoModelsListDocuments);
end;
function TCMMSHandlersAssets.ResolveAssetModelId(aBody: TJSONObject): Int64;
var
  _ModelId   : Int64;
begin
  Result := 0;
  _ModelId := aBody.Get('model_id', Int64(0));
  if _ModelId <= 0 then Exit;
  Result := FDataAccess.ScalarInt(
    'SELECT id FROM cmms.asset_model WHERE id = :id',
    ['id', _ModelId], -1, cIdroWorldMainDB);
end;
{ -------------------------------------------------------------- ASSET KINDS }
function TCMMSHandlersAssets.DoListKinds(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _All : Boolean;
begin
  aStatusCode := HTTP_STATUS_OK;
  _All := aRequestData.Get('all', False);
  if _All then
    Result := FDataAccess.SelectToJSONArray(
      'SELECT id, code, description, sort_order, is_active, visual_category FROM cmms.asset_kind ' +
      'ORDER BY sort_order',
      [], cIdroWorldMainDB)
  else
    Result := FDataAccess.SelectToJSONArray(
      'SELECT id, code, description, sort_order, is_active, visual_category FROM cmms.asset_kind ' +
      'WHERE is_active = TRUE ORDER BY sort_order',
      [], cIdroWorldMainDB);
end;
function TCMMSHandlersAssets.DoCreateKind(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Code     : String;
  _Descr    : String;
  _Category : String;
  _Exists   : Int64;
  _NewId    : Integer;
  _Answer   : TJSONObject;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Code  := Trim(aRequestData.Get('code', ''));
    _Descr := Trim(aRequestData.Get('description', ''));
    if (_Code = '') or (_Descr = '') then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'code and description are required');
      Exit;
    end;
    _Category := Trim(aRequestData.Get('visual_category', ''));
    if _Category = '' then _Category := 'plant';
    _Exists := FDataAccess.ScalarInt('SELECT id FROM cmms.asset_kind WHERE code = :code', ['code', _Code], 0, cIdroWorldMainDB);
    if _Exists > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'A kind with this code already exists');
      Exit;
    end;
    _NewId := FDataAccess.InsertReturningId(
      'INSERT INTO cmms.asset_kind (code, description, sort_order, is_active, visual_category) ' +
      'VALUES (:code, :descr, :sort, :active, :category) RETURNING id',
      ['code', _Code, 'descr', _Descr, 'sort', aRequestData.Get('sort_order', Int64(0)),
       'active', aRequestData.Get('is_active', True), 'category', _Category],
      cIdroWorldMainDB);
    if _NewId > 0 then
    begin
      // una nuova tipologia di nodo nessun ruolo la vede ancora: i ruoli che "vedevano tutto" non lo sono piu'
      NotifyReferenceDataChanged;
      _Answer := TJSONObject.Create;
      _Answer.Add('successful', True);
      _Answer.Add('id', _NewId);
      aStatusCode := HTTP_STATUS_CREATED;
      Result := _Answer;
    end
    else begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_INTERNAL_ERROR, 'Kind creation failed');
    end;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoCreateKind', '', E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoUpdateKind(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Id       : Int64;
  _Code     : String;
  _Descr    : String;
  _Category : String;
  _Clash    : Int64;
  _Ok       : Boolean;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Id := aRequestData.Get('id', Int64(0));
    if _Id <= 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
      Exit;
    end;
    _Code  := Trim(aRequestData.Get('code', ''));
    _Descr := Trim(aRequestData.Get('description', ''));
    if (_Code = '') or (_Descr = '') then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'code and description are required');
      Exit;
    end;
    _Category := Trim(aRequestData.Get('visual_category', ''));
    if _Category = '' then _Category := 'plant';
    _Clash := FDataAccess.ScalarInt(
      'SELECT id FROM cmms.asset_kind WHERE code = :code AND id <> :id',
      ['code', _Code, 'id', _Id], 0, cIdroWorldMainDB);
    if _Clash > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Another kind already uses this code');
      Exit;
    end;
    _Ok := FDataAccess.ExecCommand(
      'UPDATE cmms.asset_kind SET code = :code, description = :descr, ' +
      '  sort_order = :sort, is_active = :active, visual_category = :category WHERE id = :id',
      ['code', _Code, 'descr', _Descr, 'sort', aRequestData.Get('sort_order', Int64(0)),
       'active', aRequestData.Get('is_active', True), 'category', _Category, 'id', _Id],
      cIdroWorldMainDB);
    if _Ok then NotifyReferenceDataChanged;
    Result := OkAnswer(_Ok, aStatusCode);
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoUpdateKind', '', E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoDeleteKind(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Id         : Int64;
  _Code       : String;
  _AssetCount : Int64;
  _CheckCount : Int64;
  _Ok         : Boolean;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Id := aRequestData.Get('id', Int64(0));
  if _Id <= 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
    Exit;
  end;
  _Code := FDataAccess.ScalarStr('SELECT code FROM cmms.asset_kind WHERE id = :id', ['id', _Id], '', cIdroWorldMainDB);
  if _Code = '' then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_NOT_FOUND, 'Kind not found');
    Exit;
  end;
  _AssetCount := FDataAccess.ScalarInt('SELECT count(*) FROM cmms.asset WHERE kind_id = :id', ['id', _Id], 0, cIdroWorldMainDB);
  if _AssetCount > 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Cannot delete: this kind is used by one or more assets');
    Exit;
  end;
  _CheckCount := FDataAccess.ScalarInt('SELECT count(*) FROM cmms.checklist_template WHERE applies_to_kind_id = :id', ['id', _Id], 0, cIdroWorldMainDB);
  if _CheckCount > 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Cannot delete: this kind is used by a checklist template');
    Exit;
  end;
  _Ok := FDataAccess.ExecCommand('DELETE FROM cmms.asset_kind WHERE id = :id', ['id', _Id], cIdroWorldMainDB);
  if _Ok then NotifyReferenceDataChanged;
  Result := OkAnswer(_Ok, aStatusCode);
end;
{ ------------------------------------------------------------------ ASSETS }
function TCMMSHandlersAssets.DoGetRoots(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
begin
  aStatusCode := HTTP_STATUS_OK;
  Result := FDataAccess.SelectToJSONArray(
    'SELECT a.uuid::text AS uuid, NULL::text AS parent_uuid, a.kind_id, ak.code AS kind_code, ' +
    '       ak.description AS kind_description, a.code, a.name, a.status, a.path::text AS path, ak.visual_category, ' +
    // produttore (quello dell'asset, altrimenti quello del modello) e modello: l'albero li mostra sotto il nome
    '       COALESCE(NULLIF(a.manufacturer, ''''), m.manufacturer) AS manufacturer, m.name AS model_name ' +
    'FROM cmms.asset a ' +
    'JOIN cmms.asset_kind ak ON ak.id = a.kind_id ' +
    'LEFT JOIN cmms.asset_model m ON m.id = a.model_id ' +
    'WHERE a.parent_id IS NULL ' +
    '  AND ' + AssetTreeVisibleSql(aUserConfig) + ' ' +
    'ORDER BY a.name',
    ['user_id', aUserId], cIdroWorldMainDB);
end;
function TCMMSHandlersAssets.DoGetTree(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Uuid : String;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Uuid := Trim(aRequestData.Get('uuid', ''));
  if (_Uuid = '') or (not IsUuidText(_Uuid)) then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'uuid is required (valid uuid)');
    Exit;
  end;
  Result := FDataAccess.SelectToJSONArray(
    'SELECT a.uuid::text AS uuid, pa.uuid::text AS parent_uuid, a.kind_id, ak.code AS kind_code, ' +
    '       ak.description AS kind_description, a.code, a.name, a.status, a.path::text AS path, ak.visual_category, ' +
    '       COALESCE(NULLIF(a.manufacturer, ''''), m.manufacturer) AS manufacturer, m.name AS model_name ' +
    'FROM cmms.asset a ' +
    'LEFT JOIN cmms.asset pa ON pa.id = a.parent_id ' +
    'JOIN cmms.asset_kind ak ON ak.id = a.kind_id ' +
    'LEFT JOIN cmms.asset_model m ON m.id = a.model_id ' +
    'WHERE a.path <@ (SELECT path FROM cmms.asset WHERE uuid = CAST(:uuid AS uuid)) ' +
    '  AND ' + AssetTreeVisibleSql(aUserConfig) + ' ' +
    'ORDER BY a.path',
    ['uuid', _Uuid, 'user_id', aUserId], cIdroWorldMainDB);
end;
function TCMMSHandlersAssets.DoGetOne(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Uuid : String;
  _Obj  : TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Uuid := Trim(aRequestData.Get('uuid', ''));
  if (_Uuid = '') or (not IsUuidText(_Uuid)) then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'uuid is required (valid uuid)');
    Exit;
  end;
  _Obj := FDataAccess.SelectRowToJSON(
    'SELECT a.uuid::text AS uuid, pa.uuid::text AS parent_uuid, a.kind_id, ak.code AS kind_code, ' +
    '       ak.description AS kind_description, a.code, a.name, a.description, a.manufacturer, ' +
    '       a.serial_number, a.status, a.attributes, a.path::text AS path, ' +
    '       a.model_id, m.code AS model_code, m.name AS model_name, ' +
    '       m.manufacturer AS model_manufacturer, ' +
    // centro di costo: quello proprio (cost_center_*) e quello EFFETTIVO, proprio o ereditato dal primo antenato che ne ha uno
    '       a.cost_center_id, own.code AS cost_center_code, own.description AS cost_center_description, ' +
    '       ev.cost_center_id AS effective_cost_center_id, ev.cost_center_code AS effective_cost_center_code, ' +
    '       ev.cost_center_description AS effective_cost_center_description, ev.is_inherited AS cost_center_is_inherited ' +
    'FROM cmms.asset a ' +
    'LEFT JOIN cmms.asset pa ON pa.id = a.parent_id ' +
    'JOIN cmms.asset_kind ak ON ak.id = a.kind_id ' +
    'LEFT JOIN cmms.asset_model m ON m.id = a.model_id ' +
    'LEFT JOIN cmms.cost_center own ON own.id = a.cost_center_id ' +
    'LEFT JOIN cmms.v_asset_cost_center ev ON ev.asset_id = a.id ' +
    'WHERE a.uuid = CAST(:uuid AS uuid) ' +
    '  AND cmms.user_can_view_asset_path(:user_id, a.path)',
    ['uuid', _Uuid, 'user_id', aUserId], cIdroWorldMainDB);
  if _Obj = nil then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_NOT_FOUND, 'Node not found');
    Exit;
  end;
  Result := _Obj;
end;
function TCMMSHandlersAssets.DoCreateAsset(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _ParentUuid : String;
  _ParentId   : Int64;
  _ModelId    : Int64;
  _KindId     : Int64;
  _CcId       : Int64;
  _CcPresent  : Boolean;
  _Code       : String;
  _NewId      : Integer;
  _NewRow     : TJSONObject;
  _Answer     : TJSONObject;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  _NewRow := nil;
  try
    _KindId := aRequestData.Get('kind_id', Int64(0));
    _Code := aRequestData.Get('code', '');
    _ModelId := Self.ResolveAssetModelId(aRequestData);
    _CcId := ReadCostCenterId(FDataAccess, aRequestData, _CcPresent);
    if _CcId < 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'cost_center_id does not identify an existing cost center');
      Exit;
    end;
    if (_KindId <= 0) or (FDataAccess.ScalarInt(
         'SELECT id FROM cmms.asset_kind WHERE id = :id AND is_active',
         ['id', _KindId], 0, cIdroWorldMainDB) <= 0) then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'kind_id is required and must identify an active asset kind');
      Exit;
    end;
    if _ModelId < 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'model_id does not identify an existing asset model');
      Exit;
    end;
    _ParentUuid := Trim(aRequestData.Get('parent_uuid', ''));
    _ParentId := 0;
    if _ParentUuid <> '' then
    begin
      if IsUuidText(_ParentUuid) then
        _ParentId := FDataAccess.ScalarInt(
          'SELECT id FROM cmms.asset WHERE uuid = CAST(:uuid AS uuid)', ['uuid', _ParentUuid], 0, cIdroWorldMainDB);
      if _ParentId <= 0 then
      begin
        LBLogger.Write(3, 'TCMMSHandlersAssets.DoCreateAsset', lmt_Warning,
          'parent_uuid %s does not match any existing asset', [_ParentUuid]);
        Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'parent_uuid does not match any existing asset');
        Exit;
      end;
    end;
    if _ParentId <= 0 then
      _NewId := FDataAccess.InsertReturningId(
        'INSERT INTO cmms.asset (parent_id, kind_id, code, name, description, model_id, cost_center_id) ' +
        'VALUES (NULL, :kind_id, :code, :name, :descr, NULLIF(:model,0), NULLIF(:cc,0)) RETURNING id',
        ['kind_id', _KindId, 'code', _Code,
         'name', aRequestData.Get('name', ''), 'descr', aRequestData.Get('description', ''),
         'model', _ModelId, 'cc', _CcId],
        cIdroWorldMainDB)
    else
      _NewId := FDataAccess.InsertReturningId(
        'INSERT INTO cmms.asset (parent_id, kind_id, code, name, description, model_id, cost_center_id) ' +
        'VALUES (:parent, :kind_id, :code, :name, :descr, NULLIF(:model,0), NULLIF(:cc,0)) RETURNING id',
        ['parent', _ParentId, 'kind_id', _KindId,
         'code', _Code, 'name', aRequestData.Get('name', ''), 'descr', aRequestData.Get('description', ''),
         'model', _ModelId, 'cc', _CcId],
        cIdroWorldMainDB);
    if _NewId > 0 then
    begin
      _NewRow := FDataAccess.SelectRowToJSON(
        'SELECT uuid::text AS uuid FROM cmms.asset WHERE id = :id', ['id', _NewId], cIdroWorldMainDB);
      _Answer := TJSONObject.Create;
      _Answer.Add('successful', True);
      if _NewRow <> nil then
        _Answer.Add('uuid', _NewRow.Get('uuid', ''));
      aStatusCode := HTTP_STATUS_CREATED;
      Result := _Answer;
    end
    else begin
      LBLogger.Write(1, 'TCMMSHandlersAssets.DoCreateAsset', lmt_Warning,
        'Insert returned no id. kind_id=%d code=%s parent_uuid=%s model_id=%d',
        [_KindId, _Code, _ParentUuid, _ModelId]);
      Result := FailAnswer(aStatusCode, HTTP_STATUS_INTERNAL_ERROR, 'Insert failed');
    end;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoCreateAsset', Format('kind_id=%d code=%s', [_KindId, _Code]), E, aStatusCode);
  end;
  if _NewRow <> nil then _NewRow.Free;
end;
function TCMMSHandlersAssets.DoUpdate(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Uuid    : String;
  _KindId  : Int64;
  _ModelId : Int64;
  _CcId    : Int64;
  _CcSet   : Boolean;
  _SetCc   : Int64;
  _Ok      : Boolean;
  _Answer  : TJSONObject;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Uuid := Trim(aRequestData.Get('uuid', ''));
    if (_Uuid = '') or (not IsUuidText(_Uuid)) then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'uuid is required (valid uuid)');
      Exit;
    end;
    _KindId := aRequestData.Get('kind_id', Int64(0));
    _ModelId := Self.ResolveAssetModelId(aRequestData);
    if (_KindId <= 0) or (FDataAccess.ScalarInt(
         'SELECT id FROM cmms.asset_kind WHERE id = :id', ['id', _KindId], 0, cIdroWorldMainDB) <= 0) then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'kind_id is required and must identify an asset kind');
      Exit;
    end;
    if _ModelId < 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'model_id does not identify an existing asset model');
      Exit;
    end;
    // centro di costo: se il campo manca resta quello che c'e' (chi non puo' vedere i centri di costo non lo invia)
    _CcId := ReadCostCenterId(FDataAccess, aRequestData, _CcSet);
    if _CcId < 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'cost_center_id does not identify an existing cost center');
      Exit;
    end;
    _SetCc := 0;
    if _CcSet then _SetCc := 1;
    _Ok := FDataAccess.ExecCommand(
      'UPDATE cmms.asset SET kind_id = :kind_id, code = :code, name = :name, description = :descr, ' +
      '  status = CAST(:status AS cmms.asset_status), model_id = NULLIF(:model,0), ' +
      '  cost_center_id = CASE WHEN :setcc = 1 THEN NULLIF(:cc,0) ELSE cost_center_id END, ' +
      '  updated_at = now() WHERE uuid = CAST(:uuid AS uuid)',
      ['kind_id', _KindId, 'code', aRequestData.Get('code', ''), 'name', aRequestData.Get('name', ''),
       'descr', aRequestData.Get('description', ''), 'status', aRequestData.Get('status', 'attivo'),
       'model', _ModelId, 'setcc', _SetCc, 'cc', _CcId, 'uuid', _Uuid],
      cIdroWorldMainDB);
    _Answer := TJSONObject.Create;
    _Answer.Add('successful', _Ok);
    if not _Ok then
    begin
      LBLogger.Write(1, 'TCMMSHandlersAssets.DoUpdate', lmt_Warning,
        'ExecCommand returned False for asset uuid=%s', [_Uuid]);
      aStatusCode := HTTP_STATUS_INTERNAL_ERROR;
    end;
    Result := _Answer;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoUpdate', Format('uuid=%s', [_Uuid]), E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoDelete(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Uuid   : String;
  _Ok     : Boolean;
  _Answer : TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Uuid := Trim(aRequestData.Get('uuid', ''));
  if (_Uuid = '') or (not IsUuidText(_Uuid)) then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'uuid is required (valid uuid)');
    Exit;
  end;
  try
    _Ok := FDataAccess.ExecCommand('DELETE FROM cmms.asset WHERE uuid = CAST(:uuid AS uuid)', ['uuid', _Uuid], cIdroWorldMainDB);
    _Answer := TJSONObject.Create;
    _Answer.Add('successful', _Ok);
    if not _Ok then
    begin
      LBLogger.Write(3, 'TCMMSHandlersAssets.DoDelete', lmt_Warning,
        'Delete returned False for asset uuid=%s (children or references present)', [_Uuid]);
      _Answer.Add('error', 'Cannot delete: the node may have children or references');
      aStatusCode := HTTP_STATUS_CONFLICT;
    end;
    Result := _Answer;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoDelete', Format('uuid=%s', [_Uuid]), E, aStatusCode);
  end;
end;
{ ------------------------------------------------------------ ASSET MODELS }
function TCMMSHandlersAssets.DoSetUsable(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
{
  Imposta A MANO se un asset e' utilizzabile (es. un mezzo fermato per un problema di sicurezza o
  meccanico). Corpo: "uuid", "usable" (true/false), "note" (facoltativa: il motivo). Vale solo per le
  tipologie di nodo che lo prevedono (asset_kind.tracks_usability): per le altre 409. L'asset deve
  essere visibile all'utente (404 altrimenti, come GetOne). La prossima verifica che richiede l'esito
  (scheda SVC) sovrascrive il valore.
}
var
  _Uuid   : String;
  _Usable : TJSONData;
  _Ok     : Boolean;
  _Obj    : TJSONObject;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Uuid := Trim(aRequestData.Get('uuid', ''));
    _Usable := aRequestData.Find('usable');
    if (_Uuid = '') or (_Usable = nil) or (_Usable.JSONType <> jtBoolean) then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'uuid and usable (true/false) are required');
      Exit;
    end;
    if FDataAccess.ScalarInt(
         'SELECT count(*) FROM cmms.asset WHERE uuid = CAST(:uuid AS uuid) ' +
         'AND cmms.user_can_view_asset_path(:user_id, path)',
         ['uuid', _Uuid, 'user_id', aUserId], 0, cIdroWorldMainDB) = 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_NOT_FOUND, 'Node not found');
      Exit;
    end;
    _Ok := FDataAccess.ExecCommand(
      'UPDATE cmms.asset a SET usable = :usable, usable_note = NULLIF(:note, ''''), updated_at = now() ' +
      'WHERE a.uuid = CAST(:uuid AS uuid) ' +
      'AND EXISTS (SELECT 1 FROM cmms.asset_kind k WHERE k.id = a.kind_id AND k.tracks_usability)',
      ['usable', _Usable.AsBoolean, 'note', Trim(aRequestData.Get('note', '')), 'uuid', _Uuid],
      cIdroWorldMainDB);
    if not _Ok then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_INTERNAL_ERROR, 'The asset could not be updated');
      Exit;
    end;
    // l'UPDATE non produce errore se la tipologia non prevede il flag: si rilegge il valore
    _Obj := FDataAccess.SelectRowToJSON(
      'SELECT usable, usable_note FROM cmms.asset WHERE uuid = CAST(:uuid AS uuid)',
      ['uuid', _Uuid], cIdroWorldMainDB);
    if (_Obj = nil) or (_Obj.Get('usable', True) <> _Usable.AsBoolean) then
    begin
      _Obj.Free;
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'This kind of asset does not track usability');
      Exit;
    end;
    _Obj.Add('successful', True);
    Result := _Obj;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoSetUsable', '', E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoModelsList(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
begin
  aStatusCode := HTTP_STATUS_OK;
  Result := FDataAccess.SelectToJSONArray(
    'SELECT id, code, name, manufacturer FROM cmms.asset_model ' +
    'WHERE is_active = TRUE ORDER BY name',
    [], cIdroWorldMainDB);
end;
function TCMMSHandlersAssets.DoModelsCreate(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Code     : String;
  _Name     : String;
  _Existing : Int64;
  _NewId    : Integer;
  _Answer   : TJSONObject;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Code := Trim(aRequestData.Get('code', ''));
    _Name := Trim(aRequestData.Get('name', ''));
    if (_Code = '') or (_Name = '') then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'code and name are required');
      Exit;
    end;
    _Existing := FDataAccess.ScalarInt('SELECT id FROM cmms.asset_model WHERE code = :code', ['code', _Code], 0, cIdroWorldMainDB);
    if _Existing > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'A model with this code already exists');
      Exit;
    end;
    _NewId := FDataAccess.InsertReturningId(
      'INSERT INTO cmms.asset_model (code, name, manufacturer, description) ' +
      'VALUES (:code, :name, :manufacturer, :descr) RETURNING id',
      ['code', _Code, 'name', _Name,
       'manufacturer', aRequestData.Get('manufacturer', ''),
       'descr', aRequestData.Get('description', '')],
      cIdroWorldMainDB);
    if _NewId > 0 then
    begin
      _Answer := TJSONObject.Create;
      _Answer.Add('successful', True);
      _Answer.Add('id', _NewId);
      _Answer.Add('code', _Code);
      _Answer.Add('name', _Name);
      aStatusCode := HTTP_STATUS_CREATED;
      Result := _Answer;
    end
    else begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_INTERNAL_ERROR, 'Model creation failed');
    end;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoModelsCreate', Format('code=%s name=%s', [_Code, _Name]), E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoModelsGetOne(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Id  : Int64;
  _Obj : TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Id := aRequestData.Get('id', Int64(0));
  if _Id <= 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
    Exit;
  end;
  _Obj := FDataAccess.SelectRowToJSON(
    'SELECT id, code, name, manufacturer, description, is_active, created_at ' +
    'FROM cmms.asset_model WHERE id = :id',
    ['id', _Id], cIdroWorldMainDB);
  if _Obj = nil then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_NOT_FOUND, 'Model not found');
    Exit;
  end;
  Result := _Obj;
end;
function TCMMSHandlersAssets.DoModelsUpdate(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Id     : Int64;
  _Code   : String;
  _Name   : String;
  _Clash  : Int64;
  _Ok     : Boolean;
begin
  Result := nil;
  aStatusCode := HTTP_STATUS_OK;
  try
    _Id := aRequestData.Get('id', Int64(0));
    if _Id <= 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
      Exit;
    end;
    _Code := Trim(aRequestData.Get('code', ''));
    _Name := Trim(aRequestData.Get('name', ''));
    if (_Code = '') or (_Name = '') then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'code and name are required');
      Exit;
    end;
    _Clash := FDataAccess.ScalarInt(
      'SELECT id FROM cmms.asset_model WHERE code = :code AND id <> :id',
      ['code', _Code, 'id', _Id], 0, cIdroWorldMainDB);
    if _Clash > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Another model already uses this code');
      Exit;
    end;
    _Ok := FDataAccess.ExecCommand(
      'UPDATE cmms.asset_model SET code = :code, name = :name, ' +
      '  manufacturer = :manufacturer, description = :descr WHERE id = :id',
      ['code', _Code, 'name', _Name,
       'manufacturer', aRequestData.Get('manufacturer', ''),
       'descr', aRequestData.Get('description', ''),
       'id', _Id],
      cIdroWorldMainDB);
    Result := OkAnswer(_Ok, aStatusCode);
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoModelsUpdate', Format('id=%d', [_Id]), E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoModelsDelete(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var
  _Id         : Int64;
  _DocCount   : Int64;
  _AssetCount : Int64;
  _Ok         : Boolean;
  _Answer     : TJSONObject;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Id := aRequestData.Get('id', Int64(0));
  if _Id <= 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
    Exit;
  end;
  try
    _DocCount := FDataAccess.ScalarInt('SELECT count(*) FROM cmms.document WHERE model_id = :id', ['id', _Id], 0, cIdroWorldMainDB);
    if _DocCount > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Cannot delete: the model has linked documents - remove them first');
      Exit;
    end;
    _AssetCount := FDataAccess.ScalarInt('SELECT count(*) FROM cmms.asset WHERE model_id = :id', ['id', _Id], 0, cIdroWorldMainDB);
    if _AssetCount > 0 then
    begin
      Result := FailAnswer(aStatusCode, HTTP_STATUS_CONFLICT, 'Cannot delete: the model is attached to one or more plant nodes - detach it first');
      Exit;
    end;
    _Ok := FDataAccess.ExecCommand('DELETE FROM cmms.asset_model WHERE id = :id', ['id', _Id], cIdroWorldMainDB);
    _Answer := TJSONObject.Create;
    _Answer.Add('successful', _Ok);
    if not _Ok then
    begin
      LBLogger.Write(1, 'TCMMSHandlersAssets.DoModelsDelete', lmt_Warning,
        'Delete returned False for model id=%d', [_Id]);
      _Answer.Add('error', 'Deletion failed');
      aStatusCode := HTTP_STATUS_CONFLICT;
    end;
    Result := _Answer;
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoModelsDelete', Format('id=%d', [_Id]), E, aStatusCode);
  end;
end;
function TCMMSHandlersAssets.DoModelsListDocuments(aUserId: Integer; aUserConfig: TJSONObject;
  aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
{
  document e' identificato ESTERNAMENTE per UUID: la query espone ora
  document.uuid (mai document.id), cosi' che il client possa operare
  direttamente su ciascun documento elencato con lo stesso identificatore
  usato ovunque altrove nel sistema per un documento - si veda
  uCMMS_HandlersDocuments.pas.
}
var
  _Id : Int64;
begin
  aStatusCode := HTTP_STATUS_OK;
  _Id := aRequestData.Get('id', Int64(0));
  if _Id <= 0 then
  begin
    Result := FailAnswer(aStatusCode, HTTP_STATUS_BAD_REQUEST, 'id is required');
    Exit;
  end;
  try
    Result := FDataAccess.SelectToJSONArray(
      'SELECT uuid::text AS uuid, kind, title, mime_type, file_size_bytes, ' +
      '       processing_status, ts_config::text AS lang, created_at ' +
      'FROM cmms.document WHERE model_id = :id ORDER BY created_at DESC',
      ['id', _Id], cIdroWorldMainDB);
  except
    on E: Exception do
      Result := InternalErrorAnswer('TCMMSHandlersAssets.DoModelsListDocuments', Format('model_id=%d', [_Id]), E, aStatusCode);
  end;
end;
end.
