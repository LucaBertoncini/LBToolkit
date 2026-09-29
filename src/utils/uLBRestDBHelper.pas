unit uLBRestDBHelper;

{
  Strato di accesso al database per i controller REST di LBmicroWebServer.

  Avvolge il TSQLConnectionManager esistente (uLBDBConnectionManager) esponendo
  ai chain processor REST i mattoni ergonomici piu' usati:
    - Transaction()          : esegue un lavoro ARBITRARIO (piu' query in
                               sequenza, con logica in mezzo) dentro una
                               transazione con Commit/Rollback automatico.
                               Il lavoro deve essere un vero METODO di una
                               classe (TRestDBWork e' "of object"): una
                               funzione annidata (dichiarata al volo dentro
                               un altro metodo) NON e' compatibile con
                               questo tipo e non compilerebbe - vedi il
                               commento esteso su TRestDBWork piu' sotto.
    - ExecCommand()          : INSERT/UPDATE/DELETE in transazione, senza
                               risultato. NON passa piu' per Transaction:
                               essendo sempre "una sola SQL con i suoi
                               parametri", non ha bisogno di alcuna
                               callback - gestisce la propria transazione
                               per esteso, con lo stesso schema gia' usato
                               da ScalarInt/ScalarStr/SelectToJSONArray/
                               SelectRowToJSON qui sotto (nessun codice
                               nuovo, solo lo stesso schema applicato una
                               volta in piu').
    - InsertReturningId()    : INSERT che restituisce l'id generato
                               (PostgreSQL: la SQL deve terminare con
                               RETURNING <id>). Fallimento: -1.
    - ScalarInt / ScalarStr  : lettura di un singolo valore scalare.
    - SelectToJSONArray()    : recordset -> TJSONArray (una TJSONObject per riga).
    - SelectRowToJSON()      : prima riga -> TJSONObject (nil se vuoto).

  Regole rispettate rispetto a uLBDBConnectionManager:
    * pooling per-thread: usare l'helper SOLO dal thread della richiesta.
    * ReleaseQuery SEMPRE garantito (IPooledQuery / try-finally).
    * transazioni esplicite (Options - [sqoAutoCommit]).

  Binding parametri: PER NOME. Nella SQL segnaposto :nome; si passano coppie
  (nome, valore) in array of const. Un Pointer nil -> NULL.

  CICLO DI VITA DEL CONNECTION MANAGER (importante)
  ------------------------------------------------------------------
  TLBRestDB referenzia il TSQLConnectionManager come OGGETTO concreto
  (FConnManager : TSQLConnectionManager), NON come interfaccia
  ISQLConnectionManager. Questa e' una scelta deliberata, non un dettaglio:

  TSQLConnectionManager e' un TInterfacedObject (reference-counted). Se lo si
  tenesse tramite interfaccia, il conteggio di riferimenti si attiverebbe, e
  poiche' il manager e' posseduto e distrutto altrove con FreeAndNil (dal
  codice che lo crea, es. TCMMS_WS_Application), si mescolerebbero i due
  modelli di ciclo di vita (refcount + Free manuale) - il footgun classico
  che porta a double-free / use-after-free.

  Tenendolo come oggetto:
    * TLBRestDB NON incrementa mai il refcount del manager;
    * TLBRestDB NON possiede il manager e NON lo distrugge (nessun Free nel
      proprio distruttore): il manager e' creato e distrutto da chi lo
      inietta;
    * il guard restituito da Acquire (IPooledQuery) e' generato dal manager
      stesso e tiene a sua volta il manager come oggetto (vedi
      TPooledQueryGuard in uLBDBConnectionManager), quindi nemmeno i guard
      transitori toccano il refcount.

  L'interfaccia ISQLConnectionManager continua a esistere e resta utile per
  altri scenari (es. esportazione del manager in una DLL consumata da C#),
  dove il ciclo di vita e' gestito interamente a interfaccia: quei contesti
  sono separati da questo e non vanno mescolati con un uso a oggetto+Free.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, fpjson,
  uLBDBConnectionManager, ULBLogger;

type

  { TLBRestDB }

  TLBRestDB = class(TObject)
    strict private
      FConnManager : TSQLConnectionManager;   // oggetto, NON interfaccia - vedi commento in testa alla unit
      FDefaultDB   : String;
      FTimeout     : Integer;

      procedure BindNamed(aQuery: TSQLQueryEx; const aParams: array of const);
      procedure AssignParamValue(aParam: TParam; const aValue: TVarRec; const aName: String);

      function  FieldToJSON(aField: TField): TJSONData;
      function  RowToJSON(aQuery: TSQLQueryEx): TJSONObject;

    public
      type
        { TRestDBWork - "of object": DEVE essere un metodo di una classe
          (assegnabile come @IstanzaQualunque.NomeMetodo). Una funzione
          annidata (dichiarata dentro un altro metodo, senza appartenere a
          nessuna classe) non e' assegnabile a un tipo "of object": e'
          un'incompatibilita' di rappresentazione a livello di compilatore,
          non una questione di stile. Chi ha bisogno di eseguire piu' query
          in sequenza dentro un'unica transazione (con logica in mezzo, non
          "una sola SQL con parametri") deve scrivere quella logica come
          metodo di un piccolo oggetto dedicato, e passarne l'indirizzo qui.
          Vedi ExecCommand piu' sotto per il caso, molto piu' comune, in cui
          questa callback NON serve affatto. }
        TRestDBWork = function(aQuery: TSQLQueryEx): Boolean of object;

      // aConnManager NON e' posseduto da TLBRestDB: e' creato e distrutto da
      // chi lo inietta. TLBRestDB lo tiene come oggetto (nessun refcount) e
      // non lo libera mai. Vedi il commento in testa alla unit.
      constructor Create(aConnManager: TSQLConnectionManager; const aDefaultDB: String = '');

      { Ritorna un guard a interfaccia che rilascia automaticamente la query
        quando esce di scope. Il guard e' generato dal connection manager
        (RetrieveQueryInterface): tutta la meccanica di release vive li'. }
      function Acquire(const aDBName: String = ''): IPooledQuery;

      { Esegue aWork (un vero metodo di un vero oggetto, vedi il commento su
        TRestDBWork) dentro una transazione. Commit se aWork ritorna True,
        Rollback altrimenti (o in caso di eccezione, loggata). Usare questo
        SOLO quando serve eseguire piu' di una query in sequenza con logica
        di per se' arbitraria: per una singola SQL con i suoi parametri,
        usare ExecCommand/InsertReturningId/Scalar*/Select*, che non
        richiedono alcuna callback. }
      function Transaction(aWork: TRestDBWork; const aDBName: String = ''): Boolean;

      { Comando senza recordset (INSERT/UPDATE/DELETE) in transazione singola.
        Gestisce la propria transazione per esteso (stesso schema di
        ScalarInt/SelectToJSONArray qui sotto): non ha bisogno di passare per
        Transaction, perche' il "lavoro da fare" e' sempre lo stesso identico
        pattern (SQL.Text := aSQL; BindNamed; ExecSQL) e non varia da
        chiamata a chiamata - non c'e' nulla di arbitrario da incapsulare in
        una callback. }
      function ExecCommand(const aSQL: String; const aParams: array of const; const aDBName: String = ''): Boolean;

      { INSERT con id generato (PostgreSQL): la SQL DEVE terminare con
        RETURNING <colonna_id>. Ritorna l'id, oppure -1 in caso di errore. }
      function InsertReturningId(const aSQL: String; const aParams: array of const; const aDBName: String = ''): Integer;

      { Letture }
      function ScalarInt(const aSQL: String; const aParams: array of const; aDefault: Int64 = 0; const aDBName: String = ''): Int64;
      function ScalarStr(const aSQL: String; const aParams: array of const; const aDefault: String = ''; const aDBName: String = ''): String;

      { Serializzazione diretta in JSON. Il chiamante possiede il risultato. }
      function SelectToJSONArray(const aSQL: String; const aParams: array of const; const aDBName: String = ''): TJSONArray;
      function SelectRowToJSON(const aSQL: String; const aParams: array of const; const aDBName: String = ''): TJSONObject;

      property DefaultDB : String  read FDefaultDB write FDefaultDB;
      property Timeout   : Integer read FTimeout   write FTimeout;
  end;


implementation

{ TLBRestDB }

constructor TLBRestDB.Create(aConnManager: TSQLConnectionManager; const aDefaultDB: String);
begin
  inherited Create;

  FConnManager := aConnManager;   // riferimento a oggetto: nessun refcount, nessuna ownership
  FDefaultDB   := aDefaultDB;
  FTimeout     := cAcquireTimeout;
end;

procedure TLBRestDB.AssignParamValue(aParam: TParam; const aValue: TVarRec; const aName: String);
begin
  case aValue.VType of
    vtInteger    : aParam.AsInteger  := aValue.VInteger;
    vtInt64      : aParam.AsLargeInt := aValue.VInt64^;
    vtQWord      : aParam.AsLargeInt := Int64(aValue.VQWord^);
    vtBoolean    : aParam.AsBoolean  := aValue.VBoolean;
    vtExtended   : aParam.AsFloat    := aValue.VExtended^;
    vtCurrency   : aParam.AsCurrency := aValue.VCurrency^;
    vtChar       : aParam.AsString   := aValue.VChar;
    vtString     : aParam.AsString   := aValue.VString^;
    vtPChar      : aParam.AsString   := StrPas(aValue.VPChar);
    vtAnsiString : aParam.AsString   := AnsiString(aValue.VAnsiString);

    vtPointer:
      begin
        // Un Pointer nil viene interpretato come NULL SQL
        if aValue.VPointer = nil then
          aParam.Clear
        else
          LBLogger.Write(1, 'TLBRestDB.AssignParamValue', lmt_Warning, 'Pointer non-nil non supportato per <%s>', [aName]);
      end;

    else
      aParam.Clear;
  end;
end;

procedure TLBRestDB.BindNamed(aQuery: TSQLQueryEx; const aParams: array of const);
var
  i     : Integer;
  _Name : String;
  _Par  : TParam;

begin
  if Length(aParams) = 0 then
    Exit;

  // aParams e' una sequenza di coppie (nome, valore): lunghezza pari attesa.
  if (Length(aParams) mod 2) <> 0 then
  begin
    LBLogger.Write(1, 'TLBRestDB.BindNamed', lmt_Error,
                   'Numero dispari di elementi (%d): attese coppie nome/valore', [Length(aParams)]);
    Exit;
  end;

  i := 0;
  while i < Length(aParams) do
  begin
    // elemento di indice pari: il nome del parametro (stringa)
    case aParams[i].VType of
      vtString     : _Name := aParams[i].VString^;
      vtAnsiString : _Name := AnsiString(aParams[i].VAnsiString);
      vtPChar      : _Name := StrPas(aParams[i].VPChar);
      vtChar       : _Name := aParams[i].VChar;
      else
        begin
          LBLogger.Write(1, 'TLBRestDB.BindNamed', lmt_Error, 'Nome parametro non valido alla posizione %d', [i]);
          Exit;
        end;
    end;

    _Par := aQuery.Params.FindParam(_Name);
    if _Par <> nil then
      Self.AssignParamValue(_Par, aParams[i + 1], _Name)
    else
      LBLogger.Write(1, 'TLBRestDB.BindNamed', lmt_Warning, 'Parametro <%s> non presente nella SQL', [_Name]);

    Inc(i, 2);
  end;
end;

function TLBRestDB.FieldToJSON(aField: TField): TJSONData;
begin
  if aField.IsNull then
    Exit(TJSONNull.Create);

  case aField.DataType of
    ftSmallint, ftInteger, ftWord, ftAutoInc:
      Result := TJSONIntegerNumber.Create(aField.AsInteger);

    ftLargeint:
      Result := TJSONInt64Number.Create(aField.AsLargeInt);

    ftFloat, ftBCD, ftFMTBcd, ftCurrency:
      Result := TJSONFloatNumber.Create(aField.AsFloat);

    ftBoolean:
      Result := TJSONBoolean.Create(aField.AsBoolean);

    ftDate, ftTime, ftDateTime, ftTimeStamp:
      // ISO-8601, cosi' il client JS lo puo' parsare direttamente
      Result := TJSONString.Create(FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', aField.AsDateTime));

    ftBlob, ftGraphic, ftBytes, ftVarBytes:
      // I BLOB non vengono serializzati inline: si espone solo la lunghezza.
      Result := TJSONString.Create(Format('<blob:%d bytes>', [Length(aField.AsBytes)]));

    else
      Result := TJSONString.Create(aField.AsString);
  end;
end;

function TLBRestDB.RowToJSON(aQuery: TSQLQueryEx): TJSONObject;
var
  i : Integer;

begin
  Result := TJSONObject.Create;
  try
    for i := 0 to aQuery.Fields.Count - 1 do
      Result.Add(aQuery.Fields[i].FieldName, Self.FieldToJSON(aQuery.Fields[i]));
  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TLBRestDB.RowToJSON', lmt_Error, E.Message);
      FreeAndNil(Result);
    end;
  end;
end;

function TLBRestDB.Acquire(const aDBName: String): IPooledQuery;
var
  _DB : String;

begin
  if aDBName <> '' then
    _DB := aDBName
  else
    _DB := FDefaultDB;

  // Tutta la meccanica di acquisizione+release vive ora nel connection
  // manager: RetrieveQueryInterface restituisce gia' un guard che rilascia
  // la query quando esce di scope. TLBRestDB si limita a risolvere il
  // database di default e a delegare.
  Result := FConnManager.RetrieveQueryInterface(_DB, FTimeout);
  if Result = nil then
    LBLogger.Write(1, 'TLBRestDB.Acquire', lmt_Warning, 'Query non ottenuta per il DB <%s>', [_DB]);
end;

function TLBRestDB.Transaction(aWork: TRestDBWork; const aDBName: String): Boolean;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := False;

  _Guard := Self.Acquire(aDBName);   // il rilascio e' garantito quando _Guard esce di scope
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      Result := aWork(_Q);

      if Result then
        _Q.Commit()
      else
        _Q.Rollback();

    except
      on E: Exception do
      begin
        Result := False;
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.Transaction', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;   // esplicito: forza il rilascio della connessione
  end;
end;

function TLBRestDB.ExecCommand(const aSQL: String; const aParams: array of const; const aDBName: String): Boolean;
{
  NON passa per Transaction: non essendoci qui alcuna logica arbitraria da
  incapsulare (e' sempre "una SQL, i suoi parametri, via"), il metodo
  gestisce la propria transazione per esteso - lo stesso schema di
  ScalarInt/ScalarStr/SelectToJSONArray/SelectRowToJSON.
}
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := False;

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);
      _Q.ExecSQL();
      _Q.Commit();
      Result := True;
    except
      on E: Exception do
      begin
        Result := False;
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.ExecCommand', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

function TLBRestDB.InsertReturningId(const aSQL: String; const aParams: array of const; const aDBName: String): Integer;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := -1;

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);

      // ExecuteInsert() del tuo TSQLQueryEx su PostgreSQL fa Open e legge Fields[0]:
      // la SQL deve terminare con RETURNING <id>.
      Result := _Q.ExecuteInsert();

      _Q.Commit();
    except
      on E: Exception do
      begin
        Result := -1;
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.InsertReturningId', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

function TLBRestDB.ScalarInt(const aSQL: String; const aParams: array of const; aDefault: Int64; const aDBName: String): Int64;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := aDefault;

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);
      _Q.Open();

      if (not _Q.IsEmpty) and (not _Q.Fields[0].IsNull) then
        Result := _Q.Fields[0].AsLargeInt;

      _Q.Close();
      _Q.Commit();
    except
      on E: Exception do
      begin
        Result := aDefault;
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.ScalarInt', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

function TLBRestDB.ScalarStr(const aSQL: String; const aParams: array of const; const aDefault: String; const aDBName: String): String;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := aDefault;

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);
      _Q.Open();

      if (not _Q.IsEmpty) and (not _Q.Fields[0].IsNull) then
        Result := _Q.Fields[0].AsString;

      _Q.Close();
      _Q.Commit();
    except
      on E: Exception do
      begin
        Result := aDefault;
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.ScalarStr', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

function TLBRestDB.SelectToJSONArray(const aSQL: String; const aParams: array of const; const aDBName: String): TJSONArray;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;
  _Row   : TJSONObject;

begin
  Result := TJSONArray.Create;   // il chiamante possiede sempre un array valido (eventualmente vuoto)

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);
      _Q.Open();

      while not _Q.EOF do
      begin
        _Row := Self.RowToJSON(_Q);
        if _Row <> nil then
          Result.Add(_Row);
        _Q.Next();
      end;

      _Q.Close();
      _Q.Commit();
    except
      on E: Exception do
      begin
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.SelectToJSONArray', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

function TLBRestDB.SelectRowToJSON(const aSQL: String; const aParams: array of const; const aDBName: String): TJSONObject;
var
  _Guard : IPooledQuery;
  _Q     : TSQLQueryEx;

begin
  Result := nil;   // nil = nessuna riga trovata

  _Guard := Self.Acquire(aDBName);
  if _Guard = nil then
    Exit;

  _Q := _Guard.Query;

  try
    _Q.StartTransaction();
    try
      _Q.SQL.Text := aSQL;
      Self.BindNamed(_Q, aParams);
      _Q.Open();

      if not _Q.IsEmpty then
        Result := Self.RowToJSON(_Q);

      _Q.Close();
      _Q.Commit();
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        _Q.Rollback();
        LBLogger.Write(1, 'TLBRestDB.SelectRowToJSON', lmt_Error, E.Message);
      end;
    end;
  finally
    _Guard := nil;
  end;
end;

end.
