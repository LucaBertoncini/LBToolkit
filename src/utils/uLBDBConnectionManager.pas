unit uLBDBConnectionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, ULBLogger, Laz2_DOM, uTimedoutCriticalSection, fgl;

type
  TDBType = (dbt_Unknown   = 0,
             dbt_ODBC      = 1,
             dbt_Posgresql = 2,
             dbt_MySQL51   = 3,
             dbt_MySQL55   = 4,
             dbt_MySQL56   = 5,
             dbt_MySQL57   = 6,
             dbt_MySQL80   = 7);

  TDBNameToTypeMap = specialize TFPGMap <String, TDBType>;



  TNotifyNewDBEvent = procedure (AConnection: TSQLConnection; DBType: TDBType) of Object;

  { TSQLConnectionHelper }

  TSQLConnectionHelper = class(TObject)
    strict private
      FOnDestroying    : TNotifyEvent;
      FOwnerThread     : TThreadID;
      FReferencesCount : Integer;
      FDestroyCount    : Integer;
      FConnection      : TSQLConnection;

      procedure DoDestroying();

    private
      property Connection: TSQLConnection read FConnection write FConnection;

    public
      constructor Create;
      destructor Destroy; override;

      function IncReference(): Integer;
      function DecReference(): Integer;

      property ReferencesCount : Integer      read FReferencesCount;
      property OwnerThread     : TThreadID    read FOwnerThread write FOwnerThread;
      property OnDestroying    : TNotifyEvent                   write FOnDestroying;
  end;

  { TSQLQueryEx }

  TSQLQueryEx = class(TSQLQuery)
    strict private
      FDBName : String;
      FDBType : TDBType;
      FSchemaName : String;

    public
      constructor Create(AOwner : TComponent); override;

      function ExecuteInsert(): Integer;

      procedure StartTransaction(); reintroduce;
      procedure Commit();
      procedure Rollback();

      property DBName        : String    read FDBName      write FDBName;
      property SchemaName    : String    read FSchemaName  write FSchemaName;
      property DBType        : TDBType   read FDBType      write FDBType;
  end;

  { TSQLDBInfo }

  TSQLDBInfo = class(TObject)
    strict private
      FOnNewDatabase : TNotifyNewDBEvent;
      FOnAccessViolation : TNotifyEvent;

      FCSConnection  : TTimedOutCriticalSection;

      FHost                : String;
      FPort                : Word;
      FUser                : String;
      FPassWord            : String;
      FDBName              : String;
      FDBType              : TDBType;
      FODBC                : String;
      FSchemaName          : String;
      FDBConnectionTimeout : Integer;

      FActiveConnections : TStringList;

      function get_DataComplete: Boolean;
      function getConnection(): TSQLConnectionHelper;
      function CloseConnection(aConnection: TSQLConnection): Boolean;

      procedure DoNewDatabase(AConnection: TSQLConnection);
      procedure DoAccessViolation();

      procedure RemoveConnectionFromList(Sender: TObject);


      const
        cConnectionTimeout          = Integer(10000);
        cConnectionListTimeout      = Integer(5000);
        cDestroyTimeout             = Integer(60000);
        cDefaultDBConnectionTimeout = Integer(3);

    public
      constructor Create;
      destructor Destroy; override;

      function LoadConfiguration(ANode: TDOMNode): Boolean;

      function RetrieveQuery(): TSQLQueryEx;
      function ReleaseQuery(var AQuery: TSQLQueryEx): Boolean;

      property Host       : String  read FHost       write FHost;
      property Port       : Word    read FPort       write FPort;
      property User       : String  read FUser       write FUser;
      property PassWord   : String  read FPassWord   write FPassWord;
      property DBName     : String  read FDBName     write FDBName;
      property DBType     : TDBType read FDBType     write FDBType;
      property ODBC       : String  read FODBC       write FODBC;
      property SchemaName : String  read FSchemaName write FSchemaName;

      property DataComplete: Boolean read get_DataComplete;

      property OnNewDatabase : TNotifyNewDBEvent read FOnNewDatabase write FOnNewDatabase;
      property OnAccessViolation : TNotifyEvent write FOnAccessViolation;
  end;


const
  cAcquireTimeout = Integer(10000);

type
  ISQLConnectionManager = interface(IUnknown)
    ['{365813E9-D252-49F3-86E6-5B13F6BBDD26}']

    function RetrieveQuery(DBName: String = ''; aTimeout: Integer = cAcquireTimeout): TSQLQueryEx;
    function ReleaseQuery(var AQuery: TSQLQueryEx): Boolean;
  end;

  { TSQLConnectionManager }

  TSQLConnectionManager = class(TInterfacedObject, ISQLConnectionManager)
    strict private
      FOnAccessViolation : TNotifyEvent;
      FPostgresqlInitialized : Boolean;
      FCScm : TTimedOutCriticalSection;
      FOnNewDatabase : TNotifyNewDBEvent;
      FDatabaseInfo : TStringList;
      function get_DBInfo(DBName: String): TSQLDBInfo;
      function get_Schema(const DBName: String): String;
      procedure set_OnAccessViolation(AValue: TNotifyEvent);
      procedure set_OnNewDatabase(AValue: TNotifyNewDBEvent);

      const
        cReleaseTimeout     = Integer(60000);
        cDestroingDatabases = Integer(60000);

    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear();

      function LoadConfiguration(const AnXMLFile: String): Boolean;
      function LoadConfigurationFromXML(ParentNode: TDOMNode): boolean;

      function AddConnectionData(AnAlias: String; DBInfo: TSQLDBInfo): Boolean;


      function RetrieveQuery(DBName: String = ''; aTimeout: Integer = cAcquireTimeout): TSQLQueryEx;
      function ReleaseQuery(var AQuery: TSQLQueryEx): Boolean;

      function DataCompleted(DBName: String = ''): Boolean;

      function getDatabaseType(DBName: String): TDBType;

      property Databases: TStringList read FDatabaseInfo;
      property Schema[DBName: String]: String read get_Schema;
      property SQLDBInfo[DBName: String]: TSQLDBInfo read get_DBInfo;

      property OnNewDatabase: TNotifyNewDBEvent read FOnNewDatabase write set_OnNewDatabase;
      property OnAccessViolation: TNotifyEvent write set_OnAccessViolation;

      const
        cDBConnections = DOMString('DBConnections');
  end;

  function DBNameToDBType(const aName: String): TDBType;

  function getMySQLLastInsertID(AQuery: TSQLQuery): Integer;

implementation

uses
  db, mysql51conn, mysql55conn, mysql56conn, mysql57conn, mysql80conn, odbcconn, pqconnection, postgres3dyn, laz2_XMLRead;

var
  gv_CS : TTimedOutCriticalSection = nil;
  gv_DBNameToTypeMap: TDBNameToTypeMap = nil;

function DBNameToDBType(const aName: String): TDBType;
var
  _Idx : Integer;

begin
  Result := dbt_Unknown;

  if (gv_DBNameToTypeMap <> nil) then
  begin
    _Idx := gv_DBNameToTypeMap.IndexOf(aName);
    if _Idx > -1 then
      Result := gv_DBNameToTypeMap.Data[_Idx]
    else
      LBLogger.Write(1, 'DBNameToDBType', lmt_Warning, 'DB type <%s> not found!', [aName]);

  end
  else
    LBLogger.Write(1, 'DBNameToDBType', lmt_Warning, 'No map available!');

end;

function getMySQLLastInsertID(AQuery: TSQLQuery): Integer;
begin
  Result := 0;
  if AQuery <> nil then
  begin
    AQuery.SQL.Text := 'SELECT LAST_INSERT_ID();';
    AQuery.Open();
    if not (AQuery.BOF and AQuery.EOF) then
      Result := AQuery.Fields[0].AsInteger;
    AQuery.Close();
  end;
end;

{ TSQLConnectionHelper }

procedure TSQLConnectionHelper.DoDestroying();
begin
  try

    if Assigned(FOnDestroying) then
      FOnDestroying(Self);

  except
    on E: Exception do
      LBLogger.Write(1, 'TSQLConnectionEX.DoDestroying', lmt_Error, PChar(E.Message));
  end;
end;

constructor TSQLConnectionHelper.Create();
begin
  inherited Create();

  FConnection      := nil;
  FReferencesCount := 0;
  FOwnerThread     := 0;
  FDestroyCount    := 0;
end;

destructor TSQLConnectionHelper.Destroy;
var
  _Transaction : TSQLTransaction;

begin
  Self.DoDestroying();

  try

    if FConnection <> nil then
    begin
      _Transaction := FConnection.Transaction;
      if _Transaction <> nil then
        _Transaction.Active := False;

      FConnection.Connected := False;
      FreeAndNil(FConnection);

      if _Transaction <> nil then
        _Transaction.Free;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TSQLConnectionEX.Destroy', lmt_Error, PChar(E.Message));
  end;

  inherited Destroy;
end;

function TSQLConnectionHelper.IncReference(): Integer;
begin
  Result := InterLockedIncrement(FReferencesCount);
end;

function TSQLConnectionHelper.DecReference(): Integer;
begin
  Result := InterLockedDecrement(FReferencesCount);
end;

{ TSQLQueryEx }

constructor TSQLQueryEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDBName := '';
  FSchemaName := '';
  FDBType := dbt_Unknown;
end;

function TSQLQueryEx.ExecuteInsert(): Integer;
begin
  Result := 0;

  if FDBType <> dbt_Posgresql then
  begin
    Self.ExecSQL;
    Self.Params.Clear;

    if FDBType IN [dbt_MySQL51, dbt_MySQL55, dbt_MySQL56, dbt_MySQL57, dbt_MySQL80] then
      Self.SQL.Text := 'SELECT LAST_INSERT_ID();';
  end;

  Self.Open;
  if not Self.IsEmpty then
    Result := Self.Fields[0].AsInteger;
  Self.Close;
end;


procedure TSQLQueryEx.StartTransaction();
begin
  if Self.Transaction <> nil then
  begin
    if not Self.Transaction.Active then
      TSQLTransaction(Self.Transaction).StartTransaction;
  end
  else
    LBLogger.Write(1, 'TSQLQueryEx.StartTransaction', lmt_Warning, 'Transaction not set!', []);
end;

procedure TSQLQueryEx.Commit();
begin
  if Self.Transaction <> nil then
  begin
    if Self.Transaction.Active then
    begin
      TSQLTransaction(Self.Transaction).Commit;

      Self.Transaction.Active := False;
    end;
  end
  else
    LBLogger.Write(1, 'TSQLQueryEx.Commit', lmt_Warning, 'Transaction not set!', []);
end;

procedure TSQLQueryEx.Rollback();
begin
  if Self.Transaction <> nil then
  begin
    if Self.Transaction.Active then
    begin
      TSQLTransaction(Self.Transaction).Rollback;
      Self.Transaction.Active := False;
    end;
  end
  else
    LBLogger.Write(1, 'TSQLQueryEx.Rollback', lmt_Warning, 'Transaction not set!', []);
end;


{ TSQLConnectionManager }


function TSQLConnectionManager.getDatabaseType(DBName: String): TDBType;
var
  _idx : Integer;
  _ConnInfo : TSQLDBInfo;

begin
  Result := dbt_Unknown;

  if FDatabaseInfo.Count > 0 then
  begin

    if Length(DBName) = 0 then
      DBName := FDatabaseInfo.Strings[0];

    _idx := FDatabaseInfo.IndexOf(DBName);
    if _idx > -1 then
    begin
      _ConnInfo := FDatabaseInfo.Objects[_idx] as TSQLDBInfo;
      Result := _ConnInfo.DBType;
    end;
  end;
end;



function TSQLConnectionManager.RetrieveQuery(DBName: String; aTimeout: Integer = cAcquireTimeout): TSQLQueryEx;
var
  _idx : Integer = -1;
  _DBInfo : TSQLDBInfo;

begin
  Result := nil;

  if FCScm.Acquire('TSQLConnectionManager.RetrieveQuery', aTimeout) then
  begin
    try

      if FDatabaseInfo.Count > 0 then
      begin
        if Length(DBName) = 0 then
        begin
          DBName := FDatabaseInfo.Strings[0];
          _idx := 0;
        end
        else
          _idx := FDatabaseInfo.IndexOf(DBName);

        if _idx > -1 then
        begin
          _DBInfo := FDatabaseInfo.Objects[_idx] as TSQLDBInfo;

          if _DBInfo <> nil then
          begin
            Result := _DBInfo.RetrieveQuery();
            if Result <> nil then
            begin
              Result.DBName := DBName;
              if Result.Active then
              begin
                LBLogger.Write(3, 'TSQLConnectionManager.RetrieveQuery', lmt_Debug, 'Query active ... closing');
                Result.Active := false; // Close;
              end;
            end;
          end
          else
            LBLogger.Write(1, 'TSQLConnectionManager.RetrieveQuery', lmt_Warning, 'No DB info!');
        end
        else
          LBLogger.Write(1, 'TSQLConnectionManager.RetrieveQuery', lmt_Warning, 'No database found: <%s>', [DBName]);
      end
      else
        LBLogger.Write(1, 'TSQLConnectionManager.RetrieveQuery', lmt_Warning, 'No databases data loaded!');

    except
      on E: Exception do
        LBLogger.Write(1, 'TSQLConnectionManager.RetrieveQuery', lmt_Error, PChar(E.Message));
    end;

    FCScm.Release();
  end;
end;

function TSQLConnectionManager.ReleaseQuery(var AQuery: TSQLQueryEx): Boolean;
var
  _idx : Integer;
  _DBInfo : TSQLDBInfo;

begin
  Result := False;

  if AQuery <> nil then
  begin
    if FCScm.Acquire('TSQLConnectionManager.ReleaseQuery', cReleaseTimeout) then
    begin

      try
        _idx := FDatabaseInfo.IndexOf(AQuery.DBName);
        if _idx > -1 then
        begin
          _DBInfo := FDatabaseInfo.Objects[_idx] as TSQLDBInfo;
          Result := _DBInfo.ReleaseQuery(AQuery);
        end
        else
          LBLogger.Write(1, 'TSQLConnectionManager.ReleaseQuery', lmt_Warning, 'DB alias <%s> not found!', [AQuery.DBName]);

      except
        on E: Exception do
          LBLogger.Write(1, 'TSQLConnectionManager.ReleaseQuery', lmt_Error, PChar(E.Message));
      end;

      FCScm.Release();

    end;
  end;
end;

function TSQLConnectionManager.DataCompleted(DBName: String): Boolean;
var
  _Idx : Integer;

begin
  Result := False;

  if Length(DBName) = 0 then
  begin
    if FDatabaseInfo.Count > 0 then
      _Idx := 0
    else
      _Idx := -1;
  end
  else
    _Idx := FDatabaseInfo.IndexOf(DBName);

  if _Idx > -1 then
    Result := (FDatabaseInfo.Objects[_Idx] as TSQLDBInfo).DataComplete;
end;

procedure TSQLConnectionManager.set_OnNewDatabase(AValue: TNotifyNewDBEvent);
var
  i : Integer;

begin
  if FOnNewDatabase <> AValue then
  begin

    FOnNewDatabase := AValue;
    for i := 0 to FDatabaseInfo.Count - 1 do
      (FDatabaseInfo.Objects[i] as TSQLDBInfo).OnNewDatabase := FOnNewDatabase;

  end;
end;

function TSQLConnectionManager.get_Schema(const DBName: String): String;
var
  _Idx : Integer;

begin
  Result := '';

  if Length(DBName) > 0 then
    _Idx := FDatabaseInfo.IndexOf(DBName)
  else
    _Idx := 0;

  if (_Idx > -1) and (_Idx < FDatabaseInfo.Count) then
    Result := (FDatabaseInfo.Objects[_Idx] as TSQLDBInfo).SchemaName;
end;

function TSQLConnectionManager.get_DBInfo(DBName: String): TSQLDBInfo;
var
  _Idx : Integer;

begin
  Result := nil;

  if Length(DBName) > 0 then
    _Idx := FDatabaseInfo.IndexOf(DBName)
  else
    _Idx := 0;

  if (_Idx > -1) and (_Idx < FDatabaseInfo.Count) then
    Result := TSQLDBInfo(FDatabaseInfo.Objects[_Idx]);
end;

procedure TSQLConnectionManager.set_OnAccessViolation(AValue: TNotifyEvent);
var
  i : Integer;

begin
  if FOnAccessViolation <> AValue then
  begin

    FOnAccessViolation := AValue;
    for i := 0 to FDatabaseInfo.Count - 1 do
      (FDatabaseInfo.Objects[i] as TSQLDBInfo).OnAccessViolation := FOnAccessViolation;

  end;
end;

constructor TSQLConnectionManager.Create;
begin
  inherited Create;

  FPostgresqlInitialized := False;

  FDatabaseInfo := TStringList.Create;
  FDatabaseInfo.OwnsObjects := True;
  FDatabaseInfo.Sorted := True;
  FDatabaseInfo.Duplicates := dupIgnore;

  FCScm := TTimedOutCriticalSection.Create;
end;

destructor TSQLConnectionManager.Destroy;
begin
  if Self <> nil then
  begin

    LBLogger.Write(6, 'TSQLConnectionManager.Destroy', lmt_Debug, 'Destroying ...');

    if FCScm.Acquire('TSQLConnectionManager.Destroy', cDestroingDatabases) then
    begin

      try

        FreeAndNil(FDatabaseInfo);

      except
        on E: Exception do
          LBLogger.Write(1, 'TSQLConnectionManager.Destroy', lmt_Error, PChar(E.Message));
      end;

      FCScm.Release();

    end;

    FreeAndNil(FCScm);

    if FPostgresqlInitialized then
      ReleasePostgres3();

    inherited Destroy;

  end;
end;

procedure TSQLConnectionManager.Clear();
begin
  FDatabaseInfo.Clear();
end;

function TSQLConnectionManager.LoadConfiguration(const AnXMLFile: String): Boolean;
var
  _Doc : TXMLDocument = nil;

begin
  Result := False;

  try

    if FileExists(AnXMLFile) then
    begin
      ReadXMLFile(_Doc, AnXMLFile);
      if _Doc <> nil then
        Result := Self.LoadConfigurationFromXML(_Doc.DocumentElement);
    end
    else
      LBLogger.Write(1, 'TSQLConnectionManager.LoadConfiguration', lmt_Warning, 'Configuration file <%s> not found!', [AnXMLFile]);


  except
    on E: Exception do
      LBLogger.Write(1, 'TSQLConnectionManager.LoadConfiguration', lmt_Error, PChar(E.Message));
  end;

  if _Doc <> nil then
    _Doc.Free;
end;


function TSQLConnectionManager.LoadConfigurationFromXML(ParentNode: TDOMNode): boolean;
var
  _ConnectionsNode : TDOMNode;
  _SingleConnNode : TDomNode;
  _Item : TDOMNode;
  _AConnInfo : TSQLDBInfo;
  _Alias : String;
  _InitializePostgresql : Boolean = False;
  i : Integer;

const
  cDBSingleConn  = DOMString('Connection');
  cDBAlias       = DOMString('DBAlias');

// ----------------------------------------------------------------------------------
// <DBConnections>
//    <Connection>
//        <DBType></DBType>
//        <DBHost></DBHost>
//        <DBPort></DBPort>
//        <DBPassword></DBPassword>
//        <DBUser></DBUser>
//        <DBName></DBName>
//        <DBODBC></DBODBC>
//        <DBAlias></DBAlias>
//    </Connection>
// </DBConnections>
// ----------------------------------------------------------------------------------

begin
  Result := False;

  if ParentNode <> nil then
  begin

    if ParentNode.NodeName = cDBConnections then
      _ConnectionsNode := ParentNode
    else
      _ConnectionsNode := ParentNode.FindNode(cDBConnections);

    if _ConnectionsNode <> nil then
    begin

      FDatabaseInfo.Clear;

      if _ConnectionsNode.ChildNodes.Count > 0 then
      begin
        for i := 0 to _ConnectionsNode.ChildNodes.Count - 1 do
        begin
          _SingleConnNode := _ConnectionsNode.ChildNodes.Item[i];
          if _SingleConnNode.NodeName = cDBSingleConn then
          begin
            _Item := _SingleConnNode.FindNode(cDBAlias);
            if _Item <> nil then
              _Alias := LowerCase(AnsiString(_Item.TextContent))
            else
              _Alias := '';

            if Length(_Alias) > 0 then
            begin

              _AConnInfo := TSQLDBInfo.Create;
              _AConnInfo.OnNewDatabase := FOnNewDatabase;
              _AConnInfo.OnAccessViolation := FOnAccessViolation;
              if _AConnInfo.LoadConfiguration(_SingleConnNode) then
              begin
                if _AConnInfo.DBType = dbt_Posgresql then
                  _InitializePostgresql := True;

                FDatabaseInfo.AddObject(_Alias, _AConnInfo);
              end
              else begin
                LBLogger.Write(1, 'TSQLConnectionManager.LoadConfigurationFromXML', lmt_Warning, 'Item %d: error reading configuration!', [i]);
                FreeAndNil(_AConnInfo);
              end;

            end
            else
              LBLogger.Write(1, 'TSQLConnectionManager.LoadConfigurationFromXML', lmt_Warning, 'Item %d: alias not found!', [i]);

          end;
        end;

        Result := FDatabaseInfo.Count > 0;

        if Result and _InitializePostgresql and (not FPostgresqlInitialized) then
        begin
          InitialisePostgres3();
          FPostgresqlInitialized := True;
        end;
      end
      else
        LBLogger.Write(1, 'TSQLConnectionManager.LoadConfigurationFromXML', lmt_Warning, 'No databases!');
    end
    else
      LBLogger.Write(1, 'TSQLConnectionManager.LoadConfigurationFromXML', lmt_Warning, '<DBConnections> node not found!');

  end
  else
    LBLogger.Write(1, 'TSQLConnectionManager.LoadConfigurationFromXML', lmt_Warning, 'No parent node!');

end;

function TSQLConnectionManager.AddConnectionData(AnAlias: String; DBInfo: TSQLDBInfo): Boolean;
var
  _Idx : Integer;

begin
  Result := False;

  if (Length(AnAlias) > 0) and (DBInfo <> nil) then
  begin
    _Idx := FDatabaseInfo.Add(AnAlias);
    FDatabaseInfo.Objects[_Idx] := DBInfo;

    if DBInfo.OnNewDatabase = nil then
      DBInfo.OnNewDatabase := FOnNewDatabase;

    Result := True;
  end;
end;


{ TSQLDBInfo }

function TSQLDBInfo.get_DataComplete: Boolean;
begin
  Result := False;

  case FDBType of
    dbt_Unknown : ;

    dbt_Posgresql,
    dbt_MySQL80,
    dbt_MySQL57,
    dbt_MySQL56,
    dbt_MySQL51,
    dbt_MySQL55 : Result := (Length(FHost) > 0) and
                            (FPort > 0) and
                            (Length(FUser) > 0) and
                            (Length(FDBName) > 0);

    dbt_ODBC    : Result := Length(FODBC) > 0;
  end;
end;

procedure TSQLDBInfo.DoNewDatabase(AConnection: TSQLConnection);
begin
  if Assigned(FOnNewDatabase) then
    FOnNewDatabase(AConnection, FDBType);
end;

procedure TSQLDBInfo.DoAccessViolation();
begin
  if Assigned(FOnAccessViolation) then
  begin
    LBLogger.Write(1, 'TSQLDBInfo.DoAccessViolation', lmt_Debug, 'Raising event access violation!');
    FOnAccessViolation(Self);
  end;
end;

procedure TSQLDBInfo.RemoveConnectionFromList(Sender: TObject);
var
  _Idx : Integer;

begin
  if Sender is TSQLConnectionHelper then
  begin

    if FCSConnection.Acquire('TSQLDBInfo.RemoveConnectionFromList') then
    begin

      try

        _Idx := FActiveConnections.IndexOf(IntToStr(TSQLConnectionHelper(Sender).OwnerThread));
        if _Idx >= 0 then
        begin
          LBLogger.Write(6, 'TSQLDBInfo.RemoveConnectionFromList', lmt_Debug, 'Removing connection %d for thread %d', [_Idx, Int64(TSQLConnectionHelper(Sender).OwnerThread)]);
          FActiveConnections.Delete(_Idx);
        end;

      except
        on E: Exception do
          LBLogger.Write(1, 'TSQLDBInfo.RemoveConnectionFromList', lmt_Error, PChar(E.Message));
      end;

      FCSConnection.Release();
    end;

  end;
end;

constructor TSQLDBInfo.Create;
begin
  inherited Create;

  FHost                := '';
  FPort                := 0;
  FUser                := '';
  FPassWord            := '';
  FDBName              := '';
  FDBType              := dbt_Unknown;
  FDBConnectionTimeout := cDefaultDBConnectionTimeout;

  FActiveConnections := TStringList.Create();
  FActiveConnections.OwnsObjects := False;
  FActiveConnections.Sorted := True;
  FActiveConnections.Duplicates := dupError;

  FCSConnection      := TTimedOutCriticalSection.Create;

  FOnAccessViolation := nil;
end;

destructor TSQLDBInfo.Destroy;
var
  i : Integer;
  _Conn : TSQLConnectionHelper;


begin

  if FCSConnection.Acquire('TSQLDBInfo.Destroy', cDestroyTimeout) then
  begin
    try
      if FActiveConnections <> nil then
      begin

        for i := FActiveConnections.Count - 1 downto 0 do
        begin
          _Conn := TSQLConnectionHelper(FActiveConnections.Objects[i]);
          _Conn.OnDestroying := nil;
          LBLogger.Write(1, 'TSQLDBInfo.Destroy', lmt_Warning, 'Destroying connection with references count %d!  -  Owner thread: <%s>', [_Conn.ReferencesCount, FActiveConnections.Strings[i]]);
          _Conn.Free;
        end;

        FreeAndNil(FActiveConnections);
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TSQLDBInfo.Destroy', lmt_Error, PChar(E.Message));
    end;

    FCSConnection.Release();
  end;

  FreeAndNil(FCSConnection);

  inherited Destroy;
end;


function TSQLDBInfo.LoadConfiguration(ANode: TDOMNode): Boolean;
var
  _Item : TDOMNode;

const
  cDBType                = DOMString('DBType');
  cConnectionTimeoutNode = DOMString('ConnectionTimeout');
  cHost                  = DOMString('DBHost');
  cPort                  = DOMString('DBPort');
  cPassword              = DOMString('DBPassword');
  cUser                  = DOMString('DBUser');
  cDBName                = DOMString('DBName');
  cDBODBC                = DOMString('ODBC');
  cSchemaName            = DOMString('SchemaName');

begin
  Result := False;

  if ANode <> nil then
  begin

    _Item := ANode.FindNode(cDBType);
    if _Item <> nil then
      FDBType := TDBType(StrToIntDef(AnsiString(_Item.TextContent), 0));

    _Item := ANode.FindNode(cConnectionTimeoutNode);
    if _Item <> nil then
      FDBConnectionTimeout := StrToIntDef(_Item.TextContent, cDefaultDBConnectionTimeout)
    else
      FDBConnectionTimeout := cDefaultDBConnectionTimeout;

    case FDBType of
      dbt_Unknown: ;

      dbt_Posgresql,
      dbt_MySQL80,
      dbt_MySQL57,
      dbt_MySQL56,
      dbt_MySQL51,
      dbt_MySQL55:
        begin
          _Item := ANode.FindNode(cHost);
          if _Item <> nil then
            FHost := _Item.TextContent;

          _Item := ANode.FindNode(cPort);
          if _Item <> nil then
            FPort := StrToIntDef(_Item.TextContent, 0);

          _Item := ANode.FindNode(cPassword);
          if _Item <> nil then
            FPassword := _Item.TextContent;

          _Item := ANode.FindNode(cUser);
          if _Item <> nil then
            FUser := _Item.TextContent;

          _Item := ANode.FindNode(cDBName);
          if _Item <> nil then
            FDBName := _Item.TextContent;

          if FDBType = dbt_Posgresql then
          begin
            _Item := ANode.FindNode(cSchemaName);
            if _Item <> nil then
              FSchemaName := _Item.TextContent;
          end;
        end;

      dbt_ODBC:
        begin
          _Item := ANode.FindNode(cDBODBC);
          if _Item <> nil then
            FODBC := _Item.TextContent;

          _Item := ANode.FindNode(cPassword);
          if _Item <> nil then
            FPassword := _Item.TextContent;

          _Item := ANode.FindNode(cUser);
          if _Item <> nil then
            FUser := _Item.TextContent;
        end;
    end;

    Result := True;

  end
  else
    LBLogger.Write(1, 'TSQLDBInfo.LoadConfiguration', lmt_Warning, 'No node!');

end;

function TSQLDBInfo.getConnection(): TSQLConnectionHelper;
var
  _ATransaction : TSQLTransaction = nil;
  _Conn         : TSQLConnection = nil;

  _NewDB        : Boolean = false;
  _FilePath     : String = '';
  _Error        : Boolean = False;

  _ActualThread : TThreadID;
  _Idx          : Integer;

  _ThreadSafe : Integer;

const
  cAccessVioleationError = String('access violation');

begin
  Result := nil;

  if Self.DataComplete then
  begin
    _ActualThread := GetThreadID;

    if FCSConnection.Acquire('TSQLDBInfo.getConnection', cConnectionListTimeout) then
    begin

      try

        _Idx := FActiveConnections.IndexOf(IntToStr(_ActualThread));
        if _Idx > -1 then
        begin
          Result := TSQLConnectionHelper(FActiveConnections.Objects[_Idx]);
          Result.IncReference();
        end;

      except
        on E : Exception do
          LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, '1. Error: <%s>', [E.Message]);
      end;

      FCSConnection.Release();
    end;

    if Result = nil then
    begin
      LBLogger.Write(6, 'TSQLDBInfo.getConnection', lmt_Debug, 'Creating new connection for thread %d', [_ActualThread]);

      try

        case FDBType of

          dbt_MySQL51,
          dbt_MySQL55,
          dbt_MySQL56,
          dbt_MySQL57,
          dbt_MySQL80:
            begin
              case FDBType of
                dbt_MySQL51:
                  begin
                    _Conn := TMySQL51Connection.Create(nil);
                    (_Conn as TMySQL51Connection).Port := FPort;
                  end;

                dbt_MySQL55:
                  begin
                    _Conn := TMySQL55Connection.Create(nil);
                    (_Conn as TMySQL55Connection).Port := FPort;
                  end;

                dbt_MySQL56:
                  begin
                    _Conn := TMySQL56Connection.Create(nil);
                    (_Conn as TMySQL56Connection).Port := FPort;
                  end;

                dbt_MySQL57:
                  begin
                    _Conn := TMySQL57Connection.Create(nil);
                    (_Conn as TMySQL57Connection).Port := FPort;
                  end;

                dbt_MySQL80:
                  begin
                    _Conn := TMySQL80Connection.Create(nil);
                    (_Conn as TMySQL80Connection).Port := FPort;
                  end;
              end;
              _Conn.HostName := FHost;
              _Conn.Params.Add('MYSQL_OPT_CONNECT_TIMEOUT=' + IntToStr(FDBConnectionTimeout));
              _Conn.Params.Add('autocommit=0');
              _Conn.DatabaseName := FDBName;
              _Conn.UserName  := FUser;
              _Conn.Password := FPassword;
            end;

          dbt_Posgresql:
            begin
              _Conn := TPQConnection.Create(nil);
              _Conn.HostName := FHost;
              _Conn.Params.Add('port=' + IntToStr(FPort));
              _Conn.DatabaseName := FDBName;
              _Conn.UserName  := FUser;
              _Conn.Password := FPassword;
            end;

          dbt_ODBC:
            begin
              _Conn := TODBCConnection.Create(nil);
              _Conn.DatabaseName := FODBC;

              if Length(FUser) > 0 then
                _Conn.UserName := FUser;

              if Length(FPassWord) > 0 then
                _Conn.Password := FPassWord;
            end;

          else
            LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Warning, 'Unknown DB type: %d', [Integer(FDBType)]);
        end;

        if _Conn <> nil then
        begin
          _Error := False;

          if gv_CS.Acquire('TSQLDBInfo.getConnection', cConnectionTimeout) then
          begin
            try
              _Conn.Open();

            except
              on E: Exception do
              begin
                _Error := True;
                LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, 'Error opening connection: %s', [E.Message]);

                if Trim(LowerCase(E.Message)) = cAccessVioleationError then
                  Self.DoAccessViolation();

              end;
            end;
            gv_CS.Release();
          end
          else
            _Error := True;

        end;

      except
        on E : Exception do
        begin
          _Error := True;

          LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, '2. Error: <%s>', [E.Message]);
        end;
      end;

      if (not _Error) and (_Conn <> nil) then
      begin
        try

          if _Conn.Connected then
          begin
            if _NewDB then
            begin
              try
                Self.DoNewDatabase(_Conn);

              except
                on E : Exception do
                begin
                  LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, 'Error: <%s>', [E.Message]);
                  _Error := True;
                end;
              end;
            end;
          end
          else begin
            LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Warning, 'Database not connected! Closing connection ...');
            _Error := True;
          end;

        except
          on E: Exception do
          begin
            LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, '2. Error: <%s>', [E.Message]);
            _Error := True;
          end;
        end;
      end
      else
        LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Warning, 'Wrong connection data! Host: <%s>  -  Port: <%d>  -  User: <%s>  -  DBName: <%s>', [FHost, FPort, FUser, FDBName]);


      if (not _Error) and (_Conn <> nil) then
      begin
        Result := TSQLConnectionHelper.Create;
        Result.OwnerThread := _ActualThread;
        Result.OnDestroying := @Self.RemoveConnectionFromList;
        Result.Connection := _Conn;

//        _Conn.CharSet := 'utf8';  {$WARNING   da togliere?}

        if FCSConnection.Acquire('TSQLDBInfo.getConnection - 2', cConnectionListTimeout) then
        begin
          try
            Result.IncReference();
            LBLogger.Write(6, 'TSQLDBInfo.getConnection', lmt_Debug, 'Adding new connection for thread %d', [Int64(_ActualThread)]);
            FActiveConnections.AddObject(IntToStr(_ActualThread), Result);

          except
            on E: Exception do
            begin
              _Error := True;
              LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Error, '2. - %s', [E.Message]);
            end;
          end;
          FCSConnection.Release();
        end
        else
          _Error := True;
      end;

      if _Error then
      begin
        if Result <> nil then
          FreeAndNil(Result)
        else begin
          if _Conn <> nil then
            FreeAndNil(_Conn);

          if _ATransaction <> nil then
            FreeAndNil(_ATransaction);
        end;
      end;

    end;

  end
  else
    LBLogger.Write(1, 'TSQLDBInfo.getConnection', lmt_Warning, 'Cannot create connection: uncompleted data!');

end;

function TSQLDBInfo.RetrieveQuery(): TSQLQueryEx;
var
  _Conn : TSQLConnectionHelper;
  _ATransaction : TSQLTransaction;

begin
  Result := nil;

  try

    _Conn := Self.getConnection();
    if _Conn <> nil then
    begin
      Result := TSQLQueryEx.Create(nil);


      Result.DBType := FDBType;

      _ATransaction := TSQLTransaction.Create(nil);
      _ATransaction.DataBase := _Conn.Connection;
      Result.Transaction := _ATransaction;

      Result.SchemaName := FSchemaName;
      Result.Options := Result.Options - [sqoAutoCommit];

      // Result.StartTransaction();
    end
    else
      LBLogger.Write(1, 'TSQLDBInfo.RetrieveQuery', lmt_Warning, 'DB connection not retrieved!');

  except
    on E: Exception do
      LBLogger.Write(1, 'TSQLDBInfo.RetrieveQuery', lmt_Error, PChar(E.Message));
  end;
end;

function TSQLDBInfo.ReleaseQuery(var AQuery: TSQLQueryEx): Boolean;
var
  _Conn : TSQLConnection = nil;
  _Trans : TDBTransaction;

begin
  Result := False;

  if AQuery <> nil then
  begin
    try
      _Conn := TSQLConnection(AQuery.DataBase);

      if AQuery.Active then
      begin
        AQuery.Rollback();
        AQuery.Close;
      end;

      _Trans := AQuery.Transaction;
      if _Trans <> nil then
      begin
        AQuery.Transaction := nil;
        _Trans.Active := False;
        FreeAndNil(_Trans);
      end;

      AQuery.Free;

    except
      on E: Exception do
        LBLogger.Write(1, 'TSQLDBInfo.ReleaseQuery', lmt_Error, 'Error destroying query: %s', [E.Message]);
    end;

    AQuery := nil;

    if not Self.CloseConnection(_Conn) then
      LBLogger.Write(1, 'TSQLDBInfo.ReleaseQuery', lmt_Debug, 'Connection NOT closed!');

    Result := True;
  end;
end;

function TSQLDBInfo.CloseConnection(aConnection: TSQLConnection): Boolean;
var
  _Helper : TSQLConnectionHelper = nil;

  i : Integer;

begin
  Result := False;

  if aConnection <> nil then
  begin
    if FCSConnection.Acquire('TSQLDBInfo.CloseConnection', cConnectionListTimeout) then
    begin
      try

        for i := FActiveConnections.Count - 1 downto 0 do
        begin
          if TSQLConnectionHelper(FActiveConnections.Objects[i]).Connection = aConnection then
          begin
            _Helper := TSQLConnectionHelper(FActiveConnections.Objects[i]);
            _Helper.DecReference();
            if _Helper.ReferencesCount = 0 then
            begin
              LBLogger.Write(6, 'TSQLDBInfo.CloseConnection', lmt_Debug, 'Destroying connection for thread %d', [Int64(_Helper.OwnerThread)]);
              _Helper.Free;
            end;
            Break;
          end;
        end;

      except
        on E: Exception do
          LBLogger.Write(1, 'TSQLDBInfo.CloseConnection', lmt_Error, PChar(E.Message));
      end;

      FCSConnection.Release();

      Result := True;
    end;
  end;
end;


initialization
  gv_CS := TTimedOutCriticalSection.Create;  // Necessario per sincronizzare + istanze di DBConnectionManager

  gv_DBNameToTypeMap := TDBNameToTypeMap.Create;
  gv_DBNameToTypeMap.Add('ODBC', dbt_ODBC);
  gv_DBNameToTypeMap.Add('Posgresql', dbt_Posgresql);
  gv_DBNameToTypeMap.Add('MySQL51', dbt_MySQL51);
  gv_DBNameToTypeMap.Add('MySQL55', dbt_MySQL55);
  gv_DBNameToTypeMap.Add('MySQL56', dbt_MySQL56);
  gv_DBNameToTypeMap.Add('MySQL57', dbt_MySQL57);
  gv_DBNameToTypeMap.Add('MySQL80', dbt_MySQL80);


finalization
  FreeAndNil(gv_CS);
  FreeAndNil(gv_DBNameToTypeMap);

end.

