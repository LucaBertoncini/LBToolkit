unit uXPLCTemplates;

{$mode ObjFPC}{$H+}

{ ============================================================================
  uXPLCTemplates

  Motore di template per XPLC.

  Permette di definire macchine a stati parametrizzate (template) e di
  istanziarle più volte con parametri diversi, evitando la duplicazione
  di file XML quasi identici.

  Flusso:
    1. TXPLCManager legge il file INI
    2. Per ogni entry che punta a un .xplci (istance file), chiama
       TXPLCTemplateEngine.ResolveInstanceFile
    3. ResolveInstanceFile carica il file .xplci, per ogni <Instance>
       risolve il template corrispondente e ritorna una lista di TDOMDocument
       già pronti per essere passati a TLogicController.LoadFromXMLDocument
    4. TLogicController non sa nulla di template: riceve sempre un DOM
       già risolto

  Struttura file .xplci (XPLCInstance):
    <XPLCInstances TemplateDir="./templates">
      <Instance Code="TornelloIngresso" Template="Tornello">
        <Param Name="FC_CCW_Id"  Value="107"/>
        ...
      </Instance>
      <Instance Code="TornelloMensa" Template="Tornello">
        ...
      </Instance>
    </XPLCInstances>

  Struttura file template (.xplct):
    <XPLCTemplate Code="Tornello">
      <Parameters>
        <Param Name="FC_CCW_Id"  Type="Integer"/>
        <Param Name="TWait_Ms"   Type="Integer" Default="3000"/>
      </Parameters>
      <!-- Include di altri template (annidamento) -->
      <Include Template="SemaforoTemplate" Code="$Code_Semaforo">
        <Param Name="SEM_Id"     Value="$SEM_Id"/>
        <Param Name="ControlVar" Value="$SemVar"/>
      </Include>
      <!-- Corpo identico a un normale file XPLC -->
      <Inputs>  ... </Inputs>
      <Outputs> ... </Outputs>
      ...
    </XPLCTemplate>

  Sostituzione parametri:
    Tutti i placeholder della forma $NomeParametro vengono sostituiti
    con il valore corrispondente. La sostituzione avviene sulla stringa
    XML grezza prima del parsing DOM, quindi funziona su qualsiasi
    attributo o valore testuale.

  Template annidati:
    Un template può includere altri template tramite <Include>.
    La risoluzione è ricorsiva: ogni <Include> genera uno o più
    TDOMDocument aggiuntivi. Il documento padre (che contiene gli
    <Include>) NON viene aggiunto alla lista risultante se non ha
    una propria <StateMachine>: è un puro contenitore.

  ============================================================================ }

interface

uses
  Classes, SysUtils, Laz2_DOM, Laz2_XMLRead, fgl, uLBFileUtils;

type

  { TResolvedDocument }
  { Un documento DOM già risolto, pronto per TLogicController }
  TResolvedDocument = class(TObject)
  public
    Document : TXMLDocument;
    Code     : String;   // valore dell'attributo Code della macchina

    constructor Create(aDoc: TXMLDocument; const aCode: String);
    destructor  Destroy; override;
  end;

  TResolvedDocumentList = specialize TFPGObjectList<TResolvedDocument>;


  { TXPLCTemplateEngine }
  TXPLCTemplateEngine = class(TObject)
  strict private
    FTemplateDir : String;  // directory base per i file .xplct

    { Carica un file .xplct come stringa grezza }
    function loadTemplateText(const aTemplateName: String): String;

    { Sostituisce tutti i $NomeParam nella stringa con i valori di aParams.
      Ritorna il numero di placeholder rimasti non sostituiti. }
    function substituteParams(var aText: String;
                              aParams: TStringList): Integer;

    { Verifica che non esistano parametri obbligatori (senza Default)
      non presenti in aParams. Logga un warning per ognuno. }
    procedure checkMissingParams(aParamDefs: TDOMNode;
                                 aParams: TStringList;
                                 const aInstanceCode: String);

    { Applica i default dei parametri non forniti dall'istanza }
    procedure applyParamDefaults(aParamDefs: TDOMNode;
                                 aParams: TStringList);

    { Estrae i <Param> figli di un nodo <Include> o <Instance>
      in una TStringList Name=Value, sostituendo eventuali $Ref
      con i valori di aParentParams }
    procedure extractNodeParams(aNode: TDOMNode;
                                aParentParams: TStringList;
                                out aParams: TStringList);

    { Risolve un singolo template con i parametri dati.
      Ritorna la lista di TResolvedDocument (può essere > 1 per template annidati) }
    function resolveTemplate(const aTemplateName: String;
                             const aInstanceCode: String;
                             aParams: TStringList;
                             aDepth: Integer): TResolvedDocumentList;

    { Risolve gli <Include> trovati nel DOM di un template già sostituito.
      Aggiunge i documenti figli a aResult. }
    procedure resolveIncludes(aRootNode: TDOMNode;
                              aParams: TStringList;
                              aResult: TResolvedDocumentList;
                              aDepth: Integer);

    { Rimuove i nodi <Include> e <Parameters> dal DOM prima di
      passarlo a TLogicController (che non li conosce) }
    procedure cleanTemplateNodes(aDoc: TXMLDocument);

  public
    constructor Create(const aTemplateDir: String);

    { Punto di ingresso principale.
      Carica il file .xplci e ritorna la lista di documenti risolti.
      Il chiamante è responsabile della distruzione della lista. }
    function ResolveInstanceFile(const aInstanceFile: String): TResolvedDocumentList;

    { Risolve una singola istanza inline (usato da ResolveInstanceFile
      ma esposto per testing e usi diretti) }
    function ResolveInstance(const aTemplateName: String;
                             const aInstanceCode: String;
                             aParams: TStringList): TResolvedDocumentList;

    property TemplateDir: String read FTemplateDir write FTemplateDir;

    const
      cTemplateExtension = '.xplct';
      cInstanceExtension = '.xplci';
      cParamPrefix       = '$';
      cMaxNestingDepth   = 10;  // protezione contro ricorsione infinita
  end;


implementation

uses
  ULBLogger, FileUtil;


{ TResolvedDocument }

constructor TResolvedDocument.Create(aDoc: TXMLDocument; const aCode: String);
begin
  inherited Create;
  Document := aDoc;
  Code     := aCode;
end;

destructor TResolvedDocument.Destroy;
begin
  FreeAndNil(Document);
  inherited Destroy;
end;


{ TXPLCTemplateEngine }

constructor TXPLCTemplateEngine.Create(const aTemplateDir: String);
begin
  inherited Create;
  // Path relativo all'eseguibile (aBaseDir omesso → usa ParamStr(0))
  FTemplateDir := IncludeTrailingPathDelimiter(ResolvePathDeterministic(aTemplateDir));
end;


function TXPLCTemplateEngine.loadTemplateText(const aTemplateName: String): String;
var
  _path : String;
  _sl   : TStringList = nil;
begin
  Result := '';

  // Cerca prima nella TemplateDir, poi come percorso assoluto/relativo
  _path := FTemplateDir + aTemplateName + cTemplateExtension;
  if not FileExists(_path) then
    _path := aTemplateName + cTemplateExtension;
  if not FileExists(_path) then
    _path := aTemplateName;  // percorso già completo con estensione

  if not FileExists(_path) then
  begin
    LBLogger.Write(1, 'TXPLCTemplateEngine.loadTemplateText', lmt_Warning, 'Template file not found: <%s>', [aTemplateName]);
    Exit;
  end;

  _sl := TStringList.Create;
  try
    _sl.LoadFromFile(_path);
    Result := _sl.Text;
  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCTemplateEngine.loadTemplateText', lmt_Error, E.Message);
  end;

  if _sl <> nil then
    _sl.Free;
end;


function TXPLCTemplateEngine.substituteParams(var aText: String;
                                               aParams: TStringList): Integer;
var
  i, j       : Integer;
  _key, _val : String;
  _sorted    : TStringList;

begin
  // Ordina per lunghezza chiave decrescente per evitare
  // che $FC_CW_Id sostituisca parte di $FC_CW_Id_Name
  _sorted := TStringList.Create;
  try
    _sorted.AddStrings(aParams);
    // Bubble sort per lunghezza decrescente (lista piccola)
    for i := 0 to _sorted.Count - 2 do
    begin
      for j := i + 1 to _sorted.Count - 1 do
      begin
        if Length(_sorted.Names[i]) < Length(_sorted.Names[j]) then
          _sorted.Exchange(i, j);
      end;
    end;

    for i := 0 to _sorted.Count - 1 do
    begin
      _key  := cParamPrefix + _sorted.Names[i];
      _val  := _sorted.ValueFromIndex[i];
      aText := StringReplace(aText, _key, _val, [rfReplaceAll]);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCTemplateEngine.substituteParams', lmt_Error, E.Message);
  end;
  _sorted.Free;

  Result := 0;
  for i := 1 to Length(aText) - 1 do
  begin
    if (aText[i] = '$') and (aText[i+1] in ['A'..'Z','a'..'z','_']) then
      Inc(Result);
  end;
end;


procedure TXPLCTemplateEngine.applyParamDefaults(aParamDefs: TDOMNode;
                                                  aParams: TStringList);
var
  _child      : TDOMNode;
  _name, _def : String;
begin
  if aParamDefs = nil then Exit;

  _child := aParamDefs.FirstChild;
  while _child <> nil do
  begin
    if _child.NodeName = 'Param' then
    begin
      _name := TDOMElement(_child).AttribStrings['Name'];
      _def  := TDOMElement(_child).AttribStrings['Default'];

      // Aggiunge il default solo se il parametro non è già stato fornito
      if (_name <> '') and (_def <> '') and
         (aParams.IndexOfName(_name) = -1) then
        aParams.Add(_name + '=' + _def);
    end;
    _child := _child.NextSibling;
  end;
end;


procedure TXPLCTemplateEngine.checkMissingParams(aParamDefs: TDOMNode;
                                                  aParams: TStringList;
                                                  const aInstanceCode: String);
var
  _child   : TDOMNode;
  _name    : String;
  _hasDefault : Boolean;
begin
  if aParamDefs = nil then Exit;

  _child := aParamDefs.FirstChild;
  while _child <> nil do
  begin
    if _child.NodeName = 'Param' then
    begin
      _name       := TDOMElement(_child).AttribStrings['Name'];
      _hasDefault := TDOMElement(_child).AttribStrings['Default'] <> '';

      if (not _hasDefault) and (aParams.IndexOfName(_name) = -1) then
        LBLogger.Write(1, 'TXPLCTemplateEngine.checkMissingParams', lmt_Warning, 'Instance <%s>: required parameter <%s> not provided', [aInstanceCode, _name]);
    end;
    _child := _child.NextSibling;
  end;
end;


procedure TXPLCTemplateEngine.extractNodeParams(aNode: TDOMNode;
                                                 aParentParams: TStringList;
                                                 out aParams: TStringList);
var
  _child    : TDOMNode;
  _name     : String;
  _value    : String;
  _resolved : String;
  i         : Integer;
begin
  aParams := TStringList.Create;

  _child := aNode.FirstChild;
  while _child <> nil do
  begin
    if _child.NodeName = 'Param' then
    begin
      _name  := TDOMElement(_child).AttribStrings['Name'];
      _value := TDOMElement(_child).AttribStrings['Value'];

      // Risolve riferimenti a parametri del padre ($NomeParam)
      if (aParentParams <> nil) and (_value <> '') and
         (_value[1] = '$') then
      begin
        _resolved := _value;
        for i := 0 to aParentParams.Count - 1 do
          _resolved := StringReplace(_resolved,
            cParamPrefix + aParentParams.Names[i],
            aParentParams.ValueFromIndex[i],
            [rfReplaceAll]);
        _value := _resolved;
      end;

      if _name <> '' then
        aParams.Add(_name + '=' + _value);
    end;
    _child := _child.NextSibling;
  end;
end;


procedure TXPLCTemplateEngine.resolveIncludes(aRootNode: TDOMNode;
                                               aParams: TStringList;
                                               aResult: TResolvedDocumentList;
                                               aDepth: Integer);
var
  _child        : TDOMNode;
  _next         : TDOMNode;
  _templateName : String;
  _childCode    : String;
  _childParams  : TStringList;
  _childResults : TResolvedDocumentList;
  i             : Integer;
begin
  _child := aRootNode.FirstChild;
  while _child <> nil do
  begin
    _next := _child.NextSibling;  // salva subito: potremmo rimuovere _child

    if _child.NodeName = 'Include' then
    begin
      _templateName := TDOMElement(_child).AttribStrings['Template'];
      _childCode    := TDOMElement(_child).AttribStrings['Code'];

      // Risolve $Code dal contesto del padre se necessario
      if (_childCode <> '') and (_childCode[1] = '$') then
        for i := 0 to aParams.Count - 1 do
          _childCode := StringReplace(_childCode,
            cParamPrefix + aParams.Names[i],
            aParams.ValueFromIndex[i],
            [rfReplaceAll]);

      if _templateName <> '' then
      begin
        extractNodeParams(_child, aParams, _childParams);
        try
          _childResults := resolveTemplate(_templateName, _childCode,
                                           _childParams, aDepth + 1);
          try
            // Trasferisce i documenti figli nella lista risultato del padre
            for i := 0 to _childResults.Count - 1 do
            begin
              aResult.Add(_childResults[i]);
              _childResults[i] := nil;  // evita double-free
            end;
          finally
            _childResults.Free;
          end;
        finally
          _childParams.Free;
        end;
      end
      else
        LBLogger.Write(1, 'TXPLCTemplateEngine.resolveIncludes', lmt_Warning, 'Include node without Template attribute', []);
    end;

    _child := _next;
  end;
end;


procedure TXPLCTemplateEngine.cleanTemplateNodes(aDoc: TXMLDocument);
var
  _root     : TDOMNode;
  _child    : TDOMNode;
  _next     : TDOMNode;
  _nodeName : String;
begin
  _root := aDoc.DocumentElement;
  if _root = nil then Exit;

  _child := _root.FirstChild;
  while _child <> nil do
  begin
    _next     := _child.NextSibling;
    _nodeName := _child.NodeName;

    // Rimuove nodi che TLogicController non conosce
    if (_nodeName = 'Parameters') or (_nodeName = 'Include') then
      _root.RemoveChild(_child);

    _child := _next;
  end;

  // Cambia il tag radice da XPLCTemplate a XPLC
  // (TLogicController non controlla il tag radice, ma è più pulito)
  // Non necessario se TLogicController usa DocumentElement direttamente
end;


function TXPLCTemplateEngine.resolveTemplate(const aTemplateName: String;
                                              const aInstanceCode: String;
                                              aParams: TStringList;
                                              aDepth: Integer): TResolvedDocumentList;
var
  _templateText  : String;
  _remaining     : Integer;
  _stream        : TStringStream;
  _doc           : TXMLDocument = nil;
  _root          : TDOMNode;
  _paramDefsNode : TDOMNode;
  _allParams     : TStringList = nil;
  _hasStateMachine : Boolean;
begin
  Result := TResolvedDocumentList.Create(True);

  // Protezione contro ricorsione infinita
  if aDepth > cMaxNestingDepth then
  begin
    LBLogger.Write(1, 'TXPLCTemplateEngine.resolveTemplate', lmt_Warning, 'Max nesting depth (%d) exceeded for template <%s>', [cMaxNestingDepth, aTemplateName]);
    Exit;
  end;

  // 1. Carica il testo del template
  _templateText := Self.loadTemplateText(aTemplateName);
  if _templateText = '' then Exit;

  // 2. Prima passata: parsing DOM per estrarre <Parameters> e applicare i default
  //    senza ancora fare sostituzione (i valori di default potrebbero contenere $)
  _stream := TStringStream.Create(_templateText);
  try
    ReadXMLFile(_doc, _stream);
  finally
    _stream.Free;
  end;

  if _doc = nil then
  begin
    LBLogger.Write(1, 'TXPLCTemplateEngine.resolveTemplate', lmt_Warning, 'Failed to parse template <%s>', [aTemplateName]);
    Exit;
  end;

  _root          := _doc.DocumentElement;
  _paramDefsNode := nil;
  if _root <> nil then
    _paramDefsNode := _root.FindNode('Parameters');

  // 3. Costruisce l'insieme completo dei parametri:
  //    quelli forniti dall'istanza + i default del template
  _allParams := TStringList.Create;
  try
    _allParams.AddStrings(aParams);
    Self.applyParamDefaults(_paramDefsNode, _allParams);
    Self.checkMissingParams(_paramDefsNode, _allParams, aInstanceCode);

    FreeAndNil(_doc);  // rilascia il DOM temporaneo usato per leggere i default

    // 4. Sostituisce i parametri nel testo grezzo
    _remaining := substituteParams(_templateText, _allParams);
    if _remaining > 0 then
      LBLogger.Write(1, 'TXPLCTemplateEngine.resolveTemplate', lmt_Warning, 'Template <%s> instance <%s>: %d unresolved placeholder(s)', [aTemplateName, aInstanceCode, _remaining]);

    // 5. Parsing DOM finale sul testo con parametri sostituiti
    _stream := TStringStream.Create(_templateText);
    try
      ReadXMLFile(_doc, _stream);
    finally
      _stream.Free;
    end;

    if _doc = nil then
    begin
      LBLogger.Write(1, 'TXPLCTemplateEngine.resolveTemplate', lmt_Warning, 'Failed to parse resolved template <%s>', [aTemplateName]);
      Exit;
    end;

    _root := _doc.DocumentElement;

    // 6. Risolve gli <Include> prima di pulire il DOM
    Self.resolveIncludes(_root, _allParams, Result, aDepth);

    // 7. Controlla se questo template ha una propria <StateMachine>
    _hasStateMachine := (_root <> nil) and
                        (_root.FindNode('StateMachine') <> nil);

    if _hasStateMachine then
    begin
      // Imposta il Code dell'istanza sull'attributo radice
      if (_root <> nil) and (aInstanceCode <> '') then
        TDOMElement(_root).AttribStrings['Code'] := aInstanceCode;

      // Rimuove <Parameters> e <Include> che TLogicController non conosce
      Self.cleanTemplateNodes(_doc);

      Result.Add(TResolvedDocument.Create(_doc, aInstanceCode));
      _doc := nil;  // ownership trasferita a TResolvedDocument
    end
    else begin
      // Template puro contenitore: nessuna StateMachine propria,
      // genera solo i figli dagli Include
      LBLogger.Write(5, 'TXPLCTemplateEngine.resolveTemplate', lmt_Debug, 'Template <%s>: container only, no StateMachine', [aTemplateName]);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCTemplateEngine.resolveTemplate', lmt_Error, E.Message);
  end;

  if _allParams <> nil then
    _allParams.Free;

  if _doc <> nil then
    _doc.Free;
end;


function TXPLCTemplateEngine.ResolveInstance(const aTemplateName: String;
                                              const aInstanceCode: String;
                                              aParams: TStringList): TResolvedDocumentList;
begin
  Result := Self.resolveTemplate(aTemplateName, aInstanceCode, aParams, 0);
end;


function TXPLCTemplateEngine.ResolveInstanceFile(const aInstanceFile: String): TResolvedDocumentList;
var
  _doc             : TXMLDocument;
  _root            : TDOMNode;
  _node            : TDOMNode;
  _templateName    : String;
  _instanceCode    : String;
  _templateDir     : String;
  _params          : TStringList;
  _resolved        : TResolvedDocumentList;
  i                : Integer;
  _absInstanceFile : String;
begin
  Result := TResolvedDocumentList.Create(True);

  // 1. CREAZIONE DEL PATH ASSOLUTO PER IL FILE .XPLCI
  // Se aInstanceFile è relativo (es. "./Tornelli.xplci" dal config.ini),
  // lo risolviamo rispetto all'exe. Questo è CRITICO per i servizi/demoni
  // dove la Current Directory non è quella dell'exe.
  _absInstanceFile := ResolvePathDeterministic(aInstanceFile);

  if not FileExists(_absInstanceFile) then
  begin
    LBLogger.Write(1, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Warning, 'Instance file not found: <%s>', [_absInstanceFile]);
    Exit;
  end;

  try
    ReadXMLFile(_doc, _absInstanceFile);
  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Error, 'Error parsing instance file <%s>: %s', [_absInstanceFile, E.Message]);
      Exit;
    end;
  end;

  if _doc = nil then Exit;

  try
    _root := _doc.DocumentElement;
    if _root = nil then Exit;

    // Legge la TemplateDir dal file .xplci
    _templateDir := TDOMElement(_root).AttribStrings['TemplateDir'];
    if _templateDir <> '' then
    begin
      // 2. RISOLUZIONE TEMPLATE DIR
      // Ora ExtractFilePath(_absInstanceFile) è GARANTITO essere un path assoluto.
      // ResolvePathDeterministic lo userà come base sicura, ignorando la Current Directory.
      _templateDir := IncludeTrailingPathDelimiter(ResolvePathDeterministic(_templateDir, ExtractFilePath(_absInstanceFile)));

      // CORREZIONE BUG LOG: loggare la variabile locale _templateDir, non FTemplateDir
      LBLogger.Write(5, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Debug, 'TemplateDir resolved to: <%s>', [_templateDir]);
    end;

    // Itera su tutti i nodi <Instance>
    _node := _root.FirstChild;
    while _node <> nil do
    begin
      if _node.NodeName = 'Instance' then
      begin
        _templateName := TDOMElement(_node).AttribStrings['Template'];
        _instanceCode := TDOMElement(_node).AttribStrings['Code'];

        if _templateName = '' then
        begin
          LBLogger.Write(1, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Warning, 'Instance without Template attribute in <%s>', [_absInstanceFile]);
          _node := _node.NextSibling;
          Continue;
        end
        else if _templateDir <> '' then
          _templateName := _templateDir + _templateName; // Applica il path assoluto solo se definito nel .xplci

        if _instanceCode = '' then
        begin
          LBLogger.Write(1, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Warning, 'Instance without Code attribute in <%s>', [_absInstanceFile]);
          _node := _node.NextSibling;
          Continue;
        end;

        LBLogger.Write(5, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Info, 'Resolving instance <%s> from template <%s>', [_instanceCode, _templateName]);

        extractNodeParams(_node, nil, _params);
        try
          _resolved := Self.resolveTemplate(_templateName, _instanceCode, _params, 0);
          try
            for i := 0 to _resolved.Count - 1 do
            begin
              Result.Add(_resolved[i]);
              _resolved.Extract(_resolved[i]);
            end;
          finally
            _resolved.Free;
          end;
        finally
          _params.Free;
        end;
      end;

      _node := _node.NextSibling;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Error, E.Message);
  end;

  if _doc <> nil then
    _doc.Free;

  LBLogger.Write(5, 'TXPLCTemplateEngine.ResolveInstanceFile', lmt_Info, 'Instance file <%s>: resolved %d state machine(s)', [_absInstanceFile, Result.Count]);
end;

end.
