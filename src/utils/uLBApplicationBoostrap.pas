unit uLBApplicationBoostrap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, Laz2_DOM, IniFiles;

type

  { TLBApplicationBoostrap }

  TLBApplicationBoostrap = class(TObject)
    strict private
      type
        TLoggerConfig = record
          LogFile     : String;
          LogLevel    : Integer;
          MaxFileSize : Int64;
        end;
        pLoggerConfig = ^TLoggerConfig;

        TWebServerConfig = record
          Port         : Integer;
          DocumentRoot : String;
        end;
        pWebServerConfig = ^TWebServerConfig;

    strict private
      procedure setUpLogger(aConfiguration: pLoggerConfig);
      procedure setUpWebServer(aConfiguration: pWebServerConfig);

      const
        cBootstrapApplicationDescription = String('BootApp');

    strict protected
      FWebServer : TLBmicroWebServer;

      function LoadConfigurationFromXMLNode(aNode: TDOMNode; out anErrorMsg: String): Boolean; virtual;
      function LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean; virtual;
      function LoadConfigurationFromXMLFile(const aFilename: String; out anErrorMsg: String): Boolean;
      function LoadConfigurationFromINIFile(const aFilename: String; out anErrorMsg: String): Boolean;

      function getApplicationDescription(): String; virtual;

      {:Used to set Web-Server callbacks before it starts}
      procedure startingWebServer(); virtual;

    public
      destructor Destroy; override;

      function LoadConfiguration(const aFilename: String; out anErrorMsg: String): Boolean;

      const
        // Sezioni
        cINI_CFG_SEC_LOGGER         = 'LBLogger';
        cINI_CFG_SEC_WEBSERVER      = 'LBmicroWebServer';

        // Logger keys
        cINI_CFG_KEY_LOGFILE        = 'LogFile';
        cINI_CFG_KEY_LOGLEVEL       = 'LogLevel';
        cINI_CFG_KEY_LOGFILESIZE    = 'MaxFileSize';

        // WebServer keys
        cINI_CFG_KEY_PORT           = 'Port';
        cINI_CFG_KEY_DOCUMENTROOT   = 'DocumentRoot';

        cXML_CFG_Logger             = cINI_CFG_SEC_LOGGER;
        cXML_CFG_WEBSERVER          = cINI_CFG_SEC_WEBSERVER;

        cXML_CFG_ATTR_LOGFILE       = cINI_CFG_KEY_LOGFILE;
        cXML_CFG_ATTR_LOGLEVEL      = cINI_CFG_KEY_LOGLEVEL;
        cXML_CFG_ATTR_LOGFILESIZE   = cINI_CFG_KEY_LOGFILESIZE;

        cXML_CFG_ATTR_WEBPORT       = cINI_CFG_KEY_PORT;
        cXML_CFG_ATTR_DOCUMENTROOT  = cINI_CFG_KEY_DOCUMENTROOT;


        cExtension_INIFile = '.ini';
        cExtension_XMLFile = '.xml';
        cDefaultLogFileSize = 2 * 1024 * 1024;

  end;

implementation

uses
  ULBLogger, FileUtil, uLBFileUtils;

{ TLBApplicationBoostrap }

function TLBApplicationBoostrap.LoadConfigurationFromXMLNode(aNode: TDOMNode; out anErrorMsg: String): Boolean;
var
  _MainNode : TDOMNode;
  _Item : TDOMElement;
  _LogCfg : TLoggerConfig;
  _WebCfg : TWebServerConfig;

begin
  Result := False;

  if aNode <> nil then
  begin
    if aNode.NodeName = Self.getApplicationDescription() then
      _MainNode := aNode
    else
      _MainNode := aNode.FindNode(Self.getApplicationDescription());

    if _MainNode <> nil then
    begin
      _Item := TDOMElement(_MainNode.FindNode(cXML_CFG_Logger));
      if _Item <> nil then
      begin
        _LogCfg.LogFile := _Item.GetAttribute(cXML_CFG_ATTR_LOGFILE);
        _LogCfg.LogLevel := StrToIntDef(_Item.GetAttribute(cXML_CFG_ATTR_LOGLEVEL), 1);
        _LogCfg.MaxFileSize := StrToInt64Def(_Item.GetAttribute(cXML_CFG_ATTR_LOGFILESIZE), 0);
      end
      else
        FillChar(_LogCfg, SizeOf(_LogCfg), 0);
      Self.setUpLogger(@_LogCfg);

      _Item := TDOMElement(_MainNode.FindNode(cXML_CFG_Logger));
      if _Item <> nil then
      begin
        _WebCfg.Port := StrToIntDef(_Item.GetAttribute(cXML_CFG_ATTR_WEBPORT), 0);
        _WebCfg.DocumentRoot := Trim(_Item.GetAttribute(cXML_CFG_ATTR_DOCUMENTROOT));
      end
      else
        FillChar(_WebCfg, SizeOf(_WebCfg), 0);
      Self.setUpWebServer(@_WebCfg);
    end
    else
      anErrorMsg := Format('Node <%s> not found!', [Self.getApplicationDescription()]);
  end
  else
    anErrorMsg := 'Node not set!';
end;

function TLBApplicationBoostrap.LoadConfigurationFromINIFileInternal(aFile: TIniFile; out anErrorMsg: String): Boolean;
begin
  Result := True;
end;

function TLBApplicationBoostrap.LoadConfigurationFromXMLFile(const aFilename: String; out anErrorMsg: String): Boolean;
var
  _Doc : TXMLDocument = nil;

begin
  Result := False;

  if OpenXMLFile(aFilename, _Doc) then
    Result := Self.LoadConfigurationFromXMLNode(_Doc.DocumentElement, anErrorMsg)
  else
    anErrorMsg := Format('Not enable to open file <%s>', [aFilename]);

  if _Doc <> nil then
    _Doc.Free;
end;

function TLBApplicationBoostrap.LoadConfigurationFromINIFile(const aFilename: String; out anErrorMsg: String): Boolean;
var
  _IniF: TIniFile;
  _LogCfg : TLoggerConfig;
  _WebCfg : TWebServerConfig;

begin
  Result := False;
  anErrorMsg := '';

  if FileExists(aFilename) then
  begin
    try
      _IniF := TIniFile.Create(aFilename);

      // LBLogger
      if _IniF.SectionExists(cINI_CFG_SEC_LOGGER) then
      begin
        _LogCfg.LogFile := _IniF.ReadString(cINI_CFG_SEC_LOGGER, cINI_CFG_KEY_LOGFILE, '');
        _LogCfg.LogLevel := _IniF.ReadInteger(cINI_CFG_SEC_LOGGER, cINI_CFG_KEY_LOGLEVEL, 1);
        _LogCfg.MaxFileSize := _IniF.ReadInt64(cINI_CFG_SEC_LOGGER, cINI_CFG_KEY_LOGFILESIZE, cDefaultLogFileSize);

        Self.setUpLogger(@_LogCfg);
      end;

      // LBmicroWebServer
      if _IniF.SectionExists(cINI_CFG_SEC_WEBSERVER) then
      begin
        _WebCfg.Port := _IniF.ReadInteger(cINI_CFG_SEC_WEBSERVER, cINI_CFG_KEY_PORT, 0);
        _WebCfg.DocumentRoot := _IniF.ReadString(cINI_CFG_SEC_WEBSERVER, cINI_CFG_KEY_DOCUMENTROOT, '');

        Self.setUpWebServer(@_WebCfg);
      end;

      Result := Self.LoadConfigurationFromINIFileInternal(_IniF, anErrorMsg);

    except
      on E: Exception do
        anErrorMsg := 'Error reading INI file: ' + E.Message;
    end;

    _IniF.Free;
  end
  else
    anErrorMsg := 'INI file not found: ' + aFilename;
end;

procedure TLBApplicationBoostrap.setUpLogger(aConfiguration: pLoggerConfig);
var
  _LogFile : String;
  _LogLevel : Integer;

begin
  if aConfiguration <> nil then
  begin
    if aConfiguration^.LogFile = '' then
      ReleaseLogger()
    else begin

      _LogFile := ResolvePath(aConfiguration^.LogFile);

       _LogLevel := aConfiguration^.LogLevel;
       if _LogLevel <= 0 then
         _LogLevel := 1;

       if LBLogger = nil then
         InitLogger(_LogLevel, _LogFile, False, False)
       else begin
          LBLogger.setLogFile(PChar(_LogFile));
          LBLogger.set_MaxLogLevel(_LogLevel);
       end;

       if aConfiguration^.MaxFileSize > 0 then
         LBLogger.MaxFileSize := aConfiguration^.MaxFileSize;
    end;
  end;
end;

procedure TLBApplicationBoostrap.setUpWebServer(aConfiguration: pWebServerConfig);
var
  _DocRoot : String;

begin
  if aConfiguration <> nil then
  begin
    FreeAndNil(FWebServer);
    if aConfiguration^.Port > 0 then
    begin
      FWebServer := TLBmicroWebServer.Create;
      if aConfiguration^.DocumentRoot <> '' then
      begin
        _DocRoot := ResolvePath(aConfiguration^.DocumentRoot);
        if _DocRoot <> '' then
          FWebServer.DocumentsFolder.DocumentFolder := _DocRoot;
      end;
      Self.startingWebServer();
      FWebServer.Activate(aConfiguration^.Port, nil);
    end;
  end;
end;

function TLBApplicationBoostrap.getApplicationDescription(): String;
begin
  Result := cBootstrapApplicationDescription;
end;

procedure TLBApplicationBoostrap.startingWebServer();
begin
  //
end;

destructor TLBApplicationBoostrap.Destroy;
begin
  FreeAndNil(FWebServer);
  ReleaseLogger();
  inherited Destroy;
end;

function TLBApplicationBoostrap.LoadConfiguration(const aFilename: String; out anErrorMsg: String): Boolean;
var
  _Ext : String;

begin
  Result := False;
  anErrorMsg := '';

  if aFilename <> '' then
  begin
    if FileExists(aFilename) then
    begin
      _Ext := LowerCase(ExtractFileExt(aFileName));
      case _Ext of
        cExtension_INIFile : Result := Self.LoadConfigurationFromINIFile(aFilename, anErrorMsg);
        cExtension_XMLFile : Result := Self.LoadConfigurationFromXMLFile(aFilename, anErrorMsg);

        else                 anErrorMsg := 'Unsupported file format';
      end
    end
    else
      anErrorMsg := Format('File <%s> not found!', [aFilename]);
  end;
end;

end.

