unit uLBWebServerConfigurationLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Laz2_DOM, uLBmicroWebServer;

type
  { TWebServerConfigurationLoader }
  TWebServerConfigurationLoader = class(TObject)
  public
    function LoadConfig(aWebServer: TLBmicroWebServer): Boolean; virtual; abstract;
  end;

  { TINIConfigLoader }
  TINIConfigLoader = class(TWebServerConfigurationLoader)
  strict private
    FFileName: String;
    FSection: String;
    FIniFile : TIniFile;

    const
      cINI_DEFAULT_SECTION  = 'LBWebServer';
      cINI_LISTENING_PORT   = 'Port';
      cINI_DOCUMENTS_FOLDER = 'DocumentFolder';

  public
    function setParams(const aFilename, aSection: String): Boolean;
    function LoadConfig(aWebServer: TLBmicroWebServer): Boolean; override;

    property Section  : String            write FSection;
    property Filename : String            write FFilename;
    property IniFile  : TIniFile          write FIniFile;

  end;

  { TXMLConfigLoader }
  TXMLConfigLoader = class(TWebServerConfigurationLoader)
  strict private
    FFilename: String;
    FXMLNode: TDOMNode;

    const
      cXML_ROOT_NODENAME  = 'LBWebServer';
      cXML_SSLDATA_NODENAME = 'SSLData';
      cXML_DOCFOLDER_NODENAME = 'DocumentFolder';
      cXML_PORT_NODENAME = 'ListeningPort';

  public
    function setFilename(const aFilename: String): Boolean;
    function LoadConfig(aWebServer: TLBmicroWebServer): Boolean; override;

    property XMLNode: TDOMNode write FXMLNode;
    property Filename: String write FFilename;

  end;

  { TCallbackDataConfigLoader }
  TCallbackDataConfigLoader = class(TWebServerConfigurationLoader)
  private
    FFillDataCallback: TNotifyEvent;
    // Internal fields to hold the data pushed by the callback
    FPort: Integer;
    FDocFolder: string;
    FSSLCertFile: string;
    FSSLKeyFile: string;
    FSSLKeyPass: string;
  public
    constructor Create(aCallback: TNotifyEvent);
    function LoadConfig(aWebServer: TLBmicroWebServer): Boolean; override;
    procedure SetData(aPort: Integer; const aDocFolder, aCertFile, aKeyFile, aKeyPass: String);
  end;


implementation

uses
  ULBLogger, uLBmWsDocumentsFolder, uLBSSLConfig, uLBFileUtils;

{ TINIConfigLoader }

function TINIConfigLoader.setParams(const aFilename, aSection: String): Boolean;
begin
  FFileName := aFileName;

  if aSection = '' then
    FSection := cINI_DEFAULT_SECTION
  else
    FSection := aSection;

  Result := True;
end;

function TINIConfigLoader.LoadConfig(aWebServer: TLBmicroWebServer): Boolean;
var
  _IniFile: TIniFile;
  _DocumentFolder: String;
  _Port : Integer;

begin
  Result := False;

  if aWebServer <> nil then
  begin
    try
      _IniFile := FIniFile;
      if _IniFile = nil then
      begin
        if FFileName <> '' then
        begin
          if FileExists(FFileName) then
            _IniFile := TIniFile.Create(FFileName)
          else
            LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Warning, 'Configuration file <%s> not found!', [FFileName]);
        end
        else
          LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Warning, 'Configuration file not set!');
      end;

      if _IniFile <> nil then
      begin
        _Port := _IniFile.ReadInteger(FSection, cINI_LISTENING_PORT, 0);
        if _Port > 0 then
        begin
          Result := True;
          aWebServer.ListeningPort := _Port;

          _DocumentFolder := _IniFile.ReadString(FSection, cINI_DOCUMENTS_FOLDER, '');
          if (_DocumentFolder <> '') then
          begin
            if DirectoryExists(_DocumentFolder) then
            begin
              aWebServer.createDocumentFolder();
              aWebServer.DocumentsFolder.DocumentFolder := _DocumentFolder;
            end
            else begin
              Result := False;
              LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Warning, 'Document folder <%s> not found!', [_DocumentFolder]);
            end;
          end;

          if aWebServer.SSLData <> nil then
            _WaWebServerS.SSLData.LoadFromINISection(_IniFile, FSection);

        end
        else
          LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Warning, 'Wrong port value: %d', [_Port]);
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Error, E.Message);
    end;

    if (_IniFile <> FIniFile) and (_IniFile <> nil) then
      _IniFile.Free;
  end
  else
    LBLogger.Write(1, 'TINIConfigLoader.LoadConfig', lmt_Warning, 'WebServer not set!');
end;

{ TXMLConfigLoader }

function TXMLConfigLoader.setFilename(const aFilename: String): Boolean;
begin
  FFilename := aFilename;
  Result := True;
end;

function TXMLConfigLoader.LoadConfig(aWebServer: TLBmicroWebServer): Boolean;
var
  _Doc : TXMLDocument = nil;
  _Node: TDOMNode;
  _Port : Integer;
  _PortNode : TDOMNode;

begin
  Result := False;

  if aWebServer <> nil then
  begin
    try
      _Node := FXMLNode;
      if _Node = nil then
      begin
        if FileExists(FFilename) then
        begin
          if OpenXMLFile(FFilename, _Doc) then
          begin
            _Node := _Doc.DocumentElement;
            if (_Node.NodeName <> cXML_ROOT_NODENAME) then
            begin
              _Node := _Node.FindNode(cXML_ROOT_NODENAME);
              if _Node = nil then
                LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Warning, 'Root node <%s> not found!', [cXML_ROOT_NODENAME]);
            end;
          end;
        end
        else
          LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Warning, 'Configuration file <%s> not found!', [FFilename]);
      end;

      if _Node <> nil then
      begin
        _PortNode := _Node.FindNode(cXML_PORT_NODENAME);
        if _PortNode <> nil then
        begin
          _Port := StrToIntDef(_PortNode.TextContent, 0);

          if _Port > 0 then
          begin
            aWebServer.ListeningPort := _Port;

            if aWebServer.SSLData <> nil then
              aWebServer.SSLData.LoadFromXMLNode(_Node.FindNode(cXML_SSLDATA_NODENAME));

            _Node := _Node.FindNode(cXML_DOCFOLDER_NODENAME);
            if _Node <> nil then
            begin
              aWebServer.createDocumentFolder();
              aWebServer.DocumentsFolder.LoadFromXMLNode(_Node);
            end;
          end
          else
            LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Warning, 'Wrong port value %d', [_Port]);
        end
        else
          LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Warning, 'Port node <%s> not found!', [cXML_PORT_NODENAME]);
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Error, E.Message);
    end;

    if _Doc <> nil then
      _Doc.Free;
  end
  else
    LBLogger.Write(1, 'TXMLConfigLoader.LoadConfig', lmt_Warning, 'WebServer not set!');
end;

{ TCallbackDataConfigLoader }
constructor TCallbackDataConfigLoader.Create(aCallback: TNotifyEvent);
begin
  inherited Create;
  FFillDataCallback := aCallback;
  FPort := 0;
end;

procedure TCallbackDataConfigLoader.SetData(aPort: Integer; const aDocFolder, aCertFile, aKeyFile, aKeyPass: String);
begin
  FPort        := aPort;
  FDocFolder   := aDocFolder;
  FSSLCertFile := aCertFile;
  FSSLKeyFile  := aKeyFile;
  FSSLKeyPass  := aKeyPass;
end;

function TCallbackDataConfigLoader.LoadConfig(aWebServer: TLBmicroWebServer): Boolean;
begin
  Result := False;

  if FFillDataCallback <> nil then
  begin
    if aWebServer <> nil then
    begin

      try
        FFillDataCallback(Self);

        // use the data that the callback has pushed into this object.
        if FPort > 0 then
        begin
          aWebServer.ListeningPort := FPort;

          if (FDocFolder <> '') then
          begin
            if DirectoryExists(FDocFolder) then
            begin
              aWebServer.createDocumentFolder;
              aWebServer.DocumentsFolder.DocumentFolder := FDocFolder;
            end
            else
              LBLogger.Write(1, 'TCallbackDataConfigLoader.LoadConfig', lmt_Warning, 'Document folder <%s> not found!', [FDocFolder]);
          end;

          if (FSSLCertFile <> '') and (FSSLKeyFile <> '') then
          begin
            if aWebServer.SSLData <> nil then
            begin
              aWebServer.SSLData.CertificateFile := FSSLCertFile;
              aWebServer.SSLData.PrivateKeyFile := FSSLKeyFile;
              aWebServer.SSLData.KeyPassword := FSSLKeyPass;
            end;
          end;

          Result := True;
        end
        else
          LBLogger.Write(1, 'TCallbackDataConfigLoader.LoadConfig', lmt_Warning, 'Wrong port value %d', [FPort]);

      except
        on E: Exception do
          LBLogger.Write(1, 'TCallbackDataConfigLoader.LoadConfig', lmt_Error, 'Callback data loading failed: %s', [E.Message]);
      end;
    end
    else
      LBLogger.Write(1, 'TCallbackDataConfigLoader.LoadConfig', lmt_Warning, 'WebServer not set!');

  end
  else
    LBLogger.Write(1, 'TCallbackDataConfigLoader.LoadConfig', lmt_Warning, 'Fill data callback not set!');
end;

end.

end.
