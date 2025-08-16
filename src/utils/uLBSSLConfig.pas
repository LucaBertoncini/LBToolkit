unit uLBSSLConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, IniFiles;

type

  { TSSLConnectionData }

  TSSLConnectionData = class(TObject)
  strict private
    FCertificateFile   : String;
    FPrivateKeyFile    : String;
    FKeyPassword       : String;
    FCACertificateFile : String;

    function get_hasValidData: Boolean;

  public
    procedure Clear;
    function LoadFromXMLNode(aParentNode: TDOMNode): Boolean;
    function SaveIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;

    function LoadFromINIFile(const aINIPath, aSection: String): Boolean;
    function LoadFromINISection(aINIFile: TIniFile; const aSection: String): Boolean;
    function SaveToINIFile(const aINIPath, aSection: String): Boolean;

    property hasValidData: Boolean read get_hasValidData;

    property CertificateFile   : String read FCertificateFile   write FCertificateFile;
    property CACertificateFile : String read FCACertificateFile write FCACertificateFile;
    property PrivateKeyFile    : String read FPrivateKeyFile    write FPrivateKeyFile;
    property KeyPassword       : String read FKeyPassword       write FKeyPassword;

    const
      cDEFAULT_INI_SECTION       = 'SSLData';
      cDEFAULT_XML_NODENAME      = 'SSLData';
      cSSLCertificateNodeName    = 'SSLCertificateFile';
      cSSLCACertificateNodeName  = 'SSLCACertificateFile';
      cSSLPrivateKeyNodeName     = 'SSLPrivateKeyFile';
      cSSLKeyPasswordNodeName    = 'SSLKeyPassword';
  end;

implementation

uses
  uLBLogger;

procedure TSSLConnectionData.Clear;
begin
  FCertificateFile   := '';
  FCACertificateFile := '';
  FPrivateKeyFile    := '';
  FKeyPassword       := '';
end;

function TSSLConnectionData.get_hasValidData: Boolean;
begin
  Result := False;
  if FCertificateFile = '' then Exit;

  if FileExists(FCertificateFile) then
  begin
    if FPrivateKeyFile = '' then Exit(True);

    Result := FileExists(FPrivateKeyFile);
    if not Result then
      LBLogger.Write(1, 'TSSLConnectionData.get_hasValidData', lmt_Warning, 'Private key file <%s> not found!', [FPrivateKeyFile]);

  end
  else
    LBLogger.Write(1, 'TSSLConnectionData.get_hasValidData', lmt_Debug, 'Certificate file <%s> not found', [FCertificateFile]);
end;

function TSSLConnectionData.LoadFromXMLNode(aParentNode: TDOMNode): Boolean;

  function ReadNodeText(Node: TDOMNode; const Name: String): String;
  var
    _Node: TDOMNode;
  begin
    Result := '';
    _Node := Node.FindNode(Name);
    if _Node <> nil then
      Result := _Node.TextContent;
  end;

begin
  Result := False;
  Self.Clear;

  if aParentNode <> nil then
  begin
    FCertificateFile   := ReadNodeText(aParentNode, cSSLCertificateNodeName);
    FPrivateKeyFile    := ReadNodeText(aParentNode, cSSLPrivateKeyNodeName);
    FKeyPassword       := ReadNodeText(aParentNode, cSSLKeyPasswordNodeName);
    FCACertificateFile := ReadNodeText(aParentNode, cSSLCACertificateNodeName);

    Result := Self.hasValidData;
    if not Result then
      LBLogger.Write(1, 'TSSLConnectionData.LoadFromXMLNode', lmt_Warning, 'No valid SSL configuration from XML');
  end;
end;

function TSSLConnectionData.SaveIntoXMLNode(aDoc: TXMLDocument; aParentNode: TDOMNode): Boolean;

  procedure AddElement(const Name, Value: String);
  var
    _item: TDOMElement;
  begin
    if Value <> '' then
    begin
      _item := aDoc.CreateElement(Name);
      _item.TextContent := Value;
      aParentNode.AppendChild(_item);
    end;
  end;

begin
  Result := False;
  if (aDoc <> nil) and (aParentNode <> nil) then
  begin
    AddElement(cSSLCertificateNodeName, FCertificateFile);
    AddElement(cSSLCACertificateNodeName, FCACertificateFile);
    AddElement(cSSLPrivateKeyNodeName, FPrivateKeyFile);
    AddElement(cSSLKeyPasswordNodeName, FKeyPassword);
    Result := True;
  end;
end;

function TSSLConnectionData.LoadFromINIFile(const aINIPath, aSection: String): Boolean;
var
  _IniFile: TIniFile;

begin
  Result := False;
  Self.Clear;

  if FileExists(aINIPath) then
  begin
    _IniFile := TIniFile.Create(aINIPath);
    Result := Self.LoadFromINISection(_IniFile, aSection);
    _IniFile.Free;
  end
  else
    LBLogger.Write(1, 'TSSLConnectionData.LoadFromINIFile', lmt_Warning, 'File <%s> not found!', [aINIPath]);
end;

function TSSLConnectionData.LoadFromINISection(aINIFile: TIniFile; const aSection: String): Boolean;
var
  _Section : String;

begin
  Result := False;

  if aINIFile <> nil then
  begin
    try
      _Section := aSection;
      if _Section = '' then
        _Section := cDEFAULT_INI_SECTION;

      FCertificateFile   := aIniFile.ReadString(_Section, cSSLCertificateNodeName, '');
      FCACertificateFile := aIniFile.ReadString(_Section, cSSLCACertificateNodeName, '');
      FPrivateKeyFile    := aIniFile.ReadString(_Section, cSSLPrivateKeyNodeName, '');
      FKeyPassword       := aIniFile.ReadString(_Section, cSSLKeyPasswordNodeName, '');

      Result := Self.hasValidData;
      if not Result then
        LBLogger.Write(1, 'TSSLConnectionData.LoadFromINISection', lmt_Warning, 'No valid SSL configuration from INI');
    except
      on E: Exception do
        LBLogger.Write(1, 'TSSLConnectionData.LoadFromINISection', lmt_Error, E.message);
    end;
  end;

end;

function TSSLConnectionData.SaveToINIFile(const aINIPath, aSection: String): Boolean;
var
  _IniFile: TIniFile;
begin
  Result := False;

  _IniFile := TIniFile.Create(aINIPath);
  try
    _IniFile.WriteString(aSection, cSSLCertificateNodeName, FCertificateFile);
    _IniFile.WriteString(aSection, cSSLCACertificateNodeName, FCACertificateFile);
    _IniFile.WriteString(aSection, cSSLPrivateKeyNodeName, FPrivateKeyFile);
    _IniFile.WriteString(aSection, cSSLKeyPasswordNodeName, FKeyPassword);
    _IniFile.UpdateFile;
    Result := True;
  except
    on E: Exception do
      LBLogger.Write(1, 'TSSLConnectionData.SaveToINIFile', lmt_Error, E.message);
  end;
  _IniFile.Free;
end;

end.

