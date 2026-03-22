unit uLBmWsDocumentsFolder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM;

type
  { TLBmWsDocumentsFolder }

  TLBmWsDocumentsFolder = class(TObject)
  private
    procedure set_DocumentFolder(AValue: String);
    procedure set_UploadEndpoint(AValue: String);

  protected
    FDocumentFolder: String;
    FUploadEndpoint: String;
    FDefaultFile : String;

    function LoadSpecificSettingsFromXMLNode(aParentNode: TDOMNode): Boolean; virtual;

  public
    constructor Create; virtual;

    function LoadFromXMLNode(aParentNode: TDOMNode): Boolean;
    function isValidSubpath(const aSubPath: String): Boolean;
    function RetrieveFilename(const anURI: String): String;

    property DocumentFolder: String read FDocumentFolder write set_DocumentFolder;
    property UploadEndpoint: String read FUploadEndpoint write set_UploadEndpoint;
    property DefaultFile: String read FDefaultFile write FDefaultFile;

    const
      cRootNodeName       = DOMString('DocumentsFolder');
      cNodeName_Folder    = DOMString('Folder');
      cNodeName_UploadEndpoint = DOMString('UploadEndpoint');
      cNodeName_DefaultFile = DOMString('DefaultFile');
  end;

implementation

uses
  synacode, ULBLogger, uLBFileUtils;

{ TLBmWsDocumentsFolder }

procedure TLBmWsDocumentsFolder.set_DocumentFolder(AValue: String);
begin
  AValue := Trim(AValue);

  if AValue <> '' then
    FDocumentFolder := IncludeTrailingPathDelimiter(ResolvePath(AValue))
  else
    FDocumentFolder := '';
end;

procedure TLBmWsDocumentsFolder.set_UploadEndpoint(AValue: String);
begin
  FUploadEndpoint := Trim(AValue);
  if (FUploadEndpoint <> '') and (FUploadEndpoint[1] <> '/') then
    FUploadEndpoint := '/' + FUploadEndpoint;
end;

constructor TLBmWsDocumentsFolder.Create;
begin
  inherited Create;
  FDocumentFolder := '';
  FUploadEndpoint := '';
end;

function TLBmWsDocumentsFolder.LoadSpecificSettingsFromXMLNode(aParentNode: TDOMNode): Boolean;
begin
  Result := True;
end;

function TLBmWsDocumentsFolder.LoadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _Item: TDOMNode;
  _Folder : String;

begin
  Result := False;

  if aParentNode <> nil then
  begin
    _Item := aParentNode.FindNode(cNodeName_Folder);
    if _Item <> nil then
    begin
      _Folder := Trim(_Item.TextContent);
      if _Folder <> '' then
      begin
        Self.DocumentFolder := ExpandFileName(_Folder);

        if DirectoryExists(FDocumentFolder) then
        begin
          _Item := aParentNode.FindNode(cNodeName_UploadEndpoint);
          if _Item <> nil then
            FUploadEndpoint := Trim(_Item.TextContent)
          else
            FUploadEndpoint := '';

          Result := LoadSpecificSettingsFromXMLNode(aParentNode);
        end
        else
          LBLogger.Write(1, 'TLBmWsDocumentsFolder.LoadFromXMLNode', lmt_Warning, 'Directory not found: <%s>', [FDocumentFolder]);
      end
      else
        LBLogger.Write(1, 'TLBmWsDocumentsFolder.LoadFromXMLNode', lmt_Warning, 'Folder not set!');
    end
    else begin
      FDocumentFolder := '';
      FUploadEndpoint := '';
      LBLogger.Write(1, 'TLBmWsDocumentsFolder.LoadFromXMLNode', lmt_Warning, 'Node <%s> not found!', [cNodeName_Folder]);
    end;
  end;
end;

function TLBmWsDocumentsFolder.isValidSubpath(const aSubPath: String): Boolean;
var
  _Decoded: String;
begin
  _Decoded := Trim(DecodeURL(aSubPath));
  _Decoded := StringReplace(_Decoded, '\', '/', [rfReplaceAll]);
  _Decoded := StringReplace(_Decoded, '//', '/', [rfReplaceAll]);

  Result := (_Decoded <> '') and
            (Pos('..', _Decoded) = 0) and
            (Pos('//', _Decoded) = 0);

  if not Result then
    LBLogger.Write(1, 'TLBmWsDocumentsFolder.isValidSubpath', lmt_Warning, 'Subpath non valido: <%s>', [aSubPath]);
end;

function TLBmWsDocumentsFolder.RetrieveFilename(const anURI: String): String;
var
  _RawURI, _Decoded, _LocalPath: String;

begin
  Result := '';

  if FDocumentFolder <> '' then
  begin
    _RawURI := Trim(anURI);
    if _RawURI <> '' then
    begin

      if _RawURI[1] = '/' then
        Delete(_RawURI, 1, 1);

      _Decoded := DecodeURL(_RawURI);
      _LocalPath := ExpandFileName(FDocumentFolder + _Decoded);

      if Pos(FDocumentFolder, _LocalPath) = 1 then
        Result := _LocalPath
      else
        LBLogger.Write(1, 'TLBmWsDocumentsFolder.RetrieveFilename', lmt_Warning, 'Path traversal found: <%s>', [_LocalPath]);
    end;
  end
  else
    LBLogger.Write(1, 'TLBmWsDocumentsFolder.RetrieveFilename', lmt_Warning, 'Document folder not set!');
end;

end.

