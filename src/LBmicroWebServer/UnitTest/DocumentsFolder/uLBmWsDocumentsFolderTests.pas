unit uLBmWsDocumentsFolderTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uLBmWsDocumentsFolder, Laz2_DOM;

type

  { TLBmWsDocumentsFolderTests }

  TLBmWsDocumentsFolderTests = class(TTestCase)
  private
    FDoc: TXMLDocument;
    FFolder: TLBmWsDocumentsFolder;
    FBasePath: String;
    procedure CreateXMLNode(const FolderValue, UploadFlag: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    constructor Create; override;
    destructor Destroy; override;

  published
    procedure Test_LoadFromXML_ValidFolder;
    procedure Test_LoadFromXML_MissingFolderNode;
    procedure Test_isValidSubpath_Valid;
    procedure Test_isValidSubpath_InvalidTraversal;
    procedure Test_isValidSubpath_Empty;
    procedure Test_RetrieveFilename_Valid;
    procedure Test_RetrieveFilename_PathTraversal;
  end;

implementation

uses
  ULBLogger;

procedure TLBmWsDocumentsFolderTests.CreateXMLNode(const FolderValue, UploadFlag: String);
var
  Root, FolderNode, UploadNode: TDOMElement;
begin
  Root := FDoc.CreateElement(TLBmWsDocumentsFolder.cRootNodeName);
  FDoc.AppendChild(Root);

  FolderNode := FDoc.CreateElement(TLBmWsDocumentsFolder.cNodeName_Folder);
  FolderNode.AppendChild(FDoc.CreateTextNode(FolderValue));
  Root.AppendChild(FolderNode);

  if UploadFlag <> '' then
  begin
    UploadNode := FDoc.CreateElement(TLBmWsDocumentsFolder.cNodeName_EnableUpload);
    UploadNode.AppendChild(FDoc.CreateTextNode(UploadFlag));
    Root.AppendChild(UploadNode);
  end;
end;

procedure TLBmWsDocumentsFolderTests.SetUp;
begin
  FDoc := TXMLDocument.Create;
  FFolder := TLBmWsDocumentsFolder.Create;
  FBasePath := IncludeTrailingPathDelimiter(GetTempDir + 'test_docs');

  if not DirectoryExists(FBasePath) then
    CreateDir(FBasePath);
end;

procedure TLBmWsDocumentsFolderTests.TearDown;
begin
  FFolder.Free;
  FDoc.Free;
  if DirectoryExists(FBasePath) then
    RemoveDir(FBasePath);
end;

constructor TLBmWsDocumentsFolderTests.Create;
begin
  inherited Create;
  InitLogger(5, 'Test_DocumentsFolder.log');
end;

destructor TLBmWsDocumentsFolderTests.Destroy;
begin
  ReleaseLogger();
  inherited Destroy;
end;

procedure TLBmWsDocumentsFolderTests.Test_LoadFromXML_ValidFolder;
var
  _Root : TDOMNode;

begin
  CreateXMLNode(FBasePath, '1');
  _Root := FDoc.DocumentElement;
  AssertTrue(FFolder.LoadFromXMLNode(_Root));
  AssertEquals(FBasePath, FFolder.DocumentFolder);
  AssertTrue(FFolder.EnableUpload);
end;

procedure TLBmWsDocumentsFolderTests.Test_LoadFromXML_MissingFolderNode;
var
  _Root : TDOMNode;

begin
  CreateXMLNode('', '');
  _Root := FDoc.DocumentElement;


  AssertFalse(FFolder.LoadFromXMLNode(_Root));
  AssertEquals('', FFolder.DocumentFolder);
end;

procedure TLBmWsDocumentsFolderTests.Test_isValidSubpath_Valid;
begin
  AssertTrue(FFolder.isValidSubpath('images/photo.jpg'));
end;

procedure TLBmWsDocumentsFolderTests.Test_isValidSubpath_InvalidTraversal;
begin
  AssertFalse(FFolder.isValidSubpath('../private.txt'));
end;

procedure TLBmWsDocumentsFolderTests.Test_isValidSubpath_Empty;
begin
  AssertFalse(FFolder.isValidSubpath(''));
end;

procedure TLBmWsDocumentsFolderTests.Test_RetrieveFilename_Valid;
var
  FullPath: String;
begin
  FFolder.DocumentFolder := FBasePath;
  FullPath := FFolder.RetrieveFilename('/image.jpg');
  AssertTrue(FullPath.StartsWith(FBasePath, True));
end;

procedure TLBmWsDocumentsFolderTests.Test_RetrieveFilename_PathTraversal;
var
  Path: String;
begin
  FFolder.DocumentFolder := FBasePath;
  Path := FFolder.RetrieveFilename('/../secret.txt');
  AssertEquals('', Path);
end;

initialization
  RegisterTest(TLBmWsDocumentsFolderTests);

end.

