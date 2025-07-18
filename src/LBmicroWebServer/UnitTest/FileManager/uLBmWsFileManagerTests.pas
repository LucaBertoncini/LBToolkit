unit uLBmWsFileManagerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testutils,
  uLBmWsFileManager, uHTTPConsts, blcksock;

type

  { TLBmWsFileManagerTests }

  TLBmWsFileManagerTests = class(TTestCase)
  private
    FManager: TLBmWsFileManager;
    FHeaders: TStringList;
    FTempFileName: String;
    procedure CreateTempFile(const Content: String);
    procedure DeleteTempFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    constructor Create; override;
    destructor Destroy; override;

  published
    // File setup
    procedure Test_SetFile_Full;
    procedure Test_SetFile_RangeStartEnd;
    procedure Test_SetFile_RangeStartOnly;
    procedure Test_SetFile_RangeEndOnly;
    procedure Test_SetFile_RangeInvalid;
    procedure Test_SetFile_NotFound;

    // Headers
    procedure Test_ResponseHeaders_Full;
    procedure Test_ResponseHeaders_Range;
  end;

implementation

uses
  ULBLogger;

procedure TLBmWsFileManagerTests.CreateTempFile(const Content: String);
var
  fs: TFileStream;
begin
  FTempFileName := GetTempDir + 'test_file.txt';
  fs := TFileStream.Create(FTempFileName, fmCreate);
  fs.Write(Content[1], Length(Content));
  fs.Free;
end;

procedure TLBmWsFileManagerTests.DeleteTempFile;
begin
  if FileExists(FTempFileName) then
    DeleteFile(FTempFileName);
end;

procedure TLBmWsFileManagerTests.SetUp;
begin
  CreateTempFile('ABCDEFGHIJ1234567890'); // 20 byte
  FManager := TLBmWsFileManager.Create;
  FHeaders := TStringList.Create;
end;

procedure TLBmWsFileManagerTests.TearDown;
begin
  FManager.Free;
  FHeaders.Free;
  DeleteTempFile;
end;

constructor TLBmWsFileManagerTests.Create;
begin
  inherited Create;
  InitLogger(5, 'Test_FileManager.log');
end;

destructor TLBmWsFileManagerTests.Destroy;
begin
  ReleaseLogger();

  inherited Destroy;
end;

procedure TLBmWsFileManagerTests.Test_SetFile_Full;
begin
  AssertTrue(FManager.SetFile(FTempFileName, ''));
  AssertEquals(http_Ok, FManager.AnswerStatus);
  AssertEquals(0, FManager.RangeStart);
  AssertEquals(19, FManager.RangeEnd);
end;

procedure TLBmWsFileManagerTests.Test_SetFile_RangeStartEnd;
begin
  AssertTrue(FManager.SetFile(FTempFileName, 'bytes=5-14'));
  AssertEquals(http_PartialContent, FManager.AnswerStatus);
  AssertEquals(5, FManager.RangeStart);
  AssertEquals(14, FManager.RangeEnd);
end;

procedure TLBmWsFileManagerTests.Test_SetFile_RangeStartOnly;
begin
  AssertTrue(FManager.SetFile(FTempFileName, 'bytes=10-'));
  AssertEquals(http_PartialContent, FManager.AnswerStatus);
  AssertEquals(10, FManager.RangeStart);
  AssertEquals(19, FManager.RangeEnd);
end;

procedure TLBmWsFileManagerTests.Test_SetFile_RangeEndOnly;
begin
  AssertTrue(FManager.SetFile(FTempFileName, 'bytes=-5'));
  AssertEquals(http_PartialContent, FManager.AnswerStatus);
  AssertEquals(15, FManager.RangeStart);
  AssertEquals(19, FManager.RangeEnd);
end;

procedure TLBmWsFileManagerTests.Test_SetFile_RangeInvalid;
begin
  AssertFalse(FManager.SetFile(FTempFileName, 'bytes=100-150'));
  AssertEquals(http_RangeNotSatisfiable, FManager.AnswerStatus);
end;

procedure TLBmWsFileManagerTests.Test_SetFile_NotFound;
begin
  AssertFalse(FManager.SetFile('/nonexistent/file.txt', ''));
  AssertEquals(http_NotFound, FManager.AnswerStatus);
end;

procedure TLBmWsFileManagerTests.Test_ResponseHeaders_Full;
begin
  FManager.SetFile(FTempFileName, '');
  FManager.addResponseHeaders(FHeaders);

  FHeaders.NameValueSeparator := ':';

  AssertEquals('text/plain', Trim(FHeaders.Values[cHTTPHeader_ContentType]));
  AssertEquals('20', Trim(FHeaders.Values[cHTTPHeader_ContentLength]));
  AssertEquals('bytes', Trim(FHeaders.Values[cHTTPHeader_AcceptRanges]));
  AssertTrue(FHeaders.IndexOfName(cHTTPHeader_ContentRange) = -1); // assente
end;

procedure TLBmWsFileManagerTests.Test_ResponseHeaders_Range;
begin
  FManager.SetFile(FTempFileName, 'bytes=5-10');
  FManager.addResponseHeaders(FHeaders);

  FHeaders.NameValueSeparator := ':';

  AssertEquals('text/plain', Trim(FHeaders.Values[cHTTPHeader_ContentType]));
  AssertEquals('6', Trim(FHeaders.Values[cHTTPHeader_ContentLength]));
  AssertEquals('bytes', Trim(FHeaders.Values[cHTTPHeader_AcceptRanges]));
  AssertEquals('bytes 5-10/20', Trim(FHeaders.Values[cHTTPHeader_ContentRange]));
end;


initialization
  RegisterTest(TLBmWsFileManagerTests);
end.

