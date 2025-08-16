unit uHTTPRequestManagerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uLBmicroWebServer, blcksock, Laz2_DOM;

type


  { THTTPRequestManagerTests }

  THTTPRequestManagerTests = class(TTestCase)
  private
    DummyFilePath: String;
    procedure CreateDummyFile;
    procedure DeleteDummyFile;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_TestRequest;
    procedure Test_DownloadDummyFile;
    procedure Test_RequestForWrongFile;
    procedure Test_GETParameters;
    procedure Test_WrongGETParams;
    procedure Test_POSTRequest;
    procedure Test_WrongPOSTRequest;

  end;

implementation

uses
  ULBLogger, httpsend, uTestRunner, fpjson;

procedure THTTPRequestManagerTests.CreateDummyFile;
var
  fs: TFileStream;


begin
  DummyFilePath := GetTempDir + 'file_test.txt';
  fs := TFileStream.Create(DummyFilePath, fmCreate);
  fs.WriteBuffer(cHelloWord[1], Length(cHelloWord));
  fs.Free;
end;

procedure THTTPRequestManagerTests.DeleteDummyFile;
begin
  if FileExists(DummyFilePath) then
    DeleteFile(DummyFilePath);
end;

procedure THTTPRequestManagerTests.SetUp;
begin
  CreateDummyFile;
end;

procedure THTTPRequestManagerTests.TearDown;
begin
end;

procedure THTTPRequestManagerTests.Test_TestRequest;
var
  _Strings : TStringList;

begin
  // Sleep(5000);
  LBLogger.Write(1, 'THTTPRequestManagerTests.Test_TestRequest', lmt_Debug, '------------------------> Started');
//  Application.WebServer.OnGETRequest := nil;
  _Strings := TStringList.Create;
  LBLogger.Write(1, 'THTTPRequestManagerTests.Test_TestRequest', lmt_Debug, 'Sending test request');
  AssertTrue(HttpGetText('http://127.0.0.1:10320/test', _Strings));
  LBLogger.Write(1, 'THTTPRequestManagerTests.Test_TestRequest', lmt_Debug, 'Answer received');
  AssertTrue(_Strings.Count >= 1);
  LBLogger.Write(1, 'THTTPRequestManagerTests.Test_TestRequest', lmt_Debug, 'Received: %s', [_Strings.Strings[0]]);
  AssertTrue(_Strings.Strings[0] = '<!DOCTYPE html><html><head></head><body><br>Micro-WebServer working! ;-)</body></html>');
  _Strings.Free;
  LBLogger.Write(1, 'THTTPRequestManagerTests.Test_TestRequest', lmt_Debug, '------------------------> Done');
end;

procedure THTTPRequestManagerTests.Test_DownloadDummyFile;
var
  _Strings : TStringList;

begin
//  Application.WebServer.OnGETRequest := nil;
  _Strings := TStringList.Create;

  AssertTrue(HttpGetText('http://127.0.0.1:10320/file_test.txt', _Strings));
  AssertTrue(_Strings.Count >= 1);
  AssertTrue(_Strings.Strings[0] = cHelloWord);

  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_RequestForWrongFile;
var
  _Strings : TStringList;
begin
  _Strings := TStringList.Create;

  AssertFalse(HttpGetText('http://127.0.0.1:10320/file_test1.txt', _Strings));
  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_GETParameters;
var
  _Strings : TStringList;
begin
  _Strings := TStringList.Create;

  AssertTrue('Request', HttpGetText('http://127.0.0.1:10320/hello?name=luca&id=42', _Strings));
  AssertTrue('Lines', _Strings.Count >= 1);
  AssertTrue('First line', _Strings.Strings[0] = Format(cHelloAnswer, ['luca', '42']));

  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_WrongGETParams;
var
  _Strings : TStringList;
begin
  _Strings := TStringList.Create;

  AssertFalse(HttpGetText('http://127.0.0.1:10320/whois?name=luca&id=42', _Strings));
//  AssertTrue(_Strings.Count = 1);
//  AssertTrue(_Strings.Strings[0] = Format(cUnknownResource, ['/whois']));

  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_POSTRequest;
var
  _String : TStringStream;
  _Obj : TJSONObject;
  _Answer : TJSONObject;

begin

  _Obj := TJSONObject.Create;
  _Obj.Add('Code', '1234');
  _Obj.CompressedJSON := True;

  _Answer := TJSONObject.Create;
  _Answer.Add('Received', '1234');
  _Answer.CompressedJSON := True;

  _String := TStringStream.Create();
  AssertTrue(HttpPostURL('http://127.0.0.1:10320/', _Obj.AsJSON, _String));
  AssertTrue(_String.DataString = _Answer.AsJSON);

  _Answer.Free;
  _Obj.Free;

end;

procedure THTTPRequestManagerTests.Test_WrongPOSTRequest;
var
  _String : TStringStream;
  _Obj : TJSONObject;
  _Answer : TJSONObject;

begin
//  Application.WebServer.OnPOSTRequest := @Self.ElaboratePOSTRequest;

  _Obj := TJSONObject.Create;
  _Obj.Add('Unknown', '1234');
  _Obj.CompressedJSON := True;

  _Answer := TJSONObject.Create;
  _Answer.Add('Error', cCodeValueNotFound);
  _Answer.CompressedJSON := True;

  _String := TStringStream.Create();
  AssertTrue(HttpPostURL('http://127.0.0.1:10320/', _Obj.AsJSON, _String));
  AssertTrue(_String.DataString = _Answer.AsJSON);

  _Answer.Free;
  _Obj.Free;
end;


initialization
  RegisterTest(THTTPRequestManagerTests);
end.

