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

    function ElaborateCustomGET(const Resource: String; const Headers: TStringList;
      const URIParams: TStringList; var ResponseHeaders: TStringList;
      var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;

    function ElaboratePOSTRequest(
    const URI: String;
    const Headers: TStringList;
    const Payload: AnsiString;
    var ResponseHeaders: TStringList;
    var ResponseData: TMemoryStream;
    out ResponseCode: Integer
  ): Boolean;


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

const
  cHelloWord = String('HELLO WORLD!');
  cHelloAnswer = String('<!DOCTYPE html><html><head></head><body><br>Hello %s, your ID is %s</body></html>');
  cUnknownResource = String('Unknown resource %s');
  cCodeValueNotFound = String('Code value not found');

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

function THTTPRequestManagerTests.ElaborateCustomGET(const Resource: String;
  const Headers: TStringList; const URIParams: TStringList;
  var ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out
  ResponseCode: Integer): Boolean;
var
  _Text : String;

begin
  LBLogger.Write(1, 'THTTPRequestManagerTests.ElaborateCustomGET', lmt_Debug, 'Resource: <%s>', [Resource]);

  if Resource = '/hello' then
  begin
    URIParams.NameValueSeparator := '=';
    _Text := Format(cHelloAnswer, [URIParams.Values['name'], URIParams.Values['id']]);
  end
  else
    _Text := Format(cUnknownResource, [Resource]);

  LBLogger.Write(1, 'THTTPRequestManagerTests.ElaborateCustomGET', lmt_Debug, 'Answer: %s', [_Text]);

  if ResponseData = nil then
    ResponseData := TMemoryStream.Create;
  ResponseData.Write(_Text[1], Length(_Text));
  ResponseHeaders.Add('Content-Type: text/html');

  ResponseCode := 200;
  Result := True;
end;

function THTTPRequestManagerTests.ElaboratePOSTRequest(const URI: String;
  const Headers: TStringList; const Payload: AnsiString;
  var ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out
  ResponseCode: Integer): Boolean;
var
  _Obj : TJSONObject = nil;
  _Answer : TJSONObject = nil;
  _s : String;
  _Value : TJSONData = nil;

begin
  LBLogger.Write(1, 'THTTPRequestManagerTests.ElaboratePOSTRequest', lmt_Debug, 'Payload: %s', [Payload]);

  if Payload <> '' then
  begin
    try
      _Obj := TJSONObject(GetJSON(Payload));
      _Answer := TJSONObject.Create;

      if _Obj.Find('Code', _Value) then
      begin
        _Answer.Add('Received', _Value.AsString);
      end
      else
        _Answer.Add('Error', cCodeValueNotFound);


      _Answer.CompressedJSON := True;
      _s := _Answer.AsJSON;
      LBLogger.Write(1, 'THTTPRequestManagerTests.ElaboratePOSTRequest', lmt_Debug, 'Answer: %s', [_s]);

      if ResponseData = nil then
        ResponseData := TMemoryStream.Create;

      ResponseData.Write(_s[1], Length(_s));

      ResponseCode := 200;
      Result := True;

      _Answer.Free;
      _Obj.Free;

    except
      on E: Exception do
        LBLogger.Write(1, 'THTTPRequestManagerTests.ElaboratePOSTRequest', lmt_Error, E.Message);
    end;
  end
  else
    Result := False;
end;

procedure THTTPRequestManagerTests.SetUp;
begin
  CreateDummyFile;
end;

procedure THTTPRequestManagerTests.TearDown;
begin

//  DeleteDummyFile;
end;

procedure THTTPRequestManagerTests.Test_TestRequest;
var
  _Strings : TStringList;

begin
  Application.WebServer.OnGETRequest := nil;
  _Strings := TStringList.Create;
  AssertTrue(HttpGetText('http://127.0.0.1:10320/test', _Strings));
  AssertTrue(_Strings.Count >= 1);
  AssertTrue(_Strings.Strings[0] = '<!DOCTYPE html><html><head></head><body><br>Micro-WebServer working! ;-)</body></html>');
  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_DownloadDummyFile;
var
  _Strings : TStringList;

begin
  Application.WebServer.OnGETRequest := nil;
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
  Application.WebServer.OnGETRequest := nil;

  _Strings := TStringList.Create;

  AssertFalse(HttpGetText('http://127.0.0.1:10320/file_test1.txt', _Strings));
  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_GETParameters;
var
  _Strings : TStringList;
begin
  Application.WebServer.OnGETRequest := @Self.ElaborateCustomGET;
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
  Application.WebServer.OnGETRequest := @Self.ElaborateCustomGET;
  _Strings := TStringList.Create;

  AssertTrue(HttpGetText('http://127.0.0.1:10320/whois?name=luca&id=42', _Strings));
  AssertTrue(_Strings.Count = 1);
  AssertTrue(_Strings.Strings[0] = Format(cUnknownResource, ['/whois']));

  _Strings.Free;
end;

procedure THTTPRequestManagerTests.Test_POSTRequest;
var
  _String : TStringStream;
  _Obj : TJSONObject;
  _Answer : TJSONObject;

begin
  Application.WebServer.OnPOSTRequest := @Self.ElaboratePOSTRequest;

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
  Application.WebServer.OnPOSTRequest := @Self.ElaboratePOSTRequest;

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

