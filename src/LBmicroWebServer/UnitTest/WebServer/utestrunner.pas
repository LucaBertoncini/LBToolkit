unit uTestRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, consoletestrunner, uLBmicroWebServer, uLBmWsDocumentsFolder,
  uHTTPRequestParser;

type
  { TTestChainProcessor }

  TTestChainProcessor = class(TRequestChainProcessor)
    strict protected
      function DoProcessGETRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function DoProcessPOSTRequest(
        HTTPParser: THTTPRequestParser;
        Payload: TMemoryStream;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  strict private
    FWebServer : TLBmicroWebServer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property WebServer: TLBmicroWebServer read FWebServer;
  end;

var
  Application: TMyTestRunner;

const
  cHelloWord = String('HELLO WORLD!');
  cHelloAnswer = String('<!DOCTYPE html><html><head></head><body><br>Hello %s, your ID is %s</body></html>');
  cUnknownResource = String('Unknown resource %s');
  cCodeValueNotFound = String('Code value not found');


implementation

uses
  ULBLogger,fpjson, uHTTPConsts;





constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitLogger(5, 'Test_mWs.log');

  FWebServer := TLBmicroWebServer.Create;
  FWebServer.createDocumentFolder();
  FWebServer.DocumentsFolder.DocumentFolder := GetTempDir;
  FWebServer.addChainProcessor(TTestChainProcessor.Create, True);
  FWebServer.ListeningPort := 10320;
  FWebServer.Activate();
  Sleep(10);
end;

destructor TMyTestRunner.Destroy;
begin
  FreeAndNil(FWebServer);
  ReleaseLogger();
  inherited Destroy;
end;


{ TTestChainProcessor }

function TTestChainProcessor.DoProcessGETRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream;
  out ResponseCode: Integer): Boolean;
var
  _Text : String;

begin
  Result := False;

  HTTPParser.SplitURIIntoResourceAndParameters();

  LBLogger.Write(1, 'TTestChainProcessor.DoProcessGETRequest', lmt_Debug, 'Raw request: <%s>', [HTTPParser.RawRequestLine]);
  LBLogger.Write(1, 'TTestChainProcessor.DoProcessGETRequest', lmt_Debug, 'Resource: <%s>', [HTTPParser.Resource]);

  if HTTPParser.Resource = '/hello' then
  begin
    _Text := Format(cHelloAnswer, [HTTPParser.Params.Values['name'], HTTPParser.Params.Values['id']]);

    LBLogger.Write(1, 'TTestChainProcessor.DoProcessGETRequest', lmt_Debug, 'Answer: %s', [_Text]);

    if ResponseData = nil then
      ResponseData := TMemoryStream.Create;
    ResponseData.Write(_Text[1], Length(_Text));
    ResponseHeaders.Add('Content-Type: text/html');
    ResponseHeaders.Add(HTTP_HEADER_CONTENT_LENGTH + ': ' + IntToStr(Length(_Text)));

    ResponseCode := 200;
    Result := True;
  end
  else
    LBLogger.Write(1, 'TTestChainProcessor.DoProcessGETRequest', lmt_Warning, 'Unknown resource: %s', [HTTPParser.Resource]);
end;

function TTestChainProcessor.DoProcessPOSTRequest(HTTPParser: THTTPRequestParser;
  Payload: TMemoryStream; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Obj : TJSONObject = nil;
  _Answer : TJSONObject = nil;
  _s : String;
  _Value : TJSONData = nil;

begin
  // LBLogger.Write(1, 'TTestChainProcessor.DoProcessPOSTRequest', lmt_Debug, 'Payload: %s', [Payload]);

  if (Payload <> nil) and (Payload.Size > 0) then
  begin
    try
      LBLogger.Write(1, 'TTestChainProcessor.DoProcessPOSTRequest', lmt_Debug, 'Payload size: %d', [Payload.Size]);

      SetLength(_s, Payload.Size);
      Payload.Read(_s[1], Length(_s));

      LBLogger.Write(1, 'TTestChainProcessor.DoProcessPOSTRequest', lmt_Debug, 'JSON request: %s', [_s]);

      _Obj := TJSONObject(GetJSON(_s));
      _Answer := TJSONObject.Create;

      if _Obj.Find('Code', _Value) then
      begin
        _Answer.Add('Received', _Value.AsString);
      end
      else
        _Answer.Add('Error', cCodeValueNotFound);


      _Answer.CompressedJSON := True;
      _s := _Answer.AsJSON;
      LBLogger.Write(1, 'TTestChainProcessor.DoProcessPOSTRequest', lmt_Debug, 'Answer: %s', [_s]);

      if ResponseData = nil then
        ResponseData := TMemoryStream.Create;

      ResponseData.Write(_s[1], Length(_s));

      ResponseCode := 200;
      Result := True;

      _Answer.Free;
      _Obj.Free;

    except
      on E: Exception do
        LBLogger.Write(1, 'TTestChainProcessor.DoProcessPOSTRequest', lmt_Error, E.Message);
    end;
  end
  else
    Result := False;
end;



end.

