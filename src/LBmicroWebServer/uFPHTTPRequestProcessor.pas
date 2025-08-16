unit uFPHTTPRequestProcessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, HTTPDefs;

type

  { TRequestEx }

  TRequestEx = class(TRequest)
    public
      procedure InitContentFromStream(aStream: TStream);
      function setupRequest(): Boolean;
  end;

  { TResponseEx }

  TResponseEx = class(TResponse)
    public
      procedure retrieveHeaders(Headers: TStringList);
      procedure retrieveContent(aContentStream: TStream);
  end;

  { TFPHTTPRequestProcessor }

  TFPHTTPRequestProcessor = class(TRequestChainProcessor)
    strict private
      function getAnswer(aResponseHeaders: TStringList; var aResponseData: TMemoryStream; out aResponseCode: Integer): Boolean;

    strict protected
      FRequest : TRequestEx;
      FResponse : TResponseEx;

      function DoProcessGETRequest(
        var Resource: String;
        Headers: TStringList;
        URIParams: TStringList;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function DoProcessPOSTRequest(
        var Resource: String;
        Headers: TStringList;
        Payload: TMemoryStream;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function elaborateRequest(): Boolean; virtual; abstract;

    public
      constructor Create;
      destructor Destroy; override;

  end;

implementation

uses
  ULBLogger;

{ TRequestEx }

procedure TRequestEx.InitContentFromStream(aStream: TStream);
var
  _Content : AnsiString;

begin
  SetLength(_Content, aStream.Size);
  aStream.Read(_Content[1], Length(_Content));
  Self.InitContent(_Content);
end;

function TRequestEx.setupRequest(): Boolean;
begin
  Result := False;

  try
    Self.InitRequestVars;
    Result := True;
  except
    on E: Exception do
      LBLogger.Write(1, 'TRequestEx.setupRequest', lmt_Error, E.Message);
  end;
end;

{ TResponseEx }

procedure TResponseEx.retrieveHeaders(Headers: TStringList);
begin
  if Headers <> nil then
    Self.CollectHeaders(Headers);
end;

procedure TResponseEx.retrieveContent(aContentStream: TStream);
begin
  if aContentStream <> nil then
  begin
    if (Self.ContentStream <> nil) and (Self.ContentStream.Size > 0) then
    begin
      aContentStream.Position := 0;
      aContentStream.CopyFrom(Self.ContentStream, Self.ContentStream.Size);
    end;
  end
  else
    LBLogger.Write(1, 'TResponseEx.retrieveContent', lmt_Warning, 'Destination stream not set!');
end;

{ TFPHTTPRequestProcessor }

function TFPHTTPRequestProcessor.getAnswer(aResponseHeaders: TStringList;
  var aResponseData: TMemoryStream; out aResponseCode: Integer): Boolean;
begin
  Result := False;

  try
    if Self.elaborateRequest() then
    begin
      if FResponse <> nil then
      begin
        Result := True;
        if aResponseData = nil then
          aResponseData := TMemoryStream.Create
        else
          aResponseData.Clear;

        aResponseCode := FResponse.Code;
        FResponse.retrieveHeaders(aResponseHeaders);
        FResponse.retrieveContent(aResponseData);
      end;
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TFPHTTPRequestProcessor.getAnswer', lmt_Error, E.Message);
  end;
end;

function TFPHTTPRequestProcessor.DoProcessGETRequest(var Resource: String;
  Headers: TStringList; URIParams: TStringList; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
  FRequest.Method := 'GET';
  FRequest.LoadFromStrings(Headers, True);  // Loading Headers
  FRequest.setupRequest();
  Result := Self.getAnswer(ResponseHeaders, ResponseData, ResponseCode);
end;

function TFPHTTPRequestProcessor.DoProcessPOSTRequest(var Resource: String;
  Headers: TStringList; Payload: TMemoryStream; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
 FRequest.Method := 'POST';
 FRequest.LoadFromStrings(Headers, True);  // Loading Headers
 FRequest.InitContentFromStream(PayLoad);  // Loading Content
 FRequest.setupRequest();
 Result := Self.getAnswer(ResponseHeaders, ResponseData, ResponseCode);
end;

constructor TFPHTTPRequestProcessor.Create;
begin
  inherited Create;
  FRequest := TRequestEx.Create;
  FResponse := nil;
end;

destructor TFPHTTPRequestProcessor.Destroy;
begin
  try

    FreeAndNil(FRequest);

    if FResponse <> nil then
      FreeAndNil(FResponse);

  except
    on E: Exception do
      LBLogger.Write(1, 'TFPHTTPRequestProcessor.Destroy', lmt_Error, E.Message);
  end;

  inherited Destroy;
end;

end.

