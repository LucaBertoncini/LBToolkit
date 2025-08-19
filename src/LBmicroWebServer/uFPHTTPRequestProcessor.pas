unit uFPHTTPRequestProcessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, HTTPDefs, uHTTPRequestParser, fphttp;

type
  // Check if the HTTPDefs unit has the SetContentFromString procedure for the TRequest class
  {.$DEFINE useContentFromString}

  { TRequestEx }

  TRequestEx = class(TRequest)
    public
      function setupRequest(ARequest: THTTPRequestParser): Boolean;
  end;

  { TResponseEx }

  TResponseEx = class(TResponse)
    private
      FAnswerHeaders: TStrings;
      FAnswerData: TStream;
    public
      procedure SetInternalData(AnswerHeaders: TStrings; AnswerData: TStream);

      Procedure DoSendHeaders(Headers : TStrings); override;
      Procedure DoSendContent; override;
  end;

  { TFPHTTPRequestProcessor }

  TFPHTTPRequestProcessor = class(TRequestChainProcessor)
    strict private
      function getAnswer(aResponseHeaders: TStringList; var aResponseData: TMemoryStream; out aResponseCode: Integer): Boolean;

    strict protected
      FRequest : TRequestEx;
      FResponse : TResponseEx;
      FOnWebAction: TWebActionEvent;

      function DoProcessRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;


    public
      constructor Create;
      destructor Destroy; override;

      property OnWebAction: TWebActionEvent read FOnWebAction write FOnWebAction;

  end;

implementation

uses
  ULBLogger;

{ TRequestEx }

function TRequestEx.setupRequest(ARequest: THTTPRequestParser): Boolean;
var
  _Content : AnsiString;

begin
  Result := False;


  try
    ParseFirstHeaderLine(ARequest.RawRequestLine);

    SetLength(_Content, ARequest.Body.Size);
    ARequest.Body.Read(_Content[1], Length(_Content));

    {$IFDEF useContentFromString}
    Self.SetContentFromString(_Content);
    {$ELSE}
    Self.InitContent(_Content);
    {$ENDIF}
    Self.InitRequestVars;

    Result := True;
  except
    on E: Exception do
      LBLogger.Write(1, 'TRequestEx.setupRequest', lmt_Error, E.Message);
  end;

end;

procedure TResponseEx.SetInternalData(AnswerHeaders: TStrings; AnswerData: TStream);
begin
  FAnswerHeaders:= AnswerHeaders;
  FAnswerData:= AnswerData;
end;

{ TResponseEx }

procedure TResponseEx.DoSendHeaders(Headers: TStrings);
begin
  FAnswerHeaders.AddStrings(Headers);
end;

procedure TResponseEx.DoSendContent;
var
  _Buff: RawByteString;
begin
  if ContentStream <> nil then
  begin
    if ContentStream.Size > 0 then
    begin
      ContentStream.Seek(0, soFromBeginning);
      FAnswerData.CopyFrom(ContentStream, 0);
    end;
  end
  else
  begin
    if Length(Content) > 0 then
    begin
      _Buff:= Content;
      FAnswerData.Write(_Buff[1], Length(_Buff));
    end;
  end;
end;

{ TFPHTTPRequestProcessor }

function TFPHTTPRequestProcessor.getAnswer(aResponseHeaders: TStringList; var aResponseData: TMemoryStream; out aResponseCode: Integer): Boolean;
var
  _GoAhead: boolean;
begin
  Result := False;
  _GoAhead:= false;

  try
    if aResponseData = nil then
      aResponseData := TMemoryStream.Create
    else
      aResponseData.Clear;

    FResponse:= TResponseEx.Create(FRequest);

    FResponse.SetInternalData(aResponseHeaders, aResponseData);

    if Assigned(FOnWebAction) then
      FOnWebAction(Self, FRequest, FResponse, _GoAhead);

    if _GoAhead then
    begin
      aResponseCode := FResponse.Code;
      Result := True;

      FResponse.SendContent();
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TFPHTTPRequestProcessor.getAnswer', lmt_Error, E.Message);
  end;
end;

function TFPHTTPRequestProcessor.DoProcessRequest(
  HTTPParser: THTTPRequestParser; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
 FRequest.setupRequest(HTTPParser);
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

