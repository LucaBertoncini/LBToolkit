unit uHTTPRequestParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBCircularBuffer;

type
  TParserResult = (prIncomplete, prComplete, prError);

  TParserState = (
    psRequestLine_Start,
    psRequestLine_Method,
    psRequestLine_URI,
    psRequestLine_HTTPVersion,
    psRequestLine_End,
    psHeader_Start,
    psHeader_Name,
    psHeader_Value,
    psHeader_End,
    psBody_Identity,
    psComplete,
    psError
  );

  { THTTPRequestParser }

  THTTPRequestParser = class(TObject)
  private
    FBuffer: TLBCircularBuffer;
    FParams: TStringList;
    FState: TParserState;
    FHeaders: TStringList;
    FMethod: String;
    FURI: String;
    FHTTPVersion: String;
    FBody: TMemoryStream;
    FContentLength: Int64;
    FMaxHeaderSize: Cardinal;
    FMaxBodySize: Cardinal;
    FCurrentHeaderSize: Cardinal;
    FCurrentLine: AnsiString;
    FRawRequestLine : String;
    FResource: String;
    FUploadedFiles: TStringList;

    procedure ProcessRequestLine;
    procedure ProcessHeaderLine;
    function ParseHeaders: TParserResult;
    function ParseBody: TParserResult;
    function FindCRLF(out aPos: Integer): Boolean;

  public
    constructor Create(aBuffer: TLBCircularBuffer);
    destructor Destroy; override;

    function Parse(aParseBody: Boolean = True): TParserResult;
    procedure Reset;

    function SplitURIIntoResourceAndParameters(): Boolean;

    property Resource: String read FResource;
    property Params: TStringList read FParams;

    property Method: String read FMethod;
    property URI: String read FURI;
    property HTTPVersion: String read FHTTPVersion;
    property Headers: TStringList read FHeaders;
    property Body: TMemoryStream read FBody;
    property MaxHeaderSize: Cardinal read FMaxHeaderSize write FMaxHeaderSize;
    property MaxBodySize: Cardinal read FMaxBodySize write FMaxBodySize;
    property UploadedFiles: TStringList read FUploadedFiles;

    property State: TParserState read FState;

    property RawRequestLine: String read FRawRequestLine;
  end;

implementation

uses
  ULBLogger;


{ THTTPRequestParser }

constructor THTTPRequestParser.Create(aBuffer: TLBCircularBuffer);
begin
  inherited Create;
  FBuffer := aBuffer;

  FHeaders := TStringList.Create;
  FHeaders.CaseSensitive := False;
  FHeaders.NameValueSeparator := ':';

  FParams := TStringList.Create;
  FParams.NameValueSeparator := '=';

  FRawRequestLine := '';

  FUploadedFiles := TStringList.Create;

  FBody := TMemoryStream.Create;
  FMaxHeaderSize := 16 * 1024; // 16KB default
  FMaxBodySize := 10 * 1024 * 1024; // 10MB default

  Self.Reset;
end;

destructor THTTPRequestParser.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  FreeAndNil(FParams);
  FreeAndNil(FUploadedFiles);

  inherited Destroy;
end;

procedure THTTPRequestParser.Reset;
begin
  FState := psRequestLine_Start;
  FHeaders.Clear;
  FBody.Clear;
  FMethod := '';
  FURI := '';
  FHTTPVersion := '';
  FContentLength := -1;
  FCurrentHeaderSize := 0;
  FCurrentLine := '';
  FResource := '';
  FParams.Clear;
  FUploadedFiles.Clear;
end;

function THTTPRequestParser.SplitURIIntoResourceAndParameters(): Boolean;
var
  i: integer;
  _tokens: TStringArray;
  _Params : TStringArray;

begin
  Result := FResource <> '';

  if not Result then
  begin

    FResource := FURI;
    FParams.Clear;

    _tokens := FURI.Split(['?']);

    if High(_tokens) = 0 then Exit(True);

    FResource := _tokens[0].Trim;

    if High(_tokens) >= 1 then
    begin
      _Params := _tokens[1].Split('&');

      for i := 0 to High(_Params) do
        FParams.Add(_Params[i].Trim);
    end;

    Result := True;
  end;
end;

function THTTPRequestParser.FindCRLF(out aPos: Integer): Boolean;
const
  CRLF = #13#10;
begin
  aPos := FBuffer.FindPattern(@CRLF[1], 2, 0);
  Result := aPos >= 0;
end;

procedure THTTPRequestParser.ProcessRequestLine;
var
  s: String;
  p1, p2: Integer;
begin
  s := FCurrentLine;

  // Method
  p1 := Pos(' ', s);
  if p1 = 0 then begin FState := psError; Exit; end;
  FMethod := UpperCase(Trim(Copy(s, 1, p1 - 1)));

  // URI
  s := Trim(Copy(s, p1 + 1, Length(s)));
  p2 := Pos(' ', s);
  if p2 = 0 then begin FState := psError; Exit; end;
  FURI := Trim(Copy(s, 1, p2 - 1));

  // Version
  FHTTPVersion := UpperCase(Trim(Copy(s, p2 + 1, Length(s))));

  if (FMethod = '') or (FURI = '') or (Pos('HTTP/', FHTTPVersion) <> 1) then
  begin
    FState := psError;
    LBLogger.Write(1, 'THTTPRequestParser.ProcessRequestLine', lmt_Warning, 'Invalid request line: ' + FCurrentLine);
  end
  else
    FRawRequestLine := FCurrentLine;
end;

procedure THTTPRequestParser.ProcessHeaderLine;
var
  headerName, headerValue: String;
  p: Integer;
begin
  p := Pos(':', FCurrentLine);
  if p > 0 then
  begin
    headerName := Trim(Copy(FCurrentLine, 1, p - 1));
    headerValue := Trim(Copy(FCurrentLine, p + 1, Length(FCurrentLine)));
    FHeaders.Add(headerName + ':' + headerValue);
  end;
end;

function THTTPRequestParser.ParseHeaders: TParserResult;
var
  linePos: Integer;
  lineLen: Integer;
begin
  Result := prIncomplete;

  while Self.FindCRLF(linePos) do
  begin
    lineLen := linePos;
    FCurrentHeaderSize += lineLen + 2;
    if FCurrentHeaderSize > FMaxHeaderSize then
    begin
      FState := psError;
      Result := prError;
      LBLogger.Write(1, 'THTTPRequestParser.ParseHeaders', lmt_Warning, 'Max header size exceeded.');
      Exit;
    end;

    SetLength(FCurrentLine, lineLen);
    if lineLen > 0 then
      FBuffer.Read(@FCurrentLine[1], lineLen);
    FBuffer.Skip(2); // Skip CRLF

    if FState = psRequestLine_Start then
    begin
      ProcessRequestLine;
      if FState = psError then
      begin
        Result := prError;
        Exit;
      end;
      FState := psHeader_Start;
    end
    else begin
      if lineLen = 0 then // End of headers
      begin
        FState := psHeader_End;
        Result := prComplete; // Headers are complete
        Exit;
      end;
      Self.ProcessHeaderLine;
    end;
  end;
end;

function THTTPRequestParser.ParseBody: TParserResult;
var
  bytesToRead, bytesRead: Int64;
  tempBuffer: array[0..4095] of Byte;

begin
  Result := prIncomplete;

  if FContentLength > 0 then
  begin
    if FContentLength <= FMaxBodySize then
    begin
      bytesToRead := FContentLength - FBody.Size;
      if bytesToRead > 0 then
      begin
        bytesRead := FBuffer.AvailableForRead;
        if bytesRead > bytesToRead then
          bytesRead := bytesToRead;

        if bytesRead > 0 then
        begin
          // Read in chunks to avoid large memory allocation for tempBuffer
          while bytesRead > 0 do
          begin
            if bytesRead > SizeOf(tempBuffer) then
            begin
              FBuffer.Read(@tempBuffer[0], SizeOf(tempBuffer));
              FBody.WriteBuffer(tempBuffer[0], SizeOf(tempBuffer));
              bytesRead := bytesRead - SizeOf(tempBuffer);
            end
            else begin
              FBuffer.Read(@tempBuffer[0], bytesRead);
              FBody.WriteBuffer(tempBuffer[0], bytesRead);
              bytesRead := 0;
            end;
          end;
        end;
      end;

      if FBody.Size >= FContentLength then
      begin
        FState := psComplete;
        Result := prComplete;
        FBody.Position := 0;
      end;

    end
    else begin
      FState := psError;
      Result := prError;
      LBLogger.Write(1, 'THTTPRequestParser.ParseBody', lmt_Warning, 'Wrong content length %d  -  Max allowed %d', [FContentLength, FMaxBodySize]);
    end;
  end
  else begin
    // No body to read
    FState := psComplete;
    Result := prComplete;
  end;
end;


function THTTPRequestParser.Parse(aParseBody: Boolean): TParserResult;
var
  clHeader: String;
begin
  Result := prIncomplete;

  if FState < psHeader_End then
  begin
    if ParseHeaders = prComplete then
    begin
      // Headers are parsed, now check for body
      clHeader := FHeaders.Values['Content-Length'];
      if clHeader <> '' then
      begin
        FContentLength := StrToInt64Def(clHeader, -1);
        if (FContentLength < 0) or (aParseBody and (FContentLength > FMaxBodySize)) then
        begin
          FState := psError;
          Result := prError;
          LBLogger.Write(1, 'THTTPRequestParser.Parse', lmt_Warning, 'Invalid or too large Content-Length. Content length: %d  -  Max allowed: %d', [FContentLength, FMaxBodySize]);
          Exit;
        end;
      end
      else
        FContentLength := 0;

      // Security check for Transfer-Encoding
      if FHeaders.IndexOfName('Transfer-Encoding') > -1 then
      begin
        FState := psError;
        Result := prError;
        LBLogger.Write(1, 'THTTPRequestParser.Parse', lmt_Warning, 'Transfer-Encoding is not supported.');
        Exit;
      end;

      FState := psBody_Identity;

      if not aParseBody then
      begin
        Result := prComplete;
        Exit;
      end;

    end
    else if FState = psError then
    begin
      Result := prError;
      Exit;
    end
    else
      Exit; // Incomplete
  end;


  if aParseBody and (FState = psBody_Identity) then
    Result := Self.ParseBody
  else if not aParseBody then
    Result := prComplete;

  if FState = psComplete then
    Result := prComplete;
end;

end.

