unit uLBmWsFileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock;

type

  { TLBmWsFileManager }

  TLBmWsFileManager = class(TObject)
  strict private
    FFileStream      : TFileStream;
    FRangeStart      : Int64;
    FRangeEnd        : Int64;
    FSendHeadersOnly : Boolean;
    FStatusCode      : Integer;
    FBuffer          : TBytes;
    FRemainingBytes  : Int64;

    function ParseRangeHeader(const aRangeHeader: String; aFileSize: Int64): Boolean;

    const
      cMaxBytesToRead = Integer(10 * 1024 * 1024);

  public
    constructor Create;
    destructor Destroy; override;

    function SetFile(const aFilename, aRangeHeader: String): Boolean;

    procedure addResponseHeaders(anHeadersList: TStringList);
    function sendData(aSocket: TTCPBlockSocket): Boolean;

    property AnswerStatus: Integer read FStatusCode;
    property RangeStart: Int64 read FRangeStart;
    property RangeEnd: Int64 read FRangeEnd;

    property SendHeadersOnly: Boolean write FSendHeadersOnly;
  end;

implementation

uses
  ULBLogger, uHTTPConsts;


{ TLBmWsFileManager }

constructor TLBmWsFileManager.Create;
begin
  inherited Create;

  FRemainingBytes  := 0;
  FFileStream      := nil;
  FRangeStart      := 0;
  FRangeEnd        := 0;
  FSendHeadersOnly := False;

  SetLength(FBuffer, cMaxBytesToRead);
end;

destructor TLBmWsFileManager.Destroy;
begin
  SetLength(FBuffer, 0);

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  inherited Destroy;
end;

function TLBmWsFileManager.ParseRangeHeader(const aRangeHeader: String; aFileSize: Int64): Boolean;
const
  cRangePrefix = 'bytes=';

var
  _RangeValue, _StartStr, _EndStr: String;
  _DashPos: Integer;
  _StartVal, _EndVal: Int64;
begin
  Result := False;

  FRangeStart := 0;
  FRangeEnd := aFileSize - 1;

  if (aRangeHeader <> '') and (aRangeHeader.StartsWith(cRangePrefix, True)) then
  begin

    _RangeValue := RightStr(aRangeHeader, Length(aRangeHeader) - Length(cRangePrefix));
    _DashPos := Pos('-', _RangeValue);

    if _DashPos > 0 then
    begin

      _StartStr := Trim(Copy(_RangeValue, 1, _DashPos - 1));
      _EndStr := Trim(Copy(_RangeValue, _DashPos + 1, Length(_RangeValue)));

      // Caso 1: bytes=Start-End
      if (_StartStr <> '') and (_EndStr <> '') then
      begin
        _StartVal := StrToInt64Def(_StartStr, -1);
        _EndVal   := StrToInt64Def(_EndStr, -1);

        if (_StartVal >= 0) and (_EndVal > _StartVal) and (_StartVal < aFileSize) then
        begin
          FRangeStart := _StartVal;
          if _EndVal > aFileSize - 1 then
            FRangeEnd := aFileSize - 1
          else
            FRangeEnd := _EndVal;
//          FRangeEnd := Min(_EndVal, aFileSize - 1);
          FStatusCode := HTTP_STATUS_PARTIAL_CONTENT;
          Result := True;
        end
        else
          FStatusCode := HTTP_STATUS_RANGE_NOT_SATISFIABLE;
      end
      else if (_StartStr <> '') and (_EndStr = '') then // Caso 2: bytes=Start-
      begin
        _StartVal := StrToInt64Def(_StartStr, -1);
        if (_StartVal >= 0) and (_StartVal < aFileSize) then
        begin
          FRangeStart := _StartVal;
          FRangeEnd := aFileSize - 1;
          FStatusCode := HTTP_STATUS_PARTIAL_CONTENT;
          Result := True;
        end
        else
          FStatusCode := HTTP_STATUS_RANGE_NOT_SATISFIABLE;
      end
      else if (_StartStr = '') and (_EndStr <> '') then // Caso 3: bytes=-End
      begin
        _EndVal := StrToInt64Def(_EndStr, -1);
        if _EndVal > 0 then
        begin
          if _EndVal >= aFileSize then
            FRangeStart := 0
          else
            FRangeStart := aFileSize - _EndVal;

          FRangeEnd := aFileSize - 1;
          FStatusCode := HTTP_STATUS_PARTIAL_CONTENT;
          Result := True;
        end
        else
          FStatusCode := HTTP_STATUS_RANGE_NOT_SATISFIABLE;
      end
      else // Caso malformato
        FStatusCode := HTTP_STATUS_RANGE_NOT_SATISFIABLE;
    end
    else
      FStatusCode := HTTP_STATUS_BAD_REQUEST;
  end
  else begin
    FStatusCode := HTTP_STATUS_BAD_REQUEST;
    LBLogger.Write(1, 'TLBmWsFileManager.ParseRangeHeader', lmt_Warning, 'Wrong range header <%s>', [aRangeHeader]);
  end;

end;

function TLBmWsFileManager.SetFile(const aFilename, aRangeHeader: String): Boolean;
begin
  Result := False;
  FRemainingBytes := 0;

  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  try
    if FileExists(aFilename) then
    begin

      FFileStream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyNone);

      FRangeStart := 0;
      FRangeEnd := FFileStream.Size - 1;

      if (aRangeHeader <> '') then
        Result := ParseRangeHeader(aRangeHeader, FFileStream.Size)
      else begin
        FStatusCode := HTTP_STATUS_OK;
        Result := True;
      end;

      if Result then
      begin
        FRemainingBytes := FRangeEnd - FRangeStart + 1;
        FFileStream.Seek(FRangeStart, soFromBeginning);
      end;

    end
    else begin
      FStatusCode := HTTP_STATUS_NOT_FOUND;
      LBLogger.Write(1, 'TLBmWsFileManager.SetFile', lmt_Warning, 'File not found: %s', [aFilename]);
    end;

  except
    on E: Exception do
    begin
      FStatusCode := HTTP_STATUS_INTERNAL_ERROR;
      LBLogger.Write(1, 'TLBmWsFileManager.SetFile', lmt_Error, 'File <%s>: %s', [aFilename, E.Message]);
    end;
  end;

  if not Result then
    FreeAndNil(FFileStream);
end;

procedure TLBmWsFileManager.addResponseHeaders(anHeadersList: TStringList);
var
  _ext : String;
  _ContentType : String;

begin
  if FFileStream <> nil then
  begin
    try

      _ext := Lowercase(ExtractFileExt(FFileStream.FileName));
      case _ext of
        FILE_EXT_HTML   : _ContentType := MIME_TYPE_HTML;
        FILE_EXT_CSS    : _ContentType := MIME_TYPE_CSS;
        FILE_EXT_JS     : _ContentType := MIME_TYPE_JAVASCRIPT;
        FILE_EXT_JSON   : _ContentType := MIME_TYPE_JSON;
        FILE_EXT_XML    : _ContentType := MIME_TYPE_XML;
        FILE_EXT_TXT    : _ContentType := MIME_TYPE_PLAIN_TEXT;

        FILE_EXT_JPG,
        FILE_EXT_JPEG   : _ContentType := MIME_TYPE_JPEG;
        FILE_EXT_PNG    : _ContentType := MIME_TYPE_PNG;
        FILE_EXT_GIF    : _ContentType := MIME_TYPE_GIF;
        FILE_EXT_SVG    : _ContentType := MIME_TYPE_SVG;
        FILE_EXT_WEBP   : _ContentType := MIME_TYPE_WEBP;
        FILE_EXT_ICO    : _ContentType := MIME_TYPE_ICO;

        FILE_EXT_PDF    : _ContentType := MIME_TYPE_PDF;

        FILE_EXT_MP4    : _ContentType := MIME_TYPE_MP4;
        FILE_EXT_WEBM   : _ContentType := MIME_TYPE_WEBM;
        FILE_EXT_MOV    : _ContentType := MIME_TYPE_MOV;
        FILE_EXT_M3U8   : _ContentType := MIME_TYPE_M3U8;
        FILE_EXT_TS     : _ContentType := MIME_TYPE_TS;

        FILE_EXT_WOFF   : _ContentType := MIME_TYPE_WOFF;
        FILE_EXT_WOFF2  : _ContentType := MIME_TYPE_WOFF2;

        else
          _ContentType := MIME_TYPE_OCTET_STREAM; // fallback: application/octet-stream
      end;

      anHeadersList.Add(HTTP_HEADER_CONTENT_TYPE + ': ' + _ContentType);
      anHeadersList.Add(HTTP_HEADER_CONTENT_LENGTH + ': ' + IntToStr(FRangeEnd - FRangeStart + 1));
      anHeadersList.Add(HTTP_HEADER_ACCEPT_RANGES + ': bytes');
      if FStatusCode = HTTP_STATUS_PARTIAL_CONTENT then
        anHeadersList.Add(HTTP_HEADER_CONTENT_RANGE + ': ' + Format('bytes %d-%d/%d', [FRangeStart, FRangeEnd, FFileStream.Size]));

      anHeadersList.Add(HTTP_HEADER_CONNECTION + ': ' + HTTP_CONNECTION_KEEP_ALIVE);

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBmWsFileManager.addResponseHeaders', lmt_Error, E.Message);
    end;

  end
  else
    LBLogger.Write(1, 'TLBmWsFileManager.addResponseHeaders', lmt_Warning, 'File not set!');
end;

function TLBmWsFileManager.sendData(aSocket: TTCPBlockSocket): Boolean;
var
  _BytesRead : Int64;
  _BytesToRead : Int64;

begin
  Result := False;

  if (not FSendHeadersOnly) and (FFileStream <> nil) then
  begin

    try

      if (FRemainingBytes < cMaxBytesToRead) then
        _BytesToRead := FRemainingBytes
      else
        _BytesToRead := cMaxBytesToRead;

      if _BytesToRead > 0 then
      begin
        _BytesRead := FFileStream.Read(FBuffer[0], _BytesToRead);

        if _BytesRead > 0 then
        begin
          aSocket.SendBuffer(@FBuffer[0], _BytesRead);
          Result := aSocket.LastError = 0;
          if Result then
            Dec(FRemainingBytes, _BytesRead)
          else
            LBLogger.Write(1, 'TLBmWsFileManager.sendData', lmt_Warning, 'Error sending file <%s>: <%s>', [FFileStream.FileName, aSocket.LastErrorDesc]);
        end
        else
          LBLogger.Write(1, 'TLBmWsFileManager.sendData', lmt_Debug, 'Error reading file <%s>!', [FFileStream.FileName]);
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TLBmWsFileManager.sendData', lmt_Error, E.Message);
    end;

  end
  else
    LBLogger.Write(1, 'TLBmWsFileManager.sendData', lmt_Warning, 'No file to send!');

end;

end.

