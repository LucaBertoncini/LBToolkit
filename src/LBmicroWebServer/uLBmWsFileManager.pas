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
          FStatusCode := http_PartialContent;
          Result := True;
        end
        else
          FStatusCode := http_RangeNotSatisfiable;
      end
      else if (_StartStr <> '') and (_EndStr = '') then // Caso 2: bytes=Start-
      begin
        _StartVal := StrToInt64Def(_StartStr, -1);
        if (_StartVal >= 0) and (_StartVal < aFileSize) then
        begin
          FRangeStart := _StartVal;
          FRangeEnd := aFileSize - 1;
          FStatusCode := http_PartialContent;
          Result := True;
        end
        else
          FStatusCode := http_RangeNotSatisfiable;
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
          FStatusCode := http_PartialContent;
          Result := True;
        end
        else
          FStatusCode := http_RangeNotSatisfiable;
      end
      else // Caso malformato
        FStatusCode := http_RangeNotSatisfiable;
    end
    else
      FStatusCode := http_BadRequest;
  end
  else begin
    FStatusCode := http_BadRequest;
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
        FStatusCode := http_Ok;
        Result := True;
      end;

      if Result then
      begin
        FRemainingBytes := FRangeEnd - FRangeStart + 1;
        FFileStream.Seek(FRangeStart, soFromBeginning);
      end;

    end
    else begin
      FStatusCode := http_NotFound;
      LBLogger.Write(1, 'TLBmWsFileManager.SetFile', lmt_Warning, 'File not found: %s', [aFilename]);
    end;

  except
    on E: Exception do
    begin
      FStatusCode := http_InternalServerError;
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
        cHTTP_Ext_HTML   : _ContentType := cHTTP_MimeType_HTML;
        cHTTP_Ext_CSS    : _ContentType := cHTTP_MimeType_CSS;
        cHTTP_Ext_JS     : _ContentType := cHTTP_MimeType_JS;
        cHTTP_Ext_JSON   : _ContentType := cHTTP_MimeType_JSON;
        cHTTP_Ext_XML    : _ContentType := cHTTP_MimeType_XML;
        cHTTP_Ext_TXT    : _ContentType := cHTTP_MimeType_TXT;

        cHTTP_Ext_JPG,
        cHTTP_Ext_JPEG   : _ContentType := cHTTP_MimeType_JPEG;
        cHTTP_Ext_PNG    : _ContentType := cHTTP_MimeType_PNG;
        cHTTP_Ext_GIF    : _ContentType := cHTTP_MimeType_GIF;
        cHTTP_Ext_SVG    : _ContentType := cHTTP_MimeType_SVG;
        cHTTP_Ext_WEBP   : _ContentType := cHTTP_MimeType_WEBP;
        cHTTP_Ext_ICO    : _ContentType := cHTTP_MimeType_ICO;

        cHTTP_Ext_PDF    : _ContentType := cHTTP_MimeType_PDF;

        cHTTP_Ext_MP4    : _ContentType := cHTTP_MimeType_MP4;
        cHTTP_Ext_WEBM   : _ContentType := cHTTP_MimeType_WEBM;
        cHTTP_Ext_MOV    : _ContentType := cHTTP_MimeType_MOV;
        cHTTP_Ext_M3U8   : _ContentType := cHTTP_MimeType_M3U8;
        cHTTP_Ext_TS     : _ContentType := cHTTP_MimeType_TS;

        cHTTP_Ext_WOFF   : _ContentType := cHTTP_MimeType_WOFF;
        cHTTP_Ext_WOFF2  : _ContentType := cHTTP_MimeType_WOFF2;

        else
          _ContentType := cHTTPValue_ContentTypeOctet; // fallback: application/octet-stream
      end;

      anHeadersList.Add(cHTTPHeader_ContentType + ': ' + _ContentType);
      anHeadersList.Add(cHTTPHeader_ContentLength + ': ' + IntToStr(FRangeEnd - FRangeStart + 1));
      anHeadersList.Add(cHTTPHeader_AcceptRanges + ': bytes');
      if FStatusCode = http_PartialContent then
        anHeadersList.Add(cHTTPHeader_ContentRange + ': ' + Format('bytes %d-%d/%d', [FRangeStart, FRangeEnd, FFileStream.Size]));

      anHeadersList.Add(cHTTPHeader_Connection + ': ' + cHTTPValue_ConnectionKeepAlive);

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

