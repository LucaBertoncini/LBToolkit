unit uBase64Util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mimepart;

type
  { TBase64Util }
  TBase64Util = class(TObject)
    strict private
      FMimePart : TMimePart;

    public
      constructor Create;
      destructor Destroy; override;

      function Base64ToStream(const aBase64String: String; anOutStream: TStream): Boolean;
      function Base64ToString(const aBase64String: String; out aDecodedString: string): Boolean;
      function Base64ToFile(const Base64, AFile: String): boolean;

      function EncodeStream(anInputStream: TStream; out EncodedString: AnsiString): Boolean;
      function EncodeString(const aString: AnsiString; out EncodedString: AnsiString): Boolean;
      function EncodeBuffer(const aBuffer: TBytes; out EncodedString: AnsiString): Boolean;
      function EncodeBuffer(aBuffer: PByte; aBufferLen: Integer; out EncodedString: AnsiString): Boolean; overload;

      function EncodeFile(const AFile: String; out EncodedString: AnsiString): Boolean; overload;
      function EncodeFile(const AFile: String): AnsiString; overload;
  end;

implementation

uses
  ULBLogger, strutils;

{ TBase64Util }

constructor TBase64Util.Create;
begin
  inherited Create;

  FMimePart := TMimePart.Create;
end;

destructor TBase64Util.Destroy;
begin
  FMimePart.Free;
  inherited Destroy;
end;

function TBase64Util.EncodeStream(anInputStream: TStream; out EncodedString: AnsiString): Boolean;
begin

  Result := False;
  EncodedString := '';

  try

    FMimePart.Clear;
    FMimePart.EncodingCode := TMimeEncoding.ME_BASE64;
    FMimePart.PrimaryCode := TMimePrimary.MP_BINARY;
    FMimePart.DecodedLines.LoadFromStream(anInputStream);
    FMimePart.EncodePart;

    EncodedString := FMimePart.PartBody.Text;
    Result := True;

  except
    on E: Exception do
      LBLogger.Write(1, 'TBase64Util.StreamToBase64', lmt_Error, PChar(E.Message));
  end;
end;

function TBase64Util.Base64ToStream(const aBase64String: String; anOutStream: TStream): Boolean;
begin
  Result := False;

  if aBase64String <> '' then
  begin

    try

      FMimePart.Clear;
      FMimePart.EncodingCode := ME_BASE64;
      FMimePart.PrimaryCode := TMimePrimary.MP_BINARY;
      FMimePart.PartBody.Text := aBase64String;
      FMimePart.DecodePart;

      if FMimePart.DecodedLines.Size > 0 then
      begin
        anOutStream.Position := 0;
        Result := anOutStream.Write(FMimePart.DecodedLines.Memory, FMimePart.DecodedLines.Size) = FMimePart.DecodedLines.Size;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TBase64Util.Base64ToStream', lmt_Error, PChar(E.Message));
    end;

  end;

end;

function TBase64Util.Base64ToString(const aBase64String: String; out aDecodedString: string): Boolean;
var
  _StringStream : TStringStream = nil;

begin
  aDecodedString := '';

  try
    _StringStream := TStringStream.Create('');

    Result := Self.Base64ToStream(aBase64String, _StringStream);
    if Result then
      aDecodedString := _StringStream.DataString;

  except
    on E: Exception do
    begin
      Result := False;
      LBLogger.Write(1, 'TBase64Util.Base64ToString', lmt_Error, PChar(E.Message));
    end;
  end;

  if _StringStream <> nil then
    _StringStream.Free;
end;

function TBase64Util.Base64ToFile(const Base64, AFile: String): boolean;
var
  _OutStream: TFileStream;

begin
  Result := False;

  try
    _OutStream := TFileStream.Create(AFile, fmCreate or fmShareExclusive);

    Result := Self.Base64ToStream(Base64, _OutStream);

  finally
    _Outstream.Free;
  end;
end;

function TBase64Util.EncodeString(const aString: AnsiString; out EncodedString: AnsiString): Boolean;
var
  _StringStream : TStringStream = nil;

begin
  Result := False;
  EncodedString := '';

  if aString <> '' then
  begin

    try

      _StringStream := TStringStream.Create(aString);
      Result := Self.EncodeStream(_StringStream, EncodedString);
      if Result then
      begin
        EncodedString := AnsiReplaceStr(EncodedString, #10, '');
        EncodedString := AnsiReplaceStr(EncodedString, #13, '');
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TBase64Util.EncodeString', lmt_Error, PChar(E.Message));
    end;

    if _StringStream <> nil then
      _StringStream.Free;

  end;
end;

function TBase64Util.EncodeBuffer(const aBuffer: TBytes; out EncodedString: AnsiString): Boolean;
var
  _Stream : TBytesStream = nil;

begin
  Result := False;
  EncodedString := '';

  try

    _Stream := TBytesStream.Create(aBuffer);
    Result := Self.EncodeStream(_Stream, EncodedString);
    if Result then
    begin
      EncodedString := AnsiReplaceStr(EncodedString, #10, '');
      EncodedString := AnsiReplaceStr(EncodedString, #13, '');
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TBase64Util.EncodeBuffer', lmt_Error, PChar(E.Message));
  end;

  if _Stream <> nil then
    _Stream.Free;
end;

function TBase64Util.EncodeBuffer(aBuffer: PByte; aBufferLen: Integer; out EncodedString: AnsiString): Boolean;
var
  _Stream : TMemoryStream = nil;

begin
  Result := False;
  EncodedString := '';

  try
    _Stream := TMemoryStream.Create;
    _Stream.Write(aBuffer^, aBufferLen);
    _Stream.Position := 0;
    Result := Self.EncodeStream(_Stream, EncodedString);
    if Result then
    begin
      EncodedString := AnsiReplaceStr(EncodedString, #10, '');
      EncodedString := AnsiReplaceStr(EncodedString, #13, '');
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TBase64Util.EncodeBuffer', lmt_Error, PChar(E.Message));
  end;

  if _Stream <> nil then
    _Stream.Free;
end;

function TBase64Util.EncodeFile(const AFile: String; out EncodedString: AnsiString): Boolean;
var
  _InputStream : TFileStream = nil;

begin
  Result := False;
  EncodedString := '';

  if not FileExists(AFile) then
  begin
    LBLogger.Write(1, 'TBase64Util.EncodeFile', lmt_Warning, 'File <%s> not found!', [AFile]);
    Exit;
  end;

  try

    _InputStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    Result := Self.EncodeStream(_InputStream, EncodedString);

  except
    on E: Exception do
      LBLogger.Write(1, 'TBase64Util.EncodeFile', lmt_Error, 'Error encoding file <%s>: %s', [AFile, E.Message]);
  end;

  if _InputStream <> nil then
    _InputStream.Free;
end;

function TBase64Util.EncodeFile(const AFile: String): AnsiString;
begin
  Self.EncodeFile(AFile, Result);
end;

end.
