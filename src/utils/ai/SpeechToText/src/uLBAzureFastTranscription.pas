unit uLBAzureFastTranscription;
{
  Provider ISpeechToTextProvider per Azure Speech Fast Transcription REST API,
  implementato interamente con Ararat Synapse (THTTPSend).

  API GA usata: 2025-10-15
  POST {Endpoint}/speechtotext/transcriptions:transcribe?api-version=...
  multipart/form-data:
    - audio      = file audio
    - definition = {"locales":["it-IT"]}
  Autenticazione: Ocp-Apim-Subscription-Key.

  Nessuna asincronia in questa classe: l'operazione REST Fast Transcription e'
  sincrona. Eventuale coda/background processing appartiene all'applicazione.

  TLS: per default usa ssl_openssl. Definendo LB_SYNAPSE_OPENSSL3 usa
  ssl_openssl3. Il plugin scelto va compilato insieme al programma.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, uLBSpeechToText;

type
  { TAzureFastTranscription }
  TAzureFastTranscription = class(TInterfacedObject, ISpeechToTextProvider)
  private
    FEndpoint        : String;
    FSubscriptionKey : String;
    FDefaultLocale   : String;
    FApiVersion      : String;
    FTimeoutMs       : Integer;

    function BuildURL: String;
    function BuildDefinition(const aLocale: String): String;
    function ParseResponse(const aJSON: String; out aResult: TSpeechToTextResult;
      out anError: String): Boolean;
    function ContentTypeForFile(const aFileName: String): String;
  public
    constructor Create(const anEndpoint, aSubscriptionKey, aDefaultLocale,
      anApiVersion: String; aTimeoutMs: Integer = 120000);

    function ProviderName: String;
    function Transcribe(const aAudioFileName, aLanguage: String;
      out aResult: TSpeechToTextResult; out anError: String): Boolean;

    property Endpoint        : String read FEndpoint;
    property SubscriptionKey : String read FSubscriptionKey;
    property DefaultLocale   : String read FDefaultLocale;
    property ApiVersion      : String read FApiVersion;
    property TimeoutMs       : Integer read FTimeoutMs;
  end;

implementation

uses
  httpsend,
  {$IFDEF LB_SYNAPSE_OPENSSL3}
  ssl_openssl3,
  {$ELSE}
  ssl_openssl,
  {$ENDIF}
  jsonparser;

const
  cAzureProviderName = 'Azure';
  cCRLF = #13#10;

function TrimTrailingSlash(const s: String): String;
begin
  Result := Trim(s);
  while (Length(Result) > 0) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function JSONEscape(const s: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    case s[i] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8:  Result := Result + '\b';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      Result := Result + s[i];
    end;
end;

function RandomBoundary: String;
begin
  Randomize;
  Result := '----------------LBSTT' + IntToHex(GetTickCount64, 16) +
    IntToHex(Random(MaxInt), 8);
end;

procedure WriteRaw(aStream: TStream; const s: RawByteString);
begin
  if Length(s) > 0 then
    aStream.WriteBuffer(s[1], Length(s));
end;

function StreamToString(aStream: TStream): String;
begin
  Result := '';
  if aStream = nil then Exit;
  aStream.Position := 0;
  SetLength(Result, aStream.Size);
  if aStream.Size > 0 then
    aStream.ReadBuffer(Result[1], aStream.Size);
end;

constructor TAzureFastTranscription.Create(const anEndpoint, aSubscriptionKey,
  aDefaultLocale, anApiVersion: String; aTimeoutMs: Integer);
begin
  inherited Create;
  FEndpoint := TrimTrailingSlash(anEndpoint);
  FSubscriptionKey := Trim(aSubscriptionKey);
  FDefaultLocale := Trim(aDefaultLocale);
  if Trim(anApiVersion) = '' then
    FApiVersion := '2025-10-15'
  else
    FApiVersion := Trim(anApiVersion);
  if aTimeoutMs <= 0 then
    FTimeoutMs := 120000
  else
    FTimeoutMs := aTimeoutMs;
end;

function TAzureFastTranscription.ProviderName: String;
begin
  Result := cAzureProviderName;
end;

function TAzureFastTranscription.BuildURL: String;
begin
  Result := FEndpoint + '/speechtotext/transcriptions:transcribe?api-version=' +
    FApiVersion;
end;

function TAzureFastTranscription.BuildDefinition(const aLocale: String): String;
var
  _Locale: String;
begin
  _Locale := Trim(aLocale);
  if _Locale = '' then _Locale := FDefaultLocale;
  if _Locale = '' then
    Result := '{}'
  else
    Result := '{"locales":["' + JSONEscape(_Locale) + '"]}';
end;

function TAzureFastTranscription.ContentTypeForFile(const aFileName: String): String;
var
  _Ext: String;
begin
  _Ext := LowerCase(ExtractFileExt(aFileName));
  if _Ext = '.wav' then Result := 'audio/wav'
  else if _Ext = '.mp3' then Result := 'audio/mpeg'
  else if _Ext = '.ogg' then Result := 'audio/ogg'
  else if _Ext = '.opus' then Result := 'audio/ogg'
  else if _Ext = '.flac' then Result := 'audio/flac'
  else if _Ext = '.webm' then Result := 'audio/webm'
  else if _Ext = '.aac' then Result := 'audio/aac'
  else if _Ext = '.wma' then Result := 'audio/x-ms-wma'
  else if _Ext = '.amr' then Result := 'audio/amr'
  else Result := 'application/octet-stream';
end;

function TAzureFastTranscription.ParseResponse(const aJSON: String;
  out aResult: TSpeechToTextResult; out anError: String): Boolean;
var
  _Data, _PhrasesData, _Item: TJSONData;
  _Obj: TJSONObject;
  _Arr: TJSONArray;
  i: Integer;
  _Text, _Locale: String;
begin
  Result := False;
  aResult := nil;
  anError := '';
  _Data := nil;
  try
    try
      _Data := GetJSON(aJSON);
      if (_Data = nil) or (_Data.JSONType <> jtObject) then
      begin
        anError := 'Azure ha restituito JSON non valido o non oggetto';
        Exit;
      end;
      _Obj := TJSONObject(_Data);
      aResult := TSpeechToTextResult.Create;
      aResult.Provider := Self.ProviderName;
      aResult.DurationMs := _Obj.Get('durationMilliseconds', Int64(0));

      _Text := '';
      _PhrasesData := _Obj.Find('combinedPhrases');
      if (_PhrasesData <> nil) and (_PhrasesData.JSONType = jtArray) then
      begin
        _Arr := TJSONArray(_PhrasesData);
        for i := 0 to _Arr.Count - 1 do
        begin
          _Item := _Arr.Items[i];
          if _Item.JSONType = jtObject then
          begin
            if _Text <> '' then _Text := _Text + LineEnding;
            _Text := _Text + TJSONObject(_Item).Get('text', '');
          end;
        end;
      end;
      aResult.Text := _Text;

      _Locale := '';
      _PhrasesData := _Obj.Find('phrases');
      if (_PhrasesData <> nil) and (_PhrasesData.JSONType = jtArray) and
         (TJSONArray(_PhrasesData).Count > 0) then
      begin
        _Item := TJSONArray(_PhrasesData).Items[0];
        if _Item.JSONType = jtObject then
          _Locale := TJSONObject(_Item).Get('locale', '');
      end;
      if _Locale = '' then _Locale := FDefaultLocale;
      aResult.Language := _Locale;
      Result := True;
    except
      on E: Exception do
      begin
        FreeAndNil(aResult);
        anError := 'Risposta Azure non interpretabile: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    _Data.Free;
  end;
end;

function TAzureFastTranscription.Transcribe(const aAudioFileName,
  aLanguage: String; out aResult: TSpeechToTextResult;
  out anError: String): Boolean;
var
  _HTTP: THTTPSend;
  _Audio: TFileStream;
  _Boundary, _ResponseText, _Definition, _FileName: String;
  _Header: RawByteString;
  _TransportOK: Boolean;
begin
  Result := False;
  aResult := nil;
  anError := '';

  if FEndpoint = '' then
  begin
    anError := 'Azure Speech: Endpoint non configurato'; Exit;
  end;
  if FSubscriptionKey = '' then
  begin
    anError := 'Azure Speech: SubscriptionKey non configurata'; Exit;
  end;
  if not FileExists(aAudioFileName) then
  begin
    anError := 'File audio non trovato: ' + aAudioFileName; Exit;
  end;

  _HTTP := THTTPSend.Create;
  try
    _Boundary := RandomBoundary;
    _Definition := BuildDefinition(aLanguage);
    _FileName := ExtractFileName(aAudioFileName);

    _Header := '--' + _Boundary + cCRLF +
      'Content-Disposition: form-data; name="definition"' + cCRLF +
      'Content-Type: application/json; charset=utf-8' + cCRLF + cCRLF +
      RawByteString(_Definition) + cCRLF;
    WriteRaw(_HTTP.Document, _Header);

    _Header := '--' + _Boundary + cCRLF +
      'Content-Disposition: form-data; name="audio"; filename="' +
      RawByteString(StringReplace(_FileName, '"', '_', [rfReplaceAll])) + '"' + cCRLF +
      'Content-Type: ' + RawByteString(ContentTypeForFile(aAudioFileName)) + cCRLF + cCRLF;
    WriteRaw(_HTTP.Document, _Header);
    _Audio := TFileStream.Create(aAudioFileName, fmOpenRead or fmShareDenyWrite);
    try
      if _Audio.Size > 0 then _HTTP.Document.CopyFrom(_Audio, _Audio.Size);
    finally
      _Audio.Free;
    end;
    WriteRaw(_HTTP.Document, cCRLF + '--' + _Boundary + '--' + cCRLF);
    _HTTP.Document.Position := 0;

    // In THTTPSend Content-Type si imposta con MimeType, non in Headers.
    _HTTP.MimeType := 'multipart/form-data; boundary=' + _Boundary;
    _HTTP.Headers.Add('Ocp-Apim-Subscription-Key: ' + FSubscriptionKey);
    _HTTP.UserAgent := 'LBToolkit-SpeechToText/1.0';
    _HTTP.Timeout := FTimeoutMs;

    try
      _TransportOK := _HTTP.HTTPMethod('POST', BuildURL);
    except
      on E: Exception do
      begin
        anError := 'Azure Speech / Synapse: ' + E.Message;
        Exit;
      end;
    end;

    _ResponseText := StreamToString(_HTTP.Document);
    if (not _TransportOK) or (_HTTP.ResultCode < 200) or (_HTTP.ResultCode >= 300) then
    begin
      anError := Format('Azure Speech HTTP %d %s',
        [_HTTP.ResultCode, Trim(_HTTP.ResultString)]);
      if Trim(_ResponseText) <> '' then
        anError := anError + ': ' + Trim(_ResponseText);
      if not _TransportOK then
        anError := anError + Format(' (Synapse socket error %d: %s)',
          [_HTTP.Sock.LastError, _HTTP.Sock.LastErrorDesc]);
      Exit;
    end;

    Result := ParseResponse(_ResponseText, aResult, anError);
  finally
    _HTTP.Free;
  end;
end;

end.
