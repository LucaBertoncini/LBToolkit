unit uLBSpeechToTextFactory;
{
  Factory dei provider Speech-to-Text.

  [SpeechToText]
  Provider=Azure

  La configurazione specifica del provider resta nella sua sezione. In futuro un nuovo provider
  (Google, WhisperCpp, Vosk, ...) richiede un nuovo ramo QUI, senza modificare il codice che usa
  ISpeechToTextProvider.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, uLBSpeechToText;

function CreateSpeechToTextProvider(aIni: TIniFile;
  out anError: String): ISpeechToTextProvider;

implementation

uses
  uLBAzureFastTranscription;

const
  cSectionSpeechToText      = 'SpeechToText';
  cKeyProvider              = 'Provider';
  cSectionAzure             = 'SpeechToText.Azure';
  cKeyEndpoint              = 'Endpoint';
  cKeySubscriptionKey       = 'SubscriptionKey';
  cKeyLocale                = 'Locale';
  cKeyApiVersion            = 'ApiVersion';
  cKeyTimeoutMs             = 'TimeoutMs';

function CreateSpeechToTextProvider(aIni: TIniFile;
  out anError: String): ISpeechToTextProvider;
var
  _Provider, _Endpoint, _Key, _Locale, _ApiVersion: String;
  _TimeoutMs: Integer;
begin
  Result := nil;
  anError := '';
  if aIni = nil then
  begin
    anError := 'SpeechToText: file INI non disponibile'; Exit;
  end;

  _Provider := Trim(aIni.ReadString(cSectionSpeechToText, cKeyProvider, ''));
  if _Provider = '' then
  begin
    anError := 'SpeechToText: Provider non configurato in [SpeechToText]'; Exit;
  end;

  if SameText(_Provider, 'Azure') then
  begin
    _Endpoint := Trim(aIni.ReadString(cSectionAzure, cKeyEndpoint, ''));
    _Key := Trim(aIni.ReadString(cSectionAzure, cKeySubscriptionKey, ''));
    _Locale := Trim(aIni.ReadString(cSectionAzure, cKeyLocale, 'it-IT'));
    _ApiVersion := Trim(aIni.ReadString(cSectionAzure, cKeyApiVersion, '2025-10-15'));
    _TimeoutMs := aIni.ReadInteger(cSectionAzure, cKeyTimeoutMs, 120000);
    if _Endpoint = '' then
    begin
      anError := 'SpeechToText.Azure: Endpoint non configurato'; Exit;
    end;
    if _Key = '' then
    begin
      anError := 'SpeechToText.Azure: SubscriptionKey non configurata'; Exit;
    end;
    Result := TAzureFastTranscription.Create(_Endpoint, _Key, _Locale,
      _ApiVersion, _TimeoutMs);
    Exit;
  end;

  anError := 'SpeechToText: provider non supportato: ' + _Provider;
end;

end.
