program SpeechToTextTest;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, IniFiles,
  uLBSpeechToText, uLBSpeechToTextFactory;

var
  _Ini: TIniFile;
  _Provider: ISpeechToTextProvider;
  _Result: TSpeechToTextResult;
  _Error, _Config, _Audio, _Locale: String;
begin
  if ParamCount < 2 then
  begin
    WriteLn('Uso: SpeechToTextTest <config.ini> <file-audio> [locale]');
    Halt(2);
  end;

  _Config := ExpandFileName(ParamStr(1));
  _Audio := ExpandFileName(ParamStr(2));
  if ParamCount >= 3 then _Locale := ParamStr(3) else _Locale := '';

  _Ini := TIniFile.Create(_Config);
  try
    _Provider := CreateSpeechToTextProvider(_Ini, _Error);
  finally
    _Ini.Free;
  end;
  if _Provider = nil then
  begin
    WriteLn('ERRORE configurazione: ', _Error);
    Halt(3);
  end;

  WriteLn('Provider: ', _Provider.ProviderName);
  WriteLn('Audio: ', _Audio);
  _Result := nil;
  if not _Provider.Transcribe(_Audio, _Locale, _Result, _Error) then
  begin
    WriteLn('ERRORE trascrizione: ', _Error);
    Halt(4);
  end;
  try
    WriteLn('Durata: ', _Result.DurationMs, ' ms');
    WriteLn('Lingua: ', _Result.Language);
    WriteLn;
    WriteLn(_Result.Text);
  finally
    _Result.Free;
  end;
end.
