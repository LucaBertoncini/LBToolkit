unit uLBSpeechToText;
{
  Contratto generico LBToolkit per Speech-to-Text.

  Il provider e' volutamente SINCRONO: riceve un file audio e restituisce il risultato della
  trascrizione. Eventuale coda/background processing appartiene all'applicazione chiamante
  (per IdroWorld: worker/coda del CMMS), non al provider.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TSpeechToTextResult }
  TSpeechToTextResult = class(TObject)
  private
    FText       : String;
    FDurationMs : Int64;
    FLanguage   : String;
    FProvider   : String;
  public
    constructor Create;
    procedure Clear;

    property Text       : String read FText write FText;
    property DurationMs : Int64 read FDurationMs write FDurationMs;
    property Language   : String read FLanguage write FLanguage;
    property Provider   : String read FProvider write FProvider;
  end;

  { ISpeechToTextProvider }
  ISpeechToTextProvider = interface
    ['{A0E4FB76-01E8-4B21-B200-B475785144B0}']
    function ProviderName: String;
    function Transcribe(const aAudioFileName, aLanguage: String;
      out aResult: TSpeechToTextResult; out anError: String): Boolean;
  end;

implementation

constructor TSpeechToTextResult.Create;
begin
  inherited Create;
  Self.Clear;
end;

procedure TSpeechToTextResult.Clear;
begin
  FText := '';
  FDurationMs := 0;
  FLanguage := '';
  FProvider := '';
end;

end.
