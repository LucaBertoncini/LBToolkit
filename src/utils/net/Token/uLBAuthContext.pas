unit uLBAuthContext;
{
  Il "contesto di autenticazione" di una richiesta: chi e' l'utente e la sua configurazione, valida per TUTTA la
  durata della richiesta.

  PERCHE' E' IN UNA UNIT A PARTE
    Lo definisce chi autentica (il TTokenManager, in utils) e lo usa chi riceve la richiesta (il connettore per
    agenti AI, in LBmicroWebServer). Stava in uLBAIConnectorTypes, cioe' nella cartella del web server: cosi' utils
    dipendeva da LBmicroWebServer, nel verso sbagliato. Qui sta dove sta chi lo definisce e non dipende da nulla di
    LBToolkit. uLBAIConnectorTypes lo ripete con il vecchio nome (TAIAuthenticationContext = TLBAuthenticationContext):
    il codice che usa il vecchio nome compila senza modifiche.

  LA CONFIGURAZIONE UTENTE NON SI COPIA
    Chi autentica decide come proteggere la vita di UserConfig e restituisce un contesto che realizza quella
    protezione. Chi riceve la richiesta libera il contesto alla fine; UserConfig e' valido fino a quel momento.
    La classe base va bene anche per un autenticatore semplice che crea una configurazione a ogni richiesta: si
    passa aOwnsUserConfig=True e il contesto la libera. TTokenManager restituisce invece una classe derivata che
    "blocca" il token finche' qualche richiesta sta usando la sua configurazione.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  { TLBAuthenticationContext }
  TLBAuthenticationContext = class(TObject)
  strict private
    FUserId         : Integer;
    FUserConfig     : TJSONObject;
    FOwnsUserConfig : Boolean;
  public
    constructor Create(anUserId: Integer; aUserConfig: TJSONObject; aOwnsUserConfig: Boolean = False);
    destructor Destroy; override;

    property UserId: Integer read FUserId;
    property UserConfig: TJSONObject read FUserConfig;
  end;

implementation

constructor TLBAuthenticationContext.Create(anUserId: Integer; aUserConfig: TJSONObject;
  aOwnsUserConfig: Boolean);
begin
  inherited Create;
  FUserId := anUserId;
  FUserConfig := aUserConfig;
  FOwnsUserConfig := aOwnsUserConfig;
end;

destructor TLBAuthenticationContext.Destroy;
begin
  if FOwnsUserConfig then
    FreeAndNil(FUserConfig);
  inherited Destroy;
end;

end.
