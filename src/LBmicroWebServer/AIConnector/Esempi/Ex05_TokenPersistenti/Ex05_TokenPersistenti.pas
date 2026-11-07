program Ex05_TokenPersistenti;
(*
  ESEMPIO 5 - TOKEN SALVATI SU SQLITE: SOPRAVVIVONO A UN RIAVVIO E VENGONO AGGIORNATI

  Con [TokenManager] DBFile il gestore dei token salva su SQLite i token che crea e, ogni 5 minuti (la prima volta
  dopo 1 minuto), aggiorna la riga dei token usati nel frattempo e rimuove quelli scaduti.
  Questo programma verifica due cose, ognuna in un processo diverso, come dopo un riavvio del server:

    1. un token creato prima del riavvio e' ANCORA VALIDO dopo (caricamento dal database);
    2. l'aggiornamento periodico funziona (la colonna LastRequestTime della tabella cambia).

  Comandi:
      Ex05_TokenPersistenti emit                 crea un token e lo stampa
      Ex05_TokenPersistenti check <token>        lo valida in un NUOVO processo
      Ex05_TokenPersistenti check <token> --wait lo valida e resta in vita 75 s, il tempo che il thread di
                                                 salvataggio faccia il suo primo giro

  Sequenza completa (vedi anche il README):
      Ex05_TokenPersistenti emit                      -> stampa T
      python3 verifica_db.py                          -> annota LastRequestTime (L1)
      Ex05_TokenPersistenti check T --wait            -> "OK: token valido ..."; attende 75 s
      python3 verifica_db.py                          -> LastRequestTime deve essere cambiato (L2 > L1)

  Se L2 = L1 e nel registro dell'applicazione compare un errore di TTokenManager.UpdateTokens, i token ricaricati dal
  database non hanno il campo "token": e' il difetto corretto in uTokenManager (LoadActiveTokens).
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson,
  uTokenManager, uLBAuthContext;

function NewTokens: TTokenManager;
begin
  Result := TTokenManager.Create;
  // legge [TokenManager] DBFile e TokenExpirationTime da Ex05.ini (nella cartella corrente)
  if not Result.LoadConfigurationFromIniFile('Ex05.ini') then
    WriteLn('Attenzione: configurazione dei token non caricata');
end;

procedure Emit;
var
  _Tokens : TTokenManager;
  _Config : TJSONObject;
  _Data   : TJSONObject;
begin
  _Tokens := NewTokens;
  try
    _Config := TJSONObject.Create;       // oggetto NUOVO: addNewScopedToken lo prende in carico in ogni caso
    _Config.Add('role', 'lettore');
    _Config.Add('is_admin', False);
    // utente 7, ambito AI, validita' 1 ora, emesso al "client" ex05
    if _Tokens.addNewScopedToken(7, _Config, cTokenScope_AIAccess, 3600, 'ex05', _Data) then
      WriteLn(_Data.Get('token', ''))
    else begin
      WriteLn('Errore: token non creato');
      Halt(1);
    end;
  finally
    _Tokens.Free;
  end;
end;

procedure Check(const aToken: String; aWait: Boolean);
var
  _Tokens  : TTokenManager;
  _Request : TStringList;
  _Answer  : TStringList;
  _Ctx     : TLBAuthenticationContext;
  _Ok      : Boolean;
begin
  _Tokens := NewTokens;
  _Request := TStringList.Create;
  _Answer := TStringList.Create;
  try
    // le intestazioni HTTP del server sono una lista "Nome:valore" senza distinzione di maiuscole
    _Request.NameValueSeparator := ':';
    _Request.CaseSensitive := False;
    _Request.Add('Authorization:Bearer ' + aToken);

    _Ctx := nil;
    _Ok := _Tokens.isValidBearerRequest(_Request, _Answer, _Ctx) and (_Ctx <> nil);
    if _Ok then
      WriteLn('OK: token valido, utente ', _Ctx.UserId, ', ruolo ', _Ctx.UserConfig.Get('role', ''))
    else
      WriteLn('NON VALIDO: il token non esiste, e'' scaduto o non e'' dell''ambito AI');
    FreeAndNil(_Ctx);   // rilascia il "blocco" sul token

    if _Ok and aWait then
    begin
      _Tokens.Activate;   // avvia il thread di salvataggio: il primo giro e' dopo 60 secondi
      WriteLn('Attendo 75 secondi che il thread salvi su SQLite ...');
      Flush(Output);
      Sleep(75000);
      WriteLn('Fatto: ora esegui verifica_db.py e confronta LastRequestTime.');
    end;
  finally
    _Answer.Free;
    _Request.Free;
    _Tokens.Free;
  end;
  if not _Ok then
    Halt(2);
end;

begin
  if (ParamCount >= 1) and SameText(ParamStr(1), 'emit') then
    Emit
  else if (ParamCount >= 2) and SameText(ParamStr(1), 'check') then
    Check(ParamStr(2), (ParamCount >= 3) and SameText(ParamStr(3), '--wait'))
  else begin
    WriteLn('Uso: Ex05_TokenPersistenti emit');
    WriteLn('     Ex05_TokenPersistenti check <token> [--wait]');
    Halt(1);
  end;
end.
