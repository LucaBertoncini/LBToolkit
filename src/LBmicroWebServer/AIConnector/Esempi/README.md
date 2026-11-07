# Esempi: OAuth + connettore AI di LBmicroWebServer

Cinque esempi, ognuno su un solo argomento, più un client Python che fa quello che farebbe un agente AI.

## Stato di verifica (leggere prima)

- **I programmi Pascal non sono stati compilati né eseguiti.** Nell'ambiente in cui li ho scritti non c'è Free Pascal.
  Ho controllato che `begin`/`end`/`try` siano bilanciati e che ogni identificatore usato esista nelle unit che mi hai inviato
  (proprietà del provider, del connettore e dei due `Setup`, firme degli eventi). Al primo `fpc` ci possono essere errori che non ho visto.
- **Il client Python** (`client/mcp_client.py`) l'ho provato contro un simulatore del protocollo scritto da me (non incluso):
  26 controlli superati sullo scenario 1, compresa la revoca al rinnovo. Questo prova il client, **non** i server Pascal.
- **I controlli di `smoke_test.py`** per gli esempi 2, 3 e 4 sono scritti leggendo il codice del connettore e del provider
  (messaggi d'errore, schema, ordine dei controlli). Non sono mai stati eseguiti contro un server vero: se uno fallisce,
  guarda prima se è il test o il server a sbagliare.

## Come compilare

Servono le unit di LBToolkit e Synapse. Nei percorsi delle unit (`-Fu`) aggiungi queste cartelle del tuo progetto:

```
LBmicroWebServer          LBmicroWebServer/AIConnector       LBmicroWebServer/OAuth
utils/Token               utils (uHTTPConsts e le altre)     (la cartella Esempi, per uEsempiComune.pas)
```

```
fpc -MObjFPC -Sh -Fu<le cartelle sopra> -Fu<LBToolkit e Synapse> Ex01_Minimo/Ex01_Minimo.pas
```

Ogni esempio si lancia **dalla propria cartella**: legge `ExNN.ini` e `Routes_ExNN.xml` dalla cartella corrente
(o accanto all'eseguibile).

## Gli esempi

| | Cosa mostra | Porta | Prova automatica |
|---|---|---|---|
| **Ex01_Minimo** | Il collegamento completo scritto per esteso, in 4 punti numerati: INI, rotte e gestori, identità, catena. Tre strumenti (`demo_Ping`, `demo_Saluta`, `demo_Somma`). Usa `TLBOAuthSetup` e `TLBAIConnectorSetup` | 8081 | `python3 ../client/smoke_test.py ex01 http://127.0.0.1:8081 --server-dir .` |
| **Ex02_Parametri** | Tutti i tipi di parametro, enumerazioni, date, array; cosa controlla il connettore *prima* del worker e come arriva un errore; `aeModifies`, `aeDestructive`, `Hidden`, `ExposeUndescribed` | 8082 | `... ex02 http://127.0.0.1:8082` |
| **Ex03_PermessiAudit** | Cosa vede ogni ruolo; porta chiusa a un ruolo; `RequireOpTypeForTools`; limite di chiamate; `AllowedOrigins` (tutto nel file INI); registro delle chiamate con `OnToolCalled` impostato tramite `Setup.Connector` | 8083 | `... ex03 http://127.0.0.1:8083` |
| **Ex04_SoloOAuth** | OAuth **senza** connettore: una pagina `/whoami` protetta da token Bearer con ambiti propri (`AccessScope`, `RefreshScope`, `ScopeName`, `isValidBearerRequestForScope`); diciture della pagina di accesso scelte nel file INI | 8084 | `... ex04 http://127.0.0.1:8084` |
| **Ex05_TokenPersistenti** | Token su SQLite: sopravvivono a un riavvio e il salvataggio periodico funziona | - | a mano, vedi sotto |

`uEsempiComune.pas` contiene gli utenti di prova, i permessi per ruolo e una classe che collega i pezzi (usata da Ex02 e Ex03).
Non è codice da copiare in un'applicazione: la versione da leggere è Ex01.

## Utenti di prova

| Utente | Password | Ruolo | Cosa può |
|---|---|---|---|
| `mario` | `pw1` | lettore | usare l'AI, leggere |
| `anna` | `pw2` | tecnico | come mario, più modificare |
| `admin` | `pw3` | admin | tutto |
| `ospite` | `pw4` | ospite | leggere, ma **non** usare l'AI (il canale `/mcp` risponde 403) |
| `temp` | `pw5` | lettore | come mario; si disattiva creando il file `temp.disabled` nella cartella del server |

## Provare a mano

```
python3 client/mcp_client.py http://127.0.0.1:8081 login mario pw1
python3 client/mcp_client.py http://127.0.0.1:8081 tools
python3 client/mcp_client.py http://127.0.0.1:8081 call demo_Somma '{"a": 2, "b": 3}'
python3 client/mcp_client.py http://127.0.0.1:8081 call demo_Somma '{"a": "x", "b": 3}'    # errore spiegato all'agente
python3 client/mcp_client.py http://127.0.0.1:8081 refresh
```

Per collegare un vero client MCP (per esempio Claude Code, che ammette gli indirizzi di ritorno `localhost`):
`claude mcp add --transport http esempio1 http://127.0.0.1:8081/mcp`. Claude.ai invece chiama dal cloud e vuole HTTPS pubblico.

## Esempio 5: i token dopo un riavvio

```
cd Ex05_TokenPersistenti
Ex05_TokenPersistenti emit                      # stampa il token T e crea ex05_tokens.db
python3 verifica_db.py                          # annota LastRequestTime (L1)
Ex05_TokenPersistenti check T --wait            # NUOVO processo: "OK: token valido ..."; resta in vita 75 secondi
python3 verifica_db.py                          # LastRequestTime deve essere aumentato (L2 > L1)
```

Il primo `check` prova che il token viene ricaricato dal database. L'aumento di `LastRequestTime` prova che il thread di
salvataggio ha aggiornato la riga. Con la versione di `uTokenManager` **precedente** alle correzioni, i token ricaricati non
avevano il campo `token` e quell'aggiornamento falliva: `LastRequestTime` restava uguale e nel registro dell'applicazione
compariva un errore di `TTokenManager.UpdateTokens`.

## Cosa questi esempi non mostrano

- Il bootstrap (`TLBApplicationBoostrap`): qui ogni programma crea il web server a mano. In IdroWorld i punti 1-4 di Ex01
  vanno nei metodi `LoadConfigurationFromINIFileInternal` e `startingWebServer` della classe derivata.
- Un database vero: gli utenti sono una tabella nel codice e le password sono in chiaro. Non fare così.
- HTTPS: tutti gli esempi usano `AllowInsecureHTTP=1`, solo per la prova locale.
