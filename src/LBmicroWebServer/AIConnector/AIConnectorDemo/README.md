# AIConnectorDemo - accesso per agenti AI (OAuth + MCP) a un'applicazione LBmicroWebServer

Un programma Free Pascal **compilabile e collaudabile** che mostra come dare a un agente AI l'accesso alle funzioni di
un'applicazione, con i permessi dell'utente che lo autorizza. Segue lo schema delle tue applicazioni: una classe derivata
da `TLBApplicationBoostrap`, i `Setup` OAuth e connettore AI, `Routes.xml`, gestori `TRouteHandlerBase`.

**Solo Linux** (usa `BaseUnix` e `cthreads`, come il bootstrap).

## Stato di verifica (leggere prima)

| Cosa | Esito |
|---|---|
| Sintassi dei 4 file Pascal (parser Pascal esterno, **non** il compilatore) | nessun errore |
| Tutte le unit richieste esistono nelle tue cartelle (chiusura delle `uses`) | si, 33 unit; le esterne sono elencate sotto |
| `Routes.xml`, gestori, `DescribeAIFunction`, permessi degli utenti e test sono coerenti fra loro | si (controllato con uno script) |
| `run_tests.py` (92 controlli) contro un **finto** server che imita il comportamento letto nel codice; fallimenti e diagnostica provati | 92/92, e rileva i difetti introdotti |
| **Compilazione con `fpc`** e **esecuzione del server Pascal** | **NON eseguite**: nell'ambiente in cui l'ho scritto non c'e' Free Pascal |

Quindi il primo `./build.sh` puo' dare errori che non ho potuto vedere, e un controllo di `run_tests.py` che fallisce
puo' essere un errore del test (scritto leggendo il codice) oppure del server: confronta sempre con `bin/logs/aidemo.log`.

## Cosa serve

* Free Pascal 3.2.x con le unit di **LazUtils** (`Laz2_DOM`, `Laz2_XMLRead`, `LazFileUtils`, `LazSysUtils`, `FileUtil`) e di
  **Synapse** (`blcksock`, `synsock`, `synaser`, `synautil`, `synacode`, `mimepart`, `httpsend`, `ssl_openssl3`). Sono le stesse che
  usi gia' per compilare IdroWorld. Dall'FCL: `fpjson`, `jsonparser`, `sqlite3dyn`, `System.NetEncoding`, `fileinfo`.
* I tuoi sorgenti, in tre cartelle: `LBmicroWebServer/` (con `AIConnector/` e `OAuth/`), `utils/` (con `Token/`) e `LBLogger/`.
  Il progetto **non ne copia nessuno**: li legge dai tuoi percorsi.
* Python 3 per i test. Per lo scenario `persist` anche `libsqlite3`.

## Compilare

```sh
./build.sh
```

Per default cerca `../LBmicroWebServer`, `../utils` e `../LBLogger` (cioe' le tre cartelle accanto a questa). Altrimenti:

```sh
LB=/percorso/LBmicroWebServer UTILS=/percorso/utils LOGGER=/percorso/LBLogger \
EXTRA="-Fu/percorso/lazutils/lib/x86_64-linux -Fu/percorso/synapse" ./build.sh
```

`EXTRA` serve solo se `fpc` non trova da solo le unit di LazUtils e di Synapse. Il comando che lo script esegue e' questo:

```sh
fpc -MObjFPC -Sh -g -gl -vewnh \
    -FEbin -FUbin/units \
    -Fusrc -Fu$LB -Fu$LB/AIConnector -Fu$LB/OAuth \
    -Fu$UTILS -Fu$UTILS/Token -Fu$LOGGER $EXTRA \
    src/AIDemo.lpr
```

Risultato: `bin/AIDemo`. Con CodeTyphon/Lazarus puoi anche aprire `src/AIDemo.lpr` e dare gli stessi percorsi in "Unit search path".

## Avviare e provare a mano

Primo terminale (parte da `bin/` perche' il registro e i dati dei token stanno accanto all'eseguibile):

```sh
cd bin
./AIDemo ../config/AIDemo.ini            # stampa "READY port=8081 oauth=1 ai=1"; Ctrl+C lo ferma
```

Secondo terminale, dalla cartella del progetto:

```sh
python3 tests/mcp_client.py http://127.0.0.1:8081 login mario pw1
python3 tests/mcp_client.py http://127.0.0.1:8081 tools
python3 tests/mcp_client.py http://127.0.0.1:8081 call demo_Somma '{"a": 2, "b": 3}'
python3 tests/mcp_client.py http://127.0.0.1:8081 call demo_Somma '{"a": "x", "b": 3}'   # errore spiegato all'agente
python3 tests/mcp_client.py http://127.0.0.1:8081 refresh
```

Provando con altri utenti (`anna`/`pw2`, `admin`/`pw3`) l'elenco degli strumenti cambia. Il registro e' in `bin/logs/aidemo.log`.
Per collegare un vero client MCP, ad esempio Claude Code (ammette gli indirizzi di ritorno `localhost`):
`claude mcp add --transport http aidemo http://127.0.0.1:8081/mcp`. Claude.ai invece chiama dal cloud e vuole un indirizzo HTTPS pubblico.

## Test automatici

```sh
python3 tests/run_tests.py                 # compila prima! usa bin/AIDemo e le tre configurazioni in config/
python3 tests/run_tests.py --only base     # uno scenario: base | all | persist
python3 tests/run_tests.py --port 8090     # un'altra porta
```

Il runner avvia l'eseguibile vero, lo prova con un client OAuth + MCP, lo ferma con SIGTERM e passa allo scenario successivo.
**Se un controllo fallisce stampa le ultime righe del registro del server.** Codice di uscita 0 solo se passano tutti.

| Scenario | Configurazione | Cosa prova |
|---|---|---|
| `base` | `AIDemo.ini` | scoperta OAuth, registrazione, accesso con PKCE (anche i rifiuti), protocollo MCP, strumenti per ruolo, validazione degli argomenti, errori, rinnovo con rotazione, revoca al rinnovo, blocco dei tentativi |
| `all` | `AIDemo_all.ini` | `ExposeUndescribed`, `RequireOpTypeForTools`, `AllowedOrigins`, limite di chiamate al minuto |
| `persist` | `AIDemo_persist.ini` | token su SQLite: dopo un riavvio lo stesso token vale ancora, il refresh consumato non rinasce |

Lo scenario `persist` prova la correzione di `LoadActiveTokens`: se fallisce con una versione vecchia di `uTokenManager`,
e' proprio il difetto che avevamo trovato.

## Utenti di prova

| Utente | Password | Ruolo | Permessi | Strumenti che l'agente vede |
|---|---|---|---|---|
| `mario` | `pw1` | lettore | `ai.Uso`, `demo.Lettura` | Cerca, Ping, Somma, Stato |
| `anna` | `pw2` | tecnico | + `demo.Modifica` | + CreaNota, EliminaNota |
| `admin` | `pw3` | admin | + `demo.Amministrazione` | + Report |
| `ospite` | `pw4` | ospite | `demo.Lettura` (senza `ai.Uso`) | nessuno: `/mcp` risponde 403 |
| `temp` | `pw5` | lettore | come mario | si disattiva creando `config/temp.disabled` |

## Dove guardare nel codice

| File | Cosa mostra |
|---|---|
| `src/AIDemo.lpr` | programma: avvio, attesa, chiusura ordinata con SIGINT/SIGTERM |
| `src/uAIDemoApplication.pas` | **il collegamento**: due punti d'aggancio del bootstrap, ordine delle `Attach`, proprieta' degli oggetti, ordine di distruzione |
| `src/uAIDemoHandlers.pas` | una funzione scritta una volta e usata da sito e agente; `DescribeAIFunction`; `Hidden`, `aeModifies`, `aeDestructive`, stati >= 400 |
| `src/uAIDemoUsers.pas` | credenziali e rinnovo (`TAICredentialsEvent`, `TAIRefreshUserEvent`), configurazione utente con i permessi |
| `config/Routes.xml` | la porta MCP (`Kind="AIConnector"`, permesso `ai.Uso`) e le funzioni |
| `config/AIDemo*.ini` | le sezioni `[OAuth]`, `[AIConnector]`, `[TokenManager]` |
| `tests/mcp_client.py` | il client: fa quello che farebbe un agente |

## Se qualcosa non va

| Sintomo | Cosa guardare |
|---|---|
| `fpc`: *Can't find unit ...* | aggiungi la cartella di quella unit in `EXTRA` (LazUtils o Synapse) o correggi `LB`, `UTILS`, `LOGGER` |
| `fpc`: errore in una unit **tua** | il progetto usa solo cio' che e' dichiarato nelle tue unit attuali: mandami l'errore completo |
| `Errore di configurazione: ...` | il messaggio dice la chiave; per `DBFile` serve `libsqlite3` |
| `Il server non e' stato avviato` | `bin/logs/aidemo.log`: rotte non lette, porta occupata |
| `oauth=0` o `ai=0` in `READY` | il registro elenca **tutti** i motivi (`PublicBaseURL` non https senza `AllowInsecureHTTP=1`, manca l'endpoint `Kind="AIConnector"`, ...) |
| `GET /mcp` o `POST /mcp` danno 404 | il connettore non e' attivo: vedi sopra |

## Questo progetto non usa le correzioni dell'ultimo pacchetto

Compila sulle tue unit **attuali**. Non usa `Setup.Connector`, `Provider.AccessScope`, `isValidBearerRequestForScope` e non dipende
dal fatto che `addChainProcessor` restituisca `True` (oggi restituisce sempre `False`). Se applichi quel pacchetto il progetto
continua a funzionare.
