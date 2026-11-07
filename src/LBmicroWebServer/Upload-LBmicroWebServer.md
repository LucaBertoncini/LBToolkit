# Upload di file con LBmicroWebServer

Guida per chi configura il server e per chi sviluppa un client che carica file (video o altri contenuti multimediali) tramite LBmicroWebServer e l'applicazione DMA.

## Indice

1. [Panoramica](#1-panoramica)
2. [Configurazione del server](#2-configurazione-del-server)
3. [Autenticazione](#3-autenticazione)
4. [Dati della scheda (header `X-JSON`)](#4-dati-della-scheda-header-x-json)
5. [Modalità A: upload in una sola richiesta](#5-modalità-a-upload-in-una-sola-richiesta)
6. [Modalità B: upload a blocchi](#6-modalità-b-upload-a-blocchi)
7. [Quale modalità scegliere](#7-quale-modalità-scegliere)
8. [Risposte ed errori](#8-risposte-ed-errori)
9. [Comportamento del server dopo l'upload](#9-comportamento-del-server-dopo-lupload)
10. [Timeout e limiti](#10-timeout-e-limiti)
11. [Diagnostica](#11-diagnostica)
12. [Riepilogo del protocollo](#12-riepilogo-del-protocollo)

---

## 1. Panoramica

LBmicroWebServer riceve i file come **corpo binario grezzo** di una richiesta `POST` indirizzata all'*endpoint di upload*. Il file non viene codificato (niente `multipart/form-data`, niente base64 nel corpo): i byte del file sono il corpo della richiesta. I dati descrittivi (titolo, categorie, ecc.) viaggiano nell'header `X-JSON`.

Sono disponibili due modalità, scelte dal client a seconda della dimensione del file e dell'affidabilità della rete:

| Modalità | Descrizione | Indicata per |
|---|---|---|
| **A – richiesta singola** | L'intero file è il corpo di un'unica `POST`. | File di piccole e medie dimensioni, reti stabili, client semplici. |
| **B – a blocchi** | Il file è inviato in blocchi sequenziali, ciascuno con una propria `POST`. L'upload può riprendere dal punto raggiunto. | File di grandi dimensioni (GB), connessioni Internet, reti intermittenti. |

Le due modalità usano lo stesso endpoint, la stessa autenticazione e lo stesso formato dei dati della scheda. Il server distingue la modalità B dalla presenza dell'oggetto `upl` nell'header `X-JSON` (vedi [§6](#6-modalità-b-upload-a-blocchi)).

```
Client                                         LBmicroWebServer
  │  POST /UploadVideo                              │
  │  Cookie: token=…                                │
  │  X-JSON: <scheda in base64>                     │
  │  Content-Length: N                              │
  │  <byte del file o del blocco>  ───────────────► │ scrive in <DocumentsFolder>/tmpUpload
  │                                                 │ valida il token e i permessi
  │  ◄─────────────────────────────────────────────  │ salva il file e la scheda
  │  200 OK  {"Success":true, …}                    │
```

## 2. Configurazione del server

### 2.1 Endpoint di upload

Nel file di configurazione, sezione `[LBmicroWebServer]`:

```ini
[LBmicroWebServer]
Port=10322
DocumentsFolder=./html
UploadEndpoint=/UploadVideo
```

| Chiave | Significato |
|---|---|
| `Port` | Porta di ascolto. |
| `DocumentsFolder` | Cartella radice dei documenti serviti dal server. Deve esistere. |
| `UploadEndpoint` | Percorso a cui il server accetta gli upload. Senza questa chiave gli upload sono disattivati. |

Il server tratta come upload ogni `POST` il cui URI **inizia** con `UploadEndpoint`. Per le richieste di upload dell'applicazione DMA l'URI deve coincidere esattamente con `/UploadVideo`, senza parametri di query.

### 2.2 Cartelle di lavoro

| Cartella | Funzione |
|---|---|
| `<DocumentsFolder>/tmpUpload/` | Area in cui il server scrive i byte man mano che arrivano (intero file in modalità A, singolo blocco in modalità B). Non è raggiungibile via HTTP: ogni richiesta GET verso questo percorso viene rifiutata. |
| `[Mediafiles] Folder_incoming` | Area dei file **parziali** degli upload a blocchi (default `./Incoming`). |
| `[Mediafiles] Folder_upload` | Destinazione finale dei file caricati, organizzata in `Multimedia/aaaa/mm/gg/`. |

```ini
[Mediafiles]
Folder=./Mediafiles
Folder_incoming=./Incoming
```

Indicazioni operative:

- `Folder_incoming` deve trovarsi **sullo stesso volume** di `Folder_upload`: al completamento il file viene *rinominato* nella destinazione finale, operazione istantanea solo all'interno dello stesso volume. Su volumi diversi equivarrebbe a una copia dell'intero file.
- `Folder_incoming` non deve essere una cartella osservata dal programma di elaborazione dei video, per evitare che un file ancora incompleto venga preso in carico.
- Lo spazio libero disponibile deve poter contenere il file in arrivo più un blocco: il server non esegue un controllo preventivo dello spazio.

### 2.3 HTTPS

Per upload via Internet si raccomanda HTTPS (sezione `[SSLData]` con certificato e chiave privata). L'autenticazione si basa su un cookie di sessione e i dati della scheda viaggiano in un header: senza TLS entrambi sono esposti.

## 3. Autenticazione

Ogni richiesta di upload richiede una sessione valida. La sessione è identificata dal cookie `token`, rilasciato dal server al login.

### 3.1 Login

`POST /` con corpo JSON:

```http
POST / HTTP/1.1
Content-Type: application/json

{"Req":"DMALin","usrName":"<utente>","psw":"<password>"}
```

La risposta contiene i dati dell'utente (`"Success":true`) e l'header `Set-Cookie` con il token di sessione:

```
Set-Cookie: token=<valore>; Path=/; HttpOnly; SameSite=Strict; Max-Age=3600;
```

Il client deve inviare lo stesso cookie in ogni richiesta successiva, compresi i blocchi di upload:

```
Cookie: token=<valore>
```

### 3.2 Durata della sessione

- La sessione scade per **inattività**: ogni richiesta autenticata ne rinnova la durata. Il valore predefinito è di 1 ora (`TokenExpirationTime`, in secondi, nella sezione `[TokenManager]`). L'header di risposta `X-Session-Expires-In` indica i secondi residui.
- La verifica del token avviene quando il server ha **ricevuto interamente** la richiesta di upload. In modalità A, un upload che dura più dell'intervallo di inattività consentito termina con sessione scaduta e il file viene scartato (vedi [§7](#7-quale-modalità-scegliere)). In modalità B ogni blocco è una richiesta autenticata e rinnova la sessione.

### 3.3 Permessi

| Tipo utente | Upload |
|---|---|
| Amministratore | Consentito; può modificare tutte le schede. |
| Utente semplice | Consentito; può modificare solo le proprie schede non validate. |
| Utente in sola lettura | Non consentito. |

La sostituzione del file di una scheda **validata** non è ammessa per nessun utente, amministratore compreso.

## 4. Dati della scheda (header `X-JSON`)

Le informazioni sul contenuto sono inviate nell'header `X-JSON` come oggetto JSON codificato in **UTF-8** e poi in **base64 standard** (alfabeto `A–Z a–z 0–9 + /`, con padding `=`). Il base64 garantisce che l'header contenga soli caratteri ASCII.

Esempio di oggetto prima della codifica:

```json
{
  "title": "Concerto di primavera",
  "description": "Registrazione integrale del concerto.",
  "date": "2024-04-12",
  "IdCategories": [3, 7],
  "idProvider": 0,
  "Provider": "Associazione Corale"
}
```

| Campo | Tipo | Obbligatorio | Descrizione |
|---|---|---|---|
| `title` | stringa | sì | Titolo. |
| `description` | stringa | sì | Descrizione. |
| `date` | stringa | sì | Data di produzione (testo libero, es. `2024`, `Giugno 2024`, `2024-04-12`). |
| `IdCategories` | array di interi | sì per un nuovo inserimento | Identificativi delle categorie. |
| `idProvider` | intero | no | Id di un provider esistente; `0` per indicarne uno nuovo. |
| `Provider` | stringa | no | Descrizione del provider, usata quando `idProvider` è `0`. Se esiste già un provider con lo stesso nome viene riutilizzato. |
| `sign` | stringa | no | Campo "verifica": considerato solo per gli amministratori. |
| `mId` | intero | no | Id di una scheda esistente. Se presente, il file caricato **sostituisce** quello della scheda. |
| `upl` | oggetto | solo modalità B | Descrittore dell'upload a blocchi (vedi [§6](#6-modalità-b-upload-a-blocchi)). |

Note:

- L'insieme degli header di una richiesta non può superare la dimensione massima gestita dal server (16 KB per impostazione predefinita). Poiché il base64 aumenta la dimensione di circa un terzo, la scheda in JSON deve restare di alcuni KB, tenendo conto anche del cookie e degli altri header.
- Il nome del file proviene dall'header `Content-Disposition` (`filename="…"`) oppure, in modalità B, dal campo `upl.name`. Si raccomandano nomi composti da caratteri ASCII: il server elimina dal nome i caratteri non sicuri per il file system.

## 5. Modalità A: upload in una sola richiesta

Il file è il corpo di un'unica richiesta.

### 5.1 Richiesta

```http
POST /UploadVideo HTTP/1.1
Host: archivio.example.org
Cookie: token=<valore>
Content-Type: application/octet-stream
Content-Disposition: attachment; filename="concerto.mp4"
Content-Length: 1073741824
X-JSON: eyJ0aXRsZSI6IkNvbmNlcnRvIGRpIHByaW1hdmVyYSIs…

<byte del file>
```

Requisiti:

- `Content-Length` obbligatorio e pari alla dimensione esatta del file. Il server non supporta `Transfer-Encoding: chunked`.
- `Content-Disposition` con il parametro `filename`.
- `X-JSON` senza l'oggetto `upl`.

### 5.2 Risposta

```json
{"_type":"SaveMedia","Success":true,"mId":1234}
```

`mId` è l'identificativo della scheda creata o aggiornata. In caso di errore: `{"_type":"SaveMedia","Success":false,"ErrMsg":"…"}` (vedi [§8](#8-risposte-ed-errori)).

### 5.3 Esempio con curl

```bash
TOKEN="…"   # valore del cookie ottenuto al login
META='{"title":"Concerto di primavera","description":"Registrazione integrale.","date":"2024-04-12","IdCategories":[3]}'

curl -X POST "https://archivio.example.org:10322/UploadVideo" \
  -H "Cookie: token=$TOKEN" \
  -H "Content-Type: application/octet-stream" \
  -H 'Content-Disposition: attachment; filename="concerto.mp4"' \
  -H "X-JSON: $(printf '%s' "$META" | base64 -w0)" \
  --data-binary @concerto.mp4
```

(Su macOS usare `base64` senza `-w0`, eliminando eventuali a capo.)

## 6. Modalità B: upload a blocchi

Il file viene diviso in blocchi consecutivi. Ogni blocco è inviato con una `POST` autonoma allo stesso endpoint. Il server accoda i blocchi in un file parziale e, quando riceve l'ultimo, completa l'operazione come per la modalità A.

### 6.1 Concetti

| Termine | Significato |
|---|---|
| **Id upload** | Identificativo scelto dal client, uguale per tutti i blocchi dello stesso file. Da 8 a 64 caratteri tra `0-9`, `a-z`, `A-Z` e `-`. Deve essere unico per utente (si raccomanda un UUID o 16 byte casuali in esadecimale). |
| **Offset** | Posizione del primo byte del blocco nel file, a partire da 0. |
| **Received** | Numero di byte che il server ha ricevuto e conservato per quell'upload. È l'unica fonte di verità sull'avanzamento. |

### 6.2 Richiesta di un blocco

```http
POST /UploadVideo HTTP/1.1
Host: archivio.example.org
Cookie: token=<valore>
Content-Type: application/octet-stream
Content-Disposition: attachment; filename="concerto.mp4"
Content-Length: 16777216
X-JSON: <scheda + upl, in base64>

<byte del blocco>
```

L'oggetto codificato in `X-JSON` contiene gli stessi campi della scheda ([§4](#4-dati-della-scheda-header-x-json)) più l'oggetto `upl`:

```json
{
  "title": "Concerto di primavera",
  "description": "Registrazione integrale.",
  "date": "2024-04-12",
  "IdCategories": [3],
  "upl": {
    "id": "9f2c41d07b8e4a63a1c5d0e7f3b2a498",
    "offset": 33554432,
    "size": 1073741824,
    "name": "concerto.mp4"
  }
}
```

| Campo `upl` | Descrizione |
|---|---|
| `id` | Id upload. |
| `offset` | Posizione del blocco nel file. Deve essere minore di `size`. |
| `size` | Dimensione **totale** del file in byte (costante per tutti i blocchi, maggiore di 0). |
| `name` | Nome del file. |

Il corpo contiene i byte da `offset` a `offset + Content-Length − 1`. Si raccomandano blocchi da **16 MiB**; la dimensione può essere scelta liberamente dal client (blocchi più piccoli per reti molto instabili).

La scheda (`title`, `description`, …) deve essere ripetuta in ogni richiesta, perché ogni blocco viene elaborato in modo indipendente; è usata alla ricezione dell'ultimo blocco.

### 6.3 Risposta a un blocco

```json
{"_type":"UploadChunk","Success":true,"received":50331648}
```

| Campo | Descrizione |
|---|---|
| `Success` | `true` se il blocco è stato accettato. |
| `received` | Byte conservati dal server per questo upload. |
| `done` | Presente e `true` solo nella risposta all'ultimo blocco, quando il file è stato salvato e la scheda registrata. In questo caso la risposta include anche i campi del salvataggio (`mId`). |
| `resync` | `true` quando il blocco non è accettato perché l'offset non coincide con lo stato del server. Il client deve ripartire da `received`. |
| `ErrMsg` | Descrizione dell'errore quando `Success` è `false`. |

Risposta all'ultimo blocco:

```json
{"_type":"SaveMedia","Success":true,"mId":1234,"done":true,"received":1073741824}
```

### 6.4 Algoritmo del client

```
id        ← identificativo casuale
offset    ← 0
tentativi ← 0

finché offset < size:
    blocco  ← byte [offset, min(offset + dimBlocco, size))
    risposta ← POST(blocco, upl = {id, offset, size, name})

    se errore di rete, timeout o risposta non JSON:
        tentativi ← tentativi + 1
        se tentativi > massimo → interrompere (opzionalmente inviare CancelUpload)
        attendere (ad es. 2·tentativi secondi, max 30) e ripetere lo stesso blocco

    altrimenti se risposta.Success:
        se risposta.done → upload concluso (risposta.mId = id scheda)
        altrimenti offset ← risposta.received ; tentativi ← 0

    altrimenti se risposta.resync:
        offset ← risposta.received                       (limitare il numero di riallineamenti)

    altrimenti:
        errore definitivo: mostrare risposta.ErrMsg e interrompere
```

Punti essenziali:

- **Sequenzialità.** I blocchi vanno inviati uno alla volta, in ordine.
- **Ripetizione sicura.** Un blocco può essere inviato più volte (ad esempio perché la risposta è andata persa): se il server ha già ricevuto quei byte non li duplica; se li ha ricevuti in parte, accoda solo la parte mancante. In entrambi i casi risponde con il valore aggiornato di `received`.
- **Ripresa.** Per riprendere un upload interrotto conoscendo l'id, è sufficiente rinviare un blocco a partire dall'ultimo offset noto (anche da 0): la risposta indica da dove proseguire.
- **Completamento ripetuto.** Se la risposta all'ultimo blocco va persa e il client lo reinvia, il server risponde con lo stesso esito già registrato, senza creare una seconda scheda.
- **Offset oltre lo stato del server.** Se `offset` è maggiore di `received`, il server risponde `Success:false` con `resync:true` e `received`.
- **Errori definitivi.** Una risposta con `Success:false` senza `resync` (permessi, scheda validata, richiesta non valida, errore sul database) non va ripetuta.

### 6.5 Annullamento

Per interrompere un upload e liberare subito lo spazio occupato dal file parziale:

```http
POST / HTTP/1.1
Cookie: token=<valore>
Content-Type: application/json

{"Req":"CancelUpload","id":"9f2c41d07b8e4a63a1c5d0e7f3b2a498"}
```

Risposta: `{"_type":"CancelUpload","Success":true}`. Un upload abbandonato senza annullamento viene comunque eliminato dal server dopo 72 ore.

### 6.6 Esempio JavaScript (browser)

```javascript
const CHUNK_SIZE = 16 * 1024 * 1024;
const MAX_RETRIES = 12;

const toBase64 = (obj) => btoa(unescape(encodeURIComponent(JSON.stringify(obj))));
const wait = (ms) => new Promise((r) => setTimeout(r, ms));

async function sendChunk(url, file, meta, id, offset) {
  const end = Math.min(offset + CHUNK_SIZE, file.size);
  const info = { ...meta, upl: { id, offset, size: file.size, name: file.name } };

  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',                       // invia il cookie di sessione
    headers: {
      'Content-Type': 'application/octet-stream',
      'Content-Disposition': `attachment; filename="${file.name}"`,
      'X-JSON': toBase64(info),
    },
    body: file.slice(offset, end),
  });
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  return response.json();
}

async function uploadFile(url, file, meta, onProgress) {
  const id = crypto.randomUUID().replace(/-/g, '');
  let offset = 0;
  let retries = 0;
  let resyncs = 0;

  while (offset < file.size) {
    let answer;
    try {
      answer = await sendChunk(url, file, meta, id, offset);
    } catch (err) {
      if (++retries > MAX_RETRIES) throw err;
      await wait(Math.min(30, retries * 2) * 1000);
      continue;                                    // stesso blocco, nuovo tentativo
    }

    if (answer.Success) {
      if (answer.done) return answer;              // answer.mId = id della scheda
      offset = answer.received;
      retries = 0;
      onProgress?.(offset / file.size);
    } else if (answer.resync && ++resyncs <= 20) {
      offset = answer.received;
    } else {
      throw new Error(answer.ErrMsg || 'Upload non riuscito');
    }
  }
}
```

In Node.js sostituire `toBase64` con `Buffer.from(JSON.stringify(obj), 'utf8').toString('base64')` e gestire il cookie `token` ottenuto al login.

## 7. Quale modalità scegliere

| Situazione | Modalità consigliata |
|---|---|
| File di pochi MB o decine di MB, rete locale | A |
| File di grandi dimensioni (centinaia di MB o GB) | B |
| Connessione Internet, Wi-Fi, reti mobili | B |
| Upload che può durare più dell'intervallo di inattività della sessione | B |
| Client minimale (script, strumenti da riga di comando) su rete affidabile | A |

Considerazioni:

- In modalità A l'intero trasferimento è una sola connessione: un'interruzione impone di ricominciare da zero. Il server attende fino a 60 secondi senza ricevere dati prima di considerare fallito l'upload.
- In modalità A il token viene verificato alla fine della ricezione. Con upload molto lunghi occorre che la sessione non scada nel frattempo: aumentare `TokenExpirationTime`, oppure far eseguire al client una richiesta autenticata a intervalli regolari durante l'invio.
- In modalità B ogni blocco rinnova la sessione, e un'interruzione costa al più il blocco in corso.

## 8. Risposte ed errori

Le risposte applicative sono oggetti JSON con `Success` e, in caso di errore, `ErrMsg`. Messaggi principali:

| `ErrMsg` | Causa | Azione del client |
|---|---|---|
| `Richiesta di upload non valida!` | Dati `upl` mancanti o incoerenti (id, nome, dimensione, offset). | Correggere la richiesta; non ripetere. |
| `Operazione non consentita!` | Il tipo di utente non può caricare o modificare. | Non ripetere. |
| `Video non trovato!` | `mId` non corrisponde a una scheda esistente. | Non ripetere. |
| `Il video e' validato: non e' possibile sostituire il file.` | La scheda è validata. | Non ripetere. |
| `Blocco fuori sequenza.` | `offset` maggiore dei byte già ricevuti (insieme a `resync:true`). | Ripartire da `received`. |
| `Il blocco supera la dimensione dichiarata del file.` | Il blocco eccede `size`. | Correggere `size` o la lunghezza del blocco. |
| `Errore nella scrittura del file sul server.` | Errore del file system (ad es. spazio esaurito). | Riprovare più tardi; avvisare l'amministratore. |
| `Impossibile completare il file sul server.` | Errore nel completamento dell'upload. | Riprovare più tardi; avvisare l'amministratore. |
| `Error saving data into DB!` | Errore nel salvataggio della scheda. | Avvisare l'amministratore. |

Condizioni a livello HTTP:

- **Sessione assente o scaduta:** la richiesta non produce una risposta applicativa (corpo non JSON, stato diverso da 200) e il file ricevuto viene eliminato. Il client deve eseguire un nuovo login; in modalità B può riprendere con lo stesso id perché i blocchi già ricevuti sono conservati.
- **`Content-Length` assente o zero:** richiesta rifiutata.
- **Connessione interrotta durante la ricezione:** il server scarta quanto ricevuto in modalità A; in modalità B i blocchi già completati restano disponibili per la ripresa.

## 9. Comportamento del server dopo l'upload

Quando il file è completo (modalità A) o è stato ricevuto l'ultimo blocco (modalità B), il server:

1. verifica sessione, tipo utente e, per una sostituzione, che la scheda esista, appartenga all'utente (o l'utente sia amministratore) e non sia validata;
2. sposta il file in `Folder_upload/Multimedia/aaaa/mm/gg/`. Se nella cartella esiste già un file con lo stesso nome, il nuovo riceve un suffisso progressivo (`nome_1.mp4`, `nome_2.mp4`, …): nessun file esistente viene sovrascritto;
3. registra o aggiorna la scheda nel database. Per un nuovo inserimento la scheda è validata automaticamente se l'utente è amministratore o ha l'auto-validazione attiva;
4. per una **sostituzione**, azzera la durata della scheda (`Duration = 0`) e, solo dopo l'aggiornamento del database, elimina il vecchio file originale e le sue copie elaborate. Il programma di elaborazione ritrova il nuovo file tramite il percorso registrato nel database e rigenera le copie.

In caso di errore dopo il trasferimento (permessi, database) il file ricevuto viene eliminato e non restano file orfani.

## 10. Timeout e limiti

| Parametro | Valore |
|---|---|
| Attesa massima senza ricevere byte durante un upload | 60 secondi |
| Timeout di lettura/scrittura sul socket | 10 secondi |
| Dimensione massima degli header di una richiesta | 16 KB (predefinita) |
| Dimensione massima del file | Nessun limite applicativo; vale lo spazio su disco |
| Dimensione consigliata di un blocco (modalità B) | 16 MiB |
| Conservazione di un upload a blocchi non completato | 72 ore |
| Durata predefinita della sessione per inattività | 1 ora (`TokenExpirationTime`) |

Il timeout di 60 secondi riguarda i periodi **senza alcun byte in arrivo**: finché il client trasmette, un upload può durare a lungo. Eventuali reverse proxy o firewall davanti al server devono consentire il corpo richiesto (nessun limite di dimensione troppo basso, timeout di lettura adeguati) e non devono modificare le richieste.

## 11. Diagnostica

I messaggi del server per gli upload compaiono nel file di log (`[LBLogger]`). Quelli più utili:

| Messaggio | Significato |
|---|---|
| `Receiving raw file upload to <…>` | Inizio ricezione di un file o di un blocco. |
| `File upload completed: N bytes written` | Ricezione completata. |
| `Connection closed by the client. Remaining bytes: …` | Il client ha chiuso la connessione prima della fine; il log riporta i byte mancanti, l'ultimo errore del socket e il tempo trascorso. |
| `No data received for 60 s. Remaining bytes: …` | Nessun dato ricevuto per 60 secondi. |
| `File upload incomplete. Expected …, received …` | Trasferimento incompleto: il file parziale viene scartato (modalità A) o il blocco non viene accodato (modalità B). |
| `Access to the upload temporary folder denied` | Richiesta GET verso `tmpUpload`: rifiutata per progetto. |

Controlli rapidi in caso di problemi:

1. *L'upload non parte:* verificare `UploadEndpoint`, l'URI esatto della richiesta e la presenza del cookie `token`.
2. *Upload interrotti a metà via Internet:* usare la modalità B; verificare timeout e limiti del reverse proxy.
3. *Errore al completamento:* verificare spazio disponibile, permessi di scrittura su `Folder_incoming` e `Folder_upload` e che si trovino sullo stesso volume.
4. *Sessione scaduta a fine upload (modalità A):* passare alla modalità B o aumentare `TokenExpirationTime`.

## 12. Riepilogo del protocollo

**Endpoint di upload:** `POST <UploadEndpoint>` (DMA: `/UploadVideo`)

| Header | Modalità A | Modalità B |
|---|---|---|
| `Cookie: token=…` | obbligatorio | obbligatorio |
| `Content-Type: application/octet-stream` | sì | sì |
| `Content-Disposition: attachment; filename="…"` | sì | sì |
| `Content-Length` | dimensione del file | dimensione del blocco |
| `X-JSON` | scheda (base64 di JSON UTF-8) | scheda + `upl` (base64 di JSON UTF-8) |

**Oggetto `upl` (modalità B):** `id`, `offset`, `size`, `name`.

**Risposta a un blocco:** `Success`, `received`, `done`, `resync`, `ErrMsg`.

**Annullamento:** `POST /` con `{"Req":"CancelUpload","id":"…"}`.

**Login:** `POST /` con `{"Req":"DMALin","usrName":"…","psw":"…"}`; il token è nel cookie `token`.
