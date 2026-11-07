# LBToolkit Speech-to-Text con Ararat Synapse

Il pacchetto usa esclusivamente **Ararat Synapse** per il trasporto HTTP/HTTPS. Non dipende da
`TFPHTTPClient` o `opensslsockets`.

## Unita'

- `src/uLBSpeechToText.pas`: `ISpeechToTextProvider` e `TSpeechToTextResult`.
- `src/uLBAzureFastTranscription.pas`: Azure Fast Transcription REST tramite `THTTPSend` (`httpsend`).
- `src/uLBSpeechToTextFactory.pas`: factory configurata da INI.

L'interfaccia e' sincrona. Coda e asincronia restano responsabilita' di IdroWorld.

## Richiesta Azure

Il provider usa la REST API GA `2025-10-15`:

    POST <Endpoint>/speechtotext/transcriptions:transcribe?api-version=2025-10-15

Il corpo e' `multipart/form-data` con:

- `definition`: JSON, per esempio `{"locales":["it-IT"]}`;
- `audio`: contenuto binario del file.

La chiave e' inviata nell'header `Ocp-Apim-Subscription-Key`. Con `THTTPSend` il Content-Type
principale viene impostato tramite `MimeType`, come richiesto da Synapse; il corpo e la risposta
usano `THTTPSend.Document`.

## TLS Synapse

Per default il provider include:

    ssl_openssl

Se il tuo LBToolkit usa il plugin OpenSSL 3 di Synapse, compila con:

    -dLB_SYNAPSE_OPENSSL3

cosi' viene inclusa:

    ssl_openssl3

Il server deve avere le librerie OpenSSL richieste dal plugin Synapse scelto.

## Configurazione

```ini
[SpeechToText]
Provider=Azure

[SpeechToText.Azure]
Endpoint=https://YOUR_RESOURCE_NAME.cognitiveservices.azure.com
SubscriptionKey=YOUR_SPEECH_RESOURCE_KEY
Locale=it-IT
ApiVersion=2025-10-15
TimeoutMs=120000
```

## Compilazione del programma autonomo

```sh
cd example
SYNAPSE=/percorso/synapse/source/lib ./build.sh
```

Con `ssl_openssl3`:

```sh
OPENSSL3=1 SYNAPSE=/percorso/synapse/source/lib ./build.sh
```

Comando equivalente:

```sh
fpc -MObjFPC -Sh -g -gl -vewnh \
  -FEbin -FUbin/units \
  -Fu../src -Fu/PERCORSO/SYNAPSE \
  SpeechToTextTest.lpr
```

## Esecuzione

```sh
./bin/SpeechToTextTest SpeechToText.ini registrazione.webm
./bin/SpeechToTextTest SpeechToText.ini registrazione.wav it-IT
```

## Timeout

`TimeoutMs` viene assegnato a `THTTPSend.Timeout`, ereditato da `TSynaClient`. Per Fast
Transcription il valore proposto e' 120000 ms; aumentarlo per audio grandi o reti lente.

## Formati

La classe assegna un Content-Type esplicito per WAV, MP3, OGG/OPUS, FLAC, WebM, AAC, WMA e AMR.
Per estensioni sconosciute usa `application/octet-stream`.

## Credenziali

La prima versione legge `SubscriptionKey` dall'INI. Il futuro meccanismo generico di offuscamento
potra' essere applicato nella factory senza cambiare `ISpeechToTextProvider`.

## Stato della verifica

Il contratto Azure e le API `THTTPSend` sono stati verificati sulla documentazione pubblica. In
questo ambiente non e' installato `fpc`, quindi il pacchetto non e' stato compilato qui. Prima
dell'integrazione in IdroWorld, compilare ed eseguire `SpeechToTextTest` con un file audio reale.
