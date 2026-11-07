# Guida all'uso di `mock_openai_server.py`

## Cos'è

Un piccolo server HTTP locale, senza dipendenze esterne (usa solo la libreria standard di Python), che finge di essere un endpoint OpenAI-compatibile. Serve a collaudare `uAIChatClient.pas` senza spendere credito su un provider AI reale e senza dipendere dalla sua disponibilità in quel momento.

Non fa parte della libreria Pascal in sé: è uno strumento di supporto per lo sviluppo, da usare solo in locale durante il collaudo.

## Requisiti

Solo Python 3 (nessuna libreria aggiuntiva da installare: usa esclusivamente `http.server`, `json`, `re`, `time`, già inclusi in ogni installazione standard).

## Avvio

```bash
python3 mock_openai_server.py
```

Per impostazione predefinita, il server ascolta su `127.0.0.1:8765`. Per usare una porta diversa:

```bash
python3 mock_openai_server.py 9000
```

All'avvio, il server stampa a video l'indirizzo completo su cui è in ascolto e la chiave API attesa, così non serve ricordarli a memoria.

## Configurazione lato Pascal

Nella tua applicazione, punta `TAIChatProviderConfig` verso il server locale invece che verso un provider reale:

```pascal
_Config.Endpoint := 'http://127.0.0.1:8765/v1/chat/completions';
_Config.APIKey   := 'test-key-123';
_Config.Model    := 'qualunque-nome';  // il server finto lo accetta e lo restituisce, senza validarlo
```

Il nome del modello non viene verificato dal server: qualunque stringa passa, e viene semplicemente riportata nella risposta finta, utile per controllare che il valore configurato sia davvero quello che arriva a destinazione.

## Comportamento: risposta di successo

Una richiesta ben formata, con l'intestazione `Authorization: Bearer test-key-123`, riceve una risposta 200 con la stessa struttura di una vera risposta OpenAI. Il testo dell'ultimo messaggio dell'utente viene **incluso nella risposta finta** (i primi 60 caratteri), proprio per permetterti di verificare che il contenuto giusto sia arrivato al server — non solo che *una* risposta sia tornata indietro.

## Scenari di errore, attivabili a piacere

Il server osserva il testo del messaggio dell'utente (`role: "user"`) e, se contiene certe parole chiave letterali, simula uno scenario di errore specifico invece della risposta normale. Basta includere quella parola chiave da qualche parte nel testo della domanda/richiesta che invii tramite `uAIChatClient.pas`.

| Parola chiave nel messaggio | Cosa succede | Cosa puoi verificare |
|---|---|---|
| *(nessuna, richiesta normale)* | Risposta 200 corretta | Il percorso di successo, incluso l'eco del testo inviato |
| *(intestazione Authorization mancante o chiave diversa da `test-key-123`)* | Risposta 401, con un corpo JSON `{"error": {"message": ...}}` | Che `ErrorMessage` di `TAIChatAnswer` riporti il messaggio d'errore del provider |
| `TRIGGER_500` | Risposta 500, corpo JSON con un messaggio d'errore generico | La gestione di un errore HTTP generico del server, e - se hai configurato `MaxRetries > 0` - che il tentativo venga ripetuto (500 è tra i codici trattati come transitori) |
| `TRIGGER_MALFORMED_JSON` | Risposta con status 200, ma corpo JSON deliberatamente troncato/non valido | Che un errore di parsing JSON venga intercettato correttamente anche quando lo status HTTP sembra indicare successo |
| `TRIGGER_UNEXPECTED_SHAPE` | Risposta 200 con JSON valido, ma privo del campo `choices[0].message.content` | Che la mancanza del campo atteso produca un errore chiaro, non un'eccezione non gestita |
| `TRIGGER_SLOW` | Il server attende 5 secondi prima di rispondere | Il comportamento configurato in `TimeoutMs` di `TAIChatProviderConfig`, con un timeout più breve di 5 secondi |

**Esempio pratico**: per verificare la gestione dell'errore 500, basta chiedere qualcosa come *"TRIGGER_500 qual è la potenza del motore?"* — il server riconosce la parola chiave ovunque compaia nel testo e attiva lo scenario corrispondente, ignorando il resto della domanda.

## Verifica rapida da riga di comando, senza Pascal

Prima ancora di collegare l'applicazione Pascal, puoi verificare che il server risponda come previsto con `curl`:

```bash
# Risposta di successo
curl -s -X POST http://127.0.0.1:8765/v1/chat/completions \
  -H "Authorization: Bearer test-key-123" \
  -d '{"model":"test","messages":[{"role":"user","content":"ciao"}]}'

# Errore 500 simulato
curl -s -X POST http://127.0.0.1:8765/v1/chat/completions \
  -H "Authorization: Bearer test-key-123" \
  -d '{"model":"test","messages":[{"role":"user","content":"TRIGGER_500"}]}'
```

## Arresto del server

`Ctrl+C` nel terminale in cui è in esecuzione. Non salva né mantiene alcuno stato tra un riavvio e l'altro: ogni richiesta è valutata in modo indipendente, in base al solo contenuto di quella specifica richiesta.

## Quando passare al provider reale

Una volta verificato che `uAIChatClient.pas` gestisce correttamente tutti gli scenari sopra, basta cambiare `Endpoint` e `APIKey` in `TAIChatProviderConfig` per puntare al provider reale (OpenAI, DashScope, un server Ollama locale, ecc.) — nessun'altra modifica al codice Pascal è necessaria, perché il server finto parla deliberatamente lo stesso protocollo.
