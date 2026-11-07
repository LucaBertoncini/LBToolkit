# 09 · Esempio: provare l'AI Connector in 10 minuti

Programma: `esempi/EsempioAI.pas` con `esempi/Routes_Esempio.xml` (nella stessa cartella dell'eseguibile).
Sorgenti necessari: quelli di `codice-ai/` (versioni modificate) più il resto di LBToolkit e Synapse.

## Cosa fa

- Login con utente **Test** e password **Test**. Con una coppia diversa la pagina di login mostra **"Accesso negato"**.
- Pagina di prova locale `/prova`: avvia lo stesso login OAuth che avvia Claude. Se il login riesce il server rimanda il browser a `/prova/callback` e compare **"Tutto OK"**; se viene rifiutato compare **"Accesso negato"**.
- Quattro strumenti per l'AI, con valori fissi:

| Strumento visto dall'AI | Cosa restituisce |
|---|---|
| `api_countOdL` | `{"count":42}` |
| `api_countUsers` | `{"count":7}` |
| `api_listAssets` | tre asset con `code`, `name`, `state` |
| `api_findAsset` | un asset dato `code` (esempio di funzione con un parametro) |

Gli strumenti **non hanno un indirizzo `/api/...`**: non sono richieste HTTP raggiungibili dal browser, le chiama solo l'AI tramite `/mcp`. Il nome visto dall'AI è `<Code dell'area>_<Function>` (qui l'area è `api`). Se vuoi anche gli indirizzi `/api/countOdL` per il sito, vanno dichiarati a parte come endpoint Standard.

## Come leggere il codice

1. **`Routes_Esempio.xml`**: l'indirizzo `/mcp` (`Kind="AIConnector"`, senza `OpType`) e quattro `Endpoint` normali con `URI`, `OpType` e `Description`. Sono dichiarati una volta sola.
2. **`TDemoFunctions`**: registra le funzioni con `RegisterStandardWorker` e le descrive in `DescribeAIFunction` (qui l'unico parametro è `code` di `api_findAsset`).
3. **`TLoginGlue.Authenticate`**: l'unico punto in cui si decide chi entra. Nell'applicazione vera qui si controlla il database.
4. **`TDemoModule.hasPermission`**: qui lascia fare tutto a chi ha fatto login. Nell'applicazione vera si guarda il ruolo.
5. Programma principale: crea il server, il connettore MCP e il provider OAuth, e li mette nella catena di elaborazione.

## Prova locale (senza Claude)

```
EsempioAI 8080
```

Apri `http://127.0.0.1:8080/prova` e segui il link. Utente `Test` / password `Test` → "Tutto OK". Password sbagliata → la pagina di login mostra "Accesso negato".

All'avvio il programma stampa anche un **token** valido per l'utente Test. Con Claude Code, senza OAuth:

```
claude mcp add --transport http esempio http://127.0.0.1:8080/mcp --header "Authorization: Bearer <token stampato>"
```

Poi chiedi a Claude Code "quanti ordini di lavoro ci sono?".

## Prova con Claude.ai

Claude.ai si collega dal cloud di Anthropic, quindi il server deve essere raggiungibile da Internet **in HTTPS**. Per provare, senza installare un server pubblico, si può usare un servizio di tunnel che dia un indirizzo `https://...` e inoltri a `localhost:8080` (ad esempio `cloudflared tunnel --url http://localhost:8080` oppure ngrok; non li ho provati con questo esempio). Poi:

1. Avvia il programma indicando l'indirizzo pubblico: `EsempioAI 8080 https://xxxx.esempio.net` (senza `/` finale).
2. In Claude.ai: Impostazioni → Connettori → aggiungi un connettore personalizzato con l'indirizzo `https://xxxx.esempio.net/mcp`.
3. Claude apre la pagina di login: utente `Test`, password `Test`.
4. Chiedi a Claude: "Quanti ordini di lavoro ci sono?", "Elenca gli asset", "Dammi i dati dell'asset A-002".

I token dell'esempio sono solo in memoria: se riavvii il programma, Claude dovrà rifare il login.
