<!-- INIZIO_GUIDA_COMPLETA -->
# XPLC — Guida Completa per la Generazione Automatica di Programmi tramite AI

> **Scopo di questo documento:** Fornire a un'AI (o a qualsiasi programmatore esterno) tutte le
> informazioni necessarie per generare autonomamente e correttamente un programma XPLC in formato XML,
> senza che il lettore abbia conoscenza pregressa del sistema JACS o del linguaggio XPLC.

---

## 1. Cos'è XPLC

XPLC (eXtended PLC Logic) è il linguaggio di programmazione della logica della piattaforma **JACS**
(Just Another Control System). È basato su **macchine a stati finiti (FSM — Finite State Machine)**
descritte in XML e interpretate a runtime dall'engine `TXPLCManager`.

Il paradigma centrale è: *il sistema si trova sempre in uno stato ben definito; eventi (cambi di
ingresso, scadenza timer, flag condivisi) causano transizioni verso altri stati; ogni transizione
può eseguire azioni sull'hardware o sulle variabili interne.*

XPLC è progettato per il controllo industriale in tempo reale: gestione di sbarre, cancelli,
semafori, nastri, varchi pedonali, sistemi di rilevamento AI (telecamere YOLO) e qualsiasi
automazione che richieda reazione rapida agli ingressi fisici.

**Caratteristica fondamentale — sistema event-driven:**
Il motore XPLC non esegue un polling periodico fisso. Ogni VM si "sveglia" solo quando:
- Un ingresso fisico o shared variable cambia valore.
- Un timer scade.
- Un watchdog interno (ogni ~5 secondi) forza una rivalutazione.

Questo significa che le transizioni vengono valutate **immediatamente** al cambio di stato
degli ingressi, con latenza tipicamente < 10ms. Non è necessario progettare la logica
tenendo conto di cicli di scansione fissi come nei PLC tradizionali.

---

## 2. Architettura Multi-VM e Shared Memory

### 2.1 Macchine Virtuali Parallele

L'engine XPLC esegue in parallelo **più VM** (Virtual Machine), ciascuna definita in un file XML
separato. Le VM sono indipendenti: hanno i propri stati, timer, contatori e ciclo di valutazione.
Non si "chiamano" direttamente, ma comunicano tramite la Shared Memory (sezione 2.2).

Questo consente di **decomporre problemi complessi** in macchine specializzate:

- Una VM **Main/Supervisor** coordina le altre VM tramite shared variables.
- Una VM **Observer** monitora le telecamere e scrive flag (es. `VehiclePresent = 1`).
- Una VM **Actuator** legge i flag dell'Observer e aziona l'hardware.
- Una VM **Blinker** gestisce il lampeggio periodico di un'uscita (completamente autonoma).
- Una VM **Device** gestisce un singolo dispositivo fisico (sbarra, tornello, semaforo).

**Regola progettuale:** se una singola macchina supera 8–10 stati, valutare la divisione in due VM.
Ogni VM deve avere una **singola responsabilità** — gestisce un dispositivo fisico o un aspetto
logico del sistema, non entrambi.

### 2.2 Shared Memory — Variabili Globali

La Shared Memory è l'unico canale di comunicazione tra VM. È un dizionario chiave→valore visibile
da tutte le VM in lettura e scrittura, protetto da mutex per l'accesso concorrente.

Le variabili condivise sono dichiarate in un **unico file XML globale** (`SharedVars.xml`)
referenziato dalla configurazione dell'applicazione (chiave `GlobalVar` nel `config.ini`):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<SharedVariables>
  <Var Name="MasterEnable"         InitialValue="0"  />
  <Var Name="BlinkEnable"          InitialValue="0"  />
  <Var Name="StatoSbarraIngresso"  InitialValue="0"  />
  <Var Name="Aziona_TornelloMensa" InitialValue="0"  />
  <Var Name="SystemBooted"         InitialValue="0"  />
</SharedVariables>
```

Ogni `<Var>` ha:
- `Name` — identificatore univoco (usato come alias nelle condizioni e nelle azioni).
- `InitialValue` — stringa convertita automaticamente in `Boolean`, `Integer`, `Float` o `String`
  in base al valore (`True`/`False` → Boolean; numero intero → Integer; numero decimale → Float).

> ⚠️ **Errore critico di battitura:** l'attributo corretto è `InitialValue` (non `InitalValue`).
> Il typo non genera errori a runtime ma lascia la variabile non inizializzata — errore silenzioso
> difficile da diagnosticare. Verificare sempre l'ortografia prima del deploy.

> ⚠️ **Regola fondamentale: le shared variables non si dichiarano nei file XML delle singole VM.**
> Il `TXPLCManager` carica `SharedVars.xml` **una sola volta** all'avvio e poi passa il
> riferimento alla stessa mappa a tutte le VM. Ogni VM le riceve già inizializzate e pronte all'uso.
> Aggiungere o dichiarare un `<Input>` o `<Output>` o `<Var>` riferito a una variabile shared
> dentro un file `.xml` di una singola VM è un grave errore strutturale. Esse si usano direttamente
> indicandone il nome nel codice.

**Come si usano nelle VM:**

- **Lettura** (nelle `Expression`): il `Name` della variabile si usa direttamente come qualsiasi
  altro alias di ingresso — es. `BlinkEnable EQ 1` o `StatoSbarraIngresso NE 0`.
- **Scrittura** (nelle azioni): si usa `<SetOutput Output="NomeVar" Value="..."/>`. Il motore
  cerca prima nelle uscite fisiche, poi nelle shared variables — non c'è distinzione sintattica.
- **Reattività**: quando una VM scrive una shared variable, l'evento di cambio valore viene
  propagato a tutte le VM che la monitorano, svegliando il loro ciclo di valutazione.

### 2.3 Pattern di Comunicazione tra VM

#### Pattern A — Flag booleano (on/off)

Il meccanismo più semplice: una VM scrive `1`, l'altra legge e reagisce.

```
VM_Main  →  scrive  GestioneSbarraIngresso = 1
VM_Sbarra →  legge  GestioneSbarraIngresso NE 0  →  esce da Idle
VM_Main  →  scrive  GestioneSbarraIngresso = 0
VM_Sbarra →  legge  GestioneSbarraIngresso EQ 0  →  torna a Idle
```

#### Pattern B — Variabile di comando multi-valore (consigliato per attuatori)

Quando un dispositivo può trovarsi in più posizioni o ricevere comandi distinti, si usa
una shared variable con semantica numerica:

```
Valore 0 = nessun comando / fermo
Valore 1 = comando A (es. apri, CounterClockWise, verde)
Valore 2 = comando B (es. chiudi, ClockWise, rosso)
```

La VM **produttrice** (tipicamente SM_Main) scrive il comando.
La VM **consumatrice** (la macchina del dispositivo) legge il valore e:
1. Esegue il movimento/azione richiesta.
2. **Azzera la variabile a 0** nell'`OnExit` dello stato di esecuzione, *prima* di tornare
   allo stato di attesa — in modo che SM_Main possa rilevare il completamento e il ciclo
   successivo parta da uno stato noto.

```xml
<!-- VM consumatrice: OnExit dello stato di esecuzione -->
<OnExit>
  <StopTimer Timer="TAttesa"/>
  <SetOutput Output="Aziona_TornelloMensa" Value="0"/>
</OnExit>
```

> **Attenzione:** azzerare la variabile nell'`OnExit` (non nelle `Actions` della transizione
> uscente) garantisce che il reset avvenga anche se in futuro vengono aggiunte transizioni
> alternative verso stati diversi.

#### Pattern C — Handshake request/acknowledge

Per sincronizzazioni più precise tra VM:

```
VM_Main  →  scrive  StartCycle = 1
VM_Worker →  rileva StartCycle EQ 1  →  esegue lavoro
VM_Worker →  scrive  CycleDone = 1, StartCycle = 0
VM_Main  →  rileva  CycleDone EQ 1  →  prosegue
VM_Main  →  scrive  CycleDone = 0
```

### 2.4 Oggetti di Sistema Leggibili

Oltre alle variabili dichiarate, le condizioni possono leggere direttamente:

| Tipo oggetto | Sintassi nell'`Expression` | Valore |
|---|---|---|
| Ingresso fisico PLC | `AliasInput` (dichiarato in `<Inputs>`) | `0` o `1` |
| Uscita fisica PLC | `AliasOutput` (dichiarato in `<Outputs>`) | `0` o `1` |
| Variabile Shared Memory | `NomeVar` (da `SharedVars.xml`) | dipende dal tipo |
| Scadenza timer | `TIMEOUT(NomeTimer)` | `True` = scaduto, `False` = in corso |
| Valore contatore | `NomeContatore` | intero corrente |
| Rilevamento telecamera | `AliasInputCamera` (dichiarato in `<Inputs>`) | `0` o `1` |

> ⚠️ `TIMEOUT(NomeTimer)` è l'**unica** sintassi supportata per verificare la scadenza di un
> timer nelle espressioni. La forma `NomeTimer_Timeout` è la rappresentazione interna usata
> dall'engine dopo l'espansione di `PrepareExpression` — non deve mai comparire nei file XML
> scritti dall'utente o generati dall'AI.

I valori delle telecamere JACS (detection AI YOLO) vengono esposti come ingressi virtuali con ID
numerici fissi assegnati in `devices.xml`. Si dichiarano in `<Inputs>` con un alias descrittivo
esattamente come un ingresso fisico.

---

## 3. Struttura del File XML XPLC

Un file XPLC è un documento XML valido che descrive **una singola VM**. La struttura è la
seguente (tutte le sezioni di dichiarazione sono opzionali tranne `<StateMachine>`):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLC Id="[numero_intero]" Code="[nome_descrittivo]">

  <!-- SEZIONE 1: Ingressi fisici e virtuali (es. telecamere) -->
  <Inputs>
    <Input Id="[id_globale]" Alias="[nome_simbolico]"/>
    ...
  </Inputs>

  <!-- SEZIONE 2: Uscite fisiche (le shared vars NON vanno dichiarate qui) -->
  <Outputs>
    <Output Id="[id_globale]" Alias="[nome_simbolico]"/>
    ...
  </Outputs>

  <!-- SEZIONE 3: Timer locali alla VM -->
  <Timers>
    <Timer Name="[nome]" DefaultMs="[millisecondi]"/>
    ...
  </Timers>

  <!-- SEZIONE 4: Contatori locali alla VM -->
  <Counters>
    <Counter Name="[nome]" InitialValue="[intero, default 0]"/>
    ...
  </Counters>

  <!-- SEZIONE 5: Preset di uscite (SetOutput sono figli diretti) -->
  <OutputPresets>
    <Preset Name="[NomePreset]">
      <SetOutput Output="[Alias]" Value="[valore]"/>
      ...
    </Preset>
  </OutputPresets>

  <!-- SEZIONE 6: La macchina a stati (Stati sono figli diretti) -->
  <StateMachine Initial="[NomeStatoIniziale]">
    <State Name="...">
      ...
    </State>
    ...
  </StateMachine>

</XPLC>
```

**Attributi dell'elemento radice `<XPLC>`:**
- `Id` (opzionale): identificatore numerico univoco del controllore nell'applicazione.
- `Code` (opzionale): etichetta testuale descrittiva (usata nei log di sistema).

---

## 4. Dichiarazioni di I/O

### 4.1 Ingressi `<Inputs>`

Ogni `<Input>` mappa un ID globale del dispositivo a un alias simbolico usato nel codice:

```xml
<Inputs>
  <Input Id="101"   Alias="FC_SbarraAperta"/>
  <Input Id="102"   Alias="FC_SbarraChiusa"/>
  <Input Id="103"   Alias="Manutenzione"/>
  <Input Id="11002" Alias="Cam01_Vehicle"/>
  <Input Id="11009" Alias="Cam05_Human"/>
</Inputs>
```

- `Id`: numero intero assegnato al dispositivo in `devices.xml`. Gli ID delle telecamere JACS
  seguono una convenzione numerica specifica (11000+). L'AI riceve la lista degli ID
  disponibili come parte del contesto del problema.
- `Alias`: nome simbolico libero (pattern: `[A-Za-z_][A-Za-z0-9_]*`). Usato ovunque nel codice
  al posto dell'ID numerico. Deve essere descrittivo della funzione fisica.

### 4.2 Uscite `<Outputs>`

Identica struttura agli ingressi. Le uscite fisiche (relè, segnali digitali) si dichiarano tutte qui:

```xml
<Outputs>
  <Output Id="201" Alias="ApriSbarra"/>
  <Output Id="202" Alias="AbilitaMovimento"/>
  <Output Id="207" Alias="SemaforoPedonale"/>
</Outputs>
```

> **Nota:** le variabili Shared Memory usate come destinazione di `SetOutput` **non** vanno
> dichiarate in `<Outputs>`. Il motore le risolve direttamente dalla shared map tramite il nome.
> Dichiarare una shared variable in `<Outputs>` con `Id="0"` o un ID fittizio è un errore logico.

### 4.3 Output Preset `<OutputPresets>`

Gli `OutputPresets` permettono di definire gruppi di `SetOutput` da applicare con un'unica
istruzione `<ApplyPreset>`.
> ⚠️ **Regola grammaticale fondamentale:** non inserire nessun tag contenitore `<Actions>` dentro `<Preset>`.
> Le istruzioni `<SetOutput>` devono essere **figli diretti** dell'elemento `<Preset>`.

```xml
<OutputPresets>
  <Preset Name="TuttiRossi">
    <SetOutput Output="SemIngresso"        Value="0"/>
    <SetOutput Output="SemMensa"           Value="0"/>
    <SetOutput Output="SemScaletta"        Value="0"/>
  </Preset>
</OutputPresets>
```

I preset accettano **solo** `<SetOutput>` come azioni. Nell'`OnEnter` di uno stato:

```xml
<OnEnter>
  <ApplyPreset Preset="TuttiRossi"/>
  <StartTimer Timer="TBlink"/>
</OnEnter>
```

---

## 5. Dichiarazioni di Timer e Contatori

### 5.1 Timer

I timer sono **locali alla VM** che li dichiara. Non sono condivisi tra VM.

```xml
<Timers>
  <Timer Name="TWait"           DefaultMs="10000"/>
  <Timer Name="TBlink"          DefaultMs="500"/>
  <Timer Name="TImpulso"        DefaultMs="200"/>
  <Timer Name="TAttesaApertura" DefaultMs="15000"/>
</Timers>
```

- `Name`: identificatore simbolico del timer.
- `DefaultMs`: durata predefinita in millisecondi. Può essere sovrascritta al momento dell'avvio
  tramite l'attributo `DurationMs` dell'azione `StartTimer`.

**Ciclo di vita di un timer:**
1. Viene avviato da `<StartTimer Timer="NomeTimer"/>`.
2. Rimane in esecuzione finché non scade o viene fermato da `<StopTimer Timer="NomeTimer"/>`.
3. Dopo la scadenza, `TIMEOUT(NomeTimer)` restituisce `True`.
4. Rimane `True` finché non viene riavviato con `StartTimer`.
5. **Best practice:** avviare sempre il timer in `OnEnter` e fermarlo in `OnExit`.

**Comportamento del timer e self-loop:**
Quando una transizione ha come `Target` lo **stato corrente** (self-loop), il motore esegue
comunque `OnExit` e `OnEnter`. Questo significa che:
- `StopTimer` in `OnExit` azzera il timeout.
- `StartTimer` in `OnEnter` riavvia il timer da zero.

Questo è il meccanismo corretto per implementare **retry con timeout reset** senza stati
intermedi aggiuntivi:

```xml
<State Name="AttesaFineCorsa">
  <OnEnter>
    <SetOutput Output="ComandoMotore" Value="1"/>
    <StartTimer Timer="TAttesa"/>
    <Log Message="Comando inviato, attesa fine corsa."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TAttesa"/>
  </OnExit>
  <Transitions>
    <!-- Fine corsa raggiunto: procedi -->
    <Transition Target="Completato">
      <Conditions>
        <Condition Expression="FC_Posizione EQ 1"/>
      </Conditions>
    </Transition>
    <!-- Timeout: re-invia comando (self-loop resetta timer e output) -->
    <Transition Target="AttesaFineCorsa">
      <Conditions>
        <Condition Expression="TIMEOUT(TAttesa) AND FC_Posizione EQ 0"/>
      </Conditions>
      <Actions>
        <Log Message="Timeout — fine corsa non rilevato. Nuovo tentativo."/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

> **Importante:** il self-loop funziona correttamente **solo** se il timer viene fermato in
> `OnExit` e riavviato in `OnEnter`. Senza `StopTimer` in `OnExit`, il timer scaduto
> rimane nello stato `TIMEOUT=True` anche dopo il rientro nello stato, e la condizione
> di retry scatterebbe immediatamente ad ogni ciclo.

### 5.2 Contatori

I contatori sono **locali alla VM** e mantengono un valore intero.

```xml
<Counters>
  <Counter Name="RetryCount" InitialValue="0"/>
  <Counter Name="CycleCount" InitialValue="0"/>
</Counters>
```

Nelle azioni sono disponibili: `IncrementCounter`, `DecrementCounter`, `ResetCounter`.
Il valore corrente è leggibile nelle condizioni usando direttamente il nome del contatore.
Nei messaggi `<Log>` il valore è interpolabile con la sintassi `%NomeContatore%`.

---

## 6. La Macchina a Stati

### 6.1 Elemento `<StateMachine>`

```xml
<StateMachine Initial="NomeStatoIniziale">
  <State Name="..." />
  <State Name="..." />
  ...
</StateMachine>
```

- `Initial` (obbligatorio): nome esatto di uno degli stati dichiarati. La VM entra in questo stato all'avvio
  ed esegue immediatamente le azioni `OnEnter`.
- Gli stati possono essere in qualsiasi ordine nel file.
- ⚠️ **Regola grammaticale fondamentale:** non inserire nessun tag contenitore `<States>` dentro `<StateMachine>`.
  Gli elementi `<State>` devono essere **figli diretti** dell'elemento `<StateMachine>`.

### 6.2 Elemento `<State>`

```xml
<State Name="NomeStato">
  <OnEnter>
    <!-- azioni eseguite una sola volta all'ingresso (niente contenitori intermedi) -->
  </OnEnter>
  <OnExit>
    <!-- azioni eseguite una sola volta all'uscita (niente contenitori intermedi) -->
  </OnExit>
  <Transitions>
    <!-- transizioni valutate ad ogni evento finché si è in questo stato -->
  </Transitions>
</State>
```

Tutte e tre le sottosezioni (`OnEnter`, `OnExit`, `Transitions`) sono opzionali.
Uno stato senza `<Transitions>` è un **stato terminale** (la VM rimane lì fino a restart).

**Ordine di esecuzione durante una transizione:**
1. `Actions` della transizione (se presenti)
2. `OnExit` dello stato corrente
3. `OnEnter` dello stato destinazione

> **Nota:** `OnExit` viene eseguito anche nei self-loop (transizione verso lo stesso stato).
> Questo è intenzionale e consente il reset corretto di timer e output.

**Ciclo di valutazione:** la VM viene svegliata dagli eventi (cambio ingresso, scadenza timer).
A ogni risveglio valuta le transizioni dello stato corrente in ordine. Il loop `repeat..until`
interno continua a rivalutare finché nessuna transizione scatta ulteriormente nello stesso ciclo.

---

## 7. Azioni

Le azioni sono i comandi che la VM esegue. Possono apparire in `<OnEnter>`, `<OnExit>` e
nella sezione `<Actions>` di una transizione. Vengono eseguite nell'ordine in cui compaiono.

### 7.1 Tabella di tutte le azioni

| Elemento XML | Attributi | Descrizione |
|---|---|---|
| `<SetOutput>` | `Output="Alias"` `Value="valore"` | Imposta un'uscita fisica o una variabile Shared Memory |
| `<StartTimer>` | `Timer="Nome"` `DurationMs="ms"` (opz.) | Avvia il timer; se `DurationMs` è omesso usa `DefaultMs` |
| `<StopTimer>` | `Timer="Nome"` | Ferma il timer; `TIMEOUT()` torna a `False` |
| `<IncrementCounter>` | `Counter="Nome"` | Incrementa il contatore di 1 |
| `<DecrementCounter>` | `Counter="Nome"` | Decrementa il contatore di 1 |
| `<ResetCounter>` | `Counter="Nome"` | Azzera il contatore (torna a `InitialValue`) |
| `<Log>` | `Message="testo"` | Scrive un messaggio nel log; supporta `%NomeVariabile%` per interpolazione |
| `<ApplyPreset>` | `Preset="NomePreset"` | Esegue un gruppo di `SetOutput` predefinito in `<OutputPresets>` |

### 7.2 Interpolazione nei messaggi Log

Il tag `<Log>` supporta l'interpolazione del valore corrente di qualsiasi variabile
(ingresso, uscita, contatore, shared variable) usando la sintassi `%NomeAlias%`:

```xml
<Log Message="Stato FC_Apertura=%FC_SbarraAperta% FC_Chiusura=%FC_SbarraChiusa% Retry=%RetryCount%"/>
```

Questo è particolarmente utile per messaggi diagnostici: se una transizione non avviene
come atteso, il log mostra i valori effettivi delle variabili al momento dell'evento.

### 7.3 Quando usare `Actions` nelle transizioni vs `OnEnter`

La regola generale è: **le azioni che inizializzano uno stato appartengono a `OnEnter`**.
Le `Actions` nelle transizioni servono per:

1. **Log di transizione** — registrare il motivo specifico del cambio di stato:
   ```xml
   <Actions>
     <Log Message="Timeout fine corsa — FC=%FC_Posizione%. Nuovo tentativo."/>
   </Actions>
   ```

2. **Azzeramento di shared variable al completamento** — la VM consumatrice segnala a
   SM_Main che il comando è stato eseguito:
   ```xml
   <Actions>
     <SetOutput Output="Aziona_Tornello" Value="0"/>
     <Log Message="Comando completato, variabile azzerata."/>
   </Actions>
   ```

3. **Reset di contatori** prima di cambiare stato:
   ```xml
   <Actions>
     <ResetCounter Counter="RetryCount"/>
   </Actions>
   ```

> Non usare `Actions` nelle transizioni per impostare uscite fisiche che definiscono
> il comportamento del nuovo stato — quello appartiene a `OnEnter`. Se la stessa uscita
> deve essere impostata da più transizioni verso lo stesso stato, c'è un difetto di
> progettazione: consolidare in `OnEnter`.

---

## 8. Transizioni e Condizioni

### 8.1 Struttura di una Transizione

```xml
<Transition Target="NomeStatoDestinazione">
  <Conditions>
    <Condition Expression="Alias1 EQ 1 AND Alias2 EQ 0"/>
  </Conditions>
  <Actions>
    <!-- opzionale -->
    <Log Message="Motivo della transizione"/>
  </Actions>
</Transition>
```

Ogni `<Transition>` ha:
- `Target` (obbligatorio): nome dello stato di destinazione, deve esistere nella stessa `<StateMachine>`.
- `<Conditions>` (obbligatorio): contiene uno o più `<Condition>`, ciascuno con `Expression`.
- `<Actions>` (opzionale): eseguite nell'ordine: `Actions` transizione → `OnExit` → `OnEnter`.

> **Priorità semantica:** l'ordine in `<Transitions>` è semanticamente significativo.
> Il motore valuta le transizioni nell'ordine in cui compaiono e la **prima con espressione
> vera vince**; le successive vengono ignorate. Questo vale anche tra transizioni operative
> dello stesso tipo: se `FC_raggiunto EQ 1` e `TIMEOUT` sono entrambi veri nello stesso
> ciclo, vince la transizione dichiarata per prima. In generale:
> - Prima: transizioni di **sicurezza/emergenza** (abort, stop esterno).
> - Poi: transizioni **operative normali** (fine corsa raggiunto, condizione attesa).
> - Infine: transizioni di **timeout/retry** (fallback se le precedenti non scattano).

### 8.2 L'attributo `Expression`

Ogni `<Condition>` ha un unico attributo obbligatorio:

```xml
<Condition Expression="espressione booleana"/>
```

L'espressione è una stringa di testo che il motore passa all'engine `TExprEvaluator` dopo
una fase di pre-processing (`PrepareExpression`) che normalizza gli operatori keyword in simboli.

**Questa è l'unica sintassi supportata.** Non esistono attributi `Input`, `Op`, `Value` nel
formato attuale.

### 8.3 Divieto di Operazioni Aritmetiche e Funzioni Matematiche

> ⚠️ **Regola fondamentale di valutazione:** L'engine XPLC utilizza un analizzatore di espressioni
> configurato esclusivamente per la logica booleana (`BuiltIns := [bcBoolean]`).
> Ciò significa che **qualsiasi operatore matematico (`+`, `-`, `*`, `/`) o funzione aritmetica
> (come `Int()`, `Round()`) NON è supportato e causerà un errore di sintassi a runtime.**
> Le espressioni devono contenere esclusivamente operatori logici di confronto e variabili booleane.

### 8.4 Operatori di Confronto

Gli operatori si possono scrivere sia come **keyword** (consigliato per leggibilità) sia come
**simbolo**. `PrepareExpression` li converte automaticamente in simboli prima della valutazione:

| Keyword | Simbolo equivalente | Significato |
|---|---|---|
| `EQ` | `=` | Uguale |
| `NE` | `<>` | Diverso |
| `GT` | `>` | Maggiore |
| `LT` | `<` | Minore |
| `GE` | `>=` | Maggiore o uguale |
| `LE` | `<=` | Minore o uguale |

> **Raccomandazione:** usare sempre la forma keyword (`EQ`, `NE`, …) per evitare problemi
> di escaping XML con i caratteri `<` e `>`.

### 8.5 Operatori Logici

All'interno di una singola `Expression` sono disponibili:

| Keyword | Significato | Note |
|---|---|---|
| `AND` | Congiunzione logica | Case-insensitive |
| `OR` | Disgiunzione logica | Case-insensitive |
| `NOT` | Negazione | Case-insensitive |

Le parentesi `( )` sono supportate e consentono di controllare la precedenza:

```xml
<Condition Expression="(Cam01_Vehicle EQ 0 AND Cam02_Vehicle EQ 0) OR EmergencyStop EQ 1"/>
<Condition Expression="NOT SbarraAperta EQ 1"/>
<Condition Expression="TIMEOUT(TWait) AND Cam01_Vehicle EQ 0 AND BlinkEnable EQ 0"/>
```

### 8.6 Identificatori nell'Expression

| Tipo | Sintassi nell'Expression | Valore restituito |
|---|---|---|
| Ingresso fisico PLC | `AliasInput` (dichiarato in `<Inputs>`) | `0` o `1` |
| Uscita fisica PLC | `AliasOutput` (dichiarato in `<Outputs>`) | `0` o `1` |
| Variabile Shared Memory | `NomeVar` (da `SharedVars.xml`) | dipende dal tipo |
| Scadenza timer | `TIMEOUT(NomeTimer)` | booleano (`True`/`False`) |
| Valore contatore | `NomeContatore` | intero corrente |

### 8.6.1 Funzione `TIMEOUT()` e Regola di Espansione

La funzione `TIMEOUT(NomeTimer)` restituisce direttamente un valore **booleano** e può essere
usata direttamente nelle espressioni senza confronto esplicito.

**Come funziona internamente:** `PrepareExpression` espande `TIMEOUT(NomeTimer)` nella
sottoespressione `(NomeTimer_Timeout = 1)` prima della valutazione.

```xml
<!-- CORRETTO -->
<Condition Expression="TIMEOUT(TWait)"/>
<Condition Expression="TIMEOUT(TWait) AND FC_Chiusura EQ 0"/>
<Condition Expression="NOT TIMEOUT(TSafety) AND SbarraChiusa EQ 1"/>
```

> ⚠️ **Non usare `TIMEOUT()` in confronti espliciti.**
> Siccome `TIMEOUT()` si espande internamente in `(TWait_Timeout = 1)`, aggiungere un confronto
> esplicito (es. `TIMEOUT(TWait) EQ 1` o `TIMEOUT(TWait) EQ True`) produce una stringa
> malformata dopo la macro-espansione (es. `(TWait_Timeout = 1) = 1` oppure `(TWait_Timeout = 1) = True`).
> Il parser non è in grado di valutarla e genera un errore silenzioso, lasciando la condizione a False.
>
> ```xml
> <!-- ERRATO -->
> <Condition Expression="TIMEOUT(TWait) EQ 1"/>
>
> <!-- CORRETTO -->
> <Condition Expression="TIMEOUT(TWait)"/>
> ```

### 8.7 Esempi di Expression

```xml
<!-- Confronto semplice -->
<Condition Expression="SbarraChiusa EQ 1"/>

<!-- Variabile condivisa multi-valore (0=idle, 1=apri, 2=chiudi) -->
<Condition Expression="StatoSbarraIngresso EQ 2"/>
<Condition Expression="StatoSbarraIngresso NE 0"/>

<!-- AND di più condizioni -->
<Condition Expression="Cam01_Vehicle EQ 0 AND Cam02_Vehicle EQ 0 AND Cam03_Vehicle EQ 0"/>

<!-- OR (due ingressi alternativi) -->
<Condition Expression="Cam02_Vehicle EQ 1 OR Cam05_Vehicle EQ 1"/>

<!-- Timer scaduto -->
<Condition Expression="TIMEOUT(T10s)"/>

<!-- Timer scaduto + condizione operativa -->
<Condition Expression="TIMEOUT(TAttesa) AND FC_Chiusura EQ 0"/>

<!-- Contatore oltre soglia -->
<Condition Expression="RetryCount GE 3"/>

<!-- Espressione composita con parentesi -->
<Condition Expression="(Cam01_Vehicle EQ 0 AND Cam02_Vehicle EQ 0) AND TornelloAperto EQ 0"/>
```

### 8.8 Più `<Condition>` nella stessa transizione

Una `<Transition>` può contenere più `<Condition>` valutate in **AND implicito**.
In pratica si consiglia di usare **una sola `<Condition>` per transizione** con tutta la
logica nell'`Expression`, per chiarezza e per evitare ambiguità sull'ordine di valutazione.

```xml
<!-- Sconsigliato: due Condition separate -->
<Conditions>
  <Condition Expression="SbarraChiusa EQ 1"/>
  <Condition Expression="BlinkEnable EQ 0"/>
</Conditions>

<!-- Consigliato: unica Expression -->
<Conditions>
  <Condition Expression="SbarraChiusa EQ 1 AND BlinkEnable EQ 0"/>
</Conditions>
```

---

## 9. Linee Guida per la Generazione AI

### 9.1 Processo di Analisi Prima della Generazione

Prima di scrivere XML, l'AI deve:

1. **Identificare tutti gli attori** del sistema (dispositivi fisici, telecamere, operatori).
2. **Elencare gli ingressi** con i loro ID e alias descrittivi.
3. **Elencare le uscite** con i loro ID e alias descrittivi.
4. **Identificare le shared variables** necessarie alla comunicazione tra VM.
5. **Disegnare mentalmente il diagramma a stati** in forma testuale prima di scrivere XML.
6. **Decidere la decomposizione in VM**: una sola VM se la logica è lineare (<8 stati);
   più VM se ci sono comportamenti paralleli o cicli intermittenti (es. blinker).
7. **Identificare tutti i casi di errore e timeout** e progettare transizioni di sicurezza.
8. **Verificare i conflitti di output**: due VM non devono scrivere sullo stesso ID fisico.

### 9.2 Regole Obbligatorie

**R1 — Stato iniziale sempre sicuro:**
Ogni VM deve avere un `Initial` che corrisponde a uno stato `Idle` o di attesa. Lo stato
iniziale deve impostare tutte le uscite in uno stato sicuro e noto (`OnEnter` con tutti
gli output a 0 o al valore di riposo).

**R2 — Ogni stato deve avere una via d'uscita:**
Nessuno stato (eccetto terminali intenzionali) può bloccarsi per sempre. Se uno stato
dipende da un evento esterno incerto, aggiungere sempre una transizione di timeout.

**R3 — I timer vanno sempre fermati in `OnExit`:**
Se un timer viene avviato in `OnEnter`, deve essere fermato in `OnExit`. Questo vale
**anche** per i self-loop: il timer viene fermato dall'`OnExit` e riavviato dall'`OnEnter`
garantendo il reset corretto ad ogni iterazione del retry.

**R4 — Le transizioni di sicurezza hanno priorità:**
Dichiarare come prima `<Transition>` le condizioni di abort/emergenza (stop esterno,
disabilitazione, veicolo in zona pericolosa), prima di qualsiasi transizione operativa.

**R5 — Non duplicare la logica in `Actions` e `OnEnter`:**
Se un'azione deve essere eseguita all'ingresso di uno stato, metterla in `OnEnter`.
Le `Actions` della transizione servono per log diagnostici, azzeramento di shared variables
al completamento di un comando, e reset di contatori.

**R6 — Usare alias descrittivi:**
Gli alias devono descrivere la funzione fisica (`ApriSbarra`, `FC_SbarraChiusa`,
`Cam01_Vehicle`) e non abbreviazioni criptiche. Il codice deve essere leggibile senza
consultare `devices.xml`.

**R7 — Commentare stati e transizioni non ovvi:**
Usare i commenti XML `<!-- ... -->` per spiegare la logica di stati complessi, le priorità
tra transizioni e le condizioni di abort. Il file XML deve essere autoesplicativo.

**R8 — Inizializzare tutte le uscite nello stato iniziale:**
Lo stato `Idle` (o il primo stato della VM) deve contenere un `OnEnter` che porta tutte
le uscite a un valore sicuro e definito.

**R9 — Verificare la completezza del self-loop:**
Quando si usa un self-loop per retry, verificare che `OnExit` fermi il timer e che
`OnEnter` lo riavvii. Un self-loop senza questa coppia non resetta il timeout e causerà
un loop infinito immediato.

**R10 — Non assumere lo stato degli output fisici al boot:**
Al boot, lo stato fisico degli attuatori è sconosciuto. Lo stato `Idle` deve sempre
impostare esplicitamente tutti gli output, non fare affidamento sullo stato precedente.

**R11 — Utilizzare il protocollo `SystemBooted`:**
Per evitare partenze intempestive durante il boot del sistema, la prima transizione dallo
stato iniziale di boot (es. `Boot` o `Init`) verso gli stati operativi deve attendere
il flag di handshake globale `SystemBooted EQ 1`, che viene impostato a 1 dalla VM
Supervisor principale solo dopo la stabilizzazione di tutti i moduli.

---

### 9.3 Pattern Ricorrenti

**Pattern 1 — Stato con Timeout di Sicurezza:**
```xml
<State Name="WaitSomething">
  <OnEnter>
    <StartTimer Timer="TSafety"/>
    <Log Message="Attesa condizione X."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TSafety"/>
  </OnExit>
  <Transitions>
    <!-- PRIMA: timeout di sicurezza (ritorno a stato sicuro) -->
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="TIMEOUT(TSafety)"/>
      </Conditions>
      <Actions>
        <Log Message="Timeout sicurezza — ritorno a Idle"/>
      </Actions>
    </Transition>
    <!-- POI: condizione operativa normale -->
    <Transition Target="NextState">
      <Conditions>
        <Condition Expression="SomeInput EQ 1"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 2 — Retry con Self-Loop (reset timer automatico):**
```xml
<State Name="AttesaFineCorsa">
  <OnEnter>
    <SetOutput Output="ComandoMotore" Value="1"/>
    <StartTimer Timer="TAttesa"/>
    <Log Message="Comando inviato, attesa fine corsa."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TAttesa"/>
  </OnExit>
  <Transitions>
    <!-- Fine corsa raggiunto: procedi -->
    <Transition Target="Completato">
      <Conditions>
        <Condition Expression="FC_Posizione EQ 1"/>
      </Conditions>
      <Actions>
        <Log Message="Fine corsa rilevato."/>
      </Actions>
    </Transition>
    <!-- Timeout senza fine corsa: re-invia impulso (self-loop) -->
    <Transition Target="AttesaFineCorsa">
      <Conditions>
        <Condition Expression="TIMEOUT(TAttesa) AND FC_Posizione EQ 0"/>
      </Conditions>
      <Actions>
        <Log Message="Fine corsa non rilevato. Nuovo tentativo."/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 3 — Impulso su Output (200ms) con attesa fine corsa:**
Usato per dispositivi FAAC B614 e simili che reagiscono al fronte del segnale, non al livello.
```xml
<!-- Stato impulso: attiva l'output per la durata del TImpulso -->
<State Name="ImpulsoApertura">
  <OnEnter>
    <SetOutput Output="AbilitaMovimento" Value="1"/>
    <SetOutput Output="ApriSbarra"       Value="1"/>
    <StartTimer Timer="TImpulso"/>
    <Log Message="Impulso OPEN attivo (200ms)."/>
  </OnEnter>
  <OnExit>
    <SetOutput Output="ApriSbarra" Value="0"/>
    <StopTimer Timer="TImpulso"/>
  </OnExit>
  <Transitions>
    <Transition Target="AttesaFC_Apertura">
      <Conditions>
        <Condition Expression="TIMEOUT(TImpulso)"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>

<!-- Stato attesa: aspetta il fine corsa con timeout e retry -->
<State Name="AttesaFC_Apertura">
  <OnEnter>
    <StartTimer Timer="TAttesaApertura"/>
    <Log Message="Attesa fine corsa apertura."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TAttesaApertura"/>
  </OnExit>
  <Transitions>
    <Transition Target="SbarraAperta">
      <Conditions>
        <Condition Expression="FC_SbarraAperta EQ 1"/>
      </Conditions>
    </Transition>
    <!-- Retry: re-invia impulso se fine corsa non arriva -->
    <Transition Target="ImpulsoApertura">
      <Conditions>
        <Condition Expression="TIMEOUT(TAttesaApertura) AND FC_SbarraAperta EQ 0"/>
      </Conditions>
      <Actions>
        <Log Message="Fine corsa non rilevato. Ri-invio impulso."/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 4 — Shared variable multi-valore (produttore/consumatore):**
```xml
<!-- SM_Main (produttore): scrive il comando -->
<State Name="AperturaTornelli">
  <OnEnter>
    <SetOutput Output="Aziona_TornelloIngresso" Value="2"/>  <!-- 2 = ClockWise -->
    <SetOutput Output="Aziona_TornelloScaletta"  Value="1"/>  <!-- 1 = CounterClockWise -->
    <Log Message="Apertura tornelli: comandi inviati."/>
  </OnEnter>
  ...
</State>

<!-- TornelloIngresso (consumatore): legge e azzera al completamento -->
<State Name="ClockWise">
  <OnEnter>
    <SetOutput Output="AperturaClockWise" Value="1"/>
    <StartTimer Timer="TWait"/>
    <Log Message="Tornello in posizione ClockWise."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TWait"/>
    <!-- Azzera la variabile di comando: segnala a SM_Main che il comando è stato eseguito -->
    <SetOutput Output="Aziona_TornelloIngresso" Value="0"/>
  </OnExit>
  <Transitions>
    <Transition Target="EvaluateClockWise">
      <Conditions>
        <Condition Expression="Aziona_TornelloIngresso NE 2"/>
      </Conditions>
    </Transition>
    <Transition Target="ClockWise">
      <Conditions>
        <Condition Expression="TIMEOUT(TWait) AND FC_ClockWise EQ 0"/>
      </Conditions>
      <Actions>
        <Log Message="Fine corsa non rilevato. Nuovo tentativo."/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 5 — Inizializzazione al boot (posizione fisica sconosciuta):**
```xml
<State Name="EvaluatePosition">
  <OnEnter>
    <Log Message="Boot: verifica posizione fisica."/>
  </OnEnter>
  <Transitions>
    <!-- Sbarra già in posizione A: vai a DisableA per sicurezza, ma solo dopo che SystemBooted = 1 -->
    <Transition Target="DisableA">
      <Conditions>
        <Condition Expression="SystemBooted EQ 1 AND FC_PosizioneA EQ 1"/>
      </Conditions>
    </Transition>
    <!-- Sbarra in posizione B -->
    <Transition Target="DisableB">
      <Conditions>
        <Condition Expression="SystemBooted EQ 1 AND FC_PosizioneA EQ 0"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 6 — Sincronizzazione tra VM tramite flag:**
```xml
<!-- VM_Main: attiva VM_Worker e attende -->
<Transition Target="WaitWorkerDone">
  <Conditions>
    <Condition Expression="StartButton EQ 1"/>
  </Conditions>
  <Actions>
    <SetOutput Output="StartCycle" Value="1"/>
  </Actions>
</Transition>

<State Name="WaitWorkerDone">
  <OnEnter>
    <StartTimer Timer="TTimeout"/>
    <Log Message="Attesa completamento worker."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TTimeout"/>
  </OnExit>
  <Transitions>
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="TIMEOUT(TTimeout)"/>
      </Conditions>
      <Actions>
        <Log Message="Timeout worker — abort."/>
        <SetOutput Output="StartCycle" Value="0"/>
      </Actions>
    </Transition>
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="CycleDone EQ 1"/>
      </Conditions>
      <Actions>
        <SetOutput Output="StartCycle" Value="0"/>
        <Log Message="Worker completato."/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

**Pattern 7 — Contatore con limite e reset:**
```xml
<State Name="Retry">
  <OnEnter>
    <IncrementCounter Counter="RetryCount"/>
    <StartTimer Timer="TRetry"/>
    <Log Message="Tentativo %RetryCount%/3."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TRetry"/>
  </OnExit>
  <Transitions>
    <Transition Target="Fault">
      <Conditions>
        <Condition Expression="RetryCount GE 3"/>
      </Conditions>
      <Actions>
        <Log Message="Superato limite retry (%RetryCount%) — FAULT"/>
      </Actions>
    </Transition>
    <Transition Target="TryAgain">
      <Conditions>
        <Condition Expression="TIMEOUT(TRetry)"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>

<State Name="Success">
  <OnEnter>
    <ResetCounter Counter="RetryCount"/>
    <Log Message="Operazione riuscita. Contatore retry azzerato."/>
  </OnEnter>
  ...
</State>
```

**Pattern 8 — Blinker autonomo comandato da shared variable:**
```xml
<State Name="BlinkOn">
  <OnEnter>
    <SetOutput Output="LampeggioPedonale" Value="1"/>
    <StartTimer Timer="TBlink"/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TBlink"/>
  </OnExit>
  <Transitions>
    <!-- Disabilitazione prioritaria -->
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="BlinkEnable EQ 0"/>
      </Conditions>
    </Transition>
    <Transition Target="BlinkOff">
      <Conditions>
        <Condition Expression="TIMEOUT(TBlink)"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>
```

---

### 9.4 Anti-Pattern da Evitare

| Anti-pattern | Problema | Soluzione |
|---|---|---|
| Stato senza transizioni di timeout | VM bloccata in attesa di un evento che non arriva | Aggiungere sempre `StartTimer` + transizione di fallback |
| Timer non fermato in `OnExit` | Il timeout rimane attivo nel nuovo stato o nel self-loop | Aggiungere `StopTimer` in `OnExit` di ogni stato che fa `StartTimer` |
| Self-loop senza `StopTimer` in `OnExit` | `TIMEOUT=True` persiste dopo il rientro, loop infinito | Fermare il timer in `OnExit`; il re-enter lo riavvia da zero |
| Azioni di ingresso nelle `Actions` di transizione | Duplicazione se ci sono più transizioni entranti | Spostare in `OnEnter` del nuovo stato |
| Transizioni di sicurezza in fondo alla lista | Una transizione operativa vince su una di emergenza | Mettere abort/timeout/disabilitazione **sempre** per prime |
| Output fisico scritto da due VM diverse | Race condition: ultima scrittura vince, comportamento imprevedibile | Ogni output fisico appartiene a una sola VM; usare shared variables per coordinare |
| Shared variable dichiarata in `<Inputs>` o `<Outputs>` | Il motore duplica lo scope e genera comportamenti incoerenti | Non dichiarare mai le shared variables nei tag di I/O locali della VM |
| `TIMEOUT(T) EQ 1` | Espressione malformata dopo l'espansione interna in `(T_Timeout = 1) = 1` | Usare solo `TIMEOUT(T)` senza confronto esplicito |
| Uso di operatori matematici o funzioni come `Int()` | Errore di sintassi a runtime: `TExprEvaluator` supporta solo logica booleana | Sostituire con macchine a stati o logica booleana pura |
| Inserimento del tag intermedio `<States>` o `<Actions>` (in Preset) | Mismatch grammaticale: l'engine XML non effettua il parsing e solleva eccezioni | Mettere `<State>` sotto `<StateMachine>` e `<SetOutput>` sotto `<Preset>` come figli diretti |
| Typo `InitalValue` in SharedVars.xml | Variabile non inizializzata, errore silenzioso | Verificare sempre `InitialValue` (con la `i` al posto giusto) |
| `Port` mancante nei device Modbus TCP | Il sistema tenta la connessione su una porta casuale o default errata | Specificare sempre `Port="502"` in ogni entry di `devices.xml` |
| Alias numerici o non descrittivi (`Out1`, `FC1`) | Codice illeggibile senza `devices.xml` | Usare nomi funzionali (`ApriSbarra`, `FC_SbarraChiusa`) |
| Tutta la logica in una VM da 15+ stati | Difficile da manutenere e diagnosticare | Decomporre in 2–3 VM specializzate con singola responsabilità |
| Assunzione sullo stato fisico al boot | Attuatori in posizione sconosciuta | Aggiungere uno stato di inizializzazione che legge i fine corsa e attende `SystemBooted` |

---

## 10. Esempio Completo Annotato

L'esempio seguente implementa un controllo di un varco pedonale con sbarra, suddiviso in due VM.

### 10.1 SharedVars.xml

```xml
<?xml version="1.0" encoding="UTF-8"?>
<SharedVariables>
  <!-- Flag scritto dalla VM principale, letto dalla VM Blinker -->
  <Var Name="BlinkEnable"         InitialValue="0"/>
  <!-- Flag di boot: le VM attendono che SM_Main lo setti a 1 prima di operare -->
  <Var Name="SystemBooted"        InitialValue="0"/>
</SharedVariables>
```

### 10.2 Blinker.xml — VM Autonoma per il Lampeggio

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLC Id="200" Code="Blinker">

  <Outputs>
    <Output Id="207" Alias="SemaforoPedonale"/>
  </Outputs>

  <Timers>
    <Timer Name="TBlink" DefaultMs="500"/>
  </Timers>

  <StateMachine Initial="Idle">

    <State Name="Idle">
      <OnEnter>
        <SetOutput Output="SemaforoPedonale" Value="0"/>
      </OnEnter>
      <Transitions>
        <Transition Target="BlinkOn">
          <Conditions>
            <Condition Expression="BlinkEnable EQ 1"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>

    <State Name="BlinkOn">
      <OnEnter>
        <SetOutput Output="SemaforoPedonale" Value="1"/>
        <StartTimer Timer="TBlink"/>
      </OnEnter>
      <OnExit>
        <StopTimer Timer="TBlink"/>
      </OnExit>
      <Transitions>
        <Transition Target="Idle">
          <Conditions>
            <Condition Expression="BlinkEnable EQ 0"/>
          </Conditions>
        </Transition>
        <Transition Target="BlinkOff">
          <Conditions>
            <Condition Expression="TIMEOUT(TBlink)"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>

    <State Name="BlinkOff">
      <OnEnter>
        <SetOutput Output="SemaforoPedonale" Value="0"/>
        <StartTimer Timer="TBlink"/>
      </OnEnter>
      <OnExit>
        <StopTimer Timer="TBlink"/>
      </OnExit>
      <Transitions>
        <Transition Target="Idle">
          <Conditions>
            <Condition Expression="BlinkEnable EQ 0"/>
          </Conditions>
        </Transition>
        <Transition Target="BlinkOn">
          <Conditions>
            <Condition Expression="TIMEOUT(TBlink)"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>

  </StateMachine>
</XPLC>
```

### 10.3 Schema del Flusso della VM Principale

```
Idle
 │  ChiamataPedonale = 1
 ▼
WaitClear  [TWait 10s, BlinkEnable=1]
 │  TIMEOUT(TWait)                        │  tutte Cam = 0
 ▼                                        ▼
Idle (annulla, BlinkEnable=0)           Closing [ChiudiSbarra=1]
                                          │  SbarraChiusa=1
                                          ▼
                                        BarrierClosed [ApriTornello=1]
                                          │  TornelloAperto=1
                                          ▼
                                        PedestrianGreen [SemaforoPedonale=1, T10s]
                                          │  TIMEOUT(T10s) AND Cam05_Human=0
                                          ▼
                                        WaitTornelloClosed
                                          │  TornelloAperto=0 AND Cam05_Human=0
                                          ▼
                                        ReopenBarrier [ApriSbarra=1]
                                          │  SbarraAperta=1
                                          ▼
                                        Idle [BlinkEnable=0]
```

---

## 11. Checklist di Validazione per l'AI

Prima di consegnare un programma XPLC generato, verificare:

**Struttura XML:**
- [ ] Il file è XML valido (tag correttamente chiusi, attributi quotati, encoding UTF-8).
- [ ] L'attributo `Initial` di `<StateMachine>` corrisponde a uno `State Name` dichiarato.
- [ ] Tutti i `Target` delle transizioni corrispondono a uno `State Name` dichiarato.
- [ ] Gli elementi `<State>` sono figli diretti di `<StateMachine>` (niente contenitori `<States>`).
- [ ] Gli elementi `<SetOutput>` all'interno dei preset sono figli diretti di `<Preset>` (niente contenitori `<Actions>`).

**Dichiarazioni:**
- [ ] Tutti gli alias usati in `Expression` sono dichiarati in `<Inputs>`, `<Outputs>`,
      `<Timers>`, `<Counters>`, oppure esistono in `SharedVars.xml`.
- [ ] Le variabili globali/condivise (es. `SystemBooted`) NON sono state dichiarate in `<Inputs>` o `<Outputs>` della singola VM.
- [ ] Tutti gli alias usati in `SetOutput Output` sono dichiarati in `<Outputs>` (se fisici)
      oppure esistono in `SharedVars.xml` (se logici).
- [ ] Tutti i timer usati in `StartTimer`/`StopTimer`/`TIMEOUT()` sono dichiarati in `<Timers>`.
- [ ] Tutti i contatori usati nelle azioni sono dichiarati in `<Counters>`.
- [ ] Nessun ID fisico è usato da più di una VM per la scrittura.

**Timer:**
- [ ] Ogni stato con `StartTimer` in `OnEnter` ha il corrispettivo `StopTimer` in `OnExit`.
- [ ] I self-loop di retry hanno `StopTimer` in `OnExit` e `StartTimer` in `OnEnter`.
- [ ] Nessuna occorrenza di `TIMEOUT(X) EQ 1` o `TIMEOUT(X) = 1` (forma errata).

**Logica ed Espressioni:**
- [ ] Ogni stato (eccetto terminali intenzionali) ha almeno una transizione di timeout o abort.
- [ ] Le transizioni di sicurezza (abort, disabilitazione, timeout) sono dichiarate per prime.
- [ ] L'ordine delle transizioni rispecchia la priorità semantica desiderata.
- [ ] Lo stato `Idle` (o iniziale) imposta tutte le uscite in stato sicuro nel suo `OnEnter`.
- [ ] Le VM consumatrici di shared variable multi-valore azzerano la variabile al completamento.
- [ ] Le transizioni di inizio/valutazione dal boot attendono esplicitamente `SystemBooted EQ 1`.
- [ ] Nessuna espressione utilizza operatori matematici o funzioni aritmetiche (`+`, `-`, `*`, `/`, `Int`).

**SharedVars.xml:**
- [ ] Le variabili usate nelle VM esistono in `SharedVars.xml`.
- [ ] L'attributo è scritto `InitialValue` (non `InitalValue`).
- [ ] Nessuna `<Var>` è dichiarata dentro il file XML di una VM.

**Leggibilità:**
- [ ] Il file inizia con un commento XML contenente il diagramma del flusso in forma testuale.
- [ ] Gli alias sono descrittivi della funzione fisica.
- [ ] I commenti spiegano gli stati e le transizioni non ovvi.

**devices.xml (se modificato):**
- [ ] Ogni device Modbus TCP ha `Port="502"` esplicitamente specificato.

---

## 12. Contesto da Fornire all'AI per ogni Richiesta

Quando si chiede a un'AI di generare un programma XPLC, fornire sempre:

1. **Descrizione funzionale** del problema in linguaggio naturale.
2. **Lista ingressi** con ID numerico e descrizione fisica.
3. **Lista uscite** con ID numerico e descrizione fisica.
4. **Lista telecamere** (se presenti) con ID e quale proprietà leggere (`_Vehicle`, `_Human`).
5. **Variabili condivise esistenti** (da `SharedVars.xml`) che la nuova VM deve leggere o scrivere.
6. **Altre VM in esecuzione** che interagiscono con questa (nome, variabili scritte/lette).
7. **Requisiti di timing** (durate dei timer, durata impulsi, tempi di stazionamento).
8. **Condizioni di sicurezza** (quali situazioni devono causare abort o ritorno allo stato sicuro).
9. **Tipo di logica degli attuatori** (segnale continuo vs impulso; NC vs NA per gli ingressi).
10. **Configurazione device Modbus** (IP, porta, ID — sempre specificare `Port="502"` se Modbus TCP).

---

## 13. Formato di Output

Quando generi un programma XPLC:

1. Inizia con un commento XML in testa al file contenente il diagramma del flusso in forma testuale.
2. Genera un file XML completo e valido per ogni VM necessaria.
3. Se servono shared variables nuove, genera il contenuto aggiornato di `SharedVars.xml`.
4. Dopo il codice, elenca eventuali assunzioni fatte sugli ID dei dispositivi,
   sulle durate dei timer non specificate, o sulle variabili condivise non fornite nel contesto.
5. Non generare spiegazioni del codice al di fuori dei commenti XML — sono sufficienti.
6. Esegui internamente la checklist di validazione (sezione 11) prima di consegnare l'output.

---

## 14. Pattern Avanzati da Automazione Reale

I pattern seguenti emergono dall'analisi di sistemi multi-VM reali e completano le best
practice della sezione 9.3.

### Pattern 9 — Stato Diagnostico Intermedio (log periodico senza disturbare timer di livello superiore)

Quando si vuole loggare periodicamente lo stato di elaborazione **senza resettare** un timer
di supervisione di livello superiore (es. `T60s`), si usa uno stato "bounce" intermedio con
transizione incondizionata `1 EQ 1` per il ritorno immediato.

```xml
<Timers>
  <Timer Name="T60s"      DefaultMs="60000"/>
  <Timer Name="TDiagElab" DefaultMs="10000"/>
</Timers>

<State Name="Elaborazione">
  <OnEnter>
    <!-- T60s è già in corso — non lo tocchiamo -->
    <StartTimer Timer="TDiagElab"/>
    <Log Message="Elaborazione avviata."/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TDiagElab"/>
    <!-- T60s NON viene fermato: deve continuare indisturbato -->
  </OnExit>
  <Transitions>
    <!-- PRIORITÀ 1: condizione operativa principale -->
    <Transition Target="StatoSuccessivo">
      <Conditions>
        <Condition Expression="CondizionePrincipale EQ 1"/>
      </Conditions>
    </Transition>
    <!-- PRIORITÀ 2: log diagnostico periodico via stato bounce -->
    <Transition Target="Elaborazione_Diag">
      <Conditions>
        <Condition Expression="TIMEOUT(TDiagElab)"/>
      </Conditions>
      <Actions>
        <Log Message="[DIAG] Var1=%Var1% Var2=%Var2%"/>
      </Actions>
    </Transition>
    <!-- PRIORITÀ 3: timeout di supervisione -->
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="TIMEOUT(T60s)"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>

<!-- Stato bounce: transizione incondizionata, ritorna immediatamente -->
<State Name="Elaborazione_Diag">
  <Transitions>
    <Transition Target="Elaborazione">
      <Conditions>
        <Condition Expression="1 EQ 1"/>
      </Conditions>
    </Transition>
  </Transitions>
</State>
```

**Perché funziona:** `Elaborazione_Diag` non ha `OnEnter`/`OnExit`. La transizione `1 EQ 1`
è sempre vera: il bounce è immediato al ciclo successivo. `T60s` non viene mai toccato.
`TDiagElab` viene resettato dall'`OnExit`/`OnEnter` di `Elaborazione` ad ogni bounce.

> ⚠️ Usare `1 EQ 1` solo in stati senza altre transizioni operative, altrimenti precluderebbe
> la valutazione di qualsiasi altra condizione nello stesso stato.

### Pattern 10 — Feedback Output (verifica hardware del comando)

Per dispositivi che devono confermare l'avvenuta esecuzione del comando. Il **medesimo Id**
viene dichiarato sia in `<Inputs>` (per leggere lo stato fisico attuale) sia in `<Outputs>`
(per inviare il comando), con alias distinti.

```xml
<Inputs>
  <!-- Stesso Id dell'output 208: riletto per verificare lo stato fisico -->
  <Input Id="208" Alias="Sirena208_FB"/>
</Inputs>
<Outputs>
  <Output Id="208" Alias="Sirena208_Out"/>
</Outputs>
<Timers>
  <Timer Name="TVerifica" DefaultMs="1000"/>
</Timers>
<Counters>
  <Counter Name="RetrySpegnimento" InitialValue="0"/>
</Counters>

<State Name="Idle">
  <OnEnter>
    <SetOutput Output="Sirena208_Out" Value="0"/>
    <StartTimer Timer="TVerifica"/>
    <Log Message="Spegnimento inviato. Attesa verifica FB=%Sirena208_FB%"/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TVerifica"/>
  </OnExit>
  <Transitions>
    <!-- Spegnimento confermato: reset contatore e prosegui -->
    <Transition Target="IdleOk">
      <Conditions>
        <Condition Expression="Sirena208_FB EQ 0"/>
      </Conditions>
      <Actions>
        <ResetCounter Counter="RetrySpegnimento"/>
      </Actions>
    </Transition>
    <!-- Output ancora alto dopo timeout: ri-invia comando (self-loop) -->
    <Transition Target="Idle">
      <Conditions>
        <Condition Expression="TIMEOUT(TVerifica) AND Sirena208_FB EQ 1"/>
      </Conditions>
      <Actions>
        <IncrementCounter Counter="RetrySpegnimento"/>
        <Log Message="Output ancora alto! FB=%Sirena208_FB% Retry=%RetrySpegnimento%"/>
      </Actions>
    </Transition>
  </Transitions>
</State>
```

### Pattern 11 — Dispositivo Multi-Stato Comandato da Shared Variable Tri-State

Per dispositivi (semafori, sirene) che hanno tre comportamenti distinti (0=off, 1=attivo,
2=blink), comandati da una singola shared variable tri-state.

La VM slave **non azzera mai** la shared variable (è SM_Main a gestirla): si limita a
rispecchiare in tempo reale lo stato richiesto.

```xml
<!-- Preset per gestire più uscite contemporaneamente -->
<OutputPresets>
  <Preset Name="TuttiVerdi">
    <SetOutput Output="Sem1" Value="1"/>
    <SetOutput Output="Sem2" Value="1"/>
  </Preset>
  <Preset Name="TuttiRossi">
    <SetOutput Output="Sem1" Value="0"/>
    <SetOutput Output="Sem2" Value="0"/>
  </Preset>
</OutputPresets>

<StateMachine Initial="Rosso">
  <State Name="Rosso">
    <OnEnter>
      <ApplyPreset Preset="TuttiRossi"/>
      <StopTimer Timer="TBlink"/>
    </OnEnter>
    <Transitions>
      <Transition Target="Verde">
        <Conditions><Condition Expression="SemaforiPedonali EQ 1"/></Conditions>
      </Transition>
      <Transition Target="BlinkVerde">
        <Conditions><Condition Expression="SemaforiPedonali EQ 2"/></Conditions>
      </Transition>
    </Transitions>
  </State>

  <State Name="Verde">
    <OnEnter>
      <ApplyPreset Preset="TuttiVerdi"/>
      <StopTimer Timer="TBlink"/>
    </OnEnter>
    <Transitions>
      <Transition Target="Rosso">
        <Conditions><Condition Expression="SemaforiPedonali EQ 0"/></Conditions>
      </Transition>
      <Transition Target="BlinkVerde">
        <Conditions><Condition Expression="SemaforiPedonali EQ 2"/></Conditions>
      </Transition>
    </Transitions>
  </State>

  <State Name="BlinkVerde">
    <OnEnter>
      <ApplyPreset Preset="TuttiVerdi"/>
      <StartTimer Timer="TBlink"/>
    </OnEnter>
    <OnExit>
      <StopTimer Timer="TBlink"/>
    </OnExit>
    <Transitions>
      <!-- Disabilitazioni prioritarie sempre prima -->
      <Transition Target="Rosso">
        <Conditions><Condition Expression="SemaforiPedonali EQ 0"/></Conditions>
      </Transition>
      <Transition Target="Verde">
        <Conditions><Condition Expression="SemaforiPedonali EQ 1"/></Conditions>
      </Transition>
      <Transition Target="BlinkRosso">
        <Conditions><Condition Expression="TIMEOUT(TBlink)"/></Conditions>
      </Transition>
    </Transitions>
  </State>

  <State Name="BlinkRosso">
    <OnEnter>
      <ApplyPreset Preset="TuttiRossi"/>
      <StartTimer Timer="TBlink"/>
    </OnEnter>
    <OnExit>
      <StopTimer Timer="TBlink"/>
    </OnExit>
    <Transitions>
      <Transition Target="Rosso">
        <Conditions><Condition Expression="SemaforiPedonali EQ 0"/></Conditions>
      </Transition>
      <Transition Target="Verde">
        <Conditions><Condition Expression="SemaforiPedonali EQ 1"/></Conditions>
      </Transition>
      <Transition Target="BlinkVerde">
        <Conditions><Condition Expression="TIMEOUT(TBlink)"/></Conditions>
      </Transition>
    </Transitions>
  </State>
</StateMachine>
```

---

## 15. Nota sull'Interpolazione nei Log: Singolo `%`

La sintassi corretta per l'interpolazione nei messaggi `<Log>` è **`%NomeVar%`** con
singolo simbolo `%` sia prima che dopo il nome della variabile:

```xml
<!-- CORRETTO -->
<Log Message="Stato: FC=%FC_SbarraAperta% Retry=%RetryCount% FB=%Sirena208_FB%"/>

<!-- ERRATO (singolo %) -->
<Log Message="Stato: FC=%FC_SbarraAperta% Retry=%RetryCount%"/>
```

Usare il singolo `%` garantisce la corretta interpolazione in tutti i contesti e
previene ambiguità con altri sistemi di formattazione stringa.

---

## 16. Sistema di Template — Motore `TXPLCTemplateEngine`

> **Scopo:** Evitare la duplicazione di file XML quasi identici quando lo stesso
> dispositivo logico compare più volte nell'impianto (es. tornelli, sirene, semafori).
> Il motore di template permette di definire una **macchina a stati parametrizzata**
> e di **istanziarla** più volte con valori diversi.

### 16.1 Architettura del sistema di template

Il motore è gestito da `TXPLCTemplateEngine` (unit `uXPLCTemplates`).
Il flusso è:

```
File INI ──► TXPLCManager
                │
                ├──► File .xml diretti → TLogicController (comportamento normale)
                │
                └──► File .xplci ──► TXPLCTemplateEngine.ResolveInstanceFile()
                                           │
                                           ▼
                                    Per ogni <Instance> nel file .xplci:
                                    1. Carica il template .xplct
                                    2. Sostituisce i placeholder $Param
                                    3. Risolve eventuali <Include> annidati
                                    4. Ritorna un DOM XML pronto per TLogicController
```

**Estensioni dei file:**
| Estensione | Significato |
|---|---|
| `.xplct` | **Template** — definizione parametrizzata di una VM |
| `.xplci` | **Istance file** — elenco di istanze che usano uno o più template |

### 16.2 File template (`.xplct`)

Un template ha la stessa struttura di un file XPLC normale, ma con due sezioni aggiuntive:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLCTemplate Code="Tornello">

  <!-- 1. Dichiarazione dei parametri accettati -->
  <Parameters>
    <Param Name="FC_CCW_Id"  Type="Integer"/>
    <Param Name="FC_CW_Id"   Type="Integer"/>
    <Param Name="OUT_CCW_Id" Type="Integer"/>
    <Param Name="OUT_CW_Id"  Type="Integer"/>
    <Param Name="TWait_Ms"   Type="Integer" Default="3000"/>
    <Param Name="TClosed_Ms" Type="Integer" Default="2000"/>
    <Param Name="ControlVar" Type="String"/>
  </Parameters>

  <!-- 2. Corpo identico a un normale file XPLC, con placeholder $NomeParam -->
  <Inputs>
    <Input Id="$FC_CCW_Id" Alias="FC_CounterClockWise"/>
    <Input Id="$FC_CW_Id"  Alias="FC_ClockWise"/>
  </Inputs>

  <Outputs>
    <Output Id="$OUT_CCW_Id" Alias="AperturaCounterClockWise"/>
    <Output Id="$OUT_CW_Id"  Alias="AperturaClockWise"/>
  </Outputs>

  <Timers>
    <Timer Name="TWait"   DefaultMs="$TWait_Ms"/>
    <Timer Name="TClosed" DefaultMs="$TClosed_Ms"/>
  </Timers>

  <StateMachine Initial="EvaluateClockWise">
    <State Name="Chiusura">
      <OnEnter>
        <SetOutput Output="AperturaCounterClockWise" Value="0"/>
        <SetOutput Output="AperturaClockWise"        Value="0"/>
        <StartTimer Timer="TClosed"/>
      </OnEnter>
      <OnExit>
        <StopTimer Timer="TClosed"/>
      </OnExit>
      <Transitions>
        <Transition Target="CounterClockWise">
          <Conditions>
            <Condition Expression="TIMEOUT(TClosed) AND $ControlVar EQ 1"/>
          </Conditions>
        </Transition>
        <Transition Target="ClockWise">
          <Conditions>
            <Condition Expression="TIMEOUT(TClosed) AND $ControlVar EQ 2"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>
    <!-- ... altri stati ... -->
  </StateMachine>

</XPLCTemplate>
```

**Regole sui parametri:**
- I placeholder hanno la forma **`$NomeParametro`** (prefisso `$`).
- La sostituzione avviene **sulla stringa XML grezza prima del parsing DOM**, quindi funziona su qualsiasi attributo o valore testuale.
- I parametri con attributo `Default` sono opzionali; quelli senza `Default` sono obbligatori.
- Se un parametro obbligatorio non viene fornito dall'istanza, il motore logga un warning ma procede comunque (il placeholder rimane non sostituito).

### 16.3 File istanze (`.xplci`)

Il file `.xplci` elenca una o più istanze di uno o più template:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLCInstances TemplateDir="./templates">

  <Instance Code="TornelloIngresso" Template="Tornello">
    <Param Name="FC_CCW_Id"  Value="107"/>
    <Param Name="FC_CW_Id"   Value="105"/>
    <Param Name="OUT_CCW_Id" Value="403"/>
    <Param Name="OUT_CW_Id"  Value="206"/>
    <Param Name="ControlVar" Value="Aziona_TornelloIngresso"/>
  </Instance>

  <Instance Code="TornelloMensa" Template="Tornello">
    <Param Name="FC_CCW_Id"  Value="502"/>
    <Param Name="FC_CW_Id"   Value="503"/>
    <Param Name="OUT_CCW_Id" Value="601"/>
    <Param Name="OUT_CW_Id"  Value="603"/>
    <Param Name="ControlVar" Value="Aziona_TornelloMensa"/>
  </Instance>

  <Instance Code="TornelloScaletta" Template="Tornello">
    <Param Name="FC_CCW_Id"  Value="702"/>
    <Param Name="FC_CW_Id"   Value="703"/>
    <Param Name="OUT_CCW_Id" Value="801"/>
    <Param Name="OUT_CW_Id"  Value="803"/>
    <Param Name="ControlVar" Value="Aziona_TornelloScaletta"/>
  </Instance>

</XPLCInstances>
```

**Attributi del nodo radice `<XPLCInstances>`:**
- `TemplateDir` (opzionale): percorso della directory contenente i file `.xplct`. Se relativo, viene risolto rispetto alla directory del file `.xplci`. Se omesso, usa il valore configurato nel `config.ini` (chiave `TemplatesPath`).

**Attributi di `<Instance>`:**
- `Code` (obbligatorio): nome univoco dell'istanza (diventa `XPLC Code="..."` nel DOM risolto).
- `Template` (obbligatorio): nome del file template (senza estensione `.xplct`).

### 16.4 Inclusione di template annidati (`<Include>`)

Un template può includere altri template, generando più VM da un'unica definizione:

```xml
<XPLCTemplate Code="ZonaConSirena">
  <Parameters>
    <Param Name="SirenaOutputId" Type="Integer"/>
    <Param Name="SirenaSharedVar" Type="String"/>
    <Param Name="CamH1" Type="Integer"/>
    <Param Name="CamH2" Type="Integer"/>
  </Parameters>

  <!-- Include di un sotto-template per il semaforo associato -->
  <Include Template="SemaforoBase" Code="$Code_Semaforo">
    <Param Name="SEM_Id"     Value="$SemId"/>
    <Param Name="ControlVar" Value="$SemaforiVar"/>
  </Include>

  <!-- Corpo principale: la sirena -->
  <Inputs>
    <Input Id="$CamH1" Alias="AI_H1"/>
    <Input Id="$CamH2" Alias="AI_H2"/>
    <Input Id="$SirenaOutputId" Alias="Sirena_FB"/>
  </Inputs>

  <Outputs>
    <Output Id="$SirenaOutputId" Alias="Sirena_Out"/>
  </Outputs>

  <StateMachine Initial="Idle">
    <!-- ... logica sirena ... -->
  </StateMachine>

</XPLCTemplate>
```

**Regole sugli `<Include>`:**
- Il nodo `<Include>` genera una **VM separata** (un `TResolvedDocument` aggiuntivo).
- Il template che contiene l'`<Include>` può essere un **puro contenitore** (senza `<StateMachine>` propria): in tal caso genera solo le VM figlie.
- La profondità massima di annidamento è **10 livelli** (`cMaxNestingDepth`). Oltre, il motore interrompe con un warning.
- I parametri dell'`<Include>` possono fare riferimento ai parametri del padre con la stessa sintassi `$NomeParam`.

### 16.5 Configurazione nel file INI

Per abilitare il motore di template, aggiungere nel `config.ini`:

```ini
[StateMachines]
GlobalVar=./SharedVars.xml
TemplatesPath=./templates

; Macchine a stati dirette (file XML normali)
SM01=./Automazione.xml
SM02=./SemaforiPedonali.xml

; Istanze da template (generano N macchine ciascuna)
SM10=./Tornelli.xplci
SM11=./Sirene.xplci
```

**Regole di caricamento:**
- I file con estensione `.xplci` vengono riconosciuti automaticamente e passati al `TXPLCTemplateEngine`.
- Ogni `<Instance>` nel file `.xplci` genera una VM indipendente nel `TXPLCManager`.
- Il `Code` di ogni istanza deve essere **univoco** tra tutte le VM caricate (template o meno).

### 16.6 Best practice per l'uso dei template

**T1 — Estrarre il template quando si duplica una VM:**
Se due o più file XML differiscono solo per ID di I/O, nomi di alias, timer o variabili condivise, è opportuno convertirli in un template + istanze.

**T2 — Nomi dei parametri descrittivi e con prefisso di dominio:**
```xml
<!-- Buono -->
<Param Name="FC_SbarraChiusa_Id" Value="102"/>
<!-- Cattivo -->
<Param Name="Id1" Value="102"/>
```

**T3 — Usare i default per i timer:**
I timer condividono spesso la stessa durata tra istanze. Impostare `Default` nel template e sovrascrivere solo quando necessario:
```xml
<Param Name="TWait_Ms" Type="Integer" Default="3000"/>
```

**T4 — Evitare placeholder in nomi di stato:**
La sostituzione testuale funziona anche sui nomi degli stati (`Name="$StatoIniziale"`), ma rende il debug più difficile perché i log mostrerebbero nomi diversi per istanze diverse. Preferire nomi di stato fissi nel template.

**T5 — Verificare i placeholder non risolti:**
Il motore logga un warning con il conteggio dei placeholder rimasti non sostituiti. Controllare sempre i log dopo l'avvio:
```
WARN: Template <Tornello> instance <TornelloMensa>: 1 unresolved placeholder(s)
```

### 16.7 Esempio completo: template per le sirene

Il seguente esempio mostra come i file `Sirena_208.xml` e `Sirena_1205.xml` avrebbero potuto essere sostituiti da un template + istanze.

**File `templates/Sirena.xplct`:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLCTemplate Code="Sirena">
  <Parameters>
    <Param Name="OutputId"      Type="Integer"/>
    <Param Name="SharedVar"     Type="String"/>
    <Param Name="CamH1"         Type="Integer"/>
    <Param Name="CamH2"         Type="Integer"/>
    <Param Name="CamH3"         Type="Integer" Default="0"/>
    <Param Name="CamV1"         Type="Integer"/>
    <Param Name="CamV2"         Type="Integer"/>
    <Param Name="CamV3"         Type="Integer" Default="0"/>
    <Param Name="TBlink_Ms"     Type="Integer" Default="500"/>
    <Param Name="TVerifica_Ms"  Type="Integer" Default="1000"/>
  </Parameters>

  <Inputs>
    <Input Id="$CamH1" Alias="AI_H1"/>
    <Input Id="$CamH2" Alias="AI_H2"/>
    <Input Id="$CamH3" Alias="AI_H3"/>
    <Input Id="$CamV1" Alias="AI_V1"/>
    <Input Id="$CamV2" Alias="AI_V2"/>
    <Input Id="$CamV3" Alias="AI_V3"/>
    <Input Id="$OutputId" Alias="Sirena_FB"/>
  </Inputs>

  <Outputs>
    <Output Id="$OutputId" Alias="Sirena_Out"/>
  </Outputs>

  <Timers>
    <Timer Name="TBlink"    DefaultMs="$TBlink_Ms"/>
    <Timer Name="TVerifica" DefaultMs="$TVerifica_Ms"/>
  </Timers>

  <Counters>
    <Counter Name="RetrySpegnimento" InitialValue="0"/>
  </Counters>

  <StateMachine Initial="Idle">
    <!-- Stati Idle, IdleOk, SirenaOn, SirenaOff
         con logica identica a Sirena_208.xml -->
    <State Name="Idle">
      <OnEnter>
        <SetOutput Output="Sirena_Out" Value="0"/>
        <StartTimer Timer="TVerifica"/>
        <Log Message="Sirena $OutputId: idle — verifica FB=%Sirena_FB%"/>
      </OnEnter>
      <OnExit>
        <StopTimer Timer="TVerifica"/>
      </OnExit>
      <Transitions>
        <Transition Target="IdleOk">
          <Conditions>
            <Condition Expression="Sirena_FB EQ 0"/>
          </Conditions>
          <Actions>
            <ResetCounter Counter="RetrySpegnimento"/>
          </Actions>
        </Transition>
        <Transition Target="Idle">
          <Conditions>
            <Condition Expression="TIMEOUT(TVerifica) AND Sirena_FB EQ 1"/>
          </Conditions>
          <Actions>
            <IncrementCounter Counter="RetrySpegnimento"/>
            <Log Message="Sirena $OutputId: retry spegnimento %RetrySpegnimento%"/>
          </Actions>
        </Transition>
      </Transitions>
    </State>
    <!-- ... altri stati ... -->
  </StateMachine>
</XPLCTemplate>
```

**File `Sirene.xplci`:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLCInstances>
  <Instance Code="Sirena_208" Template="Sirena">
    <Param Name="OutputId"  Value="208"/>
    <Param Name="SharedVar" Value="SirenaZonaIngresso"/>
    <Param Name="CamH1"     Value="11005"/>
    <Param Name="CamH2"     Value="11007"/>
    <Param Name="CamH3"     Value="12003"/>
    <Param Name="CamV1"     Value="11006"/>
    <Param Name="CamV2"     Value="11008"/>
    <Param Name="CamV3"     Value="12004"/>
  </Instance>

  <Instance Code="Sirena_1205" Template="Sirena">
    <Param Name="OutputId"  Value="1205"/>
    <Param Name="SharedVar" Value="SirenaZonaDiscesa"/>
    <Param Name="CamH1"     Value="11009"/>
    <Param Name="CamH2"     Value="12001"/>
    <Param Name="CamH3"     Value="12005"/>
    <Param Name="CamV1"     Value="11010"/>
    <Param Name="CamV2"     Value="12002"/>
    <Param Name="CamV3"     Value="12006"/>
  </Instance>
</XPLCInstances>
```

> **Nota:** nel template sopra i parametri `CamH3` e `CamV3` hanno `Default="0"`.
> Se il valore è `0`, l'`<Input>` corrispondente verrebbe generato con `Id="0"`, che
> il motore ignorerà (ID 0 non è valido). Per gestire un numero variabile di telecamere
> è necessario usare template separati o includere tutte le camere con ID validi.

### 16.8 Checklist template per l'AI

Prima di generare codice con template, verificare:

- [ ] Il dispositivo compare più volte nell'impianto con logica identica?
- [ ] I parametri che variano sono solo: ID I/O, nomi alias, timer, shared variables?
- [ ] I nomi dei parametri sono descrittivi e univoci?
- [ ] Tutti i parametri obbligatori (senza `Default`) sono forniti in ogni `<Instance>`?
- [ ] Il `Code` di ogni istanza è univoco rispetto a tutte le altre VM?
- [ ] Il percorso `TemplatesPath` è configurato nel `config.ini`?
- [ ] I file `.xplci` sono referenziati nel `config.ini` con chiavi `SMxx`?
- [ ] Non ci sono placeholder `$Nome` rimasti non sostituiti nel DOM risolto?

<!-- FINE_GUIDA_COMPLETA -->
