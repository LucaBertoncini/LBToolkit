# Grammatica XML per XPLC

XPLC è un controllore logico configurabile tramite file XML. Questo documento descrive la sintassi per creare macchine a stati eseguibili dal motore XPLC.

## Struttura del file

Un file XML valido per XPLC deve avere un nodo radice `<XPLC>` con gli attributi `Id` (opzionale ma consigliato) e `Code` (etichetta).

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLC Id="1" Code="MioControllo">
  <Inputs>...</Inputs>
  <Outputs>...</Outputs>
  <Timers>...</Timers>
  <Counters>...</Counters>
  <StateMachine Initial="Idle">
    ...
  </StateMachine>
</XPLC>
```

I campi `String` non richiedono caratteri di escape speciali.

## Sezioni principali

### 1. Inputs

Dichiara gli ingressi che la macchina a stati può leggere. Ogni `<Input>` associa un ID globale (definito nel file `devices.xml`) a un alias usabile nelle condizioni.

```xml
<Inputs>
  <Input Id="1001" Alias="Camera01Human"/>
  <Input Id="1002" Alias="Camera01Vehicle"/>
  <Input Id="905"  Alias="PulsanteEmergenza"/>
</Inputs>
```

**ID globale**: è il riferimento univoco dell'item nel sistema (es. sensori Modbus, GPIO, rilevamenti video).

### 2. Outputs

Definisce le uscite che la macchina a stati può comandare. Ogni `<Output>` associa un ID globale a un alias.

```xml
<Outputs>
  <Output Id="201" Alias="ApriSbarra"/>
  <Output Id="204" Alias="SemaforoRosso"/>
  <Output Id="205" Alias="Sirena"/>
</Outputs>
```

### 3. Timers

Dichiara timer logici con nome e durata predefinita. Possono essere avviati, fermati e il loro timeout può essere usato nelle condizioni (suffisso `.Timeout`).

```xml
<Timers>
  <Timer Name="TimerSbarra"   DefaultMs="10000"/>
  <Timer Name="TimerAllarme"  DefaultMs="30000"/>
</Timers>
```

La durata predefinita è usata quando l'azione `StartTimer` non specifica `DurationMs`.

### 4. Counters

Dichiara contatori interni con nome e valore iniziale.

```xml
<Counters>
  <Counter Name="CntPassaggi" InitialValue="0"/>
  <Counter Name="CntAllarmi"  InitialValue="0"/>
</Counters>
```

Il valore del contatore può essere letto nelle condizioni usando il suo alias (es. `CntPassaggi`).

### 5. StateMachine

Contiene l'insieme degli stati e indica lo stato iniziale con l'attributo `Initial`.

```xml
<StateMachine Initial="Idle">
  <State Name="Idle">
    ...
  </State>
  <State Name="Apertura">
    ...
  </State>
</StateMachine>
```

## Descrizione dello stato

Ogni `<State>` ha un nome e può contenere:

- `<OnEnter>`: azioni eseguite quando si entra nello stato.
- `<OnExit>`: azioni eseguite quando si esce dallo stato (opzionale).
- `<Transitions>`: elenco di transizioni valutate in ordine di priorità (dall'alto verso il basso).

### Azioni disponibili (in OnEnter, OnExit o nelle Transizioni)

| Azione              | Attributi                                  | Descrizione                                  |
|---------------------|---------------------------------------------|----------------------------------------------|
| `SetOutput`         | `Output="<alias>"` `Value="<valore>"`       | Imposta un'uscita booleana (1/0)             |
| `StartTimer`        | `Timer="<nome>"` [`DurationMs="<ms>"`]      | Avvia un timer (durata opzionale)            |
| `StopTimer`         | `Timer="<nome>"`                            | Ferma un timer prima che scada               |
| `IncrementCounter`  | `Counter="<nome>"`                          | Incrementa un contatore di 1                 |
| `DecrementCounter`  | `Counter="<nome>"`                          | Decrementa un contatore di 1                 |
| `ResetCounter`      | `Counter="<nome>"`                          | Azzera un contatore                          |
| `Log`               | `Message="<testo>"`                         | Scrive un messaggio nel log di traccia       |

Esempio di stato:

```xml
<State Name="Apertura">
  <OnEnter>
    <SetOutput Output="ApriSbarra" Value="1"/>
    <StartTimer Timer="TimerSbarra" DurationMs="5000"/>
    <Log Message="Sbarra aperta"/>
  </OnEnter>
  <OnExit>
    <StopTimer Timer="TimerSbarra"/>
  </OnExit>
  <Transitions>
    ...
  </Transitions>
</State>
```

## Transizioni

Ogni `<Transition>` definisce un possibile cambio di stato. Contiene:

- `<Conditions>`: elenco di condizioni (AND) che devono essere tutte vere.
- `<Actions>` (opzionale): azioni eseguite solo quando la transizione scatta.
- Attributo `Target`: nome dello stato di destinazione.

```xml
<Transition Target="IngressoRilevato">
  <Conditions>
    <Condition Input="Camera01Vehicle" Op="EQ" Value="1"/>
  </Conditions>
  <Actions>
    <IncrementCounter Counter="CntPassaggi"/>
    <Log Message="Veicolo rilevato"/>
  </Actions>
</Transition>
```

Le transizioni sono valutate nell'ordine in cui compaiono: la prima con tutte le condizioni vere scatta.

## Condizioni

Ogni `<Condition>` confronta il valore attuale di un input (alias di ingresso, timer o contatore) con un valore atteso tramite un operatore.

```xml
<Condition Input="<alias>" Op="<operatore>" Value="<valore>"/>
```

### Operatori supportati

| Operatore | Significato             |
|-----------|-------------------------|
| `EQ`      | Uguale                  |
| `NE`      | Diverso                 |
| `GT`      | Maggiore                |
| `LT`      | Minore                  |
| `GE`      | Maggiore o uguale       |
| `LE`      | Minore o uguale         |

`Value` è sempre interpretato come `Variant` (numerico, booleano, stringa) in base al contesto.

### Condizioni su timer e contatori

- Timer: usare l'alias `<NomeTimer>.Timeout` (es. `TimerSbarra.Timeout`). Vale `1` quando il timer è scaduto, `0` altrimenti.
- Contatori: usare direttamente il nome del contatore (es. `CntPassaggi`). Il valore è l'intero attuale.

## Azioni

Le azioni possono apparire in tre punti:

1. In `<OnEnter>` dello stato.
2. In `<OnExit>` dello stato.
3. In `<Actions>` di una transizione.

La sintassi è identica in tutti e tre i contesti.

## Esempi

### Esempio 1: Semaforo semplice

Una macchina che accende un semaforo verde quando arriva un veicolo, lo tiene acceso per 5 secondi, poi torna al rosso.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLC Id="1" Code="SemaforoSemplice">
  <Inputs>
    <Input Id="1004" Alias="VeicoloPresente"/>
  </Inputs>
  <Outputs>
    <Output Id="201" Alias="Verde"/>
    <Output Id="202" Alias="Rosso"/>
  </Outputs>
  <Timers>
    <Timer Name="T" DefaultMs="5000"/>
  </Timers>
  <StateMachine Initial="Idle">

    <State Name="Idle">
      <OnEnter>
        <SetOutput Output="Rosso" Value="1"/>
        <SetOutput Output="Verde" Value="0"/>
      </OnEnter>
      <Transitions>
        <Transition Target="Permesso">
          <Conditions>
            <Condition Input="VeicoloPresente" Op="EQ" Value="1"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>

    <State Name="Permesso">
      <OnEnter>
        <SetOutput Output="Rosso" Value="0"/>
        <SetOutput Output="Verde" Value="1"/>
        <StartTimer Timer="T"/>
      </OnEnter>
      <Transitions>
        <Transition Target="Idle">
          <Conditions>
            <Condition Input="T.Timeout" Op="EQ" Value="1"/>
          </Conditions>
        </Transition>
      </Transitions>
    </State>

  </StateMachine>
</XPLC>
```

### Esempio 2: Contatore di fronti con azioni di transizione

Conta fino a 3 fronti di un timer, poi si ferma.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XPLC Id="2" Code="ContaFronti">
  <Timers>
    <Timer Name="Tick" DefaultMs="1000"/>
  </Timers>
  <Counters>
    <Counter Name="C" InitialValue="0"/>
  </Counters>
  <StateMachine Initial="Counting">

    <State Name="Counting">
      <OnEnter>
        <StartTimer Timer="Tick"/>
        <Log Message="Inizio conteggio"/>
      </OnEnter>
      <Transitions>
        <!-- Fronte 1 e 2: incrementa e resta qui -->
        <Transition Target="Counting">
          <Conditions>
            <Condition Input="Tick.Timeout" Op="EQ" Value="1"/>
            <Condition Input="C"            Op="LT" Value="2"/>
          </Conditions>
          <Actions>
            <IncrementCounter Counter="C"/>
            <Log Message="Tick %C%"/>
            <StartTimer Timer="Tick"/>
          </Actions>
        </Transition>
        <!-- Fronte 3: incrementa e vai a Fine -->
        <Transition Target="Fine">
          <Conditions>
            <Condition Input="Tick.Timeout" Op="EQ" Value="1"/>
            <Condition Input="C"            Op="GE" Value="2"/>
          </Conditions>
          <Actions>
            <IncrementCounter Counter="C"/>
            <Log Message="Ultimo tick: %C%"/>
          </Actions>
        </Transition>
      </Transitions>
    </State>

    <State Name="Fine">
      <OnEnter>
        <Log Message="Terminato. Totale = %C%"/>
      </OnEnter>
    </State>

  </StateMachine>
</XPLC>
```