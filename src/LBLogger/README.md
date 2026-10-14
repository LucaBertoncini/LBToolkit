# 🪵 LBLogger — Reference & Architecture Guide

Module: `src/LBLogger/`

| Unit | Contents |
|---|---|
| `ULBLogger.pas` | `TLBBaseLogger`, `TLBLogger`, writer thread (`TLBLoggerWriter`), `InitLogger` / `ReleaseLogger` |
| `uLBLoggerEx.pas` | Graphical subloggers (require LCL; `TVirtualMemoLogger` also requires `VirtualTrees`) |
| `uEventLog.pas` | System Event Log sublogger (`TfpEventLogger`) |
| `src/shared/LBToolkit/uCallbackLogger.pas` | Sublogger forwarding to a C function |
| `src/shared/LBToolkit/Toolkit_CAPI_Logger.pas` | Exported C-API functions for non-Pascal projects |

Asynchronous, thread-safe logging engine. Calling `LBLogger.Write` enqueues the message in a thread-safe list and returns immediately; writing to disk is performed asynchronously by a background thread (`TLBLoggerWriter`).

---

## 1. Initialization and Lifecycle

```pascal
uses ULBLogger;

if not InitLogger(3, 'myapp.log', False, False) then
  Halt(1);

LBLogger.Write(1, 'Main', lmt_Info, 'Applicazione avviata');

ReleaseLogger();
```

### Function Signature:
```pascal
function InitLogger(aMaxLogLevel: Integer; aLogFileName: string;
  aUseIntf: Boolean = False; aUseTmpFolder: Boolean = True): Boolean;
procedure ReleaseLogger();
```

- **File Folder (`aUseTmpFolder`)**: With `aUseTmpFolder = True` (**the default**), the file is created in the system temporary folder and any path component in `aLogFileName` is ignored. Pass `aUseTmpFolder = False` to write directly to a specified folder path.
- **Extension**: If `aLogFileName` does not end with `.log`, the extension is replaced with `.log`.
- **Return Value**: `InitLogger` returns `False` if a logger is already created (`TLBLogger.isAlreadyCreated`). A second call does not alter the existing configuration.
- **Interface Reference (`aUseIntf`)**: `aUseIntf = True` populates `LBLoggerI: ILBLogger` (used by C-API bridges). `ReleaseLogger` clears both references.
- **Start / End Banner**: On startup, `InitLogger` automatically writes `*********   START APPLICATION LOG   *********` (`LogLevel 1`, `lmt_Info`). `ReleaseLogger` writes the matching `END` message before destroying the writer.
- **Writer Teardown**: Upon destruction, the background writer receives `Terminate` and performs a final flush pass, ensuring queued messages in memory are saved to disk.

---

## 2. Levels and Message Types

Each log message possesses two independent attributes:

1. **`LogLevel: Byte`**: Verbosity level. The message is written to the file only if `LogLevel <= MaxLogLevel` (default `MaxLogLevel = 3`). Errors/warnings use level `1`; info and debug traces use higher levels (`3`, `5`).
2. **`MsgType: TLBLoggerMessageType`**: Category type. The message is written only if `MsgType` is included in `EnabledMessages`.

| Value | Name | Value | Name |
|---|---|---|---|
| 0 | `lmt_Unknown` | 8 | `lmt_User2` |
| 1 | `lmt_Error` | 9 | `lmt_User3` |
| 2 | `lmt_Info` | 10 | `lmt_User4` |
| 3 | `lmt_Debug` | 11 | `lmt_User5` |
| 4 | `lmt_Warning` | … | … |
| 5 | `lmt_Critical` | 16 | `lmt_User10` |
| 6 | `lmt_Report` | | |
| 7 | `lmt_User1` | | |

*Default `EnabledMessages`*: `[lmt_Unknown, lmt_Error, lmt_Info, lmt_Debug, lmt_Warning, lmt_Critical]`. `lmt_Report` and `lmt_User1`..`lmt_User10` are disabled by default.

---

## 3. Evaluation Order in `TLBLogger.Write`

1. **Sublogger Chain**: The message **first** passes through registered subloggers (Section 4), which always see it—even if `LogLevel` or `MsgType` would exclude it from the main file.
2. **Level/Type Filter**: If no sublogger consumed the message (`aMsgText := ''`), the level and type filters decide whether to enqueue the message for disk writing.

---

## 4. Sublogger Chain & Message Interception

`TLBLogger.addAlternativeLogger(aLogger: TLBBaseLogger; aPriority: Integer = -1)` hooks a sublogger into the chain.

- **Automatic Registration**: The constructor `TLBBaseLogger.Create(const aName: String; AppendToMainLogger: Boolean = True)` **automatically registers** the sublogger into the main logger's chain. Pass `AppendToMainLogger = False` if you wish to register manually with a specific priority.
- **Ownership**: The chain **does not own** subloggers; whoever creates them must free them. The sublogger's `Destroy` automatically unhooks it from the chain.

### Interception & Consumption (`virtualWrite`):
```pascal
function virtualWrite(aLogLevel: Byte; const aSender: String;
  aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean; virtual;
```

- **`aMsgText` Non-Empty**: Message continues to subsequent subloggers and then to the file.
- **`aMsgText := ''`**: Message is **consumed**. The chain stops immediately, and nothing is written to the main log file.
- **File Receives Original Text**: If a sublogger modifies `aMsgText` without clearing it, subsequent subloggers see the modified text, but the file logger receives the **original** text.

### Custom Sublogger Example (Alert Dispatcher):
```pascal
type
  TAlertLogger = class(TLBBaseLogger)
  public
    function virtualWrite(aLogLevel: Byte; const aSender: String;
      aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean; override;
  end;

function TAlertLogger.virtualWrite(aLogLevel: Byte; const aSender: String;
  aMsgType: TLBLoggerMessageType; var aMsgText: String): Boolean;
begin
  Result := False;
  if aMsgType in [lmt_Critical, lmt_Error] then
  begin
    SendAlert(aSender + ': ' + aMsgText); // Custom Telegram/Email notification
    aMsgText := '';                       // Consumed: prevents writing to disk
    Result := True;
  end;
end;
```

---

## 5. Built-In Provided Subloggers

| Class | Unit | Destination / Behavior |
|---|---|---|
| `TfpEventLogger` | `uEventLog.pas` | System Event Log (`TEventLog`). Applies its own filters; **does not consume** messages. |
| `TMemoLogger` | `uLBLoggerEx.pas` | LCL `TMemo` UI control. |
| `TLabelLogger` | `uLBLoggerEx.pas` | LCL `TLabel` UI control. |
| `TStatusBarLogger` | `uLBLoggerEx.pas` | LCL `TStatusBar` UI control. |
| `TVirtualMemoLogger` | `uLBLoggerEx.pas` | `TVirtualMemo` (requires VirtualTrees). |
| `TCallbackLogger` | `uCallbackLogger.pas` | Forwards to an external C callback procedure. |

---

## 6. Log Line Format & File Rotation

### Line Formatting Fields:
- `MessageFields`: Order of fields (`lfDateTime`, `lfMessagetype`, `lfPID`, `lfThread`, `lfSource`, `lfMessage`).
- `DateTimeFormat`: Default `'dd/mm/yy hh.nn.ss.zzz'`.
- `SourceMaxLength`: Source name truncation (default 42 chars).

Sample output:
```
30/09/26 10.15.42.123 | Info | 012345 | 012346 | Main                                       | Applicazione avviata
```

### File Rotation:
- **Max File Size**: Default `MaxFileSize = 2 MiB`. When a batch write causes the file size to exceed `MaxFileSize`, the file is renamed to `yyyymmdd_hhnnss_<name>.log` in the same directory and a new log file is opened.
- **Retention**: Rotated files are preserved; application/system scripts manage cleanup.

---

## 7. C-API Interface (`Toolkit_CAPI_Logger.pas`)

For non-Pascal projects compiling LBToolkit as a dynamic library (`.so`/`.dll`):

```c
void     Logger_Initialize(const char* logFileName, int logLevel);
void     Logger_Finalize(void);
void     Logger_Write(int level, const char* sender, uint8_t messageType, const char* message);
void*    Logger_CreateCallbackSublogger(TLogCallback callback, int maxLogLevel, void* userData);
void     Logger_DestroySublogger(void* handle);
```
