unit ULBLogger;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, contnrs, uTimedoutCriticalSection, uLBFileUtils, eventlog;

type
  TLBLoggerMessageType = (lmt_Unknown  = 0,
                          lmt_Error    = 1,
                          lmt_Info     = 2,
                          lmt_Debug    = 3,
                          lmt_Warning  = 4,
                          lmt_Critical = 5,
                          lmt_Report   = 6,
                          lmt_User1    = 7,
                          lmt_User2    = 8,
                          lmt_User3    = 9,
                          lmt_User4    = 10,
                          lmt_User5    = 11,
                          lmt_User6    = 12,
                          lmt_User7    = 13,
                          lmt_User8    = 14,
                          lmt_User9    = 15,
                          lmt_User10   = 16);

  TLBLoggerMessageTypeSet = set of TLBLoggerMessageType;

const
  cLBLoggerMessagePrefix : array [TLBLoggerMessageType] of String = ('#UNK# - ',
                                                                     '#ERR# - ',
                                                                     '#INF# - ',
                                                                     '#DBG# - ',
                                                                     '#WRN# - ',
                                                                     '#CRT# - ',
                                                                     '#RPR# - ',
                                                                     '#USR1# - ',
                                                                     '#USR2# - ',
                                                                     '#USR3# - ',
                                                                     '#USR4# - ',
                                                                     '#USR5# - ',
                                                                     '#USR6# - ',
                                                                     '#USR7# - ',
                                                                     '#USR8# - ',
                                                                     '#USR9# - ',
                                                                     '#USR10# - ');

type

  ILBBaseLoggerInterface = Interface(IUnknown)
    ['{5746C090-6093-47BB-8CB6-1787C292B43D}']
    function logWrite(LogLevel: Byte; Sender: PChar; MsgType: TLBLoggerMessageType; MsgText: PChar): Boolean; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    procedure set_MaxLogLevel(AValue: Byte); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure set_EnabledMessageTypes(AList: PChar); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};  // The string '1,3,4' will enable error, debug and warning messages
  end;

  ILBLogger = Interface(ILBBaseLoggerInterface)
    ['{1019E32D-D1EF-E411-938A-543530D8EA6B}']
    procedure set_MaxFileSize(ASize: Int64); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure setLogFile(aFilename: PChar);
  end;

  { TLBLoggerMessage }

  TLBLoggerMessage = class(TCollectionItem)
    strict private
      FCallingRoutine : AnsiString;
      FMsgType : TLBLoggerMessageType;
      FThreadId : Int64;
      FPID : Int64;

      const
        cDateTimeFormat : String = 'dd/mm/yy hh.nn.ss.zzz';


    strict protected
      FTime : TDateTime;
      FMessage : AnsiString;

      procedure Clear;

    public
      constructor Create(ACollection: TCollection); override;
      function CopyFrom(aMessage: TLBLoggerMessage): Boolean;

      function ElaborateMessageToWrite(): String; virtual;

    published
      property Message : AnsiString read FMessage write FMessage;
      property CallingRoutine : AnsiString read FCallingRoutine write FCallingRoutine;
      property Time : TDateTime read FTime write FTime;
      property MsgType : TLBLoggerMessageType read FMsgType write FMsgType;
      property ThreadId : Int64 read FThreadId write FThreadId;
      property PID: Int64 read FPID write FPID;

  end;

  TLBLoggerMessageClass = class of TLBLoggerMessage;

  { TLBLogMessageCollection }

  TLBLogMessageCollection = class(TCollection)
    public
      constructor Create(); reintroduce;
  end;


  { TLBBaseLoggerWriter }

  pLBBaseLoggerWriter = ^TLBBaseLoggerWriter;
  TLBBaseLoggerWriter = class (TThread)
    strict private
      FReference : pLBBaseLoggerWriter;

    protected
      procedure setNewMessage(); virtual;

    public
      constructor Create(const aThreadName: String); virtual;
      destructor Destroy; override;
      property Reference: pLBBaseLoggerWriter write FReference;

  end;


  { TLBLoggerWriter }
  TLBLoggerWriter = class(TLBBaseLoggerWriter)
    strict private

      type
        TLBLoggerWriterState = (lw_Unknown        = 0,
                                lw_VerifyMessages = 1,
                                lw_WriteMessages  = 2,
                                lw_WaitFileName   = 3);

      var
        FMessageList : TThreadList;
        FNewMessageEvent: PRTLEvent;
        FCanStartEvent : PRTLEvent;

        FExitFromPauseEvent : PRTLEvent;


        FFileName : String;
        FFilePath : String;
        FMaxFileSize : Int64;


      procedure ChangeLogFileName();
      procedure set_FileName(AValue: String);
      procedure set_FilePath(AValue: String);

      procedure WaitFor(mSecs: Integer);

      function OpenLogFile(): TFileStream;

      function WriteMessages: Boolean;
      function LockFile(var aFileHandle: THandle): Boolean;
      procedure UnlockFile(var aFileHandle: THandle);

    protected
      procedure Execute; override;

    public
      constructor Create(const aThreadName: String); override;
      destructor Destroy; override;

      procedure setNewMessage(); override;

      procedure Terminate; reintroduce; virtual;

      property MessageList: TThreadList write FMessageList;
      property FileName: String write set_FileName;
      property FilePath: String write set_FilePath;
      property MaxFileSize: Int64 write FMaxFileSize;
  end;


  { TLBBaseLogger }

  TLBBaseLogger = class(TInterfacedObject)
    strict protected
      FObjectName : String;

      FMaxLogLevel : Byte;
      FMessageList : TThreadList;
      FEnabledMessages : TLBLoggerMessageTypeSet;

      function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; virtual;

      procedure setMaxLogLevel(AValue: Byte);

    public
      constructor Create(const aName: String; AppendToMainLogger: Boolean = True); virtual;
      destructor Destroy; override;

      // Interface
      procedure set_MaxLogLevel(AValue: Byte); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      procedure set_EnabledMessageTypes(AList: PChar); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      procedure setLogFile(aFilename: PChar); virtual;
      // ------------------------------------------------------------------------------------------------

      property MaxLogLevel: Byte read FMaxLogLevel write SetMaxLogLevel;
      property EnabledMessages: TLBLoggerMessageTypeSet read FEnabledMessages  write FEnabledMessages;

      const
        cMaxLogLevelValue = Byte(20);
  end;

 { TLBLogger }

  TLBLogger = class(TLBBaseLogger, ILBLogger)
    strict private
      FCSWriter : TTimedOutCriticalSection;

      FFileName : AnsiString;
      FFilePath : AnsiString;
      FMaxFileSize : Int64;
      FAlternativeLoggers : TObjectList;
      FCSAlternativeLoggers : TTimedOutCriticalSection;
      FWriter : TLBLoggerWriter;
      FDestroying : Boolean;

      procedure setFileName(AValue : AnsiString);
      procedure setFilePath(AValue : AnsiString);

      function CreateWriter(const aSender: String): Boolean;
      procedure DestroyWriter;

   strict protected
     FMessageClass : TLBLoggerMessageClass;

   public
     constructor Create(const aName: String; AppendToMainLogger: Boolean = True); override;
     destructor Destroy; override;

     procedure ReleaseLogFile();

     function Write(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; const MsgText: String; Parameters: array of const): Boolean; overload;
     function Write(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; const MsgText: String): Boolean; overload;

       // Interface
     function logWrite(LogLevel: Byte; Sender: PChar; MsgType: TLBLoggerMessageType; MsgText: PChar): Boolean; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
     procedure setLogFile(aFilename: PChar); override;
     procedure set_MaxFileSize(ASize: Int64); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
     // ------------------------------------------------------------------------------------------------

     function addAlternativeLogger(anAlternativeLogger: TLBBaseLogger; aPriority: Integer = -1): Boolean;
     function removeAlternativeLogger(anAlternativeLogger: TLBBaseLogger): Boolean;

     class function isAlreadyCreated: Boolean;

     property FileName: AnsiString read FFileName;
     property FilePath: AnsiString read FFilePath;
     property MaxFileSize: Int64 read FMaxFileSize write set_MaxFileSize;
   end;

   TLBLoggerMessageDescriptions = array [TLBLoggerMessageType] of String;

   { TLBLoggerMessageDescriptionsHelper }

   TLBLoggerMessageDescriptionsHelper = Type Helper for TLBLoggerMessageDescriptions
     function getMessageType(aDescription: String): TLBLoggerMessageType;
   end;

const
  cLBLoggerMessageTypeDescription : TLBLoggerMessageDescriptions = ('Unknown',
                                                                    'Error',
                                                                    'Info',
                                                                    'Debug',
                                                                    'Warning',
                                                                    'Critical',
                                                                    'Report',
                                                                    'User1',
                                                                    'User2',
                                                                    'User3',
                                                                    'User4',
                                                                    'User5',
                                                                    'User6',
                                                                    'User7',
                                                                    'User8',
                                                                    'User9',
                                                                    'User10');


var
  LBLogger  : TLBLogger = nil;
  LBLoggerI : ILBLogger = nil;


  function InitLogger(aMaxLogLevel: Integer; aLogFileName: string; aUseIntf: Boolean = False; aUseTmpFolder: Boolean = True): Boolean;
  procedure PrintSplashInfo(const anExeName: string; anApplicationDescription: string);
  procedure ReleaseLogger();


implementation

uses
  uThreadsUtils, LazFileUtils, StrUtils;

const
  cLockFileNameSuffix = String('.lck');


procedure PrintSplashInfo(const anExeName: string; anApplicationDescription: string);
var
  _FileInfo : TFileInfoRetriever = nil;
  _AppName : string;

  procedure PrintRow(aValue1, aValue2: string);
  var
    _s : string;
  begin
    _s :=      Format('---    %0:-20s ', [aValue1]);
    _s := _s + Format('%0:-28s ---', [aValue2]);
    LBLogger.Write(1, PChar(_AppName), lmt_Info, PChar(_s));
  end;

begin

  try

    _AppName := ApplicationName();
    _FileInfo := TFileInfoRetriever.Create;
    if _FileInfo.LoadInfo(anExeName) then
    begin

      LBLogger.Write(1, PChar(_AppName), lmt_Info, ' ');
      LBLogger.Write(1, PChar(_AppName), lmt_Info, ' ');
      LBLogger.Write(1, PChar(_AppName), lmt_Info, PChar('      ' + anApplicationDescription));
      LBLogger.Write(1, PChar(_AppName), lmt_Info, ' ');
      LBLogger.Write(1, PChar(_AppName), lmt_Info, ' ');

      LBLogger.Write(1, PChar(_AppName), lmt_Info, '------------------------------------------------------------');
      PrintRow(' ', ' ');

      if _FileInfo.CompanyName = '' then
        PrintRow('Company:', _FileInfo.CompanyName);

      if _FileInfo.LegalCopyright = '' then
        PrintRow('Legal copyright:', _FileInfo.LegalCopyright);

      if _FileInfo.ProductName = '' then
        PrintRow('Product name:', _FileInfo.ProductName);

      if _FileInfo.ProductVersion = '' then
        PrintRow('Product version:', _FileInfo.ProductVersion);

      if _FileInfo.Comments = '' then
        PrintRow('Comments:', _FileInfo.Comments);

    end
    else
      PrintRow('No file info available', ' ');

    PrintRow(' ', ' ');
    LBLogger.Write(1, PChar(_AppName), lmt_Info, '------------------------------------------------------------');
    LBLogger.Write(1, PChar(_AppName), lmt_Info, ' ');

  except
    on E: Exception do
      LBLogger.Write(1, PChar(_AppName), lmt_Error, PChar(E.Message));
  end;

  if _FileInfo <> nil then
    _FileInfo.Free;

end;



function InitLogger(aMaxLogLevel: Integer; aLogFileName: string; aUseIntf: Boolean = False; aUseTmpFolder: Boolean = True): Boolean;
const
  cLogFileExt = '.log';

var
  _LogFileName : string;
  _LogPathName : string;

begin
  Result := False;

  try

    if not TLBLogger.isAlreadyCreated then
    begin

      LBLogger := TLBLogger.Create('MainLog');
      LBLogger.MaxLogLevel := aMaxLogLevel;

      if Length(aLogFileName) > 0 then
      begin
        if aUseTmpFolder then
          _LogPathName := getTemporaryFolder()
        else
          _LogPathName := ExtractFilePath(aLogFileName);

        _LogFileName := ExtractFileName(aLogFileName);
        if ExtractFileExt(_LogFileName) <> cLogFileExt then
          _LogFileName := ExtractFileNameWithoutExt(_LogFileName) + cLogFileExt;

        LBLogger.setLogFile(PChar(IncludeTrailingPathDelimiter(_LogPathName) + _LogFileName));
      end;

      LBLogger.Write(1, ApplicationName(), lmt_Info, '*********   START APPLICATION LOG   *********');

      if aUseIntf then
        LBLoggerI := LBLogger as ILBLogger;


      Result := True;
    end;

  except
  end;

end;


procedure ReleaseLogger();
begin

  try
    LBLogger.Write(1, Pchar(ApplicationName()), lmt_Info, '*********   END APPLICATION LOG   *********');

    if LBLoggerI <> nil then
    begin
      LBLoggerI := nil;
      LBLogger  := nil;
    end
    else if LBLogger <> nil then
      FreeAndNil(LBLogger);

  except
  end;

end;

{ TLBLoggerMessageDescriptionsHelper }

function TLBLoggerMessageDescriptionsHelper.getMessageType(aDescription: String): TLBLoggerMessageType;
var
  i : TLBLoggerMessageType;

begin
  Result := lmt_Unknown;

  for i in TLBLoggerMessageType do
  begin
    if Self[i] = aDescription then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TLBBaseLoggerWriter }

constructor TLBBaseLoggerWriter.Create(const aThreadName: String);
begin
  inherited Create(True);

  FReference := nil;
  FreeOnTerminate := True;

end;

destructor TLBBaseLoggerWriter.Destroy;
begin
  if FReference <> nil then
    FReference^ := nil;

  inherited Destroy;
end;


procedure TLBBaseLoggerWriter.setNewMessage();
begin
  //
end;


{ TLBBaseLogger }

constructor TLBBaseLogger.Create(const aName: String; AppendToMainLogger: Boolean);
begin
  inherited Create();

  FObjectName := aName;

  FMessageList := TThreadList.Create;
  FEnabledMessages := [lmt_Unknown, lmt_Error, lmt_Info, lmt_Debug, lmt_Warning, lmt_Critical];
  FMaxLogLevel := 3;

  if AppendToMainLogger and (LBLogger <> nil) and (LBLogger <> Self) then
    LBLogger.addAlternativeLogger(Self);
end;

destructor TLBBaseLogger.Destroy;
var
  _List : TList;
  i :Integer;

begin
  if (LBLogger <> nil) and (LBLogger <> Self) then
    LBLogger.removeAlternativeLogger(Self);


  if FMessageList = nil then Exit;

  try
    _List := FMessageList.LockList;

    for i := _List.Count - 1 downto 0 do
    begin
      TObject(_List.Items[i]).Free;
      _List.Items[i] := nil;
    end;

  finally
    FMessageList.UnlockList;
  end;
  FreeAndNil(FMessageList);

  inherited Destroy;
end;

procedure TLBBaseLogger.set_MaxLogLevel(AValue: Byte); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Self.setMaxLogLevel(AValue);
end;


procedure TLBBaseLogger.setMaxLogLevel(AValue: Byte);
begin
  if (Self = nil) or (FMaxLogLevel = AValue) then Exit;

  FMaxLogLevel := AValue;
end;


procedure TLBBaseLogger.set_EnabledMessageTypes(AList: PChar); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
var
  _sMessages : String;
  _Messages : TStringList = nil;
  _Value : Byte;
  i : Integer;

begin
  if Self = nil then Exit;


  try
    _sMessages := strpas(AList);
    if Length(AList) > 0 then
    begin
      FEnabledMessages := [];
      _Messages := TStringList.Create;
      _Messages.StrictDelimiter := True;
      _Messages.Delimiter := ',';
      _Messages.DelimitedText := _sMessages;
      if _Messages.Count > 0 then
      begin
        for i := 0 to _Messages.Count - 1 do
        begin
          _Value := StrToIntDef(Trim(_Messages.Strings[i]), 0);
          if _Value > 0 then
            FEnabledMessages += [TLBLoggerMessageType(_Value)];
        end;
      end;
    end;

  finally
    if _Messages <> nil then
      _Messages.Free;
  end;


end;

procedure TLBBaseLogger.setLogFile(aFilename: PChar);
begin
  //
end;


function TLBBaseLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
begin
  Result := False;
end;


{ TLBLogger }

function TLBLogger.CreateWriter(const aSender: String): Boolean;
begin
  Result := False;

  if FCSWriter.Acquire('TLBLogger.CreateWriter - ' + FObjectName) then
  begin

    try

      if FWriter = nil then
      begin

        if Length(FFileName) > 0 then
        begin
          FWriter := TLBLoggerWriter.Create(FObjectName);
          FWriter.Reference := @FWriter;
          FWriter.MessageList := FMessageList;
          FWriter.FileName := FFileName;
          FWriter.FilePath := FFilePath;
          FWriter.MaxFileSize := FMaxFileSize;
          FWriter.Start();

          Result := True;
        end;

      end
      else
        Result := True;

    finally
      FCSWriter.Release();
    end;
  end;

end;

procedure TLBLogger.DestroyWriter;
begin

  if FCSWriter.Acquire('TLBLogger.DestroyWriter', 20000) then
  begin

    try

      if FWriter <> nil then
      begin
        if FWriter.Suspended then
          FreeAndNil(FWriter)
        else begin

         FWriter.Terminate;
         while (FWriter <> nil) do
           Sleep(20);

        end;
      end;

    finally
    end;

    FCSWriter.Release();
  end;

end;

function TLBLogger.addAlternativeLogger(anAlternativeLogger: TLBBaseLogger; aPriority: Integer = -1): Boolean;
begin
  Result := False;

  if anAlternativeLogger <> nil then
  begin
    if FCSAlternativeLoggers.Acquire('TLBLogger.addAlternativeLogger') then
    begin
      try

        if FAlternativeLoggers.IndexOf(anAlternativeLogger) = -1 then
        begin
          if aPriority = -1 then
            FAlternativeLoggers.Add(anAlternativeLogger)
          else
            FAlternativeLoggers.Insert(aPriority, anAlternativeLogger);
        end;

        Result := True;

      finally
        FCSAlternativeLoggers.Release();
      end;
    end;
  end;
end;

class function TLBLogger.isAlreadyCreated: Boolean;
begin
  Result := LBLogger <> nil;
end;

function TLBLogger.removeAlternativeLogger(anAlternativeLogger: TLBBaseLogger): Boolean;
var
  _Idx : Integer;

begin
  Result := False;

  if (anAlternativeLogger <> nil) then
  begin
    if FCSAlternativeLoggers.Acquire('TLBLogger.removeAlternativeLogger') then
    begin
      try
        if (FAlternativeLoggers <> nil) then
        begin
          _Idx := FAlternativeLoggers.IndexOf(anAlternativeLogger);
          if _Idx > -1 then
            FAlternativeLoggers.Delete(_Idx);
        end;

      finally
        FCSAlternativeLoggers.Release();
      end;
    end;
  end;
end;

procedure TLBLogger.setFileName(AValue: AnsiString);
begin
  if (Self = nil) or (FFileName = AValue) then Exit;

  FFileName := AValue;
  if FWriter <> nil then
    FWriter.FileName := FFileName;
end;

procedure TLBLogger.setFilePath(AValue: AnsiString);
begin
  if Self <> nil then
  begin
    {$IFDEF UNIX}
    if Length(AValue) = 0 then
      AValue := '/tmp';
    {$ENDIF}


    if FFilePath <> AValue then
    begin
      FFilePath := IncludeTrailingPathDelimiter(AValue);

      if not DirectoryExists(FFilePath) then
        ForceDirectories(FFilePath);

      if (FWriter <> nil) then
        FWriter.FilePath := FFilePath;
    end;
  end;
end;


constructor TLBLogger.Create(const aName: String; AppendToMainLogger: Boolean);
begin
  inherited Create(aName, AppendToMainLogger);

  FMessageClass := TLBLoggerMessage;

  FMaxFileSize := 2 * 1024 * 1024; // 2 MB
  FFileName := '';
  FFilePath := '';

  FAlternativeLoggers := TObjectList.Create(False);
  FCSAlternativeLoggers := TTimedOutCriticalSection.Create;

  FCSWriter := TTimedOutCriticalSection.Create;

  if LBLogger = nil then
    LBLogger := Self;

  FDestroying := False;
end;

destructor TLBLogger.Destroy;
begin
  FDestroying := True;

  if Self = LBLogger then
    LBLogger := nil
  else begin
    if (LBLogger <> nil) then
      LBLogger.removeAlternativeLogger(Self);
  end;

  try

    if FCSAlternativeLoggers <> nil then
      FreeAndNil(FCSAlternativeLoggers);

    if FAlternativeLoggers <> nil then
      FreeAndNil(FAlternativeLoggers);

  except
  end;

  Self.DestroyWriter();

  FreeAndNil(FCSWriter);


  inherited Destroy;
end;

procedure TLBLogger.ReleaseLogFile;
var
  i : Integer;

begin
  Self.DestroyWriter();

  if FCSAlternativeLoggers.Acquire('TLBLogger.removeAlternativeLogger') then
  begin
    try

      if (FAlternativeLoggers <> nil) then
      begin
        for i := 0 to FAlternativeLoggers.Count - 1 do
        begin
          if FAlternativeLoggers.Items[i] is TLBLogger then
            TLBLogger(FAlternativeLoggers.Items[i]).ReleaseLogFile();
        end;
      end;

    finally
      FCSAlternativeLoggers.Release();
    end;
  end;

end;


function useLBLoggerInterface(LogLevel: Byte; Sender: PChar; MsgType: TLBLoggerMessageType; MsgText: PChar; Parameters: array of const): Boolean;
var
  _MsgText: string;
begin
  Result := False;

  if LBLoggerI <> nil then
  begin
    if Length(Parameters) = 0 then
      Result := LBLoggerI.logWrite(LogLevel, Sender, MsgType, MsgText)
    else begin

      try
        _MsgText := Format(AnsiString(MsgText), Parameters);

      except
        on E: Exception do
          _MsgText := E.Message;
      end;

      Result := LBLoggerI.logWrite(LogLevel, Sender, MsgType, PChar(_MsgText));
    end;
  end;
end;

function TLBLogger.Write(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; const MsgText: String; Parameters: array of const): Boolean; overload;
var
  _MsgText : AnsiString;

begin
  Result := False;

  if (Self = nil) or FDestroying then Exit;

  if Length(Parameters) = 0 then
    Result := Self.Write(LogLevel, Sender, MsgType, MsgText)
  else begin

    try
      _MsgText := Format(MsgText, Parameters);

    except
      on E: Exception do
        _MsgText := E.Message;
    end;

    Result := Self.Write(LogLevel, Sender, MsgType, _MsgText);
  end;
end;

function TLBLogger.Write(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; const MsgText: String): Boolean;
var
  i : Integer;
  _List : TList = nil;
  _AMessage : TLBLoggerMessage = nil;
  _isOK : Boolean;
  _Msg : String;

begin
  Result := False;

  if Self = nil then Exit;

  if (FAlternativeLoggers <> nil) and FCSAlternativeLoggers.Acquire('TLBLogger.Write') then
  begin
    try

      _Msg := MsgText;

      for i := 0 to FAlternativeLoggers.Count - 1 do
      begin
        _isOK := TLBBaseLogger(FAlternativeLoggers.Items[i]).virtualWrite(LogLevel, Sender, MsgType, _Msg);

        if _Msg = '' then
        begin
          Result := _isOK;
          Exit;
        end;
      end;

    finally
      FCSAlternativeLoggers.Release();
    end;

  end;

  if (LogLevel > FMaxLogLevel) or (not (MsgType in FEnabledMessages)) then Exit;

  if not Self.CreateWriter(Sender) then Exit;

  _AMessage := FMessageClass.Create(nil);
  _AMessage.Time := Now();
  _AMessage.MsgType := MsgType;
  _AMessage.Message := MsgText;
  _AMessage.PID := GetProcessID;
  _AMessage.ThreadId := GetThreadID;
  _AMessage.CallingRoutine := Sender;

  _List := FMessageList.LockList;
  try
    _List.Add(Pointer(_AMessage));

  finally
    FMessageList.UnlockList;
  end;

  FWriter.setNewMessage(); // Segnalo che ho qualcosa in coda

  Result := True;
end;

function TLBLogger.logWrite(LogLevel: Byte; Sender: PChar; MsgType: TLBLoggerMessageType; MsgText: PChar): Boolean; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
var
  _Sender : String;
  _Msg : String;

begin
  _Sender := StrPas(Sender);
  _Msg := StrPas(MsgText);

  Result := Self.Write(LogLevel, _Sender, MsgType, _Msg);
end;

procedure TLBLogger.setLogFile(aFilename: PChar);
var
  i : Integer;
  _File : String;
  _Name : String;
  _Path : String;

begin
  if Self <> nil then
  begin

    _File := strpas(aFilename);

    if Length(_File) > 0 then
    begin
      _Path := ExtractFilePath(_File);
      Self.setFilePath(_Path);
      _Name := ExtractFileName(_File);
      Self.setFileName(_Name);
    end
    else begin
      Self.setFilePath('');
      Self.setFileName('');
    end;

    if FAlternativeLoggers <> nil then
    begin
      if FCSAlternativeLoggers.Acquire('TLBLogger.setLogFile') then
      begin

       try

          for i := 0 to FAlternativeLoggers.Count - 1 do
            TLBBaseLogger(FAlternativeLoggers.Items[i]).setLogFile(aFilename);

        finally
        end;

        FCSAlternativeLoggers.Release();

      end;

    end;

  end;
end;


procedure TLBLogger.set_MaxFileSize(ASize: Int64); {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if Self <> nil then
  begin
    if FMaxFileSize <> ASize then
    begin
      FMaxFileSize := ASize;
      if FWriter <> nil then
        FWriter.MaxFileSize := FMaxFileSize;
    end;
  end;
end;

{ TLBLoggerWriter }

procedure TLBLoggerWriter.ChangeLogFileName();
var
  _FileSize : Int64 = 0;
  _CompleteFilename : String;

begin
  _CompleteFilename := FFilePath + FFileName;
  if FileExists(_CompleteFilename) then
   _FileSize := FileSizeUtf8(_CompleteFilename);

  if _FileSize > FMaxFileSize then
    RenameFileUTF8(_CompleteFilename, FFilePath + FormatDateTime('yyyymmdd_hhnnss', Now()) + '_' + FFileName);

end;

procedure TLBLoggerWriter.set_FileName(AValue: String);
begin
  FFileName := AValue;

  if (Length(FFileName) > 0) and (Length(FFilePath) > 0) then
    RTLEventSetEvent(FCanStartEvent);
end;

procedure TLBLoggerWriter.set_FilePath(AValue: String);
begin
  FFilePath := AValue;
  if Length(FFilePath) > 0 then
  begin
    FFilePath := IncludeTrailingPathDelimiter(AValue);
    if Length(FFileName) > 0 then
      RTLEventSetEvent(FCanStartEvent);
  end;
end;

procedure TLBLoggerWriter.WaitFor(mSecs: Integer);
begin
  if not Self.Terminated then
    RTLEventWaitFor(FExitFromPauseEvent, mSecs);
end;

function TLBLoggerWriter.OpenLogFile: TFileStream;
begin
  Result := nil;

  try

    if not FileExists(FFilePath + FFileName) then
      Result := TFileStream.Create(FFilePath + FFileName, fmOpenReadWrite or fmCreate or fmShareDenyWrite)
    else
      Result := TFileStream.Create(FFilePath + FFileName, fmOpenReadWrite or fmShareDenyWrite);

    Result.Seek(0, soFromEnd);

  except
    on E: Exception do
    begin
      writeln('TLBLoggerWriter.OpenLogFile  -  Error: ', e.Message);
      if Result <> nil then
        FreeAndNil(Result);
    end;
  end;

end;

function TLBLoggerWriter.WriteMessages(): Boolean;
var
  _txtFile : TFileStream = nil;
  _AMessage : TLBLoggerMessage;
  _sMessage : AnsiString;
  _List : TList;
  i : Integer;

begin
  Result := False;

  Self.ChangeLogFileName();

  try
    _txtFile := Self.OpenLogFile();

    if _txtFile <> nil then
    begin
      repeat

        _AMessage := nil;

        try

          _List := FMessageList.LockList;
          if (_List <> nil) then
          begin
            if _List.Count > 0 then
              _AMessage := TLBLoggerMessage(_List.Extract(_List.Items[0]));
          end;

        finally
          FMessageList.UnlockList;
        end;

        if _AMessage <> nil then
        begin
          _sMessage := _AMessage.ElaborateMessageToWrite() + sLineBreak;

          _txtFile.WriteBuffer(_sMessage[1], Length(_sMessage));

          _AMessage.Free;
        end
        else
          Break;

      until False;

      Result := True;
    end
    else begin
      // Impossibile scrivere nel file
      // Rimozione dei messaggi
      try

        _List := FMessageList.LockList;
        if (_List <> nil) then
        begin
          for i := 0 to _List.Count - 1 do
          begin
            _AMessage := TLBLoggerMessage(_List.Extract(_List.Items[0]));
            writeln(_AMessage.Message);
            _AMessage.Free;
          end;
        end;

      finally
        FMessageList.UnlockList;
      end;

    end;

  except
    on E: Exception do
      writeln('TLBLoggerWriter.WriteMessages  -  Error: ', E.Message);
  end;

  if _txtFile <> nil then
    _txtFile.Free;
end;


function TLBLoggerWriter.LockFile(var aFileHandle: THandle): Boolean;
var
  _tmpHandle : THandle;

begin
  if aFileHandle = 0 then
  begin
    if not FileExists(FFilePath + FFileName + cLockFileNameSuffix) then
    begin
      _tmpHandle := FileCreate(FFilePath + FFileName + cLockFileNameSuffix);
      FileClose(_tmpHandle);
    end;

    aFileHandle := FileOpen(FFilePath + FFileName + cLockFileNameSuffix, fmOpenWrite or fmShareExclusive);
  end;

  Result := aFileHandle <> 0;
end;

procedure TLBLoggerWriter.UnlockFile(var aFileHandle: THandle);
begin
  if aFileHandle <> 0 then
  begin
    FileClose(aFileHandle);
    aFileHandle := 0;
  end;
end;


procedure TLBLoggerWriter.Execute;
var
  _LockFileHandle : THandle = 0;
  _List : TList = nil;
  _nMsgs : Integer = 0;
  _InternalState : TLBLoggerWriterState;

 const
  cWaitTime = 10000; // 10 sec

begin
  _InternalState := lw_WaitFileName;

  while not Self.Terminated do
  begin
    try
      case _InternalState of

        lw_WaitFileName:
          begin
            // Self.CloseLogFile();
            if (Length(FFileName) > 0) and
               (Length(FFilePath) > 0) then
            begin
              if not DirectoryExists(FFilePath) then
                ForceDirectories(FFilePath);

              _InternalState := lw_VerifyMessages;
            end
            else
              RTLeventWaitFor(FCanStartEvent);
          end;

        lw_VerifyMessages:
            begin
              Self.UnlockFile(_LockFileHandle);

              _nMsgs := 0;
              try
                _List := FMessageList.LockList;
                if _List <> nil then
                  _nMsgs := _List.Count;
              finally
                FMessageList.UnlockList;
              end;

              if _nMsgs > 0 then
                _InternalState := lw_WriteMessages
              else
                RTLEventWaitFor(FNewMessageEvent);
            end;

        lw_WriteMessages:
           begin
             if Self.LockFile(_LockFileHandle) then
             begin
               Self.UnlockFile(_LockFileHandle);
               Self.WriteMessages();
               _InternalState := lw_VerifyMessages
             end
             else
               Self.WaitFor(cWaitTime);
           end;
      end;
    except
      on E: Exception do
        writeln('Error: ', E.Message);
    end;

  end;   // while not terminated

  if Self.LockFile(_LockFileHandle) then
  begin
    Self.WriteMessages();
    Self.UnlockFile(_LockFileHandle);
  end;
end;

constructor TLBLoggerWriter.Create(const aThreadName: String);
begin
  inherited Create(aThreadName);

  FMaxFileSize := 2 * 1024 * 1024;

  FNewMessageEvent := RTLEventCreate;
  FCanStartEvent := RTLEventCreate;
  FExitFromPauseEvent := RTLEventCreate;

  Self.setDebugName(aThreadName);
end;

destructor TLBLoggerWriter.Destroy;
begin
  try

    RTLeventSetEvent(FCanStartEvent);
    RTLeventDestroy(FCanStartEvent);

    RTLeventSetEvent(FNewMessageEvent);
    RTLeventDestroy(FNewMessageEvent);

    RTLeventSetEvent(FExitFromPauseEvent);
    RTLeventDestroy(FExitFromPauseEvent);

  except
  end;

  inherited Destroy;
end;

procedure TLBLoggerWriter.setNewMessage();
begin
  if Self <> nil then
    RTLEventSetEvent(FNewMessageEvent);
end;

procedure TLBLoggerWriter.Terminate;
begin
  inherited Terminate;

  Sleep(1);

  RTLEventSetEvent(FExitFromPauseEvent);
  RTLEventSetEvent(FNewMessageEvent);
  RTLEventSetEvent(FCanStartEvent);
end;



{ TWebSocketLogMessageCollection }

constructor TLBLogMessageCollection.Create();
begin
  inherited Create(TLBLoggerMessage);
end;


{ TLBLoggerMessage }

procedure TLBLoggerMessage.Clear;
begin
  FMessage        := '';
  FCallingRoutine := '';
  FTime           := 0;
  FThreadId       := 0;
  FMsgType        := lmt_Unknown;
end;

constructor TLBLoggerMessage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Self.Clear();
end;

function TLBLoggerMessage.CopyFrom(aMessage: TLBLoggerMessage): Boolean;
begin
  FMessage        := aMessage.Message;
  FCallingRoutine := aMessage.CallingRoutine;
  FTime           := aMessage.Time;
  FMsgType        := aMessage.MsgType;
  FThreadId       := aMessage.ThreadId;
  FPID            := aMessage.ThreadId;

  Result := True;
end;

function TLBLoggerMessage.ElaborateMessageToWrite(): String;
var
  _sPID         : String = '';
  _sThread      : String = '';
  _sMessageType : String;

begin
  if FPID > 0 then
    _sPID := AddChar('0', IntToStr(FPID), 6) + ' - ';

  if FThreadId > 0 then
    _sThread := AddChar('0', RightStr(IntToStr(FThreadId), 10), 10) + ' - ';

  if FMsgType <= High(cLBLoggerMessagePrefix) then
    _sMessageType := cLBLoggerMessagePrefix[FMsgType]
  else
    _sMessageType := '#???#';

  Result := FormatDateTime(cDateTimeFormat, FTime) + ' - ' +
            _sMessageType +
            _sPID +
            _sThread +
            AddCharR(' ', LeftStr(FCallingRoutine, 42), 42) + ' - ' +
            FMessage;
end;


initialization

finalization
  if LBLoggerI <> nil then
    LBLoggerI := nil
  else
    FreeAndNil(LBLogger);

end.
