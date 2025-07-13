unit uLBLoggerEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, ULBLogger, uTimedoutCriticalSection{$IFDEF useVirtualControls}, vte_stringlist, VirtualTrees{$ENDIF};

type
    { TControlLogger }

  TControlLogger = class(TLBBaseLogger)
    strict protected
      FMessages : TStringList;
      FControl : TControl;
      FCSMessage : TTimedOutCriticalSection;
      FShowMessageType : Boolean;

      procedure UpdateControl(Data: PtrInt); virtual;
      function virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean; override;

    public
      constructor Create; reintroduce; virtual;
      destructor Destroy; override;

      property ShowMessageType: Boolean write FShowMessageType;
  end;


  { TLabelLogger }

  TLabelLogger = class(TControlLogger)
    strict private
      procedure set_Control(AValue: TLabel);

    strict protected
      procedure UpdateControl(Data: PtrInt); override;

    public
      property aLabel: TLabel write set_Control;
  end;

  { TStatusBarLogger }

  TStatusBarLogger = class(TControlLogger)
    strict private
      procedure set_Control(AValue: TStatusBar);

    strict protected
      procedure UpdateControl(Data: PtrInt); override;

    public
      property StatusBar: TStatusBar write set_Control;
  end;

  { TMemoLogger }

  TMemoLogger = class(TControlLogger)
    strict private
      procedure set_Control(AValue: TMemo);
      procedure UpdateScrollBarPosition(Data: PtrInt);

    strict protected
      procedure UpdateControl(Data: PtrInt); override;

    public
      property Memo: TMemo write set_Control;
  end;


  {$IFDEF useVirtualControls}
  { TVirtualMemoLogger }

  TVirtualMemoLogger = class(TControlLogger)
    strict private
      FFollow : Boolean;
      FMaxLines : Integer;
      procedure set_Control(AValue: TVirtualMemo);

    strict protected
      procedure UpdateControl(Data: PtrInt); override;

    public
      constructor Create; override;

      property MaxLines: Integer write FMaxLines;
      property Memo: TVirtualMemo write set_Control;
      property Follow: Boolean read FFollow write FFollow;
  end;
  {$ENDIF}

implementation

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}Forms, DateUtils;

{$IFDEF useVirtualControls}
{ TVirtualMemoLogger }

procedure TVirtualMemoLogger.set_Control(AValue: TVirtualMemo);
begin
  FControl := AValue;
end;

procedure TVirtualMemoLogger.UpdateControl(Data: PtrInt);
var
//  _LastNode : PVirtualNode = nil;
  _Memo : TVirtualMemo;
  i : Integer;

begin
  if (Self <> nil) and (FControl <> nil) then
  begin
    if FCSMessage.Acquire('TLSMemoLogger.UpdateControl') then
    begin
      try
        _Memo := TVirtualMemo(FControl);

        _Memo.Lines.AddStrings(FMessages);
        FMessages.Clear;

        if FMaxLines > 0 then
        begin
          if _Memo.Lines.Count > FMaxLines then
          begin
            for i := _Memo.Lines.Count downto FMaxLines do
              _Memo.Lines.Delete(0);
          end;
        end;
      finally
        FCSMessage.Release();
      end;

      if FFollow then
        _Memo.FocusedNode := _Memo.GetLast(nil);
    end;
  end;
end;

constructor TVirtualMemoLogger.Create;
begin
  inherited Create();

  FFollow := True;
  FMaxLines := 0;
end;

{$ENDIF}

{ TLabelLogger }

procedure TLabelLogger.set_Control(AValue: TLabel);
begin
  FControl := AValue;
end;

procedure TLabelLogger.UpdateControl(Data: PtrInt);
var
  _Msg : String = '';

begin
  if (Self <> nil) and (FControl <> nil) then
  begin
    if FCSMessage.Acquire('TLabelLogger.UpdateControl') then
    begin
      try

        if FMessages.Count > 0 then
        begin
          _Msg := FMessages.Strings[FMessages.Count - 1];
          FMessages.Clear;
        end;

      finally
        FCSMessage.Release();
      end;
    end;
  end;

  if Length(_Msg) > 0 then
  begin
    TLabel(FControl).Caption := _Msg;
    TLabel(FControl).Refresh();
  end;
end;

{ TMemoLogger }

procedure TMemoLogger.set_Control(AValue: TMemo);
begin
  FControl := AValue;
end;

procedure TMemoLogger.UpdateScrollBarPosition(Data: PtrInt);
begin
  {$IFDEF WINDOWS}
  TMemo(FControl).VertScrollBar.Position := TMemo(FControl).VertScrollBar.Position + Data;
  SendMessage(TMemo(FControl).Handle, EM_LINESCROLL, 0, Data);
  {$ELSE}
  TMemo(FControl).VertScrollBar.Position := TMemo(FControl).VertScrollBar.Position + Data;
  {$ENDIF}
end;

procedure TMemoLogger.UpdateControl(Data: PtrInt);
var
  _Rows : PtrInt;

begin
  if (Self <> nil) and (FControl <> nil) then
  begin
    if FCSMessage.Acquire('TMemoLogger.UpdateControl') then
    begin
      try
        _Rows := FMessages.Count;
        TMemo(FControl).Lines.AddStrings(FMessages);
        FMessages.Clear;
      finally
        FCSMessage.Release();
      end;
      Application.QueueAsyncCall(@Self.UpdateScrollbarPosition, _Rows);
    end;
  end;
end;

{ TStatusBarLogger }

procedure TStatusBarLogger.set_Control(AValue: TStatusBar);
begin
  FControl := AValue;
end;

procedure TStatusBarLogger.UpdateControl(Data: PtrInt);
begin
  if (Self <> nil) and (FControl <> nil) then
  begin
    if FCSMessage.Acquire('TStatusBarLogger.UpdateControl') then
    begin
      try
        if FMessages.Count > 0 then
        begin
          TStatusBar(FControl).SimpleText := FMessages.Strings[FMessages.Count - 1];
          FMessages.Clear;
          FControl.Update();
        end;
      finally
        FCSMessage.Release();
      end;
    end;
  end;
end;

{ TControlLogger }

procedure TControlLogger.UpdateControl(Data: PtrInt);
begin
  //
end;

function TControlLogger.virtualWrite(LogLevel: Byte; const Sender: String; MsgType: TLBLoggerMessageType; var MsgText: String): Boolean;
var
  _msgType : String;

begin
  Result := False;

  if (MsgType in FEnabledMessages) and (LogLevel <= FMaxLogLevel) then
  begin
    if FShowMessageType then
    begin
      if MsgType <= High(cLBLoggerMessagePrefix) then
        _msgType := cLBLoggerMessagePrefix[MsgType]
      else
        _msgType := '#???#';
    end
    else
      _msgType := '';

    if FCSMessage.Acquire('TStatusBarLogger.virtualWrite') then
    begin
      try
        FMessages.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) + '  -  ' + _msgType + ' ' + MsgText);
      finally
        FCSMessage.Release();
      end;
    end;

    Application.QueueAsyncCall(@Self.UpdateControl, 0);

    Result := True;
  end;
end;

constructor TControlLogger.Create;
begin
  inherited Create('');

  FCSMessage := TTimedOutCriticalSection.Create;
  FEnabledMessages := [lmt_Error, lmt_Info, lmt_Warning, lmt_Critical];
  FMessages := TStringList.Create;

  FShowMessageType := False;
end;

destructor TControlLogger.Destroy;
begin

  Application.RemoveAsyncCalls(Self);

  try
    FControl := nil;

    FreeAndNil(FCSMessage);
    FreeAndNil(FMessages);

  except
    on E: Exception do
      LBLogger.Write(1, 'TControlLogger.Destroy', lmt_Error, PChar(E.Message));
  end;

  inherited Destroy;
end;


end.

