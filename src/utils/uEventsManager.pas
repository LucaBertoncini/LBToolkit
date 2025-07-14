unit uEventsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, uTimedoutCriticalSection;

type
  TSingleCallbackEvent = procedure (Sender: TObject; anEventName: String) of object;

  TCallbackProcedure = function(anOpaquePointer: Pointer): Integer; {$IFDEF Linux}cdecl{$ELSE}stdcall{$ENDIF};


  { TEventsManager }

  TEventsManager = class(TObject)
    public
      // Modalità operative dell'event manager
      type
        TEventsManagerMode = (
          emm_Unknown              = 0,
          emm_Events               = 1, // Usa eventi con Sender come parametro
          emm_Callbacks            = 2, // Usa callback a funzione C-style con puntatore opaco
          emm_EventsSingleCallback = 3  // Usa evento con firma estesa (Sender + nome evento che ha generato l'evento)
        );

    strict private
      type
        // Interfaccia per descrivere un singolo listener/evento
        IEventInfo = interface(IInterface)
          ['{FF86092D-17AA-449A-ABAB-3A9592FB3623}']
          function isExecuted(): Boolean;
          procedure setExecuted(aValue: Boolean);

          procedure setCallBack(aCallback: Pointer);
          function getCallBack(): Pointer;

          function getEvent(): PMethod;
          procedure setEvent(anEvent: pMethod);

          function getOwner(): Pointer;
        end;

        { TEventInfo }

        TEventInfo = class(TInterfacedObject, IEventInfo)
          strict private
            FCallback : Pointer;
            FOwner : Pointer;
            FExecuted : Boolean;
            FMethod : TMethod;

          public
            constructor Create(AnOwner: Pointer);

            function isExecuted(): Boolean;
            procedure setExecuted(aValue: Boolean);

            function getCallback(): Pointer;
            procedure setCallback(aCallback: Pointer);

            function getEvent(): PMethod;
            procedure setEvent(anEvent: pMethod);

            function getOwner(): Pointer;
        end;

        { TEventListeners }
        // Lista di listener per un determinato evento
        TEventListeners = class(TInterfaceList)
          public
            function ListenerAlreadyIn(aListener: Pointer): Boolean;
        end;

      var
        FCS : TTimedOutCriticalSection;  // Sezione critica con timeout per gestire accessi concorrenti
        FMode : TEventsManagerMode;  // Modalità di gestione eventi

        // Proprietà dell'oggetto: owner, stato, eventi
        FOwner : TObject;
        FEnabled : Boolean;
        FEvents : TStringList;            // Mappa eventi ↔︎ listener
        FDisabledEvents : TStringList;    // Eventi disabilitati
        FRaisingEvents : Boolean;         // Flag per prevenire modifiche durante RaiseEvent
        FFreeOnTerminate : Boolean;       // Flag che indica se liberare l'oggetto dopo RaiseEvent
        FDestroyingState : Boolean;       // Flag per evitare RaiseEvent in fase di distruzione

      function AddEventListenerInternal(AnEventName: String; anEventToRaise: PMethod; aCallback: Pointer; anOwner: Pointer; anEventsManager: TEventsManager): Boolean;

      function getListeners(AnEvent: String): TEventListeners;

      procedure RemoveOpaquePointerFromList(anOpaquePointer: Pointer; AList: TEventListeners);
      procedure RemoveListenerFromDestroyEvent(Sender: TObject);

    strict protected
      const
        cDestroyEvent = String('EM_Destroy');  // Evento speciale per la deregistrazione incrociata

    public
      constructor Create(AnOwner: TObject; OperationMode: TEventsManagerMode = emm_Events);
      destructor Destroy; override;

      procedure Free; reintroduce;

      procedure AddEvent(AnEvent: String);

      function AddEventListener(AnEventName: String; AnEventToRaise: TNotifyEvent; AnEventManager: TEventsManager): Boolean; overload;
      function AddEventListener(AnEventName: PAnsiChar; anOpaquePointer: Pointer; aCallback: TCallbackProcedure): Boolean; overload;
      function AddSingleCallbackListener(AnEventToRaise: TSingleCallbackEvent; AnEventManager: TEventsManager): Boolean;

      procedure RemoveListener(Sender: TObject); overload;
      procedure RemoveListener(anOpaquePointer: Pointer); overload;

      procedure RemoveEventListener(AnEventName: String; AnEventManager: TEventsManager); overload;
      procedure RemoveEventListener(AnEventName: PAnsiChar; anObscurePointer: Pointer); overload;

      procedure DisableEvent(AnEvent: String);
      procedure EnableEvent(AnEvent: String);

      procedure RaiseEvent(AnEvent: String); overload;
      procedure RaiseEvent(AnEvent: String; Sender: TObject); overload;

      property Enabled: Boolean write FEnabled;
      property Owner: TObject read FOwner;

  end;

implementation

uses
  uLBLogger;

{ TEventsManager }

function TEventsManager.AddEventListenerInternal(AnEventName: String; anEventToRaise: PMethod; aCallback: Pointer; anOwner: Pointer; anEventsManager: TEventsManager): Boolean;
var
  _EventsList : TEventListeners;
  _EventInfo : TEventInfo;

begin
  Result := False;

  if (AnEventName <> '') then
  begin
    if anOwner <> nil then
    begin

      if FCS.Acquire('TEventsManager.AddEventListener') then
      begin
        try
          _EventsList := Self.getListeners(AnEventName);
          if _EventsList <> nil then
          begin
            Result := _EventsList.ListenerAlreadyIn(anOwner);

            if not Result then
            begin
              _EventInfo := TEventInfo.Create(anOwner);

              if aCallback <> nil then
                _EventInfo.setCallback(aCallBack)
              else
                _EventInfo.setEvent(anEventToRaise);

              _EventsList.Add(_EventInfo as IEventInfo);


              if anEventsManager <> nil then // I due EventsManager si registrano sui reciproci destroy
                AnEventsManager.AddEventListener(TEventsManager.cDestroyEvent, @Self.RemoveListenerFromDestroyEvent, Self);

              Result := True;
            end;
          end;

        except
          on E: Exception do
            LBLogger.Write(1, 'TEventsManager.AddEventListenerInternal', lmt_Error, E.Message);
        end;

        FCS.Release();
      end;

    end
    else
      LBLogger.Write(1, 'TEventsManager.AddEventListener', lmt_Warning, 'Event: <%s>  -  No owner set!', [AnEventName]);
  end;
end;

function TEventsManager.getListeners(AnEvent: String): TEventListeners;
var
  _Idx : Integer;

begin
  Result := nil;

  if (AnEvent <> '') and (FEvents <> nil) then
  begin
    _Idx := FEvents.IndexOf(AnEvent);
    if _Idx > -1 then
      Result := TEventListeners(FEvents.Objects[_Idx])
    else
      LBLogger.Write(1, 'TEventsManager.getListeners', lmt_Warning, 'Event <%s> not found!', [AnEvent]);
  end;
end;


procedure TEventsManager.RemoveListener(Sender: TObject);
begin
  Self.RemoveListener(Pointer(Sender));
end;

procedure TEventsManager.RemoveOpaquePointerFromList(anOpaquePointer: Pointer; AList: TEventListeners);
var
  i : Integer;

begin

  if (AList <> nil) and (anOpaquePointer <> nil) then
  begin

    if FCS.Acquire('TEventsManager.RemoveOpaquePointerFromList') then
    begin

      try

        for i := AList.Count - 1 downto 0 do
        begin
          if (AList.Items[i] <> nil) then
          begin
            if (AList.Items[i] as IEventInfo).getOwner() = anOpaquePointer then
            begin
              AList.Delete(i);
              Break;
            end;
          end;
        end;

      except
        on E: Exception do
          LBLogger.Write(1, 'TEventsManager.RemoveOpaquePointerFromList', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

  end;
end;

procedure TEventsManager.RemoveListenerFromDestroyEvent(Sender: TObject);
var
  _Listeners : TEventListeners;

begin
  Self.RemoveListener(Sender);
  _Listeners := Self.getListeners(cDestroyEvent);
  if _Listeners <> nil then
    Self.RemoveOpaquePointerFromList(Pointer(Sender), _Listeners);
end;

constructor TEventsManager.Create(AnOwner: TObject; OperationMode: TEventsManagerMode);
begin
  inherited Create;

  FRaisingEvents := False;
  FMode := OperationMode;

  FOwner := AnOwner;
  if FOwner = nil then
    FOwner := Self;

  FEvents := TStringList.Create();
  FEvents.Sorted := True;
  FEvents.CaseSensitive := True;
  FEvents.Duplicates := dupError;
  FEvents.OwnsObjects := True;

  FCS := TTimedOutCriticalSection.Create;

  Self.AddEvent(cDestroyEvent);

  FDisabledEvents := TStringList.Create();
  FDisabledEvents.Sorted := True;
  FDisabledEvents.Duplicates := dupIgnore;

  FEnabled := True;

  FFreeOnTerminate := False;
  FDestroyingState := False;
end;

destructor TEventsManager.Destroy;
var
  _Owner : String;

begin
  if Self <> nil then
  begin

    if not FDestroyingState then
    begin
      FDestroyingState := True;

      if FOwner <> nil then
        _Owner := FOwner.ClassName
      else
        _Owner := 'No owner';

      try

        Self.RaiseEvent(cDestroyEvent);

      except
        on E: Exception do
          LBLogger.Write(1, 'TEventsManager.Destroy', lmt_Error, '1. <%s> %s', [_Owner, E.Message]);
      end;

      try

        if FCS <> nil then
          FreeAndNil(FCS);

        if FEvents <> nil then
          FreeAndNil(FEvents);

        if FDisabledEvents <> nil then
          FreeAndNil(FDisabledEvents);

      except
        on E: Exception do
          LBLogger.Write(1, 'TEventsManager.Destroy', lmt_Error, '2. <%s>  %s', [_Owner, E.Message]);
      end;

      inherited Destroy;

    end;
  end;
end;

procedure TEventsManager.Free;
begin
  if not FRaisingEvents then
    inherited Free
  else begin
    FFreeOnTerminate := True;
    LBLogger.Write(1, 'TEventsManager.Free', lmt_Warning, 'Request during raising events!');
  end;
end;

procedure TEventsManager.AddEvent(AnEvent: String);
var
  _Idx : Integer;
  _List : TEventListeners;

begin
  if AnEvent <> '' then
  begin

    if FCS.Acquire('TEventsManager.AddEvent') then
    begin

      try

        _Idx := FEvents.IndexOf(AnEvent);
        if _Idx = -1 then
        begin
          _List := TEventListeners.Create;
          FEvents.AddObject(AnEvent, _List);
          FEvents.Sort;
        end;

      except
        on E: Exception do
          LBLogger.Write(1, 'TEventsManager.AddEvent', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

  end;
end;

function TEventsManager.AddEventListener(AnEventName: String; AnEventToRaise: TNotifyEvent; AnEventManager: TEventsManager): Boolean;
begin
  Result := False;

  if (FMode = emm_Events) and (AnEventToRaise <> nil) then
    Result := Self.AddEventListenerInternal(AnEventName, @TMethod(AnEventToRaise), nil, AnEventManager.Owner, anEventManager);
end;

procedure TEventsManager.RemoveEventListener(AnEventName: String; AnEventManager: TEventsManager);
begin
  if (not FRaisingEvents) and (AnEventManager <> nil) then
    Self.RemoveOpaquePointerFromList(Pointer(AnEventManager.Owner), Self.getListeners(AnEventName));
end;

function TEventsManager.AddEventListener(AnEventName: PAnsiChar; anOpaquePointer: Pointer; aCallback: TCallbackProcedure): Boolean;
begin
  Result := False;

  if (FMode = emm_Callbacks) and (aCallback <> nil) then
    Result := Self.AddEventListenerInternal(AnsiString(AnEventName), nil, aCallback, anOpaquePointer, nil);
end;

function TEventsManager.AddSingleCallbackListener(AnEventToRaise: TSingleCallbackEvent; AnEventManager: TEventsManager): Boolean;
var
  i : Integer;

begin
  Result := False;

  if FMode = emm_EventsSingleCallback then
  begin
    if FCS.Acquire('TEventsManager.AddSingleCallbackListener') then
    begin
      try

        Result := True;
        for i := 0 to FEvents.Count - 1 do
          Result := Result and Self.AddEventListenerInternal(FEvents.Strings[i], @TMethod(AnEventToRaise), nil, AnEventManager.Owner, AnEventManager);

      except
        on E: Exception do
          LBLogger.Write(1, 'TEventsManager.AddSingleCallbackListener', lmt_Error, E.Message);
      end;
      FCS.Release();
    end;
  end;
end;

procedure TEventsManager.RemoveListener(anOpaquePointer: Pointer);
var
  i : Integer;

begin
  if (Self <> nil) and (anOpaquePointer <> nil) and (FEvents <> nil) then
  begin

    if not FDestroyingState then
    begin
      if FCS.Acquire('TEventsManager.RemoveListener') then
      begin
        try

          for i := FEvents.Count - 1 downto 0 do
          begin
            if FEvents.Strings[i] <> cDestroyEvent then
              Self.RemoveOpaquePointerFromList(anOpaquePointer, TEventListeners(FEvents.Objects[i]));
          end;

        except
          on E: Exception do
            LBLogger.Write(1, 'TEventsManager.RemoveListener', lmt_Error, E.Message);
        end;
        FCS.Release();
      end;
    end
    else
      LBLogger.Write(1, 'TEventsManager.RemoveListener', lmt_Warning, 'Nothing to do ... destroying state!');
  end;

end;

procedure TEventsManager.RemoveEventListener(AnEventName: PAnsiChar; anObscurePointer: Pointer);
begin
  if (AnEventName <> nil) and (not FRaisingEvents) then
    Self.RemoveOpaquePointerFromList(anObscurePointer, Self.getListeners(AnsiString(AnEventName)));
end;

procedure TEventsManager.DisableEvent(AnEvent: String);
begin
  FDisabledEvents.Add(AnEvent);
end;

procedure TEventsManager.EnableEvent(AnEvent: String);
var
  _idx : Integer;

begin
  _idx := FDisabledEvents.IndexOf(AnEvent);
  if _idx > -1 then
    FDisabledEvents.Delete(_idx);
end;

procedure TEventsManager.RaiseEvent(AnEvent: String);
begin
  if (Self <> nil) then
  begin
    if FOwner <> nil then
      Self.RaiseEvent(AnEvent, FOwner)
    else
      Self.RaiseEvent(AnEvent, Self);
  end
  else
    LBLogger.Write(1, 'TEventsManager.RaiseEvent', lmt_Warning, 'I''m NIL!!!');
end;

procedure TEventsManager.RaiseEvent(AnEvent: String; Sender: TObject);
var
  _List : TEventListeners;
  i : Integer;
  _DestroyEvent : Boolean;
  _EventInfo : IEventInfo;
  _Count : Integer;

begin
  if Self <> nil then
  begin
    _DestroyEvent := anEvent = cDestroyEvent;

    if FEnabled or _DestroyEvent then
    begin
      FRaisingEvents := True;

      if FCS.Acquire('TEventsManager.RaiseEvent') then
      begin
        try

          if _DestroyEvent or (FDisabledEvents = nil) or (FDisabledEvents.IndexOf(AnEvent) = -1) then
          begin


            _List := Self.getListeners(AnEvent);
            if _List <> nil then
            begin

              for i := 0 to _List.Count - 1 do
                (_List.Items[i] as IEventInfo).setExecuted(False);

              i := _List.Count - 1;
              while (_List.Count > 0) and (i >= 0) do
              begin
                _Count := _List.Count;
                if i > _Count - 1 then
                  i := _Count - 1;
                _EventInfo := _List.Items[i] as IEventInfo;

                if not _EventInfo.isExecuted then
                begin
                  _EventInfo.setExecuted(True);

                  case FMode of
                    emm_Events               : TNotifyEvent((_EventInfo.getEvent())^)(Sender);
                    emm_Callbacks            : TCallbackProcedure(_EventInfo.getCallBack())(_EventInfo.getOwner());
                    emm_EventsSingleCallback : TSingleCallbackEvent((_EventInfo.getEvent())^)(Sender, AnEvent);
                  end;
                  i -= 1;
                end;
              end;
            end;
          end;

        except
          on E: Exception do
            LBLogger.Write(1, 'TEventsManager.RaiseEvent', lmt_Error, '<%s>  -  Error raising event <%s>: <%s>', [FOwner.ClassName, AnEvent, E.Message]);
        end;

        FCS.Release();

      end;

    end;

    FRaisingEvents := False;
    if FFreeOnTerminate then
      Self.Destroy;
  end;
end;

{ TEventsManager.TEventInfo }

constructor TEventsManager.TEventInfo.Create(AnOwner: Pointer);
begin
  inherited Create;

  FOwner := AnOwner;
end;

function TEventsManager.TEventInfo.isExecuted(): Boolean;
begin
  Result := FExecuted;
end;

procedure TEventsManager.TEventInfo.setExecuted(aValue: Boolean);
begin
  FExecuted := aValue;
end;

function TEventsManager.TEventInfo.getCallback(): Pointer;
begin
  Result := FCallback;
end;

procedure TEventsManager.TEventInfo.setCallback(aCallback: Pointer);
begin
  FCallback := aCallback;
end;

function TEventsManager.TEventInfo.getEvent(): PMethod;
begin
  Result := @FMethod;
end;

procedure TEventsManager.TEventInfo.setEvent(anEvent: pMethod);
begin
  FMethod := anEvent^;
end;

function TEventsManager.TEventInfo.getOwner(): Pointer;
begin
  Result := FOwner;
end;

{ TEventsManager.TEventListeners }

function TEventsManager.TEventListeners.ListenerAlreadyIn(aListener: Pointer): Boolean;
var
  _Event : IEventInfo;
  i : Integer;

begin
  Result := False;

  for i := 0 to Self.Count - 1 do
  begin
    _Event := Self.Items[i] as IEventInfo;
    if _Event.getOwner() = aListener then
    begin
      Result := True;
      Break;
    end;
  end;

end;

end.
