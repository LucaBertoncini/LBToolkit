unit Toolkit_CAPI_VirtualKeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, uLBVirtualKeyboard, uNonFocusableForm, Interfaces, LCLIntf, LCLType;

type
  // A record to hold all the necessary handles for a keyboard instance.
  // The pointer to this record will be our C-API handle.
  TVKeyboardHandle = record
    Form: TNonFocusableForm;
    Keyboard: TKeyboard;
  end;
  PKeyboardHandle = ^TVKeyboardHandle;

// --- Virtual Keyboard C-API ---
procedure VKeyboard_Initialize(aAppHandle: THandle); export; cdecl;
procedure VKeyboard_Finalize; export; cdecl;
function VKeyboard_Create(aParentHandle: THandle): Pointer; export; cdecl;
procedure VKeyboard_Destroy(aHandle: Pointer); export; cdecl;
procedure VKeyboard_Show(aHandle: Pointer); export; cdecl;
procedure VKeyboard_Hide(aHandle: Pointer); export; cdecl;
function VKeyboard_LoadLayout(aHandle: Pointer; aLayoutFile: PChar): Boolean; export; cdecl;
procedure VKeyboard_SetPositionAndSize(aHandle: Pointer; aLeft, aTop, aWidth, aHeight: Integer); export; cdecl;

implementation

uses
  ULBLogger, uLBVKCommons;

var
  G_VKeyboardContextInitialized: Boolean = False;

procedure VKeyboard_Initialize(aAppHandle: THandle); cdecl;
begin
  // This must be called once by the host application to set the LCL context.
  if not G_VKeyboardContextInitialized then
  begin
    if aAppHandle <> 0 then
      Application.Handle := aAppHandle
    else
      Application.Initialize;

    G_VKeyboardContextInitialized := True;
  end
  else
    LBLogger.Write(1, 'VKeyboard_Initialize', lmt_Warning, 'Keyboard context already initialized!');
end;

procedure VKeyboard_Finalize; cdecl;
begin
  // Placeholder for any future global cleanup.
  G_VKeyboardContextInitialized := False;
end;

function VKeyboard_Create(aParentHandle: THandle): Pointer; cdecl;
var
  HandleRec: PKeyboardHandle;
begin
  Result := nil;
  if G_VKeyboardContextInitialized then // It is mandatory to call VKeyboard_Initialize first.
  begin

    try
      New(HandleRec);

      HandleRec^.Form := TNonFocusableForm.Create(nil);
      HandleRec^.Form.ParentWindow := aParentHandle;
      // The main application context is set via VKeyboard_Initialize.
      // The form's fsStayOnTop style keeps it on top of the parent window.

      HandleRec^.Keyboard := TKeyboard.Create(HandleRec^.Form);
      HandleRec^.Keyboard.DestroyBitmaps := False;
      HandleRec^.Keyboard.Bitmap[sk_None]      := HandleRec^.Form.EmptyKey;
      HandleRec^.Keyboard.Bitmap[sk_BackSpace] := HandleRec^.Form.BackSpace;
      HandleRec^.Keyboard.Bitmap[sk_Space]     := HandleRec^.Form.SpaceKey;
      HandleRec^.Keyboard.Bitmap[sk_Return]    := HandleRec^.Form.ReturnKey;
      HandleRec^.Keyboard.Bitmap[sk_Shift]     := HandleRec^.Form.ShiftKey;
      HandleRec^.Keyboard.Bitmap[sk_Tab]       := HandleRec^.Form.TabKey;
      HandleRec^.Keyboard.Bitmap[sk_Switcher]  := HandleRec^.Form.SwitcherKey;


      Result := Pointer(HandleRec);

    except
      on E: Exception do
      begin
        Result := nil;
        LBLogger.Write(1, 'VKeyboard_Create', lmt_Error, E.Message);
      end;
    end;

  end
  else
    LBLogger.Write(1, 'VKeyboard_Create', lmt_Warning, 'Keyboard context not initialized!');
end;

procedure VKeyboard_Destroy(aHandle: Pointer); cdecl;
var
  _HandleRec: PKeyboardHandle;
begin
  if aHandle <> nil then
  begin
    LBLogger.Write(5, 'VKeyboard_Destroy', lmt_Debug, 'Destroying virtual keyboard ...');
    try

      _HandleRec := PKeyboardHandle(aHandle);
      _HandleRec^.Form.Hide;
      _HandleRec^.Form.VirtualKeyboard := nil;
      if _HandleRec^.Keyboard <> nil then
      begin
        LBLogger.Write(5, 'VKeyboard_Destroy', lmt_Debug, 'Destroying keyboard ...');
        FreeAndNil(_HandleRec^.Keyboard);
      end;

      if _HandleRec^.Form <> nil then
      begin
        LBLogger.Write(5, 'VKeyboard_Destroy', lmt_Debug, 'Virtual keyboard destroyed, destroying form ...');
        FreeAndNil(_HandleRec^.Form);
      end;

      Dispose(_HandleRec);

    except
      on E: Exception do
        LBLogger.Write(1, 'VKeyboard_Destroy', lmt_Error, E.Message);
    end;
  end;
end;

procedure VKeyboard_Show(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    PKeyboardHandle(aHandle)^.Form.Show;
end;

procedure VKeyboard_Hide(aHandle: Pointer); cdecl;
begin
  if aHandle <> nil then
    PKeyboardHandle(aHandle)^.Form.Hide;
end;

function VKeyboard_LoadLayout(aHandle: Pointer; aLayoutFile: PChar): Boolean; cdecl;
var
  HandleRec: PKeyboardHandle;
begin
  Result := False;
  if (aHandle <> nil) and (aLayoutFile <> nil) then
  begin
    try
      HandleRec := PKeyboardHandle(aHandle);
      Result := HandleRec^.Keyboard.LoadLayoutFromXMLFile(StrPas(aLayoutFile));
      if Result then
        HandleRec^.Keyboard.DrawKeyboard;

    except
      on E: Exception do
        LBLogger.Write(1, 'VKeyboard_LoadLayout', lmt_Error, E.Message);
    end;
  end
  else
    LBLogger.Write(1, 'VKeyboard_LoadLayout', lmt_Warning, 'Handle or layout file not set!');
end;

procedure VKeyboard_SetPositionAndSize(aHandle: Pointer; aLeft, aTop, aWidth, aHeight: Integer); cdecl;
var
  HandleRec: PKeyboardHandle;
begin
  if aHandle <> nil then
  begin
    HandleRec := PKeyboardHandle(aHandle);
    HandleRec^.Form.SetBounds(aLeft, aTop, aWidth, aHeight);
    HandleRec^.Keyboard.DrawKeyboard;
  end;
end;

end.

