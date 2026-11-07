unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uEventsManager,
  Variants;

type

  { TSumObject }

  TSumObject = class(TObject)
    strict private
      FSum : Integer;
      FEventsManager : TEventsManager;

    private
      procedure addValue(Sender: TObject);

    public
      constructor Create;
      destructor Destroy; override;

      property EventsManager: TEventsManager read FEventsManager;
  end;

  { TInfoObject }

  TInfoObject = class(TObject)
    strict private
      FLastMessage : String;
      FEventsManager : TEventsManager;

    private
      procedure setMessage(Sender: TObject);

    public
      constructor Create;
      destructor Destroy; override;

      property EventsManager: TEventsManager read FEventsManager;
  end;

  { Tfrmmain }

  Tfrmmain = class(TForm)
    btnraiseevent: TButton;
    eddata: TEdit;
    memolog: TMemo;
    procedure btnraiseeventClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSum : TSumObject;
    FText : TInfoObject;

    FEventsSource : TEventsManager;
    FValue : Variant;

  public
    property Value: Variant read FValue;
  end;

var
  frmmain: Tfrmmain;

implementation

{$R *.lfm}

{ TSumObject }

procedure TSumObject.addValue(Sender: TObject);
var
  _MainForm : Tfrmmain;

begin
  if Sender is Tfrmmain then
  begin
    _MainForm := Tfrmmain(Sender);
    Inc(FSum, Integer(_MainForm.Value));
    _MainForm.memolog.Append(Format('New value: %d', [FSum]));
  end;
end;

constructor TSumObject.Create;
begin
  FSum := 0;
  FEventsManager := TEventsManager.Create(Self);
end;

destructor TSumObject.Destroy;
begin
  FreeAndNil(FEventsManager);

  inherited Destroy;
end;

{ TInfoObject }

procedure TInfoObject.setMessage(Sender: TObject);
var
  _MainForm : Tfrmmain;

begin
  if Sender is Tfrmmain then
  begin
    _MainForm := Tfrmmain(Sender);
    FLastMessage := VarToStr(_MainForm.Value);
    _MainForm.memolog.Append(Format('Message: %s', [FLastMessage]));
  end;
end;

constructor TInfoObject.Create;
begin
  FEventsManager := TEventsManager.Create(Self);
end;

destructor TInfoObject.Destroy;
begin
  FreeAndNil(FEventsManager);

  inherited Destroy;
end;

{ Tfrmmain }

procedure Tfrmmain.FormCreate(Sender: TObject);
begin
  FEventsSource := TEventsManager.Create(Self);
  FEventsSource.AddEvent('Integer');
  FEventsSource.AddEvent('String');

  FSum := TSumObject.Create;
  FEventsSource.AddEventListener('Integer', @FSum.addValue, FSum.EventsManager);

  FText := TInfoObject.Create;
  FEventsSource.AddEventListener('Integer', @FText.setMessage, FText.EventsManager);
  FEventsSource.AddEventListener('String', @FText.setMessage, FText.EventsManager);
end;

procedure Tfrmmain.btnraiseeventClick(Sender: TObject);
var
  _Value : String;
  _iValue : Integer;

begin
  _Value := Trim(eddata.Text);
  if _Value <> '' then
  begin
    if TryStrToInt(_Value, _iValue) then
    begin
      FValue := _iValue;
      FEventsSource.RaiseEvent('Integer');
    end
    else begin
      FValue := _Value;
      FEventsSource.RaiseEvent('String');
    end;
  end;
end;

procedure Tfrmmain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSum);
  FreeAndNil(FText);
  FreeAndNil(FEventsSource);
end;

end.

