unit ufrmMain;

{$mode objfpc}{$H+}

{
  Form principale del software RTK Rover.

  ── Flusso dati ──────────────────────────────────────────────────────────

    Caster NTRIP ──TCP──► TRTCMClient ──OnCorrections──► TUBXRTKRover
                                                               │
                                                          GPS (COM3)
                                                         /          \
                                                    OnPVT        OnNMEARaw
                                                       │               │
                                                 [aggiorna UI]   TNMEAForwarder
                                                                  └──► COM8
                                                                         │
                                                                   [FVSP bridge]
                                                                         │
                                                                    COM9 (terze parti)

  ── Modalità output ──────────────────────────────────────────────────────
    UBX:  il modulo emette NAV-PVT binario → massima precisione (hAcc/vAcc)
          → OnNMEARaw genera GGA+RMC sintetici per il forwarder
    NMEA: il modulo emette GGA+RMC ASCII → massima compatibilità
          → OnNMEARaw passa le righe originali al forwarder
}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UBX_RTKRover, NMEAForwarder, RTCMClient, RTKTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    lblGPSPort    : TLabel;
    edGPSPort     : TEdit;
    rgMode        : TRadioGroup;
    lblVirtPort   : TLabel;
    edVirtPort    : TEdit;
    cbForward     : TCheckBox;
    lblNTRIP      : TLabel;
    edNTRIPHost   : TEdit;
    edNTRIPPort   : TEdit;
    edMountpoint  : TEdit;
    edUsername    : TEdit;
    edPassword    : TEdit;
    cbNTRIPActive : TCheckBox;
    btnStart      : TButton;
    btnStop       : TButton;
    btnSaveConfig : TButton;
    mLog          : TMemo;

    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSaveConfigClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRover       : TUBXRTKRover;
    FForwarder   : TNMEAForwarder;
    FNTRIPClient : TRTCMClient;

    procedure OnPVTReceived(Sender: TObject; const Data: TPVTData);
    procedure OnNMEARawReceived(Sender: TObject; const Line: string);
    procedure OnCorrectionsReceived(Sender: TObject; const Data: TBytes);

    procedure UpdateUI(Active: Boolean);
    procedure LogLine(const S: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.frm}

uses
  ULBLogger, StrUtils;

const
  RTK_LABEL : array[TRTKStatus] of string = ('GNSS', 'Float', 'Fixed');
  FIX_LABEL  : array[TFixType]   of string = (
    'NoFix', 'DeadReck', '2D', '3D', 'GNSS+DR', 'TimeOnly');

{ ══ init ═════════════════════════════════════════════════════════════════════ }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitLogger(5, 'RTKRover.log');

  if rgMode.Items.Count = 0 then
  begin
    rgMode.Caption := 'Modalità output posizione';
    rgMode.Items.Add('UBX — NAV-PVT binario (hAcc/vAcc disponibili)');
    rgMode.Items.Add('NMEA — GGA+RMC ASCII (compatibile con app esterne)');
  end;
  rgMode.ItemIndex := 0;

  cbForward.Caption     := 'Inoltro NMEA su porta virtuale';
  cbNTRIPActive.Caption := 'Abilita correzioni NTRIP';

  mLog.ScrollBars := ssVertical;
  mLog.ReadOnly   := True;

  edGPSPort.Text   := 'COM3';
  edVirtPort.Text  := 'COM8';
  edNTRIPPort.Text := '2101';

  UpdateUI(False);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if FNTRIPClient <> nil then FreeAndNil(FNTRIPClient);
  if FRover       <> nil then FreeAndNil(FRover);
  if FForwarder   <> nil then FreeAndNil(FForwarder);
  ReleaseLogger();
end;

procedure TfrmMain.UpdateUI(Active: Boolean);
begin
  btnStart.Enabled      := not Active;
  btnStop.Enabled       := Active;
  btnSaveConfig.Enabled := Active;
  edGPSPort.Enabled     := not Active;
  edVirtPort.Enabled    := not Active;
  rgMode.Enabled        := not Active;
  cbForward.Enabled     := not Active;
  edNTRIPHost.Enabled   := not Active;
  edNTRIPPort.Enabled   := not Active;
  edMountpoint.Enabled  := not Active;
  edUsername.Enabled    := not Active;
  edPassword.Enabled    := not Active;
  cbNTRIPActive.Enabled := not Active;
end;

procedure TfrmMain.LogLine(const S: string);
begin
  mLog.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + S);
  mLog.SelStart  := Length(mLog.Text);
  mLog.SelLength := 0;
end;

{ ══ Start / Stop ═════════════════════════════════════════════════════════════ }

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  Mode: TRoverOutputMode;
begin
  if FRover <> nil then Exit;

  { ── 1. Porta virtuale NMEA (opzionale) ──────────────────────────────── }
  if cbForward.Checked then
  begin
    FForwarder := TNMEAForwarder.Create;
    if not FForwarder.Open(Trim(edVirtPort.Text)) then
    begin
      FreeAndNil(FForwarder);
      LogLine('AVVISO: porta virtuale ' + edVirtPort.Text +
              ' non disponibile — forwarding disabilitato');
    end;
  end;

  { ── 2. Rover GPS ────────────────────────────────────────────────────── }
  case rgMode.ItemIndex of
    1: Mode := romNMEA;
  else Mode := romUBX;
  end;

  FRover            := TUBXRTKRover.Create(Trim(edGPSPort.Text));
  FRover.OutputMode := Mode;
  FRover.OnPVT      := @OnPVTReceived;
  if FForwarder <> nil then
    FRover.OnNMEARaw := @OnNMEARawReceived;

  if not FRover.Open then
  begin
    FreeAndNil(FRover);
    FreeAndNil(FForwarder);
    LogLine('ERRORE: impossibile aprire la porta GPS ' + edGPSPort.Text);
    Exit;
  end;

  { ── 3. Client NTRIP (opzionale) ─────────────────────────────────────── }
  if cbNTRIPActive.Checked and (Trim(edNTRIPHost.Text) <> '') then
  begin
    FNTRIPClient := TRTCMClient.Create(
      stNTRIP,
      Trim(edNTRIPHost.Text),
      StrToIntDef(Trim(edNTRIPPort.Text), 2101));

    FNTRIPClient.Mountpoint    := Trim(edMountpoint.Text);
    FNTRIPClient.Username      := Trim(edUsername.Text);
    FNTRIPClient.Password      := Trim(edPassword.Text);
    FNTRIPClient.OnCorrections := @OnCorrectionsReceived;

    if not FNTRIPClient.Open then
    begin
      FreeAndNil(FNTRIPClient);
      LogLine('AVVISO: impossibile avviare il client NTRIP');
    end;
  end;

  UpdateUI(True);
  LogLine('Avviato — GPS: ' + edGPSPort.Text +
    ' | Modalità: ' + rgMode.Items[rgMode.ItemIndex] +
    IfThen(FForwarder   <> nil, ' | Forward: ' + edVirtPort.Text, '') +
    IfThen(FNTRIPClient <> nil, ' | NTRIP: ' + edNTRIPHost.Text, ''));
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  if FNTRIPClient <> nil then FreeAndNil(FNTRIPClient);
  if FRover       <> nil then FreeAndNil(FRover);
  if FForwarder   <> nil then FreeAndNil(FForwarder);
  UpdateUI(False);
  LogLine('Fermato.');
end;

procedure TfrmMain.btnSaveConfigClick(Sender: TObject);
begin
  if FRover = nil then Exit;
  if FRover.SaveConfig then
    LogLine('Configurazione salvata in flash')
  else
    LogLine('SaveConfig: nessun ACK dal modulo');
end;

{ ══ Callback rover ═══════════════════════════════════════════════════════════ }

procedure TfrmMain.OnPVTReceived(Sender: TObject; const Data: TPVTData);
{ Chiamato dal thread seriale — usa Queue per aggiornare la UI in sicurezza. }
var
  Fix        : TFixType;
  RTK        : TRTKStatus;
  Lat, Lon   : Double;
  Alt, Spd   : Double;
  Hdg        : Double;
  SV         : Byte;
  HAcc, VAcc : Cardinal;
begin
  Fix  := Data.FixType;
  RTK  := Data.RTKStatus;
  Lat  := Data.Latitude;
  Lon  := Data.Longitude;
  Alt  := Data.AltitudeMSL;
  Spd  := Data.SpeedKmH;
  Hdg  := Data.HeadingDeg;
  SV   := Data.NumSV;
  HAcc := Data.HAccMm;
  VAcc := Data.VAccMm;

  TThread.Queue(nil, procedure
  begin
    if frmMain = nil then Exit;
    frmMain.LogLine(Format(
      '%s %s  lat=%.7f  lon=%.7f  alt=%.1fm  sv=%d  ' +
      'hacc=%d mm  vacc=%d mm  spd=%.1f km/h  hdg=%.1f°',
      [FIX_LABEL[Fix], RTK_LABEL[RTK],
       Lat, Lon, Alt, SV, HAcc, VAcc, Spd, Hdg]));
  end);
end;

procedure TfrmMain.OnNMEARawReceived(Sender: TObject; const Line: string);
{ Chiamato dal thread seriale.
  TNMEAForwarder.Write è thread-safe: nessun Queue necessario. }
begin
  if FForwarder <> nil then
    FForwarder.Write(Line);
end;

{ ══ Callback NTRIP ═══════════════════════════════════════════════════════════ }

procedure TfrmMain.OnCorrectionsReceived(Sender: TObject; const Data: TBytes);
{ Chiamato dal thread TCP del client NTRIP.
  PushCorrections è thread-safe. }
begin
  if FRover <> nil then
    FRover.PushCorrections(Data);
end;

end.
