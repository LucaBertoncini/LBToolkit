program EsempioR700;

{
  Esempio di utilizzo di TImpinjR700.
  Dimostra:
  - Configurazione potenza e sensibilità per antenna
  - Connessione e avvio scan
  - Ricezione tag con EPC, antenna, RSSI
  - Stop scan e disconnessione

  Per usarlo in un'applicazione VCL:
  - sostituire le WriteLn con aggiornamento di Label/ListBox
  - StartScan/StopScan da pulsanti
  - Open/Close on form create/destroy
}

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ImpinjR700;

var
  Reader   : TImpinjR700;
  TagCount : Integer = 0;

{ ── Callback tag letto ─────────────────────────────────────────────────────── }

procedure OnTagRead(Sender: TObject; const Tag: TTagData);
begin
  Inc(TagCount);
  WriteLn(Format('[%s] #%d  EPC: %-24s  Ant: %d  RSSI: %d dBm  Viste: %d',
    [FormatDateTime('hh:nn:ss.zzz', Tag.Timestamp),
     TagCount,
     Tag.EPC,
     Tag.AntennaID,
     Tag.RSSI,
     Tag.SeenCount]));
end;

{ ── Callback stato ──────────────────────────────────────────────────────────── }

procedure OnStatus(Sender: TObject; const Msg: string);
begin
  WriteLn('[STATUS] ' + Msg);
end;

{ ── Callback errore ─────────────────────────────────────────────────────────── }

procedure OnError(Sender: TObject; const Msg: string);
begin
  WriteLn('[ERRORE] ' + Msg);
end;

{ ── Main ─────────────────────────────────────────────────────────────────────── }

begin
  WriteLn('══════════════════════════════════════════');
  WriteLn('  Impinj R700 — Test lettura RFID');
  WriteLn('══════════════════════════════════════════');

  Reader := TImpinjR700.Create('192.168.1.100');  // ← IP del lettore
  try
    { ── Configura i callback ─────────────────────────────────────────── }
    Reader.OnTagRead := @OnTagRead;
    Reader.OnStatus  := @OnStatus;
    Reader.OnError   := @OnError;

    { ── Configura le antenne ─────────────────────────────────────────── }
    { Antenna 1: potenza massima per campo aperto (verificare normativa) }
    Reader.SetAntennaPower(1, 25.0);      // 25.00 dBm
    Reader.SetAntennaRxSens(1, -70);      // -70 dBm

    { Antenna 2: potenza ridotta (es. corridoio stretto del magazzino) }
    Reader.SetAntennaPower(2, 20.0);      // 20.00 dBm
    Reader.SetAntennaRxSens(2, -65);      // -65 dBm (meno sensibile = meno rumore)

    { Antenna 3 e 4: disabilitate in questo scenario }
    Reader.SetAntennaEnabled(3, False);
    Reader.SetAntennaEnabled(4, False);

    { In alternativa puoi impostare una configurazione completa così: }
    {
    var Cfg: TAntennaConfig;
    Cfg.Enabled   := True;
    Cfg.PowerDBm  := 23.0;
    Cfg.RxSensDBm := -70;
    Reader.AntennaConfig[1] := Cfg;
    }

    { ── Connetti e configura il reader ────────────────────────────────── }
    WriteLn('Connessione in corso...');
    Reader.Open;

    { Breve attesa per la configurazione iniziale (SET_READER_CONFIG +
      ADD_ROSPEC + ENABLE_ROSPEC avvengono nel thread, internamente) }
    Sleep(1500);

    if not Reader.Active then
    begin
      WriteLn('Connessione fallita. Uscita.');
      Exit;
    end;

    { ── Avvia la scansione ────────────────────────────────────────────── }
    WriteLn('Avvio scan... (premi INVIO per fermare)');
    Reader.StartScan;

    { Attendi input utente (in app VCL questo sarebbe gestito da un Button) }
    ReadLn;

    { ── Ferma la scansione ────────────────────────────────────────────── }
    WriteLn('Stop scan...');
    Reader.StopScan;
    Sleep(500);  // lascia tempo al reader di terminare i report pendenti

    WriteLn(Format('Tag unici letti: %d', [TagCount]));

    { ── Disconnetti ───────────────────────────────────────────────────── }
    WriteLn('Disconnessione...');
    Reader.Close;
    WriteLn('Chiuso.');

  finally
    Reader.Free;
  end;
end.


{ ══════════════════════════════════════════════════════════════════════════════
  ESEMPIO 2: Utilizzo in un muletto — dispatcher RTCM + lettura RFID

  L'idea è che il muletto abbia:
  - TRTCMClient      → riceve correzioni GPS dalla base
  - TUBXRTKRover     → rover GPS a 5 Hz
  - TImpinjR700      → lettore RFID sul portale di carico

  Frammento di codice per la gestione integrata:

  type
    TForkliftController = class
    private
      FGPS    : TUBXRTKRover;
      FRFID   : TImpinjR700;
      FCorr   : TRTCMClient;
      procedure OnGPSFix(Sender: TObject; const Data: TPVTData);
      procedure OnTagRead(Sender: TObject; const Tag: TTagData);
      procedure OnCorrections(Sender: TObject; const Data: TBytes; Len: Integer);
    public
      procedure Start;
      procedure Stop;
    end;

  procedure TForkliftController.OnCorrections(Sender: TObject;
    const Data: TBytes; Len: Integer);
  begin
    FGPS.PushCorrections(Data);  // thread-safe, nessun Synchronize necessario
  end;

  procedure TForkliftController.OnGPSFix(Sender: TObject;
    const Data: TPVTData);
  begin
    if Data.RTKStatus = rtkFixed then
      // posizione cm-level disponibile: aggiorna mappa muletto
      UpdateForkliftPosition(Data.Latitude, Data.Longitude, Data.HAccMm);
  end;

  procedure TForkliftController.OnTagRead(Sender: TObject;
    const Tag: TTagData);
  begin
    // Tag letto dal portale RFID: registra su server
    LogTagRead(Tag.EPC, Tag.AntennaID, Tag.RSSI, Now);
  end;

  procedure TForkliftController.Start;
  begin
    FCorr := TRTCMClient.Create('192.168.1.1', 2101);
    FCorr.OnCorrections := @OnCorrections;
    FCorr.Open;

    FGPS := TUBXRTKRover.Create('COM3', 38400, 5);
    FGPS.OnPVT   := @OnGPSFix;
    FGPS.Open;

    FRFID := TImpinjR700.Create('192.168.1.100');
    FRFID.SetAntennaPower(1, 23.0);
    FRFID.OnTagRead := @OnTagRead;
    FRFID.Open;
    Sleep(1500);
    FRFID.StartScan;
  end;

══════════════════════════════════════════════════════════════════════════════ }
