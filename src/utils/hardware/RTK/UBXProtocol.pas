unit UBXProtocol;

{
  Livello comune del protocollo UBX u-blox.
  Indipendente dal dispositivo e dalla versione firmware.

  Contiene:
  - Costanti di framing (sync, header, checksum)
  - Costanti classi e ID messaggi universali (CFG, ACK, NAV)
  - Costanti layer CFG-VALSET con nomi che esprimono lo scopo
  - Framing e checksum: BuildUBX, UBXChecksum, ValidUBXChecksum
  - Parser ACK minimale: WaitACK (lavora su TBlockSerial)
  - Helper encoding CFG-VALSET: AddBool, AddU1, AddU2, AddU4

  Riferimento: u-blox F9 HPG 1.51 Interface Description
               UBXDOC-963802114-13124 — struttura valida per tutte
               le famiglie u-blox che usano il protocollo UBX binario.

  ── Note sui layer CFG-VALSET ──────────────────────────────────────────
  I layer determinano dove viene scritta la configurazione:

    VALSET_LAYER_RAM        Solo RAM — persa al riavvio.
                            Uso tipico: configurazione operativa normale,
                            modificabile senza consumare cicli flash.

    VALSET_LAYER_BBR        Solo Battery-Backed RAM — sopravvive al
                            riavvio se la batteria è presente, ma non
                            a una perdita totale di alimentazione.
                            Uso tipico: config semi-permanente su sistemi
                            con backup batteria.

    VALSET_LAYER_FLASH      Solo Flash — permanente, sopravvive a ogni
                            reset e perdita di alimentazione.
                            ATTENZIONE: il ZED-F9P ha ~10.000 cicli di
                            scrittura flash. Usare solo per configurazioni
                            definitive.

    VALSET_LAYER_RAM_FLASH  RAM + Flash in un solo invio — applica la
                            configurazione subito (RAM) e la rende
                            permanente (Flash) contemporaneamente.
                            Uso tipico: SaveConfig dopo aver verificato
                            che la config RAM funziona correttamente.

  I layer sono maschere di bit combinabili: bit0=RAM, bit1=BBR, bit2=Flash.

  ── Dipendenze ─────────────────────────────────────────────────────────
  SynaSer (Synapse) per TBlockSerial usato in WaitACK.
}

interface

uses
  Classes, SysUtils, SynaSer;

{ ══ Costanti di framing ═════════════════════════════════════════════════════ }

const
  UBX_SYNC1     = $B5;   // primo byte di sincronizzazione
  UBX_SYNC2     = $62;   // secondo byte di sincronizzazione
  UBX_HDR_SIZE  = 6;     // sync(2) + class(1) + id(1) + length(2)
  UBX_CRC_SIZE  = 2;     // CK_A + CK_B (Fletcher-8)

{ ══ Classi messaggi ═════════════════════════════════════════════════════════ }

  UBX_CLASS_NAV = $01;   // Navigation messages (NAV-PVT, NAV-STATUS, ...)
  UBX_CLASS_CFG = $06;   // Configuration messages
  UBX_CLASS_MON = $0A;   // Monitoring messages
  UBX_CLASS_ACK = $05;   // Acknowledge / Not-acknowledge

{ ══ ID messaggi ACK ════════════════════════════════════════════════════════ }

  UBX_ID_ACK_ACK = $01;  // comando accettato
  UBX_ID_ACK_NAK = $00;  // comando rifiutato

{ ══ ID messaggi CFG ════════════════════════════════════════════════════════ }

  UBX_ID_CFG_CFG    = $09;   // Clear/Save/Load configuration (legacy)
  UBX_ID_CFG_VALSET = $8A;   // Set configuration items (HPG ≥ 23.01)
  UBX_ID_CFG_VALGET = $8B;   // Get configuration items
  UBX_ID_CFG_VALDEL = $8C;   // Delete configuration items

{ ══ Layer CFG-VALSET ════════════════════════════════════════════════════════
  Nomi che esprimono lo scopo, non solo il valore del bit.
  Ref: Interface Description §3.10.25, payload byte 1 "layers".         }

  { Applica solo alla RAM — persa al riavvio.
    Uso normale durante lo sviluppo e il funzionamento operativo. }
  VALSET_LAYER_RAM       = $01;  // bit0

  { Applica solo alla Battery-Backed RAM — sopravvive al riavvio
    se la batteria è presente. }
  VALSET_LAYER_BBR       = $02;  // bit1

  { Applica solo alla Flash — permanente ma consuma cicli (~10.000).
    Usare solo per configurazioni definitive e verificate. }
  VALSET_LAYER_FLASH     = $04;  // bit2

  { Applica a RAM e Flash in un solo messaggio: la config entra in
    vigore subito (RAM) ed è resa permanente (Flash) contestualmente.
    Equivalente a Configure + SaveConfig in un unico round-trip. }
  VALSET_LAYER_RAM_FLASH = $05;  // bit0 | bit2

  { Applica a tutti i layer non-volatili disponibili (BBR + Flash).
    Uso: salvataggio massimo senza ri-applicare alla RAM corrente. }
  VALSET_LAYER_PERSIST   = $06;  // bit1 | bit2

{ ══ Dimensione fissa header payload CFG-VALSET ══════════════════════════════
  version(1) + layers(1) + reserved0(2) = 4 byte.                      }
  VALSET_HDR_SIZE = 4;

{ ══ Funzioni di supporto protocollo UBX ════════════════════════════════════ }

{ Calcola il checksum Fletcher-8 sui byte Msg[Start .. Start+Len-1].
  Restituisce CK_A nel byte basso, CK_B nel byte alto. }
function UBXChecksum(const Msg: TBytes; Start, Len: Integer): Word;

{ Costruisce un frame UBX completo: [SYNC1][SYNC2][Class][ID][LenL][LenH]
  [Payload...][CK_A][CK_B].  Payload può essere vuoto (lunghezza zero). }
function BuildUBX(ClassID, MsgID: Byte;
                  const Payload: array of Byte): TBytes;

{ Verifica il checksum di un frame UBX già letto in Buf.
  Offset = posizione del byte SYNC1 nel buffer.
  PayLen = lunghezza del payload (letta dall'header).
  Restituisce True se il checksum è valido. }
function ValidUBXChecksum(const Buf: array of Byte;
                          Offset, PayLen: Integer): Boolean;

{ Attende un ACK o NAK per il comando (ExpClass, ExpID) leggendo da Serial.
  Restituisce True se ACK ricevuto entro TimeoutMs millisecondi.
  Restituisce False su NAK o timeout.
  Scarta silenziosamente ACK riferiti ad altri comandi. }
function WaitACK(Serial: TBlockSerial;
                 ExpClass, ExpID: Byte;
                 TimeoutMs: Integer = 300): Boolean;

{ ══ Helper encoding CFG-VALSET key-value ════════════════════════════════════
  Ogni coppia è: [KeyID 4 byte LE] + [valore N byte LE].
  Buf deve essere pre-allocato con dimensione sufficiente.
  Pos viene avanzato del numero di byte scritti.

  Tipi u-blox:
    L  (bool/flag) → AddBool  → 1 byte  (0x00 / 0x01)
    U1 / X1 / E1   → AddU1   → 1 byte
    U2 / X2        → AddU2   → 2 byte LE
    U4 / X4        → AddU4   → 4 byte LE                                }

procedure AddBool(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Boolean);

procedure AddU1(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Byte);

procedure AddU2(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Word);

procedure AddU4(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: UInt32);

implementation

{ ══ UBXChecksum ═════════════════════════════════════════════════════════════ }

function UBXChecksum(const Msg: TBytes; Start, Len: Integer): Word;
var
  CK_A, CK_B: Byte;
  i: Integer;
begin
  CK_A := 0; CK_B := 0;
  for i := Start to Start + Len - 1 do
  begin
    CK_A := (CK_A + Msg[i]) and $FF;
    CK_B := (CK_B + CK_A)  and $FF;
  end;
  Result := CK_A or (Word(CK_B) shl 8);
end;

{ ══ BuildUBX ════════════════════════════════════════════════════════════════ }

function BuildUBX(ClassID, MsgID: Byte;
                  const Payload: array of Byte): TBytes;
var
  PayLen : Integer;
  CS     : Word;
  i      : Integer;
begin
  PayLen := Length(Payload);
  SetLength(Result, UBX_HDR_SIZE + PayLen + UBX_CRC_SIZE);
  Result[0] := UBX_SYNC1;
  Result[1] := UBX_SYNC2;
  Result[2] := ClassID;
  Result[3] := MsgID;
  Result[4] :=  PayLen        and $FF;
  Result[5] := (PayLen shr 8) and $FF;
  for i := 0 to PayLen - 1 do
    Result[6 + i] := Payload[i];
  CS := UBXChecksum(Result, 2, 4 + PayLen);
  Result[6 + PayLen]     :=  CS        and $FF;
  Result[6 + PayLen + 1] := (CS shr 8) and $FF;
end;

{ ══ ValidUBXChecksum ════════════════════════════════════════════════════════ }

function ValidUBXChecksum(const Buf: array of Byte;
                           Offset, PayLen: Integer): Boolean;
var
  CK_A, CK_B: Byte;
  i: Integer;
begin
  CK_A := 0; CK_B := 0;
  { Il checksum copre da Class (Offset+2) fino alla fine del payload }
  for i := Offset + 2 to Offset + UBX_HDR_SIZE + PayLen - 1 do
  begin
    CK_A := (CK_A + Buf[i]) and $FF;
    CK_B := (CK_B + CK_A)  and $FF;
  end;
  Result := (CK_A = Buf[Offset + UBX_HDR_SIZE + PayLen]) and
            (CK_B = Buf[Offset + UBX_HDR_SIZE + PayLen + 1]);
end;

{ ══ WaitACK ═════════════════════════════════════════════════════════════════ }

function WaitACK(Serial: TBlockSerial;
                 ExpClass, ExpID: Byte;
                 TimeoutMs: Integer): Boolean;
{
  Parser minimale UBX per rilevare ACK-ACK o ACK-NAK.
  Macchina a stati sul singolo byte:
    0 = attesa SYNC1 ($B5)
    1 = attesa SYNC2 ($62)
    2 = attesa Class  → deve essere $05 (ACK)
    3 = attesa ID     → $01=ACK-ACK | $00=ACK-NAK
    4 = attesa LEN_LO (fisso $02, non verificato)
    5 = attesa LEN_HI (fisso $00, non verificato)
    6 = attesa clsID  del messaggio originale
    7 = attesa msgID  del messaggio originale → confronto e uscita

  ACK riferiti ad altri comandi vengono scartati (State torna a 0).
}
var
  Deadline  : QWord; // TDateTime;
  B         : Byte;
  State     : Integer;
  GotClass  : Byte;
  IsACK     : Boolean;


begin
  Result   := False;
  IsACK    := False;
  GotClass := 0;
  State    := 0;
//  Deadline := Now + TimeoutMs / 86400000.0;
  Deadline := GetTickCount64 + TimeoutMs; // / 86400000.0;

//  while Now < Deadline do
  while GetTickCount64 < Deadline do
  begin
    if Serial.WaitingData > 0 then
    begin
      B := Serial.RecvByte(50);
      if Serial.LastError <> 0 then Break;

      case State of
        0: if B = UBX_SYNC1 then State := 1;
        1: if B = UBX_SYNC2 then State := 2 else State := 0;
        2: if B = UBX_CLASS_ACK then State := 3 else State := 0;
        3: begin
             if      B = UBX_ID_ACK_ACK then IsACK := True
             else if B = UBX_ID_ACK_NAK then IsACK := False
             else begin State := 0; Continue; end;
             State := 4;
           end;
        4: State := 5;   // LEN_LO — fisso $02, non verificato
        5: State := 6;   // LEN_HI — fisso $00, non verificato
        6: begin GotClass := B; State := 7; end;
        7: begin
             if (GotClass = ExpClass) and (B = ExpID) then
             begin
               Result := IsACK;
               Exit;
             end;
             State := 0;  // ACK per altro comando: ricomincia
           end;
      end;
    end
    else
      Sleep(5);
  end;
end;

{ ══ Helper encoding key-value ═══════════════════════════════════════════════ }

procedure AddBool(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Boolean);
begin
  Buf[Pos]   :=  KeyID         and $FF;
  Buf[Pos+1] := (KeyID shr  8) and $FF;
  Buf[Pos+2] := (KeyID shr 16) and $FF;
  Buf[Pos+3] := (KeyID shr 24) and $FF;
  Buf[Pos+4] := Byte(Value);
  Inc(Pos, 5);
end;

procedure AddU1(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Byte);
begin
  Buf[Pos]   :=  KeyID         and $FF;
  Buf[Pos+1] := (KeyID shr  8) and $FF;
  Buf[Pos+2] := (KeyID shr 16) and $FF;
  Buf[Pos+3] := (KeyID shr 24) and $FF;
  Buf[Pos+4] := Value;
  Inc(Pos, 5);
end;

procedure AddU2(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: Word);
begin
  Buf[Pos]   :=  KeyID         and $FF;
  Buf[Pos+1] := (KeyID shr  8) and $FF;
  Buf[Pos+2] := (KeyID shr 16) and $FF;
  Buf[Pos+3] := (KeyID shr 24) and $FF;
  Buf[Pos+4] :=  Value         and $FF;
  Buf[Pos+5] := (Value shr 8)  and $FF;
  Inc(Pos, 6);
end;

procedure AddU4(var Buf: TBytes; var Pos: Integer; KeyID: UInt32; Value: UInt32);
begin
  Buf[Pos]   :=  KeyID         and $FF;
  Buf[Pos+1] := (KeyID shr  8) and $FF;
  Buf[Pos+2] := (KeyID shr 16) and $FF;
  Buf[Pos+3] := (KeyID shr 24) and $FF;
  Buf[Pos+4] :=  Value         and $FF;
  Buf[Pos+5] := (Value shr  8) and $FF;
  Buf[Pos+6] := (Value shr 16) and $FF;
  Buf[Pos+7] := (Value shr 24) and $FF;
  Inc(Pos, 8);
end;

end.
