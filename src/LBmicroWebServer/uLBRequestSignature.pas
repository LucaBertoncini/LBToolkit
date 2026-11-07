unit uLBRequestSignature;

{
  Firma delle richieste (HMAC-SHA256 con chiave condivisa): parte PURA, senza
  rete, condivisa da chi firma (uLBSignedProxy, uLBWebSocketClient) e da chi
  verifica (uLBSignedRequestVerifier, distributori WebSocket, ...).

  Spostata da uLBSignedProxy senza cambiarne il comportamento. In piu':
  LBBuildSignatureHeaders, che produce in un colpo solo i 9 header X-LB-* che
  prima SendSigned scriveva a mano.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

const
  LB_SIGNATURE_VERSION = '1';
  LB_HEADER_VERSION     = 'X-LB-Signature-Version';
  LB_HEADER_TIMESTAMP   = 'X-LB-Timestamp';
  LB_HEADER_NONCE       = 'X-LB-Nonce';
  LB_HEADER_SIGNATURE   = 'X-LB-Signature';
  LB_HEADER_USER_ID     = 'X-LB-User-Id';
  LB_HEADER_ROLE_ID     = 'X-LB-Role-Id';
  LB_HEADER_IS_ADMIN    = 'X-LB-Is-Admin';
  LB_HEADER_CORRELATION = 'X-LB-Correlation-Id';
  LB_HEADER_BODY_SHA256 = 'X-LB-Body-SHA256';

type
  TLBSignatureContext = record
    UserId        : Int64;
    RoleId        : Int64;
    IsAdmin       : Boolean;
    CorrelationId : String;
  end;

procedure ClearSignatureContext(out aContext: TLBSignatureContext);
function LBUtcUnixTime: Int64;
function LBNewNonce: String;
function LBNormalizeURI(const aURI: String): String;
function LBSHA256Hex(const aText: RawByteString): String;
function LBHMACSHA256Hex(const aKey, aData: RawByteString): String;
function LBBuildCanonicalRequest(const aMethod, aURI, aBody: String; aTimestamp: Int64; const aNonce: String; const aContext: TLBSignatureContext): String;
function LBConstantTimeSameText(const aLeft, aRight: String): Boolean;
{ Come LBConstantTimeSameText ma DISTINGUE maiuscole e minuscole: per valori base64/base64url
  (es. la sfida PKCE), dove 'a' e 'A' sono simboli diversi. }
function LBConstantTimeSameStr(const aLeft, aRight: String): Boolean;

{ Aggiunge a aHeaders (es. THTTPSend.Headers) i 9 header di firma, con timestamp
  e nonce NUOVI a ogni chiamata. aBody e' il corpo della richiesta ('' se assente). }
procedure LBBuildSignatureHeaders(aHeaders: TStrings; const aMethod, aURI, aBody, aSharedKey: String; const aContext: TLBSignatureContext);

implementation

type
  TSHA256State = record
    H: array[0..7] of LongWord;
    Buffer: array[0..63] of Byte;
    BufferLen: Integer;
    TotalLen: QWord;
  end;

const
  K256: array[0..63] of LongWord = (
    $428A2F98,$71374491,$B5C0FBCF,$E9B5DBA5,$3956C25B,$59F111F1,$923F82A4,$AB1C5ED5,
    $D807AA98,$12835B01,$243185BE,$550C7DC3,$72BE5D74,$80DEB1FE,$9BDC06A7,$C19BF174,
    $E49B69C1,$EFBE4786,$0FC19DC6,$240CA1CC,$2DE92C6F,$4A7484AA,$5CB0A9DC,$76F988DA,
    $983E5152,$A831C66D,$B00327C8,$BF597FC7,$C6E00BF3,$D5A79147,$06CA6351,$14292967,
    $27B70A85,$2E1B2138,$4D2C6DFC,$53380D13,$650A7354,$766A0ABB,$81C2C92E,$92722C85,
    $A2BFE8A1,$A81A664B,$C24B8B70,$C76C51A3,$D192E819,$D6990624,$F40E3585,$106AA070,
    $19A4C116,$1E376C08,$2748774C,$34B0BCB5,$391C0CB3,$4ED8AA4A,$5B9CCA4F,$682E6FF3,
    $748F82EE,$78A5636F,$84C87814,$8CC70208,$90BEFFFA,$A4506CEB,$BEF9A3F7,$C67178F2);

function ROR32(x: LongWord; n: Byte): LongWord; inline;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

procedure SHA256Transform(var S: TSHA256State; const Block);
var
  W: array[0..63] of LongWord;
  A,B,C,D,E,F,G,H,T1,T2,S0,S1,Ch,Maj: LongWord;
  P: PByte;
  I: Integer;
begin
  P := @Block;
  for I := 0 to 15 do
    W[I] := (LongWord(P[I*4]) shl 24) or (LongWord(P[I*4+1]) shl 16) or
            (LongWord(P[I*4+2]) shl 8) or LongWord(P[I*4+3]);
  for I := 16 to 63 do
  begin
    S0 := ROR32(W[I-15],7) xor ROR32(W[I-15],18) xor (W[I-15] shr 3);
    S1 := ROR32(W[I-2],17) xor ROR32(W[I-2],19) xor (W[I-2] shr 10);
    W[I] := W[I-16] + S0 + W[I-7] + S1;
  end;
  A:=S.H[0]; B:=S.H[1]; C:=S.H[2]; D:=S.H[3];
  E:=S.H[4]; F:=S.H[5]; G:=S.H[6]; H:=S.H[7];
  for I := 0 to 63 do
  begin
    S1 := ROR32(E,6) xor ROR32(E,11) xor ROR32(E,25);
    Ch := (E and F) xor ((not E) and G);
    T1 := H + S1 + Ch + K256[I] + W[I];
    S0 := ROR32(A,2) xor ROR32(A,13) xor ROR32(A,22);
    Maj := (A and B) xor (A and C) xor (B and C);
    T2 := S0 + Maj;
    H:=G; G:=F; F:=E; E:=D+T1; D:=C; C:=B; B:=A; A:=T1+T2;
  end;
  Inc(S.H[0],A); Inc(S.H[1],B); Inc(S.H[2],C); Inc(S.H[3],D);
  Inc(S.H[4],E); Inc(S.H[5],F); Inc(S.H[6],G); Inc(S.H[7],H);
end;

procedure SHA256Init(var S: TSHA256State);
begin
  FillChar(S, SizeOf(S), 0);
  S.H[0]:=$6A09E667; S.H[1]:=$BB67AE85; S.H[2]:=$3C6EF372; S.H[3]:=$A54FF53A;
  S.H[4]:=$510E527F; S.H[5]:=$9B05688C; S.H[6]:=$1F83D9AB; S.H[7]:=$5BE0CD19;
end;

procedure SHA256Update(var S: TSHA256State; const Data; Len: SizeInt);
var
  P: PByte;
  N: Integer;
begin
  if Len <= 0 then Exit;
  P := @Data;
  Inc(S.TotalLen, QWord(Len));
  while Len > 0 do
  begin
    N := 64 - S.BufferLen;
    if N > Len then N := Len;
    Move(P^, S.Buffer[S.BufferLen], N);
    Inc(S.BufferLen, N); Inc(P, N); Dec(Len, N);
    if S.BufferLen = 64 then
    begin
      SHA256Transform(S, S.Buffer);
      S.BufferLen := 0;
    end;
  end;
end;

function SHA256FinalBytes(var S: TSHA256State): RawByteString;
var
  BitLen: QWord;
  I: Integer;
  B: Byte;
begin
  BitLen := S.TotalLen * 8;
  B := $80; SHA256Update(S, B, 1);
  B := 0;
  while S.BufferLen <> 56 do SHA256Update(S, B, 1);
  for I := 7 downto 0 do
  begin
    B := Byte(BitLen shr (I*8));
    SHA256Update(S, B, 1);
  end;
  SetLength(Result, 32);
  for I := 0 to 7 do
  begin
    Result[I*4+1] := AnsiChar(S.H[I] shr 24);
    Result[I*4+2] := AnsiChar(S.H[I] shr 16);
    Result[I*4+3] := AnsiChar(S.H[I] shr 8);
    Result[I*4+4] := AnsiChar(S.H[I]);
  end;
end;

function SHA256Bytes(const Data: RawByteString): RawByteString;
var S: TSHA256State;
begin
  SHA256Init(S);
  if Length(Data) > 0 then SHA256Update(S, Data[1], Length(Data));
  Result := SHA256FinalBytes(S);
end;

function BytesToHex(const Data: RawByteString): String;
const
  Hex: array[0..15] of Char = '0123456789abcdef';
var
  I,V: Integer;
begin
  SetLength(Result, Length(Data)*2);
  for I := 1 to Length(Data) do
  begin
    V := Ord(Data[I]);
    Result[I*2-1] := Hex[V shr 4];
    Result[I*2] := Hex[V and $0F];
  end;
end;

function LBSHA256Hex(const aText: RawByteString): String;
begin
  Result := BytesToHex(SHA256Bytes(aText));
end;

function LBHMACSHA256Hex(const aKey, aData: RawByteString): String;
var
  KeyBlock, InnerPad, OuterPad, Digest: RawByteString;
  I: Integer;
begin
  KeyBlock := aKey;
  if Length(KeyBlock) > 64 then KeyBlock := SHA256Bytes(KeyBlock);
  SetLength(KeyBlock, 64);
  SetLength(InnerPad, 64); SetLength(OuterPad, 64);
  for I := 1 to 64 do
  begin
    InnerPad[I] := AnsiChar(Ord(KeyBlock[I]) xor $36);
    OuterPad[I] := AnsiChar(Ord(KeyBlock[I]) xor $5C);
  end;
  Digest := SHA256Bytes(InnerPad + aData);
  Result := BytesToHex(SHA256Bytes(OuterPad + Digest));
end;

procedure ClearSignatureContext(out aContext: TLBSignatureContext);
begin
  FillChar(aContext, SizeOf(aContext), 0);
  aContext.CorrelationId := '';
end;

function LBUtcUnixTime: Int64;
begin
  Result := DateTimeToUnix(Now, False);
end;

function LBNewNonce: String;
var G: TGUID;
begin
  if CreateGUID(G) = 0 then
    Result := LowerCase(StringReplace(StringReplace(GUIDToString(G),'{','',[rfReplaceAll]),'}','',[rfReplaceAll]))
  else
    Result := IntToHex(QWord(GetTickCount64),16) + IntToHex(QWord(Random(MaxInt)),8);
end;

function LBNormalizeURI(const aURI: String): String;
var P: SizeInt;
begin
  Result := Trim(aURI);
  P := Pos('://', Result);
  if P > 0 then
  begin
    Delete(Result, 1, P+2);
    P := Pos('/', Result);
    if P > 0 then Result := Copy(Result, P, MaxInt) else Result := '/';
  end;
  if Result = '' then Result := '/';
  if Result[1] <> '/' then Result := '/' + Result;
end;

function BoolCanonical(aValue: Boolean): String;
begin
  if aValue then Result := '1' else Result := '0';
end;

function LBBuildCanonicalRequest(const aMethod, aURI, aBody: String;
  aTimestamp: Int64; const aNonce: String;
  const aContext: TLBSignatureContext): String;
begin
  Result := LB_SIGNATURE_VERSION + #10 +
    UpperCase(Trim(aMethod)) + #10 +
    LBNormalizeURI(aURI) + #10 +
    IntToStr(aTimestamp) + #10 +
    Trim(aNonce) + #10 +
    LBSHA256Hex(RawByteString(aBody)) + #10 +
    IntToStr(aContext.UserId) + #10 +
    IntToStr(aContext.RoleId) + #10 +
    BoolCanonical(aContext.IsAdmin) + #10 +
    Trim(aContext.CorrelationId);
end;

function LBConstantTimeSameText(const aLeft, aRight: String): Boolean;
var I, Diff, L1, L2, LMax: Integer; C1,C2: Byte;
begin
  L1:=Length(aLeft); L2:=Length(aRight);
  if L1>L2 then LMax:=L1 else LMax:=L2;
  Diff := L1 xor L2;
  for I:=1 to LMax do
  begin
    if I<=L1 then C1:=Ord(UpCase(aLeft[I])) else C1:=0;
    if I<=L2 then C2:=Ord(UpCase(aRight[I])) else C2:=0;
    Diff := Diff or (C1 xor C2);
  end;
  Result := Diff=0;
end;

function LBConstantTimeSameStr(const aLeft, aRight: String): Boolean;
var I, Diff, L1, L2, LMax: Integer; C1,C2: Byte;
begin
  L1:=Length(aLeft); L2:=Length(aRight);
  if L1>L2 then LMax:=L1 else LMax:=L2;
  Diff := L1 xor L2;
  for I:=1 to LMax do
  begin
    if I<=L1 then C1:=Ord(aLeft[I]) else C1:=0;
    if I<=L2 then C2:=Ord(aRight[I]) else C2:=0;
    Diff := Diff or (C1 xor C2);
  end;
  Result := Diff=0;
end;

procedure LBBuildSignatureHeaders(aHeaders: TStrings; const aMethod, aURI, aBody,
  aSharedKey: String; const aContext: TLBSignatureContext);
var
  TS: Int64;
  Nonce, Canonical, Sig: String;
begin
  TS := LBUtcUnixTime;
  Nonce := LBNewNonce;
  Canonical := LBBuildCanonicalRequest(aMethod, aURI, aBody, TS, Nonce, aContext);
  Sig := LBHMACSHA256Hex(RawByteString(aSharedKey), RawByteString(Canonical));

  aHeaders.Add(LB_HEADER_VERSION + ': ' + LB_SIGNATURE_VERSION);
  aHeaders.Add(LB_HEADER_TIMESTAMP + ': ' + IntToStr(TS));
  aHeaders.Add(LB_HEADER_NONCE + ': ' + Nonce);
  aHeaders.Add(LB_HEADER_SIGNATURE + ': ' + Sig);
  aHeaders.Add(LB_HEADER_BODY_SHA256 + ': ' + LBSHA256Hex(RawByteString(aBody)));
  aHeaders.Add(LB_HEADER_USER_ID + ': ' + IntToStr(aContext.UserId));
  aHeaders.Add(LB_HEADER_ROLE_ID + ': ' + IntToStr(aContext.RoleId));
  aHeaders.Add(LB_HEADER_IS_ADMIN + ': ' + BoolCanonical(aContext.IsAdmin));
  aHeaders.Add(LB_HEADER_CORRELATION + ': ' + Trim(aContext.CorrelationId));
end;

end.
