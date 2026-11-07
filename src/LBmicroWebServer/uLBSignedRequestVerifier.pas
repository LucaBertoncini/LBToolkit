unit uLBSignedRequestVerifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLBRequestSignature;

type
  TLBNonceCheckEvent = function(const aNonce: String; aTimestamp: Int64): Boolean of object;

  TLBVerifyFailure = (
    lvfNone, lvfMissingHeader, lvfUnsupportedVersion, lvfInvalidTimestamp,
    lvfExpired, lvfBodyHashMismatch, lvfReplay, lvfSignatureMismatch,
    lvfInvalidContext
  );

  TLBVerificationResult = record
    Valid        : Boolean;
    Failure      : TLBVerifyFailure;
    ErrorMessage : String;
    Context      : TLBSignatureContext;
    Timestamp    : Int64;
    Nonce        : String;
  end;

  TLBSignedRequestVerifier = class
  private
    FMaxClockSkewSeconds: Integer;
    FOnAcceptNonce: TLBNonceCheckEvent;
    function HeaderValue(aHeaders: TStrings; const aName: String): String;
    procedure Fail(var R: TLBVerificationResult; aFailure: TLBVerifyFailure;
      const aMessage: String);
  public
    constructor Create;
    function Verify(aHeaders: TStrings; const aMethod, aURI, aBody,
      aSharedKey: String): TLBVerificationResult;
    property MaxClockSkewSeconds: Integer read FMaxClockSkewSeconds
      write FMaxClockSkewSeconds;
    property OnAcceptNonce: TLBNonceCheckEvent read FOnAcceptNonce write FOnAcceptNonce;
  end;

implementation

constructor TLBSignedRequestVerifier.Create;
begin
  inherited Create;
  FMaxClockSkewSeconds := 300;
  FOnAcceptNonce := nil;
end;

function TLBSignedRequestVerifier.HeaderValue(aHeaders: TStrings;
  const aName: String): String;
var I,P:Integer; S,N:String;
begin
  Result:='';
  if aHeaders=nil then Exit;
  Result:=Trim(aHeaders.Values[aName]);
  if Result<>'' then Exit;
  for I:=0 to aHeaders.Count-1 do
  begin
    S:=aHeaders[I]; P:=Pos(':',S);
    if P<=0 then Continue;
    N:=Trim(Copy(S,1,P-1));
    if SameText(N,aName) then Exit(Trim(Copy(S,P+1,MaxInt)));
  end;
end;

procedure TLBSignedRequestVerifier.Fail(var R: TLBVerificationResult;
  aFailure: TLBVerifyFailure; const aMessage: String);
begin
  R.Valid:=False; R.Failure:=aFailure; R.ErrorMessage:=aMessage;
end;

function ParseBool01(const S:String; out V:Boolean):Boolean;
begin
  Result:=True;
  if S='1' then V:=True else if S='0' then V:=False else Result:=False;
end;

function TLBSignedRequestVerifier.Verify(aHeaders: TStrings;
  const aMethod, aURI, aBody, aSharedKey: String): TLBVerificationResult;
var
  Version,TimestampText,Nonce,Signature,BodyHash,ExpectedBodyHash:String;
  UserText,RoleText,AdminText,Corr,Canonical,ExpectedSignature:String;
  NowTS,Delta:Int64; AdminValue:Boolean;
begin
  FillChar(Result,SizeOf(Result),0);
  ClearSignatureContext(Result.Context);
  Result.Failure:=lvfNone;
  Version:=HeaderValue(aHeaders,LB_HEADER_VERSION);
  TimestampText:=HeaderValue(aHeaders,LB_HEADER_TIMESTAMP);
  Nonce:=HeaderValue(aHeaders,LB_HEADER_NONCE);
  Signature:=HeaderValue(aHeaders,LB_HEADER_SIGNATURE);
  BodyHash:=HeaderValue(aHeaders,LB_HEADER_BODY_SHA256);
  UserText:=HeaderValue(aHeaders,LB_HEADER_USER_ID);
  RoleText:=HeaderValue(aHeaders,LB_HEADER_ROLE_ID);
  AdminText:=HeaderValue(aHeaders,LB_HEADER_IS_ADMIN);
  Corr:=HeaderValue(aHeaders,LB_HEADER_CORRELATION);
  if (Version='') or (TimestampText='') or (Nonce='') or (Signature='') or
     (BodyHash='') or (UserText='') or (RoleText='') or (AdminText='') then
  begin Fail(Result,lvfMissingHeader,'Mancano uno o piu'' header di firma'); Exit; end;
  if Version<>LB_SIGNATURE_VERSION then
  begin Fail(Result,lvfUnsupportedVersion,'Versione della firma non supportata'); Exit; end;
  if (not TryStrToInt64(TimestampText,Result.Timestamp)) or (Result.Timestamp<=0) then
  begin Fail(Result,lvfInvalidTimestamp,'Timestamp non valido'); Exit; end;
  NowTS:=LBUtcUnixTime; Delta:=NowTS-Result.Timestamp; if Delta<0 then Delta:=-Delta;
  if (FMaxClockSkewSeconds>=0) and (Delta>FMaxClockSkewSeconds) then
  begin Fail(Result,lvfExpired,'Richiesta scaduta o orologi non sincronizzati'); Exit; end;
  if (not TryStrToInt64(UserText,Result.Context.UserId)) or
     (not TryStrToInt64(RoleText,Result.Context.RoleId)) or
     (not ParseBool01(AdminText,AdminValue)) then
  begin Fail(Result,lvfInvalidContext,'Contesto applicativo non valido'); Exit; end;
  Result.Context.IsAdmin:=AdminValue; Result.Context.CorrelationId:=Corr; Result.Nonce:=Nonce;
  ExpectedBodyHash:=LBSHA256Hex(RawByteString(aBody));
  if not LBConstantTimeSameText(BodyHash,ExpectedBodyHash) then
  begin Fail(Result,lvfBodyHashMismatch,'Hash del corpo non corrispondente'); Exit; end;
  Canonical:=LBBuildCanonicalRequest(aMethod,aURI,aBody,Result.Timestamp,Nonce,Result.Context);
  ExpectedSignature:=LBHMACSHA256Hex(RawByteString(aSharedKey),RawByteString(Canonical));
  if not LBConstantTimeSameText(Signature,ExpectedSignature) then
  begin Fail(Result,lvfSignatureMismatch,'Firma non valida'); Exit; end;
  // Il nonce viene registrato solo dopo che la firma e' valida: richieste
  // arbitrarie non possono saturare inutilmente il replay store.
  if Assigned(FOnAcceptNonce) and (not FOnAcceptNonce(Nonce,Result.Timestamp)) then
  begin Fail(Result,lvfReplay,'Nonce gia'' utilizzato'); Exit; end;
  Result.Valid:=True; Result.Failure:=lvfNone; Result.ErrorMessage:='';
end;

end.
