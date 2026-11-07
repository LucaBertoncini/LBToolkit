unit testlocalipc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uIPCUtils
  {$IFDEF UNIX}, Unix{$ENDIF}
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  TTestLocalIPC = class(TTestCase)
  private
    FServer: TLocalServerSocket;
    FClient: TLocalSocket;
    FPeer: TLocalSocket;
    procedure StartServer;
    procedure ConnectClient;
  published
    procedure TestStringExchange;
    procedure TestBinaryExchange;
  end;

implementation

uses
  ULBLogger;

const
  IPCPath =
    {$IFDEF UNIX} '/tmp/test_ipc.sock' {$ENDIF}
    {$IFDEF WINDOWS} '\\.\pipe\TestLocalIPC' {$ENDIF};

type

  { TDelayTimer }

  TDelayTimer = class(TThread)
    strict private
      FClient : TLocalSocket;
      FOwner : TTestCase;

    protected
      procedure Execute; override;

    public
      constructor Create(aOwner: TTestCase); reintroduce;

      property Client: TLocalSocket write FClient;
  end;

{ TDelayTimer }

procedure TDelayTimer.Execute;
begin
  Self.Sleep(100);
  FOwner.AssertTrue('Client Connect failed', FClient.Connect(IPCPath));
end;

constructor TDelayTimer.Create(aOwner: TTestCase);
begin
  inherited Create(True);
  FOwner := aOwner;
  FreeOnTerminate := True;
end;

procedure TTestLocalIPC.StartServer;
begin
  FServer := TLocalServerSocket.Create(IPCPath);
  FServer.Timeout := 3000;
  AssertTrue('Server Listen failed', FServer.Listen);
end;

procedure TTestLocalIPC.ConnectClient;
var
  _Delay : TDelayTimer;

begin
  FClient := TLocalSocket.Create;
  FClient.Timeout := 3000;

  _Delay := TDelayTimer.Create(Self);
  _Delay.Client := FClient;
  _Delay.Start;

  FPeer := FServer.Accept;
  AssertNotNull('Server Accept failed', FPeer);
end;

procedure TTestLocalIPC.TestStringExchange;
var
  Sent, Received: string;
begin
  StartServer;
  ConnectClient;

  Sent := 'Test message';
  AssertTrue('SendString failed', FPeer.SendString(Sent));
  Received := FClient.RecvString;
  LBLogger.Write(5, 'TTestLocalIPC.TestStringExchange', lmt_Debug, 'Msg sent: <%s>  -  Received: <%s>', [Sent, Received]);
  AssertEquals('String mismatch', Sent, Received);

  FPeer.Free;
  FClient.Free;
  FServer.Free;
end;

procedure TTestLocalIPC.TestBinaryExchange;
var
  BufSent, BufRecv: array[0..3] of Byte;
  I: Integer;
begin
  StartServer;
  ConnectClient;

  for I := 0 to 3 do BufSent[I] := I * 11;

  AssertEquals('SendBuffer failed', SizeOf(BufSent),
               FPeer.SendBuffer(@BufSent, SizeOf(BufSent)));

  AssertEquals('RecvBuffer failed', SizeOf(BufRecv),
               FClient.RecvBuffer(@BufRecv, SizeOf(BufRecv)));

  for I := 0 to 3 do
    AssertEquals('Buffer mismatch at index ' + IntToStr(I),
                 BufSent[I], BufRecv[I]);

  FPeer.Free;
  FClient.Free;
  FServer.Free;
end;

initialization
  RegisterTest(TTestLocalIPC);
end.

