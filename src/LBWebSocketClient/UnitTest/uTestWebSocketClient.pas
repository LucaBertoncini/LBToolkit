unit uTestWebSocketClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uLBWebSocketClient;

type
  TWebSocketClientTest = class(TTestCase)
  private
    FClient: TLBWebSocketClient;
    FReceivedMessage: string;
    FConnected: Boolean;
    FDisconnected: Boolean;
    procedure OnTextMessage(Sender: TObject; const Msg: string);
    procedure OnConnected(Sender: TObject);
    procedure OnDisconnected(Sender: TObject);
    procedure RunEchoTest(const AHost, AURI: string; AUseSSL: Boolean; const ExpectedEcho: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPostmanEcho;
    procedure TestPieSocket;
    procedure TestWebSocketEvents;
  end;

implementation

uses
  ULBLogger;

procedure TWebSocketClientTest.SetUp;
begin
  FReceivedMessage := '';
  FConnected := False;
  FDisconnected := False;
end;

procedure TWebSocketClientTest.TearDown;
begin
end;

procedure TWebSocketClientTest.OnConnected(Sender: TObject);
begin
  FConnected := True;
  LBLogger.Write(5, 'TWebSocketClientTest.OnConnected', lmt_Debug, 'Connected');
end;

procedure TWebSocketClientTest.OnDisconnected(Sender: TObject);
begin
  FDisconnected := True;
  LBLogger.Write(5, 'TWebSocketClientTest.OnDisconnected', lmt_Debug, 'Disconnected from server');
end;

procedure TWebSocketClientTest.OnTextMessage(Sender: TObject; const Msg: string);
begin
  FReceivedMessage := Msg;
  LBLogger.Write(5, 'TWebSocketClientTest.OnTextMessage', lmt_Debug, 'Received message: <%s>', [Msg]);
end;

procedure TWebSocketClientTest.RunEchoTest(const AHost, AURI: string; AUseSSL: Boolean; const ExpectedEcho: string);
begin
  LBLogger.Write(5, 'TWebSocketClientTest.RunEchoTest', lmt_Debug, 'Starting test for host: <%s>', [AHost]);

  FClient := TLBWebSocketClient.Create();
  FClient.RemoteConnectionData.Host := AHost;

  if AUseSSL then
    FClient.RemoteConnectionData.Port := 443
  else
    FClient.RemoteConnectionData.Port := 80;

  FClient.RemoteConnectionData.UseSSL := AUseSSL;
  FClient.URI := AURI;

  FClient.OnWebSocketTextMessage := @OnTextMessage;
  FClient.OnConnected := @OnConnected;
  FClient.OnDisconnected := @OnDisconnected;

  FClient.Start;
  Sleep(2000);
  AssertTrue('Client should be connected to ' + AHost, FConnected);

  FClient.AddWebSocketMessageToSend(ExpectedEcho);
  LBLogger.Write(5, 'TWebSocketClientTest.RunEchoTest', lmt_Debug, 'Sent message: <%s>', [ExpectedEcho]);
  Sleep(3000);

  if AHost = 'demo.piesocket.com' then
    AssertEquals('Should receive error', '{"error":"Unknown api key"}', FReceivedMessage)
  else
    AssertEquals('Echoed message should match', ExpectedEcho, FReceivedMessage);

  FreeAndNil(FClient);
  Sleep(1000);
  AssertTrue('Client should be disconnected', FDisconnected);
end;

procedure TWebSocketClientTest.TestPostmanEcho;
begin
  RunEchoTest('ws.postman-echo.com', '/raw', True, 'Hello from Postman');
end;

procedure TWebSocketClientTest.TestPieSocket;
begin
  RunEchoTest('demo.piesocket.com', '/v3/channel_1?api_key=demo', True, 'Hello from PieSocket');
end;

procedure TWebSocketClientTest.TestWebSocketEvents;
begin
  RunEchoTest('echo.websocket.events', '/', True, 'Hello from Kaazing');
end;

initialization
  RegisterTest(TWebSocketClientTest);
end.

