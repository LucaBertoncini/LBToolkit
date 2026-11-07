unit uRemoteConnectionData;

interface

uses
  Classes, SysUtils, Laz2_DOM;

type
  TConnectionProtocol = (cpHTTP, cpWebSocket);

  { TRemoteConnectionData }

  TRemoteConnectionData = class(TObject)
  strict private
    const
      cRemoteHostNodeName        = 'RemoteHost';
      cRemotePortNodeName        = 'RemotePort';
      cUseSSLNodeName            = 'UseSSL';
      cVerifyCertificateNodeName = 'VerifyCertificate';

  strict protected
    FHost    : String;
    FPort    : Integer;
    FUseSSL  : Boolean;
    FVerifyCert : Boolean;

    function GetHasValidData: Boolean;

  public
    constructor Create; virtual;

    procedure Clear();

    function LoadFromXMLNode(aParentNode: TDOMNode): Boolean; virtual;
    function SaveIntoXMLNode(aDocument: TXMLDocument; aParentNode: TDOMNode): Boolean; virtual;

    function CopyFrom(aRemoteConnection: TRemoteConnectionData): Boolean;

    function GetFullProtocol(aProtocol: TConnectionProtocol): String;
    function GetLocation(aProtocol: TConnectionProtocol): String;

    class function GetClassDescription: String; virtual;

    property Host         : String  read FHost       write FHost;
    property Port         : Integer read FPort       write FPort;
    property UseSSL       : Boolean read FUseSSL     write FUseSSL;
    property VerifyCert   : Boolean read FVerifyCert write FVerifyCert;
    property HasValidData : Boolean read GetHasValidData;
  end;

implementation

uses
  ULBLogger, Laz2_XMLRead;

{ TRemoteConnectionData }

constructor TRemoteConnectionData.Create;
begin
  inherited Create;

  Self.Clear();
end;

procedure TRemoteConnectionData.Clear();
begin
  FHost       := '';
  FPort       := 0;
  FUseSSL     := False;
  FVerifyCert := False;
end;

function TRemoteConnectionData.GetHasValidData: Boolean;
begin
  Result := (FHost <> '') and (FPort > 0);
end;

function TRemoteConnectionData.LoadFromXMLNode(aParentNode: TDOMNode): Boolean;
var
  _Node : TDOMNode;

begin
  Result := False;
  Self.Clear();

  if Assigned(aParentNode) then
  begin
    _Node := aParentNode.FindNode(cRemoteHostNodeName);
    if _Node <> nil then
      FHost := _Node.TextContent;

    _Node := aParentNode.FindNode(cRemotePortNodeName);
    if _Node <> nil then
      FPort := StrToIntDef(_Node.TextContent, 0);

    _Node := aParentNode.FindNode(cUseSSLNodeName);
    if _Node <> nil then
      FUseSSL := _Node.TextContent = '1';

    _Node := aParentNode.FindNode(cVerifyCertificateNodeName);
    if _Node <> nil then
      FVerifyCert := _Node.TextContent = '1';

    Result := Self.HasValidData;
  end
  else
    LBLogger.Write(1, 'TRemoteConnectionData.LoadFromXMLNode', lmt_Warning, 'XML node not set');

end;

function TRemoteConnectionData.SaveIntoXMLNode(aDocument: TXMLDocument; aParentNode: TDOMNode): Boolean;
var
  _Node: TDOMNode;
begin
  Result := False;

  if (aDocument <> nil) and (aParentNode <> nil) then
  begin

    _Node := aDocument.CreateElement(cRemoteHostNodeName);
    _Node.TextContent := FHost;
    aParentNode.AppendChild(_Node);

    _Node := aDocument.CreateElement(cRemotePortNodeName);
    _Node.TextContent := IntToStr(FPort);
    aParentNode.AppendChild(_Node);

    _Node := aDocument.CreateElement(cUseSSLNodeName);
    if FUseSSL then
      _Node.TextContent := '1'
    else
      _Node.TextContent := '0';
    aParentNode.AppendChild(_Node);

    _Node := aDocument.CreateElement(cVerifyCertificateNodeName);
    if FVerifyCert then
      _Node.TextContent := '1'
    else
      _Node.TextContent := '0';
    aParentNode.AppendChild(_Node);


    Result := True;
  end
  else
    LBLogger.Write(1, 'TRemoteConnectionData.SaveIntoXMLNode', lmt_Warning, 'XML document or node not set');

end;

function TRemoteConnectionData.CopyFrom(aRemoteConnection: TRemoteConnectionData): Boolean;
begin
  Result := False;
  if (aRemoteConnection <> nil) then
  begin

    FHost       := aRemoteConnection.Host;
    FPort       := aRemoteConnection.Port;
    FUseSSL     := aRemoteConnection.UseSSL;
    FVerifyCert := aRemoteConnection.VerifyCert;

  end;
end;

function TRemoteConnectionData.GetFullProtocol(aProtocol: TConnectionProtocol): String;
begin
  case aProtocol of
    cpHTTP      : if FUseSSL then Result := 'https' else Result := 'http';
    cpWebSocket : if FUseSSL then Result := 'wss'   else Result := 'ws';
  end;
end;

function TRemoteConnectionData.GetLocation(aProtocol: TConnectionProtocol): String;
begin
  Result := Format('%s://%s:%d/', [GetFullProtocol(aProtocol), FHost, FPort]);
end;

class function TRemoteConnectionData.GetClassDescription: String;
begin
  Result := 'RemoteConnectionData';
end;

end.

