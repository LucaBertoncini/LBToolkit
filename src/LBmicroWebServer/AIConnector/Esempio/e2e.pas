program e2e;
{$mode objfpc}{$H+}
{$codepage utf8}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson, jsonparser, jsonscanner,
  uLBmicroWebServer, uWebRouteRegistry, uTokenManager, uLBAIConnectorTypes, uLBAIConnector, uLBOAuthProvider;

type
  TTestModule = class(TWebRouteModule)
  protected
    function hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean; override;
  end;

  TDemoHandler = class(TRouteHandlerBase, IAIConnectorFunctions)
  private
    function DoList(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoCreate(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoKill(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoNoDesc(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
    function DoWeb(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
  public
    constructor Create;
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

  TGlue = class
    function Credentials(const aUsername, aPassword: String; out anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
    function Refresh(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
    function WebAuth(RequestHeaders, ResponseHeaders: TStringList; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;
  end;

function TTestModule.hasPermission(aUserConfig: TJSONObject; const anAreaCode, anOpType: String): Boolean;
begin
  Result := False;
  if aUserConfig = nil then Exit;
  if aUserConfig.Get('is_admin', False) then Exit(True);
  // role "reader": only demo.Lettura
  Result := (aUserConfig.Get('role', '') = 'reader') and (anAreaCode = 'demo') and ((anOpType = 'Lettura') or (anOpType = 'Uso AI'));
end;

constructor TDemoHandler.Create;
begin
  inherited Create('demo');
  RegisterStandardWorker('List', @DoList);
  RegisterStandardWorker('Create', @DoCreate);
  RegisterStandardWorker('Kill', @DoKill);
  RegisterStandardWorker('NoDesc', @DoNoDesc);
  RegisterStandardWorker('Web', @DoWeb);
end;

function TDemoHandler.DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
begin
  Result := True;
  if aFunctionName = 'List' then
  begin
    aDescriptor.Effect := aeReadOnly;
    aDescriptor.AddEnumParam('status', 'Stato degli elementi', 'open,closed');
    aDescriptor.AddParam('limit', aptInteger, 'Numero massimo di righe');
    aDescriptor.AddParam('from', aptDate, 'Dal giorno');
    aDescriptor.AddParam('at', aptDateTime, 'Istante');
    aDescriptor.AddArrayParam('ids', aptInteger, 'Elenco id');
  end
  else if aFunctionName = 'Create' then
  begin
    aDescriptor.Effect := aeModifies;
    aDescriptor.AddParam('title', aptString, 'Titolo', True);
    aDescriptor.AddParam('due', aptDate, 'Scadenza (YYYY-MM-DD)');
  end
  else if aFunctionName = 'Kill' then
  begin
    aDescriptor.Effect := aeDestructive;
    aDescriptor.AddParam('id', aptInteger, 'Id', True);
  end
  else
    Result := False;
end;

function TDemoHandler.DoList(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var a: TJSONArray; o: TJSONObject;
begin
  aStatusCode := 200;
  a := TJSONArray.Create;
  o := TJSONObject.Create; o.Add('id', 1); o.Add('title', 'Pompa è guasta'); o.Add('status', aRequestData.Get('status', 'open')); o.Add('user', aUserId); a.Add(o);
  Result := a;
end;
function TDemoHandler.DoCreate(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var o: TJSONObject;
begin
  aStatusCode := 201; o := TJSONObject.Create; o.Add('created', aRequestData.Get('title', '')); Result := o;
end;
function TDemoHandler.DoKill(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
var o: TJSONObject;
begin
  aStatusCode := 404; o := TJSONObject.Create; o.Add('error', 'Not found'); Result := o;
end;
function TDemoHandler.DoNoDesc(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
begin aStatusCode := 200; Result := TJSONObject.Create; end;
function TDemoHandler.DoWeb(aUserId: Integer; aUserConfig: TJSONObject; aRequestData: TJSONObject; out aStatusCode: Integer): TJSONData;
begin aStatusCode := 200; Result := TJSONObject.Create; TJSONObject(Result).Add('web', True); end;

function TGlue.Credentials(const aUsername, aPassword: String; out anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
begin
  Result := False; aUserConfig := nil; anUserId := 0;
  if (aUsername = 'mario') and (aPassword = 'pw1') then
  begin anUserId := 7; aUserConfig := TJSONObject.Create; aUserConfig.Add('role', 'reader'); Result := True; end
  else if (aUsername = 'admin') and (aPassword = 'pw2') then
  begin anUserId := 1; aUserConfig := TJSONObject.Create; aUserConfig.Add('is_admin', True); Result := True; end;
end;
function TGlue.Refresh(anUserId: Integer; out aUserConfig: TJSONObject): Boolean;
begin
  aUserConfig := nil; Result := anUserId <> 99;   // user 99 = "disabled"
  if Result then begin aUserConfig := TJSONObject.Create; aUserConfig.Add('role', 'reader'); end;
end;

var
  TMG: TTokenManager;
function TGlue.WebAuth(RequestHeaders, ResponseHeaders: TStringList; out anUserId: Integer; out anUserConfig: TJSONObject): Boolean;
begin
  Result := TMG.isValidRequest(RequestHeaders, ResponseHeaders, anUserId, anUserConfig);
end;

var
  WS: TLBmicroWebServer; Mod_: TTestModule; H: TDemoHandler; TM: TTokenManager; C: TLBAIConnector; O: TLBOAuthProvider;
  G: TGlue; TD: TJSONObject; N: Integer;
begin
  WS := TLBmicroWebServer.Create;
  WS.ListeningPort := 18125;
  TM := TTokenManager.Create;
  G := TGlue.Create;
  Mod_ := TTestModule.Create;
  WriteLn('LOAD=', Mod_.LoadRoutesFromXML('routes_demo.xml'));
  H := TDemoHandler.Create;
  Mod_.RegisterHandler(H);
  Mod_.ValidateRoutes; WriteLn('AIEP=', Mod_.Routes.AIEndpointCount);
  TMG := TM; Mod_.OnAuthenticateRequest := @G.WebAuth;

  C := TLBAIConnector.Create(Mod_);
  C.OnAuthenticateRequest := @TM.isValidBearerRequest;
  C.Instructions := 'Server di prova.';
  C.ExposeUndescribed := ParamStr(1) = 'all';
  O := TLBOAuthProvider.Create(TM);
  O.PublicBaseURL := 'http://127.0.0.1:18125';
  O.AllowedRedirectURIs.Add('https://claude.ai/api/mcp/auth_callback');
  O.OnAuthenticateCredentials := @G.Credentials;
  O.OnRefreshUser := @G.Refresh;
  C.OAuthProvider := O;
  begin
    for N := 0 to Mod_.Routes.AIEndpointCount - 1 do
      with Mod_.Routes.GetAIEndpoint(N) do
        WriteLn('  ep ', Descriptor.FunctionName, ' kindmatches=', KindMatches, ' handler=', Handler <> nil, ' supports=', Supports(Handler, IAIConnectorFunctions));
  end;
  N := C.Prepare();
  WriteLn('TOOLS=', N);

  // a fixed token for quick curl tests (user 7 = reader)
  TD := nil;
  TM.addNewScopedToken(7, TJSONObject(GetJSON('{"role":"reader"}')), cTokenScope_AIAccess, 0, 'test', TD);
  WriteLn('FIXEDTOKEN=', TD.Get('token', ''));
  TD := nil;
  TM.addNewScopedToken(1, TJSONObject(GetJSON('{"is_admin":true}')), cTokenScope_AIAccess, 0, 'test', TD);
  WriteLn('ADMINTOKEN=', TD.Get('token', ''));
  TD := nil;
  TM.addNewScopedToken(9, TJSONObject(GetJSON('{"role":"noai"}')), cTokenScope_AIAccess, 0, 'test', TD);
  WriteLn('NOAITOKEN=', TD.Get('token', ''));
  Flush(Output);

  WS.addChainProcessor(Mod_, True);
  WS.addChainProcessor(O, True);
  WS.addChainProcessor(C, True);
  WS.Activate;
  Sleep(600000);
end.
