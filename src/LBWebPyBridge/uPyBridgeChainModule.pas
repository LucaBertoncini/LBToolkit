unit uPyBridgeChainModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, uPyBridge;

type
  { TPyBridgeChainModule }

  TPyBridgeChainModule = class(TRequestChainProcessor)
    strict private
      FOrchestrator : TBridgeOrchestrator;
      function get_isActive: boolean;

    public
      destructor Destroy; override;

      function ProcessGETRequest(
        const Resource: String;
        const Headers: TStringList;
        const URIParams: TStringList;
        var ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function ProcessPOSTRequest(
        const Resource: String;
        const Headers: TStringList;
        const Payload: AnsiString;
        var ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      procedure setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer; aWorkerTimeoutMs: Integer);

      property isActive: boolean read get_isActive;
  end;

const
  cInvalidAnswer = String('wpbError');
  cOperationAborted = String('Request aborted for internal error');


implementation

uses
  ULBLogger, fpjson;

{ TPyBridgeChainModule }

function TPyBridgeChainModule.get_isActive: boolean;
begin
  Result := FOrchestrator <> nil;
end;

destructor TPyBridgeChainModule.Destroy;
begin
  if FOrchestrator <> nil then
    FreeAndNil(FOrchestrator);

  inherited Destroy;
end;

function TPyBridgeChainModule.ProcessGETRequest(const Resource: String;
  const Headers: TStringList; const URIParams: TStringList;
  var ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out
  ResponseCode: Integer): Boolean;
begin
  Result := false;
end;

function TPyBridgeChainModule.ProcessPOSTRequest(const Resource: String;
  const Headers: TStringList; const Payload: AnsiString;
  var ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out
  ResponseCode: Integer): Boolean;
var
  _Request : TRequestData;
  _ErrMsg : TJSONObject;
  _sErrMsg : String;

begin
  LBLogger.Write(5, 'TPyBridgeChainModule.ProcessPOSTRequest', lmt_Debug, 'Resource <%s>', [Resource]);

  // Called by THTTPRequestManager
  // Used to insert request into orchestrator and waiting for answer
  _Request.initialize();
  if Resource[1] = '/' then
    _Request.Script := Copy(Resource, 2, Length(Resource) - 1)
  else
    _Request.Script := Resource;

  if Payload <> '' then
  begin
    _Request.Params := @Payload[1];
    _Request.ParamsLen := Length(Payload);
  end;

  if FOrchestrator.insertRequest(@_Request) then
    RTLEventWaitFor(_Request.TerminateEvent)
  else
    LBLogger.Write(1, 'TPyBridgeChainModule.ProcessPOSTRequest', lmt_Warning, 'Request <%s> not accepted', [Resource]);


  if not _Request.Aborted then
  begin
    if (_Request.Payload <> nil) and (_Request.PayloadLen > 0) then
    begin
      if ResponseData = nil then
        ResponseData := TMemoryStream.Create
      else
        ResponseData.Clear;

      ResponseData.Write(_Request.Payload^, _Request.PayloadLen);
    end;

  end
  else begin
    _ErrMsg := TJSONObject.Create();

    if (_Request.Payload <> nil) and (_Request.PayloadLen > 0) then
    begin
      SetLength(_sErrMsg, _Request.PayloadLen);
      Move(_Request.Payload^, _sErrMsg[1], _Request.PayloadLen);
      _ErrMsg.Add(cInvalidAnswer, _sErrMsg);
    end
    else
      _ErrMsg.Add(cInvalidAnswer, cOperationAborted);

    _ErrMsg.CompressedJSON := True;
    _sErrMsg := _ErrMsg.AsJSON;
    _ErrMsg.Free;

    if ResponseData = nil then
      ResponseData := TMemoryStream.Create
    else
      ResponseData.Clear;

    ResponseData.Write(_sErrMsg[1], Length(_sErrMsg));
  end;

  ResponseCode := 200;
  Result := True;
end;

procedure TPyBridgeChainModule.setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer; aWorkerTimeoutMs: Integer);
begin
  if FOrchestrator <> nil then
  begin
    LBLogger.Write(5, 'TPyBridgeChainModule.setOrchestratorParams', lmt_Debug, 'Destroying old orchestrator ...');
    FreeAndNil(FOrchestrator);
  end;

  if (aThreadPoolSize > 0) and (aSharedMemorySize > 0) then
    FOrchestrator := TBridgeOrchestrator.Create(aThreadPoolSize, aSharedMemorySize, aWorkerTimeoutMs)
  else
    LBLogger.Write(1, 'TPyBridgeChainModule.setOrchestratorParams', lmt_Warning, 'Wrong values for the thread pool size or for the shared memory size!');
end;


end.


