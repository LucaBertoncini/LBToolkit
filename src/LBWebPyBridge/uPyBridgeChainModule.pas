unit uPyBridgeChainModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, uPyBridge, uHTTPRequestParser;

type
  { TPyBridgeChainModule }

  TPyBridgeChainModule = class(TRequestChainProcessor)
    strict private
      FOrchestrator : TBridgeOrchestrator;

      function get_isActive: boolean;

    strict protected
      function DoProcessGETRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;

      function DoProcessPOSTRequest(
        HTTPParser: THTTPRequestParser;
        Payload: TMemoryStream;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;


    public
      destructor Destroy; override;

      procedure setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer; aWorkerTimeoutMs: Integer; const ScriptsFolder: String);

      property isActive: boolean read get_isActive;
  end;

const
  cInvalidAnswer = String('wpbError');
  cOperationAborted = String('Request aborted for internal error');


implementation

uses
  ULBLogger, fpjson, uHTTPConsts;

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

function TPyBridgeChainModule.DoProcessGETRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
begin
  Result := false;
end;

function TPyBridgeChainModule.DoProcessPOSTRequest(HTTPParser: THTTPRequestParser;
  Payload: TMemoryStream; ResponseHeaders: TStringList;
  var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Request : TRequestData;
  _ErrMsg : TJSONObject;
  _sErrMsg : String;

begin
  Result := False;
  ResponseCode := HTTP_STATUS_INTERNAL_ERROR;

  HTTPParser.SplitURIIntoResourceAndParameters();

  LBLogger.Write(5, 'TPyBridgeChainModule.ProcessPOSTRequest', lmt_Debug, 'Resource <%s>', [HTTPParser.Resource]);

  if HTTPParser.Resource <> '' then
  begin
    // Called by THTTPRequestManager
    // Used to insert request into orchestrator and waiting for answer
    _Request.initialize();
    if HTTPParser.Resource[1] = '/' then
      _Request.Script := Copy(HTTPParser.Resource, 2, Length(HTTPParser.Resource) - 1)
    else
      _Request.Script := HTTPParser.Resource;

    if (Payload <> nil) and (Payload.Size > 0) then
    begin
      _Request.Params := Payload.Memory;
      _Request.ParamsLen := Payload.Size;
    end;

    if FOrchestrator.insertRequest(@_Request) then
      RTLEventWaitFor(_Request.TerminateEvent)
    else
      LBLogger.Write(1, 'TPyBridgeChainModule.ProcessPOSTRequest', lmt_Warning, 'Request <%s> not accepted', [HTTPParser.Resource]);


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

    ResponseCode := HTTP_STATUS_OK;
    Result := True;

  end
  else
    LBLogger.Write(1, 'TPyBridgeChainModule.DoProcessPOSTRequest', lmt_Warning, 'Resource not found!');
end;

procedure TPyBridgeChainModule.setOrchestratorParams(aThreadPoolSize: Integer; aSharedMemorySize: Integer; aWorkerTimeoutMs: Integer; const ScriptsFolder: String);
begin
  if FOrchestrator <> nil then
  begin
    LBLogger.Write(5, 'TPyBridgeChainModule.setOrchestratorParams', lmt_Debug, 'Destroying old orchestrator ...');
    FreeAndNil(FOrchestrator);
  end;

  if (aThreadPoolSize > 0) and (aSharedMemorySize > 0) then
    FOrchestrator := TBridgeOrchestrator.Create(aThreadPoolSize, aSharedMemorySize, aWorkerTimeoutMs, ScriptsFolder)
  else
    LBLogger.Write(1, 'TPyBridgeChainModule.setOrchestratorParams', lmt_Warning, 'Wrong values for the thread pool size or for the shared memory size!');
end;


end.


