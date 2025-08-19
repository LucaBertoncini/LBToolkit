unit uBridgeChainModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLBmicroWebServer, uBaseBridgeManager, uHTTPRequestParser;

type
  { TBridgeChainModule }

  TBridgeChainModule = class(TRequestChainProcessor)
    strict private
      FOrchestrator : TBridgeOrchestrator;

      function get_isActive: boolean;

    strict protected
      function DoProcessRequest(
        HTTPParser: THTTPRequestParser;
        ResponseHeaders: TStringList;
        var ResponseData: TMemoryStream;
        out ResponseCode: Integer
      ): Boolean; override;


    public
      destructor Destroy; override;

      procedure setOrchestratorParams(ConfigParams: TBridgeConfigParams);  { ConfigParams must be destroyed from caller }

      property isActive: boolean read get_isActive;
  end;

const
  cInvalidAnswer = String('wpbError');
  cOperationAborted = String('Request aborted for internal error');


implementation

uses
  ULBLogger, fpjson, uHTTPConsts;

{ TBridgeChainModule }

function TBridgeChainModule.get_isActive: boolean;
begin
  Result := FOrchestrator <> nil;
end;

destructor TBridgeChainModule.Destroy;
begin
  if FOrchestrator <> nil then
    FreeAndNil(FOrchestrator);

  inherited Destroy;
end;

function TBridgeChainModule.DoProcessRequest(HTTPParser: THTTPRequestParser;
  ResponseHeaders: TStringList; var ResponseData: TMemoryStream; out ResponseCode: Integer): Boolean;
var
  _Request : TRequestData;
  _ErrMsg : TJSONObject;
  _sErrMsg : String;

begin
  Result := False;
  ResponseCode := HTTP_STATUS_INTERNAL_ERROR;
//  LBLogger.Write(5, 'TBridgeChainModule.DoProcessRequest', lmt_Debug, 'Received request ...');

  if FOrchestrator <> nil then
  begin
    try
      HTTPParser.SplitURIIntoResourceAndParameters();

      if HTTPParser.Resource <> '' then
      begin
        // Called by THTTPRequestManager
        // Used to insert request into orchestrator and waiting for answer
        _Request.initialize();
        if HTTPParser.Resource[1] = '/' then
          _Request.Script := Copy(HTTPParser.Resource, 2, Length(HTTPParser.Resource) - 1)
        else
          _Request.Script := HTTPParser.Resource;

        _Request.HTTPParser := HTTPParser;
        // LBLogger.Write(5, 'TBridgeChainModule.DoProcessRequest', lmt_Debug, 'Adding request ...');

        if FOrchestrator.insertRequest(@_Request) then
          RTLEventWaitFor(_Request.TerminateEvent)
        else
          LBLogger.Write(1, 'TBridgeChainModule.DoProcessRequest', lmt_Warning, 'Request <%s> not accepted', [HTTPParser.Resource]);


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

          Result := True;
        end
        else begin
          if FNext = nil then  // Last in chain
          begin
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

            Result := True;
          end
          else
            Result := False;
        end;

        ResponseCode := HTTP_STATUS_OK;

      end
      else
        LBLogger.Write(1, 'TBridgeChainModule.DoProcessRequest', lmt_Warning, 'Resource not found!');

    except
      on E: Exception do
        LBLogger.Write(1, 'TBridgeChainModule.DoProcessRequest', lmt_Error, E.Message);
    end;

  end
  else
    LBLogger.Write(1, 'TBridgeChainModule.DoProcessRequest', lmt_Warning, 'Orchestrator doesn''t exist');
end;

procedure TBridgeChainModule.setOrchestratorParams(ConfigParams: TBridgeConfigParams);
begin
  if FOrchestrator <> nil then
  begin
    LBLogger.Write(5, 'TBridgeChainModule.setOrchestratorParams', lmt_Debug, 'Destroying old orchestrator ...');
    FreeAndNil(FOrchestrator);
  end;

  if ConfigParams <> nil then
  begin
    if ConfigParams.Completed then
      FOrchestrator := TBridgeOrchestrator.Create(ConfigParams)
    else
      LBLogger.Write(1, 'TBridgeChainModule.setOrchestratorParams', lmt_Warning, 'Incomplete params! Orchestrator not created!');

  end
  else
    LBLogger.Write(1, 'TBridgeChainModule.setOrchestratorParams', lmt_Warning, 'No configuration parameters');

end;


end.


