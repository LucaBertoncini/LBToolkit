unit uLBToolkitLoader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Non-Thread-Safe API
  TCircularBuffer_Create               = function(aSize: Cardinal): Pointer; cdecl;
  TCircularBuffer_Destroy              = procedure(aHandle: Pointer); cdecl;
  TCircularBuffer_Write                = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBuffer_Read                 = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBuffer_GetAvailableForRead  = function(aHandle: Pointer): Cardinal; cdecl;
  TCircularBuffer_GetAvailableForWrite = function(aHandle: Pointer): Cardinal; cdecl;
  TCircularBuffer_FindPattern          = function(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; cdecl;
  TCircularBuffer_Peek                 = function(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean; cdecl;
  TCircularBuffer_Seek                 = function(aHandle: Pointer; aOffset: Cardinal): Boolean; cdecl;
  TCircularBuffer_Clear                = procedure(aHandle: Pointer); cdecl;

  // Thread-Safe API
  TCircularBufferTS_Create               = function(aSize: Cardinal): Pointer; cdecl;
  TCircularBufferTS_Destroy              = procedure(aHandle: Pointer); cdecl;
  TCircularBufferTS_Write                = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBufferTS_Read                 = function(aHandle: Pointer; aData: PByte; aCount: Cardinal): Boolean; cdecl;
  TCircularBufferTS_GetAvailableForRead  = function(aHandle: Pointer): Cardinal; cdecl;
  TCircularBufferTS_GetAvailableForWrite = function(aHandle: Pointer): Cardinal; cdecl;
  TCircularBufferTS_FindPattern          = function(aHandle: Pointer; aPattern: PByte; aPatternSize: Cardinal; aOffset: Cardinal): Integer; cdecl;
  TCircularBufferTS_Peek                 = function(aHandle: Pointer; aBuffer: Pointer; aCount: Cardinal; aOffset: Cardinal = 0): Boolean; cdecl;
  TCircularBufferTS_Seek                 = function(aHandle: Pointer; aOffset: Cardinal): Boolean; cdecl;
  TCircularBufferTS_Clear                = procedure(aHandle: Pointer); cdecl;

  // Logger API
  TLogCallback                    = function(aLevel: Integer; aSender: PChar; aMsgType: Byte; aMessage: PChar; aStopPropagation: PBoolean; userData: Pointer): Boolean; cdecl;
  TLogger_Initialize              = procedure(aLogFileName: PChar; aLogLevel: Integer); cdecl;
  TLogger_Finalize                = procedure(); cdecl;
  TLogger_Write                   = procedure(aLevel: Integer; aSender: PChar; aMsgType: Byte; aMessage: PChar); cdecl;
  TLogger_CreateCallbackSublogger = function(aCallback: TLogCallback; aMaxLogLevel: Integer; userData: Pointer): Pointer; cdecl;
  TLogger_DestroySublogger        = procedure(aHandle: Pointer); cdecl;
  TLogger_GetMsgType_Error        = function(): byte; cdecl;
  TLogger_GetMsgType_Warning      = function(): byte; cdecl;
  TLogger_GetMsgType_Debug        = function(): byte; cdecl;
  TLogger_GetMsgType_Info         = function(): byte; cdecl;

  // WebServer API
  TGETCallback = function (RequestElaborator: Pointer; aResource: pChar; aResponseCode: pInteger): Boolean; cdecl;
  TPOSTCallback = function (RequestElaborator: Pointer; aResource: pChar; aPayload: pByte; aPayloadLen: Integer; aResponseCode: pInteger): Boolean; cdecl;
  TWebSocketMessageCallback = procedure (aPayLoad: pByte; aPayloadLen: Integer); cdecl;
  TWebSocketConnectedCallback = procedure (); cdecl;

  TWebServer_Create            = function (aConfigFilename: PChar;
                                           GETCallback: TGETCallback;
                                           POSTCallback: TPOSTCallback;
                                           WebSocketConnectedCallback: TWebSocketConnectedCallback;
                                           WebSocketMessageCallback: TWebSocketMessageCallback): Pointer; cdecl;

  TWebServer_Destroy           = function (aWebServer: Pointer): Boolean; cdecl;

  TReqElab_RequestHeaderValue  = function (RequestElaborator: Pointer; Index: Integer; aValueBuffer: pByte; aValueBufferLen: Integer): Integer; cdecl;
  TReqElab_RequestHeaderName   = function (RequestElaborator: Pointer; Index: Integer; aNameBuffer: pByte; aNameBufferLen: Integer): Integer; cdecl;
  TReqElab_RequestHeadersCount = function (RequestElaborator: Pointer): Integer; cdecl;
  TReqElab_setResponseData     = function (RequestElaborator: Pointer; aBuffer: pByte; aBufferLen: Integer): Boolean; cdecl;
  TReqElab_addResponseHeader   = function (RequestElaborator: Pointer; aName: PChar; aValue: PChar): Boolean; cdecl;
  TReqElab_URIParamsCount      = function (RequestElaborator: Pointer): Integer; cdecl;
  TReqElab_URIParam            = function (RequestElaborator: Pointer; Index: Integer; aURIParamBuff: pByte; aURIParamLen: Integer): Integer; cdecl;

  // WebPyBridge API
  TWebPyBridge_Create = function (aConfigurationFile: PChar): Pointer; cdecl;
  TWebPyBridge_Destroy = procedure (aWebPyBridge: Pointer); cdecl;



var
  // Non-Thread-Safe Pointers
  CircularBuffer_Create               : TCircularBuffer_Create;
  CircularBuffer_Destroy              : TCircularBuffer_Destroy;
  CircularBuffer_Write                : TCircularBuffer_Write;
  CircularBuffer_Read                 : TCircularBuffer_Read;
  CircularBuffer_GetAvailableForRead  : TCircularBuffer_GetAvailableForRead;
  CircularBuffer_GetAvailableForWrite : TCircularBuffer_GetAvailableForWrite;
  CircularBuffer_FindPattern          : TCircularBuffer_FindPattern;
  CircularBuffer_Peek                 : TCircularBuffer_Peek;
  CircularBuffer_Seek                 : TCircularBuffer_Seek;
  CircularBuffer_Clear                : TCircularBuffer_Clear;

  // Thread-Safe Pointers
  CircularBufferTS_Create               : TCircularBufferTS_Create;
  CircularBufferTS_Destroy              : TCircularBufferTS_Destroy;
  CircularBufferTS_Write                : TCircularBufferTS_Write;
  CircularBufferTS_Read                 : TCircularBufferTS_Read;
  CircularBufferTS_GetAvailableForRead  : TCircularBufferTS_GetAvailableForRead;
  CircularBufferTS_GetAvailableForWrite : TCircularBufferTS_GetAvailableForWrite;
  CircularBufferTS_FindPattern          : TCircularBufferTS_FindPattern;
  CircularBufferTS_Peek                 : TCircularBufferTS_Peek;
  CircularBufferTS_Seek                 : TCircularBufferTS_Seek;
  CircularBufferTS_Clear                : TCircularBufferTS_Clear;

  // Logger Pointers
  Logger_Initialize              : TLogger_Initialize;
  Logger_Finalize                : TLogger_Finalize;
  Logger_Write                   : TLogger_Write;
  Logger_CreateCallbackSublogger : TLogger_CreateCallbackSublogger;
  Logger_DestroySublogger        : TLogger_DestroySublogger;
  Logger_GetMsgType_Error        : TLogger_GetMsgType_Error;
  Logger_GetMsgType_Warning      : TLogger_GetMsgType_Warning;
  Logger_GetMsgType_Debug        : TLogger_GetMsgType_Debug;
  Logger_GetMsgType_Info         : TLogger_GetMsgType_Info;

  // WebServer Pointers
  WebServer_Create            : TWebServer_Create;
  WebServer_Destroy           : TWebServer_Destroy;
  ReqElab_RequestHeaderValue  : TReqElab_RequestHeaderValue;
  ReqElab_RequestHeaderName   : TReqElab_RequestHeaderName;
  ReqElab_RequestHeadersCount : TReqElab_RequestHeadersCount;
  ReqElab_setResponseData     : TReqElab_setResponseData;
  ReqElab_addResponseHeader   : TReqElab_addResponseHeader;
  ReqElab_URIParamsCount      : TReqElab_URIParamsCount;
  ReqElab_URIParam            : TReqElab_URIParam;

  // WebPyBridge API
  WebPyBridge_Create  : TWebPyBridge_Create;
  WebPyBridge_Destroy : TWebPyBridge_Destroy;


function LoadLBToolkit(const aFilePath: String): Boolean;
function isLBToolkitLoaded(): Boolean;


implementation

uses
  dynlibs, ULBLogger, uExternalLibrariesManager;

var
  gv_LibraryManager : TExternalLibraryLoader = nil;

function LoadLBToolkit(const aFilePath: String): Boolean;
var
  _LibFile : String = '';
  _ErrMsg : String;

begin
  Result := (gv_LibraryManager <> nil) and gv_LibraryManager.isLoaded();

  if not Result then
  begin
    if aFilePath <> '' then
      _LibFile := ExpandFileName(aFilePath);

    if (_LibFile = '') then
    begin
      {$IFDEF UNIX}
      _LibFile := ExpandFileName('./libLBToolkit.so');
      {$ENDIF}
      {$IFDEF WINDOWS}
      _LibFile := 'LBToolkit.dll';
      {$ENDIF}
    end;

    gv_LibraryManager := TExternalLibraryLoader.Create;

    // Circular Buffer Non-TS functions
    gv_LibraryManager.addFunction('CircularBuffer_Create', @CircularBuffer_Create);
    gv_LibraryManager.addFunction('CircularBuffer_Destroy', @CircularBuffer_Destroy);
    gv_LibraryManager.addFunction('CircularBuffer_Write', @CircularBuffer_Write);
    gv_LibraryManager.addFunction('CircularBuffer_Read', @CircularBuffer_Read);
    gv_LibraryManager.addFunction('CircularBuffer_GetAvailableForRead', @CircularBuffer_GetAvailableForRead);
    gv_LibraryManager.addFunction('CircularBuffer_GetAvailableForWrite', @CircularBuffer_GetAvailableForWrite);
    gv_LibraryManager.addFunction('CircularBuffer_FindPattern', @CircularBuffer_FindPattern);
    gv_LibraryManager.addFunction('CircularBuffer_Peek', @CircularBuffer_Peek);
    gv_LibraryManager.addFunction('CircularBuffer_Seek', @CircularBuffer_Seek);
    gv_LibraryManager.addFunction('CircularBuffer_Clear', @CircularBuffer_Clear);

    // Circular Buffer TS functions
    gv_LibraryManager.addFunction('CircularBufferTS_Create', @CircularBufferTS_Create);
    gv_LibraryManager.addFunction('CircularBufferTS_Destroy', @CircularBufferTS_Destroy);
    gv_LibraryManager.addFunction('CircularBufferTS_Write', @CircularBufferTS_Write);
    gv_LibraryManager.addFunction('CircularBufferTS_Read', @CircularBufferTS_Read);
    gv_LibraryManager.addFunction('CircularBufferTS_GetAvailableForRead', @CircularBufferTS_GetAvailableForRead);
    gv_LibraryManager.addFunction('CircularBufferTS_GetAvailableForWrite', @CircularBufferTS_GetAvailableForWrite);
    gv_LibraryManager.addFunction('CircularBufferTS_FindPattern', @CircularBufferTS_FindPattern);
    gv_LibraryManager.addFunction('CircularBufferTS_Peek', @CircularBufferTS_Peek);
    gv_LibraryManager.addFunction('CircularBufferTS_Seek', @CircularBufferTS_Seek);
    gv_LibraryManager.addFunction('CircularBufferTS_Clear', @CircularBufferTS_Clear);

    // Logger functions
    gv_LibraryManager.addFunction('Logger_Initialize', @Logger_Initialize);
    gv_LibraryManager.addFunction('Logger_Finalize', @Logger_Finalize);
    gv_LibraryManager.addFunction('Logger_Write', @Logger_Write);
    gv_LibraryManager.addFunction('Logger_CreateCallbackSublogger', @Logger_CreateCallbackSublogger);
    gv_LibraryManager.addFunction('Logger_DestroySublogger', @Logger_DestroySublogger);
    gv_LibraryManager.addFunction('Logger_GetMsgType_Error', @Logger_GetMsgType_Error);
    gv_LibraryManager.addFunction('Logger_GetMsgType_Warning', @Logger_GetMsgType_Warning);
    gv_LibraryManager.addFunction('Logger_GetMsgType_Debug', @Logger_GetMsgType_Debug);
    gv_LibraryManager.addFunction('Logger_GetMsgType_Info', @Logger_GetMsgType_Info);

    // WebServer functions
    gv_LibraryManager.addFunction('WebServer_Create',            @WebServer_Create);
    gv_LibraryManager.addFunction('WebServer_Destroy',           @WebServer_Destroy);
    gv_LibraryManager.addFunction('ReqElab_RequestHeaderValue',  @ReqElab_RequestHeaderValue);
    gv_LibraryManager.addFunction('ReqElab_RequestHeaderName',   @ReqElab_RequestHeaderName);
    gv_LibraryManager.addFunction('ReqElab_RequestHeadersCount', @ReqElab_RequestHeadersCount);
    gv_LibraryManager.addFunction('ReqElab_setResponseData',     @ReqElab_setResponseData);
    gv_LibraryManager.addFunction('ReqElab_addResponseHeader',   @ReqElab_addResponseHeader);
    gv_LibraryManager.addFunction('ReqElab_URIParamsCount',      @ReqElab_URIParamsCount);
    gv_LibraryManager.addFunction('ReqElab_URIParam',            @ReqElab_URIParam);

    // WebPyBridge functions
    gv_LibraryManager.addFunction('WebPyBridge_Create',  @WebPyBridge_Create);
    gv_LibraryManager.addFunction('WebPyBridge_Destroy', @WebPyBridge_Destroy);


    Result := gv_LibraryManager.LoadLibrary(_LibFile, _ErrMsg);

  end;
end;

function isLBToolkitLoaded(): Boolean;
begin
  Result := (gv_LibraryManager <> nil) and (gv_LibraryManager.isLoaded());
end;

finalization
  if gv_LibraryManager <> nil then
    FreeAndNil(gv_LibraryManager);

end.

