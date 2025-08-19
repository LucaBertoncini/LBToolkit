library LBToolkit_shared;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF} Toolkit_CAPI_Logger, Toolkit_CAPI_CircularBuffer,
  Toolkit_CAPI_microWebServer, Toolkit_CAPI_WebPrism, uFPHTTPRequestProcessor;

exports
  // Circular Buffer API
  CircularBuffer_Create,
  CircularBuffer_Destroy,
  CircularBuffer_Write,
  CircularBuffer_Read,
  CircularBuffer_GetAvailableForRead,
  CircularBuffer_GetAvailableForWrite,
  CircularBuffer_FindPattern,
  CircularBuffer_Peek,
  CircularBuffer_Seek,
  CircularBuffer_Clear,

  CircularBufferTS_Create,
  CircularBufferTS_Destroy,
  CircularBufferTS_Write,
  CircularBufferTS_Read,
  CircularBufferTS_GetAvailableForRead,
  CircularBufferTS_GetAvailableForWrite,
  CircularBufferTS_FindPattern,
  CircularBufferTS_Peek,
  CircularBufferTS_Seek,
  CircularBufferTS_Clear,

  // Logger API
  Logger_Initialize,
  Logger_Finalize,
  Logger_Write,
  Logger_CreateCallbackSublogger,
  Logger_DestroySublogger,
  Logger_GetMsgType_Error,
  Logger_GetMsgType_Warning,
  Logger_GetMsgType_Debug,
  Logger_GetMsgType_Info,


  // WebServer API
  WebServer_Create,
  WebServer_Destroy,
  ReqElab_RequestHeaderValue,
  ReqElab_RequestHeaderName,
  ReqElab_RequestHeadersCount,
  ReqElab_setResponseData,
  ReqElab_addResponseHeader,
  ReqElab_URIParamsCount,
  ReqElab_URIParam,
  ReqElab_BodySize,
  ReqElab_Body,

  // WebPrism API
  WebPrism_Create,
  WebPrism_Destroy;



{$R *.res}

begin
end.
