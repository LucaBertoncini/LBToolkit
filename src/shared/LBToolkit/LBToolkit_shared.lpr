library LBToolkit_shared;

{$mode objfpc}{$H+}

uses
  toolkit_capi_circularbuffer in 'toolkit_capi_circularbuffer.pas',
  toolkit_capi_logger in 'toolkit_capi_logger.pas';

// Add more units here as we implement their APIs

exports
  // Circular Buffer API
  CircularBuffer_Create,
  CircularBuffer_Destroy,
  CircularBuffer_Write,
  CircularBuffer_Read,
  CircularBuffer_GetAvailableForRead,
  CircularBuffer_GetAvailableForWrite,
  CircularBuffer_Clear,
  CircularBufferTS_Create,
  CircularBufferTS_Destroy,
  CircularBufferTS_Write,
  CircularBufferTS_Read,
  CircularBufferTS_GetAvailableForRead,
  CircularBufferTS_GetAvailableForWrite,
  CircularBufferTS_Clear,

  // Logger API
  Logger_Initialize,
  Logger_Finalize,
  Logger_Write,
  Logger_CreateCallbackSublogger,
  Logger_DestroySublogger;

// Add more exports here for other components

begin
end.
