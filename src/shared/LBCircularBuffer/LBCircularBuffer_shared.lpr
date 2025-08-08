library LBCircularBuffer_shared;

{$mode objfpc}{$H+}

uses
  lbcbuffer_capi in 'lbcbuffer_capi.pas';

exports
  LBCB_Initialize,
  LBCB_Finalize,
  LBCB_Create,
  LBCB_Destroy,
  LBCB_Write,
  LBCB_Read,
  LBCB_GetAvailableForRead,
  LBCB_GetAvailableForWrite,
  LBCB_Clear;

begin
end.
