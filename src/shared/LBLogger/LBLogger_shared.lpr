library LBLogger_shared;

{$mode objfpc}{$H+}

uses
  lblogger_capi in 'lblogger_capi.pas';

exports
  LBLog_Initialize,
  LBLog_Finalize,
  LBLog_Write,
  LBLog_CreateCallbackSublogger,
  LBLog_DestroySublogger;

begin
end.
