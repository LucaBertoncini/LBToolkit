unit uSWResampleManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAVLibraryBaseManager, libswresample, ctypes, libavutil;

type

  { TSWResampleManager }

  TSWResampleManager = class(TAVLibraryBaseManager)
    strict private
      Fswr_free : Tswr_free;

    public
      constructor Create(aPath: String); override;

      procedure swr_free(var s: PSwrContext);

      var
        swr_alloc          : Tswr_alloc;
        swr_init           : Tswr_init;
        swr_convert        : Tswr_convert;
        swr_get_delay      : Tswr_get_delay;
        swr_convert_frame  : Tswr_convert_frame;
  end;

implementation

{ TSWResampleManager }

constructor TSWResampleManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

//  FLibraryLoader.addFunction('swr_alloc_set_opts', @swr_alloc_set_opts);
  FLibraryLoader.addFunction('swr_alloc',          @swr_alloc);
  FLibraryLoader.addFunction('swr_init',           @swr_init);
  FLibraryLoader.addFunction('swr_convert',        @swr_convert);
  FLibraryLoader.addFunction('swr_free',           @Fswr_free);
  FLibraryLoader.addFunction('swr_get_delay',      @swr_get_delay);
  FLibraryLoader.addFunction('swr_convert_frame',  @swr_convert_frame);


  FInitialized := FLibraryLoader.LoadLibrary(swresampleLibName, _Error);
end;

procedure TSWResampleManager.swr_free(var s: PSwrContext);
begin
  if s <> nil then
  begin
    Fswr_free(@s);
    s := nil;
  end;
end;

end.
