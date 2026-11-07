unit uAVFilterManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAVLibraryBaseManager, libavfilter, libavutil;

type

  { TAVFilterManager }

  TAVFilterManager = class(TAVLibraryBaseManager)
    strict private
      Favfilter_free : Tavfilter_free;
      Favfilter_graph_free : Tavfilter_graph_free;

    public
      constructor Create(aPath: String); override;

      procedure avfilter_free(var filter: PAVFilterContext);
      procedure avfilter_graph_free(var graph: PAVFilterGraph);

      var
        avfilter_graph_create_filter : Tavfilter_graph_create_filter;
        avfilter_link                : Tavfilter_link;
        avfilter_graph_send_command  : Tavfilter_graph_send_command;
        avfilter_get_by_name         : Tavfilter_get_by_name;
        avfilter_graph_alloc         : Tavfilter_graph_alloc;
        avfilter_graph_config        : Tavfilter_graph_config;
        av_buffersrc_add_frame_flags : Tav_buffersrc_add_frame_flags;
        av_buffersink_get_frame      : Tav_buffersink_get_frame;
  end;


implementation

{ TAVFilterManager }

constructor TAVFilterManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

  FLibraryLoader.addFunction('avfilter_graph_create_filter', @avfilter_graph_create_filter);
  FLibraryLoader.addFunction('avfilter_graph_send_command',  @avfilter_graph_send_command);
  FLibraryLoader.addFunction('avfilter_link',                @avfilter_link);
  FLibraryLoader.addFunction('avfilter_free',                @Favfilter_free);
  FLibraryLoader.addFunction('avfilter_get_by_name',         @avfilter_get_by_name);
  FLibraryLoader.addFunction('avfilter_graph_alloc',         @avfilter_graph_alloc);
  FLibraryLoader.addFunction('avfilter_graph_config',        @avfilter_graph_config);
  FLibraryLoader.addFunction('avfilter_graph_free',          @Favfilter_graph_free);
  FLibraryLoader.addFunction('av_buffersrc_add_frame_flags', @av_buffersrc_add_frame_flags);
  FLibraryLoader.addFunction('av_buffersink_get_frame',      @av_buffersink_get_frame);

  if FLibraryLoader.LoadLibrary(avfilterLibName, _Error) then
    FInitialized := True;
end;

procedure TAVFilterManager.avfilter_free(var filter: PAVFilterContext);
begin
  if filter <> nil then
  begin
    Favfilter_free(filter);
    filter := nil;
  end;
end;

procedure TAVFilterManager.avfilter_graph_free(var graph: PAVFilterGraph);
begin
  if (graph <> nil) then
  begin
    Favfilter_graph_free(@graph);
    graph := nil;
  end;
end;

end.

