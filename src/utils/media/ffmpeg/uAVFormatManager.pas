unit uAVFormatManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTimedoutCriticalSection, uAVLibraryBaseManager, libavformat, libavutil;

type

  { TAVFormatManager }

  TAVFormatManager = class(TAVLibraryBaseManager)
    strict private
      FCSThreadSafe : TTimedOutCriticalSection;
      FAvformat_close_input : Tavformat_close_input;
      FAvformat_free_context : Tavformat_free_context;
      Favformat_open_input   : Tavformat_open_input;

    public
      constructor Create(aPath: String); override;
      destructor Destroy; override;

      function avformat_open_input(var ps: PAVFormatContext; aFilename: String; fmt: PAVInputFormat; options: PPAVDictionary; out aResultValue: Integer): Boolean;

      procedure avformat_close_input(var ctx: PAVFormatContext);
      procedure avformat_free_context(var ctx: PAVFormatContext);

      var
        avformat_network_init          : Tavformat_network_init;
        avformat_alloc_context         : Tavformat_alloc_context;
        avformat_alloc_output_context2 : Tavformat_alloc_output_context2;
        avformat_find_stream_info      : Tavformat_find_stream_info;
        av_read_frame                  : Tav_read_frame;
        av_seek_frame                  : Tav_seek_frame;
        avio_seek                      : Tavio_seek;
        avio_open                      : Tavio_open;
        avio_close                     : Tavio_close;
        avio_closep                    : Tavio_closep;
        av_guess_format                : Tav_guess_format;
        avformat_write_header          : Tavformat_write_header;
        av_interleaved_write_frame     : Tav_interleaved_write_frame;
        av_write_frame                 : Tav_write_frame;
        avformat_new_stream            : Tavformat_new_stream;
        av_write_trailer               : Tav_write_trailer;
  end;


implementation

uses
  ULBLogger;

{ TAVFormatManager }

constructor TAVFormatManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

  FCSThreadSafe := TTimedOutCriticalSection.Create();

  FLibraryLoader.addFunction('avformat_network_init',          @avformat_network_init);
  FLibraryLoader.addFunction('avformat_alloc_context',         @avformat_alloc_context);
  FLibraryLoader.addFunction('avformat_alloc_output_context2', @avformat_alloc_output_context2);
  FLibraryLoader.addFunction('avformat_open_input',            @FAvformat_open_input);
  FLibraryLoader.addFunction('avformat_close_input',           @FAvformat_close_input);
  FLibraryLoader.addFunction('avformat_free_context',          @FAvformat_free_context);
  FLibraryLoader.addFunction('avformat_find_stream_info',      @avformat_find_stream_info);
  FLibraryLoader.addFunction('av_read_frame',                  @av_read_frame);
  FLibraryLoader.addFunction('av_seek_frame',                  @av_seek_frame);
  FLibraryLoader.addFunction('avio_seek',                      @avio_seek);
  FLibraryLoader.addFunction('avio_open',                      @avio_open);
  FLibraryLoader.addFunction('avio_close',                     @avio_close);
  FLibraryLoader.addFunction('avio_closep',                    @avio_closep);
  FLibraryLoader.addFunction('av_guess_format',                @av_guess_format);
  FLibraryLoader.addFunction('avformat_write_header',          @avformat_write_header);
  FLibraryLoader.addFunction('av_interleaved_write_frame',     @av_interleaved_write_frame);
  FLibraryLoader.addFunction('avformat_new_stream',            @avformat_new_stream);
  FLibraryLoader.addFunction('av_write_trailer',               @av_write_trailer);
  FLibraryLoader.addFunction('av_write_frame',                 @av_write_frame);

  FInitialized := FLibraryLoader.LoadLibrary(avformatLibName, _Error);
  if not FInitialized then
    LBLogger.Write(1, 'TAVFormatManager.Create', lmt_Warning, _Error);
end;

destructor TAVFormatManager.Destroy();
begin
  FreeAndNil(FCSThreadSafe);
  inherited Destroy;
end;

function TAVFormatManager.avformat_open_input(var ps: PAVFormatContext; aFilename: String; fmt: PAVInputFormat; options: PPAVDictionary; out aResultValue: Integer): Boolean;
begin
  Result := False;

  if ps <> nil then
    Self.avformat_close_input(ps);

  aFilename := Utf8ToAnsi(aFilename) + #0;

  if FCSThreadSafe.Acquire('TAVFormatManager.avformat_open_input') then
  begin
    try
      aResultValue := Favformat_open_input(@ps, PAnsiChar(aFilename), fmt, options);

    except
      on E: Exception do
      begin
        ps := nil;
        LBLogger.Write(1, 'TAVFormatManager.avformat_open_input', lmt_Error, 'Source <%s>  -  %s', [aFilename, E.Message]);
      end;
    end;
    FCSThreadSafe.Release();

    Result := aResultValue >= 0;
    if not Result then
      Self.avformat_close_input(ps);
  end;
end;

procedure TAVFormatManager.avformat_close_input(var ctx: PAVFormatContext);
begin
  // Free it and all its contents and set @ctx to NULL
  // avformat_free_context + avio_close
  if ctx <> nil then
  begin
    if FCSThreadSafe.Acquire('TAVFormatManager.avformat_close_input') then
    begin
      try
        FAvformat_close_input(@ctx);

      except
        on E: Exception do
          LBLogger.Write(1, 'TAVFormatManager.avformat_close_input', lmt_Error, PChar(E.Message));
      end;
      FCSThreadSafe.Release();
      ctx := nil;
    end;
  end;
end;

procedure TAVFormatManager.avformat_free_context(var ctx: PAVFormatContext);
begin
//  Free an AVFormatContext and all its streams
  if ctx <> nil then
  begin
    FAvformat_free_context(ctx);
    ctx := nil;
  end;
end;


end.
