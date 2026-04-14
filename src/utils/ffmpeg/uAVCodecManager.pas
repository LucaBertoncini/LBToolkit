unit uAVCodecManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTimedoutCriticalSection, uAVLibraryBaseManager, libavcodec, ctypes, libavutil;

type

  { TAVCodecManager }

  TAVCodecManager = class(TAVLibraryBaseManager)
    strict private
      type
        Tavcodec_parameters_alloc        = function  (): PAVCodecParameters; cdecl;

      var
        FAvCodec_Open2        : Tavcodec_open2;
        FAvcodec_free_context : Tavcodec_free_context;
        FAvcodec_close        : Tavcodec_close;

        FCS : TTimedOutCriticalSection;

    public
      constructor Create(aPath: String); override;
      destructor Destroy; override;

      function avcodec_open2(var avctx: PAVCodecContext; const codec: PAVCodec; options: PPAVDictionary): Boolean;
      procedure avcodec_free_context(var avctx: PAVCodecContext);
      procedure avcodec_close(avctx: PAVCodecContext);  // deprecated
      procedure av_free_packet(pkt: PAVPacket); deprecated;


      function isSupportedSampleFormat(aCodec: PAVCodec; aSampleFormat: TAVSampleFormat): Boolean;
      function isSupportedSampleRate(aCodec: PAVCodec; aSampleRate: Integer): Boolean;
      function isSupportedChannelLayout(aCodec: PAVCodec; aChannelLayout: Int64): Boolean;

      function initializeBSFContext(const aFilter: AnsiString; aParams: PAVCodecParameters): PAVBSFContext;

      var
        avcodec_find_decoder     : Tavcodec_find_decoder;
        avcodec_flush_buffers    : Tavcodec_flush_buffers;
        avcodec_send_packet      : Tavcodec_send_packet;
        avcodec_receive_frame    : Tavcodec_receive_frame;
        av_new_packet            : Tav_new_packet;
        av_packet_alloc          : Tav_packet_alloc;
        av_packet_free           : Tav_packet_free;
        av_packet_clone          : Tav_packet_clone;
        avcodec_find_encoder     : Tavcodec_find_encoder;
        avcodec_find_encoder_by_name    : Tavcodec_find_encoder_by_name;
        avcodec_alloc_context3          : Tavcodec_alloc_context3;
        avcodec_send_frame              : Tavcodec_send_frame;
        avcodec_receive_packet          : Tavcodec_receive_packet;
        av_packet_ref                   : Tav_packet_ref;
        av_packet_unref                 : Tav_packet_unref;
        avcodec_parameters_alloc        : Tavcodec_parameters_alloc;
        avcodec_parameters_free         : Tavcodec_parameters_free;
        avcodec_parameters_from_context : Tavcodec_parameters_from_context;
        avcodec_parameters_to_context   : Tavcodec_parameters_to_context;
        avcodec_parameters_copy         : Tavcodec_parameters_copy;
        av_init_packet                  : Tav_init_packet;
        avcodec_string                  : Tavcodec_string;
        avcodec_find_decoder_by_name    : Tavcodec_find_decoder_by_name;
        avcodec_fill_audio_frame        : Tavcodec_fill_audio_frame;
        av_packet_rescale_ts            : Tav_packet_rescale_ts;

        avcodec_get_hw_config           : Tavcodec_get_hw_config;

        av_bsf_get_by_name              : Tav_bsf_get_by_name;
        av_bsf_alloc                    : Tav_bsf_alloc;
        av_bsf_init                     : Tav_bsf_init;
        av_bsf_send_packet              : Tav_bsf_send_packet;
        av_bsf_receive_packet           : Tav_bsf_receive_packet;
        av_bsf_free                     : Tav_bsf_free;
  end;

implementation

uses
  ULBLogger, uFFmpeg;


{ TAVCodecManager }

constructor TAVCodecManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

  FCS := TTimedOutCriticalSection.Create;

  FLibraryLoader.addFunction('avcodec_free_context',            @FAvcodec_free_context);
  FLibraryLoader.addFunction('avcodec_find_decoder',            @avcodec_find_decoder);
  FLibraryLoader.addFunction('avcodec_find_decoder_by_name',    @avcodec_find_decoder_by_name);
  FLibraryLoader.addFunction('avcodec_open2',                   @FAvCodec_Open2);
  FLibraryLoader.addFunction('avcodec_flush_buffers',           @avcodec_flush_buffers);
  FLibraryLoader.addFunction('avcodec_send_packet',             @avcodec_send_packet);
  FLibraryLoader.addFunction('avcodec_receive_frame',           @avcodec_receive_frame);
  FLibraryLoader.addFunction('av_packet_free',                  @av_packet_free);
  FLibraryLoader.addFunction('av_packet_alloc',                 @av_packet_alloc);
  FLibraryLoader.addFunction('av_packet_ref',                   @av_packet_ref);
  FLibraryLoader.addFunction('av_packet_clone',                 @av_packet_clone);
  FLibraryLoader.addFunction('av_new_packet',                   @av_new_packet);
  FLibraryLoader.addFunction('avcodec_close',                   @FAvcodec_close);
  FLibraryLoader.addFunction('avcodec_find_encoder',            @avcodec_find_encoder);
  FLibraryLoader.addFunction('avcodec_find_encoder_by_name',    @avcodec_find_encoder_by_name);
  FLibraryLoader.addFunction('avcodec_alloc_context3',          @avcodec_alloc_context3);
  FLibraryLoader.addFunction('avcodec_send_frame',              @avcodec_send_frame);
  FLibraryLoader.addFunction('avcodec_receive_packet',          @avcodec_receive_packet);
  FLibraryLoader.addFunction('av_packet_unref',                 @av_packet_unref);
  FLibraryLoader.addFunction('avcodec_parameters_alloc',        @avcodec_parameters_alloc);
  FLibraryLoader.addFunction('avcodec_parameters_free',         @avcodec_parameters_free);
  FLibraryLoader.addFunction('avcodec_parameters_from_context', @avcodec_parameters_from_context);
  FLibraryLoader.addFunction('avcodec_parameters_to_context',   @avcodec_parameters_to_context);
  FLibraryLoader.addFunction('avcodec_parameters_copy',         @avcodec_parameters_copy);
  FLibraryLoader.addFunction('av_init_packet',                  @av_init_packet);
  FLibraryLoader.addFunction('avcodec_string',                  @avcodec_string);
  FLibraryLoader.addFunction('avcodec_fill_audio_frame',        @avcodec_fill_audio_frame);
  FLibraryLoader.addFunction('av_packet_rescale_ts',            @av_packet_rescale_ts);

  FLibraryLoader.addFunction('av_bsf_get_by_name',              @av_bsf_get_by_name);
  FLibraryLoader.addFunction('av_bsf_alloc',                    @av_bsf_alloc);
  FLibraryLoader.addFunction('av_bsf_init',                     @av_bsf_init);
  FLibraryLoader.addFunction('av_bsf_send_packet',              @av_bsf_send_packet);
  FLibraryLoader.addFunction('av_bsf_receive_packet',           @av_bsf_receive_packet);
  FLibraryLoader.addFunction('av_bsf_free',                     @av_bsf_free);

  FLibraryLoader.addFunction('avcodec_get_hw_config',           @avcodec_get_hw_config);

  FInitialized := FLibraryLoader.LoadLibrary(avcodecLibName, _Error);
end;

destructor TAVCodecManager.Destroy;
begin
  FreeAndNil(FCS);

  inherited Destroy;
end;

function TAVCodecManager.avcodec_open2(var avctx: PAVCodecContext; const codec: PAVCodec; options: PPAVDictionary): Boolean;
var
  _res : Integer = -1;

begin
  Result := False;

  if FCS.Acquire('TAVCodecManager.avcodec_open2') then
  begin

    try

      _res := FAvCodec_Open2(avctx, codec, options);

    except
      on E: Exception do
        LBLogger.Write(1, 'TAVCodecManager.avcodec_open2', lmt_Error, PChar(E.Message));
    end;

    FCS.Release();

  end;

  Result := _res = 0;

  if not Result then
  begin
    if avctx <> nil then
      FAvcodec_free_context(@avctx);

    avctx := nil;
    LBLogger.Write(1, 'TAVCodecManager.avcodec_open2', lmt_Warning, 'Could not open output codec: <%s>!', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
  end;

end;

procedure TAVCodecManager.avcodec_free_context(var avctx: PAVCodecContext);
begin
  try

    if (avctx <> nil) then
    begin
      // avcodec_flush_buffers(avctx);
      FAvcodec_free_context(@avctx);
    end;

  except
    on E: Exception do
      LBLogger.Write(1, 'TAVCodecManager.avcodec_free_context', lmt_Error, E.Message);
  end;

  avctx := nil;
end;

procedure TAVCodecManager.avcodec_close(avctx: PAVCodecContext);
var
  _res : Integer = -1;

begin
  if avctx <> nil then
  begin

    if FCS.Acquire('TAVCodecManager.avcodec_close') then
    begin

      try

        _res := FAvcodec_close(avctx);
        if _res < 0 then
          LBLogger.Write(1, 'TAVCodecManager.avcodec_close', lmt_Warning, 'Error closing context!');

      except
        on E: Exception do
          LBLogger.Write(1, 'TAVCodecManager.avcodec_close', lmt_Error, E.Message);
      end;

      FCS.Release();
    end;

  end;
end;


procedure TAVCodecManager.av_free_packet(pkt: PAVPacket);
begin
  av_packet_unref(pkt);
end;

function TAVCodecManager.isSupportedSampleFormat(aCodec: PAVCodec; aSampleFormat: TAVSampleFormat): Boolean;
var
  i : Integer = 0;
  _List : TStringList = nil;

begin
  Result := False;

  if aCodec <> nil then
  begin
    try

      _List := TStringList.Create;

      if aCodec^.sample_fmts <> nil then
      begin

        repeat

  //        if aCodec^.sample_fmts^[i] = aSampleFormat then
          if aCodec^.sample_fmts[i] = aSampleFormat then
          begin
            Result := True;
            Break;
          end
  //        else if aCodec^.sample_fmts^[i] = TAVSampleFormat.AV_SAMPLE_FMT_NONE then
          else if aCodec^.sample_fmts[i] = TAVSampleFormat.AV_SAMPLE_FMT_NONE then
            Break
          else begin
  //          _List.Add(IntToStr(Integer(aCodec^.sample_fmts^[i])));
            _List.Add(IntToStr(Integer(aCodec^.sample_fmts[i])));
            i += 1;
          end;

        until False;


      end;

      if not Result then
        LBLogger.Write(1, 'TAVCodecManager.isSupportedSampleFormat', lmt_Debug, 'Supported sample format for codec %d: <%s>', [Integer(aCodec^.id), _List.CommaText]);


    except
      on E: Exception do
        LBLogger.Write(1, 'TAVCodecManager.isSupportedSampleFormat', lmt_Error, PChar(E.Message));
    end;

  end;

  if _List <> nil then
    _List.Free;
end;

function TAVCodecManager.isSupportedSampleRate(aCodec: PAVCodec; aSampleRate: Integer): Boolean;
var
  _pSampleRate : pcint;
  _List : TStringList = nil;

begin
  Result := False;

  if aCodec <> nil then
  begin

    try

      if aCodec^.supported_samplerates <> nil then
      begin
        _pSampleRate := aCodec^.supported_samplerates;

        _List := TStringList.Create;

        while (_pSampleRate^ <> 0) do
        begin
          if _pSampleRate^ = aSampleRate then
          begin
            Result := True;
            Break;
          end
          else begin
            _List.Add(IntToStr(_pSampleRate^));
            _pSampleRate += 1;
          end;
        end;

        if not Result then
          LBLogger.Write(1, 'TAVCodecManager.isSupportedSampleRate', lmt_Debug, 'Supported sample rate for codec %d: <%s>', [Integer(aCodec^.id), _List.CommaText]);

      end
      else begin
        LBLogger.Write(1, 'TAVCodecManager.isSupportedSampleRate', lmt_Debug, 'Supported sample rate not specified!');
        Result := True;
      end;


    except
      on E: Exception do
        LBLogger.Write(1, 'TAVCodecManager.isSupportedSampleRate', lmt_Error, PChar(E.Message));
    end;

  end;

  if _List <> nil then
    _List.Free;
end;

function TAVCodecManager.isSupportedChannelLayout(aCodec: PAVCodec; aChannelLayout: Int64): Boolean;
var
  _pChannelLayout : PInt64;
  // _pChannelLayout : PCuint64;
begin
  Result := aCodec^.channel_layouts = nil;

  if not Result then
  begin
    _pChannelLayout := aCodec^.channel_layouts;
    while (_pChannelLayout^ <> 0) do
    begin
      if _pChannelLayout^ = aChannelLayout then
      begin
        Result := True;
        Break;
      end
      else
        _pChannelLayout += 1;
    end;
  end;
end;

function TAVCodecManager.initializeBSFContext(const aFilter: AnsiString; aParams: PAVCodecParameters): PAVBSFContext;
var
  _Filter : PAVBitStreamFilter;
  _res : Integer;
  _Go : Boolean = False;

begin
  Result := nil;

  try

    _Filter := Self.av_bsf_get_by_name(PAnsiChar(aFilter));

    if _Filter <> nil then
    begin
      _res := Self.av_bsf_alloc(_Filter, @Result);
      if _res >= 0 then
      begin
        _res := Self.avcodec_parameters_copy(Result^.par_in, aParams);
        if _res >= 0 then
        begin
          _res := Self.av_bsf_init(Result);
          if _res >= 0 then
          begin
            _Go := True;
            LBLogger.Write(1, 'TAVCodecManager.initializeBSFContext', lmt_Warning, 'BSF context successfully initialized!', []);
          end
          else
            LBLogger.Write(1, 'TAVCodecManager.initializeBSFContext', lmt_Warning, 'Error initilizing bsf!', []);
        end
        else
          LBLogger.Write(1, 'TStreamerManager.initializeBSFContext', lmt_Warning, 'Error coping parameters!');
      end
      else
        LBLogger.Write(1, 'TStreamerManager.initializeBSFContext', lmt_Warning, 'Not enable to alloc bsf for filter "%s"!', [aFilter]);

      if (not _Go) and (Result <> nil) then
      begin
        Self.av_bsf_free(@Result);
        Result := nil;
      end;
    end
    else
      LBLogger.Write(1, 'TStreamerManager.initializeBSFContext', lmt_Warning, 'Not enable to find filter "%s"!', [aFilter]);

  except
    on E: Exception do
      LBLogger.Write(1, 'TAVCodecManager.initializeBSFContext', lmt_Error, PChar(E.Message));
  end;

end;

end.
