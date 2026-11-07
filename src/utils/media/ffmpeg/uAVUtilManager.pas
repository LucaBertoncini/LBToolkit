unit uAVUtilManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAVLibraryBaseManager, libavutil, {$IFDEF Unix}BaseUnix{$ELSE} ctypes{$ENDIF};


type

  { TAVUtilManager }

  TAVUtilManager = class(TAVLibraryBaseManager)
    strict private
      FAv_frame_free : Tav_frame_free;
      Fav_strerror   : Tav_strerror;

     public
       constructor Create(aPath: String); override;

       procedure av_frame_free(var frame: PAVFrame);
       function av_strerror(aRes: Integer): AnsiString;

       var
         AVERROR_AGAIN                      : Integer;
         av_malloc                          : Tav_malloc;
         av_mallocz                         : Tav_mallocz;
         av_image_alloc                     : Tav_image_alloc;

         av_freep                           : Tav_freep;
         av_free                            : Tav_free;

         av_frame_alloc                     : Tav_frame_alloc;
         av_frame_unref                     : Tav_frame_unref;

         av_channel_layout_default          : Tav_channel_layout_default;
         av_channel_layout_from_mask        : Tav_channel_layout_from_mask;
         av_get_bytes_per_sample            : Tav_get_bytes_per_sample;
         av_frame_get_buffer                : Tav_frame_get_buffer;

         av_dict_get                        : Tav_dict_get;
         av_dict_set                        : Tav_dict_set;
         av_dict_free                       : Tav_dict_free;

         av_int_list_length_for_size        : Tav_int_list_length_for_size;
         av_rescale_q                       : Tav_rescale_q;
         av_rescale_rnd                     : Tav_rescale_rnd;
         av_samples_get_buffer_size         : Tav_samples_get_buffer_size;
//         av_strerror                        : Tav_strerror;
         av_strlcpy                         : Tav_strlcpy;

         av_opt_set_bin                     : Tav_opt_set_bin;
         av_opt_set                         : Tav_opt_set;
         av_opt_set_int                     : Tav_opt_set_int;
         av_opt_set_sample_fmt              : Tav_opt_set_sample_fmt;

         av_opt_set_chlayout                : Tav_opt_set_chlayout;

         av_get_sample_fmt_name             : Tav_get_sample_fmt_name;
         av_sample_fmt_is_planar            : Tav_sample_fmt_is_planar;

         av_gcd                             : Tav_gcd;

         av_buffer_ref                      : Tav_buffer_ref;
         av_buffer_unref                    : Tav_buffer_unref;

         av_hwdevice_find_type_by_name      : Tav_hwdevice_find_type_by_name;
         av_hwdevice_iterate_types          : Tav_hwdevice_iterate_types;
         av_hwdevice_get_type_name          : Tav_hwdevice_get_type_name;
         av_hwdevice_ctx_create             : Tav_hwdevice_ctx_create;
         av_hwframe_transfer_data           : Tav_hwframe_transfer_data;

         av_frame_copy_props                : Tav_frame_copy_props;

  end;


{..$I libavutil_error.pas}


implementation


uses
  ULBLogger;

{ TAVUtilManager }

constructor TAVUtilManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

  FLibraryLoader.addFunction('av_malloc',                          @av_malloc);
  FLibraryLoader.addFunction('av_mallocz',                         @av_mallocz);
  FLibraryLoader.addFunction('av_image_alloc',                     @av_image_alloc);

  FLibraryLoader.addFunction('av_freep',                           @av_freep);
  FLibraryLoader.addFunction('av_free',                            @av_free);

  FLibraryLoader.addFunction('av_frame_alloc',                     @av_frame_alloc);
  FLibraryLoader.addFunction('av_frame_unref',                     @av_frame_unref);
  FLibraryLoader.addFunction('av_frame_free',                      @FAv_frame_free);

  FLibraryLoader.addFunction('av_dict_get',                        @av_dict_get);
  FLibraryLoader.addFunction('av_dict_set',                        @av_dict_set);
  FLibraryLoader.addFunction('av_dict_free',                       @av_dict_free);

  FLibraryLoader.addFunction('av_channel_layout_default',          @av_channel_layout_default);
  FLibraryLoader.addFunction('av_channel_layout_from_mask',        @av_channel_layout_from_mask);
  FLibraryLoader.addFunction('av_get_bytes_per_sample',            @av_get_bytes_per_sample);
  FLibraryLoader.addFunction('av_frame_get_buffer',                @av_frame_get_buffer);

  FLibraryLoader.addFunction('av_int_list_length_for_size',        @av_int_list_length_for_size);
  FLibraryLoader.addFunction('av_rescale_q',                       @av_rescale_q);
  FLibraryLoader.addFunction('av_rescale_rnd',                     @av_rescale_rnd);
  FLibraryLoader.addFunction('av_samples_get_buffer_size',         @av_samples_get_buffer_size);
  FLibraryLoader.addFunction('av_strerror',                        @Fav_strerror);
  FLibraryLoader.addFunction('av_strlcpy',                         @av_strlcpy);

  FLibraryLoader.addFunction('av_opt_set_bin',                     @av_opt_set_bin);
  FLibraryLoader.addFunction('av_opt_set',                         @av_opt_set);
  FLibraryLoader.addFunction('av_opt_set_int',                     @av_opt_set_int);
  FLibraryLoader.addFunction('av_opt_set_sample_fmt',              @av_opt_set_sample_fmt);
  FLibraryLoader.addFunction('av_opt_set_chlayout',                @av_opt_set_chlayout);


  FLibraryLoader.addFunction('av_get_sample_fmt_name',             @av_get_sample_fmt_name);
  FLibraryLoader.addFunction('av_sample_fmt_is_planar',            @av_sample_fmt_is_planar);

  FLibraryLoader.addFunction('av_gcd',                             @av_gcd);
  FLibraryLoader.addFunction('av_buffer_ref',                      @av_buffer_ref);
  FLibraryLoader.addFunction('av_buffer_unref',                    @av_buffer_unref);

  FLibraryLoader.addFunction('av_hwdevice_find_type_by_name',      @av_hwdevice_find_type_by_name);
  FLibraryLoader.addFunction('av_hwdevice_iterate_types',          @av_hwdevice_iterate_types);
  FLibraryLoader.addFunction('av_hwdevice_get_type_name',          @av_hwdevice_get_type_name);
  FLibraryLoader.addFunction('av_hwdevice_ctx_create',             @av_hwdevice_ctx_create);
  FLibraryLoader.addFunction('av_hwframe_transfer_data',           @av_hwframe_transfer_data);

  FLibraryLoader.addFunction('av_frame_copy_props',                @av_frame_copy_props);


  FInitialized := FLibraryLoader.LoadLibrary(avutilLibName, _Error);

  if FInitialized then
    AVERROR_AGAIN := -ESysEAGAIN
  else
    LBLogger.Write(1, 'TAVUtilManager.Create', lmt_Warning, 'Error loading library <%s>: <%s>', [avutilLibName, _Error]);

end;

procedure TAVUtilManager.av_frame_free(var frame: PAVFrame);
begin
  if frame <> nil then
  begin
    FAv_frame_free(@frame);
    frame := nil;
  end;
end;

(*
function TAVUtilManager.AVERROR(e: integer): integer;
begin
  AVERROR := AVERROR_SIGN * e;
end;
*)

function TAVUtilManager.av_strerror(aRes: Integer): AnsiString;
var
  _errBuff : array [0 .. 511] of Byte;

begin
  Fav_strerror(aRes, @_errBuff[0], Length(_errBuff));
  Result := AnsiString(PChar(@_errBuff[0]));
end;


end.
