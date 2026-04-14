unit uAVStructures;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}
{$modeswitch advancedrecords}

(*
  uAVStructures - FFmpeg wrapper for Free Pascal

  Tested with:
    FFmpeg 7.x (libavcodec.so.61, libavformat.so.61, libavutil.so.59)
    Free Pascal 3.2.x
    Target platforms: Linux ARM (Raspberry Pi 4/5, Arduino Uno Q)

  Dependencies:
    - PilotLogic FFmpeg Pascal bindings (libavformat.pas, libavcodec.pas,
      libavutil.pas, libswscale.pas)
    - uIPCUtils (LBToolkit)

  Known FFmpeg behaviors not in official docs:
    - size=0 packets (flush packets) must be filtered before avcodec_send_packet
      or the decoder will be permanently invalidated (AVERROR_EOF from codec)
    - h264_v4l2m2m is the hardware decoder on ARM Linux via V4L2 M2M interface
    - AVInterruptCallback is registered but may not be called on all FFmpeg builds
*)

interface

uses
  Classes, SysUtils, uLBBaseThread, uTimedoutCriticalSection, uFFmpeg, uFFmpeg.Types, cTypes, fgl;


type
  TAVReadResult = (rr_OK       = 0,
                   rr_EOF      = 1,
                   rr_TryAgain = 2,
                   rr_Error    = 3);

  TAVStreamType = (st_Unknown   = 0,
                   st_Audio     = 1,
                   st_Video     = 2,
                   st_Subtitles = 3);

  TAVInternalCodecType = (ict_Unknown   = 0,
                          ict_H264      = 1,
                          ict_H265      = 2,
                          ict_VORBIS    = 3,
                          ict_OPUS      = 4,
                          ict_ALAW      = 5,
                          ict_MULAW     = 6,
                          ict_MP3       = 7,
                          ict_AAC       = 8,
                          ict_WMAV1     = 9,
                          ict_WMAV2     = 10,
                          ict_GSM       = 11,
                          ict_G723      = 12,
                          ict_G729      = 13,
                          ict_PCM_S16LE = 14,
                          ict_PCM_S16BE = 15,
                          ict_G722      = 16);

  TAVInternalPixelFormat = (ipf_Unknown   = 0,
                            ipf_YUV420P   = 1,
                            ipf_YUYV422   = 2,
                            ipf_RGB24     = 3,
                            ipf_BGR24     = 4,
                            ipf_YUV422P   = 5,
                            ipf_YUV444P   = 6,
                            ipf_YUV410P   = 7,
                            ipf_YUV411P   = 8,
                            ipf_GRAY8     = 9,
                            ipf_ARGB      = 10,
                            ipf_RGBA      = 11,
                            ipf_ABGR      = 12,
                            ipf_BGRA      = 13,
                            ipf_YUVJ420P  = 14,
                            ipf_YUVJ422P  = 15,
                            ipf_YUVJ444P  = 16,
                            ipf_UYVY422   = 17,
                            ipf_UYYVYY411 = 18);

  TAVDictionaryObj = specialize TFPGMap<AnsiString, AnsiString>;

  TAVPacketWrapper = class;

  IAVPacketWrapper = Interface(IInterface)
    ['{5BCDD4A0-8150-476F-BAE7-E60954831C1B}']
    function getRelatedObject(): TAVPacketWrapper;
  end;

  { TAVPacketWrapper }

  TAVPacketWrapper = class(TInterfacedObject, IAVPacketWrapper)
    public
      type
        TAVPacketType = (avpt_Unknown = 0,
                         avpt_Audio   = 1,
                         avpt_Video   = 2);
    strict private
      FPacket     : PAVPacket;
      FPacketType : TAVPacketType;

    public
      constructor Create(aPayloadSize: Integer = 0);
      constructor CreateFromH264NAL(aBuffer: PByte; aBuffLen: Integer);
      destructor Destroy; override;

      function getRelatedObject(): TAVPacketWrapper;
      function createNewPacket(aPayloadSize: Integer = 0): Boolean;
      function destroyPacket(): Boolean;
      function clearPacket(): Boolean;
      function CopyFrom(aPacket: IAVPacketWrapper): Boolean;

      property Packet: PAVPacket read FPacket;
      property PacketType: TAVPacketType read FPacketType write FPacketType;
  end;

  TAVPacketWrapperList = specialize TFPGInterfacedObjectList<TAVPacketWrapper>;

  { TAVPacketHelper }

  TAVPacketHelper = record helper for TAVPacket
    function ContainsKeyFrame(): Boolean;
    function Clone(aPacket: PAVPacket): Boolean;
  end;

  TAVFrameWrapper = class;

  IAVFrameWrapper = Interface(IInterface)
    ['{ABE59A7D-F070-4368-A15B-3CDE39CEA4BF}']
    function getRelatedObject(): TAVFrameWrapper;
  end;

  { TAVFrameWrapper }

  TAVFrameWrapper = class(TInterfacedObject, IAVFrameWrapper)
    strict private
      FFrame              : PAVFrame;
      FOwnsData           : Boolean;
      FUsed_av_image_alloc: Boolean;

      function destroyFrame(): Boolean;

    public
      constructor Create;
      destructor Destroy; override;

      function getRelatedObject(): TAVFrameWrapper;
      function clearFrame(): Boolean;

      property Frame: PAVFrame read FFrame;
      property OwnsData: Boolean write FOwnsData;
      property Used_av_image_alloc: Boolean write FUsed_av_image_alloc;
  end;

  { TAVSourceReader }

  TAVSourceReader = class(TObject)
    public
      type

       { TAVInputInfo }

        TAVInputInfo = class(TObject)
          strict private
            FFileName        : String;
            FFileSize        : Int64;
            FFileFormat      : String;
            FProgramCount    : Integer;
            FVideoStreams    : TList;
            FAudioStreams    : TList;
            FSubtitleStreams : TList;
            FStreams         : TList;
            FStreamsTypes    : array of TAVStreamType;
            FStartTime       : Int64;
            FDuration        : Int64;
            FBitRate         : Integer;
            FTimeStamp       : Int64;
            FYear            : Integer;
            FTrack           : Integer;
            FTitle           : string;
            FAuthor          : string;
            FCopyright       : string;
            FComment         : string;
            FAlbum           : string;
            FGenre           : string;

            function get_DataAsText: TStringList;
            procedure Reset();
            procedure CalculateDuration(anAVFormatContext: PAVFormatContext);

          public
            destructor Destroy; override;

            function getStreamTypeByStreamIndex(anIdx: Integer): TAVStreamType;
            function FillFromContext(const aFilename: String; anAVFormatContext: PAVFormatContext): Boolean;

            property Duration: Int64 read FDuration;
            property Filename: String read FFileName;
            property TimeStamp: Int64 read FTimeStamp;
            property AudioStreams: TList read FAudioStreams;
            property VideoStreams: TList read FVideoStreams;
            property AllStreams: TList read FStreams;
            property DataAsText: TStringList read get_DataAsText;
        end;

        TAVInterruptData = record
          LastPacketTime : Int64;   // timestamp in ms dell'ultimo pacchetto ricevuto
          TimeoutMs      : Int64;   // timeout in ms (es. 10000 = 10 secondi)
        end;
        PAVInterruptData = ^TAVInterruptData;

    strict private
      FInterruptData: TAVInterruptData;
      procedure CloseInput();

    strict protected
      FAVFormatContext : PAVFormatContext;
      FInputInfo       : TAVInputInfo;

    public
      constructor Create();
      destructor Destroy(); override;

      function OpenInput(const anURL: AnsiString; out anErrorMsg: String; aDictionary: TAVDictionaryObj = nil): Boolean;
      function ReadStreamPacket(var aPacket: IAVPacketWrapper; aStreamIdx: Integer): Boolean;
      function ReadPacket(var aPacket: IAVPacketWrapper; out aStreamType: TAVStreamType; out aStatus: TAVReadResult): Boolean;
      function SeekVideoFrame(anAbsoluteStreamIdx: Integer; aPos: Int64): Boolean;

      property InputInfo: TAVInputInfo read FInputInfo;
  end;

  { TAVFileReader }

  TAVFileReader = class(TAVSourceReader)
    public
      function OpenFile(const AFile: AnsiString; out anErrorMsg: String): Boolean;
      function seek(aStreamIdx: Integer; aTimestamp: Int64): Boolean;
  end;

  TBufferArray        = array [0 .. AV_NUM_DATA_POINTERS - 1] of TBytes;
  PBufferArray        = ^TBufferArray;
  TBufferArrayPointer = array [0 .. AV_NUM_DATA_POINTERS - 1] of pByte;

  { TAVDecoder }

  TAVDecoder = class(TObject)
    strict private
      function buildHWCandidateList(aCodecId: TAVCodecID): TStringList;
      function ProbeDecoder(aDecoder: PAVCodec; aCodecId: TAVCodecID; aDeviceType: TAVHWDeviceType): Boolean;

    strict protected
      FHWAccelType    : TAVHWDeviceType;
      FAVCodecContext : PAVCodecContext;
      FInputTimeBase  : TAVRational;
      FHWPixelFormat  : TAVPixelFormat;
      FHWDeviceCtx    : PAVBufferRef;
      FDecodedFrame   : IAVFrameWrapper;

      function retrieveVideoDecoder(aCodecId: TAVCodecID; out HWAcceleration: Boolean): PAVCodec;
      function elaborateDecodedFrame(var aFrame: IAVFrameWrapper): Boolean; virtual;
      function setHardwarePixelFormat(aDecoder: PAVCodec): Boolean;
      function initializeHWDecoder(): Boolean;

      // Precondizione: FAVCodecContext già allocato e popolato dal chiamante.
      // Gestisce HW context, flags, avcodec_open2, flush.
      // In caso di errore libera FAVCodecContext e restituisce False.
      function openVideoDecoderContext(aDecoder: PAVCodec; const aTimeBase: TAVRational; aHWAccel: Boolean; out anErrorMsg: String): Boolean;

    public
      constructor Create(); virtual;
      destructor Destroy(); override;

      procedure ClearDecoderContext(); virtual;

      function rescalePTS2ms(aPTS: Int64): Int64;
      function ms2PTS(aTimecode: Int64): Int64;
      function setHWAccelType(const aDeviceType: String): Boolean;
      function InitDecoder_ByInputStream(anAVStream: PAVStream; out anErrorMsg: String): Boolean;
      function decodePacket(aPacket: PAVPacket; out NeedMore: Boolean; out Error: Boolean): Boolean;

      function getDecodedFrame(out aDecodedFrame: IAVFrameWrapper): Boolean;

      property HWPixelFormat: TAVPixelFormat read FHWPixelFormat;
  end;

  { TAVNetworkDecoder
    Extends TAVDecoder with the ability to initialize a decoder from
    serialized network header structures (TAudioHeader / TVideoHeader).
    Use this as base class when you need to decode streams whose codec
    parameters arrive via network instead of a local AVStream. }

  TAVNetworkDecoder = class(TAVDecoder)
    public
      type
        TAudioHeader = packed record
          StreamIdx     : Byte;
          CodecId       : Integer;
          SampleRate    : Integer;
          SampleFormat  : Integer;
          Channels      : Integer;
          ChLayout      : Int64;
          BlockAlign    : Integer;
          ExtraDataSize : Integer;
          time_base     : TAVRational;
          frame_size    : Integer;
        end;
        pAudioHeader = ^TAudioHeader;

        TVideoHeader = packed record
          StreamIdx           : Byte;
          CodecId             : Integer;
          ExtraDataSize       : Integer;
          Width               : Integer;
          Height              : Integer;
          PixelFormat         : Integer;
          FrameRate           : TAVRational;
          sample_aspect_ratio : TAVRational;
          time_base           : TAVRational;
        end;
        pVideoHeader = ^TVideoHeader;

        THeaderType = (ht_Unknown = 0,
                       ht_Audio   = 1,
                       ht_Video   = 2);

    strict private
      function InitDecoder_ByAudioHeader(anHeader: pAudioHeader; ExtraData: pByte; out anErrorMsg: String): Boolean;
      function InitDecoder_ByVideoHeader(anHeader: pVideoHeader; ExtraData: pByte; out anErrorMsg: String): Boolean;


    public
      function InitDecoder_ByNetworkData(anHeader: pAudioHeader; ExtraData: pByte; out anErrorMsg: String): Boolean; overload;
      function InitDecoder_ByNetworkData(anHeader: pVideoHeader; ExtraData: pByte; out anErrorMsg: String): Boolean; overload;
  end;

  { TAudioParams }

  TAudioParams = class(TObject)
    strict private
      function get_Bytes4Sample: Integer;

    strict protected
      FSampleRate    : Integer;
      FChannels      : Integer;
      FSampleFormat  : TAVSampleFormat;
      FChannelLayout : Int64;

      function get_NeededPlanes(): Integer;
      function get_ByteResolutionPerSample: Integer;

    public
      function isEqual(anAudioParams: TAudioParams): Boolean;
      function getEmptyFrame(nb_samples: cint; allocateBuffers: Boolean): IAVFrameWrapper;

      property SampleRate    : Integer         read FSampleRate    write FSampleRate;
      property Channels      : Integer         read FChannels      write FChannels;
      property SampleFormat  : TAVSampleFormat read FSampleFormat  write FSampleFormat;
      property ChannelLayout : Int64           read FChannelLayout write FChannelLayout;

      property ByteResolutionPerSample : Integer read get_ByteResolutionPerSample;
      property Bytes4Sample : Integer read get_Bytes4Sample;
  end;

  { TBufferAudioParams }

  TBufferAudioParams = class(TAudioParams)
    strict private
      FBufferMsDuration : Int64;
      procedure setDefaultValues();

    public
      constructor Create;

      function resampleNeeded(aFrame: PAVFrame; out FrameChannelLayout: Int64): Boolean;
      function resampleNeeded(anInputParams: TAudioParams): Boolean; overload;
      function createBuffers(var aBuffersArray: TBufferArray; msFullDuration: Int64; samplesFullDuration: Int64 = 0): Boolean;

      property msDuration: Int64 read FBufferMsDuration;
  end;

  { TAudioBufferData }

  TAudioBufferData = class(TAVDecoder)
    strict private
      FResampler       : PSwrContext;
      FBuffersPointers : TBufferArrayPointer;

      function checkEncoderCompatibility(anEncoder: PAVCodec): Boolean;
      function get_DurationByBufferOffset: Int64;
      function get_FullDuration: Int64;
      function insertDecodedFrameIntoBufferByByteOffset(const aDecodedFrame: IAVFrameWrapper): Boolean;
      function msToBytesOffset(msTimeOffset: Int64): Int64;

    strict protected
      FAudioParams  : TBufferAudioParams;
      FBuffers      : TBufferArray;
      FBufferOffset : Int64;

      function elaborateDecodedFrame(var aFrame: IAVFrameWrapper): Boolean; override;
      procedure elaborateNewData(StartBytePos, Size: Int64); virtual;

    public
      constructor Create(); override;
      destructor Destroy; override;

      procedure ClearDecoderContext(); override;

      function createBuffers(msAudioDuration: Int64): Boolean;
      procedure destroyBuffers();
      function setOffsetByTime(msTimeOffset: Int64): Boolean;
      function incOffset(anOffset: Int64): Boolean;

      property AudioParams: TBufferAudioParams read FAudioParams;
      property BufferOffset: Int64 read FBufferOffset write FBufferOffset;
      property Buffers: TBufferArray read FBuffers;
      property DurationByBufferOffset: Int64 read get_DurationByBufferOffset;
      property fullDuration: Int64 read get_FullDuration;
  end;

  { TVideoDecoder }

  TVideoDecoder = class(TAVDecoder)
    strict private
      FAspectRatio  : Single;

    private
      function get_AspectRatio: Single;

    const
      AR_4_3_Value  = 1.333333333333333;
      AR_16_9_Value = 1.777777777777778;

    public
      constructor Create(); override;
      destructor Destroy(); override;

      function decodePacket(pkt: IAVPacketWrapper; flushBuffers: Boolean; out DecodedTimecode: Int64; out isKeyFrame: Boolean; out NeedMore: Boolean): Boolean; reintroduce;

      property CodecContext: PAVCodecContext read FAVCodecContext;
      property AspectRatio: Single read get_AspectRatio;
  end;

  { TAVSampleFormatHelper }

  TAVSampleFormatHelper = type helper for TAVSampleFormat
    function BytesPerSample(): Integer;
  end;

  function AVPixelFormat2InternalPixelFormat(aPixelFormat: TAVPixelFormat): TAVInternalPixelFormat;
  function AVCodecId2InternalCodecId(aAVCodec: TAVCodecID): TAVInternalCodecType;

implementation

uses
  FileUtil, ULBLogger, Math, StrUtils, libavcodec;


type
  { TAVCodecContextHelper }

  TAVCodecContextHelper = type helper for TAVCodecContext
    function asString(): String;
  end;


  TAVHWDecoderEntry = record
    DeviceType : TAVHWDeviceType;   // tipo device FFmpeg (CUDA, VAAPI, …)
    H264Name   : AnsiString;        // nome decoder per AV_CODEC_ID_H264
    HEVCName   : AnsiString;        // nome decoder per AV_CODEC_ID_HEVC
  end;

const
  // Decoder che richiedono un hw_device_ctx (AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX)
  // Ordinati per priorità generale: CUDA > VAAPI > D3D11VA > DXVA2 > QSV > VT
  cHWDecoderMap : array[0..6] of TAVHWDecoderEntry = (
    (DeviceType: AV_HWDEVICE_TYPE_CUDA;         H264Name: 'h264_cuvid';        HEVCName: 'hevc_cuvid'),
    (DeviceType: AV_HWDEVICE_TYPE_VAAPI;        H264Name: 'h264_vaapi';        HEVCName: 'hevc_vaapi'),
    (DeviceType: AV_HWDEVICE_TYPE_D3D11VA;      H264Name: 'h264_d3d11va';      HEVCName: 'hevc_d3d11va'),
    (DeviceType: AV_HWDEVICE_TYPE_DXVA2;        H264Name: 'h264_dxva2';        HEVCName: ''),
    (DeviceType: AV_HWDEVICE_TYPE_QSV;          H264Name: 'h264_qsv';          HEVCName: 'hevc_qsv'),
    (DeviceType: AV_HWDEVICE_TYPE_VIDEOTOOLBOX; H264Name: 'h264_videotoolbox'; HEVCName: 'hevc_videotoolbox'),
    (DeviceType: AV_HWDEVICE_TYPE_DRM;          H264Name: 'h264';              HEVCName: 'hevc')
  );

  // Decoder che usano HW interno senza device_ctx (v4l2m2m, mmal)
  // Vengono aggiunti PRIMA della lista device-ctx perché su ARM sono i più veloci
  cInternalHW_H264 : array[0..1] of AnsiString = ('h264_v4l2m2m', 'h264_mmal');
  cInternalHW_HEVC : array[0..0] of AnsiString = ('hevc_v4l2m2m');


function AVPixelFormat2InternalPixelFormat(aPixelFormat: TAVPixelFormat): TAVInternalPixelFormat;
begin
  case aPixelFormat of
    AV_PIX_FMT_NONE      : Result := ipf_Unknown;
    AV_PIX_FMT_YUV420P   : Result := ipf_YUV420P;
    AV_PIX_FMT_YUYV422   : Result := ipf_YUYV422;
    AV_PIX_FMT_RGB24     : Result := ipf_RGB24;
    AV_PIX_FMT_BGR24     : Result := ipf_BGR24;
    AV_PIX_FMT_YUV422P   : Result := ipf_YUV422P;
    AV_PIX_FMT_YUV444P   : Result := ipf_YUV444P;
    AV_PIX_FMT_YUV410P   : Result := ipf_YUV410P;
    AV_PIX_FMT_YUV411P   : Result := ipf_YUV411P;
    AV_PIX_FMT_GRAY8     : Result := ipf_GRAY8;
    AV_PIX_FMT_ARGB      : Result := ipf_ARGB;
    AV_PIX_FMT_RGBA      : Result := ipf_RGBA;
    AV_PIX_FMT_ABGR      : Result := ipf_ABGR;
    AV_PIX_FMT_BGRA      : Result := ipf_BGRA;
    AV_PIX_FMT_YUVJ420P  : Result := ipf_YUVJ420P;
    AV_PIX_FMT_YUVJ422P  : Result := ipf_YUVJ422P;
    AV_PIX_FMT_YUVJ444P  : Result := ipf_YUVJ444P;
    AV_PIX_FMT_UYVY422   : Result := ipf_UYVY422;
    AV_PIX_FMT_UYYVYY411 : Result := ipf_UYYVYY411;
    else begin
      Result := ipf_Unknown;
      LBLogger.Write(1, 'AVPixelFormat2InternalPixelFormat', lmt_Warning, 'Unknown pixel format %d!', [Integer(aPixelFormat)]);
    end;
  end;
end;

function InternalPixelFormat2AVPixelFormat(aPixelFormat: TAVInternalPixelFormat): TAVPixelFormat;
begin
  case aPixelFormat of
    ipf_Unknown   : Result := AV_PIX_FMT_NONE;
    ipf_YUV420P   : Result := AV_PIX_FMT_YUV420P;
    ipf_YUYV422   : Result := AV_PIX_FMT_YUYV422;
    ipf_RGB24     : Result := AV_PIX_FMT_RGB24;
    ipf_BGR24     : Result := AV_PIX_FMT_BGR24;
    ipf_YUV422P   : Result := AV_PIX_FMT_YUV422P;
    ipf_YUV444P   : Result := AV_PIX_FMT_YUV444P;
    ipf_YUV410P   : Result := AV_PIX_FMT_YUV410P;
    ipf_YUV411P   : Result := AV_PIX_FMT_YUV411P;
    ipf_GRAY8     : Result := AV_PIX_FMT_GRAY8;
    ipf_ARGB      : Result := AV_PIX_FMT_ARGB;
    ipf_RGBA      : Result := AV_PIX_FMT_RGBA;
    ipf_ABGR      : Result := AV_PIX_FMT_ABGR;
    ipf_BGRA      : Result := AV_PIX_FMT_BGRA;
    ipf_YUVJ420P  : Result := AV_PIX_FMT_YUVJ420P;
    ipf_YUVJ422P  : Result := AV_PIX_FMT_YUVJ422P;
    ipf_YUVJ444P  : Result := AV_PIX_FMT_YUVJ444P;
    ipf_UYVY422   : Result := AV_PIX_FMT_UYVY422;
    ipf_UYYVYY411 : Result := AV_PIX_FMT_UYYVYY411;
  end;
end;

function AVCodecId2InternalCodecId(aAVCodec: TAVCodecID): TAVInternalCodecType;
begin
  case aAVCodec of
    AV_CODEC_ID_NONE       : Result := ict_Unknown;
    AV_CODEC_ID_H264       : Result := ict_H264;
    AV_CODEC_ID_HEVC       : Result := ict_H265;
    AV_CODEC_ID_MP3        : Result := ict_MP3;
    AV_CODEC_ID_AAC        : Result := ict_AAC;
    AV_CODEC_ID_VORBIS     : Result := ict_VORBIS;
    AV_CODEC_ID_WMAV1      : Result := ict_WMAV1;
    AV_CODEC_ID_WMAV2      : Result := ict_WMAV2;
    AV_CODEC_ID_GSM        : Result := ict_GSM;
    AV_CODEC_ID_G723_1     : Result := ict_G723;
    AV_CODEC_ID_G729       : Result := ict_G729;
    AV_CODEC_ID_OPUS       : Result := ict_OPUS;
    AV_CODEC_ID_PCM_S16LE  : Result := ict_PCM_S16LE;
    AV_CODEC_ID_PCM_S16BE  : Result := ict_PCM_S16BE;
    AV_CODEC_ID_PCM_MULAW  : Result := ict_MULAW;
    AV_CODEC_ID_PCM_ALAW   : Result := ict_ALAW;
    AV_CODEC_ID_ADPCM_G722 : Result := ict_G722;
    else begin
      Result := ict_Unknown;
      LBLogger.Write(1, 'AVCodecId2InternalCodecId', lmt_Warning, 'Unknown codec: %d', [Integer(aAVCodec)]);
    end;
  end;
end;

function InternalCodecId2AVCodecId(aInternalCodec: TAVInternalCodecType): TAVCodecID;
begin
  case aInternalCodec of
    ict_Unknown   : Result := AV_CODEC_ID_NONE;
    ict_H264      : Result := AV_CODEC_ID_H264;
    ict_H265      : Result := AV_CODEC_ID_HEVC;
    ict_MP3       : Result := AV_CODEC_ID_MP3;
    ict_AAC       : Result := AV_CODEC_ID_AAC;
    ict_VORBIS    : Result := AV_CODEC_ID_VORBIS;
    ict_WMAV1     : Result := AV_CODEC_ID_WMAV1;
    ict_WMAV2     : Result := AV_CODEC_ID_WMAV2;
    ict_GSM       : Result := AV_CODEC_ID_GSM;
    ict_G723      : Result := AV_CODEC_ID_G723_1;
    ict_G729      : Result := AV_CODEC_ID_G729;
    ict_OPUS      : Result := AV_CODEC_ID_OPUS;
    ict_G722      : Result := AV_CODEC_ID_ADPCM_G722;
    ict_PCM_S16LE : Result := AV_CODEC_ID_PCM_S16LE;
    ict_PCM_S16BE : Result := AV_CODEC_ID_PCM_S16BE;
    ict_MULAW     : Result := AV_CODEC_ID_PCM_MULAW;
    ict_ALAW      : Result := AV_CODEC_ID_PCM_ALAW;
  end;
end;

Function GetMetaVal(const aDict: PAVDictionary; const AKey: AnsiString; const ADefault: string = ''): string;
Var
  _Tag : PAVDictionaryEntry;
Begin
  _Tag := gv_FFmpeg.AVUtil.av_dict_get(aDict, PAnsiChar(AKey), nil, 0);
  if _Tag <> nil then
    Result := string(_Tag^.value)
  else
    Result := ADefault;
end;

{ TAVSampleFormatHelper }

function TAVSampleFormatHelper.BytesPerSample: Integer;
begin
  case Self of
    AV_SAMPLE_FMT_NONE,
    AV_SAMPLE_FMT_NB    : Result := 0;
    AV_SAMPLE_FMT_U8,
    AV_SAMPLE_FMT_U8P   : Result := 1;
    AV_SAMPLE_FMT_S16,
    AV_SAMPLE_FMT_S16P  : Result := 2;
    AV_SAMPLE_FMT_S32,
    AV_SAMPLE_FMT_S32P  : Result := 4;
    AV_SAMPLE_FMT_FLT,
    AV_SAMPLE_FMT_DBL,
    AV_SAMPLE_FMT_FLTP,
    AV_SAMPLE_FMT_DBLP  : Result := 8;
    AV_SAMPLE_FMT_S64,
    AV_SAMPLE_FMT_S64P  : Result := 8;
  end;
end;

{ TAVFileReader }

function TAVFileReader.OpenFile(const AFile: AnsiString; out anErrorMsg: String): Boolean;
begin
  Result := False;
  if FileExists(AFile) then
    Result := Self.OpenInput(AFile, anErrorMsg)
  else
    LBLogger.Write(1, 'TAVFileReader.OpenFile', lmt_Warning, 'File <%s> not found!', [AFile]);
end;

function TAVFileReader.seek(aStreamIdx: Integer; aTimestamp: Int64): Boolean;
var
  _res : Integer;
begin
  _res := gv_FFmpeg.AVFormat.av_seek_frame(FAVFormatContext, aStreamIdx, aTimestamp, 0);
  if _res >= 0 then
    Result := True
  else begin
    Result := False;
    LBLogger.Write(1, 'TAVFileReader.seek', lmt_Warning, 'Error seeking file <%s> on stream %d at %d: <%s>', [FInputInfo.Filename, aStreamIdx, aTimestamp, gv_FFmpeg.AVUtil.av_strerror(_res)]);
  end;
end;

{ TAVPacketHelper }

function TAVPacketHelper.ContainsKeyFrame: Boolean;
begin
  Result := (flags and AV_PKT_FLAG_KEY) = AV_PKT_FLAG_KEY;
end;

function TAVPacketHelper.Clone(aPacket: PAVPacket): Boolean;
var
  _cloned: PAVPacket;
begin
  Result := False;
  if (aPacket <> nil) then
  begin
    _cloned := gv_FFmpeg.AVCodec.av_packet_clone(aPacket);
    if _cloned <> nil then
    begin
      gv_FFmpeg.AVCodec.av_packet_unref(@Self);
      gv_FFmpeg.AVCodec.av_packet_ref(@Self, _cloned);
      gv_FFmpeg.AVCodec.av_packet_free(@_cloned);
      Result := True;
    end;
  end;
end;

{ TAVFrameWrapper }

function TAVFrameWrapper.destroyFrame(): Boolean;
begin
  Result := True;
  if Self.clearFrame() then
    gv_FFmpeg.AVUtil.av_frame_free(FFrame);
end;

constructor TAVFrameWrapper.Create;
begin
  inherited Create;
  FFrame := gv_FFmpeg.AVUtil.av_frame_alloc();
  FOwnsData := True;
  FUsed_av_image_alloc := False;
end;

destructor TAVFrameWrapper.Destroy;
begin
  Self.destroyFrame();
  inherited Destroy;
end;

function TAVFrameWrapper.getRelatedObject(): TAVFrameWrapper;
begin
  Result := Self;
end;

function TAVFrameWrapper.clearFrame(): Boolean;
var
  i : Integer;
begin
  Result := False;
  if FFrame <> nil then
  begin
    try
      if FOwnsData then
      begin
        if FUsed_av_image_alloc then
          gv_FFmpeg.AVUtil.av_freep(@FFrame^.data[0]);
      end;
      gv_FFmpeg.AVUtil.av_frame_unref(FFrame);
      Result := True;
    except
      on E: Exception do
        LBLogger.Write(1, 'TAVFrameWrapper.clearFrame', lmt_Error, E.Message);
    end;
  end;
end;

{ TAVPacketWrapper }

constructor TAVPacketWrapper.Create(aPayloadSize: Integer);
begin
  inherited Create;
  FPacket := nil;
  Self.createNewPacket(aPayloadSize);
end;

constructor TAVPacketWrapper.CreateFromH264NAL(aBuffer: PByte; aBuffLen: Integer);
const
  NAL_IDR = 5;
var
  _NALType      : Byte;
  i             : Integer;
  _isKeyFrame   : Boolean = False;
  _startCodeLen : Integer;
begin
  inherited Create;
  FPacket := nil;
  if (aBuffer = nil) or (aBuffLen < 5) then Exit;

  i := 0;
  while i < aBuffLen - 4 do
  begin
    if (aBuffer[i] = 0) and (aBuffer[i+1] = 0) then
    begin
      if (aBuffer[i + 2] = 1) then _startCodeLen := 3
      else if (aBuffer[i + 2] = 0) and (aBuffer[i + 3] = 1) then
        _startCodeLen := 4
      else begin
        Inc(i);
        Continue;
      end;
      _nalType := aBuffer[i + _startCodeLen] and $1F;
      if _nalType = NAL_IDR then
      begin
        _isKeyFrame := True;
        Break;
      end;
      Inc(i, _startCodeLen);
    end
    else
      Inc(i);
  end;

  if not Self.createNewPacket(aBuffLen) then Exit;
  Move(aBuffer^, FPacket^.data^, aBuffLen);
  if _isKeyFrame then
    FPacket^.flags := FPacket^.flags or AV_PKT_FLAG_KEY;
end;

destructor TAVPacketWrapper.Destroy;
begin
  Self.destroyPacket();
  inherited Destroy;
end;

function TAVPacketWrapper.getRelatedObject(): TAVPacketWrapper;
begin
  Result := Self;
end;

function TAVPacketWrapper.createNewPacket(aPayloadSize: Integer): Boolean;
var
  _res : Integer;
begin
  Result := False;
  Self.destroyPacket();
  FPacket := gv_FFmpeg.AVCodec.av_packet_alloc();
  if FPacket = nil then Exit;
  if aPayloadSize > 0 then
  begin
    _res := gv_FFmpeg.AVCodec.av_new_packet(FPacket, aPayloadSize);
    if _res <> 0 then
    begin
      LBLogger.Write(1, 'TAVPacketWrapper.createNewPacket', lmt_Warning, 'Could not initialize packet: <%s>!', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
      Self.destroyPacket();
    end;
  end
  else
    gv_FFmpeg.AVCodec.av_init_packet(FPacket);
  Result := True;
end;

function TAVPacketWrapper.destroyPacket(): Boolean;
begin
  Result := True;
  if FPacket <> nil then
    gv_FFmpeg.AVCodec.av_packet_free(@FPacket);
end;

function TAVPacketWrapper.clearPacket(): Boolean;
begin
  Result := False;
  if FPacket = nil then Exit;
  gv_FFmpeg.AVCodec.av_packet_unref(FPacket);
  Result := True;
end;

function TAVPacketWrapper.CopyFrom(aPacket: IAVPacketWrapper): Boolean;
begin
  Result := False;
  if FPacket = nil then
    Self.createNewPacket()
  else
    Self.clearPacket();
  if aPacket <> nil then
    Result := FPacket^.Clone(aPacket.getRelatedObject().Packet);
end;

{ TAVCodecContextHelper }

function TAVCodecContextHelper.asString(): String;
var
  _List : TStringList = nil;
  _tmp  : String;
const
  cMaxLen = Integer(32);
begin
  Result := '';
  try
    _List := TStringList.Create;
    _tmp := AddChar('_', 'codec_type: ', cMaxLen);
    case Self.codec_type of
      AVMEDIA_TYPE_UNKNOWN    : _tmp += 'AVMEDIA_TYPE_UNKNOWN';
      AVMEDIA_TYPE_VIDEO      : _tmp += 'AVMEDIA_TYPE_VIDEO';
      AVMEDIA_TYPE_AUDIO      : _tmp += 'AVMEDIA_TYPE_AUDIO';
      AVMEDIA_TYPE_DATA       : _tmp += 'AVMEDIA_TYPE_DATA';
      AVMEDIA_TYPE_SUBTITLE   : _tmp += 'AVMEDIA_TYPE_SUBTITLE';
      AVMEDIA_TYPE_ATTACHMENT : _tmp += 'AVMEDIA_TYPE_ATTACHMENT';
      AVMEDIA_TYPE_NB         : _tmp += 'AVMEDIA_TYPE_NB';
    end;
    _List.Add(_tmp);
    _List.Add(AddChar('_', 'codec_id: ',     cMaxLen) + IntToStr(Integer(Self.codec_id)));
    _List.Add(AddChar('_', 'width: ',        cMaxLen) + IntToStr(Self.width));
    _List.Add(AddChar('_', 'height: ',       cMaxLen) + IntToStr(Self.height));
    _List.Add(AddChar('_', 'bit_rate: ',     cMaxLen) + IntToStr(Self.bit_rate));
    _List.Add(AddChar('_', 'time_base: ',    cMaxLen) + IntToStr(Self.time_base.num) + '/' + IntToStr(Self.time_base.den));
    _List.Add(AddChar('_', 'pix_fmt: ',      cMaxLen) + IntToStr(Integer(Self.pix_fmt)));
    _List.Add(AddChar('_', 'thread_count: ', cMaxLen) + IntToStr(Self.thread_count));
    _List.Add(AddChar('_', 'sample_rate: ',  cMaxLen) + IntToStr(Self.sample_rate));
    _List.Add(AddChar('_', 'sample_fmt: ',   cMaxLen) + IntToStr(Integer(Self.sample_fmt)));
    _List.Add(AddChar('_', 'frame_size: ',   cMaxLen) + IntToStr(Self.frame_size));
  except
    on E: Exception do
      LBLogger.Write(1, 'TAVCodecContextHelper.asString', lmt_Error, E.Message);
  end;
  if _List <> nil then
  begin
    Result := _List.Text;
    _List.Free;
  end;
end;

{ TAVSourceReader.TAVInputInfo }

procedure TAVSourceReader.TAVInputInfo.Reset();
begin
  try
    FFileName     := '';
    FFileSize     := -1;
    FFileFormat   := '';
    FProgramCount := -1;
    SetLength(FStreamsTypes, 0);
    FreeAndNil(FStreams);
    FreeAndNil(FVideoStreams);
    FreeAndNil(FAudioStreams);
    FreeAndNil(FSubtitleStreams);
    FStartTime := -1;
    FDuration  := -1;
    FBitRate   := -1;
    FTimeStamp := -1;
    FYear      := -1;
    FTrack     := -1;
    FTitle     := '';
    FAuthor    := '';
    FCopyright := '';
    FComment   := '';
    FAlbum     := '';
    FGenre     := '';
  except
    on E: Exception do
      LBLogger.Write(1, 'TAVInputInfo.Reset', lmt_Error, E.Message);
  end;
end;

function TAVSourceReader.TAVInputInfo.get_DataAsText: TStringList;
var
  _AVStream : PAVStream;
  _tmp      : String;
  i         : Integer;
begin
  Result := TStringList.Create;
  Result.Add(AddCharR(' ', 'File', 20) + ': ' + FFileName);
  Result.Add(AddCharR(' ', 'Duration', 20) + ': ' + IntToStr(FDuration));
  Result.Add(AddCharR(' ', 'Bitrate', 20) + ': ' + IntToStr(FBitRate));

  if FAudioStreams <> nil then _tmp := IntToStr(FAudioStreams.Count) else _tmp := '0';
  Result.Add(AddCharR(' ', 'Audio streams', 20) + ': ' + _tmp);

  if FVideoStreams <> nil then _tmp := IntToStr(FVideoStreams.Count) else _tmp := '0';
  Result.Add(AddCharR(' ', 'Video streams', 20) + ': ' + _tmp);

  if (FAudioStreams <> nil) and (FAudioStreams.Count > 0) then
  begin
    Result.Add('');
    for i := 0 to FAudioStreams.Count - 1 do
    begin
      _AVStream := PAVStream(FAudioStreams.Items[i]);
      Result.Add(AddChar('#', 'Audio stream ' + IntToStr(i), 30));
      Result.Add(AddCharR(' ', 'Coded Id', 20) + ': ' + IntToStr(Integer(_AVStream^.codecpar^.codec_id)));
      Result.Add(AddCharR(' ', 'Sample rate', 20) + ': ' + IntToStr(Integer(_AVStream^.codecpar^.sample_rate)));
      Result.Add(AddChar('#', '', 30));
    end;
  end;

  if (FVideoStreams <> nil) and (FVideoStreams.Count > 0) then
  begin
    Result.Add('');
    for i := 0 to FVideoStreams.Count - 1 do
    begin
      _AVStream := PAVStream(FVideoStreams.Items[i]);
      Result.Add(AddChar('#', 'Video stream ' + IntToStr(i), 30));
      Result.Add(AddCharR(' ', 'Coded Id', 20) + ': ' + IntToStr(Integer(_AVStream^.codecpar^.codec_id)));
      Result.Add(AddCharR(' ', 'Duration', 20) + ': ' + IntToStr(Integer(_AVStream^.duration)));
      Result.Add(AddChar('#', '', 30));
    end;
  end;
end;

procedure TAVSourceReader.TAVInputInfo.CalculateDuration(anAVFormatContext: PAVFormatContext);
var
  _FileDuration         : Int64;
  _AVStream             : PAVStream;
  _AudioStreamDuration  : Int64 = 0;
  _VideoStreamDuration  : Int64 = 0;

begin
  try
    If anAVFormatContext^.Duration >= gv_FFmpeg.cFFScale Then
      _FileDuration := Round(anAVFormatContext^.Duration / gv_FFmpeg.cFFScale)
    Else
      _FileDuration := 0;

    if (FAudioStreams <> nil) and (FAudioStreams.Count > 0) then
    begin
      _AVStream := PAVStream(FAudioStreams.Items[0]);
      if _AVStream^.duration <> AV_NOPTS_VALUE then
      begin
        _AudioStreamDuration := gv_FFmpeg.AVUtil.av_rescale_q(_AVStream^.duration, _AVStream^.time_base, AV_TIME_BASE_Q);
        If _AudioStreamDuration >= gv_FFmpeg.cFFScale Then
          _AudioStreamDuration := Round(_AudioStreamDuration / gv_FFmpeg.cFFScale)
        Else
          _AudioStreamDuration := 0;
      end;
    end;

    if (FVideoStreams <> nil) and (FVideoStreams.Count > 0) then
    begin
      _AVStream := PAVStream(FVideoStreams.Items[0]);
      if _AVStream^.duration <> AV_NOPTS_VALUE then
      begin
        _VideoStreamDuration := gv_FFmpeg.AVUtil.av_rescale_q(_AVStream^.duration, _AVStream^.time_base, AV_TIME_BASE_Q);
        If _VideoStreamDuration >= gv_FFmpeg.cFFScale Then
          _VideoStreamDuration := Round(_VideoStreamDuration / gv_FFmpeg.cFFScale)
        Else
          _VideoStreamDuration := 0;
      end;
    end;

    FDuration := _FileDuration;
    If _VideoStreamDuration > FDuration Then FDuration := _VideoStreamDuration;
    If _AudioStreamDuration > FDuration Then FDuration := _AudioStreamDuration;
    If (_VideoStreamDuration > 0) And (_VideoStreamDuration = _AudioStreamDuration) And (_FileDuration > _VideoStreamDuration) Then
      FDuration := _VideoStreamDuration;

  except
    on E: Exception do
      LBLogger.Write(1, 'TAVInputInfo.CalculateDuration', lmt_Error, E.Message);
  end;
end;

destructor TAVSourceReader.TAVInputInfo.Destroy;
begin
  Self.Reset();
  inherited Destroy;
end;

function TAVSourceReader.TAVInputInfo.getStreamTypeByStreamIndex(anIdx: Integer): TAVStreamType;
begin
  Result := st_Unknown;
  if (anIdx >= 0) and (anIdx < Length(FStreamsTypes)) then
    Result := FStreamsTypes[anIdx];
end;

function TAVSourceReader.TAVInputInfo.FillFromContext(const aFilename: String; anAVFormatContext: PAVFormatContext): Boolean;
var
  _AVStream : PAVStream;
  i         : Integer;
begin
  Result := False;
  {$R-}
  try
    Self.Reset();
    FFileName := aFilename;
    If FileExists(FFileName) then
      FFileSize := FileSize(FFileName);

    if anAVFormatContext^.iformat <> nil then
      FFileFormat := AnsiString(anAVFormatContext^.iformat^.long_name) + ' [ ' + AnsiString(anAVFormatContext^.iformat^.name) + ' ]';

    FStartTime    := anAVFormatContext^.start_time;
    FDuration     := anAVFormatContext^.Duration;
    FBitRate      := Integer(anAVFormatContext^.bit_rate);
    FProgramCount := Integer(anAVFormatContext^.nb_programs);

    SetLength(FStreamsTypes, anAVFormatContext^.nb_streams);
    If anAVFormatContext^.nb_streams > 0 Then
    Begin
      FStreams := TList.Create;
      For i := 0 to anAVFormatContext^.nb_streams - 1 do
      Begin
        _AVStream := anAVFormatContext^.streams[i];
        FStreams.Add(_AVStream);
        case _AVStream^.codecpar^.codec_type of
          AVMEDIA_TYPE_Video:
            begin
              if FVideoStreams = nil then FVideoStreams := TList.Create;
              FVideoStreams.Add(_AVStream);
              FStreamsTypes[i] := st_Video;
            end;
          AVMEDIA_TYPE_AUDIO:
            begin
              if FAudioStreams = nil then FAudioStreams := TList.Create;
              FAudioStreams.Add(_AVStream);
              FStreamsTypes[i] := st_Audio;
            end;
          AVMEDIA_TYPE_SUBTITLE:
            begin
              if FSubtitleStreams = nil then FSubtitleStreams := TList.Create;
              FSubtitleStreams.Add(_AVStream);
              FStreamsTypes[i] := st_Subtitles;
            end;
        end;
      End;
    End;

    FTimeStamp := StrToInt64Def(GetMetaVal(anAVFormatContext^.metadata, 'timestamp'), 0);
    Self.CalculateDuration(anAVFormatContext);
    Result := True;

  except
    on E: Exception do
      LBLogger.Write(1, 'TAVInputInfo.FillFromContext', lmt_Error, E.Message);
  end;
  {$R+}
end;

{ TAVDecoder }

function get_hw_PixelFormat(s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat; cdecl;
var
  _pf    : PAVPixelFormat;
  _Found : Boolean;
begin
  _pf := fmt;
  repeat
    if (_pf^ = AV_PIX_FMT_NONE) or (_pf^ = TAVDecoder(s^.opaque).HWPixelFormat) then
    begin
      _Found := True;
      Break;
    end
    else
      Inc(_pf);
  until False;

  if _Found then Result := _pf^
  else Result := AV_PIX_FMT_NONE;
end;


function TAVDecoder.retrieveVideoDecoder(aCodecId: TAVCodecID; out HWAcceleration: Boolean): PAVCodec;
var
  candidates  : TStringList = nil;
  i           : Integer;
  candidate   : PAVCodec;
  deviceType  : TAVHWDeviceType;

begin
  Result         := nil;
  HWAcceleration := False;

  try
    if not (aCodecId in [AV_CODEC_ID_H264, AV_CODEC_ID_HEVC]) then
    begin
      Result := gv_FFmpeg.AVCodec.avcodec_find_decoder(aCodecId);
      Exit;
    end;

    candidates := Self.buildHWCandidateList(aCodecId);

    for i := 0 to candidates.Count - 1 do
    begin
      candidate := gv_FFmpeg.AVCodec.avcodec_find_decoder_by_name(
                     PAnsiChar(AnsiString(candidates[i])));
      if candidate = nil then Continue;

      // Recupera il device type salvato da buildHWCandidateList
      deviceType := TAVHWDeviceType(PtrInt(candidates.Objects[i]));

      if Self.ProbeDecoder(candidate, aCodecId, deviceType) then
      begin
        LBLogger.Write(1, 'TAVDecoder.retrieveVideoDecoder', lmt_Debug,
          'Selected HW decoder: %s', [candidates[i]]);
        HWAcceleration := True;
        FHWAccelType   := deviceType;
        Exit(candidate);
      end;
    end;

    // Fallback SW garantito
    Result := gv_FFmpeg.AVCodec.avcodec_find_decoder(aCodecId);
    if Result <> nil then
      LBLogger.Write(1, 'TAVDecoder.retrieveVideoDecoder', lmt_Debug,
        'Falling back to SW decoder for codec %d', [Integer(aCodecId)])
    else
      LBLogger.Write(1, 'TAVDecoder.retrieveVideoDecoder', lmt_Warning,
        'No decoder available for codec %d!', [Integer(aCodecId)]);

  finally
    candidates.Free;
  end;
end;


function TAVDecoder.elaborateDecodedFrame(var aFrame: IAVFrameWrapper): Boolean;
var
  _res        : Integer;
  _inputFrame : PAVFrame;
  _destFrame  : PAVFrame;

begin
  Result := False;
  FDecodedFrame := nil;
  if aFrame <> nil then
  begin
    _inputFrame := aFrame.getRelatedObject().Frame;

    if (FHWPixelFormat <> AV_PIX_FMT_NONE) and
       (_inputFrame^.format = LongInt(FHWPixelFormat)) then
    begin
      // Frame in memoria HW (es. V4L2, CUDA) — trasferisce in RAM
      FDecodedFrame := TAVFrameWrapper.Create as IAVFrameWrapper;
      _destFrame    := FDecodedFrame.getRelatedObject().Frame;

      _res := gv_FFmpeg.AVUtil.av_hwframe_transfer_data(_destFrame, _inputFrame, 0);
      if _res >= 0 then
      begin
        _res := gv_FFmpeg.AVUtil.av_frame_copy_props(_destFrame, _inputFrame);
        if _res < 0 then
          LBLogger.Write(1, 'TAVDecoder.elaborateDecodedFrame', lmt_Warning, 'Error copying frame metadata: <%s>', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
      end
      else begin
        LBLogger.Write(1, 'TAVDecoder.elaborateDecodedFrame', lmt_Warning, 'Error transferring frame from HW: <%s>', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
        FDecodedFrame := nil;
        Exit;
      end;
    end
    else begin
      // Frame già in RAM
      FDecodedFrame := aFrame;
      aFrame := nil;
    end;

    Result := True;

  end;
end;

function TAVDecoder.getDecodedFrame(out aDecodedFrame: IAVFrameWrapper): Boolean;
begin
  aDecodedFrame := FDecodedFrame;
  Result := True;
end;


function TAVDecoder.setHardwarePixelFormat(aDecoder: PAVCodec): Boolean;
var
  _Config : PAVCodecHWConfig;
  _Idx    : Integer = 0;
begin
  Result         := False;
  FHWPixelFormat := AV_PIX_FMT_NONE;
//  FHWAccelType   := AV_HWDEVICE_TYPE_NONE;

  if aDecoder <> nil then
  begin

    // Interroga FFmpeg per capire quale device type e pixel format
    // usa il decoder trovato — funziona per qualsiasi decoder HW
    repeat
      _Config := gv_FFmpeg.AVCodec.avcodec_get_hw_config(aDecoder, _Idx);
      if _Config = nil then
        Break;

      if (_Config^.methods and Integer(AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX)) = Integer(AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX) then
      begin
        // Se FHWAccelType è già impostato (da retrieveVideoDecoder),
        // cerca solo la config che corrisponde a quel device type.
        // Necessario per decoder multi-config come 'hevc' (CUDA prima di DRM).
        if (FHWAccelType = AV_HWDEVICE_TYPE_NONE) or
           (_Config^.device_type = FHWAccelType) then
        begin
          FHWPixelFormat := _Config^.pix_fmt;
          FHWAccelType   := _Config^.device_type;
          Result         := True;
          LBLogger.Write(1, 'TAVDecoder.setHardwarePixelFormat', lmt_Debug, 'HW pixel format: %d device type: %d', [Integer(FHWPixelFormat), Integer(FHWAccelType)]);
          Break;
        end;
      end;

      Inc(_Idx);
    until False;

    // h264_v4l2m2m non usa HW_DEVICE_CTX ma funziona comunque —
    // in questo caso Result=False è corretto e InitDecoder_ByInputStream
    // non tenterà initializeHWDecoder
  end;
end;



function TAVDecoder.initializeHWDecoder(): Boolean;
var
  _res : Integer;
begin
  Result := False;

  _res := gv_FFmpeg.AVUtil.av_hwdevice_ctx_create(@FHWDeviceCtx, FHWAccelType, nil, nil, 0);
  if _res >= 0 then
  begin
    FAVCodecContext^.hw_device_ctx := gv_FFmpeg.AVUtil.av_buffer_ref(FHWDeviceCtx);
    Result := True;
  end
  else begin
    LBLogger.Write(1, 'TAVDecoder.initializeHWDecoder', lmt_Warning, 'Failed to create specific HW device %d: <%s>', [Integer(FHWAccelType), gv_FFmpeg.AVUtil.av_strerror(_res)]);
    FHWAccelType := AV_HWDEVICE_TYPE_NONE;
    FAVCodecContext^.hw_device_ctx := nil;
    if FHWDeviceCtx <> nil then
      gv_FFmpeg.AVUtil.av_buffer_unref(@FHWDeviceCtx);
  end;
end;


function TAVDecoder.openVideoDecoderContext(aDecoder: PAVCodec; const aTimeBase: TAVRational; aHWAccel: Boolean; out anErrorMsg: String): Boolean;
var
  _NeedsHWContext : Boolean = False;

begin
  Result := False;
  anErrorMsg := '';

  // 1. HW context (CUDA, VAAPI, …) — v4l2m2m non ne ha bisogno
  if aHWAccel then
    _NeedsHWContext := Self.setHardwarePixelFormat(aDecoder);

  if _NeedsHWContext then
  begin
    FAVCodecContext^.opaque     := Self;
    FAVCodecContext^.get_format := @get_hw_PixelFormat;
    gv_FFmpeg.AVUtil.av_opt_set_int(FAVCodecContext, 'refcounted_frames', 1, 0);

    if not Self.initializeHWDecoder() then
    begin
      FAVCodecContext^.get_format := nil;
      FAVCodecContext^.opaque     := nil;
      FHWPixelFormat              := AV_PIX_FMT_NONE;
      FHWAccelType                := AV_HWDEVICE_TYPE_NONE;
      LBLogger.Write(1, 'TAVDecoder.openVideoDecoderContext', lmt_Warning, '<%s> - HW device context failed, continuing without it', [Self.ClassName]);
    end;
  end
  else
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - No HW device context needed', [Self.ClassName]);

  // 2. Flags e timebase
  FInputTimeBase                := aTimeBase;
  FAVCodecContext^.thread_count := 1;

  // AV_CODEC_FLAG_LOW_DELAY incompatibile con decoder INTERNAL HW (v4l2m2m, mmal)
  if not (aHWAccel and (not _NeedsHWContext)) then
  begin
    if (FAVCodecContext^.flags and AV_CODEC_FLAG_LOW_DELAY) = 0 then
      FAVCodecContext^.flags := FAVCodecContext^.flags or AV_CODEC_FLAG_LOW_DELAY;
  end
  else
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - Skipping LOW_DELAY for INTERNAL HW decoder (v4l2m2m/mmal)', [Self.ClassName]);

  // 3. Apertura
  Result := gv_FFmpeg.AVCodec.avcodec_open2(FAVCodecContext, aDecoder, nil);
  if Result then
  begin
    gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - Video decoder opened OK (HWAccel=%s)', [Self.ClassName, BoolToStr(aHWAccel, True)]);
  end
  else begin
    anErrorMsg := Format('Error opening video decoder <%s>', [AnsiString(aDecoder^.name)]);
    LBLogger.Write(1, 'TAVDecoder.openVideoDecoderContext', lmt_Warning, '<%s> - %s', [Self.ClassName, anErrorMsg]);
    gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);
    FAVCodecContext := nil;
  end;
end;

(*
function TAVDecoder.openVideoDecoderContext(aDecoder: PAVCodec; const aTimeBase: TAVRational; aHWAccel: Boolean; out anErrorMsg: String): Boolean;
var
  _NeedsHWContext : Boolean = False;

begin
  Result := False;
  anErrorMsg := '';

  // 1. HW context (CUDA, VAAPI, …) — v4l2m2m non ne ha bisogno
  if aHWAccel then
    _NeedsHWContext := Self.setHardwarePixelFormat(aDecoder);

  if _NeedsHWContext then
  begin
    FAVCodecContext^.opaque    := Self;
    FAVCodecContext^.get_format := @get_hw_PixelFormat;
    gv_FFmpeg.AVUtil.av_opt_set_int(FAVCodecContext, 'refcounted_frames', 1, 0);

    if not Self.initializeHWDecoder() then
    begin
      // device context non disponibile — continua senza:
      // molti decoder (v4l2m2m) funzionano ugualmente
      FAVCodecContext^.get_format := nil;
      FAVCodecContext^.opaque     := nil;
      FHWPixelFormat              := AV_PIX_FMT_NONE;
      FHWAccelType                := AV_HWDEVICE_TYPE_NONE;
      LBLogger.Write(1, 'TAVDecoder.openVideoDecoderContext', lmt_Warning, '<%s> - HW device context failed, continuing without it', [Self.ClassName]);
    end;
  end
  else
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - No HW device context needed', [Self.ClassName]);

  // 2. Flags e timebase — uguali per tutti i path
  FInputTimeBase               := aTimeBase;
  FAVCodecContext^.thread_count := 1;
  // AV_CODEC_FLAG_LOW_DELAY NON deve essere impostato per decoder INTERNAL HW
  // (v4l2m2m, mmal): usano una pipeline stateful asincrona incompatibile.
  // _NeedsHWContext=False + aHWAccel=True identifica esattamente questi decoder.
  if not (aHWAccel and (not _NeedsHWContext)) then
  begin
    if (FAVCodecContext^.flags and AV_CODEC_FLAG_LOW_DELAY) = 0 then
      FAVCodecContext^.flags := FAVCodecContext^.flags or AV_CODEC_FLAG_LOW_DELAY;
  end
  else
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - Skipping LOW_DELAY for INTERNAL HW decoder (v4l2m2m/mmal)', [Self.ClassName]);

  // 3. Apertura
  Result := gv_FFmpeg.AVCodec.avcodec_open2(FAVCodecContext, aDecoder, nil);
  if Result then
  begin
    gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);
    LBLogger.Write(5, 'TAVDecoder.openVideoDecoderContext', lmt_Debug, '<%s> - Video decoder opened OK (HWAccel=%s)', [Self.ClassName, BoolToStr(aHWAccel, True)]);
  end
  else begin
    anErrorMsg := Format('Error opening video decoder <%s>', [AnsiString(aDecoder^.name)]);
    LBLogger.Write(1, 'TAVDecoder.openVideoDecoderContext', lmt_Warning, '<%s> - %s', [Self.ClassName, anErrorMsg]);
    gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);
  end;
end;
*)

function TAVDecoder.buildHWCandidateList(aCodecId: TAVCodecID): TStringList;
var
  hwType      : TAVHWDeviceType;
  i           : Integer;
  decoderName : AnsiString;
  internalHW  : pAnsiString;
  internalCnt : Integer;

begin
  Result := TStringList.Create;

  // 1. Prima: decoder che richiedono device_ctx —
  //    enumera SOLO i tipi compilati in questo FFmpeg
  //    (CUDA, VAAPI, DRM/V4L2-stateless, D3D11VA, QSV, VideoToolbox…)
  hwType := gv_FFmpeg.AVUtil.av_hwdevice_iterate_types(AV_HWDEVICE_TYPE_NONE);

  while hwType <> AV_HWDEVICE_TYPE_NONE do
  begin
    LBLogger.Write(5, 'TAVDecoder.buildHWCandidateList', lmt_Debug,
      'FFmpeg HW device type available: %d', [Integer(hwType)]);

    decoderName := '';
    for i := 0 to High(cHWDecoderMap) do
    begin
      if cHWDecoderMap[i].DeviceType = hwType then
      begin
        case aCodecId of
          AV_CODEC_ID_H264: decoderName := cHWDecoderMap[i].H264Name;
          AV_CODEC_ID_HEVC: decoderName := cHWDecoderMap[i].HEVCName;
        end;
        Break;
      end;
    end;

    if (decoderName <> '') and
       (gv_FFmpeg.AVCodec.avcodec_find_decoder_by_name(PAnsiChar(decoderName)) <> nil) then
    begin
      // Objects[i] porta il device type atteso — usato da ProbeDecoder per
      // scegliere la hwconfig corretta (es. 'hevc' ha CUDA prima di DRM)
      Result.AddObject(decoderName, TObject(PtrInt(hwType)));
      LBLogger.Write(5, 'TAVDecoder.buildHWCandidateList', lmt_Debug,
        'Device-ctx HW decoder available: %s (device_type=%d)',
        [decoderName, Integer(hwType)]);
    end;

    hwType := gv_FFmpeg.AVUtil.av_hwdevice_iterate_types(hwType);
  end;

  // 2. Dopo: decoder "internal HW" (v4l2m2m, mmal) — no device ctx
  //    tentati solo se nessun decoder device-ctx ha funzionato
  case aCodecId of
    AV_CODEC_ID_H264: begin internalHW := @cInternalHW_H264[0]; internalCnt := Length(cInternalHW_H264); end;
    AV_CODEC_ID_HEVC: begin internalHW := @cInternalHW_HEVC[0]; internalCnt := Length(cInternalHW_HEVC); end;
  else
    internalHW  := nil;
    internalCnt := 0;
  end;

  for i := 0 to internalCnt - 1 do
  begin
    if gv_FFmpeg.AVCodec.avcodec_find_decoder_by_name(PAnsiChar(internalHW[i])) <> nil then
    begin
      // INTERNAL: device type = NONE (nessun device ctx richiesto)
      Result.AddObject(internalHW[i], TObject(PtrInt(AV_HWDEVICE_TYPE_NONE)));
      LBLogger.Write(5, 'TAVDecoder.buildHWCandidateList', lmt_Debug,
        'Internal HW decoder available: %s', [internalHW[i]]);
    end;
  end;

  LBLogger.Write(1, 'TAVDecoder.buildHWCandidateList', lmt_Debug,
    'Codec %d: %d HW candidates found: [%s]',
    [Integer(aCodecId), Result.Count, Result.CommaText]);
end;


function TAVDecoder.ProbeDecoder(aDecoder: PAVCodec; aCodecId: TAVCodecID; aDeviceType: TAVHWDeviceType): Boolean;
var
  _testCtx  : PAVCodecContext;
  hwConfig  : PAVCodecHWConfig;
  _hwDevice : PAVBufferRef = nil;
  hwType    : TAVHWDeviceType;
  idx, res  : Integer;
  needsHW   : Boolean = False;

begin
  Result := False;

  if aDecoder <> nil then
  begin
    try
      _testCtx := gv_FFmpeg.AVCodec.avcodec_alloc_context3(aDecoder);
      if _testCtx <> nil then
      begin
        _testCtx^.codec_type   := AVMEDIA_TYPE_VIDEO;
        _testCtx^.codec_id     := aCodecId;
        _testCtx^.width        := 640;
        _testCtx^.height       := 480;
        _testCtx^.pix_fmt      := AV_PIX_FMT_NONE;
        _testCtx^.thread_count := 1;

        needsHW := False;
        hwType  := AV_HWDEVICE_TYPE_NONE;
        idx     := 0;

        repeat
          hwConfig := gv_FFmpeg.AVCodec.avcodec_get_hw_config(aDecoder, idx);
          if hwConfig <> nil then
          begin
            LBLogger.Write(5, 'TAVDecoder.ProbeDecoder', lmt_Debug,
              'decoder=%s hwconfig[%d] methods=$%x pixfmt=%d device_type=%d',
              [AnsiString(aDecoder^.name), idx, hwConfig^.methods,
               Integer(hwConfig^.pix_fmt), Integer(hwConfig^.device_type)]);

            if (hwConfig^.methods and Integer(AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX)) <> 0 then
            begin
              // Cerca la config con il device type specifico passato dal chiamante.
              // Necessario per decoder come 'hevc' che hanno più config HW
              // (es. hwconfig[0]=CUDA, hwconfig[1]=DRM): senza questo controllo
              // verrebbe sempre tentato CUDA anche su Pi 5.
              if (aDeviceType = AV_HWDEVICE_TYPE_NONE) or
                 (hwConfig^.device_type = aDeviceType) then
              begin
                needsHW := True;
                hwType  := hwConfig^.device_type;
                LBLogger.Write(5, 'TAVDecoder.ProbeDecoder', lmt_Debug,
                  'decoder=%s requires HW_DEVICE_CTX device_type=%d',
                  [AnsiString(aDecoder^.name), Integer(hwType)]);
                Break;
              end;
            end;
            Inc(idx);
          end
          else
            Break;
        until False;

        if needsHW then
        begin
          res := gv_FFmpeg.AVUtil.av_hwdevice_ctx_create(@_hwDevice, hwType, nil, nil, 0);
          if res = 0 then
          begin
            _testCtx^.hw_device_ctx := _hwDevice;
            _hwDevice := nil;
          end
          else begin
            LBLogger.Write(1, 'TAVDecoder.ProbeDecoder', lmt_Warning,
              'Decoder not available: <%s>', [gv_FFmpeg.AVUtil.av_strerror(res)]);
            Exit;
          end;
        end
        else begin
          // INTERNAL (v4l2m2m, mmal): avcodec_open2 aprirebbe il device HW reale
          // con parametri fittizi → EINVAL. La disponibilità è già confermata
          // da avcodec_find_decoder_by_name in buildHWCandidateList.
          LBLogger.Write(5, 'TAVDecoder.ProbeDecoder', lmt_Debug,
            'decoder=%s does NOT require HW_DEVICE_CTX (uses INTERNAL or none)',
            [AnsiString(aDecoder^.name)]);
          Exit(True);
        end;

        Result := gv_FFmpeg.AVCodec.avcodec_open2(_testCtx, aDecoder, nil);
        if not Result then
          LBLogger.Write(1, 'TAVDecoder.ProbeDecoder', lmt_Warning,
            'Probe FAILED decoder=%s', [AnsiString(aDecoder^.name)])
        else
          LBLogger.Write(5, 'TAVDecoder.ProbeDecoder', lmt_Debug,
            'Probe OK decoder=%s needsHW=%s',
            [AnsiString(aDecoder^.name), BoolToStr(needsHW, True)]);
      end;

    finally
      if _testCtx <> nil then
        gv_FFmpeg.AVCodec.avcodec_free_context(_testCtx);
      if _hwDevice <> nil then
        gv_FFmpeg.AVUtil.av_buffer_unref(@_hwDevice);
    end;
  end;
end;


constructor TAVDecoder.Create();
begin
  inherited Create;
  FAVCodecContext := nil;
  FHWAccelType    := AV_HWDEVICE_TYPE_NONE;
  FHWPixelFormat  := AV_PIX_FMT_NONE;

  if not createFFmpegWrapper([]) then
    LBLogger.Write(1, 'TAVDecoder.Create', lmt_Warning, 'FFMPEG wrapper not initialized!');
end;

destructor TAVDecoder.Destroy();
begin
  Self.ClearDecoderContext();
  inherited Destroy;
end;

procedure TAVDecoder.ClearDecoderContext();
begin
  if FAVCodecContext <> nil then
    gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);

  if FHWDeviceCtx <> nil then
    gv_FFmpeg.AVUtil.av_buffer_unref(@FHWDeviceCtx);
end;

function TAVDecoder.rescalePTS2ms(aPTS: Int64): Int64;
var
  _Output : TAVRational;
begin
  _Output.num := 1;
  _Output.den := 1000;
  Result := gv_FFmpeg.AVUtil.av_rescale_q(aPTS, FInputTimeBase, _Output);
end;

function TAVDecoder.ms2PTS(aTimecode: Int64): Int64;
var
  _msTimebase : TAVRational;
begin
  _msTimebase.num := 1;
  _msTimebase.den := 1000;
  Result := gv_FFmpeg.AVUtil.av_rescale_q(aTimecode, _msTimebase, FInputTimeBase);
end;

function TAVDecoder.setHWAccelType(const aDeviceType: String): Boolean;
begin
  Result := False;
  FHWAccelType := AV_HWDEVICE_TYPE_NONE;
  if aDeviceType <> '' then
  begin
    FHWAccelType := gv_FFmpeg.AVUtil.av_hwdevice_find_type_by_name(PAnsiChar(aDeviceType));
    Result := FHWAccelType <> AV_HWDEVICE_TYPE_NONE;
    if not Result then
      LBLogger.Write(1, 'TAVDecoder.setHWAccelType', lmt_Warning, '<%s>  -  No acceleration type for <%s>!', [Self.ClassName, aDeviceType]);
  end
  else
    LBLogger.Write(1, 'TAVDecoder.setHWAccelType', lmt_Warning, '<%s>  -  No device type selected!', [Self.ClassName]);
end;



function TAVDecoder.InitDecoder_ByInputStream(anAVStream: PAVStream; out anErrorMsg: String): Boolean;
var
  _Decoder : PAVCodec;
//  _NeedsHWContext : Boolean = False;
  _HWAccel : Boolean = False;
//  i : Integer;

begin
  Result := False;
  anErrorMsg := '';

  if anAVStream <> nil then
  begin

    try
      Self.ClearDecoderContext();

      case anAVStream^.codecpar^.codec_type of
        AVMEDIA_TYPE_AUDIO:
          begin
            _Decoder := gv_FFmpeg.AVCodec.avcodec_find_decoder(anAVStream^.codecpar^.codec_id);
            if _Decoder <> nil then
            begin
              FAVCodecContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Decoder);
              gv_FFmpeg.AVCodec.avcodec_parameters_to_context(FAVCodecContext, anAVStream^.codecpar);
              if (anAVStream^.time_base.num = 0) or (anAVStream^.time_base.den = 0) then
              begin
                FInputTimeBase.num := 1;
                FInputTimeBase.den := 1000;
              end
              else
                FInputTimeBase := anAVStream^.time_base;
              FAVCodecContext^.thread_count := 1;
              Result := gv_FFmpeg.AVCodec.avcodec_open2(FAVCodecContext, _Decoder, nil);
              if not Result then
              begin
                gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);
                anErrorMsg := 'Error opening audio decoder!';
              end;
            end
            else
              anErrorMsg := Format('Unsupported audio codec (id=%d) for stream #%d', [Ord(anAVStream^.codecpar^.codec_id), anAVStream^.index]);
          end;

        AVMEDIA_TYPE_VIDEO:
          begin
            if (anAVStream^.codecpar^.width > 0) and (anAVStream^.codecpar^.height > 0) then
            begin
              _Decoder := Self.retrieveVideoDecoder(anAVStream^.codecpar^.codec_id, _HWAccel);
              if _Decoder <> nil then
              begin
                LBLogger.Write(5, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Debug, '<%s>  -  Decoder for codec <%d> found', [Self.ClassName, anAVStream^.codecpar^.codec_id]);

                FAVCodecContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Decoder);
                if FAVCodecContext <> nil then
                begin
                  gv_FFmpeg.AVCodec.avcodec_parameters_to_context(FAVCodecContext, anAVStream^.codecpar);

                  Result := Self.openVideoDecoderContext(_Decoder, anAVStream^.time_base, _HWAccel, anErrorMsg);
                end
                else begin
                  anErrorMsg := 'Codec context not allocated!';
                  LBLogger.Write(1, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Warning, '<%s>  -  %s', [Self.ClassName, anErrorMsg]);
                end;
              end
              else begin
                anErrorMsg := Format('Unsupported video codec (id=%d) for stream #%d', [Ord(anAVStream^.codecpar^.codec_id), anAVStream^.index]);
                LBLogger.Write(1, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Warning, '<%s>  -  %s', [Self.ClassName, anErrorMsg]);
              end;
            end
            else begin
              anErrorMsg := 'Invalid frame size!';
              LBLogger.Write(1, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Warning, '<%s>  -  %s', [Self.ClassName, anErrorMsg]);
            end;
          end;

        else
          LBLogger.Write(1, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Warning, 'Not yet implemented!');
      end;

      if Result then
        gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);

    except
      on E: Exception do
        LBLogger.Write(1, 'TAVDecoder.InitDecoder_ByInputStream', lmt_Error, '<%s>  -  %s', [Self.ClassName, E.Message]);
    end;

  end
  else
    anErrorMsg := 'No stream!';

end;

function TAVDecoder.decodePacket(aPacket: PAVPacket; out NeedMore: Boolean; out Error: Boolean): Boolean;
var
  _res          : Integer;
  _Frame        : IAVFrameWrapper = nil;
  _FrameDecoded : Boolean = False;
const
  cAVERROR_OK = Integer(0);
begin
  Result   := False;
  NeedMore := False;
  Error    := False;

  if FAVCodecContext <> nil then
  begin
    try
      if aPacket = nil then
        _res := gv_FFmpeg.AVCodec.avcodec_send_packet(FAVCodecContext, nil)
      else
        _res := gv_FFmpeg.AVCodec.avcodec_send_packet(FAVCodecContext, aPacket);

      if (_res = cAVERROR_OK) or (_res = gv_FFmpeg.AVUtil.AVERROR_AGAIN) then
      begin
        repeat
          if _Frame = nil then
            _Frame := TAVFrameWrapper.Create as IAVFrameWrapper
          else
            _Frame.getRelatedObject().clearFrame();

          _res := gv_FFmpeg.AVCodec.avcodec_receive_frame(FAVCodecContext, _Frame.getRelatedObject().Frame);

          case _res of
            cAVERROR_OK:
              begin
                Result := Self.elaborateDecodedFrame(_Frame);
                if Result then
                  _FrameDecoded := True
                else
                  LBLogger.Write(1, 'TAVDecoder.decodePacket', lmt_Warning, '<%s>  -  Error elaborating decoded frame!', [Self.ClassName]);
              end;

            AVERROR_EOF:
              begin
                LBLogger.Write(6, 'TAVDecoder.decodePacket', lmt_Debug, '<%s>  -  The decoder has been fully flushed, and there are no more output frames', [Self.ClassName]);
                Result := False;
                Break;
              end;

            else begin
              if _res = gv_FFmpeg.AVUtil.AVERROR_AGAIN then
              begin
                NeedMore := not _FrameDecoded;
                Result := True;
              end
              else begin
                LBLogger.Write(1, 'TAVDecoder.decodePacket', lmt_Warning, '<%s>  -  Error reading frame: <%s>', [Self.ClassName, gv_FFmpeg.AVUtil.av_strerror(_res)]);
                Result := False;
              end;
              Break;
            end;
          end;
        until False;
      end
      else begin
        LBLogger.Write(1, 'TAVDecoder.decodePacket', lmt_Warning, 'Error sending packet to codec: %d <%s>', [_res, gv_FFmpeg.AVUtil.av_strerror(_res)]);
        gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);
        Error := True;
      end;

    except
      on E: Exception do
        LBLogger.Write(1, 'TAVDecoder.decodePacket', lmt_Error, '%s  -  %s', [Self.ClassName, E.Message]);
    end;
  end
  else
    LBLogger.Write(1, 'TAVDecoder.decodePacket', lmt_Warning, 'Codec not initialized!');
end;


{ TAVNetworkDecoder }
(*
function TAVNetworkDecoder.InitDecoder_ByNetworkDataInternal(anHeader: Pointer; HeaderType: THeaderType; ExtraData: pByte; out anErrorMsg: String): Boolean;
var
  _Decoder     : PAVCodec = nil;
  _AudioHeader : TAVNetworkDecoder.pAudioHeader;
  _VideoHeader : TAVNetworkDecoder.pVideoHeader;
  _CodecId     : TAVCodecID;
  _HWAccel     : Boolean = False;
  _NeedsHWContext : Boolean = False;

begin
  Result := False;
  anErrorMsg := 'Decoder not initialized!';

  if gv_FFmpeg <> nil then
  begin
    if anHeader <> nil then
    begin

      try
        Self.ClearDecoderContext();

        case HeaderType of
          ht_Audio : _CodecId := InternalCodecId2AVCodecId(TAVInternalCodecType(TAVNetworkDecoder.pAudioHeader(anHeader)^.CodecId));
          ht_Video : _CodecId := InternalCodecId2AVCodecId(TAVInternalCodecType(TAVNetworkDecoder.pVideoHeader(anHeader)^.CodecId));
          else       _CodecId := AV_CODEC_ID_NONE;
        end;

        if _CodecId <> AV_CODEC_ID_NONE then
        begin
          case HeaderType of
            ht_Video: _Decoder := Self.retrieveVideoDecoder(_CodecId, _HWAccel);
            ht_Audio: _Decoder := gv_FFmpeg.AVCodec.avcodec_find_decoder(_CodecId);
          end
        end
        else
          LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkDataInternal', lmt_Warning, 'Decoder not found!');

        if _Decoder <> nil then
        begin
          FAVCodecContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Decoder);
          if FAVCodecContext <> nil then
          begin
            case HeaderType of
              ht_Audio:
                begin
                  _AudioHeader := TAVNetworkDecoder.pAudioHeader(anHeader);
                  FAVCodecContext^.sample_rate    := _AudioHeader^.SampleRate;
                  FAVCodecContext^.sample_fmt     := TAVSampleFormat(_AudioHeader^.SampleFormat);
                  gv_FFmpeg.AVUtil.av_channel_layout_from_mask(@FAVCodecContext^.ch_layout, _AudioHeader^.ChLayout);
                  FAVCodecContext^.block_align    := _AudioHeader^.BlockAlign;
                  FAVCodecContext^.extradata_size := _AudioHeader^.ExtraDataSize;
                  FAVCodecContext^.thread_count   := 1;
                  if (_AudioHeader^.time_base.num = 0) or (_AudioHeader^.time_base.den = 0) then
                  begin
                    FInputTimeBase.num := 1;
                    FInputTimeBase.den := 1000;
                  end
                  else
                    FInputTimeBase := _AudioHeader^.time_base;
                end;

              ht_Video:
                begin
                  _VideoHeader := TAVNetworkDecoder.pVideoHeader(anHeader);
                  FAVCodecContext^.pix_fmt := InternalPixelFormat2AVPixelFormat(TAVInternalPixelFormat(_VideoHeader^.PixelFormat));
                  FAVCodecContext^.width   := _VideoHeader^.Width;
                  FAVCodecContext^.height  := _VideoHeader^.Height;
                  if (_VideoHeader^.time_base.num = 0) or (_VideoHeader^.time_base.den = 0) then
                  begin
                    FInputTimeBase.num := 1;
                    FInputTimeBase.den := 1000;
                  end
                  else
                    FInputTimeBase := _VideoHeader^.time_base;
                  FAVCodecContext^.extradata_size := _VideoHeader^.ExtraDataSize;

                  Result := Self.openVideoDecoderContext(_Decoder, _VideoHeader^.time_base, _HWAccel, anErrorMsg);
                end;

              else
                LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkDataInternal', lmt_Warning, 'Unknown header type!');
            end;

            if (FAVCodecContext^.extradata_size > 0) and (ExtraData <> nil) then
            begin
              FAVCodecContext^.extradata := gv_FFmpeg.AVUtil.av_malloc(FAVCodecContext^.extradata_size);
              Move(ExtraData^, FAVCodecContext^.extradata^, FAVCodecContext^.extradata_size);
            end;

            Result := gv_FFmpeg.AVCodec.avcodec_open2(FAVCodecContext, _Decoder, nil);
            if Result then
            begin
              gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);
              case HeaderType of
                ht_Audio: FAVCodecContext^.time_base := FInputTimeBase;
                ht_Video:
                  begin
                    FAVCodecContext^.time_base := FInputTimeBase;
                    FAVCodecContext^.sample_aspect_ratio := TAVNetworkDecoder.pVideoHeader(anHeader)^.sample_aspect_ratio;
                  end;
              end;
            end
            else begin
              anErrorMsg := Format('Error opening decoder (id = %d): %s!', [Integer(FAVCodecContext^.codec_id), String(_Decoder^.name)]);
              gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);
            end;
          end
          else
            anErrorMsg := 'Error allocating context!';
        end
        else
          anErrorMsg := Format('Decoder not found for codec id %d', [Integer(_CodecId)]);

      except
        on E: Exception do
          LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkDataInternal', lmt_Error, E.Message);
      end;
    end;

  end
  else
    LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkDataInternal', lmt_Warning, 'FFMPEG not initialized!');
end;
*)

function TAVNetworkDecoder.InitDecoder_ByAudioHeader(anHeader  : pAudioHeader; ExtraData : pByte; out anErrorMsg : String): Boolean;
var
  _Decoder : PAVCodec;

begin
  Result     := False;
  anErrorMsg := '';

  if anHeader <> nil then
  begin

    Self.ClearDecoderContext();

    _Decoder := gv_FFmpeg.AVCodec.avcodec_find_decoder(InternalCodecId2AVCodecId(TAVInternalCodecType(anHeader^.CodecId)));
    if _Decoder = nil then
    begin
      anErrorMsg := Format('Audio decoder not found for codec id %d', [anHeader^.CodecId]);
      LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByAudioHeader', lmt_Warning, anErrorMsg);
      Exit;
    end;

    FAVCodecContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Decoder);
    if FAVCodecContext = nil then
    begin
      anErrorMsg := 'Failed to allocate audio codec context';
      Exit;
    end;

    // Parametri codec
    FAVCodecContext^.codec_type     := AVMEDIA_TYPE_AUDIO;
    FAVCodecContext^.codec_id       := InternalCodecId2AVCodecId(TAVInternalCodecType(anHeader^.CodecId));
    FAVCodecContext^.sample_rate    := anHeader^.SampleRate;
    FAVCodecContext^.sample_fmt     := TAVSampleFormat(anHeader^.SampleFormat);
//    FAVCodecContext^.channels       := anHeader^.Channels;
//    FAVCodecContext^.channel_layout := anHeader^.ChLayout;
    gv_FFmpeg.AVUtil.av_channel_layout_from_mask(@FAVCodecContext^.ch_layout, anHeader^.ChLayout);
    FAVCodecContext^.block_align    := anHeader^.BlockAlign;
    FAVCodecContext^.frame_size     := anHeader^.frame_size;

    // time_base con fallback sicuro
    if (anHeader^.time_base.num = 0) or (anHeader^.time_base.den = 0) then
    begin
      FInputTimeBase.num := 1;
      FInputTimeBase.den := anHeader^.SampleRate;
    end
    else
      FInputTimeBase := anHeader^.time_base;

    // extradata PRIMA di avcodec_open2
    FAVCodecContext^.extradata_size := anHeader^.ExtraDataSize;
    if (anHeader^.ExtraDataSize > 0) and (ExtraData <> nil) then
    begin
      FAVCodecContext^.extradata := gv_FFmpeg.AVUtil.av_malloc(anHeader^.ExtraDataSize + AV_INPUT_BUFFER_PADDING_SIZE);
      Move(ExtraData^, FAVCodecContext^.extradata^, anHeader^.ExtraDataSize);
      FillChar(PByte(FAVCodecContext^.extradata)[anHeader^.ExtraDataSize], AV_INPUT_BUFFER_PADDING_SIZE, 0);
    end;

    // Apertura
    Result := gv_FFmpeg.AVCodec.avcodec_open2(FAVCodecContext, _Decoder, nil);
    if Result then
    begin
      gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);
      FAVCodecContext^.time_base := FInputTimeBase;
      LBLogger.Write(5, 'TAVNetworkDecoder.InitDecoder_ByAudioHeader', lmt_Debug, 'Audio decoder opened OK: %s', [AnsiString(_Decoder^.name)]);
    end
    else begin
      anErrorMsg := Format('Error opening audio decoder <%s>', [AnsiString(_Decoder^.name)]);
      LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByAudioHeader', lmt_Warning, anErrorMsg);
      gv_FFmpeg.AVCodec.avcodec_free_context(FAVCodecContext);
    end;
  end;
end;

function TAVNetworkDecoder.InitDecoder_ByVideoHeader(anHeader  : pVideoHeader; ExtraData : pByte; out anErrorMsg : String): Boolean;
var
  _Decoder : PAVCodec;
  _HWAccel : Boolean = False;

begin
  Result     := False;
  anErrorMsg := '';

  if anHeader <> nil then
  begin

    Self.ClearDecoderContext();

    // retrieveVideoDecoder prova HW (v4l2m2m, VAAPI, CUDA…) con fallback SW
    _Decoder := Self.retrieveVideoDecoder(InternalCodecId2AVCodecId(TAVInternalCodecType(anHeader^.CodecId)), _HWAccel);
    if _Decoder = nil then
    begin
      anErrorMsg := Format('Video decoder not found for codec id %d', [anHeader^.CodecId]);
      LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByVideoHeader', lmt_Warning, anErrorMsg);
      Exit;
    end;

    FAVCodecContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Decoder);
    if FAVCodecContext = nil then
    begin
      anErrorMsg := 'Failed to allocate video codec context';
      Exit;
    end;

    // Parametri codec
    FAVCodecContext^.codec_type := AVMEDIA_TYPE_VIDEO;
    FAVCodecContext^.codec_id   := TAVCodecID(anHeader^.CodecId);
    FAVCodecContext^.width      := anHeader^.Width;
    FAVCodecContext^.height     := anHeader^.Height;
    FAVCodecContext^.pix_fmt    := InternalPixelFormat2AVPixelFormat(TAVInternalPixelFormat(anHeader^.PixelFormat));

    // time_base con fallback sicuro
    if (anHeader^.time_base.num = 0) or (anHeader^.time_base.den = 0) then
    begin
      FInputTimeBase.num := 1;
      FInputTimeBase.den := 1000;
    end
    else
      FInputTimeBase := anHeader^.time_base;

    // extradata PRIMA di openVideoDecoderContext —
    // HEVC/H264 ne hanno bisogno per le SPS/PPS durante avcodec_open2
    FAVCodecContext^.extradata_size := anHeader^.ExtraDataSize;
    if (anHeader^.ExtraDataSize > 0) and (ExtraData <> nil) then
    begin
      FAVCodecContext^.extradata := gv_FFmpeg.AVUtil.av_malloc(anHeader^.ExtraDataSize + AV_INPUT_BUFFER_PADDING_SIZE);
      Move(ExtraData^, FAVCodecContext^.extradata^, anHeader^.ExtraDataSize);
      FillChar(PByte(FAVCodecContext^.extradata)[anHeader^.ExtraDataSize], AV_INPUT_BUFFER_PADDING_SIZE, 0);
    end;

    // openVideoDecoderContext gestisce: HW context + flags + avcodec_open2 + flush
    // In caso di errore libera FAVCodecContext internamente
    Result := Self.openVideoDecoderContext(_Decoder, FInputTimeBase, _HWAccel, anErrorMsg);

    if Result then
    begin
      FAVCodecContext^.time_base           := FInputTimeBase;
      FAVCodecContext^.sample_aspect_ratio := anHeader^.sample_aspect_ratio;
    end;
  end;
end;


function TAVNetworkDecoder.InitDecoder_ByNetworkData(anHeader: pAudioHeader; ExtraData: pByte; out anErrorMsg: String): Boolean;
begin
  LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkData', lmt_Debug, 'Initializing audio decoder ...');
  Result := Self.InitDecoder_ByAudioHeader(anHeader, ExtraData, anErrorMsg);
end;

function TAVNetworkDecoder.InitDecoder_ByNetworkData(anHeader: pVideoHeader; ExtraData: pByte; out anErrorMsg: String): Boolean;
begin
  LBLogger.Write(1, 'TAVNetworkDecoder.InitDecoder_ByNetworkData', lmt_Debug, 'Initializing video decoder ...');
  Result := Self.InitDecoder_ByVideoHeader(anHeader, ExtraData, anErrorMsg);
end;

{ TVideoDecoder }


function TVideoDecoder.decodePacket(pkt: IAVPacketWrapper; flushBuffers: Boolean; out DecodedTimecode: Int64; out isKeyFrame: Boolean; out NeedMore: Boolean): Boolean;
var
  _decFrame : PAVFrame;
  _Error    : Boolean;
begin
  Result := False;
  DecodedTimecode := 0;

  if flushBuffers then
    gv_FFmpeg.AVCodec.avcodec_flush_buffers(FAVCodecContext);

  if (inherited decodePacket(pkt.getRelatedObject().Packet, NeedMore, _Error)) and (FDecodedFrame <> nil) then
  begin
    _decFrame := FDecodedFrame.getRelatedObject().Frame;
    if _decFrame^.pts <> AV_NOPTS_VALUE then
      DecodedTimecode := _decFrame^.pts
    else
      DecodedTimecode := _decFrame^.best_effort_timestamp;
    isKeyFrame := _decFrame^.pict_type = AV_PICTURE_TYPE_I;
    Result := True;
  end;
end;


function TVideoDecoder.get_AspectRatio: Single;
begin
  if FAspectRatio <= 0 then
  begin
    if FAVCodecContext <> nil then
    begin
      if (FAVCodecContext^.sample_aspect_ratio.num > 0) and (FAVCodecContext^.sample_aspect_ratio.den <> 0) then
        FAspectRatio := (FAVCodecContext^.width * FAVCodecContext^.sample_aspect_ratio.num) /
                        (FAVCodecContext^.Height * FAVCodecContext^.sample_aspect_ratio.den)
      else
        FAspectRatio := FAVCodecContext^.width / FAVCodecContext^.Height;
    end;
  end;
  if FAspectRatio > 0 then Result := FAspectRatio
  else Result := AR_16_9_Value;
end;

constructor TVideoDecoder.Create();
begin
  inherited Create();
  FDecodedFrame := nil;
  FAspectRatio  := -1;
end;

destructor TVideoDecoder.Destroy();
begin
  FDecodedFrame := nil;
  inherited Destroy;
end;

{ TAVSourceReader }

// interrupt_callback: registered after avformat_open_input on the final context.
function AVInterruptCallback(aOpaque: Pointer): Integer; cdecl;
var
  _Data : TAVSourceReader.PAVInterruptData;

begin
  _Data := TAVSourceReader.PAVInterruptData(aOpaque);
  if (GetTickCount64 - _Data^.LastPacketTime) > _Data^.TimeoutMs then
  begin
    Result := 1;
    LBLogger.Write(1, 'AVInterruptCallback', lmt_Warning, 'Read timeout exceeded (%d ms)! Interrupting ...', [_Data^.TimeoutMs]);
  end
  else
    Result := 0;
end;

procedure TAVSourceReader.CloseInput;
begin
  try
    if gv_FFmpeg <> nil then
    begin
      if FAVFormatContext <> nil then
        gv_FFmpeg.AVFormat.avformat_close_input(FAVFormatContext);
    end;
    if FInputInfo <> nil then
      FreeAndNil(FInputInfo);
  except
    on E: Exception do
      LBLogger.Write(1, 'TAVSourceReader.CloseInput', lmt_Error, E.Message);
  end;
end;

constructor TAVSourceReader.Create();
begin
  inherited Create;
  FAVFormatContext := nil;
  FInputInfo       := nil;
  if not createFFmpegWrapper([]) then
    LBLogger.Write(1, 'TAVSourceReader.Create', lmt_Warning, 'FFMPEG wrapper not initialized!');
end;

destructor TAVSourceReader.Destroy();
begin
  Self.CloseInput();
  inherited Destroy;
end;

function TAVSourceReader.OpenInput(const anURL: AnsiString; out anErrorMsg: String; aDictionary: TAVDictionaryObj): Boolean;
var
  _res  : Integer;
  _tmp  : TAVInputInfo = nil;
  _dict : PAVDictionary = nil;

begin
  Result := False;

  if gv_FFmpeg <> nil then
  begin

    Self.CloseInput();

    try
      if (aDictionary <> nil) and (aDictionary.Count > 0) then
      begin
        for _res := 0 to aDictionary.Count - 1 do
          gv_FFmpeg.AVUtil.av_dict_set(@_dict, PAnsiChar(aDictionary.Keys[_res]), PAnsiChar(aDictionary.Data[_res]), 0);

        if _dict = nil then
          LBLogger.Write(1, 'TAVSourceReader.OpenInput', lmt_Warning, 'Dictionary not set!');
      end;

      LBLogger.Write(6, 'TAVSourceReader.OpenInput', lmt_Debug, 'Opening context for URL <%s>', [anURL]);
      If gv_FFmpeg.AVFormat.avformat_open_input(FAVFormatContext, anURL, Nil, @_dict, _res) then
      begin
        if (FAVFormatContext <> nil) then
        begin
          // Register interrupt callback on the final context after open
          FInterruptData.LastPacketTime := GetTickCount64;
          FInterruptData.TimeoutMs      := 10000;

          FAVFormatContext^.interrupt_callback.callback := @AVInterruptCallback;
          FAVFormatContext^.interrupt_callback.opaque   := @FInterruptData;

          LBLogger.Write(6, 'TAVSourceReader.OpenInput', lmt_Debug, 'Context opened ...');

          if Assigned(FAVFormatContext^.pb) then
          begin
            FAVFormatContext^.pb^.eof_reached := 0;
            FAVFormatContext^.pb^.error       := 0;
          end;

          _res := gv_FFmpeg.AVFormat.avformat_find_stream_info(FAVFormatContext, nil);
          if _res >= 0 then
          begin
            _tmp := TAVInputInfo.Create;
            if _tmp.FillFromContext(anURL, FAVFormatContext) then
            begin
              Result := True;
              FInputInfo := _tmp;
              _tmp := nil;
            end
            else
              anErrorMsg := 'Error reading file info!';
          end
          else
            anErrorMsg := 'Stream info not found!';
        end
        else
          anErrorMsg := Format('Context for URL <%s> opened but pointer is null!', [anURL]);
      end
      else
        anErrorMsg := Format('Error opening input <%s>: <%s>', [anURL, gv_FFmpeg.AVUtil.av_strerror(_res)]);

      if not Result then
      begin
        Self.CloseInput();
        LBLogger.Write(1, 'TAVSourceReader.OpenInput', lmt_Warning, '%s', [anErrorMsg]);
      end;

    except
      on E: Exception do
      begin
        anErrorMsg := E.Message;
        LBLogger.Write(1, 'TAVSourceReader.OpenInput', lmt_Error, 'Source: <%s>  -  %s', [anURL, E.Message]);
      end;
    end;

    if _tmp <> nil then
      _tmp.Free;

    if _dict <> nil then
      gv_FFmpeg.AVUtil.av_dict_free(@_dict);
  end
  else
    LBLogger.Write(1, 'TAVSourceReader.OpenInput', lmt_Warning, 'FFMPEG not initialized!');
  
end;

function TAVSourceReader.ReadStreamPacket(var aPacket: IAVPacketWrapper; aStreamIdx: Integer): Boolean;
var
  _StreamType : TAVStreamType;
  _Status     : TAVReadResult;
begin
  Result := False;
  repeat
    if Self.ReadPacket(aPacket, _StreamType, _Status) then
    begin
      if _Status = rr_OK then
      begin
        if aPacket.getRelatedObject().Packet^.stream_index = aStreamIdx then
        begin
          Result := True;
          Break;
        end
        else
          aPacket.getRelatedObject().clearPacket();
      end
      else
        aPacket.getRelatedObject().clearPacket();
    end
    else if _Status in [rr_EOF, rr_Error] then
    begin
      if aPacket <> nil then aPacket.getRelatedObject().clearPacket();
      Break;
    end;
  until False;
end;

function TAVSourceReader.ReadPacket(var aPacket: IAVPacketWrapper; out aStreamType: TAVStreamType; out aStatus: TAVReadResult): Boolean;
var
  _res    : Integer;
  _Packet : PAVPacket;
begin
  Result      := False;
  aStreamType := st_Unknown;
  aStatus     := rr_Error;

  try
    if aPacket = nil then
      aPacket := TAVPacketWrapper.Create() as IAVPacketWrapper
    else
      aPacket.getRelatedObject().clearPacket();

    _Packet := aPacket.getRelatedObject().Packet;
    _res := gv_FFmpeg.AVFormat.av_read_frame(FAVFormatContext, _Packet);

    if _res = gv_FFmpeg.AVUtil.AVERROR_AGAIN then
    begin
      aStatus := rr_TryAgain;
      if Assigned(FAVFormatContext^.pb) then
      begin
        FAVFormatContext^.pb^.eof_reached := 0;
        FAVFormatContext^.pb^.error := 0;
      end;
      Result := True;
    end
    else if _res = 0 then
    begin
      aStatus := rr_OK;
      FInterruptData.LastPacketTime := GetTickCount64;
      aStreamType := FInputInfo.getStreamTypeByStreamIndex(_Packet^.stream_index);
      Result := True;
    end
    else if _res = AVERROR_EOF then
      aStatus := rr_EOF;

  except
    on E: Exception do
      LBLogger.Write(1, 'TAVSourceReader.ReadPacket', lmt_Error, E.Message);
  end;
end;

function TAVSourceReader.SeekVideoFrame(anAbsoluteStreamIdx: Integer; aPos: Int64): Boolean;
begin
  Result := gv_FFmpeg.AVFormat.av_seek_frame(FAVFormatContext, anAbsoluteStreamIdx, aPos, AVSEEK_FLAG_BACKWARD) >= 0;
end;

{ TAudioParams }

function TAudioParams.get_ByteResolutionPerSample: Integer;
begin
  case FSampleFormat of
    AV_SAMPLE_FMT_NONE,
    AV_SAMPLE_FMT_NB    : Result := 0;
    AV_SAMPLE_FMT_U8,
    AV_SAMPLE_FMT_U8P   : Result := 1;
    AV_SAMPLE_FMT_S16,
    AV_SAMPLE_FMT_S16P  : Result := 2;
    AV_SAMPLE_FMT_S32,
    AV_SAMPLE_FMT_S32P,
    AV_SAMPLE_FMT_FLT,
    AV_SAMPLE_FMT_FLTP  : Result := 4;
    AV_SAMPLE_FMT_DBL,
    AV_SAMPLE_FMT_DBLP,
    AV_SAMPLE_FMT_S64,
    AV_SAMPLE_FMT_S64P  : Result := 8;
    else Result := 0;
  end;
end;

function TAudioParams.get_Bytes4Sample: Integer;
begin
  Result := get_ByteResolutionPerSample * FChannels;
end;

function TAudioParams.get_NeededPlanes(): Integer;
begin
  case FSampleFormat of
    AV_SAMPLE_FMT_U8P,
    AV_SAMPLE_FMT_S16P,
    AV_SAMPLE_FMT_S32P,
    AV_SAMPLE_FMT_FLTP,
    AV_SAMPLE_FMT_DBLP,
    AV_SAMPLE_FMT_S64P  : Result := FChannels;
    else Result := 1;
  end;
end;

function TAudioParams.isEqual(anAudioParams: TAudioParams): Boolean;
begin
  Result := (anAudioParams <> nil) and
            (FSampleRate    = anAudioParams.SampleRate) and
            (FChannels      = anAudioParams.Channels) and
            (FSampleFormat  = anAudioParams.SampleFormat) and
            (FChannelLayout = anAudioParams.ChannelLayout);
end;

function TAudioParams.getEmptyFrame(nb_samples: cint; allocateBuffers: Boolean): IAVFrameWrapper;
var
  _Frame      : PAVFrame;
  _FrameWrapper : TAVFrameWrapper;
begin
  _FrameWrapper := TAVFrameWrapper.Create;
  _Frame := _FrameWrapper.Frame;
  _Frame^.nb_samples     := nb_samples;
  _Frame^.format         := Integer(FSampleFormat);
  _Frame^.sample_rate    := FSampleRate;
  gv_FFmpeg.AVUtil.av_channel_layout_from_mask(@_Frame^.ch_layout, FChannelLayout);
  if allocateBuffers then
    gv_FFmpeg.AVUtil.av_frame_get_buffer(_Frame, 0);
  Result := _FrameWrapper as IAVFrameWrapper;
end;

{ TBufferAudioParams }

procedure TBufferAudioParams.setDefaultValues();
begin
  FSampleRate    := 8000;
  FChannels      := 1;
  FSampleFormat  := AV_SAMPLE_FMT_S16;
  FChannelLayout := AV_CH_LAYOUT_MONO;
  FBufferMsDuration := 0;
end;

constructor TBufferAudioParams.Create;
begin
  inherited Create;
  Self.setDefaultValues();
end;

function TBufferAudioParams.resampleNeeded(aFrame: PAVFrame; out FrameChannelLayout: Int64): Boolean;
begin
  gv_FFmpeg.AVUtil.av_channel_layout_default(@FrameChannelLayout, aFrame^.ch_layout.nb_channels);
  Result := (TAVSampleFormat(aFrame^.format) <> FSampleFormat) or
            (aFrame^.sample_rate <> FSampleRate) or
            (FrameChannelLayout <> FChannelLayout);
end;

function TBufferAudioParams.resampleNeeded(anInputParams: TAudioParams): Boolean;
begin
  Result := not Self.isEqual(anInputParams);
end;

function TBufferAudioParams.createBuffers(var aBuffersArray: TBufferArray; msFullDuration: Int64; samplesFullDuration: Int64): Boolean;
var
  i                : Integer;
  _NeededPlanes    : Integer;
  _SingleBufferSize: Integer;
  _TotalSamples    : Int64;
begin
  Result := False;
  try
    FBufferMsDuration := msFullDuration;
    _NeededPlanes := Self.get_NeededPlanes();

    if samplesFullDuration > 0 then
      _TotalSamples := samplesFullDuration
    else
      _TotalSamples := Round((msFullDuration * FSampleRate) / 1000);

    for i := 0 to High(aBuffersArray) do
    begin
      if i < _NeededPlanes then
      begin
        _SingleBufferSize := _TotalSamples * Self.get_ByteResolutionPerSample;
        SetLength(aBuffersArray[i], _SingleBufferSize);
        FillChar(aBuffersArray[i][0], _SingleBufferSize, 0);
      end
      else
        SetLength(aBuffersArray[i], 0);
    end;
    Result := True;
  except
    on E: Exception do
      LBLogger.Write(1, 'TBufferAudioParams.createBuffers', lmt_Error, E.Message);
  end;
end;

{ TAudioBufferData }

function TAudioBufferData.checkEncoderCompatibility(anEncoder: PAVCodec): Boolean;
begin
  Result := False;
  if FAudioParams <> nil then
  begin
    if gv_FFmpeg.AVCodec.isSupportedSampleFormat(anEncoder, FAudioParams.SampleFormat) then
      if gv_FFmpeg.AVCodec.isSupportedSampleRate(anEncoder, FAudioParams.SampleRate) then
        if gv_FFmpeg.AVCodec.isSupportedChannelLayout(anEncoder, FAudioParams.ChannelLayout) then
          Result := True;
  end;
end;

function TAudioBufferData.get_DurationByBufferOffset: Int64;
var
  _Samples : Int64;
begin
  _Samples := FBufferOffset div FAudioParams.Bytes4Sample;
  Result := Round((_Samples * 1000) / FAudioParams.SampleRate);
end;

function TAudioBufferData.get_FullDuration: Int64;
begin
  Result := FAudioParams.msDuration;
end;

procedure TAudioBufferData.destroyBuffers();
var
  i : Integer;
begin
  for i := 0 to High(FBuffers) do
  begin
    SetLength(FBuffers[i], 0);
    FBuffersPointers[i] := nil;
  end;
  FBufferOffset := 0;
end;

function TAudioBufferData.elaborateDecodedFrame(var aFrame: IAVFrameWrapper): Boolean;
begin
  Result := Self.insertDecodedFrameIntoBufferByByteOffset(aFrame);
end;

procedure TAudioBufferData.elaborateNewData(StartBytePos, Size: Int64);
begin
  // Override in subclasses for custom processing
end;

constructor TAudioBufferData.Create();
begin
  inherited Create;
  FAudioParams  := TBufferAudioParams.Create;
  FResampler    := nil;
  FBufferOffset := 0;
  Self.destroyBuffers();
end;

destructor TAudioBufferData.Destroy;
begin
  try
    Self.destroyBuffers();
    FreeAndNil(FAudioParams);
    gv_FFmpeg.SWResample.swr_free(FResampler);
  except
    on E: Exception do
      LBLogger.Write(1, 'TAudioBufferData.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

procedure TAudioBufferData.ClearDecoderContext();
begin
  inherited ClearDecoderContext();
  gv_FFmpeg.SWResample.swr_free(FResampler);
end;

function TAudioBufferData.createBuffers(msAudioDuration: Int64): Boolean;
begin
  Result := FAudioParams.createBuffers(FBuffers, msAudioDuration);
  FBufferOffset := 0;
  Self.setOffsetByTime(0);
end;

function TAudioBufferData.setOffsetByTime(msTimeOffset: Int64): Boolean;
var
  _Samples : Int64;
begin
  FBufferOffset := 0;
  _Samples := Round((msTimeOffset * FAudioParams.SampleRate) / 1000);
  Result := Self.incOffset(_Samples * FAudioParams.Bytes4Sample); // get_Bytes4Sample);
end;

function TAudioBufferData.incOffset(anOffset: Int64): Boolean;
var
  i         : Integer;
  _BuffSize : Integer;
begin
  Inc(FBufferOffset, anOffset);
  for i := 0 to High(FBuffers) do
  begin
    _BuffSize := Length(FBuffers[i]);
    if (_BuffSize > 0) and (_BuffSize > FBufferOffset) then
      FBuffersPointers[i] := @FBuffers[i][FBufferOffset]
    else
      FBuffersPointers[i] := nil;
  end;
  Result := True;
end;

function TAudioBufferData.msToBytesOffset(msTimeOffset: Int64): Int64;
begin
  Result := Round((msTimeOffset * FAudioParams.SampleRate) / 1000) * FAudioParams.Bytes4Sample; // get_Bytes4Sample;
end;

function TAudioBufferData.insertDecodedFrameIntoBufferByByteOffset(const aDecodedFrame: IAVFrameWrapper): Boolean;
var
  _ChannelLayout : Int64;
  _SamplesOut    : Int64;
  i              : Integer;
  _remain        : Int64;
  _InsertFrame   : Boolean = True;
  _decFrame      : PAVFrame;

begin
  Result := False;

  if aDecodedFrame <> nil then
  begin

    try
      _decFrame := aDecodedFrame.getRelatedObject().Frame;
      if _decFrame <> nil then
      begin
        if FAudioParams.resampleNeeded(_decFrame, _ChannelLayout) then
        begin
          if FResampler = nil then
          begin
            FResampler := gv_FFmpeg.SWResample.swr_alloc();
            gv_FFmpeg.AVUtil.av_opt_set_chlayout(FResampler, 'out_chlayout', @FAudioParams.ChannelLayout, 0);
            gv_FFmpeg.AVUtil.av_opt_set_sample_fmt(FResampler, 'out_sample_fmt', FAudioParams.SampleFormat, 0);
            gv_FFmpeg.AVUtil.av_opt_set_int(FResampler, 'out_sample_rate', FAudioParams.SampleRate, 0);
            gv_FFmpeg.AVUtil.av_opt_set_chlayout(FResampler, 'in_chlayout', @_ChannelLayout, 0);
            gv_FFmpeg.AVUtil.av_opt_set_sample_fmt(FResampler, 'in_sample_fmt', TAVSampleFormat(_decFrame^.format), 0);
            gv_FFmpeg.AVUtil.av_opt_set_int(FResampler, 'in_sample_rate', _decFrame^.sample_rate, 0);
            if gv_FFmpeg.SWResample.swr_init(FResampler) < 0 then
            begin
              gv_FFmpeg.SWResample.swr_free(FResampler);
              LBLogger.Write(1, 'TAudioBufferData.insertDecodedFrame', lmt_Warning, 'Error initializing resampler!');
            end;
          end;

          if FResampler <> nil then
          begin
            _SamplesOut := gv_FFmpeg.AVUtil.av_rescale_rnd(
              gv_FFmpeg.SWResample.swr_get_delay(FResampler, _decFrame^.sample_rate) + _decFrame^.nb_samples,
              FAudioParams.SampleRate, _decFrame^.sample_rate, AV_ROUND_UP);

            if _InsertFrame then
            begin
              _remain := Length(FBuffers[0]) - FBufferOffset - (_SamplesOut * FAudioParams.Bytes4Sample);
              if _remain >= 10 then
              begin
                _SamplesOut := gv_FFmpeg.SWResample.swr_convert(FResampler, FBuffersPointers, _SamplesOut, _decFrame^.data, _decFrame^.nb_samples);
                if _SamplesOut > 0 then
                begin
                  _SamplesOut *= FAudioParams.Bytes4Sample;
                  Self.elaborateNewData(FBufferOffset, _SamplesOut);
                  Self.incOffset(_SamplesOut);
                  Result := True;
                end;
              end;
            end
            else begin
              Self.incOffset(_SamplesOut * FAudioParams.Bytes4Sample);
              Result := True;
            end;
          end;
        end
        else begin
          if _InsertFrame then
          begin
            for i := 0 to High(FBuffersPointers) do
            begin
              if FBuffersPointers[i] <> nil then
                Move(_decFrame^.data[i], FBuffersPointers[i][0], _decFrame^.linesize[i])
              else
                Break;
            end;
            Self.elaborateNewData(FBufferOffset, _decFrame^.linesize[0]);
          end;
          Self.incOffset(_decFrame^.linesize[0]);
          Result := True;
        end;
      end;
    except
      on E: Exception do
        LBLogger.Write(1, 'TAudioBufferData.insertDecodedFrame', lmt_Error, E.Message);
    end;

  end
  else
    LBLogger.Write(1, 'TAudioBufferData.insertDecodedFrame', lmt_Warning, 'No decoded frame!');

end;

end.
