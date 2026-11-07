unit uFFmpeg.Types;

{$mode objfpc}{$H+}

interface

uses
  libavutil, libavformat, libavcodec, libswscale, libswresample;

type
  // Tipi base re-esportati
  PAVFrame           = libavutil.PAVFrame;
  TAVPixelFormat     = libavutil.TAVPixelFormat;
  TAVRational        = libavutil.TAVRational;
  TAVHWDeviceType    = libavutil.TAVHWDeviceType;
  PAVBufferRef       = libavutil.PAVBufferRef;
  TAVSampleFormat    = libavutil.TAVSampleFormat;
  PAVDictionary      = libavutil.PAVDictionary;
  PAVDictionaryEntry = libavutil.PAVDictionaryEntry;
  PAVPixelFormat     = libavutil.PAVPixelFormat;
  TAVChannelLayout   = libavutil.TAVChannelLayout;
  TAVChannelOrder    = libavutil.TAVChannelOrder;

  PAVStream          = libavformat.PAVStream;
  PAVFormatContext   = libavformat.PAVFormatContext;

  PAVCodec           = libavcodec.PAVCodec;
  PAVPacket          = libavcodec.PAVPacket;
  TAVPacket          = libavcodec.TAVPacket;
  PAVBSFContext      = libavcodec.PAVBSFContext;
  PAVCodecContext    = libavcodec.PAVCodecContext;
  TAVCodecContext    = libavcodec.TAVCodecContext;
  TAVCodecID         = libavcodec.TAVCodecID;
  PAVCodecHWConfig   = libavcodec.PAVCodecHWConfig;



  PSwsContext        = libswscale.pSwsContext;

  PSwrContext        = libswresample.PSwrContext;

const
  AV_PIX_FMT_DRM_PRIME = libavutil.AV_PIX_FMT_DRM_PRIME;

  AV_HWDEVICE_TYPE_NONE    = libavutil.AV_HWDEVICE_TYPE_NONE;
  AV_HWDEVICE_TYPE_VDPAU   = libavutil.AV_HWDEVICE_TYPE_VDPAU;
  AV_HWDEVICE_TYPE_CUDA    = libavutil.AV_HWDEVICE_TYPE_CUDA;
  AV_HWDEVICE_TYPE_VAAPI   = libavutil.AV_HWDEVICE_TYPE_VAAPI;
  AV_HWDEVICE_TYPE_DXVA2   = libavutil.AV_HWDEVICE_TYPE_DXVA2;
  AV_HWDEVICE_TYPE_QSV     = libavutil.AV_HWDEVICE_TYPE_QSV;
  AV_HWDEVICE_TYPE_VIDEOTOOLBOX = libavutil.AV_HWDEVICE_TYPE_VIDEOTOOLBOX;
  AV_HWDEVICE_TYPE_D3D11VA = libavutil.AV_HWDEVICE_TYPE_D3D11VA;
  AV_HWDEVICE_TYPE_DRM     = libavutil.AV_HWDEVICE_TYPE_DRM;
  AV_HWDEVICE_TYPE_OPENCL  = libavutil.AV_HWDEVICE_TYPE_OPENCL;
  AV_HWDEVICE_TYPE_MEDIACODEC = libavutil.AV_HWDEVICE_TYPE_MEDIACODEC;
  AV_HWDEVICE_TYPE_VULKAN  = libavutil.AV_HWDEVICE_TYPE_VULKAN;

  // Media types
  AVMEDIA_TYPE_UNKNOWN    = libavutil.AVMEDIA_TYPE_UNKNOWN;
  AVMEDIA_TYPE_VIDEO      = libavutil.AVMEDIA_TYPE_VIDEO;
  AVMEDIA_TYPE_AUDIO      = libavutil.AVMEDIA_TYPE_AUDIO;
  AVMEDIA_TYPE_DATA       = libavutil.AVMEDIA_TYPE_DATA;
  AVMEDIA_TYPE_SUBTITLE   = libavutil.AVMEDIA_TYPE_SUBTITLE;
  AVMEDIA_TYPE_ATTACHMENT = libavutil.AVMEDIA_TYPE_ATTACHMENT;
  AVMEDIA_TYPE_NB         = libavutil.AVMEDIA_TYPE_NB;

  // Sample formats
  AV_SAMPLE_FMT_NONE   = libavutil.AV_SAMPLE_FMT_NONE;
  AV_SAMPLE_FMT_U8     = libavutil.AV_SAMPLE_FMT_U8;
  AV_SAMPLE_FMT_S16    = libavutil.AV_SAMPLE_FMT_S16;
  AV_SAMPLE_FMT_S32    = libavutil.AV_SAMPLE_FMT_S32;
  AV_SAMPLE_FMT_FLT    = libavutil.AV_SAMPLE_FMT_FLT;
  AV_SAMPLE_FMT_DBL    = libavutil.AV_SAMPLE_FMT_DBL;
  AV_SAMPLE_FMT_U8P    = libavutil.AV_SAMPLE_FMT_U8P;
  AV_SAMPLE_FMT_S16P   = libavutil.AV_SAMPLE_FMT_S16P;
  AV_SAMPLE_FMT_S32P   = libavutil.AV_SAMPLE_FMT_S32P;
  AV_SAMPLE_FMT_FLTP   = libavutil.AV_SAMPLE_FMT_FLTP;
  AV_SAMPLE_FMT_DBLP   = libavutil.AV_SAMPLE_FMT_DBLP;
  AV_SAMPLE_FMT_S64    = libavutil.AV_SAMPLE_FMT_S64;
  AV_SAMPLE_FMT_S64P   = libavutil.AV_SAMPLE_FMT_S64P;
  AV_SAMPLE_FMT_NB     = libavutil.AV_SAMPLE_FMT_NB;

  // Pixel formats
  AV_PIX_FMT_NONE      = libavutil.AV_PIX_FMT_NONE;
  AV_PIX_FMT_YUV420P   = libavutil.AV_PIX_FMT_YUV420P;
  AV_PIX_FMT_YUYV422   = libavutil.AV_PIX_FMT_YUYV422;
  AV_PIX_FMT_RGB24     = libavutil.AV_PIX_FMT_RGB24;
  AV_PIX_FMT_BGR24     = libavutil.AV_PIX_FMT_BGR24;
  AV_PIX_FMT_YUV422P   = libavutil.AV_PIX_FMT_YUV422P;
  AV_PIX_FMT_YUV444P   = libavutil.AV_PIX_FMT_YUV444P;
  AV_PIX_FMT_YUV410P   = libavutil.AV_PIX_FMT_YUV410P;
  AV_PIX_FMT_YUV411P   = libavutil.AV_PIX_FMT_YUV411P;
  AV_PIX_FMT_GRAY8     = libavutil.AV_PIX_FMT_GRAY8;
  AV_PIX_FMT_ARGB      = libavutil.AV_PIX_FMT_ARGB;
  AV_PIX_FMT_RGBA      = libavutil.AV_PIX_FMT_RGBA;
  AV_PIX_FMT_ABGR      = libavutil.AV_PIX_FMT_ABGR;
  AV_PIX_FMT_BGRA      = libavutil.AV_PIX_FMT_BGRA;
  AV_PIX_FMT_YUVJ420P  = libavutil.AV_PIX_FMT_YUVJ420P;
  AV_PIX_FMT_YUVJ422P  = libavutil.AV_PIX_FMT_YUVJ422P;
  AV_PIX_FMT_YUVJ444P  = libavutil.AV_PIX_FMT_YUVJ444P;
  AV_PIX_FMT_UYVY422   = libavutil.AV_PIX_FMT_UYVY422;
  AV_PIX_FMT_UYYVYY411 = libavutil.AV_PIX_FMT_UYYVYY411;


  // Color spaces
  AVCOL_SPC_RGB                = libavutil.AVCOL_SPC_RGB;
  AVCOL_SPC_BT709              = libavutil.AVCOL_SPC_BT709;
  AVCOL_SPC_UNSPECIFIED        = libavutil.AVCOL_SPC_UNSPECIFIED;
  AVCOL_SPC_RESERVED           = libavutil.AVCOL_SPC_RESERVED;
  AVCOL_SPC_FCC                = libavutil.AVCOL_SPC_FCC;
  AVCOL_SPC_BT470BG            = libavutil.AVCOL_SPC_BT470BG;
  AVCOL_SPC_SMPTE170M          = libavutil.AVCOL_SPC_SMPTE170M;
  AVCOL_SPC_SMPTE240M          = libavutil.AVCOL_SPC_SMPTE240M;
  AVCOL_SPC_YCGCO              = libavutil.AVCOL_SPC_YCGCO;
  AVCOL_SPC_BT2020_NCL         = libavutil.AVCOL_SPC_BT2020_NCL;
  AVCOL_SPC_BT2020_CL          = libavutil.AVCOL_SPC_BT2020_CL;
  AVCOL_SPC_SMPTE2085          = libavutil.AVCOL_SPC_SMPTE2085;
  AVCOL_SPC_CHROMA_DERIVED_NCL = libavutil.AVCOL_SPC_CHROMA_DERIVED_NCL;
  AVCOL_SPC_CHROMA_DERIVED_CL  = libavutil.AVCOL_SPC_CHROMA_DERIVED_CL;
  AVCOL_SPC_ICTCP              = libavutil.AVCOL_SPC_ICTCP;
  AVCOL_SPC_NB                 = libavutil.AVCOL_SPC_NB;

  // Color range
  AVCOL_RANGE_UNSPECIFIED = libavutil.AVCOL_RANGE_UNSPECIFIED;
  AVCOL_RANGE_MPEG        = libavutil.AVCOL_RANGE_MPEG;
  AVCOL_RANGE_JPEG        = libavutil.AVCOL_RANGE_JPEG;
  AVCOL_RANGE_NB          = libavutil.AVCOL_RANGE_NB;

  // Chroma location
  AVCHROMA_LOC_UNSPECIFIED = libavutil.AVCHROMA_LOC_UNSPECIFIED;
  AVCHROMA_LOC_LEFT        = libavutil.AVCHROMA_LOC_LEFT;
  AVCHROMA_LOC_CENTER      = libavutil.AVCHROMA_LOC_CENTER;
  AVCHROMA_LOC_TOPLEFT     = libavutil.AVCHROMA_LOC_TOPLEFT;
  AVCHROMA_LOC_TOP         = libavutil.AVCHROMA_LOC_TOP;
  AVCHROMA_LOC_BOTTOMLEFT  = libavutil.AVCHROMA_LOC_BOTTOMLEFT;
  AVCHROMA_LOC_BOTTOM      = libavutil.AVCHROMA_LOC_BOTTOM;
  AVCHROMA_LOC_NB          = libavutil.AVCHROMA_LOC_NB;

  // Channel layout masks
  AV_CH_LAYOUT_MONO         = libavutil.AV_CH_LAYOUT_MONO;
  AV_CH_LAYOUT_STEREO       = libavutil.AV_CH_LAYOUT_STEREO;
  AV_CH_LAYOUT_2_1          = libavutil.AV_CH_LAYOUT_2_1;
  AV_CH_LAYOUT_SURROUND     = libavutil.AV_CH_LAYOUT_SURROUND;
  AV_CH_LAYOUT_4POINT0      = libavutil.AV_CH_LAYOUT_4POINT0;
  AV_CH_LAYOUT_2_2          = libavutil.AV_CH_LAYOUT_2_2;
  AV_CH_LAYOUT_QUAD         = libavutil.AV_CH_LAYOUT_QUAD;
  AV_CH_LAYOUT_5POINT0      = libavutil.AV_CH_LAYOUT_5POINT0;
  AV_CH_LAYOUT_5POINT1      = libavutil.AV_CH_LAYOUT_5POINT1;
  AV_CH_LAYOUT_7POINT1      = libavutil.AV_CH_LAYOUT_7POINT1;
  AV_CH_LAYOUT_7POINT1_WIDE = libavutil.AV_CH_LAYOUT_7POINT1_WIDE;

  AV_CODEC_ID_NONE        = libavcodec.AV_CODEC_ID_NONE;
  AV_CODEC_ID_MPEG1VIDEO  = libavcodec.AV_CODEC_ID_MPEG1VIDEO;
  AV_CODEC_ID_MPEG2VIDEO  = libavcodec.AV_CODEC_ID_MPEG2VIDEO;
  AV_CODEC_ID_H261        = libavcodec.AV_CODEC_ID_H261;
  AV_CODEC_ID_H263        = libavcodec.AV_CODEC_ID_H263;
  AV_CODEC_ID_H264        = libavcodec.AV_CODEC_ID_H264;
  AV_CODEC_ID_H265        = libavcodec.AV_CODEC_ID_H265;
  AV_CODEC_ID_HEVC        = libavcodec.AV_CODEC_ID_HEVC;
  AV_CODEC_ID_VP8         = libavcodec.AV_CODEC_ID_VP8;
  AV_CODEC_ID_VP9         = libavcodec.AV_CODEC_ID_VP9;
  AV_CODEC_ID_AV1         = libavcodec.AV_CODEC_ID_AV1;

  AV_CODEC_ID_MP2         = libavcodec.AV_CODEC_ID_MP2;
  AV_CODEC_ID_MP3         = libavcodec.AV_CODEC_ID_MP3;
  AV_CODEC_ID_AAC         = libavcodec.AV_CODEC_ID_AAC;
  AV_CODEC_ID_AC3         = libavcodec.AV_CODEC_ID_AC3;
  AV_CODEC_ID_EAC3        = libavcodec.AV_CODEC_ID_EAC3;
  AV_CODEC_ID_FLAC        = libavcodec.AV_CODEC_ID_FLAC;
  AV_CODEC_ID_OPUS        = libavcodec.AV_CODEC_ID_OPUS;
  AV_CODEC_ID_VORBIS      = libavcodec.AV_CODEC_ID_VORBIS;
  AV_CODEC_ID_PCM_S16LE   = libavcodec.AV_CODEC_ID_PCM_S16LE;
  AV_CODEC_ID_PCM_S16BE   = libavcodec.AV_CODEC_ID_PCM_S16BE;

  AV_CODEC_ID_SUBRIP      = libavcodec.AV_CODEC_ID_SUBRIP;
  AV_CODEC_ID_ASS         = libavcodec.AV_CODEC_ID_ASS;
  AV_CODEC_ID_DVB_SUBTITLE= libavcodec.AV_CODEC_ID_DVB_SUBTITLE;

  // Windows Media Audio
  AV_CODEC_ID_WMAV1      = libavcodec.AV_CODEC_ID_WMAV1;
  AV_CODEC_ID_WMAV2      = libavcodec.AV_CODEC_ID_WMAV2;

  // Codec vocali compressi
  AV_CODEC_ID_GSM        = libavcodec.AV_CODEC_ID_GSM;
  AV_CODEC_ID_G723_1     = libavcodec.AV_CODEC_ID_G723_1;
  AV_CODEC_ID_G729       = libavcodec.AV_CODEC_ID_G729;


  // PCM
  AV_CODEC_ID_PCM_MULAW  = libavcodec.AV_CODEC_ID_PCM_MULAW;
  AV_CODEC_ID_PCM_ALAW   = libavcodec.AV_CODEC_ID_PCM_ALAW;

  // ADPCM
  AV_CODEC_ID_ADPCM_G722 = libavcodec.AV_CODEC_ID_ADPCM_G722;

  AV_PKT_FLAG_KEY        = libavcodec.AV_PKT_FLAG_KEY;




  // Error codes and special values
  AVERROR_EOF              = libavutil.AVERROR_EOF;
  AV_OPT_SEARCH_CHILDREN   = libavutil.AV_OPT_SEARCH_CHILDREN;
  AV_NUM_DATA_POINTERS     = libavutil.AV_NUM_DATA_POINTERS;
  AV_PICTURE_TYPE_I        = libavutil.AV_PICTURE_TYPE_I;
  AV_ROUND_UP              = libavutil.AV_ROUND_UP;
  AV_NOPTS_VALUE           = Int64($8000000000000000);


  AV_TIME_BASE_I = libavutil.AV_TIME_BASE_I;
  AV_TIME_BASE_Q: TAVRational = (num: 1; den: AV_TIME_BASE_I);

  SWS_FAST_BILINEAR = libswscale.SWS_FAST_BILINEAR;

  AVFMT_GLOBALHEADER        = libavformat.AVFMT_GLOBALHEADER;
  AVIO_FLAG_WRITE = libavformat.AVIO_FLAG_WRITE;
  AVSEEK_FLAG_BACKWARD = libavformat.AVSEEK_FLAG_BACKWARD;

  AV_CODEC_FLAG_GLOBAL_HEADER = libavcodec.AV_CODEC_FLAG_GLOBAL_HEADER;
  AV_CODEC_FLAG_LOW_DELAY = libavcodec.AV_CODEC_FLAG_LOW_DELAY;
  AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX = libavcodec.AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX;



implementation

end.
