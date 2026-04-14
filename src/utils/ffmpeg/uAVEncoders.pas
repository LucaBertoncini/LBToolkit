unit uAVEncoders;

{$mode objfpc}{$H+}

(*
  uAVEncoders - FFmpeg encoding/writing wrapper for Free Pascal

  Depends on uAVStructures (LBToolkit).
  Tested with FFmpeg 7.x (libavcodec.so.61, libavformat.so.61)
*)

interface

uses
  Classes, SysUtils, uLBBaseThread, uTimedoutCriticalSection,
  uFFmpeg, uFFmpeg.Types, fgl,
  uAVStructures;

type

  TEncodingData = record
    dataBuffer  : pByte;
    BufferLen   : Integer;
    pts         : QWord;
    pixelFormat : TAVPixelFormat;
    CopyNeeded  : Boolean;
  end;
  pEncodingData = ^TEncodingData;

  TStreamFilterCollection = specialize TFPGMap<Integer, PAVBSFContext>;
  TPTSList                = specialize TFPGList<QWord>;

  { TH264_YUV420P_Encoder }

  TH264_YUV420P_Encoder = class(TLBBaseThread)
    strict private
      FOnEncodedPacketReady : TNotifyEvent;
      FNewDataAvailable     : PRTLEvent;
      FAdditionalData       : TObject;
      FScaleContext         : PSwsContext;
      FHeight               : Integer;
      FWidth                : Integer;
      FLineSize             : Integer;
      FImageSize            : Integer;
      FFPS                  : Byte;
      FFrames               : TList;
      FCSFrames             : TTimedOutCriticalSection;
      FEncodedPacket        : IAVPacketWrapper;
      FEncoderContext       : PAVCodecContext;
      FFrameToEncode        : TAVFrameWrapper;
      FDefaultInputPixelFormat : TAVPixelFormat;
      FPTS                  : TPTSList;
      FEncoderInitialized   : Boolean;
      FStreamIdx            : Integer;

      function get_Bitrate: Integer;
      function InitializeEncoder(aBitrate, aFPS: Integer; const aPreset: String): Boolean;
      procedure EncodeNullPacket();
      procedure ClearFrames();

    protected
      procedure Execute; override;

    public
      constructor Create(aBitrate: Integer; aFPS: Integer; aWidth, aHeight: Integer; const aPreset: String); reintroduce;
      destructor Destroy; override;
      procedure Terminate; override;

      function addDataToEncode(aDataToEncode: pEncodingData): Boolean;
      function EncodeFrame(aDataToEncode: pEncodingData; out anEncodedPacket: TAVPacketWrapper): Boolean;
      function addStreamToContainer(aContainer: PAVFormatContext; out aStreamIdx: Integer): Boolean;

      property AdditionalData: TObject read FAdditionalData write FAdditionalData;
      property Bitrate: Integer read get_Bitrate;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property FPS: Byte read FFPS;
      property DefaultInputPixelFormat: TAVPixelFormat read FDefaultInputPixelFormat write FDefaultInputPixelFormat;
      property SuccesfullyInitialized: Boolean read FEncoderInitialized;
      property EncodedPacket: IAVPacketWrapper read FEncodedPacket;
      property OnEncodedPacketReady: TNotifyEvent write FOnEncodedPacketReady;
      property StreamIdx: Integer read FStreamIdx write FStreamIdx;
  end;

  { TAVFileWriter }

  TAVFileWriter = class(TObject)
    strict protected
      FContainer          : PAVFormatContext;
      FSuccessfullyOpened : Boolean;

      function addStreams(): Boolean; virtual;
      function OpenFile(const aDestinationFile: AnsiString; out anErrorMsg: String): Boolean;

    public
      constructor Create; virtual;
      destructor Destroy; override;

      function WriteEncodedPacket(const aPacket: IAVPacketWrapper): Boolean;
      procedure closeFile(); virtual;
  end;

  { TH264_FileWriter }

  TH264_FileWriter = class(TAVFileWriter)
    strict private
      FEncoder  : TH264_YUV420P_Encoder;
      FThreaded : Boolean;

      procedure writeFrame(Sender: TObject);

    strict protected
      function addStreams(): Boolean; override;

    public
      constructor Create(aBitrate: Integer; aFPS: Integer; aWidth, aHeight: Integer; const aPreset: String; Threaded: Boolean = False); reintroduce;
      destructor Destroy; override;

      function createFile(aDestinationFileName: String): Boolean;
      procedure closeFile(); override;
      function addFrame(anImage: pByte; anImageSize: Integer; aPixelFormat: TAVPixelFormat; aPTS: QWord; isKeyFrame: Boolean): Boolean;
  end;

  { TAudioEncoder }

  TAudioEncoder = class(TObject)
    strict private
      FAudioParams           : TBufferAudioParams;
      FEncoderContext        : PAVCodecContext;
      FDestroyEncoderContext : Boolean;
      FUseReadingOffset      : Boolean;
      FEncodingOffset        : Int64;
      FEncodingFrame         : IAVFrameWrapper;
      FMaxEncodingOffset     : Int64;
      FBuffers               : PBufferArray;
      FPTSValue              : Int64;

      procedure ClearEncoderContext();
      function checkEncoderCompatibility(anEncoder: PAVCodec): Boolean;
      function get_FrameSize: Int64;

    public
      constructor Create(aBuffers: PBufferArray; aAudioParams: TBufferAudioParams; aUseReadingOffset: Boolean); reintroduce;
      destructor Destroy; override;

      function setEncoderParams(anAudioCodec: TAVCodecID; aBitrate: Int64 = 64000): Boolean;
      function getEncodedPackets(aPacketsList: TAVPacketWrapperList; out isEOF: Boolean; FlushBuffer: Boolean): Boolean;
      function addStreamToContainer(aContainer: PAVFormatContext; anAudioCodec: TAVCodecID): Boolean;

      property FrameSize: Int64 read get_FrameSize;
      property MaxEncodingOffset: Int64 write FMaxEncodingOffset;
  end;

  { TAVFileRewriter }

  TAVFileRewriter = class(TAVFileWriter)
    strict private
      FSourceFile      : String;
      FDestinationFile : String;
      FReader          : TAVFileReader;

      function addStreamToContainer(aStream: PAVStream): Boolean;

    strict protected
      function addStreams(): Boolean; override;

    public
      destructor Destroy; override;
      function ReWrite(): Boolean;

      property SourceFile: String write FSourceFile;
      property DestinationFile: String write FDestinationFile;
  end;

  { TAVFileAssembler }

  TAVFileAssembler = class(TAVFileWriter)
    strict private
      FFilesToAssemble : TStringList;
      FDestinationFile : String;
      FReader          : TAVFileReader;

      function addStreamToContainer(aStream: PAVStream): Boolean;

    strict protected
      function addStreams(): Boolean; override;

    public
      constructor Create; override;
      destructor Destroy; override;

      function WriteFile(): Boolean;

      property SourceFiles: TStringList read FFilesToAssemble;
      property DestinationFile: String write FDestinationFile;
  end;

  { TAudioFileWriter }

  TAudioFileWriter = class(TAVFileWriter)
    strict private
      FAudioEncoder      : TAudioEncoder;
      FAudioBuffer       : TAudioBufferData;
      FAudioCodec        : TAVCodecID;
      FBitRate           : Int64;
      FMaxEncodingOffset : Int64;

      procedure set_AudioBuffer(AValue: TAudioBufferData);
      procedure set_MaxEncodingOffsetByDuration(AValue: Int64);

    strict protected
      function addStreams(): Boolean; override;

    public
      constructor Create(anAudioCodec: TAVCodecID; aBitrate: Integer = 64000); reintroduce;
      destructor Destroy; override;

      function createFile(aDestinationFileName: String): Boolean;

      property AudioBuffer: TAudioBufferData write set_AudioBuffer;
      property MaxEncodingOffset: Int64 write FMaxEncodingOffset;
      property MaxEncodingOffsetByDuration: Int64 write set_MaxEncodingOffsetByDuration;
  end;

  { TAudioStreamFileWriter }

  TAudioStreamFileWriter = class(TAVFileWriter)
    strict private
      FEncoder     : TAudioEncoder;
      FPackets     : TAVPacketWrapperList;
      FAudioCodec  : TAVCodecID;
      FBitrate     : Integer;
      FPts         : Int64;
      FAudioParams : TBufferAudioParams;

      procedure ClearPackets();

    strict protected
      function addStreams(): Boolean; override;

    public
      constructor Create(pcmBuffers: PBufferArray; aAudioParams: TBufferAudioParams; aAudioCodec: TAVCodecID; aBitrate: Integer); reintroduce;
      destructor Destroy; override;

      function openDestinationFile(const aDestinationFileName: String): Boolean;
      function write(aFlushBuffers: Boolean): Boolean;
      procedure Close();
  end;


implementation

uses
  ULBLogger;

const
  cHardware_h264_V4L2m2m = String('h264_v4l2m2m');

{ TAVFileWriter }

procedure TAVFileWriter.closeFile();
begin
  if FContainer = nil then Exit;
  try
    if FSuccessfullyOpened then
      gv_FFmpeg.AVFormat.av_write_trailer(FContainer)
    else
      LBLogger.Write(1, 'TAVFileWriter.closeFile', lmt_Warning, 'File not opened, trailer not written!');
    gv_FFmpeg.AVFormat.avformat_close_input(FContainer);
    FSuccessfullyOpened := False;
  except
    on E: Exception do
      LBLogger.Write(1, 'TAVFileWriter.closeFile', lmt_Error, E.Message);
  end;
end;

function TAVFileWriter.addStreams(): Boolean;
begin
  Result := False;
end;

constructor TAVFileWriter.Create;
begin
  inherited Create;
  FContainer := nil;
  FSuccessfullyOpened := False;
  if not createFFmpegWrapper([]) then
    LBLogger.Write(1, 'TAVFileWriter.Create', lmt_Warning, 'FFMPEG wrapper not initialized!');
end;

destructor TAVFileWriter.Destroy;
begin
  Self.closeFile();
  inherited Destroy;
end;

function TAVFileWriter.OpenFile(const aDestinationFile: AnsiString; out anErrorMsg: String): Boolean;
var
  _res : Integer;
begin
  Result := False;
  if Length(aDestinationFile) > 0 then
  begin
    try
      gv_FFmpeg.AVFormat.avformat_alloc_output_context2(@FContainer, nil, nil, PAnsiChar(aDestinationFile));
      if FContainer <> nil then
      begin
        if Self.addStreams() then
        begin
          _res := gv_FFmpeg.AVFormat.avio_open(@FContainer^.pb, PAnsiChar(aDestinationFile), AVIO_FLAG_WRITE);
          if _res >= 0 then
          begin
            _res := gv_FFmpeg.AVFormat.avformat_write_header(FContainer, nil);
            if _res >= 0 then
              Result := True
            else
              LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Warning, 'Error writing output file headers: %s', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
          end
          else
            LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Warning, 'Error opening output file <%s>: %s', [aDestinationFile, gv_FFmpeg.AVUtil.av_strerror(_res)]);
        end
        else
          LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Warning, 'Error inserting streams!');
      end
      else
        LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Warning, 'Cannot allocate output format context');
    except
      on E: Exception do
        LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Error, E.Message);
    end;
  end
  else
    LBLogger.Write(1, 'TAVFileWriter.OpenFile', lmt_Warning, 'No output file name!');
  FSuccessfullyOpened := Result;
end;

function TAVFileWriter.WriteEncodedPacket(const aPacket: IAVPacketWrapper): Boolean;
var
  _res : Integer;
begin
  Result := False;
  try
    _res := gv_FFmpeg.AVFormat.av_interleaved_write_frame(FContainer, aPacket.getRelatedObject().Packet);
    if _res >= 0 then
      Result := True
    else
      LBLogger.Write(1, 'TAVFileWriter.WriteEncodedPacket', lmt_Warning, 'Error writing packet: <%s>', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
  except
    on E: Exception do
      LBLogger.Write(1, 'TAVFileWriter.WriteEncodedPacket', lmt_Error, E.Message);
  end;
end;

{ TH264_YUV420P_Encoder }

function TH264_YUV420P_Encoder.InitializeEncoder(aBitrate, aFPS: Integer; const aPreset: String): Boolean;
var
  _Encoder : PAVCodec;
  _Frame   : PAVFrame;
  _res     : Integer;
const
  cPreset : PChar = 'preset';
  cSlow   : PChar = 'slow';
begin
  Result := False;
  if (FHeight > 0) and (FWidth > 0) then
  begin
    _Encoder := gv_FFmpeg.AVCodec.avcodec_find_encoder_by_name(cHardware_h264_V4L2m2m);
    if _Encoder = nil then
      _Encoder := gv_FFmpeg.AVCodec.avcodec_find_encoder(AV_CODEC_ID_H264);

    if _Encoder <> nil then
    begin
      try
        FEncoderContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Encoder);
        if FEncoderContext <> nil then
        begin
          FEncoderContext^.bit_rate      := aBitrate;
          FEncoderContext^.width         := FWidth;
          FEncoderContext^.height        := FHeight;
          FEncoderContext^.framerate.num := 1;
          FEncoderContext^.framerate.den := aFPS;
          FEncoderContext^.time_base.num := 1;
          FEncoderContext^.time_base.den := 1000;
          FEncoderContext^.pix_fmt       := AV_PIX_FMT_YUV420P;
          FEncoderContext^.delay         := 0;
          FEncoderContext^.thread_count  := 0;

          {$IFNDEF RaspHwAccel}
          FEncoderContext^.gop_size  := aFPS;
          FEncoderContext^.keyint_min := 0;
          FEncoderContext^.qmin := 10;
          FEncoderContext^.qmax := 51;
          {$ENDIF}

          if Length(aPreset) = 0 then
            gv_FFmpeg.AVUtil.av_opt_set(FEncoderContext, cPreset, cSlow, 0)
          else
            gv_FFmpeg.AVUtil.av_opt_set(FEncoderContext, cPreset, PAnsiChar(aPreset), 0);

          gv_FFmpeg.AVUtil.av_opt_set(FEncoderContext, 'crf', '0', 0);
          gv_FFmpeg.AVUtil.av_opt_set(FEncoderContext, 'qp', '0', 0);

          FLineSize  := FWidth * 2;
          FImageSize := FLineSize * FHeight;

          if gv_FFmpeg.AVCodec.avcodec_open2(FEncoderContext, _Encoder, nil) then
          begin
            gv_FFmpeg.AVCodec.avcodec_flush_buffers(FEncoderContext);
            _Frame := FFrameToEncode.Frame;
            _Frame^.width  := FEncoderContext^.width;
            _Frame^.height := FEncoderContext^.height;
            _Frame^.format := Integer(FEncoderContext^.pix_fmt);
            _res := gv_FFmpeg.AVUtil.av_image_alloc(_Frame^.data, _Frame^.linesize,
              FEncoderContext^.width, FEncoderContext^.height, FEncoderContext^.pix_fmt, 1);
            if _res > 0 then
            begin
              FFrameToEncode.Used_av_image_alloc := True;
              Result := True;
              _Frame^.pts := 0;
            end
            else
              LBLogger.Write(1, 'TH264_YUV420P_Encoder.InitializeEncoder', lmt_Warning, 'Error allocating picture: %s', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
          end
          else
            LBLogger.Write(1, 'TH264_YUV420P_Encoder.InitializeEncoder', lmt_Warning, 'Error opening codec!');
        end;
      except
        on E: Exception do
          LBLogger.Write(1, 'TH264_YUV420P_Encoder.InitializeEncoder', lmt_Error, E.Message);
      end;
    end
    else
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.InitializeEncoder', lmt_Warning, 'Encoder not found!');
  end
  else
    LBLogger.Write(1, 'TH264_YUV420P_Encoder.InitializeEncoder', lmt_Warning, 'Wrong frame height or width!');
end;

procedure TH264_YUV420P_Encoder.EncodeNullPacket;
var
  _EncodedPacket : TAVPacketWrapper = nil;
begin
  if Self.EncodeFrame(nil, _EncodedPacket) then
  begin
    if Assigned(FOnEncodedPacketReady) then
    begin
      _EncodedPacket.PacketType := avpt_Video;
      FEncodedPacket := _EncodedPacket as IAVPacketWrapper;
      FOnEncodedPacketReady(Self);
    end;
  end
  else
  begin
    if _EncodedPacket <> nil then FreeAndNil(_EncodedPacket);
  end;
end;

procedure TH264_YUV420P_Encoder.ClearFrames;
var
  i : Integer;
begin
  if (FFrames = nil) or (not FCSFrames.Acquire('TH264_YUV420P_Encoder.ClearFrames')) then Exit;
  try
    for i := 0 to FFrames.Count - 1 do
      Dispose(pEncodingData(FFrames.Items[i]));
    FFrames.Clear;
  except
    on E: Exception do
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.ClearFrames', lmt_Error, E.Message);
  end;
  FCSFrames.Release();
end;

procedure TH264_YUV420P_Encoder.Execute;
var
  _dataToEncode  : pEncodingData;
  _EncodedPacket : TAVPacketWrapper = nil;
const
  cWaitBeforeNextFrame = Integer(100);
begin
  while not Self.Terminated do
  begin
    _dataToEncode := nil;
    if FCSFrames.Acquire('TH264_YUV420P_Encoder.Execute') then
    begin
      try
        if FFrames.Count > 0 then
        begin
          _dataToEncode := FFrames.Items[0];
          FFrames.Delete(0);
        end;
      except
        on E: Exception do
          LBLogger.Write(1, 'TH264_YUV420P_Encoder.Execute', lmt_Error, E.Message);
      end;
      FCSFrames.Release();
    end;

    if _dataToEncode <> nil then
    begin
      if Self.EncodeFrame(_dataToEncode, _EncodedPacket) then
      begin
        if Assigned(FOnEncodedPacketReady) then
        begin
          _EncodedPacket.PacketType := avpt_Video;
          FEncodedPacket := _EncodedPacket as IAVPacketWrapper;
          FOnEncodedPacketReady(Self);
        end;
      end
      else
      begin
        if _EncodedPacket <> nil then FreeAndNil(_EncodedPacket);
      end;
      Dispose(_dataToEncode);
    end
    else
      RTLEventWaitFor(FNewDataAvailable, cWaitBeforeNextFrame);
  end;
  Self.EncodeNullPacket();
end;

function TH264_YUV420P_Encoder.get_Bitrate: Integer;
begin
  Result := 0;
  if FEncoderContext <> nil then
    Result := FEncoderContext^.bit_rate;
end;

constructor TH264_YUV420P_Encoder.Create(aBitrate: Integer; aFPS: Integer; aWidth, aHeight: Integer; const aPreset: String);
begin
  inherited Create;
  createFFmpegWrapper([]);
  Self.setThreadName('H264_Encoder');
  FHeight := aHeight;
  FWidth  := aWidth;
  FFPS    := aFPS;
  FScaleContext   := nil;
  FEncoderContext := nil;
  FNewDataAvailable := RTLEventCreate;
  FDefaultInputPixelFormat := TAVPixelFormat.AV_PIX_FMT_NONE;
  FFrameToEncode := TAVFrameWrapper.Create;
  FPTS    := TPTSList.Create;
  FFrames := TList.Create;
  FCSFrames := TTimedOutCriticalSection.Create;
  FEncoderInitialized := Self.InitializeEncoder(aBitrate, aFPS, aPreset);
  if not FEncoderInitialized then
    LBLogger.Write(1, 'TH264_YUV420P_Encoder.Create', lmt_Warning, 'Error initializing encoder!');
end;

destructor TH264_YUV420P_Encoder.Destroy;
begin
  try
    RTLEventSetEvent(FNewDataAvailable);
    RTLEventDestroy(FNewDataAvailable);
    Self.ClearFrames();
    FreeAndNil(FCSFrames);
    FreeAndNil(FFrames);
    if FScaleContext <> nil then
    begin
      gv_FFmpeg.SWScale.sws_freeContext(FScaleContext);
      FScaleContext := nil;
    end;
    if FEncoderContext <> nil then
      gv_FFmpeg.AVCodec.avcodec_free_context(FEncoderContext);
    FreeAndNil(FFrameToEncode);
    FreeAndNil(FPTS);
  except
    on E: Exception do
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

procedure TH264_YUV420P_Encoder.Terminate;
begin
  RTLEventSetEvent(FNewDataAvailable);
  inherited Terminate;
end;

function TH264_YUV420P_Encoder.addDataToEncode(aDataToEncode: pEncodingData): Boolean;
begin
  Result := False;
  if not FCSFrames.Acquire('TH264_YUV420P_Encoder.addDataToEncode') then Exit;
  try
    FFrames.Add(aDataToEncode);
    Result := True;
    RTLEventSetEvent(FNewDataAvailable);
  except
    on E: Exception do
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.addDataToEncode', lmt_Error, E.Message);
  end;
  FCSFrames.Release();
end;

function TH264_YUV420P_Encoder.EncodeFrame(aDataToEncode: pEncodingData; out anEncodedPacket: TAVPacketWrapper): Boolean;
var
  _res     : Integer;
  _srcData : array [0 .. AV_NUM_DATA_POINTERS - 1] of pByte;
  _srcLen  : array [0 .. AV_NUM_DATA_POINTERS - 1] of Integer;
  _Frame   : PAVFrame;
  _Packet  : TAVPacketWrapper;
begin
  Result := False;
  anEncodedPacket := nil;

  if FEncoderContext <> nil then
  begin
    if (aDataToEncode <> nil) then
    begin
      if aDataToEncode^.pixelFormat = AV_PIX_FMT_NONE then
      begin
        anEncodedPacket := TAVPacketWrapper.Create;
        anEncodedPacket.Packet^.size := aDataToEncode^.BufferLen;
        anEncodedPacket.Packet^.pts  := aDataToEncode^.pts;
        anEncodedPacket.Packet^.data := gv_FFmpeg.AVUtil.av_malloc(aDataToEncode^.BufferLen);
        move(aDataToEncode^.dataBuffer, anEncodedPacket.Packet^.data[0], aDataToEncode^.BufferLen);
        Result := True;
        Exit;
      end
      else begin
        _Frame := FFrameToEncode.Frame;
        FScaleContext := gv_FFmpeg.SWScale.sws_getCachedContext(FScaleContext,
          FWidth, FHeight, Integer(aDataToEncode^.pixelFormat),
          FWidth, FHeight, Integer(FEncoderContext^.pix_fmt),
          SWS_FAST_BILINEAR, nil, nil, nil);

        FillChar(_srcData[0], sizeOf(_srcData), 0);
        FillChar(_srcLen[0], SizeOf(_srcLen), 0);
        _srcData[0] := aDataToEncode^.dataBuffer;

        case aDataToEncode^.pixelFormat of
          AV_PIX_FMT_RGB24   : _srcLen[0] := aDataToEncode^.BufferLen div FHeight;
          AV_PIX_FMT_YUYV422 : _srcLen[0] := aDataToEncode^.BufferLen div FHeight;
          AV_PIX_FMT_YUV420P :
            begin
              _srcData[1] := _srcData[0] + (FWidth * FHeight);
              _srcData[2] := _srcData[1] + ((FWidth * FHeight) div 4);
              _srcLen[0]  := FWidth;
              _srcLen[1]  := FWidth div 2;
              _srcLen[2]  := FWidth div 2;
            end;
        end;

        gv_FFmpeg.SWScale.sws_scale(FScaleContext, _srcData, _srcLen, 0, FHeight, _Frame^.data, _Frame^.linesize);
        FPTS.Add(aDataToEncode^.pts);
      end;
    end
    else begin
      _Frame := nil;
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.EncodeFrame', lmt_Debug, 'No image, flushing encoder ...');
    end;

    _res := gv_FFmpeg.AVCodec.avcodec_send_frame(FEncoderContext, _Frame);
    if _res >= 0 then
    begin
      _Packet := TAVPacketWrapper.Create;
      _res := gv_FFmpeg.AVCodec.avcodec_receive_packet(FEncoderContext, _Packet.Packet);
      if (_res = 0) then
      begin
        anEncodedPacket := _Packet;
        if FPTS.Count > 0 then
          _Packet.Packet^.pts := FPTS.Extract(FPTS.Items[0]);
        Result := True;
      end
      else begin
        FreeAndNil(_Packet);
        LBLogger.Write(1, 'TH264_YUV420P_Encoder.EncodeFrame', lmt_Warning, 'Packet not received: %s', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
      end;
    end
    else
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.EncodeFrame', lmt_Warning, 'Error sending frame: %s', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
  end;
end;

function TH264_YUV420P_Encoder.addStreamToContainer(aContainer: PAVFormatContext; out aStreamIdx: Integer): Boolean;
var
  _Stream : PAVStream;
begin
  Result := False;
  try
    if (aContainer <> nil) and (FEncoderContext <> nil) then
    begin
      _Stream := gv_FFmpeg.AVFormat.avformat_new_stream(aContainer, FEncoderContext^.codec);
      if _Stream <> nil then
      begin
        gv_FFmpeg.AVCodec.avcodec_parameters_from_context(_Stream^.codecpar, FEncoderContext);
        _Stream^.codecpar^.extradata := gv_FFmpeg.AVUtil.av_mallocz(64);
        _Stream^.codecpar^.extradata_size := 64;
        aStreamIdx := _Stream^.index;
        Result := True;
      end
      else
        LBLogger.Write(1, 'TH264_YUV420P_Encoder.addStreamToContainer', lmt_Warning, 'Stream not added!');
    end
    else
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.addStreamToContainer', lmt_Warning, 'Container or encoder not set!');
  except
    on E: Exception do
      LBLogger.Write(1, 'TH264_YUV420P_Encoder.addStreamToContainer', lmt_Error, E.Message);
  end;
end;

{ TH264_FileWriter }

procedure TH264_FileWriter.writeFrame(Sender: TObject);
begin
  Self.WriteEncodedPacket(FEncoder.EncodedPacket);
end;

function TH264_FileWriter.addStreams: Boolean;
var
  _StreamIdx : Integer;
begin
  if FContainer = nil then Exit(False);
  FContainer^.bit_rate := FEncoder.Bitrate;
  Result := FEncoder.addStreamToContainer(FContainer, _StreamIdx);
end;

constructor TH264_FileWriter.Create(aBitrate: Integer; aFPS: Integer; aWidth, aHeight: Integer; const aPreset: String; Threaded: Boolean);
begin
  inherited Create;
  FThreaded := Threaded;
  FEncoder  := TH264_YUV420P_Encoder.Create(aBitrate, aFPS, aWidth, aHeight, aPreset);
  if FThreaded then
  begin
    FEncoder.AddReference(@FEncoder);
    FEncoder.OnEncodedPacketReady := @Self.writeFrame;
    FEncoder.Start;
  end;
end;

destructor TH264_FileWriter.Destroy;
begin
  try
    if FEncoder <> nil then
    begin
      if FThreaded then
      begin
        FEncoder.FreeOnTerminate := False;
        FEncoder.Terminate;
        FEncoder.WaitFor;
        // while FEncoder <> nil do Sleep(5);
      end;

      FreeAndNil(FEncoder);
    end;
    Self.closeFile();
  except
    on E: Exception do
      LBLogger.Write(1, 'TH264_FileWriter.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

function TH264_FileWriter.createFile(aDestinationFileName: String): Boolean;
var
  _ErrMsg : String = '';
begin
  Result := Self.OpenFile(aDestinationFileName, _ErrMsg);
end;

procedure TH264_FileWriter.closeFile;
begin
  Self.addFrame(nil, 0, TAVPixelFormat.AV_PIX_FMT_NB, GetTickCount64, false);
  inherited closeFile;
end;

function TH264_FileWriter.addFrame(anImage: pByte; anImageSize: Integer; aPixelFormat: TAVPixelFormat; aPTS: QWord; isKeyFrame: Boolean): Boolean;
var
  _EncodedPacket : TAVPacketWrapper = nil;
  _EncPacketI    : IAVPacketWrapper;
  _dataToEncode  : pEncodingData;
begin
  if FEncoder <> nil then
  begin
    New(_dataToEncode);
    _dataToEncode^.dataBuffer  := anImage;
    _dataToEncode^.BufferLen   := anImageSize;
    _dataToEncode^.pixelFormat := aPixelFormat;
    _dataToEncode^.pts         := aPts;

    if FThreaded then
      Result := FEncoder.addDataToEncode(_dataToEncode)
    else begin
      Result := FEncoder.EncodeFrame(_dataToEncode, _EncodedPacket);
      if Result then
      begin
        if isKeyFrame then
          _EncodedPacket.Packet^.flags := _EncodedPacket.Packet^.flags and $1;
        _EncPacketI := _EncodedPacket as IAVPacketWrapper;
        Self.WriteEncodedPacket(_EncPacketI);
        _EncodedPacket := nil;
        _EncPacketI := nil;
      end
      else if _EncodedPacket <> nil then
        FreeAndNil(_EncodedPacket);

      Dispose(_dataToEncode);
    end;
  end;
end;

{ TAVFileRewriter }

function TAVFileRewriter.addStreamToContainer(aStream: PAVStream): Boolean;
var
  _Stream  : PAVStream;
  _Decoder : PAVCodec;
begin
  Result := False;
  if (aStream <> nil) and (FContainer <> nil) then
  begin

    _Decoder := gv_FFmpeg.AVCodec.avcodec_find_decoder(aStream^.codecpar^.codec_id);
    if _Decoder <> nil then
    begin

      _Stream := gv_FFmpeg.AVFormat.avformat_new_stream(FContainer, _Decoder);
      if _Stream <> nil then
      begin

        _Stream^.time_base.num := aStream^.time_base.num;
        _Stream^.time_base.den := aStream^.time_base.den;

        Result := gv_FFmpeg.AVCodec.avcodec_parameters_copy(_Stream^.codecpar, aStream^.codecpar) >= 0;

      end;

    end;

  end;
end;

function TAVFileRewriter.addStreams(): Boolean;
var
  i : Integer;
begin
  Result := False;
  if FReader = nil then Exit;
  for i := 0 to FReader.InputInfo.AllStreams.Count - 1 do
  begin
    Result := Self.addStreamToContainer(FReader.InputInfo.AllStreams.Items[i]);
    if not Result then
      LBLogger.Write(1, 'TAVFileRewriter.addStreams', lmt_Warning, 'Error adding stream %d', [i]);
  end;
end;

destructor TAVFileRewriter.Destroy;
begin
  if FReader <> nil then FReader.Free;
  inherited Destroy;
end;

function TAVFileRewriter.ReWrite(): Boolean;
var
  _AVPacket   : IAVPacketWrapper = nil;
  _StreamType : TAVStreamType;
  _Status     : TAVReadResult;
  _Err        : String = '';
begin
  Result := False;
  if (Length(FSourceFile) > 0) and (Length(FDestinationFile) > 0) then
  begin
    if FileExists(FSourceFile) then
    begin
      if FileExists(FDestinationFile) then DeleteFile(FDestinationFile);
      FReader := TAVFileReader.Create();
      if FReader.OpenFile(FSourceFile, _Err) then
      begin
        if Self.OpenFile(FDestinationFile, _Err) then
        begin
          repeat
            FReader.ReadPacket(_AVPacket, _StreamType, _Status);
            if _Status <> rr_EOF then
            begin
              if _AVPacket <> nil then
                if not Self.WriteEncodedPacket(_AVPacket) then
                begin
                  LBLogger.Write(1, 'TAVFileRewriter.ReWrite', lmt_Warning, 'Error writing packet!');
                  Break;
                end;
            end
            else
              Break;
          until False;
        end;
      end
      else
        LBLogger.Write(1, 'TAVFileRewriter.ReWrite', lmt_Warning, 'Error opening file <%s>: <%s>', [FSourceFile, _Err]);
    end
    else
      LBLogger.Write(1, 'TAVFileRewriter.ReWrite', lmt_Warning, 'Source file <%s> not found!', [FSourceFile]);
  end;
end;

{ TAVFileAssembler }

function TAVFileAssembler.addStreamToContainer(aStream: PAVStream): Boolean;
var
  _Stream  : PAVStream;
  _Decoder : PAVCodec;
begin
  Result := False;
  if (aStream = nil) or (FContainer = nil) then Exit;

  _Decoder := gv_FFmpeg.AVCodec.avcodec_find_decoder(aStream^.codecpar^.codec_id);
  if _Decoder = nil then Exit;

  _Stream := gv_FFmpeg.AVFormat.avformat_new_stream(FContainer, _Decoder);
  if _Stream = nil then Exit;

  _Stream^.time_base.num := aStream^.time_base.num;
  _Stream^.time_base.den := aStream^.time_base.den;

  if gv_FFmpeg.AVCodec.avcodec_parameters_copy(_Stream^.codecpar, aStream^.codecpar) >= 0 then
    Result := True;
end;

function TAVFileAssembler.addStreams: Boolean;
var
  i : Integer;
begin
  Result := False;
  if FReader = nil then Exit;
  for i := 0 to FReader.InputInfo.AllStreams.Count - 1 do
  begin
    Result := Self.addStreamToContainer(FReader.InputInfo.AllStreams.Items[i]);
    if not Result then
      LBLogger.Write(1, 'TAVFileAssembler.addStreams', lmt_Warning, 'Error adding stream %d', [i]);
  end;
end;

constructor TAVFileAssembler.Create;
begin
  inherited Create;
  FFilesToAssemble := TStringList.Create;
  FFilesToAssemble.Sorted := True;
end;

destructor TAVFileAssembler.Destroy;
begin
  FreeAndNil(FFilesToAssemble);
  if FReader <> nil then FreeAndNil(FReader);
  inherited Destroy;
end;

function TAVFileAssembler.WriteFile: Boolean;
var
  _AVPacket   : IAVPacketWrapper = nil;
  _StreamType : TAVStreamType;
  _Status     : TAVReadResult;
  _Err        : String = '';
  i           : Integer;
begin
  Result := False;
  if (Length(FDestinationFile) > 0) and (FFilesToAssemble.Count > 1) then
  begin
    if FileExists(FDestinationFile) then DeleteFile(FDestinationFile);
    if FileExists(FFilesToAssemble.Strings[0]) then
    begin
      FReader := TAVFileReader.Create();
      if FReader.OpenFile(FFilesToAssemble.Strings[0], _Err) then
      begin
        if Self.OpenFile(FDestinationFile, _Err) then
        begin
          for i := 0 to FFilesToAssemble.Count - 1 do
          begin
            repeat
              FReader.ReadPacket(_AVPacket, _StreamType, _Status);
              if _Status <> rr_EOF then
              begin
                if _AVPacket <> nil then
                  if not Self.WriteEncodedPacket(_AVPacket) then
                  begin
                    LBLogger.Write(1, 'TAVFileAssembler.WriteFile', lmt_Warning, 'Error writing packet!');
                    Break;
                  end;
              end
              else
                Break;
            until False;

            if i < FFilesToAssemble.Count - 1 then
            begin
              FreeAndNil(FReader);
              FReader := TAVFileReader.Create();
              if not FReader.OpenFile(FFilesToAssemble.Strings[i + 1], _Err) then
              begin
                LBLogger.Write(1, 'TAVFileAssembler.WriteFile', lmt_Warning, 'Error opening file <%s>: <%s>', [FFilesToAssemble.Strings[i + 1], _Err]);
                Break;
              end;
            end;
          end;
          Result := True;
        end;
      end
      else
        LBLogger.Write(1, 'TAVFileAssembler.WriteFile', lmt_Warning, 'Error opening file <%s>: <%s>', [FFilesToAssemble.Strings[0], _Err]);
    end
    else
      LBLogger.Write(1, 'TAVFileAssembler.WriteFile', lmt_Warning, 'Source file <%s> not found!', [FFilesToAssemble.Strings[0]]);
  end
  else
    LBLogger.Write(1, 'TAVFileAssembler.WriteFile', lmt_Warning, 'Missing sources or destination file!');
end;

{ TAudioEncoder }

procedure TAudioEncoder.ClearEncoderContext();
begin
  if FDestroyEncoderContext then
    gv_FFmpeg.AVCodec.avcodec_free_context(FEncoderContext)
  else
    FEncoderContext := nil;
end;

function TAudioEncoder.checkEncoderCompatibility(anEncoder: PAVCodec): Boolean;
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

function TAudioEncoder.get_FrameSize: Int64;
begin
  Result := 0;
  if FEncoderContext <> nil then Result := FEncoderContext^.frame_size;
end;

constructor TAudioEncoder.Create(aBuffers: PBufferArray; aAudioParams: TBufferAudioParams; aUseReadingOffset: Boolean);
begin
  inherited Create;
  FAudioParams           := aAudioParams;
  FBuffers               := aBuffers;
  FUseReadingOffset      := aUseReadingOffset;
  FEncoderContext        := nil;
  FDestroyEncoderContext := True;
  FEncodingFrame         := nil;
  FEncodingOffset        := 0;
  FMaxEncodingOffset     := 0;
  FPTSValue              := 0;
end;

destructor TAudioEncoder.Destroy;
begin
  Self.ClearEncoderContext();
  inherited Destroy;
end;

function TAudioEncoder.setEncoderParams(anAudioCodec: TAVCodecID; aBitrate: Int64): Boolean;
var
  _Encoder : PAVCodec;
  _res     : Integer;
const
  OPUS_APPLICATION_VOIP = Integer(2048);
begin
  Result := False;
  Self.ClearEncoderContext();
  FDestroyEncoderContext := True;

  _Encoder := gv_FFmpeg.AVCodec.avcodec_find_encoder(anAudioCodec);
  if _Encoder <> nil then
  begin
    if Self.checkEncoderCompatibility(_Encoder) then
    begin
      FEncoderContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Encoder);
      if FEncoderContext <> nil then
      begin
        FEncoderContext^.bit_rate   := aBitrate;
        FEncoderContext^.sample_fmt := FAudioParams.SampleFormat;
        FEncoderContext^.sample_rate := FAudioParams.SampleRate;
        gv_FFmpeg.AVUtil.av_channel_layout_default(@FEncoderContext^.ch_layout, FAudioParams.Channels);

        if anAudioCodec = AV_CODEC_ID_OPUS then
        begin
          _res := gv_FFmpeg.AVUtil.av_opt_set_int(FEncoderContext^.priv_data, 'application', OPUS_APPLICATION_VOIP, AV_OPT_SEARCH_CHILDREN);
          if _res <> 0 then
            LBLogger.Write(1, 'TAudioEncoder.setEncoderParams', lmt_Warning, 'Error setting application type: <%s>', [gv_FFmpeg.AVUtil.av_strerror(_res)]);
        end;

        Result := gv_FFmpeg.AVCodec.avcodec_open2(FEncoderContext, _Encoder, nil);
      end;
    end;
  end;
  if not Result then gv_FFmpeg.AVCodec.avcodec_free_context(FEncoderContext);
end;

function TAudioEncoder.getEncodedPackets(aPacketsList: TAVPacketWrapperList; out isEOF: Boolean; FlushBuffer: Boolean): Boolean;
var
  _IPacket       : IAVPacketWrapper;
  _internalFrame : PAVFrame = nil;
  _res           : Integer;
  i              : Integer;
begin
  Result := False;
  isEOF  := False;

  if FEncoderContext <> nil then
  begin

    isEOF := ((FMaxEncodingOffset > 0) and (FEncodingOffset >= FMaxEncodingOffset)) or
              (FEncodingOffset >= Length(FBuffers^[0]));

    if (not isEOF) or FlushBuffer then
    begin
      if not FlushBuffer then
      begin
        if FEncodingFrame = nil then
          FEncodingFrame := FAudioParams.getEmptyFrame(FEncoderContext^.frame_size, false);

        if FEncodingFrame <> nil then
        begin
          _internalFrame := FEncodingFrame.getRelatedObject().Frame;
          for i := 0 to High(FBuffers^) do
            if FBuffers^[i] <> nil then
              _internalFrame^.data[i] := @FBuffers^[i][FEncodingOffset];
        end;
      end
      else
        FEncodingFrame := nil;

      if FlushBuffer or (_internalFrame <> nil) then
      begin
        _res := gv_FFmpeg.AVCodec.avcodec_send_frame(FEncoderContext, _internalFrame);
        if _res >= 0 then
        begin
          Result := True;
          repeat
            _IPacket := TAVPacketWrapper.Create as IAVPacketWrapper;
            _res := gv_FFmpeg.AVCodec.avcodec_receive_packet(FEncoderContext, _IPacket.getRelatedObject().Packet);
            if (_res = gv_FFmpeg.AVUtil.AVERROR_AGAIN) or (_res = AVERROR_EOF) then
              Break
            else if _res >= 0 then
              aPacketsList.Add(_IPacket.getRelatedObject())
            else begin
              LBLogger.Write(1, 'TAudioEncoder.getEncodedPackets', lmt_Warning, 'Error encoding audio frame!');
              Result := False;
              Break;
            end;
          until False;

          if FUseReadingOffset and (_internalFrame <> nil) then
            Inc(FEncodingOffset, _internalFrame^.nb_samples * FAudioParams.Bytes4Sample);
        end
        else
          LBLogger.Write(1, 'TAudioEncoder.getEncodedPackets', lmt_Warning, 'Error sending frame to encoder!');
      end;
    end
    else
      Result := True;

  end
  else
    LBLogger.Write(1, 'TAudioEncoder.getEncodedPackets', lmt_Warning, 'Encoder not initialized!');

end;

function TAudioEncoder.addStreamToContainer(aContainer: PAVFormatContext; anAudioCodec: TAVCodecID): Boolean;
var
  _Stream  : PAVStream;
  _Encoder : PAVCodec;
begin
  Result := False;
  try
    if aContainer <> nil then
    begin
      Self.ClearEncoderContext();
      _Encoder := gv_FFmpeg.AVCodec.avcodec_find_encoder(anAudioCodec);
      if _Encoder <> nil then
      begin
        if Self.checkEncoderCompatibility(_Encoder) then
        begin
          _Stream := gv_FFmpeg.AVFormat.avformat_new_stream(aContainer, nil);
          if _Stream <> nil then
          begin
            _Stream^.time_base.num := 1;
            _Stream^.time_base.den := FAudioParams.SampleRate;
            FEncoderContext := gv_FFmpeg.AVCodec.avcodec_alloc_context3(_Encoder);
            FDestroyEncoderContext := True;
            if FEncoderContext <> nil then
            begin
              FEncoderContext^.bit_rate  := aContainer^.bit_rate;
              gv_FFmpeg.AVUtil.av_channel_layout_from_mask(@FEncoderContext^.ch_layout, FAudioParams.ChannelLayout);
              FEncoderContext^.sample_rate   := FAudioParams.SampleRate;
              FEncoderContext^.sample_fmt    := FAudioParams.SampleFormat;
              FEncoderContext^.time_base.num := 1;
              FEncoderContext^.time_base.den := FAudioParams.SampleRate;
              if (aContainer^.oformat^.flags and AVFMT_GLOBALHEADER) > 0 then
                FEncoderContext^.flags := FEncoderContext^.flags or AV_CODEC_FLAG_GLOBAL_HEADER;
              if gv_FFmpeg.AVCodec.avcodec_open2(FEncoderContext, _Encoder, nil) then
              begin
                if gv_FFmpeg.AVCodec.avcodec_parameters_from_context(_Stream^.codecpar, FEncoderContext) >= 0 then
                begin
                  _Stream^.time_base := FEncoderContext^.time_base;
                  Result := True;
                end;
              end;
            end;
          end;
        end;
      end
      else
        LBLogger.Write(1, 'TAudioEncoder.addStreamToContainer', lmt_Warning,
          'Encoder %d not found!', [Integer(anAudioCodec)]);
    end;
  except
    on E: Exception do
      LBLogger.Write(1, 'TAudioEncoder.addStreamToContainer', lmt_Error, E.Message);
  end;
end;

{ TAudioFileWriter }

procedure TAudioFileWriter.set_AudioBuffer(AValue: TAudioBufferData);
begin
  if FAudioBuffer <> AValue then
  begin
    FreeAndNil(FAudioEncoder);
    FAudioBuffer := AValue;
    if FAudioBuffer <> nil then
      FAudioEncoder := TAudioEncoder.Create(@FAudioBuffer.Buffers, FAudioBuffer.AudioParams, True);
  end;
end;

procedure TAudioFileWriter.set_MaxEncodingOffsetByDuration(AValue: Int64);
begin
  FMaxEncodingOffset := Round((AValue * FAudioBuffer.AudioParams.SampleRate * FAudioBuffer.AudioParams.Bytes4Sample) / 1000);
end;

function TAudioFileWriter.addStreams(): Boolean;
begin
  Result := False;
  if FAudioEncoder <> nil then
  begin
    if FAudioCodec <> AV_CODEC_ID_NONE then
    begin
      FContainer^.bit_rate := FBitrate;
      Result := FAudioEncoder.addStreamToContainer(FContainer, FAudioCodec);
    end;
  end;
end;

constructor TAudioFileWriter.Create(anAudioCodec: TAVCodecID; aBitrate: Integer);
begin
  inherited Create;
  FAudioBuffer  := nil;
  FAudioEncoder := nil;
  FAudioCodec   := anAudioCodec;
  FBitRate      := aBitrate;
  FMaxEncodingOffset := 0;
end;

destructor TAudioFileWriter.Destroy;
begin
  FreeAndNil(FAudioEncoder);
  inherited Destroy;
end;

function TAudioFileWriter.createFile(aDestinationFileName: String): Boolean;
var
  _List         : TAVPacketWrapperList = nil;
  _strError     : String;
  _Packet       : IAVPacketWrapper;
  _EOF          : Boolean;
  i             : Integer;
  _AudioEncoder : TAudioEncoder = nil;
  _StartOp      : QWord;
begin
  Result := False;
  try
    _StartOp := GetTickCount64();
    if Self.OpenFile(Utf8ToAnsi(aDestinationFileName), _strError) then
    begin
      _List := TAVPacketWrapperList.Create;
      _AudioEncoder := TAudioEncoder.Create(@FAudioBuffer.Buffers, FAudioBuffer.AudioParams, True);
      if _AudioEncoder.setEncoderParams(FAudioCodec) then
      begin
        _AudioEncoder.MaxEncodingOffset := FMaxEncodingOffset;
        repeat
          _List.Clear;
          if _AudioEncoder.getEncodedPackets(_List, _EOF, False) then
          begin
            if _EOF then
            begin
              LBLogger.Write(5, 'TAudioFileWriter.createFile', lmt_Debug,
                'File <%s> terminated in %d ms', [aDestinationFileName, Int64(GetTickCount64() - _StartOp)]);
              Result := True;
              Break;
            end
            else
            begin
              for i := 0 to _List.Count - 1 do
              begin
                _Packet := _List.Items[i] as IAVPacketWrapper;
                if not Self.WriteEncodedPacket(_Packet) then
                  LBLogger.Write(1, 'TAudioFileWriter.createFile', lmt_Warning, 'Error writing encoded packet!');
              end;
            end;
          end
          else begin
            LBLogger.Write(1, 'TAudioFileWriter.createFile', lmt_Warning, 'Error encoding packet!');
            Break;
          end;
        until False;
        Self.closeFile();
      end
      else
        LBLogger.Write(1, 'TAudioFileWriter.createFile', lmt_Warning, 'Error setting encode parameters!');
    end
    else
      LBLogger.Write(1, 'TAudioFileWriter.createFile', lmt_Warning,
        'Error creating file <%s>: <%s>', [aDestinationFileName, _strError]);
    if _List <> nil then _List.Free;
  except
    on E: Exception do
      LBLogger.Write(1, 'TAudioFileWriter.createFile', lmt_Error, E.Message);
  end;
  FreeAndNil(_AudioEncoder);
end;

{ TAudioStreamFileWriter }

constructor TAudioStreamFileWriter.Create(pcmBuffers: PBufferArray; aAudioParams: TBufferAudioParams; aAudioCodec: TAVCodecID; aBitrate: Integer);
begin
  inherited Create;
  FAudioCodec  := aAudioCodec;
  FBitrate     := aBitrate;
  FAudioParams := aAudioParams;
  FEncoder     := TAudioEncoder.Create(pcmBuffers, aAudioParams, False);
  FPackets     := TAVPacketWrapperList.Create;
  FPts         := 0;
end;

destructor TAudioStreamFileWriter.Destroy;
begin
  try
    FreeAndNil(FEncoder);
    Self.ClearPackets();
    FreeAndNil(FPackets);
  except
    on E: Exception do
      LBLogger.Write(1, 'TAudioStreamFileWriter.Destroy', lmt_Error, E.Message);
  end;
  inherited Destroy;
end;

function TAudioStreamFileWriter.openDestinationFile(const aDestinationFileName: String): Boolean;
var
  _errMsg : String;
begin
  if FEncoder = nil then Exit(False);
  Result := Self.OpenFile(aDestinationFileName, _errMsg);
end;

function TAudioStreamFileWriter.write(aFlushBuffers: Boolean): Boolean;
var
  i       : Integer;
  _Packet : PAVPacket;
  _isEOF  : Boolean;
begin
  Result := False;
  Self.ClearPackets();

  if not FEncoder.getEncodedPackets(FPackets, _isEOF, aFlushBuffers) then
  begin
    LBLogger.Write(1, 'TAudioStreamFileWriter.write', lmt_Warning, 'No packets retrieved!');
    Exit;
  end;

  for i := 0 to FPackets.Count - 1 do
  begin
    _Packet := FPackets.Items[i].Packet;
    _Packet^.pts := FPts;
    _Packet^.dts := FPts;
    Inc(FPts, FEncoder.FrameSize);
    Result := Self.WriteEncodedPacket(FPackets.Items[i]);
    if not Result then
    begin
      LBLogger.Write(1, 'TAudioStreamFileWriter.write', lmt_Warning, 'Error writing encoded packet!');
      Break;
    end;
  end;
end;

procedure TAudioStreamFileWriter.Close();
begin
  Self.closeFile();
end;

procedure TAudioStreamFileWriter.ClearPackets();
var
  _Packet : PAVPacket;
  i       : Integer;
begin
  if FPackets.Count = 0 then Exit;
  for i := 0 to FPackets.Count - 1 do
  begin
    _Packet := PAVPacket(FPackets.Items[i]);
    if _Packet <> nil then
      gv_FFmpeg.AVCodec.av_packet_free(@_Packet);
  end;
  FPackets.Clear;
end;

function TAudioStreamFileWriter.addStreams(): Boolean;
begin
  Result := False;
  if FAudioCodec <> AV_CODEC_ID_NONE then
  begin
    if FContainer <> nil then
    begin
      FContainer^.bit_rate := FBitrate;
      Result := FEncoder.addStreamToContainer(FContainer, FAudioCodec);
    end;
  end;
end;

end.
