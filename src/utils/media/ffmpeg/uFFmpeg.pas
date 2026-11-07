unit uFFmpeg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAVFormatManager, uAVUtilManager, uAVCodecManager, uSWResampleManager, uAVFilterManager, uSWScaleManager;

type

  { TFFmpeg }

  TFFmpeg = class(TObject)
    strict private
      FAVFormat   : TAVFormatManager;
      FAVUtil     : TAVUtilManager;
      FAVCodec    : TAVCodecManager;
      FSWResample : TSWResampleManager;
      FSWScale    : TSWScaleManager;
      FAVFilter   : TAVFilterManager;

      procedure DestroyLibraries();
      function LoadLibrariesFromPath(aPath: String) : Boolean;

    public
      constructor Create();
      destructor Destroy; override;

      function LoadLibraries(const Paths: TStringArray): Boolean;
      function checkResult(aRes: Integer; const AContext, AMessage: string): Boolean;

      property AVFormat   : TAVFormatManager   read FAVFormat;
      property AVUtil     : TAVUtilManager     read FAVUtil;
      property AVCodec    : TAVCodecManager    read FAVCodec;
      property SWResample : TSWResampleManager read FSWResample;
      property SWScale    : TSWScaleManager    read FSWScale;
      property AVFilter   : TAVFilterManager   read FAVFilter;

      const
        cFFScale = Integer(1000);
  end;

  function createFFmpegWrapper(const Paths: TStringArray): Boolean;

var
  gv_FFmpeg : TFFmpeg = nil;

implementation

uses
  ULBLogger;

function createFFmpegWrapper(const Paths: TStringArray): Boolean;
begin
  if gv_FFmpeg = nil then
  begin
    gv_FFmpeg := TFFmpeg.Create();
    if not gv_FFmpeg.LoadLibraries(Paths) then
    begin
      LBLogger.Write(1, 'createFFmpegWrapper', lmt_Warning, 'Error loading libraries!');
      FreeAndNil(gv_FFmpeg);
    end
    else
      LBLogger.Write(1, 'createFFmpegWrapper', lmt_Debug, 'FFMPEG libraries loaded', []);
  end;

  Result := gv_FFmpeg <> nil;
end;

{ TFFmpeg }

procedure TFFmpeg.DestroyLibraries();
begin
  try

    FreeAndNil(FAVFormat);
    FreeAndNil(FAVUtil);
    FreeAndNil(FAVCodec);
    FreeAndNil(FSWResample);
    FreeAndNil(FSWScale);
    FreeAndNil(FAVFilter);

  except
    on E: Exception do
      LBLogger.Write(1, 'TFFmpeg.DestroyLibraries', lmt_Error, PChar(E.Message));
  end;
end;

function TFFmpeg.LoadLibrariesFromPath(aPath: String): Boolean;
var
  _GoOn : Boolean;

begin
  Result := False;

  if Length(aPath) > 0 then
  begin

  end
  else
    _GoOn := True;

  if _GoOn then
  begin
    FAVFormat := TAVFormatManager.Create(aPath);
    if FAVFormat.Initialized then
    begin
      FAVCodec := TAVCodecManager.Create(aPath);
      if FAVCodec.Initialized then
      begin
        FAVUtil := TAVUtilManager.Create(aPath);
        if FAVUtil.Initialized then
        begin
          FSWResample := TSWResampleManager.Create(aPath);
          if FSWResample.Initialized then
          begin
            FSWScale := TSWScaleManager.Create(aPath);
            if FSWScale.Initialized then
            begin
              Result := True;

              FAVFilter := TAVFilterManager.Create(aPath);
              if not FAVFilter.Initialized then
                LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'AVFilter not initialized!');
            end
            else
              LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'SWScale not initialized!');
          end
          else
            LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'SWResample not initialized!');
        end
        else
          LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'AVUtil not initialized!');
      end
      else
        LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'AVCodec not initialized!');
    end
    else
      LBLogger.Write(1, 'TFFmpeg.LoadLibrariesFromPath', lmt_Warning, 'AVFormat not initialized!');
  end;
end;

constructor TFFmpeg.Create();
begin
  inherited Create;

  FAVFormat   := nil;
  FAVUtil     := nil;
  FAVCodec    := nil;
  FSWResample := nil;
  FAVFilter   := nil;
  FSWScale    := nil;
end;

destructor TFFmpeg.Destroy;
begin
  try

    Self.DestroyLibraries();

  except
    on E: Exception do
      LBLogger.Write(1, 'TFFmpeg.Destroy', lmt_Error, PChar(E.Message));
  end;
  inherited Destroy;
end;

function TFFmpeg.LoadLibraries(const Paths: TStringArray): Boolean;
var
  i : Integer;

begin
  Result := False;

  if Length(Paths) = 0 then
    Result := Self.LoadLibrariesFromPath('')
  else begin

    for i := 0 to High(Paths) do
    begin
      if Self.LoadLibrariesFromPath(Paths[i]) then
      begin
        Result := True;
        Break;
      end
      else
        Self.DestroyLibraries();
    end;

  end;

  if not Result then
    Self.DestroyLibraries();
end;

function TFFmpeg.checkResult(aRes: Integer; const AContext, AMessage: string): Boolean;
begin
  if Self = nil then Exit(False);

  Result := aRes >= 0;

  if not Result then
    LBLogger.Write(1, AContext, lmt_Warning, '%s: <%s>', [AMessage, gv_FFmpeg.AVUtil.av_strerror(aRes)]);
end;


finalization
  if gv_FFmpeg <> nil then
    FreeAndNil(gv_FFmpeg);

end.
