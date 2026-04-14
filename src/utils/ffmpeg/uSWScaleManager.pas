unit uSWScaleManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAVLibraryBaseManager, libswscale, libavutil;

type

  { TSWScaleManager }

  TSWScaleManager = class(TAVLibraryBaseManager)
    public
      constructor Create(aPath: String); override;

      var
        sws_getCachedContext : Tsws_getCachedContext;
        sws_scale : Tsws_scale;
        sws_freeContext : Tsws_freeContext;
  end;

implementation

{ TSWScaleManager }

constructor TSWScaleManager.Create(aPath: String);
var
  _Error : String;

begin
  inherited Create(aPath);

  FLibraryLoader.addFunction('sws_getCachedContext', @sws_getCachedContext);
  FLibraryLoader.addFunction('sws_scale', @sws_scale);
  FLibraryLoader.addFunction('sws_freeContext', @sws_freeContext);


  FInitialized := FLibraryLoader.LoadLibrary(swscaleLibName, _Error);
end;

end.

