unit uLBFileUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM;


type
  { TFileInfoRetriever }

  TFileInfoRetriever = class(TObject)
    strict private
      FCompanyName : String;
      FProductName : String;
      FProductVersion : String;
      FLegalCopyright : String;
      FComments : String;

      procedure Clear();

    public
      function LoadInfo(anExeFilename: String): Boolean;

      property CompanyName: String read FCompanyName;
      property ProductName: String read FProductName;
      property ProductVersion: String read FProductVersion;
      property LegalCopyright: String read FLegalCopyright;
      property Comments: String read FComments;
  end;

  TResourceFileInfo = record
    Filename : String;
    Code     : String;
  end;
  pResourceFileInfo = ^TResourceFileInfo;

  function extractFilesFormResource(const aDestinationFolder: String; aFilesList: pResourceFileInfo; aFileCount: Integer): Boolean;

  function getTemporaryFolder(GlobalFolder: Boolean = false): String;

  function ResolvePath(const aPath: String): String;

  function FindFilesInFolder(const Path: String; const Mask: String; Recursive: Boolean; Files: TStringList): Boolean; overload;
  function FindFilesInFolder(Path: String; const Mask: String; aRecursiveLevel: Integer; Files: TStringList): Boolean; overload;

  function getFolderSize(aFolder: String; Recursive: Boolean): Int64;

  function FindSubfolders(Path: String; SubFolders: TStringList): Boolean;

  function OpenXMLFile(const aFilename: String; out aDocument: TXMLDocument): Boolean;

  const
    cSearchAllFileMask = String({$IFDEF Linux}'*'{$ELSE}'*.*'{$ENDIF});

implementation

uses
  fileinfo, ULBLogger, Laz2_XMLRead{$IFDEF Windows}, Windows{$ENDIF};

{ TFileInfoRetriever }

procedure TFileInfoRetriever.Clear();
begin
  FCompanyName    := '';
  FLegalCopyright := '';
  FProductName    := '';
  FProductVersion := '';
  FComments       := '';
end;

function TFileInfoRetriever.LoadInfo(anExeFilename: String): Boolean;
var
  _FileVerInfo : TFileVersionInfo = nil;

begin
  Result := False;
  Self.Clear();

  if FileExists(anExeFilename) then
  begin

    try
      _FileVerInfo := TFileVersionInfo.Create(nil);
      _FileVerInfo.FileName := anExeFilename;
      _FileVerInfo.ReadFileInfo;

      FCompanyName    := _FileVerInfo.VersionStrings.Values['CompanyName'];
      FLegalCopyright := _FileVerInfo.VersionStrings.Values['LegalCopyright'];
      FProductName    := _FileVerInfo.VersionStrings.Values['ProductName'];
      FProductVersion := _FileVerInfo.VersionStrings.Values['ProductVersion'];
      FComments       := _FileVerInfo.VersionStrings.Values['Comments'];

      Result := True;
    except
      on E: Exception do
        LBLogger.Write(1, 'TFileInfoRetriever.LoadInfo', lmt_Error, 'Error reading info from <%s>: <%s>', [anExeFilename, E.Message]);
    end;

    if _FileVerInfo <> nil then
      _FileVerInfo.Free;

  end
  else
    LBLogger.Write(1, 'TFileInfoRetriever.LoadInfo', lmt_Warning, 'File <%s> not found!', [anExeFilename]);
end;

function extractFilesFormResource(const aDestinationFolder: String; aFilesList: pResourceFileInfo; aFileCount: Integer): Boolean;
var
  _Stream : TResourceStream;
  _DestFolder : String;
  _Path : String;
  i : Integer;

begin
  Result := False;

  if aDestinationFolder <> '' then
  begin
    if not DirectoryExists(aDestinationFolder) then
      ForceDirectories(aDestinationFolder);

    if (aFilesList <> nil) and (aFileCount > 0) then
    begin
      try

        _DestFolder := IncludeTrailingPathDelimiter(aDestinationFolder);

        for i := 0 to aFileCount - 1 do
        begin
          if not FileExists(_DestFolder + aFilesList[i].Filename) then
          begin
            _Stream := TResourceStream.Create(HINSTANCE, aFilesList[i].Code, RT_RCDATA);
            if (_Stream <> nil) and (_Stream.Size > 0) then
            begin
              _Path := ExtractFilePath(_DestFolder + aFilesList[i].Filename);
              if not DirectoryExists(_Path) then
                ForceDirectories(_Path);

              _Stream.SaveToFile(_DestFolder + aFilesList[i].Filename);
            end;
            _Stream.Free;
          end;
        end;

        Result := True;

      except
        on E: Exception do
          LBLogger.Write(1, 'extractFilesFormResource', lmt_Error, E.Message);
      end;

    end
    else
      LBLogger.Write(1, 'extractFilesFormResource', lmt_Warning, 'Nothing to extract!');

  end
  else
    LBLogger.Write(1, 'extractFilesFormResource', lmt_Warning, 'No destination folder!');
end;

function getTemporaryFolder(GlobalFolder: Boolean = false): String;
begin
  Result := '';

  if GlobalFolder then
    Result := GetTempDir(True)
  else begin
    Result := GetTempDir(False);

    if Length(Result) = 0 then
      Result := GetTempDir(True);
  end;

  {$IFDEF Linux}
  if Length(Result) = 0 then
    Result := '/tmp/';
  {$ENDIF}
end;


function FindSubfolders(Path: String; SubFolders: TStringList): Boolean;
var
  _Search : TSearchRec;

begin
  Result := false;

  if SubFolders <> nil then
  begin

    SubFolders.Clear;

    if DirectoryExists(Path) then
    begin
      Path := IncludeTrailingPathDelimiter(Path);
      try
        if FindFirst(Path + '*', faDirectory, _Search) = 0 then
        begin
          repeat

            if (_Search.Name <> '.') and
               (_Search.Name <> '..') and
               (_Search.Attr and faDirectory > 0) then
              SubFolders.Add(_Search.Name);

	  until FindNext(_Search) <> 0;

          Result := True;
        end;

      finally
        SysUtils.FindClose(_Search);
      end;
    end;

  end;
end;

function OpenXMLFile(const aFilename: String; out aDocument: TXMLDocument): Boolean;
begin
  Result := False;
  aDocument := nil;

  try

    if FileExists(aFilename) then
    begin

      ReadXMLFile(aDocument, aFilename);
      Result := aDocument <> nil;

    end
    else
      LBLogger.Write(1, 'OpenXMLFile', lmt_Warning, 'File <%s> not found!', [aFilename]);

  except
    on E: Exception do
    begin
      LBLogger.Write(1, 'OpenXMLFile', lmt_Error, E.Message);
      if aDocument <> nil then
        FreeAndNil(aDocument);
    end;
  end;
end;


function ResolvePath(const aPath: String): String;
begin
  Result := '';

  if aPath <> '' then
  begin
    {$IFDEF Windows}
    // C:\..., D:\...
    if (Length(aPath) >= 3) and (aPath[2] = ':') then
      Exit(ExpandFileName(aPath));

    // \\server\share\... (UNC)
    if (Copy(aPath, 1, 2) = '\\') then
      Exit(ExpandFileName(aPath));
    {$ELSE}
    // Unix/Linux/macOS: /...
    if aPath[1] = '/' then
      Exit(ExpandFileName(aPath));
    {$ENDIF}

    // Relativo: ../..., ./..., logs/...
    if (Pos('..', aPath) > 0) or (Pos('./', aPath) = 1) or (Pos('/', aPath) > 0) then
      Exit(ExpandFileName(ExtractFilePath(ParamStr(0)) + aPath));

    // Solo nome (es. pippo.log) → cartella temporanea
    Result := ExpandFileName(getTemporaryFolder(False) + aPath);
  end;
end;

function FindFilesInFolder(const Path: String; const Mask: String; Recursive: Boolean; Files: TStringList): Boolean;
var
  _RecursiveLevel: Integer = 0;
begin

  if Recursive then
    _RecursiveLevel := High(Integer) - 1;

  Result := FindFilesInFolder(Path, Mask, _RecursiveLevel, Files);
end;

function FindFilesInFolder(Path: String; const Mask: String; aRecursiveLevel: Integer; Files: TStringList): Boolean;
var
  _Search : SysUtils.TSearchRec;

begin
  Result := False;

  if Length(Path) > 0 then
  begin
    Path := IncludeTrailingPathDelimiter(Path);

    if (Files <> nil) and DirectoryExists(Path) then
    begin
      try
        if SysUtils.FindFirst(Path + Mask, faAnyFile, _Search) = 0 then
        begin
          Result := True;

          repeat

            if (_Search.Attr and faDirectory > 0) then
            begin
              if (aRecursiveLevel > 0) and (_Search.Name <> '.') and (_Search.Name <> '..') and (Length(_Search.Name) > 0) then
                FindFilesInFolder(Path + _Search.Name, Mask, aRecursiveLevel - 1, Files);
            end
            else
              Files.Add(Path + _Search.Name);

          until SysUtils.FindNext(_Search) <> 0;

        end;

      finally
        SysUtils.FindClose(_Search);
      end;
    end;

  end;
end;

function getFolderSize(aFolder: String; Recursive: Boolean): Int64;
var
  _rec: TSearchRec;
  _found: Integer;
  _Size : Int64;


const
  cClusterSize = Int64(2048);

begin
   Result := 0;

   aFolder := IncludeTrailingPathDelimiter(aFolder);

   _found := FindFirst(aFolder + cSearchAllFileMask, faAnyFile, _rec);

   while _found = 0 do
   begin
     _Size := _rec.Size;
     if _Size mod cClusterSize <> 0 then
       Inc(Result, ((_Size div cClusterSize) + 1) * cClusterSize)
     else
       Inc(Result, _Size);

     if (_rec.Attr and faDirectory > 0) and (_rec.Name[1] <> '.') and (Recursive) then
       Result += getFolderSize(aFolder + _rec.Name, True);

     _found := FindNext(_rec);
   end;

   sysUtils.FindClose(_rec);
end;


end.

