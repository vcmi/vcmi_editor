{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit filesystem;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,
  gmap, fgl,
  filesystem_base, lod, FileUtil;

type

  {
  real FS
    ROOT\
      CONFIG\
        filesystem.json
      DATA\
      MODS\
        ONE_MOD\
          mod.json (use DATA and SPRITES paths)
          CONTENT ?
          DATA ?
            OBJECTS ?

          first.lod ?
          second.pack ?

   VFS
     ROOT\
       CONFIG\

       DATA\
         eobjects.txt
         zebjcts.txt
       SPRITES\
         foo.def
         bar.def

         smth.json
         SMTH\
           carde1
           carde2



  }

  //TODO: filsystem scan
  //TODO: mod support
  //TODO: lod support

  {$push}
  {$m+}

  { TFilesystemConfigItem }

  TFilesystemConfigItem = class(TCollectionItem)
  private
    FPath: string;
    FTyp: string;
    procedure SetPath(AValue: string);
    procedure SetTyp(AValue: string);
  published
    property Typ:string read FTyp write SetTyp; //TODO: map this to name "type"
    property Path: string read FPath write SetPath;
  end;

  { TFilesystemConfigItems }

  TFilesystemConfigItems = class(TCollection)
  public

    constructor Create;
    destructor Destroy; override;
  end;

  { TFilesystemConfigPath }

  TFilesystemConfigPath = class(TCollectionItem)
  private
    FItems: TFilesystemConfigItems;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Items: TFilesystemConfigItems read FItems;
  end;


  { TFilesystemConfig }

  TFilesystemConfig = class (TCollection)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

  {$pop}

  TLocationType = (InLod, InFile);

  { TResLocation }

  TResLocation = object
    lt: TLocationType;
    //for files
    path: string;
    //for lods
    lod:TLod;
    FileHeader: TLodItem;

    procedure SetLod(ALod: TLod; AFileHeader: TLodItem); //???
    procedure SetFile(AFullPath: string); //???
  end;

  TResId = record
    VFSPath: String;
    Typ: TResourceType;
  end;

  { TResIDCompare }

  TResIDCompare = class
  public
    class function c(a,b :TResId):boolean;
  end;

  TResIDToLcationMap = specialize TMap<TResId, TResLocation,TResIDCompare>;

  TLodList = specialize TFPGObjectList<TLod>;

  { TFSManager }

  TFSManager = class (TComponent,IResourceLoader)
  private
    FConfig: TFilesystemConfig;

    FResMap: TResIDToLcationMap;
    FLodList: TLodList;

    FGamePath: string;

    FCurrentFilter:TResourceTypes;
    FCurrentVFSPath: string;
    FCurrentRelPath: string;

    procedure SetCurrentVFSPath(ACurrentVFSPath: string);

    function MakeFullPath(RelPath: string):string;

    function MatchFilter(AExt: string; out AType: TResourceType): boolean;

    procedure OnLodItemFound(Alod: TLod; constref AItem: TLodItem);
    procedure ScanLod(LodRelPath: string);

    procedure OnFileFound(FileIterator: TFileIterator);
    procedure ScanDir(RelDir: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ScanFilesystem;

    procedure LoadToStream(AStream: TStream; AResType: TResourceType; AName: string);
  end;


implementation


const
  RES_TO_EXT: array[TResourceType] of string = (
    'TXT','JSON','DEF'
  );

{ TResLocation }

procedure TResLocation.SetFile(AFullPath: string);
begin
  lt := TLocationType.InFile;
  path := AFullPath;
  lod := nil;
end;

procedure TResLocation.SetLod(ALod: TLod; AFileHeader: TLodItem);
begin
  lt := TLocationType.InLod;
  lod := ALod;
  FileHeader := AFileHeader;

  path := '';
end;

{ TResIDCompare }

class function TResIDCompare.c(a, b: TResId): boolean;
var
  cres:Integer;
begin
  cres := CompareStr(a.VFSPath , b.VFSPath);
  Result := (cres< 0) or ((cres=0) and (a.Typ < b.Typ));
end;

{ TFilesystemConfigPath }

constructor TFilesystemConfigPath.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FItems := TFilesystemConfigItems.Create;
end;

destructor TFilesystemConfigPath.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TFilesystemConfigItems }

constructor TFilesystemConfigItems.Create;
begin
  inherited Create(TFilesystemConfigItem);
end;

destructor TFilesystemConfigItems.Destroy;
begin
  inherited Destroy;
end;

{ TFilesystemConfigItem }

procedure TFilesystemConfigItem.SetPath(AValue: string);
begin
  if FPath = AValue then Exit;
  FPath := AValue;
end;

procedure TFilesystemConfigItem.SetTyp(AValue: string);
begin
  if FTyp = AValue then Exit;
  FTyp := AValue;
end;

{ TFilesystemConfig }

constructor TFilesystemConfig.Create;
begin
  inherited Create(TFilesystemConfigPath);
end;

destructor TFilesystemConfig.Destroy;
begin
  inherited Destroy;
end;


{ TFSManager }

constructor TFSManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := TFilesystemConfig.Create;
  FResMap := TResIDToLcationMap.Create;
  FLodList := TLodList.Create(True);
end;

destructor TFSManager.Destroy;
begin
  FLodList.Free;
  FResMap.Free;
  FConfig.Free;
  inherited Destroy;
end;

procedure TFSManager.ScanLod(LodRelPath: string);
var
  lod: TLod;
begin
  //TODO: no duplicates

  LodRelPath := SetDirSeparators(LodRelPath);

  lod := TLod.Create(MakeFullPath(LodRelPath));

  lod.Scan(@OnLodItemFound);

  FLodList.Add(lod);
end;

procedure TFSManager.SetCurrentVFSPath(ACurrentVFSPath: string);
begin
  FCurrentVFSPath := SetDirSeparators(ACurrentVFSPath);
end;

procedure TFSManager.LoadToStream(AStream: TStream;
  AResType: TResourceType; AName: string);
var
  stm: TFileStream;

  res_id: TResId;
  res_loc: TResLocation;
  it : TResIDToLcationMap.TIterator;
begin

  AName := SetDirSeparators(AName);

  AName := UpperCase(AName);

  res_id.VFSPath := AName;
  res_id.Typ := AResType;

  it := FResMap.Find(res_id);

  if not Assigned(it) then
  begin
    raise Exception.Create('Res not found '+AName);
  end;

  res_loc := it.Value;
  it.Free;

  case res_loc.lt of
    TLocationType.InLod: res_loc.lod.LoadToStream(AStream,res_loc.FileHeader) ;
    TLocationType.InFile: begin
      stm := TFileStream.Create(res_loc.path,fmOpenRead or fmShareDenyWrite);
      try
        stm.Seek(0,soBeginning);
        AStream.CopyFrom(stm,stm.Size);
      finally
        stm.Free;
      end;
    end;
  end;

end;

function TFSManager.MakeFullPath(RelPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FGamePath)+RelPath;
end;

function TFSManager.MatchFilter(AExt: string; out AType: TResourceType
  ): boolean;
var
  fltr: TResourceType;
begin
  Result := (FCurrentFilter = []); //empty filter = all files match
  if Result then
    Exit;
  AExt  := Trim(UpperCase(AExt));
  for fltr in FCurrentFilter do
  begin
    if AExt = '.'+RES_TO_EXT[fltr] then
    begin
      Result := True;
      AType:=  fltr;
      Exit;
    end;
  end;
end;

procedure TFSManager.OnFileFound(FileIterator: TFileIterator);
var
  res_id: TResId;
  res_loc: TResLocation;
  res_typ: TResourceType;

  file_name: String;
  file_ext: String;

  rel_path: string;
begin
  if FileIterator.IsDirectory then
    Exit;
  file_ext := ExtractFileExt(FileIterator.FileName);

  if not MatchFilter(file_ext,res_typ) then
    Exit;

  rel_path := CreateRelativePath(FileIterator.Path, MakeFullPath(FCurrentRelPath)); //???

  file_name := ExtractFileNameOnly(FileIterator.FileName);

  res_id.Typ := res_typ;
  res_id.VFSPath := SetDirSeparators(UpperCase(FCurrentVFSPath+ExtractFilePath(rel_path)+file_name));//

  res_loc.SetFile(FileIterator.FileName);

  FResMap.Insert(res_id,res_loc);
end;

procedure TFSManager.OnLodItemFound(Alod: TLod; constref AItem: TLodItem);
var
  file_name: String;
  file_ext: String;


  res_id: TResId;
  res_loc: TResLocation;
  res_typ: TResourceType;
begin
  file_name := AItem.Filename;
  file_name := Trim(file_name);
  file_name := upcase(file_name);

  file_ext := ExtractFileExt(file_name); //including "."

  file_name := ExtractFileNameWithoutExt(file_name);


  if not  MatchFilter(file_ext,res_typ) then
    exit;

  res_id.Typ := res_typ;
  res_id.VFSPath := FCurrentVFSPath+file_name;//

  res_loc.SetLod(Alod, AItem);

  FResMap.Insert(res_id,res_loc);

end;

procedure TFSManager.ScanDir(RelDir: string);
var
  srch: TFileSearcher;
  p: string;
begin
  srch := TFileSearcher.Create;
  srch.OnFileFound := @OnFileFound;
  try
    FCurrentRelPath := RelDir;
    p := IncludeTrailingPathDelimiter(MakeFullPath(RelDir));
    srch.Search(p);
  finally
    srch.Free;
  end;
end;

procedure TFSManager.ScanFilesystem;
const
  GAME_PATH_CONFIG = 'gamepath.txt';

var
  s: String;
  sl: TStringList;

begin
  sl := TStringList.Create;
  try
    s := ExtractFilePath(ParamStr(0))+GAME_PATH_CONFIG;
    if FileUtil.FileExistsUTF8(s) then
    begin
      sl.LoadFromFile(s);
      FGamePath := sl[0];
    end
    else begin
      FGamePath := ExtractFilePath(ParamStr(0));
    end;
  finally
    sl.Free;
  end;

  IncludeTrailingPathDelimiter(FGamePath);

  //todo: use config
  //"DATA/" :
  //[
  //	{"type" : "lod", "path" : "ALL/Data/H3ab_bmp.lod"},
  //	{"type" : "lod", "path" : "ALL/Data/H3bitmap.lod"},
  //	{"type" : "dir",  "path" : "ALL/Data"}
  //],
  //"SPRITES/":
  //[
  //	{"type" : "lod", "path" : "ALL/Data/H3ab_spr.lod"},
  //	{"type" : "lod", "path" : "ALL/Data/H3sprite.lod"},
  //	{"type" : "dir",  "path" : "ALL/Sprites"}

  FCurrentFilter := [TResourceType.Text];
  SetCurrentVFSPath('DATA/');
  ScanLod('Data/H3ab_bmp.lod');
  ScanLod('Data/H3bitmap.lod');
  ScanDir('Data');

  SetCurrentVFSPath('SPRITES/');
  FCurrentFilter := [TResourceType.Animation];
  ScanLod('Data/H3ab_spr.lod');
  ScanLod('Data/H3sprite.lod');
  ScanDir('Sprites');

  FCurrentFilter := [TResourceType.Text];
  SetCurrentVFSPath('DATA/');
  ScanLod('Mods/WoG/Data/hmm35wog.pac');

  FCurrentFilter := [TResourceType.Animation];
  SetCurrentVFSPath('SPRITES/');
  ScanLod('Mods/WoG/Data/hmm35wog.pac');

//
// "CONFIG/":
//[
//{"type" : "dir",  "path" : "GLOBAL/Config",}, // separate to avoid overwriting global resources
//{"type" : "dir",  "path" : "LOCAL/Config", "writeable": true}
//],

  SetCurrentVFSPath('CONFIG/');
  FCurrentFilter := [TResourceType.Text, TResourceType.Json];
  ScanDir('config');
end;

end.

