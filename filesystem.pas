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
  Classes, SysUtils,FileUtil,
  gmap, fgl,
  vcmi_json,
  editor_classes,
  filesystem_base, lod ;

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

type

  TVFSDir = (DATA, SPRITES, CONFIG);
const
  VFS_PATHS: array [TVFSDir] of string = ('DATA/','SPRITES/','CONFIG/');

  VFS_FILTERS: array [TVFSDir] of TResourceTypes = (
    [TResourceType.Text],
    [TResourceType.Animation, TResourceType.Json],
    [TResourceType.Json,TResourceType.Text]);

  //TODO: mod support

type
  {$push}
  {$m+}

  { TFilesystemConfigItem }

  TFilesystemConfigItem = class(TCollectionItem)
  private
    FPath: string;
    FType: string;
    procedure SetPath(AValue: string);
    procedure SetType(AValue: string);
  published
    property &Type:string read FType write SetType;
    property Path: string read FPath write SetPath;
  end;

  { TFilesystemConfigItems }

  TFilesystemConfigItems = class(specialize TGArrayCollection<TFilesystemConfigItem>)
  public

    constructor Create;
    destructor Destroy; override;
  end;

  { TFilesystemConfigPath }

  TFilesystemConfigPath = class(TNamedCollectionItem, IEmbeddedCollection)
  private
    FItems: TFilesystemConfigItems;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Items: TFilesystemConfigItems read FItems;

    //IEmbeddedCollection
    function GetCollection: TCollection;
  end;


  { TFilesystemConfig }

  TFilesystemConfig = class (specialize TGNamedCollection<TFilesystemConfigPath>)
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
    path: TFilename;
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

    procedure SetCurrentVFSPath(ACurrentVFSPath: TVFSDir);
    procedure SetCurrentVFSPath(ACurrentVFSPath: string);

    function MakeFullPath(RelPath: string):string;

    function MatchFilter(AExt: string; out AType: TResourceType): boolean;
    function VCMIRelPathToRelPath(APath: string): string;

    procedure OnLodItemFound(Alod: TLod; constref AItem: TLodItem);
    procedure ScanLod(LodRelPath: string);

    procedure OnFileFound(FileIterator: TFileIterator);
    procedure ScanDir(RelDir: string);

    procedure LoadFileResource(AResource: IResource; APath: TFilename);

    procedure LoadFSConfig;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ScanFilesystem;

    procedure LoadToStream(AStream: TStream; AResType: TResourceType; AName: string);  deprecated;
    procedure LoadResource(AResource: IResource; AResType: TResourceType;
      AName: string);

  public
    class function NormalizeResName(const AName: string): string; static;
  end;


implementation

const
  GAME_PATH_CONFIG = 'gamepath.txt';

  RES_TO_EXT: array[TResourceType] of string = (
    'TXT','JSON','DEF'
  );

  FS_CONFIG = 'config'+DirectorySeparator+'filesystem.json';
  FS_CONFIG_FIELD = 'filesystem';

  MOD_CONFIG = 'mod.json';
  MOD_ROOT = 'Content';

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

function TFilesystemConfigPath.GetCollection: TCollection;
begin
  Result := FItems;
end;

{ TFilesystemConfigItems }

constructor TFilesystemConfigItems.Create;
begin
  inherited Create;
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

procedure TFilesystemConfigItem.SetType(AValue: string);
begin
  if FType = AValue then Exit;
  FType := AValue;
end;

{ TFilesystemConfig }

constructor TFilesystemConfig.Create;
begin
  inherited Create;
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

procedure TFSManager.LoadFileResource(AResource: IResource; APath: TFilename);
var
  stm: TFileStream;
begin
  stm := TFileStream.Create(APath,fmOpenRead or fmShareDenyWrite);
  try
    stm.Seek(0,soBeginning);
    AResource.LoadFromStream(stm);
  finally
    stm.Free;
  end;
end;

procedure TFSManager.LoadFSConfig;
var
  config_res: TJsonResource;
begin
  config_res := TJsonResource.Create;
  try
    LoadFileResource(config_res, FGamePath+FS_CONFIG);
    config_res.DestreamTo(FConfig,FS_CONFIG_FIELD);
  finally
    config_res.Free;
  end;
end;

procedure TFSManager.LoadResource(AResource: IResource;
  AResType: TResourceType; AName: string);
var
  stm: TFileStream;

  res_id: TResId;
  res_loc: TResLocation;
  it : TResIDToLcationMap.TIterator;
begin
  AName := NormalizeResName(AName);

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
    TLocationType.InLod: res_loc.lod.LoadResource(AResource,res_loc.FileHeader) ;
    TLocationType.InFile: LoadFileResource(AResource,res_loc.path);
  end;
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

procedure TFSManager.SetCurrentVFSPath(ACurrentVFSPath: TVFSDir);
begin
  SetCurrentVFSPath(VFS_PATHS[ACurrentVFSPath]);
  FCurrentFilter := VFS_FILTERS[ACurrentVFSPath];
end;

function TFSManager.VCMIRelPathToRelPath(APath: string): string;
var
  p: SizeInt;
  root: String;
begin
  Result := '';
  p :=  Pos('/',APath);

  root := Copy(APath,1,p-1);

  case root of
    'ALL',
    'GLOBAL': begin
      Result := Copy(APath,p+1,MaxInt);
    end;
  end;

end;

procedure TFSManager.LoadToStream(AStream: TStream;
  AResType: TResourceType; AName: string);
var
  stm: TFileStream;

  res_id: TResId;
  res_loc: TResLocation;
  it : TResIDToLcationMap.TIterator;
begin
  AName := NormalizeResName(AName);

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

class function TFSManager.NormalizeResName(const AName: string): string;
begin
  Result := SetDirSeparators(AName);
  Result := UpperCase(Result);
  Result := ExtractFileNameWithoutExt(Result);
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
  procedure ProcessConfigItem(APath: TFilesystemConfigPath);
  var
    vfs_path: String;
    item: TFilesystemConfigItem ;
    rel_path: String;
    i: Integer;
  begin
    vfs_path := APath.DisplayName;

    SetCurrentVFSPath(vfs_path);

    FCurrentFilter := [TResourceType.Text,TResourceType.Animation,TResourceType.Json];

    for i := 0 to APath.Items.Count - 1 do
    begin
      item := APath.Items.Items[i];

      rel_path := VCMIRelPathToRelPath(item.Path);

      if rel_path = '' then
        Continue;

      case item.&Type of
        'lod':begin
          ScanLod(rel_path);
        end;
        'dir':begin
           ScanDir(rel_path);
        end;
      end;
    end;


  end;
var
  s: String;
  sl: TStringList;
  i: Integer;

begin
  sl := TStringList.Create;  //TODO: platform handling
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

  FGamePath := IncludeTrailingPathDelimiter(FGamePath);

  LoadFSConfig;

  for i := 0 to FConfig.Count - 1 do
  begin
    ProcessConfigItem(FConfig[i]);
  end;

  //TODO: use mod config

  SetCurrentVFSPath(TVFSDir.DATA);
  ScanLod('Mods/WoG/Data/hmm35wog.pac');

  SetCurrentVFSPath(TVFSDir.SPRITES);
  ScanLod('Mods/WoG/Data/hmm35wog.pac');


end;

end.

