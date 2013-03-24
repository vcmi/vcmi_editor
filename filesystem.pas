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
  gmap, gqueue, fgl,
  lazutf8classes,
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

  TModId = AnsiString;

{$push}
{$M+}
  { TModConfig }

  TModConfig =class
  private
    FArtifacts: TStringList;
    FConflicts: TStringList;
    FCreatures: TStringList;
    FDepends: TStringList;
    FDescription: string;
    FDisabled: Boolean;
    FFactions: TStringList;
    FFilesystem: TFilesystemConfig;
    FHeroClasses: TStringList;
    FHeroes: TStringList;
    FID: TModId;
    FName: string;
    FPath: String;
    function GetArtifacts: TStrings;
    function GetConflicts: TStrings;
    function GetCreatures: TStrings;
    function GetDepends: TStrings;
    function GetFactions: TStrings;
    function GetHeroClasses: TStrings;
    function GetHeroes: TStrings;
    procedure SetDescription(AValue: string);
    procedure SetDisabled(AValue: Boolean);
    procedure SetID(AValue: TModId);
    procedure SetName(AValue: string);
    procedure SetPath(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    property Disabled:Boolean read FDisabled write SetDisabled;
    property ID: TModId read FID write SetID;
    property Path: String read FPath write SetPath;
    procedure MayBeSetDefaultFSConfig;
  published
    //short decription
    property Name: string read FName write SetName;
    //long description
    property Description: string read FDescription write SetDescription;
    //list of IDs
    property Depends: TStrings read GetDepends;
    //list of IDs
    property Conflicts: TStrings read GetConflicts;

    property Factions: TStrings read GetFactions ;
    property HeroClasses: TStrings read GetHeroClasses ;
    property Heroes: TStrings read GetHeroes ;
    property Creatures: TStrings read GetCreatures ;
    property Artifacts: TStrings read GetArtifacts ;
    //TODO: Spells
    //TODO: ObjectTypes
    //TODO: Objects

    property Filesystem: TFilesystemConfig read FFilesystem;

  end;
{$pop}

  TModConfigs = specialize TFPGObjectList<TModConfig>;

  TIdToModMap = specialize TFPGMap<TModId,TModConfig>;

  TModQueue = specialize TQueue<TModConfig>;

  //index = index in TModConfigs
  //TModDependencyMatrix[i,j] === TModConfigs[i] depends on TModConfigs[j]
  TModDependencyMatrix = array of array of boolean;


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


  { TFSManager }

  TFSManager = class (TComponent,IResourceLoader)
  private
    FConfig: TFilesystemConfig;

    FResMap: TResIDToLcationMap;
    FLodList: TLodList;

    FModMap: TIdToModMap;
    FMods: TModConfigs;

    FGamePath: string;

    FCurrentFilter:TResourceTypes;
    FCurrentVFSPath: string;
    FCurrentRelPath: string;
    FCurrentRootPath: string;


    function GetPrivateConfigPath: string;

    procedure SetCurrentVFSPath(ACurrentVFSPath: TVFSDir);
    procedure SetCurrentVFSPath(ACurrentVFSPath: string);

    function MakeFullPath(ARootPath: string; RelPath: string):string;

    function MatchFilter(AExt: string; out AType: TResourceType): boolean;
    function VCMIRelPathToRelPath(APath: string): string;

    procedure OnLodItemFound(Alod: TLod; constref AItem: TLodItem);
    procedure ScanLod(LodRelPath: string; ARootPath: string);

    procedure OnFileFound(FileIterator: TFileIterator);
    procedure ScanDir(RelDir: string; ARootPath: string);

    procedure LoadFileResource(AResource: IResource; APath: TFilename);

    procedure ProcessConfigItem(APath: TFilesystemConfigPath; ARootPath: string);
    procedure ProcessFSConfig(AConfig: TFilesystemConfig; ARootPath: string);

    procedure LoadFSConfig;
    procedure ScanFilesystem;

    function GetModPath: string;
    procedure OnModConfigFound(FileIterator: TFileIterator);

    procedure ScanMods;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load(AProgress: IProgressCallback);

    procedure LoadResource(AResource: IResource; AResType: TResourceType;
      AName: string);

  public
    class function NormalizeModId(AModId: TModId): TModId; static;
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
  MOD_ROOT = '/Content';

function ComapreModId(const a, b: TModId): integer;
begin
  Result := CompareText(a,b);
end;

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


{ TModConfig }

constructor TModConfig.Create;
begin
  FFilesystem := TFilesystemConfig.Create;

  FConflicts := TStringList.Create;
  FDepends := TStringList.Create;

  FArtifacts := TStringList.Create;
  FCreatures := TStringList.Create;
  FFactions := TStringList.Create;
  FHeroClasses := TStringList.Create;
  FHeroes := TStringList.Create;
end;

destructor TModConfig.Destroy;
begin
  FHeroes.Free;
  FHeroClasses.Free;
  FFactions.Free;
  FCreatures.Free;
  FArtifacts.Free;

  FDepends.Free;
  FConflicts.Free;

  FFilesystem.Free;
  inherited Destroy;
end;

function TModConfig.GetArtifacts: TStrings;
begin
  Result := FArtifacts;
end;

function TModConfig.GetConflicts: TStrings;
begin
  Result := FConflicts;
end;

function TModConfig.GetCreatures: TStrings;
begin
  Result := FCreatures;
end;

function TModConfig.GetDepends: TStrings;
begin
  Result := FDepends;
end;

function TModConfig.GetFactions: TStrings;
begin
  Result := FFactions;
end;

function TModConfig.GetHeroClasses: TStrings;
begin
  Result := FHeroClasses;
end;

function TModConfig.GetHeroes: TStrings;
begin
  Result := FHeroes;
end;

procedure TModConfig.MayBeSetDefaultFSConfig;
var
  cur_path: TFilesystemConfigPath;
  item: TFilesystemConfigItem;
begin
  if Filesystem.Count = 0 then
  begin
    cur_path := Filesystem.Add;
    cur_path.DisplayName := '';

    item :=  cur_path.Items.Add;
    item.&Type := 'dir';
    item.Path := MOD_ROOT;
  end;
end;

procedure TModConfig.SetDescription(AValue: string);
begin
  if FDescription = AValue then Exit;
  FDescription := AValue;
end;

procedure TModConfig.SetDisabled(AValue: Boolean);
begin
  if FDisabled = AValue then Exit;
  FDisabled := AValue;
end;

procedure TModConfig.SetID(AValue: TModId);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TModConfig.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TModConfig.SetPath(AValue: String);
begin
  if FPath = AValue then Exit;
  FPath := AValue;
end;



{ TFSManager }

constructor TFSManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := TFilesystemConfig.Create;
  FResMap := TResIDToLcationMap.Create;
  FLodList := TLodList.Create(True);
  FMods := TModConfigs.Create(True);
  FModMap := TIdToModMap.Create;
  FModMap.OnKeyCompare := @ComapreModId;
end;

destructor TFSManager.Destroy;
begin
  FModMap.Free;
  FMods.Free;
  FLodList.Free;
  FResMap.Free;
  FConfig.Free;
  inherited Destroy;
end;

function TFSManager.GetModPath: string;
begin
  Result := FGamePath + 'Mods';

end;

function TFSManager.GetPrivateConfigPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TFSManager.Load(AProgress: IProgressCallback);
begin
  ScanFilesystem;
  ScanMods;
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

procedure TFSManager.ScanLod(LodRelPath: string; ARootPath: string);
var
  lod: TLod;
begin
  //TODO: no duplicates

  LodRelPath := SetDirSeparators(LodRelPath);

  lod := TLod.Create(MakeFullPath(ARootPath, LodRelPath));

  lod.Scan(@OnLodItemFound);

  FLodList.Add(lod);
end;

procedure TFSManager.ScanMods;
var
  searcher: TFileSearcher;

  sl: TStringListUTF8;
  mod_paths_config: String;
  mod_id: String;
  mod_idx: Integer;
  i: Integer;
begin
  //find mods
  searcher := TFileSearcher.Create;
  searcher.OnFileFound := @OnModConfigFound;

  sl := TStringListUTF8.Create;
  try
    mod_paths_config := GetPrivateConfigPath + 'modpath.txt';
    if FileExistsUTF8(mod_paths_config) then
      sl.LoadFromFile(mod_paths_config);

    sl.Insert(0,GetModPath);

    for i := 0 to sl.Count - 1 do
    begin
      searcher.Search(sl[i],MOD_CONFIG)
    end;

    //<STUB>

    sl.LoadFromFile(GetPrivateConfigPath + 'modlist.txt');

    for i := 0 to sl.Count - 1 do
    begin
      mod_id := NormalizeModId(sl[i]);

      mod_idx := FModMap.IndexOf(mod_id);

      if mod_idx >=0 then
      begin
        ProcessFSConfig(FModMap.Data[mod_idx].Filesystem,FModMap.Data[mod_idx].Path);
      end;
    end;

    //</STUB>
    //TODO:determine load order
  finally
    sl.Free;
    searcher.Free;
  end;

  //configs loaded at this point



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
    'LOCAL':;
  else
    Result := APath;
  end;

end;


function TFSManager.MakeFullPath(ARootPath: string; RelPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ARootPath)+RelPath;
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

class function TFSManager.NormalizeModId(AModId: TModId): TModId;
begin
  Result := Trim(LowerCase(AModId));
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

  rel_path := CreateRelativePath(FileIterator.Path, MakeFullPath(FCurrentRootPath, FCurrentRelPath)); //???

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

procedure TFSManager.OnModConfigFound(FileIterator: TFileIterator);
var
  stm: TFileStreamUTF8;
  destreamer: TVCMIJSONDestreamer;
  mod_path: String;
  mod_id: String;
  mod_config: TModConfig;
begin
  //iterator points to mod.json
  destreamer := TVCMIJSONDestreamer.Create(nil);

  mod_path := FileIterator.Path;

  mod_id := NormalizeModId(ExtractFileNameOnly(ExcludeTrailingBackslash(mod_path)));

  stm := TFileStreamUTF8.Create(FileIterator.FileName,fmOpenRead or fmShareDenyWrite);
  try
    mod_config := TModConfig.Create;
    mod_config.ID := mod_id;
    mod_config.Path := mod_path;
    destreamer.JSONStreamToObject(stm, mod_config,'');
    mod_config.MayBeSetDefaultFSConfig;
    FMods.Add(mod_config);
    FModMap.Add(mod_config.ID,mod_config);
  finally
    stm.Free;
    destreamer.Free;
  end;
end;

procedure TFSManager.ProcessConfigItem(APath: TFilesystemConfigPath;
  ARootPath: string);
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
        ScanLod(rel_path,ARootPath);
      end;
      'dir':begin
         ScanDir(rel_path,ARootPath);
      end;
    end;
  end;
end;

procedure TFSManager.ProcessFSConfig(AConfig: TFilesystemConfig;
  ARootPath: string);
var
  i: Integer;
begin
  for i := 0 to AConfig.Count - 1 do
  begin
    ProcessConfigItem(AConfig[i],ARootPath);
  end;
end;

procedure TFSManager.ScanDir(RelDir: string; ARootPath: string);
var
  srch: TFileSearcher;
  p: string;
begin
  srch := TFileSearcher.Create;
  srch.OnFileFound := @OnFileFound;
  try
    FCurrentRelPath := RelDir;
    FCurrentRootPath := ARootPath;
    p := IncludeTrailingPathDelimiter(MakeFullPath(ARootPath,RelDir));
    srch.Search(p);
  finally
    srch.Free;
  end;
end;

procedure TFSManager.ScanFilesystem;
var
  s: String;
  sl: TStringList;

begin
  sl := TStringList.Create;  //TODO: platform handling
  try
    s := GetPrivateConfigPath+GAME_PATH_CONFIG;
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
  ProcessFSConfig(FConfig,FGamePath);

end;

end.

