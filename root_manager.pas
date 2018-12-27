{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit root_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,  FileUtil, Graphics, LazFileUtils, LazLogger, LazUTF8Classes, gl, vcmi.glext, Forms, Controls,
  progress_form, filesystem_base, root_form, filesystem, terrain, map_objects,
  editor_graphics, lists_manager, vcmi.OpenGLContext, editor_gl, editor_types,
  locale_manager, editor_classes, vcmi.dirs.base, vcmi.image_loaders, zlib_stream;

type
  TConsoleCommand = (None,Help,Convert);

  { TRootManager }

  TRootManager = class(TDataModule)
    HeroIcons: TImageList;
    DefeatIcons: TImageList;
    VictoryIcons: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    type
      TLoadObjectProc = procedure(AProgess: IProgressCallback; APaths: TModdedConfigPaths) of object;
  private
    FConsoleCommand: TConsoleCommand;
    FDirs: TDirs;

    FLocaleManager: TLocaleManager;
    FParam1: AnsiString;
    FParam2: AnsiString;
    FProgressForm:    TProgressForm;
    FHiddenForm:      TRootForm;
    FResourceManager: TFSManager;

    FTerrianManager:  TTerrainManager;
    FObjManager:      TObjectsManager;
    FGraphicsManager: TGraphicsManager;
    FListsManager:    TListsManager;

    FGLContext: TOpenGLControl;

    FDestDirectory: AnsiString;
    FSrcFiles: TStrings;

    function GetBatchMode: boolean;
    function GetResourceManager: IResourceLoader;

    procedure GetConvertedModConfig(AMetaClass: string; AConverted: TModdedConfigPaths);

    procedure DoLoadObjects(AMetaClass: AnsiString; ALoader: TLoadObjectProc);

    procedure LoadMapObjects;
    procedure LoadHeroPortraits;
    procedure LoadVictIcons; unimplemented;
    procedure LoadLossIcons; unimplemented;

    procedure PrintUsage;
    procedure ProcessCommandLine;
    procedure SetConsoleCommand(AValue: TConsoleCommand);

    //src - h3m file, dst - path to store result
    procedure MapConvertionOneFile(src: AnsiString);
    procedure MapConvertionFileFound(FileIterator: TFileIterator);
    procedure RunMapConvertion;
    procedure SetParam1(AValue: AnsiString);
    procedure SetParam2(AValue: AnsiString);
  public
    procedure InitComplete;

    procedure RunBatch;

    property ProgressForm: TProgressForm read FProgressForm;
    property ResourceManager: IResourceLoader read GetResourceManager;

    property GraphicsManager: TGraphicsManager read FGraphicsManager;
    property ObjectsManager: TObjectsManager read FObjManager;
    property TerrainManager: TTerrainManager read FTerrianManager;
    property SharedContext: TOpenGLControl read FGLContext;
    property ListsManager: TListsManager read FListsManager;
    property LocaleManager: TLocaleManager read FLocaleManager;

    property BatchMode: boolean read GetBatchMode;
    property ConsoleCommand: TConsoleCommand read FConsoleCommand write SetConsoleCommand;

    property Param1: AnsiString read FParam1 write SetParam1;
    property Param2: AnsiString read FParam2 write SetParam2;
  end;

var
  RootManager: TRootManager;

implementation

uses
  editor_utils, editor_consts, Map, map_format, map_format_h3m, map_format_zip;

{$R *.lfm}

{ TRootManager }

procedure TRootManager.DataModuleCreate(Sender: TObject);
var
  log_name: string;
begin
  FHiddenForm := TRootForm.Create(Self);

  FProgressForm := TProgressForm.Create(Self);
  FProgressForm.Visible := True;

  Application.OptionChar := '-';

  ProcessCommandLine;
  FDirs := TDirs.GetActualClass.Create;

  log_name := FDirs.UserCachePath + 'VCMI_Editor.log';

  if FileExistsUTF8(log_name) then
  begin
    DeleteFileUTF8(log_name);
  end;

  DebugLogger.LogName := log_name;
  DebugLogger.CloseLogFileBetweenWrites := False;

  FGLContext := FHiddenForm.RootContext;

  if not BatchMode then
  begin
    if not FGLContext.MakeCurrent() then
    begin
      Application.Terminate;
      raise Exception.Create('Unable to switch GL context');
    end;

    DebugLn('Version: ', glGetString(GL_VERSION));
    DebugLn('Vendor: ', glGetString(GL_VENDOR));
    DebugLn('Renderer: ', glGetString(GL_RENDERER));
    DebugLn('Glsl: ', glGetString(GL_SHADING_LANGUAGE_VERSION));

    if not Load_GL_version_3_3_CORE() then
    begin
      Application.Terminate;
      raise Exception.Create('Error initializing OpenGL. Version 3.3 core required.');
    end;

    GImageLoaders.CheckUpdated();

    //if not Load_GL_ARB_texture_rg() then
    //begin
    //   Application.Terminate;
    //   raise Exception.Create('Required extension GL_ARB_texture_rg missing');
    //end;

  end;

  glGetError();//ignore

  Application.ProcessMessages;


  FResourceManager := TFSManager.Create(self);
  FDirs.FillDataPaths(FResourceManager.DataPath);
  FResourceManager.Load(FProgressForm);

  FGraphicsManager := TGraphicsManager.Create(FResourceManager);
  FGraphicsManager.Blocked:=BatchMode;
  FGraphicsManager.PreLoad;

  ProgressForm.NextStage('Loading configuration ...');

  FLocaleManager := TLocaleManager.Create(FResourceManager);
  FLocaleManager.LoadTexts;

  FListsManager := TListsManager.Create(FResourceManager);
  FListsManager.PreLoad;

  DoLoadObjects(FACTION_METACLASS, @FListsManager.LoadFactions);
  DoLoadObjects(HEROCLASS_METACLASS, @FListsManager.LoadHeroClasses);
  DoLoadObjects(CREATURE_METACLASS, @FListsManager.LoadCreatures);
  DoLoadObjects(ARTIFACT_METACLASS, @FListsManager.LoadArtifacts);
  DoLoadObjects(HERO_METACLASS, @FListsManager.LoadHeroes);
  DoLoadObjects(SPELL_METACLASS, @FListsManager.LoadSpells);

  FListsManager.ProcessResolveRequests;
  FListsManager.LoadHeroPortraits(ProgressForm);

  if not BatchMode then
  begin
    ProgressForm.NextStage('Loading icons ...');
    LoadHeroPortraits;
    LoadVictIcons;
    LoadLossIcons;
  end;

  FTerrianManager := TTerrainManager.Create(FGraphicsManager);

  ProgressForm.NextStage('Loading terrain graphics ...');
  FTerrianManager.LoadConfig;

  if not BatchMode then
  begin
    FTerrianManager.LoadTerrainGraphics;
  end;

  LoadMapObjects;

  ProgressForm.SetMax(1);
  ProgressForm.NextStage('Building objects search index ...');
  FObjManager.BuildIndex;
  ProgressForm.Advance(1);

  FSrcFiles := TStringList.Create;
end;

procedure TRootManager.DataModuleDestroy(Sender: TObject);
begin
  FSrcFiles.Free;
  FDirs.Free;
end;

function TRootManager.GetResourceManager: IResourceLoader;
begin
  Result := FResourceManager;
end;

function TRootManager.GetBatchMode: boolean;
begin
  Result := FConsoleCommand <> TConsoleCommand.None;
end;

procedure TRootManager.GetConvertedModConfig(AMetaClass: string; AConverted: TModdedConfigPaths);
var
  i: Integer;
  mod_config: TBaseConfig;
  item: TModdedConfigPath;
begin
  for i := 0 to FResourceManager.Configs.Count - 1 do
  begin
    mod_config :=  FResourceManager.Configs.Data[i];
    item.ModID:=FResourceManager.Configs.Keys[i];

    FillStringArray(mod_config.ConfigPath[AMetaClass],item.Config);
    AConverted.PushBack(item);
  end;
end;

procedure TRootManager.DoLoadObjects(AMetaClass: AnsiString;
  ALoader: TLoadObjectProc);
var
  Config: TModdedConfigPaths;
begin
  Config := TModdedConfigPaths.Create;
  try
    GetConvertedModConfig(AMetaClass, Config);
    ALoader(FProgressForm, Config);
  finally
    Config.Free;
  end;
end;

procedure TRootManager.LoadMapObjects;
var
  ObjectTypesConfig: TModdedConfigPaths;
begin
  ObjectTypesConfig := TModdedConfigPaths.Create;
  try
    FObjManager := TObjectsManager.Create(FGraphicsManager);
    FObjManager.ListsManager := FListsManager;
    GetConvertedModConfig(OBJECT_METACLASS, ObjectTypesConfig);
    FObjManager.LoadObjects(ProgressForm, ObjectTypesConfig);
  finally
    ObjectTypesConfig.Free;
  end;
end;

procedure TRootManager.LoadHeroPortraits;
const
  ICON_WIDTH = 58;
  ICON_HEIGHT = 64;

  procedure AddEmpty();
  var
    bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    bmp.Width := ICON_WIDTH;
    bmp.Height:=ICON_HEIGHT;

    HeroIcons.Add(bmp, nil);
    bmp.Free;
  end;

var
  i, portrait_count: Integer;
  info: THeroPortraitInfo;

  res: TImageResource;
  path: AnsiString;
begin
  HeroIcons.Width:=ICON_WIDTH;
  HeroIcons.Height:=ICON_HEIGHT;

  portrait_count := FListsManager.HeroPortraits.Count;

  for i := 0 to portrait_count - 1 do
  begin
    info :=  FListsManager.HeroPortraits[i];

    if info.IsEmpty then
    begin
      AddEmpty();
      DebugLn('empty portrait info');
    end
    else
    begin
      path := 'SPRITES\'+info.IconPath;

      if not ResourceManager.ExistsResource(TResourceType.Image, path) then
      begin
        path := 'DATA\'+info.IconPath;

        if not ResourceManager.ExistsResource(TResourceType.Image, path) then
        begin
          AddEmpty();
          Continue;
        end;
      end;

      res := TImageResource.Create(path);
      res.Load(ResourceManager);

      if Assigned(res.Data.Graphic) then
      begin
        HeroIcons.Add(res.Data.Graphic as TCustomBitmap, nil)
      end
      else
      begin
        AddEmpty();
        DebugLn('Portrait loading failed ', info.IconPath);
      end;

      res.Free;
    end;
  end;
end;

procedure TRootManager.LoadVictIcons;
begin
  //SCNRVICT
  VictoryIcons.Clear;
end;

procedure TRootManager.LoadLossIcons;
begin
  //SCNRLOSS
end;

procedure TRootManager.PrintUsage;
begin
  FProgressForm.AddError('Usage: ');
  FProgressForm.AddError('vcmieditor [/path/to/map] - normal run with optional map open on start');
  FProgressForm.AddError('-h, --help - show this information');
  FProgressForm.AddError('-c, --convert /source/path /destination/path - convert single map or directory to vcmi format');
end;

procedure TRootManager.ProcessCommandLine;
var
  opts, nonopts: TStrings;
  errors: String;
begin
  opts := TStringList.Create;
  nonopts := TStringList.Create;

  try
    errors := Application.CheckOptions('hc',['help','convert'], Opts, nonopts);

    if errors <> '' then
    begin
      FProgressForm.AddError(errors);
    end
    else
    begin
      if Application.HasOption('c', 'convert') then
      begin
        ConsoleCommand:=TConsoleCommand.Convert;

        if nonopts.Count = 2 then
        begin
           Param1:=nonopts[0];
           Param2:=nonopts[1];
        end
        else
        begin
          PrintUsage;
        end;
      end
      else if Application.HasOption('h', 'help') then
      begin
        ConsoleCommand:=TConsoleCommand.Help;
        PrintUsage;
      end;
    end;
  finally
    opts.Free;
    nonopts.Free;
  end;
end;

procedure TRootManager.SetConsoleCommand(AValue: TConsoleCommand);
begin
  FConsoleCommand:=AValue;
end;

procedure TRootManager.MapConvertionOneFile(src: AnsiString);
var
  dest_file: AnsiString;

  map: TVCMIMap;

  reader: IMapReader;
  stm: TFileStreamUTF8;
  cstm: TStream;
  is_compressed: Boolean;
  magic: dword;

  FEnv: TMapEnvironment;

  FZbuffer: TZBuffer;

  writer: IMapWriter;
  stm_out: TFileStreamUTF8;
begin
  map := nil;
  dest_file := IncludeTrailingPathDelimiter(FDestDirectory) + ExtractFileNameOnly(src)+'.vmap';

  FEnv.i18n := LocaleManager;
  FEnv.lm := ListsManager;
  FEnv.om := ObjectsManager;
  FEnv.tm := TerrainManager;
  FEnv.gm := GraphicsManager;

  FZbuffer := TZBuffer.Create;

  stm := TFileStreamUTF8.Create(src,fmOpenRead or fmShareDenyWrite);
  stm.Seek(0,soBeginning);

  try
     reader := TMapReaderH3m.Create(FEnv);
     magic := 0;
     stm.Read(magic,SizeOf(magic));
     stm.Seek(0,soBeginning);

     if (magic and $ffffff) = $00088B1F then
     begin
        cstm := TZlibInputStream.CreateGZip(FZbuffer, stm,0);
        is_compressed := true;
     end
     else begin
        cstm := stm;
     end;

     map := reader.Read(cstm);
   finally
     FreeAndNil(stm);
     if is_compressed then FreeAndNil(cstm);
     FreeAndNil(FZbuffer);
   end;

   stm_out := TFileStreamUTF8.Create(dest_file,fmCreate);
   stm_out.Size := 0;

   try
     writer := TMapWriterZIP.Create(FEnv);
     map.SaveToStream(stm_out,writer);
   finally
     stm_out.Free;
     FreeAndNil(map);
   end;
end;

procedure TRootManager.MapConvertionFileFound(FileIterator: TFileIterator);
begin
  FSrcFiles.Add(FileIterator.FileName);
end;

procedure TRootManager.RunMapConvertion;
var
  s, src_path, dst_path: AnsiString;

  sr: TFileSearcher;
begin
  FSrcFiles.Clear;
  FProgressForm.NextStage('Converting maps');

  src_path:=Param1;
  dst_path:=Param2;

  FProgressForm.AddMessage(Format('From "%s" to "%s"',[src_path, dst_path]));

  if DirectoryExistsUTF8(src_path) then
  begin
    ForceDirectoriesUTF8(dst_path);
    FDestDirectory := dst_path;

    sr := TFileSearcher.Create;
    sr.OnFileFound:=@MapConvertionFileFound;
    sr.Search(src_path,'*.h3m');
    sr.Free;
  end
  else if FileExistsUTF8(src_path) then
  begin
    FSrcFiles.Add(src_path);
  end
  else
  begin
    FProgressForm.AddError('Invalid source path');
  end;

  ForceDirectoriesUTF8(dst_path);
  FDestDirectory := dst_path;
  FProgressForm.SetMax(FSrcFiles.Count);

  for s in FSrcFiles do
  begin
    try
      MapConvertionOneFile(s);
    except
      on e:Exception do
      begin
        FProgressForm.AddError('Failed '+s);
        FProgressForm.AddError(e.Message);
      end;
    end;
    FProgressForm.Advance(1);
    Application.ProcessMessages;
  end;
end;

procedure TRootManager.SetParam1(AValue: AnsiString);
begin
  FParam1:=AValue;
end;

procedure TRootManager.SetParam2(AValue: AnsiString);
begin
  FParam2:=AValue;
end;

procedure TRootManager.InitComplete;
begin
  if FProgressForm.Visible then
  begin
    if FProgressForm.HasErrors then
    begin
      FProgressForm.Completed:=True;
    end
    else
    begin
      FProgressForm.Close;
    end;
  end;
end;

procedure TRootManager.RunBatch;
begin
  case FConsoleCommand of
    TConsoleCommand.None: ;
    TConsoleCommand.Help: ;
    TConsoleCommand.Convert: RunMapConvertion;
  end;

  FProgressForm.Completed := True;
  while FProgressForm.Visible do
  begin
    Application.ProcessMessages;
  end;
end;

end.

