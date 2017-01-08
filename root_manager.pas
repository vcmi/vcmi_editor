{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit root_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,  FileUtil, Graphics, LazFileUtils, LazLogger, gl, glext40, Forms, Controls,
  progress_form, filesystem_base, root_form, filesystem, terrain, map_objects,
  editor_graphics, lists_manager, OpenGLContext, editor_gl, editor_types,
  locale_manager, editor_classes, vcmi.dirs.base, vcmi.image_loaders;

type

  { TRootManager }

  TRootManager = class(TDataModule)
    HeroIcons: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    type
      TLoadObjectProc = procedure(AProgess: IProgressCallback; APaths: TModdedConfigPaths) of object;
  private
    FDirs: TDirs;

    FLocaleManager: TLocaleManager;
    FProgressForm:    TProgressForm;
    FHiddenForm:      TRootForm;
    FResourceManager: TFSManager;

    FTerrianManager:  TTerrainManager;
    FObjManager:      TObjectsManager;
    FGraphicsManager: TGraphicsManager;
    FListsManager:    TListsManager;


    FGLContext: TOpenGLControl;
    function GetResourceManager: IResourceLoader;

    procedure GetConvertedModConfig(AMetaClass: string; AConverted: TModdedConfigPaths);

    procedure DoLoadObjects(AMetaClass: AnsiString; ALoader: TLoadObjectProc);

    procedure LoadMapObjects;
    procedure LoadHeroPortraits;
  public
    procedure InitComplete;

    property ProgressForm: TProgressForm read FProgressForm;
    property ResourceManager: IResourceLoader read GetResourceManager;

    property GraphicsManager: TGraphicsManager read FGraphicsManager;
    property ObjectsManager: TObjectsManager read FObjManager;
    property TerrainManager: TTerrainManager read FTerrianManager;
    property SharedContext: TOpenGLControl read FGLContext;
    property ListsManager: TListsManager read FListsManager;
    property LocaleManager: TLocaleManager read FLocaleManager;
  end;

var
  RootManager: TRootManager;

implementation

uses
  editor_utils, editor_consts;

{$R *.lfm}

{ TRootManager }

procedure TRootManager.DataModuleCreate(Sender: TObject);
var
  log_name: string;
begin
  FDirs := TDirs.GetActualClass.Create;

  log_name := FDirs.UserCachePath + 'VCMI_Editor.log';

  if FileExistsUTF8(log_name) then
  begin
    DeleteFileUTF8(log_name);
  end;

  DebugLogger.LogName := log_name;
  DebugLogger.CloseLogFileBetweenWrites := False;

  FHiddenForm := TRootForm.Create(Self);

  FGLContext := FHiddenForm.RootContext;

  if not FGLContext.MakeCurrent() then
  begin
    Application.Terminate;
    raise Exception.Create('Unable to switch GL context');
  end;

  DebugLn('Version: ', glGetString(GL_VERSION));
  DebugLn('Vendor: ', glGetString(GL_VENDOR));
  DebugLn('Renderer: ', glGetString(GL_RENDERER));
  DebugLn('Glsl: ', glGetString(GL_SHADING_LANGUAGE_VERSION));

  if not Load_GL_VERSION_3_3() then
  begin
    Application.Terminate;
    raise Exception.Create('Error initializing OpenGL');
  end;

  FProgressForm := TProgressForm.Create(Self);
  FProgressForm.Visible := True;

  Application.ProcessMessages;

  GlobalContextState := TGlobalState.Create;
  GlobalContextState.Init;

  FResourceManager := TFSManager.Create(self);
  FDirs.FillDataPaths(FResourceManager.DataPath);
  FResourceManager.Load(FProgressForm);

  FGraphicsManager := TGraphicsManager.Create(FResourceManager);

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
  ProgressForm.NextStage('Loading hero portraits ...');
  FListsManager.LoadHeroPortraits(ProgressForm);
  LoadHeroPortraits;

  ProgressForm.NextStage('Loading terrain graphics ...');
  FTerrianManager := TTerrainManager.Create(FGraphicsManager);

  FTerrianManager.LoadConfig;
  FTerrianManager.LoadTerrainGraphics;

  LoadMapObjects;

  ProgressForm.SetMax(1);
  ProgressForm.NextStage('Building objects search index ...');
  FObjManager.BuildIndex;
  ProgressForm.Advance(1);
end;

procedure TRootManager.DataModuleDestroy(Sender: TObject);
begin
  GlobalContextState.Free;
  GlobalContextState := nil;
  FDirs.Free;
end;

function TRootManager.GetResourceManager: IResourceLoader;
begin
  Result := FResourceManager;
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
  //TODO: TRootManager.LoadHeroPortraits
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

end.

