{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLogger, gl, glext40, Forms, Controls,
  progress_form, filesystem_base, root_form, filesystem, terrain, objects,
  editor_graphics, lists_manager, OpenGLContext, editor_gl, editor_types;

type

  { TRootManager }

  TRootManager = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FProgressForm:    TProgressForm;
    FHiddenForm:      TRootForm;
    FResourceManager: TFSManager;

    FTerrianManager:  TTerrainManager;
    FObjManager:      TObjectsManager;
    FGraphicsManager: TGraphicsManager;
    FListsManager:    TListsManager;


    FGLContext: TOpenGLControl;
    function GetResourceManager: IResourceLoader;

    procedure ConvertModConfig(AMetaClass: string; AConverted: TModdedConfigVector);

    procedure LoadObjects;
  public
    procedure InitComplete;

    property ProgressForm: TProgressForm read FProgressForm;
    property ResourceManager: IResourceLoader read GetResourceManager;

    property GraphicsManger: TGraphicsManager read FGraphicsManager;
    property ObjectsManager: TObjectsManager read FObjManager;
    property TerrainManager: TTerrainManager read FTerrianManager;
    property SharedContext: TOpenGLControl read FGLContext;
    property ListsManager: TListsManager read FListsManager;
  end;

var
  RootManager: TRootManager;

implementation

uses
  Math, editor_utils;

{$R *.lfm}

{ TRootManager }

procedure TRootManager.DataModuleCreate(Sender: TObject);
var
  log_name: string;
begin
  log_name := ExtractFilePath(ParamStr(0)) + 'editor.log';

  if FileExistsUTF8(log_name) then
  begin
    DeleteFileUTF8(log_name);
  end;

  DebugLogger.LogName := log_name;
  DebugLogger.CloseLogFileBetweenWrites := True;

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
  ProgressForm.StageCount := ifthen(Paramcount > 0, 5, 4);

  Application.ProcessMessages;

  GlobalContextState := TGlobalState.Create;
  GlobalContextState.Init;

  //stage 1
  ProgressForm.NextStage('Scanning filesystem.');

  FResourceManager := TFSManager.Create(self);
  FResourceManager.Load(FProgressForm);

  FGraphicsManager := TGraphicsManager.Create(FResourceManager);

  FListsManager := TListsManager.Create(FResourceManager);
  FListsManager.Load;

  FListsManager.LoadFactions(FResourceManager.GameConfig.Factions);
  FListsManager.LoadSpells(FResourceManager.GameConfig.Spells);

  //stage 2
  ProgressForm.NextStage('Loading terrain graphics.');
  FTerrianManager := TTerrainManager.Create(FGraphicsManager);

  FTerrianManager.LoadConfig;
  FTerrianManager.LoadTerrainGraphics;

  //stage 3

  ProgressForm.NextStage('Loading objects.');
  LoadObjects;

end;

procedure TRootManager.DataModuleDestroy(Sender: TObject);
begin
  GlobalContextState.Free;
  GlobalContextState := nil;
end;

function TRootManager.GetResourceManager: IResourceLoader;
begin
  Result := FResourceManager;
end;

procedure TRootManager.ConvertModConfig(AMetaClass: string;
  AConverted: TModdedConfigVector);
var
  i: Integer;
  mod_config: TBaseConfig;
  item: TModdedConfig;
begin
  for i := 0 to FResourceManager.Configs.Count - 1 do
  begin
    mod_config :=  FResourceManager.Configs.Data[i];
    item.ModID:=FResourceManager.Configs.Keys[i];

    FillStringArray(mod_config.ConfigPath[AMetaClass],item.Config);
    AConverted.PushBack(item);
  end;
end;

procedure TRootManager.LoadObjects;
var
  ObjectTypesConfig: TModdedConfigVector;
begin
  ObjectTypesConfig := TModdedConfigVector.Create;
  try
    FObjManager := TObjectsManager.Create(FGraphicsManager);
    FObjManager.LoadObjects(RootManager.ProgressForm, ObjectTypesConfig);
  finally
    ObjectTypesConfig.Free;
  end;


end;

procedure TRootManager.InitComplete;
begin
  if FProgressForm.Visible then
  begin
    FProgressForm.Close;
  end;

end;

end.

