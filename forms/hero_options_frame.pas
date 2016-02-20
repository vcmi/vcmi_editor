{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit hero_options_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComboEx, EditBtn,
  ComCtrls, Graphics, Dialogs, hero_frame, gui_helpers, object_options,
  editor_types, base_info, editor_consts, lists_manager, editor_classes, Map;

type

  { THeroOptionsFrame }

  THeroOptionsFrame = class(THeroFrame)
    procedure cbBiographyChange(Sender: TObject);
    procedure cbExperienceChange(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure cbPortraitChange(Sender: TObject);
    procedure cbSexChange(Sender: TObject);
    procedure cbSkillsChange(Sender: TObject);
    procedure CustomiseChange(Sender: TObject);
    procedure edHeroClassChange(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
    procedure edTypeChange(Sender: TObject);
  private
    FOptions: THeroOptions;

    function GetHeroClass():AnsiString;
    function GetHeroClassName():TLocalizedString;
  protected
    procedure Load(); override;
  public
    procedure VisitHero(AOptions: THeroOptions); override;

  end;

implementation

{$R *.lfm}

{ THeroOptionsFrame }

procedure THeroOptionsFrame.cbBiographyChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.cbExperienceChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.cbNameChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.cbPortraitChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.cbSexChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.cbSkillsChange(Sender: TObject);
begin
  Inherited;
end;

procedure THeroOptionsFrame.CustomiseChange(Sender: TObject);
begin
  Inherited;
end;

procedure THeroOptionsFrame.edHeroClassChange(Sender: TObject);
var
  class_info: THeroClassInfo;

  editor: TCustomComboBox;
  idx: Integer;
  hero_type: String;
begin
  if not edType.Visible then
    Exit;

  //fill hero types
  editor := Sender as TCustomComboBox;

  if editor.ItemIndex < 0 then
  begin
    edType.ItemIndex := -1;
    edType.Items.Clear;
    edType.Enabled:=false;
  end
  else begin
    edType.Enabled:=true;
    edType.ItemIndex := -1;

    class_info := editor.Items.Objects[editor.ItemIndex] as THeroClassInfo;

    ListsManager.FillWithHeroesOfClass(edType.Items, class_info.Identifier);

    hero_type :=  FOptions.&type;

    for idx := 0 to edType.Items.Count - 1 do
    begin
      if (edType.Items.Objects[idx] as TBaseInfo).Identifier = hero_type then
      begin
        edType.ItemIndex := idx;
        break;
      end;
    end;

    if (edType.ItemIndex = -1) and (edType.Items.Count >0) then
      edType.ItemIndex := 0;
  end;

  edTypeChange(edType);
end;

procedure THeroOptionsFrame.edNameChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.edNameEditingDone(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.edPatrolKeyPress(Sender: TObject; var Key: char);
begin
  inherited;
end;

procedure THeroOptionsFrame.edSexChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroOptionsFrame.edTypeChange(Sender: TObject);
var
  editor: TCustomComboBox;

  info: TBaseInfo;

  definition : THeroDefinition;
begin
  FHeroTypeDefaults := nil;

  editor := Sender as TCustomComboBox;

  info := editor.SelectedInfo;
  definition := nil;
  if Assigned(info) then
  begin
    FHeroTypeDefaults := info as THeroInfo;

    definition := Map.PredefinedHeroes.FindItem(info.Identifier);

    InstanceType:=info.Identifier;
  end;

  FHeroMapDefaults := definition;

  cbPortraitChange(cbPortrait);
  cbExperienceChange(cbExperience);
  cbNameChange(cbName);
  cbSexChange(cbSex);
  cbBiographyChange(cbBiography);
  cbSkillsChange(cbSkills);
  UpdateControls;

end;

procedure THeroOptionsFrame.Load;
begin
  edHeroClass.FillFromList(ListsManager.HeroClassInfos, GetHeroClass);

  edHeroClassChange(edHeroClass); //also loads type

  inherited;

  UpdateControls();
end;

function THeroOptionsFrame.GetHeroClass: AnsiString;
begin
  if not Assigned(FOptions) then
  begin
    exit('');//just to safe
  end;

  if FOptions.&type = '' then
  begin

    if FOptions.MapObject.GetID = TYPE_HERO then
    begin
      Exit(FOptions.MapObject.GetSubId());
    end;
    exit('');
  end;

  Result := ListsManager.Heroes[FOptions.&type].&Class;
end;

function THeroOptionsFrame.GetHeroClassName: TLocalizedString;
var
  hero_class: String;
begin
  if not Assigned(FOptions) then
  begin
    exit('');
  end;

  if FOptions.&type = '' then
  begin
    exit('');
  end;

  hero_class := ListsManager.Heroes[FOptions.&type].&Class;

  Result := ListsManager.HeroClasses[hero_class].Name;
end;

procedure THeroOptionsFrame.VisitHero(AOptions: THeroOptions);
begin
  FOptions := AOptions;
  inherited VisitHero(AOptions); //continue dispatch

  Load();
end;

end.

