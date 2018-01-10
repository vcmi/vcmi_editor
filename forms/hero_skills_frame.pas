{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit hero_skills_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, Buttons, base_options_frame, gui_helpers, Map, editor_classes,
  base_info, object_options, editor_types, lists_manager;

type

  { THeroSkillsFrame }

  THeroSkillsFrame = class(TBaseOptionsFrame)
    act: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    ButtonAdd: TSpeedButton;
    ButtonRemove: TSpeedButton;
    cbCustomise: TCheckBox;
    iml: TImageList;
    MasterySelector: TComboBox;
    SkillsEdit: TListBox;
    SkillSelector: TComboBox;
    procedure actAddExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure cbCustomiseChange(Sender: TObject);
    procedure SkillsEditSelectionChange(Sender: TObject; User: boolean);
  private
    FObject: THeroSecondarySkills;
    FCustomSkills, FDefaultSkills: THeroSecondarySkills;

    procedure SaveSkills;
    procedure LoadSkills(ASrc:THeroSecondarySkills);

    procedure LoadDefaultSkills(AHeroId: AnsiString);

    function FormatInfo(AInfo: THeroSecondarySkill): TLocalizedString;
  protected
    procedure Load; override;
    procedure UpdateControls(); override;
    procedure ApplyDefaults; override;
    procedure ReloadDefaults; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;

    procedure VisitNormalHero(AOptions: TNormalHeroOptions);override;
    procedure VisitRandomHero(AOptions: TRandomHeroOptions);override;
    procedure VisitPrison(AOptions: TPrisonOptions);override;

    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

{$R *.lfm}

{ THeroSkillsFrame }

procedure THeroSkillsFrame.cbCustomiseChange(Sender: TObject);
begin
  UpdateControls();

  if cbCustomise.Checked then
  begin
    LoadSkills(FCustomSkills);
  end
  else
  begin
    SkillSelector.ItemIndex:=-1;
    MasterySelector.ItemIndex:=-1;

    SaveSkills;
    LoadSkills(FDefaultSkills);
  end;
end;

procedure THeroSkillsFrame.SkillsEditSelectionChange(Sender: TObject; User: boolean);
var
  idx: Integer;
  o: THeroSecondarySkill;
begin
  if not cbCustomise.Checked then
    exit;

  idx := SkillsEdit.ItemIndex;

  if idx < 0 then
  begin
    SkillSelector.ItemIndex:=-1;
    MasterySelector.ItemIndex:=-1;

    exit;
  end;

  o := SkillsEdit.Items.Objects[idx] as THeroSecondarySkill;

  SkillSelector.SetValue(ListsManager.SkillInfos, o.Identifier);
  MasterySelector.ItemIndex := Integer(o.Level) - 1;
end;

procedure THeroSkillsFrame.actAddUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=cbCustomise.Checked and (SkillSelector.ItemIndex>=0) and (MasterySelector.ItemIndex>=0);
end;

procedure THeroSkillsFrame.actAddExecute(Sender: TObject);
var
  i, matched: Integer;

  skill_info: TBaseInfo;
  mastery: TSkillLevel;

  o: THeroSecondarySkill;
begin
  matched := -1;

  skill_info := SkillSelector.SelectedInfo;

  if not Assigned(skill_info) then
    exit;

  if MasterySelector.ItemIndex < 0 then
    Exit;
  mastery:=TSkillLevel(MasterySelector.ItemIndex+1);

  for i := 0 to SkillsEdit.Items.Count - 1 do
  begin
    o := SkillsEdit.Items.Objects[i] as THeroSecondarySkill;

    if o.Identifier = skill_info.Identifier then
    begin
      matched:=i;
      break;
    end;
  end;

  if matched < 0 then
  begin
    o := THeroSecondarySkill.Create(nil);
    o.Identifier:=skill_info.Identifier;
    o.Level:=mastery;
    SkillsEdit.AddItem(FormatInfo(o), o);
    FFreeList.Add(o);
  end
  else
  begin
    //update
    o := SkillsEdit.Items.Objects[matched] as THeroSecondarySkill;
    o.Level:=mastery;
    SkillsEdit.Items.Delete(matched);
    SkillsEdit.AddItem(FormatInfo(o), o);
  end;
end;

procedure THeroSkillsFrame.actDeleteExecute(Sender: TObject);
var
  idx: Integer;
begin
  idx := SkillsEdit.ItemIndex;

  if idx >= 0 then
    SkillsEdit.Items.Delete(idx);
end;

procedure THeroSkillsFrame.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=cbCustomise.Checked and (SkillsEdit.ItemIndex>=0);
end;

procedure THeroSkillsFrame.SaveSkills;
var
  i: Integer;
  src, dst: THeroSecondarySkill;
begin
  FCustomSkills.Clear;

  for i := 0 to SkillsEdit.Items.Count - 1 do
  begin
    src := SkillsEdit.Items.Objects[i] as THeroSecondarySkill;

    dst := FCustomSkills.Add;
    dst.Assign(src);
  end;
end;

procedure THeroSkillsFrame.LoadSkills(ASrc: THeroSecondarySkills);
var
  i: Integer;
  src, dst: THeroSecondarySkill;
begin
  FFreeList.Clear;
  SkillsEdit.Clear;

  for i := 0 to ASrc.Count - 1 do
  begin
    src := ASrc.Items[i];

    dst := THeroSecondarySkill.Create(nil);
    dst.Assign(src);

    SkillsEdit.AddItem(FormatInfo(dst), dst);

    FFreeList.Add(dst);
  end;
end;

procedure THeroSkillsFrame.LoadDefaultSkills(AHeroId: AnsiString);
var
  FHeroTypeDefaults, FHeroMapDefaults: IHeroInfo;
begin
  FDefaultSkills.Clear;

  FHeroTypeDefaults := nil;
  FHeroMapDefaults := nil;

  if AHeroId = '' then
  begin
    Exit;
  end;

  FHeroTypeDefaults := ListsManager.Heroes[AHeroId];

  if FUseMapDefaults then
  begin
    FHeroMapDefaults := Map.PredefinedHeroes.FindItem(AHeroId);
  end;

  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetSecondarySkills.Count <> 0) then
    FDefaultSkills.Assign(FHeroMapDefaults.GetSecondarySkills)
  else if Assigned(FHeroTypeDefaults) then
    FDefaultSkills.Assign(FHeroTypeDefaults.GetSecondarySkills);
end;

function THeroSkillsFrame.FormatInfo(AInfo: THeroSecondarySkill): TLocalizedString;
var
  skill_name, skill_mastery: TLocalizedString;
begin
  skill_name := ListsManager.GetSkill(AInfo.Identifier).Name;
  skill_mastery:= MasterySelector.Items[Integer(AInfo.Level) - 1];

  Result := Format('%s %s', [skill_mastery, skill_name]);
end;

procedure THeroSkillsFrame.Load;
begin
  SkillSelector.FillFromList(ListsManager.SkillInfos, '');

  cbCustomise.Checked:=FObject.Count <> 0;

  UpdateControls();

  if cbCustomise.Checked then
  begin
    FCustomSkills.Assign(FObject);
  end
  else begin
    FCustomSkills.Assign(FDefaultSkills);
  end;

  LoadSkills(FCustomSkills);

  cbCustomise.OnChange:=@cbCustomiseChange;
end;

procedure THeroSkillsFrame.UpdateControls;
begin
  inherited UpdateControls;
  SkillsEdit.Enabled := cbCustomise.Checked;
  SkillSelector.Enabled := cbCustomise.Checked;
  MasterySelector.Enabled := cbCustomise.Checked;
end;

procedure THeroSkillsFrame.ApplyDefaults;
begin
  inherited ApplyDefaults;

  if not cbCustomise.Checked then
    LoadSkills(FDefaultSkills);
end;

procedure THeroSkillsFrame.ReloadDefaults;
begin
  inherited ReloadDefaults;
  LoadDefaultSkills(InstanceType);
end;

procedure THeroSkillsFrame.VisitNormalHero(AOptions: TNormalHeroOptions);
begin
  inherited VisitNormalHero(AOptions);

  FUseMapDefaults := True;

  LoadDefaultSkills(AOptions.&type);

  FObject := AOptions.SecondarySkills;
  Load;
end;

procedure THeroSkillsFrame.VisitRandomHero(AOptions: TRandomHeroOptions);
begin
  inherited VisitRandomHero(AOptions);

  FUseMapDefaults := True;

  LoadDefaultSkills('');

  FObject := AOptions.SecondarySkills;
  Load;
end;

procedure THeroSkillsFrame.VisitPrison(AOptions: TPrisonOptions);
begin
  inherited VisitPrison(AOptions);

  FUseMapDefaults := True;

  LoadDefaultSkills(AOptions.&type);

  FObject := AOptions.SecondarySkills;
  Load;
end;

constructor THeroSkillsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCustomSkills := THeroSecondarySkills.Create;
  FDefaultSkills := THeroSecondarySkills.Create;
end;

destructor THeroSkillsFrame.Destroy;
begin
  FCustomSkills.Free;
  FDefaultSkills.Free;
  inherited Destroy;
end;

procedure THeroSkillsFrame.Commit;
begin
  inherited Commit;
  if cbCustomise.Checked then
    SaveSkills;

  if cbCustomise.Checked then
  begin
    FObject.Assign(FCustomSkills);
  end
  else
  begin
    FObject.Clear;
  end;
end;

procedure THeroSkillsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  inherited VisitHeroDefinition(AOptions);

  FUseMapDefaults := False;

  LoadDefaultSkills(AOptions.Identifier);

  FObject := AOptions.Skills;
  Load;
end;

end.

