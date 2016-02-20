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
unit hero_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, strutils, typinfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComboEx, EditBtn, ComCtrls, base_options_frame,
  gui_helpers, object_options, editor_consts, editor_types, base_info,
  lists_manager, editor_rtti, editor_classes, Map, LCLType, ExtCtrls, Spin,
  rttiutils;

type

  { THeroFrame }

  THeroFrame = class(TBaseOptionsFrame)
    cbName: TCheckBox;
    cbSex: TCheckBox;
    cbPortrait: TCheckBox;
    cbExperience: TCheckBox;
    cbBiography: TCheckBox;
    cbSkills: TCheckBox;
    AvailableFor: TCheckGroup;
    edBiography: TMemo;
    edSex: TComboBox;
    edPatrol: TComboBox;
    edHeroClass: TComboBox;
    edExperience: TEdit;
    edName: TEdit;
    edPortrait: TComboBoxEx;
    edOwner: TComboBox;
    edType: TComboBox;
    Label1: TLabel;
    AvailableForLabel: TLabel;
    AvailableForPlaceholder: TLabel;
    lbAttack: TLabel;
    lbDefence: TLabel;
    lbSpellPower: TLabel;
    lbKnowledge: TLabel;
    lbPatrol: TLabel;
    lbBiography: TLabel;
    lbSex: TLabel;
    lbName: TLabel;
    lbExperience: TLabel;
    lbPortrait: TLabel;
    lbHeroClass: TLabel;
    lbOwner: TLabel;
    pnSkills: TPanel;
    Placeholder1: TLabel;
    lbType: TLabel;
    Placeholder2: TLabel;
    Placeholder3: TLabel;
    Placeholder5: TLabel;
    Attack: TSpinEdit;
    Defence: TSpinEdit;
    SpellPower: TSpinEdit;
    Knowledge: TSpinEdit;
    procedure cbBiographyChange(Sender: TObject);
    procedure cbExperienceChange(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure cbPortraitChange(Sender: TObject);
    procedure cbSexChange(Sender: TObject);
    procedure cbSkillsChange(Sender: TObject);
    procedure CustomiseChange(Sender: TObject);
    procedure edExperienceEditingDone(Sender: TObject);
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
  private
    FOptions: IEditableHeroInfo;
    FHeroOptions: THeroOptions;
    FHeroDefinition: THeroDefinition;

    procedure Save;

    procedure LoadAvilableFor;
    procedure SaveAvilableFor;

    procedure CommitHeroOptions;
    procedure CommitHeroDefinition;
  protected
    FCustomName: TLocalizedString;
    FCustomFemale: Boolean;
    FCustomBiography: TLocalizedString;
    FCustomPortrait: AnsiString;
    FCustomExperience: UInt64;

    FCustomSkills: THeroPrimarySkills;

    FHeroTypeDefaults, FHeroMapDefaults: IHeroInfo;

    function GetDefaultBiography: TLocalizedString;
    function GetDefaultExperience: UInt64;
    function GetDefaultName: TLocalizedString;
    function GetDefaultSex: THeroSex;
    function GetDefaultPortrait: AnsiString;
    function GetDefaultSkills: THeroPrimarySkills;

    procedure UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString);


    procedure StashSkills;
    procedure LoadSkills;
    procedure ResetSkills;

    procedure Load; virtual;
  protected
    procedure ApplyDefaults; override;
    procedure ReloadDefaults; override;
    procedure UpdateControls(); override;


  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;

    procedure VisitNormalHero(AOptions: TNormalHeroOptions); override;
    procedure VisitRandomHero(AOptions: TRandomHeroOptions); override;
    procedure VisitPrison(AOptions: TPrisonOptions); override;

    procedure ReadHero(AOptions: THeroOptions); virtual;

    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

{$R *.lfm}

{ THeroFrame }

procedure THeroFrame.edNameEditingDone(Sender: TObject);
begin
  FCustomName := edName.Text;
end;

procedure THeroFrame.edPatrolKeyPress(Sender: TObject; var Key: char);
begin
  if not (key in DigitChars +[#8]) then
  begin
    Key:=#0;
  end;
end;

procedure THeroFrame.edSexChange(Sender: TObject);
begin
  case edSex.ItemIndex of
    0: FCustomFemale:=false;
    1: FCustomFemale:=true;
  end;
end;

procedure THeroFrame.Load;
begin
  cbBiography.Checked:=FOptions.GetBiography <> '';
  if cbBiography.Checked then
    FCustomBiography := FOptions.GetBiography()
  else
    FCustomBiography := GetDefaultBiography();
  cbBiographyChange(cbBiography);

  cbExperience.Checked := FOptions.GetExperience() <> 0;
  if cbExperience.Checked then
    FCustomExperience := FOptions.GetExperience()
  else
    FCustomExperience := GetDefaultExperience();
  cbExperienceChange(cbExperience);

  cbName.Checked:=FOptions.GetName <> '';
  if cbName.Checked then
    FCustomName := FOptions.GetName()
  else
    FCustomName:=GetDefaultName();
  cbNameChange(cbName);

  cbPortrait.Checked:=FOptions.GetPortrait <> '';
  if cbPortrait.Checked then
    FCustomPortrait := FOptions.GetPortrait()
  else
    FCustomName:=GetDefaultPortrait();
  cbPortraitChange(cbPortrait);

  cbSkills.Checked:=not FOptions.GetPrimarySkills.IsDefault;
  if cbSkills.Checked then
  begin
    FCustomSkills.Assign(FOptions.GetPrimarySkills());
  end
  else
  begin
    FCustomSkills.Assign(GetDefaultSkills);
  end;
  cbSkillsChange(cbSkills);


  cbSex.Checked:= FOptions.GetSex <> THeroSex.default;
  if cbSex.Checked then
    FCustomFemale := FOptions.GetSex = THeroSex.female
  else
    FCustomFemale := GetDefaultSex = THeroSex.female;
  cbSexChange(cbSex);

end;

procedure THeroFrame.Save;
begin
  if cbBiography.Checked then
  begin
    FOptions.SetBiography(edBiography.Text);
  end
  else
  begin
    FOptions.SetBiography('');
  end;

  if cbExperience.Checked then
  begin
    FOptions.SetExperience(StrToInt64Def(edExperience.Text, 0));
  end
  else
  begin
    FOptions.SetExperience(0);
  end;

  if cbName.Checked then
  begin
    FOptions.SetName(edName.Text);
  end
  else begin
    FOptions.SetName('');
  end;

  if cbSex.Checked then
  begin
    FOptions.SetSex(THeroSex(Byte(edSex.ItemIndex)));
  end
  else begin
    FOptions.SetSex(THeroSex.default);
  end;

  if cbSkills.Checked then
  begin
    StashSkills;
    FOptions.GetPrimarySkills.Assign(FCustomSkills);
  end
  else
  begin
    FOptions.GetPrimarySkills.Clear;
  end;
end;

procedure THeroFrame.LoadAvilableFor;
var
  p: TPlayerColor;
begin
  for p in TPlayerColor do
  begin
    AvailableFor.Checked[Integer(p)] := FHeroDefinition.AvailableFor * [p] <> [];
  end;
end;

procedure THeroFrame.SaveAvilableFor;
var
  p: TPlayerColor;
  available_for: TPlayers;
begin
  available_for := [];
  for p in TPlayerColor do
  begin
    if AvailableFor.Checked[Integer(p)] then
      Include(available_for, p);
  end;
  FHeroDefinition.AvailableFor := available_for;
end;

procedure THeroFrame.CommitHeroOptions;
begin
  if edType.Visible then
  begin
    FHeroOptions.&type := edType.SelectedIdentifier();
  end;

  if edOwner.Visible then
  begin
    FHeroOptions.Owner := TPlayer(edOwner.ItemIndex);
  end;

  if edPatrol.ItemIndex >=0 then
  begin
    FHeroOptions.PatrolRadius := edPatrol.ItemIndex-1;
  end
  else
  begin
    FHeroOptions.PatrolRadius := StrToIntDef(edPatrol.Text, -1);
  end;
end;

procedure THeroFrame.CommitHeroDefinition;
begin
  SaveAvilableFor;
end;

function THeroFrame.GetDefaultBiography: TLocalizedString;
begin
  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetBiography <> '') then
     FHeroMapDefaults.GetBiography()
  else if Assigned(FHeroTypeDefaults) then
    Result := FHeroTypeDefaults.GetBiography()
  else
    Result := '';
end;

function THeroFrame.GetDefaultExperience: UInt64;
begin
  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetExperience > 0) then
     FHeroMapDefaults.GetExperience()
  else if Assigned(FHeroTypeDefaults) then
    Result := FHeroTypeDefaults.GetExperience()
  else
    Result := 0;
end;

function THeroFrame.GetDefaultName: TLocalizedString;
begin
  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetName <> '') then
     FHeroMapDefaults.GetName()
  else if Assigned(FHeroTypeDefaults) then
    Result := FHeroTypeDefaults.GetName()
  else
    Result := '';
end;

function THeroFrame.GetDefaultSex: THeroSex;
begin
  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetSex() <> THeroSex.default) then
     FHeroMapDefaults.GetSex()
  else if Assigned(FHeroTypeDefaults) then
    Result := FHeroTypeDefaults.GetSex()
  else
    Result := THeroSex.male;
end;

function THeroFrame.GetDefaultPortrait: AnsiString;
begin
  if Assigned(FHeroMapDefaults) and (FHeroMapDefaults.GetPortrait() <> '') then
     FHeroMapDefaults.GetPortrait()
  else if Assigned(FHeroTypeDefaults) then
    Result := FHeroTypeDefaults.GetPortrait()
  else
    Result := FOptions.GetHeroIdentifier();
end;

function THeroFrame.GetDefaultSkills: THeroPrimarySkills;
begin
  Result := nil;//todo: maybe static object?
  if Assigned(FHeroMapDefaults) and (not FHeroMapDefaults.GetPrimarySkills.IsDefault) then
  begin
    Result := FHeroMapDefaults.GetPrimarySkills;
  end
  else if Assigned(FHeroTypeDefaults) then
  begin
    Result := FHeroTypeDefaults.GetPrimarySkills;
  end;
end;

procedure THeroFrame.CustomiseChange(Sender: TObject);
begin
  UpdateControls();
end;

procedure THeroFrame.edExperienceEditingDone(Sender: TObject);
begin
  FCustomExperience := StrToInt64Def(edExperience.Text, 0);
end;

procedure THeroFrame.cbPortraitChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  //todo: THeroFrame.cbPortraitChange
//  UpdateText(edPortrait, cbPortrait, FCustomPortrait, GetDefaultPortrait);
end;

procedure THeroFrame.cbSexChange(Sender: TObject);
begin
  CustomiseChange(Sender);

  if cbSex.Checked then
  begin
    edSex.ItemIndex := Integer(FCustomFemale);
  end
  else
  begin
    edSex.ItemIndex := Integer(GetDefaultSex);
  end;
end;

procedure THeroFrame.cbSkillsChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  if (Sender as TCheckBox).Checked then
  begin
    LoadSkills;
  end
  else
  begin
    StashSkills;
    ResetSkills;
  end;
end;

procedure THeroFrame.cbExperienceChange(Sender: TObject);
begin
  CustomiseChange(Sender);

  if cbExperience.State = cbChecked then
  begin
    if FCustomExperience = 0 then
    begin
      edExperience.Text := IntToStr(GetDefaultExperience);
    end
    else
    begin
      edExperience.Text := IntToStr(FCustomExperience);
    end;
  end
  else
  begin
    edExperience.Text := IntToStr(GetDefaultExperience);
  end;
end;

procedure THeroFrame.cbBiographyChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  UpdateText(edBiography, cbBiography, FCustomBiography,GetDefaultBiography());
end;

procedure THeroFrame.cbNameChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  UpdateText(edName, cbName, FCustomName,GetDefaultName);
end;

procedure THeroFrame.UpdateControls;
begin
  inherited UpdateControls;
  edPortrait.Enabled:=cbPortrait.Checked;
  edExperience.Enabled:=cbExperience.Checked;
  edName.Enabled:=cbName.Checked;
  edSex.Enabled:=cbSex.Checked;
  edBiography.Enabled:=cbBiography.Checked;
  pnSkills.Enabled := cbSkills.Checked;
end;

procedure THeroFrame.StashSkills;
begin
  FCustomSkills.Attack := Attack.Value;
  FCustomSkills.Defence := Defence.Value;
  FCustomSkills.Spellpower := SpellPower.Value;
  FCustomSkills.Knowledge := Knowledge.Value;
end;

procedure THeroFrame.LoadSkills;
begin
  Attack.Value     := FCustomSkills.Attack;
  Defence.Value    := FCustomSkills.Defence;
  SpellPower.Value := FCustomSkills.Spellpower;
  Knowledge.Value  := FCustomSkills.Knowledge;
end;

procedure THeroFrame.ResetSkills;
var
  src: THeroPrimarySkills;
begin
  src := GetDefaultSkills();

  Attack.Value     := src.Attack;
  Defence.Value    := src.Defence;
  SpellPower.Value := src.Spellpower;
  Knowledge.Value  := src.Knowledge;
end;

procedure THeroFrame.ApplyDefaults;
begin
  inherited ApplyDefaults;
end;

procedure THeroFrame.ReloadDefaults;
begin
  inherited ReloadDefaults;


end;

procedure THeroFrame.VisitNormalHero(AOptions: TNormalHeroOptions);
begin
  inherited VisitNormalHero(AOptions);
  edHeroClass.Enabled:=False;
  ReadHero(AOptions);
end;

procedure THeroFrame.VisitRandomHero(AOptions: TRandomHeroOptions);
begin
  inherited VisitRandomHero(AOptions);

  lbHeroClass.Visible:=False;
  edHeroClass.Visible:=False;
  Placeholder1.Visible:=False;

  lbType.Visible:=False;
  edType.Visible:=False;
  Placeholder2.Visible:=False;

  ReadHero(AOptions);
end;

procedure THeroFrame.VisitPrison(AOptions: TPrisonOptions);
begin
  inherited VisitPrison(AOptions);

  lbOwner.Visible:=False;
  edOwner.Visible := False;
  Placeholder3.Visible:=False;

  ReadHero(AOptions);
end;

procedure THeroFrame.UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox;
  ACustom: TLocalizedString; ADefault: TLocalizedString);
begin
  CustomiseChange(AFlag);
  DoUpdateText(AControl, AFlag, ACustom, ADefault);
end;

constructor THeroFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ListsManager.FillWithPlayers(edOwner.Items, False);

  FCustomSkills := THeroPrimarySkills.Create;
end;

destructor THeroFrame.Destroy;
begin
  FCustomSkills.Free;
  inherited Destroy;
end;

procedure THeroFrame.Commit;
begin
  inherited Commit;

  Save();

  if Assigned(FHeroOptions) then
  begin
    CommitHeroOptions;
  end
  else
  begin
    Assert(Assigned(FHeroDefinition));

    CommitHeroDefinition;
  end;
end;

procedure THeroFrame.ReadHero(AOptions: THeroOptions);
begin
  FOptions := AOptions;
  FHeroOptions := AOptions;

  AvailableForPlaceholder.Visible:=false;
  AvailableFor.Visible := false;
  AvailableForLabel.Visible:=False;

  InstanceType:=AOptions.&Type;

  Load;

  if edOwner.Visible then
  begin
    edOwner.ItemIndex := Integer(FHeroOptions.Owner);
  end;

  case FHeroOptions.PatrolRadius of
    -1: edPatrol.ItemIndex := 0 ;
    0: edPatrol.ItemIndex := 1;
  else
    begin
      edPatrol.Text:=IntToStr(FHeroOptions.PatrolRadius);
    end;
  end;

end;

procedure THeroFrame.VisitHeroDefinition(AOptions: THeroDefinition);
var
  h_info: THeroInfo;
  c_info: THeroClassInfo;
begin
  FOptions := AOptions;
  FHeroDefinition := AOptions;

  lbOwner.Visible:=false;
  edOwner.Visible:=false;
  Placeholder3.Visible:=false;

  lbPatrol.Visible:=false;
  edPatrol.Visible:=false;
  Placeholder5.Visible:=false;

  edHeroClass.Enabled := false;
  edType.Enabled:=false;

  InstanceType:=AOptions.Identifier;

  h_info := ListsManager.Heroes[AOptions.Identifier];
  c_info := ListsManager.HeroClasses[h_info.&Class];

  FHeroTypeDefaults := h_info;
  FHeroMapDefaults := nil;

  edHeroClass.FillFromList(ListsManager.HeroClassInfos, c_info.Identifier);
  edType.FillFromList(ListsManager.HeroInfos, h_info.Identifier);

  Load();

  inherited VisitHeroDefinition(AOptions);

  LoadAvilableFor;

  UpdateControls();
end;


end.


