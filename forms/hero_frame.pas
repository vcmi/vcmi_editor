{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge.net

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
  lists_manager, editor_rtti, editor_classes, LCLType, ExtCtrls, Spin,
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
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
  private

  protected
    FCustomName: TLocalizedString;
    FCustomFemale: Boolean;
    FCustomBiography: TLocalizedString;

    FCustomSkills, FDefaultSkills, FClassSkills, FMapSkills:  THeroPrimarySkills;

    FCurrentHero: IHeroInfo;

    function GetDefaultBiography: TLocalizedString; virtual;
    function GetDefaultName: TLocalizedString; virtual;
    function GetDefaultSex: THeroSex; virtual;

    procedure UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString);
    procedure UpdateControls(); override;

    procedure SaveSkills;
    procedure LoadSkills;
    procedure ResetSkills;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;

    procedure VisitHero(AOptions: THeroOptions); override;
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

function THeroFrame.GetDefaultBiography: TLocalizedString;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetBiography;
  end
  else begin
    Result := '';
  end;
end;

function THeroFrame.GetDefaultName: TLocalizedString;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetName;
  end
  else begin
    Result := '';
  end;
end;

function THeroFrame.GetDefaultSex: THeroSex;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetSex;
  end
  else begin
    Result := THeroSex.default; //???
  end;
end;

procedure THeroFrame.CustomiseChange(Sender: TObject);
begin
  UpdateControls();
end;

procedure THeroFrame.cbPortraitChange(Sender: TObject);
begin
  CustomiseChange(Sender);
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
    SaveSkills;
    ResetSkills;
  end;
end;

procedure THeroFrame.cbExperienceChange(Sender: TObject);
begin
  CustomiseChange(Sender);
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
  edBiography.Enabled:=cbBiography.Checked;
  edSex.Enabled:=cbSex.Checked;
  pnSkills.Enabled := cbSkills.Checked;
end;

procedure THeroFrame.SaveSkills;
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
begin
  Attack.Value     := FDefaultSkills.Attack;
  Defence.Value    := FDefaultSkills.Defence;
  SpellPower.Value := FDefaultSkills.Spellpower;
  Knowledge.Value  := FDefaultSkills.Knowledge;
end;

procedure THeroFrame.UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox;
  ACustom: TLocalizedString; ADefault: TLocalizedString);
begin
  CustomiseChange(AFlag);
  DoUpdateText(AControl, AFlag, ACustom, ADefault);
end;

constructor THeroFrame.Create(TheOwner: TComponent);
var
  p: TPlayer;
begin
  inherited Create(TheOwner);

  edOwner.Items.Clear;
  for p in TPlayerColor do
  begin
    edOwner.Items.Add(ListsManager.PlayerName[p]);
  end;
  FCustomSkills := THeroPrimarySkills.Create;
  FDefaultSkills := THeroPrimarySkills.Create;
  FClassSkills := THeroPrimarySkills.Create;
  FMapSkills := THeroPrimarySkills.Create;
end;

destructor THeroFrame.Destroy;
begin
  FMapSkills.Free;
  FClassSkills.Free;
  FCustomSkills.Free;
  FDefaultSkills.Free;
  inherited Destroy;
end;

procedure THeroFrame.Commit;
begin
  inherited Commit;
  if cbSkills.Checked then
    SaveSkills;
end;

procedure THeroFrame.VisitHero(AOptions: THeroOptions);
begin
  AvailableForPlaceholder.Visible:=false;
  AvailableFor.Visible := false;
  AvailableForLabel.Visible:=False;
  inherited VisitHero(AOptions);
end;


end.


