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
  Dialogs, StdCtrls, ComboEx, base_options_frame,
  gui_helpers, object_options, editor_consts, editor_types,
  lists_manager, editor_classes, Map, base_info, LCLType, ExtCtrls, Spin,
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
    TypeHeaderLabel: TLabel;
    TypeHeader: TLabel;
    TypeHeaderPlaceholder: TLabel;
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
    HeroClassPlaceholder: TLabel;
    lbType: TLabel;
    TypePlaceholder: TLabel;
    OwnerPlaceholder: TLabel;
    PatrolPlaceholder: TLabel;
    Attack: TSpinEdit;
    Defence: TSpinEdit;
    SpellPower: TSpinEdit;
    Knowledge: TSpinEdit;
    procedure AttackChange(Sender: TObject);
    procedure cbExperienceChange(Sender: TObject);
    procedure cbPortraitChange(Sender: TObject);
    procedure cbSexChange(Sender: TObject);
    procedure cbSkillsChange(Sender: TObject);
    procedure CustomiseChange(Sender: TObject);
    procedure DefenceChange(Sender: TObject);
    procedure edExperienceEditingDone(Sender: TObject);
    procedure edHeroClassChange(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
    procedure edTypeChange(Sender: TObject);
    procedure KnowledgeChange(Sender: TObject);
    procedure SpellPowerChange(Sender: TObject);
  strict private
    FOptions: IEditableHeroInfo;

    FHeroDefinition: THeroDefinition;

    function OptionsObj: TObject;

    procedure Save;

    procedure LoadAvilableFor;
    procedure SaveAvilableFor;

    procedure CommitHeroOptions;
    procedure CommitHeroDefinition;

    procedure ReadHero(AOptions: THeroOptions);
  protected
    FHeroOptions: THeroOptions;
    FEmptySkills:THeroPrimarySkills;

    FCustomFemale: Boolean;
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

    procedure LoadSkills;
    procedure ResetSkills;

    function GetHeroClass():AnsiString;
    procedure Load; override;

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

    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

{$R *.lfm}

{ THeroFrame }

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

procedure THeroFrame.edTypeChange(Sender: TObject);
var
  editor: TCustomComboBox;
  info: TBaseInfo;
  definition: THeroDefinition;
begin
  editor := Sender as TCustomComboBox;

  if editor.Visible and editor.Enabled then
  begin
    FHeroTypeDefaults := nil;

    info := editor.SelectedInfo;
    definition := nil;
    if Assigned(info) then
    begin
      FHeroTypeDefaults := info as THeroInfo;

      definition := Map.PredefinedHeroes.FindItem(info.Identifier);

      NotifyInstanceTypeChange(info.Identifier);
    end;

    FHeroMapDefaults := definition;
  end
  else
  begin
    NotifyInstanceTypeChange('');
  end;

  cbPortraitChange(cbPortrait);
  cbExperienceChange(cbExperience);
  cbSexChange(cbSex);
  cbSkillsChange(cbSkills);
end;

procedure THeroFrame.KnowledgeChange(Sender: TObject);
begin
  if cbSkills.Checked then
  begin
    FCustomSkills.Knowledge:=(Sender as TSpinEdit).Value;
  end;
end;

procedure THeroFrame.SpellPowerChange(Sender: TObject);
begin
  if cbSkills.Checked then
  begin
    FCustomSkills.Spellpower:=(Sender as TSpinEdit).Value;
  end;
end;

function THeroFrame.OptionsObj: TObject;
begin
  if Assigned(FHeroOptions) then
  begin
    Result := FHeroOptions;
  end
  else
  begin
    Assert(Assigned(FHeroDefinition));
    Result := FHeroDefinition;
  end;
end;

procedure THeroFrame.Load;
begin
  AddStrEditor(OptionsObj(), 'Biography', edBiography, cbBiography, @GetDefaultBiography);
  AddStrEditor(OptionsObj(), 'Name', edName, cbName, @GetDefaultName);

  inherited Load;

  cbExperience.Checked := FOptions.GetExperience() <> 0;
  if cbExperience.Checked then
    FCustomExperience := FOptions.GetExperience()
  else
    FCustomExperience := GetDefaultExperience();
  cbExperienceChange(cbExperience);

  cbPortrait.Checked:=FOptions.GetPortrait <> '';
  if cbPortrait.Checked then
    FCustomPortrait := FOptions.GetPortrait()
  else
    FCustomPortrait:=GetDefaultPortrait();
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

  UpdateControls();
end;

procedure THeroFrame.Save;
begin
  if cbExperience.Checked then
  begin
    FOptions.SetExperience(StrToInt64Def(edExperience.Text, 0));
  end
  else
  begin
    FOptions.SetExperience(0);
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
    Result := FHeroMapDefaults.GetExperience()
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
    Result := FHeroMapDefaults.GetSex()
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
  Result := FEmptySkills;
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

procedure THeroFrame.DefenceChange(Sender: TObject);
begin
  if cbSkills.Checked then
  begin
    FCustomSkills.Defence:=(Sender as TSpinEdit).Value;
  end;
end;

procedure THeroFrame.edExperienceEditingDone(Sender: TObject);
begin
  FCustomExperience := StrToInt64Def(edExperience.Text, 0);
end;

procedure THeroFrame.edHeroClassChange(Sender: TObject);
var
  class_info: THeroClassInfo;

  editor: TCustomComboBox;
  idx: Integer;
  hero_type: String;
begin
  if edType.Visible and Assigned(FHeroOptions) then
  begin
    //fill hero types
    editor := Sender as TCustomComboBox;

    if editor.ItemIndex < 0 then
    begin
      edType.ItemIndex := -1;
      edType.Items.Clear;
      edType.Enabled:=false;
    end
    else
    begin
      edType.Enabled:=true;
      edType.ItemIndex := -1;

      class_info := editor.Items.Objects[editor.ItemIndex] as THeroClassInfo;

      ListsManager.FillWithHeroesOfClass(edType.Items, class_info.Identifier);

      hero_type :=  FHeroOptions.&type;

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
  end;

  edTypeChange(edType);
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

procedure THeroFrame.AttackChange(Sender: TObject);
begin
  if cbSkills.Checked then
  begin
    FCustomSkills.Attack:=(Sender as TSpinEdit).Value;
  end;
end;

procedure THeroFrame.UpdateControls;
begin
  inherited UpdateControls;
  edPortrait.Enabled:=cbPortrait.Checked;
  edExperience.Enabled:=cbExperience.Checked;
  edSex.Enabled:=cbSex.Checked;
  pnSkills.Enabled := cbSkills.Checked;
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

function THeroFrame.GetHeroClass: AnsiString;
begin
  if not Assigned(FHeroOptions) then
  begin
    exit('');//just to safe
  end;

  if FHeroOptions.&type = '' then
  begin

    if FHeroOptions.MapObject.GetID = TYPE_HERO then
    begin
      Exit(FHeroOptions.MapObject.GetSubId());
    end;
    exit('');
  end;

  Result := ListsManager.Heroes[FHeroOptions.&type].&Class;
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
  HeroClassPlaceholder.Visible:=False;

  lbType.Visible:=False;
  edType.Visible:=False;
  TypePlaceholder.Visible:=False;

  ReadHero(AOptions);
end;

procedure THeroFrame.VisitPrison(AOptions: TPrisonOptions);
begin
  inherited VisitPrison(AOptions);

  lbOwner.Visible:=False;
  edOwner.Visible := False;
  OwnerPlaceholder.Visible:=False;

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
  FEmptySkills := THeroPrimarySkills.Create;
  FEmptySkills.SetZero;
  inherited Create(TheOwner);

  ListsManager.FillWithPlayers(edOwner.Items, False);

  FCustomSkills := THeroPrimarySkills.Create;
end;

destructor THeroFrame.Destroy;
begin
  FEmptySkills.Free;
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

  TypeHeaderLabel.Visible:=false;
  TypeHeader.Visible:=False;
  TypeHeaderPlaceholder.Visible:=False;

  AvailableForPlaceholder.Visible:=false;
  AvailableFor.Visible := false;
  AvailableForLabel.Visible:=False;

  edHeroClass.ItemIndex := Map.FillWithAvilableClasses(edHeroClass.Items, GetHeroClass());

  edHeroClassChange(edHeroClass); //also loads type

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

  TypeHeaderLabel.Visible:=True;
  TypeHeader.Visible:=True;
  TypeHeaderPlaceholder.Visible:=True;

  lbOwner.Visible:=false;
  edOwner.Visible:=false;
  OwnerPlaceholder.Visible:=false;

  lbPatrol.Visible:=false;
  edPatrol.Visible:=false;
  PatrolPlaceholder.Visible:=false;

  lbHeroClass.Visible:=False;
  edHeroClass.Visible:=False;
  HeroClassPlaceholder.Visible:=False;

  lbType.Visible:=False;
  edType.Visible:=False;
  TypePlaceholder.Visible:=False;

  NotifyInstanceTypeChange(AOptions.Identifier);

  h_info := ListsManager.Heroes[AOptions.Identifier];
  c_info := ListsManager.HeroClasses[h_info.&Class];

  FHeroTypeDefaults := h_info;
  FHeroMapDefaults := nil;

  TypeHeader.Caption:=Format('%s %s',[c_info.Name, h_info.Name]);

  Load();

  inherited VisitHeroDefinition(AOptions);

  LoadAvilableFor;

  UpdateControls();
end;


end.


