{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit quest_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, typinfo, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls,
  CheckLst, Menus, base_options_frame, gui_helpers, creature_set_frame, object_link_frame, object_options,
  field_editors, editor_types;

type

  { TQuestFrame }

  TQuestFrame = class(TBaseOptionsFrame)
    cbFirstVisitText: TCheckBox;
    cbNextVisitText: TCheckBox;
    cbCompletedText: TCheckBox;
    cbTimeLimit: TCheckBox;
    Artifacts: TCheckListBox;
    Attack: TSpinEdit;
    lbLevel: TLabel;
    Hero: TListBox;
    Player: TComboBox;
    edCrystal: TSpinEdit;
    Defence: TSpinEdit;
    edGems: TSpinEdit;
    edGold: TSpinEdit;
    Knowledge: TSpinEdit;
    edMercury: TSpinEdit;
    edMithril: TSpinEdit;
    edOre: TSpinEdit;
    lbPlayer: TLabel;
    SpellPower: TSpinEdit;
    edSulfur: TSpinEdit;
    edWood: TSpinEdit;
    lbAttack: TLabel;
    lbDefence: TLabel;
    lbSpellPower: TLabel;
    lbKnowledge: TLabel;
    lbCrystal: TLabel;
    lbGems: TLabel;
    lbGold: TLabel;
    lbMercury: TLabel;
    lbMithril: TLabel;
    lbOre: TLabel;
    lbSulfur: TLabel;
    lbWood: TLabel;
    MissionTypeLabel: TLabel;
    MissionType: TComboBox;
    FirstVisitText: TMemo;
    NextVisitText: TMemo;
    CompletedText: TMemo;
    Missions: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Level: TSpinEdit;
    tsHero: TTabSheet;
    tsDefault: TTabSheet;
    tsKillCreature: TTabSheet;
    tsKillHero: TTabSheet;
    tsResources: TTabSheet;
    tsCreatures: TTabSheet;
    tsArtifacts: TTabSheet;
    TimeLimit: TSpinEdit;
    TopPanel: TPanel;
    TextPanel: TPanel;
    procedure MissionTypeChange(Sender: TObject);
  private
    FCreaturesEdit: TCreatureSetFrame;
    FKillHeroEdit: TObjectLinkFrame;
    FKillCreatureEdit: TObjectLinkFrame;
    FObject: TQuest;
    FResEditors: TFieldEditors;
    FPrimaryStatsEditors: TFieldEditors;
    procedure SetupMissionTypes;
  protected
    procedure Load; override;
    procedure UpdateControls; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;

    procedure VisitQuestGuard(AOptions: TQuestGuardOptions); override;
    procedure VisitSeerHut(AOptions: TSeerHutOptions); override;
  end;

implementation

{$R *.lfm}

{ TQuestFrame }

procedure TQuestFrame.Commit;
begin
  FObject.Clear;
  inherited Commit;//will save with field editors
  FObject.MissionType:=TQuestMission(MissionType.ItemIndex);

  case FObject.MissionType of
  TQuestMission.None: ;
  TQuestMission.Level:
    FObject.HeroLevel := Level.Value;
  TQuestMission.PrimaryStat:
    FPrimaryStatsEditors.Commit;
  TQuestMission.KillHero:
    FObject.KillTarget:=FKillHeroEdit.SelectedIdentifier;
  TQuestMission.KillCreature:
    FObject.KillTarget:=FKillCreatureEdit.SelectedIdentifier;
  TQuestMission.Artifact:
    Artifacts.SaveTo(FObject.Artifacts);
  TQuestMission.Army:
    FCreaturesEdit.Commit;
  TQuestMission.Resources:
    FResEditors.Commit;
  TQuestMission.Hero:
    FObject.Hero := Hero.SelectedIdentifier;
  TQuestMission.Player:
    FObject.Player := TPlayer(Player.ItemIndex);
  end;
end;

procedure TQuestFrame.MissionTypeChange(Sender: TObject);

  procedure OpenPage(APage: TTabSheet);
  begin
    APage.PageControl.ActivePage := APage;
    APage.TabVisible := APage <> tsDefault;
  end;

var
  tp: TQuestMission;
  flag: Boolean;
begin
  Missions.HideAllTabs;
  tp := TQuestMission(MissionType.ItemIndex);

  case tp of
  TQuestMission.None: OpenPage(tsDefault) ;
  TQuestMission.Level: OpenPage(tsDefault);
  TQuestMission.PrimaryStat: OpenPage(tsDefault);
  TQuestMission.KillHero: OpenPage(tsKillHero);
  TQuestMission.KillCreature: OpenPage(tsKillCreature);
  TQuestMission.Artifact: OpenPage(tsArtifacts);
  TQuestMission.Army: OpenPage(tsCreatures);
  TQuestMission.Resources: OpenPage(tsResources);
  TQuestMission.Hero: OpenPage(tsHero);
  TQuestMission.Player: OpenPage(tsDefault);
  end;

  flag := tp = TQuestMission.PrimaryStat;

  lbAttack.Enabled := flag;
  Attack.Enabled := flag;
  lbDefence.Enabled := flag;
  Defence.Enabled := flag;
  lbSpellPower.Enabled := flag;
  SpellPower.Enabled := flag;
  lbKnowledge.Enabled := flag;
  Knowledge.Enabled := flag;

  flag := tp = TQuestMission.Player;

  lbPlayer.Enabled := flag;
  Player.Enabled := flag;

  if flag and (Player.ItemIndex < 0) then
    Player.ItemIndex := 0;

  flag := tp = TQuestMission.Level;

  lbLevel.Enabled := flag;
  Level.Enabled := flag;

  flag := tp = TQuestMission.Hero;

  if flag and (Hero.ItemIndex < 0) then
  begin
    Hero.ItemIndex := 0;
  end;

  UpdateControls;
end;

procedure TQuestFrame.SetupMissionTypes;
var
  tp: TQuestMission;
begin
  for tp in TQuestMission do
  begin
    MissionType.AddItem(GetEnumName(TypeInfo(TQuestMission), Integer(tp)), TObject(PtrInt(tp)));
  end;
end;

procedure TQuestFrame.Load;
begin
  FKillCreatureEdit.Map := Map;
  FCreaturesEdit.Map := Map;
  FKillHeroEdit.Map := Map;

  AddStrEditor(FObject, 'FirstVisitText', FirstVisitText, cbFirstVisitText);
  AddStrEditor(FObject, 'NextVisitText', NextVisitText, cbNextVisitText);
  AddStrEditor(FObject, 'CompletedText', CompletedText, cbCompletedText);
  AddIntEditor(FObject, 'TimeLimit', TimeLimit, cbTimeLimit);

  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Wood', edWood));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Mercury', edMercury));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Ore', edOre));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Sulfur', edSulfur));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Crystal', edCrystal));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Gems', edGems));
  FResEditors.Add(TIntEditor.Create(FObject.Resources, 'Gold', edGold));

  FPrimaryStatsEditors.Add(TIntEditor.Create(FObject.PrimarySkills, 'Attack', Attack));
  FPrimaryStatsEditors.Add(TIntEditor.Create(FObject.PrimarySkills, 'Defence', Defence));
  FPrimaryStatsEditors.Add(TIntEditor.Create(FObject.PrimarySkills, 'Spellpower', SpellPower));
  FPrimaryStatsEditors.Add(TIntEditor.Create(FObject.PrimarySkills, 'Knowledge', Knowledge));

  inherited Load;//do after editors setup; will load to field editors

  MissionType.ItemIndex := Integer(FObject.MissionType);

  FKillHeroEdit.Load(FObject.KillTarget);
  FKillCreatureEdit.Load(FObject.KillTarget);
  FCreaturesEdit.VisitQuest(FObject);

  case FObject.MissionType of
  TQuestMission.None: ;
  TQuestMission.Level:
    Level.Value:=FObject.HeroLevel;
  TQuestMission.PrimaryStat:
    FPrimaryStatsEditors.Load;
  TQuestMission.KillHero:;
  TQuestMission.KillCreature:;
  TQuestMission.Artifact:
    Artifacts.LoadItems(FObject.Artifacts);
  TQuestMission.Army:;
  TQuestMission.Resources:
    FResEditors.Load;
  TQuestMission.Hero:
    Hero.Select(ListsManager.HeroInfos, FObject.Hero);
  TQuestMission.Player:
    Player.ItemIndex := Integer(FObject.Player);
  end;

  MissionTypeChange(nil);

  UpdateControls;
end;

procedure TQuestFrame.UpdateControls;
begin
  inherited UpdateControls;
end;

constructor TQuestFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MissionType.Clear;
  SetupMissionTypes();
  Missions.HideAllTabs;
  Missions.ActivePage := tsDefault;

  Artifacts.FillItems(ListsManager.ArtifactInfos);

  Hero.FillFromList(ListsManager.HeroInfos, '');

  FCreaturesEdit := TCreatureSetFrame.Create(Self);

  FCreaturesEdit.Parent := tsCreatures;
  FCreaturesEdit.Align := alClient;

  FResEditors := TFieldEditors.Create;

  FKillCreatureEdit := TObjectLinkFrame.Create(tsKillCreature);

  FKillCreatureEdit.TypeFilter := TCreatureOptions;
  FKillCreatureEdit.Parent := tsKillCreature;
  FKillCreatureEdit.Align:=alClient;

  FKillHeroEdit := TObjectLinkFrame.Create(tsKillHero);
  FKillHeroEdit.TypeFilter := THeroOptions;
  FKillHeroEdit.Parent := tsKillHero;
  FKillHeroEdit.Align := alClient;

  FPrimaryStatsEditors := TFieldEditors.Create;

  ListsManager.FillWithPlayers(Player.Items, False);
end;

destructor TQuestFrame.Destroy;
begin
  FPrimaryStatsEditors.Free;
  FResEditors.Free;
  inherited Destroy;
end;

procedure TQuestFrame.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  inherited VisitQuestGuard(AOptions);
  FObject := AOptions.Quest;
  Load;
end;

procedure TQuestFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  inherited VisitSeerHut(AOptions);
  FObject := AOptions.Quest;
  Load;
end;

end.

