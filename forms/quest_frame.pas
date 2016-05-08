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
  field_editors;

type

  { TQuestFrame }

  TQuestFrame = class(TBaseOptionsFrame)
    cbFirstVisitText: TCheckBox;
    cbNextVisitText: TCheckBox;
    cbCompletedText: TCheckBox;
    cbTimeLimit: TCheckBox;
    Artifacts: TCheckListBox;
    edCrystal: TSpinEdit;
    edGems: TSpinEdit;
    edGold: TSpinEdit;
    edMercury: TSpinEdit;
    edMithril: TSpinEdit;
    edOre: TSpinEdit;
    edSulfur: TSpinEdit;
    edWood: TSpinEdit;
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
    NoMission: TTabSheet;
    tsKillCreature: TTabSheet;
    tsKillHero: TTabSheet;
    tsResources: TTabSheet;
    tsCreatures: TTabSheet;
    tsArtifacts: TTabSheet;
    TimeLimit: TSpinEdit;
    TopPanel: TPanel;
    TextPanel: TPanel;
    procedure cbCompletedTextChange(Sender: TObject);
    procedure cbFirstVisitTextChange(Sender: TObject);
    procedure cbNextVisitTextChange(Sender: TObject);
    procedure MissionTypeChange(Sender: TObject);
  private
    FCreaturesEdit: TCreatureSetFrame;
    FKillHeroEdit: TObjectLinkFrame;
    FKillCreatureEdit: TObjectLinkFrame;
    FObject: TQuest;
    FResEditors: TFieldEditors;
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
  TQuestMission.Level: ;
  TQuestMission.PrimaryStat: ;
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
  TQuestMission.Hero: ;
  TQuestMission.Player: ;
  end;
end;

procedure TQuestFrame.MissionTypeChange(Sender: TObject);

  procedure OpenPage(APage: TTabSheet);
  begin
    APage.PageControl.ActivePage := APage;
    APage.TabVisible := true;
  end;

var
  tp: TQuestMission;
begin
  Missions.HideAllTabs;
  tp := TQuestMission(MissionType.ItemIndex);

  case tp of
  TQuestMission.None: OpenPage(NoMission) ;
  TQuestMission.Level: ;
  TQuestMission.PrimaryStat: ;
  TQuestMission.KillHero:OpenPage(tsKillHero) ;
  TQuestMission.KillCreature: OpenPage(tsKillCreature);
  TQuestMission.Artifact: OpenPage(tsArtifacts);
  TQuestMission.Army: OpenPage(tsCreatures);
  TQuestMission.Resources: OpenPage(tsResources);
  TQuestMission.Hero: ;
  TQuestMission.Player: ;
  end;
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

procedure TQuestFrame.cbFirstVisitTextChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQuestFrame.cbCompletedTextChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TQuestFrame.cbNextVisitTextChange(Sender: TObject);
begin
  UpdateControls;
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


  inherited Load;//do after editors setup; will load to field editors

  MissionType.ItemIndex := Integer(FObject.MissionType);

  FKillHeroEdit.Load(FObject.KillTarget);
  FKillCreatureEdit.Load(FObject.KillTarget);

  case FObject.MissionType of
  TQuestMission.None: ;
  TQuestMission.Level: ;
  TQuestMission.PrimaryStat: ;
  TQuestMission.KillHero:;

  TQuestMission.KillCreature:;

  TQuestMission.Artifact:
    Artifacts.LoadItems(FObject.Artifacts);
  TQuestMission.Army:
    FCreaturesEdit.VisitQuest(FObject);
  TQuestMission.Resources:
    FResEditors.Load;
  TQuestMission.Hero: ;
  TQuestMission.Player: ;
  end;

  MissionTypeChange(nil);

  UpdateControls;
end;

procedure TQuestFrame.UpdateControls;
begin
  inherited UpdateControls;
  TimeLimit.Enabled := cbTimeLimit.Checked;
  FirstVisitText.Enabled:=cbFirstVisitText.Checked;
  NextVisitText.Enabled:=cbNextVisitText.Checked;
  CompletedText.Enabled:=cbCompletedText.Checked;
end;

constructor TQuestFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MissionType.Clear;
  SetupMissionTypes();
  Missions.HideAllTabs;
  Missions.ActivePage := NoMission;

  Artifacts.FillItems(ListsManager.ArtifactInfos);

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
end;

destructor TQuestFrame.Destroy;
begin
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

