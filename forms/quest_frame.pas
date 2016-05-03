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
  CheckLst, base_options_frame, gui_helpers, object_options;

type

  { TQuestFrame }

  TQuestFrame = class(TBaseOptionsFrame)
    cbFirstVisitText: TCheckBox;
    cbNextVisitText: TCheckBox;
    cbCompletedText: TCheckBox;
    cbTimeLimit: TCheckBox;
    Artifacts: TCheckListBox;
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
    tsArtifacts: TTabSheet;
    TimeLimit: TSpinEdit;
    TopPanel: TPanel;
    TextPanel: TPanel;
    procedure cbCompletedTextChange(Sender: TObject);
    procedure cbFirstVisitTextChange(Sender: TObject);
    procedure cbNextVisitTextChange(Sender: TObject);
    procedure cbTimeLimitChange(Sender: TObject);
    procedure MissionTypeChange(Sender: TObject);
  private
    FObject: TQuest;
    procedure SetupMissionTypes;
  protected
    procedure Load; override;
    procedure UpdateControls; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;

    procedure VisitQuestGuard(AOptions: TQuestGuardOptions); override;
    procedure VisitSeerHut(AOptions: TSeerHutOptions); override;
  end;

implementation

{$R *.lfm}

{ TQuestFrame }

procedure TQuestFrame.Commit;
begin
  inherited Commit;
  FObject.MissionType:=TQuestMission(MissionType.ItemIndex);

  if cbTimeLimit.Checked then
  begin
    FObject.TimeLimit:=TimeLimit.Value;
  end
  else
  begin
    FObject.TimeLimit := -1
  end;
end;

procedure TQuestFrame.cbTimeLimitChange(Sender: TObject);
begin
  UpdateControls;
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
  TQuestMission.KillHero: ;
  TQuestMission.KillCreature: ;
  TQuestMission.Artifact: OpenPage(tsArtifacts) ;
  TQuestMission.Army: ;
  TQuestMission.Resources: ;
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
  MissionType.ItemIndex := Integer(FObject.MissionType);
  MissionTypeChange(nil);

  cbTimeLimit.Checked := FObject.TimeLimit >= 0;
  if cbTimeLimit.Checked then
  begin
    TimeLimit.Value := FObject.TimeLimit;
  end
  else
  begin
    TimeLimit.Value := 0;
  end;

  case FObject.MissionType of
  TQuestMission.None: ;
  TQuestMission.Level: ;
  TQuestMission.PrimaryStat: ;
  TQuestMission.KillHero: ;
  TQuestMission.KillCreature: ;
  TQuestMission.Artifact: Artifacts.LoadItems(FObject.Artifacts);
  TQuestMission.Army: ;
  TQuestMission.Resources: ;
  TQuestMission.Hero: ;
  TQuestMission.Player: ;
  end;

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

