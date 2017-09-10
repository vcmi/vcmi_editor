{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit edit_object_options;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ActnList, object_options, map,
  editor_str_consts, base_options_frame, creature_frame, abandoned_frame, scholar_frame, creature_set_frame,
  resource_frame, local_event_frame, hero_artifacts_frame, message_frame,
  hero_spells_frame, hero_skills_frame, town_frame, town_buildings_frame, town_spells_frame, quest_frame, reward_frame,
  gui_helpers, hero_frame;

type

  { TEditObjectOptions }

  TEditObjectOptions = class(TForm, IObjectOptionsVisitor)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btOk: TButton;
    btCancel: TButton;
    ErrorLabel: TLabel;
    lbNothingToEdit: TLabel;
    pcMain: TPageControl;
    tsCommon: TTabSheet;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
  strict private
    FMap: TVCMIMap;

    FActiveEditors: TBaseOptionsFrameList;

    procedure SaveChanges;

    procedure VisitDwelling(AOptions: TObjectOptions);
    procedure VisitGuardedObject(AOptions: TObjectOptions);
    procedure VisitArmedObject(AOptions: TObjectOptions);

    procedure VisitHero(AOptions: THeroOptions);//+
  public//IObjectOptionsVisitor
    procedure VisitLocalEvent(AOptions: TLocalEventOptions);//+
    procedure VisitSignBottle(AOptions: TSignBottleOptions); //+

    procedure VisitNormalHero(AOptions: TNormalHeroOptions);//+
    procedure VisitRandomHero(AOptions: TRandomHeroOptions);//+
    procedure VisitPrison(AOptions: TPrisonOptions);//+

    procedure VisitMonster(AOptions: TCreatureOptions);//+
    procedure VisitSeerHut(AOptions: TSeerHutOptions);//+
    procedure VisitWitchHut(AOptions: TWitchHutOptions);//+
    procedure VisitScholar(AOptions: TScholarOptions);//+
    procedure VisitGarrison(AOptions: TGarrisonOptions);//+
    procedure VisitArtifact(AOptions: TArtifactOptions);//+
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions);//+
    procedure VisitResource(AOptions: TResourceOptions);//+
    procedure VisitTown(AOptions: TTownOptions);
    procedure VisitAbandonedMine(AOptions: TAbandonedOptions);//+
    procedure VisitMine(AOptions: TMineOptions);//+
    procedure VisitShrine(AOptions: TShrineOptions);//+
    procedure VisitPandorasBox(AOptions: TPandorasOptions); //+
    procedure VisitGrail(AOptions: TGrailOptions); //+
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions);//+
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);//+
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions);//+
    procedure VisitQuestGuard(AOptions:TQuestGuardOptions);//+
    procedure VisitHeroPlaceholder(AOptions: THeroPlaceholderOptions);

    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions);//+

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function EditObject(Obj: TMapObject): TModalResult;
  end;


implementation

uses
  grail_frame, flaggable_object_frame, witchhut_frame,
  shrine_frame, spellscroll_frame, dwelling_frame, root_manager;

{$R *.lfm}

{ TEditObjectOptions }

procedure TEditObjectOptions.actSaveExecute(Sender: TObject);
begin
  try
    ErrorLabel.Caption := '';
    ErrorLabel.Visible:=False;
    SaveChanges;
    ModalResult:=mrOK;
  except
    on e: Exception do
    begin
      ErrorLabel.Caption := e.Message;
      ErrorLabel.Visible:=True;
    end;
  end;
end;

procedure TEditObjectOptions.actSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=FActiveEditors.IsDirty;
end;

procedure TEditObjectOptions.actDontSaveExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

constructor TEditObjectOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActiveEditors := TBaseOptionsFrameList.Create(Self);
end;

destructor TEditObjectOptions.Destroy;
begin
  FActiveEditors.Free;
  inherited Destroy;
end;

function TEditObjectOptions.EditObject(Obj: TMapObject): TModalResult;
begin
  FActiveEditors.Clear;
  FMap := Obj.GetMap;

  FActiveEditors.Map:=FMap;
  FActiveEditors.ListsManager := RootManager.ListsManager;

  Obj.Options.ApplyVisitor(Self);

  if pcMain.PageCount <= 1 then
  begin
    tsCommon.TabVisible := true;
    pcMain.ActivePage := tsCommon;
  end
  else
  begin
    pcMain.ActivePage := pcMain.Pages[1];
  end;

  Caption:=Obj.DisplayName;
  Result := ShowModal;
end;

procedure TEditObjectOptions.SaveChanges;
begin
  FActiveEditors.Commit;
end;

procedure TEditObjectOptions.VisitDwelling(AOptions: TObjectOptions);
begin
  FActiveEditors.AddFrame(TFlaggableFrame, AOptions, rsTabMainOptions, pcMain);
  FActiveEditors.AddFrame(TDwellingFrame, AOptions, rsTabSpecificOptions, pcMain);
end;

procedure TEditObjectOptions.VisitGuardedObject(AOptions: TObjectOptions);
begin
  FActiveEditors.AddFrame(TCreatureSetFrame, AOptions, rsGuards, pcMain);
end;

procedure TEditObjectOptions.VisitArmedObject(AOptions: TObjectOptions);
begin
  FActiveEditors.AddFrame(TCreatureSetFrame, AOptions, rsArmy, pcMain);
end;

procedure TEditObjectOptions.VisitAbandonedMine(AOptions: TAbandonedOptions);
begin
  FActiveEditors.AddFrame(TAbandonedFrame, AOptions, rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitMine(AOptions: TMineOptions);
begin
  VisitOwnedObject(AOptions);
  VisitArmedObject(AOptions);
end;

procedure TEditObjectOptions.VisitArtifact(AOptions: TArtifactOptions);
begin
  FActiveEditors.AddFrame(TMessageFrame, AOptions, rsTabMainOptions, pcMain);
  VisitGuardedObject(AOptions);
end;

procedure TEditObjectOptions.VisitGarrison(AOptions: TGarrisonOptions);
begin
  VisitOwnedObject(AOptions);
  VisitArmedObject(AOptions);
end;

procedure TEditObjectOptions.VisitGrail(AOptions: TGrailOptions);
begin
  FActiveEditors.AddFrame(TGrailFrame,AOptions,rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitHero(AOptions: THeroOptions);
begin
  FActiveEditors.AddFrame(THeroFrame, AOptions, rsTabMainOptions, pcMain);
  FActiveEditors.AddFrame(THeroSkillsFrame, AOptions, rsTabSecondarySkills, pcMain);
  VisitArmedObject(AOptions);
  FActiveEditors.AddFrame(THeroArtifactsFrame, AOptions, rsTabArtifacts, pcMain);
  FActiveEditors.AddFrame(THeroSpellsFrame, AOptions,rsTabSpells, pcMain);
end;

procedure TEditObjectOptions.VisitHeroPlaceholder(AOptions: THeroPlaceholderOptions);
begin
  //TODO: TEditObjectOptions.VisitHeroPlaceholder
end;

procedure TEditObjectOptions.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  FActiveEditors.AddFrame(TLocalEventFrame, AOptions, rsTabMainOptions, pcMain);
  FActiveEditors.AddFrame(TRewardFrame, AOptions, rsTabRewards, pcMain);
  VisitGuardedObject(AOptions);
end;

procedure TEditObjectOptions.VisitMonster(AOptions: TCreatureOptions);
begin
  FActiveEditors.AddFrame(TCreatureFrame, AOptions, rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  FActiveEditors.AddFrame(TFlaggableFrame, AOptions, rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  FActiveEditors.AddFrame(TMessageFrame,AOptions, rsTabMainOptions, pcMain);
  FActiveEditors.AddFrame(TRewardFrame, AOptions, rsTabRewards, pcMain);
  VisitGuardedObject(AOptions);
end;

procedure TEditObjectOptions.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  FActiveEditors.AddFrame(TQuestFrame, AOptions, rsTabQuest, pcMain);
end;

procedure TEditObjectOptions.VisitRandomDwelling(
  AOptions: TRandomDwellingOptions);
begin
  VisitDwelling(AOptions);
end;

procedure TEditObjectOptions.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  VisitDwelling(AOptions);
end;

procedure TEditObjectOptions.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  VisitDwelling(AOptions);
end;

procedure TEditObjectOptions.VisitResource(AOptions: TResourceOptions);
begin
  FActiveEditors.AddFrame(TResourceFrame, AOptions, rsTabMainOptions, pcMain);
  VisitGuardedObject(AOptions);
end;

procedure TEditObjectOptions.VisitScholar(AOptions: TScholarOptions);
begin
  FActiveEditors.AddFrame(TScholarFrame, AOptions, rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  FActiveEditors.AddFrame(TQuestFrame, AOptions, rsTabQuest, pcMain);
  FActiveEditors.AddFrame(TRewardFrame, AOptions, rsTabRewards, pcMain);
end;

procedure TEditObjectOptions.VisitShrine(AOptions: TShrineOptions);
begin
  FActiveEditors.AddFrame(TShrineFrame,AOptions,rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  FActiveEditors.AddFrame(TMessageFrame,AOptions,rsTabMainOptions, pcMain);
end;

procedure TEditObjectOptions.VisitNormalHero(AOptions: TNormalHeroOptions);
begin
  VisitHero(AOptions);
end;

procedure TEditObjectOptions.VisitRandomHero(AOptions: TRandomHeroOptions);
begin
  VisitHero(AOptions);
end;

procedure TEditObjectOptions.VisitPrison(AOptions: TPrisonOptions);
begin
  VisitHero(AOptions);
end;

procedure TEditObjectOptions.VisitSpellScroll(AOptions: TSpellScrollOptions);
begin
  FActiveEditors.AddFrame(TSpellScrollFrame,AOptions,rsTabMainOptions, pcMain);
  VisitGuardedObject(AOptions);
end;

procedure TEditObjectOptions.VisitTown(AOptions: TTownOptions);
begin
  FActiveEditors.AddFrame(TTownOptionsFrame, AOptions, rsTabMainOptions, pcMain);
  VisitArmedObject(AOptions);
  FActiveEditors.AddFrame(TTownBuildingsFrame, AOptions, rsTabBuildings, pcMain);
  FActiveEditors.AddFrame(TTownSpellsFrame, AOptions, rsTabSpells, pcMain);
end;

procedure TEditObjectOptions.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  FActiveEditors.AddFrame(TWitchHutFrame,AOptions,rsTabMainOptions, pcMain);
end;

end.

