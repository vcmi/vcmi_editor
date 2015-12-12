{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge,net

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

unit pandoras_reward_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, Grids, CheckLst, base_options_frame,
  gui_helpers, object_options, base_info, editor_classes, editor_types;

type

  { TPandorasRewardFrame }

  TPandorasRewardFrame = class(TBaseOptionsFrame)
    edArtifacts: TCheckListBox;
    edSpells: TCheckListBox;
    edMercury: TSpinEdit;
    edSulfur: TSpinEdit;
    edOre: TSpinEdit;
    edWood: TSpinEdit;
    edCrystal: TSpinEdit;
    edGems: TSpinEdit;
    edGold: TSpinEdit;
    edMithril: TSpinEdit;
    lbSulfur: TLabel;
    lbWood: TLabel;
    lbMercury: TLabel;
    lbOre: TLabel;
    lbCrystal: TLabel;
    lbGems: TLabel;
    lbGold: TLabel;
    lbMithril: TLabel;
    SecondarySkillSelector: TComboBox;
    MasterySelector: TComboBox;
    edAttack: TSpinEdit;
    edKnowledge: TSpinEdit;
    edDefence: TSpinEdit;
    edSpellPower: TSpinEdit;
    lbExperience: TLabel;
    lbMana: TLabel;
    lbMorale: TLabel;
    lbLuck: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    pcMain: TPageControl;
    edExperience: TSpinEdit;
    edMana: TSpinEdit;
    edLuck: TSpinEdit;
    edMorale: TSpinEdit;
    edSecondarySkills: TStringGrid;
    tsArtifacts: TTabSheet;
    tsSpells: TTabSheet;
    tsResources: TTabSheet;
    tsSecondary: TTabSheet;
    tsMain: TTabSheet;
    procedure edSecondarySkillsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edSecondarySkillsResize(Sender: TObject);
    procedure edSecondarySkillsSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
    procedure MasterySelectorCloseUp(Sender: TObject);
    procedure MasterySelectorEditingDone(Sender: TObject);
    procedure SecondarySkillSelectorCloseUp(Sender: TObject);
    procedure SecondarySkillSelectorEditingDone(Sender: TObject);
  private
    FOptions: TPandorasOptions;

    procedure LoadMain(AOptions: TPandorasOptions);
    procedure LoadSecondarySkills(AOptions: THeroSecondarySkills);
    procedure LoadResources(AOptions: TResourceSet);
    procedure Load(AOptions: TPandorasOptions);

    procedure SaveMain(AOptions: TPandorasOptions);
    procedure SaveSecondarySkills(AOptions: THeroSecondarySkills);
    procedure SaveResources(AOptions: TResourceSet);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure VisitLocalEvent(AOptions: TLocalEventOptions); override;
  end;

implementation

{$R *.lfm}

{ TPandorasRewardFrame }

procedure TPandorasRewardFrame.edSecondarySkillsSelectEditor(Sender: TObject;
  aCol, aRow: Integer; var Editor: TWinControl);

var
  grid: TCustomStringGrid;
begin
  grid := Sender as TCustomStringGrid;

  case aCol of
    0:
    begin
      Editor := SecondarySkillSelector;

      SecondarySkillSelector.FillFromList(ListsManager.SkillMap,TBaseInfo(grid.Objects[grid.Col, Grid.Row]))
    end;
    1:
    begin
      Editor := MasterySelector;
      MasterySelector.ItemIndex := Integer(PtrUInt(grid.Objects[grid.Col, Grid.Row]));
    end
  end;

  Editor.BoundsRect := grid.CellRect(aCol,aRow);
end;

procedure TPandorasRewardFrame.MasterySelectorCloseUp(Sender: TObject);
begin
  (Sender as TComboBox).EditingDone;
end;

procedure TPandorasRewardFrame.edSecondarySkillsResize(Sender: TObject);
begin
  HandleStringGridResize(Sender);
end;

procedure TPandorasRewardFrame.edSecondarySkillsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleStringGridKeyDown(Sender, Key, Shift)
end;

procedure TPandorasRewardFrame.MasterySelectorEditingDone(Sender: TObject);
begin
  edSecondarySkills.Cells[edSecondarySkills.Col,edSecondarySkills.Row]:=(Sender as TCustomComboBox).Text;

  edSecondarySkills.Objects[edSecondarySkills.Col,edSecondarySkills.Row] := TObject(PtrUint((Sender as TCustomComboBox).ItemIndex));
end;

procedure TPandorasRewardFrame.SecondarySkillSelectorCloseUp(Sender: TObject);
begin
  (Sender as TComboBox).EditingDone;
end;

procedure TPandorasRewardFrame.SecondarySkillSelectorEditingDone(Sender: TObject
  );
var
  editor : TCustomComboBox;
begin

  editor :=  (Sender as TCustomComboBox);

  edSecondarySkills.Cells[edSecondarySkills.Col,edSecondarySkills.Row]:=editor.Text;

  if editor.ItemIndex >=0 then
  begin
    edSecondarySkills.Objects[edSecondarySkills.Col,edSecondarySkills.Row] := editor.Items.Objects[editor.ItemIndex];
  end
  else begin
   edSecondarySkills.Objects[edSecondarySkills.Col,edSecondarySkills.Row] := nil;
  end;

end;

procedure TPandorasRewardFrame.LoadMain(AOptions: TPandorasOptions);
begin
  edAttack.Value:=AOptions.PrimarySkills.Attack;
  edDefence.Value:=AOptions.PrimarySkills.Defence;
  edExperience.Value:=AOptions.Experience;  //todo: suppport 64 bit value
  edKnowledge.Value:=AOptions.PrimarySkills.Knowledge;
  edLuck.Value:=AOptions.Luck;
  edMana.Value:=AOptions.Mana;
  edMorale.Value:=AOptions.Morale;
  edSpellPower.Value:=AOptions.PrimarySkills.Spellpower;
end;

procedure TPandorasRewardFrame.LoadSecondarySkills(
  AOptions: THeroSecondarySkills);
var
  i: Integer;

  option: THeroSecondarySkill;
  row: Integer;
  info: TBaseInfo;
begin
  edSecondarySkills.Clean([gzNormal]);

  for i := 0 to AOptions.Count - 1 do
  begin
    option := AOptions[i];
    row := edSecondarySkills.RowCount;
    edSecondarySkills.InsertColRow(false, row);

    info := (ListsManager.SkillMap.Objects[ListsManager.SkillMap.IndexOf(option.Identifier)] as TBaseInfo);

    edSecondarySkills.Cells[0, row] := info.Name;
    edSecondarySkills.Objects[0, row] := info;

    edSecondarySkills.Cells[1, row] := MasterySelector.Items[Integer(option.Level)];
    edSecondarySkills.Objects[1, row] := TObject(PtrUint(option.Level));
  end;
end;

procedure TPandorasRewardFrame.LoadResources(AOptions: TResourceSet);
begin
  edWood.Value:=AOptions.Wood;
  edMercury.Value:=AOptions.Mercury;
  edOre.Value:=AOptions.Ore;
  edSulfur.Value:=AOptions.Sulfur;
  edCrystal.Value:=AOptions.Crystal;
  edGems.Value:=AOptions.Gems;
  edGold.Value:=AOptions.Gold;
  edMithril.Value:=AOptions.Mithril;
end;

procedure TPandorasRewardFrame.Load(AOptions: TPandorasOptions);
begin
  FOptions := AOptions;

  LoadMain(AOptions);
  LoadSecondarySkills(AOptions.SecondarySkills);
  LoadResources(AOptions.Resources);

  edArtifacts.FillFromList(ListsManager.ArtifactMap, AOptions.Artifacts);
  edSpells.FillFromList(ListsManager.SpellMap, AOptions.Spells);

end;

procedure TPandorasRewardFrame.SaveMain(AOptions: TPandorasOptions);
begin
  AOptions.PrimarySkills.Attack := edAttack.Value;
  AOptions.PrimarySkills.Defence := edDefence.Value;
  AOptions.Experience := edExperience.Value;  //todo: suppport 64 bit value
  AOptions.PrimarySkills.Knowledge := edKnowledge.Value;
  AOptions.Luck := edLuck.Value;
  AOptions.Mana := edMana.Value;
  AOptions.Morale := edMorale.Value;
  AOptions.PrimarySkills.Spellpower := edSpellPower.Value;
end;

procedure TPandorasRewardFrame.SaveSecondarySkills(
  AOptions: THeroSecondarySkills);
var
  i: Integer;
  info: TBaseInfo;
  option: THeroSecondarySkill;
begin
  AOptions.Clear;

  for i := edSecondarySkills.FixedRows to edSecondarySkills.RowCount - 1 do
  begin
    if Not Assigned(edSecondarySkills.Objects[0, i]) then
    begin
      Continue;
    end;

    info := edSecondarySkills.Objects[0, i] as TBaseInfo;

    option := AOptions.Add;
    option.Identifier := info.ID;
    option.Level:=TSkillLevel(Integer(PtrUint(edSecondarySkills.Objects[1, i])));
  end;

end;

procedure TPandorasRewardFrame.SaveResources(AOptions: TResourceSet);
begin
  AOptions.Wood := edWood.Value;
  AOptions.Mercury := edMercury.Value;
  AOptions.Ore := edOre.Value;
  AOptions.Sulfur := edSulfur.Value;
  AOptions.Crystal := edCrystal.Value;
  AOptions.Gems := edGems.Value;
  AOptions.Gold := edGold.Value;
  AOptions.Mithril := edMithril.Value;
end;

constructor TPandorasRewardFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  edSecondarySkills.FastEditing := true;

  pcMain.ActivePage := tsMain;
end;

procedure TPandorasRewardFrame.Commit;
begin
  inherited Commit;
  SaveMain(FOptions);
  SaveSecondarySkills(FOptions.SecondarySkills);
  SaveResources(FOptions.Resources);

  edArtifacts.SaveToList(FOptions.Artifacts);
  edSpells.SaveToList(FOptions.Spells);
end;

procedure TPandorasRewardFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  inherited VisitPandorasBox(AOptions);
  Load(AOptions);
end;

procedure TPandorasRewardFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  inherited VisitLocalEvent(AOptions);
  Load(AOptions);
end;

end.

