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
unit hero_artifacts_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Grids, base_options_frame, gui_helpers,
  object_options, editor_types, editor_consts, base_info, Map;

type

  { THeroArtifactsFrame }

  THeroArtifactsFrame = class(TBaseOptionsFrame)
    BackpackSelector: TComboBox;
    cbCustomise: TCheckBox;
    Panel1: TPanel;
    pnSlots: TPanel;
    pnBackpack: TPanel;
    Splitter1: TSplitter;
    BackPack: TStringGrid;
    procedure BackPackKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BackPackResize(Sender: TObject);
    procedure BackPackSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure BackpackSelectorCloseUp(Sender: TObject);
    procedure BackpackSelectorEditingDone(Sender: TObject);
    procedure BackpackSelectorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbCustomiseChange(Sender: TObject);
  private
    FOptions: THeroArtifacts;
    FMapDefaults: THeroArtifacts;

    FSlotCaptions: array[0..ARTIFACT_SLOT_COUNT-1] of TLocalizedString;

    FSlotEditors: array[0..ARTIFACT_SLOT_COUNT-1] of TCustomComboBox;

    procedure FillSlotCaptions;
    procedure CreateSlots;
    procedure PrepareSlots;

    procedure Clear;

    procedure LoadDefaultArtifacts(AHeroID: AnsiString);

    procedure Load(AOptions: THeroArtifacts);

    procedure DoLoad(ASrc: THeroArtifacts);
  protected
    procedure Load; override;
    procedure ReloadDefaults; override;
    procedure ApplyDefaults; override;
    procedure UpdateControls;override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;
    procedure VisitHero(AOptions: THeroOptions); override;
    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

uses editor_str_consts;

{$R *.lfm}

{ THeroArtifactsFrame }

procedure THeroArtifactsFrame.BackPackKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleStringGridKeyDown(Sender, Key, Shift)
end;

procedure THeroArtifactsFrame.BackPackResize(Sender: TObject);
begin
  HandleStringGridResize(Sender);
end;

procedure THeroArtifactsFrame.BackPackSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  grid: TCustomStringGrid;
begin
  grid := Sender as TCustomStringGrid;
  Editor := BackpackSelector;

  BackpackSelector.SetValue(ListsManager.ArtifactInfos,TBaseInfo(grid.Objects[grid.Col, Grid.Row])) ;

  Editor.BoundsRect := grid.CellRect(aCol,aRow);

end;

procedure THeroArtifactsFrame.BackpackSelectorCloseUp(Sender: TObject);
begin
  (Sender as TComboBox).EditingDone;
end;

procedure THeroArtifactsFrame.BackpackSelectorEditingDone(Sender: TObject);
var
  editor: TCustomComboBox;
begin

  editor :=  (Sender as TCustomComboBox);

  BackPack.Cells[BackPack.Col,BackPack.Row]:=editor.Text;

  if editor.ItemIndex >=0 then
  begin
    BackPack.Objects[BackPack.Col,BackPack.Row] := editor.Items.Objects[editor.ItemIndex];
  end
  else begin
    BackPack.Objects[BackPack.Col,BackPack.Row] := nil;
  end;

end;

procedure THeroArtifactsFrame.BackpackSelectorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleStringGridKeyDown(BackPack, key, shift);
end;

procedure THeroArtifactsFrame.cbCustomiseChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure THeroArtifactsFrame.FillSlotCaptions;
var
  slot: Integer;

  procedure setslot(s: TLocalizedString);
  begin
    FSlotCaptions[slot] := s;
    inc(slot);
  end;

begin
  slot := 0;

  setslot(rsSlotHead);
  setslot(rsSlotShoulders);
  setslot(rsSlotNeck);
  setslot(rsSlotRightHand);
  setslot(rsSlotLeftHand);
  setslot(rsSlotTorso);

  setslot(rsSlotRightRing);
  setslot(rsSlotLeftRing);
  setslot(rsSlotFeet);

  setslot(rsSlotMisc1);
  setslot(rsSlotMisc2);
  setslot(rsSlotMisc3);
  setslot(rsSlotMisc4);

  setslot(rsSlotMach1);
  setslot(rsSlotMach2);
  setslot(rsSlotMach3);
  setslot(rsSlotMach4);

  setslot(rsSlotSpellbook);
  setslot(rsSlotMisc5);
end;

procedure THeroArtifactsFrame.CreateSlots;
var
  slot: Integer;

  lb: TCustomLabel;
  ed: TCustomComboBox;
begin
  pnSlots.DestroyComponents;

  for slot in [0..ARTIFACT_SLOT_COUNT-1] do
  begin
    lb := TCustomLabel.Create(pnSlots);
    lb.Parent:=pnSlots;
    lb.Caption:=FSlotCaptions[slot];

    ed:=TCustomComboBox.Create(pnSlots);
    ed.Parent:=pnSlots;
    ed.ReadOnly:=true;

    FSlotEditors[slot]:=ed;
  end;
end;

procedure THeroArtifactsFrame.PrepareSlots;
var
  slot: Integer;
begin
  for slot in [0..ARTIFACT_SLOT_COUNT-1] do
  begin
    FSlotEditors[slot].FillFromListWithEmptyOption(ListsManager.ArtifactSlotMap[slot], '');
  end;
end;

procedure THeroArtifactsFrame.Load;
begin
  DoLoad(FOptions);
end;

procedure THeroArtifactsFrame.ReloadDefaults;
begin
  inherited ReloadDefaults;

  if FUseMapDefaults then
    LoadDefaultArtifacts(InstanceType);
end;

procedure THeroArtifactsFrame.ApplyDefaults;
begin
  inherited ApplyDefaults;

  if not cbCustomise.Checked then
  begin
    Clear;
  end;
end;

procedure THeroArtifactsFrame.Clear;
var
  slot: Integer;
begin
  if FUseMapDefaults and Assigned(FMapDefaults) and not FMapDefaults.IsEmpty then
  begin
    DoLoad(FMapDefaults);
  end
  else
  begin
    BackPack.RowCount:=BackPack.FixedRows;

    for slot in [0..ARTIFACT_SLOT_COUNT-1] do
    begin
      FSlotEditors[slot].ItemIndex := -1;
    end;
  end;
end;

procedure THeroArtifactsFrame.LoadDefaultArtifacts(AHeroID: AnsiString);
var
  definition: THeroDefinition;
begin
  definition := map.PredefinedHeroes.FindItem(AHeroID);

  if Assigned(definition) then
  begin
    FMapDefaults := definition.Artifacts;
  end
  else
  begin
    FMapDefaults := nil;
  end;
end;

procedure THeroArtifactsFrame.UpdateControls;
begin
  inherited UpdateControls;
  BackPack.Enabled := cbCustomise.Checked;
  pnSlots.Enabled := cbCustomise.Checked;

  if cbCustomise.Checked then
    Load()
  else
    Clear();
end;

procedure THeroArtifactsFrame.Load(AOptions: THeroArtifacts);
begin
  FillItems(BackpackSelector.Items, ListsManager.ArtifactInfos);
  FOptions := AOptions;
  cbCustomise.OnChange:=nil;
  cbCustomise.Checked:=not FOptions.IsEmpty;
  cbCustomise.OnChange:=@cbCustomiseChange;

  UpdateControls;
end;

procedure THeroArtifactsFrame.DoLoad(ASrc: THeroArtifacts);
var
  slot: Integer;

  art: AnsiString;
  row: Integer;
  info: TBaseInfo;
begin
  DisableAutoSizing;
  BackPack.BeginUpdate;
  try
    for slot in [0..ARTIFACT_SLOT_COUNT-1] do
    begin
      FSlotEditors[slot].Items.BeginUpdate;
    end;

    for slot in [0..ARTIFACT_SLOT_COUNT-1] do
    begin
      FSlotEditors[slot].SetValueWithEmptyOption(ListsManager.ArtifactSlotMap[slot], ASrc.BySlotNumber[slot]);
    end;

    BackPack.RowCount:=BackPack.FixedRows + ASrc.Backpack.Count;

    BackPack.Clean([gzNormal, gzInvalid]);

    row := BackPack.FixedRows;

    for art in ASrc.Backpack do
    begin
      info := (ListsManager.ArtifactInfos.FindItem(art) as TBaseInfo);

      BackPack.Cells[0, row] := info.Name;
      BackPack.Objects[0, row] := info;
      inc(row);
    end;
  finally
    for slot in [0..ARTIFACT_SLOT_COUNT-1] do
    begin
      FSlotEditors[slot].Items.EndUpdate;
    end;
    BackPack.EndUpdate(True);
    EnableAutoSizing;
  end;
end;

constructor THeroArtifactsFrame.Create(TheOwner: TComponent);
begin
  FillSlotCaptions;
  inherited Create(TheOwner);
  CreateSlots;
end;

procedure THeroArtifactsFrame.Commit;
var
  slot: Integer;
  i: Integer;
  info: TBaseInfo;
begin
  inherited Commit;

  if not cbCustomise.Checked then
  begin
    FOptions.Clear;
    Exit;
  end;

  for slot in [0..ARTIFACT_SLOT_COUNT-1] do
    FOptions.BySlotNumber[slot]:=FSlotEditors[slot].SelectedIdentifier();

  FOptions.Backpack.Clear;

  for i := BackPack.FixedRows to BackPack.RowCount - 1 do
  begin
    if Not Assigned(BackPack.Objects[0, i]) then
    begin
      Continue;
    end;

    info := BackPack.Objects[0, i] as TBaseInfo;

    FOptions.Backpack.Add(info.Identifier);
  end;
end;

procedure THeroArtifactsFrame.VisitHero(AOptions: THeroOptions);
begin
  PrepareSlots;
  FUseMapDefaults:=True;

  LoadDefaultArtifacts(AOptions.&type);

  inherited VisitHero(AOptions);
  Load(AOptions.Artifacts);
end;

procedure THeroArtifactsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  PrepareSlots;
  FUseMapDefaults:=False;

  inherited VisitHeroDefinition(AOptions);
  Load(AOptions.Artifacts);
end;

end.

