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
unit hero_skills_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, base_options_frame, gui_helpers, Map, editor_classes,
  base_info, object_options, editor_types;

type

  { THeroSkillsFrame }

  THeroSkillsFrame = class(TBaseOptionsFrame)
    cbCustomise: TCheckBox;
    edSecondarySkills: TStringGrid;
    MasterySelector: TComboBox;
    Panel1: TPanel;
    SecondarySkillSelector: TComboBox;
    procedure cbCustomiseChange(Sender: TObject);
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
    FObject: THeroSecondarySkills;
    FCustomSkills, FDefaultSkills:  THeroSecondarySkills;

    procedure SaveSkills;
    procedure LoadSkills(ASrc:THeroSecondarySkills);

    procedure Load;
  protected

    procedure VisitNormalHero(AOptions: THeroOptions);override;
    procedure VisitRandomHero(AOptions: THeroOptions);override;
    procedure VisitPrison(AOptions: THeroOptions);override;

    procedure UpdateControls();override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
    procedure VisitHero(AOptions: THeroOptions); override;
    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

{$R *.lfm}

{ THeroSkillsFrame }

procedure THeroSkillsFrame.edSecondarySkillsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleStringGridKeyDown(Sender, Key, Shift);
end;

procedure THeroSkillsFrame.cbCustomiseChange(Sender: TObject);
begin
  UpdateControls();

  if cbCustomise.Checked then
  begin
    LoadSkills(FCustomSkills);
  end
  else
  begin
    SaveSkills;
    LoadSkills(FDefaultSkills);
  end;
end;

procedure THeroSkillsFrame.edSecondarySkillsResize(Sender: TObject);
begin
  HandleStringGridResize(Sender);
end;

procedure THeroSkillsFrame.edSecondarySkillsSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
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
      MasterySelector.ItemIndex := Integer(PtrUInt(grid.Objects[grid.Col, Grid.Row])) - 1;
    end
  end;

  Editor.BoundsRect := grid.CellRect(aCol,aRow);
end;

procedure THeroSkillsFrame.MasterySelectorCloseUp(Sender: TObject);
begin
  (Sender as TComboBox).EditingDone;
end;

procedure THeroSkillsFrame.MasterySelectorEditingDone(Sender: TObject);
begin
  edSecondarySkills.Cells[edSecondarySkills.Col,edSecondarySkills.Row]:=(Sender as TCustomComboBox).Text;

  edSecondarySkills.Objects[edSecondarySkills.Col,edSecondarySkills.Row] := TObject(PtrUint((Sender as TCustomComboBox).ItemIndex) + 1);
end;

procedure THeroSkillsFrame.SecondarySkillSelectorCloseUp(Sender: TObject);
begin
  (Sender as TComboBox).EditingDone;
end;

procedure THeroSkillsFrame.SecondarySkillSelectorEditingDone(Sender: TObject);
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

procedure THeroSkillsFrame.SaveSkills;
var
  i: Integer;
  info: TBaseInfo;
  option: THeroSecondarySkill;
begin
  FCustomSkills.Clear;
  for i := edSecondarySkills.FixedRows to edSecondarySkills.RowCount - 1 do
  begin
    if Not Assigned(edSecondarySkills.Objects[0, i]) then
    begin
      Continue;
    end;

    info := edSecondarySkills.Objects[0, i] as TBaseInfo;

    option := FCustomSkills.Add;
    option.Identifier := info.ID;
    option.Level:=TSkillLevel(Integer(PtrUint(edSecondarySkills.Objects[1, i])));
  end;
end;

procedure THeroSkillsFrame.LoadSkills(ASrc: THeroSecondarySkills);
var
  i: Integer;

  option: THeroSecondarySkill;
  row: Integer;
  info: TBaseInfo;
begin
  edSecondarySkills.Clean([gzNormal]);
  edSecondarySkills.RowCount := edSecondarySkills.FixedRows;

  for i := 0 to ASrc.Count - 1 do
  begin
    option := ASrc[i];
    row := edSecondarySkills.RowCount;
    edSecondarySkills.InsertColRow(false, row);

    info := (ListsManager.SkillMap.Objects[ListsManager.SkillMap.IndexOf(option.Identifier)] as TBaseInfo);

    edSecondarySkills.Cells[0, row] := info.Name;
    edSecondarySkills.Objects[0, row] := info;

    edSecondarySkills.Cells[1, row] := MasterySelector.Items[Integer(option.Level) - 1];
    edSecondarySkills.Objects[1, row] := TObject(PtrUint(option.Level));
  end;
end;

procedure THeroSkillsFrame.Load;
begin
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
  edSecondarySkills.Enabled := cbCustomise.Checked;
end;

procedure THeroSkillsFrame.VisitNormalHero(AOptions: THeroOptions);
begin
  inherited VisitNormalHero(AOptions);

  if AOptions.&type <> '' then
  begin
    FDefaultSkills.Assign(ListsManager.Heroes[AOptions.&type].Skills);
  end;
end;

procedure THeroSkillsFrame.VisitRandomHero(AOptions: THeroOptions);
begin
  inherited VisitRandomHero(AOptions);
end;

procedure THeroSkillsFrame.VisitPrison(AOptions: THeroOptions);
begin
  inherited VisitPrison(AOptions);

  if AOptions.&type <> '' then
  begin
    FDefaultSkills.Assign(ListsManager.Heroes[AOptions.&type].Skills);
  end;
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

procedure THeroSkillsFrame.VisitHero(AOptions: THeroOptions);
begin
  inherited VisitHero(AOptions);
  FObject := AOptions.SecondarySkills;
  Load;
end;

procedure THeroSkillsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  inherited VisitHeroDefinition(AOptions);
  FObject := AOptions.Skills;

  Load;
end;

end.

