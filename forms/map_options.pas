{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

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

unit map_options;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, CheckLst, ActnList, gui_helpers,
  edit_hero_options, Map, lists_manager, base_info, root_manager;

type

  { TMapOptionsForm }

  TMapOptionsForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btOk: TButton;
    btCancel: TButton;
    cbSpellsNegate: TComboBox;
    cbSkillsNegate: TComboBox;
    cbArtifactsNegate: TComboBox;
    edAllowedHeroes: TCheckListBox;
    edSpells: TCheckListBox;
    edAbilities: TCheckListBox;
    edName: TEdit;
    edArtifacts: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbRequiredMods: TLabel;
    edRequiredMods: TListBox;
    lMapName: TLabel;
    lMapDescription: TLabel;
    edDescription: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pcMain: TPageControl;
    edDifficulty: TRadioGroup;
    edLevelLimit: TSpinEdit;
    tsMods: TTabSheet;
    tsArtifacts: TTabSheet;
    tsHeroes: TTabSheet;
    tsSpells: TTabSheet;
    tsAbilities: TTabSheet;
    tsMain: TTabSheet;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure cbEnableLevelLimitChange(Sender: TObject);
    procedure edAllowedHeroesDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure UpdateControls;
  public
    { public declarations }
    property Map: TVCMIMap read FMap write SetMap;

  end;


implementation

uses editor_types;

{$R *.lfm}

{ TMapOptionsForm }

procedure TMapOptionsForm.actSaveExecute(Sender: TObject);
begin
  //todo: validate
  //todo: save

  FMap.Difficulty := TDifficulty(edDifficulty.ItemIndex);
  FMap.LevelLimit:=edLevelLimit.Value;

  FMap.Name := edName.Text;
  FMap.Description := edDescription.Text;

  edAbilities.SaveToCondition    (FMap.ListsManager.SkillMap,    Fmap.AllowedAbilities, cbSkillsNegate.ItemIndex = 1);
  edSpells.SaveToCondition       (FMap.ListsManager.SpellMap,    FMap.AllowedSpells, cbSpellsNegate.ItemIndex = 1);
  edAllowedHeroes.SaveToCondition(FMap.ListsManager.HeroMap,     FMap.AllowedHeroes, True);
  edArtifacts.SaveToCondition    (FMap.ListsManager.ArtifactMap, FMap.AllowedArtifacts, cbArtifactsNegate.ItemIndex = 1);

  ModalResult := mrOK;
end;

procedure TMapOptionsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMapOptionsForm.cbEnableLevelLimitChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMapOptionsForm.edAllowedHeroesDblClick(Sender: TObject);
var
  frm: TEditHeroOptions;

  option: THeroDefinition;

  info: TBaseInfo;
begin
  frm := TEditHeroOptions.Create(Self);

  frm.Map := Map;
  frm.ListsManager := RootManager.ListsManager;

  info := edAllowedHeroes.selectedInfo();

  option := Map.PredefinedHeroes.FindItem(info.ID);

  if not Assigned(option) then
  begin
    option := Map.PredefinedHeroes.Add;
    option.Identifier := info.ID;
  end;

  frm.EditObject(option);
end;

procedure TMapOptionsForm.FormShow(Sender: TObject);
begin
  pcMain.ActivePage := tsMain;
end;

procedure TMapOptionsForm.ReadData;
var
  mod_id: AnsiString;
begin

  edDifficulty.ItemIndex := Integer(FMap.Difficulty);
  edLevelLimit.Value := FMap.LevelLimit;

  edName.Text := FMap.Name;
  edDescription.Text := FMap.Description;

  edAbilities.FillFromCondition(FMap.ListsManager.SkillMap, FMap.AllowedAbilities);
  cbSkillsNegate.ItemIndex:=ifthen(FMap.AllowedAbilities.IsPermissive, 1, 0);

  edSpells.FillFromCondition(FMap.ListsManager.SpellMap, FMap.AllowedSpells);
  cbSpellsNegate.ItemIndex := ifthen(FMap.AllowedSpells.IsPermissive, 1, 0);

  edArtifacts.FillFromCondition(FMap.ListsManager.ArtifactMap, FMap.AllowedArtifacts);
  cbArtifactsNegate.ItemIndex := ifthen(FMap.AllowedArtifacts.IsPermissive, 1, 0);

  edAllowedHeroes.FillFromCondition(Fmap.ListsManager.HeroMap, FMap.AllowedHeroes);

  for mod_id in FMap.Mods.AllOf do
  begin
    edRequiredMods.AddItem(mod_id, nil);
  end;

  UpdateControls;
end;

procedure TMapOptionsForm.UpdateControls;
begin
 //
end;

procedure TMapOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;
  if Assigned( FMap) then ReadData;
end;



end.

