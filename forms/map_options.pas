{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
  StdCtrls, ComCtrls, ExtCtrls, Spin, CheckLst, ActnList, Buttons, gui_helpers,
  edit_hero_options, Map, lists_manager, base_info, root_manager, editor_classes;

type

  { TMapOptionsForm }

  TMapOptionsForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actAddMod: TAction;
    actRemoveMod: TAction;
    actSave: TAction;
    btOk: TButton;
    btCancel: TButton;
    cbSpellsNegate: TComboBox;
    cbSkillsNegate: TComboBox;
    cbArtifactsNegate: TComboBox;
    AllMods: TComboBox;
    cbHeroesNegate: TComboBox;
    edAllowedHeroes: TCheckListBox;
    edSpells: TCheckListBox;
    edAbilities: TCheckListBox;
    edName: TEdit;
    edArtifacts: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbRequiredMods: TLabel;
    edRequiredMods: TListBox;
    lMapName: TLabel;
    lMapDescription: TLabel;
    edDescription: TMemo;
    Panel3: TPanel;
    pcMain: TPageControl;
    edDifficulty: TRadioGroup;
    edLevelLimit: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    tsMods: TTabSheet;
    tsArtifacts: TTabSheet;
    tsHeroes: TTabSheet;
    tsSpells: TTabSheet;
    tsAbilities: TTabSheet;
    tsMain: TTabSheet;
    procedure actAddModExecute(Sender: TObject);
    procedure actAddModUpdate(Sender: TObject);
    procedure actDontSaveExecute(Sender: TObject);
    procedure actRemoveModExecute(Sender: TObject);
    procedure actRemoveModUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure cbEnableLevelLimitChange(Sender: TObject);
    procedure edAllowedHeroesDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMap: TVCMIMap;

    FModUsageCache: TModUsage;

    procedure LoadModUsage;

    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure UpdateControls;

    procedure Commit;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Map: TVCMIMap read FMap write SetMap;

  end;


implementation

uses editor_types;

{$R *.lfm}

{ TMapOptionsForm }

procedure TMapOptionsForm.actSaveExecute(Sender: TObject);
begin
  Commit;

  ModalResult := mrOK;
end;

procedure TMapOptionsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMapOptionsForm.actAddModUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:= AllMods.ItemIndex >= 0;
end;

procedure TMapOptionsForm.actAddModExecute(Sender: TObject);
begin
  FModUsageCache.EnsureItem(AllMods.Items[AllMods.ItemIndex]).Forced := true;
  LoadModUsage;
end;

procedure TMapOptionsForm.actRemoveModExecute(Sender: TObject);
var
  item: TModRefCountInfo;
begin
  item := edRequiredMods.Items.Objects[edRequiredMods.ItemIndex] as TModRefCountInfo;
  item.Forced:=False;

  LoadModUsage;
end;

procedure TMapOptionsForm.actRemoveModUpdate(Sender: TObject);
var
  flag: Boolean;
  item: TModRefCountInfo;
begin
  flag := (edRequiredMods.ItemIndex >= 0);

  if flag then
  begin
    item := edRequiredMods.Items.Objects[edRequiredMods.ItemIndex] as TModRefCountInfo;
    flag := item.Forced and (item.RefCount = 0);
  end;

  (Sender as TAction).Enabled:= flag;
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

  option := Map.PredefinedHeroes.FindItem(info.Identifier);

  if not Assigned(option) then
  begin
    option := Map.PredefinedHeroes.Add;
    option.Identifier := info.Identifier;
  end;

  frm.EditObject(option);
end;

procedure TMapOptionsForm.FormShow(Sender: TObject);
begin
  pcMain.ActivePage := tsMain;
end;

procedure TMapOptionsForm.LoadModUsage;
var
  i: Integer;
  item: TModRefCountInfo;
begin
  edRequiredMods.Items.BeginUpdate;
  try
    edRequiredMods.Clear;

    for i := 0 to FModUsageCache.Count - 1 do
    begin
      item := FModUsageCache.Items[i];

      if item.Forced or (item.RefCount > 0) then
      begin
        edRequiredMods.AddItem(item.Identifier, item);
      end;
    end;
  finally
    edRequiredMods.Items.EndUpdate;
  end;
end;

procedure TMapOptionsForm.ReadData;
begin
  AllMods.Items.Clear;
  AllMods.Items.AddStrings(FMap.ListsManager.GetEnabledMods());

  FModUsageCache.Assign(FMap.ModUsage);
  LoadModUsage;

  edDifficulty.ItemIndex := Integer(FMap.Difficulty);
  edLevelLimit.Value := FMap.HeroLevelLimit;

  edName.Text := FMap.Name;
  edDescription.Text := FMap.Description;

  edAbilities.FillFrom(FMap.ListsManager.SkillInfos, FMap.AllowedAbilities);
  cbSkillsNegate.ItemIndex:=ifthen(FMap.AllowedAbilities.IsPermissive, 1, 0);

  edSpells.FillFrom(FMap.ListsManager.SpellInfos, FMap.AllowedSpells);
  cbSpellsNegate.ItemIndex := ifthen(FMap.AllowedSpells.IsPermissive, 1, 0);

  edArtifacts.FillFrom(FMap.ListsManager.ArtifactInfos, FMap.AllowedArtifacts);
  cbArtifactsNegate.ItemIndex := ifthen(FMap.AllowedArtifacts.IsPermissive, 1, 0);

  edAllowedHeroes.FillFrom(Fmap.ListsManager.HeroInfos, FMap.AllowedHeroes);
  cbHeroesNegate.ItemIndex := ifthen(FMap.AllowedHeroes.IsPermissive, 1, 0);

  UpdateControls;
end;

procedure TMapOptionsForm.UpdateControls;
begin
 //
end;

procedure TMapOptionsForm.Commit;
begin
  //todo: validate

  FMap.Difficulty := TDifficulty(edDifficulty.ItemIndex);
  FMap.HeroLevelLimit:=edLevelLimit.Value;

  FMap.Name := edName.Text;
  FMap.Description := edDescription.Text;

  edAbilities.SaveTo    (FMap.ListsManager.SkillInfos,    Fmap.AllowedAbilities, cbSkillsNegate.ItemIndex = 1);
  edSpells.SaveTo       (FMap.ListsManager.SpellInfos,    FMap.AllowedSpells, cbSpellsNegate.ItemIndex = 1);
  edAllowedHeroes.SaveTo(FMap.ListsManager.HeroInfos,     FMap.AllowedHeroes, cbHeroesNegate.ItemIndex = 1);
  edArtifacts.SaveTo    (FMap.ListsManager.ArtifactInfos, FMap.AllowedArtifacts, cbArtifactsNegate.ItemIndex = 1);

  FMap.ModUsage.UpdateForced(FModUsageCache);

  FMap.IsDirty:=True;
end;

constructor TMapOptionsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModUsageCache := TModUsage.Create(nil);
end;

destructor TMapOptionsForm.Destroy;
begin
  FModUsageCache.Free;
  inherited Destroy;
end;

procedure TMapOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;
  if Assigned( FMap) then ReadData;
end;



end.

