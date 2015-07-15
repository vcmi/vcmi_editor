{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge.net

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
  Classes, SysUtils, FileUtil, strutils, typinfo, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComboEx, EditBtn, ComCtrls,
  base_object_options_frame, gui_helpers, object_options, editor_consts,
  editor_types, base_info, lists_manager, editor_rtti, LCLType, rttiutils;

type

  { THeroFrame }

  THeroFrame = class(TBaseObjectOptionsFrame)
    cbName: TCheckBox;
    cbSex: TCheckBox;
    cbPortrait: TCheckBox;
    cbExperience: TCheckBox;
    cbBiography: TCheckBox;
    edBiography: TMemo;
    edSex: TComboBox;
    edPatrol: TComboBox;
    edHeroClass: TComboBox;
    edExperience: TEdit;
    edName: TEdit;
    edPortrait: TComboBoxEx;
    edOwner: TComboBox;
    edType: TComboBox;
    lbPatrol: TLabel;
    lbBiography: TLabel;
    lbSex: TLabel;
    lbName: TLabel;
    lbExperience: TLabel;
    lbPortrait: TLabel;
    lbHeroClass: TLabel;
    lbOwner: TLabel;
    Placeholder1: TLabel;
    lbType: TLabel;
    Placeholder2: TLabel;
    Placeholder3: TLabel;
    Placeholder5: TLabel;
    procedure cbBiographyChange(Sender: TObject);
    procedure cbExperienceChange(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure cbPortraitChange(Sender: TObject);
    procedure cbSexChange(Sender: TObject);
    procedure CustomiseChange(Sender: TObject);
    procedure edHeroClassChange(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
    procedure edTypeChange(Sender: TObject);
  private
    FOptions: THeroOptions;
    FCurrentHero: THeroInfo;

    FCustomName: TLocalizedString;
    FCustomFemale: Boolean;
    FCustomBiography: TLocalizedString;

    procedure Load();

    function GetHeroClass():AnsiString;
    function GetHeroClassName():TLocalizedString;

    procedure UpdateControls();

    procedure UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString);
  protected
    procedure VisitNormalHero(AOptions: THeroOptions);override;
    procedure VisitRandomHero(AOptions: THeroOptions);override;
    procedure VisitPrison(AOptions: THeroOptions);override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;
    procedure VisitHero(AOptions: THeroOptions); override;
  end;

implementation

{$R *.lfm}

{ THeroFrame }

procedure THeroFrame.edHeroClassChange(Sender: TObject);
var
  base_class_info: TBaseInfo;

  editor: TCustomComboBox;
  idx: Integer;
  hero_type: String;
begin

  if not edType.Visible then
    Exit;

  //fill hero types
  editor := Sender as TCustomComboBox;

  if editor.ItemIndex < 0 then
  begin
    edType.ItemIndex := -1;
    edType.Items.Clear;
    edType.Enabled:=false;
  end
  else begin
    edType.Enabled:=true;
    edType.ItemIndex := -1;

    base_class_info := editor.Items.Objects[editor.ItemIndex] as TBaseInfo;

    ListsManager.FillWithHeroesOfClass(edType.Items, base_class_info.ID);

    hero_type :=  FOptions.&type;

    for idx := 0 to edType.Items.Count - 1 do
    begin
      if (edType.Items.Objects[idx] as TBaseInfo).ID = hero_type then
      begin
        edType.ItemIndex := idx;
        break;
      end;
    end;

    if (edType.ItemIndex = -1) and (edType.Items.Count >0) then
      edType.ItemIndex := 0;
  end;

  edTypeChange(edType);
end;

procedure THeroFrame.edNameChange(Sender: TObject);
begin

end;

procedure THeroFrame.edNameEditingDone(Sender: TObject);
begin
  FCustomName := edName.Text;
end;

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

procedure THeroFrame.CustomiseChange(Sender: TObject);
begin
  UpdateControls();
end;

procedure THeroFrame.cbPortraitChange(Sender: TObject);
begin
  CustomiseChange(Sender);
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
    if Assigned(FCurrentHero) then
    begin
      edSex.ItemIndex := Integer(FCurrentHero.Female);
    end;
  end;
end;

procedure THeroFrame.cbExperienceChange(Sender: TObject);
begin
  CustomiseChange(Sender);
end;

procedure THeroFrame.cbBiographyChange(Sender: TObject);
var
  ADefault: TLocalizedString;
begin
  CustomiseChange(Sender);

  if Assigned(FCurrentHero) then
  begin
    ADefault := FCurrentHero.Texts.Biography;
  end
  else begin
    ADefault := '';
  end;

  UpdateText(edBiography, cbBiography, FCustomBiography,ADefault);
end;

procedure THeroFrame.cbNameChange(Sender: TObject);
var
  ADefault: TLocalizedString;

begin
  CustomiseChange(Sender);

  if Assigned(FCurrentHero) then
  begin
    ADefault := FCurrentHero.Name;
  end
  else begin
    ADefault := '';
  end;

  UpdateText(edName, cbName, FCustomName,ADefault);

end;

procedure THeroFrame.edTypeChange(Sender: TObject);
var
  editor: TCustomComboBox;
begin
  FCurrentHero := nil;

  editor := Sender as TCustomComboBox;

  if Assigned(editor.SelectedInfo) then
  begin
    FCurrentHero := editor.SelectedInfo as THeroInfo;
  end;

  cbPortraitChange(cbPortrait);
  cbNameChange(cbName);
  cbSexChange(cbSex);
  cbBiographyChange(cbBiography);
  UpdateControls;
end;

procedure THeroFrame.VisitNormalHero(AOptions: THeroOptions);
begin
  edHeroClass.Enabled:=False;
  Load();
end;

procedure THeroFrame.VisitRandomHero(AOptions: THeroOptions);
begin
  lbHeroClass.Visible:=False;
  edHeroClass.Visible:=False;
  Placeholder1.Visible:=False;

  lbType.Visible:=False;
  edType.Visible:=False;
  Placeholder2.Visible:=False;

  Load();
end;

procedure THeroFrame.VisitPrison(AOptions: THeroOptions);
begin
  lbOwner.Visible:=False;
  edOwner.Visible := False;
  Placeholder3.Visible:=False;

  Load();
end;

procedure THeroFrame.Load;
begin

  cbPortrait.Checked:=FOptions.Portrait <> '';
  cbExperience.Checked := FOptions.Experience <> 0;

  cbName.Checked:=FOptions.Name <> '';
  cbSex.Checked:=FOptions.Sex <> THeroSex.default;
  cbBiography.Checked:=FOptions.Biography <> '';


  edHeroClass.FillFromList(ListsManager.HeroClassMap, GetHeroClass);

  edHeroClassChange(edHeroClass); //also loads type


  if edOwner.Visible then
  begin
    edOwner.ItemIndex := Integer(FOptions.Owner);
  end;

  //TODO: load portrait



  if cbExperience.Checked then
  begin
    edExperience.Text := IntToStr(FOptions.Experience);
  end
  else
  begin
    edExperience.Text := '0';
  end;

  cbName.Checked:= FOptions.Name <> '';
  cbNameChange(cbName);

  case FOptions.PatrolRadius of
    -1: edPatrol.ItemIndex := 0 ;
    0: edPatrol.ItemIndex := 1;
  else
    begin
      edPatrol.Text:=IntToStr(FOptions.PatrolRadius);
    end;
  end;

  UpdateControls();
end;

function THeroFrame.GetHeroClass: AnsiString;
begin
  if not Assigned(FOptions) then
  begin
    exit('');//just to safe
  end;

  if FOptions.&type = '' then
  begin

    if FOptions.MapObject.GetID = TYPE_HERO then
    begin
      Exit(FOptions.MapObject.GetSubId());
    end;
    exit('');
  end;

  Result := ListsManager.Heroes[FOptions.&type].&Class;
end;

function THeroFrame.GetHeroClassName: TLocalizedString;
var
  hero_class: String;
begin
  if not Assigned(FOptions) then
  begin
    exit('');
  end;

  if FOptions.&type = '' then
  begin
    exit('');
  end;

  hero_class := ListsManager.Heroes[FOptions.&type].&Class;

  Result := ListsManager.HeroClasses[hero_class].Name;
end;

procedure THeroFrame.UpdateControls;
begin
  edPortrait.Enabled:=cbPortrait.Checked;
  edExperience.Enabled:=cbExperience.Checked;
  edName.Enabled:=cbName.Checked;
  edBiography.Enabled:=cbBiography.Checked;
  edSex.Enabled:=cbSex.Checked;
end;

procedure THeroFrame.UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox;
  ACustom: TLocalizedString; ADefault: TLocalizedString);

begin
  CustomiseChange(AFlag);

  if AFlag.State = cbChecked then
  begin
    if ACustom = '' then
    begin
      AControl.Text := ADefault;
    end
    else
    begin
      AControl.Text := ACustom;
    end;
  end
  else
  begin
    AControl.Text := ADefault;
  end;
end;

constructor THeroFrame.Create(TheOwner: TComponent);
var
  p: TPlayer;
begin
  inherited Create(TheOwner);

  edOwner.Items.Clear;
  for p in TPlayerColor do
  begin
    edOwner.Items.Add(ListsManager.PlayerName[p]);
  end;
end;

procedure THeroFrame.Commit;
begin
  inherited Commit;

  if edType.Visible then
  begin
    FOptions.&type := edType.GetValueWithEmptyOption();
  end;

  if edOwner.Visible then
  begin
    FOptions.Owner := TPlayer(edOwner.ItemIndex);
  end;

  if cbExperience.Checked then
  begin
    FOptions.Experience := StrToQWordDef(edExperience.Text, 0);
  end
  else begin
    FOptions.Experience := 0;
  end;
end;

procedure THeroFrame.VisitHero(AOptions: THeroOptions);
begin
  FOptions := AOptions;
  inherited VisitHero(AOptions); //continue dispatch
end;

end.


