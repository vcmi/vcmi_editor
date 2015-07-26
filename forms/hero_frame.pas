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
  Classes, SysUtils, FileUtil, strutils, typinfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComboEx, EditBtn, ComCtrls, base_options_frame,
  gui_helpers, object_options, editor_consts, editor_types, base_info,
  lists_manager, editor_rtti, editor_classes, LCLType, rttiutils;

type

  { THeroFrame }

  THeroFrame = class(TBaseOptionsFrame)
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
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
  private

  protected
    FCustomName: TLocalizedString;
    FCustomFemale: Boolean;
    FCustomBiography: TLocalizedString;

    FCurrentHero: IHeroInfo;//todo: use map specicfic defaults

    function GetDefaultBiography: TLocalizedString; virtual;
    function GetDefaultName: TLocalizedString; virtual;
    function GetDefaultSex: THeroSex; virtual;

    procedure UpdateText(AControl: TCustomEdit; AFlag: TCustomCheckBox; ACustom: TLocalizedString; ADefault: TLocalizedString);

    procedure UpdateControls(); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ THeroFrame }

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

function THeroFrame.GetDefaultBiography: TLocalizedString;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetBiography;
  end
  else begin
    Result := '';
  end;
end;

function THeroFrame.GetDefaultName: TLocalizedString;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetName;
  end
  else begin
    Result := '';
  end;
end;

function THeroFrame.GetDefaultSex: THeroSex;
begin
  if Assigned(FCurrentHero) then
  begin
    Result := FCurrentHero.GetSex;
  end
  else begin
    Result := THeroSex.default; //???
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
    edSex.ItemIndex := Integer(GetDefaultSex);
  end;
end;

procedure THeroFrame.cbExperienceChange(Sender: TObject);
begin
  CustomiseChange(Sender);
end;

procedure THeroFrame.cbBiographyChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  UpdateText(edBiography, cbBiography, FCustomBiography,GetDefaultBiography());
end;

procedure THeroFrame.cbNameChange(Sender: TObject);
begin
  CustomiseChange(Sender);
  UpdateText(edName, cbName, FCustomName,GetDefaultName);
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


end.


