{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit scholar_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, base_options_frame, gui_helpers, object_options,
  editor_types, editor_consts, lists_manager, base_info;

type

  { TScholarFrame }

  TScholarFrame = class(TBaseOptionsFrame)
    edBonusID: TComboBox;
    edBonusType: TRadioGroup;
    Label1: TLabel;
    procedure edBonusIDChange(Sender: TObject);
    procedure edBonusTypeClick(Sender: TObject);
  private
    FOptions: TScholarOptions;
    FBonusID: AnsiString;

  protected
    procedure UpdateControls; override;
  public
    procedure Commit; override;
    procedure VisitScholar(AOptions: TScholarOptions); override;
  end;

implementation

{$R *.lfm}

{ TScholarFrame }

procedure TScholarFrame.edBonusTypeClick(Sender: TObject);
begin
  FBonusID := '';
  UpdateControls;
end;

procedure TScholarFrame.edBonusIDChange(Sender: TObject);
begin
  FBonusID:='';

  if edBonusID.ItemIndex<>-1 then
  begin
    FBonusID:=(edBonusID.Items.Objects[edBonusID.ItemIndex] as TBaseInfo).Identifier;
  end;
end;

procedure TScholarFrame.UpdateControls;
var
  tmp : AnsiString;
begin
  inherited UpdateControls;
  tmp := FBonusID;

  edBonusID.Enabled := edBonusType.ItemIndex <> Integer(TScholarBonus.random);

  edBonusID.Items.Clear;
  edBonusID.ItemIndex := -1;


  FBonusID := tmp;

  case edBonusType.ItemIndex of
    Integer(TScholarBonus.random):
    begin

    end;
    Integer(TScholarBonus.primSkill):
    begin
      edBonusID.FillFromList(ListsManager.PrimarySkills, FBonusID);
    end;
    Integer(TScholarBonus.skill):
    begin
      edBonusID.FillFromList(ListsManager.SkillInfos, FBonusID);
    end;
    Integer(TScholarBonus.spell):
    begin
      edBonusID.FillFromList(ListsManager.SpellInfos, FBonusID);
    end;
  end;

  if (edBonusID.Items.Count > 0) and (edBonusID.ItemIndex = -1) then
  begin
    edBonusID.ItemIndex := 0;
  end;
end;

procedure TScholarFrame.Commit;
begin
  FOptions.Clear;
  case TScholarBonus(edBonusType.ItemIndex) of
    TScholarBonus.primSkill: FOptions.RewardPrimSkill := FBonusID;
    TScholarBonus.skill: FOptions.RewardSkill := FBonusID;
    TScholarBonus.spell: FOptions.RewardSpell := FBonusID;
  end;
end;

procedure TScholarFrame.VisitScholar(AOptions: TScholarOptions);
begin
  FOptions := AOptions;

  FBonusID := '';

  if AOptions.RewardPrimSkill <> '' then
  begin
    edBonusType.ItemIndex:=Integer(TScholarBonus.primSkill);
    FBonusID:=AOptions.RewardPrimSkill;
  end
  else if AOptions.RewardSkill <> '' then
  begin
    edBonusType.ItemIndex:=Integer(TScholarBonus.skill);
    FBonusID:=AOptions.RewardSkill;
  end
  else if AOptions.RewardSpell <> '' then
  begin
    edBonusType.ItemIndex:=Integer(TScholarBonus.spell);
    FBonusID:=AOptions.RewardSpell;
  end
  else
     edBonusType.ItemIndex := Integer(TScholarBonus.random);

  UpdateControls;
end;

end.

