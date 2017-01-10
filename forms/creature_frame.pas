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

unit creature_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, object_options, editor_types, base_options_frame, gui_helpers;

type

  { TCreatureFrame }

  TCreatureFrame = class(TBaseOptionsFrame)
    edRewardArtifact: TComboBox;
    edNoGrowing: TCheckBox;
    edRandomCount: TCheckBox;
    edNeverFlees: TCheckBox;
    gbCount: TGroupBox;
    edCount: TSpinEdit;
    gbReward: TGroupBox;
    edRewardMessage: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rgCharacter: TRadioGroup;
    edWood: TSpinEdit;
    edCrystal: TSpinEdit;
    edMercury: TSpinEdit;
    edGems: TSpinEdit;
    edOre: TSpinEdit;
    edGold: TSpinEdit;
    edSulfur: TSpinEdit;
    edMithril: TSpinEdit;
    procedure edRandomCountChange(Sender: TObject);
  private
    FOptions: TCreatureOptions;

  protected
    procedure UpdateControls; override;
  public
    procedure Commit; override;
    procedure VisitMonster(AOptions: TCreatureOptions); override;
  end;

implementation

{$R *.lfm}

{ TCreatureFrame }

procedure TCreatureFrame.edRandomCountChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TCreatureFrame.UpdateControls;
begin
  inherited UpdateControls;
  edCount.Enabled:=not edRandomCount.Checked;
end;

procedure TCreatureFrame.Commit;
begin
  inherited Commit;

  if edRandomCount.Checked then
  begin
    FOptions.Amount := 0;
  end
  else begin
    FOptions.Amount := edCount.Value;
  end;

  FOptions.NoGrowing := edNoGrowing.Checked;

  FOptions.Character := TCreatureCharacter(rgCharacter.ItemIndex);
  FOptions.NeverFlees := edNeverFlees.Checked;
  FOptions.RewardMessage := edRewardMessage.Text;

  if edRewardArtifact.ItemIndex = -1 then
  begin
    FOptions.RewardArtifact := '';
  end
  else begin
    FOptions.RewardArtifact := edRewardArtifact.SelectedInfo.Identifier;
  end;

  SaveResourceSet(gbReward, FOptions.RewardResources);
end;

procedure TCreatureFrame.VisitMonster(AOptions: TCreatureOptions);
begin
  inherited VisitMonster(AOptions);
  FOptions := AOptions;

  edCount.Value := FOptions.Amount;
  edNoGrowing.Checked := FOptions.NoGrowing;
  edRandomCount.Checked := FOptions.Amount = 0;

  rgCharacter.ItemIndex := Integer(FOptions.Character);
  edNeverFlees.Checked := FOptions.NeverFlees;

  edRewardMessage.Text:=FOptions.RewardMessage;

  ReadResourceSet(gbReward, FOptions.RewardResources);

  edRewardArtifact.FillFromList(ListsManager.ArtifactInfos, AOptions.RewardArtifact);

  UpdateControls;
end;

end.

