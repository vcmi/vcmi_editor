unit monster_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, object_options, base_object_options_frame, gui_helpers;

type

  { TMonsterFrame }

  TMonsterFrame = class(TBaseObjectOptionsFrame)
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
    FOptions: TMonsterOptions;

    procedure UpdateControls;
  public
    procedure Commit; override;
    procedure VisitMonster(AOptions: TMonsterOptions); override;
  end;

implementation

{$R *.lfm}

{ TMonsterFrame }

procedure TMonsterFrame.edRandomCountChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMonsterFrame.UpdateControls;
begin
  edCount.Enabled:=not edRandomCount.Checked;
end;

procedure TMonsterFrame.Commit;
begin
  inherited Commit;

  if edRandomCount.Checked then
  begin
    FOptions.Count := 0;
  end
  else begin
    FOptions.Count := edCount.Value;
  end;

  FOptions.NoGrowing := edNoGrowing.Checked;

  FOptions.Character := rgCharacter.ItemIndex;
  FOptions.NeverFlees := edNeverFlees.Checked;
  FOptions.RewardMessage := edRewardMessage.Text;

  if edRewardArtifact.ItemIndex = -1 then
  begin
    FOptions.RewardArtifact := '';
  end
  else begin
    FOptions.RewardArtifact := ListsManager.ArtifactMap[edRewardArtifact.ItemIndex];
  end;

  SaveResourceSet(gbReward, FOptions.RewardResources);
end;

procedure TMonsterFrame.VisitMonster(AOptions: TMonsterOptions);
begin
  inherited VisitMonster(AOptions);
  FOptions := AOptions;

  edCount.Value := FOptions.Count;
  edNoGrowing.Checked := FOptions.NoGrowing;
  edRandomCount.Checked := FOptions.Count = 0;

  rgCharacter.ItemIndex := FOptions.Character;
  edNeverFlees.Checked := FOptions.NeverFlees;

  edRewardMessage.Text:=FOptions.RewardMessage;

  ReadResourceSet(gbReward, FOptions.RewardResources);

  edRewardArtifact.FillFromList(ListsManager.ArtifactMap, AOptions.RewardArtifact);

  UpdateControls;
end;

end.

