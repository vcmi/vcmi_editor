unit monster_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, object_options, base_object_options_frame;

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

  FOptions.Count := edCount.Value;
  FOptions.NoGrowing := edNoGrowing.Checked;
  FOptions.RandomCount := edRandomCount.Checked;

  FOptions.Character := rgCharacter.ItemIndex;
  FOptions.NeverFlees := edNeverFlees.Checked;
  FOptions.RewardMessage := edRewardMessage.Text;

  SaveResourceSet(gbReward, FOptions.RewardResources);
end;

procedure TMonsterFrame.VisitMonster(AOptions: TMonsterOptions);
begin
  inherited VisitMonster(AOptions);
  FOptions := AOptions;

  edCount.Value := FOptions.Count;
  edNoGrowing.Checked := FOptions.NoGrowing;
  edRandomCount.Checked := FOptions.RandomCount;

  rgCharacter.ItemIndex := FOptions.Character;
  edNeverFlees.Checked := FOptions.NeverFlees;

  edRewardMessage.Text:=FOptions.RewardMessage;

  ReadResourceSet(gbReward, FOptions.RewardResources);

  UpdateControls;
end;

end.

