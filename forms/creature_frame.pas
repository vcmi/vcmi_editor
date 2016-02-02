unit creature_frame;

{$mode objfpc}{$H+}

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

