unit creature_set_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Spin, base_options_frame, gui_helpers, object_options,
  lists_manager, editor_str_consts;

type

  { TCreatureSetFrame }

  TCreatureSetFrame = class(TBaseOptionsFrame)
    edTightFormation: TCheckBox;
    edRemovableUnits: TCheckBox;
    edCustomize: TCheckBox;
    edCell1: TComboBox;
    edCell2: TComboBox;
    edCell3: TComboBox;
    edCell5: TComboBox;
    edCell4: TComboBox;
    edCell6: TComboBox;
    edCell7: TComboBox;
    lbType: TLabel;
    lbAmount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnOptions: TPanel;
    pnCreatures: TPanel;
    edCell1Amount: TSpinEdit;
    edCell2Amount: TSpinEdit;
    edCell3Amount: TSpinEdit;
    edCell4Amount: TSpinEdit;
    edCell5Amount: TSpinEdit;
    edCell6Amount: TSpinEdit;
    edCell7Amount: TSpinEdit;
    procedure CreatureTypeChange(Sender: TObject);
    procedure edCustomizeChange(Sender: TObject);
  private
    FOptions: TCreatureSet;

    FGarrisonOptions: TGarrisonOptions;

    FCellTypes: array of TCustomComboBox;
    FCellAmounts: array of TCustomSpinEdit;

    FAllowRandom: Boolean;
  protected
    procedure UpdateControls; override;

    procedure Load(ASrc: TCreatureSet);

    procedure VisitNormalHero(AOptions: THeroOptions); override;
    procedure VisitPrison(AOptions: THeroOptions); override;
    procedure VisitRandomHero(AOptions: THeroOptions); override;

    procedure VisitNormalTown(AOptions: TTownOptions); override;
    procedure VisitRandomTown(AOptions: TTownOptions); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;

    procedure VisitGarrison(AOptions: TGarrisonOptions); override;
    procedure VisitArtifact(AOptions: TArtifactOptions); override;
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions); override;
    procedure VisitResource(AOptions: TResourceOptions); override;
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure VisitLocalEvent(AOptions: TLocalEventOptions); override;
    procedure VisitHero(AOptions: THeroOptions); override;
    procedure VisitTown(AOptions: TTownOptions); override;
  end;

implementation

{$R *.lfm}

{ TCreatureSetFrame }

procedure TCreatureSetFrame.edCustomizeChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TCreatureSetFrame.CreatureTypeChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TCreatureSetFrame.UpdateControls;
var
  cell_number: Integer;
  disableAll: Boolean;
begin
  inherited UpdateControls;
  disableAll := not edCustomize.Checked;

  for cell_number in [0..6] do
  begin
    FCellTypes[cell_number].Enabled:= not disableAll;
    FCellAmounts[cell_number].Enabled:= (not disableAll) and (FCellTypes[cell_number].ItemIndex>0);
  end;
end;

procedure TCreatureSetFrame.Load(ASrc: TCreatureSet);
var
  cell_number: Integer;

  inst_info: TCreatureInstInfo;
  customised: Boolean;
  creature_level: Integer;
  shift: Integer;
begin
  FOptions := ASrc;

  customised :=  ASrc.Count>0;

  edCustomize.Checked:=customised;

  for cell_number in [0..6] do
  begin
    if cell_number < ASrc.Count then
    begin
      inst_info := ASrc[cell_number];

      FCellTypes[cell_number].FillFromListWithEmptyOption(ListsManager.CreatureInfos, inst_info.&type);
      FCellAmounts[cell_number].Value:=inst_info.Amount;
    end
    else begin
      FCellTypes[cell_number].FillFromListWithEmptyOption(ListsManager.CreatureInfos, '');
      FCellAmounts[cell_number].Value:=0;
    end;
  end;

  if FAllowRandom then
  begin
    for cell_number in [0..6] do
    begin
      for creature_level := 1 to 7 do
      begin
        FCellTypes[cell_number].Items.Add(Format(rsRandomCreatureName,[creature_level]));
        FCellTypes[cell_number].Items.Add(Format(rsRandomCreatureNameUpgrade,[creature_level]));
      end;
      if (cell_number < ASrc.Count) and (ASrc[cell_number].Amount > 0) and (ASrc[cell_number].&type = '') then
      begin
        inst_info := ASrc[cell_number];
        FCellTypes[cell_number].ItemIndex:=FCellTypes[cell_number].Items.Count - 1 - 13 + inst_info.RawRandom;
      end;
    end;
  end;

  if edTightFormation.Visible then
  begin
    edTightFormation.Checked:=ASrc.TightFormation
  end;

  UpdateControls;
end;

procedure TCreatureSetFrame.VisitNormalHero(AOptions: THeroOptions);
begin
  inherited VisitNormalHero(AOptions);
  FAllowRandom := False;
end;

procedure TCreatureSetFrame.VisitPrison(AOptions: THeroOptions);
begin
  inherited VisitPrison(AOptions);
  FAllowRandom := False;
end;

procedure TCreatureSetFrame.VisitRandomHero(AOptions: THeroOptions);
begin
  inherited VisitRandomHero(AOptions);
  FAllowRandom := True;
end;

procedure TCreatureSetFrame.VisitNormalTown(AOptions: TTownOptions);
begin
  inherited VisitNormalTown(AOptions);
  FAllowRandom := False;
end;

procedure TCreatureSetFrame.VisitRandomTown(AOptions: TTownOptions);
begin
  inherited VisitRandomTown(AOptions);
  FAllowRandom := True;
end;

constructor TCreatureSetFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetLength(FCellAmounts, 7);

  FCellAmounts[0] := edCell1Amount;
  FCellAmounts[1] := edCell2Amount;
  FCellAmounts[2] := edCell3Amount;
  FCellAmounts[3] := edCell4Amount;
  FCellAmounts[4] := edCell5Amount;
  FCellAmounts[5] := edCell6Amount;
  FCellAmounts[6] := edCell7Amount;

  SetLength(FCellTypes, 7);

  FCellTypes[0] := edCell1;
  FCellTypes[1] := edCell2;
  FCellTypes[2] := edCell3;
  FCellTypes[3] := edCell4;
  FCellTypes[4] := edCell5;
  FCellTypes[5] := edCell6;
  FCellTypes[6] := edCell7;

end;

procedure TCreatureSetFrame.Commit;
var
  cell_number: Integer;

  inst_info: TCreatureInstInfo;
begin
  inherited Commit;

  if Assigned(FGarrisonOptions) then
  begin
    FGarrisonOptions.RemovableUnits:=edRemovableUnits.Checked;
  end;

  if edTightFormation.Visible then
  begin
    FOptions.TightFormation := edTightFormation.Checked;
  end;

  FOptions.Clear;

  if not edCustomize.Checked then
  begin
    Exit; //not tuned army is empty collection
  end;

  for cell_number in [0..6] do
  begin
    inst_info :=  FOptions.Add;
    inst_info.&type := FCellTypes[cell_number].SelectedIdentifier();

    //index 0 = empty slot, empty slot is marked by zero amount
    if FCellTypes[cell_number].ItemIndex = 0 then
    begin
      inst_info.Amount:=0;
    end
    else
    begin
      inst_info.Amount:=FCellAmounts[cell_number].Value;

      if (inst_info.&type = '') and FAllowRandom then
      begin
        inst_info.RawRandom:=13 - (FCellTypes[cell_number].Items.Count - FCellTypes[cell_number].ItemIndex - 1);
      end;
    end;
  end;
end;

procedure TCreatureSetFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin
  inherited VisitGarrison(AOptions);

  Load(AOptions.Army);

  FGarrisonOptions := AOptions;

  edRemovableUnits.Checked:=AOptions.RemovableUnits;
  edRemovableUnits.Visible := True;
end;

procedure TCreatureSetFrame.VisitArtifact(AOptions: TArtifactOptions);
begin
  inherited VisitArtifact(AOptions);
  Load(AOptions.Guards);
end;

procedure TCreatureSetFrame.VisitSpellScroll(AOptions: TSpellScrollOptions);
begin
  inherited VisitSpellScroll(AOptions);
  Load(AOptions.Guards);
end;

procedure TCreatureSetFrame.VisitResource(AOptions: TResourceOptions);
begin
  inherited VisitResource(AOptions);
  Load(AOptions.Guards);
end;

procedure TCreatureSetFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  inherited VisitPandorasBox(AOptions);
  Load(AOptions.Guards);
end;

procedure TCreatureSetFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  inherited VisitLocalEvent(AOptions);
  Load(AOptions.Guards);
end;

procedure TCreatureSetFrame.VisitHero(AOptions: THeroOptions);
begin
  inherited VisitHero(AOptions);
  Load(AOptions.Army);
  edTightFormation.Visible := true;
end;

procedure TCreatureSetFrame.VisitTown(AOptions: TTownOptions);
begin
  inherited VisitTown(AOptions);
  Load(AOptions.Army);
  edTightFormation.Visible := true;
end;

end.

