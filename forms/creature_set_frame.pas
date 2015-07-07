unit creature_set_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, base_object_options_frame, gui_helpers, object_options,
  lists_manager;

type

  { TCreatureSetFrame }

  TCreatureSetFrame = class(TBaseObjectOptionsFrame)
    edRemovableUnits: TCheckBox;
    edTune: TCheckBox;
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
    procedure edTuneChange(Sender: TObject);
  private
    FOptions: TCreatureSet;

    FGarrisonOptions: TGarrisonOptions;

    FCellTypes: array of TCustomComboBox;
    FCellAmounts: array of TCustomSpinEdit;

    procedure UpdateControls;
  protected
    procedure Load(ASrc: TCreatureSet);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; override;

    procedure VisitGarrison(AOptions: TGarrisonOptions); override;
    procedure VisitArtifact(AOptions: TArtifactOptions); override;
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions); override;
  end;

implementation

{$R *.lfm}

{ TCreatureSetFrame }

procedure TCreatureSetFrame.edTuneChange(Sender: TObject);
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
  disableAll := not edTune.Checked;

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
begin
  FOptions := ASrc;

  edTune.Checked:=ASrc.Count>0;

  for cell_number in [0..6] do
  begin
    if cell_number < ASrc.Count then
    begin
      inst_info := ASrc[cell_number];

      FCellTypes[cell_number].FillFromListWithEmptyOption(ListsManager.CreatureMap, inst_info.&type);
      FCellAmounts[cell_number].Value:=inst_info.Amount;
    end
    else begin
      FCellTypes[cell_number].FillFromListWithEmptyOption(ListsManager.CreatureMap, '');
      FCellAmounts[cell_number].Value:=0;
    end;
  end;

  UpdateControls;
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

  FOptions.Clear;

  if not edTune.Checked then
  begin
    Exit; //not tuned army is empty collection
  end;

  for cell_number in [0..6] do
  begin
    inst_info :=  FOptions.Add;
    inst_info.&type := FCellTypes[cell_number].GetValueWithEmptyOption(ListsManager.CreatureMap);

    if inst_info.IsEmptyType() then
    begin
      inst_info.Amount:=0;
    end
    else begin
      inst_info.Amount:=FCellAmounts[cell_number].Value;
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

end.

