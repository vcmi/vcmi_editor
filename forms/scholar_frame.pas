unit scholar_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, base_object_options_frame, gui_helpers, object_options,
  editor_types, editor_consts, lists_manager, base_info;

type

  { TScholarFrame }

  TScholarFrame = class(TBaseObjectOptionsFrame)
    edBonusID: TComboBox;
    edBonusType: TRadioGroup;
    Label1: TLabel;
    procedure edBonusIDChange(Sender: TObject);
    procedure edBonusTypeClick(Sender: TObject);
  private
    FOptions: TScholarOptions;
    FBonusID: AnsiString;

    procedure UpdateControls;
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
    FBonusID:=(edBonusID.Items.Objects[edBonusID.ItemIndex] as TBaseInfo).ID;
  end;
end;

procedure TScholarFrame.UpdateControls;
var
  prim_skill: TPrimarySkill;
begin
  edBonusID.Enabled := edBonusType.ItemIndex <> Integer(TScholarBonus.random);

  edBonusID.Items.Clear;
  edBonusID.ItemIndex := -1;

  case edBonusType.ItemIndex of
    Integer(TScholarBonus.random):
    begin

    end;
    Integer(TScholarBonus.primSkill):
    begin
      edBonusID.FillFromList(ListsManager.PrimSkillMap, FBonusID);
    end;
    Integer(TScholarBonus.skill):
    begin
      edBonusID.FillFromList(ListsManager.SkillMap, FBonusID);
    end;
    Integer(TScholarBonus.spell):
    begin
      edBonusID.FillFromList(ListsManager.SpellMap, FBonusID);
    end;
  end;

  if (edBonusID.Items.Count > 0) and (edBonusID.ItemIndex = -1) then
  begin
    edBonusID.ItemIndex := 0;
  end;
end;

procedure TScholarFrame.Commit;
begin
  FOptions.BonusType:=TScholarBonus(edBonusType.ItemIndex);
  FOptions.BonusId:=FBonusID;
end;

procedure TScholarFrame.VisitScholar(AOptions: TScholarOptions);
begin
  FOptions := AOptions;

  edBonusType.ItemIndex:=Integer(AOptions.BonusType);
  FBonusID := AOptions.BonusId;
  UpdateControls;
end;

end.

