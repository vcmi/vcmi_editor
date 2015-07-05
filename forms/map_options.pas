unit map_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, CheckLst, gui_helpers, Map;

type

  { TMapOptionsForm }

  TMapOptionsForm = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    cbSpellsNegate: TComboBox;
    cbSkillsNegate: TComboBox;
    cbArtifactsNegate: TComboBox;
    edAllowedHeroes: TCheckListBox;
    edSpells: TCheckListBox;
    edAbilities: TCheckListBox;
    edName: TEdit;
    edArtifacts: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lMapName: TLabel;
    lMapDescription: TLabel;
    edDescription: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pcMain: TPageControl;
    edDifficulty: TRadioGroup;
    edLevelLimit: TSpinEdit;
    tsArtifacts: TTabSheet;
    tsHeroes: TTabSheet;
    tsSpells: TTabSheet;
    tsAbilities: TTabSheet;
    tsMain: TTabSheet;
    procedure btOkClick(Sender: TObject);
    procedure cbEnableLevelLimitChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure UpdateControls;
  public
    { public declarations }
    property Map: TVCMIMap read FMap write SetMap;

  end;


implementation

uses editor_types;

{$R *.lfm}

{ TMapOptionsForm }

procedure TMapOptionsForm.btOkClick(Sender: TObject);
begin
  //todo: validate
  //todo: save

  FMap.Difficulty := TDifficulty(edDifficulty.ItemIndex);
  FMap.LevelLimit:=edLevelLimit.Value;

  FMap.Name := edName.Text;
  FMap.Description := edDescription.Text;

  edAbilities.SaveToCondition    (FMap.ListsManager.SkillMap,    Fmap.AllowedAbilities, cbSkillsNegate.ItemIndex = 1);
  edSpells.SaveToCondition       (FMap.ListsManager.SpellMap,    FMap.AllowedSpells, cbSpellsNegate.ItemIndex = 1);
  edArtifacts.SaveToCondition    (FMap.ListsManager.ArtifactMap, FMap.AllowedArtifacts, cbArtifactsNegate.ItemIndex = 1);

  edAllowedHeroes.SaveToCondition(FMap.ListsManager.HeroMap,     FMap.AllowedHeroes, True);

  ModalResult := mrOK;

  Close;
end;

procedure TMapOptionsForm.cbEnableLevelLimitChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMapOptionsForm.FormShow(Sender: TObject);
begin
  pcMain.ActivePage := tsMain;
end;

procedure TMapOptionsForm.ReadData;
begin

  edDifficulty.ItemIndex := Integer(FMap.Difficulty);
  edLevelLimit.Value := FMap.LevelLimit;

  edName.Text := FMap.Name;
  edDescription.Text := FMap.Description;

  edAbilities.FillFromCondition(FMap.ListsManager.SkillMap, FMap.AllowedAbilities);
  cbSkillsNegate.ItemIndex:=ifthen(FMap.AllowedAbilities.IsPermissive, 1, 0);

  edSpells.FillFromCondition(FMap.ListsManager.SpellMap, FMap.AllowedSpells);
  cbSpellsNegate.ItemIndex := ifthen(FMap.AllowedSpells.IsPermissive, 1, 0);

  edArtifacts.FillFromCondition(FMap.ListsManager.ArtifactMap, FMap.AllowedArtifacts);
  cbArtifactsNegate.ItemIndex := ifthen(FMap.AllowedArtifacts.IsPermissive, 1, 0);

  edAllowedHeroes.FillFromCondition(Fmap.ListsManager.HeroMap, FMap.AllowedHeroes);

  UpdateControls;
end;

procedure TMapOptionsForm.UpdateControls;
begin
 //
end;

procedure TMapOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;
  if Assigned( FMap) then ReadData;
end;



end.

