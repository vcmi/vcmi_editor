unit map_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, CheckLst, gui_helpers, Map;

type

  { TMapOptionsForm }

  TMapOptionsForm = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    edSpells: TCheckListBox;
    edAbilities: TCheckListBox;
    edName: TEdit;
    Label1: TLabel;
    lMapName: TLabel;
    lMapDescription: TLabel;
    edDescription: TMemo;
    pcMain: TPageControl;
    edDifficulty: TRadioGroup;
    edLevelLimit: TSpinEdit;
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

  edAbilities.SaveToList(FMap.AllowedAbilities);
  edSpells.SaveToList(FMap.AllowedSpells);

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

  edAbilities.FillFromList(FMap.ListsManager.SkillMap,FMap.AllowedAbilities);
  edSpells.FillFromList(FMap.ListsManager.SpellMap,FMap.AllowedSpells);

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

