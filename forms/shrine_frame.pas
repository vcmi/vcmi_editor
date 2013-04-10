unit shrine_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gui_helpers, lists_manager,
  base_object_options_frame, object_options;

type

  { TShrineFrame }

  TShrineFrame = class(TBaseObjectOptionsFrame)
    GroupBox1: TGroupBox;
    edSpell: TListBox;
    rbRandom: TRadioButton;
    rbSpecified: TRadioButton;
    procedure rbRandomChange(Sender: TObject);
  strict private
    FObject: TShrineOptions;
    procedure UpdateControls();
  public
    procedure Commit; override;
    procedure VisitShrine(AOptions: TShrineOptions); override;
  end;

implementation

{$R *.lfm}

{ TShrineFrame }

procedure TShrineFrame.Commit;
begin
  inherited Commit;

  FObject.IsRandom := rbRandom.Checked;
  if not FObject.IsRandom then
  begin
    FObject.SpellID := edSpell.SelectedInfo().Id;
  end;
end;

procedure TShrineFrame.rbRandomChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TShrineFrame.UpdateControls;
begin
  edSpell.Enabled := rbSpecified.Checked;

  if (edSpell.ItemIndex < 0) and (edSpell.Items.Count > 0) then
    edSpell.ItemIndex := 0;
end;

procedure TShrineFrame.VisitShrine(AOptions: TShrineOptions);
var
  AviableSpells: TStringList;
  i: Integer;
  sinfo:TSpellInfo;
begin
  inherited VisitShrine(AOptions);
  FObject := AOptions;

  rbRandom.Checked := FObject.IsRandom;
  rbSpecified.Checked := not rbRandom.Checked;
  AviableSpells := TStringList.Create;
  try
    for i := 0 to ListsManager.SpellMap.Count - 1 do
    begin
      sinfo := ListsManager.SpellMap.Objects[i] as TSpellInfo;
      if sinfo.Level = AOptions.SpellLevel then
      begin
        AviableSpells.AddObject(sinfo.ID,sinfo);
      end;
    end;
    sinfo := nil;

    if not FObject.IsRandom then
    begin
      sinfo := ListsManager.GetSpell(AOptions.SpellID);
    end;

    edSpell.FillFromList(AviableSpells,sinfo);
  finally
    AviableSpells.Free;
    UpdateControls();
  end;
end;

end.

