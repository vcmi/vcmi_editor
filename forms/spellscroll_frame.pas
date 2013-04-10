unit spellscroll_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  object_options, gui_helpers, lists_manager,
    base_object_options_frame;

type

  { TSpellScrollFrame }

  TSpellScrollFrame = class(TBaseObjectOptionsFrame)
    edSpell: TListBox;
  private
    FObject: TSpellScrollOptions;
  public
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TSpellScrollFrame }

procedure TSpellScrollFrame.Commit;
begin
  inherited Commit;
  FObject.SpellID := (edSpell.SelectedInfo as TSpellInfo).ID;
end;

procedure TSpellScrollFrame.VisitSpellScroll(AOptions: TSpellScrollOptions);
var
  sinfo:TSpellInfo;
begin
  inherited VisitSpellScroll(AOptions);
  FObject := AOptions;
  if AOptions.SpellID = '' then
  begin
    sinfo := nil;
  end
  else begin
    sinfo := ListsManager.GetSpell(AOptions.SpellID);
  end;

  edSpell.FillFromList(ListsManager.SpellMap,sinfo);
end;

end.

