{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit shrine_frame;

{$I compilersetup.inc}

{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gui_helpers, lists_manager,
  base_options_frame, object_options, base_info;

type

  { TShrineFrame }

  TShrineFrame = class(TBaseOptionsFrame)
    GroupBox1: TGroupBox;
    edSpell: TListBox;
    rbRandom: TRadioButton;
    rbSpecified: TRadioButton;
    procedure rbRandomChange(Sender: TObject);
  strict private
    FObject: TShrineOptions;
  protected
    procedure UpdateControls; override;
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

  if rbRandom.Checked then
  begin
    FObject.Spell := '';
  end
  else
  begin
    FObject.Spell := edSpell.SelectedIdentifier;
  end;
end;

procedure TShrineFrame.rbRandomChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TShrineFrame.UpdateControls;
begin
  inherited UpdateControls;
  edSpell.Enabled := rbSpecified.Checked;

  if (edSpell.ItemIndex < 0) and (edSpell.Items.Count > 0) then
    edSpell.ItemIndex := 0;
end;

procedure TShrineFrame.VisitShrine(AOptions: TShrineOptions);

  function spell_filter(ATarget:TBaseInfo ): Boolean;
  begin
    Result := TSpellInfo(ATarget).Level = AOptions.SpellLevel;
  end;

begin
  inherited VisitShrine(AOptions);
  FObject := AOptions;

  rbRandom.Checked := FObject.Spell = '';
  rbSpecified.Checked := not rbRandom.Checked;


  edSpell.FillFromList(ListsManager.SpellInfos, FObject.Spell, @spell_filter);

  UpdateControls();

end;

end.

