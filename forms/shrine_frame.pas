{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
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

  if rbRandom.Checked then
  begin
    FObject.SpellID := '';
  end
  else
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

  rbRandom.Checked := FObject.SpellID = '';
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

    if not (FObject.SpellID = '') then
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

