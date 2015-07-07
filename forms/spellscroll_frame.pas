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
unit spellscroll_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, object_options, gui_helpers, lists_manager,
  base_object_options_frame;

type

  { TSpellScrollFrame }

  TSpellScrollFrame = class(TBaseObjectOptionsFrame)
    edSpell: TListBox;
    Label1: TLabel;
    edMessage: TMemo;
    pnMessage: TPanel;
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
  FObject.Spell := edSpell.SelectedInfo.ID;
  FObject.GuardMessage:=edMessage.Text;
end;

procedure TSpellScrollFrame.VisitSpellScroll(AOptions: TSpellScrollOptions);
var
  sinfo:TSpellInfo;
begin
  inherited VisitSpellScroll(AOptions);
  FObject := AOptions;
  if AOptions.Spell = '' then
  begin
    sinfo := nil;
  end
  else begin
    sinfo := ListsManager.GetSpell(AOptions.Spell);
  end;

  edSpell.FillFromList(ListsManager.SpellMap,sinfo);

  edMessage.Text:=AOptions.GuardMessage;
end;

end.

