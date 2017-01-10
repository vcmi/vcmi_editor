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
unit witchhut_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, gui_helpers, base_options_frame, object_options;

type

  { TWitchHutFrame }

  TWitchHutFrame = class(TBaseOptionsFrame)
    edAllowedSkills: TCheckListBox;
    lbAllowedSkills: TLabel;
    procedure edAllowedSkillsClickCheck(Sender: TObject);
  private
    FObject: TWitchHutOptions;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure VisitWitchHut(AOptions: TWitchHutOptions); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TWitchHutFrame }

procedure TWitchHutFrame.Commit;
begin
  inherited Commit;
  //todo: allow permissive mode
  edAllowedSkills.SaveTo(FObject.AllowedSkills, False);
end;

constructor TWitchHutFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TWitchHutFrame.edAllowedSkillsClickCheck(Sender: TObject);
var
  i, first_checked, last_checked: Integer;
begin
  first_checked := -1;
  last_checked := -1;
  for i := 0 to edAllowedSkills.Count - 1 do
  begin
    if edAllowedSkills.State[i] = cbChecked then
    begin
      if first_checked = -1 then
        first_checked := i;
      last_checked := i;
    end;
  end;

  //should not hapend but ...
  if first_checked = -1 then
  begin
    edAllowedSkills.State[0] := cbChecked;
    first_checked := 0;
    last_checked := 0;
  end;

  //disallow all uncheked
  if first_checked = last_checked then
  begin
    edAllowedSkills.ItemEnabled[first_checked] := False;
  end
  else
  begin
    for i := 0 to edAllowedSkills.Count - 1 do
    begin
      edAllowedSkills.ItemEnabled[i] := True;
    end
  end;
end;

procedure TWitchHutFrame.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  inherited VisitWitchHut(AOptions);
  FObject := AOptions;

  edAllowedSkills.FillFrom(ListsManager.SkillInfos, AOptions.AllowedSkills);

  edAllowedSkillsClickCheck(nil);
end;

end.

