{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit town_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, base_options_frame, object_options, editor_types;

type

  { TTownOptionsFrame }

  TTownOptionsFrame = class(TBaseOptionsFrame)
    NameCustomise: TCheckBox;
    edName: TEdit;
    NameLabel: TLabel;
    OwnerPlaceholder: TLabel;
    OwnerLabel: TLabel;
    edOwner: TRadioGroup;
    TypLabel: TLabel;
    Typ: TLabel;
    TypPlaceholder: TLabel;
    procedure NameCustomiseChange(Sender: TObject);
  private
    FObject: TTownOptions;
    FCustomName: TLocalizedString;

    procedure Load;
  protected
    procedure UpdateControls; override;
  public
    procedure Commit; override;
    procedure VisitTown(AOptions: TTownOptions); override;
  end;

implementation

{$R *.lfm}

{ TTownOptionsFrame }

procedure TTownOptionsFrame.NameCustomiseChange(Sender: TObject);
begin
  UpdateControls;

  DoUpdateText(edName, NameCustomise, FCustomName, '');
end;

procedure TTownOptionsFrame.Load;
begin
  ReadOwner(FObject, edOwner);

  NameCustomise.Checked:=FObject.Name <> '';

  FCustomName:=FObject.Name;

  NameCustomiseChange(NameCustomise);
end;

procedure TTownOptionsFrame.UpdateControls;
begin
  inherited UpdateControls;
  edName.Enabled := NameCustomise.Checked;
end;

procedure TTownOptionsFrame.Commit;
begin
  inherited Commit;
  WriteOwner(FObject, edOwner);
  if NameCustomise.Checked then
  begin
    FObject.Name := edName.Text;
  end
  else
  begin
    FObject.Name := '';
  end;
end;

procedure TTownOptionsFrame.VisitTown(AOptions: TTownOptions);
begin
  ListsManager.FillWithPlayers(edOwner.Items, True);
  inherited VisitTown(AOptions);
  FObject:=AOptions;
  Load;
end;

end.

