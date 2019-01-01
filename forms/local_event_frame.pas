{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit local_event_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, base_options_frame, gui_helpers, object_options,
  editor_types;

type

  { TLocalEventFrame }

  TLocalEventFrame = class(TBaseOptionsFrame)
    edRemoveAfterVisit: TCheckBox;
    edPlayers: TCheckGroup;
    edActivableBy: TComboBox;
    Label1: TLabel;
    MessageEdit: TMemo;
    MessageLabel: TLabel;
  private
    FOptions: TLocalEventOptions;
  public
    { public declarations }
    procedure VisitLocalEvent(AOptions: TLocalEventOptions); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TLocalEventFrame }

procedure TLocalEventFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
var
  pl: TPlayer;
begin
  FOptions := AOptions;

  inherited VisitLocalEvent(AOptions);

  AddStrEditor(AOptions, 'GuardMessage', MessageEdit);
  AddBoolEditor(AOptions, 'RemoveAfterVisit', edRemoveAfterVisit);

  if FOptions.HumanActivable then
  begin
    if AOptions.AIActivable then
    begin
      edActivableBy.ItemIndex:=2;
    end
    else begin
      edActivableBy.ItemIndex:=0;
    end;
  end
  else begin
    edActivableBy.ItemIndex:=1;
  end;

  for pl in TPlayerColor do
  begin
    edPlayers.Checked[Integer(pl)] := FOptions.AvailableFor * [pl] = [pl];
  end;

  Load();
end;

procedure TLocalEventFrame.Commit;
var
  pl: TPlayer;
begin
  inherited Commit;

  case edActivableBy.ItemIndex of
    0:begin FOptions.HumanActivable := true; FOptions.AIActivable:=False; end;
    1:begin FOptions.HumanActivable := False; FOptions.AIActivable:=True; end;
    2:begin FOptions.HumanActivable := true; FOptions.AIActivable:=True; end;
  end;

  FOptions.AvailableFor := [];

  for pl in TPlayerColor do
  begin
    if edPlayers.Checked[Integer(pl)] then
    begin
      FOptions.AvailableFor := FOptions.AvailableFor + [pl];
    end;
  end;

end;

end.

