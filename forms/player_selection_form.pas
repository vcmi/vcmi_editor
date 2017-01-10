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

unit player_selection_form;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, root_manager, lists_manager, editor_types;

type

  { TPlayerSelectionForm }

  TPlayerSelectionForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btCancel: TButton;
    btOk: TButton;
    edOwnerRG: TRadioGroup;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;

    function SelectedPlayer:TPlayer;
  end;


implementation

{$R *.lfm}

{ TPlayerSelectionForm }

procedure TPlayerSelectionForm.actSaveExecute(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TPlayerSelectionForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

constructor TPlayerSelectionForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  RootManager.ListsManager.FillWithPlayers(edOwnerRG.Items, False);
  edOwnerRG.ItemIndex := 0;
end;

function TPlayerSelectionForm.SelectedPlayer: TPlayer;
begin
  Result := TPlayer(edOwnerRG.ItemIndex);
end;

end.

