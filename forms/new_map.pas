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
unit new_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ActnList, Map;

type

  { TNewMapForm }

  TNewMapForm = class(TForm)
    act: TActionList;
    actCreate: TAction;
    actCancel: TAction;
    btOk: TButton;
    btCancel: TButton;
    cbSquare: TCheckBox;
    lbWidth: TLabel;
    lbHeight: TLabel;
    lbLevels: TLabel;
    pnButtons: TPanel;
    edWidth: TSpinEdit;
    edHeight: TSpinEdit;
    edLevels: TSpinEdit;
    procedure actCancelExecute(Sender: TObject);
    procedure actCreateExecute(Sender: TObject);
    procedure cbSquareChange(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
  private
    procedure UpdateControls;
  public
    { public declarations }

    function Execute(out AParams: TMapCreateParams): boolean;
  end;


implementation

uses math;

{$R *.lfm}

{ TNewMapForm }

procedure TNewMapForm.cbSquareChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TNewMapForm.actCreateExecute(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TNewMapForm.actCancelExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TNewMapForm.edWidthChange(Sender: TObject);
begin
  if cbSquare.Checked then
  begin
    edHeight.Value := edWidth.Value;
  end;
end;

procedure TNewMapForm.UpdateControls;
var
  dim: Integer;
begin
  edHeight.Enabled := not cbSquare.Checked;

  if cbSquare.Checked then
  begin
    dim := Max(edWidth.Value, edHeight.Value);
    edWidth.Value := dim;
    edHeight.Value:= dim;
  end;
end;

function TNewMapForm.Execute(out AParams: TMapCreateParams): boolean;
begin
  Result := ShowModal() = mrOK;

  if Result then
  begin
    AParams.Height := edHeight.Value;
    AParams.Width := edWidth.Value;
    AParams.Levels := edLevels.Value;
  end;
end;

end.

