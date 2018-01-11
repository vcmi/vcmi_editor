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
    StandardSize: TComboBox;
    Label1: TLabel;
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
    procedure edHeightChange(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StandardSizeChange(Sender: TObject);
  private
    procedure UpdateControls;

    procedure CheckStandardSize;
  public
    { public declarations }

    function Execute(out AParams: TMapCreateParams): boolean;
  end;


implementation

uses math;

const
  STANDARD_SIZES : array[0..8] of Integer =
  (
    18, 36, 72, 108, 144, 180, 216, 252, 1008
  );
  STANDARD_SIZES_NAMES : array[0..8] of String =
  (
    'Min 18x18',
    'S 36x36',
    'M 72x72',
    'L 108x108',
    'XL 144x144',
    'H 180x180',
    'XH 216x216',
    'G 252x252',
    'Max 1008x1008'
  );

{$R *.lfm}

{ TNewMapForm }

procedure TNewMapForm.cbSquareChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TNewMapForm.edHeightChange(Sender: TObject);
begin
  CheckStandardSize;
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
  end
  else
  begin
    CheckStandardSize;
  end;
end;

procedure TNewMapForm.FormCreate(Sender: TObject);
var
  idx: Integer;
begin
  StandardSize.Items.Clear;

  for idx := Low(STANDARD_SIZES) to High(STANDARD_SIZES) do
  begin
    StandardSize.Items.Add(STANDARD_SIZES_NAMES[idx]);
  end;
end;

procedure TNewMapForm.StandardSizeChange(Sender: TObject);
begin
  if StandardSize.ItemIndex >= 0 then
  begin
    cbSquare.Checked := true;
    edWidth.Value:=STANDARD_SIZES[StandardSize.ItemIndex];
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

procedure TNewMapForm.CheckStandardSize;
var
  idx: Integer;
  found: Boolean;
begin
  if edWidth.Value <> edHeight.Value then
  begin
    StandardSize.ItemIndex:=-1;
    Exit;
  end;

  found := false;

  for idx := Low(STANDARD_SIZES) to High(STANDARD_SIZES) do
  begin
    if edWidth.Value = STANDARD_SIZES[idx] then
    begin
      found := true;
      Break;
    end;
  end;

  if not found then
  begin
    StandardSize.ItemIndex:=-1;
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

