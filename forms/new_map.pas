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
unit new_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Map;

type

  { TNewMapForm }

  TNewMapForm = class(TForm)
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
  private
    { private declarations }
  public
    { public declarations }

    function Execute(var AParams: TMapCreateParams): boolean;
  end;


implementation

{$R *.lfm}

{ TNewMapForm }

function TNewMapForm.Execute(var AParams: TMapCreateParams): boolean;
begin
  Result := ShowModal = btOk.ModalResult;

  if Result then
  begin
    AParams.Height := edHeight.Value;
    AParams.Width := edWidth.Value;
    AParams.Levels := edLevels.Value;
  end;
end;

end.

