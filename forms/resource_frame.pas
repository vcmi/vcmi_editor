{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit resource_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, base_options_frame, object_options;

type

  { TResourceFrame }

  TResourceFrame = class(TBaseOptionsFrame)
    cbRandom: TCheckBox;
    GroupAmount: TGroupBox;
    edAmount: TSpinEdit;
    GroupMessage: TGroupBox;
    edMessage: TMemo;
  private
    FOptions: TResourceOptions;
  protected
  public
    procedure Commit; override;
    procedure VisitResource(AOptions: TResourceOptions); override;
  end;

implementation

{$R *.lfm}

{ TResourceFrame }


procedure TResourceFrame.Commit;
begin
  inherited Commit;
  FOptions.GuardMessage := edMessage.Text;
end;

procedure TResourceFrame.VisitResource(AOptions: TResourceOptions);
begin
  FOptions := AOptions;

  AddIntEditor(AOptions, 'Amount', edAmount, cbRandom);

  inherited VisitResource(AOptions);

  edMessage.Text:=AOptions.GuardMessage;

  Load;

  UpdateControls;
end;

end.

