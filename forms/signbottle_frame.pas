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
unit signbottle_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, object_options,
  base_object_options_frame;

type

  { TSignBottleFrame }

  TSignBottleFrame = class(TBaseObjectOptionsFrame)
    edDescription: TMemo;
    lbDescription: TLabel;
  strict private
    FObject: TSignBottleOptions;
  public
    procedure VisitSignBottle(AOptions: TSignBottleOptions); override;
  public
    procedure Commit;override;
  end;

implementation

{$R *.lfm}

{ TSignBottleFrame }

procedure TSignBottleFrame.Commit;
begin
  FObject.Text := edDescription.Text;
end;

procedure TSignBottleFrame.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  inherited VisitSignBottle(AOptions);
  FObject := AOptions;
  edDescription.Text := FObject.Text;
end;

end.

