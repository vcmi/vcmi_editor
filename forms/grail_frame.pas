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

unit grail_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, object_options, base_options_frame;

type

  { TGrailFrame }

  TGrailFrame = class(TBaseOptionsFrame)
    lbRadius: TLabel;
    edRadius: TSpinEdit;
  strict private
    FObject: TGrailOptions;
  public
    procedure Commit; override;
    procedure VisitGrail(AOptions: TGrailOptions); override;
  end;

implementation

{$R *.lfm}

{ TGrailFrame }

procedure TGrailFrame.Commit;
begin
  inherited Commit;
  FObject.Radius := edRadius.Value;
end;

procedure TGrailFrame.VisitGrail(AOptions: TGrailOptions);
begin
  inherited VisitGrail(AOptions);
  FObject := AOptions;
  edRadius.Value := AOptions.Radius;
end;

end.

