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
unit message_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, 
    base_options_frame, object_options;

type

  { TMessageFrame }

  TMessageFrame = class(TBaseOptionsFrame)
    edMessage: TMemo;
    lbMessage: TLabel;
  public
    procedure Commit; override;

    function IsDirty: Boolean; override;

    procedure VisitArtifact(AOptions: TArtifactOptions); override;
    procedure VisitPandorasBox(AOptions: TPandorasOptions); override;
    procedure VisitSignBottle(AOptions: TSignBottleOptions); override;
  end;

implementation

{$R *.lfm}

{ TMessageFrame }

procedure TMessageFrame.Commit;
begin
  inherited Commit;
end;

function TMessageFrame.IsDirty: Boolean;
begin
  Result:=InternalIsDirty;
end;

procedure TMessageFrame.VisitArtifact(AOptions: TArtifactOptions);
begin
  inherited VisitArtifact(AOptions);
  AddStrEditor(AOptions, 'GuardMessage', edMessage);
  Load;
end;

procedure TMessageFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  inherited VisitPandorasBox(AOptions);
  AddStrEditor(AOptions, 'GuardMessage', edMessage);
  Load;
end;

procedure TMessageFrame.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  inherited VisitSignBottle(AOptions);
  AddStrEditor(AOptions, 'Text', edMessage);
  Load;
end;

end.

