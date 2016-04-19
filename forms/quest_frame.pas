{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit quest_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, base_options_frame, object_options;

type

  { TQuestFrame }

  TQuestFrame = class(TBaseOptionsFrame)
    Label1: TLabel;
  private
    FObject: TQuest;
  protected
    procedure Load; override;
  public
    procedure Commit; override;

    procedure VisitQuestGuard(AOptions: TQuestGuardOptions); override;
    procedure VisitSeerHut(AOptions: TSeerHutOptions); override;
  end;

implementation

{$R *.lfm}

{ TQuestFrame }

procedure TQuestFrame.Commit;
begin
  inherited Commit;
end;

procedure TQuestFrame.Load;
begin

end;

procedure TQuestFrame.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  inherited VisitQuestGuard(AOptions);
  FObject := AOptions.Quest;
  Load;
end;

procedure TQuestFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  inherited VisitSeerHut(AOptions);
  FObject := AOptions.Quest;
  Load;
end;

end.

