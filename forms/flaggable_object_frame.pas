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

unit flaggable_object_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, object_options, base_options_frame;

type

  { TFlaggableFrame }

  TFlaggableFrame = class(TBaseOptionsFrame)
    edOwnerRG: TRadioGroup;
  strict private
    FObject: TObjectOptions;
    procedure SetupControls;
  private
    procedure VisitOptions(AOptions:TObjectOptions);
  public
    { public declarations }
    procedure Commit; override;
    procedure VisitMine(AOptions: TMineOptions); override;
    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions); override;
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions); override;
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions); override;
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions); override;
    procedure VisitGarrison(AOptions: TGarrisonOptions); override;
  end;

implementation


{$R *.lfm}

{ TFlaggableFrame }

procedure TFlaggableFrame.Commit;
begin
  inherited Commit;
  WriteOwner(FObject, edOwnerRG);
end;

procedure TFlaggableFrame.VisitMine(AOptions: TMineOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.SetupControls;
begin
  ListsManager.FillWithPlayers(edOwnerRG.Items, True);
end;

procedure TFlaggableFrame.VisitOptions(AOptions: TObjectOptions);
begin
  SetupControls;
  FObject := AOptions;
  ReadOwner(FObject, edOwnerRG);
end;

procedure TFlaggableFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwelling(AOptions: TRandomDwellingOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin
  VisitOptions(AOptions);
end;

end.

