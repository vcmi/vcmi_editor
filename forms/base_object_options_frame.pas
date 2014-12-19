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
unit base_object_options_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, gvector, FileUtil, Forms, Controls, object_options,
  lists_manager;

type

  { TBaseObjectOptionsFrame }

  TBaseObjectOptionsFrame = class(TFrame,IObjectOptionsVisitor)
  private
    FListsManager: TListsManager;
    procedure SetListsManager(AValue: TListsManager);
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Commit; virtual;

  public //IObjectOptionsVisitor
    procedure VisitLocalEvent({%H-}AOptions: TLocalEventOptions); virtual;
    procedure VisitSignBottle({%H-}AOptions: TSignBottleOptions);virtual;
    procedure VisitHero({%H-}AOptions: THeroOptions);virtual;
    procedure VisitMonster({%H-}AOptions: TMonsterOptions);virtual;
    procedure VisitSeerHut({%H-}AOptions: TSeerHutOptions);virtual;
    procedure VisitWitchHut({%H-}AOptions: TWitchHutOptions);virtual;
    procedure VisitScholar({%H-}AOptions: TScholarOptions);virtual;
    procedure VisitGarrison({%H-}AOptions: TGarrisonOptions);virtual;
    procedure VisitArtifact({%H-}AOptions: TArtifactOptions);virtual;
    procedure VisitSpellScroll({%H-}AOptions: TSpellScrollOptions);virtual;
    procedure VisitResource({%H-}AOptions: TResourceOptions);virtual;
    procedure VisitTown({%H-}AOptions: TTownOptions);virtual;
    procedure VisitAbandonedMine({%H-}AOptions: TAbandonedOptions); virtual;
    procedure VisitShrine({%H-}AOptions: TShrineOptions);virtual;
    procedure VisitPandorasBox({%H-}AOptions: TPandorasOptions);virtual;
    procedure VisitGrail({%H-}AOptions: TGrailOptions);virtual;
    procedure VisitRandomDwelling({%H-}AOptions: TRandomDwellingOptions);virtual;
    procedure VisitRandomDwellingLVL({%H-}AOptions: TRandomDwellingLVLOptions);virtual;
    procedure VisitRandomDwellingTown({%H-}AOptions: TRandomDwellingTownOptions);virtual;
    procedure VisitQuestGuard({%H-}AOptions:TQuestGuardOptions);virtual;
    procedure VisitHeroPlaseholder({%H-}AOptions: THeroPlaceholderOptions);virtual;

    procedure VisitOwnedObject({%H-}AOptions: TOwnedObjectOptions);virtual;
  public
    property ListsManager: TListsManager read FListsManager write SetListsManager;
  end;

  TBaseObjectOptionsFrameClass = class of TBaseObjectOptionsFrame;

  { TObjectOptionsFrameList }

  TObjectOptionsFrameList = class(specialize TVector<TBaseObjectOptionsFrame>)
  public
    procedure Commit;
  end;

implementation

{ TObjectOptionsFrameList }

procedure TObjectOptionsFrameList.Commit;
var
  i: SizeInt;
begin
  for i := 0 to Size - 1 do
  begin
    Items[i].Commit;
  end;

end;

{$R *.lfm}

{ TBaseObjectOptionsFrame }

procedure TBaseObjectOptionsFrame.Commit;
begin

end;

constructor TBaseObjectOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TBaseObjectOptionsFrame.SetListsManager(AValue: TListsManager);
begin
  if FListsManager = AValue then Exit;
  FListsManager := AValue;
end;

procedure TBaseObjectOptionsFrame.VisitAbandonedMine(AOptions: TAbandonedOptions
  );
begin

end;

procedure TBaseObjectOptionsFrame.VisitArtifact(AOptions: TArtifactOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitGrail(AOptions: TGrailOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitHero(AOptions: THeroOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitHeroPlaseholder(
  AOptions: THeroPlaceholderOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitMonster(AOptions: TMonsterOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions
  );
begin

end;

procedure TBaseObjectOptionsFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitRandomDwelling(
  AOptions: TRandomDwellingOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitResource(AOptions: TResourceOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitScholar(AOptions: TScholarOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitShrine(AOptions: TShrineOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitSignBottle(AOptions: TSignBottleOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitSpellScroll(AOptions: TSpellScrollOptions
  );
begin

end;

procedure TBaseObjectOptionsFrame.VisitTown(AOptions: TTownOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitWitchHut(AOptions: TWitchHutOptions);
begin

end;

end.

