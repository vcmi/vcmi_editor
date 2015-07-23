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
  Classes, SysUtils, gvector, FileUtil,  LCLType,  Forms, Controls, Spin, Grids,
  editor_types,
  object_options, map,
  lists_manager, editor_consts;

type

  { TBaseObjectOptionsFrame }

  TBaseObjectOptionsFrame = class(TFrame,IObjectOptionsVisitor)
  private
    FListsManager: TListsManager;
    FMap: TVCMIMap;
    procedure SetListsManager(AValue: TListsManager);
    procedure SetMap(AValue: TVCMIMap);
  protected
    procedure ReadResourceSet(AParentControl: TWinControl; ASrc: TResourceSet);
    procedure SaveResourceSet(AParentControl: TWinControl; ADest: TResourceSet);

    procedure VisitNormalHero({%H-}AOptions: THeroOptions);virtual;
    procedure VisitRandomHero({%H-}AOptions: THeroOptions);virtual;
    procedure VisitPrison({%H-}AOptions: THeroOptions);virtual;

    procedure HandleStringGridKeyDown(Sender: TObject;  var Key: Word; Shift: TShiftState);
    procedure HandleStringGridResize(Sender: TObject);
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
  public //map options

    procedure VisitHeroOptions({%H-}AOptions: THeroDefinition); virtual;

  public
    property ListsManager: TListsManager read FListsManager write SetListsManager;
    property Map: TVCMIMap read FMap write SetMap;
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

procedure TBaseObjectOptionsFrame.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TBaseObjectOptionsFrame.ReadResourceSet(AParentControl: TWinControl;
  ASrc: TResourceSet);
var
  res_type: TResType;
  c: TControl;
  res_name: String;
  editor: TCustomSpinEdit;
begin
  for res_type in TResType do
  begin
    res_name := RESOURCE_NAMES[res_type];

    c := AParentControl.FindChildControl('ed'+res_name);

    if not Assigned(c) then
    begin
      Assert(false, 'no editor control for '+res_name);
    end;

    if not (c is TCustomSpinEdit) then
    begin
      Assert(false, 'wrong conrol class for '+res_name+ ' '+c.ClassName);
    end;

    editor := TCustomSpinEdit(c);

    editor.Value := ASrc.Amount[res_type];
  end;
end;

procedure TBaseObjectOptionsFrame.SaveResourceSet(AParentControl: TWinControl;
  ADest: TResourceSet);
var
  res_type: TResType;
  c: TControl;
  res_name: String;
  editor: TCustomSpinEdit;
begin
  for res_type in TResType do
  begin
    res_name := RESOURCE_NAMES[res_type];

    c := AParentControl.FindChildControl('ed'+res_name);

    if not Assigned(c) then
    begin
      Assert(false, 'no editor control for '+res_name);
    end;

    if not (c is TCustomSpinEdit) then
    begin
       Assert(false, 'wrong conrol class for '+res_name+ ' '+c.ClassName);
    end;

    editor := TCustomSpinEdit(c);

    ADest.Amount[res_type] := editor.Value;
  end;

end;

procedure TBaseObjectOptionsFrame.VisitNormalHero(AOptions: THeroOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitRandomHero(AOptions: THeroOptions);
begin

end;

procedure TBaseObjectOptionsFrame.VisitPrison(AOptions: THeroOptions);
begin

end;

procedure TBaseObjectOptionsFrame.HandleStringGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (key = VK_DELETE) and (Shift=[]) then
  begin
    (Sender as TCustomStringGrid).DeleteRow((Sender as TCustomStringGrid).Row);
    Exit;
  end;

  if (key = VK_INSERT) and (Shift=[]) then
  begin
    (Sender as TCustomStringGrid).InsertColRow(false, (Sender as TCustomStringGrid).Row+1);
    Exit;
  end;
end;

procedure TBaseObjectOptionsFrame.HandleStringGridResize(Sender: TObject);
var
  grid: TCustomStringGrid;
begin
  grid := Sender as TCustomStringGrid;

  if grid.EditorMode then
    grid.Editor.BoundsRect := grid.CellRect(grid.Col,grid.Row);
end;

procedure TBaseObjectOptionsFrame.VisitAbandonedMine(AOptions: TAbandonedOptions
  );
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitArtifact(AOptions: TArtifactOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitGrail(AOptions: TGrailOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitHero(AOptions: THeroOptions);
begin
  case AOptions.MapObject.GetID of
    TYPE_HERO: VisitNormalHero(AOptions);
    TYPE_PRISON: VisitPrison(AOptions);
    TYPE_RANDOMHERO: VisitRandomHero(AOptions);
  end;
end;

procedure TBaseObjectOptionsFrame.VisitHeroPlaseholder(
  AOptions: THeroPlaceholderOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitLocalEvent(AOptions: TLocalEventOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitMonster(AOptions: TMonsterOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions
  );
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitHeroOptions(AOptions: THeroDefinition);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitPandorasBox(AOptions: TPandorasOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitRandomDwelling(
  AOptions: TRandomDwellingOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitResource(AOptions: TResourceOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitScholar(AOptions: TScholarOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitSeerHut(AOptions: TSeerHutOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitShrine(AOptions: TShrineOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitSpellScroll(AOptions: TSpellScrollOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitTown(AOptions: TTownOptions);
begin
  //do nothig
end;

procedure TBaseObjectOptionsFrame.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  //do nothig
end;

end.

