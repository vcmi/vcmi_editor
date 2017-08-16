{ This file is a part of Map editor for VCMI project

  Copyright (C) 2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_erase_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, fgl, Map, gset, undo_base, editor_types, map_rect, undo_map,
  editor_gl, map_actions, map_object_actions, map_road_river_actions, editor_str_consts;

type

  TEraseTarget = (Roads, Rivers, StaticObjects, InteractiveObjects);
  TEraseFilter = set of TEraseTarget;



  { TEraseBrush }

  TEraseBrush = class(TMapBrush)
  strict private
    FFilter: TEraseFilter;
    FObjectFilter: TSelectObjectBy;

    FSelectedObjects: TMapObjectSet;
    FVisibleObjects: TMapObjectsSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ClearSelection; override;
    procedure CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord); override;
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;
    procedure RenderSelection(State: TLocalState); override;
    property Filter: TEraseFilter read FFilter write FFilter;
    property ObjectFilter: TSelectObjectBy read FObjectFilter write FObjectFilter;
    property VisibleObjects: TMapObjectsSelection read FVisibleObjects write FVisibleObjects;
  end;

  { TFixedEraseBrush }

  TFixedEraseBrush = class(TEraseBrush)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TAreaEraseBrush }

  TAreaEraseBrush = class(TEraseBrush)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TActiveActions = specialize TFPGObjectList<TMapUndoItem>;

  { TEraseAction }

  TEraseAction = class (TMultiTileMapAction)
  private
    FEraseRoad: TEditRoad;
    FEraseRiver: TEditRiver;
    FEraseObjects: TDeleteObjects;
    FFilter: TEraseFilter;
    FActiveFilter: TEraseFilter;

    FTiles: TCoordSet;

    FActiveActions: TActiveActions;

  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure AddTile(X, Y: integer); override;
    procedure AddObject(AObject: TMapObject);

    function GetDescription: string; override;
    function Execute: boolean; override;
    procedure Redo; override;
    procedure Undo; override;

    property Filter: TEraseFilter read FFilter write FFilter;
  end;

implementation

{ TEraseBrush }

constructor TEraseBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedObjects := TMapObjectSet.Create;
end;

destructor TEraseBrush.Destroy;
begin
  FSelectedObjects.Free;
  inherited Destroy;
end;

procedure TEraseBrush.Clear;
begin
  inherited Clear;
end;

procedure TEraseBrush.ClearSelection;
begin
  inherited;
  FSelectedObjects.free;
  FSelectedObjects := TMapObjectSet.Create;
end;

procedure TEraseBrush.CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord);
begin
  inherited CheckAddOneTile(AMap, Coord);

  if Assigned(FVisibleObjects) and (([TEraseTarget.StaticObjects, TEraseTarget.InteractiveObjects] * Filter) <> []) then
  begin
    AMap.SelectObjectsOnTile(FVisibleObjects.Data, AMap.CurrentLevelIndex, Coord.X, Coord.Y, FSelectedObjects, FObjectFilter);
  end;
end;

procedure TEraseBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
var
  item: TEraseAction;
  it: TMapObjectSet.TIterator;
begin
  if not Selection.IsEmpty or not FSelectedObjects.IsEmpty then
  begin
    item := TEraseAction.Create(AMap);
    item.Filter:=Filter;
    item.LoadTiles(Selection);

    it := FSelectedObjects.Min;

    if Assigned(it) then
    begin
      repeat
        item.AddObject(it.Data);
      until not it.Next;
      FreeAndNil(it);
    end;

    AManager.ExecuteItem(item);
  end;

  Clear;
end;

procedure TEraseBrush.RenderSelection(State: TLocalState);
var
  it: TMapObjectSet.TIterator;
begin
  inherited RenderSelection(State);

  it := FSelectedObjects.Min;

  if Assigned(it) then
  begin
    repeat
      it.Data.RenderSelectionRect(State);
    until not it.Next;
    FreeAndNil(it);
  end;
end;

{ TAreaEraseBrush }

constructor TAreaEraseBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size:=1;
  SetMode(TBrushMode.area);
end;

{ TEraseAction }

constructor TEraseAction.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FEraseRiver := TEditRiver.Create(AMap, TRiverType.noRiver);
  FEraseRoad := TEditRoad.Create(AMap, TRoadType.noRoad);
  FEraseObjects := TDeleteObjects.Create(AMap);

  FActiveActions := TActiveActions.Create(false);
  FTiles :=  TCoordSet.Create;
end;

destructor TEraseAction.Destroy;
begin
  FActiveActions.Free;
  FTiles.Free;
  FEraseObjects.Free;
  FEraseRiver.Free;
  FEraseRoad.Free;
  inherited Destroy;
end;

procedure TEraseAction.AddTile(X, Y: integer);
var
  coord: TMapCoord;
begin
  coord.Reset(x,y);
  FTiles.Insert(coord);
end;

procedure TEraseAction.AddObject(AObject: TMapObject);
var
  add: Boolean = false;
begin

  if TEraseTarget.StaticObjects in Filter then
  begin
    if Assigned(AObject.MapObjectGroup) and (AObject.MapObjectGroup.Handler = 'static') then
      add := true;
  end
  else if TEraseTarget.InteractiveObjects in Filter then
  begin
    if Assigned(AObject.Template) and AObject.Template.Visitable then
      add := true;
  end;
  if add then
    FEraseObjects.Targets.Add(AObject);
end;

function TEraseAction.GetDescription: string;
begin
  Result := rsEraseDescription;
end;

function TEraseAction.Execute: boolean;
var
  iter: TMapUndoItem;
begin
  FActiveFilter := Filter;
  Result := FActiveFilter <> [];

  if TEraseTarget.Roads in FActiveFilter then
  begin
    FEraseRoad.LoadTiles(FTiles);
    FActiveActions.Add(FEraseRoad);
  end;

  if TEraseTarget.Rivers in FActiveFilter then
  begin
    FEraseRiver.LoadTiles(FTiles);
    FActiveActions.Add(FEraseRiver);
  end;

  if ([TEraseTarget.StaticObjects, TEraseTarget.InteractiveObjects] * FActiveFilter) <> [] then
  begin
    //objects already loaded
    FActiveActions.Add(FEraseObjects);
  end;

  for iter in FActiveActions do
    iter.Execute;
end;

procedure TEraseAction.Redo;
var
  iter: TMapUndoItem;
begin
  for iter in FActiveActions do
  begin
    iter.Redo;
    iter.State := TUndoItemState.ReDone;
  end;
end;

procedure TEraseAction.Undo;
var
  iter: TMapUndoItem;
begin
  for iter in FActiveActions do
  begin
    iter.Undo;
    iter.State := TUndoItemState.UnDone;
  end;
end;

{ TFixedEraseBrush }

constructor TFixedEraseBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size:=1;
  SetMode(TBrushMode.fixed);
end;


end.

