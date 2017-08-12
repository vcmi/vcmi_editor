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
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types, map_rect, undo_map,
  editor_gl, map_actions, map_object_actions, map_road_river_actions, editor_str_consts;

type

  TEraseTarget = (Roads, Rivers, StaticObjects, InteractiveObjects);
  TEraseFilter = set of TEraseTarget;

  TEraseObjectBy = (BBox, VisibleMask, BlockMask, VisitableMask);

  { TEraseBrush }

  TEraseBrush = class(TMapBrush)
  private
    FFilter: TEraseFilter;
    FObjectFilter: TEraseObjectBy;
  public
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;
    property Filter: TEraseFilter read FFilter write FFilter;
    property ObjectFilter: TEraseObjectBy read FObjectFilter write FObjectFilter;
  end;

  { TFixedEraseBrush }

  TFixedEraseBrush = class(TEraseBrush)
  public
    constructor Create(AOwner: TComponent); override;
    procedure RenderSelection(State: TLocalState); override;
  end;

  { TAreaEraseBrush }

  TAreaEraseBrush = class(TEraseBrush)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  { TEraseAction }

  TEraseAction = class (TMultiTileMapAction)
  private
    FEraseRoad: TEditRoad;
    FEraseRiver: TEditRiver;
    FEraseObjects: TDeleteObjects;
    FFilter: TEraseFilter;
    FActiveFilter: TEraseFilter;

    FTiles: TCoordSet;

    FActions: array[TEraseTarget] of TMultiTileMapAction;
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure AddTile(X, Y: integer); override;

    function GetDescription: string; override;
    function Execute: boolean; override;
    procedure Redo; override;
    procedure Undo; override;

    property Filter: TEraseFilter read FFilter write FFilter;
  end;

implementation

{ TEraseBrush }

procedure TEraseBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
var
  item: TEraseAction;
begin
  if not Selection.IsEmpty then
  begin
    item :=  TEraseAction.Create(AMap);
    item.Filter:=Filter;
    item.LoadTiles(Selection);
    AManager.ExecuteItem(item);
  end;

  Clear;
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
  FTiles :=  TCoordSet.Create;
end;

destructor TEraseAction.Destroy;
begin
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

function TEraseAction.GetDescription: string;
begin
  Result := rsEraseDescription;
end;

function TEraseAction.Execute: boolean;
var
  iter: TEraseTarget;
begin
  FActiveFilter := Filter;
  Result := FActiveFilter <> [];

  for iter in TEraseTarget do
    FActions[iter] := nil;

  FActions[TEraseTarget.Rivers] := FEraseRiver;

  FActions[TEraseTarget.Roads] := FEraseRoad;

  FActions[TEraseTarget.InteractiveObjects] := nil;//todo

  FActions[TEraseTarget.StaticObjects] := nil;//todo

  for iter in FActiveFilter do
    if Assigned(FActions[iter]) then
    begin
      FActions[iter].LoadTiles(FTiles);
      FActions[iter].Execute;
    end;
end;

procedure TEraseAction.Redo;
var
  iter: TEraseTarget;
begin
  for iter in FActiveFilter do
    if Assigned(FActions[iter]) then
      FActions[iter].Redo;
end;

procedure TEraseAction.Undo;
var
  iter: TEraseTarget;
begin
  for iter in FActiveFilter do
    if Assigned(FActions[iter]) then
      FActions[iter].Undo;
end;

{ TFixedEraseBrush }

constructor TFixedEraseBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size:=1;
  SetMode(TBrushMode.fixed);
end;

procedure TFixedEraseBrush.RenderSelection(State: TLocalState);
begin
  RenderSelectionAllTiles(State);
end;

end.

