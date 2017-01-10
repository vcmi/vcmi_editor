{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_road_river_actions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, gset, gvector, undo_base, undo_map, Map, editor_types, editor_gl, map_actions,
  transitions, road_transitions, map_rect, editor_consts;

const
  INVALID_COORDINATE: TMapCoord = (x:-1; y:-1);
type

  { TRoadRiverBrush }

  TRoadRiverBrushKind = (road, river);

  TRoadRiverBrush = class(TMapBrush)
  private
    FKind: TRoadRiverBrushKind;
    FRiverType: TRiverType;
    FRoadType: TRoadType;
    FLastPoint: TMapCoord;
    procedure SetKind(AValue: TRoadRiverBrushKind);
    procedure SetRiverType(AValue: TRiverType);
    procedure SetRoadType(AValue: TRoadType);
  protected
    procedure AddTile(AMap: TVCMIMap; AX, AY: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);override;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap; X,Y: integer); override;

    procedure RenderSelection(State: TLocalState); override;

    property RoadType: TRoadType read FRoadType write SetRoadType;
    property RiverType: TRiverType read FRiverType write SetRiverType;
    property Kind:TRoadRiverBrushKind read FKind write SetKind;
  end;

  PTileRoadInfo = ^TTileRoadInfo;
  TTileRoadInfo = record
    X,Y: integer;
    RoadType: TRoadType;
    RoadDir: UInt8;
    RoadFlip: UInt8;

    RiverType: TRiverType;
    RiverDir: UInt8;
    RiverFlip: UInt8;
  end;

  TRoadInfoLess = specialize TGLessCoord <TTileRoadInfo>;

  TTileRoadInfoSet = specialize TSet<TTileRoadInfo,TRoadInfoLess>;
  TTileRoadInfos = specialize TVector<TTileRoadInfo>;

  { TEditRoadRiver }

  TEditRoadRiver = class abstract (TMultiTileMapAction)
  strict private
    procedure UpdateTiles();
    function ValidateTile(info: PTileRoadInfo; pattern: TSimplePattern): TValidationResult;
  protected
    FInQueue: TTileRoadInfoSet;
    FOutQueue : TTileRoadInfoSet;
    FNewTileInfos,
    FOldTileInfos: TTileRoadInfos;

    //no checking, with changes
    function GetTinfo(x, y: integer): TTileRoadInfo;
    //no checking
    function GetTileInfo(x,y: Integer): TTileRoadInfo;

    procedure ChangeTiles(ASrc: TTileRoadInfos); virtual; abstract;
    function NeedUpdateTile(ATile: PTileRoadInfo): boolean; virtual; abstract;
    procedure UpdateTile(ATile: PTileRoadInfo; pattern: TSimplePattern; flip: Integer); virtual; abstract;
    function TileHasRoadRiver(ATile: TTileRoadInfo): boolean; virtual; abstract;
    function CanApplyPattern(APattern: TSimplePattern): boolean; virtual; abstract;

    function GetChangedRegion(): TMapRect; override; final;
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Redo; override;
    procedure Undo; override;

    function Execute: boolean; override;

  end;

  { TEditRoad }

  TEditRoad = class (TEditRoadRiver)
  strict private
    FRoadType: TRoadType;
  protected
    function NeedUpdateTile(ATile: PTileRoadInfo): boolean; override;
    procedure UpdateTile(ATile: PTileRoadInfo; pattern: TSimplePattern; flip: Integer); override;
    procedure ChangeTiles(ASrc: TTileRoadInfos); override;
    function TileHasRoadRiver(ATile: TTileRoadInfo): boolean; override;
    function CanApplyPattern(APattern: TSimplePattern): boolean; override;
  public
    constructor Create(AMap: TVCMIMap; ARoadType: TRoadType); reintroduce;

    function GetDescription: string; override;
    property RoadType: TRoadType read FRoadType;

    procedure AddTile(X,Y: integer); override;
  end;

  { TEditRiver }

  TEditRiver = class (TEditRoadRiver)
  private
    FRiverType: TRiverType;
  protected
    procedure ChangeTiles(ASrc: TTileRoadInfos); override;
    function NeedUpdateTile(ATile: PTileRoadInfo): boolean; override;
    procedure UpdateTile(ATile: PTileRoadInfo; pattern: TSimplePattern; flip: Integer); override;
    function TileHasRoadRiver(ATile: TTileRoadInfo): boolean; override;
    function CanApplyPattern(APattern: TSimplePattern): boolean; override;
  public
    constructor Create(AMap: TVCMIMap; ARiverType: TRiverType); reintroduce;

    function GetDescription: string; override;
    property RiverType: TRiverType read FRiverType;
    procedure AddTile(X,Y: integer); override;
  end;

implementation

uses editor_str_consts;

{ TRoadRiverBrush }

procedure TRoadRiverBrush.SetRoadType(AValue: TRoadType);
begin
  if FRoadType=AValue then Exit;
  FRoadType:=AValue;
  FRiverType:=TRiverType.noRiver;
  Kind := TRoadRiverBrushKind.road;
end;

procedure TRoadRiverBrush.AddTile(AMap: TVCMIMap; AX, AY: integer);

  procedure CheckAddTile(point: TMapCoord);
  begin
    //do not draw on water and rock
    if not (AMap.CurrentLevel.Tile[point.x,point.y]^.TerType in [TTerrainType.rock, TTerrainType.water]) then
       Selection.Insert(point);
  end;

var
  NewPoint: TMapCoord = (x:0; y:0);
  d, c: TMapCoord;
  SX, SY, E: integer;
begin
  if not (AMap.IsOnMap(AMap.CurrentLevelIndex,AX,AY)) then
    exit;

  NewPoint.Reset(AX,AY);

  if (FLastPoint <> INVALID_COORDINATE) and (FLastPoint <> NewPoint) then
  begin
    //draw line
    //(modified)Bresenham's algorimth
    //implementation based on BGRADrawLineAliased from http://sourceforge.net/projects/lazpaint (LGPL)

    D := NewPoint-FLastPoint;

    if D.X < 0 then
    begin
      SX := -1;
      D.X := -D.X;
    end
    else
    begin
      SX := 1;
    end;

    if D.Y < 0 then
    begin
      SY := -1;
      D.Y := -D.Y;
    end
    else
    begin
      SY := 1;
    end;

    D.X := D.X shl 1;
    D.Y := D.Y shl 1;

    c := FLastPoint;

    if D.X > D.Y then
    begin
      E := D.Y - D.X shr 1;

      while c.X <> NewPoint.X do
      begin
        CheckAddTile(c);
        if E >= 0 then
        begin
          Inc(c.Y, SY);
          CheckAddTile(c);
          Dec(E, D.X);
        end;
        Inc(c.X, SX);
        Inc(E, D.Y);
      end;
    end
    else
    begin
      E := D.X - D.Y shr 1;

      while c.Y <> NewPoint.Y do
      begin
        CheckAddTile(c);
        if E >= 0 then
        begin
          Inc(c.X, SX);
          CheckAddTile(c);
          Dec(E, D.Y);
        end;
        Inc(c.Y, SY);
        Inc(E, D.X);
      end;
    end;
  end;

  CheckAddTile(NewPoint);

  FLastPoint := NewPoint;
end;

constructor TRoadRiverBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clear;
end;

procedure TRoadRiverBrush.Clear;
begin
  inherited Clear;
  FLastPoint := INVALID_COORDINATE;
end;

procedure TRoadRiverBrush.SetRiverType(AValue: TRiverType);
begin
  if FRiverType=AValue then Exit;
  FRiverType:=AValue;
  FRoadType := TRoadType.noRoad;
  Kind := TRoadRiverBrushKind.river;
end;

procedure TRoadRiverBrush.SetKind(AValue: TRoadRiverBrushKind);
begin
  FKind:=AValue;
  Clear;
end;

procedure TRoadRiverBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap
  );
var
  action_item : TEditRoadRiver;
begin
  case Kind of
    TRoadRiverBrushKind.road:
    begin
      action_item := TEditRoad.Create(AMap, RoadType);
    end;
    TRoadRiverBrushKind.river:
    begin
      action_item := TEditRiver.Create(AMap,RiverType);
    end;
  end;
  FillActionObjectTiles(action_item);

  AManager.ExecuteItem(action_item);
  Clear;
end;

procedure TRoadRiverBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin
  if (AMap.IsOnMap(AMap.CurrentLevelIndex,X,Y))
    and not (AMap.CurrentLevel.Tile[X,Y]^.TerType in [TTerrainType.rock, TTerrainType.water])
  then
    inherited RenderCursor(State, X, Y, 1);
end;

procedure TRoadRiverBrush.RenderSelection(State: TLocalState);
var
  it: TCoordSet.TIterator;
  dim,cx,cy: Integer;
begin
  if Dragging then
  begin
    it := Selection.Min;
    if Assigned(it) then
    begin
      State.StartDrawingRects;
      State.SetFragmentColor(RECT_COLOR);
      dim := TILE_SIZE;
      repeat
        cx := it.Data.X * TILE_SIZE;
        cy := it.Data.Y * TILE_SIZE;
        State.RenderRect(cx,cy,dim,dim);
      until not it.next ;
      FreeAndNil(it);
      State.StopDrawing;
    end;
  end;
end;

{ TEditRoadRiver }

procedure TEditRoadRiver.UpdateTiles;
var
  i: SizeInt;
  tile: PTileRoadInfo;
  vr: TValidationResult;
  BestPattern: Integer;
  k: Integer;
  pattern: TSimplePattern;
begin
  for i := 0 to SizeInt(FNewTileInfos.Size) - 1 do
  begin
    tile := FNewTileInfos.Mutable[i];

    if not NeedUpdateTile(tile) then
      Continue;

    BestPattern := -1;

    for k := Low(ROAD_RULES) to High(ROAD_RULES) do
    begin

      pattern := ROAD_RULES[k];

      vr := ValidateTile(tile,pattern);

      if vr.result then
      begin
        BestPattern:=k;
        Break;
      end;

    end;

    if BestPattern = -1 then
    begin
      Continue;
    end;

    UpdateTile(tile,ROAD_RULES[BestPattern], vr.flip);
  end;
end;

function TEditRoadRiver.ValidateTile(info: PTileRoadInfo;
  pattern: TSimplePattern): TValidationResult;
var
  flip: integer;
  flipped: TSimplePattern;
  i: Integer;
  cx: Integer;
  cy: Integer;
  hasRoadRiver: Boolean;
  cur_tinfo: TTileRoadInfo;
  validated: Boolean;
begin
  Result.result := False;
  Result.flip := 0;

  if not CanApplyPattern(pattern) then
    exit;

  for flip := 0 to 4 - 1 do
  begin

    if (flip in [FLIP_PATTERN_BOTH,FLIP_PATTERN_VERTICAL]) and not pattern.HasVFlip then
    begin
      Continue;
    end;

    if (flip in [FLIP_PATTERN_BOTH,FLIP_PATTERN_HORIZONTAL]) and not pattern.HasHFlip then
    begin
      Continue;
    end;

    flipped := road_transitions.GetFlippedPattern(pattern,flip);
    validated := true;
    for i := 0 to 9 - 1 do
    begin
      if i = 4 then
         Continue;// skip self

      cx := info^.x + (i mod 3) - 1;
      cy := info^.Y + (i div 3) - 1;

      if FMap.IsOnMap(Level, cx,cy) then
      begin
        cur_tinfo := GetTinfo(cx, cy);
        hasRoadRiver := TileHasRoadRiver(cur_tinfo);
      end
      else
      begin
        hasRoadRiver := True;//let road to go out of map
      end;

      case flipped.Rules[i] of
        '+':begin
           if not hasRoadRiver then
           begin
             validated:=False;
             Break;
           end;

        end;
        '-':begin
           if hasRoadRiver then
           begin
             validated:=False;
             Break;
           end;
        end;
        else
        begin
          //ANY
        end;
      end;

    end;

    if validated then
    begin
      Result.result:=True;
      Result.flip:=flip;
      Exit;
    end;
  end;

end;

function TEditRoadRiver.GetTinfo(x, y: integer): TTileRoadInfo;
var
  tmp: TTileRoadInfo;
  n: TTileRoadInfoSet.PNode;
begin
  tmp.X := x;
  tmp.Y := Y;

  n := FOutQueue.NFind(tmp);

  if Assigned(n) then
  begin
    exit(n^.Data);
  end;

  n := FInQueue.NFind(tmp);

  if Assigned(n) then
  begin
    exit(n^.Data);
  end;

  Result := GetTileInfo(x,y);
end;

function TEditRoadRiver.GetTileInfo(x, y: Integer): TTileRoadInfo;
var
  tile: PMapTile;
begin
  tile := FMap.GetTile(Level,X,Y);
  Result.X:=X;
  Result.Y:=Y;
  Result.RoadType:=tile^.RoadType;
  Result.RoadDir:=tile^.RoadDir;
  Result.RoadFlip:=(tile^.Flags shr 4) mod 4;

  Result.RiverType:=tile^.RiverType;
  Result.RiverDir:=tile^.RiverDir;
  Result.RiverFlip:=(tile^.Flags shr 2) mod 4;
end;

function TEditRoadRiver.GetChangedRegion: TMapRect;
begin
  Result.Create(); //no invalidation
end;

constructor TEditRoadRiver.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FInQueue := TTileRoadInfoSet.Create;
  FOutQueue := TTileRoadInfoSet.Create;
  FOldTileInfos := TTileRoadInfos.Create;
  FNewTileInfos := TTileRoadInfos.Create;
  Level:=AMap.CurrentLevelIndex;
end;

destructor TEditRoadRiver.Destroy;
begin
  FInQueue.Free;
  FOutQueue.Free;
  FNewTileInfos.Free;
  FOldTileInfos.Free;
  inherited Destroy;
end;

procedure TEditRoadRiver.Redo;
begin
  ChangeTiles(FNewTileInfos);
end;

procedure TEditRoadRiver.Undo;
begin
  ChangeTiles(FOldTileInfos);
end;

function TEditRoadRiver.Execute: boolean;

  procedure CopyTile(const Coord: TMapCoord; var Stop: Boolean);
  var
    info: TTileRoadInfo;
  begin
    info := GetTinfo(Coord.X, Coord.Y);
    FOutQueue.Insert(info);
  end;
var
  it: TTileRoadInfoSet.TIterator;
  r: TMapRect;
begin
  Result := true;
  it := FInQueue.Min;

  if Assigned(it) then
  begin
    repeat
      r.SetFromCenter(it.data.x,it.data.Y, 3,3);
      r := FMap.GetCurrentLevelDimensions().Intersect(r);

      r.Iterate(@CopyTile);

    until not it.next;
    FreeAndNil(it);
  end;

  it := FOutQueue.Min;
  if Assigned(it) then
  begin
    repeat
      FNewTileInfos.PushBack(it.Data);
      FOldTileInfos.PushBack(GetTileInfo(it.Data.X, it.Data.Y));
    until not it.next;
    FreeAndNil(it);
  end;

  UpdateTiles;

  FreeAndNil(FInQueue);
  FreeAndNil(FOutQueue);

  Redo;
end;

{ TEditRoad }

procedure TEditRoad.ChangeTiles(ASrc: TTileRoadInfos);
var
  map_level: TMapLevel;
  i: SizeInt;

  pinfo: PTileRoadInfo;
  ptile: PMapTile;
begin
  map_level := FMap.MapLevels[Level];

  for i := 0 to SizeInt(ASrc.Size) - 1 do
  begin
    pinfo :=  ASrc.Mutable[i];
    ptile := map_level.Tile[pinfo^.X, pinfo^.Y];

    ptile^.SetRoad(pinfo^.RoadType, pinfo^.RoadDir, pinfo^.RoadFlip);
  end;
end;

function TEditRoad.TileHasRoadRiver(ATile: TTileRoadInfo): boolean;
begin
  Result := ATile.RoadType<>TRoadType.noRoad;
end;

function TEditRoad.CanApplyPattern(APattern: TSimplePattern): boolean;
begin
  Result := APattern.RoadMapping.Lower>=0;
end;

function TEditRoad.NeedUpdateTile(ATile: PTileRoadInfo): boolean;
begin
  Result := ATile^.RoadType <> TRoadType.noRoad;
end;

procedure TEditRoad.UpdateTile(ATile: PTileRoadInfo; pattern: TSimplePattern;
  flip: Integer);
var
  mapping: TMapping;
begin
  mapping := pattern.RoadMapping;

  ATile^.RoadDir:=system.Random(Mapping.Upper-Mapping.Lower+1)+Mapping.Lower;
  ATile^.RoadFlip:=flip;
end;

constructor TEditRoad.Create(AMap: TVCMIMap; ARoadType: TRoadType);
begin
  inherited Create(AMap);

  FRoadType := ARoadType;
end;

function TEditRoad.GetDescription: string;
begin
  Result := rsEditRoadDescription;
end;

procedure TEditRoad.AddTile(X, Y: integer);
var
  info: TTileRoadInfo;
begin
  if FMap.IsOnMap(Level,x,y) then
  begin
    info.X := X;
    info.Y := Y;
    info.RoadType:=RoadType;
    info.RoadDir:=14;
    info.RoadFlip:=0;
    info.RiverType:=TRiverType.noRiver;
    info.RiverDir:=0;
    info.RiverFlip := 0;

    FInQueue.Insert(info);
  end;
end;

{ TEditRiver }

procedure TEditRiver.ChangeTiles(ASrc: TTileRoadInfos);
var
  map_level: TMapLevel;
  i: SizeInt;
begin
  map_level := FMap.MapLevels[Level];

  for i := 0 to SizeInt(ASrc.Size) - 1 do
  begin
    with ASrc.Mutable[i]^ do
      map_level.SetRiver(X, Y,RiverType, RiverDir, RiverFlip);
  end;
end;

function TEditRiver.NeedUpdateTile(ATile: PTileRoadInfo): boolean;
begin
  Result := atile^.RiverType <> TRiverType.noRiver;
end;

procedure TEditRiver.UpdateTile(ATile: PTileRoadInfo; pattern: TSimplePattern;
  flip: Integer);
var
  mapping: TMapping;
begin
  mapping := pattern.RiverMapping;

  Assert(mapping.Lower>=0);
  Assert(mapping.Upper>=0);

  ATile^.RiverDir:=system.Random(Mapping.Upper-Mapping.Lower+1)+Mapping.Lower;
  Atile^.RiverFlip:=flip;

end;

function TEditRiver.TileHasRoadRiver(ATile: TTileRoadInfo): boolean;
begin
  Result := ATile.RiverType<>TRiverType.noRiver;
end;

function TEditRiver.CanApplyPattern(APattern: TSimplePattern): boolean;
begin
  Result := APattern.RiverMapping.Lower>=0;
end;

constructor TEditRiver.Create(AMap: TVCMIMap; ARiverType: TRiverType);
begin
  inherited Create(AMap);
  FRiverType := ARiverType;
end;

function TEditRiver.GetDescription: string;
begin
  Result := rsEditRiverDescription;
end;

procedure TEditRiver.AddTile(X, Y: integer);
var
  info: TTileRoadInfo;
begin
  if FMap.IsOnMap(Level,x,y) then
  begin
    info.X := X;
    info.Y := Y;
    info.RoadType:=TRoadType.noRoad;
    info.RoadDir:=0;
    info.RoadFlip:=0;
    info.RiverType:=RiverType;
    info.RiverDir:=0;
    info.RiverFlip := 0;

    FInQueue.Insert(info);
  end;
end;

end.

