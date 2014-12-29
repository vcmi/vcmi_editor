{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013,2014 Alexander Shishkin alexvins@users.sourceforge,net

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

unit map_road_river_actions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, gset, gvector, undo_base, undo_map, Map, editor_types, map_actions,
  editor_classes, transitions, road_transitions;

type

  { TRoadRiverBrush }

  TRoadRiverBrushKind = (road, river);

  TRoadRiverBrush = class(TMapBrush)
  private
    FKind: TRoadRiverBrushKind;
    FRiverType: TRiverType;
    FRoadType: TRoadType;
    procedure SetKind(AValue: TRoadRiverBrushKind);
    procedure SetRiverType(AValue: TRiverType);
    procedure SetRoadType(AValue: TRoadType);
  protected
    procedure AddTile(X,Y: integer); override;

  public
    procedure Execute(AManager: TAbstractUndoManager);override;

    procedure RenderCursor(X,Y: integer); override;

    property RoadType: TRoadType read FRoadType write SetRoadType;
    property RiverType: TRiverType read FRiverType write SetRiverType;
    property Kind:TRoadRiverBrushKind read FKind write SetKind;
  end;

  PTileRoadInfo = ^TTileRoadInfo;
  TTileRoadInfo = record
    X,Y: integer;
    RoadType: UInt8;
    RoadDir: UInt8;
    RoadFlip: UInt8;

    RiverType: UInt8;
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
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Redo; override;
    procedure Undo; override;
    procedure Execute; override;

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

procedure TRoadRiverBrush.AddTile(X, Y: integer);
var
  c: TMapCoord = (x:0; y:0);
begin
  c.Reset(x,y);

  //TODO:do not draw on water and rock
  //TODO: draw line from previouse point to not skip tiles on fast mouse move
  //TODO: connect road on diagonal move

  Selection.Insert(c);
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

procedure TRoadRiverBrush.Execute(AManager: TAbstractUndoManager);
var
  action_item : TEditRoadRiver;
begin
  case Kind of
    TRoadRiverBrushKind.road:
    begin
      action_item := TEditRoad.Create(FMap, RoadType);
    end;
    TRoadRiverBrushKind.river:
    begin
      action_item := TEditRiver.Create(FMap,RiverType);
    end;
  end;
  FillActionObjectTiles(action_item);

  AManager.ExecuteItem(action_item);
  Clear;
end;

procedure TRoadRiverBrush.RenderCursor(X, Y: integer);
begin
  inherited RenderCursor(X, Y,1);
end;

{ TEditRoadRiver }

procedure TEditRoadRiver.UpdateTiles;
var
  i: Integer;
  tile: PTileRoadInfo;
  vr: TValidationResult;
  BestPattern: Integer;
  k: Integer;
  pattern: TSimplePattern;
  mapping: TMapping;
begin
  for i := 0 to FNewTileInfos.Size - 1 do
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

procedure TEditRoadRiver.Execute;

  procedure CopyTile(const Coord: TMapCoord; var Stop: Boolean);
  var
    info: TTileRoadInfo;
  begin
    info := GetTinfo(Coord.X, Coord.Y);
    FOutQueue.Insert(info);
  end;
var
  it: TTileRoadInfoSet.TIterator;
  map_level: TMapLevel;
  r: TMapRect;
begin
  it := FInQueue.Min;

  if Assigned(it) then
  begin
    repeat
      r.SetFromCenter(it.data.x,it.data.Y, 3,3);
      r := TMapRect.DimOfMap(FMap).Intersect(r);

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
  i: Integer;
begin
  map_level := FMap.Levels[Level];

  for i := 0 to ASrc.Size - 1 do
  begin
    with ASrc.Mutable[i]^ do
      map_level.SetRoad(X, Y,RoadType, RoadDir, RoadFlip);
  end;
end;

function TEditRoad.TileHasRoadRiver(ATile: TTileRoadInfo): boolean;
begin
  Result := ATile.RoadType<>Uint8(TRoadType.noRoad);
end;

function TEditRoad.CanApplyPattern(APattern: TSimplePattern): boolean;
begin
  Result := APattern.RoadMapping.Lower>=0;
end;

function TEditRoad.NeedUpdateTile(ATile: PTileRoadInfo): boolean;
begin
  Result := ATile^.RoadType <> UInt8(TRoadType.noRoad);
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
    info.RoadType:=UInt8(RoadType);
    info.RoadDir:=14;
    info.RoadFlip:=0;
    info.RiverType:=0;
    info.RiverDir:=0;
    info.RiverFlip := 0;

    FInQueue.Insert(info);
  end;
end;

{ TEditRiver }

procedure TEditRiver.ChangeTiles(ASrc: TTileRoadInfos);
var
  map_level: TMapLevel;
  i: Integer;
begin
  map_level := FMap.Levels[Level];

  for i := 0 to ASrc.Size - 1 do
  begin
    with ASrc.Mutable[i]^ do
      map_level.SetRiver(X, Y,RiverType, RiverDir, RiverFlip);
  end;
end;

function TEditRiver.NeedUpdateTile(ATile: PTileRoadInfo): boolean;
begin
  Result := atile^.RiverType <> UInt8(TRiverType.noRiver);
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
  Result := ATile.RiverType<>Uint8(TRiverType.noRiver);
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
    info.RoadType:=0;
    info.RoadDir:=0;
    info.RoadFlip:=0;
    info.RiverType:=UInt8(RiverType);
    info.RiverDir:=9;
    info.RiverFlip := 0;

    FInQueue.Insert(info);
  end;
end;

end.

