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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
     override;

    property RoadType: TRoadType read FRoadType write SetRoadType;
    property RiverType: TRiverType read FRiverType write SetRiverType;
    property Kind:TRoadRiverBrushKind read FKind write SetKind;
  end;

  PTileRoadInfo = ^TTileRoadInfo;
  TTileRoadInfo = record
    X,Y: integer;
    RoadType: TRoadType;
    RoadDir: UInt8;
    mir: UInt8;
  end;

  TRoadInfoLess = specialize TGLessCoord <TTileRoadInfo>;

  TTileRoadInfoSet = specialize TSet<TTileRoadInfo,TRoadInfoLess>;
  TTileRoadInfos = specialize TVector<TTileRoadInfo>;

  { TEditRoadRiver }

  TEditRoadRiver = class abstract (TMultiTileMapAction)
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;
  end;

  { TEditRoad }

  TEditRoad = class (TEditRoadRiver)
  strict private
    FRoadType: TRoadType;
    FInQueue: TTileRoadInfoSet;
    FOutQueue : TTileRoadInfoSet;
    FNewTileInfos,
    FOldTileInfos: TTileRoadInfos;
    //no checking, with changes
    function GetTinfo(x, y: integer): TTileRoadInfo;

    //no checking
    function GetTileInfo(x,y: Integer): TTileRoadInfo;
    procedure ChangeTiles(ASrc: TTileRoadInfos);
    procedure UpdateTiles();
    function ValidateTile(info: PTileRoadInfo; pattern: TSimplePattern): TValidationResult;
  public
    constructor Create(AMap: TVCMIMap; ARoadType: TRoadType); reintroduce;
    destructor Destroy; override;

    function GetDescription: string; override;
    property RoadType: TRoadType read FRoadType;

    procedure AddTile(X,Y: integer); override;

    procedure Redo; override;
    procedure Undo; override;
    procedure Execute; override;
  end;

  { TEditRiver }

  TEditRiver = class (TEditRoadRiver)
  public
    function GetDescription: string; override;
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

constructor TRoadRiverBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRoadRiverBrush.Destroy;
begin
  inherited Destroy;
end;

procedure TRoadRiverBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap
  );
var
  action_item : TEditRoadRiver;
begin
  inherited Execute(AManager, AMap);

  case Kind of
    TRoadRiverBrushKind.road:
    begin
      action_item := TEditRoad.Create(AMap, RoadType);
      FillActionObjectTiles(action_item);

      AManager.ExecuteItem(action_item);
    end;
    TRoadRiverBrushKind.river:
    begin
      //todo: TRoadRiverBrush river case
//      action_item := TEditRiver.c;
    end;
  end;

  Clear;
end;

{ TEditRoadRiver }

constructor TEditRoadRiver.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
end;

destructor TEditRoadRiver.Destroy;
begin
  inherited Destroy;
end;

{ TEditRoad }

function TEditRoad.GetTinfo(x, y: integer): TTileRoadInfo;
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

function TEditRoad.GetTileInfo(x, y: Integer): TTileRoadInfo;
var
  tile: PMapTile;
begin
  tile := FMap.GetTile(Level,X,Y);
  Result.X:=X;
  Result.Y:=Y;
  Result.RoadType:=TRoadType(tile^.RoadType);
  Result.RoadDir:=tile^.RoadDir;
  Result.mir:=(tile^.Flags shr 4) mod 4;
end;

procedure TEditRoad.ChangeTiles(ASrc: TTileRoadInfos);
var
  map_level: TMapLevel;
  i: Integer;
begin
  map_level := FMap.Levels[Level];

  for i := 0 to ASrc.Size - 1 do
  begin
    with ASrc.Mutable[i]^ do
      map_level.SetRoad(X, Y,RoadType, RoadDir, Mir);
  end;
end;

procedure TEditRoad.UpdateTiles;
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

    if tile^.RoadType = TRoadType.noRoad then
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

    pattern:=ROAD_RULES[BestPattern];

    mapping := pattern.Mapping;

    tile^.RoadDir:=system.Random(Mapping.Upper-Mapping.Lower)+Mapping.Lower;
    tile^.mir:=vr.flip;

  end;
end;

function TEditRoad.ValidateTile(info: PTileRoadInfo; pattern: TSimplePattern
  ): TValidationResult;
var
  flip: integer;
  flipped: TSimplePattern;
  i: Integer;
  cx: Integer;
  cy: Integer;
  hasRoad: Boolean;
  cur_tinfo: TTileRoadInfo;
  validated: Boolean;
begin
  Result.result := False;
  Result.flip := 0;

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
        hasRoad := cur_tinfo.RoadType<>TRoadType.noRoad;
      end
      else
      begin
        hasRoad := True;//let road to go out of map
      end;

      case pattern.Rules[i] of
        '+':begin
           if not hasRoad then
           begin
             validated:=False;
             Break;
           end;

        end;
        '-':begin
           if hasRoad then
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

constructor TEditRoad.Create(AMap: TVCMIMap; ARoadType: TRoadType);
begin
  inherited Create(AMap);
  FInQueue := TTileRoadInfoSet.Create;
  FOutQueue := TTileRoadInfoSet.Create;
  FOldTileInfos := TTileRoadInfos.Create;
  FNewTileInfos := TTileRoadInfos.Create;
  FRoadType := ARoadType;
  Level:=AMap.CurrentLevelIndex;
end;

destructor TEditRoad.Destroy;
begin
  FInQueue.Free;
  FOutQueue.Free;
  FNewTileInfos.Free;
  FOldTileInfos.Free;
  inherited Destroy;
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
    info.mir:=0;

    FInQueue.Insert(info);
  end;
end;

procedure TEditRoad.Redo;
begin
  ChangeTiles(FNewTileInfos);
end;

procedure TEditRoad.Undo;
begin
  ChangeTiles(FOldTileInfos);
end;

procedure TEditRoad.Execute;

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
  r,mr: TMapRect;
begin
  it := FInQueue.Min;

  if Assigned(it) then
  begin
    repeat
      r.SetFromCenter(it.data.x,it.data.Y, 3,3);
      mr.DimOfMap(FMap);
      r := r.Intersect(mr);

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

{ TEditRiver }

function TEditRiver.GetDescription: string;
begin
  Result := rsEditRiverDescription;
end;

end.

