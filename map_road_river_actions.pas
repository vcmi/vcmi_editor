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
  Classes, SysUtils, gset, undo_base, undo_map, Map, editor_types, map_actions,
  editor_classes;

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

  TTileRoadInfo = record
    X,Y: integer;
    RoadType: TRoadType;
    RoadDir: UInt8;
    mir: UInt8;
  end;

  TRoadInfoLess = specialize TGLessCoord <TTileRoadInfo>;

  TTileRoadInfoSet = specialize TSet<TTileRoadInfo,TRoadInfoLess>;

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
    FOutQueue : TTileRoadInfoSet; //also new tile infos
    FOldTileInfos: TTileRoadInfoSet;
    //no checking
    function GetTileInfo(x,y: Integer): TTileRoadInfo;
    procedure ChangeTiles(ASrc: TTileRoadInfoSet);
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

procedure TEditRoad.ChangeTiles(ASrc: TTileRoadInfoSet);
var
  it: TTileRoadInfoSet.TIterator;
  map_level: TMapLevel;
begin
  it := ASrc.Min;
  map_level := FMap.Levels[Level];
  if Assigned(it) then
  begin
    repeat
      with it.Data do
        map_level.SetRoad(X, Y,RoadType, RoadDir, Mir);
    until not it.next;
    FreeAndNil(it);
  end;
end;

constructor TEditRoad.Create(AMap: TVCMIMap; ARoadType: TRoadType);
begin
  inherited Create(AMap);
  FInQueue := TTileRoadInfoSet.Create;
  FOutQueue := TTileRoadInfoSet.Create;
  FOldTileInfos := TTileRoadInfoSet.Create;
  FRoadType := ARoadType;
end;

destructor TEditRoad.Destroy;
begin
  FInQueue.Free;
  FOutQueue.Free;
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
    info.RoadDir:=0;//???
    info.mir:=0;

    FInQueue.Insert(info);
  end;
end;

procedure TEditRoad.Redo;
begin
  ChangeTiles(FOutQueue);
end;

procedure TEditRoad.Undo;
begin
  ChangeTiles(FOldTileInfos);
end;

procedure TEditRoad.Execute;
var
  it: TTileRoadInfoSet.TIterator;
  map_level: TMapLevel;
begin

  //STUB

  it := FInQueue.Min;
  map_level := FMap.Levels[Level];
  if Assigned(it) then
  begin
    repeat
      FOutQueue.Insert(it.data);
    until not it.next;
    FreeAndNil(it);
  end;


  //save old state
  it := FOutQueue.Min;
  map_level := FMap.Levels[Level];
  if Assigned(it) then
  begin
    repeat
      FOldTileInfos.Insert(GetTileInfo(it.Data.X, it.Data.Y));
    until not it.next;
    FreeAndNil(it);
  end;

  //apply new state
  redo;
end;

{ TEditRiver }

function TEditRiver.GetDescription: string;
begin
  Result := rsEditRiverDescription;
end;

end.

