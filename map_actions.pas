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
unit map_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Map, gvector, undo_map, editor_types;

type

  { TEditTerrain }

  TBrushMode = (none,fixed, area, fill);

  TTileInfo = record
    X,Y: integer;
    TerType: TTerrainType;
    TerSubtype: UInt8;
    //todo: river, road
  end;

  TTileInfos = specialize TVector<TTileInfo>;

  TEditTerrain = class(TMapUndoItem)
  strict private
    procedure UndoTile(constref Tile: TTileInfo);
  strict private
    FLevel: Integer;

    FOldTileInfos: TTileInfos;
    FNewTileInfos: TTileInfos;

    FBrushMode: TBrushMode;
    FBrushSize: integer;
    FTerrainType: TTerrainType;
    procedure SetBrushMode(AValue: TBrushMode);
    procedure SetBrushSize(AValue: integer);
    procedure SetLevel(AValue: Integer);
    procedure SetTerrainType(AValue: TTerrainType);
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Redo; override;
    procedure Undo; override;
    procedure Execute; override;
    function GetDescription: string; override;

    property BrushMode: TBrushMode read FBrushMode write SetBrushMode;
    property BrushSize: integer read FBrushSize write SetBrushSize;

    property Level: Integer read FLevel write SetLevel;

    procedure AddTile(X,Y: integer);

    property TerrainType: TTerrainType read FTerrainType write SetTerrainType;
  end;

implementation

{ TEditTerrain }

procedure TEditTerrain.AddTile(X, Y: integer);
var
  info: TTileInfo;
begin
  info.TerType := TerrainType;
  info.TerSubtype := 0;//not used
  info.X := X;
  info.Y := Y;

  FNewTileInfos.PushBack(info);
end;

constructor TEditTerrain.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FOldTileInfos := TTileInfos.Create;
  FNewTileInfos := TTileInfos.Create;
end;

destructor TEditTerrain.Destroy;
begin
  FOldTileInfos.Free;
  FNewTileInfos.Free;

  inherited Destroy;
end;

procedure TEditTerrain.Execute;
var
  t: TMapTile;
  old_info, new_info: TTileInfo;

  i: Integer;
begin
  //TODO: terrain transitions

  FOldTileInfos.Clear;

  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    t := FMap.GetTile(FLevel,new_info.X,new_info.Y);

    old_info.X := new_info.X;
    old_info.Y := new_info.Y;
    old_info.TerType := t.TerType;
    old_info.TerSubtype := t.TerSubType;

    FOldTileInfos.PushBack(old_info);

    FMap.SetTerrain(new_info.X,new_info.Y, FTerrainType);
  end;
end;

function TEditTerrain.GetDescription: string;
begin
  Result := 'Edit terrain'; //todo: l18n
end;

procedure TEditTerrain.Redo;
var
  new_info: TTileInfo;
  i: Integer;
begin
  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    FMap.SetTerrain(new_info.X,new_info.Y, FTerrainType);
  end;

end;

procedure TEditTerrain.SetBrushMode(AValue: TBrushMode);
begin
  if FBrushMode = AValue then Exit;
  FBrushMode := AValue;
end;

procedure TEditTerrain.SetBrushSize(AValue: integer);
begin
  if FBrushSize = AValue then Exit;
  FBrushSize := AValue;
end;

procedure TEditTerrain.SetLevel(AValue: Integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

procedure TEditTerrain.SetTerrainType(AValue: TTerrainType);
begin
  if FTerrainType = AValue then Exit;
  FTerrainType := AValue;
end;

procedure TEditTerrain.Undo;
var
  i: Integer;
begin
  for i := 0 to FOldTileInfos.Size - 1 do
  begin
    UndoTile(FOldTileInfos[i]);
  end;
end;

procedure TEditTerrain.UndoTile(constref Tile: TTileInfo);
begin
  FMap.SetTerrain(FLevel, Tile.X, Tile.Y,Tile.TerType, Tile.TerSubtype);
end;

end.

