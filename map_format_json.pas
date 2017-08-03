{ This file is a part of Map editor for VCMI project

  Copyright (C) 2014-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_format_json;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, RegExpr, editor_types, map_format, map, terrain,
  vcmi_json, vcmi_fpjsonrtti, fpjson, lists_manager;

const
  TERRAIN_CODES: array[TTerrainType] of string = ('dt', 'sa', 'gr', 'sn', 'sw', 'rg', 'sb', 'lv', 'wt', 'rc');
  ROAD_CODES: array[TRoadType] of String = ('', 'pd','pg', 'pc');
  RIVER_CODES: array[TRiverType] of string = ('', 'rw', 'ri', 'rm', 'rl');
  FLIP_CODES: array[0..3] of string = ('_','-','|','+');

  TILES_FIELD = 'tiles';
  TEMPLATES_FIELD = 'templates';
  OBJECTS_FIELD = 'objects';
type
  TTerrainTypeMap = specialize TFPGMap<string, TTerrainType>;
  TRoadTypeMap = specialize TFPGMap<string, TRoadType>;
  TRiverTypeMap = specialize TFPGMap<string, TRiverType>;

  { TMapReaderJson }

  TMapReaderJson = class abstract(TBaseMapFormatHandler)
  private
    FTerrainTypeMap:TTerrainTypeMap;
    FRoadTypeMap: TRoadTypeMap;
    FRiverTypeMap: TRiverTypeMap;

    FTileExpression : TRegExpr;
  protected
    FDestreamer: TVCMIJSONDestreamer;

    procedure DeStreamTile(Encoded: string; Tile:PMapTile);
    procedure DeStreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap; const Level: Integer);
    procedure DeStreamTiles(AJson: TJSONArray; AMap: TVCMIMap);

  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
  end;

  { TMapWriterJson }

  TMapWriterJson = class abstract(TBaseMapFormatHandler)
  protected
    FStreamer: TVCMIJSONStreamer;
    function EncodeTile(tile: PMapTile): TJSONStringType;

    procedure StreamTilesLevel(ADest: TStream; AMap: TVCMIMap; const Level: Integer);

  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
  end;

implementation

{ TMapWriterJson }

function TMapWriterJson.EncodeTile(tile: PMapTile): TJSONStringType;
begin
  //   [terrain type][terrain index][flip]
  //[P][path type][path index][flip]
  //[R][river type][river index][flip]

  Result := TERRAIN_CODES[tile^.TerType]+IntToStr(tile^.TerSubType)+FLIP_CODES[tile^.Flags mod 4];

  if tile^.RoadType <> TRoadType.noRoad then
  begin
    Result := Result + ROAD_CODES[tile^.RoadType]+IntToStr(tile^.RoadDir)+FLIP_CODES[(tile^.Flags shr 4) mod 4];
  end;

  if tile^.RiverType <> TRiverType.noRiver then
  begin
    Result := Result + RIVER_CODES[tile^.RiverType]+IntToStr(tile^.RiverDir)+FLIP_CODES[(tile^.Flags shr 2) mod 4];
  end;
end;

procedure TMapWriterJson.StreamTilesLevel(ADest: TStream; AMap: TVCMIMap; const Level: Integer);

  Procedure W(T : AnsiString);
  begin
    ADest.WriteBuffer(T[1],Length(T));
  end;

const
  ARRAY_START = '[';
  ARRAY_END = '],'+LineEnding;

  ARRAY_END2 = ']';

  STR_START_END = '"';
  STR_END = '",';

var
  row, row_max: Integer;
  col, col_max: Integer;
  map_level: TMapLevel;
  tile: PMapTile;
  s: AnsiString;
begin
  map_level := AMap.MapLevels[Level];

  row_max := map_level.Height - 1;
  col_max := map_level.Width - 1;

  W(ARRAY_START);

  for row := 0 to row_max do
  begin
    W(ARRAY_START);

    for col := 0 to col_max do
    begin
      W(STR_START_END);

      tile := map_level.Tile[col, row];

      s := EncodeTile(tile);

      ADest.WriteBuffer(s[1], Length(S));

      if col = col_max  then
        W(STR_START_END)
      else
        W(STR_END);

    end;

    if row = row_max  then
      W(ARRAY_END2)
    else
      W(ARRAY_END);

  end;
  W(ARRAY_END2);
end;

constructor TMapWriterJson.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
  FStreamer := TVCMIJSONStreamer.Create(nil);
  FStreamer.Options := [jsoTStringsAsArray];
end;

destructor TMapWriterJson.Destroy;
begin
  FStreamer.Free;
  inherited Destroy;
end;

{ TMapReaderJson }

procedure TMapReaderJson.DeStreamTile(Encoded: string; Tile: PMapTile);

  function ParseFlip(Src: AnsiChar): Uint8;
  begin
    //('_','-','|','+');
    case Src of
      '_': Result := 0;
      '-': Result := 1;
      '|': Result := 2;
      '+': Result := 3;
    else
      raise Exception.CreateFmt('Invalid tile flip %s',[Src]);
    end;
  end;

var
  terrainCode: String;
  tt: TTerrainType;
  styp, flip: UInt8;
  road_type: TRoadType;
  river_type: TRiverType;
begin

  if not FTileExpression.Exec(Encoded) then
     raise Exception.CreateFmt('Invalid tile format %s',[Encoded]);

  //1-tt, 2 - tcode, 3 - tflip , 4-road (5 6 7) 8 - river (9 10 11)

  if (FTileExpression.MatchLen[1]=0) or (FTileExpression.MatchLen[2]=0) then
    raise Exception.CreateFmt('Invalid tile format %s',[Encoded]);


  terrainCode := FTileExpression.Match[1];
  tt := FTerrainTypeMap.KeyData[terrainCode];

  Tile^.SetTerrain(tt,StrToInt(FTileExpression.Match[2]), ParseFlip(FTileExpression.Match[3][1]));

  if FTileExpression.MatchLen[4]>0 then
  begin
    Assert(FTileExpression.MatchLen[5]>0);
    Assert(FTileExpression.MatchLen[6]>0);

    road_type := FRoadTypeMap[FTileExpression.Match[5]];
    styp := UInt8(StrToInt( FTileExpression.Match[6]));
    flip := ParseFlip(FTileExpression.Match[7][1]);

    Tile^.SetRoad(road_type, styp, flip);
  end;

  if FTileExpression.MatchLen[8]>0 then
  begin
    Assert(FTileExpression.MatchLen[9]>0);
    Assert(FTileExpression.MatchLen[10]>0);

    river_type := FRiverTypeMap[FTileExpression.Match[9]];
    styp := StrToInt(FTileExpression.Match[10]);
    flip := ParseFlip(FTileExpression.Match[11][1]);

    Tile^.SetRiver(river_type, styp, flip);
  end;
end;

procedure TMapReaderJson.DeStreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
  const Level: Integer);
var
  row: Integer;
  col: Integer;

  d: TJSONData;
  tile: PMapTile;

  ARow:TJSONArray;
  map_level: TMapLevel;
begin
  map_level := AMap.MapLevels[Level];
  for row := 0 to map_level.Height - 1 do
  begin
    //todo: more error checking
    ARow := AJson.Arrays[row];
    for col := 0 to map_level.Width - 1 do
    begin
      tile := map_level.Tile[col,row];

      d := ARow.Items[col];

      case d.JSONType of
        jtString:begin
           DeStreamTile(d.AsString,tile);
        end;
      else
        raise Exception.CreateFmt('Invalid tile format at  L: %d, row: %d, col: %d',[Level, row, col]);
      end;
    end;
  end;
end;

procedure TMapReaderJson.DeStreamTiles(AJson: TJSONArray; AMap: TVCMIMap);
var
  level_array: TJSONArray;
  i: Integer;
begin
  for i := 0 to AMap.MapLevels.Count - 1 do
  begin
    level_array := AJson.Arrays[i];
    DeStreamTilesLevel(level_array,AMap,i);
  end;
end;

constructor TMapReaderJson.Create(AMapEnv: TMapEnvironment);
var
  tt: TTerrainType;
  rdt: TRoadType;
  rvt: TRiverType;
begin
  inherited Create(AMapEnv);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
  FDestreamer.Options := [jdoCaseInsensitive];

  FTerrainTypeMap := TTerrainTypeMap.Create;
  FRoadTypeMap := TRoadTypeMap.Create;
  FRiverTypeMap := TRiverTypeMap.Create;

  for tt in TTerrainType do
    FTerrainTypeMap.Add(TERRAIN_CODES[tt], tt);

  for rdt in TRoadType do
    FRoadTypeMap.Add(ROAD_CODES[rdt], rdt);

  for rvt in TRiverType do
    FRiverTypeMap.Add(RIVER_CODES[rvt], rvt);

  FTileExpression := TRegExpr.Create;

  //1-tt, 2 - tcode, 3 - tflip , 4-road (5 6 7) 8 - river (9 10 11)

  FTileExpression.Expression:='^(\w{2,2})(\d+)(.)((p\w)(\d+)(.))?((r\w)(\d+)(.))?$';

  FTileExpression.Compile;
end;

destructor TMapReaderJson.Destroy;
begin
  FTileExpression.Free;

  FRiverTypeMap.free;
  FRoadTypeMap.Free;
  FTerrainTypeMap.Free;

  FDestreamer.Free;
  inherited Destroy;
end;

end.

