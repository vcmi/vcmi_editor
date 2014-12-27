{ This file is a part of Map editor for VCMI project

  Copyright (C) 2014 Alexander Shishkin alexvins@users.sourceforge,net

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
    procedure BeforeReadObject(const JSON: TJSONObject; AObject: TObject);
  protected
    FDestreamer: TVCMIJSONDestreamer;

    procedure DeStreamTile(Encoded: string; Tile:PMapTile);
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
  end;

  { TMapWriterJson }

  TMapWriterJson = class abstract(TBaseMapFormatHandler)
  protected
    FStreamer: TVCMIJSONStreamer;
    procedure StreamTile(AJson: TJSONArray; tile: PMapTile);
    procedure StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap; const Level: Integer);
    function StreamTiles(AMap: TVCMIMap): TJSONData;
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
  end;

implementation

{ TMapWriterJson }

procedure TMapWriterJson.StreamTile(AJson: TJSONArray; tile: PMapTile);
var
  s: string;
begin
//  [terrain code][terrain index]
//[P][path type][path index]
//[R][river type][river index]
  s := TERRAIN_CODES[tile^.TerType]+IntToStr(tile^.TerSubType);

  if tile^.RoadType <> 0 then
  begin
    s := s + ROAD_CODES[TRoadType(tile^.RoadType)]+IntToStr(tile^.RoadDir);
  end;

  if tile^.RiverType <> 0 then
  begin
    s := s + RIVER_CODES[TRiverType(tile^.RiverType)]+IntToStr(tile^.RiverDir);
  end;

  if tile^.Flags <> 0 then
  begin
    s := s + 'f' + IntToStr(tile^.Flags);
  end;

  AJson.Add(s)

end;

procedure TMapWriterJson.StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
  const Level: Integer);
var
  row: Integer;
  col: Integer;

  o: TJSONObject;
  //s: string;
  tile: TMapTile;

  ARowArray: TJSONArray;
begin
  for row := 0 to AMap.Height - 1 do
  begin
    ARowArray := TJSONArray.Create;
    for col := 0 to AMap.Width - 1 do
    begin
      StreamTile(ARowArray, AMap.GetTile(Level,Col,Row));
    end;
    AJson.Add(ARowArray);
  end;
end;

function TMapWriterJson.StreamTiles(AMap: TVCMIMap): TJSONData;
var
  level_array: TJSONArray;
  i: Integer;
begin
  result := TJSONArray.Create;

  for i := 0 to AMap.Levels - 1 do
  begin
    level_array := TJSONArray.Create;
    StreamTilesLevel(level_array,AMap,i);
    TJSONArray(result).Add(level_array);
  end;
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

procedure TMapReaderJson.BeforeReadObject(const JSON: TJSONObject;
  AObject: TObject);
begin
  if AObject is TMapObject then
  begin
    //hack to read TemplateID before other properties

    TMapObject(AObject).TemplateID := JSON.Integers['templateID'];
  end;
end;

procedure TMapReaderJson.DeStreamTile(Encoded: string; Tile: PMapTile);
var
  terrainCode: String;
  tt: TTerrainType;
begin

  if not FTileExpression.Exec(Encoded) then
     raise Exception.CreateFmt('Invalid tile format %s',[Encoded]);

  if (FTileExpression.MatchLen[1]=0) or (FTileExpression.MatchLen[2]=0) then
    raise Exception.CreateFmt('Invalid tile format %s',[Encoded]);

  terrainCode := FTileExpression.Match[1];
  tt := FTerrainTypeMap.KeyData[terrainCode];
  Tile^.TerType:=tt;
  Tile^.TerSubType:= StrToInt(FTileExpression.Match[2]);

  if FTileExpression.MatchLen[3]>0 then
  begin
    Assert(FTileExpression.MatchLen[4]>0);
    Assert(FTileExpression.MatchLen[5]>0);

    Tile^.RoadType:= UInt8(FRoadTypeMap[FTileExpression.Match[4]]);
    Tile^.RoadDir:= StrToInt(FTileExpression.Match[5]);;
  end;

  if FTileExpression.MatchLen[6]>0 then
  begin
    Assert(FTileExpression.MatchLen[7]>0);
    Assert(FTileExpression.MatchLen[8]>0);

    Tile^.RiverType:= UInt8(FRiverTypeMap[FTileExpression.Match[7]]);
    Tile^.RiverDir:= StrToInt(FTileExpression.Match[8]);
  end;

  if FTileExpression.MatchLen[9]>0 then
  begin
    Assert(FTileExpression.MatchLen[10]>0);
    Tile^.Flags:=StrToInt(FTileExpression.Match[10]);
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
  FDestreamer.CaseInsensitive := True;
  FDestreamer.OnBeforeReadObject := @BeforeReadObject;

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

  //1-tt, 2 - tst ,  4-pt, 5-pst, 7-rt, 8 - rst , 10- flags

  FTileExpression.Expression:='^(\w{2,2})(\d+)((p\w)(\d+))?((r\w)(\d+))?(f(\d+))?$';
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

