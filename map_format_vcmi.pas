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
unit map_format_vcmi;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, RegExpr, editor_types, map, map_format, terrain, vcmi_json, vcmi_fpjsonrtti, fpjson, lists_manager;

type

  TTerrainTypeMap = specialize TFPGMap<string, TTerrainType>;
  TRoadTypeMap = specialize TFPGMap<string, TRoadType>;
  TRiverTypeMap = specialize TFPGMap<string, TRiverType>;

  { TMapReaderVCMI }

  TMapReaderVCMI = class(TBaseMapFormatHandler, IMapReader)
  private
    FDestreamer: TVCMIJSONDestreamer;

    FTerrainTypeMap:TTerrainTypeMap;
    FRoadTypeMap: TRoadTypeMap;
    FRiverTypeMap: TRiverTypeMap;

    FTileExpression : TRegExpr;

    procedure DeStreamTile(Encoded: string; Tile:TMapTile);
    procedure DeStreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
    procedure DeStreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap; const Level: Integer);

    procedure BeforeReadObject(const JSON: TJSONObject; AObject: TObject);
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;

    function Read(AStream: TStream): TVCMIMap;
  end;

  { TMapWriterVCMI }

  TMapWriterVCMI = class(TBaseMapFormatHandler, IMapWriter)
  private
    FStreamer: TVCMIJSONStreamer;

    procedure StreamTile(AJson: TJSONArray; tile: TMapTile);

    procedure StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap; const Level: Integer);
    procedure StreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;


implementation

uses
  typinfo;

const

  TILES_FIELD = 'tiles';
  TEMPLATES_FIELD = 'templates';
  OBJECTS_FIELD = 'objects';

  TERRAIN_CODES: array[TTerrainType] of string = ('dt', 'sa', 'gr', 'sn', 'sw', 'rg', 'sb', 'lv', 'wt', 'rc');
  ROAD_CODES: array[TRoadType] of String = ('', 'pd','pg', 'pc');
  RIVER_CODES: array[TRiverType] of string = ('', 'rw', 'ri', 'rm', 'rl');


{ TMapWriterVCMI }

constructor TMapWriterVCMI.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
  FStreamer := TVCMIJSONStreamer.Create(nil);
  FStreamer.Options := [jsoTStringsAsArray];
end;

destructor TMapWriterVCMI.Destroy;
begin
  FStreamer.Free;
  inherited Destroy;
end;

procedure TMapWriterVCMI.StreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
var
  main_array, level_array: TJSONArray;
  i: Integer;
begin
  main_array := TJSONArray.Create;

  for i := 0 to AMap.Levels - 1 do
  begin
    level_array := TJSONArray.Create;
    StreamTilesLevel(level_array,AMap,i);
    main_array.Add(level_array);
  end;

  ARoot.Add(TILES_FIELD,main_array);
end;

procedure TMapWriterVCMI.StreamTile(AJson: TJSONArray; tile: TMapTile);
var
  s: string;
begin
//  [terrain code][terrain index]
//[P][path type][path index]
//[R][river type][river index]
  s := TERRAIN_CODES[tile.TerType]+IntToStr(tile.TerSubType);

  if tile.RoadType <> 0 then
  begin
    s := s + ROAD_CODES[TRoadType(tile.RoadType)]+IntToStr(tile.RoadDir);
  end;

  if tile.RiverType <> 0 then
  begin
    s := s + RIVER_CODES[TRiverType(tile.RiverType)]+IntToStr(tile.RiverDir);
  end;

  if tile.Flags <> 0 then
  begin
    s := s + 'f' + IntToStr(tile.Flags);
  end;

  AJson.Add(s);
end;

procedure TMapWriterVCMI.StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
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

procedure TMapWriterVCMI.Write(AStream: TStream; AMap: TVCMIMap);
var
  json_text: string;
  map_o:     TJSONObject;
  //o: TJSONObject;
  d: TJSONData;
begin
  map_o := FStreamer.ObjectToJSON(AMap);
  try
    //todo: use stored modifier

    d := FStreamer.StreamCollection(AMap.Templates);

    map_o.Add(TEMPLATES_FIELD, d);

    d := FStreamer.StreamCollection(AMap.Objects);
    map_o.Add(OBJECTS_FIELD,d);

    StreamTiles(map_o, AMap);

    json_text := map_o.FormatJSON([foUseTabchar], 1);

    AStream.Write(json_text[1], Length(json_text));
  finally
    map_o.Free;
  end;
end;

{ TMapReaderVCMI }

procedure TMapReaderVCMI.BeforeReadObject(const JSON: TJSONObject;
  AObject: TObject);
begin
  if AObject is TMapObject then
  begin
    //hack to read TemplateID before other properties

    TMapObject(AObject).TemplateID := JSON.Integers['templateID'];
  end;
end;

constructor TMapReaderVCMI.Create(AMapEnv: TMapEnvironment);
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

procedure TMapReaderVCMI.DeStreamTile(Encoded: string; Tile: TMapTile);
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
  Tile.TerType:=tt;
  Tile.TerSubType:= StrToInt(FTileExpression.Match[2]);

  if FTileExpression.MatchLen[3]>0 then
  begin
    Assert(FTileExpression.MatchLen[4]>0);
    Assert(FTileExpression.MatchLen[5]>0);

    Tile.RoadType:= UInt8(FRoadTypeMap[FTileExpression.Match[4]]);
    Tile.RoadDir:= StrToInt(FTileExpression.Match[5]);;
  end;

  if FTileExpression.MatchLen[6]>0 then
  begin
    Assert(FTileExpression.MatchLen[7]>0);
    Assert(FTileExpression.MatchLen[8]>0);

    Tile.RiverType:= UInt8(FRiverTypeMap[FTileExpression.Match[7]]);
    Tile.RiverDir:= StrToInt(FTileExpression.Match[8]);
  end;

  if FTileExpression.MatchLen[9]>0 then
  begin
    Assert(FTileExpression.MatchLen[10]>0);
    Tile.Flags:=StrToInt(FTileExpression.Match[10]);
  end;
end;

procedure TMapReaderVCMI.DeStreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
var
  main_array, level_array: TJSONArray;
  i: Integer;
begin
  main_array := ARoot.Arrays[TILES_FIELD];

  for i := 0 to AMap.Levels - 1 do
  begin
    level_array := main_array.Arrays[i];
    DeStreamTilesLevel(level_array,AMap,i);
  end;
end;

procedure TMapReaderVCMI.DeStreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
  const Level: Integer);
var
  row: Integer;
  col: Integer;
  idx: Integer;

  o: TJSONObject;
  d: TJSONData;
  tile: TMapTile;

  ARow:TJSONArray;
begin
  for row := 0 to AMap.Height - 1 do
  begin
    //todo: more error checking
    ARow := AJson.Arrays[row];
    for col := 0 to AMap.Width - 1 do
    begin
      tile := AMap.GetTile(Level,col,row);

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

destructor TMapReaderVCMI.Destroy;
begin
  FTileExpression.Free;
  FDestreamer.Free;
  FRiverTypeMap.free;
  FRoadTypeMap.Free;
  FTerrainTypeMap.Free;
  inherited Destroy;
end;

function TMapReaderVCMI.Read(AStream: TStream): TVCMIMap;
var
  map_o: TJSONObject;
  cp:    TMapCreateParams;
begin
  map_o := FDestreamer.JSONStreamToJson(AStream) as TJSONObject;

  cp.Levels := map_o.Integers['levels'];
  cp.Height := map_o.Integers['height'];
  cp.Width  := map_o.Integers['width'];

  Result := TVCMIMap.CreateExisting(FMapEnv, cp);
  try
    try
      DeStreamTiles(map_o,Result);

      FDestreamer.JSONToObject(map_o, Result);

      FDestreamer.JSONToCollection(map_o.Elements[TEMPLATES_FIELD],Result.Templates);
      FDestreamer.JSONToCollection(map_o.Elements[OBJECTS_FIELD],Result.Objects);

    finally
      map_o.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

end.


