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
  Classes, SysUtils, map, map_format, terrain, vcmi_json, vcmi_fpjsonrtti, fpjson, lists_manager;

type

  { TMapReaderVCMI }

  TMapReaderVCMI = class(TBaseMapFormatHandler, IMapReader)
  private
    FDestreamer: TVCMIJSONDestreamer;

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

procedure TMapWriterVCMI.StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
  const Level: Integer);
var
  row: Integer;
  col: Integer;

  o: TJSONObject;
  //s: string;
  tile: TMapTile;
begin
  for row := 0 to AMap.Height - 1 do
  begin
    for col := 0 to AMap.Width - 1 do
    begin
      //todo: string encoding
      //o:= FStreamer.ObjectToJSON(AMap.GetTile(Level,Col,Row));
      //gr55Rcb14
      tile := AMap.GetTile(Level,Col,Row);
      //s := Format('%d %d %d %d %d %d %d', [
      //  Integer(tile.TerType), tile.TerSubType,
      //  tile.RiverType,tile.RiverDir,
      //  tile.RoadType, tile.RoadDir,
      //  tile.Flags]);
      o := FStreamer.ObjectToJSON(tile);
      AJson.Add(o);
      //AJson.Add(s);
    end;
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
begin
  inherited Create(AMapEnv);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
  FDestreamer.CaseInsensitive := True;
  FDestreamer.OnBeforeReadObject := @BeforeReadObject;
end;

procedure TMapReaderVCMI.DestreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
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
begin
  for row := 0 to AMap.Height - 1 do
  begin
    for col := 0 to AMap.Width - 1 do
    begin
      //todo: new format
      tile := AMap.GetTile(Level,col,row);
      idx := row*AMap.Height+col;
      d := AJson.Items[idx];

      case d.JSONType of
        jtObject:begin
          o := d as TJSONObject;
          FDestreamer.JSONToObject(o,tile);
        end;
        jtString:begin

        end;
      else
        raise Exception.CreateFmt('Invalid tile format at  L: %d, row: %d, col: %d',[Level, row, col]);
      end;

      //o := AJson.Objects[idx];


    end;
  end;
end;

destructor TMapReaderVCMI.Destroy;
begin
  FDestreamer.Free;
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


