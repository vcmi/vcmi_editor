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
  Classes, SysUtils, map, map_format, terrain, vcmi_json, vcmi_fpjsonrtti, fpjson;

type

  { TMapReaderVCMI }

  TMapReaderVCMI = class(TBaseMapFormatHandler, IMapReader)
  private
    FDestreamer: TVCMIJSONDestreamer;

    procedure DeStreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
    procedure DeStreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap; const Level: Integer);
  public
    constructor Create(tm: TTerrainManager); override;
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
    constructor Create(tm: TTerrainManager); override;
    destructor Destroy; override;
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;


implementation


{ TMapWriterVCMI }

constructor TMapWriterVCMI.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
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

  ARoot.Add('tiles',main_array);
end;

procedure TMapWriterVCMI.StreamTilesLevel(AJson: TJSONArray; AMap: TVCMIMap;
  const Level: Integer);
var
  row: Integer;
  col: Integer;

  o: TJSONObject;
begin
  for row := 0 to AMap.Height - 1 do
  begin
    for col := 0 to AMap.Width - 1 do
    begin
      o:= FStreamer.ObjectToJSON(AMap.GetTile(Level,Col,Row));
      AJson.Add(o);
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

    map_o.Add('templates', d);

    d := FStreamer.StreamCollection(AMap.Objects);
    map_o.Add('objects',d);

    StreamTiles(map_o, AMap);

    json_text := map_o.FormatJSON([foUseTabchar], 1);

    AStream.Write(json_text[1], Length(json_text));
  finally
    map_o.Free;
  end;
end;

{ TMapReaderVCMI }

constructor TMapReaderVCMI.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
  FDestreamer.CaseInsensitive := True;
end;

procedure TMapReaderVCMI.DestreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
var
  main_array, level_array: TJSONArray;
  i: Integer;
begin
  main_array := ARoot.Arrays['tiles'];

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

  o: TJSONObject;
begin
  for row := 0 to AMap.Height - 1 do
  begin
    for col := 0 to AMap.Width - 1 do
    begin
      o := AJson.Objects[row*AMap.Height+col];

      FDestreamer.JSONToObject(o,AMap.GetTile(Level,col,row));
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

  //TODO: !!! Read object template ID before options

  map_o := FDestreamer.JSONStreamToJson(AStream) as TJSONObject;

  cp.Levels := map_o.Integers['Levels'];
  cp.Height := map_o.Integers['Height'];
  cp.Width  := map_o.Integers['Width'];

  Result := TVCMIMap.Create(FTM, cp);
  try
    try

      FDestreamer.JSONToObject(map_o, Result);

      FDestreamer.JSONToObject(map_o.Objects['templates'],Result.Templates);
      FDestreamer.JSONToObject(map_o.Objects['objects'],Result.Objects);

    finally
      map_o.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

end.


