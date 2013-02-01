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

const
  MAP_PROPERTY_NAME = 'Map';

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
  json:      TJSONObject; //root
  json_text: string;
  map_o:     TJSONObject;
begin
  json := TJSONObject.Create;
  try
    map_o := FStreamer.ObjectToJSON(AMap);
    StreamTiles(map_o, AMap);

    json.Objects[MAP_PROPERTY_NAME] := map_o;

    json_text := json.FormatJSON([foUseTabchar], 1);

    AStream.Write(json_text[1], Length(json_text));
  finally
    json.Free;
  end;
end;

{ TMapReaderVCMI }

constructor TMapReaderVCMI.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
  FDestreamer.CaseInsensitive := True;
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
  map_o := FDestreamer.JSONStreamToJSONObject(AStream, MAP_PROPERTY_NAME);

  cp.Levels := map_o.Integers['Levels'];
  cp.Height := map_o.Integers['Height'];
  cp.Width  := map_o.Integers['Width'];

  Result := TVCMIMap.Create(FTM, cp);
  try
    try

      FDestreamer.JSONToObject(map_o, Result);

    finally
      map_o.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

end.


