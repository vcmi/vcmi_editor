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

{
  Single JSON file format
}

unit map_format_vcmi;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, RegExpr, editor_types, map, map_format, terrain,
  vcmi_json, vcmi_fpjsonrtti, fpjson, lists_manager, map_format_json;

type

  { TMapReaderVCMI }

  TMapReaderVCMI = class(TMapReaderJson, IMapReader)
  private
    procedure DeStreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);

  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;

    function Read(AStream: TStream): TVCMIMap;
  end;

  { TMapWriterVCMI }

  TMapWriterVCMI = class(TMapWriterJson, IMapWriter)
  private
    procedure StreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;


implementation

uses
  typinfo;

{ TMapWriterVCMI }

constructor TMapWriterVCMI.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
end;

destructor TMapWriterVCMI.Destroy;
begin
  inherited Destroy;
end;

procedure TMapWriterVCMI.StreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
var
  main_array: TJSONData;
begin
  main_array :=  inherited StreamTiles(AMap);

  ARoot.Add(TILES_FIELD,main_array);
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


constructor TMapReaderVCMI.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
end;

procedure TMapReaderVCMI.DeStreamTiles(ARoot: TJSONObject; AMap: TVCMIMap);
begin
  Inherited DeStreamTiles(ARoot.Arrays[TILES_FIELD], AMap);
end;

destructor TMapReaderVCMI.Destroy;
begin
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


