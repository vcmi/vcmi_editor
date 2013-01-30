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
  public
    constructor Create(tm: TTerrainManager); override;
    function Read(AStream: TStream): TVCMIMap;
  end;

  { TMapWriterVCMI }

  TMapWriterVCMI = class(TBaseMapFormatHandler, IMapWriter)
  public
    constructor Create(tm: TTerrainManager); override;
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;


implementation

{ TMapWriterVCMI }

constructor TMapWriterVCMI.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
end;

procedure TMapWriterVCMI.Write(AStream: TStream; AMap: TVCMIMap);
var
  json, map_obj:     TJSONObject; //root
  streamer: TJSONStreamer;

  json_text: string;
begin
  json     := TJSONObject.Create;
  streamer := TJSONStreamer.Create(nil);
  try
    json.Objects['map'] := streamer.ObjectToJSON(AMap);

    json_text := json.FormatJSON([foUseTabchar],1);

    AStream.Write(json_text[1],Length(json_text));
  finally
    streamer.Free;
    json.Free;
  end;
end;

{ TMapReaderVCMI }

constructor TMapReaderVCMI.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
end;

function TMapReaderVCMI.Read(AStream: TStream): TVCMIMap;
begin
  Result := TVCMIMap.Create(FTM); //TODO:  TMapReaderVCMI.Read
end;

end.


