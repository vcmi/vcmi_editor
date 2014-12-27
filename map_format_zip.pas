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
  Multiple JSON files in zip archive format
}

unit map_format_zip;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, contnrs, editor_types, map, map_format, terrain,
  vcmi_json, vcmi_fpjsonrtti, fpjson, lists_manager, map_format_json, zipper;

type

  { TMapReaderZIP }

  TMapReaderZIP = class(TMapReaderJson, IMapReader)
  public
    function Read(AStream: TStream): TVCMIMap;
  end;

  { TMapWriterZIP }

  TMapWriterZIP = class(TMapWriterJson, IMapWriter)
  private
    FZipper: TZipper;
    FFreeList: TFPObjectList;
    procedure WriteHeader(AMap: TVCMIMap);
    procedure WriteTemplates(AMap: TVCMIMap);
    procedure WriteObjects(AMap: TVCMIMap);
    procedure WriteTerrain(AMap: TVCMIMap);

    procedure AddArchiveEntry(AData: TJSONData; AFilename: AnsiString);
  public
    constructor Create(AMapEnv: TMapEnvironment); override;
    destructor Destroy; override;
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;

implementation


{ TMapReaderZIP }

function TMapReaderZIP.Read(AStream: TStream): TVCMIMap;
begin

end;

{ TMapWriterZIP }

procedure TMapWriterZIP.WriteHeader(AMap: TVCMIMap);
begin
  AddArchiveEntry( FStreamer.ObjectToJSON(AMap), 'header.json');
  //todo: separate real header
end;

procedure TMapWriterZIP.WriteTemplates(AMap: TVCMIMap);
begin
  AddArchiveEntry(FStreamer.StreamCollection(AMap.Templates),'templates.json');
end;

procedure TMapWriterZIP.WriteObjects(AMap: TVCMIMap);
begin
  AddArchiveEntry(FStreamer.StreamCollection(AMap.Objects),'objects.json');
end;

procedure TMapWriterZIP.WriteTerrain(AMap: TVCMIMap);
begin
  AddArchiveEntry(StreamTiles(AMap),'terrain.json');
end;

procedure TMapWriterZIP.AddArchiveEntry(AData: TJSONData; AFilename: AnsiString
  );
var
  json_text: TJSONStringType;
  Stream: TStringStream;
begin
  json_text := AData.FormatJSON([foUseTabchar], 1);

  Stream := TStringStream.Create(json_text);

  FZipper.Entries.AddFileEntry(Stream, AFilename);

  FFreeList.Add(Stream);

  FreeAndNil(AData);
end;

constructor TMapWriterZIP.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
  FZipper := TZipper.Create;
  FFreeList := TFPObjectList.Create(true);
end;

destructor TMapWriterZIP.Destroy;
begin
  FFreeList.Free;
  FZipper.Free;
  inherited Destroy;
end;

procedure TMapWriterZIP.Write(AStream: TStream; AMap: TVCMIMap);
begin

  WriteHeader(AMap);
  WriteTemplates(AMap);
  WriteObjects(AMap);
  WriteTerrain(AMap);
  FZipper.SaveToStream(AStream);
end;

end.

