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
unit lod;

{$I compilersetup.inc}

interface

uses
  SysUtils, Classes, gvector;

//Format Specifications
//
//char {4}     - Header ("LOD\0")
//byte {4}     - "file use flag", i.e. used as 'base' or 'extension' resource file. 200 if base resource file, 500 if extension, little endian values.
//uint32 {4}   - Number Of Files
//byte {80}    - Unknown
//
//// for each file
//
//    char {16}    - Filename (null)
//    uint32 {4}   - File Offset
//    uint32 {4}   - Uncompressed file size
//    uint32 {4}   - File type(?)
//    uint32 {4}   - File Length
//
//
//byte {X}     - File Data
//

type

  TLod = class;

  TLodItem = packed record
    Filename:   array [0..16 - 1] of AnsiChar;
    FileOffset: UInt32;
    UncompressedFileSize: UInt32;
    FileType:   UInt32;
    FileLength: UInt32;
  end;

  //todo: add match filter flag to not to store filtered items
  TOnItemFound = procedure (Alod: TLod; constref AItem: TLodItem) of object;

  { TLod }

  TLod = class
  private
    FFileStream: TFileStream;

  public
    constructor Create(AFullPath: string);
    destructor Destroy; override;

    procedure Scan(ACallback: TOnItemFound);

    procedure LoadToStream(AStream: TStream; constref AItem:TLodItem);
  end;

implementation

uses editor_utils, zlib_stream;

const
  LOD_MAGIC: packed array[1..4] of char = ('L','O','D',#0);
  LOD_HEADER_SIZE = 4+4+4+80;

{ TLod }

constructor TLod.Create(AFullPath: string);
begin
  FFileStream := TFileStream.Create(AFullPath, fmOpenRead+fmShareDenyWrite);
end;

destructor TLod.Destroy;
begin
  FFileStream.Free;
  inherited Destroy;
end;

procedure TLod.LoadToStream(AStream: TStream; constref AItem: TLodItem);
var
  stm: TZlibStream;
begin
  FFileStream.Seek(AItem.FileOffset,soBeginning);
  if AItem.FileLength <> 0 then
  begin
    stm := TZlibStream.Create(FFileStream,AItem.UncompressedFileSize);
    AStream.CopyFrom(stm,AItem.UncompressedFileSize);
    stm.free;
  end
  else begin
    AStream.CopyFrom(FFileStream,AItem.UncompressedFileSize);
  end;
end;

procedure TLod.Scan(ACallback: TOnItemFound);
var
  nomber_of_files: UInt32;

  item: TLodItem;
  i: Integer;

  magic: packed array[1..4] of char;
begin
  if FFileStream.Size < LOD_HEADER_SIZE then
  begin
    Exit; //too small, may be empty, ignore silently
  end;
  FFileStream.Seek(0,soBeginning);

  FFileStream.Read(magic{%H-},SizeOf(magic));

  if magic <> LOD_MAGIC then
    raise Exception.Create('Wrong LOD archive');

  FFileStream.Seek(8,soBeginning);

  nomber_of_files := FFileStream.ReadDWord;

  FFileStream.Seek(LOD_HEADER_SIZE,soBeginning);

  for i := 0 to nomber_of_files - 1 do
  begin
    FFileStream.Read(item{%H-},SizeOf(item));

    LeToNInPlase(item.FileLength);
    LeToNInPlase(item.FileOffset);
    LeToNInPlase(item.FileType);
    LeToNInPlase(item.UncompressedFileSize);

    ACallback(self,item);
  end;
end;

end.
