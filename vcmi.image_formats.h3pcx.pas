{ This file is a part of Map editor for VCMI project.

  Copyright (C) 2016-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit vcmi.image_formats.h3pcx;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FPImage, Graphics, IntfGraphics, GraphType, stream_adapter, editor_types;

  procedure LoadH3Pcx(ASourceStream: TStream; ADest: TLazIntfImage);

implementation

procedure LoadH3Pcx(ASourceStream: TStream; ADest: TLazIntfImage);
var
  size, width, height: UInt32;
  source: TStreamReadAdapter;

  procedure InitBitmap();
  begin
    ADest.SetSize(width, height);
  end;

  procedure LoadH3Pcx24;
  var
    c: TH3DefColor;
    i, j: Integer;
  begin

    ADest.BeginUpdate();
    try
      for i := 0 to height - 1 do
      begin
        for j := 0 to width - 1 do
        begin
          ASourceStream.Read(c, 3);
          ADest.Colors[j,i] := FPColor(word(c.r) shl 8 + c.r, word(c.g) shl 8 + c.g, word(c.b) shl 8 + c.b);
        end;
      end;
    finally
      ADest.EndUpdate()
    end;

  end;

  procedure LoadH3Pcx8;
  var
    initial_pos: Int64;
    c: TH3DefColor;
    buffer: packed array of byte;
    i, j: Integer;
    p: PByte;
  begin
    ADest.BeginUpdate();
    try
      ADest.UsePalette:=true;
      ADest.Palette.Create(256);
      initial_pos := ASourceStream.Position;

      //load palette from end of file
      ASourceStream.Seek(size, soCurrent);

      for i := 0 to 256 - 1 do
      begin
        ASourceStream.Read(c, 3);

        ADest.Palette.Color[i] := FPColor(word(c.r) shl 8 + c.r, word(c.g) shl 8 + c.g, word(c.b) shl 8 + c.b);
      end;

      //load graphics itself
      ASourceStream.Seek(initial_pos, soBeginning);

      SetLength(buffer, size);

      ASourceStream.Read(buffer[0], size);

      p := @buffer[0];

      for i := 0 to height - 1 do
      begin
        for j := 0 to width - 1 do
        begin
          ADest.Pixels[j, i] := p^;
          inc(p);
        end;
      end;
    finally
      ADest.EndUpdate();
    end;
  end;
begin
  source.Create(ASourceStream);

  size := source.ReadDWord;
  width := source.ReadDWord;
  height := source.ReadDWord;

  InitBitmap();

  if size = width * height * 3 then
  begin
    LoadH3Pcx24();
  end
  else if size = width * height then
  begin
    LoadH3Pcx8();
  end;
end;


end.

