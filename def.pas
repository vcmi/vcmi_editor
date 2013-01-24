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
unit def;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, editor_types, gvector, Gl, GLext, editor_utils;

const
  TILE_SIZE = 32; //in pixels

type

  TH3DefColor = packed record
    r,g,b : UInt8;
  end;

  TH3Palette = packed array [0..255] of TH3DefColor;

  TRBGAColor = packed record   //todo: optimise
    r,g,b,a : UInt8;
  end;

  TRGBAPalette = array[0..255] of TRBGAColor;

  PH3DefHeader = ^TH3DefHeader;
  TH3DefHeader = packed record
    typ: UInt32;
    width: UInt32;
    height: UInt32;
    blockCount: UInt32;
    palette: TH3Palette;
  end;

  PH3DefEntryBlockHeader = ^TH3DefEntryBlockHeader;
  TH3DefEntryBlockHeader = packed record
    unknown1: UInt32;
    totalInBlock: UInt32;
    unknown2: UInt32;
    unknown3: UInt32;

//  char *names[length];
//  uint32_t offsets[length];

     //folowed by sprites
  end;

  TH3SpriteHeader = packed record
    prSize: UInt32;
    defType2: UInt32;
    FullWidth: UInt32;
    FullHeight: UInt32;
    SpriteWidth: UInt32;
    SpriteHeight: UInt32;
    LeftMargin: UInt32;
    TopMargin: UInt32;
  end;

  PDefEntry = ^TDefEntry;

  { TDefEntry }

  TDefEntry = object
    name: string;
    offcet: UInt32;
    group: Integer;
    TextureId: GLuint;

    FullWidth: UInt32;
    FullHeight: UInt32;

    raw_image: packed array of TRBGAColor;

    procedure Resize(w,h: Integer);
    procedure SetRaw (Ofs: Int32; color: TRBGAColor);

    procedure Bind(ATextureId: GLuint);
    procedure UnBind;
  end;

  TDefEntries = specialize gvector.TVector<TDefEntry>;

  { TDef }

  TDef = class
  strict private

    typ: UInt32;
    width: UInt32;
    height: UInt32;
    blockCount: UInt32;
    palette: TRGBAPalette;
    entries: TDefEntries;

    FTexturesBinded: boolean;

    function GetFrameCount: Integer; inline;
    procedure LoadSprite(AStream: TStream; const SpriteIndex: UInt8);
    procedure UnBindTextures;
  public
    constructor Create;
    destructor Destroy; override;
    (*H3 def format*)
    procedure LoadFromDefStream(AStream: TStream);

    procedure BindTextures;


    (*
      TileX,TileY: map coords of topleft tile
    *)
    procedure Render(const SpriteIndex: UInt8; X,Y: Integer; dim:integer = -1);

    property FrameCount: Integer read GetFrameCount;
  end;

implementation

const
  STANDARD_COLORS: array[0..7] of TRBGAColor = (
    (r: 0; g: 0; b:0; a: 0),
    (r: 0; g: 0; b:0; a: 192),
    (r: 0; g: 0; b:0; a: 128), //???
    (r: 0; g: 0; b:0; a: 128), //???
    (r: 0; g: 0; b:0; a: 128),
    (r: 0; g: 0; b:0; a: 0),
    (r: 0; g: 0; b:0; a: 128),
    (r: 0; g: 0; b:0; a: 192));

{ TDefEntry }

procedure TDefEntry.Bind(ATextureId: GLuint);
begin
  TextureId := ATextureId;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, TextureId);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA,FullWidth,FullHeight,0,GL_RGBA, GL_UNSIGNED_BYTE, @(raw_image[0]));
  glDisable(GL_TEXTURE_2D);
end;

procedure TDefEntry.Resize(w, h: Integer);
begin
  SetLength(raw_image, h * w);
end;

procedure TDefEntry.SetRaw(Ofs: Int32; color: TRBGAColor);
begin
  raw_image[ofs] := color;
end;

procedure TDefEntry.UnBind;
begin
  glDeleteTextures(1,@TextureId);
  TextureId := 0;
end;

{ TDef }

procedure TDef.BindTextures;
var
  id_s: array of GLuint;
  count: GLsizei;
  SpriteIndex: Integer;
begin
  count := entries.Size;

  SetLength(id_s,count);

  glGenTextures(count, @id_s[0]);

  for SpriteIndex := 0 to count - 1 do
  begin
    entries.Mutable[SpriteIndex]^.Bind(id_s[SpriteIndex]);
  end;

  FTexturesBinded := True;
end;

constructor TDef.Create;
begin
  entries := TDefEntries.Create;

end;

destructor TDef.Destroy;
begin
  if FTexturesBinded then
    UnBindTextures;

  entries.Destroy;
  inherited Destroy;
end;

function TDef.GetFrameCount: Integer;
begin
  Result := entries.Size;
end;

procedure TDef.LoadFromDefStream(AStream: TStream);
var
  total_entries: Integer;
  block_nomber: Integer;

  current_block_head: TH3DefEntryBlockHeader;
  total_in_block: Integer;

  current_offcet: UInt32;

  current_name: packed array [1..13] of char;
  i: Uint8;


  header: TH3DefHeader;
begin
  AStream.Seek(0,soBeginning);
  AStream.Read(header{%H-},SizeOf(header));


  typ := LEtoN(header.typ);
  height := LEtoN(header.height);
  blockCount := LEtoN(header.blockCount);
  width := LEtoN(header.width);

  for i := 0 to 7 do
  begin
    palette[i] := STANDARD_COLORS[i];
  end;

  for i := 8 to 255 do
  begin
    palette[i].a := 255; //no alpha in h3 def
    palette[i].b := header.palette[i].b;
    palette[i].g := header.palette[i].g;
    palette[i].r := header.palette[i].r;
  end;

  total_entries := 0;

  for block_nomber := 0 to header.blockCount - 1 do
  begin
     AStream.Read(current_block_head{%H-},SizeOf(current_block_head));

     total_in_block := current_block_head.totalInBlock;

     entries.Resize(total_entries + total_in_block);

     //names
     for i := 0 to total_in_block - 1 do
     begin
       AStream.Read(current_name{%H-},SizeOf(current_name));
       entries.Mutable[total_entries+i]^.name := current_name;
     end;

     //offcets
     for i := 0 to total_in_block - 1 do
     begin
       AStream.Read(current_offcet{%H-},SizeOf(current_offcet));
       entries.Mutable[total_entries+i]^.offcet := current_offcet;

       entries.Mutable[total_entries+i]^.group := block_nomber;
     end;

     total_entries += total_in_block;
  end;

  for i := 0 to entries.Size - 1 do
  begin
    LoadSprite(AStream, i);
  end;
end;

procedure TDef.LoadSprite(AStream: TStream; const SpriteIndex: UInt8);
var
  ftcp: Int32;

  procedure Skip(Count: Int32); inline;
  begin
    ftcp +=Count;
  end;

  procedure SkipIfPositive(Count: Int32); inline;
  begin
    if Count > 0 then
      Skip(Count);
  end;

  procedure SetColor(var C: TRBGAColor; const ColorIndex: Byte); inline;
  begin
    C := palette[ColorIndex];
  end;

  function GetColor(const ColorIndex: Byte): TRBGAColor;
  begin
    Result := palette[ColorIndex];
  end;

var
  h: TH3SpriteHeader;

  LeftMargin,TopMargin,
  RightMargin, BottomMargin: Int32;
  SpriteHeight, SpriteWidth: Int32;

  add: Int32;

  BaseOffset: Int32;
  BaseOffsetor: Int32;

  PEntry: PDefEntry;

  procedure ReadType0;
  var
    i,j: Int32;
  begin
    for i:=0 to SpriteHeight - 1 do
    begin
      SkipIfPositive(LeftMargin);

      for j := 0 to SpriteWidth - 1 do
      begin
        PEntry^.SetRaw(i * Int32(h.FullWidth) + j + ftcp, GetColor(AStream.ReadByte()));
      end;

      SkipIfPositive(RightMargin);
    end;
  end;

  procedure ReadType1;
  var
    RWEntriesLoc: Int32;
    row: Integer;
    TotalRowLength : Int32;
    RowAdd: UInt32;
    SegmentLength: Int32;
    SegmentType: UInt8;
    i: Int32;
    C: TRBGAColor;
  begin
    //const ui32 * RWEntriesLoc = reinterpret_cast<const ui32 *>(FDef+BaseOffset);

    //BaseOffset += sizeof(int) * SpriteHeight;
    for row := 0 to SpriteHeight - 1 do
    begin
      AStream.Seek(BaseOffsetor + SizeOf(UInt32)*row, soFromBeginning);
      AStream.Read(BaseOffset,SizeOf(BaseOffset));


      SkipIfPositive(LeftMargin);
      TotalRowLength :=  0;
      repeat
         SegmentType := AStream.ReadByte;
         SegmentLength := AStream.ReadByte + 1;

         if SegmentType = $FF then
         begin
           AStream.Seek(BaseOffsetor+Int32(BaseOffset), soFromBeginning);
           for i := 0 to SegmentLength - 1 do
           begin
             SetColor(PEntry^.raw_image[i + ftcp], AStream.ReadByte());
           end;
           BaseOffset += SegmentLength;
         end
         else begin
           SetColor(C{%H-}, SegmentType);
           for i := 0 to SegmentLength - 1 do
           begin
             PEntry^.raw_image[i + ftcp] := C;
           end;

         end;

         Skip(SegmentLength);

         TotalRowLength += SegmentLength;
       until TotalRowLength>=(SpriteWidth);
       RowAdd := Uint32(SpriteWidth) - TotalRowLength;

       SkipIfPositive(RightMargin);

       if add >0 then
         Skip(add+Int32(RowAdd));
     end;
  end;

  procedure ReadType2();
  begin
    raise Exception.Create('Unknown sprite compression format');  //TODO: format 2
  end;

  procedure ReadType3();
  var
    row: UInt32;
    tmp: UInt16;
    TotalRowLength : Int32;

    SegmentType, code, value: UInt8;
    len: Integer;

    i: Int32;
    RowAdd: Int32;
  begin
    for row := 0 to SpriteHeight - 1 do
    begin
      AStream.Seek(Uint32(BaseOffsetor)+row*2*(Uint32(SpriteWidth) div 32),soFromBeginning);
      tmp := AStream.ReadWord();
      BaseOffset := BaseOffsetor + tmp;
      SkipIfPositive(LeftMargin);
      TotalRowLength := 0;

      AStream.Seek(BaseOffset,soFromBeginning);

      repeat
         SegmentType := AStream.ReadByte;
         code := SegmentType div 32;
         value := (SegmentType and 31) + 1;

         len := Min(Int32(value), SpriteWidth - TotalRowLength) - Max(0, -LeftMargin);

         len := Max(0,len);

         if code = 7 then
         begin
           for i := 0 to len - 1 do
           begin
             PEntry^.SetRaw(i+ftcp, GetColor(AStream.ReadByte()));
           end;
         end
         else begin
           for i := 0 to len - 1 do
           begin
             PEntry^.SetRaw(i+ftcp, GetColor(Code));
           end;
         end;
         Skip(len);

         TotalRowLength += ifthen(LeftMargin>=0,value, value+LeftMargin );

      until TotalRowLength>=SpriteWidth;

      SkipIfPositive(RightMargin);

      RowAdd := SpriteWidth-TotalRowLength;

      if (add>0) then
         ftcp += add+RowAdd;

    end;
  end;

begin

  PEntry := entries.Mutable[SpriteIndex];

  BaseOffset := PEntry^.offcet;

  AStream.Seek(BaseOffset,soFromBeginning);
  AStream.Read(h{%H-},SizeOf(h));

  BaseOffset := AStream.Position;
  BaseOffsetor := BaseOffset;

  with h do
  begin
    LeToNInPlase(prSize);
    LeToNInPlase(defType2);
    LeToNInPlase(FullHeight);
    LeToNInPlase(FullWidth);
    LeToNInPlase(SpriteHeight);
    LeToNInPlase(SpriteWidth);
    LeToNInPlase(LeftMargin);
    LeToNInPlase(TopMargin);
  end;

  PEntry^.FullHeight := h.FullHeight;
  PEntry^.FullWidth := h.FullWidth;

  LeftMargin := h.LeftMargin;
  TopMargin := h.TopMargin;

  SpriteHeight := h.SpriteHeight;
  SpriteWidth := h.SpriteWidth;

  RightMargin := Int32(h.FullWidth) - SpriteWidth - LeftMargin;
  BottomMargin := Int32(h.FullHeight) - SpriteHeight - TopMargin;

  if LeftMargin<0 then
     SpriteWidth+=LeftMargin;
  if RightMargin<0 then
     SpriteWidth+=RightMargin;

  add := 4 - (h.FullWidth mod 4);
  if add = 4 then
     add :=0;

  SetLength(PEntry^.raw_image, h.FullWidth*h.FullHeight);

  if (TopMargin > 0) or (BottomMargin > 0) or (LeftMargin > 0) or (RightMargin > 0) then
    FillDWord(PEntry^.raw_image[0], Length(PEntry^.raw_image) ,$00000000);

  ftcp := 0;

  // Skip top margin
  if TopMargin > 0 then
  begin
    Skip(TopMargin*(Int32(h.FullWidth)+add));
  end;

  case h.defType2 of
    0:begin
      ReadType0;
    end;
    1:begin
      ReadType1; //TODO: test format 1
    end;
    2:begin
      ReadType2();
    end;
    3:begin
      ReadType3();
    end
  else
    raise Exception.Create('Unknown sprite compression format');
  end;

end;

procedure TDef.Render(const SpriteIndex: UInt8; X, Y: Integer; dim: integer);
var
  cx: Integer;
  cy: Integer;

  factor: Double;
  cur_dim: integer;
  H: Int32;
  W: Int32;
begin

  if dim <=0 then //render real size w|o scale
  begin
    H := height;
    W := width;
  end
  else
  begin
    cur_dim := Max(width,height);
    factor := dim / cur_dim;

    h := round(Double(height) * factor);
    w := round(Double(width) * factor);
  end;

  cx := X;
  cy := Y;

  glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,entries[SpriteIndex].TextureId);
    glBegin(GL_POLYGON);

      glTexCoord2d(0,0); glVertex2i(cx,  cy);
      glTexCoord2d(1,0); glVertex2i(cx+W,cy);
      glTexCoord2d(1,1); glVertex2i(cx+W,cy+H);
      glTexCoord2d(0,1); glVertex2i(cx,  cy+H);

    glEnd();

  glDisable(GL_TEXTURE_2D);
end;

procedure TDef.UnBindTextures;
var
  SpriteIndex: Integer;
begin
  for SpriteIndex := 0 to entries.Size - 1 do
  begin
    entries.Mutable[SpriteIndex]^.UnBind();
  end;
end;

end.

