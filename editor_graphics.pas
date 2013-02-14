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
unit editor_graphics;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math,
  gvector, fgl,
  Gl,
  editor_types, editor_utils, filesystem_base, editor_gl;

const
  TILE_SIZE = 32; //in pixels

type
  TRBGAColor = packed record   //todo: optimise
    r,g,b,a : UInt8;
  end;

  TRGBAPalette = array[0..255] of TRBGAColor;

  { TBaseSprite }

  TBaseSprite = object
    TextureId: GLuint;
    raw_image: packed array of TRBGAColor;

    procedure Resize(w,h: Integer);
    procedure SetRaw (Ofs: Int32; color: TRBGAColor); inline;
    procedure UnBind; inline;
  end;

  PDefEntry = ^TDefEntry;

  { TDefEntry }

  TDefEntry = object (TBaseSprite)

    offcet: UInt32;
    group: Integer;

    FullWidth: UInt32;
    FullHeight: UInt32;

    procedure Bind(ATextureId: GLuint);
  end;

  TDefEntries = specialize gvector.TVector<TDefEntry>;

  { TFlagTexture }

  TFlagTexture = object (TBaseSprite)
    //h_ofc, v_ofc: Int32;
    //w,h: Int32;
    procedure Bind(ATextureId: GLuint; w,h: Int32);
  end;

  TFlagTextures = array[TPlayerColor] of TFlagTexture;

  { TDef }

  TDef = class (IResource)
  strict private

    typ: UInt32;
    width: UInt32;
    height: UInt32;
    blockCount: UInt32;
    palette: TRGBAPalette;
    entries: TDefEntries;

    flag_entries: TFlagTextures;

    flag_entry_h_ofc, flag_entry_v_ofc, flag_entry_w, flag_entry_h: int32;

    FFlaggable: Boolean;

    FTexturesBinded: boolean;

    function GetFrameCount: Integer; inline;

    procedure PreInitFlagEntries(w,h: Int32);
    procedure CompressFlagEntries();

    procedure LoadSprite(AStream: TStream; const SpriteIndex: UInt8);
    procedure MayBeUnBindTextures; inline;
    procedure UnBindTextures;
  private //for internal use
     (*H3 def format*)
    procedure LoadFromStream(AStream: TStream);
    procedure BindTextures;

    procedure RenderFlag (var Sprite: TGLSprite; dim:integer; color: TPlayer); inline;
  public
    constructor Create;
    destructor Destroy; override;

    (*
      X,Y: map coords of topleft tile
    *)
    procedure Render(const SpriteIndex: UInt8; X,Y: Integer; dim:integer; color: TPlayer = TPlayer.none);
    procedure RenderF(const SpriteIndex: UInt8; X,Y: Integer; flags:UInt8);

    procedure RenderO (const SpriteIndex: UInt8; X,Y: Integer; color: TPlayer = TPlayer.none);

    property FrameCount: Integer read GetFrameCount;

    property Flaggable: Boolean read FFlaggable;
  end;

  TDefMapBase = specialize fgl.TFPGMap<string,TDef>;

  { TDefMap }

  TDefMap = class (TDefMapBase)
  protected
    procedure Deref(Item: Pointer); override;
  public
    constructor Create;
  end;

  { TGraphicsManager }

  TGraphicsManager = class (TFSConsumer)
  private
    FNameToDefMap: TDefMap;

    FBuffer: TMemoryStream;

    procedure LoadDef(const AResourceName:string; ADef: TDef);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetGraphics (const AResourceName:string): TDef;

    procedure BindTextures;  deprecated;
  end;

  { TGraphicsCosnumer }

  TGraphicsCosnumer = class abstract (TFSConsumer)
  private
    FGraphicsManager: TGraphicsManager;
    procedure SetGraphicsManager(AValue: TGraphicsManager);
  public
    constructor Create(AOwner: TComponent); override;
    property GraphicsManager:TGraphicsManager read FGraphicsManager write SetGraphicsManager;
  end;

implementation


type

  TH3DefColor = packed record
    r,g,b : UInt8;
  end;

  TH3Palette = packed array [0..255] of TH3DefColor;

  TH3DefHeader = packed record
    typ: UInt32;
    width: UInt32;
    height: UInt32;
    blockCount: UInt32;
    palette: TH3Palette;
  end;

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

const
  STANDARD_COLORS: array[0..7] of TRBGAColor = (
    (r: 0; g: 0; b:0; a: 0),
    (r: 0; g: 0; b:0; a: 64),
    (r: 0; g: 0; b:0; a: 128), //???
    (r: 0; g: 0; b:0; a: 128), //???
    (r: 0; g: 0; b:0; a: 128),
    (r: 128; g: 128; b:128; a: 255),  //player color
    (r: 0; g: 0; b:0; a: 128),
    (r: 0; g: 0; b:0; a: 64));

  PLAYER_COLOR_INDEX = 5;

  PLAYER_FLAG_COLORS: array[TPlayerColor] of TRBGAColor = (
    (r: 255; g: 0;   b:0;   a: 255),//RED
    (r: 0;   g: 0;   b:255; a: 255),//BLUE
    (r: 120; g: 180; b:140; a: 255),//TAN
    (r: 0;   g: 255; b:0;   a: 255),//GREEN
    (r: 255; g: 165; b:0;   a: 255),//ORANGE
    (r: 128; g: 0;   b:128; a: 255),//PURPLE
    (r: 0;   g: 255; b:255; a: 255),//TEAL
    (r: 255; g: 192; b:203; a: 255) //PINK
  );

  //DEF_TYPE_MAP_OBJECT = $43;

function CompareDefs(const d1,d2: TDef): integer;
begin
  Result := PtrInt(d1) - PtrInt(d2);
end;


{ TBaseSprite }

procedure TBaseSprite.Resize(w, h: Integer);
begin
  SetLength(raw_image, h * w);
  FillChar(raw_image[0],Length(raw_image)*SizeOf(TRBGAColor),#0);
end;

procedure TBaseSprite.SetRaw(Ofs: Int32; color: TRBGAColor);
begin
  //if Ofs < Length(raw_image) then //todo: investigate range overflow in type2
  raw_image[ofs] := color;
end;

procedure TBaseSprite.UnBind;
begin
  editor_gl.Unbind(TextureId);
end;

{ TFlagTexture }

procedure TFlagTexture.Bind(ATextureId: GLuint; w, h: Int32);
begin
  TextureId := ATextureId;

  BindCompressedRGBA(TextureId,w,h, raw_image[0]);

  SetLength(raw_image,0); //clear

end;

{ TGraphicsCosnumer }

constructor TGraphicsCosnumer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if AOwner is TGraphicsManager then
  begin
    GraphicsManager := (AOwner as TGraphicsManager);
  end;
end;

procedure TGraphicsCosnumer.SetGraphicsManager(AValue: TGraphicsManager);
begin
  if FGraphicsManager = AValue then Exit;
  FGraphicsManager := AValue;
end;

{ TGraphicsManager }

procedure TGraphicsManager.BindTextures;
var
  i: Integer;
begin
  for i := 0 to FNameToDefMap.Count - 1 do
  begin
    FNameToDefMap.Data[i].BindTextures;
  end;
end;

constructor TGraphicsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameToDefMap := TDefMap.Create;
  FBuffer := TMemoryStream.Create;
end;

destructor TGraphicsManager.Destroy;
begin
  FBuffer.Free;
  FNameToDefMap.Free;
  inherited Destroy;
end;

function TGraphicsManager.GetGraphics(const AResourceName: string): TDef;
var
  res_index: Integer;
begin

  res_index :=  FNameToDefMap.IndexOf(AResourceName);

  if res_index >= 0 then
  begin
    Result := FNameToDefMap.Data[res_index];
  end
  else begin
    Result := TDef.Create;
    LoadDef(AResourceName,Result);
    FNameToDefMap.Add(AResourceName,Result);
  end;

end;

procedure TGraphicsManager.LoadDef(const AResourceName: string; ADef: TDef);
begin
  ResourceLoader.LoadResource(Adef,TResourceType.Animation,'SPRITES/'+AResourceName);
end;


{ TDefMap }

constructor TDefMap.Create;
begin
  inherited Create;
  OnKeyCompare := @CompareStr;
  OnDataCompare := @CompareDefs;

  Sorted := True;
end;

procedure TDefMap.Deref(Item: Pointer);
begin
  //inherited Deref(Item);
  Finalize(string(Item^));
  TDef(Pointer(PByte(Item)+KeySize)^).Free;
end;

{ TDefEntry }

procedure TDefEntry.Bind(ATextureId: GLuint);
begin
  TextureId := ATextureId;

  BindUncompressedRGBA(TextureId,FullWidth,FullHeight, raw_image[0]);

  SetLength(raw_image,0); //clear
end;




{ TDef }

procedure TDef.BindTextures;
var
  id_s: array of GLuint;
  count: GLsizei;
  SpriteIndex: Integer;
  flag: TPlayerColor;

  procedure Regen; inline;
  begin
    SetLength(id_s,count);
    glGenTextures(count, @id_s[0]);
  end;

begin
  if FTexturesBinded then Exit;
  count := entries.Size;

  Regen;

  for SpriteIndex := 0 to count - 1 do
  begin
    entries.Mutable[SpriteIndex]^.Bind(id_s[SpriteIndex]);
  end;

  if FFlaggable then
  begin
    count := 8;
    Regen;
    for flag in TPlayerColor do
    begin
      flag_entries[flag].Bind(id_s[Integer(flag)], width,height );
    end;
  end;

  FTexturesBinded := True;
end;

procedure TDef.CompressFlagEntries;
var
  flag: TPlayerColor;
begin
  //TODO: CompressFlagEntries
  for flag in TPlayerColor do
  begin
    //flag_entries[flag].SetRaw(ofc+i, PLAYER_FLAG_COLORS[flag]);
  end;
end;

constructor TDef.Create;
begin
  entries := TDefEntries.Create;
  FTexturesBinded := false;
end;

destructor TDef.Destroy;
begin
  MayBeUnBindTextures;

  entries.Destroy;
  inherited Destroy;
end;

function TDef.GetFrameCount: Integer;
begin
  Result := entries.Size;
end;

procedure TDef.LoadFromStream(AStream: TStream);
var
  total_entries: Integer;
  block_nomber: Integer;

  current_block_head: TH3DefEntryBlockHeader;
  total_in_block: Integer;

  current_offcet: UInt32;

  current_name: packed array [1..13] of char;
  i: Uint8;


  header: TH3DefHeader;
  orig_position: Int32;
begin
  if FTexturesBinded then
    UnBindTextures;
  FTexturesBinded := false;
  FFlaggable := False;
  orig_position := AStream.Position;

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
       //entries.Mutable[total_entries+i]^.name := current_name;
     end;

     //offcets
     for i := 0 to total_in_block - 1 do
     begin
       AStream.Read(current_offcet{%H-},SizeOf(current_offcet));
       entries.Mutable[total_entries+i]^.offcet := current_offcet+UInt32(orig_position);

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

var
  h: TH3SpriteHeader;

  LeftMargin,TopMargin,
  RightMargin, BottomMargin: Int32;
  SpriteHeight, SpriteWidth: Int32;

  add: Int32;

  BaseOffset: Int32;
  BaseOffsetor: Int32;

  PEntry: PDefEntry;

  procedure SetRaw(const ColorIndex: Byte; ofc: int32; len: integer = 1); inline;
  var
    C: TRBGAColor;
    i: Integer;
    entry: TFlagTexture;
    flag: TPlayerColor;
  begin
    c := palette[ColorIndex];

    for i := 0 to len - 1 do
    begin
      PEntry^.SetRaw(ofc+i,c)
    end;

    if (ColorIndex = PLAYER_COLOR_INDEX) and (SpriteIndex = 0) then
    begin
      if (not FFlaggable) then
      begin
        PreInitFlagEntries(h.FullWidth,h.FullHeight);
      end;

      for flag in TPlayerColor do
      begin
        for i := 0 to len - 1 do
        begin
          flag_entries[flag].SetRaw(ofc+i, PLAYER_FLAG_COLORS[flag]);
        end;
      end;
      //row := ofc div h.FullWidth;
      //col := ofc mod h.FullWidth;

      //todo: update flag dimentions
    end;

  end;

  procedure ReadType0;
  var
    i,j: Int32;
  begin
    for i:=0 to SpriteHeight - 1 do
    begin
      SkipIfPositive(LeftMargin);

      for j := 0 to SpriteWidth - 1 do
      begin
        SetRaw(AStream.ReadByte(), i * Int32(h.FullWidth) + j + ftcp );
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
             SetRaw(AStream.ReadByte(), i + ftcp);
           end;
           BaseOffset += SegmentLength;
         end
         else begin
           SetRaw(SegmentType, ftcp, SegmentLength);
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
  var
    row: Integer;
    TotalRowLength: Integer;
    SegmentType, code, value: Byte;
    i: Integer;
    RowAdd: Int32;
  begin
    BaseOffset := BaseOffsetor + AStream.ReadWord();
    AStream.Seek(BaseOffset,soBeginning);

    for row := 0 to SpriteHeight - 1 do
    begin
      SkipIfPositive(LeftMargin);

      TotalRowLength:=0;
      repeat
         SegmentType := AStream.ReadByte();
         code := SegmentType div 32;
         value := (SegmentType and 31) + 1;

         if code=7 then
         begin
           for i := 0 to value - 1 do
           begin
             SetRaw(AStream.ReadByte(), i + ftcp);
           end;
         end
         else begin
           for i := 0 to value - 1 do
           begin
             SetRaw(Code, i + ftcp);
           end;
         end;
         Skip(Value);
         TotalRowLength+=value;

      until TotalRowLength >= SpriteWidth ;


      SkipIfPositive(RightMargin);

      RowAdd := SpriteWidth-TotalRowLength;

      if (add>0) then
         ftcp += add+RowAdd;
    end;
  end;

  procedure ReadType3();
  var
    row: Int32;
    tmp: UInt16;
    TotalRowLength : Int32;

    SegmentType, code, value: UInt8;
    len: Integer;

    i: Int32;
    RowAdd: Int32;
  begin
    for row := 0 to SpriteHeight - 1 do
    begin
      AStream.Seek(BaseOffsetor+row*2*(SpriteWidth div 32),soBeginning);
      tmp := AStream.ReadWord();
      BaseOffset := BaseOffsetor + tmp;
      SkipIfPositive(LeftMargin);
      TotalRowLength := 0;

      AStream.Seek(BaseOffset,soBeginning);

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
             SetRaw(AStream.ReadByte(), i + ftcp);
           end;
         end
         else begin
           for i := 0 to len - 1 do
           begin
             SetRaw(Code, i + ftcp);
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

  AStream.Seek(BaseOffset,soBeginning);
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

  PEntry^.Resize(h.FullWidth,h.FullHeight);



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
      ReadType0();
    end;
    1:begin
      ReadType1(); //TODO: test format 1
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

  if FFlaggable then
    CompressFlagEntries;

end;

procedure TDef.MayBeUnBindTextures;
begin
  if FTexturesBinded then
    UnBindTextures;
  FTexturesBinded := False;
end;

procedure TDef.PreInitFlagEntries(w, h: Int32);
var
  flag: TPlayerColor;
begin
  FFlaggable := True;
  for flag in TPlayerColor do
  begin
    flag_entries[flag].Resize(w,h);
  end;

  flag_entry_h := 0;
  flag_entry_h_ofc := 0;
  flag_entry_v_ofc := 0;
  flag_entry_w := 0;

end;

procedure TDef.Render(const SpriteIndex: UInt8; X, Y: Integer; dim: integer;
  color: TPlayer);
var
  Sprite: TGLSprite;
begin
  Sprite.TextureID := entries[SpriteIndex].TextureId;
  Sprite.X := X;
  Sprite.Y := Y;
  Sprite.Height := height;
  Sprite.Width := width;

  editor_gl.RenderSprite(Sprite, dim);

  RenderFlag(Sprite, dim, color);
end;

procedure TDef.RenderF(const SpriteIndex: UInt8; X, Y: Integer; flags: UInt8);
var
  mir: UInt8;
  Sprite: TGLSprite;
begin
  Sprite.X := X;
  Sprite.Y := Y;
  Sprite.Height := height;
  Sprite.Width := width;
  Sprite.TextureID := entries[SpriteIndex].TextureId;

  mir := flags mod 4;

  editor_gl.RenderSprite(Sprite,-1,mir);

end;

procedure TDef.RenderFlag(var Sprite: TGLSprite; dim: integer; color: TPlayer);
begin
  if Flaggable and (color in [TPlayer.RED..TPlayer.PINK]) then
  begin
    Sprite.TextureID := flag_entries[color].TextureId;
    editor_gl.RenderSprite(Sprite, dim);
  end;

end;

procedure TDef.RenderO(const SpriteIndex: UInt8; X, Y: Integer; color: TPlayer);
var
  Sprite: TGLSprite;
begin
  Sprite.X := X - width;
  Sprite.Y := Y - height;
  Sprite.Height := height;
  Sprite.Width := width;
  Sprite.TextureID := entries[SpriteIndex].TextureId;

  editor_gl.RenderSprite(Sprite);

  RenderFlag(Sprite,-1,color);
end;

procedure TDef.UnBindTextures;
var
  SpriteIndex: Integer;
  flag: TPlayerColor;
begin
  for SpriteIndex := 0 to entries.Size - 1 do
  begin
    entries.Mutable[SpriteIndex]^.UnBind();
  end;

  if FFlaggable then
  begin
    for flag in TPlayerColor do
    begin
      flag_entries[flag].UnBind();
    end;
  end;
end;

end.

