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
  editor_types, editor_consts, editor_utils, filesystem_base, editor_gl;

const

  NEWTRAL_PLAYER_COLOR:TRBGAColor = (r: 128; g: 128; b:128; a: 255);

  PLAYER_FLAG_COLORS: array[TPlayerColor] of TRBGAColor = (
    //(r: 128; g: 128; b:128; a: 255),//NONE
    (r: 255; g: 0;   b:0;   a: 255),//RED
    (r: 0;   g: 0;   b:255; a: 255),//BLUE
    (r: 120; g: 180; b:140; a: 255),//TAN
    (r: 0;   g: 255; b:0;   a: 255),//GREEN
    (r: 255; g: 165; b:0;   a: 255),//ORANGE
    (r: 128; g: 0;   b:128; a: 255),//PURPLE
    (r: 0;   g: 255; b:255; a: 255),//TEAL
    (r: 255; g: 192; b:203; a: 255) //PINK
  );

type
  TRBGAColor = editor_gl.TRBGAColor;

  TRGBAPalette = packed array[0..255] of TRBGAColor;

  { TBaseSprite }

  TBaseSprite = object
    TextureId: GLuint;
    procedure UnBind; inline;
  end;

  PDefEntry = ^TDefEntry;

  { TDefEntry }

  TDefEntry = object (TBaseSprite)
    TopMagin: int32;
    LeftMargin: int32;
    SpriteWidth: Int32;
    SpriteHeight: Int32;
  end;

  TDefEntries = specialize gvector.TVector<TDefEntry>;


  { TDef }

  TDef = class
  private
    FLoaded: Boolean;
    FPaletteID: GLuint;
    FResourceID: AnsiString;

    //typ: UInt32;
    FWidth: UInt32;
    FHeight: UInt32;

    entries: TDefEntries;

    FTexturesBinded: boolean;

    function GetFrameCount: Integer; inline;

    procedure MayBeUnBindTextures; inline;
    procedure SetLoaded(AValue: Boolean);
    procedure SetResourceID(AValue: AnsiString);
    procedure UnBindTextures;
    class procedure SetPalyerColor(color: TPlayer); static;
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

    procedure RenderBorder(TileX,TileY: Integer);

    property Width: UInt32 read FWidth;
    property Height: UInt32 read FHeight;

    property ResourceID: AnsiString read FResourceID write SetResourceID;

    property Loaded: Boolean read FLoaded write SetLoaded;
  end;

  TDefMapBase = specialize fgl.TFPGMap<string,TDef>;

  { TDefMap }

  TDefMap = class (TDefMapBase)
  protected
    procedure Deref(Item: Pointer); override;
  public
    constructor Create;
  end;

  TGraphicsLoadMode = (LoadFisrt, LoadRest, LoadComplete);

  { TDefFormatLoader }

  TDefFormatLoader = class (IResource)
  strict private
    const
      INITIAL_BUFFER_SIZE = 32768;
  strict private
    FBuffer: packed array of byte; //indexed bitmap
    palette: TRGBAPalette;
    procedure IncreaseBuffer(ANewSize: SizeInt);
  private
    FCurrentDef: TDef;
    FMode: TGraphicsLoadMode;
    procedure LoadSprite(AStream: TStream; const SpriteIndex: UInt8; ATextureID: GLuint; offset: Uint32);

    procedure SetCurrentDef(AValue: TDef);
  public
    procedure LoadFromStream(AStream: TStream);//IResource
  public
    constructor Create;
    destructor Destroy; override;

    property CurrentDef: TDef read FCurrentDef write SetCurrentDef;
    property Mode: TGraphicsLoadMode read FMode write FMode;
  end;

  { TGraphicsManager }

  TGraphicsManager = class (TFSConsumer)
  private
    FNameToDefMap: TDefMap;
    FDefLoader: TDefFormatLoader;

    FHeroFlagDefs: array[TPlayerColor] of TDef;

    FBuffer: TMemoryStream;

    procedure LoadDef(const AResourceName:string; ADef: TDef; ALoadComplete: Boolean);

    function DoGetGraphics (const AResourceName:string; ALoadComplete: Boolean): TDef;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //complete load
    function GetGraphics (const AResourceName:string): TDef;
    //load first frame
    function GetPreloadedGraphics(const AResourceName:string): TDef;
    //load all expect first
    procedure LoadGraphics(Adef: TDef);

    function GetHeroFlagDef(APlayer: TPlayer): TDef;
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
    prSize: Int32;
    defType2: Int32;
    FullWidth: Int32;
    FullHeight: Int32;
    SpriteWidth: Int32;
    SpriteHeight: Int32;
    LeftMargin: Int32;
    TopMargin: Int32;
  end;
const

  H3_SPECAIL_COLORS: array[0..7] of TH3DefColor = (
   {0} (r: 0; g: 255; b:255),
   {1} (r: 255; g: 150; b:255),
   {2} (r: 255; g: 100; b:255),
   {3} (r: 255; g: 50; b:255),
   {4} (r: 255; g: 0; b:255),
   {5} (r: 255; g: 255; b:0),
   {6} (r: 180; g: 0; b:255),
   {7} (r: 0; g: 255; b:0));

const
  STANDARD_COLORS: array[0..7] of TRBGAColor = (
    (r: 0; g: 0; b:0; a: 0), //Transparency
    (r: 0; g: 0; b:0; a: 64),//Shadow border (75% transparent)
    (r: 0; g: 0; b:0; a: 128), //shadow
    (r: 0; g: 0; b:0; a: 128), //shadow
    (r: 0; g: 0; b:0; a: 128), //shadow 50%
    (r: 255; g: 255; b:0; a: 0),  //player color | Selection highlight (creatures)
    (r: 0; g: 0; b:0; a: 128), //Selection + shadow body
    (r: 0; g: 0; b:0; a: 64)); // Selection + shadow border

  PLAYER_COLOR_INDEX = 5;



  //DEF_TYPE_MAP_OBJECT = $43;

function CompareDefs(const d1,d2: TDef): integer;
begin
  Result := PtrInt(d1) - PtrInt(d2);
end;

{ TDefFormatLoader }

constructor TDefFormatLoader.Create;
begin
  SetLength(FBuffer, INITIAL_BUFFER_SIZE);
end;

destructor TDefFormatLoader.Destroy;
begin
  inherited;
end;

procedure TDefFormatLoader.IncreaseBuffer(ANewSize: SizeInt);
begin
  if ANewSize > Length(FBuffer) then
  begin

    ANewSize := (ANewSize div INITIAL_BUFFER_SIZE + 1)*INITIAL_BUFFER_SIZE;

    SetLength(FBuffer, ANewSize);
  end;
end;

procedure TDefFormatLoader.LoadSprite(AStream: TStream; const SpriteIndex: UInt8;
  ATextureID: GLuint; offset: Uint32);
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
  RightMargin, BottomMargin: Int32;
  add: Int32;

  BaseOffset: Int32;
  BaseOffsetor: Int32;

  PEntry: PDefEntry;

  procedure ReadType0;
  var
    row: Int32;
  begin
    for row:=0 to h.SpriteHeight - 1 do
    begin
      SkipIfPositive(h.LeftMargin);

      AStream.Read(FBuffer[ftcp+row * h.SpriteWidth],h.SpriteWidth);

      SkipIfPositive(RightMargin);
    end;
  end;

  procedure ReadType1;
  var
    row: Int32;
    TotalRowLength : Int32;
    RowAdd: Int32;
    SegmentLength: Int32;
    SegmentType: UInt8;
  begin
    for row := 0 to h.SpriteHeight - 1 do
    begin
      AStream.Seek(BaseOffsetor + SizeOf(UInt32)*row, soFromBeginning);
      AStream.Read(BaseOffset,SizeOf(BaseOffset));


      SkipIfPositive(h.LeftMargin);
      TotalRowLength :=  0;
      repeat
         SegmentType := AStream.ReadByte;
         SegmentLength := AStream.ReadByte + 1;

         if SegmentType = $FF then
         begin
           AStream.Seek(BaseOffsetor+Int32(BaseOffset), soFromBeginning);
           AStream.Read(FBuffer[ftcp],SegmentLength);
           BaseOffset += SegmentLength;
         end
         else begin
           FillChar(FBuffer[ftcp],SegmentLength,SegmentType);
         end;

         Skip(SegmentLength);

         TotalRowLength += SegmentLength;
       until TotalRowLength>=(h.SpriteWidth);
       RowAdd := h.SpriteWidth - TotalRowLength;

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
    RowAdd: Int32;
  begin
    BaseOffset := BaseOffsetor + AStream.ReadWord();
    AStream.Seek(BaseOffset,soBeginning);

    for row := 0 to h.SpriteHeight - 1 do
    begin
      SkipIfPositive(h.LeftMargin);

      TotalRowLength:=0;
      repeat
         SegmentType := AStream.ReadByte();
         code := SegmentType div 32;
         value := (SegmentType and 31) + 1;

         if code=7 then
         begin
           AStream.Read(FBuffer[ftcp],value);
         end
         else begin
           FillChar(FBuffer[ftcp],value,code);
         end;
         Skip(Value);
         TotalRowLength+=value;

      until TotalRowLength >= h.SpriteWidth ;

      SkipIfPositive(RightMargin);

      RowAdd := h.SpriteWidth-TotalRowLength;

      Skip(RowAdd);

      if (add>0) then
         ftcp += add;
    end;
  end;

  procedure ReadType3();
  var
    row: Int32;
    tmp: UInt16;
    TotalRowLength : Int32;

    SegmentType, code, value: UInt8;
    len: Integer;
    RowAdd: Int32;
  begin
    for row := 0 to h.SpriteHeight - 1 do
    begin
      AStream.Seek(BaseOffsetor+row*2*(h.SpriteWidth div 32),soBeginning);
      tmp := AStream.ReadWord();
      BaseOffset := BaseOffsetor + tmp;
      SkipIfPositive(h.LeftMargin);
      TotalRowLength := 0;

      AStream.Seek(BaseOffset,soBeginning);

      repeat
         SegmentType := AStream.ReadByte;
         code := SegmentType div 32;
         value := (SegmentType and 31) + 1;

         len := Min(Int32(value), h.SpriteWidth - TotalRowLength) - Max(0, -h.LeftMargin);

         len := Max(0,len);

         if code = 7 then
         begin
           AStream.Read(FBuffer[ftcp],len);
         end
         else begin
           FillChar(FBuffer[ftcp],len,code);
         end;
         Skip(len);

         TotalRowLength += ifthen(h.LeftMargin>=0,value, value+h.LeftMargin );

      until TotalRowLength>=h.SpriteWidth;

      SkipIfPositive(RightMargin);

      RowAdd := h.SpriteWidth-TotalRowLength;

      if (add>0) then
         ftcp += add+RowAdd;

    end;
  end;

begin
  PEntry := FCurrentDef.entries.Mutable[SpriteIndex];

  BaseOffset := offset;

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

  RightMargin := h.FullWidth - h.SpriteWidth - h.LeftMargin;
  BottomMargin := h.FullHeight - h.SpriteHeight - h.TopMargin;

  if h.LeftMargin<0 then
     h.SpriteWidth+=h.LeftMargin;
  if RightMargin<0 then
     h.SpriteWidth+=RightMargin;

  add := 4 - (h.FullWidth mod 4);
  if add = 4 then
     add :=0;

  PEntry^.LeftMargin := h.LeftMargin;
  PEntry^.TopMagin := h.TopMargin;
  PEntry^.TextureId := ATextureID;
  PEntry^.SpriteHeight := h.SpriteHeight;
  PEntry^.SpriteWidth := h.SpriteWidth;

  IncreaseBuffer(h.FullWidth*h.FullHeight);


  if (h.TopMargin > 0) or (BottomMargin > 0) or (h.LeftMargin > 0) or (RightMargin > 0) then
    FillChar(FBuffer[0], Length(FBuffer) ,0); //todo: use marging in texture coords

  ftcp := 0;

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

  BindUncompressedPaletted(ATextureID,h.FullWidth,h.SpriteHeight, @FBuffer[0]);
end;

procedure TDefFormatLoader.LoadFromStream(AStream: TStream);
var
  id_s: array of GLuint;
  total_entries: Integer;

  procedure GenerateTextureIds(count, offcet: Integer);
    begin
      SetLength(id_s,total_entries);
      glGenTextures(count, @id_s[offcet]);
    end;
var
  offsets : packed array of UInt32;

  block_nomber: Integer;

  current_block_head: TH3DefEntryBlockHeader;
  total_in_block: Integer;

  current_offcet: UInt32;

  i: Integer;

  blockCount: UInt32;

  header: TH3DefHeader;
  orig_position: Int32;
begin
  Assert(Assigned(FCurrentDef),'TDefFormatLoader.LoadFromStream: nil CurrentDef');

  if Mode <> TGraphicsLoadMode.LoadRest then
    FCurrentDef.MayBeUnBindTextures;

  orig_position := AStream.Position;

  AStream.Read(header{%H-},SizeOf(header));

  //typ := LEtoN(header.typ);
  FCurrentDef.FHeight := LEtoN(header.height);
  blockCount := LEtoN(header.blockCount);
  FCurrentDef.FWidth := LEtoN(header.width);

  //TODO: use color comparison instead of index

  if mode <> TGraphicsLoadMode.LoadRest then
  begin

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

    glGenTextures(1,@FCurrentDef.FPaletteID);
    BindPalette(FCurrentDef.FPaletteID,@palette);

  end;

    total_entries := 0;

    for block_nomber := 0 to header.blockCount - 1 do
    begin
       AStream.Read(current_block_head{%H-},SizeOf(current_block_head));

       total_in_block := current_block_head.totalInBlock;

       SetLength(offsets, total_entries + total_in_block);

       //entries.Resize(total_entries + total_in_block);

       //names
       AStream.Seek(13*total_in_block,soCurrent);

       //offcets
       for i := 0 to total_in_block - 1 do
       begin
         AStream.Read(current_offcet{%H-},SizeOf(current_offcet));
         offsets[total_entries+i] := current_offcet+UInt32(orig_position);

         //todo: use block_nomber to load heroes defs from mods

         //entries.Mutable[total_entries+i]^.group := block_nomber;
       end;

       total_entries += total_in_block;

    end;

  if mode <> TGraphicsLoadMode.LoadRest then
  begin
    FCurrentDef.entries.Resize(total_entries);
  end;



  case Mode of
    TGraphicsLoadMode.LoadFisrt: begin
      GenerateTextureIds(1,0);
      LoadSprite(AStream, 0, id_s[0], offsets[0]);
    end;
    TGraphicsLoadMode.LoadRest:
    begin
      if total_entries > 1 then
      begin
        GenerateTextureIds(total_entries-1,1);
        for i := 1 to total_entries - 1 do
        begin
          LoadSprite(AStream, i, id_s[i], offsets[i]);
        end;
      end;

      FCurrentDef.Loaded:=True;
    end;
    TGraphicsLoadMode.LoadComplete:
    begin
      GenerateTextureIds(total_entries, 0);
      for i := 0 to total_entries - 1 do
      begin
        LoadSprite(AStream, i, id_s[i], offsets[i]);
      end;
      FCurrentDef.Loaded:=True;
    end;
  end;
end;

procedure TDefFormatLoader.SetCurrentDef(AValue: TDef);
begin
  if FCurrentDef = AValue then Exit;
  FCurrentDef := AValue;
end;

{ TBaseSprite }

procedure TBaseSprite.UnBind;
begin
  editor_gl.Unbind(TextureId);
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

constructor TGraphicsManager.Create(AOwner: TComponent);
const
  FMT = 'AF0%dE';
var
  i: TPlayer;
begin
  inherited Create(AOwner);
  FNameToDefMap := TDefMap.Create;
  FBuffer := TMemoryStream.Create;

  FDefLoader := TDefFormatLoader.Create;

  for i in TPlayerColor do
  begin
    FHeroFlagDefs[i] := GetGraphics(Format(FMT,[Integer(i)]));
  end;
end;

destructor TGraphicsManager.Destroy;
begin
  FDefLoader.Free;
  FBuffer.Free;
  FNameToDefMap.Free;
  inherited Destroy;
end;

function TGraphicsManager.GetGraphics(const AResourceName: string): TDef;
begin
  Result := DoGetGraphics(AResourceName, True);
end;

function TGraphicsManager.GetPreloadedGraphics(const AResourceName: string
  ): TDef;
begin
  Result := DoGetGraphics(AResourceName, False);
end;

procedure TGraphicsManager.LoadGraphics(Adef: TDef);
begin
  FDefLoader.CurrentDef := ADef;
  FDefLoader.Mode := TGraphicsLoadMode.LoadRest;
  ResourceLoader.LoadResource(FDefLoader,TResourceType.Animation,'SPRITES/'+ADef.ResourceID);
end;

function TGraphicsManager.GetHeroFlagDef(APlayer: TPlayer): TDef;
begin
  Result := FHeroFlagDefs[APlayer];
end;

procedure TGraphicsManager.LoadDef(const AResourceName: string; ADef: TDef;
  ALoadComplete: Boolean);
begin
  FDefLoader.CurrentDef := ADef;

  if ALoadComplete then
    FDefLoader.Mode := TGraphicsLoadMode.LoadComplete
  else
    FDefLoader.Mode := TGraphicsLoadMode.LoadFisrt;

  ResourceLoader.LoadResource(FDefLoader,TResourceType.Animation,'SPRITES/'+AResourceName);

end;

function TGraphicsManager.DoGetGraphics(const AResourceName: string;
  ALoadComplete: Boolean): TDef;
var
  res_index: Integer;
begin
  res_index :=  FNameToDefMap.IndexOf(AResourceName);

  if res_index >= 0 then
  begin
    Result := FNameToDefMap.Data[res_index];

    if ALoadComplete and not Result.Loaded then
    begin
      LoadGraphics(Result);
    end;
  end
  else begin
    Result := TDef.Create;
    LoadDef(AResourceName,Result, ALoadComplete);
    FNameToDefMap.Add(AResourceName,Result);
    Result.ResourceID := AResourceName;
  end;
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

{ TDef }

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

procedure TDef.MayBeUnBindTextures;
begin
  if FTexturesBinded then
    UnBindTextures;
  FTexturesBinded := False;
end;

procedure TDef.SetLoaded(AValue: Boolean);
begin
  if FLoaded=AValue then Exit;
  FLoaded:=AValue;
end;

procedure TDef.SetResourceID(AValue: AnsiString);
begin
  if FResourceID=AValue then Exit;
  FResourceID:=AValue;
end;

procedure TDef.Render(const SpriteIndex: UInt8; X, Y: Integer; dim: integer;
  color: TPlayer);
var
  Sprite: TGLSprite;
begin
  Sprite.TextureID := entries[SpriteIndex].TextureId;
  Sprite.PaletteID := FPaletteID;
  Sprite.X := X;
  Sprite.Y := Y;
  Sprite.Height := height;
  Sprite.Width := width;
  Sprite.SpriteHeight:=entries[SpriteIndex].SpriteHeight;
  Sprite.SpriteWidth:=entries[SpriteIndex].SpriteWidth;
  Sprite.LeftMargin:=entries[SpriteIndex].LeftMargin;
  Sprite.TopMagin:=entries[SpriteIndex].TopMagin;

  SetPalyerColor(color);

  editor_gl.CurrentContextState.RenderSprite(Sprite, dim);

end;

procedure TDef.RenderBorder(TileX, TileY: Integer);
var
  cx: Integer;
  cy: Integer;
begin
  cx := (TileX+1) * TILE_SIZE;
  cy := (TileY+1) * TILE_SIZE;

  editor_gl.CurrentContextState.StartDrawingRects;
  editor_gl.CurrentContextState.RenderRect(cx,cy,-width,-height);
  editor_gl.CurrentContextState.StopDrawing;
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
  Sprite.PaletteID := FPaletteID;
  Sprite.SpriteHeight:=entries[SpriteIndex].SpriteHeight;
  Sprite.SpriteWidth:=entries[SpriteIndex].SpriteWidth;
  Sprite.LeftMargin:=entries[SpriteIndex].LeftMargin;
  Sprite.TopMagin:=entries[SpriteIndex].TopMagin;

  mir := flags mod 4;

  CurrentContextState.SetFlagColor(NEWTRAL_PLAYER_COLOR);

  editor_gl.CurrentContextState.RenderSprite(Sprite,-1,mir);


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
  Sprite.PaletteID := FPaletteID;
  Sprite.SpriteHeight:=entries[SpriteIndex].SpriteHeight;
  Sprite.SpriteWidth:=entries[SpriteIndex].SpriteWidth;
  Sprite.LeftMargin:=entries[SpriteIndex].LeftMargin;
  Sprite.TopMagin:=entries[SpriteIndex].TopMagin;

  SetPalyerColor(color);

  editor_gl.CurrentContextState.RenderSprite(Sprite);

end;

class procedure TDef.SetPalyerColor(color: TPlayer);
begin
  if color = TPlayer.NONE then
  begin
    CurrentContextState.SetFlagColor(NEWTRAL_PLAYER_COLOR);
  end
  else begin
    CurrentContextState.SetFlagColor(PLAYER_FLAG_COLORS[color]);
  end;

end;

procedure TDef.UnBindTextures;
var
  SpriteIndex: Integer;
begin
  for SpriteIndex := 0 to entries.Size - 1 do
  begin
    entries.Mutable[SpriteIndex]^.UnBind();
  end;
  glDeleteTextures(1,@FPaletteID);
end;

end.

