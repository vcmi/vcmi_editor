{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit editor_graphics;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, gvector, fgl, Gl, Graphics, IntfGraphics, GraphType, editor_types, editor_consts,
  editor_utils, filesystem_base, editor_gl, vcmi_json, editor_classes, vcmi.image_loaders, LazLoggerBase, LazUTF8,
  fpjson;

type
  TSprites = specialize gvector.TVector<TGLSprite>;
  TGraphicsLoadMode = (LoadFisrt, LoadRest, LoadComplete);
  TGraphicsLoadFlag = (None, First, Complete);

  { TIconList }

  TIconList = class
  strict private
    type
       TImages = specialize TFPGObjectList<TLazIntfImage>;
    var
       FImages: TImages;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TAnimation }

  TAnimation = class
  private
    FLoaded: TGraphicsLoadFlag;

    FPaletteID: GLuint;

    FResourceID: AnsiString;

    //typ: UInt32;
    FWidth: UInt32;
    FHeight: UInt32;

    entries: TSprites;

    function GetFrameCount: Integer; inline;
    procedure SetFrameCount(AValue: Integer);

    procedure UnbindPalette;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RenderBorder(AState: TLocalState; TileX,TileY: Integer);
    procedure RenderIcon(AState: TLocalState; const SpriteIndex: UInt8; dim:integer; color: TPlayer = TPlayer.none);
    //for hero flags
    procedure RenderOverlayIcon(AState: TLocalState; dim:integer; h: integer);

    procedure RenderF(AState: TLocalState; const SpriteIndex: UInt8; flags:UInt8);
    procedure RenderO(AState: TLocalState; const SpriteIndex: UInt8; X,Y: Integer; color: TPlayer = TPlayer.none);

    property FrameCount: Integer read GetFrameCount write SetFrameCount;

    property Width: UInt32 read FWidth;
    property Height: UInt32 read FHeight;

    function GetSpriteHeight(const SpriteIndex: UInt8): UInt32;

    property ResourceID: AnsiString read FResourceID write FResourceID;

    procedure FixLoadMode(var AMode: TGraphicsLoadMode);
    procedure UpdateLoadFlag(AMode: TGraphicsLoadMode);

    procedure UnBindTextures;
    property Loaded: TGraphicsLoadFlag read FLoaded write FLoaded;

    procedure SetPalette(constref APlalette: TRGBAPalette);
  end;

  { TAnimationMap }

  TAnimationMap = class (specialize TObjectMap<string,TAnimation>)
  public
    constructor Create;
  end;

  { TAnimationLoader }

  TAnimationLoader = class abstract(TFSConsumer)
  strict private
    FCurrentPath: AnsiString;
    FMode: TGraphicsLoadMode;
    FCurrent: TAnimation;
  strict protected
    FFrameCount: Integer;
    FTextureIDs: array of GLuint;

    function DoTryLoad: Boolean; virtual; abstract;

    function ConvertCurrentPath(): AnsiString;
    procedure GenerateTextureIds();
  public
    function TryLoad: Boolean;

    property Mode: TGraphicsLoadMode read FMode write FMode;
    property Current: TAnimation read FCurrent write FCurrent;
    property CurrentPath: AnsiString read FCurrentPath write FCurrentPath;
  end;

  { TDefFormatLoader }

  TDefFormatLoader = class (TAnimationLoader, IResource)
  strict private
    const
      INITIAL_BUFFER_SIZE = 32768;
    var
      FBuffer: packed array of byte; //indexed bitmap
      FDefBuffer: packed array of byte;
      palette: TRGBAPalette;
    procedure IncreaseBuffer(ANewSize: SizeInt);
    procedure IncreaseDefBuffer(ANewSize: SizeInt);
    procedure LoadSprite(AStream: TStream; const SpriteIndex: UInt8; ATextureID: GLuint; offset: Uint32);
  strict protected
    function DoTryLoad: Boolean; override;
  public
    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); //IResource
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TAnimationSequence }

  TAnimationSequence = class(TCollectionItem)
  private
    FFrames: TStrings;
    FGroup: Integer;
    procedure SetGroup(AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Group: Integer read FGroup write SetGroup default 0;
    property Frames: TStrings read FFrames;
  end;

  TAnimationSequences = specialize TGArrayCollection<TAnimationSequence>;

  { TAnimationFrame }

  TAnimationFrame = class(TCollectionItem)
  private
    FFile: AnsiString;
    FFrame: Integer;
    FGroup: Integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Group: Integer read FGroup write FGroup default 0;
    property Frame: Integer read FFrame write FFrame default 0;
    property &File: AnsiString read FFile write FFile;
  end;

  TAnimationOverrides = specialize TGArrayCollection<TAnimationFrame>;

  { TJsonAnimationHeader }

  TJsonAnimationHeader = class(TPersistent)
  private
    FBasepath: AnsiString;
    FImages: TAnimationOverrides;
    FSequences: TAnimationSequences;
    procedure SetBasepath(AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function FindSequence(AGroup: Integer):TAnimationSequence;
  published
    property Basepath: AnsiString read FBasepath write SetBasepath;
    property Sequences: TAnimationSequences read FSequences;

    property Images: TAnimationOverrides read FImages;
  end;

  { TJsonFormatLoader }

  TJsonFormatLoader = class(TAnimationLoader)
  strict private
    FHeader:TJsonAnimationHeader;

    procedure LoadFrame(idx: Integer; path: AnsiString);
    function LoadSequence(): Boolean;
    procedure UpdateAnimationSize(ASpriteWidth, ASpriteHeight: integer);

    //if json header is not present try to load single frame from raster image
    function TryLoadSingle: Boolean;
  strict protected
    function DoTryLoad: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TGraphicsManager }

  TGraphicsManager = class (TFSConsumer)
  strict private
    FBlocked: Boolean;
    FNameToAnimMap: TAnimationMap;
    FDefLoader: TDefFormatLoader;
    FJsonLoader: TJsonFormatLoader;

    FHeroFlagDefs: array[TPlayerColor] of TAnimation;

    FBuffer: TMemoryStream;

    procedure DoLoadGraphics(const AResourceName:string; ADef: TAnimation; ALoadMode: TGraphicsLoadMode);
    function DoGetGraphics (const AResourceName:string; ALoadMode: TGraphicsLoadMode): TAnimation;
    procedure SetBlocked(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PreLoad;

    //complete load
    function GetGraphics(const AResourceName:string): TAnimation;
    //load first frame
    function GetPreloadedGraphics(const AResourceName:string): TAnimation;
    //load all expect first
    procedure LoadGraphics(AAnimation: TAnimation);

    function GetHeroFlagDef(APlayer: TPlayer): TAnimation;

    //if true blocks all actual assets operations
    property Blocked: Boolean read FBlocked write SetBlocked;
  end;

  { TGraphicsConsumer }

  TGraphicsConsumer = class abstract (TFSConsumer)
  private
    FGraphicsManager: TGraphicsManager;
    procedure SetGraphicsManager(AValue: TGraphicsManager);
  public
    constructor Create(AOwner: TComponent); override;
    property GraphicsManager:TGraphicsManager read FGraphicsManager write SetGraphicsManager;
  end;

implementation

type

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

  H3_SPECIAL_COLORS: array[0..7] of TH3DefColor = (
   {0} (r: 0;   g: 255; b:255),
   {1} (r: 255; g: 150; b:255),
   {2} (r: 255; g: 100; b:255),
   {3} (r: 255; g: 50;  b:255),
   {4} (r: 255; g: 0;   b:255),
   {5} (r: 255; g: 255; b:0),
   {6} (r: 180; g: 0;   b:255),
   {7} (r: 0;   g: 255; b:0));

const
  STANDARD_COLORS: array[0..7] of TRBGAColor = (
    (r: 0; g: 0; b:0; a: 0), //Transparency
    (r: 0; g: 0; b:0; a: 73),//Shadow border (75% transparent)
    (r: 0; g: 0; b:0; a: 93), //shadow
    (r: 0; g: 0; b:0; a: 112), //shadow
    (r: 0; g: 0; b:0; a: 131), //shadow 50%
    (r: 255; g: 255; b:0; a: 0),  //player color | Selection highlight (creatures)
    (r: 0; g: 0; b:0; a: 128), //Selection + shadow body
    (r: 0; g: 0; b:0; a: 64)); // Selection + shadow border

  PLAYER_COLOR_INDEX = 5;

type
  TSpecialColorUsage = set of byte;

  TDefType = (
    Spell = $40,
    Sprite = $41,
    Creature = $42,
    MapObject = $43,
    Hero = $44,
    Terrain = $45,
    Cursor = $46,
    Gui = $47,
    SpriteFrame = $48,
    CombatHero = $49
  );

  TSpecialColorOptions = array[0..9] of TSpecialColorUsage;

const

  SpecialColorOptions : TSpecialColorOptions =
  (
    [0],
    [0,1,2,3,4,5,6,7],
    [0,1,4,5,6,7],
    [0,1,4],
    [0,1,4],
    [0,1,2,3,4],
    [0],
    [0,1,4],
    [0,1,2,3,4,5,6,7],
    [0,1,4]
  );


function CompareDefs(const d1,d2: TAnimation): integer;
begin
  Result := PtrInt(d1) - PtrInt(d2);
end;

{ TIconList }

constructor TIconList.Create;
begin
  FImages := TImages.Create(true);
end;

destructor TIconList.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

{ TAnimationFrame }

constructor TAnimationFrame.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TAnimationFrame.Destroy;
begin
  inherited Destroy;
end;

{ TAnimationSequence }

procedure TAnimationSequence.SetGroup(AValue: Integer);
begin
  FGroup:=AValue;
end;

constructor TAnimationSequence.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFrames := TStringList.Create;
end;

destructor TAnimationSequence.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;

{ TJsonAnimationHeader }

procedure TJsonAnimationHeader.SetBasepath(AValue: AnsiString);
begin
  FBasepath:=AValue;
end;

constructor TJsonAnimationHeader.Create;
begin
  FSequences := TAnimationSequences.Create;
  FImages := TAnimationOverrides.Create;
end;

destructor TJsonAnimationHeader.Destroy;
begin
  FImages.Free;
  FSequences.Free;
  inherited Destroy;
end;

procedure TJsonAnimationHeader.Clear;
begin
  FBasepath:='';
  FSequences.Clear;
  FImages.Clear;
end;

function TJsonAnimationHeader.FindSequence(AGroup: Integer): TAnimationSequence;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Sequences.Count - 1 do
  begin
    if Sequences.Items[i].Group = AGroup then
    begin
      Result := Sequences.Items[i];
      Exit;
    end;
  end;
end;

{ TAnimationLoader }

function TAnimationLoader.ConvertCurrentPath: AnsiString;
begin
  Result := 'SPRITES/'+ CurrentPath;
end;

procedure TAnimationLoader.GenerateTextureIds;
begin
  SetLength(FTextureIDs, FFrameCount);
  case Mode of
    TGraphicsLoadMode.LoadFisrt:
    begin
      glGenTextures(1, @FTextureIDs[0]);
    end;
    TGraphicsLoadMode.LoadRest:
    begin
      if FFrameCount > 1 then
      begin
        glGenTextures(FFrameCount-1, @FTextureIDs[1]);
      end;
    end;
    TGraphicsLoadMode.LoadComplete:
    begin
      glGenTextures(FFrameCount, @FTextureIDs[0]);
    end;
  end;;
end;


function TAnimationLoader.TryLoad: Boolean;
begin
  Assert(Assigned(Current),'TDefFormatLoader.TryLoad: nil Current');
  Result := DoTryLoad();
end;

{ TJsonFormatLoader }

procedure TJsonFormatLoader.LoadFrame(idx: Integer; path: AnsiString);
var
  PEntry: PGLSprite;
  AImage:TIntfImageResource;
begin
  PEntry:=Current.entries.Mutable[idx];

  AImage := TIntfImageResource.Create(path);
  try
    AImage.Load(ResourceLoader);

    PEntry^.Height:=AImage.Data.Height;
    PEntry^.LeftMargin:=0;
    PEntry^.SpriteHeight:=AImage.Data.Height;
    PEntry^.SpriteWidth:=AImage.Data.Width;
    PEntry^.TopMargin:=0;
    PEntry^.Width:=AImage.Data.Width;
    PEntry^.PaletteID:=0;
    PEntry^.TextureID:=FTextureIDs[idx];
    UpdateAnimationSize(PEntry^.SpriteWidth, PEntry^.SpriteHeight);

    BindUncompressedRGBA(PEntry^.TextureID, PEntry^.SpriteWidth, PEntry^.SpriteHeight, AImage.Data.PixelData^);
  finally
    AImage.Free;
  end;
end;

function TJsonFormatLoader.LoadSequence: Boolean;
var
  sequence: TAnimationSequence;
  framePath, realPath: String;
  idx: Integer;
begin
  sequence := FHeader.FindSequence(0);
  Result := Assigned(sequence);

  if Result then
  begin
    Current.UnBindTextures;
    idx := 0;
    FFrameCount := sequence.Frames.Count;
    Current.FrameCount := FFrameCount;
    GenerateTextureIds();

    for framePath in sequence.Frames do
    begin
      realPath := 'SPRITES/'+UTF8Trim(FHeader.Basepath)+UTF8Trim(framePath);

      Result := ResourceLoader.ExistsResource(TResourceType.Image, realPath);

      if not Result then
      begin
        DebugLn('Frame not found: '+realPath);
        Exit;
      end;

      LoadFrame(idx, realPath);
      inc(idx);
    end;
  end;
end;

procedure TJsonFormatLoader.UpdateAnimationSize(ASpriteWidth, ASpriteHeight: integer);
begin
  //TODO: align to tile size (with margins)
  Current.FWidth := ASpriteWidth;
  Current.FHeight := ASpriteHeight;
end;

function TJsonFormatLoader.TryLoadSingle: Boolean;
var
  path: AnsiString;
begin
  path := ConvertCurrentPath();
  Result := false;
  if ResourceLoader.ExistsResource(TResourceType.Image, path) then
  begin
    Current.UnBindTextures;
    FFrameCount := 1;
    Current.FrameCount := FFrameCount;
    GenerateTextureIds();
    LoadFrame(0, path);
    Result := true;
  end;
end;

constructor TJsonFormatLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeader := TJsonAnimationHeader.Create;
end;

destructor TJsonFormatLoader.Destroy;
begin
  FHeader.Free;
  inherited;
end;

function TJsonFormatLoader.DoTryLoad: Boolean;
var
  FHeaderJson: TJsonResource;
begin
  FHeader.Clear;
  Result := false;

  FHeaderJson := TJsonResource.Create(ConvertCurrentPath());
  try
    Result := FHeaderJson.TryLoad(ResourceLoader);

    if Result then
    begin
      FHeaderJson.DestreamTo(FHeader);
      //TODO: patch single frames

      Result := LoadSequence();
    end
    else
      Result := TryLoadSingle();
  finally
    FHeaderJson.Free;
  end;
end;

{ TDefFormatLoader }

constructor TDefFormatLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FBuffer, INITIAL_BUFFER_SIZE);
end;

destructor TDefFormatLoader.Destroy;
begin
  inherited;
end;

function TDefFormatLoader.DoTryLoad: Boolean;
begin
  Result := ResourceLoader.TryLoadResource(Self, TResourceType.Animation, ConvertCurrentPath());
end;

procedure TDefFormatLoader.IncreaseBuffer(ANewSize: SizeInt);
begin
  if ANewSize > Length(FBuffer) then
  begin
    ANewSize := (ANewSize div INITIAL_BUFFER_SIZE + 1)*INITIAL_BUFFER_SIZE;

    SetLength(FBuffer, ANewSize);
  end;
end;

procedure TDefFormatLoader.IncreaseDefBuffer(ANewSize: SizeInt);
begin
  if ANewSize > Length(FDefBuffer) then
  begin
    ANewSize := (ANewSize div INITIAL_BUFFER_SIZE + 1)*INITIAL_BUFFER_SIZE;

    SetLength(FDefBuffer, ANewSize);
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

  BaseOffset: Int32;
  BaseOffsetor: Int32;

  PEntry: PGLSprite;

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
    SegmentLength: Int32;
    SegmentType: UInt8;
  begin
    for row := 0 to h.SpriteHeight - 1 do
    begin
      AStream.Seek(BaseOffsetor + SizeOf(UInt32)*row, soFromBeginning);
      AStream.Read(BaseOffset,SizeOf(BaseOffset));

      AStream.Seek(BaseOffsetor + BaseOffset, soFromBeginning);

      SkipIfPositive(h.LeftMargin);
      TotalRowLength :=  0;
      repeat
         SegmentType := AStream.ReadByte;
         SegmentLength := AStream.ReadByte + 1;

         if SegmentType = $FF then
         begin
           AStream.Read(FBuffer[ftcp],SegmentLength);
           BaseOffset += SegmentLength;
         end
         else begin
           FillChar(FBuffer[ftcp],SegmentLength,SegmentType);
         end;

         Skip(SegmentLength);

         TotalRowLength += SegmentLength;
       until TotalRowLength>=(h.SpriteWidth);

       SkipIfPositive(RightMargin);

     end;
  end;

  procedure ReadType2();
  var
    row: Integer;
    TotalRowLength: Integer;
    SegmentType, code, value: Byte;
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
    end;
  end;

  procedure ReadType3();
  var
    row: Int32;
    tmp: UInt16;
    TotalRowLength : Int32;

    SegmentType, code, value: UInt8;
    len: Integer;
  begin
    for row := 0 to h.SpriteHeight - 1 do
    begin
      tmp := PWord(@FDefBuffer[row*2*(h.SpriteWidth div 32)])^;
      BaseOffset := LEtoN(tmp);

      SkipIfPositive(h.LeftMargin);
      TotalRowLength := 0;

      repeat
         SegmentType := FDefBuffer[BaseOffset]; inc(BaseOffset);
         code := SegmentType div 32;
         value := (SegmentType and 31) + 1;

         len := Min(Int32(value), h.SpriteWidth - TotalRowLength) - Max(0, -h.LeftMargin);

         len := Max(0,len);

         if code = 7 then
         begin
           Move(FDefBuffer[BaseOffset], FBuffer[ftcp], len); inc(BaseOffset, len);
         end
         else begin
           FillChar(FBuffer[ftcp],len,code);
         end;
         Skip(len);

         TotalRowLength += ifthen(h.LeftMargin>=0,value, value+h.LeftMargin);
      until TotalRowLength>=h.SpriteWidth;

      SkipIfPositive(RightMargin);
    end;
  end;
begin
  PEntry := Current.entries.Mutable[SpriteIndex];

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

  //todo: optimize other compression types
  if h.defType2 = 3 then
  begin
    IncreaseDefBuffer(h.prSize);
    AStream.Read(FDefBuffer[0], h.prSize);
  end;

  RightMargin := h.FullWidth - h.SpriteWidth - h.LeftMargin;
  BottomMargin := h.FullHeight - h.SpriteHeight - h.TopMargin;

  if h.LeftMargin<0 then
     h.SpriteWidth+=h.LeftMargin;
  if RightMargin<0 then
     h.SpriteWidth+=RightMargin;

  Assert(ATextureID <> 0);

  PEntry^.PaletteID := Current.FPaletteID;
  PEntry^.TextureId := ATextureID;

  PEntry^.LeftMargin := h.LeftMargin;
  PEntry^.TopMargin := h.TopMargin;

  PEntry^.SpriteHeight := h.SpriteHeight;
  PEntry^.SpriteWidth := h.SpriteWidth;

  PEntry^.Width:=Current.Width;
  PEntry^.Height:=Current.Height;

  IncreaseBuffer(h.FullWidth*h.FullHeight);

  if (h.TopMargin > 0) or (BottomMargin > 0) or (h.LeftMargin > 0) or (RightMargin > 0) then
    FillChar(FBuffer[0], Length(FBuffer) ,0); //todo: use horz marging in texture coords

  ftcp := 0;

  case h.defType2 of
    0:begin
      ReadType0();
    end;
    1:begin
      ReadType1();
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

  BindUncompressedPaletted(ATextureID, h.FullWidth, h.SpriteHeight, @FBuffer[0]);
end;

procedure TDefFormatLoader.LoadFromStream(AFileName: AnsiString; AStream: TStream);
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

  spec_color_usage: TSpecialColorUsage;
  typ: DWord;
begin
  orig_position := AStream.Position;

  AStream.Read(header{%H-},SizeOf(header));

  typ := LEtoN(header.typ);
  Current.FHeight := LEtoN(header.height);
  blockCount := LEtoN(header.blockCount);
  Current.FWidth := LEtoN(header.width);

  spec_color_usage := [0];

  if (typ >= $40) and (typ <= $49) then
  begin
    spec_color_usage := SpecialColorOptions[typ-$40];
  end
  else
  begin
     DebugLn('Def type %d from %s not supported',[typ, AFileName]);
  end;

  if mode <> TGraphicsLoadMode.LoadRest then
  begin

    for i := 0 to 255 do
    begin
      palette[i].a := 255; //no alpha in h3 def
      palette[i].b := header.palette[i].b;
      palette[i].g := header.palette[i].g;
      palette[i].r := header.palette[i].r;
    end;

    for i in spec_color_usage do
    begin
      palette[i] := STANDARD_COLORS[i];
    end;

    //if header.palette[5] = H3_SPECIAL_COLORS[5] then
    //begin
    //  palette[5] := STANDARD_COLORS[i];
    //end;

    Current.SetPalette(palette);

  end;

  FFrameCount := 0;

  for block_nomber := 0 to blockCount - 1 do
  begin
     AStream.Read(current_block_head{%H-},SizeOf(current_block_head));

     total_in_block := current_block_head.totalInBlock;

     SetLength(offsets, FFrameCount + total_in_block);

     //names
     AStream.Seek(13*total_in_block,soCurrent);

     //offcets
     for i := 0 to total_in_block - 1 do
     begin
       AStream.Read(current_offcet{%H-},SizeOf(current_offcet));
       offsets[FFrameCount+i] := current_offcet+UInt32(orig_position);

       //todo: use block_nomber to load heroes defs from mods

       //entries.Mutable[total_entries+i]^.group := block_nomber;
     end;

     FFrameCount += total_in_block;
  end;

  if mode <> TGraphicsLoadMode.LoadRest then
  begin
    Current.FrameCount := FFrameCount;
  end;

  GenerateTextureIds();

  case Mode of
    TGraphicsLoadMode.LoadFisrt:
    begin
      LoadSprite(AStream, 0, FTextureIDs[0], offsets[0]);
    end;
    TGraphicsLoadMode.LoadRest:
    begin
      if FFrameCount > 1 then
      begin
        for i := 1 to FFrameCount - 1 do
        begin
          LoadSprite(AStream, i, FTextureIDs[i], offsets[i]);
        end;
      end;
    end;
    TGraphicsLoadMode.LoadComplete:
    begin
      for i := 0 to FFrameCount - 1 do
      begin
        LoadSprite(AStream, i, FTextureIDs[i], offsets[i]);
      end;
    end;
  end;

  glBindTexture(GL_TEXTURE_2D, 0);
  glBindTexture(GL_TEXTURE_1D, 0);
end;

{ TGraphicsConsumer }

constructor TGraphicsConsumer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if AOwner is TGraphicsManager then
  begin
    GraphicsManager := (AOwner as TGraphicsManager);
  end;
end;

procedure TGraphicsConsumer.SetGraphicsManager(AValue: TGraphicsManager);
begin
  if FGraphicsManager = AValue then Exit;
  FGraphicsManager := AValue;
end;

{ TGraphicsManager }

constructor TGraphicsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameToAnimMap := TAnimationMap.Create;
  FBuffer := TMemoryStream.Create;

  FDefLoader := TDefFormatLoader.Create(Self);
  FJsonLoader := TJsonFormatLoader.Create(Self);
end;

destructor TGraphicsManager.Destroy;
begin
  FBuffer.Free;
  FNameToAnimMap.Free;
  inherited Destroy;
end;

procedure TGraphicsManager.PreLoad;
const
  FMT = 'AF0%dE';
var
  i: TPlayer;
begin
  for i in TPlayerColor do
  begin
    FHeroFlagDefs[i] := GetGraphics(Format(FMT,[Integer(i)]));
  end;
end;

function TGraphicsManager.GetGraphics(const AResourceName: string): TAnimation;
begin
  Result := DoGetGraphics(AResourceName, TGraphicsLoadMode.LoadComplete);
end;

function TGraphicsManager.GetPreloadedGraphics(const AResourceName: string): TAnimation;
begin
  Result := DoGetGraphics(AResourceName, TGraphicsLoadMode.LoadFisrt);
end;

procedure TGraphicsManager.LoadGraphics(AAnimation: TAnimation);
begin
  if AAnimation.Loaded <> TGraphicsLoadFlag.Complete then
    DoLoadGraphics(AAnimation.ResourceID, AAnimation, TGraphicsLoadMode.LoadRest);
end;

function TGraphicsManager.GetHeroFlagDef(APlayer: TPlayer): TAnimation;
begin
  Result := FHeroFlagDefs[APlayer];
end;

procedure TGraphicsManager.DoLoadGraphics(const AResourceName: string; ADef: TAnimation; ALoadMode: TGraphicsLoadMode);
var
  found: Boolean;
begin
  if Blocked or (ADef.Loaded = TGraphicsLoadFlag.Complete) then
  begin
    exit;
  end;

  ADef.FixLoadMode(ALoadMode);

  FDefLoader.Current := ADef;
  FDefLoader.Mode := ALoadMode;
  FDefLoader.CurrentPath := AResourceName;
  found := FDefLoader.TryLoad();

  FJsonLoader.Current := ADef;
  FJsonLoader.Mode:=ALoadMode;
  FJsonLoader.CurrentPath:=AResourceName;

  if FJsonLoader.TryLoad() then
  begin
    found := true;
  end;

  if not found then
  begin
    DebugLn('Animation not found: '+AResourceName);

    ResourceLoader.LoadResource(FDefLoader, TResourceType.Animation, 'SPRITES/DEFAULT');
  end;

  ADef.UpdateLoadFlag(ALoadMode);
end;

function TGraphicsManager.DoGetGraphics(const AResourceName: string; ALoadMode: TGraphicsLoadMode): TAnimation;
var
  res_index: Integer;
begin
  res_index := FNameToAnimMap.IndexOf(AResourceName);

  if res_index >= 0 then
  begin
    Result := FNameToAnimMap.Data[res_index];

    if ALoadMode = TGraphicsLoadMode.LoadComplete then
    begin
      LoadGraphics(Result);
    end;
  end
  else begin
    Result := TAnimation.Create;
    DoLoadGraphics(AResourceName,Result, ALoadMode);
    FNameToAnimMap.Add(AResourceName,Result);
    Result.ResourceID := AResourceName;
  end;
end;

procedure TGraphicsManager.SetBlocked(AValue: Boolean);
begin
  FBlocked:=AValue;
end;


{ TAnimationMap }

constructor TAnimationMap.Create;
begin
  inherited Create;
  OnKeyCompare := @CompareStr;
  OnDataCompare := @CompareDefs;

  Sorted := True;
end;

{ TAnimation }

constructor TAnimation.Create;
begin
  entries := TSprites.Create;
  FLoaded := TGraphicsLoadFlag.None;
  FWidth := TILE_SIZE;
  FHeight:= TILE_SIZE;
end;

destructor TAnimation.Destroy;
begin
  UnBindTextures;

  entries.Destroy;
  inherited Destroy;
end;

function TAnimation.GetFrameCount: Integer;
begin
  Result := entries.Size;
end;

procedure TAnimation.SetFrameCount(AValue: Integer);
var
  old_size, i: Integer;
  PEntry: PGLSprite;
begin
  old_size := entries.Size;

  if old_size <> AValue then
  begin
    entries.Resize(AValue);

    for i := old_size to AValue - 1 do
    begin
      PEntry := entries.Mutable[i];
      PEntry^.Init;
    end;
  end;
end;

procedure TAnimation.UnbindPalette;
begin
  if FPaletteID <> 0 then
  begin
    glDeleteTextures(1,@FPaletteID);
    FPaletteID := 0;
  end;
end;

procedure TAnimation.RenderBorder(AState: TLocalState; TileX, TileY: Integer);
const
  RECT_COLOR: TRBGAColor = (r:50; g:50; b:50; a:255);
var
  cx: Integer;
  cy: Integer;
begin
  glLineWidth(2);
  cx := (TileX+1) * TILE_SIZE+2;
  cy := (TileY+1) * TILE_SIZE+2;

  AState.SetFragmentColor(RECT_COLOR);
  AState.StartDrawingRects;
  AState.RenderRect(cx,cy,-width-4,-height-4);
  AState.StopDrawing;
end;

procedure TAnimation.RenderIcon(AState: TLocalState; const SpriteIndex: UInt8; dim: integer; color: TPlayer);
begin
  if SpriteIndex < entries.Size then
  begin
    AState.SetPlayerColor(color);
    AState.RenderSpriteIcon(entries.Mutable[SpriteIndex], dim);
  end;
end;

procedure TAnimation.RenderOverlayIcon(AState: TLocalState; dim: integer; h: integer);
begin
  if entries.Size = 0 then
  begin
    Exit;
  end;
  AState.RenderSpriteOverlayIcon(entries.Mutable[0], dim, h);
end;

procedure TAnimation.RenderF(AState: TLocalState; const SpriteIndex: UInt8; flags: UInt8);
var
  mir: UInt8;
begin
  if SpriteIndex > entries.Size then
  begin
    Exit;
  end;
  mir := flags and $3;
  AState.RenderSpriteMirrored(entries.Mutable[SpriteIndex], mir);
end;

procedure TAnimation.RenderO(AState: TLocalState; const SpriteIndex: UInt8; X, Y: Integer; color: TPlayer);
begin
  if SpriteIndex < entries.Size then
  begin
    AState.SetPlayerColor(color);
    AState.SetTranslation(X - width, Y - height);
    AState.RenderSpriteSimple(entries.Mutable[SpriteIndex]);
  end;
end;

function TAnimation.GetSpriteHeight(const SpriteIndex: UInt8): UInt32;
begin
  if SpriteIndex < entries.Size then
    Result := entries.Mutable[SpriteIndex]^.SpriteHeight
  else
    Result := Height;//???
end;

procedure TAnimation.FixLoadMode(var AMode: TGraphicsLoadMode);
begin
  if (AMode = TGraphicsLoadMode.LoadComplete) and (Self.Loaded = TGraphicsLoadFlag.First) then
  begin
    AMode := TGraphicsLoadMode.LoadRest;
  end;

  if (AMode = TGraphicsLoadMode.LoadRest) and (Self.Loaded = TGraphicsLoadFlag.None) then
  begin
    AMode := TGraphicsLoadMode.LoadComplete;
  end;
end;

procedure TAnimation.UpdateLoadFlag(AMode: TGraphicsLoadMode);
begin
  case AMode of
    TGraphicsLoadMode.LoadFisrt:
    begin
      Loaded:= TGraphicsLoadFlag.First;
    end;
    TGraphicsLoadMode.LoadRest:
    begin
      Loaded:= TGraphicsLoadFlag.Complete;
    end;
    TGraphicsLoadMode.LoadComplete:
    begin
      Loaded:= TGraphicsLoadFlag.Complete;
    end;
  end;
end;

procedure TAnimation.UnBindTextures;
var
  SpriteIndex: SizeInt;
  PEntry: PGLSprite;
begin
  for SpriteIndex := 0 to SizeInt(entries.Size) - 1 do
  begin
    PEntry := entries.Mutable[SpriteIndex];
    glDeleteTextures(1,@(PEntry^.TextureID));
    PEntry^.TextureID:=0;
  end;
  if FPaletteID <> 0 then
  begin
    glDeleteTextures(1,@FPaletteID);
    FPaletteID := 0;
  end;
end;

procedure TAnimation.SetPalette(constref APlalette: TRGBAPalette);
begin
  UnbindPalette;
  glGenTextures(1, @FPaletteID);
  BindPalette(FPaletteID,@APlalette);
end;

end.

