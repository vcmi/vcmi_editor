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
unit map_actions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Math, fgl, Map, gvector, gset, undo_map, undo_base, editor_types,
  terrain, objects;

type
  TTerrainBrushMode = (none, fixed, area, fill);

  { TMapCoord }

  TMapCoord = record
    X,Y: integer;

    procedure Clear(); inline;
  end;

  operator+ (a,b:TMapCoord):TMapCoord;

type

  { TCompareCoord }

  TCompareCoord = class
  public
    class function c(a,b: TMapCoord): boolean;
  end;


  TCoordSet = specialize TSet<TMapCoord,TCompareCoord>;
type
  { TMapRect }

  TMapCoordForEach = procedure (const Coord: TMapCoord; var Stop: Boolean) is nested;

  TMapRect = object
    FTopLeft: TMapCoord;
    FWidth,FHeight: SizeInt;

    constructor Create(); //empty
    constructor SetFromCenter(X,Y, Width,Height: integer);

    function Left(): integer; inline;
    function Right(): integer; inline;
    function Top(): integer; inline;
    function Bottom(): integer; inline;

    function TopLeft():TMapCoord; inline;
    function TopRight():TMapCoord; inline;
    function BottomLeft():TMapCoord; inline;
    function BottomRight():TMapCoord; inline;

    function Intersect(Other: TMapRect):TMapRect;

    procedure Clear();

    procedure Iterate(Callback: TMapCoordForEach);
  end;

  TTileInfo = record
    X,Y: integer;
    TerType: TTerrainType;
    TerSubtype: UInt8;
    mir: UInt8;
  end;

  TTileInfos = specialize TVector<TTileInfo>;

  { TTileCompare }

  TTileCompareByCoord = class
  public
    class function c(a,b: TTileInfo): boolean;
  end;

  TTileSet = specialize TSet<TTileInfo,TTileCompareByCoord> ;

  { TMapBrush }

  TMapBrush = class (TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); virtual;

    procedure TileClicked(X,Y: integer);virtual;

    procedure RenderCursor(X,Y: integer); virtual;
  end;

  { TIdleMapBrush }

  TIdleMapBrush = class(TMapBrush)

  end;

  { TTerrainBrush }

  TTerrainBrush = class(TMapBrush)
  private
    FMode: TTerrainBrushMode;
    FSize: Integer;
    FTT: TTerrainType;
    FSelection: TCoordSet;
    procedure SetMode(AValue: TTerrainBrushMode);
    procedure SetSize(AValue: Integer);
    procedure Settt(AValue: TTerrainType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;

    property Mode: TTerrainBrushMode read FMode write SetMode;
    property Size: Integer read FSize write SetSize;
    property TT: TTerrainType read FTT write Settt;

    //property Level: Integer read FLevel write SetLevel;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;

    procedure TileClicked(X,Y: integer);override;

    procedure AddTile(X,Y: integer);

    procedure RenderCursor(X,Y: integer); override;
  end;




  { TValidationResult }

  TValidationResult = record
    result: Boolean;
    transitionReplacement: string;
    flip: Integer;
  end;

  { TInvalidTiles }

  TInvalidTiles = class
  public
    ForeignTiles, NativeTiles: TTileSet;
    constructor Create();
    destructor Destroy; override;

  end;

  { TEditTerrain }

  TEditTerrain = class(TMapUndoItem)
  strict private

    FInQueue: TTileSet;
    FOutQueue : TTileSet;

    FInvalidated: TTileSet;

    function GetTinfo(x, y: integer): TTileInfo;
    procedure UndoTile(constref Tile: TTileInfo);
    //no checking
    function GetTileInfo(x,y: Integer): TTileInfo;
    //safe, with checking
    function GetTileInfo(x,y: Integer; out Info: TTileInfo): boolean;

    function ValidateTerrainView(info: TTileInfo; pattern: TPattern; recDepth: integer = 0): TValidationResult;
    function ValidateTerrainViewInner(info: TTileInfo; pattern: TPattern; recDepth: integer = 0): TValidationResult;
  strict private
    FLevel: Integer;

    FOldTileInfos: TTileInfos;
    FNewTileInfos: TTileInfos;

    FTerrainType: TTerrainType;
    procedure SetLevel(AValue: Integer);
    procedure SetTerrainType(AValue: TTerrainType);

    function MapRect:TMapRect;
    function ExtendTileAround(X,Y: integer):TMapRect;
    function SafeExtendTileAround(X,Y: integer):TMapRect;

    procedure InvalidateTerrainViews(X,Y: integer);

    function GetInvalidTiles(center: TTileInfo): TInvalidTiles;

    procedure UpdateTerrainTypes();
    procedure	UpdateTerrainViews();
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Redo; override;
    procedure Undo; override;
    procedure Execute; override;
    function GetDescription: string; override;

    property Level: Integer read FLevel write SetLevel;

    procedure AddTile(X,Y: integer);

    property TerrainType: TTerrainType read FTerrainType write SetTerrainType;
  end;

  { TObjectAction }

  TObjectAction = class(TMapUndoItem)
  private
    FTargetObject: TMapObject;
    procedure SetTargetObject(AValue: TMapObject);
  protected

  public
    property  TargetObject: TMapObject read FTargetObject write SetTargetObject;
  end;

  { TAddObject }

  TAddObject = class(TObjectAction)
  private
    FCurrentPlayer: TPlayer;
    FL: Integer;
    FTemplate: TObjTemplate;
    FX: Integer;
    FY: Integer;

    procedure SetCurrentPlayer(AValue: TPlayer);
    procedure SetL(AValue: Integer);
    procedure SetTemplate(AValue: TObjTemplate);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    destructor Destroy; override;
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property  Template: TObjTemplate read FTemplate write SetTemplate;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
    property CurrentPlayer: TPlayer read FCurrentPlayer write SetCurrentPlayer;

  end;


  { TDeleteObject }
  //todo: free target object on freeing in undo queue (but not in redo queue)
  //todo: cleaup unused map templates
  TDeleteObject = class(TObjectAction)
  public
    destructor Destroy; override;
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TMoveObject }

  TMoveObject = class(TObjectAction)
  private
    FOldX, FOldY,FOldL: Integer;
    FL: Integer;
    FX: Integer;
    FY: Integer;
    procedure SetL(AValue: Integer);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
  end;

implementation

uses
  LazLogger, editor_gl, editor_consts;

operator+(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x+b.X;
  result.Y:=a.y+b.y;
end;

{ TCompareCoord }

class function TCompareCoord.c(a, b: TMapCoord): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TMapBrush }

constructor TMapBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMapBrush.Destroy;
begin
  inherited Destroy;
end;

procedure TMapBrush.Clear;
begin

end;

procedure TMapBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin

end;

procedure TMapBrush.TileClicked(X, Y: integer);
begin

end;

procedure TMapBrush.RenderCursor(X, Y: integer);
begin

end;

{ TTerrainBrush }

procedure TTerrainBrush.SetMode(AValue: TTerrainBrushMode);
begin
  if FMode=AValue then Exit;
  FMode:=AValue;
  Clear;
end;

procedure TTerrainBrush.SetSize(AValue: Integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
  Clear;
end;

procedure TTerrainBrush.Settt(AValue: TTerrainType);
begin
  if FTT=AValue then Exit;
  FTT:=AValue;
  Clear;
end;

constructor TTerrainBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelection := TCoordSet.Create;
end;

destructor TTerrainBrush.Destroy;
begin
  FSelection.Free;
  inherited Destroy;
end;

procedure TTerrainBrush.Clear;
begin
  inherited Clear;
  FSelection.Free;
  FSelection := TCoordSet.Create;
end;

procedure TTerrainBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
var
  action: TEditTerrain;
  it: TCoordSet.TIterator;
begin
  inherited Execute(AManager, AMap);

  action := TEditTerrain.Create(AMap);
  action.Level := AMap.CurrentLevelIndex;
  action.TerrainType := tt;

  it := FSelection.Min;

  if Assigned(it) then
  begin
    repeat
      action.AddTile(it.Data.X, it.Data.Y);
    until not it.next ;
    FreeAndNil(it);
  end;

  AManager.ExecuteItem(action);
  Clear;
end;

procedure TTerrainBrush.TileClicked(X, Y: integer);
begin
  inherited TileClicked(X, Y);
  AddTile(X,Y);
end;

procedure TTerrainBrush.AddTile(X, Y: integer);
  procedure ProcessTile(const Coord: TMapCoord; var Stop: Boolean);
  begin
    FSelection.Insert(Coord);
  end;

var
  r: TMapRect;

begin
  case Mode of
  TTerrainBrushMode.fixed:begin
    r.FTopLeft.X := X;
    r.FTopLeft.Y := Y;
    r.FHeight := Size;
    r.FWidth := Size;

    r.Iterate(@ProcessTile);
  end;
  TTerrainBrushMode.area:; //todo: handle area mode, fill mode
  TTerrainBrushMode.fill:;

  end;
end;

procedure TTerrainBrush.RenderCursor(X, Y: integer);
var
  dim: Integer;
  cx,cy: Integer;
begin
  inherited RenderCursor(X, Y);
  cx := X * TILE_SIZE;
  cy := Y * TILE_SIZE;

  if Mode = TTerrainBrushMode.fixed then
  begin
    editor_gl.CurrentContextState.StartDrawingRects;
    dim := TILE_SIZE * Size;
    editor_gl.CurrentContextState.RenderRect(cx,cy,dim,dim);
    editor_gl.CurrentContextState.StopDrawing;
  end;
end;

{ TMoveObject }

procedure TMoveObject.SetL(AValue: Integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TMoveObject.SetX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TMoveObject.SetY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

procedure TMoveObject.Execute;
begin
  FOldL:=TargetObject.L;
  FOldX:=TargetObject.X;
  FOldY:=TargetObject.Y;
  Redo;
end;

function TMoveObject.GetDescription: string;
begin
  Result := 'Move object';
end;

procedure TMoveObject.Redo;
begin
  TargetObject.L:=L;
  TargetObject.X:=X;
  TargetObject.Y:=Y;
end;

procedure TMoveObject.Undo;
begin
  TargetObject.L:=FOldL;
  TargetObject.X:=FOldX;
  TargetObject.Y:=FOldY;
end;

{ TAddObject }

procedure TAddObject.SetTemplate(AValue: TObjTemplate);
begin
  if FTemplate=AValue then Exit;
  FTemplate:=AValue;
end;

procedure TAddObject.SetL(AValue: Integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TAddObject.SetCurrentPlayer(AValue: TPlayer);
begin
  if FCurrentPlayer=AValue then Exit;
  FCurrentPlayer:=AValue;
end;

procedure TAddObject.SetX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TAddObject.SetY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

destructor TAddObject.Destroy;
begin
  if State = TUndoItemState.UnDone then
  begin
    FreeAndNil(FTargetObject);
  end;
  inherited Destroy;
end;

procedure TAddObject.Execute;
var
  ot: TMapObjectTemplate;
begin
  ot :=  FMap.Templates.Add;

  ot.FillFrom(Template);

  TargetObject := FMap.Objects.Add;

  TargetObject.TemplateID := ot.TID;

  Assert(Assigned(TargetObject.Template), 'Template not assigned by ID');

  TargetObject.L := l;
  TargetObject.X := X;
  TargetObject.Y := Y;

  if TargetObject.Options.MayBeOwned then
  begin
    TargetObject.Options.Owner := CurrentPlayer;
  end;
  //(!)do not redo here
end;

function TAddObject.GetDescription: string;
begin
  Result := 'Add object';
end;

procedure TAddObject.Redo;
begin
  TargetObject.Collection := FMap.Objects;
end;

procedure TAddObject.Undo;
begin
  TargetObject.Collection := nil;
end;

{ TObjectAction }

procedure TObjectAction.SetTargetObject(AValue: TMapObject);
begin
  if FTargetObject=AValue then Exit;
  FTargetObject:=AValue;
end;

{ TDeleteObject }

destructor TDeleteObject.Destroy;
begin
  if State = TUndoItemState.ReDone then
  begin
    FreeAndNil(FTargetObject);
  end;
  inherited Destroy;
end;

procedure TDeleteObject.Execute;
begin
  Redo;
end;

function TDeleteObject.GetDescription: string;
begin
  Result := 'Delete object';
end;

procedure TDeleteObject.Redo;
begin
  FTargetObject.Collection := nil;
end;

procedure TDeleteObject.Undo;
begin
  FTargetObject.Collection := FMap.Objects;
end;

{ TInvalidTiles }

constructor TInvalidTiles.Create;
begin
  ForeignTiles := TTileSet.Create;
  NativeTiles := TTileSet.Create;
end;

destructor TInvalidTiles.Destroy;
begin
  ForeignTiles.Free;
  NativeTiles.Free;
  inherited Destroy;
end;

{ TMapCoord }

procedure TMapCoord.Clear;
begin
  X:=0;
  Y:=0;
end;

{ TMapRect }

constructor TMapRect.Create;
begin
  Clear();
end;

function TMapRect.Left: integer;
begin
  Result := FTopLeft.x;
end;

function TMapRect.Right: integer;
begin
  Result := FTopLeft.x + FWidth;
end;

function TMapRect.Top: integer;
begin
  Result := FTopLeft.y;
end;

function TMapRect.Bottom: integer;
begin
  Result := FTopLeft.y + FHeight;
end;

function TMapRect.TopLeft: TMapCoord;
begin
  Result := FTopLeft;
end;

function TMapRect.TopRight: TMapCoord;
begin
  Result.X := Top();
  Result.Y := Right();
end;

function TMapRect.BottomLeft: TMapCoord;
begin
  Result.X := Bottom();
  Result.Y := Left();
end;

function TMapRect.BottomRight: TMapCoord;
begin
  Result.X := Bottom();
  Result.Y := Right();
end;

function TMapRect.Intersect(Other: TMapRect): TMapRect;
var
  intersects: Boolean;
begin
  intersects := (Right() > Other.Left())
    and (Other.Right() > Left())
    and (Bottom()>Other.Top())
    and (Other.Bottom()>Top());

  Result.Create();

  if intersects then
  begin
    Result.FTopLeft.X:= max(Left(),Other.Left());
    Result.FTopLeft.Y:= max(Top(),Other.Top());

    Result.FWidth:= Min(Right(),Other.Right()) - Result.FTopLeft.X;
    Result.FHeight:= Min(Bottom(),Other.Bottom()) - Result.FTopLeft.Y;
  end;
end;

procedure TMapRect.Clear;
begin
  FTopLeft.Clear();
  FHeight:=0;
  FWidth:=0;
end;

procedure TMapRect.Iterate(Callback: TMapCoordForEach);
var
  Current: TMapCoord;
  Stop: Boolean;
  i,j: SizeInt;
begin
  Stop := false;

  for i := 0 to FWidth - 1 do
  begin
    for j := 0 to FHeight - 1 do
    begin
      Current.X := FTopLeft.X+i;
      Current.Y := FTopLeft.Y+j;

      Callback(Current, Stop);
      if Stop then Exit;
    end;
  end;
end;

constructor TMapRect.SetFromCenter(X, Y, Width, Height: integer);
begin
  Assert(width mod 2 = 1);
  Assert(Height mod 2 = 1);
  Clear();

  FTopLeft.X:= X - (Width-1) div 2;
  FTopLeft.Y:= Y - (Height-1) div 2;
  FWidth:=Width;
  FHeight:=Height;
end;

{ TTileCompare }

class function TTileCompareByCoord.c(a, b: TTileInfo): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TEditTerrain }

procedure TEditTerrain.AddTile(X, Y: integer);
var
  info: TTileInfo;
begin

  if not FMap.IsOnMap(FLevel,X,Y) then
  begin
    exit; //silently ignore
  end;

  info.TerType := TerrainType;
  info.TerSubtype := 0; //will be randomized by pattern selection algorithm
  info.X := X;
  info.Y := Y;
  info.mir := 0;

  FNewTileInfos.PushBack(info);

  //TODO: if brush mode = fill, ignore transitions
end;

constructor TEditTerrain.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FOldTileInfos := TTileInfos.Create;
  FNewTileInfos := TTileInfos.Create;
end;

destructor TEditTerrain.Destroy;
begin
  FOldTileInfos.Free;
  FNewTileInfos.Free;

  inherited Destroy;
end;

procedure TEditTerrain.Execute;
var
  old_info, new_info, info: TTileInfo;

  i, dx,dy: Integer;

  loop_guard: Integer;
  x: LongInt;
  y: LongInt;
  info_n: TTileSet.PNode;
  it: TTileSet.TIterator;
begin
  FInQueue := TTileSet.Create;
  FOutQueue := TTileSet.Create;
  FInvalidated := TTileSet.Create;

  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    InvalidateTerrainViews(FNewTileInfos[i].X,FNewTileInfos[i].Y);
  end;

  for i := 0 to FNewTileInfos.Size - 1 do
  begin
     FInQueue.Insert(FNewTileInfos[i]);
  end;

  UpdateTerrainTypes();
	UpdateTerrainViews();

  //process out queue

  FNewTileInfos.Clear;

  Assert(FInQueue.IsEmpty);

  it := FOutQueue.Min;

  if Assigned(it) then
  begin
    repeat
      FNewTileInfos.PushBack(it.data);
    until not it.next ;
    FreeAndNil(it);
  end;

  //save old state
  FOldTileInfos.Clear;
  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    old_info := GetTileInfo(new_info.x,new_info.y);

    FOldTileInfos.PushBack(old_info);
  end;
  //apply new state
  Redo;

  FOutQueue.Free;
  FInQueue.Free;
  FInvalidated.Free;
end;

function TEditTerrain.GetDescription: string;
begin
  Result := 'Edit terrain'; //todo: l18n
end;

function TEditTerrain.GetTinfo(x,y: integer): TTileInfo;
var
  tmp: TTileInfo;
  n: TTileSet.PNode;
begin
  tmp.X := x;
  tmp.Y := Y;

  n := FOutQueue.NFind(tmp);

  if Assigned(n) then
  begin
    getTinfo := n^.Data;
    exit;
  end;

  n := FInQueue.NFind(tmp);

  if Assigned(n) then
  begin
    getTinfo := n^.Data;
    exit;
  end;

  getTinfo := GetTileInfo(x,y);
end;

function TEditTerrain.GetTileInfo(x, y: Integer): TTileInfo;
var
  t: PMapTile;
begin

  t := FMap.GetTile(FLevel,X,Y);

  Result.X := X;
  Result.Y := Y;
  Result.TerType := t^.TerType;
  Result.TerSubtype := t^.TerSubType;
  Result.mir := t^.Flags mod 4;
end;

function TEditTerrain.GetTileInfo(x, y: Integer; out Info: TTileInfo): boolean;
begin
  Result := False;
  if FMap.IsOnMap(Level,x,y) then
  begin
    info := GetTileInfo(x,y);
    Result := true;
  end;
end;

procedure TEditTerrain.Redo;
var
  new_info: TTileInfo;
  i: Integer;
begin
  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    FMap.SetTerrain(FLevel,
      new_info.X,
      new_info.Y,
      new_info.TerType,
      new_info.TerSubtype,
      new_info.mir);
  end;

end;

procedure TEditTerrain.SetLevel(AValue: Integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

procedure TEditTerrain.SetTerrainType(AValue: TTerrainType);
begin
  if FTerrainType = AValue then Exit;
  FTerrainType := AValue;
end;

function TEditTerrain.MapRect: TMapRect;
begin
  Result.Create();
  Result.FWidth:=FMap.CurrentLevel.Width;
  Result.FHeight:=FMap.CurrentLevel.Height;
end;

function TEditTerrain.ExtendTileAround(X, Y: integer): TMapRect;
begin
  Result.SetFromCenter(x,y,3,3);
end;

function TEditTerrain.SafeExtendTileAround(X, Y: integer): TMapRect;
begin
  Result := ExtendTileAround(x,y).Intersect(MapRect());
end;

procedure TEditTerrain.InvalidateTerrainViews(X, Y: integer);

  procedure ProcessTile(const Coord: TMapCoord; var Stop: Boolean);
  begin
    FInvalidated.Insert(GetTinfo(Coord.X, Coord.Y));
  end;
begin
  SafeExtendTileAround(x,y).Iterate(@ProcessTile);
end;

function TEditTerrain.GetInvalidTiles(center: TTileInfo): TInvalidTiles;
var
  centerTile: TTileInfo;

  config: TTerrainPatternConfig;

  res:TInvalidTiles;

  procedure ProcessTile(const Coord: TMapCoord; var Stop: Boolean);
  const
    WATER_ROCK_P: array[0..1] of string = ('s1','s2');
    OTHER_P: array[0..1] of string = ('n2','n3');
  var
    valid: Boolean;
    curTile: TTileInfo;
    pName: string;
  begin
    curTile := GetTinfo(Coord.X, Coord.Y);

    valid := ValidateTerrainView(curTile,config.GetTerrainTypePatternById('n1')).result;

    // Special validity check for rock & water

    if valid and (centerTile.TerType<>curTile.TerType) and (curTile.TerType in [TTerrainType.water, TTerrainType.rock]) then
    begin
      for pName in WATER_ROCK_P do
      begin
         valid := not ValidateTerrainView(curTile,config.GetTerrainTypePatternById(pName)).result;
         if not valid then
           break;
      end;
    end
    // Additional validity check for non rock OR water
    else if (not valid and not (curTile.TerType in [TTerrainType.water, TTerrainType.rock])) then
    begin
      for pName in OTHER_P do
      begin
         valid := ValidateTerrainView(curTile,config.GetTerrainTypePatternById(pName)).result;
         if valid then
           break;
      end;
    end;

    if not valid then
    begin
      if curTile.TerType = centerTile.TerType then
        Res.NativeTiles.Insert(curTile)
      else
        Res.ForeignTiles.Insert(curTile)
    end;
  end;

begin
  Res := TInvalidTiles.Create;
  config :=  fmap.TerrainManager.PatternConfig;

  centerTile := GetTinfo(center.X,center.Y);

  SafeExtendTileAround(center.X,center.Y).Iterate(@ProcessTile);

  Exit(res)
end;

procedure TEditTerrain.UpdateTerrainTypes;
var
  tiles: TInvalidTiles;
  centerTile:TTileInfo;

  procedure UpdateTerrain(tile:TTileInfo; RequiresValidation: Boolean);
  begin
    tile.TerType:=centerTile.TerType;
    if RequiresValidation then
      FInQueue.Insert(tile)
    else
      FOutQueue.Insert(tile);
    InvalidateTerrainViews(tile.X,tile.Y);

  end;
const
  SHIFTS: array[1..8] of TMapCoord = (
    (x:0; y:-1),(x:-1; y:0),(x:0; y:1),(x:1; y:0),
    (x:-1; y:-1),(x:-1; y:1),(x:1; y:1),(x:1; y:-1));

var
  i: Integer;
  tile, tmp:TTileInfo;

  it: TTileSet.TIterator;
  it2: TTileSet.PNode;
  rect: TMapRect;
  SuitableTiles: TTileSet;

  TestTile: TTileInfo;

  invalidForeignTilesCnt,invalidNativeTilesCnt,nativeTilesCntNorm: Integer;
  j: Integer;

  invalid: TInvalidTiles;
  tileRequiresValidation: Boolean;

  currentCoord: TMapCoord;
  delta: TMapCoord;
begin
   while not FInQueue.IsEmpty do
   begin
     centerTile := FInQueue.NMin^.Data;

     tiles := GetInvalidTiles(centerTile);

     it := tiles.ForeignTiles.Min;

     if Assigned(it)  then
     begin
       repeat
          UpdateTerrain(it.data, true);
       until not it.Next;
     end;

     FreeAndNil(it);

     it:=tiles.NativeTiles.Find(centerTile);

     if Assigned(it) then
     begin
       rect := SafeExtendTileAround(centerTile.x, centerTile.y);
       SuitableTiles := TTileSet.Create;

       invalidForeignTilesCnt:=MaxInt;
       invalidNativeTilesCnt:=0;

        for i := 0 to rect.FWidth - 1 do
        begin
          for j := 0 to rect.FHeight - 1 do
          begin
            TestTile := GetTinfo(rect.FTopLeft.X+i, rect.FTopLeft.Y+j);

            if TestTile.TerType <> centerTile.TerType then
            begin
              TestTile.TerType:=centerTile.TerType;
              FOutQueue.Insert(TestTile);

              invalid := GetInvalidTiles(TestTile);

              nativeTilesCntNorm:=ifthen(invalid.NativeTiles.IsEmpty, MaxInt, invalid.NativeTiles.Size);

              if (nativeTilesCntNorm > invalidNativeTilesCnt)
                or ((nativeTilesCntNorm = invalidNativeTilesCnt) and (invalid.ForeignTiles.Size < invalidForeignTilesCnt) ) then
              begin
                invalidNativeTilesCnt := nativeTilesCntNorm;
                invalidForeignTilesCnt:=invalid.ForeignTiles.Size;
                SuitableTiles.Free;
                SuitableTiles := TTileSet.Create;

                SuitableTiles.Insert(TestTile);

              end
              else if (nativeTilesCntNorm = invalidNativeTilesCnt) and (invalid.ForeignTiles.Size = invalidForeignTilesCnt) then
              begin
                SuitableTiles.Insert(TestTile);
              end;

              invalid.Free;
              FOutQueue.Delete(TestTile);
            end;
          end;
        end;

        tileRequiresValidation := invalidForeignTilesCnt > 0;

        if SuitableTiles.Size = 1 then
        begin
          UpdateTerrain(SuitableTiles.NMin^.Data, tileRequiresValidation);
        end
        else
        begin
          //first suitable tile around center

          //TODO: why is it needed?
          for delta in SHIFTS do
          begin
             currentCoord.x := centerTile.X;
             currentCoord.Y := centerTile.Y;

             currentCoord += delta;

             tmp.X:=currentCoord.X;
             tmp.Y:=currentCoord.Y;

             it2 := SuitableTiles.NFind(tmp);

             if Assigned(it2) then
             begin
                UpdateTerrain(it2^.Data, tileRequiresValidation);
                Break;
             end;
          end;
        end;
        SuitableTiles.Free;
     end
     else begin
        FInQueue.Delete(centerTile);
        FOutQueue.Insert(centerTile);
     end;
     FreeAndNil(it);

     tiles.Free;
   end;

end;

procedure TEditTerrain.UpdateTerrainViews;
var
  it: TTileSet.TIterator;
  groupPatterns: TPatternsVector;
  info: TTileInfo;
  BestPattern: Integer;
  vr: TValidationResult;
  pattern: TPattern;
  Mapping: TMapping;
  k: Integer;
  FramesPerRotation: Integer;
  Flip: Integer;
  oqIt:  TTileSet.PNode;
  FirstFrame, x, y: Integer;
begin
  it  := FInvalidated.Min;

  if Assigned(it) then
  begin
    repeat
      x := it.data.x;
      y := it.data.y;
      info := GetTinfo(x,y);
      groupPatterns := fmap.TerrainManager.PatternConfig.GetTerrainViewPatternsForGroup(TERRAIN_GROUPS[info.TerType]);

      BestPattern := -1;
      vr.result:=false;
      vr.flip:=0;

      for k := 0 to groupPatterns.Count - 1 do
      begin
        pattern := groupPatterns[k];

        vr := ValidateTerrainView(info, pattern);
        if vr.result then
        begin
          BestPattern := k;
          Break;
        end;
      end;

      if BestPattern = -1 then
      begin
        //raise Exception.Create('No pattern found');
        Continue;
      end;

      pattern := groupPatterns[BestPattern];

      if vr.transitionReplacement = '' then
      begin
        Mapping := pattern.Mappings[0];
      end
      else begin
         if vr.transitionReplacement = RULE_DIRT then
           Mapping := pattern.Mappings[0]
         else
            Mapping := pattern.Mappings[1];
      end;

      if not pattern.DiffImages then
      begin
        info.TerSubtype:=system.Random(Mapping.Upper-Mapping.Lower)+Mapping.Lower;
        info.mir:=vr.flip;
      end
      else begin

        FramesPerRotation := (Mapping.Upper-Mapping.Lower+1) div pattern.RotationTypesCount;

        Flip := ifthen((pattern.rotationTypesCount = 2) and (vr.flip = 2),1, vr.flip);
        FirstFrame := Mapping.Lower + Flip*FramesPerRotation;

        info.TerSubtype:= FirstFrame + system.Random(FramesPerRotation-1);
        info.mir:=0;
      end;

      oqIt := FOutQueue.NFind(info);

      if Assigned(oqIt) then
      begin
        FOutQueue.Delete(info); //it was queued with old values
      end;

      FOutQueue.Insert(info);

    until not it.next;
    FreeAndNil(it);
  end;
end;

procedure TEditTerrain.Undo;
var
  i: Integer;
begin
  for i := 0 to FOldTileInfos.Size - 1 do
  begin
    UndoTile(FOldTileInfos[i]);
  end;
end;

procedure TEditTerrain.UndoTile(constref Tile: TTileInfo);
begin
  FMap.SetTerrain(FLevel, Tile.X, Tile.Y,Tile.TerType, Tile.TerSubtype, Tile.mir);
end;

function TEditTerrain.ValidateTerrainView(info: TTileInfo; pattern: TPattern;
  recDepth: integer): TValidationResult;
var
  flip: integer;
  flipped: TPattern;
begin
  Result.result := False;
  Result.transitionReplacement := '';
  Result.flip := 0;

  for flip := 0 to 4 - 1 do
  begin
    flipped := FMap.TerrainManager.PatternConfig.GetFlippedPattern(pattern,flip);

    Result := ValidateTerrainViewInner(info,flipped, recDepth);

    if Result.result then
    begin
      Result.flip:=flip;
      Exit;
    end;
  end;
end;

function TEditTerrain.ValidateTerrainViewInner(info: TTileInfo;
  pattern: TPattern; recDepth: integer): TValidationResult;

  function isSandType(tt: TTerrainType ): Boolean;
  begin
    case tt of
      TTerrainType.water,TTerrainType.sand, TTerrainType.rock:isSandType := True  ;
    else
      isSandType := False;
    end;
  end;

var
  terType: TTerrainType;
  centerTerGroup: TTerrainGroup;

  totalPoints: Integer;
  transitionReplacement: String;

  currentPos: TMapCoord;

  cur_tinfo: TTileInfo;

  i: Integer;
  j: Integer;
  cx: Integer;
  cy: Integer;
  isAlien: Boolean;
  topPoints: Integer;
  rule: TWeightedRule;
  patternForRule: TPattern;
  rslt: TValidationResult;

  procedure applyValidationRslt(AResult:Boolean);
  begin
    if AResult then
    begin
      topPoints := Max(topPoints,rule.points);
    end;
  end;
var

  nativeTestOk: Boolean;
  nativeTestStrongOk: Boolean;
  dirtTestOk: Boolean;
  sandTestOK: Boolean;
begin
  Result.result:=false;
  Result.flip:=0;
  Result.transitionReplacement:='';

  centerTerGroup :=TERRAIN_GROUPS[info.TerType];

  totalPoints:=0;
  transitionReplacement:='';

  for i := 0 to 9 - 1 do
  begin

    if i = 4 then
      Continue;// The center, middle cell can be skipped

    cx := info.x + (i mod 3) - 1;
    cy := info.Y + (i div 3) - 1;

    currentPos.X:=cx;
    currentPos.y:=cy;

    isAlien := false;


    if not FMap.IsOnMap(FLevel, cx,cy) then
    begin
      cur_tinfo := info;
    end
    else
    begin
      cur_tinfo := getTinfo(cx,cy);

      if cur_tinfo.TerType <> info.TerType then
        isAlien := True;
    end;

    // Validate all rules per cell

    topPoints:=-1;
    for j := 0 to pattern.RData[i].Count - 1 do
    begin
      rule := pattern.RData[i][j].Clone;

      if not rule.IsStandartRule then
      begin
        if (recDepth = 0) and FMap.IsOnMap(FLevel, currentPos.X,currentPos.Y) then
        begin
          if cur_tinfo.TerType = info.TerType  then
          begin
            patternForRule := FMap.TerrainManager.PatternConfig.GetTerrainViewPatternById(centerTerGroup, rule.name);

            if Assigned(patternForRule) then
            begin
                rslt := ValidateTerrainView(cur_tinfo,patternForRule,1);
                if rslt.result then
                begin
                  topPoints := Max(topPoints,rule.points);
                end;
            end;

          end;
          rule.free;
          Continue;
        end
        else
        begin
          rule.name := RULE_NATIVE;
        end;
      end;

      // Validate cell with the ruleset of the pattern

      nativeTestOk := ((rule.name = RULE_NATIVE) or (rule.name = RULE_NATIVE_STRONG)) and not isAlien;
      nativeTestStrongOk:=nativeTestOk;

      case centerTerGroup of
        TTerrainGroup.NORMAL:
        begin
          dirtTestOk := (rule.IsDirt or rule.IsTrans)
            and isAlien and not isSandType(cur_tinfo.TerType);

          sandTestOK := (rule.IsSand or rule.IsTrans)
           and isSandType(cur_tinfo.TerType);

          if (transitionReplacement = '')
            and (rule.IsTrans)
            and (dirtTestOk or sandTestOK) then
          begin
            if dirtTestOk then
            begin
              transitionReplacement := RULE_DIRT;
            end
            else begin
              transitionReplacement := RULE_SAND;
            end;
          end;

          if rule.IsTrans then
          begin
            applyValidationRslt(
              (dirtTestOk and (transitionReplacement <> RULE_SAND))
              or (sandTestOK and (transitionReplacement <> RULE_DIRT))
            );
          end
          else begin
            applyValidationRslt( rule.IsAny or dirtTestOk or sandTestOK or nativeTestOk);
          end;
        end;
        TTerrainGroup.DIRT:
        begin
          nativeTestOk:=rule.IsNative and not isSandType(cur_tinfo.TerType);

          sandTestOK := (rule.IsSand or rule.IsTrans) and isSandType(cur_tinfo.TerType);

          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk or nativeTestStrongOk);
        end;
        TTerrainGroup.SAND:
        begin
          applyValidationRslt(true);
        end;
        TTerrainGroup.WATER,
        TTerrainGroup.ROCK:
        begin
          sandTestOK := (rule.IsSand or rule.IsTrans) and isAlien;
          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk);
        end;
      end;

      rule.free;
    end;

    if topPoints = -1 then
       Exit
    else
       totalPoints+=topPoints;
  end;

  if (totalPoints >= pattern.MinPoints) and (totalPoints <= pattern.MaxPoints) then
  begin
    Result.result:=true;
    Result.transitionReplacement:=transitionReplacement;
  end

end;

end.

