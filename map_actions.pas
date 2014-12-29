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
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types, undo_map;

type

  { TMapCoord }

  TMapCoord = record
    X,Y: integer;

    procedure Clear(); inline;
    procedure Reset(AX,AY: integer); inline;
  end;

  operator+ (a,b:TMapCoord):TMapCoord;

type

  { TMapRect }

  TMapCoordForEach = procedure (const Coord: TMapCoord; var Stop: Boolean) is nested;

  TMapRect = object
    FTopLeft: TMapCoord;
    FWidth,FHeight: SizeInt;

    constructor Create();
    constructor SetFromCenter(X,Y, Width,Height: integer);
    constructor SetFromCorners(AFirst, ASecond:TMapCoord);

    function Left(): integer; inline;
    function Right(): integer; inline;
    function Top(): integer; inline;
    function Bottom(): integer; inline;

    function TopLeft():TMapCoord; inline;
    function TopRight():TMapCoord; inline;
    function BottomLeft():TMapCoord; inline;
    function BottomRight():TMapCoord; inline;

    function Intersect(Other: TMapRect):TMapRect;

    procedure Clear(); inline;

    procedure Iterate(Callback: TMapCoordForEach);

    class function DimOfMap(Amap:TVCMIMap):TMapRect; static;
  end;

type

  { TGLessCoord }

  generic TGLessCoord <T> = class
  public
    class function c(a,b: T): boolean;
  end;

  TCompareCoord = specialize TGLessCoord<TMapCoord>;

  TCoordSet = specialize TSet<TMapCoord,TCompareCoord>;
type

  { TMapBrush }

  TMapBrush = class abstract (TComponent)
  strict private
    FSize: Integer;
    FTT: TTerrainType;
    FDragging: Boolean;
    FSelection: TCoordSet;
    procedure SetMap(AValue: TVCMIMap);
    procedure SetSize(AValue: Integer);
    procedure Settt(AValue: TTerrainType);
  protected
    FMap: TVCMIMap;
    property Selection: TCoordSet read FSelection;
    property Dragging: Boolean read FDragging;
    procedure AddTile(X,Y: integer); virtual; abstract;

    procedure FillActionObjectTiles(AObject:TMultiTileMapAction);
    procedure RenderCursor(X,Y, Size: integer);
  public
    constructor Create(AOwner: TComponent; AMap: TVCMIMap); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(AManager: TAbstractUndoManager); virtual; abstract;

    procedure TileClicked(X,Y: integer);virtual;
    procedure TileMouseDown(X,Y: integer);virtual;
    procedure TileMouseUp(X,Y: integer);virtual;
    procedure TileMouseMove(X,Y: integer);virtual;

    procedure RenderCursor(X,Y: integer); virtual;
    procedure RenderSelection(); virtual;

    property TT: TTerrainType read FTT write Settt;
    property Size: Integer read FSize write SetSize;

    property Map:TVCMIMap read FMap write SetMap;
  end;

  { TIdleMapBrush }

  TIdleMapBrush = class(TMapBrush)
  protected
    procedure AddTile(X,Y: integer); override;
    procedure Execute(AManager: TAbstractUndoManager); override;
  end;



implementation

uses editor_gl, editor_consts;

operator+(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x+b.X;
  result.Y:=a.y+b.y;
end;

{ TGLessCoord }

class function TGLessCoord.c(a, b: T): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TIdleMapBrush }

procedure TIdleMapBrush.AddTile(X, Y: integer);
begin
  //to nothing here
end;

procedure TIdleMapBrush.Execute(AManager: TAbstractUndoManager);
begin
  //do nothing
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

class function TMapRect.DimOfMap(Amap: TVCMIMap): TMapRect;
begin
  Result.Create;
  Result.FWidth:=Amap.CurrentLevel.Width;
  Result.FHeight:=Amap.CurrentLevel.Height;
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

constructor TMapRect.SetFromCorners(AFirst, ASecond: TMapCoord);
begin
  Clear();

  FTopLeft.Reset(Min(AFirst.X,ASecond.X), Min(AFirst.Y,ASecond.Y));

  FWidth := abs(AFirst.X-ASecond.X)+1;
  FHeight := abs(AFirst.Y-ASecond.Y)+1;
end;

{ TMapBrush }

procedure TMapBrush.SetSize(AValue: Integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
  Clear;
end;

procedure TMapBrush.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TMapBrush.Settt(AValue: TTerrainType);
begin
  if FTT=AValue then Exit;
  FTT:=AValue;
  Clear;
end;

procedure TMapBrush.FillActionObjectTiles(AObject: TMultiTileMapAction);
var
  it: TCoordSet.TIterator;
begin
  it := Selection.Min;

  if Assigned(it) then
  begin
    repeat
      AObject.AddTile(it.Data.X, it.Data.Y);
    until not it.next ;
    FreeAndNil(it);
  end;
end;

procedure TMapBrush.RenderCursor(X, Y, Size: integer);
var
  dim: Integer;
  cx,cy: Integer;
begin
  cx := X * TILE_SIZE;
  cy := Y * TILE_SIZE;

    editor_gl.CurrentContextState.StartDrawingRects;
    dim := TILE_SIZE * Size;
    editor_gl.CurrentContextState.RenderRect(cx,cy,dim,dim);
    editor_gl.CurrentContextState.StopDrawing;

end;

constructor TMapBrush.Create(AOwner: TComponent; AMap: TVCMIMap);
begin
  inherited Create(AOwner);
  FSelection := TCoordSet.Create;
  FMap := AMap;
end;

destructor TMapBrush.Destroy;
begin
  FSelection.Free;
  inherited Destroy;
end;

procedure TMapBrush.Clear;
begin
  FDragging := false;
  FSelection.Free;
  FSelection := TCoordSet.Create;
end;

procedure TMapBrush.TileClicked(X, Y: integer);
begin

end;

procedure TMapBrush.TileMouseDown(X, Y: integer);
begin
  FDragging:=True;
  AddTile(X,Y);
end;

procedure TMapBrush.TileMouseUp(X, Y: integer);
begin
  FDragging := False;
end;

procedure TMapBrush.TileMouseMove(X, Y: integer);
begin
  if Dragging then
  begin
    AddTile(X,Y);
  end;
end;

procedure TMapBrush.RenderCursor(X, Y: integer);
begin

end;

procedure TMapBrush.RenderSelection;
var
  it: TCoordSet.TIterator;
  dim,cx,cy: Integer;
begin
  if Dragging then
  begin
    it := Selection.Min;
    if Assigned(it) then
    begin
      editor_gl.CurrentContextState.StartDrawingRects;
      dim := TILE_SIZE;
      repeat
        cx := it.Data.X * TILE_SIZE;
        cy := it.Data.Y * TILE_SIZE;
        editor_gl.CurrentContextState.RenderRect(cx,cy,dim,dim);
      until not it.next ;
      FreeAndNil(it);
      editor_gl.CurrentContextState.StopDrawing;
    end;
  end;
end;

{ TMapCoord }

procedure TMapCoord.Clear;
begin
  Reset(0,0);
end;

procedure TMapCoord.Reset(AX, AY: integer);
begin
  Self.X:=AX;
  Self.Y:=AY;
end;


end.
