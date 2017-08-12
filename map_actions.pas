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
unit map_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types, map_rect, undo_map,
  editor_gl;

const
  INVALID_COORDINATE: TMapCoord = (x:-1; y:-1);

type
  TBrushMode = (none, fixed, area);

  { TGLessCoord }

  generic TGLessCoord <T> = class
  public
    class function c(a,b: T): boolean;
  end;

  TCompareCoord = specialize TGLessCoord<TMapCoord>;

  TCoordSet = specialize TSet<TMapCoord,TCompareCoord>;

type
  { TMultiTileMapAction }

  TMultiTileMapAction = class(TMapUndoItem)
  private
    FLevel: Integer;
  protected
    //by default invalidates all tiles
    function GetChangedRegion(): TMapRect; virtual;
  public
    constructor Create(AMap: TVCMIMap); override;
    procedure AddTile(X,Y: integer); virtual; abstract;
    property Level: Integer read FLevel;

    procedure LoadTiles(ASource: TCoordSet);

    function GetChangedRegion(ALevelIndex: integer): TMapRect; override; final;
  end;

  TMultiTileMapActionClass = class of TMultiTileMapAction;

  { TMapBrush }

  TMapBrush = class abstract (TComponent)
  strict private
    FDragging: Boolean;
    FMode: TBrushMode;
    FSelection: TCoordSet;
    FSize: Integer;

    FStartCoord: TMapCoord;
    FEndCooord: TMapCoord;

    procedure SetSize(AValue: Integer);

    procedure AddSquare(AMap: TVCMIMap; AX, AY: integer);

    procedure RenderSelectionRect(State: TLocalState; const StartCoord: TMapCoord; const EndCooord: TMapCoord);
  protected
  const
    RECT_COLOR: TRBGAColor = (r:50; g:50; b:50; a:255);
  protected
    property Selection: TCoordSet read FSelection;
    property Dragging: Boolean read FDragging;

    procedure ClearSelection;

    procedure CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord; var Stop: Boolean); virtual;

    procedure CheckAddTiles(AMap: TVCMIMap; point: TMapCoord);

    procedure RenderCursor(State: TLocalState; X, Y: integer);
    procedure RenderSelectionAllTiles(State: TLocalState);


    procedure SetMode(AMode: TBrushMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); virtual; abstract;

    procedure TileClicked(AMap: TVCMIMap; X, Y: integer);virtual;
    procedure TileMouseDown(AMap: TVCMIMap; X, Y: integer);virtual;
    procedure TileMouseUp(AMap: TVCMIMap; X, Y: integer);virtual;
    procedure TileMouseMove(AMap: TVCMIMap; X, Y: integer);virtual;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer); virtual;
    procedure RenderSelection(State: TLocalState); virtual;

    procedure MouseLeave(AMap: TVCMIMap); virtual;

    property Mode: TBrushMode read FMode;
    property Size: Integer read FSize write SetSize;
  end;

  { TIdleMapBrush }

  TIdleMapBrush = class(TMapBrush)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;
  end;

implementation

uses editor_consts;

{ TMultiTileMapAction }

function TMultiTileMapAction.GetChangedRegion: TMapRect;
begin
  Result := inherited GetChangedRegion(Level);
end;

constructor TMultiTileMapAction.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FLevel := AMap.CurrentLevelIndex;
end;

procedure TMultiTileMapAction.LoadTiles(ASource: TCoordSet);
var
  it: TCoordSet.TIterator;
begin
  it := ASource.Min;

  if Assigned(it) then
  begin
    repeat
      AddTile(it.Data.X, it.Data.Y);
    until not it.next;
    FreeAndNil(it);
  end;
end;

function TMultiTileMapAction.GetChangedRegion(ALevelIndex: integer): TMapRect;
begin
  if ALevelIndex = Level then
  begin
    Result := GetChangedRegion();
  end
  else
  begin
    Result.Create();
  end;
end;

{ TGLessCoord }

class function TGLessCoord.c(a, b: T): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TIdleMapBrush }

constructor TIdleMapBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Size := 0;
  SetMode(TBrushMode.none);
end;

procedure TIdleMapBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin
  //do nothing
end;


{ TMapBrush }

procedure TMapBrush.SetSize(AValue: Integer);
begin
  FSize:=AValue;
  Clear;
end;

procedure TMapBrush.AddSquare(AMap: TVCMIMap; AX, AY: integer);

  procedure CheckAddOneTileNested(const Coord: TMapCoord; var Stop: Boolean);
  begin
    CheckAddOneTile(Amap, Coord, stop);
  end;

var
  r: TMapRect;

begin
  r.Create();
  r.FTopLeft.Reset(AX,AY);
  r.FHeight := Size;
  r.FWidth := Size;
  r.Iterate(@CheckAddOneTileNested);
end;

procedure TMapBrush.CheckAddTiles(AMap: TVCMIMap; point: TMapCoord);
begin
  case Mode of
    TBrushMode.area:
    begin

    end;
    TBrushMode.fixed:
    begin
      AddSquare(AMap, point.X, point.Y);
    end;
  end;
end;

procedure TMapBrush.ClearSelection;
begin
  FSelection.Free;
  FSelection := TCoordSet.Create;
end;

procedure TMapBrush.CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord; var Stop: Boolean);
begin
  Selection.Insert(Coord);
end;

procedure TMapBrush.RenderCursor(State: TLocalState; X, Y: integer);
var
  dim: Integer;
  cx,cy: Integer;
begin
  cx := X * TILE_SIZE;
  cy := Y * TILE_SIZE;

  State.StartDrawingRects;

  State.SetFragmentColor(RECT_COLOR);

  dim := TILE_SIZE * Size;
  State.RenderRect(cx,cy,dim,dim);
  State.StopDrawing;
end;

procedure TMapBrush.RenderSelectionAllTiles(State: TLocalState);
var
  it: TCoordSet.TIterator;
  dim,cx,cy: Integer;
begin
  if Dragging then
  begin
    it := Selection.Min;
    if Assigned(it) then
    begin
      State.StartDrawingRects;
      State.SetFragmentColor(RECT_COLOR);
      dim := TILE_SIZE;
      repeat
        cx := it.Data.X * TILE_SIZE;
        cy := it.Data.Y * TILE_SIZE;
        State.RenderRect(cx,cy,dim,dim);
      until not it.next ;
      FreeAndNil(it);
      State.StopDrawing;
    end;
  end;
end;

procedure TMapBrush.RenderSelectionRect(State: TLocalState; const StartCoord: TMapCoord; const EndCooord: TMapCoord);
var
  cx,cy: Integer;
  r:TMapRect;
begin
  if Dragging then
  begin
    State.StartDrawingRects;
    r.SetFromCorners(StartCoord,EndCooord);

    cx := r.FTopLeft.X * TILE_SIZE;
    cy := r.FTopLeft.Y * TILE_SIZE;
    State.SetFragmentColor(RECT_COLOR);
    State.RenderRect(cx,cy,r.FWidth * TILE_SIZE ,r.FHeight * TILE_SIZE);
    State.StopDrawing;
  end;
end;

procedure TMapBrush.SetMode(AMode: TBrushMode);
begin
  FMode := AMode;
  Clear;
end;

constructor TMapBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelection := TCoordSet.Create;
end;

destructor TMapBrush.Destroy;
begin
  FSelection.Free;
  inherited Destroy;
end;

procedure TMapBrush.Clear;
begin
  FDragging := false;
  ClearSelection;
end;

procedure TMapBrush.TileClicked(AMap: TVCMIMap; X, Y: integer);
begin

end;

procedure TMapBrush.TileMouseDown(AMap: TVCMIMap; X, Y: integer);
begin
  FDragging:=True;

  FStartCoord.Reset(X,Y);
  FEndCooord.Reset(X,Y);
end;

procedure TMapBrush.TileMouseUp(AMap: TVCMIMap; X, Y: integer);
  procedure ProcessTile(const Coord: TMapCoord; var Stop: Boolean);
  begin
    Selection.Insert(Coord);
  end;
var
  r:TMapRect;
begin
  if FDragging then
  begin
    FDragging := False;

    case Mode of
      TBrushMode.area:
      begin
        ClearSelection;
        r.SetFromCorners(FStartCoord,FEndCooord);
        r.Iterate(@ProcessTile);
      end;
      TBrushMode.fixed:
      begin

      end;
    end;
  end;
end;

procedure TMapBrush.TileMouseMove(AMap: TVCMIMap; X, Y: integer);
var
  NewPoint: TMapCoord = (x:0; y:0);
  d, c: TMapCoord;
  SX, SY, E: integer;

begin
  if not (AMap.IsOnMap(AMap.CurrentLevelIndex,X,Y)) then
    exit;

  if Dragging then
  begin
    if Mode = TBrushMode.fixed then
    begin
      NewPoint.Reset(X,Y);

      if FEndCooord <> NewPoint then
      begin
        //draw line
        //(modified)Bresenham's algorimth
        //implementation based on BGRADrawLineAliased from http://sourceforge.net/projects/lazpaint (LGPL)

        D := NewPoint-FEndCooord;

        if D.X < 0 then
        begin
          SX := -1;
          D.X := -D.X;
        end
        else
        begin
          SX := 1;
        end;

        if D.Y < 0 then
        begin
          SY := -1;
          D.Y := -D.Y;
        end
        else
        begin
          SY := 1;
        end;

        D.X := D.X shl 1;
        D.Y := D.Y shl 1;

        c := FEndCooord;

        if D.X > D.Y then
        begin
          E := D.Y - D.X shr 1;

          while c.X <> NewPoint.X do
          begin
            CheckAddTiles(AMap, c);
            if E >= 0 then
            begin
              Inc(c.Y, SY);
              CheckAddTiles(AMap, c);
              Dec(E, D.X);
            end;
            Inc(c.X, SX);
            Inc(E, D.Y);
          end;
        end
        else
        begin
          E := D.X - D.Y shr 1;

          while c.Y <> NewPoint.Y do
          begin
            CheckAddTiles(AMap, c) ;
            if E >= 0 then
            begin
              Inc(c.X, SX);
              CheckAddTiles(AMap, c);
              Dec(E, D.Y);
            end;
            Inc(c.Y, SY);
            Inc(E, D.X);
          end;
        end;
      end;

      FEndCooord.Reset(x,y);

      CheckAddTiles(AMap, FEndCooord);

      AddSquare(AMap, x, y);
    end;


  end;
end;

procedure TMapBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin
  if (AMap.IsOnMap(AMap.CurrentLevelIndex,X,Y))
    and not (AMap.CurrentLevel.Tile[X,Y]^.TerType in [TTerrainType.rock, TTerrainType.water]) then
  begin
    //TODO: check cursor partially outside map
    if Mode = TBrushMode.fixed then
      RenderCursor(State, X, Y);
  end;
end;

procedure TMapBrush.RenderSelection(State: TLocalState);
begin
  case Mode of
    TBrushMode.area:
    begin
      RenderSelectionRect(State, FStartCoord, FEndCooord);
    end;
    TBrushMode.fixed:
    begin

    end;
  end;
end;

procedure TMapBrush.MouseLeave(AMap: TVCMIMap);
begin
  Clear;
end;

end.
