{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types, map_rect, undo_map,
  editor_gl;

type
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
    procedure AddTile(X,Y: integer); virtual; abstract;
    property Level: Integer read FLevel write FLevel;

    function GetChangedRegion(ALevelIndex: integer): TMapRect; override;
  end;

  { TMapBrush }

  TMapBrush = class abstract (TComponent)
  strict private
    FDragging: Boolean;
    FSelection: TCoordSet;
  protected
  const
    RECT_COLOR: TRBGAColor = (r:50; g:50; b:50; a:255);
  protected
    property Selection: TCoordSet read FSelection;
    property Dragging: Boolean read FDragging;
    procedure AddTile(AMap: TVCMIMap;AX,AY: integer); virtual; abstract;

    procedure FillActionObjectTiles(AObject:TMultiTileMapAction);
    procedure RenderCursor(State: TLocalState; X,Y, Size: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); virtual; abstract;

    procedure TileClicked(AMap: TVCMIMap; X,Y: integer);virtual;
    procedure TileMouseDown(AMap: TVCMIMap; X,Y: integer);virtual;
    procedure TileMouseUp(AMap: TVCMIMap; X,Y: integer);virtual;
    procedure TileMouseMove(AMap: TVCMIMap; X,Y: integer);virtual;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap; X,Y: integer); virtual;
    procedure RenderSelection(State: TLocalState); virtual;
  end;

  { TIdleMapBrush }

  TIdleMapBrush = class(TMapBrush)
  protected
    procedure AddTile(AMap: TVCMIMap;X,Y: integer); override;
  public
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;
  end;

implementation

uses editor_consts;

{ TMultiTileMapAction }

function TMultiTileMapAction.GetChangedRegion: TMapRect;
begin
  Result := inherited GetChangedRegion(Level);
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

procedure TIdleMapBrush.AddTile(AMap: TVCMIMap; X, Y: integer);
begin
  //to nothing here
end;

procedure TIdleMapBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin
  //do nothing
end;


{ TMapBrush }

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

procedure TMapBrush.RenderCursor(State: TLocalState; X, Y, Size: integer);
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
  FSelection.Free;
  FSelection := TCoordSet.Create;
end;

procedure TMapBrush.TileClicked(AMap: TVCMIMap; X, Y: integer);
begin

end;

procedure TMapBrush.TileMouseDown(AMap: TVCMIMap; X, Y: integer);
begin
  FDragging:=True;
  AddTile(AMap,X,Y);
end;

procedure TMapBrush.TileMouseUp(AMap: TVCMIMap; X, Y: integer);
begin
  FDragging := False;
end;

procedure TMapBrush.TileMouseMove(AMap: TVCMIMap; X, Y: integer);
begin
  if Dragging then
  begin
    AddTile(AMap, X,Y);
  end;
end;

procedure TMapBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin

end;

procedure TMapBrush.RenderSelection(State: TLocalState);
begin

end;

end.
