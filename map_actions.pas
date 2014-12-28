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
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types;

type

  { TMapCoord }

  TMapCoord = record
    X,Y: integer;

    procedure Clear(); inline;
    procedure Reset(AX,AY: integer); inline;
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

  { TMapBrush }

  TMapBrush = class abstract (TComponent)
  strict private
    FSize: Integer;
    FTT: TTerrainType;
    FDragging: Boolean;
    procedure SetSize(AValue: Integer);
    procedure Settt(AValue: TTerrainType);
  protected
    property Dragging: Boolean read FDragging;
    procedure AddTile(X,Y: integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); virtual;

    procedure TileClicked(X,Y: integer);virtual;
    procedure TileMouseDown(X,Y: integer);virtual;
    procedure TileMouseUp(X,Y: integer);virtual;
    procedure TileMouseMove(X,Y: integer);virtual;

    procedure RenderCursor(X,Y: integer); virtual;
    procedure RenderSelection(); virtual;

    property TT: TTerrainType read FTT write Settt;
    property Size: Integer read FSize write SetSize;
  end;

  { TIdleMapBrush }

  TIdleMapBrush = class(TMapBrush)
  protected
    procedure AddTile(X,Y: integer); override;
  end;



implementation

operator+(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x+b.X;
  result.Y:=a.y+b.y;
end;

{ TIdleMapBrush }

procedure TIdleMapBrush.AddTile(X, Y: integer);
begin
  //to nothing here
end;

{ TCompareCoord }

class function TCompareCoord.c(a, b: TMapCoord): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TMapBrush }

procedure TMapBrush.SetSize(AValue: Integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
  Clear;
end;

procedure TMapBrush.Settt(AValue: TTerrainType);
begin
  if FTT=AValue then Exit;
  FTT:=AValue;
  Clear;
end;

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
  FDragging := false;
end;

procedure TMapBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin

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
begin

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
