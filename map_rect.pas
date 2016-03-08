{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_rect;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, editor_types;

type
  { TMapRect }

  TMapRect = object
    FTopLeft: TMapCoord;
    FWidth,FHeight: SizeInt;

    constructor Create();
    constructor SetFromCenter(X,Y, Width,Height: integer);
    constructor SetFromCorners(AFirst, ASecond:TMapCoord);
    constructor SetFromCorners(X1,Y1,X2,Y2:Integer);

    function Left(): integer; inline;
    function Right(): integer; inline;
    function Top(): integer; inline;
    function Bottom(): integer; inline;

    function TopLeft():TMapCoord; inline;
    function TopRight():TMapCoord; inline;
    function BottomLeft():TMapCoord; inline;
    function BottomRight():TMapCoord; inline;

    function Intersect(Other: TMapRect):TMapRect;

    procedure CombineWith(Other: TMapRect);

    procedure Clear(); inline;

    function IsEmpty: Boolean; inline;

    procedure Iterate(Callback: TMapCoordForEach);
  end;

implementation

uses
  Math;

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

procedure TMapRect.CombineWith(Other: TMapRect);
var
  new_topleft, new_bottomright: TMapCoord;
begin
  if IsEmpty then
  begin
    SetFromCorners(Other.TopLeft, Other.BottomRight());
  end
  else
  begin
    new_topleft.Reset(Min(FTopLeft.X, Other.FTopLeft.X), Min(FTopLeft.Y, Other.FTopLeft.Y));
    new_bottomright.Reset(Max(Right(), Other.Right()), Max(Bottom(), Other.Bottom()));

    SetFromCorners(new_topleft,new_bottomright);
  end;
end;

procedure TMapRect.Clear;
begin
  FTopLeft.Clear();
  FHeight:=0;
  FWidth:=0;
end;

function TMapRect.IsEmpty: Boolean;
begin
  Result := (FHeight = 0) and (FWidth = 0);
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

constructor TMapRect.SetFromCorners(AFirst, ASecond: TMapCoord);
begin
  Clear();

  FTopLeft.Reset(Min(AFirst.X,ASecond.X), Min(AFirst.Y,ASecond.Y));

  FWidth := abs(AFirst.X-ASecond.X)+1;
  FHeight := abs(AFirst.Y-ASecond.Y)+1;
end;

constructor TMapRect.SetFromCorners(X1, Y1, X2, Y2: Integer);
var
  AFirst, ASecond: TMapCoord;
begin
  AFirst.Reset(x1,y1);
  ASecond.Reset(x2,y2);
  SetFromCorners(AFirst, ASecond);
end;

end.

