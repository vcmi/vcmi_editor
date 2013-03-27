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
unit minimap;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Graphics, Map, editor_types, ExtCtrls;

type

  { TMinimap }

  TMinimap = class (TComponent)
  private
    FterrainColors: array[TTerrainType] of TColor;
    FMap: TVCMIMap;

    FMapImg : TBitmap;

    FMapImgValid: Boolean;
    FMapDimentionsValid: boolean;

    procedure MayBeResizeImg;
    procedure MayBeUpdateImg;

    procedure SetMap(AValue: TVCMIMap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Map: TVCMIMap read FMap write SetMap;

    procedure Paint(pb: TPaintBox);

    procedure InvalidateDimensions;
    procedure InvalidateLevel; //todo:InvalidateLevel

    procedure InvalidateMap;
    //procedure InvalidateAxis;
  end;

implementation


{ TMinimap }

constructor TMinimap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FterrainColors[TTerrainType.dirt] := RGBToColor( 82, 56, 8 );
  FterrainColors[TTerrainType.sand] := RGBToColor(222, 207, 140);
  FterrainColors[TTerrainType.grass] := RGBToColor(0, 65, 0 );
  FterrainColors[TTerrainType.snow] := RGBToColor(181, 199, 198);

  FterrainColors[TTerrainType.rough] := RGBToColor(132, 113, 49);
  FterrainColors[TTerrainType.sub] := RGBToColor(132, 48, 0);
  FterrainColors[TTerrainType.lava] := RGBToColor(74, 73, 74);
  FterrainColors[TTerrainType.water] := RGBToColor (8, 81, 148);

  FterrainColors[TTerrainType.rock] := RGBToColor(0, 0, 0);

  FMapImg := TBitmap.Create;
end;

destructor TMinimap.Destroy;
begin
  FMapImg.Free;
  inherited Destroy;
end;

procedure TMinimap.InvalidateDimensions;
begin
  FMapDimentionsValid := False;
end;

procedure TMinimap.InvalidateLevel;
begin
  //todo
end;

procedure TMinimap.InvalidateMap;
begin
  FMapImgValid := False;
end;

procedure TMinimap.MayBeResizeImg;
begin
  if not Assigned(FMap) then
    Exit;
  if not FMapDimentionsValid then
  begin

  end;

  FMapDimentionsValid := True;
end;

procedure TMinimap.MayBeUpdateImg;
var
  level: Integer;
  ctx: TCanvas;

  scale: Double;
  row, col: Integer;
  tile_size: Integer;

  tile: TMapTile;
  tile_rect: TRect;
begin
  if not Assigned(FMap) then
    Exit;
  if FMapImgValid then
    Exit;
  level := FMap.CurrentLevel;
  ctx := FMapImg.Canvas;

  ctx.Brush.Color := clWhite;
  ctx.FillRect(0, 0, FMapImg.Width,FMapImg.Height);

  scale := Double(FMapImg.Width) / Double(Fmap.Width);

  tile_size := trunc(scale)+1;

  for row := 0 to FMap.Height - 1 do
  begin
    for col := 0 to FMap.Width - 1 do
    begin
      tile := FMap.GetTile(level,col,row);
      tile_rect := Rect(
        trunc(col*scale),
        trunc(row*scale),
        trunc(col*scale) + tile_size,
        trunc(row*scale) + tile_size);

      ctx.Brush.Color := FterrainColors[tile.TerType];

      ctx.FillRect(tile_rect);

    end;
  end;

  FMapImgValid := True;
end;

procedure TMinimap.Paint(pb: TPaintBox);
begin
  if not Assigned(FMap) then
    Exit;
  //TODO: more accurate painting
  //todo: paint selection
  //todo: invalidate map only on change

  if (FMapImg.Width<>pb.Width) or (FMapImg.Height<>pb.Height) then
  begin
    FMapImg.SetSize(pb.Width,pb.Height);
    InvalidateMap;
  end;

  MayBeUpdateImg;

  try
    pb.Canvas.Changing;
    pb.Canvas.Draw(0,0,FMapImg);

  finally
    pb.Canvas.Changed;
  end;

end;

procedure TMinimap.SetMap(AValue: TVCMIMap);
begin
  if FMap = AValue then Exit;
  FMap := AValue;

  FMapImgValid := False;
  FMapDimentionsValid := False;
end;

end.

