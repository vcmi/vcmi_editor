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
unit Map;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, editor_types, terrain;

const
  MAP_DEFAULT_SIZE = 512;
  MAP_DEFAULT_LEVELS = 1;

type

  TMapDiscreteSize = (S = 36, M = 72, L = 108, XL = 144);

  { TMapTile }
  PMapTile = ^TMapTile;
  TMapTile = object
  private  //not strict, used by map
    FTerType: TTerrainType;
    FTerSubtype: UInt8;
  public
    constructor Create();

    property TerType: TTerrainType read FTerType;
    property TerSubType: UInt8 read FTerSubtype;

    procedure Render(mgr: TTerrainManager; X,Y: Integer); inline;
  end;

  { TVCMIMap }

  TVCMIMap = class
  private
    FCurrentLevel: Integer;

    FHeight: Integer;
    FTerrainManager: TTerrainManager;
    FWigth: Integer;
    FLevels: Integer;

    FTerrain: array of array of array of TMapTile; //levels, X, Y

    procedure RecreateTerrainArray;
    procedure SetCurrentLevel(AValue: Integer);
    procedure SetTerrainManager(AValue: TTerrainManager);
  public
    constructor Create(tm: TTerrainManager); //default constructor
    destructor Destroy; override;

    property Height: Integer read FHeight;
    property Width: Integer read FWigth;

    procedure SetTerrain(X, Y: Integer; TT: TTerrainType); overload; //set default terrain
    procedure SetTerrain(Level, X, Y: Integer; TT: TTerrainType; TS: UInt8); overload; //set concete terrain
    procedure FillLevel(TT: TTerrainType);

    function GetTile(Level, X, Y: Integer): PMapTile;

    //Left, Right, top, Bottom - clip rect in Tiles
    procedure Render(Left, Right, Top, Bottom: Integer);

    property CurrentLevel: Integer read FCurrentLevel write SetCurrentLevel;
  end;

implementation

{ TMapTile }

constructor TMapTile.Create;
begin

end;

procedure TMapTile.Render(mgr: TTerrainManager; X, Y: Integer);
begin
  mgr.Render(FTerType,FTerSubtype,X,Y);
end;

{ TVCMIMap }

constructor TVCMIMap.Create(tm: TTerrainManager);
begin
  FHeight := MAP_DEFAULT_SIZE;
  FWigth := MAP_DEFAULT_SIZE;
  FLevels := MAP_DEFAULT_LEVELS;

  SetTerrainManager(tm);

  RecreateTerrainArray;
  CurrentLevel := 0;
end;

destructor TVCMIMap.Destroy;
begin

  inherited Destroy;
end;

procedure TVCMIMap.FillLevel(TT: TTerrainType);
var
  x: Integer;
  Y: Integer;
begin
  for x := 0 to FWigth - 1 do
  begin
    for Y := 0 to FHeight - 1 do
    begin
      SetTerrain(x,y,tt);
    end;

  end;

end;

function TVCMIMap.GetTile(Level, X, Y: Integer): PMapTile;
begin
  Result := @(FTerrain[Level][X][Y]);
end;

procedure TVCMIMap.RecreateTerrainArray;
var
  Level: Integer;
  X: Integer;
  Y: Integer;

  tt: TTerrainType;
begin
  SetLength(FTerrain, FLevels);

  for Level := 0 to FLevels-1 do
  begin

    tt := FTerrainManager.GetDefaultTerrain(Level);

    SetLength(FTerrain[Level],FWigth);
    for X := 0 to FWigth - 1 do
    begin
      SetLength(FTerrain[Level, X],FHeight);
      for Y := 0 to FHeight - 1 do
      begin
        FTerrain[Level][X][Y].Create();
        FTerrain[Level][X][Y].FTerType :=  tt;
        FTerrain[Level][X][Y].FTerSubtype := FTerrainManager.GetRandomNormalSubtype(tt);
      end;
    end;
  end;
end;

procedure TVCMIMap.Render(Left, Right, Top, Bottom: Integer);
var
  i: Integer;
  j: Integer;
begin

  Right := Min(Right, FWigth - 1);
  Bottom := Min(Bottom, FHeight - 1);

  for i := Left to Right do
  begin
    for j := Top to Bottom do
    begin
      FTerrain[FCurrentLevel][i][j].Render(FTerrainManager,i,j);
    end;
  end;
end;

procedure TVCMIMap.SetCurrentLevel(AValue: Integer);
begin
  if FCurrentLevel = AValue then Exit; //TODO: check
  FCurrentLevel := AValue;
end;

procedure TVCMIMap.SetTerrain(Level, X, Y: Integer; TT: TTerrainType; TS: UInt8
  );
begin
  FTerrain[Level][X][Y].FTerType := TT;
  FTerrain[Level][X][Y].FTerSubtype := TS;
end;

procedure TVCMIMap.SetTerrain(X, Y: Integer; TT: TTerrainType);
begin
  SetTerrain(CurrentLevel,X,Y,TT,FTerrainManager.GetRandomNormalSubtype(TT));
end;

procedure TVCMIMap.SetTerrainManager(AValue: TTerrainManager);
begin
  if FTerrainManager = AValue then Exit;
  FTerrainManager := AValue;
end;

end.

