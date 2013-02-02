{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
unit terrain;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, editor_types, def, fpjson,fpjsonrtti,vcmi_json,filesystem_base;

type

  TTerrainGroup = (
    NORMAL,
    DIRT,
    SAND,
    WATER,
    ROCK);

  {$push}
  {$m+}

  { TTransition }

  TTransition = class(TCollectionItem)
  private
    FData: TStringList;
    FID: string;
    FMapping: string;
    FMinPoints: integer;
    function GetData: TStrings;
    procedure SetID(AValue: string);
    procedure SetMapping(AValue: string);
    procedure SetMinPoints(AValue: integer);
  published
    constructor Create(ACollection: TCollection); override;
    property Data: TStrings read GetData;
    property Mapping: string read FMapping write SetMapping;
    property ID:string read FID write SetID;
    property MinPoints: integer read FMinPoints write SetMinPoints;
  end;

  { TTransitions }

  TTransitions = class(TCollection)
  public
    constructor Create;
  end;

  { TTerrainPatternConfig }

  TTerrainPatternConfig = class
  private
    FDirt: TTransitions;
    FNormal: TTransitions;
    FRock: TTransitions;
    FSand: TTransitions;
    FWater: TTransitions;

  public
    constructor Create;
    destructor Destroy; override;

    function GetGroupConfig(AGroup: TTerrainGroup):TTransitions;

    function GetTerrainConfig(ATerrain: TTerrainType):TTransitions;
  published
    property Dirt: TTransitions read FDirt;
    property Normal: TTransitions read FNormal;
    property Sand: TTransitions read FSand;
    property Water: TTransitions read FWater;
    property Rock: TTransitions read FRock;
  end;


  {$pop}

  { TTerrainManager }

  TTerrainManager = class (TGraphicsCosnumer)
  private
    FTerrainDefs: array [TTerrainType] of TDef;

    FPatternConfig: TTerrainPatternConfig;

    procedure InitTerrainDef(tt: TTerrainType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadConfig;

    procedure LoadTerrainGraphics;

    procedure Render(const tt: TTerrainType; sbt: UInt8; X, Y: Integer; Flags: UInt8);

    function GetDefaultTerrain(const Level: Integer): TTerrainType;
    function GetRandomNormalSubtype(const tt: TTerrainType): UInt8;

  end;

implementation

type
  TTerrainViewInterval = record
    min, max: uint8;
  end;

const

  TERRAIN_DEF_FILES: array[TTerrainType] of string = (
    'DIRTTL',
    'sandtl',
    'GRASTL',
    'Snowtl',
    'Swmptl',
    'ROUGTL',
    'Subbtl',
    'Lavatl',
    'Watrtl',
    'rocktl'
    );

  // 'Clrrvr.def' "Icyrvr.def" "Lavrvr.def" "Mudrvr.def"
  // 'cobbrd.def 'dirtrd.def' 'gravrd.def'

procedure SetView(out V: TTerrainViewInterval; min,max: uint8);
begin
  v.max:=max;
  v.min:=min;
end;

{ TTerrainPatternConfig }

constructor TTerrainPatternConfig.Create;
begin
  FDirt := TTransitions.Create;
  FNormal := TTransitions.Create;
  FRock := TTransitions.Create;
  FSand := TTransitions.Create;
  FWater := TTransitions.Create;
end;

destructor TTerrainPatternConfig.Destroy;
begin
  FDirt.Free;
  FNormal.Free;
  FRock.Free;
  FSand.Free;
  FWater.Free;

  inherited Destroy;
end;

function TTerrainPatternConfig.GetGroupConfig(AGroup: TTerrainGroup
  ): TTransitions;
begin
  case AGroup of
    TTerrainGroup.DIRT: Result := FDirt;
    TTerrainGroup.NORMAL: Result := FNormal;
    TTerrainGroup.ROCK: Result := FRock;
    TTerrainGroup.SAND: Result := FSand;
    TTerrainGroup.WATER: Result := FWater;
  end;
end;

function TTerrainPatternConfig.GetTerrainConfig(ATerrain: TTerrainType
  ): TTransitions;
const
  TERRAIN_GROUPS: array[TTerrainType] of TTerrainGroup =
    (TTerrainGroup.DIRT,
    TTerrainGroup.SAND,
    TTerrainGroup.NORMAL,
    TTerrainGroup.NORMAL,
    TTerrainGroup.NORMAL,
    TTerrainGroup.NORMAL,
    TTerrainGroup.NORMAL,
    TTerrainGroup.NORMAL,
    TTerrainGroup.WATER,
    TTerrainGroup.ROCK);
begin
  Result := GetGroupConfig(TERRAIN_GROUPS[ATerrain]);
end;

{ TTransitions }

constructor TTransitions.Create;
begin
  inherited Create(TTransition);
end;

{ TTransition }

constructor TTransition.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

function TTransition.GetData: TStrings;
begin
  Result := FData;
end;

procedure TTransition.SetID(AValue: string);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TTransition.SetMapping(AValue: string);
begin
  if FMapping = AValue then Exit;
  FMapping := AValue;
end;

procedure TTransition.SetMinPoints(AValue: integer);
begin
  if FMinPoints = AValue then Exit;
  FMinPoints := AValue;
end;

{ TTerrainManager }

constructor TTerrainManager.Create(AOwner: TComponent);
var
  tt: TTerrainType;
begin
  inherited Create(AOwner);

  FPatternConfig := TTerrainPatternConfig.Create;
end;

destructor TTerrainManager.Destroy;
var
  tt: TTerrainType;
begin
  FPatternConfig.Free;
  inherited Destroy;
end;

function TTerrainManager.GetDefaultTerrain(const Level: Integer): TTerrainType;
begin
  if Level <=0 then
  begin
    Result := TTerrainType.water;
  end
  else begin
    Result := TTerrainType.rock;
  end;
end;

function TTerrainManager.GetRandomNormalSubtype(const tt: TTerrainType): UInt8;
var
  vews: TTerrainViewInterval;
begin
  Result :=0;
  case tt of
    TTerrainType.dirt:SetView(vews,21,44);
    TTerrainType.sand:SetView(vews,0,23);
    TTerrainType.grass,
    TTerrainType.snow,
    TTerrainType.swamp,
    TTerrainType.rough,
    TTerrainType.sub,
    TTerrainType.lava:SetView(vews,49,63); //SetView(vews,49,72);
    TTerrainType.water:SetView(vews,20,32);
    TTerrainType.rock: SetView(vews,0,0);
  else
    raise Exception.Create('Unknown terrain: '+IntToStr(Ord(tt)));
  end;

  { TODO : Handle decorative tiles }
  Result := Random(vews.max-vews.min)+vews.min;

end;

procedure TTerrainManager.InitTerrainDef(tt: TTerrainType);
begin
  FTerrainDefs[tt] := GraphicsManager.GetGraphics(TERRAIN_DEF_FILES[tt])
end;

procedure TTerrainManager.LoadConfig;
begin
  //TODO: TTerrainManager.LoadConfig
end;

procedure TTerrainManager.LoadTerrainGraphics;
var
  tt: TTerrainType;
begin
  for tt := Low(TTerrainType) to High(TTerrainType) do
  begin
    InitTerrainDef(tt);
  end;
end;

procedure TTerrainManager.Render(const tt: TTerrainType; sbt: UInt8; X,
  Y: Integer; Flags: UInt8);
begin
  FTerrainDefs[tt].RenderF(sbt, x*TILE_SIZE, y*TILE_SIZE,Flags);
end;


end.






