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
unit editor_types;

{$I compilersetup.inc}



interface

uses
  Classes, SysUtils, gvector, FPimage;

type
{$push}
{$packenum 1}
  TTerrainType  = (dirt=0, sand, grass, snow, swamp, rough, subterra, lava, water, rock{,border=$FF});
  TTerrainTypes = set of TTerrainType;
  TRiverType = (noRiver=0, clearRiver=1, icyRiver=2, muddyRiver=3, lavaRiver=4);
  TRoadType = (noRoad = 0, dirtRoad=1, gravelRoad=2, cobblestoneRoad=3);
  TPlayer = (red=0, blue, tan, green, orange, purple, teal, pink, none = 255);

  TAITactics = (NONE=-1,RANDOM = 0,WARRIOR,BUILDER,EXPLORER);

  TDifficulty = (EASY = 0, NORMAL, HARD, EXPERT, IMPOSSIBLE);

  TPrimarySkill = (attack=0, defence=1, spellpower=2, knowledge=3);

  TResType = (wood = 0, mercury, ore, sulfur, crystal, gems, gold, mithril);

  THeroSex = (male=0, female=1, default = $FF);

  TSeerHutReward = (nothing, experience, manaPoints, moraleBonus, luckBonus, resources, primarySkill, secondarySkill, artifact, spell, creature);

  TCreatureCharacter = (compliant = 0, friendly = 1, aggressive = 2, hostile = 3, savage = 4);

{$pop}

type

  TPlayableBy = (None=0, PlayerOrAI=1, PlayerOnly=2, AIOnly=3);

type
  TScholarBonus = (primSkill, skill, spell, random);

  TPlayerColor = TPlayer.RED..TPlayer.PINK;
  TPlayers = set of TPlayerColor;

  TDefFrame = UInt8;

  TCustomID = type integer;

  TWinLossCondition = (have_0, haveBuilding_0,  destroy_0,  daysPassed,  isHuman,  standardWin,  daysWithoutTown,  constValue);

  TIdentifier = type AnsiString;//marks identifiers requiring resolution

  TInstanceID = type Int32;

  TSkillLevel = (none, basic, advanced, expert);

  TBuildMode = (normal, auto, special, grail);

  TMetaclass = (invalid, artifact, creature, faction, experience, hero, heroClass, luck, mana, morale, movement, primarySkill, secondarySkill, spell, resource);

  TArtifactClass = (SPECIAL, TREASURE, MINOR, MAJOR, RELIC);

  TObjectCategory = (Artifact, Creature, Dwelling, Hero, Other, Resource, Static, Town);
const
  ALL_PLAYERS = [TPlayer.red, TPlayer.blue, TPlayer.tan, TPlayer.green, TPlayer.orange, TPlayer.purple, TPlayer.teal, TPlayer.pink];

const
  ID_RANDOM = -1;
  ID_INVALID = -1000;
  FACTION_RANDOM = ID_RANDOM;

const
  MASK_NOT_VISIBLE = '0'; //not visible,  passable
  MASK_VISIBLE     = 'V'; //visible,  passable
  MASK_BLOCKED     = 'B'; //visible,  blocked
  MASK_HIDDEN      = 'H'; //not visible, blocked
  MASK_ACTIVABLE   = 'A'; //visible, blocked, activable
  MASK_TRIGGER     = 'T'; //not visible, blocked, activable

  Z_INDEX_OVERLAY = 100;
const
  ALL_TERRAINS =
 [TTerrainType.dirt,
  TTerrainType.sand,
  TTerrainType.grass,
  TTerrainType.snow,
  TTerrainType.swamp,
  TTerrainType.rough,
  TTerrainType.subterra,
  TTerrainType.lava];

type
  TLocalizedString = type AnsiString; //LCL use ansisring encode to UTF8

  TModId = AnsiString;

  TModdedConfigPath = record
    ModID: AnsiString;
    Config: array of string; //list of resource ids of config files
  end;

  TModdedConfigPaths = specialize TVector<TModdedConfigPath>;

type
  //raised if there are errors in VCMI configuration
  EConfigurationError = class (Exception)

  end;

type
  TH3DefColor = packed record
    r,g,b : UInt8;
  end;

  TRBGAColor = packed record
    r,g,b,a : UInt8;
  end;

  TRGBAPalette = packed array[0..255] of TRBGAColor;

  { TMapCoord }

  TMapCoord = record
    X,Y: integer;

    procedure Clear(); inline;
    procedure Reset(AX,AY: integer); inline;
  end;

  TMapCoordForEach = procedure (const Coord: TMapCoord; var Stop: Boolean) is nested;
  TMapCoordForEachO = procedure (const Coord: TMapCoord; var Stop: Boolean) of object;

  TPassabilityMask = array of array of Boolean;

  operator+ (a,b:TMapCoord):TMapCoord; inline;
  operator- (a,b:TMapCoord):TMapCoord; inline;
  operator= (a,b:TMapCoord):boolean; inline;

  operator= (a,b:TH3DefColor):boolean; inline;

implementation


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


operator+(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x+b.X;
  result.Y:=a.y+b.y;
end;

operator-(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x-b.X;
  result.Y:=a.y-b.y;
end;

operator=(a, b: TMapCoord): boolean;
begin
  result := (a.X = b.X) and (a.Y = b.Y);
end;

operator=(a, b: TH3DefColor): boolean;
begin
  result := (a.b = b.b) and (a.g = b.g) and (a.r = b.r);
end;

end.

