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
unit editor_types;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, gset, gutil, gvector;

type
{$push}
{$packenum 1}
  TTerrainType  = (dirt=0, sand, grass, snow, swamp, rough, subterranean, lava, water, rock{,border=$FF});
  TTerrainTypes = set of TTerrainType;
  TRiverType = (noRiver=0, clearRiver, icyRiver, muddyRiver, lavaRiver);
  TRoadType = (noRoad = 0, dirtRoad=1, grazvelRoad, cobblestoneRoad);
  TPlayer = (RED=0, BLUE, TAN, GREEN, ORANGE, PURPLE, TEAL, PINK, NONE = 255);

  TAITactics = (NONE=-1,RANDOM = 0,WARRIOR,BUILDER,EXPLORER);

  TDifficulty = (EASY = 0, NORMAL, HARD, EXPERT, IMPOSSIBLE);

{$pop}

  TPlayerColor = TPlayer.RED..TPlayer.PINK;
  TPlayers = set of TPlayerColor;

  TDefFrame = UInt8;

  //TFactionID = type integer;
  //TFactionIDCompare = specialize gutil.TLess<TFactionID> ;
  //TFactions  = specialize gset.TSet<TFactionID,TFactionIDCompare>;
  THeroClassID = type integer;
  THeroID = type integer;

  TObjectTypeID = type Integer;
  TCustomID = type integer;

  TArtifactID = type Integer;
  TCreatureID = type Integer;
  TSkillID = type Integer;
  TSpellID = type Integer;

  TResType = (wood = 0, mercury, ore, sulfur, crystal, gems, gold, mithril);

  TSex = (Male, Female);

const
  ALL_PLAYERS = [TPlayer.RED, TPlayer.BLUE, TPlayer.TAN, TPlayer.GREEN, TPlayer.ORANGE, TPlayer.PURPLE, TPlayer.TEAL, TPlayer.PINK];

const
  ID_RANDOM = -1;
  ID_INVALID = -1000;
  FACTION_RANDOM = ID_RANDOM;

const
  MASK_NOT_VISIBLE = '0'; //not visible,  passable
  MASK_VISIBLE     = 'V'; //visible,  passable
  MASK_BLOCKED     = 'B'; //blocked, visible
  MASK_HIDDEN      = 'H'; //blocked, not visible
  MASK_ACTIVABLE   = 'A'; //visible, activable
  MASK_TRIGGER     = 'T'; //not visible, activable

  Z_INDEX_OVERLAY = -1000000;
const
  ALL_TERRAINS = [TTerrainType.dirt,
  TTerrainType.sand,
  TTerrainType.grass,
  TTerrainType.snow,
  TTerrainType.swamp,
  TTerrainType.rough,
  TTerrainType.subterranean,
  TTerrainType.lava,
  TTerrainType.water];

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

implementation

end.

