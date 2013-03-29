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
  Classes, SysUtils,gset,gutil;

type
{$push}
{$packenum 1}
  TTerrainType  = (dirt=0, sand, grass, snow, swamp, rough, sub, lava, water, rock{,border=$FF});
  TTerrainTypes = set of TTerrainType;
  TRiverType = (noRiver=0, clearRiver, icyRiver, muddyRiver, lavaRiver);
  TRoadType = (noRoad = 0, dirtRoad=1, grazvelRoad, cobblestoneRoad);
  TPlayer = (RED=0, BLUE, TAN, GREEN, ORANGE, PURPLE, TEAL, PINK, NONE = 255);

  TAITactics = (None=-1,Random = 0,Warrior,Builder,Explorer);

  TDifficulty = (Easy = 0, Normal, Hard, Expert, Impossible);

{$pop}

  TPlayerColor = TPlayer.RED..TPlayer.PINK;
  TPlayers = set of TPlayerColor;

  TDefFrame = UInt8;

  TFactionID = type integer;
  TFactionIDCompare = specialize gutil.TLess<TFactionID> ;
  TFactions  = specialize gset.TSet<TFactionID,TFactionIDCompare>;
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
  ID_RANDOM = -1;
  ID_INVALID = -1000;
  FACTION_RANDOM = TFactionID(ID_RANDOM);

const
  MASK_NOT_VISIBLE = '0';
  MASK_VISIBLE     = 'V';
  MASK_BLOCKED     = 'B';
  MASK_ACTIVABLE   = 'A';

  Z_INDEX_OVERLAY = -1000000;
const
  ALL_TERRAINS = [TTerrainType.dirt,
  TTerrainType.sand,
  TTerrainType.grass,
  TTerrainType.snow,
  TTerrainType.swamp,
  TTerrainType.rough,
  TTerrainType.sub,
  TTerrainType.lava,
  TTerrainType.water];

type
  TLocalizedString = AnsiString; //LCL use ansisring encode to UTF8

implementation

end.

