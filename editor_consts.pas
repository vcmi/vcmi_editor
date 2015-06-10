unit editor_consts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type //h3 object types
  TObj = (
    ALTAR_OF_SACRIFICE = 2,
    ANCHOR_POINT = 3,
    ARENA = 4,
    ARTIFACT = 5,
    PANDORAS_BOX = 6,
    BLACK_MARKET = 7,
    BOAT = 8,
    BORDERGUARD = 9,
    KEYMASTER = 10,
    BUOY = 11,
    CAMPFIRE = 12,
    CARTOGRAPHER = 13,
    SWAN_POND = 14,
    COVER_OF_DARKNESS = 15,
    CREATURE_BANK = 16,
    CREATURE_GENERATOR1 = 17,
    CREATURE_GENERATOR2 = 18,
    CREATURE_GENERATOR3 = 19,
    CREATURE_GENERATOR4 = 20,
    CURSED_GROUND1 = 21,
    CORPSE = 22,
    MARLETTO_TOWER = 23,
    DERELICT_SHIP = 24,
    DRAGON_UTOPIA = 25,
    EVENT = 26,
    EYE_OF_MAGI = 27,
    FAERIE_RING = 28,
    FLOTSAM = 29,
    FOUNTAIN_OF_FORTUNE = 30,
    FOUNTAIN_OF_YOUTH = 31,
    GARDEN_OF_REVELATION = 32,
    GARRISON = 33,
    HERO = 34,
    HILL_FORT = 35,
    GRAIL = 36,
    HUT_OF_MAGI = 37,
    IDOL_OF_FORTUNE = 38,
    LEAN_TO = 39,
    LIBRARY_OF_ENLIGHTENMENT = 41,
    LIGHTHOUSE = 42,
    MONOLITH1 = 43,
    MONOLITH2 = 44,
    MONOLITH3 = 45,
    MAGIC_PLAINS1 = 46,
    SCHOOL_OF_MAGIC = 47,
    MAGIC_SPRING = 48,
    MAGIC_WELL = 49,
    MERCENARY_CAMP = 51,
    MERMAID = 52,
    MINE = 53,
    MONSTER = 54,
    MYSTICAL_GARDEN = 55,
    OASIS = 56,
    OBELISK = 57,
    REDWOOD_OBSERVATORY = 58,
    OCEAN_BOTTLE = 59,
    PILLAR_OF_FIRE = 60,
    STAR_AXIS = 61,
    PRISON = 62,
    PYRAMID = 63,//subtype 0
    //WOG_OBJECT = 63,//subtype > 0
    RALLY_FLAG = 64,
    RANDOM_ART = 65,
    RANDOM_TREASURE_ART = 66,
    RANDOM_MINOR_ART = 67,
    RANDOM_MAJOR_ART = 68,
    RANDOM_RELIC_ART = 69,
    RANDOM_HERO = 70,
    RANDOM_MONSTER = 71,
    RANDOM_MONSTER_L1 = 72,
    RANDOM_MONSTER_L2 = 73,
    RANDOM_MONSTER_L3 = 74,
    RANDOM_MONSTER_L4 = 75,
    RANDOM_RESOURCE = 76,
    RANDOM_TOWN = 77,
    REFUGEE_CAMP = 78,
    RESOURCE = 79,
    SANCTUARY = 80,
    SCHOLAR = 81,
    SEA_CHEST = 82,
    SEER_HUT = 83,
    CRYPT = 84,
    SHIPWRECK = 85,
    SHIPWRECK_SURVIVOR = 86,
    SHIPYARD = 87,
    SHRINE_OF_MAGIC_INCANTATION = 88,
    SHRINE_OF_MAGIC_GESTURE = 89,
    SHRINE_OF_MAGIC_THOUGHT = 90,
    SIGN = 91,
    SIRENS = 92,
    SPELL_SCROLL = 93,
    STABLES = 94,
    TAVERN = 95,
    TEMPLE = 96,
    DEN_OF_THIEVES = 97,
    TOWN = 98,
    TRADING_POST = 99,
    LEARNING_STONE = 100,
    TREASURE_CHEST = 101,
    TREE_OF_KNOWLEDGE = 102,
    SUBTERRANEAN_GATE = 103,
    UNIVERSITY = 104,
    WAGON = 105,
    WAR_MACHINE_FACTORY = 106,
    SCHOOL_OF_WAR = 107,
    WARRIORS_TOMB = 108,
    WATER_WHEEL = 109,
    WATERING_HOLE = 110,
    WHIRLPOOL = 111,
    WINDMILL = 112,
    WITCH_HUT = 113,
    HOLE = 124,
    RANDOM_MONSTER_L5 = 162,
    RANDOM_MONSTER_L6 = 163,
    RANDOM_MONSTER_L7 = 164,
    BORDER_GATE = 212,
    FREELANCERS_GUILD = 213,
    HERO_PLACEHOLDER = 214,
    QUEST_GUARD = 215,
    RANDOM_DWELLING = 216,
    RANDOM_DWELLING_LVL = 217, //subtype = creature level
    RANDOM_DWELLING_FACTION = 218, //subtype = faction
    GARRISON2 = 219,
    ABANDONED_MINE = 220,
    TRADING_POST_SNOW = 221,
    CLOVER_FIELD = 222,
    CURSED_GROUND2 = 223,
    EVIL_FOG = 224,
    FAVORABLE_WINDS = 225,
    FIERY_FIELDS = 226,
    HOLY_GROUNDS = 227,
    LUCID_POOLS = 228,
    MAGIC_CLOUDS = 229,
    MAGIC_PLAINS2 = 230,
    ROCKLANDS = 231);

const
  WOG_OBJECT = TObj.PYRAMID;//subtype > 0

const

  MODID_CORE = 'core';

  ARTIFACT_METACLASS = 'artifact';
  ARTIFACT_QUANTITY = 144;

  CREATURE_METACLASS = 'creature';
  CREATURE_QUANTITY = 150;

  FACTION_METACLASS = 'faction';
  FACTION_QUANTITY = 9;

  HEROCLASS_METACLASS = 'heroClass';
  HEROCLASS_QUANTITY = 18;

  HERO_METACLASS = 'hero';
  HERO_QUANTITY = 156;

  SPELL_METACLASS = 'spell';
  SPELL_QUANTITY = 81;

  OBJECT_METACLASS = 'object';

  RESOURCE_NAMESPACE = 'resource';

  SKILL_QUANTITY = 28;
  SKILL_NAMESPACE = 'skill';

  SKILL_NAMES: array[0..SKILL_QUANTITY - 1] of string =
  (
    'pathfinding',  'archery',      'logistics',    'scouting',     'diplomacy',    //  5
    'navigation',   'leadership',   'wisdom',       'mysticism',    'luck',         // 10
    'ballistics',   'eagleEye',     'necromancy',   'estates',      'fireMagic',    // 15
    'airMagic',     'waterMagic',   'earthMagic',   'scholar',      'tactics',      // 20
    'artillery',    'learning',     'offence',      'armorer',      'intelligence', // 25
    'sorcery',      'resistance',   'firstAid'
  );

  SPELL_QUANTITY_ACTUAL = 70;

const
  MAX_HERO_LEVEL = 199; //VCMI technical limitation

  TILE_SIZE = 32; //in pixels

implementation

end.

