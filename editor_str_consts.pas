unit editor_str_consts;

{$mode objfpc}{$H+}

interface

const
  FORMAT_H3M_EXT = '.H3M';
  FORMAT_VCMI_EXT = '.VMAP';

resourcestring
  rsDefaultMapName = 'Untitled';

  rsConfirm  = 'Confirm';
  rsFileExists = 'File already exists. Replase?';

  rsMapChanged = 'Map has been changed. Save?';

  rsRedo = 'Redo';
  rsUndo = 'Undo';

  rsRoadTypeNone = 'Delete';
  rsRoadTypeDirt = 'Dirt';
  rsRoadTypeGrazvel= 'Grazvel';
  rsRoadTypeCobblestone = 'Cobblestone';

  rsRiverTypeNone = 'Delete';
  rsRiverTypeClear = 'Water';
  rsRiverTypeIcy = 'Ice';
  rsRiverTypeMuddy = 'Muddy';
  rsRiverTypeLava = 'Lava';

  rsEditTerrainDescription = 'Edit terrain';
  rsEditRoadDescription = 'Edit roads';
  rsEditRiverDescription = 'Edit rivers';

  rsEmpty = '(Empty)';
  rsNone  = '(None)';

  rsArmy = 'Army';
  rsGuards = 'Guards';

  rsSlotHead = 'Head';
  rsSlotShoulders = 'Shoulders';
  rsSlotNeck = 'Neck';
  rsSlotRightHand = 'RightHand';
  rsSlotLeftHand = 'LeftHand';
  rsSlotTorso = 'Torso';
  rsSlotRightRing = 'RightRing';
  rsSlotLeftRing = 'LeftRing';

  rsSlotFeet = 'Feet';
  rsSlotMisc1 = 'Misc1';
  rsSlotMisc2 = 'Misc2';
  rsSlotMisc3 = 'Misc3';
  rsSlotMisc4 = 'Misc4';
  rsSlotMisc5 = 'Misc5';
  rsSlotMach1 = 'Mach1';
  rsSlotMach2 = 'Mach2';

  rsSlotMach3 = 'Mach3';
  rsSlotMach4 = 'Mach4';
  rsSlotSpellbook = 'Spellbook';

  rsSlotBackpack = 'Backpack';

  rsRandomCreatureName = 'Creature of level %d';
  rsRandomCreatureNameUpgrade = 'Upgraded creature of level %d';

implementation

end.

