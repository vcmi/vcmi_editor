unit editor_str_consts;

{$mode objfpc}{$H+}

interface

const

  FORMAT_VCMI_EXT = '.JSON';
  FORMAT_H3M_EXT = '.H3M';
  FORMAT_ZIP_EXT = '.ZIP';
  FORMAT_VMAP_EXT = '.VMAP';

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

implementation

end.

