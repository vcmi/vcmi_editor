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
  Classes, SysUtils;

type
{$push}
{$packenum 1}
  TTerrainType  = (dirt=0, sand, grass, snow, swamp, rough, sub, lava, water, rock{,border=$FF});
  TRiverType = (noRiver=0, clearRiver, icyRiver, muddyRiver, lavaRiver);
  TRoadType = (noRoad = 0, dirtRoad=1, grazvelRoad, cobblestoneRoad);

  TPlayerColor = (RED=0, BLUE, TAN, GREEN, ORANGE, PURPLE, TEAL, PINK,none=$FF);
{$pop}

  TDefFrame = UInt8;


implementation

end.

