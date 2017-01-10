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

unit transitions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, gvector;
const


  FLIP_PATTERN_HORIZONTAL = 1;
  FLIP_PATTERN_VERTICAL = 2;
  FLIP_PATTERN_BOTH = 3;

type

  TMapping = record
    Lower, Upper: Integer;
  end;

  TMappings = specialize TVector<TMapping>;

  TValidationResult = record
    result: Boolean;
    transitionReplacement: string;
    flip: Integer;
  end;

implementation

end.

