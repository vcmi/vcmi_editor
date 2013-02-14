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

unit editor_classes;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type

  { TNamedCollection
    Stored as object in JSON
    uses DisplayName as a name of field
  }

  TNamedCollection = class(TCollection)

  end;

  { TArrayCollection
    Stored as array in JSON
  }

  TArrayCollection = class(TCollection)

  end;

  { IProgressCallback }

  IProgressCallback = interface
    function GetMax: Integer;
    procedure SetMax(AValue: Integer);

    property Max: Integer read GetMax write SetMax;

    procedure Advance(ADelta: integer);
  end;


implementation

end.

