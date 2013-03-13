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

  { TGCollection }

  generic TGCollection <TItem> = class (TCollection)
  private
    function GetItems(const Idx: Integer): TItem;
    procedure SetItems(const Idx: Integer; AValue: TItem);
  public
    type
      TItemType = Titem;
    constructor Create;

    function Add: TItem;

    property Items[const Idx: Integer]: TItem read GetItems write SetItems; default;
  end;

  { INamedCollection
    Stored as object in JSON
    uses DisplayName as a name of field }

  INamedCollection = interface ['{3C14D8A9-3BAB-46D1-9A77-6216869F58D9}']

  end;

  { IArrayCollection
    Stored as array in JSON  }

  IArrayCollection = interface ['{8CD3BB79-5DD4-44D2-BE35-F179848262A0}']

  end;

  { TGArrayCollection }

  generic TGArrayCollection <TItem> = class (specialize TGCollection <TItem>, IArrayCollection)

  end;

  { TGNamedCollection }

  generic TGNamedCollection <TItem> = class (specialize TGCollection <TItem>, INamedCollection)

  end;

  { IProgressCallback }

  IProgressCallback = interface
    function GetMax: Integer;
    procedure SetMax(AValue: Integer);

    property Max: Integer read GetMax write SetMax;

    procedure Advance(ADelta: integer);
  end;


implementation

{ TGCollection }

function TGCollection.Add: TItem;
begin
  Result := TItem(inherited Add);
end;

constructor TGCollection.Create;
begin
  inherited Create(TItem);
end;

function TGCollection.GetItems(const Idx: Integer): TItem;
begin
  Result := TItem( inherited Items[Idx]);
end;

procedure TGCollection.SetItems(const Idx: Integer; AValue: TItem);
begin
  inherited Items[Idx] := AValue;
end;


end.

