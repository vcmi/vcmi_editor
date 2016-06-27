{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit editor_rtti;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo;


function IsDefaultValue(AObject: TObject; APropName: String): Boolean;

function IsDefaultValue(AObject: TObject; PropertyInfo: PPropInfo): Boolean;


implementation

function IsDefaultValue(AObject: TObject; APropName: String): Boolean;
begin
  Result := IsDefaultValue(AObject, GetPropInfo(AObject,APropName));
end;

function IsDefaultValue(AObject: TObject; PropertyInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;
  Value: Int64;
  DefValue: Int64;
  SValue: String;
begin
  PropType := PropertyInfo^.PropType;

  case PropType^.Kind of
    tkInteger, tkInt64, tkChar, tkEnumeration, tkWChar,tkSet, tkBool, tkQWord: begin
      Value := GetOrdProp(AObject, PropertyInfo);
      DefValue := PropertyInfo^.Default;

      if (Value = DefValue) and (DefValue<>longint($80000000)) then
      begin
        Exit(True);
      end;
    end;
    tkString, tkAString:begin
      SValue := GetStrProp(AObject, PropertyInfo);
      if SValue = '' then
      begin
        Exit(True);
      end;
    end;
  end;

  Result := False;
end;

end.

