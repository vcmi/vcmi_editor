{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit vcmi.dirs.windows platform;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, LazUTF8, vcmi.dirs.base;

type

  { TDirsWindows }

  TDirsWindows = class(TDirs)
  protected
    function GetUserProfilePath: AnsiString; override;
  public
    constructor Create; override;
  end platform;


implementation

uses windirs;

{ TDirsWindows }

function TDirsWindows.GetUserProfilePath: AnsiString;
begin
  result := GetWindowsSpecialDir(CSIDL_PERSONAL);

  if result = '' then
  begin
    result:= GetEnvironmentVariableUTF8('HOMEDRIVE') + GetEnvironmentVariableUTF8('HOMEPATH');

    result := IncludeTrailingPathDelimiter(result);

    result := result + 'Documents\My Games\vcmi\';
  end
  else
  begin
    Result := Result+'My Games\vcmi\';
  end;
end;

constructor TDirsWindows.Create;
begin
  inherited Create;
end;

end.

