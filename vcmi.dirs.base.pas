{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit vcmi.dirs.base;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils;

type
  TDirs = class;
  TDirsClass = class of TDirs;

  { TDirs }

  TDirs = class
  private

  protected
    function GetUserProfilePath: AnsiString; virtual;
    function GetUserCachePath: AnsiString; virtual;
    function GetUserDataPath: AnsiString; virtual;
  public
    constructor Create; virtual;

    property UserCachePath: AnsiString read GetUserCachePath;
    property UserDataPath: AnsiString read GetUserDataPath;

    class function GetActualClass: TDirsClass;
  end;

implementation

{$PUSH}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

{$IF DEFINED (MSWINDOWS)}
  uses vcmi.dirs.windows;
  type
    TDirsActual = TDirsWindows;
{$ELSEIF DEFINED (LINUX) OR DEFINED(BSD) AND NOT DEFINED(DARWIN)}
  uses vcmi.dirs.xdg;

  type
    TDirsActual = TDirsXDG;
{$ELSE}
  {$WARNING OS target not supported}
  type
    TDirsActual = TDirs;
{$ENDIF}
{$POP}


{ TDirs }

function TDirs.GetUserDataPath: AnsiString;
begin
  Result := GetUserProfilePath();
end;

function TDirs.GetUserProfilePath: AnsiString;
begin
  Result := ExtractFilePath(ParamStr(0)); //fallback to executable path
end;

function TDirs.GetUserCachePath: AnsiString;
begin
  Result := GetUserProfilePath();
end;

constructor TDirs.Create;
begin

end;

class function TDirs.GetActualClass: TDirsClass;
begin
  Result := TDirsActual;
end;

end.

