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

unit vcmi.dirs.base;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils, LazUTF8Classes;

type
  TDirs = class;
  TDirsClass = class of TDirs;

  { TDirs }

  TDirs = class
  private
    FExecutablePath: AnsiString;
    FPathsConfig: TStringListUTF8;
  protected
    function GetUserProfilePath: AnsiString; virtual;
    function GetUserCachePath: AnsiString; virtual;
    function GetUserConfigPath: AnsiString; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property UserCachePath: AnsiString read GetUserCachePath;
    property UserConfigPath: AnsiString read GetUserConfigPath;

    procedure FillDataPaths(AList: TStringListUTF8); virtual;

    procedure CreatePaths;

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

const
  GAME_PATH_CONFIG = 'gamepath.txt';


{ TDirs }

function TDirs.GetUserConfigPath: AnsiString;
begin
  Result := GetUserProfilePath();
end;

function TDirs.GetUserProfilePath: AnsiString;
begin
  Result := FExecutablePath; //fallback to executable path
end;

function TDirs.GetUserCachePath: AnsiString;
begin
  Result := GetUserProfilePath();
end;

constructor TDirs.Create;
var
  s: AnsiString;
begin
  FPathsConfig := TStringListUTF8.Create;
  FExecutablePath := ExtractFilePath(ParamStrUTF8(0));

  s := FExecutablePath + GAME_PATH_CONFIG;

  if FileExistsUTF8(s) then
  begin
    FPathsConfig.LoadFromFile(s);
  end;

  FPathsConfig.Insert(0, FExecutablePath);
end;

destructor TDirs.Destroy;
begin
  FPathsConfig.Free;
  inherited Destroy;
end;

procedure TDirs.FillDataPaths(AList: TStringListUTF8);
begin
  AList.AddStrings(FPathsConfig);

  AList.Add(GetUserProfilePath());
end;

procedure TDirs.CreatePaths;
begin
  if not ForceDirectoriesUTF8(UserCachePath) then
  begin
    raise Exception.CreateFmt('Unable to create path %s',[UserCachePath]);
  end;
end;

class function TDirs.GetActualClass: TDirsClass;
begin
  Result := TDirsActual;
end;

end.

