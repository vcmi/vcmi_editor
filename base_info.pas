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
unit base_info;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, editor_types;

type
  {$push}
  {$m+}

  { TBaseInfo }

  TBaseInfo = class abstract
  private
    FID: AnsiString;
    FName: TLocalizedString;
    FIndex: TCustomID;

    procedure SetID(AValue: AnsiString);

    procedure SetIndex(AValue: TCustomID);
  protected
    function GetFullID: AnsiString; virtual;

    function GetName: TLocalizedString; virtual;
    procedure SetName(AValue: TLocalizedString); virtual;
  public
    constructor Create;
    property ID: AnsiString read FID write SetID;

    property FullID: AnsiString read GetFullID;
  published
    property Name: TLocalizedString read GetName write SetName;
    property Index: TCustomID read FIndex write SetIndex default ID_INVALID;
  end;

  { TMapObjectInfo }

  TMapObjectInfo = class abstract (TBaseInfo)
  private
    FMapObject: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MapObject: TJSONObject read FMapObject;
  end;

  {$pop}

implementation

{ TMapObjectInfo }

constructor TMapObjectInfo.Create;
begin
  inherited;
  FMapObject := TJSONObject.Create;
end;

destructor TMapObjectInfo.Destroy;
begin
  FMapObject.Free;
  inherited Destroy;
end;

{ TBaseInfo }

constructor TBaseInfo.Create;
begin
  FIndex := ID_INVALID;
end;

function TBaseInfo.GetFullID: AnsiString;
begin
  Result := ID;
end;

procedure TBaseInfo.SetID(AValue: AnsiString);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

function TBaseInfo.GetName: TLocalizedString;
begin
  Result := FName;
end;

procedure TBaseInfo.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TBaseInfo.SetIndex(AValue: TCustomID);
begin
  if FIndex = AValue then Exit;
  FIndex := AValue;
end;

end.

