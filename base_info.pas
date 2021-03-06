{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit base_info;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, editor_types, editor_classes;

type
  {$push}
  {$m+}

  { TBaseInfo }

  TBaseInfo = class abstract(TNamedCollectionItem)
  private
    FName: TLocalizedString;
    FIndex: TCustomID;

    function GetCollectionIndex: integer;
    procedure SetCollectionIndex(AValue: integer);

    procedure SetIndex_(AValue: TCustomID);
  protected
    function GetName: TLocalizedString; virtual;
    procedure SetName(const AValue: TLocalizedString); virtual;
  public
    constructor Create(ACollection: TCollection); override;

    property CollectionIndex: integer read GetCollectionIndex write SetCollectionIndex;

    function IsValid: Boolean; virtual;
  published
    property Name: TLocalizedString read GetName write SetName;
    property Index: TCustomID read FIndex write SetIndex_ default ID_INVALID;
  end;

  { TMapObjectInfo }

  TMapObjectInfo = class abstract (TBaseInfo)
  private
    FMapObject: TJSONObject;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property MapObject: TJSONObject read FMapObject;
  end;

  {$pop}

implementation

{ TMapObjectInfo }

constructor TMapObjectInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMapObject := CreateJSONObject([]);
end;

destructor TMapObjectInfo.Destroy;
begin
  FMapObject.Free;
  inherited Destroy;
end;

{ TBaseInfo }

constructor TBaseInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIndex := ID_INVALID;
end;

function TBaseInfo.IsValid: Boolean;
begin
  Result := true;
end;

function TBaseInfo.GetCollectionIndex: integer;
begin
  Result := inherited Index;
end;

procedure TBaseInfo.SetCollectionIndex(AValue: integer);
begin
  inherited Index := AValue;
end;

function TBaseInfo.GetName: TLocalizedString;
begin
  if FName = '' then
  begin
    Result := Identifier;
  end
  else
  begin
    Result := FName;
  end;
end;

procedure TBaseInfo.SetName(const AValue: TLocalizedString);
begin
  FName := AValue;
end;

procedure TBaseInfo.SetIndex_(AValue: TCustomID);
begin
  if FIndex = AValue then Exit;
  FIndex := AValue;
end;

end.

