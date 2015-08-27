{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge,net

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

unit position;

{$I compilersetup.inc}

interface

uses
  SysUtils, fpjson, vcmi_json, editor_classes;

type
  { TPosition }

  TPosition = class(TObject, ISerializeSpecial)
  private
    FL: Integer;
    FX: Integer;
    FY: Integer;
    procedure SetL(AValue: Integer);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    constructor Create;
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);

    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property L: Integer read FL write SetL;

    function IsEmpty: Boolean;
  end;

implementation

{ TPosition }

procedure TPosition.SetL(AValue: Integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TPosition.SetX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TPosition.SetY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

constructor TPosition.Create;
begin
  FX := -1;
  FY := -1;
  FL := -1;
end;

function TPosition.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
begin
  result :=  CreateJSONArray([X,Y,L]);
end;

procedure TPosition.Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
var
  src_array: TJSONArray;
begin
  src_array := TJSONArray(ASrc);
  X := src_array.Integers[0];
  Y := src_array.Integers[1];
  L := src_array.Integers[2];
end;

function TPosition.IsEmpty: Boolean;
begin
  Result := (X>=0) or (Y>=0) or (L>=0);
end;

end.

