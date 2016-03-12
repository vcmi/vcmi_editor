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
unit logical_building_condition;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, logical_expression, vcmi_json;

type
  { TBuildingConditionItem }

  TBuildingConditionItem = class(TLogicalExpressionItem, ISerializeSpecial)
  private
    FBuilding: AnsiString;
    procedure SetBuilding(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  public
    property Building: AnsiString read FBuilding write SetBuilding;
  end;

  TBuildingCondition = class(TLogicalExpression)

  end;

implementation

uses
  typinfo;

{ TBuildingConditionItem }

procedure TBuildingConditionItem.SetBuilding(AValue: AnsiString);
begin
  if FBuilding=AValue then Exit;
  FBuilding:=AValue;
end;

constructor TBuildingConditionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TBuildingConditionItem.Destroy;
begin
  inherited Destroy;
end;

function TBuildingConditionItem.Serialize(AHandler: TVCMIJSONStreamer
  ): TJSONData;
var
  item: TCollectionItem;
begin
  Result := CreateJSONArray([]);

  if SubExpressions.Count > 0 then
  begin
     TJSONArray(Result).Add(GetEnumName( TypeInfo(TLogicalOperator), Integer(LogicalOperator)));

     for item in SubExpressions do
     begin
       TJSONArray(Result).Add((item as TBuildingConditionItem).Serialize(AHandler));
     end;
  end
  else
  begin
    TJSONArray(Result).Add(Building);
  end;

end;

procedure TBuildingConditionItem.Deserialize(AHandler: TVCMIJSONDestreamer;
  ASrc: TJSONData);
var
  ASrcArray: TJSONArray;
  instruction_name: TJSONStringType;
  raw_instruction: Integer;
  i: Integer;
  SubExpression: TBuildingConditionItem;
begin
  if ASrc.JSONType <> jtArray then
  begin
    raise Exception.Create('invalid format for building condition, array required');
  end;

  ASrcArray :=  TJSONArray(ASrc);

  instruction_name :=  ASrcArray.Strings[0];

  raw_instruction := GetEnumValue(TypeInfo(TLogicalOperator), instruction_name);

  if raw_instruction >=0 then
  begin
    LogicalOperator:=TLogicalOperator(raw_instruction);

    for i := 1 to ASrcArray.Count - 1 do
    begin
      SubExpression := TBuildingConditionItem(SubExpressions.Add);

      SubExpression.Deserialize(AHandler, ASrcArray.Items[i]);
    end;
  end
  else
  begin
    Building:=instruction_name;
  end;
end;

end.

