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
unit logical_expression;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, editor_classes, fpjson, vcmi_json;

type

  TLogicalOperator = (nop, anyOf, allOf, noneOf);

type

  TLogicalExpression = class;
  TLogicalExpressionClass = class of TLogicalExpression;

  { TLogicalExpressionItem }

  TLogicalExpressionItem = class (TCollectionItem)
  private
    FSubExpressions: TLogicalExpression;
    FLogicalOperator: TLogicalOperator;

    procedure SetLogicalOperator(AValue: TLogicalOperator);
  protected
    class function GetSubExpressionsClass():TLogicalExpressionClass; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property LogicalOperator: TLogicalOperator read FLogicalOperator write SetLogicalOperator;
    property SubExpressions:TLogicalExpression read FSubExpressions;
  end;

  TLogicalExpression = class (TCollection, IArrayCollection, ISerializeSpecial)
  private
    FIsRoot: Boolean;
  protected
    function IsRoot:Boolean;
  public
    constructor Create(AItemClass: TCollectionItemClass); virtual;
    constructor CreateRoot(AItemClass: TCollectionItemClass);

    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  end;

implementation

{ TLogicalExpression }

function TLogicalExpression.IsRoot: Boolean;
begin
  Result := FIsRoot;
end;

constructor TLogicalExpression.Create(AItemClass: TCollectionItemClass);
begin
  Inherited Create(AItemClass);
  FIsRoot:=false;
end;

constructor TLogicalExpression.CreateRoot(AItemClass: TCollectionItemClass);
begin
  Create(AItemClass);
  FIsRoot:=true;
end;

function TLogicalExpression.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
begin
  if IsRoot then
  begin
    if Count = 1 then
    begin
      Result := AHandler.ObjectToJsonEx(Items[0]);
    end
    else begin
      Result := CreateJSONArray([]);
    end;
  end
  else begin
    Result := AHandler.DoStreamCollection(Self); //use default
  end;
end;

procedure TLogicalExpression.Deserialize(AHandler: TVCMIJSONDestreamer;
  ASrc: TJSONData);
var
  Item: TCollectionItem;
begin
  if IsRoot then
  begin
    If ASrc.JSONType = TJSONtype.jtArray then
    begin
      Item := Add;
      AHandler.JSONToObjectEx(ASrc, Item);
    end;
  end
  else begin
    AHandler.DoDeStreamCollection(ASrc,Self); //use default
  end;
end;

{ TLogicalExpressionItem }

procedure TLogicalExpressionItem.SetLogicalOperator(AValue: TLogicalOperator);
begin
  FLogicalOperator:=AValue;
end;

class function TLogicalExpressionItem.GetSubExpressionsClass: TLogicalExpressionClass;
begin
  Result := TLogicalExpression;
end;

constructor TLogicalExpressionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSubExpressions := GetSubExpressionsClass().Create(TCollectionItemClass(ClassType));
end;

destructor TLogicalExpressionItem.Destroy;
begin
  FSubExpressions.Free;
  inherited Destroy;
end;

end.

