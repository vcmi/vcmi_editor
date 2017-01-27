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
unit logical_expression;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, editor_classes, fpjson, vcmi_json;

type

  TLogicalOperator = (nop, anyOf, allOf, noneOf);

type
  TLogicalExpressionItem = class;

  TEvaluate = function(AExpression: TLogicalExpressionItem) : boolean of object;

  TLogicalExpression = class;
  TLogicalExpressionClass = class of TLogicalExpression;

  { TLogicalExpressionItem }

  TLogicalExpressionItem = class (TCollectionItem)
  private
    FSubExpressions: TLogicalExpression;
    FLogicalOperator: TLogicalOperator;

    procedure SetLogicalOperator(AValue: TLogicalOperator);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    class function GetSubExpressionsClass():TLogicalExpressionClass; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property LogicalOperator: TLogicalOperator read FLogicalOperator write SetLogicalOperator;
    property SubExpressions:TLogicalExpression read FSubExpressions;

    function Evaluate(ACallback: TEvaluate): Boolean;
  end;

  TLogicalExpression = class (TCollection, IArrayCollection, ISerializeSpecial)
  private
    FIsRoot: Boolean;
  protected
    function IsRoot:Boolean;
  public
    constructor Create(AItemClass: TCollectionItemClass); virtual;
    constructor CreateRoot(AItemClass: TCollectionItemClass);

    procedure Assign(Source: TPersistent); override;

    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);

    function Evaluate(ACallback: TEvaluate): Boolean;
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

procedure TLogicalExpression.Assign(Source: TPersistent);
begin
  if Source is TLogicalExpression then
  begin
    FIsRoot := TLogicalExpression(Source).FIsRoot;
  end;

  inherited Assign(Source);
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

function TLogicalExpression.Evaluate(ACallback: TEvaluate): Boolean;
begin
  Result := False;
  if FIsRoot then
  begin
    if Count = 1 then
    begin
      Result := TLogicalExpressionItem(Items[0]).Evaluate(ACallback);
    end
    else if Count = 0 then
    begin
      Result := true
    end;
  end;
end;

{ TLogicalExpressionItem }

procedure TLogicalExpressionItem.SetLogicalOperator(AValue: TLogicalOperator);
begin
  FLogicalOperator:=AValue;
end;

procedure TLogicalExpressionItem.AssignTo(Dest: TPersistent);
var
  dest_typed:  TLogicalExpressionItem;
begin
  if Dest is TLogicalExpressionItem then
  begin
    dest_typed := TLogicalExpressionItem(Dest);
    dest_typed.LogicalOperator:=LogicalOperator;
    dest_typed.SubExpressions.Assign(SubExpressions);
  end
  else
  begin
     inherited AssignTo(Dest);
  end;
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

function TLogicalExpressionItem.Evaluate(ACallback: TEvaluate): Boolean;
var
  i: Integer;
begin
  case LogicalOperator of
    TLogicalOperator.nop: Result := ACallback(Self);
    TLogicalOperator.anyOf:
    begin
      for i := 0 to SubExpressions.Count - 1 do
      begin
        if TLogicalExpressionItem(SubExpressions.Items[i]).Evaluate(ACallback) then
          exit(true);
      end;
      Result := false;
    end;
    TLogicalOperator.allOf:
    begin
      for i := 0 to SubExpressions.Count - 1 do
      begin
        if not TLogicalExpressionItem(SubExpressions.Items[i]).Evaluate(ACallback) then
          exit(false);
      end;
      Result := True;
    end;
    TLogicalOperator.noneOf:
    begin
      for i := 0 to SubExpressions.Count - 1 do
      begin
        if TLogicalExpressionItem(SubExpressions.Items[i]).Evaluate(ACallback) then
          exit(false);
      end;
      Result := True;
    end;
  end;
end;

end.

