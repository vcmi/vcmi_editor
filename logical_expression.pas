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
  Classes, SysUtils, editor_classes;

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

  TLogicalExpression = class (TCollection, IArrayCollection)
  public
    constructor Create(AItemClass: TCollectionItemClass); virtual;
  end;

implementation

{ TLogicalExpression }

constructor TLogicalExpression.Create(AItemClass: TCollectionItemClass);
begin
  Inherited Create(AItemClass);
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

