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

  { TLogicalExpressionItem }

  TLogicalExpressionItem = class (TCollectionItem)
  private
    FSubExpressions: TLogicalExpression;
    FLogicalOperator: TLogicalOperator;

    procedure SetLogicalOperator(AValue: TLogicalOperator);
  public
    property LogicalOperator: TLogicalOperator read FLogicalOperator write SetLogicalOperator;
    property SubExpressions:TLogicalExpression read FSubExpressions;
  end;

  TLogicalExpression = class (TCollection, IArrayCollection)
  public

  end;

implementation

{ TLogicalExpressionItem }

procedure TLogicalExpressionItem.SetLogicalOperator(AValue: TLogicalOperator);
begin
  FLogicalOperator:=AValue;
end;

end.

