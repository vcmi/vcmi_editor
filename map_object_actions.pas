{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013,2014 Alexander Shishkin alexvins@users.sourceforge,net

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

unit map_object_actions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, typinfo, undo_base, undo_map, Map, editor_types, objects;

type

   { TObjectAction }

  TObjectAction = class(TMapUndoItem)
  private
    FTargetObject: TMapObject;
    procedure SetTargetObject(AValue: TMapObject);
  protected

  public
    property  TargetObject: TMapObject read FTargetObject write SetTargetObject;
  end;

  { TAddObject }

  TAddObject = class(TObjectAction)
  private
    FCurrentPlayer: TPlayer;
    FL: Integer;
    FTemplate: TObjTemplate;
    FX: Integer;
    FY: Integer;

    procedure SetCurrentPlayer(AValue: TPlayer);
    procedure SetL(AValue: Integer);
    procedure SetTemplate(AValue: TObjTemplate);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    destructor Destroy; override;
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property  Template: TObjTemplate read FTemplate write SetTemplate;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
    property CurrentPlayer: TPlayer read FCurrentPlayer write SetCurrentPlayer;

  end;


  { TDeleteObject }
  //todo: cleaup unused map templates
  TDeleteObject = class(TObjectAction)
  public
    destructor Destroy; override;
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TMoveObject }

  TMoveObject = class(TObjectAction)
  private
    FOldX, FOldY,FOldL: Integer;
    FL: Integer;
    FX: Integer;
    FY: Integer;
    procedure SetL(AValue: Integer);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
  end;

implementation

{ TObjectAction }

procedure TObjectAction.SetTargetObject(AValue: TMapObject);
begin
  if FTargetObject=AValue then Exit;
  FTargetObject:=AValue;
end;

{ TAddObject }

procedure TAddObject.SetTemplate(AValue: TObjTemplate);
begin
  if FTemplate=AValue then Exit;
  FTemplate:=AValue;
end;

procedure TAddObject.SetL(AValue: Integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TAddObject.SetCurrentPlayer(AValue: TPlayer);
begin
  if FCurrentPlayer=AValue then Exit;
  FCurrentPlayer:=AValue;
end;

procedure TAddObject.SetX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TAddObject.SetY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

destructor TAddObject.Destroy;
begin
  if State = TUndoItemState.UnDone then
  begin
    FreeAndNil(FTargetObject);
  end;
  inherited Destroy;
end;

procedure TAddObject.Execute;
begin
  TargetObject := FMap.Objects.Add;

  TargetObject.&Type:=Template.ObjType.DisplayName;
  TargetObject.Subtype:=Template.ObjSubType.DisplayName;

  TargetObject.Template.Assign(Template);

  TargetObject.L := l;
  TargetObject.X := X;
  TargetObject.Y := Y;

  if IsPublishedProp(TargetObject.Options, 'Owner') then
  begin
    TargetObject.Options.Owner := CurrentPlayer;
  end;
  //(!)do not redo here
end;

function TAddObject.GetDescription: string;
begin
  Result := 'Add object';
end;

procedure TAddObject.Redo;
begin
  TargetObject.Collection := FMap.Objects;
end;

procedure TAddObject.Undo;
begin
  TargetObject.Collection := nil;
end;

{ TDeleteObject }

destructor TDeleteObject.Destroy;
begin
  if State = TUndoItemState.ReDone then
  begin
    FreeAndNil(FTargetObject);
  end;
  inherited Destroy;
end;

procedure TDeleteObject.Execute;
begin
  Redo;
end;

function TDeleteObject.GetDescription: string;
begin
  Result := 'Delete object';
end;

procedure TDeleteObject.Redo;
begin
  FTargetObject.Collection := nil;
end;

procedure TDeleteObject.Undo;
begin
  FTargetObject.Collection := FMap.Objects;
end;

{ TMoveObject }

procedure TMoveObject.SetL(AValue: Integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TMoveObject.SetX(AValue: Integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TMoveObject.SetY(AValue: Integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

procedure TMoveObject.Execute;
begin
  FOldL:=TargetObject.L;
  FOldX:=TargetObject.X;
  FOldY:=TargetObject.Y;
  Redo;
end;

function TMoveObject.GetDescription: string;
begin
  Result := 'Move object';
end;

procedure TMoveObject.Redo;
begin
  TargetObject.L:=L;
  TargetObject.X:=X;
  TargetObject.Y:=Y;
end;

procedure TMoveObject.Undo;
begin
  TargetObject.L:=FOldL;
  TargetObject.X:=FOldX;
  TargetObject.Y:=FOldY;
end;


end.

