{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
  Classes, SysUtils, typinfo, undo_base, undo_map, Map, editor_types, objects,
  editor_str_consts, map_actions;

type
  TMapObjectBrush = class (TMapBrush)

  end;


  TObjectOwnershipTrait = (NoFree, FreeIfDone, FreeIfUndone);

  { TBaseObjectAction }

  TBaseObjectAction = class abstract(TMapUndoItem)
  strict protected
    procedure FreeTargets; virtual;
  public
    destructor Destroy; override;
    class function GetOwnershipTrait: TObjectOwnershipTrait; virtual;
  end;

  { TObjectAction }

  TObjectAction = class abstract (TBaseObjectAction)
  strict private
    FTargetObject: TMapObject;
    procedure SetTargetObject(AValue: TMapObject);
  strict protected
    procedure FreeTargets; override; final;
  public
    property TargetObject: TMapObject read FTargetObject write SetTargetObject;
  end;

  { TMultiObjectAction }

  TMultiObjectAction = class abstract(TBaseObjectAction)
  strict private
    FTargets: TMapObjectList;
  strict protected
    procedure FreeTargets; override; final;
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    property Targets: TMapObjectList read FTargets;
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
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property Template: TObjTemplate read FTemplate write SetTemplate;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
    property CurrentPlayer: TPlayer read FCurrentPlayer write SetCurrentPlayer;

    class function GetOwnershipTrait: TObjectOwnershipTrait; override; final;
  end;


  { TDeleteObject }
  TDeleteObject = class(TObjectAction)
  public
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;
    class function GetOwnershipTrait: TObjectOwnershipTrait; override; final;
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

{ TBaseObjectAction }

procedure TBaseObjectAction.FreeTargets;
begin

end;

destructor TBaseObjectAction.Destroy;
var
  ot: TObjectOwnershipTrait;
begin
  ot := GetOwnershipTrait();

  case ot of
    TObjectOwnershipTrait.FreeIfDone:
      if State = TUndoItemState.ReDone then
        FreeTargets;
    TObjectOwnershipTrait.FreeIfUndone:
      if State = TUndoItemState.UnDone then
        FreeTargets;
  end;

  inherited Destroy;
end;

class function TBaseObjectAction.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result := TObjectOwnershipTrait.NoFree;
end;

{ TMultiObjectAction }

procedure TMultiObjectAction.FreeTargets;
var
  o:TMapObject;
begin
  for o in FTargets do
    o.Free;
end;

constructor TMultiObjectAction.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FTargets := TMapObjectList.Create();
end;

destructor TMultiObjectAction.Destroy;
begin
  inherited Destroy;
  FTargets.Free; //used by inherited Destroy
end;

{ TObjectAction }

procedure TObjectAction.SetTargetObject(AValue: TMapObject);
begin
  if FTargetObject=AValue then Exit;
  FTargetObject:=AValue;
end;

procedure TObjectAction.FreeTargets;
begin
  FreeAndNil(FTargetObject);
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

procedure TAddObject.Execute;
begin
  TargetObject := FMap.Objects.Add;

  TargetObject.&Type:=Template.ObjType.Identifier;
  TargetObject.Subtype:=Template.ObjSubType.Identifier;

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
  Result := rsAddObjectDescription;
end;

procedure TAddObject.Redo;
begin
  TargetObject.Collection := FMap.Objects;
end;

procedure TAddObject.Undo;
begin
  TargetObject.Collection := nil;
end;

class function TAddObject.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.FreeIfUndone;
end;

{ TDeleteObject }

procedure TDeleteObject.Execute;
begin
  Redo;
end;

function TDeleteObject.GetDescription: string;
begin
  Result := rsDeleteObjectDescription;
end;

procedure TDeleteObject.Redo;
begin
  TargetObject.Collection := nil;
end;

procedure TDeleteObject.Undo;
begin
  TargetObject.Collection := FMap.Objects;
end;

class function TDeleteObject.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.FreeIfDone;
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
  Result := rsMoveObjectDescription;
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

