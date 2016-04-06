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
  Classes, SysUtils, typinfo, undo_base, undo_map, Map, editor_types, map_objects, editor_str_consts, map_actions,
  editor_gl, editor_consts, map_rect, vcmi_json, edit_object_options, vcmi_fpjsonrtti, gset, fpjson;

type
  { TMapObjectBrush }

  TMapObjectBrush = class (TMapBrush)
  strict private
    FStartCoord: TMapCoord;
    FEndCooord: TMapCoord;

    FSelectedObjects: TMapObjectSet;
    FVisibleObjects: TMapObjectList;
  protected
    procedure AddTile(AMap: TVCMIMap;AX,AY: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap;X,Y: integer); override;
    procedure RenderSelection(State: TLocalState); override;

    procedure TileMouseDown(AMap: TVCMIMap; X, Y: integer); override;

    property VisibleObjects: TMapObjectList read FVisibleObjects write FVisibleObjects;
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
    function GetChangedRegion(ALevelIndex: integer): TMapRect; override;
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
    FTemplate: TMapObjectTemplate;
    FX: Integer;
    FY: Integer;

    procedure SetCurrentPlayer(AValue: TPlayer);
    procedure SetL(AValue: Integer);
    procedure SetTemplate(AValue: TMapObjectTemplate);
    procedure SetX(AValue: Integer);
    procedure SetY(AValue: Integer);
  public
    procedure Execute; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property Template: TMapObjectTemplate read FTemplate write SetTemplate;

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

  { TEditObject }

  TEditObject = class(TObjectAction)
  private
    FOldOptions, FNewOptions: TJSONData;
    FStreamer: TVCMIJSONStreamer;
    FDestreamer: TVCMIJSONDestreamer;
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Execute; override;
    procedure Redo; override;
    procedure Undo; override;

    function GetDescription: string; override;
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

    function GetChangedRegion(ALevelIndex: integer): TMapRect; override;

    property X:Integer read FX write SetX;
    property Y:Integer read FY write SetY;
    property L:Integer read FL write SetL;
  end;

implementation

{ TEditObject }

constructor TEditObject.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FStreamer := TVCMIJSONStreamer.Create(nil);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
end;

destructor TEditObject.Destroy;
begin
  FStreamer.Free;
  FDestreamer.Free;
  FreeAndNil(FNewOptions);
  FreeAndNil(FOldOptions);
  inherited Destroy;
end;

procedure TEditObject.Execute;
var
  edit_form: TEditObjectOptions;
begin
  edit_form := TEditObjectOptions.Create(nil);
  try
    FOldOptions := FStreamer.ObjectToJsonEx(TargetObject.Options);
    edit_form.EditObject(TargetObject);
    FNewOptions := FStreamer.ObjectToJsonEx(TargetObject.Options);
  finally
    edit_form.Free;
  end;
end;

procedure TEditObject.Redo;
begin
  TargetObject.Options.Clear;
  FDestreamer.JSONToObjectEx(FNewOptions, TargetObject.Options);
end;

procedure TEditObject.Undo;
begin
  TargetObject.Options.Clear;
  FDestreamer.JSONToObjectEx(FOldOptions, TargetObject.Options);
end;

function TEditObject.GetDescription: string;
begin
  Result := rsEditObjectDescription;
end;

class function TEditObject.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.NoFree;
end;

{ TMapObjectBrush }

procedure TMapObjectBrush.AddTile(AMap: TVCMIMap; AX, AY: integer);
begin
  if Assigned(FVisibleObjects) then
  begin
    AMap.SelectObjectsOnTile(FVisibleObjects, AMap.CurrentLevelIndex, Ax, AY, FSelectedObjects);
  end;
  FEndCooord.Reset(AX,AY);
end;

constructor TMapObjectBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedObjects := TMapObjectSet.Create;
end;

destructor TMapObjectBrush.Destroy;
begin
  FSelectedObjects.Free;
  inherited Destroy;
end;

procedure TMapObjectBrush.Clear;
begin
  inherited Clear;
  FreeAndNil(FSelectedObjects);
  FSelectedObjects := TMapObjectSet.Create;
end;

procedure TMapObjectBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin
  //do nothing
end;

procedure TMapObjectBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin
  //do nothing, default sysytem cursor is enough
end;

procedure TMapObjectBrush.RenderSelection(State: TLocalState);
var
  it: TMapObjectSet.TIterator;

  cx,cy: Integer;
  r:TMapRect;
begin

  it := FSelectedObjects.Min;

  if Assigned(it) then
  begin
    repeat
      it.Data.RenderSelectionRect(State);

    until not it.Next;

    FreeAndNil(it);
  end;


  if Dragging then
  begin
    State.StartDrawingRects;
    r.SetFromCorners(FStartCoord,FEndCooord);

    cx := r.FTopLeft.X * TILE_SIZE;
    cy := r.FTopLeft.Y * TILE_SIZE;
    State.SetFragmentColor(RECT_COLOR);
    State.RenderRect(cx,cy,r.FWidth * TILE_SIZE ,r.FHeight * TILE_SIZE);
    State.StopDrawing;
  end;
end;

procedure TMapObjectBrush.TileMouseDown(AMap: TVCMIMap; X, Y: integer);
begin
  inherited TileMouseDown(AMap, X, Y);
  FStartCoord.Reset(X,Y);
end;

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

function TObjectAction.GetChangedRegion(ALevelIndex: integer): TMapRect;
begin
  if ALevelIndex = TargetObject.L then
  begin
    Result := TargetObject.GetRegion;
  end
  else
  begin
    Result.Create();
  end;
end;

{ TAddObject }

procedure TAddObject.SetTemplate(AValue: TMapObjectTemplate);
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
  TargetObject := TMapObject.Create(nil);
  TargetObject.AssignTemplate(Template);

  TargetObject.L := l;
  TargetObject.X := X;
  TargetObject.Y := Y;

  TargetObject.Collection := FMap.Objects; //add object with valid configuration

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
  TargetObject.Collection := nil;
  TargetObject.L:=L;
  TargetObject.X:=X;
  TargetObject.Y:=Y;
  TargetObject.Collection := FMap.Objects;
end;

procedure TMoveObject.Undo;
begin
  TargetObject.Collection := nil;
  TargetObject.L:=FOldL;
  TargetObject.X:=FOldX;
  TargetObject.Y:=FOldY;
  TargetObject.Collection := FMap.Objects;
end;

function TMoveObject.GetChangedRegion(ALevelIndex: integer): TMapRect;
begin
  if (ALevelIndex = FL) and (FOldL = FL) then
  begin
    Result := TargetObject.GetRegion(FOldX, FOldY);
    Result.CombineWith(TargetObject.GetRegion(FX, FY));
  end
  else if ALevelIndex = FOldL then
  begin
    Result := TargetObject.GetRegion(FOldX, FOldY);
  end
  else if ALevelIndex = FL then
  begin
    Result := TargetObject.GetRegion(FX, FY);
  end
  else
    Result.Create();
end;


end.

