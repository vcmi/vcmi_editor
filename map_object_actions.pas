{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_object_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, LCLType, Controls, Forms, typinfo, undo_base, undo_map, Map, editor_types, map_objects, editor_str_consts, map_actions,
  editor_gl, editor_consts, map_rect, vcmi_json, edit_object_options, vcmi_fpjsonrtti, gset, fpjson;

type
  { TObjectSelectBrush }

  TObjectSelectBrush = class (TMapBrush)
  strict private
    FSelectedObjects: TMapObjectSet;
    FVisibleObjects: TMapObjectsSelection;
  protected
    procedure CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure ClearSelection; override;

    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap;X,Y: integer); override;
    procedure RenderSelection(State: TLocalState); override;

    property VisibleObjects: TMapObjectsSelection read FVisibleObjects write FVisibleObjects;

    procedure MouseLeave(AMap: TVCMIMap); override;
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
  strict protected
    procedure FreeTargets; override; final;
  public
    function GetChangedRegion(ALevelIndex: integer): TMapRect; override;
    property TargetObject: TMapObject read FTargetObject write FTargetObject;
  end;

  { TObjectPositionAction }

  TObjectPositionAction = class abstract(TObjectAction)
  private
    FL: Integer;
    FX: Integer;
    FY: Integer;
  protected
    procedure SetObjectPosition;
  public
    property X:Integer read FX write FX;
    property Y:Integer read FY write FY;
    property L:Integer read FL write FL;
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

  TAddObject = class(TObjectPositionAction)
  private
    FCurrentPlayer: TPlayer;
    FTemplate: TMapObjectTemplate;

    procedure SetCurrentPlayer(AValue: TPlayer);
    procedure SetTemplate(AValue: TMapObjectTemplate);
  public
    function Execute: boolean; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    property Template: TMapObjectTemplate read FTemplate write SetTemplate;

    property CurrentPlayer: TPlayer read FCurrentPlayer write SetCurrentPlayer;

    class function GetOwnershipTrait: TObjectOwnershipTrait; override; final;
  end;

  { TCopyObject }

  TCopyObject = class(TObjectPositionAction)
  private
    FSource: TMapObject;
  public
    function Execute: boolean; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    class function GetOwnershipTrait: TObjectOwnershipTrait; override; final;

    property Source: TMapObject read FSource write FSource;
    property TargetObject; //stores destination, do not edit
  end;

  { TDeleteObjects }

  TDeleteObjects = class(TMultiObjectAction)
  public
    function Execute: boolean; override;
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

    function Execute: boolean; override;
    procedure Redo; override;
    procedure Undo; override;

    function GetDescription: string; override;
    class function GetOwnershipTrait: TObjectOwnershipTrait; override; final;
  end;

  { TMoveObject }

  TMoveObject = class(TObjectPositionAction)
  private
    FOldX, FOldY, FOldL: Integer;
  public
    function Execute: boolean; override;
    function GetDescription: string; override;
    procedure Redo; override;
    procedure Undo; override;

    function GetChangedRegion(ALevelIndex: integer): TMapRect; override;
  end;

implementation

{ TDeleteObjects }

function TDeleteObjects.Execute: boolean;
begin
  Result := true;
  Redo;
end;

function TDeleteObjects.GetDescription: string;
begin
  Result := rsDeleteObjectsDescription;
end;

procedure TDeleteObjects.Redo;
var
  o:TMapObject;
begin
  for o in Targets do
    o.Collection := nil;
end;

procedure TDeleteObjects.Undo;
var
  o:TMapObject;
begin
  for o in Targets do
    o.Collection := FMap.Objects;
end;

class function TDeleteObjects.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.FreeIfDone;
end;

{ TObjectPositionAction }

procedure TObjectPositionAction.SetObjectPosition;
begin
  TargetObject.L := l;
  TargetObject.X := X;
  TargetObject.Y := Y;
end;

{ TCopyObject }

function TCopyObject.Execute: boolean;
var
  FStreamer: TVCMIJSONStreamer;
  FDestreamer: TVCMIJSONDestreamer;
  FBuffer: TJSONData;
begin
  FStreamer := TVCMIJSONStreamer.Create(nil);
  FDestreamer := TVCMIJSONDestreamer.Create(nil);
  FBuffer := nil;
  TargetObject := TMapObject.CreateIndep(FMap);
  try
    FBuffer := FStreamer.ObjectToJsonEx(Source);
    FDestreamer.JSONToObjectEx(FBuffer, TargetObject);
    TargetObject.Identifier := '';
    SetObjectPosition;
    Result := true;
    Redo;
  finally
    FStreamer.Free;
    FDestreamer.Free;
    FreeAndNil(FBuffer);
  end;
end;

function TCopyObject.GetDescription: string;
begin
  Result := rsCopyObjectDescription;
end;

procedure TCopyObject.Redo;
begin
  TargetObject.Collection := FMap.Objects;
end;

procedure TCopyObject.Undo;
begin
  TargetObject.Collection := nil;
end;

class function TCopyObject.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.FreeIfUndone;
end;

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

function TEditObject.Execute: boolean;
var
  edit_form: TEditObjectOptions;
begin
  edit_form := TEditObjectOptions.Create(nil);
  try
    FOldOptions := FStreamer.ObjectToJsonEx(TargetObject.Options);
    Result := edit_form.EditObject(TargetObject) = mrok;
    FNewOptions := FStreamer.ObjectToJsonEx(TargetObject.Options);
    TargetObject.ValidateAppearance;
  finally
    edit_form.Free;
  end;
end;

procedure TEditObject.Redo;
begin
  TargetObject.Options.Clear;
  FDestreamer.JSONToObjectEx(FNewOptions, TargetObject.Options);
  TargetObject.ValidateAppearance;
end;

procedure TEditObject.Undo;
begin
  TargetObject.Options.Clear;
  FDestreamer.JSONToObjectEx(FOldOptions, TargetObject.Options);
  TargetObject.ValidateAppearance;
end;

function TEditObject.GetDescription: string;
begin
  Result := rsEditObjectDescription;
end;

class function TEditObject.GetOwnershipTrait: TObjectOwnershipTrait;
begin
  Result:=TObjectOwnershipTrait.NoFree;
end;

{ TObjectSelectBrush }

procedure TObjectSelectBrush.CheckAddOneTile(AMap: TVCMIMap; const Coord: TMapCoord);
begin
  inherited CheckAddOneTile(AMap, Coord);

  if Assigned(FVisibleObjects) then
  begin
    AMap.SelectObjectsOnTile(FVisibleObjects.Data, AMap.CurrentLevelIndex, Coord.X, Coord.Y, FSelectedObjects);
  end;
end;

constructor TObjectSelectBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedObjects := TMapObjectSet.Create;
  Size:=0;
  SetMode(TBrushMode.area);
end;

destructor TObjectSelectBrush.Destroy;
begin
  FSelectedObjects.Free;
  inherited Destroy;
end;

procedure TObjectSelectBrush.Clear;
begin
  inherited Clear;
end;

procedure TObjectSelectBrush.ClearSelection;
begin
  inherited ClearSelection;
  FreeAndNil(FSelectedObjects);
  FSelectedObjects := TMapObjectSet.Create;
end;

procedure TObjectSelectBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin
  //do nothing
end;

procedure TObjectSelectBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin
  //do nothing, default system cursor is enough
end;

procedure TObjectSelectBrush.RenderSelection(State: TLocalState);
var
  it: TMapObjectSet.TIterator;
begin
  inherited RenderSelection(State);

  it := FSelectedObjects.Min;

  if Assigned(it) then
  begin
    repeat
      it.Data.RenderSelectionRect(State);

    until not it.Next;

    FreeAndNil(it);
  end;
end;

procedure TObjectSelectBrush.MouseLeave(AMap: TVCMIMap);
begin
  //ignore
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
      if State <> TUndoItemState.UnDone then
        FreeTargets;

    TObjectOwnershipTrait.FreeIfUndone:
      if State <> TUndoItemState.ReDone then
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

procedure TAddObject.SetCurrentPlayer(AValue: TPlayer);
begin
  if FCurrentPlayer=AValue then Exit;
  FCurrentPlayer:=AValue;
end;

function TAddObject.Execute: boolean;
begin
  TargetObject := TMapObject.CreateIndep(FMap);
  try
    TargetObject.AssignTemplate(Template);
    SetObjectPosition();
    TargetObject.Collection := FMap.Objects; //add object with valid configuration

    if IsPublishedProp(TargetObject.Options, 'Owner') then
    begin
      TargetObject.Options.Owner := CurrentPlayer;
    end;
    Result := true;
  except
    on e: Exception do
    begin
      Result := false;
      Application.MessageBox(Pchar(e.Message), 'Error', MB_OK or MB_ICONERROR);
    end;
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

{ TMoveObject }

function TMoveObject.Execute: boolean;
begin
  Result := true;
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

