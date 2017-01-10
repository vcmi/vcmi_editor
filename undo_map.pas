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
unit undo_map;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, undo_base, map, map_rect, fgl;

type

  { TMapUndoItem }

  TMapUndoItem = class(TAbstractUndoItem)
  protected
    FMap: TVCMIMap;
  public
    constructor Create(AMap: TVCMIMap); virtual;

    //by default invalidates all tiles
    function GetChangedRegion(ALevelIndex: integer): TMapRect; virtual;
  end;

  TItemStack = specialize TFPGObjectList<TAbstractUndoItem>;

  TOnRegionInvalidated = procedure (ALevel: Integer; ARegion: TMapRect) of object;
  TOnActionPreformed = procedure (AItem: TAbstractUndoItem) of object;

  { TMapUndoManager }

  TMapUndoManager = class(TAbstractUndoManager)
  strict private
    FItemStack: TItemStack;
    FCheckPoint: Integer;

    FCurrentPosition: Integer;
    FMap: TVCMIMap;
    FOnActionPerformed: TOnActionPreformed;
    FOnRegionInvalidated: TOnRegionInvalidated;
    procedure SetMap(AValue: TVCMIMap);
    procedure DoRegionInvalidated(ALevel: Integer; ARegion: TMapRect);
    procedure CheckInvalidatedRegions(AItem: TAbstractUndoItem);
    procedure DoActionPerformed(AItem: TAbstractUndoItem);
  public
    constructor Create;
    destructor Destroy; override;

    function CanRedo: boolean; override;
    function CanUndo: boolean; override;

    procedure Undo; override;
    procedure Redo; override;

    function ExecuteItem(AItem: TAbstractUndoItem): boolean; override;
    function PeekCurrent: TAbstractUndoItem; override;
    function PeekNext: TAbstractUndoItem; override;

    procedure MarkCheckPoint;
    function IsCheckPoint: Boolean;

    procedure Clear; override;

    property Map: TVCMIMap read FMap write SetMap;

    property OnRegionInvalidated: TOnRegionInvalidated read FOnRegionInvalidated write FOnRegionInvalidated;
    property OnActionPerformed: TOnActionPreformed read FOnActionPerformed write FOnActionPerformed;
  end;

implementation

{ TMapUndoItem }

constructor TMapUndoItem.Create(AMap: TVCMIMap);
begin
  FMap := AMap;
end;

function TMapUndoItem.GetChangedRegion(ALevelIndex: integer): TMapRect;
begin
  Result := FMap.MapLevels[ALevelIndex].GetDimentions;
end;

{ TMapUndoManger }

function TMapUndoManager.CanRedo: boolean;
begin
  Result := (FCurrentPosition >=-1) and (FCurrentPosition < FItemStack.Count - 1);
end;

function TMapUndoManager.CanUndo: boolean;
begin
  Result := (FCurrentPosition >=0) and (FCurrentPosition < FItemStack.Count);
end;

procedure TMapUndoManager.Clear;
begin
  FItemStack.Clear;
  FCurrentPosition := -1;
  FCheckPoint := -2;
end;

procedure TMapUndoManager.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TMapUndoManager.DoRegionInvalidated(ALevel: Integer; ARegion: TMapRect);
begin
  if Assigned(FOnRegionInvalidated) then
  begin
    FOnRegionInvalidated(ALevel, ARegion);
  end;
end;

procedure TMapUndoManager.CheckInvalidatedRegions(AItem: TAbstractUndoItem);
var
  level_index: Integer;
  invalid_region: TMapRect;
begin
  if not Assigned(FMap) then
    Exit;

  if not (AItem is TMapUndoItem) then
    Exit;

  for level_index := 0 to FMap.MapLevels.Count - 1 do
  begin
    invalid_region :=  TMapUndoItem(AItem).GetChangedRegion(level_index);
    if not invalid_region.IsEmpty then
      DoRegionInvalidated(level_index, invalid_region);
  end;
end;

procedure TMapUndoManager.DoActionPerformed(AItem: TAbstractUndoItem);
begin
  if Assigned(FOnActionPerformed) then
  begin
    FOnActionPerformed(AItem);
  end;
end;

constructor TMapUndoManager.Create;
begin
  FItemStack := TItemStack.Create(True);
  FCurrentPosition := -1;
  FCheckPoint:= -2;
end;

destructor TMapUndoManager.Destroy;
begin
  FItemStack.Free;
  inherited Destroy;
end;

function TMapUndoManager.ExecuteItem(AItem: TAbstractUndoItem): boolean;
var
  i: Integer;
begin
  SetItemState(AItem,TUndoItemState.Idle);
  Result := AItem.Execute;

  if not Result then
  begin
    AItem.Free;
    Exit;
  end;

  CheckInvalidatedRegions(AItem);
  SetItemState(AItem,TUndoItemState.ReDone);
  for i := FItemStack.Count - 1 downto Max(FCurrentPosition+1, 0) do
  begin
    FItemStack.Delete(i);
  end;

  FCurrentPosition := FItemStack.Add(AItem);
  FMap.IsDirty:= not IsCheckPoint;
  DoActionPerformed(AItem);
end;

function TMapUndoManager.PeekCurrent: TAbstractUndoItem;
begin
  if CanUndo then
  begin
    Result := FItemStack[FCurrentPosition];
  end
  else begin
    Result := nil;
  end;
end;

function TMapUndoManager.PeekNext: TAbstractUndoItem;
begin
  if CanRedo then
  begin
    Result := FItemStack[FCurrentPosition+1];
  end
  else begin
    Result := nil;
  end;
end;

procedure TMapUndoManager.MarkCheckPoint;
begin
  FCheckPoint := FCurrentPosition;
end;

function TMapUndoManager.IsCheckPoint: Boolean;
begin
  Result := FCheckPoint = FCurrentPosition;
end;

procedure TMapUndoManager.Redo;
var
  item: TAbstractUndoItem;
begin
  item := PeekNext;
  item.Redo;
  CheckInvalidatedRegions(item);
  SetItemState(item,TUndoItemState.ReDone);
  Inc(FCurrentPosition);
  FMap.IsDirty:= not IsCheckPoint;
  DoActionPerformed(Item);
end;

procedure TMapUndoManager.Undo;
var
  item: TAbstractUndoItem;
begin
  item := PeekCurrent;
  item.Undo;
  CheckInvalidatedRegions(item);
  SetItemState(item,TUndoItemState.UnDone);
  Dec(FCurrentPosition);
  FMap.IsDirty:= not IsCheckPoint;
  DoActionPerformed(Item);
end;

end.

