{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
unit undo_map;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, undo_base, map, fgl;

type

  { TMapUndoItem }

  TMapUndoItem = class(TAbstractUndoItem)
  strict protected
    FMap: TVCMIMap;
  public
    constructor Create(AMap: TVCMIMap); virtual;
  end;

  TItemStack = specialize TFPGObjectList<TAbstractUndoItem>;

  { TMapUndoManger }

  TMapUndoManager = class(TAbstractUndoManager)
  strict private
    FItemStack: TItemStack;

    FCurrentPosition: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function CanRedo: boolean; override;
    function CanUndo: boolean; override;

    procedure Undo; override;
    procedure Redo; override;

    procedure ExecuteItem(AItem: TAbstractUndoItem); override;
    function PeekCurrent: TAbstractUndoItem; override;
    function PeekNext: TAbstractUndoItem; override;

    procedure Clear; override;
  end;

implementation

{ TMapUndoItem }

constructor TMapUndoItem.Create(AMap: TVCMIMap);
begin
  FMap := AMap;
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
end;

constructor TMapUndoManager.Create;
begin
  FItemStack := TItemStack.Create(True);
  FCurrentPosition := -1;
end;

destructor TMapUndoManager.Destroy;
begin
  FItemStack.Free;
  inherited Destroy;
end;

procedure TMapUndoManager.ExecuteItem(AItem: TAbstractUndoItem);
var
  i: Integer;
begin
  SetItemState(AItem,TUndoItemState.Idle);
  AItem.Execute;
  SetItemState(AItem,TUndoItemState.ReDone);
  for i := FItemStack.Count - 1 downto Max(FCurrentPosition+1, 0) do
  begin
    FItemStack.Delete(i);
  end;

  FCurrentPosition := FItemStack.Add(AItem);

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

procedure TMapUndoManager.Redo;
begin
  PeekNext.Redo;
  SetItemState(PeekNext,TUndoItemState.ReDone);
  Inc(FCurrentPosition);
end;

procedure TMapUndoManager.Undo;
begin
  PeekCurrent.Undo;
  SetItemState(PeekCurrent,TUndoItemState.UnDone);
  Dec(FCurrentPosition);
end;

end.

