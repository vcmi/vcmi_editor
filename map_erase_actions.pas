{ This file is a part of Map editor for VCMI project

  Copyright (C) 2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit map_erase_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, Map, gset, undo_base, editor_types, map_rect, undo_map,
  editor_gl, map_actions, map_object_actions, map_road_river_actions, editor_str_consts;

type

  TEraseTarget = (Roads, Rivers, StaicObjects, InteractiveObjects);
  TEraseFilter = set of TEraseTarget;

  { TEraseBrush }

  TEraseBrush = class(TMapBrush)
  private
    FFilter: TEraseFilter;
    FSize: Integer;
  protected
    procedure AddTile(AMap: TVCMIMap; AX, AY: integer); override;
  public
    procedure Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap); override;

    procedure RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer); override;
    procedure RenderSelection(State: TLocalState); override;

    property Size: Integer read FSize write FSize;
    property Filter: TEraseFilter read FFilter write FFilter;
  end;

  { TEraseAction }

  TEraseAction = class (TMultiTileMapAction)
  private
    FEraseRoad: TEditRoad;
    FEraseRiver: TEditRiver;
    FFilter: TEraseFilter;
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    function GetDescription: string; override;
    function Execute: boolean; override;
    procedure Redo; override;
    procedure Undo; override;

    property Filter: TEraseFilter read FFilter write FFilter;
  end;

implementation

{ TEraseAction }

constructor TEraseAction.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FEraseRiver := TEditRiver.Create(AMap, TRiverType.noRiver);
  FEraseRoad := TEditRoad.Create(AMap, TRoadType.noRoad);
end;

destructor TEraseAction.Destroy;
begin
  FEraseRiver.Free;
  FEraseRoad.Free;
  inherited Destroy;
end;

function TEraseAction.GetDescription: string;
begin
  Result := rsEraseDescription;
end;

function TEraseAction.Execute: boolean;
begin

end;

procedure TEraseAction.Redo;
begin

end;

procedure TEraseAction.Undo;
begin

end;

{ TEraseBrush }

procedure TEraseBrush.AddTile(AMap: TVCMIMap; AX, AY: integer);
begin
  //TODO:
end;

procedure TEraseBrush.Execute(AManager: TAbstractUndoManager; AMap: TVCMIMap);
begin
  //TODO:
end;

procedure TEraseBrush.RenderCursor(State: TLocalState; AMap: TVCMIMap; X, Y: integer);
begin
  //TODO:
end;

procedure TEraseBrush.RenderSelection(State: TLocalState);
begin
  //TODO:
end;

end.

