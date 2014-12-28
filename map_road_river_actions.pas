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

unit map_road_river_actions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, undo_base, undo_map, Map, editor_types, map_actions;

type

   { TRoadRiverBrush }

   TRoadRiverBrushKind = (road, river);

   TRoadRiverBrush = class(TMapBrush)
   private
     FKind: TRoadRiverBrushKind;
     FRiverType: TRiverType;
     FRoadType: TRoadType;
     procedure SetKind(AValue: TRoadRiverBrushKind);
     procedure SetRiverType(AValue: TRiverType);
     procedure SetRoadType(AValue: TRoadType);
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

     property RoadType: TRoadType read FRoadType write SetRoadType;
     property RiverType: TRiverType read FRiverType write SetRiverType;
     property Kind:TRoadRiverBrushKind read FKind write SetKind;
   end;


   { TEditRoadRiver }

   TEditRoadRiver = class abstract (TMapUndoItem)
   public
     constructor Create(AMap: TVCMIMap); override;
     destructor Destroy; override;

     procedure Redo; override;
     procedure Undo; override;
     procedure Execute; override;
   end;

   { TEditRoad }

   TEditRoad = class (TEditRoadRiver)
   public
     function GetDescription: string; override;
   end;

   { TEditRiver }

   TEditRiver = class (TEditRoadRiver)
   public
     function GetDescription: string; override;
   end;

implementation

uses editor_str_consts;

{ TRoadRiverBrush }

procedure TRoadRiverBrush.SetRoadType(AValue: TRoadType);
begin
  if FRoadType=AValue then Exit;
  FRoadType:=AValue;
  FRiverType:=TRiverType.noRiver;
  Kind := TRoadRiverBrushKind.road;
end;

procedure TRoadRiverBrush.SetRiverType(AValue: TRiverType);
begin
  if FRiverType=AValue then Exit;
  FRiverType:=AValue;
  FRoadType := TRoadType.noRoad;
  Kind := TRoadRiverBrushKind.river;
end;

procedure TRoadRiverBrush.SetKind(AValue: TRoadRiverBrushKind);
begin
  FKind:=AValue;
  Clear;
end;

constructor TRoadRiverBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRoadRiverBrush.Destroy;
begin
  inherited Destroy;
end;

{ TEditRoadRiver }

constructor TEditRoadRiver.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
end;

destructor TEditRoadRiver.Destroy;
begin
  inherited Destroy;
end;

procedure TEditRoadRiver.Redo;
begin

end;

procedure TEditRoadRiver.Undo;
begin

end;

procedure TEditRoadRiver.Execute;
begin

end;

{ TEditRoad }

function TEditRoad.GetDescription: string;
begin
  Result := rsEditRoadDescription;
end;

{ TEditRiver }

function TEditRiver.GetDescription: string;
begin
  Result := rsEditRiverDescription;
end;

end.

