{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge.net

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
unit town_buildings_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, base_options_frame, object_options,
  lists_manager, logical_id_condition, editor_classes;

type

  { TTownBuildingsFrame }

  TTownBuildingsFrame = class(TBaseOptionsFrame)
    Buildings: TTreeView;
    Built: TCheckBox;
    Allowed: TCheckBox;
    Panel1: TPanel;
  private
    FObject: TLogicalIDCondition;
    FConfig: TTownInfo;

    procedure FillBuildings;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
    procedure VisitTown(AOptions: TTownOptions); override;
  end;

implementation

{$R *.lfm}

{ TTownBuildingsFrame }

procedure TTownBuildingsFrame.FillBuildings;
var
  i: Integer;
  building: TTownBuilding;

  last_node: TTreeNode;

begin
  last_node := nil;
  for i := 0 to FConfig.Buildings.Count - 1 do
  begin
    building :=  FConfig.Buildings[i];

    if building.Upgrades = '' then
    begin
      last_node := Buildings.Items.AddObject(last_node, building.DisplayName, building);
    end
    else
    begin

    end;
  end;


end;

constructor TTownBuildingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TTownBuildingsFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TTownBuildingsFrame.Commit;
begin
  inherited Commit;
end;

procedure TTownBuildingsFrame.VisitTown(AOptions: TTownOptions);
var
  town_type: AnsiString;
begin
  inherited VisitTown(AOptions);

  FObject := AOptions.Buildings;

  town_type := AOptions.MapObject.GetSubId;

  FConfig := ListsManager.GetFaction(town_type).Town;

  FillBuildings;
end;

end.

