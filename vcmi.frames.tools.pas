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

unit vcmi.frames.tools;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, typinfo, FileUtil, Forms, Controls, ComCtrls, ActnList, StdCtrls, Buttons, ExtCtrls, editor_types,
  editor_str_consts, map, map_actions, map_road_river_actions, map_terrain_actions, map_object_actions;

type

  { TToolsFrame }

  TToolsFrame = class(TFrame)
    act: TActionList;
    actObjSizeRect: TAction;
    actObjSize0: TAction;
    actSize0: TAction;
    actSizeRect: TAction;
    actSize4: TAction;
    actSize2: TAction;
    actSize1: TAction;
    btnBrush3: TSpeedButton;
    btnBrush4: TSpeedButton;
    btnBrush5: TSpeedButton;
    btnBrush6: TSpeedButton;
    btnBrush7: TSpeedButton;
    btnBrush8: TSpeedButton;
    btnBrushArea: TSpeedButton;
    btnBrushArea1: TSpeedButton;
    btnBrushArea2: TSpeedButton;
    btnSelect: TSpeedButton;
    EraseFilter: TCheckGroup;
    gbBrushObjects: TGroupBox;
    gbBrushTerrain: TGroupBox;
    gbBrushTerrain1: TGroupBox;
    gbTerrain: TGroupBox;
    RiverType: TRadioGroup;
    RoadType: TRadioGroup;
    tsErase: TTabSheet;
    ToolsPages: TPageControl;
    tsRivers: TTabSheet;
    tsRoads: TTabSheet;
    tsTerrain: TTabSheet;
    tsObjects: TTabSheet;
    procedure actObjSize0Execute(Sender: TObject);
    procedure actObjSizeRectExecute(Sender: TObject);
    procedure actSize0Execute(Sender: TObject);
    procedure actSize1Execute(Sender: TObject);
    procedure actSize2Execute(Sender: TObject);
    procedure actSize4Execute(Sender: TObject);
    procedure actSizeRectExecute(Sender: TObject);
    procedure RiverTypeSelectionChanged(Sender: TObject);
    procedure RoadTypeSelectionChanged(Sender: TObject);
    procedure ToolsPagesChange(Sender: TObject);
    procedure ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
  private
    //all brushes
    FIdleBrush: TIdleMapBrush;
    FFixedTerrainBrush: TFixedTerrainBrush;
    FAreaTerrainBrush: TAreaTerrainBrush;
    FRoadRiverBrush: TRoadRiverBrush;
    FObjectSelectBrush: TObjectSelectBrush;

    FActiveBrush: TMapBrush;
    FSelectedObject: TMapObject;
    procedure FillLandscapeMenu; unimplemented;
    procedure FillRoadRiverMenu;

    procedure OnTerrainButtonClick(Sender: TObject);

    procedure ObjectPageSelected;
    procedure SetSelectedObject(AValue: TMapObject);
    procedure TerrainPageSelected;
    procedure RiversPageSelected;
    procedure RoadsPageSelected;
    procedure ErasePageSelected;

    procedure SetActiveBrush(ABrush: TMapBrush);
  public
    constructor Create(TheOwner: TComponent); override;

    property ActiveBrush: TMapBrush read FActiveBrush;

    property IdleBrush: TIdleMapBrush read FIdleBrush;
    property ObjectSelectBrush: TObjectSelectBrush read FObjectSelectBrush;

    procedure SwitchToObjects;

    property SelectedObject: TMapObject read FSelectedObject write SetSelectedObject;
  end;

implementation

{$R *.lfm}

{ TToolsFrame }

procedure TToolsFrame.ToolsPagesChange(Sender: TObject);
begin
  //todo: select active brush
  if ToolsPages.ActivePage = tsObjects then
  begin
    ObjectPageSelected;
  end
  else if ToolsPages.ActivePage = tsTerrain then
  begin
    TerrainPageSelected;
  end
  else if ToolsPages.ActivePage = tsRivers then
  begin
    RiversPageSelected;
  end
  else if ToolsPages.ActivePage = tsRoads then
  begin
    RoadsPageSelected;
  end
  else
  begin
    ErasePageSelected;
  end;
end;

procedure TToolsFrame.RoadTypeSelectionChanged(Sender: TObject);
begin
  FRoadRiverBrush.RoadType:=TRoadType(PtrInt(RoadType.Items.Objects[RoadType.ItemIndex]));
end;

procedure TToolsFrame.RiverTypeSelectionChanged(Sender: TObject);
begin
  FRoadRiverBrush.RiverType:=TRiverType(PtrInt(RiverType.Items.Objects[RiverType.ItemIndex]));
end;

procedure TToolsFrame.actSize0Execute(Sender: TObject);
begin
  SetActiveBrush(FIdleBrush);
end;

procedure TToolsFrame.actObjSize0Execute(Sender: TObject);
begin
  SetActiveBrush(FIdleBrush);
end;

procedure TToolsFrame.actObjSizeRectExecute(Sender: TObject);
begin
  SetActiveBrush(FObjectSelectBrush);
end;

procedure TToolsFrame.actSize1Execute(Sender: TObject);
begin
  SetActiveBrush(FFixedTerrainBrush);
  FFixedTerrainBrush.Size := 1;
end;

procedure TToolsFrame.actSize2Execute(Sender: TObject);
begin
  SetActiveBrush(FFixedTerrainBrush);
  FFixedTerrainBrush.Size := 2;
end;

procedure TToolsFrame.actSize4Execute(Sender: TObject);
begin
  SetActiveBrush(FFixedTerrainBrush);
  FFixedTerrainBrush.Size := 4;
end;

procedure TToolsFrame.actSizeRectExecute(Sender: TObject);
begin
  SetActiveBrush(FAreaTerrainBrush);
end;

procedure TToolsFrame.ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := true;
  //TODO: clear active brush
end;

procedure TToolsFrame.FillLandscapeMenu;
var
  tt: TTerrainType;
  idx: Integer;
  button: TSpeedButton;
  first: Boolean;
begin
  //todo: localization

  first := true;

  for tt in TTerrainType do
  begin
    idx := Integer(tt);
    button := TSpeedButton.Create(Self);
    button.Parent := gbTerrain;
    button.Caption:=GetEnumName(TypeInfo(tt),idx);
    button.ShowCaption := true;
    button.OnClick := @OnTerrainButtonClick;
    button.Tag:=idx;
    button.GroupIndex:=2;

    if first then
    begin
      button.Down := true;
      first:=false;
    end;
  end;
end;

procedure TToolsFrame.FillRoadRiverMenu;
begin
  RoadType.Items.Clear;
  RoadType.Items.AddObject(rsRoadTypeDirt, TObject(PtrInt(TRoadType.dirtRoad)));
  RoadType.Items.AddObject(rsRoadTypeGravel, TObject(PtrInt(TRoadType.gravelRoad)));
  RoadType.Items.AddObject(rsRoadTypeCobblestone, TObject(PtrInt(TRoadType.cobblestoneRoad)));
  RoadType.Items.AddObject(rsRoadTypeNone, TObject(PtrInt(TRoadType.noRoad)));
  RoadType.ItemIndex := 0;

  RiverType.Items.Clear;
  RiverType.Items.AddObject(rsRiverTypeClear, TObject(PtrInt(TRiverType.clearRiver)));
  RiverType.Items.AddObject(rsRiverTypeIcy, TObject(PtrInt(TRiverType.icyRiver)));
  RiverType.Items.AddObject(rsRiverTypeMuddy, TObject(PtrInt(TRiverType.muddyRiver)));
  RiverType.Items.AddObject(rsRiverTypeLava, TObject(PtrInt(TRiverType.lavaRiver)));
  RiverType.Items.AddObject(rsRiverTypeNone, TObject(PtrInt(TRiverType.noRiver)));

  RiverType.ItemIndex:=0;
end;

procedure TToolsFrame.OnTerrainButtonClick(Sender: TObject);
var
  tt: TTerrainType;
begin
  tt := TTerrainType((Sender as TSpeedButton).Tag);
  FFixedTerrainBrush.TT := tt;
  FAreaTerrainBrush.TT:=tt;
end;

procedure TToolsFrame.ObjectPageSelected;
begin
  if actSize0.Checked then
  begin
    SetActiveBrush(FIdleBrush);
  end
  else if actSizeRect.Checked then
  begin
    SetActiveBrush(FObjectSelectBrush);
  end
  else
  begin
    actSize0.Checked := true;
    SetActiveBrush(FIdleBrush);
  end
end;

procedure TToolsFrame.SetSelectedObject(AValue: TMapObject);
begin
  FSelectedObject:=AValue;
end;

procedure TToolsFrame.TerrainPageSelected;
begin
  if actSize1.Checked then
  begin
    SetActiveBrush(FFixedTerrainBrush);
    FFixedTerrainBrush.Size:=1;
  end
  else if actSize2.Checked then
  begin
    SetActiveBrush(FFixedTerrainBrush);
    FFixedTerrainBrush.Size:=2;
  end
  else if actSize4.Checked then
  begin
    SetActiveBrush(FFixedTerrainBrush);
    FFixedTerrainBrush.Size:=4;
  end
  else if actSizeRect.Checked then
  begin
    SetActiveBrush(FAreaTerrainBrush);
  end
  else
  begin
    actSize1.Checked := true;
    SetActiveBrush(FFixedTerrainBrush);
    FFixedTerrainBrush.Size:=1;
  end
end;

procedure TToolsFrame.RiversPageSelected;
begin
  SetActiveBrush(FRoadRiverBrush);
  RiverTypeSelectionChanged(nil);
end;

procedure TToolsFrame.RoadsPageSelected;
begin
  SetActiveBrush(FRoadRiverBrush);
  RoadTypeSelectionChanged(nil);
end;

procedure TToolsFrame.ErasePageSelected;
begin
  //TODO
  SetActiveBrush(FIdleBrush);
end;

procedure TToolsFrame.SetActiveBrush(ABrush: TMapBrush);
begin
  FActiveBrush :=  ABrush;
  FActiveBrush.Clear;
  FSelectedObject := nil;
end;

constructor TToolsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIdleBrush := TIdleMapBrush.Create(Self);
  FFixedTerrainBrush := TFixedTerrainBrush.Create(Self);
  FAreaTerrainBrush := TAreaTerrainBrush.Create(Self);
  FRoadRiverBrush := TRoadRiverBrush.Create(Self);
  FObjectSelectBrush := TObjectSelectBrush.Create(Self);

  FActiveBrush := FIdleBrush;

  FillLandscapeMenu;
  FillRoadRiverMenu;
end;

procedure TToolsFrame.SwitchToObjects;
begin
  ToolsPages.ActivePage := tsObjects;
  if not actSize0.Checked then
  begin
    actSize0.Execute;
  end;
end;

end.

