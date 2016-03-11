{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
  ExtCtrls, StdCtrls, GraphType, contnrs, base_options_frame, object_options,
  lists_manager, logical_id_condition, editor_classes, editor_types;

type

  { TBuildingData }

  TBuildingData = class
  private
    FAllowed: Boolean;
    FBuilding: TTownBuilding;
    FBuilt: Boolean;
    function GetIdentifier: AnsiString;
    procedure SetAllowed(AValue: Boolean);
    procedure SetBuilding(AValue: TTownBuilding);
    procedure SetBuilt(AValue: Boolean);
  public
    constructor Create(ABuilding: TTownBuilding);
    property Building: TTownBuilding read FBuilding write SetBuilding;
    property Identifier: AnsiString read GetIdentifier;

    property Allowed: Boolean read FAllowed write SetAllowed;
    property Built: Boolean read FBuilt write SetBuilt;

  end;

  { TTownBuildingsFrame }

  TTownBuildingsFrame = class(TBaseOptionsFrame)
    Buildings: TTreeView;
    Built: TCheckBox;
    Allowed: TCheckBox;
    cbCustomise: TCheckBox;
    edHasFort: TCheckBox;
    img: TImageList;
    pnHeader: TPanel;
    pnMain: TPanel;
    pnDetails: TPanel;
    procedure AllowedChange(Sender: TObject);
    procedure BuildingsAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure BuildingsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure BuildingsDeletion(Sender: TObject; Node: TTreeNode);
    procedure BuildingsGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure BuildingsGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure BuildingsSelectionChanged(Sender: TObject);
    procedure BuiltChange(Sender: TObject);
    procedure cbCustomiseChange(Sender: TObject);
  private
    FObject: TTownOptions;
    FConfig: TTownInfo;

    FDoUpdateNodeData: Boolean;

    procedure Load;
    procedure FillBuildings;
    procedure LoadBuildings;
  protected
    procedure UpdateControls; override;

    procedure VisitNormalTown(AOptions: TTownOptions); override;
    procedure VisitRandomTown(AOptions: TTownOptions); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ TBuildingData }

procedure TBuildingData.SetBuilding(AValue: TTownBuilding);
begin
  if FBuilding=AValue then Exit;
  FBuilding:=AValue;
end;

procedure TBuildingData.SetBuilt(AValue: Boolean);
begin
  if FBuilt=AValue then Exit;
  FBuilt:=AValue;
end;

function TBuildingData.GetIdentifier: AnsiString;
begin
  Result := FBuilding.Identifier;
end;

procedure TBuildingData.SetAllowed(AValue: Boolean);
begin
  if FAllowed=AValue then Exit;
  FAllowed:=AValue;
end;

constructor TBuildingData.Create(ABuilding: TTownBuilding);
begin
  FBuilding := ABuilding;
end;

{ TTownBuildingsFrame }

procedure TTownBuildingsFrame.BuildingsDeletion(Sender: TObject; Node: TTreeNode
  );
begin
  if Assigned(Node.Data) then
  begin
    TObject(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TTownBuildingsFrame.BuildingsGetImageIndex(Sender: TObject; Node: TTreeNode);
var
  node_data: TBuildingData;
begin
  node_data := TBuildingData(Node.Data);

  if node_data.Built then
  begin
    Node.ImageIndex:=0;
  end
  else if not node_data.Allowed then
  begin
    Node.ImageIndex:=1;
  end
  else
  begin
    Node.ImageIndex:=-1;
  end;
end;

procedure TTownBuildingsFrame.BuildingsGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  BuildingsGetImageIndex(Sender, Node);
end;

procedure TTownBuildingsFrame.AllowedChange(Sender: TObject);
begin
  if FDoUpdateNodeData and Assigned(Buildings.Selected) then
  begin
    TBuildingData(Buildings.Selected.Data).Allowed:= Allowed.Checked;
  end;
  UpdateControls;
  Buildings.Invalidate;
end;

procedure TTownBuildingsFrame.BuildingsAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  PaintImages := true;
  DefaultDraw := true;
end;

procedure TTownBuildingsFrame.BuildingsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  DefaultDraw := true;
end;

procedure TTownBuildingsFrame.BuildingsSelectionChanged(Sender: TObject);
var
  node_data: TBuildingData;
begin
  FDoUpdateNodeData := false;
  try
    if Assigned(Buildings.Selected) then
    begin
      node_data := TBuildingData(Buildings.Selected.Data);
      Allowed.Checked:=node_data.Allowed;

      Built.Checked:=node_data.Allowed and node_data.Built;
    end
    else
    begin
      Built.Checked := false;
      Allowed.Checked := false;
    end;

    UpdateControls;
  finally
    FDoUpdateNodeData := true;
  end;
end;

procedure TTownBuildingsFrame.BuiltChange(Sender: TObject);
begin
  if FDoUpdateNodeData and Assigned(Buildings.Selected) then
  begin
    TBuildingData(Buildings.Selected.Data).Built:= Allowed.Checked and Built.Checked;
  end;
  UpdateControls;
  Buildings.Invalidate;
end;

procedure TTownBuildingsFrame.cbCustomiseChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TTownBuildingsFrame.Load;
begin
  cbCustomise.Checked := not FObject.Buildings.IsEmpty;
  edHasFort.Checked:=FObject.HasFort;

  FillBuildings;
  LoadBuildings;

  UpdateControls;

  FDoUpdateNodeData := true;
end;

procedure TTownBuildingsFrame.FillBuildings;
var
  i: Integer;
  building: TTownBuilding;
  node_data: TBuildingData;
  last_node,  node: TTreeNode;

  q: TObjectQueue;

  placed: TStringList;
  idx: Integer;
begin
  last_node := nil;

  q := TObjectQueue.Create;
  placed := TStringList.Create;
  placed.Duplicates := dupError;
  placed.Sorted:=true;
  try
    for i := 0 to FConfig.Buildings.Count - 1 do
    begin
      building :=  FConfig.Buildings[i];

      if building.Upgrades = '' then
      begin
        node_data := TBuildingData.Create(building);
        last_node := Buildings.Items.AddObject(last_node, building.DisplayName, node_data);
        placed.AddObject(building.Identifier, last_node);
      end
      else
      begin
        q.Push(building);
      end;
    end;

    while q.Count > 0 do
    begin
      building := TTownBuilding(q.Pop);

      idx := placed.IndexOf(building.Upgrades);

      if idx >= 0 then
      begin
        node_data := TBuildingData.Create(building);
        node := placed.Objects[idx] as TTreeNode;
        node.Expanded := true;

        last_node :=  Buildings.Items.AddChildObject(node, building.DisplayName, node_data);
        placed.AddObject(building.Identifier, last_node);
      end
      else
      begin
        q.Push(building);
      end;
    end;
  finally
    placed.Free;
    q.Free;
  end;

end;

procedure TTownBuildingsFrame.LoadBuildings;
var
  node: TTreeNode;
  node_data: TBuildingData;
  default_allowed: Boolean;
begin
  default_allowed := FObject.Buildings.AnyOf.Count = 0;

  for node in Buildings.Items do
  begin
    node_data := TBuildingData(node.Data);
    node_data.Allowed:=(default_allowed or (FObject.Buildings.AnyOf.IndexOf(node_data.Identifier) >= 0)) and (FObject.Buildings.NoneOf.IndexOf(node_data.Identifier) < 0);
    node_data.Built:=node_data.Allowed and (FObject.Buildings.AllOf.IndexOf(node_data.Identifier) >= 0);
  end;
end;

procedure TTownBuildingsFrame.UpdateControls;
var
  node_data: TBuildingData;
begin
  inherited UpdateControls;

  pnMain.Enabled := cbCustomise.Checked;
  edHasFort.Enabled := not cbCustomise.Checked;

  if Assigned(Buildings.Selected) then
  begin
    node_data := TBuildingData(Buildings.Selected.Data);
    if node_data.Building.Mode in [TBuildMode.normal, TBuildMode.grail] then
    begin
      Allowed.Enabled := true;

      Built.Enabled := Allowed.Checked;
    end
    else
    begin
      Allowed.Enabled := false;
      Built.Enabled := false;
    end;
  end
  else
  begin
    Built.Enabled := false;
    Allowed.Enabled := false;
  end;
end;

procedure TTownBuildingsFrame.VisitNormalTown(AOptions: TTownOptions);
var
  town_type: AnsiString;
begin
  inherited VisitNormalTown(AOptions);

  FObject := AOptions;

  town_type := AOptions.MapObject.GetSubId;

  FConfig := ListsManager.GetFaction(town_type).Town;

  Load;
end;

procedure TTownBuildingsFrame.VisitRandomTown(AOptions: TTownOptions);
begin
  inherited VisitRandomTown(AOptions);
  FObject := AOptions;

  FConfig := ListsManager.RandomFaction.Town;

  Load;
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
var
  node: TTreeNode;
  node_data: TBuildingData;
begin
  inherited Commit;

  FObject.Buildings.Clear;

  if cbCustomise.Checked then
  begin
    for node in Buildings.Items do
    begin
      node_data := TBuildingData(node.Data);

      if node_data.Building.Mode in [TBuildMode.normal, TBuildMode.grail] then
      begin
        if node_data.Built then
        begin
          FObject.Buildings.AllOf.Add(node_data.Identifier);
        end
        else if not node_data.Allowed then
        begin
          FObject.Buildings.NoneOf.Add(node_data.Identifier);
        end;
      end;
    end;
  end
  else
  begin
    FObject.HasFort:=edHasFort.Checked;
  end;
end;


end.

