{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit town_buildings_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, GraphType, ActnList, Buttons, contnrs, base_options_frame, object_options,
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

    function IsModifiable(): Boolean;
  end;

  TBuildingProc =  procedure (ANode: TTreeNode; AData: TBuildingData) of object;

  { TTownBuildingsFrame }

  TTownBuildingsFrame = class(TBaseOptionsFrame)
    act: TActionList;
    actAllowAll: TAction;
    actDenyAll: TAction;
    actInvertAllowed: TAction;
    actBuildAll: TAction;
    actInvertBuilt: TAction;
    actRazeAll: TAction;
    Buildings: TTreeView;
    Built: TCheckBox;
    Allowed: TCheckBox;
    cbCustomise: TCheckBox;
    edHasFort: TCheckBox;
    SelectedBuilding: TGroupBox;
    AllBuildings: TGroupBox;
    img: TImageList;
    pnHeader: TPanel;
    pnMain: TPanel;
    pnDetails: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    procedure actAllowAllExecute(Sender: TObject);
    procedure actBuildAllExecute(Sender: TObject);
    procedure actDenyAllExecute(Sender: TObject);
    procedure actInvertAllowedExecute(Sender: TObject);
    procedure actInvertBuiltExecute(Sender: TObject);
    procedure actRazeAllExecute(Sender: TObject);
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

    procedure FillBuildings;
    procedure LoadBuildings;

    procedure Allow(ANode: TTreeNode; AData: TBuildingData);
    procedure Deny(ANode: TTreeNode; AData: TBuildingData);
    procedure InvertAllowed(ANode: TTreeNode; AData: TBuildingData);
    procedure Build(ANode: TTreeNode; AData: TBuildingData);
    procedure Raze(ANode: TTreeNode; AData: TBuildingData);
    procedure InvertBuilt(ANode: TTreeNode; AData: TBuildingData);

    procedure ModifyEachBuilding(AProc: TBuildingProc);

    procedure ModifyParents(ACurrentNode: TTreeNode; AProc: TBuildingProc);
    procedure ModifyChildren(ACurrentNode: TTreeNode; AProc: TBuildingProc);

    procedure BuildingChanged(ACurrentNode: TTreeNode);

    procedure ValidateTree;
  protected
    procedure Load; override;

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

function TBuildingData.IsModifiable(): Boolean;
begin
  Result := Building.Mode in [TBuildMode.normal,TBuildMode.grail]
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

    BuildingChanged(Buildings.Selected);
  end;
  UpdateControls;
  Buildings.Invalidate;
end;

procedure TTownBuildingsFrame.actAllowAllExecute(Sender: TObject);
begin
  ModifyEachBuilding(@Allow);
end;

procedure TTownBuildingsFrame.actBuildAllExecute(Sender: TObject);
begin
  ModifyEachBuilding(@Build);
end;

procedure TTownBuildingsFrame.actDenyAllExecute(Sender: TObject);
begin
  ModifyEachBuilding(@Deny);
end;

procedure TTownBuildingsFrame.actInvertAllowedExecute(Sender: TObject);
begin
  ModifyEachBuilding(@InvertAllowed);
  ValidateTree;
end;

procedure TTownBuildingsFrame.actInvertBuiltExecute(Sender: TObject);
begin
  ModifyEachBuilding(@InvertBuilt);
  ValidateTree;
end;

procedure TTownBuildingsFrame.actRazeAllExecute(Sender: TObject);
begin
  ModifyEachBuilding(@Raze);
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
    BuildingChanged(Buildings.Selected);
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

procedure TTownBuildingsFrame.Allow(ANode: TTreeNode; AData: TBuildingData);
begin
   AData.Allowed:=True;
end;

procedure TTownBuildingsFrame.Deny(ANode: TTreeNode; AData: TBuildingData);
begin
  AData.Allowed:=False;
  AData.Built:=False;
end;

procedure TTownBuildingsFrame.InvertAllowed(ANode: TTreeNode; AData: TBuildingData);
begin
  AData.Allowed:=not AData.Allowed;
  if not AData.Allowed then
     AData.Built:=False;
end;

procedure TTownBuildingsFrame.Build(ANode: TTreeNode; AData: TBuildingData);
begin
  AData.Built:=true;
  AData.Allowed:=true;
end;

procedure TTownBuildingsFrame.Raze(ANode: TTreeNode; AData: TBuildingData);
begin
  AData.Built:=false;
end;

procedure TTownBuildingsFrame.InvertBuilt(ANode: TTreeNode; AData: TBuildingData);
begin
  AData.Built:=not AData.Built;
  if AData.Built then
    AData.Allowed:=true;
end;

procedure TTownBuildingsFrame.ModifyEachBuilding(AProc: TBuildingProc);
var
  node: TTreeNode;
  data: TBuildingData;
begin
  for node in Buildings.Items do
  begin
    data := TBuildingData(node.Data);
    if data.IsModifiable() then
    begin
      AProc(node,data);
    end;
  end;
  BuildingsSelectionChanged(Buildings);
  Buildings.Invalidate;
end;

procedure TTownBuildingsFrame.ModifyParents(ACurrentNode: TTreeNode; AProc: TBuildingProc);
var
  parent_node: TTreeNode;
  data: TBuildingData;
begin
  parent_node := ACurrentNode.Parent;

  while Assigned(parent_node) do
  begin
    data := TBuildingData(parent_node.Data);

    if data.IsModifiable() then
    begin
      AProc(parent_node,data);
    end;

    parent_node := parent_node.Parent;
  end;
end;

procedure TTownBuildingsFrame.ModifyChildren(ACurrentNode: TTreeNode; AProc: TBuildingProc);
var
  idx: Integer;
  child_node: TTreeNode;
  data: TBuildingData;
begin
  for idx := 0 to Pred(ACurrentNode.Count) do
  begin
    child_node := ACurrentNode.Items[idx];

    data := TBuildingData(child_node.Data);

    if data.IsModifiable() then
    begin
      AProc(child_node,data);
    end;

    ModifyChildren(child_node, AProc);
  end;
end;

procedure TTownBuildingsFrame.BuildingChanged(ACurrentNode: TTreeNode);
var
  node_data: TBuildingData;
begin
  node_data := TBuildingData(ACurrentNode.Data);

  if node_data.Built then
  begin
    ModifyParents(ACurrentNode, @Build);
    ModifyParents(ACurrentNode, @Allow);
  end;

  if not node_data.Built then
  begin
    ModifyChildren(ACurrentNode, @Raze);
  end;

  if node_data.Allowed then
  begin
    ModifyParents(ACurrentNode, @Allow);
  end;

  if not node_data.Allowed then
  begin
    ModifyChildren(ACurrentNode, @Raze);
    ModifyChildren(ACurrentNode, @Deny);
  end;
end;

procedure TTownBuildingsFrame.ValidateTree;
begin
  //TODO: TTownBuildingsFrame.ValidateTree
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

  town_type := AOptions.MapObject.GetSubtype;

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

