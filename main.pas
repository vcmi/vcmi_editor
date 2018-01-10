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

unit main;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, GL, vcmi.OpenGLContext, LCLType, Forms, Controls, Graphics, GraphType,
  Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, ComCtrls, Buttons, EditBtn, PopupNotifier, Map, terrain, editor_types,
  undo_base, map_actions, map_objects, editor_graphics, minimap, filesystem, filesystem_base, lists_manager,
  zlib_stream, editor_gl, map_terrain_actions, map_road_river_actions, map_object_actions, undo_map, object_options,
  map_rect, map_format_json, search_index, vcmi.frames.tools, player_options_form, edit_triggered_events,
  player_selection_form, types;

type
  TAxisKind = (Vertical,Horizontal);

  TDragSubject = (MapObject, MapTemplate);
  TfMain = class;
  { TDragProxy }

  TDragProxy = class(TDragObject)
  protected
    FOwner: TFMain;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    constructor Create(AOwner: TfMain); reintroduce;
    destructor Destroy; override;
    procedure DropOnMap; virtual; abstract;
    procedure Render(AState: TLocalState; x,y: integer); virtual; abstract;
    procedure RenderOverlay(AState: TLocalState; x,y: integer); virtual; abstract;
  end;

  { TTemplateDragProxy }

  TTemplateDragProxy = class(TDragProxy)
  private
    FDraggingTemplate: TMapObjectTemplate;
  public
    constructor Create(AOwner: TfMain; ADraggingTemplate: TMapObjectTemplate);

    procedure DropOnMap; override;
    procedure Render(AState: TLocalState; x, y: integer); override;
    procedure RenderOverlay(AState: TLocalState; x,y: integer); override;
  end;

  { TObjectDragProxy }

  TObjectDragProxy = class(TDragProxy)
  private
    FShiftX, FShiftY: Integer;
    FDraggingObject: TMapObject;
  public
    constructor Create(AOwner: TfMain;ADraggingObject: TMapObject; CurrentX, CurrentY: integer);
    procedure DropOnMap; override;
    procedure Render(AState: TLocalState; x, y: integer); override;
    procedure RenderOverlay(AState: TLocalState; x,y: integer); override;
  end;

  { TfMain }

  TfMain = class(TForm)
    actDelete: TAction;
    actAnimateObjects: TAction;
    actFilterTowns: TAction;
    actFilterHeroes: TAction;
    actFilterResources: TAction;
    actFilterArtifacts: TAction;
    actFilterStatic: TAction;
    actFilterOther: TAction;
    actFilterCreatures: TAction;
    actFilterDwelling: TAction;
    actFilterOnMap: TAction;
    actClearActiveBrush: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    actViewGrid: TAction;
    actViewPassability: TAction;
    actVictoryLossConditions: TAction;
    actPlayerOptions: TAction;
    actProperties: TAction;
    actMapOptions: TAction;
    actSaveMapAs: TAction;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    ObjectsSearch: TEditButton;
    imlMainActionsSmall: TImageList;
    MainActions: TActionList;
    actCreateMap: TAction;
    actRedo: TAction;
    actUndo: TAction;
    actSaveMap: TAction;
    actOpenMap: TAction;
    ApplicationProperties1: TApplicationProperties;
    imlMainActions: TImageList;
    itmFreateMap: TMenuItem;
    MenuEdit: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem7: TMenuItem;
    menuLevel: TMenuItem;
    menuPlayer: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mm:      TMainMenu;
    menuFile: TMenuItem;
    MapView: TOpenGLControl;
    ObjectsView: TOpenGLControl;
    OpenMapDialog: TOpenDialog;
    pcMain: TPageControl;
    pnTop: TPanel;
    pnObjectsSearch: TPanel;
    pnObjects: TPanel;
    pnLeft: TPanel;
    HorisontalAxis: TPaintBox;
    MapViewNotifier: TPopupNotifier;
    SaveMapAsDialog: TSaveDialog;
    sbObjects: TScrollBar;
    StatusBar: TStatusBar;
    AnimTimer: TTimer;
    SearchTimer: TTimer;
    FilterToolBar: TToolBar;
    tsMap: TTabSheet;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton23: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    MainToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    VerticalAxis: TPaintBox;
    Minimap: TPaintBox;
    pnVAxis: TPanel;
    pnHAxis: TPanel;
    pnRight:  TPanel;
    pnMinimap: TPanel;
    pnMap:   TPanel;
    hScrollBar: TScrollBar;
    vScrollBar: TScrollBar;
    procedure actAnimateObjectsExecute(Sender: TObject);
    procedure actClearActiveBrushExecute(Sender: TObject);
    procedure actCreateMapExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actFilterArtifactsExecute(Sender: TObject);
    procedure actFilterCreaturesExecute(Sender: TObject);
    procedure actFilterDwellingExecute(Sender: TObject);
    procedure actFilterHeroesExecute(Sender: TObject);
    procedure actFilterOnMapExecute(Sender: TObject);
    procedure actFilterOtherExecute(Sender: TObject);
    procedure actFilterResourcesExecute(Sender: TObject);
    procedure actFilterStaticExecute(Sender: TObject);
    procedure actFilterTownsExecute(Sender: TObject);
    procedure actMapOptionsExecute(Sender: TObject);
    procedure actOpenMapExecute(Sender: TObject);
    procedure actPlayerOptionsExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actPropertiesUpdate(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actRedoUpdate(Sender: TObject);
    procedure actSaveMapAsExecute(Sender: TObject);
    procedure actSaveMapExecute(Sender: TObject);
    procedure actSaveMapUpdate(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure actVictoryLossConditionsExecute(Sender: TObject);
    procedure actVictoryLossConditionsUpdate(Sender: TObject);
    procedure actViewGridExecute(Sender: TObject);
    procedure actViewPassabilityExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomInUpdate(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actZoomOutUpdate(Sender: TObject);
    procedure AnimTimerTimer(Sender: TObject);
    procedure ApplicationProperties1IdleEnd(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HorisontalAxisPaint(Sender: TObject);
    procedure hScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MapViewClick(Sender: TObject);
    procedure MapViewDblClick(Sender: TObject);
    procedure MapViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MapViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MapViewMakeCurrent(Sender: TObject; var Allow: boolean);
    procedure MapViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapViewMouseEnter(Sender: TObject);
    procedure MapViewMouseLeave(Sender: TObject);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MapViewPaint(Sender: TObject);
    procedure MinimapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MinimapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ObjectsSearchButtonClick(Sender: TObject);
    procedure ObjectsSearchChange(Sender: TObject);
    procedure ObjectsSearchEditingDone(Sender: TObject);
    procedure ObjectsViewClick(Sender: TObject);
    procedure ObjectsViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ObjectsViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pcMainChange(Sender: TObject);
    procedure pcToolBoxChange(Sender: TObject);
    procedure PlayerMenuClick(Sender: TObject);
    procedure LevelMenuClick(Sender: TObject);
    procedure MinimapPaint(Sender: TObject);
    procedure ObjectsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsViewPaint(Sender: TObject);
    procedure ObjectsViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbObjectsResize(Sender: TObject);
    procedure sbObjectsScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SearchTimerTimer(Sender: TObject);
    procedure VerticalAxisPaint(Sender: TObject);
    procedure vScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    const
      OBJ_PER_ROW = 3;
      OBJ_CELL_SIZE = 60;
      OBJ_BORDER_WIDTH = 2;

      FTileSizes: array [0..3] of Integer = (8, 16, 24, 32);
  private
    FEnv: TMapEnvironment;
    FZoomIndex: integer;

    FObjectsPerRow: Integer;
    FObjectCellSize,FObjectCellTotalSize: Integer;
    FObjectBorderWidth: Integer;

    FZbuffer: TZBuffer;

    FResourceManager: IResourceLoader;

    FRealTileSize: Integer;

    FMapHPos, FMapVPos: Integer; //topleft visible tile
    FViewTilesH, FViewTilesV: Integer; //amount of tiles visible on mapview

    FMap: TVCMIMap;
    FMapFilename: string;
    FObjManager: TObjectsManager;
    FGraphicsManager: TGraphicsManager;

    FMouseTileX, FMouseTileY: Integer;  //current position of mouse in map coords
    FMouseX, FMouseY: Integer;  //current position of mouse in screen coords

    FMouseDTileX, FMouseDTileY: Integer;

    FToolsFrame: TToolsFrame;

    FUndoManager: TMapUndoManager;

    FMinimap: TMinimap;

    FObjectCategory: TObjectCategory;
    FTemplatesResult: TObjectsSelection;
    FMapObjectsResult: TMapObjectsSelection;

    FVisibleObjectsValid: boolean;
    FVisibleObjects: TVisibleObjects;

    FObjectCount: integer;
    FObjectRows: integer;
    FObjectReminder: integer;
    FObjectsVPos: Integer;
    FViewObjectRowsH: Integer;

    FDragging: TDragProxy;

    FMapDragging: boolean;
    FNextDragSubject: TDragSubject;
    FSelectedTemplate: TMapObjectTemplate;

    FCurrentPlayer: TPlayer;

    FMapViewState, FObjectsViewState: TLocalState;

    FAnimTick: int64;

    function GetObjIdx(col, row:integer): integer;
    function GetActiveSelection: ISearchResult;

    function getMapHeight: Integer;
    function getMapWidth: Integer;

    procedure InvalidateMapDimensions;

    procedure InvalidateMapAxis;
    procedure InvalidateMapContent;

    procedure InvalidateVisibleObjects;

    procedure SetZoomIndex(AIndex: Integer);
    procedure SetTileSize(ASize: Integer);

    procedure MapScrollByMinimap(x,y: integer);
    procedure MapScrollToObject(AObject:TMapObject);
    procedure SetMapPosition(APosition:TPoint);
    procedure ChangeMapPosition(ADelta:TPoint);
    procedure SetMapViewMouse(x,y: integer);

    procedure InvalidateObjects;
    procedure InvalidateObjPos;

    procedure MapChanded;

    procedure DoSetMapLevelIndex(ANewIndex: Integer);
    procedure DoMapLevelWheelScroll(WheelDelta: Integer);
    procedure DoMapViewWheelScroll(sb: TScrollBar;Shift: TShiftState; WheelDelta: Integer;MousePos: TPoint);

    procedure PaintAxis(Kind: TAxisKind; Axis: TPaintBox);

    procedure RenderGrid;
    procedure RenderSelection;

    function CheckUnsavedMap: boolean;
    procedure LoadMap(AFileName: string);
    procedure SaveMap(AFileName: string);

    procedure SetCurrentPlayer(APlayer: TPlayer);

    procedure SetupPlayerSelection;
    procedure SetupLevelSelection;

    procedure UpdateWidgets();

    procedure ResetFocus;

    procedure DoObjectsSearch();
    procedure DoObjectsCatSearch(ACategory: TObjectCategory);
    procedure UndoManagerOnActionPerformed(AItem: TAbstractUndoItem);
    procedure ClearSelection;
  protected
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
  public
    procedure MoveOrCopyObject(AObject: TMapObject; l,x,y: integer);
    procedure CopyObject(AObject: TMapObject; l,x,y: integer);
    procedure MoveObject(AObject: TMapObject; l,x,y: integer);
    procedure DeleteObject(AObject: TMapObject);
  end;

var
  fMain: TfMain;

implementation

uses
  map_format, map_format_h3m, editor_str_consts,
  root_manager, map_format_zip, editor_consts, edit_map_options,
  new_map, edit_object_options, Math, lazutf8classes, LazUTF8SysUtils;

{$R *.lfm}

{ TDragProxy }

function TDragProxy.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
  begin
    Result := crHandPoint;
  end
  else
  begin
    Result:=inherited GetDragCursor(Accepted, X, Y);
  end;
end;

constructor TDragProxy.Create(AOwner: TfMain);
begin
  FOwner := AOwner;
  AOwner.FDragging := Self;
  inherited AutoCreate(AOwner);
end;

destructor TDragProxy.Destroy;
begin
  FOwner.FDragging := nil;
  inherited Destroy;
end;

{ TObjectDragProxy }

constructor TObjectDragProxy.Create(AOwner: TfMain;
  ADraggingObject: TMapObject; CurrentX, CurrentY: integer);
begin
  FDraggingObject := ADraggingObject;
  FShiftX:=FDraggingObject.X - CurrentX;
  FShiftY:=FDraggingObject.Y - CurrentY;
  inherited Create(AOwner);
end;


procedure TObjectDragProxy.DropOnMap;
begin
  FOwner.MoveOrCopyObject(FDraggingObject,FOwner.FMap.CurrentLevelIndex,FOwner.FMouseTileX+FShiftX,FOwner.FMouseTileY+FShiftY);
end;

procedure TObjectDragProxy.Render(AState: TLocalState; x, y: integer);
begin
  FDraggingObject.RenderStatic(AState, (x+1+FShiftX)*TILE_SIZE,(y+1+FShiftY)*TILE_SIZE);
end;

procedure TObjectDragProxy.RenderOverlay(AState: TLocalState; x, y: integer);
begin
  FDraggingObject.RenderOverlay(AState, (x+1+FShiftX)*TILE_SIZE,(y+1+FShiftY)*TILE_SIZE);
end;

{ TTemplateDragProxy }

constructor TTemplateDragProxy.Create(AOwner: TfMain;
  ADraggingTemplate: TMapObjectTemplate);
begin
  FDraggingTemplate := ADraggingTemplate;
  inherited Create(AOwner);
end;

procedure TTemplateDragProxy.DropOnMap;
var
  action_item: TAddObject;
  c: TObjectOptionsClass;
  frm: TPlayerSelectionForm;
  mr: Integer;

  p: TPlayer;
begin
  c :=  TObjectOptions.GetClassByID(FDraggingTemplate.MapObjectGroup.Identifier, FDraggingTemplate.MapObjectType.Identifier);

  p := FOwner.FCurrentPlayer;

  if c.MustBeOwned then
  begin
    while p = TPlayer.none do
    begin
      frm := TPlayerSelectionForm.Create(nil);
      try
        mr := frm.ShowModal;
        if mr = mrOK then
        begin
          p := frm.SelectedPlayer;
        end;
      finally
        frm.Free;
      end;
      if mr = mrCancel then
        Exit;
    end;
  end;

  action_item := TAddObject.Create(FOwner.FMap);

  action_item.L:=FOwner.FMap.CurrentLevelIndex;
  action_item.X := FOwner.FMouseTileX;
  action_item.Y := FOwner.FMouseTileY;
  action_item.Template := FDraggingTemplate;

  action_item.CurrentPlayer:=p;

  if FOwner.FUndoManager.ExecuteItem(action_item) then
  begin
    FOwner.FToolsFrame.SelectedObject := action_item.TargetObject;
  end;
end;

procedure TTemplateDragProxy.Render(AState: TLocalState; x, y: integer);
var
  cx: Integer;
  cy: Integer;
begin
  cx := (x+1) * TILE_SIZE;
  cy := (y+1) * TILE_SIZE;

  FDraggingTemplate.RenderFloating(AState, cx, cy, FOwner.FCurrentPlayer);
end;

procedure TTemplateDragProxy.RenderOverlay(AState: TLocalState; x, y: integer);
var
  cx: Integer;
  cy: Integer;
begin
  cx := (x+1) * TILE_SIZE;
  cy := (y+1) * TILE_SIZE;

  //TODO: passability
  //FDraggingTemplate.RenderFloating(AState, cx, cy, FOwner.FCurrentPlayer);
end;

{ TfMain }

procedure TfMain.actCreateMapExecute(Sender: TObject);
var
  frm: TNewMapForm;
  params: TMapCreateParams;
begin
  if not CheckUnsavedMap then
    Exit;

  frm := TNewMapForm.Create(nil);
  try
    if frm.Execute(params) then
    begin
      FMapFilename := '';
      FreeAndNil(FMap);

      FMap := TVCMIMap.Create(fenv,params);
      FMap.Loaded;
      MapChanded;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfMain.actAnimateObjectsExecute(Sender: TObject);
begin
  AnimTimer.Enabled := actAnimateObjects.Checked;
end;

procedure TfMain.actClearActiveBrushExecute(Sender: TObject);
begin
  FToolsFrame.ActiveBrush.Clear;
end;

procedure TfMain.actDeleteExecute(Sender: TObject);
begin
  DeleteObject(FToolsFrame.SelectedObject);
  ClearSelection();
end;

procedure TfMain.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FToolsFrame.SelectedObject);
end;

procedure TfMain.actFilterArtifactsExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Artifact);
end;

procedure TfMain.actFilterCreaturesExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Creature);
end;

procedure TfMain.actFilterDwellingExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Dwelling);
end;

procedure TfMain.actFilterHeroesExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Hero);
end;

procedure TfMain.actFilterOnMapExecute(Sender: TObject);
begin
  DoObjectsSearch();
end;

procedure TfMain.actFilterOtherExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Other);
end;

procedure TfMain.actFilterResourcesExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Resource);
end;

procedure TfMain.actFilterStaticExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Static);
end;

procedure TfMain.actFilterTownsExecute(Sender: TObject);
begin
  DoObjectsCatSearch(TObjectCategory.Town);
end;

procedure TfMain.actMapOptionsExecute(Sender: TObject);
var
  f: TMapOptionsForm;
begin
  f := TMapOptionsForm.Create(Self);

  f.Map := FMap;
  f.ShowModal;
end;


procedure TfMain.actOpenMapExecute(Sender: TObject);
begin
  if CheckUnsavedMap() then
  begin
    if OpenMapDialog.Execute then
    begin
      LoadMap(OpenMapDialog.FileName);
    end;
  end;
end;

procedure TfMain.actPlayerOptionsExecute(Sender: TObject);
var
  f: TPlayerOptionsForm;
begin
  f := TPlayerOptionsForm.Create(self);
  f.Map := Fmap;
  f.ShowModal;
end;

procedure TfMain.actPropertiesExecute(Sender: TObject);
var
  action_item : TEditObject;
  p: TPoint;
begin
  Assert(Assigned(FToolsFrame.SelectedObject), 'actPropertiesExecute: No object selected');

  if not FToolsFrame.SelectedObject.HasOptions then
  begin
    MapViewNotifier.Title := 'Not editable';
    MapViewNotifier.Text := 'This object has no options to edit';

    p := Point(FMouseX, FMouseY);

    p := MapView.ClientToScreen(p);

    MapViewNotifier.ShowAtPos(p.x+20, p.y+20);
    exit;
  end;

  action_item := TEditObject.Create(FMap);
  action_item.TargetObject := FToolsFrame.SelectedObject;

  FUndoManager.ExecuteItem(action_item);
end;

procedure TfMain.actPropertiesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FToolsFrame.SelectedObject);
end;

procedure TfMain.actRedoExecute(Sender: TObject);
begin
  FUndoManager.Redo;
  InvalidateMapContent;
end;

procedure TfMain.actRedoUpdate(Sender: TObject);
var
  a: TAction;
begin
  a := (Sender as TAction);

  if FUndoManager.CanRedo then
  begin
    a.Enabled := True;
    a.Caption := rsRedo + ' ' + FUndoManager.PeekNext.Description;
  end else
  begin
    a.Enabled := False;
    a.Caption := rsRedo;
  end;
end;


procedure TfMain.actSaveMapAsExecute(Sender: TObject);
begin
  if SaveMapAsDialog.Execute then
  begin
    SaveMap(SaveMapAsDialog.FileName);
  end;
end;

procedure TfMain.actSaveMapExecute(Sender: TObject);
begin
  if (FMapFilename = '') and (FMap.Name = '') then
  begin
    actSaveMapAs.Execute;
  end
  else if (FMapFilename = '') and (FMap.Name <> '') then
  begin
    SaveMapAsDialog.FileName:=FMap.Name;
    if SaveMapAsDialog.Execute then
    begin
      SaveMap(SaveMapAsDialog.FileName);
    end;
  end
  else begin
    SaveMap(FMapFilename);
  end;
end;

procedure TfMain.actSaveMapUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FMap) and FMap.IsDirty {and not FUndoManager.IsCheckPoint};
end;

procedure TfMain.actUndoExecute(Sender: TObject);
begin
  FUndoManager.Undo;
  InvalidateMapContent;
end;

procedure TfMain.actUndoUpdate(Sender: TObject);
var
  a: TAction;
begin
  a := (Sender as TAction);

  if FUndoManager.CanUndo then
  begin
    a.Enabled := True;
    a.Caption := rsUndo +' '+ FUndoManager.PeekCurrent.Description;
  end else
  begin
    a.Enabled := False;
    a.Caption := rsUndo;
  end;

end;

procedure TfMain.actVictoryLossConditionsExecute(Sender: TObject);
var
  f: TTriggeredEventsForm;
begin
  f := TTriggeredEventsForm.Create(Self);

  f.Map := FMap;
  f.ShowModal;
end;

procedure TfMain.actVictoryLossConditionsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FMap);
end;

procedure TfMain.actViewGridExecute(Sender: TObject);
begin
  MapView.Invalidate;
end;

procedure TfMain.actViewPassabilityExecute(Sender: TObject);
begin
  MapView.Invalidate;
end;

procedure TfMain.actZoomInExecute(Sender: TObject);
begin
  SetZoomIndex(FZoomIndex + 1);
end;

procedure TfMain.actZoomInUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FMap) and (FZoomIndex < 3);
end;

procedure TfMain.actZoomOutExecute(Sender: TObject);
begin
  SetZoomIndex(FZoomIndex - 1);
end;

procedure TfMain.actZoomOutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FMap) and (FZoomIndex > 0);
end;

procedure TfMain.AnimTimerTimer(Sender: TObject);
begin
  if Visible and (WindowState<>wsMinimized) then
    MapView.Invalidate;
end;

procedure TfMain.ApplicationProperties1IdleEnd(Sender: TObject);
begin
  UpdateWidgets;
end;

function TfMain.CheckUnsavedMap: boolean;
var
  res: Integer;
begin
  Result := True;
  if Assigned(FMap) and FMap.IsDirty then
  begin
    res := MessageDlg(rsConfirm,rsMapChanged, TMsgDlgType.mtConfirmation, mbYesNoCancel,0);

    case res of
      mrYes:begin
        actSaveMap.Execute;
        Result := not FMap.IsDirty;
      end;
      mrCancel:Result := False ;
    end;
  end;
end;

procedure TfMain.FormActivate(Sender: TObject);
begin
  try
    MapView.MakeCurrent();
  finally
    RootManager.InitComplete;
  end;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckUnsavedMap();
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  dir: String;
  map_filename: String;
begin
  FObjectCellSize:=OBJ_CELL_SIZE;
  FObjectsPerRow:=OBJ_PER_ROW;
  FRealTileSize := TILE_SIZE;
  FObjectBorderWidth:=OBJ_BORDER_WIDTH;
  FObjectCellTotalSize:=FObjectCellSize+FObjectBorderWidth*2;

  FZbuffer := TZBuffer.Create;
  pnHAxis.DoubleBuffered := True;
  pnVAxis.DoubleBuffered := True;
  pnMinimap.DoubleBuffered := True;
  MapView.SharedControl := RootManager.SharedContext;
  ObjectsView.SharedControl := RootManager.SharedContext;

  FToolsFrame := TToolsFrame.Create(self);
  FToolsFrame.BorderStyle :=  bsNone;
  FToolsFrame.Align:=alClient;
  FToolsFrame.Parent := pnLeft;

  FResourceManager := RootManager.ResourceManager;
  FEnv.tm := RootManager.TerrainManager;
  FObjManager := RootManager.ObjectsManager;
  FEnv.om := FObjManager;

  FGraphicsManager := RootManager.GraphicsManager;
  FEnv.gm := FGraphicsManager;

  FEnv.lm := RootManager.ListsManager;

  FEnv.i18n := RootManager.LocaleManager;

  FUndoManager := TMapUndoManager.Create;

  FMinimap := TMinimap.Create(Self);

  FUndoManager.OnRegionInvalidated := @FMinimap.InvalidateRegion;
  FUndoManager.OnActionPerformed:=@UndoManagerOnActionPerformed;

  FMap := TVCMIMap.CreateDefault(FEnv);

  FCurrentPlayer := TPlayer.none;

  SetupPlayerSelection();

  FVisibleObjects := TVisibleObjects.Create();
  FVisibleObjectsValid:=false;

  FToolsFrame.SetVisibleObjects(FVisibleObjects);

  FObjectCategory:=TObjectCategory.Hero;

  FTemplatesResult := TObjectsSelection.Create();
  FMapObjectsResult := TMapObjectsSelection.Create();

  FMapViewState := TLocalState.Create(MapView);
  FObjectsViewState := TLocalState.Create(ObjectsView);

  //load map if specified

  if Paramcount > 0 then
  begin
    RootManager.ProgressForm.NextStage('Loading map ...');
    RootManager.ProgressForm.SetMax(1);
    try
      map_filename := UTF8Trim(ParamStrUTF8(1));

      if FileExistsUTF8(map_filename) then
      begin
        LoadMap(map_filename);
      end
      else
      begin
        dir := GetCurrentDirUTF8();
        dir := IncludeTrailingPathDelimiter(dir);
        map_filename:= dir + map_filename;
        if FileExistsUTF8(map_filename) then
          LoadMap(map_filename)
        else
          map_filename := '';
      end;
    except
      on e: Exception do
      begin
        Application.ShowException(e);
      end;
    end;
    RootManager.ProgressForm.Advance(1);
  end
  else
  begin
  end;

  MapChanded;
  DoObjectsSearch();
  UpdateWidgets;

  SetZoomIndex(3);

  FToolsFrame.SwitchToObjects;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMapViewState);
  FreeAndNil(FObjectsViewState);

  FZbuffer.Free;
  FMap.Free;

  FUndoManager.Free;

  FreeAndNil(FMapObjectsResult);
  FreeAndNil(FTemplatesResult);
  FreeAndNil(FVisibleObjects);
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  dx,dy: integer;
begin
  dx := 0;
  dy := 0;

  case Key of
    VK_UP: Dec(dy) ;
    VK_DOWN: Inc(dy);
    VK_LEFT: Dec(dx);
    VK_RIGHT: Inc(dx);
  end;

  if (dx<>0) or (dy<>0) then
  begin
    if Assigned(FToolsFrame.SelectedObject) then
    begin
      MoveObject(FToolsFrame.SelectedObject, FMap.CurrentLevelIndex, FToolsFrame.SelectedObject.X + dx, FToolsFrame.SelectedObject.Y + dy);
    end
    else
    begin

      if ssShift in Shift then
      begin
        dx *= FViewTilesH div 2 - 1;
        dy *= FViewTilesV div 2 - 1;
      end;

      ChangeMapPosition(Point(dx, dy));
    end;

    Key := VK_UNKNOWN;
    Exit;
  end;
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  InvalidateMapDimensions;
  InvalidateObjects;
  StatusBar.Panels[0].Width:=StatusBar.Width - 210;
end;

function TfMain.getMapHeight: Integer;
begin
  Result := 0;
  if Assigned(FMap) then
    Result := FMap.CurrentLevel.Height;
end;

function TfMain.getMapWidth: Integer;
begin
  Result := 0;
  if Assigned(FMap) then
     Result := FMap.CurrentLevel.Width;
end;

function TfMain.GetObjIdx(col, row: integer): integer;
begin
   result := col + FObjectsPerRow * (row + FObjectsVPos);
end;

function TfMain.GetActiveSelection: ISearchResult;
begin
  if actFilterOnMap.Checked then
  begin
    Result := FMapObjectsResult;
  end
  else
  begin
    Result := FTemplatesResult;
  end;
end;

procedure TfMain.HorisontalAxisPaint(Sender: TObject);
begin
  PaintAxis(TAxisKind.Horizontal,Sender as TPaintBox);
end;

procedure TfMain.hScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  ScrollPos := Min((sender as TScrollBar).Max - (sender as TScrollBar).PageSize +1,ScrollPos);
  ScrollPos := Max(ScrollPos, 0);
  FMapHPos := ScrollPos;
  InvalidateMapAxis;
end;

procedure TfMain.InvalidateMapAxis;
begin
  MapView.Invalidate;
  Minimap.Invalidate;
  InvalidateVisibleObjects;
  VerticalAxis.Invalidate;
  HorisontalAxis.Invalidate;
end;

procedure TfMain.InvalidateMapContent;
begin
  InvalidateVisibleObjects;
  MapView.Invalidate;
  Minimap.Invalidate;
end;

procedure TfMain.InvalidateVisibleObjects;
begin
  FVisibleObjectsValid:=false;
  if not Assigned(FMap) then
  begin
    FVisibleObjects.Clear;
  end;
end;

procedure TfMain.SetZoomIndex(AIndex: Integer);
begin
  FZoomIndex:=AIndex;
  SetTileSize(FTileSizes[FZoomIndex]);
end;

procedure TfMain.SetTileSize(ASize: Integer);
begin
  FRealTileSize:= ASize;
  FMapViewState.Scale := FRealTileSize / TILE_SIZE;
  InvalidateMapDimensions;
  InvalidateMapAxis;
end;

procedure TfMain.InvalidateMapDimensions;
var
  factor: Double;
  dim: Integer;
begin
  FViewTilesV := MapView.Height div FRealTileSize;
  FViewTilesH := MapView.Width div FRealTileSize;

  vScrollBar.Max := getMapHeight - 1;
  hScrollBar.Max := getMapWidth - 1;

  vScrollBar.PageSize := FViewTilesV;
  hScrollBar.PageSize := FViewTilesH;

  hScrollBar.LargeChange := Max(1,FViewTilesH);
  vScrollBar.LargeChange := Max(1,FViewTilesV);

  dim := max(getMapHeight(), getMapWidth());

  factor := Double(getMapHeight()) / dim;
  Minimap.Height := round(factor * pnMinimap.Height);

  factor := Double(getMapWidth()) / dim;

  Minimap.Width := round(factor * pnMinimap.Width);

  if (FMapHPos + FViewTilesH) > getMapWidth then
  begin
    FMapHPos := Max(0, getMapWidth-FViewTilesH);
  end;

  if (FMapVPos + FViewTilesV) > getMapHeight then
  begin
    FMapVPos := Max(0, getMapHeight-FViewTilesV);
  end;

  InvalidateVisibleObjects;
end;

procedure TfMain.InvalidateObjects;
var
  ActiveSelection: ISearchResult;
begin
  ActiveSelection := GetActiveSelection;

  FObjectCount := ActiveSelection.Count;

  FObjectRows := FObjectCount div 3;
  FObjectReminder := FObjectCount mod 3;
  if FObjectReminder > 0 then inc(FObjectRows);

  sbObjects.Min := 0;
  sbObjects.Max := Max(0, FObjectRows - 1);

  sbObjects.PageSize := ObjectsView.Height div FObjectCellTotalSize;
  sbObjects.Position:=0;

  FViewObjectRowsH := sbObjects.PageSize;
  InvalidateObjPos;
end;

procedure TfMain.InvalidateObjPos;
begin
  FObjectsVPos := sbObjects.Position;
  ObjectsView.Invalidate;
end;

procedure TfMain.LoadMap(AFileName: string);
var
  file_ext: String;
  new_map: TVCMIMap;
  reader: IMapReader;
  stm: TFileStreamUTF8;
  cstm: TStream;
  set_filename, is_compressed: Boolean;
  magic: dword;
begin
  cstm := nil;
  set_filename := False;
  is_compressed := False;

  file_ext := Trim(UpperCase(ExtractFileExt(AFileName)));

  stm := TFileStreamUTF8.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  stm.Seek(0,soBeginning);

  try
    case file_ext of
      FORMAT_H3M_EXT:
        begin
          reader := TMapReaderH3m.Create(FEnv);
          magic := 0;
          stm.Read(magic,SizeOf(magic));
          stm.Seek(0,soBeginning);

          if (magic and $ffffff) = $00088B1F then
          begin
             cstm := TZlibInputStream.CreateGZip(FZbuffer, stm,0);
             is_compressed := true;
          end
          else begin
             cstm := stm;
          end;
        end;
      FORMAT_VCMI_EXT:
      begin
         reader := TMapReaderZIP.Create(FEnv);
         cstm := stm;
         set_filename := True; //support saving
      end
      else
        begin
          raise Exception.Create('Unsuported map extension');
        end;
    end;

    new_map := reader.Read(cstm);
  finally
    FreeAndNil(stm);
    if is_compressed then FreeAndNil(cstm);
  end;

  FreeAndNil(FMap); //destroy old map
  FMap := new_map;
  FMap.CurrentLevelIndex := 0;
  if set_filename then FMapFilename := AFileName;

  MapChanded;
end;

procedure TfMain.MapChanded;
begin
  FUndoManager.Clear;
  ClearSelection();

  FMinimap.Map := FMap;
  InvalidateMapDimensions;
  InvalidateMapContent;

  FMapObjectsResult.Clear;

  if actFilterOnMap.Checked then
  begin
    InvalidateObjects;
  end;

  FUndoManager.Map := FMap;
  SetupLevelSelection;
end;

procedure TfMain.DoSetMapLevelIndex(ANewIndex: Integer);
begin
  FMap.CurrentLevelIndex := ANewIndex;
  ClearSelection();
  InvalidateMapDimensions;
  InvalidateMapContent;
end;

procedure TfMain.DoMapLevelWheelScroll(WheelDelta: Integer);
var
  direction : Integer;

  old_level, new_level: integer;
begin
  direction:= - Sign(WheelDelta);

  old_level := FMap.CurrentLevelIndex;

  new_level := old_level + direction;

  if (new_level >=0) and (new_level < FMap.MapLevels.Count) then
  begin
    DoSetMapLevelIndex(new_level);
  end;
end;

procedure TfMain.DoMapViewWheelScroll(sb: TScrollBar; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint);
begin
  sb.Position := sb.Position - Sign(WheelDelta) * 3;

  sb.Position := Min(sb.Max-sb.PageSize+1,sb.Position);

  if ssShift in Shift then
  begin
    FMapHPos := sb.Position;
  end
  else begin
    FMapVPos := sb.Position;
  end;

  SetMapViewMouse(MousePos.x,MousePos.Y);

  InvalidateMapAxis;

end;

procedure TfMain.MapViewClick(Sender: TObject);
begin
  MapViewNotifier.Hide;
end;

procedure TfMain.MapViewDblClick(Sender: TObject);
begin
  //
end;

procedure TfMain.MapViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if not Assigned(FMap) then
    exit;
  FToolsFrame.SwitchToObjects;

  SetMapViewMouse(x,y);

  FDragging.DropOnMap;

  InvalidateMapContent;
end;

procedure TfMain.MapViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not Assigned(FMap) then
  begin
    Accept:=false;
    exit;
  end;

  FToolsFrame.SwitchToObjects;

  //TODO: handle accceptible terrain
  //TODO: handle activity-blocking intersection
  Accept := true;

  SetMapViewMouse(x,y);

  case State of
    TDragState.dsDragEnter: FMapDragging := True ;
    TDragState.dsDragLeave: FMapDragging := False ;
    TDragState.dsDragMove: FMapDragging := True ;
  end;

  MapView.Invalidate;
end;

procedure TfMain.MapViewMakeCurrent(Sender: TObject; var Allow: boolean);
begin

end;

procedure TfMain.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  q: TMapObjectQueue;
  o: TMapObject;
  DragStarted: Boolean;

   procedure PerformStartDrag();
   begin
     if not DragStarted then
     begin
       if Assigned(FToolsFrame.SelectedObject)
         and (FToolsFrame.SelectedObject.CoversTile(FMap.CurrentLevelIndex,FMouseTileX,FMouseTileY, TSelectObjectBy.BBox)) then
       begin
         //MapView.Cursor:=crHandPoint;
         FNextDragSubject:=TDragSubject.MapObject;
         DragManager.DragStart(self,False, TILE_SIZE div 2);
         DragStarted := true;
       end;
     end;
   end;
begin
  if not Assigned(FMap) then
    exit;

  SetMapViewMouse(x,y);

  if Button = TMouseButton.mbLeft then
  begin
    //todo: move to TIdleMapBrush
    if FToolsFrame.ActiveBrush = FToolsFrame.IdleBrush then
    begin
      DragStarted:=False;

      PerformStartDrag();

      q := TMapObjectQueue.Create;
      try
        FMap.SelectObjectsOnTile(FVisibleObjects.Data, FMap.CurrentLevelIndex,FMouseTileX,FMouseTileY,q);

        if Assigned(FToolsFrame.SelectedObject)
          and (FToolsFrame.SelectedObject.CoversTile(FMap.CurrentLevelIndex,FMouseTileX,FMouseTileY, TSelectObjectBy.BBox))
          then
        begin
          //select next object
          o := nil;
          while not q.IsEmpty do
          begin
            o := q.Top;
            q.Pop;

            if (q.IsEmpty) or (q.Top = FToolsFrame.SelectedObject) then
            begin
              break;
            end;

          end;
          FToolsFrame.SelectedObject := o;
        end
        else
        begin
          if q.IsEmpty then
            FToolsFrame.SelectedObject := nil
          else
            FToolsFrame.SelectedObject := q.Top;
        end;
        PerformStartDrag();
      finally
        q.Free;
      end;
    end
    else
    begin
      FToolsFrame.ActiveBrush.TileMouseDown(fmap, FMouseTileX,FMouseTileY);
    end;
  end
  else if Button = TMouseButton.mbRight then
  begin
    FMouseDTileX := FMouseTileX;
    FMouseDTileY := FMouseTileY;
  end;
end;

procedure TfMain.MapViewMouseEnter(Sender: TObject);
begin
  ResetFocus;
end;

procedure TfMain.MapViewMouseLeave(Sender: TObject);
begin
  if not Assigned(FMap) then
    exit;

  InvalidateMapAxis;

  FToolsFrame.ActiveBrush.MouseLeave(FMap);
end;

procedure TfMain.MapViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  FOldTileX: Integer;
  FOldTileY: Integer;
begin
  if not Assigned(FMap) then
    exit;

  ResetFocus;

  FOldTileX := FMouseTileX;
  FOldTileY := FMouseTileY;
  SetMapViewMouse(x,y);

  if (FOldTileX<>FMouseTileX) or (FOldTileY<>FMouseTileY) then
  begin
    InvalidateMapAxis;
  end;
  FToolsFrame.ActiveBrush.TileMouseMove(fmap, FMouseTileX, FMouseTileY);
end;

procedure TfMain.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FMap) then
    exit;

  if Button = TMouseButton.mbLeft then
  begin
    FToolsFrame.ActiveBrush.TileMouseUp(fmap, FMouseTileX, FMouseTileY);
    FToolsFrame.ActiveBrush.Execute(FUndoManager, FMap);
    InvalidateMapContent;
  end;

  if Button = TMouseButton.mbRight then
  begin
    if (FMouseDTileX = FMouseTileX) and (FMouseDTileY = FMouseTileY) then
    begin
      if Assigned(FToolsFrame.SelectedObject) and FToolsFrame.SelectedObject.CoversTile(FMap.CurrentLevelIndex, FMouseDTileX, FMouseDTileY, TSelectObjectBy.BBox) then
      begin
        actProperties.Execute;
      end;
    end;
  end;
end;

procedure TfMain.MapViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  ss: TShiftState;
begin
  if not Assigned(FMap) then
    exit;

  ss := [ssShift, ssCtrl, ssAlt, ssAltGr] * Shift;
  Handled := True;

  if ss = [ssShift] then
    DoMapViewWheelScroll(hScrollBar, Shift, WheelDelta, MousePos)
  else if ss = [] then
    DoMapViewWheelScroll(vScrollBar, Shift, WheelDelta, MousePos)
  else if ss = [ssCtrl] then
    DoMapLevelWheelScroll(WheelDelta)
  else
    Handled := false;
end;

procedure TfMain.MapViewPaint(Sender: TObject);
var
  scissor_x: Integer;
  scissor_w: Integer;
  scissor_h: Integer;
  scissor_y: Integer;

  new_tick: Int64;
  new_anim_frame: Boolean;
begin
  if not FMapViewState.StartFrame then
    Exit;

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);

  if not Assigned(FMap) then
    exit;

  if not FVisibleObjectsValid then
  begin
    FMap.SelectVisibleObjects(FVisibleObjects, FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);
    FVisibleObjectsValid := True;
  end;

  FMapViewState.UseTextures(true, true);

  FMapViewState.SetOrtho(FRealTileSize * FMapHPos, MapView.Width + FRealTileSize * FMapHPos,
    MapView.Height + FRealTileSize * FMapVPos, FRealTileSize * FMapVPos);

  scissor_x := 0;
  scissor_y := ifthen(FMapVPos + FViewTilesV >= getMapHeight(), MapView.Height mod FRealTileSize, 0);

  scissor_w := ifthen(FMapHPos + FViewTilesH >= getMapWidth(), FViewTilesH * FRealTileSize, MapView.Width);
  scissor_h := ifthen(FMapVPos + FViewTilesV >= getMapHeight(),  FViewTilesV * FRealTileSize, MapView.Height);

  FMapViewState.EnableScissor();
  glScissor(scissor_x,scissor_y, scissor_w,scissor_h);

  FMapViewState.StartDrawingSprites;

  FMapViewState.SetUseFlag(false);
  FMap.RenderTerrain(FMapViewState, FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);

  new_tick := LazUTF8SysUtils.GetTickCount64;

  new_anim_frame := false;

  if abs(new_tick - FAnimTick) > 155 then
  begin
    new_anim_frame := true;

    FAnimTick := new_tick;
  end;

  FVisibleObjects.RenderAnimation(FMapViewState, actAnimateObjects.Checked, new_anim_frame);


  if FMapDragging then
  begin
    Assert(Assigned(FDragging));
    FMapViewState.StartDrawingSprites;
    FMapViewState.UseTextures(True, True);

    FDragging.Render(FMapViewState, FMouseTileX, FMouseTileY);
  end;

  if actViewPassability.Checked then
  begin
    FVisibleObjects.RenderOverlay(FMapViewState);

    if FMapDragging then
      FDragging.RenderOverlay(FMapViewState, FMouseTileX, FMouseTileY);
  end;

  glLineWidth(1);
  //glEnable(GL_LINE_SMOOTH);

  if actViewGrid.Checked then
  begin
    RenderGrid();
  end;

  RenderSelection;

  FToolsFrame.ActiveBrush.RenderCursor(FMapViewState, fmap, FMouseTileX, FMouseTileY);

  FMapViewState.DisableScissor;
  //glDisable(GL_LINE_SMOOTH);

  FMapViewState.FinishFrame;
end;

procedure TfMain.MapScrollByMinimap(x,y: integer);
var
  cx,cy: Double;
  pos: TPoint;
begin
  //set map view center position to mouse pos

  cx := x / (Minimap.Width);
  cy := y / (Minimap.Height);

  pos.x := round(cx * FMap.CurrentLevel.Width);
  pos.y := round(cy * FMap.CurrentLevel.Height);

  pos.x := pos.x- (FViewTilesH div 2);
  pos.y := pos.y- (FViewTilesV div 2);

  pos.x := Max(pos.x, 0);
  pos.y := Max(pos.y, 0);

  SetMapPosition(pos);
end;

procedure TfMain.MapScrollToObject(AObject: TMapObject);
var
  pos: TPoint;
begin
  DoSetMapLevelIndex(AObject.L);
  pos.x:=AObject.X;
  pos.y:=AObject.Y;

  pos.x := pos.x - FViewTilesH div 2;
  pos.y := pos.y - FViewTilesV div 2;

  FToolsFrame.SelectedObject := AObject;
  SetMapPosition(pos);
end;

procedure TfMain.MinimapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FMap) then
  begin
    Exit;
  end;

  if (Button = TMouseButton.mbLeft) and ([ssShift,ssCtrl,ssAlt] * Shift = []) then
  begin
    MapScrollByMinimap(X,Y);
  end;
end;

procedure TfMain.MinimapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    MapScrollByMinimap(X,Y);
  end;
end;

procedure TfMain.ObjectsSearchButtonClick(Sender: TObject);
begin
  DoObjectsSearch();
end;

procedure TfMain.ObjectsSearchChange(Sender: TObject);
begin
  SearchTimer.Enabled:=false;
  SearchTimer.Enabled:=true;
end;

procedure TfMain.ObjectsSearchEditingDone(Sender: TObject);
begin
  DoObjectsSearch();
end;

procedure TfMain.ObjectsViewClick(Sender: TObject);
begin
  //
end;

procedure TfMain.ObjectsViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //
end;

procedure TfMain.ObjectsViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := false;
end;

procedure TfMain.pcMainChange(Sender: TObject);
begin

end;

procedure TfMain.PlayerMenuClick(Sender: TObject);
var
  m : TMenuItem;
begin
  m := Sender as TMenuItem;

  SetCurrentPlayer(TPlayer(m.Tag));
end;

procedure TfMain.LevelMenuClick(Sender: TObject);
var
  m, mc : TMenuItem;
  i: Integer;
begin
  m := Sender as TMenuItem;

  for i := 0 to menuLevel.Count - 1 do
  begin
    mc := menuLevel[i];

    if mc=m then
    begin
      DoSetMapLevelIndex(M.Tag);
      mc.Checked := True;
    end
    else begin
      mc.Checked := False;
    end;
  end;
end;

procedure TfMain.MinimapPaint(Sender: TObject);
var
  radar_rect: TRect;
begin
  radar_rect.Top:=FMapVPos;
  radar_rect.Left:=FMapHPos;
  radar_rect.Bottom:=FMapVPos+FViewTilesV;
  radar_rect.Right:=FMapHPos+FViewTilesH;

  FMinimap.Paint(Sender as TPaintBox, radar_rect);
end;

procedure TfMain.ObjectsViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col: Integer;
  row: Integer;
  o_idx: Integer;
  obj: TMapObject;
begin
  col := x div FObjectCellTotalSize;
  row := y div FObjectCellTotalSize;

  o_idx := GetObjIdx(col, row);

  if (Button = TMouseButton.mbLeft) and (o_idx < FObjectCount) then
  begin
    if actFilterOnMap.Checked then
    begin
      obj := FMapObjectsResult.Objcts[o_idx];
      MapScrollToObject(obj);
    end
    else
    begin
      FNextDragSubject:=TDragSubject.MapTemplate;
      FSelectedTemplate := FTemplatesResult.Objcts[o_idx];
      ClearSelection;
      DragManager.DragStart(self, False,10); //after state change
    end;

    FToolsFrame.SwitchToObjects;
  end;
end;

procedure TfMain.ObjectsViewPaint(Sender: TObject);
const
  GRID_COLOR: TRBGAColor = (r:200; g:200; b:200; a:255);
var
  row: Integer;
  col: Integer;
  o_idx: Integer;
  cx: Integer;
  cy, sh: Integer;
  ActiveSelection: ISearchResult;
begin
  if not FObjectsViewState.StartFrame then
  begin
    Exit;
  end;

  glClearColor(255, 255, 255, 0);

  FObjectsViewState.UseTextures(False, false);

  FObjectsViewState.SetOrtho(0, ObjectsView.Width + 0, ObjectsView.Height + 0, 0);

  glClear(GL_COLOR_BUFFER_BIT);

  FObjectsViewState.EnableScissor();
  FObjectsViewState.SetScissor();

  FObjectsViewState.StartDrawingRects;
  FObjectsViewState.SetFragmentColor(GRID_COLOR);

  cy := 0;

  glLineWidth(FObjectBorderWidth);

  sh := FObjectBorderWidth;

  for row := 0 to FViewObjectRowsH + 1 do
  begin
    cx := 0;
    for col := 0 to FObjectsPerRow - 1 do
    begin
      o_idx := GetObjIdx(col, row);

      if o_idx >= FObjectCount then
        break;

      FObjectsViewState.RenderRect(cx+sh ,cy+sh,FObjectCellTotalSize-sh,FObjectCellTotalSize-sh);

      cx += FObjectCellTotalSize;
    end;

    cy += FObjectCellTotalSize;
  end;

  FObjectsViewState.StopDrawing;

  FObjectsViewState.UseTextures(true, true);
  FObjectsViewState.StartDrawingSprites();

  cy := 0;

  ActiveSelection := GetActiveSelection();

  for row := 0 to FViewObjectRowsH + 1 do
  begin
    cx := 0;
    for col := 0 to FObjectsPerRow - 1 do
    begin
      o_idx := GetObjIdx(col, row);

      if o_idx >= FObjectCount then
        break;

      ActiveSelection.RenderIcon(o_idx,FObjectsViewState, cx+FObjectBorderWidth, cy+FObjectBorderWidth, FObjectCellSize, FCurrentPlayer);
      cx += FObjectCellTotalSize;
    end;

    cy += FObjectCellTotalSize;
  end;

  FObjectsViewState.DisableScissor();
  FObjectsViewState.FinishFrame;
end;

procedure TfMain.PaintAxis(Kind: TAxisKind; Axis: TPaintBox);
var
  ctx: TCanvas;
  i:   Integer;
  tiles, tmp: Integer;
  text_width: Integer;
  txt: string;
  ofs, text_scale: Integer;

  img: TBitmap;
begin
  case Kind of
    TAxisKind.Horizontal: tmp := Axis.Width;
    TAxisKind.Vertical: tmp := Axis.Height;
  end;

  tiles := tmp div FRealTileSize;

  img := TBitmap.Create;

  img.SetSize(Axis.Width,Axis.Height);

  ctx := img.Canvas;

  case Kind of
    TAxisKind.Horizontal: tmp := FMapHPos;
    TAxisKind.Vertical: tmp := FMapVPos;
  end;

  txt := IntToStr(tmp + tiles);//max number
  text_width := ctx.GetTextWidth(txt);

  text_scale := 1 + (text_width div FRealTileSize);

  try
    ctx.Brush.Color := clWhite;
    ctx.FillRect(0, 0, Axis.Width, Axis.Height);

    for i := 0 to tiles do
    begin
      if (text_scale > 1) and (i mod text_scale <> 0) then
      begin
        Continue;
      end;

      txt := IntToStr(tmp + i);
      text_width := ctx.GetTextWidth(txt);

      case Kind of
        TAxisKind.Horizontal: ofs := (FRealTileSize - text_width) div 2;
        TAxisKind.Vertical: ofs :=(FRealTileSize + text_width) div 2;
      end;

      case Kind of
        TAxisKind.Horizontal: begin
          if (FMapHPos + 1 + i) <= FMap.CurrentLevel.Width then
          begin
            ctx.TextOut(I * FRealTileSize + ofs, 0, txt);
          end;
        end;
        TAxisKind.Vertical: begin
          if (FMapVPos + 1 + i) <= FMap.CurrentLevel.Height then
          begin
            ctx.Font.Orientation := 900;
            ctx.TextOut(0, I * FRealTileSize + ofs, txt);
            ctx.Font.Orientation := 0;
          end;
        end;
      end;
    end;

    axis.Canvas.Changing;
    Axis.Canvas.Draw(0,0,img);
    Axis.Canvas.Changed;
  finally
    img.Free;
  end;
end;

procedure TfMain.ObjectsViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  delta, pos_old, pos_new: integer;
begin
  delta:= - Sign(WheelDelta) * sbObjects.PageSize;
  pos_old := sbObjects.Position;
  pos_new := pos_old + delta;

  //do not scroll if we cannot scroll full page
  if (pos_new >=sbObjects.Min) and (pos_new <= sbObjects.Max) then
  begin
    sbObjects.Position := pos_new;
    InvalidateObjPos;
  end;

  Handled := true;
end;

procedure TfMain.pbObjectsResize(Sender: TObject);
begin
  InvalidateObjects;
end;

procedure TfMain.pcToolBoxChange(Sender: TObject);
begin
  ClearSelection();
end;

procedure TfMain.RenderGrid;
const
  GRID_COLOR: TRBGAColor = (r:255; g:255; b:255; a:125);
var
  i, j: Integer;
begin
  FMapViewState.UseTextures(false, false);
  FMapViewState.StartDrawingRects;
  FMapViewState.SetFragmentColor(GRID_COLOR);

  for i := FMapHPos to FMapHPos + FViewTilesH do
  begin
    for j := FMapVPos to FMapVPos + FViewTilesV do
    begin
      FMapViewState.RenderRect(i*TILE_SIZE,j*TILE_SIZE, TILE_SIZE, TILE_SIZE);
    end;
  end;
end;

procedure TfMain.RenderSelection;
begin
  FMapViewState.UseTextures(False, False);
  FMapViewState.StartDrawingRects;

  if Assigned(FToolsFrame.SelectedObject) then
  begin
    FToolsFrame.SelectedObject.RenderSelectionRect(FMapViewState);
  end;

  FToolsFrame.ActiveBrush.RenderSelection(FMapViewState);
end;

procedure TfMain.SaveMap(AFileName: string);
var
  writer: IMapWriter;
  stm: TFileStreamUTF8;
  file_ext: String;
begin
  if (FMapFilename <> AFileName) and FileExistsUTF8(AFileName) then
  begin
    if MessageDlg(rsConfirm,rsFileExists, TMsgDlgType.mtConfirmation, mbYesNo,0) <> mrYes then
      exit;
  end;

  file_ext := Trim(UpperCase(ExtractFileExt(AFileName)));

  stm := TFileStreamUTF8.Create(AFileName,fmCreate);
  stm.Size := 0;

  try
   case file_ext of
    FORMAT_VCMI_EXT:
    begin
       writer := TMapWriterZIP.Create(FEnv);
    end
    else
      begin
        raise Exception.Create('Unsuported map extension');
      end;
    end;

    FMap.SaveToStream(stm,writer);
    FMapFilename := AFileName;
    FUndoManager.MarkCheckPoint;
  finally
    stm.Free;
  end;
end;

procedure TfMain.sbObjectsScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FObjectsVPos := ScrollPos;
  ObjectsView.Invalidate;
end;

procedure TfMain.SearchTimerTimer(Sender: TObject);
begin
  DoObjectsSearch();
end;

procedure TfMain.SetCurrentPlayer(APlayer: TPlayer);
var
  i: Integer;
  mc: TMenuItem;
begin
  FCurrentPlayer := APlayer;

  for i := 0 to menuPlayer.Count - 1 do
  begin
    mc := menuPlayer[i];
    if mc.Tag = PtrInt(APlayer) then
      mc.Checked := True
    else
      mc.Checked := False;
  end;

  InvalidateObjects;
end;

procedure TfMain.UpdateWidgets;
var
  s, status_text: String;
  t: PMapTile;
begin
  if Assigned(FMap) then
  begin
    s := '';
    if FMap.IsDirty then
      s := '*';
    Caption := FMap.Name + s + ' - VCMI editor'
  end
  else
    Caption := 'VCMI editor';

  status_text := '';

  if Assigned(FMap) then
  begin
    if FMap.IsOnMap(FMap.CurrentLevelIndex, FMouseTileX, FMouseTileY) then
    begin
      t := FMap.CurrentLevel.Tile[FMouseTileX, FMouseTileY];

      status_text := Format('[%d %d %d] %s %d %s',[FMouseTileX, FMouseTileY, FMap.CurrentLevelIndex, TERRAIN_CODES[t^.TerType], t^.TerSubType, FLIP_CODES[t^.Flags mod 4]]);
    end;
  end;

  if Assigned(FToolsFrame.SelectedObject) then
  begin
    if status_text <> '' then
      status_text += ' | ';

    status_text += FToolsFrame.SelectedObject.FormatDisplayName(FObjManager.FormatObjectName(FToolsFrame.SelectedObject.&Type, FToolsFrame.SelectedObject.Subtype));
  end;

  StatusBar.Panels[0].Text:= status_text;

  status_text := '';

  if Assigned(FMap) and (FMap.CurrentLevelIndex >=0) then
  begin
    status_text := Format('Level %d %dx%d',[FMap.CurrentLevelIndex, FMap.CurrentLevel.Width,FMap.CurrentLevel.Height])
  end;

  StatusBar.Panels[1].Text:=status_text;

  StatusBar.Panels[2].Text:=FEnv.lm.PlayerName[FCurrentPlayer];
end;

procedure TfMain.ResetFocus;
begin
  //if ObjectsSearch.Focused then
  //begin
  //  ObjectsSearch.EditingDone;
  //end;
  //
  //FocusControl(nil);
end;

procedure TfMain.DoObjectsSearch;
begin
  SearchTimer.Enabled:=false;

  if actFilterOnMap.Checked then
  begin
    if Assigned(FMap) then
    begin
      FMap.SelectByKeywords(FMapObjectsResult, ObjectsSearch.Text, FObjectCategory);
    end
    else
    begin
      FMapObjectsResult.Clear;
    end;
  end
  else
  begin
    FObjManager.SelectByKeywords(FTemplatesResult, ObjectsSearch.Text, FObjectCategory);
  end;

  InvalidateObjects;
end;

procedure TfMain.DoObjectsCatSearch(ACategory: TObjectCategory);
begin
  FObjectCategory := ACategory;
  DoObjectsSearch();
end;

procedure TfMain.UndoManagerOnActionPerformed(AItem: TAbstractUndoItem);
begin
  if actFilterOnMap.Checked then
  begin
    DoObjectsSearch;
  end;
end;

procedure TfMain.ClearSelection;
begin
  FToolsFrame.SelectedObject := nil;
end;

procedure TfMain.SetMapPosition(APosition: TPoint);
   procedure UpdateScrollbar(sb: TCustomScrollBar; var APosition: Integer);
   begin
     APosition := Max(APosition,0);
     APosition := Min(sb.Max-sb.PageSize+1,APosition);
     APosition := Max(APosition,0);
     sb.Position := APosition;
   end;
begin
  UpdateScrollbar(hScrollBar,APosition.x);
  FMapHPos := APosition.x;

  UpdateScrollbar(vScrollBar,APosition.y);
  FMapVPos := APosition.y;

  InvalidateMapAxis;
end;

procedure TfMain.ChangeMapPosition(ADelta: TPoint);
var
  new_pos: TPoint;
begin
  new_pos.x:=FMapHPos + ADelta.x;
  new_pos.y:=FMapVPos + ADelta.y;
  SetMapPosition(new_pos);
end;

procedure TfMain.SetMapViewMouse(x, y: integer);
var
  ofs_x: Integer;
  ofs_y: Integer;
begin
  //x,y in viewport coords

  FMouseX := x;
  FMouseY := y;

  ofs_x := X div FRealTileSize;
  ofs_y := Y div FRealTileSize;

  FMouseTileX := FMapHPos + ofs_x;
  FMouseTileY := FMapVPos + ofs_y;

end;

procedure TfMain.SetupPlayerSelection;
var
  m: TMenuItem;
  p: TPlayer;
begin
  menuPlayer.Clear;

  m := TMenuItem.Create(menuPlayer);

  m.Tag := Integer(TPlayer.none);
  m.OnClick := @PlayerMenuClick;
  m.Checked := True;
  m.Caption := FEnv.lm.PlayerName[TPlayer.none];
  m.ShortCut:= scCtrl+VK_0;
  menuPlayer.Add(m);

  for p in [TPlayer.RED..TPlayer.PINK] do
  begin
    m := TMenuItem.Create(menuPlayer);

    m.Tag := Integer(p);
    m.OnClick := @PlayerMenuClick;
    m.Caption := FEnv.lm.PlayerName[p];
     m.ShortCut:= scCtrl+(VK_1+Integer(p));
    menuPlayer.Add(m);
  end;
end;

procedure TfMain.SetupLevelSelection;
var
  m: TMenuItem;
  l: TMapLevel;
  iter: TCollectionItem;
begin
  menuLevel.Clear;

  for iter in FMap.MapLevels do
  begin
    l := iter as TMapLevel;
    m := TMenuItem.Create(menuLevel);

    m.Tag := Integer(l.Index);
    m.OnClick := @LevelMenuClick;
    m.Caption := l.DisplayName;

    if FMap.CurrentLevelIndex = l.Index then
    begin
      m.Checked:=true;
    end;

    menuLevel.Add(m);
  end;

end;


procedure TfMain.DoStartDrag(var DragObject: TDragObject);
begin
  case FNextDragSubject of
    TDragSubject.MapObject: DragObject := TObjectDragProxy.Create(self, FToolsFrame.SelectedObject, FMouseTileX, FMouseTileY);
    TDragSubject.MapTemplate: DragObject := TTemplateDragProxy.Create(self, FSelectedTemplate);
  else
  inherited DoStartDrag(DragObject);
  end;
end;

procedure TfMain.DragCanceled;
begin
  inherited DragCanceled;
  FMapDragging:=false;
  //MapView.Cursor:=crDefault;
end;

procedure TfMain.MoveOrCopyObject(AObject: TMapObject; l, x, y: integer);
begin
  if ssCtrl in GetKeyShiftState then
  begin
    CopyObject(AObject, l, x, y);
  end
  else
  begin
    MoveObject(AObject, l, x, y);
  end;
end;

procedure TfMain.CopyObject(AObject: TMapObject; l, x, y: integer);
var
  action_item: TCopyObject;
begin
  action_item := TCopyObject.Create(FMap);
  action_item.Source := AObject;

  action_item.L := l;
  action_item.X := x;
  action_item.Y := y;

  FUndoManager.ExecuteItem(action_item);

  FToolsFrame.SelectedObject := action_item.TargetObject;
  InvalidateMapContent;
end;

procedure TfMain.MoveObject(AObject: TMapObject; l, x, y: integer);
var
  action_item: TMoveObject;
begin
  action_item := TMoveObject.Create(FMap);

  action_item.TargetObject := AObject;

  action_item.L := l;
  action_item.X := x;
  action_item.Y := y;

  FUndoManager.ExecuteItem(action_item);

  FToolsFrame.SelectedObject := AObject;
  InvalidateMapContent;
end;

procedure TfMain.DeleteObject(AObject: TMapObject);
var
  action_item: TDeleteObjects;
begin
  action_item := TDeleteObjects.Create(FMap);
  action_item.Targets.Add(AObject);
  FUndoManager.ExecuteItem(action_item);
  InvalidateMapContent;
end;

procedure TfMain.VerticalAxisPaint(Sender: TObject);
begin
  PaintAxis(TAxisKind.Vertical,Sender as TPaintBox);
end;

procedure TfMain.vScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  ScrollPos := Min((sender as TScrollBar).Max - (sender as TScrollBar).PageSize +1,ScrollPos);
  ScrollPos := Max(ScrollPos, 0);
  FMapVPos := ScrollPos;
  InvalidateMapAxis;
end;

end.
