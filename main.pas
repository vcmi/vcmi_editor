{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

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
unit main;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, GL, OpenGLContext, LCLType, Forms, Controls,
  Graphics, GraphType, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, ComCtrls,
  Buttons, Map, terrain, editor_types, undo_base, map_actions, objects,
  editor_graphics, minimap, filesystem, filesystem_base, lists_manager,
  zlib_stream, editor_gl, gpriorityqueue, types;

type
  TAxisKind = (Vertical,Horizontal);

  TDragSubject = (MapObject, MapTemplate);
  TfMain = class;
  { TDragProxy }

  TDragProxy = class(TDragObject)
  protected
    FOwner: TFMain;
  public
    constructor Create(AOwner: TfMain); reintroduce;
    destructor Destroy; override;
    procedure Drop; virtual; abstract;
    procedure Render(x,y: integer); virtual; abstract;
  end;

  { TTemplateDragProxy }

  TTemplateDragProxy = class(TDragProxy)
  private
    FDraggingTemplate: TObjTemplate;
  public
    constructor Create(AOwner: TfMain;ADraggingTemplate: TObjTemplate);

    procedure Drop; override;
    procedure Render(x, y: integer); override;
  end;

  { TObjectDragProxy }

  TObjectDragProxy = class(TDragProxy)
  private
    FShiftX, FShiftY: Integer;
    FDraggingObject: TMapObject;
  public
    constructor Create(AOwner: TfMain;ADraggingObject: TMapObject; CurrentX, CurrentY: integer);
    procedure Drop; override;
    procedure Render(x, y: integer); override;
  end;

  { TfMain }

  TfMain = class(TForm)
    actHeroes: TAction;
    actArtifacts: TAction;
    actDelete: TAction;
    actProperties: TAction;
    actRivers: TAction;
    actRoads: TAction;
    actMonsters: TAction;
    actUnderground: TAction;
    actMapOptions: TAction;
    actSaveMapAs: TAction;
    actTerrain: TAction;
    actObjects: TAction;
    imlMainActionsSmall: TImageList;
    MainActions: TActionList;
    actCreateMap: TAction;
    actRedo: TAction;
    actUndo: TAction;
    actSaveMap: TAction;
    actOpenMap: TAction;
    ApplicationProperties1: TApplicationProperties;
    btnBrush1: TSpeedButton;
    btnRock: TSpeedButton;
    btnWater: TSpeedButton;
    btnSnow: TSpeedButton;
    btnRough: TSpeedButton;
    btnSand: TSpeedButton;
    btnGrass: TSpeedButton;
    btnSub:  TSpeedButton;
    btnSwamp: TSpeedButton;
    btnLava: TSpeedButton;
    GroupBox1: TGroupBox;
    imlMainActions: TImageList;
    lbBrush: TLabel;
    itmFreateMap: TMenuItem;
    MenuEdit: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mm:      TMainMenu;
    menuFile: TMenuItem;
    MapView: TOpenGLControl;
    ObjectsView: TOpenGLControl;
    OpenMapDialog: TOpenDialog;
    pcToolBox: TPageControl;
    HorisontalAxis: TPaintBox;
    btnDirt: TSpeedButton;
    btnBrush2: TSpeedButton;
    btnBrush4: TSpeedButton;
    btnBrushFill: TSpeedButton;
    btnBrushArea: TSpeedButton;
    menuPlayer: TPopupMenu;
    SaveMapAsDialog: TSaveDialog;
    sbObjects: TScrollBar;
    StatusBar: TStatusBar;
    AnimTimer: TTimer;
    ActionsToolBar: TToolBar;
    tbSelectPlayer: TToolButton;
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
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tsObjects: TTabSheet;
    MainToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    VerticalAxis: TPaintBox;
    Minimap: TPaintBox;
    pnVAxis: TPanel;
    pnHAxis: TPanel;
    Panel3:  TPanel;
    pnMinimap: TPanel;
    pnMap:   TPanel;
    hScrollBar: TScrollBar;
    SpeedButton11: TSpeedButton;
    tsTerrain: TTabSheet;
    vScrollBar: TScrollBar;
    procedure actCreateMapExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actMapOptionsExecute(Sender: TObject);
    procedure actObjectsExecute(Sender: TObject);
    procedure actObjectsUpdate(Sender: TObject);
    procedure actOpenMapExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actPropertiesUpdate(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actRedoUpdate(Sender: TObject);
    procedure actSaveMapAsExecute(Sender: TObject);
    procedure actSaveMapExecute(Sender: TObject);
    procedure actSaveMapUpdate(Sender: TObject);
    procedure actTerrainExecute(Sender: TObject);
    procedure actTerrainUpdate(Sender: TObject);
    procedure actUndergroundExecute(Sender: TObject);
    procedure actUndergroundUpdate(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure AnimTimerTimer(Sender: TObject);
    procedure btnBrush1Click(Sender: TObject);
    procedure btnBrush2Click(Sender: TObject);
    procedure btnBrush4Click(Sender: TObject);
    procedure btnBrushAreaClick(Sender: TObject);
    procedure btnBrushFillClick(Sender: TObject);
    procedure btnDirtClick(Sender: TObject);
    procedure btnGrassClick(Sender: TObject);
    procedure btnLavaClick(Sender: TObject);
    procedure btnRockClick(Sender: TObject);
    procedure btnRoughClick(Sender: TObject);
    procedure btnSandClick(Sender: TObject);
    procedure btnSnowClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnSwampClick(Sender: TObject);
    procedure btnWaterClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure MapViewResize(Sender: TObject);
    procedure MinimapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsViewMakeCurrent(Sender: TObject; var Allow: boolean);
    procedure pcToolBoxChange(Sender: TObject);
    procedure PlayerMenuClick(Sender: TObject);
    procedure MinimapPaint(Sender: TObject);
    procedure ObjectsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsViewPaint(Sender: TObject);
    procedure ObjectsViewResize(Sender: TObject);
    procedure ObjectsViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbObjectsResize(Sender: TObject);
    procedure sbObjectsScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VerticalAxisPaint(Sender: TObject);
    procedure vScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

  private
    FEnv: TMapEnvironment;
    procedure SetupPlayerSelection;

    function CheckUnsavedMap: boolean;
  private
    const
      OBJ_PER_ROW = 3;
      OBJ_CELL_SIZE = 60;

  private
    FZbuffer: TZBuffer;

    FResourceManager: IResourceLoader;

    glInited: Boolean;

    FMapHPos, FMapVPos: Integer; //topleft visible tile
    FViewTilesH, FViewTilesV: Integer; //amount of tiles visible on mapview

    FMap: TVCMIMap;
    FMapFilename: string;
    FObjManager: TObjectsManager;
    FGraphicsManager: TGraphicsManager;

    FHightLightTile: Boolean;

    FMouseTileX, FMouseTileY: Integer;  //current position of mouse in map coords
    FMouseX, FMouseY: Integer;  //current position of mouse in screen coords

    FMouseDown: boolean;

    FCurrentTerrain:   TTerrainType;
    FTerrainBrushMode: TBrushMode;
    FTerrainBrushSize: Integer;

    FUndoManager: TAbstractUndoManager;

    FMinimap: TMinimap;

    FObjectCount: integer;
    FObjectRows: integer;
    FObjectReminder: integer;

    FObjectsVPos: Integer;
    FViewObjectRowsH: Integer;

    FDragging: TDragProxy;

    FSelectedObject: TMapObject;

    FMapDragging: boolean;
    FNextDragSubject: TDragSubject;
    FSelectedTemplate: TObjTemplate;

    FCurrentPlayer: TPlayer;

    FMapViewState, FObjectsViewState: TLocalState;

    function GetObjIdx(col, row:integer): integer;

    function getMapHeight: Integer;
    function getMapWidth: Integer;

    procedure InvalidateMapDimensions;

    procedure InvalidateMapAxis;
    procedure InvalidateMap;
    procedure InvalidateMapContent;

    procedure SetMapPosition(APosition:TPoint);

    procedure SetMapViewMouse(x,y: integer);

    procedure InvalidateObjects;
    procedure InvalidateObjPos;

    procedure Init;
    procedure MapChanded;

    procedure PaintAxis(Kind: TAxisKind; Axis: TPaintBox);

    procedure RenderCursor;

    procedure LoadMap(AFileName: string);
    procedure SaveMap(AFileName: string);

    procedure SetCurrentPlayer(APlayer: TPlayer);
  protected
    procedure UpdateActions; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
  public
    procedure MoveObject(AObject: TMapObject; l,x,y: integer);
  end;

var
  fMain: TfMain;

implementation

uses
  undo_map, map_format, map_format_h3m, map_format_vcmi,
  editor_str_consts, root_manager, map_options, new_map,
  edit_object_options, Math, lazutf8classes;

{$R *.lfm}

{ TDragProxy }

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


procedure TObjectDragProxy.Drop;
begin
  FOwner.MoveObject(FDraggingObject,FOwner.FMap.CurrentLevel,FOwner.FMouseTileX+FShiftX,FOwner.FMouseTileY+FShiftY);
end;

procedure TObjectDragProxy.Render(x, y: integer);
begin
  FDraggingObject.RenderStatic((x+1+FShiftX)*TILE_SIZE,(y+1+FShiftY)*TILE_SIZE);
end;

{ TTemplateDragProxy }

constructor TTemplateDragProxy.Create(AOwner: TfMain;
  ADraggingTemplate: TObjTemplate);
begin
  FDraggingTemplate := ADraggingTemplate;
  inherited Create(AOwner);
end;

procedure TTemplateDragProxy.Drop;
var
  action_item: TAddObject;
begin

  action_item := TAddObject.Create(FOwner.FMap);

  action_item.L:=FOwner.FMap.CurrentLevel;
  action_item.X := FOwner.FMouseTileX;
  action_item.Y := FOwner.FMouseTileY;
  action_item.Template := FDraggingTemplate;

  action_item.CurrentPlayer:=FOwner.FCurrentPlayer;

  FOwner.FUndoManager.ExecuteItem(action_item);

  FOwner.FSelectedObject := action_item.TargetObject;
end;

procedure TTemplateDragProxy.Render(x, y: integer);
var
  cx: Integer;
  cy: Integer;
begin
  cx := (x +1 ) * TILE_SIZE;
  cy := (y+1) * TILE_SIZE;

  FDraggingTemplate.Def.RenderO(0,cx, cy, FOwner.FCurrentPlayer);
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
      FreeAndNil(FMap);

      FMap := TVCMIMap.Create(fenv,params);
      MapChanded;
    end;

  finally
    frm.Free;
  end;

end;

procedure TfMain.actDeleteExecute(Sender: TObject);
var
  action_item: TDeleteObject;
begin
  action_item := TDeleteObject.Create(FMap);
  action_item.TargetObject :=FSelectedObject;

  FUndoManager.ExecuteItem(action_item);

  FSelectedObject := nil;
end;

procedure TfMain.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FSelectedObject);
end;

procedure TfMain.actMapOptionsExecute(Sender: TObject);
var
  f: TMapOptionsForm;
begin
  f := TMapOptionsForm.Create(Self);

  f.Map := FMap;
  f.ShowModal;
end;

procedure TfMain.actObjectsExecute(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.none;
  pcToolBox.ActivePage := tsObjects;

  actObjects.Checked := True;
  actTerrain.Checked := False;
end;

procedure TfMain.actObjectsUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (pcToolBox.ActivePage = tsObjects);
end;

procedure TfMain.actOpenMapExecute(Sender: TObject);
begin
  if OpenMapDialog.Execute then
  begin
    LoadMap(OpenMapDialog.FileName);

  end;
end;

procedure TfMain.actPropertiesExecute(Sender: TObject);
var
  edit_form: TEditObjectOptions;
begin
  Assert(Assigned(FSelectedObject), 'actPropertiesExecute: No object selected');
  edit_form := TEditObjectOptions.Create(Self);
  try
    edit_form.EditObject(FSelectedObject);
  finally
    edit_form.Free;
  end;
end;

procedure TfMain.actPropertiesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FSelectedObject);
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
    FMapFilename := SaveMapAsDialog.FileName;
  end;
end;

procedure TfMain.actSaveMapExecute(Sender: TObject);
begin
  if FMapFilename = '' then
  begin
    actSaveMapAs.Execute;
  end
  else begin
    SaveMap(FMapFilename);
  end;
end;

procedure TfMain.actSaveMapUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FMap) and FMap.IsDirty;
end;

procedure TfMain.actTerrainExecute(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.fixed;
  pcToolBox.ActivePage := tsTerrain;

end;

procedure TfMain.actTerrainUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (pcToolBox.ActivePage = tsTerrain);
end;

procedure TfMain.actUndergroundExecute(Sender: TObject);
var
  under: Boolean;
begin
  under :=  (Sender as TAction).Checked;

  FMap.CurrentLevel := ifthen(under,1,0);
  FSelectedObject := nil;
  InvalidateMapDimensions;
  InvalidateMapContent;
end;

procedure TfMain.actUndergroundUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FMap) and (FMap.Levels > 1);
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

procedure TfMain.AnimTimerTimer(Sender: TObject);
begin
  if Visible and (WindowState<>wsMinimized) then
    MapView.Invalidate;
end;

procedure TfMain.btnBrush1Click(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.fixed;
  FTerrainBrushSize := 1;
end;

procedure TfMain.btnBrush2Click(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.fixed;
  FTerrainBrushSize := 2;
end;

procedure TfMain.btnBrush4Click(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.fixed;
  FTerrainBrushSize := 4;
end;

procedure TfMain.btnBrushAreaClick(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.area;
  FTerrainBrushSize := 0;
end;

procedure TfMain.btnBrushFillClick(Sender: TObject);
begin
  FTerrainBrushMode := TBrushMode.fill;
  FTerrainBrushSize := 0;
end;

procedure TfMain.btnDirtClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.dirt;
end;

procedure TfMain.btnGrassClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.grass;
end;

procedure TfMain.btnLavaClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.lava;
end;

procedure TfMain.btnRockClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.rock;
end;

procedure TfMain.btnRoughClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.rough;
end;

procedure TfMain.btnSandClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.sand;
end;

procedure TfMain.btnSnowClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.snow;
end;

procedure TfMain.btnSubClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.sub;
end;

procedure TfMain.btnSwampClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.swamp;
end;

procedure TfMain.btnWaterClick(Sender: TObject);
begin
  FCurrentTerrain := TTerrainType.water;
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
    if not MapView.MakeCurrent() then
    begin
      Exit;
    end;
    if not glInited then
    begin
      Init;
    end;
  finally
    RootManager.InitComplete;
  end;

end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

  CanClose := CheckUnsavedMap;

end;

procedure TfMain.FormCreate(Sender: TObject);
var
  dir: String;
  map_filename: String;
begin
  FZbuffer := TZBuffer.Create;
  pnHAxis.DoubleBuffered := True;
  pnVAxis.DoubleBuffered := True;
  pnMinimap.DoubleBuffered := True;
  MapView.SharedControl := RootManager.SharedContext;
  ObjectsView.SharedControl := RootManager.SharedContext;

  FCurrentTerrain := TTerrainType.dirt;
  FTerrainBrushMode := TBrushMode.fixed;
  FTerrainBrushSize := 1;
  pcToolBox.ActivePage := tsTerrain;

  FResourceManager := RootManager.ResourceManager;
  FEnv.tm := RootManager.TerrainManager;
  FObjManager := RootManager.ObjectsManager;
  FGraphicsManager := RootManager.GraphicsManger;
  FEnv.lm := RootManager.ListsManager;

  FUndoManager := TMapUndoManager.Create;

  FMinimap := TMinimap.Create(Self);

  FMap := TVCMIMap.CreateDefault(FEnv);


  FCurrentPlayer := TPlayer.none;

  SetupPlayerSelection;


  //load map if specified

  if Paramcount > 0 then
  begin
    //stage 4
    RootManager.ProgressForm.NextStage('Loading map.');

    try
      dir := GetCurrentDirUTF8();
      dir := IncludeTrailingPathDelimiter(dir);
      map_filename:= dir + ParamStr(1);
      if FileExistsUTF8(map_filename) then
        LoadMap(map_filename);
    except
      on e: Exception do
      begin
        Application.ShowException(e);
      end;
    end;

  end;

  MapChanded;
  InvalidateObjects;

  if MapView.MakeCurrent() then
    Init;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMapViewState);
  FreeAndNil(FObjectsViewState);

  FZbuffer.Free;
  FMap.Free;

  FUndoManager.Free;
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  dx,dy: integer;
begin

  if not Assigned(FSelectedObject) then
  begin
    Exit;
  end;
  //TODO: use UNDO/REDO

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
    MoveObject(FSelectedObject, FMap.CurrentLevel, FSelectedObject.X + dx, FSelectedObject.Y + dy);
    Key := VK_UNKNOWN;
    Exit;
  end;

end;

function TfMain.getMapHeight: Integer;
begin
  Result := 0;
  if Assigned(FMap) then
    Result := FMap.Height;
end;

function TfMain.getMapWidth: Integer;
begin
  Result := 0;
  if Assigned(FMap) then
     Result := FMap.Width;
end;

function TfMain.GetObjIdx(col, row: integer): integer;
begin
   result := col + OBJ_PER_ROW * (row + FObjectsVPos);
end;

procedure TfMain.HorisontalAxisPaint(Sender: TObject);
begin
  PaintAxis(TAxisKind.Horizontal,Sender as TPaintBox);
end;

procedure TfMain.hScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  ScrollPos := Min((sender as TScrollBar).Max - (sender as TScrollBar).PageSize +1,ScrollPos);

  FMapHPos := ScrollPos;
  InvalidateMapAxis;
end;

procedure TfMain.Init;
begin
  RootManager.ProgressForm.NextStage('Initializing GL context.');
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_DITHER);

  glInited := True;
end;

procedure TfMain.InvalidateMapAxis;
begin
  InvalidateMap;
  VerticalAxis.Invalidate;
  HorisontalAxis.Invalidate;
end;

procedure TfMain.InvalidateMapContent;
begin
  MapView.Invalidate;
  FMinimap.InvalidateMap;
  Minimap.Invalidate;
end;

procedure TfMain.InvalidateMap;
begin
  MapView.Invalidate;
  Minimap.Invalidate;
end;

procedure TfMain.InvalidateMapDimensions;
var
  factor: Double;
begin
  FViewTilesV := MapView.Height div TILE_SIZE;
  FViewTilesH := MapView.Width div TILE_SIZE;

  vScrollBar.Max := getMapHeight - 1;
  hScrollBar.Max := getMapWidth - 1;

  vScrollBar.PageSize := FViewTilesV;
  hScrollBar.PageSize := FViewTilesH;

  hScrollBar.LargeChange := FViewTilesH;
  vScrollBar.LargeChange := FViewTilesV;

  factor := Double(getMapHeight) / Double(getMapWidth);

  pnMinimap.Height := round(factor * pnMinimap.Width) ;

  if (FMapHPos + FViewTilesH) > getMapWidth then
  begin
    FMapHPos := Max(0, getMapWidth-FViewTilesH);
  end;

  if (FMapVPos + FViewTilesV) > getMapHeight then
  begin
    FMapVPos := Max(0, getMapHeight-FViewTilesV);
  end;
end;

procedure TfMain.InvalidateObjects;
begin
  FObjectCount := FObjManager.ObjCount;

  FObjectRows := FObjectCount div 3;
  FObjectReminder := FObjectCount mod 3;
  if FObjectReminder > 0 then inc(FObjectRows);

  sbObjects.Min := 0;
  sbObjects.Max := FObjectRows - 1;

  sbObjects.PageSize := ObjectsView.Height div OBJ_CELL_SIZE;

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
  //todo: ask to save map
  cstm := nil;
  set_filename := False;
  is_compressed := False;

  file_ext := Trim(UpperCase(ExtractFileExt(AFileName)));

  stm := TFileStreamUTF8.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  stm.Seek(0,soBeginning);

  try
    case file_ext of
      FORMAT_VCMI_EXT:
        begin
          reader := TMapReaderVCMI.Create(FEnv);
          cstm := stm;
          set_filename := True; //support saving
        end;
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
  if set_filename then FMapFilename := AFileName;

  MapChanded;
end;

procedure TfMain.MapChanded;
begin
  FSelectedObject := nil;

  FMinimap.Map := FMap;
  InvalidateMapDimensions;
  FUndoManager.Clear;
end;

procedure TfMain.MapViewClick(Sender: TObject);
var
  action_item:TEditTerrain;
  i,j: Integer;

  q: TMapObjectQueue;

  o: TMapObject;
begin
  //TODO: handle all cases

  if not FMap.IsOnMap(FMap.CurrentLevel, FMouseTileX, FMouseTileY) then
  begin
    exit;
  end;

  if FTerrainBrushMode <> TBrushMode.none then
  begin
    action_item := TEditTerrain.Create(FMap);

    action_item.BrushMode := FTerrainBrushMode;
    action_item.TerrainType := FCurrentTerrain;
    action_item.Level := FMap.CurrentLevel;

      case FTerrainBrushMode of
      TBrushMode.fixed:begin
        for i := 0 to FTerrainBrushSize -1 do
        begin
          for j := 0 to FTerrainBrushSize - 1 do
          begin
            action_item.AddTile(FMouseTileX+i,FMouseTileY+j);
          end;
        end;
      end;
      TBrushMode.area:; //todo: handle area mode, fill mode
      TBrushMode.fill:;
    end;


    FUndoManager.ExecuteItem(action_item);

    InvalidateMapContent;
    Exit;
  end;

  //TODO: road/river mode



end;

procedure TfMain.MapViewDblClick(Sender: TObject);
var
  q: TMapObjectQueue;
begin
  if Assigned(FSelectedObject) then
  begin
    if FSelectedObject.CoversTile(FMap.CurrentLevel,FMouseTileX,FMouseTileY) then
    begin
      actProperties.Execute;
      Exit;
    end;
  end;

  q := TMapObjectQueue.Create;
  FMap.SelectObjectsOnTile(FMap.CurrentLevel,FMouseTileX,FMouseTileY,q);
  try
    if not q.IsEmpty then
    begin
      FSelectedObject := q.Top;
      actProperties.Execute;
    end;
  finally
    q.Free;
  end;
end;

procedure TfMain.MapViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  o: TMapObject;
  ot: TMapObjectTemplate;
begin
  SetMapViewMouse(x,y);

  FDragging.Drop;

  InvalidateMapContent;
end;

procedure TfMain.MapViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := true; //TODO: handle accceptible terrain

  FSelectedObject := nil;

  SetMapViewMouse(x,y);

  case State of
    TDragState.dsDragEnter: FMapDragging := True ;
    TDragState.dsDragLeave: FMapDragging := False ;
    TDragState.dsDragMove: FMapDragging := True ;
  end;

  InvalidateMap;

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
       if Assigned(FSelectedObject)
         and (FSelectedObject.CoversTile(FMap.CurrentLevel,FMouseTileX,FMouseTileY)) then
       begin
         FNextDragSubject:=TDragSubject.MapObject;
         DragManager.DragStart(self,False, TILE_SIZE div 2);
         DragStarted := true;
       end;
     end;
   end;
begin
  SetMapViewMouse(x,y);
  if FTerrainBrushMode=TBrushMode.none then
  begin

    DragStarted:=False;

    PerformStartDrag();

    q := TMapObjectQueue.Create;
    try
      FMap.SelectObjectsOnTile(FMap.CurrentLevel,FMouseTileX,FMouseTileY,q);

      if Assigned(FSelectedObject)
        and (FSelectedObject.CoversTile(FMap.CurrentLevel,FMouseTileX,FMouseTileY))
        then
      begin
        //select next object
        o := nil;
        while not q.IsEmpty do
        begin
          o := q.Top;
          q.Pop;

          if (q.IsEmpty) or (q.Top = FSelectedObject) then
          begin
            break;
          end;

        end;
        FSelectedObject := o;
      end
      else
      begin
        if q.IsEmpty then
          FSelectedObject := nil
        else
          FSelectedObject := q.Top;
      end;
      PerformStartDrag();
    finally
      q.Free;
    end;
  end;

  //todo: editor mode
  if Button = TMouseButton.mbLeft then
  begin
    FMouseDown := True;
  end;
end;

procedure TfMain.MapViewMouseEnter(Sender: TObject);
begin
  FHightLightTile := True;
end;

procedure TfMain.MapViewMouseLeave(Sender: TObject);
begin
  FHightLightTile := False;
  InvalidateMapAxis;
end;

procedure TfMain.MapViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  FOldTileX: Integer;
  FOldTileY: Integer;
begin
  FOldTileX := FMouseTileX;
  FOldTileY := FMouseTileY;
  SetMapViewMouse(x,y);

  if (FOldTileX<>FMouseTileX) or (FOldTileY<>FMouseTileY) then
  begin
    InvalidateMapAxis;
  end;
end;

procedure TfMain.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //todo: editor mode
  if Button = TMouseButton.mbLeft then
  begin
    FMouseDown := False;
  end;

end;

procedure TfMain.MapViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  sb: TScrollBar;
begin
  //
  if ssShift in Shift then
  begin
    sb := hScrollBar;
  end
  else begin
    sb := vScrollBar;
  end;

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

  Handled := True;
end;

procedure TfMain.MapViewPaint(Sender: TObject);
var
  c: TOpenGLControl;
  scissor_x: Integer;
  scissor_w: Integer;
  scissor_h: Integer;
  scissor_y: Integer;
  cx: Integer;
  cy: Integer;

begin
  c := TOpenGLControl(Sender);

  if not c.MakeCurrent() then
  begin
    Exit;
  end;
  if not glInited then
  begin
    Init;
  end;

  if not Assigned(FMapViewState) then
  begin
    FMapViewState := TLocalState.Create;

    FMapViewState.Init;
  end;

  CurrentContextState := FMapViewState;

  CurrentContextState.UsePalettedTextures();

  glEnable(GL_SCISSOR_TEST);
  glScissor(0, 0, MapView.Width, MapView.Height);

  CurrentContextState.SetOrtho(TILE_SIZE * FMapHPos,
    MapView.Width + TILE_SIZE * FMapHPos,
    MapView.Height + TILE_SIZE * FMapVPos,
    TILE_SIZE * FMapVPos);

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);

  scissor_x := 0;
  scissor_y := ifthen(FMapVPos + FViewTilesV >= getMapHeight(), MapView.Height mod TILE_SIZE, 0);

  scissor_w := ifthen(FMapHPos + FViewTilesH >= getMapWidth(), FViewTilesH * TILE_SIZE, MapView.Width);
  scissor_h := ifthen(FMapVPos + FViewTilesV >= getMapHeight(),  FViewTilesV * TILE_SIZE, MapView.Height);

  glScissor(scissor_x,scissor_y, scissor_w,scissor_h);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  editor_gl.CurrentContextState.StartDrawingSprites;

  FMap.RenderTerrain(FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);

  //glEnable (GL_BLEND);
  //glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  FMap.RenderObjects(FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);


  if FMapDragging then
  begin
    Assert(Assigned(FDragging));

    FDragging.Render(FMouseTileX,FMouseTileY);
  end;

  editor_gl.CurrentContextState.StartDrawingRects;

  //todo: render passability

  CurrentContextState.UseNoTextures();

  if Assigned(FSelectedObject) then
  begin
    FSelectedObject.RenderSelectionRect;
  end;

  glDisable(GL_SCISSOR_TEST);

  RenderCursor;

  glDisable (GL_BLEND);

  editor_gl.CurrentContextState.StopDrawing;
  //glDisable(GL_ALPHA_TEST);

  c.SwapBuffers;
end;

procedure TfMain.MapViewResize(Sender: TObject);
begin
  InvalidateMapDimensions;
end;

procedure TfMain.MinimapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cx,cy: Double;
  pos: TPoint;
begin
  if not Assigned(FMap) then
  begin
    Exit;
  end;

  if (Button = TMouseButton.mbLeft) and ([ssShift,ssCtrl,ssAlt] * Shift = []) then
  begin
    //set map view center position to mouse pos

    cx := X / (Minimap.Width);
    cy := Y / (Minimap.Height);

    pos.x := round(cx * FMap.Width);
    pos.y := round(cy * FMap.Height);

    pos.x := pos.x- (FViewTilesH div 2);
    pos.y := pos.y- (FViewTilesV div 2);

    SetMapPosition(pos);;

  end;
end;

procedure TfMain.ObjectsViewMakeCurrent(Sender: TObject; var Allow: boolean);
begin

end;

procedure TfMain.PlayerMenuClick(Sender: TObject);
var
  m, mc : TMenuItem;
  i: Integer;
begin
  m := Sender as TMenuItem;

  for i := 0 to menuPlayer.Items.Count - 1 do
  begin
    mc := menuPlayer.Items[i];

    if mc=m then
    begin
      SetCurrentPlayer(TPlayer(m.Tag));
      mc.Checked := True;
    end
    else begin
      mc.Checked := False;
    end;

  end;

  InvalidateObjects;

end;

procedure TfMain.MinimapPaint(Sender: TObject);
begin
  FMinimap.Paint(Sender as TPaintBox);
end;

procedure TfMain.ObjectsViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col: Integer;
  row: Integer;
  o_idx: Integer;
begin
  col := x div OBJ_CELL_SIZE;
  row := y div OBJ_CELL_SIZE;

  o_idx := GetObjIdx(col, row);

  if (Button = TMouseButton.mbLeft) and (o_idx < FObjManager.ObjCount) then
  begin
    FNextDragSubject:=TDragSubject.MapTemplate;
    FSelectedTemplate := FObjManager.Objcts[o_idx];
    DragManager.DragStart(self, True,0); //after state change
  end;

end;

procedure TfMain.ObjectsViewPaint(Sender: TObject);
var
  c: TOpenGLControl;
  row: Integer;
  col: Integer;
  o_idx: Integer;
  o_def: TObjTemplate;
  cx: Integer;
  cy: Integer;
  dim: Integer;

begin
  c := TOpenGLControl(Sender);

  if not c.MakeCurrent() then
  begin
    Exit;
  end;
  if not glInited then
  begin
    Init;
  end;

  if not Assigned(FObjectsViewState) then
  begin
    FObjectsViewState := TLocalState.Create;

    FObjectsViewState.Init;
  end;
  CurrentContextState := FObjectsViewState;

  glEnable(GL_SCISSOR_TEST);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glScissor(0, 0, c.Width, c.Height);

  CurrentContextState.UseNoTextures();

  CurrentContextState.SetOrtho(0, c.Width + 0, c.Height + 0, 0);

  glClearColor(255, 255, 255, 0);
  glClear(GL_COLOR_BUFFER_BIT);

  editor_gl.CurrentContextState.StartDrawingRects;

  for row := 0 to FViewObjectRowsH + 1 do
  begin
    for col := 0 to OBJ_PER_ROW - 1 do
    begin
      o_idx := GetObjIdx(col, row);

      if o_idx >= FObjectCount then
        break;
      cx := col * OBJ_CELL_SIZE;
      cy := row * OBJ_CELL_SIZE;

      editor_gl.CurrentContextState.RenderRect(cx,cy,OBJ_CELL_SIZE,OBJ_CELL_SIZE);
    end;
  end;

  editor_gl.CurrentContextState.StopDrawing;

  CurrentContextState.UsePalettedTextures();
  editor_gl.CurrentContextState.StartDrawingSprites;

  for row := 0 to FViewObjectRowsH + 1 do
  begin
    for col := 0 to OBJ_PER_ROW - 1 do
    begin
      o_idx := GetObjIdx(col, row);

      if o_idx >= FObjectCount then
        break;
      cx := col * OBJ_CELL_SIZE;
      cy := row * OBJ_CELL_SIZE;

      o_def := FObjManager.Objcts[o_idx];
      o_def.Def.Render(0,cx,cy, OBJ_CELL_SIZE, FCurrentPlayer);

    end;
  end;
  editor_gl.CurrentContextState.StopDrawing;

  glDisable (GL_BLEND);
  glDisable(GL_SCISSOR_TEST);

  c.SwapBuffers;
end;

procedure TfMain.ObjectsViewResize(Sender: TObject);
begin
  InvalidateObjects;
end;

procedure TfMain.PaintAxis(Kind: TAxisKind; Axis: TPaintBox);
var
  ctx: TCanvas;
  i:   Integer;
  tiles, tmp: Integer;
  text_width: Integer;
  txt: string;
  ofs: Integer;

  img: TBitmap;
begin
  case Kind of
    TAxisKind.Horizontal: tmp := Axis.Width;
    TAxisKind.Vertical: tmp := Axis.Height;
  end;

  tiles := tmp div TILE_SIZE;

  img := TBitmap.Create;

  img.SetSize(Axis.Width,Axis.Height);

  ctx := img.Canvas;

  try
    ctx.Brush.Color := clWhite;
    ctx.FillRect(0, 0, Axis.Width, Axis.Height);

    for i := 0 to tiles do
    begin
      case Kind of
        TAxisKind.Horizontal: tmp := FMapHPos;
        TAxisKind.Vertical: tmp := FMapVPos;
      end;
      txt := IntToStr(tmp + i);
      text_width := ctx.GetTextWidth(txt);

      case Kind of
        TAxisKind.Horizontal: ofs := (TILE_SIZE - text_width) div 2;
        TAxisKind.Vertical: ofs :=(TILE_SIZE + text_width) div 2;
      end;

      case Kind of
        TAxisKind.Horizontal: begin
          if (FMapHPos + 1 + i) <= FMap.Width then
          begin
            ctx.TextOut(I * TILE_SIZE + ofs, 0, txt);
          end;
        end;
        TAxisKind.Vertical: begin
          if (FMapVPos + 1 + i) <= FMap.Height then
          begin
            ctx.Font.Orientation := 900;
            ctx.TextOut(0, I * TILE_SIZE + ofs, txt);
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
begin
  sbObjects.Position := sbObjects.Position - Sign(WheelDelta) * sbObjects.PageSize;
  InvalidateObjPos;
  Handled := true;
end;

procedure TfMain.pbObjectsResize(Sender: TObject);
begin
  InvalidateObjects;
end;

procedure TfMain.pcToolBoxChange(Sender: TObject);
begin
  FSelectedObject := nil;
end;

procedure TfMain.RenderCursor;
var
  dim: Integer;
  cx,cy: Integer;
begin
  cx := FMouseTileX * TILE_SIZE;
  cy := FMouseTileY * TILE_SIZE;

  if FHightLightTile and (FTerrainBrushMode = TBrushMode.fixed) then
  begin
    editor_gl.CurrentContextState.StartDrawingRects;
    dim := TILE_SIZE * FTerrainBrushSize;
    editor_gl.CurrentContextState.RenderRect(cx,cy,dim,dim);
    editor_gl.CurrentContextState.StopDrawing;
  end;
end;

procedure TfMain.SaveMap(AFileName: string);
var
  writer: IMapWriter;
  stm: TFileStreamUTF8;
begin

  if FileExistsUTF8(AFileName) then
  begin
    if MessageDlg(rsConfirm,rsFileExists, TMsgDlgType.mtConfirmation, mbYesNo,0) <> mrYes then
      exit;
  end;

  stm := TFileStreamUTF8.Create(AFileName,fmCreate);
  stm.Size := 0;

  try
    writer := TMapWriterVCMI.Create(FEnv);
    FMap.SaveToStream(stm,writer);
    FMapFilename := AFileName;
  finally
    stm.Free;
  end;
end;

procedure TfMain.sbObjectsScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  InvalidateObjPos;
end;

procedure TfMain.SetCurrentPlayer(APlayer: TPlayer);
begin
  FCurrentPlayer := APlayer;

  InvalidateObjects;

end;

procedure TfMain.SetMapPosition(APosition: TPoint);
   procedure UpdateScrollbar(sb: TCustomScrollBar; var APosition: Integer);
   begin
     APosition := Max(APosition,0);
     APosition := Min(sb.Max-sb.PageSize+1,APosition);
     sb.Position := APosition;
   end;
begin

  UpdateScrollbar(hScrollBar,APosition.x);
  FMapHPos := APosition.x;


  UpdateScrollbar(vScrollBar,APosition.y);
  FMapVPos := APosition.y;

  InvalidateMapAxis;
end;

procedure TfMain.SetMapViewMouse(x, y: integer);
var
  ofs_x: Integer;
  ofs_y: Integer;
begin
  //x,y in viewport coords

  FMouseX := x;
  FMouseY := y;

  ofs_x := X div TILE_SIZE;
  ofs_y := Y div TILE_SIZE;

  FMouseTileX := FMapHPos + ofs_x;
  FMouseTileY := FMapVPos + ofs_y;

end;

procedure TfMain.SetupPlayerSelection;
var
  m: TMenuItem;
  p: TPlayer;
begin
  menuPlayer.Items.Clear;

  m := TMenuItem.Create(menuPlayer);

  m.Tag := Integer(TPlayer.none);
  m.OnClick := @PlayerMenuClick;
  m.Checked := True;
  menuPlayer.Items.Add(m);
  m.Caption := FEnv.lm.PlayerName[TPlayer.none];

  for p in [TPlayer.RED..TPlayer.PINK] do
  begin
    m := TMenuItem.Create(menuPlayer);

    m.Tag := Integer(p);
    m.OnClick := @PlayerMenuClick;
    m.Caption := FEnv.lm.PlayerName[p];
    menuPlayer.Items.Add(m);
  end;


end;

procedure TfMain.UpdateActions;
var
  s: String;
begin
  inherited UpdateActions;

  if Assigned(FMap) then
  begin
    s := '';
    if FMap.IsDirty then
      s := '*';
    Caption := FMap.Name + s + ' - VCMI editor'
  end
  else
    Caption := 'VCMI editor';
end;

procedure TfMain.DoStartDrag(var DragObject: TDragObject);
begin
  case FNextDragSubject of
    TDragSubject.MapObject: DragObject := TObjectDragProxy.Create(self, FSelectedObject, FMouseTileX, FMouseTileY);
    TDragSubject.MapTemplate: DragObject := TTemplateDragProxy.Create(self, FSelectedTemplate);
  else
  inherited DoStartDrag(DragObject);
  end;
end;

procedure TfMain.DragCanceled;
begin
  inherited DragCanceled;
  FMapDragging:=false;
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

  FSelectedObject := AObject;
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
  FMapVPos := ScrollPos;
  InvalidateMapAxis;
end;

end.
