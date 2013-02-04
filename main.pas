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
  Classes, SysUtils, FileUtil, GL, GLext, OpenGLContext, Forms, Controls,
  Graphics, GraphType, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, ComCtrls,
  Buttons, Map, terrain, editor_types, undo_base, map_actions, objects, def,
  minimap, filesystem, filesystem_base, types;

type
  TAxisKind = (Vertical,Horizontal);

  { TfMain }

  TfMain = class(TForm)
    actUnderground: TAction;
    actMapOptions: TAction;
    actSaveMapAs: TAction;
    actTerrain: TAction;
    actObjects: TAction;
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
    MenuItem7: TMenuItem;
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
    ToolBar2: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tsObjects: TTabSheet;
    ToolBar1: TToolBar;
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
    procedure actMapOptionsExecute(Sender: TObject);
    procedure actObjectsExecute(Sender: TObject);
    procedure actObjectsUpdate(Sender: TObject);
    procedure actOpenMapExecute(Sender: TObject);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HorisontalAxisPaint(Sender: TObject);
    procedure hScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MapViewClick(Sender: TObject);
    procedure MapViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MapViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
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
    procedure MinimapPaint(Sender: TObject);
    procedure ObjectsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsViewPaint(Sender: TObject);
    procedure ObjectsViewResize(Sender: TObject);
    procedure ObjectsViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbObjectsPaint(Sender: TObject);
    procedure pbObjectsResize(Sender: TObject);
    procedure sbObjectsScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VerticalAxisPaint(Sender: TObject);
    procedure vScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    const
      OBJ_PER_ROW = 3;
      OBJ_CELL_SIZE = 60;

  private

    FResourceManager: TFSManager;

    glInited: Boolean;

    FMapHPos, FMapVPos: Integer; //topleft visible tile
    FViewTilesH, FViewTilesV: Integer; //amount of tiles visible on mapview

    FMap: TVCMIMap;
    FMapFilename: string;
    FTerrianManager: TTerrainManager;
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

    FDraggingTemplate: TObjTemplate;

    FMapDragging: boolean;

    function GetObjIdx(col, row:integer): integer;

    function getMapHeight: Integer;
    function getMapWidth: Integer;

    procedure InvalidateMapDimensions;

    procedure InvalidateMapAxis;
    procedure InvalidateMap;
    procedure InvalidateMapContent;

    procedure SetMapViewMouse(x,y: integer);

    procedure InvalidateObjects;
    procedure InvalidateObjPos;

    procedure Init;
    procedure MapChanded;

    procedure PaintAxis(Kind: TAxisKind; Axis: TPaintBox);

    procedure RenderCursor;

    procedure LoadMap(AFileName: string);
    procedure SaveMap(AFileName: string);
  protected
    procedure UpdateActions; override;
  end;

var
  fMain: TfMain;

implementation

uses
  undo_map, map_format, map_format_h3m, zlib_stream, map_format_vcmi,
  editor_str_consts, map_options, Math, lazutf8classes;

{$R *.lfm}


{ TfMain }

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
    a.Caption := rsRedo + FUndoManager.PeekNext.Description;
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
var
  stm: TFileStreamUTF8;
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
  (Sender as TAction).Enabled := Assigned(FMap) and FMap.IsDirty; //todo: use IsDirty
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
    a.Caption := rsUndo + FUndoManager.PeekCurrent.Description;
  end else
  begin
    a.Enabled := False;
    a.Caption := rsUndo;
  end;

end;

procedure TfMain.AnimTimerTimer(Sender: TObject);
begin
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

procedure TfMain.FormActivate(Sender: TObject);
begin
  if not MapView.MakeCurrent() then
  begin
    Exit;
  end;
  if not glInited then
  begin
    Init;
  end;

end;

procedure TfMain.FormCreate(Sender: TObject);
var
  dir: String;
  map_filename: String;
begin
  pnHAxis.DoubleBuffered := True;
  pnVAxis.DoubleBuffered := True;
  pnMinimap.DoubleBuffered := True;
  ObjectsView.SharedControl := MapView;

  FResourceManager := TFSManager.Create(self);
  FResourceManager.ScanFilesystem;

  FGraphicsManager := TGraphicsManager.Create(FResourceManager);

  FTerrianManager := TTerrainManager.Create(FGraphicsManager);

  FTerrianManager.LoadConfig;
  FTerrianManager.LoadTerrainGraphics;


  FCurrentTerrain := TTerrainType.dirt;
  FTerrainBrushMode := TBrushMode.fixed;
  FTerrainBrushSize := 1;
  pcToolBox.ActivePage := tsTerrain;


  FUndoManager := TMapUndoManger.Create;

  FObjManager := TObjectsManager.Create(FGraphicsManager);
  FObjManager.LoadObjects;

  FMinimap := TMinimap.Create(Self);

  FMap := TVCMIMap.Create(FTerrianManager);

  //load map if specified

  if Paramcount > 0 then
  begin
    dir := GetCurrentDirUTF8();
    dir := IncludeTrailingPathDelimiter(dir);
    map_filename:= dir + ParamStr(1);
    if FileExistsUTF8(map_filename) then
      LoadMap(map_filename);

  end;


  //FMinimap.Map := FMap;

  MapChanded;
  InvalidateObjects;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin

  FMap.Free;

  FUndoManager.Free;
end;

function TfMain.getMapHeight: Integer;
begin
  Result := FMap.Height;
end;

function TfMain.getMapWidth: Integer;
begin
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
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_DITHER);
  glDisable(GL_TEXTURE_2D);

  FGraphicsManager.BindTextures;

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
          reader := TMapReaderVCMI.Create(FTerrianManager);
          cstm := stm;
          set_filename := True; //support saving
        end;
      FORMAT_H3M_EXT:
        begin
          reader := TMapReaderH3m.Create(FTerrianManager);
          cstm := TZlibInputStream.CreateGZip(stm,0);
          is_compressed := true;
          //TODO: support uncompressed maps
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

  if glInited then FGraphicsManager.BindTextures; //???
  MapChanded;
end;

procedure TfMain.MapChanded;
begin
  FMinimap.Map := FMap;
  InvalidateMapDimensions;
  FUndoManager.Clear;
end;

procedure TfMain.MapViewClick(Sender: TObject);
var
  action_item:TEditTerrain;
  i,j: Integer;
begin
  //TODO: handle all cases



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
end;

procedure TfMain.MapViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //TODO: plase obejct
end;

procedure TfMain.MapViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := true; //TODO: hanlde accceptible terrain

  SetMapViewMouse(x,y);

  case State of
    TDragState.dsDragEnter: FMapDragging := True ;
    TDragState.dsDragLeave: FMapDragging := False ;
    TDragState.dsDragMove: FMapDragging := True ;
  end;

  InvalidateMap;

  //TODO: check terrain
end;

procedure TfMain.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
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

  glEnable(GL_SCISSOR_TEST);


  glScissor(0, 0, MapView.Width, MapView.Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(
    TILE_SIZE * FMapHPos,
    MapView.Width + TILE_SIZE * FMapHPos,
    MapView.Height + TILE_SIZE * FMapVPos,
    TILE_SIZE * FMapVPos,
    0, 1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  //glTranslatef(0.375, 0.375, 0);   //???
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);

  scissor_x := 0;
  scissor_y := ifthen(FMapVPos + FViewTilesV >= getMapHeight(), MapView.Height mod TILE_SIZE, 0);

  scissor_w := ifthen(FMapHPos + FViewTilesH >= getMapWidth(), FViewTilesH * TILE_SIZE, MapView.Width);
  scissor_h := ifthen(FMapVPos + FViewTilesV >= getMapHeight(),  FViewTilesV * TILE_SIZE, MapView.Height);

  glScissor(scissor_x,scissor_y, scissor_w,scissor_h);

  FMap.Render(FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  FMap.RenderObjects(FMapHPos, FMapHPos + FViewTilesH, FMapVPos, FMapVPos + FViewTilesV);

  //todo: render passability

  RenderCursor;

  glDisable (GL_BLEND);

  glDisable(GL_SCISSOR_TEST);
  //glDisable(GL_ALPHA_TEST);

  c.SwapBuffers;
end;

procedure TfMain.MapViewResize(Sender: TObject);
begin
  InvalidateMapDimensions;
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
  FDraggingTemplate := nil;

  col := x div OBJ_CELL_SIZE;
  row := y div OBJ_CELL_SIZE;

  o_idx := GetObjIdx(col, row);

  if (Button = TMouseButton.mbLeft) and (o_idx < FObjManager.ObjCount) then
  begin
    DragManager.DragStart(ObjectsView, True,0);

    FDraggingTemplate := FObjManager.Objcts[o_idx];
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

  glEnable(GL_SCISSOR_TEST);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glScissor(0, 0, c.Width, c.Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(
    0,
    c.Width + 0,
    c.Height + 0,
    0,
    0, 1);

  glMatrixMode(GL_MODELVIEW);
  glClearColor(255, 255, 255, 0);
  glClear(GL_COLOR_BUFFER_BIT);

    for row := 0 to FViewObjectRowsH + 1 do
    begin
      for col := 0 to OBJ_PER_ROW - 1 do
      begin
        o_idx := GetObjIdx(col, row);

        if o_idx >= FObjectCount then
          break;

            o_def := FObjManager.Objcts[o_idx];

            cx := col * OBJ_CELL_SIZE;
            cy := row * OBJ_CELL_SIZE;

            glPushAttrib(GL_CURRENT_BIT);

            glBegin(GL_LINE_LOOP);

            glColor4ub(200, 200, 200, 255);
            glLineWidth(1);

            dim := OBJ_CELL_SIZE;

            glVertex2i(cx, cy);
            glVertex2i(cx + dim, cy);

            glVertex2i(cx + dim, cy + dim);
            glVertex2i(cx, cy + dim);


            glEnd();
            glPopAttrib();

            o_def.Def.Render(0,cx,cy, OBJ_CELL_SIZE);

      end;
    end;
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
      txt := IntToStr(tmp + 1 + i);
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

procedure TfMain.pbObjectsPaint(Sender: TObject);
var
  pb: TPaintBox;
  ctx: TCanvas;
  col: Integer;
  row: Integer;

  o_idx: Integer;

  pos_x: integer;

  o_def: TObjTemplate;
begin
  pb := Sender as TPaintBox;

  ctx := pb.Canvas;

  ctx.Lock;

  try
    ctx.Brush.Color := clWhite;
    ctx.FillRect(0, 0, pb.Width, pb.Height);

    for row := 1 to FViewObjectRowsH +1 do
    begin
      for col := 1 to 3 do
      begin
        o_idx := col + 3 * (row + FObjectsVPos);

        if o_idx >= FObjectCount then
          break; //TODO: remove workaround

        o_def := FObjManager.Objcts[o_idx];

        ctx.Brush.Color := clGray;

        ctx.Rectangle((col-1)*OBJ_CELL_SIZE, (row-1)*OBJ_CELL_SIZE,col*OBJ_CELL_SIZE, row*OBJ_CELL_SIZE);
      end;

    end;
  except
    ctx.Unlock;
  end;

  //TODO: reminder


end;

procedure TfMain.pbObjectsResize(Sender: TObject);
begin
  InvalidateObjects;
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
    glPushAttrib(GL_CURRENT_BIT);
    glBegin(GL_LINE_LOOP);

    glColor4ub(200, 200, 200, 255);
    glLineWidth(1);



    dim := TILE_SIZE * FTerrainBrushSize;

    glVertex2i(cx, cy);
    glVertex2i(cx + dim, cy);

    glVertex2i(cx + dim, cy + dim);
    glVertex2i(cx, cy + dim);


    glEnd();
    glPopAttrib();
  end;

  if FMapDragging then
  begin
    Assert(Assigned(FDraggingTemplate));

    FDraggingTemplate.Def.RenderO(0,cx +TILE_SIZE, cy+TILE_SIZE);
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
     writer := TMapWriterVCMI.Create(FTerrianManager);
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

procedure TfMain.UpdateActions;
begin
  inherited UpdateActions;

  if Assigned(FMap) then
    Caption := FMap.Name + ' - VCMI editor'
  else
    Caption := 'VCMI editor';
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
