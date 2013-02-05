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
unit map_actions;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, Map, gvector, gset, undo_map, editor_types, terrain;

type

  { TEditTerrain }

  TBrushMode = (none,fixed, area, fill);

  TTileInfo = record
    X,Y: integer;
    TerType: TTerrainType;
    TerSubtype: UInt8;
    mir: UInt8;
  end;

  TTileInfos = specialize TVector<TTileInfo>;

  { TTileCompare }

  TTileCompare = class
  public
    class function c(a,b: TTileInfo): boolean;
  end;

  TTileSet = specialize TSet<TTileInfo,TTileCompare> ;

  TValidationResult = record
    result: Boolean;
    transitionReplacement: string;

  end;

  TEditTerrain = class(TMapUndoItem)
  strict private

    FInQueue, FOutQueue: TTileSet;

    procedure UndoTile(constref Tile: TTileInfo);
    //no checking
    function GetTileInfo(x,y: Integer): TTileInfo;
    //safe, with checking
    function GetTileInfo(x,y: Integer; out Info: TTileInfo): boolean;

    procedure ProcessTile(var Info: TTileInfo);

    function ValidateTerrainView(info: TTileInfo; pattern: TPattern; recDepth: integer = 0): TValidationResult;
  strict private
    FLevel: Integer;

    FOldTileInfos: TTileInfos;
    FNewTileInfos: TTileInfos;

    FBrushMode: TBrushMode;
    FTerrainType: TTerrainType;
    procedure SetBrushMode(AValue: TBrushMode);
    procedure SetLevel(AValue: Integer);
    procedure SetTerrainType(AValue: TTerrainType);
  public
    constructor Create(AMap: TVCMIMap); override;
    destructor Destroy; override;

    procedure Redo; override;
    procedure Undo; override;
    procedure Execute; override;
    function GetDescription: string; override;

    property BrushMode: TBrushMode read FBrushMode write SetBrushMode;

    property Level: Integer read FLevel write SetLevel;

    procedure AddTile(X,Y: integer);

    property TerrainType: TTerrainType read FTerrainType write SetTerrainType;
  end;

    //operator= (a,b: TTileInfo): Boolean;

implementation

//operator = (a, b: TTileInfo): Boolean;
//begin
//  Result := (a.X = b.x) and;
//end;

{ TTileCompare }

class function TTileCompare.c(a, b: TTileInfo): boolean;
begin
  Result := (a.X < b.X) or ((a.X=b.X) and (a.y<b.Y));
end;

{ TEditTerrain }

procedure TEditTerrain.AddTile(X, Y: integer);
var
  info: TTileInfo;
begin
  info.TerType := TerrainType;
  info.TerSubtype := 0; //todo: maybe selecet random at this time
  info.X := X;
  info.Y := Y;
  info.mir := 0;

  FNewTileInfos.PushBack(info);

  //TODO: if brush mode = fill, ignore transitions
end;

constructor TEditTerrain.Create(AMap: TVCMIMap);
begin
  inherited Create(AMap);
  FOldTileInfos := TTileInfos.Create;
  FNewTileInfos := TTileInfos.Create;
end;

destructor TEditTerrain.Destroy;
begin
  FOldTileInfos.Free;
  FNewTileInfos.Free;

  inherited Destroy;
end;

procedure TEditTerrain.Execute;
var
  old_info, new_info, info: TTileInfo;

  i, dx,dy: Integer;


  loop_guard: Integer;
  x: LongInt;
  y: LongInt;
  info_n: TTileSet.PNode;
begin
  //TODO: terrain transitions

  FInQueue := TTileSet.Create;
  FOutQueue := TTileSet.Create;



  try
    //first insert all edited
    for i := 0 to FNewTileInfos.Size - 1 do
    begin
      FInQueue.Insert(FNewTileInfos[i]);
    end;

    //then insert tiles around
    for i := 0 to FNewTileInfos.Size - 1 do
    begin
      x := FNewTileInfos[i].x;
      y := FNewTileInfos[i].y;

      for dx := -1 to 2 do
      begin
        for dy := -1 to 2 do
        begin
          if not((dx= 0) and (dy=0)) and GetTileInfo(x+dx,y+dy,new_info) then
             FInQueue.Insert(new_info);
        end
      end;
    end;

    FNewTileInfos.Clear;

    //todo: more passes of validation

    loop_guard := 0; //will be useful later, now guard useless

    while (not FInQueue.IsEmpty) and (loop_guard < 1000000) do
    begin

      info_n := FInQueue.NMin;
      info := info_n^.Data;

      ProcessTile(info);

      FInQueue.Delete(info);
      FOutQueue.Insert(info);

      FNewTileInfos.PushBack(info);
      inc(loop_guard);
    end;
  finally
    FInQueue.Free;
    FOutQueue.Free;

    FInQueue := nil;
    FOutQueue := nil;
  end;

  //save old state
  FOldTileInfos.Clear;
  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    old_info := GetTileInfo(new_info.x,new_info.y);

    FOldTileInfos.PushBack(old_info);
  end;
  //apply new state
  Redo;
end;

function TEditTerrain.GetDescription: string;
begin
  Result := 'Edit terrain'; //todo: l18n
end;

function TEditTerrain.GetTileInfo(x, y: Integer): TTileInfo;
var
  t: TMapTile;
begin

  t := FMap.GetTile(FLevel,X,Y);

  Result.X := X;
  Result.Y := Y;
  Result.TerType := t.TerType;
  Result.TerSubtype := t.TerSubType;
  Result.mir := t.Flags mod 4;
end;

function TEditTerrain.GetTileInfo(x, y: Integer; out Info: TTileInfo): boolean;
begin
  Result := False;
  if FMap.IsOnMap(Level,x,y) then
  begin
    info := GetTileInfo(x,y);
    Result := true;
  end;
end;

procedure TEditTerrain.ProcessTile(var Info: TTileInfo);
var
  config: TPatterns;
  pattern : TPattern;
  bestFlip, bestPattern: Integer;
  tr_repl: String;
  i: Integer;
  flip: Integer;
  FlippedPattern: TPattern;
  vr: TValidationResult;
  mapping: TMapping;
  range: Integer;
begin
  config := FMap.TerrainManager.PatternConfig.GetTerrainConfig(info.TerType);

  bestPattern := -1;
  bestFlip := -1;

  tr_repl := '';

  for i := 0 to config.Count - 1 do
  begin
    pattern := config[i];

    for flip := 0 to 4 - 1 do
    begin
      FlippedPattern := FMap.TerrainManager.PatternConfig.GetFlippedPattern(pattern, flip);
      vr := ValidateTerrainView(info,FlippedPattern);
      if vr.result then
      begin
        bestPattern := i;
        bestFlip := flip;
        tr_repl := vr.transitionReplacement;
        Break;
      end;

      FlippedPattern.Free;
    end;
  end;

  if bestPattern = -1 then
  begin
    raise Exception.Create('No pattern detected');
    //Exit;
  end;

  pattern := config[bestPattern];

  if tr_repl = '' then
  begin
    mapping := pattern.Mappings[0];
  end
  else begin
    if tr_repl = RULE_DIRT then
    begin
      mapping := pattern.Mappings[0];
    end
    else begin
      mapping := pattern.Mappings[1];
    end;
  end;

  if pattern.FlipMode = FLIP_MODE_SAME_IMAGE then
  begin
    info.TerSubtype := RandomRange(mapping.Lower,mapping.Upper);
    Info.mir := bestFlip;
  end
  else begin
    range := (mapping.Upper - mapping.Lower) div 4;
    info.TerSubtype := RandomRange(mapping.Lower + bestFlip*range, mapping.Lower + (bestFlip + 1)*range -1);
    info.mir := 0;
  end;
end;

procedure TEditTerrain.Redo;
var
  new_info: TTileInfo;
  i: Integer;
begin
  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    new_info := FNewTileInfos[i];

    FMap.SetTerrain(FLevel,
      new_info.X,
      new_info.Y,
      new_info.TerType,
      new_info.TerSubtype,
      new_info.mir);
  end;

end;

procedure TEditTerrain.SetBrushMode(AValue: TBrushMode);
begin
  if FBrushMode = AValue then Exit;
  FBrushMode := AValue;
end;


procedure TEditTerrain.SetLevel(AValue: Integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

procedure TEditTerrain.SetTerrainType(AValue: TTerrainType);
begin
  if FTerrainType = AValue then Exit;
  FTerrainType := AValue;
end;

procedure TEditTerrain.Undo;
var
  i: Integer;
begin
  for i := 0 to FOldTileInfos.Size - 1 do
  begin
    UndoTile(FOldTileInfos[i]);
  end;
end;

procedure TEditTerrain.UndoTile(constref Tile: TTileInfo);
begin
  FMap.SetTerrain(FLevel, Tile.X, Tile.Y,Tile.TerType, Tile.TerSubtype, Tile.mir);
end;

function TEditTerrain.ValidateTerrainView(info: TTileInfo; pattern: TPattern;
  recDepth: integer): TValidationResult;


  function getTinfo(x,y: integer ): TTileInfo;
  var
    tmp: TTileInfo;
    n: TTileSet.PNode;
  begin
    tmp.X := x;
    tmp.Y := Y;

    n := FInQueue.NFind(tmp);

    if Assigned(n) then
    begin
      getTinfo := n^.Data;
      exit;
    end;

    n := FOutQueue.NFind(tmp);

    if Assigned(n) then
    begin
      getTinfo := n^.Data;
      exit;
    end;

    getTinfo := GetTileInfo(x,y);
  end;


var
  centerTerType: TTerrainType;
  totalPoints: Integer;
  transitionReplacement: String;
  i: Integer;
  cx: Integer;
  cy: Integer;
  isAlien: Boolean;

  cur_tinfo: TTileInfo;

  topPoints: Integer;
  j: Integer;
  rule: TWeightedRule;
  patternForRule: TPattern;
  rslt: TValidationResult;
  nativeTestOk: Boolean;
  dirtTestOk: Boolean;
  sandTestOK: Boolean;

  procedure applyValidationRslt(AResult:Boolean);
  begin
    if AResult then
    begin
      topPoints := Max(topPoints,rule.points);
    end;
  end;

  function isSandType(tt: TTerrainType ): Boolean;
  begin
    case tt of
      TTerrainType.water,TTerrainType.sand, TTerrainType.rock:isSandType := True  ;
    else
      isSandType := False;
    end;
  end;

begin
  Result.result := False;
  Result.transitionReplacement := '';

  centerTerType := info.TerType;
  totalPoints := 0;
  transitionReplacement := '';

  for i := 0 to 9 - 1 do
  begin

    if i = 4 then
      Continue;// The center, middle cell can be skipped

    cx := info.x + (i mod 3) - 1;
    cy := info.Y + (i div 3) - 1;

    isAlien := false;

    if not FMap.IsOnMap(FLevel, cx,cy) then
    begin
      cur_tinfo := info;
    end
    else
    begin
      cur_tinfo := getTinfo(cx,cy);

      if cur_tinfo.TerType <> centerTerType then
        isAlien := True;
    end;

    topPoints := -1;

    for j := 0 to pattern.RData[i].Size - 1 do
    begin
      rule := pattern.RData[i][j];

      if not rule.IsStandartRule then
      begin
        if recDepth = 0 then
        begin
          patternForRule := FMap.TerrainManager.PatternConfig.GetConfigById(pattern.Group,rule.name);
          rslt := ValidateTerrainView(cur_tinfo,patternForRule,1);

          if not rslt.result then
          begin
            Exit;
          end
          else
          begin
            topPoints := Max(topPoints,rule.points);
            Continue;
          end;
        end
        else
        begin
          rule.name := RULE_NATIVE;
        end;
      end;

      nativeTestOk := ((rule.name = RULE_NATIVE) or rule.IsAny) and not isAlien;

      case pattern.Group of
        TTerrainGroup.NORMAL:begin
          dirtTestOk := (rule.IsDirt or rule.IsTrans or rule.IsAny)
            and isAlien and not isSandType(cur_tinfo.TerType);

          sandTestOK := (rule.IsSand or rule.IsTrans or rule.IsAny)
           and isSandType(cur_tinfo.TerType);

          if (transitionReplacement = '')
            and (rule.IsTrans or rule.IsAny)
            and (dirtTestOk or sandTestOK) then
          begin
            if dirtTestOk then
            begin
              transitionReplacement := RULE_DIRT;
            end
            else begin
              transitionReplacement := RULE_SAND;
            end;
          end;
          applyValidationRslt(
            (dirtTestOk and (transitionReplacement <> RULE_SAND))
            or (sandTestOK and (transitionReplacement <> RULE_DIRT))
            or nativeTestOk
          );
        end;
        TTerrainGroup.DIRT:begin
          sandTestOK := rule.IsSand and isSandType(cur_tinfo.TerType);
          dirtTestOk := rule.IsDirt and not isSandType(cur_tinfo.TerType) and not nativeTestOk;

          applyValidationRslt(rule.IsAny or sandTestOK or dirtTestOk or nativeTestOk);
        end;
        TTerrainGroup.SAND:begin
          sandTestOK := rule.IsSand and isAlien;
          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk);
        end;
        TTerrainGroup.WATER:begin
          sandTestOK := rule.IsSand
            and (cur_tinfo.TerType<>TTerrainType.dirt)
            and (cur_tinfo.TerType<>TTerrainType.water);
          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk);
        end;
        TTerrainGroup.ROCK:begin
          sandTestOK := rule.IsSand
            and (cur_tinfo.TerType<>TTerrainType.dirt)
            and (cur_tinfo.TerType<>TTerrainType.rock);
          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk);

        end;
      end;
    end;

    if topPoints  = -1 then
    begin
      Exit;
    end
    else begin
      totalPoints += topPoints;
    end;
  end;

  if pattern.MinPoints > totalPoints then
  begin
    Exit;
  end;

  Result.result := True;
  Result.transitionReplacement := transitionReplacement;
end;

end.

