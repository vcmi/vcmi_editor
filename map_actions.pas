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
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Math, Map, gvector, gset, undo_map, editor_types, terrain;

type

  { TEditTerrain }

  TBrushMode = (none,fixed, area, fill);

  { TMapCoord }

  TMapCoord = record
    X,Y: integer;

    procedure Clear();
  end;

  operator+ (a,b:TMapCoord):TMapCoord;

type
  { TMapRect }

  TMapRect = record
    FTopLeft: TMapCoord;
    FWidth,FHeight: Integer;

    function Left(): integer;
    function Right(): integer;
    function Top(): integer;
    function Bottom(): integer;

    function TopLeft():TMapCoord;
    function TopRight():TMapCoord;
    function BottomLeft():TMapCoord;
    function BottomRight():TMapCoord;

    function Intersect(Other: TMapRect):TMapRect;

    procedure Clear();

    procedure SetFromCenter(X,Y, Width,Height: integer);
  end;



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
    flip: Integer;
  end;

  { TInvalidTiles }

  TInvalidTiles = class
  public
    ForeignTiles, NativeTiles: TTileSet;
    constructor Create();
    destructor Destroy; override;

  end;

  TEditTerrain = class(TMapUndoItem)
  strict private

    FInQueue: TTileSet;
    FOutQueue : TTileSet;

    FInvalidated: TTileSet;

    function GetTinfo(x, y: integer): TTileInfo;
    procedure UndoTile(constref Tile: TTileInfo);
    //no checking
    function GetTileInfo(x,y: Integer): TTileInfo;
    //safe, with checking
    function GetTileInfo(x,y: Integer; out Info: TTileInfo): boolean;

    procedure ProcessTile(var Info: TTileInfo);

    function ValidateTerrainView(info: TTileInfo; pattern: TPattern; recDepth: integer = 0): TValidationResult;
    function ValidateTerrainViewInner(info: TTileInfo; pattern: TPattern; recDepth: integer = 0): TValidationResult;
  strict private
    FLevel: Integer;

    FOldTileInfos: TTileInfos;
    FNewTileInfos: TTileInfos;

    FBrushMode: TBrushMode;
    FTerrainType: TTerrainType;
    procedure SetBrushMode(AValue: TBrushMode);
    procedure SetLevel(AValue: Integer);
    procedure SetTerrainType(AValue: TTerrainType);

    function MapRect:TMapRect;
    function ExtendTileAround(X,Y: integer):TMapRect;
    function SafeExtendTileAround(X,Y: integer):TMapRect;

    procedure InvalidateTerrainViews(X,Y: integer);

    function GetInvalidTiles(center: TTileInfo): TInvalidTiles;

    procedure UpdateTerrainTypes();
    procedure	UpdateTerrainViews();
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




implementation

uses
  LazLogger;

operator+(a, b: TMapCoord): TMapCoord;
begin
  result.X:=a.x+b.X;
  result.Y:=a.y+b.y;
end;

{ TInvalidTiles }

constructor TInvalidTiles.Create;
begin
  ForeignTiles := TTileSet.Create;
  NativeTiles := TTileSet.Create;
end;

destructor TInvalidTiles.Destroy;
begin
  ForeignTiles.Free;
  NativeTiles.Free;
  inherited Destroy;
end;

{ TMapCoord }

procedure TMapCoord.Clear;
begin
  X:=0;
  Y:=0;
end;

{ TMapRect }

function TMapRect.Left: integer;
begin
  Result := FTopLeft.x;
end;

function TMapRect.Right: integer;
begin
  Result := FTopLeft.x + FWidth;
end;

function TMapRect.Top: integer;
begin
  Result := FTopLeft.y;
end;

function TMapRect.Bottom: integer;
begin
  Result := FTopLeft.y + FHeight;
end;

function TMapRect.TopLeft: TMapCoord;
begin
  Result := FTopLeft;
end;

function TMapRect.TopRight: TMapCoord;
begin
  Result.X := Top();
  Result.Y := Right();
end;

function TMapRect.BottomLeft: TMapCoord;
begin
  Result.X := Bottom();
  Result.Y := Left();

end;

function TMapRect.BottomRight: TMapCoord;
begin
  Result.X := Bottom();
  Result.Y := Right();
end;

function TMapRect.Intersect(Other: TMapRect): TMapRect;
var
  intersects: Boolean;
begin
  intersects := (Right() > Other.Left())
    and (Other.Right() > Left())
    and (Bottom()>Other.Top())
    and (Other.Bottom()>Top());

  Result.Clear();

  if intersects then
  begin
    Result.FTopLeft.X:= max(Left(),Other.Left());
    Result.FTopLeft.Y:= max(Top(),Other.Top());

    Result.FWidth:= Min(Right(),Other.Right()) - Result.FTopLeft.X;
    Result.FHeight:= Min(Bottom(),Other.Bottom()) - Result.FTopLeft.Y;
  end;
end;

procedure TMapRect.Clear;
begin
  FTopLeft.Clear();
  FHeight:=0;
  FWidth:=0;
end;

procedure TMapRect.SetFromCenter(X, Y, Width, Height: integer);
begin
  Assert(width mod 2 = 1);
  Assert(Height mod 2 = 1);
  Clear();

  FTopLeft.X:= X - (Width-1) div 2;
  FTopLeft.Y:= Y - (Height-1) div 2;
  FWidth:=Width;
  FHeight:=Height;
end;


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
  info.TerSubtype := 0; //todo: maybe select random at this time
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
  it: TTileSet.TIterator;
begin
  FInQueue := TTileSet.Create;
  FOutQueue := TTileSet.Create;
  FInvalidated := TTileSet.Create;

  for i := 0 to FNewTileInfos.Size - 1 do
  begin
    InvalidateTerrainViews(FNewTileInfos[i].X,FNewTileInfos[i].Y);
  end;

  for i := 0 to FNewTileInfos.Size - 1 do
  begin
     FInQueue.Insert(FNewTileInfos[i]);
  end;

  UpdateTerrainTypes();
	UpdateTerrainViews();

  //process out queue

  FNewTileInfos.Clear;

  Assert(FInQueue.IsEmpty);

  it := FOutQueue.Min;

  while Assigned(it) and (it.next) do
     FNewTileInfos.PushBack(it.data);


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

  FOutQueue.Free;
  FInQueue.Free;
  FInvalidated.Free;
end;

function TEditTerrain.GetDescription: string;
begin
  Result := 'Edit terrain'; //todo: l18n
end;

function TEditTerrain.GetTinfo(x,y: integer): TTileInfo;
var
  tmp: TTileInfo;
  n: TTileSet.PNode;
begin
  tmp.X := x;
  tmp.Y := Y;

  n := FOutQueue.NFind(tmp);

  if Assigned(n) then
  begin
    getTinfo := n^.Data;
    exit;
  end;

  n := FInQueue.NFind(tmp);

  if Assigned(n) then
  begin
    getTinfo := n^.Data;
    exit;
  end;

  getTinfo := GetTileInfo(x,y);
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
  //config: TPatterns;
  pattern : TPattern;
  bestFlip, bestPattern: Integer;
  tr_repl: String;
  i: Integer;
  flip: Integer;
  FlippedPattern: TPattern;
  vr: TValidationResult;
  mapping: TMapping;
  range: Integer;
  ofc: Integer;
begin
  //config := FMap.TerrainManager.PatternConfig.GetTerrainConfig(info.TerType);

  //bestPattern := -1;
  //bestFlip := -1;
  //
  //tr_repl := '';
  //
  //for i := 0 to config.Count - 1 do
  //begin
  //  pattern := config[i];
  //
  //  for flip := 0 to 4 - 1 do
  //  begin
  //    //FlippedPattern := FMap.TerrainManager.PatternConfig.GetFlippedPattern(pattern, flip);
  //    vr := ValidateTerrainView(info,FlippedPattern);
  //    if vr.result then
  //    begin
  //      bestPattern := i;
  //      bestFlip := flip;
  //      tr_repl := vr.transitionReplacement;
  //      FreeAndNil(FlippedPattern);
  //      Break;
  //    end;
  //
  //    FreeAndNil(FlippedPattern);
  //  end;
  //end;
  //
  //if bestPattern = -1 then
  //begin
  //  DebugLn('No pattern detected at (X:%d , Y:%d, L: %d)',[Info.X,Info.Y,FLevel]);
  //  Exit;
  //end;
  //
  //pattern := config[bestPattern];
  //
  //if tr_repl = '' then
  //begin
  //  mapping := pattern.Mappings[0];
  //end
  //else begin
  //  if tr_repl = RULE_DIRT then
  //  begin
  //    mapping := pattern.Mappings[0];
  //  end
  //  else begin
  //    mapping := pattern.Mappings[1];
  //  end;
  //end;

  //if pattern.FlipMode = TFlipMode.sameImage then
  //begin
  //  if (Info.mir <> bestFlip)
  //    or (Info.TerSubtype<mapping.Lower)
  //    or (Info.TerSubtype>mapping.Upper)
  //    then
  //  begin
  //    info.TerSubtype := RandomRange(mapping.Lower,mapping.Upper);
  //    Info.mir := bestFlip;
  //  end;
  //end
  //else if (info.TerType = TerrainType.rock) and (mapping.Lower = 16)  then
  //begin
  //  //todo: fix workaround
  //   range := 2;
  //
  //   ofc := mapping.Lower + (bestFlip mod 2) * range;
  //
  //    info.TerSubtype := RandomRange(ofc, ofc + range);
  //    info.mir := 0;
  //
  //end
  //else begin
  //  range := (mapping.Upper - mapping.Lower) div 4 + 1;
  //  ofc := mapping.Lower + bestFlip*range;
  //
  //  info.TerSubtype := RandomRange(ofc, ofc + range);
  //  info.mir := 0;
  //end;
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

function TEditTerrain.MapRect: TMapRect;
begin
  Result.Clear();
  Result.FWidth:=FMap.Width;
  Result.FHeight:=FMap.Height;
end;

function TEditTerrain.ExtendTileAround(X, Y: integer): TMapRect;
begin
  Result.SetFromCenter(x,y,3,3);
end;

function TEditTerrain.SafeExtendTileAround(X, Y: integer): TMapRect;
begin
  Result := ExtendTileAround(x,y).Intersect(MapRect());
end;

procedure TEditTerrain.InvalidateTerrainViews(X, Y: integer);
var
  r: TMapRect;
  i: Integer;
  j: Integer;
begin
  r := SafeExtendTileAround(x,y);

  for i := 0 to r.FWidth - 1 do
  begin
    for j := 0 to r.FHeight - 1 do
    begin
       FInvalidated.Insert(GetTinfo(r.FTopLeft.X+i, r.FTopLeft.Y+j));
    end;
  end;

end;

function TEditTerrain.GetInvalidTiles(center: TTileInfo): TInvalidTiles;
const
  WATER_ROCK_P: array[0..1] of string = ('s1','s2');
  OTHER_P: array[0..1] of string = ('n2','n3');

var
  r: TMapRect;
  i: Integer;
  j: Integer;

  curTile, centerTile: TTileInfo;

  config: TTerrainPatternConfig;
  valid: Boolean;

  pName: string;

begin
  Result := TInvalidTiles.Create;
  config :=  fmap.TerrainManager.PatternConfig;

  centerTile := GetTinfo(center.X,center.Y);

  r := SafeExtendTileAround(center.X,center.Y);

  for i := 0 to r.FWidth - 1 do
  begin
    for j := 0 to r.FHeight - 1 do
    begin
         curTile := GetTinfo(r.FTopLeft.X+i, r.FTopLeft.Y+j);

         valid := ValidateTerrainView(curTile,config.GetTerrainTypePatternById('n1')).result;

         // Special validity check for rock & water

         if valid and (centerTile.TerType<>curTile.TerType) and (curTile.TerType in [TTerrainType.water, TTerrainType.rock]) then
         begin
           for pName in WATER_ROCK_P do
           begin
              valid := not ValidateTerrainView(curTile,config.GetTerrainTypePatternById(pName)).result;
              if not valid then
                break;
           end;
         end
         // Additional validity check for non rock OR water
         else if (not valid and not (curTile.TerType in [TTerrainType.water, TTerrainType.rock])) then
         begin
           for pName in OTHER_P do
           begin
              valid := ValidateTerrainView(curTile,config.GetTerrainTypePatternById(pName)).result;
              if valid then
                break;
           end;
         end;

         if not valid then
         begin
           if curTile.TerType = centerTile.TerType then
             Result.NativeTiles.Insert(curTile)
           else
             Result.ForeignTiles.Insert(curTile)
         end;


    end;
  end;

end;

procedure TEditTerrain.UpdateTerrainTypes;
var
  tiles: TInvalidTiles;
  centerTile:TTileInfo;

  procedure UpdateTerrain(tile:TTileInfo; RequiresValidation: Boolean);
  begin
    tile.TerType:=centerTile.TerType;
    if RequiresValidation then
      FInQueue.Insert(tile)
    else
      FOutQueue.Insert(tile);
    InvalidateTerrainViews(tile.X,tile.Y);

  end;
const
  SHIFTS: array[1..8] of TMapCoord = (
    (x:0; y:-1),(x:-1; y:0),(x:0; y:1),(x:1; y:0),
    (x:-1; y:-1),(x:-1; y:1),(x:1; y:1),(x:1; y:-1));

var
  i: Integer;
  tile, tmp:TTileInfo;

  it: TTileSet.TIterator;
  rect: TMapRect;
  SuitableTiles: TTileSet;

  TestTile: TTileInfo;

  invalidForeignTilesCnt,invalidNativeTilesCnt,nativeTilesCntNorm: Integer;
  j: Integer;

  invalid: TInvalidTiles;
  tileRequiresValidation: Boolean;

  currentCoord: TMapCoord;
  delta: TMapCoord;
begin

   while not FInQueue.IsEmpty do
   begin
     centerTile := FInQueue.NMin^.Data;

     tiles := GetInvalidTiles(centerTile);

     it := tiles.ForeignTiles.Min;

     while Assigned(it) and it.Next do
       UpdateTerrain(it.data, true);

     FreeAndNil(it);

     it:=tiles.NativeTiles.Find(centerTile);

     if Assigned(it) then
     begin
       rect := SafeExtendTileAround(centerTile.x, centerTile.y);
       SuitableTiles := TTileSet.Create;

       invalidForeignTilesCnt:=MaxInt;
       invalidNativeTilesCnt:=0;

        for i := 0 to rect.FWidth - 1 do
        begin
          for j := 0 to rect.FHeight - 1 do
          begin
            TestTile := GetTinfo(rect.FTopLeft.X+i, rect.FTopLeft.Y+j);

            if TestTile.TerType <> centerTile.TerType then
            begin
              TestTile.TerType:=centerTile.TerType;
              FOutQueue.Insert(TestTile);

              invalid := GetInvalidTiles(TestTile);

              nativeTilesCntNorm:=ifthen(invalid.NativeTiles.IsEmpty, MaxInt, invalid.NativeTiles.Size);

              if (nativeTilesCntNorm > invalidNativeTilesCnt)
                or ((nativeTilesCntNorm = invalidNativeTilesCnt) and (invalid.ForeignTiles.Size < invalidForeignTilesCnt) ) then
              begin
                invalidNativeTilesCnt := nativeTilesCntNorm;
                invalidForeignTilesCnt:=invalid.ForeignTiles.Size;
                SuitableTiles.Free;
                SuitableTiles := TTileSet.Create;

                SuitableTiles.Insert(TestTile);

              end
              else if (nativeTilesCntNorm = invalidNativeTilesCnt) and (invalid.ForeignTiles.Size = invalidForeignTilesCnt) then
              begin
                SuitableTiles.Insert(TestTile);
              end;

              invalid.Free;
              FOutQueue.Delete(TestTile);
            end;
          end;
        end;

        tileRequiresValidation := invalidForeignTilesCnt > 0;

        if SuitableTiles.Size = 1 then
        begin
          UpdateTerrain(SuitableTiles.NMin^.Data, tileRequiresValidation);
        end
        else
        begin
          //first suitable tile around center

          //TODO: why is it needed?
          for delta in SHIFTS do
          begin
             currentCoord.x := centerTile.X;
             currentCoord.Y := centerTile.Y;

             currentCoord += delta;

             tmp.X:=currentCoord.X;
             tmp.Y:=currentCoord.Y;

             it := SuitableTiles.Find(tmp);

             if Assigned(it) then
             begin
                UpdateTerrain(it.Data, tileRequiresValidation);
             end;
          end;


        end;
        SuitableTiles.Free;
     end
     else begin
        FInQueue.Delete(centerTile);
     end;
     FreeAndNil(it);

     tiles.Free;
   end;

end;

procedure TEditTerrain.UpdateTerrainViews;
begin

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
var
  flip: integer;
  flipped: TPattern;
begin
  Result.result := False;
  Result.transitionReplacement := '';
  Result.flip := 0;

  for flip := 0 to 4 - 1 do
  begin
    flipped := FMap.TerrainManager.PatternConfig.GetFlippedPattern(pattern,flip);

    Result := ValidateTerrainViewInner(info,flipped, recDepth);

    if Result.result then
    begin
      Result.flip:=flip;
      Exit;
    end;
  end;
end;

function TEditTerrain.ValidateTerrainViewInner(info: TTileInfo;
  pattern: TPattern; recDepth: integer): TValidationResult;




  function isSandType(tt: TTerrainType ): Boolean;
  begin
    case tt of
      TTerrainType.water,TTerrainType.sand, TTerrainType.rock:isSandType := True  ;
    else
      isSandType := False;
    end;
  end;

var
  centerTerType, terType: TTerrainType;
  centerTerGroup: TTerrainGroup;

  totalPoints: Integer;
  transitionReplacement: String;

  currentPos: TMapCoord;

  cur_tinfo: TTileInfo;

  i: Integer;
  j: Integer;
  cx: Integer;
  cy: Integer;
  isAlien: Boolean;
  topPoints: Integer;
  rule: TWeightedRule;
  patternForRule: TPattern;
  rslt: TValidationResult;

  procedure applyValidationRslt(AResult:Boolean);
  begin
    if AResult then
    begin
      topPoints := Max(topPoints,rule.points);
    end;
  end;
var

  nativeTestOk: Boolean;
  nativeTestStrongOk: Boolean;
  dirtTestOk: Boolean;
  sandTestOK: Boolean;
begin
  Result.result:=false;
  Result.flip:=0;
  Result.transitionReplacement:='';

  centerTerType := info.TerType;
  centerTerGroup :=TERRAIN_GROUPS[centerTerType];

  totalPoints:=0;
  transitionReplacement:='';

  for i := 0 to 9 - 1 do
  begin

    if i = 4 then
      Continue;// The center, middle cell can be skipped

    cx := info.x + (i mod 3) - 1;
    cy := info.Y + (i div 3) - 1;

    currentPos.X:=cx;
    currentPos.y:=cy;

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

    // Validate all rules per cell

    topPoints:=-1;
    for j := 0 to pattern.RData[i].Count - 1 do
    begin
      rule := pattern.RData[i][j].Clone;

      if not rule.IsStandartRule then
      begin
        if (recDepth = 0) and FMap.IsOnMap(FLevel, currentPos.X,currentPos.Y) then
        begin
          if cur_tinfo.TerType = centerTerType  then
          begin
            patternForRule := FMap.TerrainManager.PatternConfig.GetTerrainViewPatternById(TERRAIN_GROUPS[centerTerType], rule.name);

            if Assigned(patternForRule) then
            begin
                rslt := ValidateTerrainView(cur_tinfo,patternForRule,1);
                if rslt.result then
                begin
                  topPoints := Max(topPoints,rule.points);
                end;
            end;

          end;
          Continue;
        end
        else
        begin
          rule.name := RULE_NATIVE;
        end;
      end;


      // Validate cell with the ruleset of the pattern

      nativeTestOk := ((rule.name = RULE_NATIVE) or (rule.name = RULE_NATIVE_STRONG)) and not isAlien;
      nativeTestStrongOk:=nativeTestOk;

      case centerTerGroup of
        TTerrainGroup.NORMAL:
        begin
          dirtTestOk := (rule.IsDirt or rule.IsTrans)
            and isAlien and not isSandType(cur_tinfo.TerType);

          sandTestOK := (rule.IsSand or rule.IsTrans)
           and isSandType(cur_tinfo.TerType);

          if (transitionReplacement = '')
            and (rule.IsTrans)
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

          if rule.IsTrans then
          begin
            applyValidationRslt(
              (dirtTestOk and (transitionReplacement <> RULE_SAND))
              or (sandTestOK and (transitionReplacement <> RULE_DIRT))
            );
          end
          else begin
            applyValidationRslt( rule.IsAny or dirtTestOk or sandTestOK or nativeTestOk);
          end;
        end;
        TTerrainGroup.DIRT:
        begin
          nativeTestOk:=rule.IsNative and not isSandType(cur_tinfo.TerType);

          sandTestOK := (rule.IsSand or rule.IsTrans) and isSandType(cur_tinfo.TerType);

          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk or nativeTestStrongOk);
        end;
        TTerrainGroup.SAND:
        begin
          applyValidationRslt(true);
        end;
        TTerrainGroup.WATER,
        TTerrainGroup.ROCK:
        begin
          sandTestOK := (rule.IsSand or rule.IsTrans) and isAlien;
          applyValidationRslt(rule.IsAny or sandTestOK or nativeTestOk);
        end;
      end;

      rule.free;
    end;

    if topPoints = -1 then
       Exit
    else
       totalPoints+=topPoints;
  end;

  if (totalPoints >= pattern.MinPoints) and (totalPoints <= pattern.MaxPoints) then
  begin
    Result.result:=true;
    Result.transitionReplacement:=transitionReplacement;
  end

end;

end.

