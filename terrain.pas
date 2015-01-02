{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
unit terrain;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, contnrs,
   fgl, gvector,
  editor_types, editor_graphics, vcmi_json,
  filesystem_base, editor_classes, transitions;

const

  RULE_DIRT = 'D';
  RULE_SAND = 'S';
  RULE_TRANSITION = 'T';
  RULE_NATIVE = 'N';
  RULE_NATIVE_STRONG = 'N!';
  RULE_ANY = '?';

type

  TFlipMode = (sameImage,diffImages);

  TTerrainGroup = (
    NORMAL,
    DIRT,
    SAND,
    WATER,
    ROCK);

const

  TERRAIN_GROUPS: array [TTerrainType] of TTerrainGroup = (
   TTerrainGroup.DIRT,
   TTerrainGroup.SAND,
   TTerrainGroup.NORMAL,
   TTerrainGroup.NORMAL,
   TTerrainGroup.NORMAL,
   TTerrainGroup.NORMAL,
   TTerrainGroup.NORMAL,
   TTerrainGroup.NORMAL,
   TTerrainGroup.WATER,
   TTerrainGroup.ROCK);

type

  { TWeightedRule }

  TWeightedRule = class
    name : string;
    points: Integer;
    constructor Create();
    destructor Destroy; override;
    function IsStandartRule: boolean;
    function IsDirt: boolean; inline;
    function IsSand: boolean; inline;
    function IsTrans: boolean; inline;
    function IsAny: boolean; inline;
    function IsNative: boolean; inline;
    function Clone: TWeightedRule;
  end;

  //todo: use object or record
  TRules = specialize TFPGObjectList<TWeightedRule>;


  TRulesArray =  array[0..8] of TRules ;


  {$push}
  {$m+}

  { TMappingTemplate }

  TMappingTemplate = class (TNamedCollectionItem)
  private
    FValue: string;
    procedure SetValue(AValue: string);
  public
     constructor Create(ACollection: TCollection); override;
     destructor Destroy; override;
  published
    property Value: string read FValue write SetValue;
  end;

  { TMappingTemplates }

  TMappingTemplates = class(specialize TGNamedCollection<TMappingTemplate>)
  public
    constructor Create;

  end;

  { TPatternTemplate }

  TPatternTemplate = class (TCollectionItem)
  private
    FId: string;
    FMapping: TMappingTemplates;
    FMinPoints: integer;
    FData: TStringList;
    function GetData: TStrings;
    procedure SetId(AValue: string);
    procedure SetMinPoints(AValue: integer);
  public
     constructor Create(ACollection: TCollection); override;
     destructor Destroy; override;
  published
    property Id:string read FId write SetId;
    property MinPoints: integer read FMinPoints write SetMinPoints;
    property Mapping: TMappingTemplates read FMapping;
    property Data: TStrings read GetData;
  end;


  { TPatternTemplates }

  TPatternTemplates = class(specialize TGArrayCollection<TPatternTemplate>)
  public
    constructor Create;

  end;

  { TPattern }

  TPattern = class (TPersistent)
  strict private
    FData: TRulesArray;
    FID: string;
    FMinPoints: integer;
    FDiffImages: Boolean;
    FMappings: TMappings;
    FMaxPoints: integer;
    FRotationTypesCount: Integer;
    function GetRData(idx: Integer): TRules;
    procedure SetID(AValue: string);
    procedure SetMinPoints(AValue: integer);
    procedure SetDiffImages(AValue: Boolean);
    procedure SetMaxPoints(AValue: integer);
    procedure SetRotationTypesCount(AValue: Integer);
    procedure FillFrom(ATemplate: TPatternTemplate);
  private
    FFlipped: array[1..3] of TPattern;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create();
    constructor Create(ATemplate: TPatternTemplate);
    destructor Destroy; override;
    procedure SwapRules(idx1,idx2: Integer);
    property RData[idx:Integer]: TRules read GetRData;
    property Id:string read FID write SetID;
    property MinPoints: integer read FMinPoints write SetMinPoints;
    property MaxPoints: integer read FMaxPoints write SetMaxPoints;
    property DiffImages: Boolean read FDiffImages write SetDiffImages;
    property RotationTypesCount: Integer read FRotationTypesCount write SetRotationTypesCount;
    property Mappings:TMappings read FMappings;
  end;


  TPatternsVector = specialize TFPGObjectList<TPattern>;
  TTerrainViewMap = specialize TFPGMap<TTerrainGroup, TPatternsVector>;
  TTerrainTypeMap = specialize TFPGMap<string, TPattern>;

  { TTerrainPatternConfig }

  TTerrainPatternConfig = class
  private
    FTerrainType: TPatternTemplates;
    FTerrainView: TPatternTemplates;
    FViewMap: TTerrainViewMap;
    FTypeMap: TTerrainTypeMap;
    FFreeList: TFPObjectList;
    procedure FlipPattern(APattern: TPattern; flip:Integer);
    procedure PreparePattern(APattern: TPattern);
    procedure ConvertTerrainTypes;
    procedure ConvertTerrainViews;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConvertConfig;
    function GetFlippedPattern(const APattern: TPattern; flip:Integer ): TPattern;
    function GetTerrainViewPatternsForGroup(AGroup: TTerrainGroup): TPatternsVector;
    //may return nil
    function GetTerrainViewPatternById(AGroup: TTerrainGroup; AId:string): TPattern;
    //will raise if not found
    function GetTerrainTypePatternById(AId:string): TPattern;
    function GetTerrainGroup(AGroup:string): TTerrainGroup;
  published
    property TerrainView:TPatternTemplates read FTerrainView;
    property TerrainType:TPatternTemplates read FTerrainType;
  end;


  {$pop}

  { TTerrainManager }

  TTerrainManager = class (TGraphicsCosnumer)
  private
    FTerrainDefs: array [TTerrainType] of TDef;

    FRiverDefs: array [TRiverType.clearRiver..TRiverType.lavaRiver] of TDef;

    FRoadDefs: array [TRoadType.dirtRoad..TRoadType.cobblestoneRoad] of TDef;

    FPatternConfig: TTerrainPatternConfig;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadConfig;

    procedure LoadTerrainGraphics;

    procedure Render(const tt: TTerrainType; sbt: UInt8; X, Y: Integer; Flags: UInt8);

    procedure RenderRoad(const rdt: TRoadType; const Dir: UInt8; X, Y: Integer;  Flags: UInt8);

    procedure RenderRiver(const rt: TRiverType; const Dir: UInt8; X, Y: Integer; Flags: UInt8 );

    function GetDefaultTerrain(const Level: Integer): TTerrainType;
    function GetRandomNormalSubtype(const tt: TTerrainType): UInt8;

    property PatternConfig: TTerrainPatternConfig read FPatternConfig;
  end;

implementation

uses
  RegExpr, LazLogger, editor_consts;

type
  TTerrainViewInterval = record
    min, max: uint8;
  end;

const

  TERRAIN_DEF_FILES: array[TTerrainType] of string = (
    'DIRTTL',
    'SANDTL',
    'GRASTL',
    'SNOWTL',
    'SWMPTL',
    'ROUGTL',
    'SUBBTL',
    'LAVATL',
    'WATRTL',
    'ROCKTL'
    );

  RIVER_DEF_FILES: array[TRiverType.clearRiver..TRiverType.lavaRiver] of string =
  (
    'CLRRVR','ICYRVR','MUDRVR','LAVRVR'
  );

  ROAD_DEF_FILES: array[TRoadType.dirtRoad..TRoadType.cobblestoneRoad] of string =
  (
     'DIRTRD','GRAVRD','COBBRD'
  );


  TERRAIN_CONFIG_FILE = 'config/terrainViewPatterns.json';

procedure SetView(out V: TTerrainViewInterval; min,max: uint8);
begin
  v.max:=max;
  v.min:=min;
end;

procedure RexpSplit (rexp: TRegExpr; const AInputStr : RegExprString; APieces : TStrings);
var
  PrevPos : PtrInt;
  s : string;
begin
 PrevPos := 1;
 if rexp.Exec (AInputStr) then
  REPEAT
   s := Copy (AInputStr, PrevPos, rexp.MatchPos [0] - PrevPos);
   APieces.Add (Trim(s));
   PrevPos := rexp.MatchPos [0] + rexp.MatchLen [0];
  UNTIL not rexp.ExecNext;
 if PrevPos <= Length(AInputStr) then
 begin
   s :=  Copy (AInputStr, PrevPos, MaxInt);

   APieces.Add(Trim(s));
  end;
end;

procedure RexpSplit (Expr: string; const AInputStr : RegExprString; APieces : TStrings);
var
  o: TRegExpr;
begin
  o := TRegExpr.Create;
  try
    o.Expression:=Expr;
    RexpSplit(o, AInputStr, APieces);
  finally
    o.Free;
  end;

end;

procedure RexpSplit2(rexp: TRegExpr; const AInputStr : RegExprString; out AFirst: string; out ASecond: String);
var
  sl: TStringList;
begin
  AFirst:='';
  ASecond:='';
  sl := TStringList.Create;
  try
    RexpSplit(rexp, AInputStr, sl);

    if sl.Count>0 then
    begin
      AFirst:=sl[0];
    end;

    if sl.Count>1 then
    begin
      ASecond:=sl[1];
    end;

    if sl.Count>2 then
    begin
      assert(false);
    end;

  finally
    sl.Free;
  end;
end;

procedure RexpSplit2(rexp: String; const AInputStr : RegExprString; out AFirst: string; out ASecond: String);
var
  o: TRegExpr;
begin
  o := TRegExpr.Create;
  try
    o.Expression:=rexp;
    RexpSplit2(o, AInputStr, AFirst, ASecond);
  finally
    o.Free;
  end;
end;

{ TMappingTemplates }

constructor TMappingTemplates.Create;
begin
  inherited Create;
end;

{ TMappingTemplate }

procedure TMappingTemplate.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TMappingTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TMappingTemplate.Destroy;
begin
  inherited Destroy;
end;

{ TPatternTemplates }

constructor TPatternTemplates.Create;
begin
  inherited Create;
end;


{ TPatternTemplate }

function TPatternTemplate.GetData: TStrings;
begin
  Result := FData;
end;

procedure TPatternTemplate.SetId(AValue: string);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TPatternTemplate.SetMinPoints(AValue: integer);
begin
  if FMinPoints=AValue then Exit;
  FMinPoints:=AValue;
end;

constructor TPatternTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FData := TStringList.Create;
  FMapping := TMappingTemplates.Create;
end;

destructor TPatternTemplate.Destroy;
begin
  FMapping.Free;
  FData.Free;
  inherited Destroy;
end;

{ TWeightedRule }

function TWeightedRule.Clone: TWeightedRule;
begin
  Result := TWeightedRule.Create();
  Result.name := name;
  Result.points := points;
end;

constructor TWeightedRule.Create;
begin
  points := 0;
  name := '';
end;

destructor TWeightedRule.Destroy;
begin
  inherited Destroy;
end;

function TWeightedRule.IsAny: boolean;
begin
  Result := name = RULE_ANY;
end;

function TWeightedRule.IsNative: boolean;
begin
  Result := name = RULE_NATIVE;
end;

function TWeightedRule.IsDirt: boolean;
begin
  Result := name = RULE_DIRT;
end;

function TWeightedRule.IsSand: boolean;
begin
  Result := name = RULE_SAND;
end;

function TWeightedRule.IsStandartRule: boolean;
begin
  Result := (name = RULE_ANY)
    or (name = RULE_DIRT)
    or (name = RULE_NATIVE)
    or (name = RULE_SAND)
    or (name = RULE_TRANSITION)
    or (name = RULE_NATIVE_STRONG);
end;

function TWeightedRule.IsTrans: boolean;
begin
   Result := name = RULE_TRANSITION;
end;

{ TTerrainPatternConfig }

procedure TTerrainPatternConfig.ConvertConfig;
begin
  ConvertTerrainTypes();
  ConvertTerrainViews();
end;

procedure TTerrainPatternConfig.FlipPattern(APattern: TPattern; flip: Integer);
var
  i: Integer;
  y: Integer;

begin
  Assert(flip > 0);
  Assert(flip <= 3);
  if flip in [FLIP_PATTERN_HORIZONTAL, FLIP_PATTERN_BOTH] then
    for i := 0 to 3 - 1 do
    begin
      y := i*3;
      APattern.SwapRules(Y+2,Y);
    end;


  if flip in [FLIP_PATTERN_VERTICAL, FLIP_PATTERN_BOTH] then
  begin
    for i := 0 to 3 - 1 do
    begin
      APattern.SwapRules(i,i+6);
    end;
  end;
end;

procedure TTerrainPatternConfig.PreparePattern(APattern: TPattern);
var
  flipped:TPattern;
  flip: Integer;
begin
  for flip := 1 to 4 - 1 do
  begin
    flipped := TPattern.Create();
    APattern.AssignTo(flipped);
    FlipPattern(flipped,flip);
    APattern.FFlipped[flip] := flipped;
  end;
end;

procedure TTerrainPatternConfig.ConvertTerrainTypes;
var
  pattern: TPattern;
  i: Integer;
  template: TPatternTemplate;
begin

  for i := 0 to FTerrainType.Count - 1 do
  begin
    template := FTerrainType[i];
    pattern := TPattern.Create(template);

    PreparePattern(pattern);

    FTypeMap.Add(pattern.Id,pattern);
    FFreeList.Add(pattern);
  end;

end;

procedure TTerrainPatternConfig.ConvertTerrainViews;
var
  pattern: TPattern;
  i: Integer;
  template: TPatternTemplate;
  j: Integer;

  MappingString: String;
  FlipModeStr, s1,s2: string;

  MappingsList: TStringList;
  k: Integer;
  Lower: string;
  Upper: String;

  m: TMapping;
begin
  MappingsList := TStringList.Create;

  try
    for i := 0 to FTerrainView.Count - 1 do
    begin
      template := FTerrainView[i];

      for j := 0 to template.Mapping.Count - 1 do
      begin
        pattern := TPattern.Create(template);

        MappingString := template.Mapping[j].Value;

        RexpSplit2('\s*:\s*',MappingString, FlipModeStr, s2);

        if s2<>'' then
        begin
          //we have flip mode
          MappingString := s2;
          pattern.DiffImages:= ('D' = Copy(FlipModeStr, Length(FlipModeStr), 1));
          if pattern.DiffImages then
          begin
            pattern.RotationTypesCount:=StrToInt(Copy(FlipModeStr,1, Length(FlipModeStr)-1));
            Assert((pattern.RotationTypesCount = 2) or (pattern.RotationTypesCount = 4));
          end;
        end;
        MappingsList.Clear;
        RexpSplit('\s*,\s*', MappingString, MappingsList);

        for k := 0 to MappingsList.Count - 1 do
        begin
          RexpSplit2('\s*-\s*', MappingsList[k], Lower, Upper);

          m.Lower:=StrToInt(Lower);

          if Upper = '' then
          begin
            m.Upper:=m.Lower;
          end
          else begin
            m.Upper:=StrToInt(Upper);
          end;
          pattern.Mappings.PushBack(m);
        end;
        PreparePattern(pattern);
        FViewMap.KeyData[GetTerrainGroup(template.Mapping[j].DisplayName)].Add(pattern);
      end;
    end;
  finally
    MappingsList.Free;
  end;
end;


constructor TTerrainPatternConfig.Create;
var
  tt: TTerrainGroup;
  v: TPatternsVector;
begin
  FFreeList := TFPObjectList.Create(true);

  FTerrainType := TPatternTemplates.Create;
  FTerrainView := TPatternTemplates.Create;

  FViewMap := TTerrainViewMap.Create;
  FTypeMap := TTerrainTypeMap.Create;

  for tt in TTerrainGroup do
  begin
    v:=TPatternsVector.Create(true);
    FViewMap.Add(tt,v);
    FFreeList.Add(v);
  end;
end;

destructor TTerrainPatternConfig.Destroy;
begin

  FTypeMap.Free;
  FViewMap.Free;

  FTerrainView.Free;
  FTerrainType.Free;

  FFreeList.Free;
  inherited Destroy;
end;

function TTerrainPatternConfig.GetFlippedPattern(const APattern: TPattern;
  flip: Integer): TPattern;
begin
  if flip = 0 then
  begin
    Exit(APattern);
  end
  else begin
    Assert(flip > 0);
    Assert(flip < 4);
    Exit(APattern.FFlipped[flip]);
  end;
end;

function TTerrainPatternConfig.GetTerrainViewPatternsForGroup(
  AGroup: TTerrainGroup): TPatternsVector;
begin
  Result := FViewMap.KeyData[AGroup];
end;

function TTerrainPatternConfig.GetTerrainViewPatternById(AGroup: TTerrainGroup;
  AId: string): TPattern;
var
  item : TPattern;
begin
  for item in GetTerrainViewPatternsForGroup(AGroup) do
  begin
    if item.Id = AID then
       Exit(item);
  end;

  Exit(nil);
end;

function TTerrainPatternConfig.GetTerrainTypePatternById(AId: string): TPattern;
begin
  Result := FTypeMap.KeyData[AID];
end;

function TTerrainPatternConfig.GetTerrainGroup(AGroup: string): TTerrainGroup;
begin
  case AGroup of
    'normal': Result := TTerrainGroup.NORMAL;
    'dirt':  Result := TTerrainGroup.DIRT;
    'sand':  Result := TTerrainGroup.SAND;
    'water':  Result := TTerrainGroup.WATER;
    'rock':  Result := TTerrainGroup.ROCK;
  else
    raise Exception.Create('Unknown terrain typename '+AGroup);
  end;
end;


{ TPattern }

procedure TPattern.AssignTo(Dest: TPersistent);
var
  dest_o: TPattern;
  tmp: TWeightedRule;
  i: Integer;
  j: Integer;
begin
  if Dest is TPattern then
  begin
    dest_o := TPattern(Dest);

    dest_o.ID := ID;
    dest_o.MinPoints := MinPoints;
    dest_o.MaxPoints := MaxPoints;
    dest_o.FDiffImages:=FDiffImages;
    dest_o.FRotationTypesCount:=FRotationTypesCount;

    for i := 0 to 9 - 1 do
    begin
      for j := 0 to RData[i].Count - 1 do
      begin
        tmp := RData[i].Items[j].Clone();

        dest_o.RData[i].Add(tmp);
      end;
    end;

    for i := 0 to SizeInt(FMappings.Size) - 1 do
    begin
      dest_o.FMappings.PushBack(FMappings[i]);
    end;
  end
  else begin
    inherited AssignTo(Dest);
  end;
end;

constructor TPattern.Create();
var
  i: Integer;
begin
  for i := Low(FData) to High(FData) do
  begin
    FData[i] := TRules.Create(True);
  end;

  FMinPoints:=0;
  FMaxPoints:=MaxInt;
  FDiffImages:=false;

  FMappings := TMappings.Create;
end;

constructor TPattern.Create(ATemplate: TPatternTemplate);
begin
  Create();
  FillFrom(ATemplate);
end;

destructor TPattern.Destroy;
var
  i: Integer;
begin
  FMappings.Free;

  for i := Low(FData) to High(FData) do
  begin
    FData[i].Free;
  end;

  for i := Low(FFlipped) to High(FFlipped) do
  begin
     FFlipped[i].Free;
  end;

  inherited Destroy;
end;

function TPattern.GetRData(idx: Integer): TRules;
begin
  Result := FData[idx];
end;

procedure TPattern.SetDiffImages(AValue: Boolean);
begin
  if FDiffImages=AValue then Exit;
  FDiffImages:=AValue;
end;

procedure TPattern.SetMaxPoints(AValue: integer);
begin
  if FMaxPoints=AValue then Exit;
  FMaxPoints:=AValue;
end;

procedure TPattern.SetRotationTypesCount(AValue: Integer);
begin
  if FRotationTypesCount=AValue then Exit;
  FRotationTypesCount:=AValue;
end;

procedure TPattern.FillFrom(ATemplate: TPatternTemplate);
var
  rexp: TRegExpr;

  i: Integer;

  tmp:TStringList;

  rule: TWeightedRule;
  j: Integer;
  p: SizeInt;

  FStrData: TStrings;

begin
  FID := ATemplate.Id;
  FMinPoints:=ATemplate.MinPoints;
  //todo: maxpoints

  FStrData := ATemplate.Data;

  if not FStrData.Count = 9 then raise Exception.Create('terrain config invalid');

  tmp := TStringList.Create;
  rexp := TRegExpr.Create;
  try
    rexp.Expression := '\s*,\s*';

    for I := Low(FData) to High(FData) do
    begin
      tmp.Clear;
      FData[i].Clear;
      RexpSplit(rexp, FStrData[i],tmp);

      for j := 0 to tmp.Count - 1 do
      begin
        rule := TWeightedRule.Create();
        p := Pos('-',tmp[j]);

        if p <> 0 then
        begin
          rule.name := Copy(tmp[j],1,p-1);

          rule.points := StrToInt(Copy(tmp[j],p+1,MaxInt));
        end
        else
        begin
          rule.name := tmp[j];
        end;

        FData[i].Add(rule);
      end;

    end;
  finally
   rexp.Free;
   tmp.Free;
  end;

end;

procedure TPattern.SetID(AValue: string);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TPattern.SetMinPoints(AValue: integer);
begin
  if FMinPoints = AValue then Exit;
  FMinPoints := AValue;
end;

procedure TPattern.SwapRules(idx1, idx2: Integer);
var
  tmp: TRules;
begin
  tmp := FData[idx1];
  FData[idx1] := FData[idx2];
  FData[idx2] := tmp;
end;

{ TTerrainManager }

constructor TTerrainManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPatternConfig := TTerrainPatternConfig.Create;
end;

destructor TTerrainManager.Destroy;
begin
  FPatternConfig.Free;
  inherited Destroy;
end;

function TTerrainManager.GetDefaultTerrain(const Level: Integer): TTerrainType;
begin
  if Level <=0 then
  begin
    Result := TTerrainType.water;
  end
  else begin
    Result := TTerrainType.rock;
  end;
end;

function TTerrainManager.GetRandomNormalSubtype(const tt: TTerrainType): UInt8;
var
  vews: TTerrainViewInterval;
begin
  Result :=0;
  case tt of
    TTerrainType.dirt:SetView(vews,21,44);
    TTerrainType.sand:SetView(vews,0,23);
    TTerrainType.grass,
    TTerrainType.snow,
    TTerrainType.swamp,
    TTerrainType.rough,
    TTerrainType.subterranean,
    TTerrainType.lava:SetView(vews,49,63); //SetView(vews,49,72);
    TTerrainType.water:SetView(vews,20,32);
    TTerrainType.rock: SetView(vews,0,0);
  else
    raise Exception.Create('Unknown terrain: '+IntToStr(Ord(tt)));
  end;

  { TODO : Handle decorative tiles }
  Result := Random(vews.max-vews.min)+vews.min;

end;

procedure TTerrainManager.LoadConfig;
var
  config: TJsonResource;
begin
  config := TJsonResource.Create;
  try
    ResourceLoader.LoadResource (config,TResourceType.Json,TERRAIN_CONFIG_FILE);
    config.DestreamTo(FPatternConfig,'');

  finally
    config.Free;
  end;
  FPatternConfig.ConvertConfig;
end;

procedure TTerrainManager.LoadTerrainGraphics;
var
  tt: TTerrainType;
  rt: TRiverType;
  rdt: TRoadType;
begin
  for tt := Low(TTerrainType) to High(TTerrainType) do
  begin
    FTerrainDefs[tt] := GraphicsManager.GetGraphics(TERRAIN_DEF_FILES[tt])
  end;

  for rt in [TRiverType.clearRiver..TRiverType.lavaRiver] do
  begin
    FRiverDefs[rt] := GraphicsManager.GetGraphics(RIVER_DEF_FILES[rt]);
  end;

  for rdt in [TRoadType.dirtRoad..TRoadType.cobblestoneRoad] do
  begin
    FRoadDefs[rdt] := GraphicsManager.GetGraphics(ROAD_DEF_FILES[rdt]);
  end;
end;

procedure TTerrainManager.Render(const tt: TTerrainType; sbt: UInt8; X,
  Y: Integer; Flags: UInt8);
begin
  FTerrainDefs[tt].RenderF(sbt, x*TILE_SIZE, y*TILE_SIZE,Flags);
end;

procedure TTerrainManager.RenderRiver(const rt: TRiverType; const Dir: UInt8;
  X, Y: Integer; Flags: UInt8);
begin
  if rt <> TRiverType.noRiver then
  begin
    FRiverDefs[rt].RenderF(dir,x*TILE_SIZE, y*TILE_SIZE, Flags shr 2);
  end;
end;

procedure TTerrainManager.RenderRoad(const rdt: TRoadType; const Dir: UInt8; X,
  Y: Integer; Flags: UInt8);
begin
  if rdt <> TRoadType.noRoad then
  begin
    FRoadDefs[rdt].RenderF(dir,x*TILE_SIZE, y*TILE_SIZE + TILE_SIZE div 2, Flags shr 4);
  end;
end;

end.
