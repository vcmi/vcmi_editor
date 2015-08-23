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
unit Map;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, fgl, LCLIntf, fpjson, gvector, gpriorityqueue,
  editor_types, editor_consts, terrain, editor_classes, editor_graphics,
  objects, object_options, lists_manager, logical_id_condition,
  logical_event_condition, logical_expression, vcmi_json, object_link;

const
  MAP_DEFAULT_SIZE = 36;
  MAP_DEFAULT_LEVELS = 1;

  MAP_PLAYER_COUNT = 8;

type
  TVCMIMap = class;

  IMapWriter = interface
    procedure Write(AStream: TStream; AMap: TVCMIMap);
  end;

  TMapCreateParams = object
  public
    Width: Integer;
    Height: Integer;
    Levels: Integer;
  end;

  {$push}
  {$m+}

  { TPlasedHero }

  TPlasedHero = class (TCollectionItem)
  private
    FName: TLocalizedString;
    FType: AnsiString;
    procedure SetName(AValue: TLocalizedString);
    procedure SetType(AValue: AnsiString);
  published
    property &type:AnsiString read FType write SetType;
    property Name: TLocalizedString read FName write SetName;
  end;

  { TPlasedHeroes }

  TPlasedHeroes = class (specialize TGArrayCollection<TPlasedHero>)
  end;

  { TPlayerAttr }

  TPlayerAttr = class(Tobject)
  private
    FOwner: IReferenceNotify;
    FAITactics: TAITactics;
    FAllowedFactions: TLogicalIDCondition;
    FCanComputerPlay: boolean;
    FCanHumanPlay: boolean;
    FPlasedHeroes: TPlasedHeroes;
    FGenerateHeroAtMainTown: boolean;
    FRandomFaction: boolean;
    FMainHeroName: TLocalizedString;
    FMainHeroPortrait: AnsiString;

    FMainTown: TObjectLink;
    FRandomHero: Boolean;
    FMainHero: AnsiString;
    FTeam: Integer;

    function HasMainTown: boolean;
    procedure SetAITactics(AValue: TAITactics);
    procedure SetCanComputerPlay(AValue: boolean);
    procedure SetCanHumanPlay(AValue: boolean);
    procedure SetGenerateHeroAtMainTown(AValue: boolean);
    procedure SetRandomFaction(AValue: boolean);
    procedure SetMainHeroName(AValue: TLocalizedString);
    procedure SetMainHeroPortrait(AValue: AnsiString);
    procedure SetRandomHero(AValue: Boolean);
    procedure SetMainHero(AValue: AnsiString);
    procedure SetTeam(AValue: Integer);

  public
    constructor Create(AOwner: IReferenceNotify);
    destructor Destroy; override;


  published
    property AllowedFactions: TLogicalIDCondition read FAllowedFactions;
    property RandomFaction: boolean read FRandomFaction write SetRandomFaction default false;

    property CanComputerPlay: boolean read FCanComputerPlay write SetCanComputerPlay;
    property CanHumanPlay: boolean read FCanHumanPlay write SetCanHumanPlay;

    property PlasedHeroes: TPlasedHeroes read FPlasedHeroes;

    property MainTown: TObjectLink read FMainTown stored HasMainTown;

    property GenerateHeroAtMainTown: boolean read FGenerateHeroAtMainTown write SetGenerateHeroAtMainTown stored HasMainTown;

    property RandomHero:Boolean read FRandomHero write SetRandomHero; //if true main hero is random and not selectable
    property MainHero: AnsiString read FMainHero write SetMainHero; //if empty main hero is selectable from all available
    property MainHeroPortrait:AnsiString read FMainHeroPortrait write SetMainHeroPortrait;
    property MainHeroName:TLocalizedString read FMainHeroName write SetMainHeroName;

    property Team: Integer read FTeam write SetTeam nodefault;
  public
    property AITactics: TAITactics read FAITactics write SetAITactics; //not used in vcmi (yet)
  end;

  { TPlayerAttrs }

  TPlayerAttrs = class
  private
    Fowner:IReferenceNotify;
    FColors : array[TPlayerColor] of TPlayerAttr;
  public
    constructor Create(AOwner: IReferenceNotify);
    destructor Destroy; override;

    function GetAttr(color: Integer): TPlayerAttr;

  published
    property Red:TPlayerAttr index Integer(TPlayerColor.Red) read GetAttr ;
    property Blue:TPlayerAttr index Integer(TPlayerColor.Blue) read GetAttr;
    property Tan:TPlayerAttr index Integer(TPlayerColor.Tan) read GetAttr;
    property Green:TPlayerAttr index Integer(TPlayerColor.Green) read GetAttr;

    property Orange:TPlayerAttr index Integer(TPlayerColor.Orange) read GetAttr;
    property Purple:TPlayerAttr index Integer(TPlayerColor.Purple) read GetAttr;
    property Teal:TPlayerAttr index Integer(TPlayerColor.Teal) read GetAttr;
    property Pink:TPlayerAttr index Integer(TPlayerColor.Pink) read GetAttr;
  end;

  { TRumor }

  TRumor = class(TCollectionItem)
  private
    FName: TLocalizedString;
    FText: TLocalizedString;
    procedure SetName(AValue: TLocalizedString);
    procedure SetText(AValue: TLocalizedString);
  published
    property Name: TLocalizedString read FName write SetName;
    property Text: TLocalizedString read FText write SetText;
  end;

  { TRumors }

  TRumors = class(specialize TGArrayCollection<TRumor>)
  public
    constructor Create;
  end;

  { TMapTile }

  PMapTile = ^TMapTile;

  TMapTile = object
  strict private
    //start binary compatible with H3 part
    FTerType: TTerrainType;
    FTerSubtype: UInt8;
    FRiverType: UInt8;
    FRiverDir: UInt8;
    FRoadType: UInt8;
    FRoadDir: UInt8;
    FFlags: UInt8;
    //end binary compatible with H3 part
    FOwner: TPlayer;
    procedure SetFlags(AValue: UInt8);
    procedure SetRiverDir(AValue: UInt8);
    procedure SetRiverType(AValue: UInt8);
    procedure SetRoadDir(AValue: UInt8);
    procedure SetRoadType(AValue: UInt8);
    procedure SetTerSubtype(AValue: UInt8);
    procedure SetTerType(AValue: TTerrainType);
  public
    constructor Create();

    procedure Render(mgr: TTerrainManager; X,Y: Integer); inline;
    procedure RenderRoad(mgr: TTerrainManager; X,Y: Integer); inline;

    property TerType: TTerrainType read FTerType write SetTerType;
    property TerSubType: UInt8 read FTerSubtype write SetTerSubtype;

    property RiverType:UInt8 read FRiverType write SetRiverType;
    property RiverDir:UInt8 read FRiverDir write SetRiverDir;
    property RoadType:UInt8 read FRoadType write SetRoadType;
    property RoadDir:UInt8 read FRoadDir write SetRoadDir;
    property Flags:UInt8 read FFlags write SetFlags;
  end;

  {$push}
  {$m+}
  { TMapObjectTemplate }

  TMapObjectTemplate = class(TObject, ISerializeNotify)
  private
    FDef: TDef;

    FAnimation: AnsiString;
    FEditorAnimation: AnsiString;
    FMask: TStrings;
    FVisitableFrom: TStrings;
    FzIndex: Integer;
    procedure SetAnimation(AValue: AnsiString);
    procedure SetEditorAnimation(AValue: AnsiString);
    procedure SetzIndex(AValue: Integer);
    procedure SetDef(AValue:TDef);

    procedure AnimationChanged;

    procedure CompactMask;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(AOther: TObjTemplate);
    procedure BeforeSerialize(Sender:TObject);
    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
  published
    property Animation: AnsiString read FAnimation write SetAnimation;
    property EditorAnimation: AnsiString read FEditorAnimation write SetEditorAnimation;
    property VisitableFrom: TStrings read FVisitableFrom;
    property Mask: TStrings read FMask;
    property ZIndex: Integer read FzIndex write SetzIndex default 0;
  end;
  {$pop}

  { TMapObject }

  TMapObject = class (TCollectionItem, IMapObject)
  strict private
    FLastFrame: Integer;
    FLastTick: DWord;
    FOptions: TObjectOptions;

    FL: integer;
    FSubtype: AnsiString;
    FTemplate: TMapObjectTemplate;
    FType: AnsiString;
    FX: integer;
    FY: integer;
    function GetIdx: integer;
    function GetPlayer: TPlayer; inline;
    procedure Render(Frame:integer; Ax,Ay: integer);
    procedure SetL(AValue: integer);
    procedure SetSubtype(AValue: AnsiString);
    procedure SetType(AValue: AnsiString);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);
  protected
    procedure TypeChanged;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure RenderStatic(); inline;
    procedure RenderStatic(X,Y: integer); inline;
    procedure RenderAnim(); inline;

    procedure RenderSelectionRect; inline;

    function CoversTile(ALevel, AX, AY: Integer): boolean;

    function GetMap:TVCMIMap;

    procedure AssignTemplate(ATemplate: TObjTemplate);

  public //ImapObject
    function GetID: AnsiString;
    function GetSubId: AnsiString;
    procedure NotifyReferenced(AIdentifier: AnsiString);
  published
    property X:integer read FX write SetX;
    property Y:integer read FY write SetY;
    property L:integer read FL write SetL;

    property &Type: AnsiString read FType write SetType;
    property Subtype: AnsiString read FSubtype write SetSubtype;

    property Template: TMapObjectTemplate read FTemplate;
  public
    property Options: TObjectOptions read FOptions;

    function HasOptions: boolean;
  end;

  { TObjPriorityCompare }

  TObjPriorityCompare = class
  public
    class function c(a,b: TMapObject): boolean;
  end;

  TMapObjectQueue = specialize TPriorityQueue<TMapObject, TObjPriorityCompare>;

  { TMapObjects }

  TMapObjects = class (specialize TGArrayCollection<TMapObject>)
  private
    FMap: TVCMIMap;
  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(AOwner: TVCMIMap);

    property Map: TVCMIMap read FMap;
  end;

  TMapEnvironment = record
    tm: TTerrainManager;
    lm: TListsManager;
    om: TObjectsManager;
  end;

  ///WIP, MULTILEVEL MAP

  { TMapLevel }

  TMapLevel = class(TNamedCollectionItem)
  strict private
    FHeight: Integer;
    FObjects: TFilename;
    FTerrain: TFilename;

    FTiles: array of array of TMapTile; //X, Y
    FWidth: Integer;
    function GetMap: TVCMIMap;
    function GetTile(X, Y: Integer): PMapTile; inline;
    procedure SetHeight(AValue: Integer);
    procedure SetObjects(AValue: TFilename);
    procedure SetTerrain(AValue: TFilename);

    procedure SetWidth(AValue: Integer);

    procedure Resize;
  protected
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property Tile[X, Y: Integer]: PMapTile read GetTile;

    property Map: TVCMIMap read GetMap;

    procedure SetRoad(x,y: integer; ARoadType: uint8; ARoadDir: UInt8; AMir: Uint8);
    procedure SetRiver(x,y: integer; ARiverType: uint8; ARriverDir: UInt8; AMir: Uint8);

    procedure BeforeSerialize;
  published
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Index;
  end;

  { TMapLevels }

  TMapLevels = class(specialize TGNamedCollection<TMapLevel>)
  private
    FOwner: TVCMIMap;
  public
    constructor Create(AOwner: TVCMIMap);
    procedure BeforeSerialize;
  end;

  { THeroDefinition }

  THeroDefinition = class (TNamedCollectionItem, ISerializeNotify, IHeroInfo)
  private
    FArtifacts: THeroArtifacts;
    FBiography: TLocalizedString;
    FExperience: UInt64;
    FName: TLocalizedString;
    FPortrait: TIdentifier;
    FPrimarySkills: THeroPrimarySkills;
    FSex: THeroSex;
    FSkills: THeroSecondarySkills;
    FSpellBook: TStrings;
    function IsPrimarySkillsStored: Boolean;
    function IsSpellBookStored: Boolean;
    procedure SetBiography(AValue: TLocalizedString);
    procedure SetExperience(AValue: UInt64);
    procedure SetName(AValue: TLocalizedString);
    procedure SetPortrait(AValue: TIdentifier);
    procedure SetSex(AValue: THeroSex);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public//ISerializeNotify
    procedure BeforeDeSerialize({%H-}Sender: TObject; {%H-}AData: TJSONData);
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData);
    procedure BeforeSerialize({%H-}Sender: TObject);
    procedure AfterSerialize(Sender: TObject; AData: TJSONData);
  public//IHeroInfo
    function GetSex: THeroSex;
    function GetBiography: TLocalizedString;
    function GetName: TLocalizedString;
  published
    property Experience: UInt64 read FExperience write SetExperience default 0;
    property Skills: THeroSecondarySkills read FSkills;
    property Artifacts: THeroArtifacts read FArtifacts;

    property Biography: TLocalizedString read FBiography write SetBiography;
    property SpellBook: TStrings read FSpellBook stored IsSpellBookStored;
    property PrimarySkills:THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property Name: TLocalizedString read FName write SetName;
    property Portrait: TIdentifier read FPortrait write SetPortrait;
  public //manual streaming
    property Sex:THeroSex read FSex write SetSex default THeroSex.default;
  end;

  { THeroDefinitions }

  THeroDefinitionsCollection = specialize TGNamedCollection<THeroDefinition>;

  THeroDefinitions = class (THeroDefinitionsCollection)
  private
    FOwner: TVCMIMap;
  public
    constructor Create(AOwner: TVCMIMap);
  end;

  TInstanceMap = specialize TFPGMap<TInstanceID, TMapObject>;


  { TVCMIMap }

  TVCMIMap = class (TPersistent, IFPObserver, IReferenceNotify)
  private
    FAllowedAbilities: TLogicalIDCondition;
    FAllowedArtifacts: TLogicalIDCondition;
    FAllowedSpells: TLogicalIDCondition;
    FAllowedHeroes: TLogicalIDCondition;

    FCurrentLevel: Integer;
    FDescription: TLocalizedString;
    FDifficulty: TDifficulty;
    FMods: TLogicalIDCondition;
    FTriggeredEvents: TTriggeredEvents;


    FLevelLimit: Integer;
    FName: TLocalizedString;
    FObjects: TMapObjects;
    FPlayers: TPlayerAttrs;
    FPredefinedHeroes: THeroDefinitions;
    FTerrainManager: TTerrainManager;
    FListsManager: TListsManager;

    FLevels: TMapLevels;

    FRumors: TRumors;

    FIsDirty: boolean;

    procedure Changed;
    function GetCurrentLevelIndex: Integer; inline;

    procedure AttachTo(AObserved: IFPObserved);
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    procedure SetCurrentLevelIndex(AValue: Integer);
    procedure SetDifficulty(AValue: TDifficulty);
    procedure SetLevelLimit(AValue: Integer);
    procedure SetDescription(AValue: TLocalizedString);
    procedure SetName(AValue: TLocalizedString);

    procedure SetTerrain(X, Y: Integer; TT: TTerrainType); overload; //set terrain with random subtype
  public
    //create with default params
    constructor CreateDefault(env: TMapEnvironment);
    //create with specified params and set default options
    constructor Create(env: TMapEnvironment; Params: TMapCreateParams);

    constructor CreateEmpty(env: TMapEnvironment);

    destructor Destroy; override;

    procedure SetTerrain(Level, X, Y: Integer; TT: TTerrainType; TS: UInt8; mir: UInt8 =0); overload;

    procedure FillLevel(TT: TTerrainType);

    function GetTile(Level, X, Y: Integer): PMapTile;

    function IsOnMap(Level, X, Y: Integer): boolean;

    //Left, Right, Top, Bottom - clip rect in Tiles
    procedure RenderTerrain(Left, Right, Top, Bottom: Integer);
    procedure RenderObjects(Left, Right, Top, Bottom: Integer);

    property CurrentLevelIndex: Integer read GetCurrentLevelIndex write SetCurrentLevelIndex;

    procedure SaveToStream(ADest: TStream; AWriter: IMapWriter);

    property IsDirty: Boolean read FIsDirty;

    property TerrainManager: TTerrainManager read FTerrainManager;
    property ListsManager: TListsManager read FListsManager;

    procedure SelectObjectsOnTile(Level, X, Y: Integer; dest: TMapObjectQueue);

    procedure BeforeSerialize;

    procedure NotifyReferenced(AIdentifier: AnsiString);
  published
    property Name:TLocalizedString read FName write SetName; //+
    property Description:TLocalizedString read FDescription write SetDescription; //+

    property Difficulty: TDifficulty read FDifficulty write SetDifficulty nodefault; //?
    property LevelLimit: Integer read FLevelLimit write SetLevelLimit default 199;//+

    property Players: TPlayerAttrs read FPlayers;

    property Rumors: TRumors read FRumors;

    property AllowedSpells: TLogicalIDCondition read FAllowedSpells;
    property AllowedAbilities: TLogicalIDCondition read FAllowedAbilities;
    property AllowedArtifacts: TLogicalIDCondition read FAllowedArtifacts;
    property AllowedHeroes: TLogicalIDCondition read FAllowedHeroes;

    property Levels: TMapLevels read FLevels;

    property PredefinedHeroes: THeroDefinitions read FPredefinedHeroes;

    property TriggeredEvents: TTriggeredEvents read FTriggeredEvents;

    property Mods: TLogicalIDCondition read FMods;
  public //manual streamimg
    property Objects: TMapObjects read FObjects;
  public
    function CurrentLevel: TMapLevel;
  end;

  {$pop}

implementation

uses FileUtil, LazLoggerBase, editor_str_consts, root_manager, editor_utils,
  strutils;

{ TMapObjectTemplate }

procedure TMapObjectTemplate.SetAnimation(AValue: AnsiString);
begin
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;
  AnimationChanged;
end;

procedure TMapObjectTemplate.SetEditorAnimation(AValue: AnsiString);
begin
  if FEditorAnimation=AValue then Exit;
  FEditorAnimation:=AValue;
  AnimationChanged;
end;

procedure TMapObjectTemplate.SetzIndex(AValue: Integer);
begin
  if FzIndex=AValue then Exit;
  FzIndex:=AValue;
end;

procedure TMapObjectTemplate.SetDef(AValue: TDef);
begin
  FDef := AValue;
  RootManager.GraphicsManager.LoadGraphics(FDef);
end;

procedure TMapObjectTemplate.AnimationChanged;
begin
  if FEditorAnimation='' then
  begin
    SetDef(RootManager.GraphicsManager.GetGraphics(FAnimation));
  end
  else
  begin
    SetDef(RootManager.GraphicsManager.GetGraphics(FEditorAnimation));
  end;
end;

procedure TMapObjectTemplate.CompactMask;
var
  stop: Boolean;
  s: AnsiString;
begin

  stop := False;

  while not stop and (FMask.Count > 0) do
  begin
    s := FMask[0];

    s := StringReplace(s, ' ', '0',[rfReplaceAll]);

    if s=StringOfChar('0', Length(s)) then
    begin
      FMask.Delete(0);
    end
    else
      stop:=true;
  end;

  //todo: trim left

  //for i := 0 to FMask.Count - 1 do
  //begin
  //  s := FMask[i];
  //end;
end;

constructor TMapObjectTemplate.Create;
begin
  FMask := TStringList.Create;
  FVisitableFrom := TStringList.Create;
  SetDef(RootManager.GraphicsManager.GetGraphics('default'));
end;

destructor TMapObjectTemplate.Destroy;
begin
  FMask.Free;
  FVisitableFrom.Free;
  inherited Destroy;
end;

procedure TMapObjectTemplate.Assign(AOther: TObjTemplate);
begin
  FAnimation := AOther.Animation;
  FEditorAnimation := AOther.EditorAnimation;
  FVisitableFrom.Assign(AOther.VisitableFrom);
  FMask.Assign(AOther.Mask);

  SetDef(AOther.Def);


  FZIndex := AOther.ZIndex;
end;

procedure TMapObjectTemplate.BeforeSerialize(Sender: TObject);
begin
  CompactMask;
end;

procedure TMapObjectTemplate.BeforeDeSerialize(Sender: TObject; AData: TJSONData
  );
begin

end;

procedure TMapObjectTemplate.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TMapObjectTemplate.AfterDeSerialize(Sender: TObject; AData: TJSONData
  );
begin
  CompactMask;
end;

{ THeroDefinition }

procedure THeroDefinition.SetExperience(AValue: UInt64);
begin
  if FExperience=AValue then Exit;
  FExperience:=AValue;
end;

procedure THeroDefinition.SetName(AValue: TLocalizedString);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure THeroDefinition.SetPortrait(AValue: TIdentifier);
begin
  if FPortrait=AValue then Exit;
  FPortrait:=AValue;
end;

procedure THeroDefinition.SetSex(AValue: THeroSex);
begin
  if FSex=AValue then Exit;
  FSex:=AValue;
end;

procedure THeroDefinition.SetBiography(AValue: TLocalizedString);
begin
  if FBiography=AValue then Exit;
  FBiography:=AValue;
end;

function THeroDefinition.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function THeroDefinition.GetSex: THeroSex;
begin
  Result := FSex;
end;

function THeroDefinition.GetBiography: TLocalizedString;
begin
  Result := FBiography;
end;

function THeroDefinition.GetName: TLocalizedString;
begin
  Result := FName;
end;

function THeroDefinition.IsSpellBookStored: Boolean;
begin
  Result := FSpellBook.Count>0;
end;

constructor THeroDefinition.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSkills := THeroSecondarySkills.Create;
  FArtifacts := THeroArtifacts.Create;
  FSpellBook := TStringList.Create;
  FPrimarySkills := THeroPrimarySkills.Create;

  FSex:=THeroSex.default;
end;

destructor THeroDefinition.Destroy;
begin
  FPrimarySkills.Free;
  FSpellBook.Free;
  FArtifacts.Free;
  FSkills.Free;
  inherited Destroy;
end;

procedure THeroDefinition.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin
  //do nothing
end;

procedure THeroDefinition.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  Sex:=LoadHeroSex(AData);
end;

procedure THeroDefinition.BeforeSerialize(Sender: TObject);
begin
  //do nothing
end;

procedure THeroDefinition.AfterSerialize(Sender: TObject; AData: TJSONData);
begin
  SaveHeroSex(AData, Sex);
end;

{ THeroDefinitions }

constructor THeroDefinitions.Create(AOwner: TVCMIMap);
begin
  inherited Create;
  Fowner := aowner;
end;

{ TMapLevels }

constructor TMapLevels.Create(AOwner: TVCMIMap);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TMapLevels.BeforeSerialize;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    items[i].BeforeSerialize;
  end;
end;

{ TRumor }

procedure TRumor.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TRumor.SetText(AValue: TLocalizedString);
begin
  if FText = AValue then Exit;
  FText := AValue;
end;

{ TRumors }

constructor TRumors.Create;
begin
  inherited Create;
end;

{ TObjPriorityCompare }

class function TObjPriorityCompare.c(a, b: TMapObject): boolean;
begin

  if a.Template.ZIndex > b.Template.ZIndex then
  begin
    Exit(True);
  end;

  if a.Template.ZIndex < b.Template.ZIndex then
  begin
    Exit(False);
  end;

  if a.Y > b.Y then
  begin
    Exit(True);
  end;

  if a.Y < b.Y then
  begin
    Exit(False);
  end;


  Exit(a.X < b.X);

end;

{ TMapObject }

function TMapObject.CoversTile(ALevel, AX, AY: Integer): boolean;
var
  w: UInt32;
  h: UInt32;
begin
  //TODO: use MASK
  w := Template.FDef.Width div TILE_SIZE;
  h := Template.FDef.Height div TILE_SIZE;
  Result := (FL = ALevel)
    and (X>=AX) and (Y>=AY)
    and (X-w<AX) and (Y-h<AY);
end;

constructor TMapObject.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLastFrame := 0;
  FTemplate := TMapObjectTemplate.Create;
  FOptions := TObjectOptions.Create(Self);
end;

destructor TMapObject.Destroy;
begin
  FreeAndNil(FTemplate);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TMapObject.GetPlayer: TPlayer;
begin
  Result := Options.Owner;
end;

function TMapObject.GetIdx: integer;
begin
  Result := Index;
end;

function TMapObject.HasOptions: boolean;
begin
  Result := Assigned(FOptions) and (FOptions.ClassType <> TObjectOptions);
end;

procedure TMapObject.Render(Frame: integer; Ax, Ay: integer);
var
  owner : TPlayer;
begin
  owner := GetPlayer;
  Template.FDef.RenderO(Frame, Ax,Ay,GetPlayer);

  if (owner <> TPlayer.none) and
    ((&type = 'hero') or (&type = 'randomHero') or (&type = 'heroPlaceholder')) then
  begin
    RootManager.GraphicsManager.GetHeroFlagDef(owner).RenderO(0, Ax, Ay);
  end;
end;

procedure TMapObject.RenderAnim;
var
  new_tick: DWord;
begin
  new_tick := GetTickCount;

  if abs(new_tick - FLastTick) > 155 then
  begin
    Inc(FLastFrame);

    FLastTick := new_tick;

    if FLastFrame >= Template.FDef.FrameCount then
      FLastFrame := 0;
  end;

  Render(FLastFrame,(x+1)*TILE_SIZE,(y+1)*TILE_SIZE);
end;

procedure TMapObject.RenderSelectionRect;
begin
  Template.FDef.RenderBorder(FX,FY);
end;

procedure TMapObject.RenderStatic;
begin
  Render(FLastFrame, (x+1)*TILE_SIZE,(y+1)*TILE_SIZE);
end;

procedure TMapObject.RenderStatic(X, Y: integer);
begin
  Render(FLastFrame, x,y);
end;

procedure TMapObject.SetL(AValue: integer);
begin
  if not GetMap.IsOnMap(AValue, FX,FY) then exit;

  if FL = AValue then Exit;

  FL := AValue;
end;

procedure TMapObject.SetSubtype(AValue: AnsiString);
begin
  if FSubtype=AValue then Exit;
  FSubtype:=AValue;
  TypeChanged;
  NotifyReferenced(AValue);
end;

procedure TMapObject.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;
  FType:=AValue;
  TypeChanged;
  NotifyReferenced(AValue);
end;

procedure TMapObject.SetX(AValue: integer);
begin
  //todo: more accurate check

  //if not GetMap.IsOnMap(FL, AValue,FY) then
  //begin
  //  DebugLn('Invalid object X position '+IntToStr(AValue));
  //  exit;
  //end;
  if FX = AValue then Exit;
  FX := AValue;
end;

procedure TMapObject.SetY(AValue: integer);
begin
  //if not GetMap.IsOnMap(FL, FX,AValue) then
  //begin
  //  DebugLn('Invalid object Y position '+IntToStr(AValue));
  //  exit;
  //end;

  if FY = AValue then Exit;
  FY := AValue;
end;

procedure TMapObject.TypeChanged;
begin
  FreeAndNil(FOptions);

  if (FType <>'') and (FSubtype<>'') then
  begin
    FOptions := CreateByID(FType, FSubtype,Self);
  end;
end;

function TMapObject.GetMap: TVCMIMap;
begin
  Result := (Collection as TMapObjects).Map;
end;

procedure TMapObject.AssignTemplate(ATemplate: TObjTemplate);
begin
  Template.Assign(ATemplate);
  &Type:=ATemplate.ObjType.DisplayName;
  Subtype := ATemplate.ObjSubType.DisplayName;
end;

function TMapObject.GetID: AnsiString;
begin
  Result := &type;
end;

function TMapObject.GetSubId: AnsiString;
begin
  Result := subtype;
end;

procedure TMapObject.NotifyReferenced(AIdentifier: AnsiString);
begin
  GetMap.NotifyReferenced(AIdentifier);
end;

{ TMapObjects }

constructor TMapObjects.Create(AOwner: TVCMIMap);
begin
  inherited Create;
  FMap  := AOwner;
end;

function TMapObjects.GetOwner: TPersistent;
begin
  Result := FMap;
end;



{ TPlasedHero }

procedure TPlasedHero.SetName(AValue: TLocalizedString);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TPlasedHero.SetType(AValue: AnsiString);
begin
  if FType = AValue then Exit;
  FType := AValue;
end;

constructor TPlayerAttr.Create(AOwner: IReferenceNotify);
begin
  FOwner:= AOwner;
  FAllowedFactions := TLogicalIDCondition.Create;

  FPlasedHeroes := TPlasedHeroes.Create;
  FMainTown := TObjectLink.Create();
end;

destructor TPlayerAttr.Destroy;
begin
  FMainTown.Free;
  FPlasedHeroes.Free;
  FAllowedFactions.Free;
  inherited Destroy;
end;

procedure TPlayerAttr.SetAITactics(AValue: TAITactics);
begin
  if FAITactics = AValue then Exit;
  FAITactics := AValue;
end;

function TPlayerAttr.HasMainTown: boolean;
begin
  Result := not FMainTown.IsEmpty;
end;

procedure TPlayerAttr.SetCanComputerPlay(AValue: boolean);
begin
  if FCanComputerPlay = AValue then Exit;
  FCanComputerPlay := AValue;
end;

procedure TPlayerAttr.SetCanHumanPlay(AValue: boolean);
begin
  if FCanHumanPlay = AValue then Exit;
  FCanHumanPlay := AValue;
end;

procedure TPlayerAttr.SetGenerateHeroAtMainTown(AValue: boolean);
begin
  if FGenerateHeroAtMainTown = AValue then Exit;
  FGenerateHeroAtMainTown := AValue;
end;

procedure TPlayerAttr.SetRandomFaction(AValue: boolean);
begin
  if FRandomFaction = AValue then Exit;
  FRandomFaction := AValue;
end;

procedure TPlayerAttr.SetRandomHero(AValue: Boolean);
begin
  if FRandomHero = AValue then Exit;
  FRandomHero := AValue;
end;

procedure TPlayerAttr.SetTeam(AValue: Integer);
begin
  if FTeam = AValue then Exit;
  FTeam := AValue;
end;

procedure TPlayerAttr.SetMainHero(AValue: AnsiString);
begin
  if FMainHero = AValue then Exit;
  FMainHero := AValue;
end;

procedure TPlayerAttr.SetMainHeroName(AValue: TLocalizedString);
begin
  if FMainHeroName = AValue then Exit;
  FMainHeroName := AValue;
end;

procedure TPlayerAttr.SetMainHeroPortrait(AValue: AnsiString);
begin
  if FMainHeroPortrait = AValue then Exit;
  FMainHeroPortrait := AValue;
end;

{ TPlayerAttrs }

constructor TPlayerAttrs.Create(AOwner: IReferenceNotify);
var
  color: TPlayerColor;
begin
  Fowner := AOwner;
  for color in TPlayerColor do
    FColors[color] := TPlayerAttr.Create(Fowner);
end;

destructor TPlayerAttrs.Destroy;
var
  color: TPlayerColor;
begin
  for color in TPlayerColor do
    FColors[color].Free;
  inherited Destroy;
end;

function TPlayerAttrs.GetAttr(color: Integer): TPlayerAttr;
begin
  Result := FColors[TPlayerColor(color)];
end;

{ TMapTile }

constructor TMapTile.Create;
begin
  FTerType := TTerrainType.rock;
  FTerSubtype := 0;

  FRiverType:=0;
  FRiverDir:=0;
  FRoadType:=0;
  FRoadDir:=0;
  FFlags:=0;
  FOwner := TPlayer.NONE;
end;

procedure TMapTile.Render(mgr: TTerrainManager; X, Y: Integer);
begin
  mgr.Render(FTerType,FTerSubtype,X,Y,Flags);

  mgr.RenderRiver(TRiverType(FRiverType),FRiverDir,X,Y,Flags);
end;

procedure TMapTile.RenderRoad(mgr: TTerrainManager; X, Y: Integer);
begin
  mgr.RenderRoad(TRoadType(FRoadType),FRoadDir,X,Y,Flags);
end;

procedure TMapTile.SetFlags(AValue: UInt8);
begin
  if FFlags = AValue then Exit;
  FFlags := AValue;
end;

procedure TMapTile.SetRiverDir(AValue: UInt8);
begin
  if FRiverDir = AValue then Exit;
  FRiverDir := AValue;
end;

procedure TMapTile.SetRiverType(AValue: UInt8);
begin
  if FRiverType = AValue then Exit;
  FRiverType := AValue;
end;

procedure TMapTile.SetRoadDir(AValue: UInt8);
begin
  if FRoadDir = AValue then Exit;
  FRoadDir := AValue;
end;

procedure TMapTile.SetRoadType(AValue: UInt8);
begin
  if FRoadType = AValue then Exit;
  FRoadType := AValue;
end;

procedure TMapTile.SetTerSubtype(AValue: UInt8);
begin
  if FTerSubtype = AValue then Exit;
  FTerSubtype := AValue;
end;

procedure TMapTile.SetTerType(AValue: TTerrainType);
begin
  if FTerType = AValue then Exit;
  FTerType := AValue;
end;


{ TMapLevel }

destructor TMapLevel.Destroy;
begin
  inherited Destroy;
end;

procedure TMapLevel.SetRoad(x, y: integer; ARoadType: uint8; ARoadDir: UInt8;
  AMir: Uint8);
begin
  with Tile[x,y]^ do
  begin
    RoadType:= ARoadType;
    RoadDir:=ARoadDir;
    Flags := (Flags and $CF) or (AMir shl 4);
  end;
end;

procedure TMapLevel.SetRiver(x, y: integer; ARiverType: uint8;
  ARriverDir: UInt8; AMir: Uint8);
begin
  with Tile[x,y]^ do
  begin
    RiverType:= ARiverType;
    RiverDir:=ARriverDir;
    Flags := (Flags and $F3) or (AMir shl 2);
  end;
end;

procedure TMapLevel.BeforeSerialize;
begin
end;

function TMapLevel.GetTile(X, Y: Integer): PMapTile;
begin
  Result :=@FTiles[x,y];
end;

function TMapLevel.GetMap: TVCMIMap;
begin
  Result := (Collection as TMapLevels).FOwner;
end;

procedure TMapLevel.Resize;
var
  tt: TTerrainType;
  X: Integer;
  Y: Integer;
begin
  Assert(Height>=0, 'Invalid height');
  Assert(Width>=0, 'Invalid width');
  if (Height=0) or (Width=0) or (Index <0) then Exit;


  tt := Map.FTerrainManager.GetDefaultTerrain(Index);

  SetLength(FTiles,FWidth);
  for X := 0 to FWidth - 1 do
  begin
    SetLength(FTiles[X],FHeight);
    for Y := 0 to FHeight - 1 do
    begin
      FTiles[X][Y].Create();
      FTiles[X][Y].TerType :=  tt;
      FTiles[X][Y].TerSubtype := Map.FTerrainManager.GetRandomNormalSubtype(tt);
    end;
  end;

end;

procedure TMapLevel.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  Resize;
end;

constructor TMapLevel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TMapLevel.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  Resize();
end;

procedure TMapLevel.SetObjects(AValue: TFilename);
begin
  if FObjects=AValue then Exit;
  FObjects:=AValue;
end;

procedure TMapLevel.SetTerrain(AValue: TFilename);
begin
  if FTerrain=AValue then Exit;
  FTerrain:=AValue;
end;

procedure TMapLevel.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  Resize();
end;

{ TVCMIMap }

procedure TVCMIMap.AttachTo(AObserved: IFPObserved);
begin
  AObserved.FPOAttachObserver(Self);
end;

procedure TVCMIMap.Changed;
begin
  FIsDirty := True;
end;

constructor TVCMIMap.Create(env: TMapEnvironment; Params: TMapCreateParams);
var
  lvl: TMapLevel;
begin
  CreateEmpty(env);

  lvl := Levels.Add;
  lvl.Width:=Params.Width;
  lvl.Height:=Params.Height;
  lvl.DisplayName := 'surface';

  if Params.Levels>1 then
  begin
    lvl := Levels.Add;
    lvl.Width:=Params.Width;
    lvl.Height:=Params.Height;
    lvl.DisplayName := 'underground';
  end;

  CurrentLevelIndex := 0;

  FListsManager.SpellInfos.FillWithAllIds(FAllowedSpells);
  FListsManager.SkillInfos.FillWithAllIds(FAllowedAbilities);
  FListsManager.ArtifactInfos.FillWithAllIds(FAllowedArtifacts);
  FListsManager.HeroInfos.FillWithNotSpecial(FAllowedHeroes);
end;

constructor TVCMIMap.CreateDefault(env: TMapEnvironment);
var
  params: TMapCreateParams;
begin
  params.Height := MAP_DEFAULT_SIZE;
  params.Width := MAP_DEFAULT_SIZE;
  params.Levels := MAP_DEFAULT_LEVELS;

  Create(env,params);

end;


constructor TVCMIMap.CreateEmpty(env: TMapEnvironment);
begin
  FTerrainManager := env.tm;
  FListsManager := env.lm;

  FLevels := TMapLevels.Create(Self);

  CurrentLevelIndex := -1;

  Name := rsDefaultMapName;

  FDifficulty := TDifficulty.NORMAL;

  FIsDirty := False;

  FAllowedAbilities := TLogicalIDCondition.Create;
  AttachTo(FAllowedAbilities);

  FAllowedSpells := TLogicalIDCondition.Create;
  AttachTo(FAllowedSpells);

  FAllowedArtifacts := TLogicalIDCondition.Create;
  AttachTo(FAllowedArtifacts);

  FAllowedHeroes := TLogicalIDCondition.Create;
  AttachTo(FAllowedHeroes);

  FPlayers := TPlayerAttrs.Create(Self);
  FObjects := TMapObjects.Create(Self);
  AttachTo(FObjects);

  FRumors := TRumors.Create;
  AttachTo(FRumors);

  FLevelLimit:=MAX_HERO_LEVEL;
  FPredefinedHeroes := THeroDefinitions.Create(Self);
  AttachTo(FPredefinedHeroes);

  FTriggeredEvents := TTriggeredEvents.Create;
  AttachTo(FTriggeredEvents);

  FMods := TLogicalIDCondition.Create;
end;

destructor TVCMIMap.Destroy;
begin
  FMods.Free;
  FTriggeredEvents.Free;

  FPredefinedHeroes.Free;

  FAllowedArtifacts.Free;
  FAllowedAbilities.Free;
  FAllowedSpells.Free;
  FAllowedHeroes.Free;

  FRumors.Free;
  FObjects.Free;
  FPlayers.Free;
  FLevels.Free;
  inherited Destroy;
end;

procedure TVCMIMap.FillLevel(TT: TTerrainType);
var
  x: Integer;
  Y: Integer;
begin
  for x := 0 to CurrentLevel.Width - 1 do
  begin
    for Y := 0 to CurrentLevel.Height - 1 do
    begin
      SetTerrain(x,y,tt);
    end;

  end;

end;

procedure TVCMIMap.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange,ooAddItem,ooDeleteItem: FIsDirty := true ;
  end;
end;

function TVCMIMap.GetCurrentLevelIndex: Integer;
begin
  Result := FCurrentLevel;
end;

function TVCMIMap.GetTile(Level, X, Y: Integer): PMapTile;
begin
  Result := FLevels[Level].Tile[x,y]//todo: check
end;

function TVCMIMap.IsOnMap(Level, X, Y: Integer): boolean;
begin
  Result := (Level >=0)
  and (Level < FLevels.Count)
  and (x>=0) and (x<Levels[Level].Width)
  and (y>=0) and (y<Levels[Level].Height);
end;

procedure TVCMIMap.RenderTerrain(Left, Right, Top, Bottom: Integer);
var
  i: Integer;
  j: Integer;
begin

  Right := Min(Right, CurrentLevel.Width - 1);
  Bottom := Min(Bottom, CurrentLevel.Height - 1);

  for i := Left to Right do
  begin
    for j := Top to Bottom do
    begin
      GetTile(FCurrentLevel,i,j)^.Render(FTerrainManager,i,j);
    end;
  end;

  Top := Max(0,Top-1);

  for i := Left to Right do
  begin
    for j := Top to Bottom do
    begin
      GetTile(FCurrentLevel,i,j)^.RenderRoad(FTerrainManager,i,j);
    end;
  end;



end;

procedure TVCMIMap.RenderObjects(Left, Right, Top, Bottom: Integer);
var
  i: Integer;
  o: TMapObject;

  FQueue : TMapObjectQueue;
begin
  FQueue := TMapObjectQueue.Create;

  for i := 0 to FObjects.Count - 1 do
  begin
    o := FObjects.Items[i];
    if o.L <> CurrentLevelIndex then
      Continue;

    if (o.X < Left)
      or (o.Y < Top)
      or (o.X - 8 > Right)
      or (o.y - 6 > Bottom)
      then Continue; //todo: use visisblity mask

    FQueue.Push(o);

  end;

  while not FQueue.IsEmpty do
  begin
    o := FQueue.Top;
    o.RenderAnim;
    FQueue.Pop;
  end;

  FQueue.Free;
end;

procedure TVCMIMap.SaveToStream(ADest: TStream; AWriter: IMapWriter);
begin
  AWriter.Write(ADest,Self);
  FIsDirty := False;
end;

procedure TVCMIMap.SelectObjectsOnTile(Level, X, Y: Integer;
  dest: TMapObjectQueue);
var
  o: TMapObject;
  i: Integer;
begin
  //TODO: use mask

  for i := 0 to FObjects.Count - 1 do
  begin
    o := FObjects[i];

    if o.CoversTile(Level,x,y) then
    begin
      dest.Push(o);
    end;
  end;
end;

procedure TVCMIMap.BeforeSerialize;
begin
   FLevels.BeforeSerialize;
end;

procedure TVCMIMap.NotifyReferenced(AIdentifier: AnsiString);
var
  colon_pos: SizeInt;
  mod_id: AnsiString;
begin
  if(AIdentifier = '') then
    exit;

  colon_pos := pos(':',AIdentifier);

  if colon_pos <= 0 then
    exit;//object is from core

  mod_id := copy(AIdentifier, 1, colon_pos-1);

  DebugLn(['Referenced ', copy(AIdentifier, colon_pos +1, MaxInt), ' from ', mod_id]);
end;

function TVCMIMap.CurrentLevel: TMapLevel;
begin
  Result := FLevels[CurrentLevelIndex];
end;

procedure TVCMIMap.SetCurrentLevelIndex(AValue: Integer);
begin
  if FCurrentLevel = AValue then Exit;
  FCurrentLevel := AValue;
end;

procedure TVCMIMap.SetDescription(AValue: TLocalizedString);
begin
  if FDescription = AValue then Exit;
  FDescription := AValue;
  Changed;
end;

procedure TVCMIMap.SetDifficulty(AValue: TDifficulty);
begin
  if FDifficulty = AValue then Exit;
  FDifficulty := AValue;
  Changed;
end;

procedure TVCMIMap.SetLevelLimit(AValue: Integer);
begin
  if FLevelLimit = AValue then Exit;
  FLevelLimit := AValue;
  if FLevelLimit = 0 then
  begin
    FLevelLimit := MAX_HERO_LEVEL;
  end;
  Changed;
end;

procedure TVCMIMap.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
  Changed;
end;

procedure TVCMIMap.SetTerrain(Level, X, Y: Integer; TT: TTerrainType;
  TS: UInt8; mir: UInt8);
var
  t: PMapTile;
begin
  t := GetTile(Level,X,Y);
  t^.TerType := TT;
  t^.TerSubtype := TS;
  t^.Flags := (t^.Flags and $FC) or (mir and 3);

  Changed;
end;

procedure TVCMIMap.SetTerrain(X, Y: Integer; TT: TTerrainType);
begin
  SetTerrain(CurrentLevelIndex,X,Y,TT,FTerrainManager.GetRandomNormalSubtype(TT));
end;

end.

