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
  logical_event_condition, logical_expression, vcmi_json, vcmi_fpjsonrtti;

const
  MAP_DEFAULT_SIZE = 36;
  MAP_DEFAULT_LEVELS = 1;

  MAP_PLAYER_COUNT = 8;

type
  TVCMIMap = class;
  TMapObject = class;

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

  { TPlayerHero }

  TPlayerHero = class (TNamedCollectionItem)
  private
    FMapObject: TMapObject;
    function GetName: TLocalizedString;
    function GetType: AnsiString;
    procedure SetMapObject(AValue: TMapObject);
  protected
    function GetDisplayName: string; override;
  public
    property MapObject: TMapObject read FMapObject write SetMapObject;
  published
    property &type:AnsiString read GetType;
    property Name: TLocalizedString read GetName;
  end;

  { TPlayerHeroes }

  TPlayerHeroCollection = specialize TGNamedCollection<TPlayerHero>;

  TPlayerHeroes = class (TPlayerHeroCollection)
  end;

  { TPlayerTown }

  TPlayerTown = class(TNamedCollectionItem)
  private
    FMapObject: TMapObject;
    function GetType: AnsiString;
    procedure SetMapObject(AValue: TMapObject);
  public
    property MapObject: TMapObject read FMapObject write SetMapObject;
  published
    property &type:AnsiString read GetType;
  end;


  TPlayerTownCollection = specialize TGNamedCollection<TPlayerTown>;

  { TPlayerTowns }

  TPlayerTowns = class (TPlayerTownCollection)

  end;

  { TPlayerInfo }

  TPlayerInfo = class(TObject, ISerializeSpecial, IFPObserver)
  private
    FCanPlay: TPlayableBy;
    FColor: TPlayerColor;
    FOwner: IReferenceNotify;
    FAITactics: TAITactics;
    FAllowedFactions: TLogicalIDCondition;
    FHeroes: TPlayerHeroes;
    FGenerateHeroAtMainTown: boolean;
    FRandomFaction: boolean;

    FMainTown: string;
    FRandomHero: Boolean;
    FMainHero: String;
    FTowns: TPlayerTowns;

    function HasMainTown: boolean;
    procedure SetAITactics(AValue: TAITactics);
    procedure SetCanPlay(AValue: TPlayableBy);
    procedure SetGenerateHeroAtMainTown(AValue: boolean);
    procedure SetRandomFaction(AValue: boolean);
    procedure SetRandomHero(AValue: Boolean);

  public
    constructor Create(AColor: TPlayerColor; AOwner: IReferenceNotify);
    destructor Destroy; override;

    property Color: TPlayerColor read FColor;

  public // ISerializeSpecial
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;

  public//IFPObserver
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

  published
    property AllowedFactions: TLogicalIDCondition read FAllowedFactions;

    property CanPlay: TPlayableBy read FCanPlay write SetCanPlay default TPlayableBy.None;

    property MainTown: String read FMainTown write FMainTown stored HasMainTown;

    property GenerateHeroAtMainTown: boolean read FGenerateHeroAtMainTown write SetGenerateHeroAtMainTown stored HasMainTown;

    property MainHero: String read FMainHero write FMainHero;
  public
    property AITactics: TAITactics read FAITactics write SetAITactics; //not used in vcmi (yet)
  public //special streaming
    property Towns: TPlayerTowns read FTowns;
    property Heroes: TPlayerHeroes read FHeroes;
  end;

  { TPlayerInfos }

  TPlayerInfos = class
  private
    Fowner:IReferenceNotify;
    FColors : array[TPlayerColor] of TPlayerInfo;
    function IsPlayable(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: IReferenceNotify);
    destructor Destroy; override;

    function GetPlayerInfo(color: Integer): TPlayerInfo;

  published
    property Red:TPlayerInfo index Integer(TPlayerColor.Red) read GetPlayerInfo stored IsPlayable;
    property Blue:TPlayerInfo index Integer(TPlayerColor.Blue) read GetPlayerInfo stored IsPlayable;
    property Tan:TPlayerInfo index Integer(TPlayerColor.Tan) read GetPlayerInfo stored IsPlayable;
    property Green:TPlayerInfo index Integer(TPlayerColor.Green) read GetPlayerInfo stored IsPlayable;

    property Orange:TPlayerInfo index Integer(TPlayerColor.Orange) read GetPlayerInfo stored IsPlayable;
    property Purple:TPlayerInfo index Integer(TPlayerColor.Purple) read GetPlayerInfo stored IsPlayable;
    property Teal:TPlayerInfo index Integer(TPlayerColor.Teal) read GetPlayerInfo stored IsPlayable;
    property Pink:TPlayerInfo index Integer(TPlayerColor.Pink) read GetPlayerInfo stored IsPlayable;
  end;

  { TTeam }

  TTeam = class(TCollectionItem, IEmbeddedValue)
  private
    FMembers: TPlayers;
  public
    procedure Include(APlayer: TPlayerColor);
    procedure Exclude(APlayer: TPlayerColor);

    function FormatDescription(PoV: TPlayer): TLocalizedString;
  published
    property Members: TPlayers read FMembers;
  end;

  TTeamCollection = specialize TGArrayCollection<TTeam>;

  { TTeamSettings }

  TTeamSettings = class (TTeamCollection)
  public
    constructor Create;
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

{$pop}

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

  { TMapObject }

  TMapObject = class (TNamedCollectionItem, IMapObject)
  strict private
    FLastFrame: Integer;
    FLastTick: DWord;
    FOptions: TObjectOptions;
    FPlayer: TPlayer;
    FL: integer;
    FSubtype: AnsiString;
    FTemplate: TMapObjectTemplate;
    FType: AnsiString;
    FX: integer;
    FY: integer;
    function GetIdx: integer;

    procedure Render(Frame:integer; Ax,Ay: integer);
    procedure SetL(AValue: integer);
    procedure SetSubtype(AValue: AnsiString);
    procedure SetType(AValue: AnsiString);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);

    procedure UpdateIdentifier;
  protected
    procedure TypeChanged;
    procedure SetCollection(Value: TCollection); override;
    function GetDisplayName: string; override;
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

    function FormatDisplayName(ACustomName: TLocalizedString): TLocalizedString;
  public //IMapObject
    function GetID: AnsiString;
    function GetSubId: AnsiString;
    function GetPlayer: TPlayer;
    procedure SetPlayer(AValue: TPlayer);

    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
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

  TMapObjectCollection = specialize TGNamedCollection<TMapObject>;

  TMapObjects = class (TMapObjectCollection)
  strict private
    FMap: TVCMIMap;
    FNextIdentifier: Integer;

    procedure ProcessItemIdentifier(AIdentifier: String);
  protected
    function GetOwner: TPersistent; override;
    procedure ItemAdded(Item: TCollectionItem); override;
    procedure ItemRemoved(Item: TCollectionItem); override;
    procedure ItemIdentifierChanged(Item: TCollectionItem; AOldName: String;
      ANewName: String); override;
  public
    constructor Create(AOwner: TVCMIMap);

    property Map: TVCMIMap read FMap;

    function GenerateIdentifier: Integer;
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

  published
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Index;
  end;

  { TMapLevels }

  TMapLevelsCollection = specialize TGNamedCollection<TMapLevel>;

  TMapLevels = class(TMapLevelsCollection)
  private
    FOwner: TVCMIMap;
  public
    constructor Create(AOwner: TVCMIMap);
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
    FPlayers: TPlayerInfos;
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

  private
    FTeams: TTeamSettings;
    type
      { TModRefCountInfo }

      TModRefCountInfo = class(TNamedCollectionItem)
      private
        FForced: boolean;
        FRefCount: Integer;
        procedure SetForced(AValue: boolean);
        procedure SetRefCount(AValue: Integer);
      public
        property RefCount: Integer read FRefCount write SetRefCount;
        property Forced: boolean read FForced write SetForced;
      end;

      TModRefCountInfos = specialize TGNamedCollection <TModRefCountInfo>;

      { TModUsage }

      TModUsage = class (TModRefCountInfos)
      strict private
        FModsCondition: TLogicalIDCondition;
        function GetMod(AIdentifier: AnsiString): TModRefCountInfo;
        procedure UpdateMod(AIdentifier: AnsiString; AInfo: TModRefCountInfo);
      public
        constructor Create(AModsCondition: TLogicalIDCondition);
        procedure ForceMod(AIdentifier: AnsiString; AForce: Boolean);
        procedure ReferenceMod(AIdentifier: AnsiString);
        procedure DeReferenceMod(AIdentifier: AnsiString);
      end;

     var
       FModUsage:TModUsage;
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

    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
    procedure NotifyOwnerChanged(AObject: TMapObject; AOldOwner, ANewOwner: TPlayer);

    //actual hero Name
    function GetHeroName(AObject: TMapObject): TLocalizedString;

  published
    property Name:TLocalizedString read FName write SetName; //+
    property Description:TLocalizedString read FDescription write SetDescription; //+

    property Difficulty: TDifficulty read FDifficulty write SetDifficulty nodefault; //?
    property LevelLimit: Integer read FLevelLimit write SetLevelLimit default 199;//+

    property Players: TPlayerInfos read FPlayers;
    property Teams:TTeamSettings read FTeams;

    property Rumors: TRumors read FRumors;

    property AllowedSpells: TLogicalIDCondition read FAllowedSpells;
    property AllowedAbilities: TLogicalIDCondition read FAllowedAbilities;
    property AllowedArtifacts: TLogicalIDCondition read FAllowedArtifacts;
    property AllowedHeroes: TLogicalIDCondition read FAllowedHeroes;

    property MapLevels: TMapLevels read FLevels;

    property PredefinedHeroes: THeroDefinitions read FPredefinedHeroes;

    property TriggeredEvents: TTriggeredEvents read FTriggeredEvents;

    property Mods: TLogicalIDCondition read FMods;
  public //manual streamimg
    property Objects: TMapObjects read FObjects;
  public
    function CurrentLevel: TMapLevel;
    property ModUsage:TModUsage read FModUsage;
  end;

{$pop}

implementation

uses FileUtil, LazLoggerBase, editor_str_consts, root_manager, editor_utils,
  strutils, typinfo;

{ TTeam }

procedure TTeam.Include(APlayer: TPlayerColor);
var
  item: TCollectionItem;
begin

  for item in Collection do
  begin
    (item as TTeam).Exclude(APlayer);
  end;

  FMembers += [APlayer];
end;

procedure TTeam.Exclude(APlayer: TPlayerColor);
begin
  FMembers -= [APlayer];
end;

function TTeam.FormatDescription(PoV: TPlayer): TLocalizedString;
var
  iter: TPlayer;
  first: Boolean;

  tmp : TPlayers;
begin
  Result := '';

  tmp := FMembers;

  tmp -= [PoV];

  first := True;

  for iter in tmp do
  begin

    if first then
    begin
      first:=false;

      Result := RootManager.ListsManager.PlayerName[iter];
    end
    else
    begin
      Result := Result + ', ' + RootManager.ListsManager.PlayerName[iter];
    end;
  end;
end;

{ TTeamSettings }

constructor TTeamSettings.Create;
begin
  inherited Create;
end;

{ TPlayerTown }

function TPlayerTown.GetType: AnsiString;
begin
  if Assigned(FMapObject) and (FMapObject.&Type = TYPE_TOWN) then
  begin
    Result := FMapObject.Subtype;
  end
  else
    Result := '';
end;

procedure TPlayerTown.SetMapObject(AValue: TMapObject);
begin
  if FMapObject=AValue then Exit;
  FMapObject:=AValue;
  Identifier:=AValue.Identifier;
end;

{ TVCMIMap.TModRefCountInfo }

procedure TVCMIMap.TModRefCountInfo.SetRefCount(AValue: Integer);
begin
  if FRefCount=AValue then Exit;
  FRefCount:=AValue;
end;

procedure TVCMIMap.TModRefCountInfo.SetForced(AValue: boolean);
begin
  if FForced=AValue then Exit;
  FForced:=AValue;
end;

{ TVCMIMap.TModUsage }

function TVCMIMap.TModUsage.GetMod(AIdentifier: AnsiString): TModRefCountInfo;
begin
  Result := FindItem(AIdentifier);
  if not Assigned(Result) then
  begin
    Result := TModRefCountInfo(ItemClass.Create(nil));
    Result.Identifier := AIdentifier;
    Result.Collection := self;
  end;
end;

procedure TVCMIMap.TModUsage.UpdateMod(AIdentifier: AnsiString;
  AInfo: TModRefCountInfo);
var
  idx: Integer;
begin
  if (AInfo.Forced) or (AInfo.RefCount > 0 ) then
  begin
    FModsCondition.AllOf.Add(AIdentifier);
  end
  else
  begin
    idx := FModsCondition.AllOf.IndexOf(AIdentifier);
    if idx >=0 then
       FModsCondition.AllOf.Delete(idx);
  end;
end;

constructor TVCMIMap.TModUsage.Create(AModsCondition: TLogicalIDCondition);
begin
  inherited Create;
  FModsCondition := AModsCondition;
end;

procedure TVCMIMap.TModUsage.ForceMod(AIdentifier: AnsiString; AForce: Boolean);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := GetMod(AIdentifier);

  if mod_info.Forced <> AForce then
  begin
    mod_info.Forced:=AForce;
    UpdateMod(AIdentifier, mod_info);
  end;
end;

procedure TVCMIMap.TModUsage.ReferenceMod(AIdentifier: AnsiString);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := GetMod(AIdentifier);
  mod_info.RefCount:=mod_info.RefCount+1;

  if (mod_info.RefCount = 1) and (not mod_info.Forced) then
  begin
    UpdateMod(AIdentifier, mod_info);
  end;
end;

procedure TVCMIMap.TModUsage.DeReferenceMod(AIdentifier: AnsiString);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := GetMod(AIdentifier);
  mod_info.RefCount:=mod_info.RefCount-1;

  if (mod_info.RefCount = 0) and (not mod_info.Forced) then
  begin
    UpdateMod(AIdentifier, mod_info);
  end;
end;

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
  FLastFrame := 0;
  FTemplate := TMapObjectTemplate.Create;
  FOptions := TObjectOptions.Create(Self);
  FPlayer:=TPlayer.none;
  inherited Create(ACollection);
end;

destructor TMapObject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTemplate);
  FreeAndNil(FOptions);
end;

function TMapObject.GetPlayer: TPlayer;
begin
  Result := FPlayer;
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
  NotifyReferenced(FSubtype,AValue);
  FSubtype:=AValue;
  TypeChanged;
end;

procedure TMapObject.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;
  NotifyReferenced(FType,AValue);
  FType:=AValue;
  TypeChanged;
  UpdateIdentifier;
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

procedure TMapObject.UpdateIdentifier;
begin
  if(FType <> '') and (Identifier = '') and (Assigned(Collection)) then
  begin
    Identifier := StripScope(FType)+'_'+IntToStr((Collection as TMapObjects).GenerateIdentifier());
  end;
end;

procedure TMapObject.TypeChanged;
begin
  FreeAndNil(FOptions);

  if (FType <>'') and (FSubtype<>'') then
  begin
    FOptions := CreateByID(FType, FSubtype,Self);
  end;
end;

procedure TMapObject.SetCollection(Value: TCollection);
begin
  //notify old collection
  if Assigned(Collection) then
  begin
    GetMap.NotifyReferenced(FType, '');
    GetMap.NotifyReferenced(FSubtype, '');
  end;

  inherited SetCollection(Value);

  //notify new collection
  if Assigned(Collection) then
  begin
    GetMap.NotifyReferenced('', FType);
    GetMap.NotifyReferenced('', FSubtype);
    UpdateIdentifier;
  end;
end;

function TMapObject.GetDisplayName: string;
begin
  Result:=FormatDisplayName('');
end;

function TMapObject.GetMap: TVCMIMap;
begin
  Result := (Collection as TMapObjects).Map;
end;

procedure TMapObject.AssignTemplate(ATemplate: TObjTemplate);
begin
  Template.Assign(ATemplate);
  &Type:=ATemplate.ObjType.Identifier;
  Subtype := ATemplate.ObjSubType.Identifier;
end;

function TMapObject.FormatDisplayName(ACustomName: TLocalizedString
  ): TLocalizedString;
begin
  if ACustomName = '' then
    Result:=Format('%s::%s @ %d %d %d',[&type, subtype,X,Y,L])
  else
    Result:=Format('%s @ %d %d %d',[ACustomName,X,Y,L]);
end;

procedure TMapObject.SetPlayer(AValue: TPlayer);
begin
  if FPlayer = AValue then
    Exit;
  if Assigned(Collection) then
  begin
    GetMap.NotifyOwnerChanged(self, FPlayer, AValue);
  end;
  FPlayer:=AValue;
end;

function TMapObject.GetID: AnsiString;
begin
  Result := &type;
end;

function TMapObject.GetSubId: AnsiString;
begin
  Result := subtype;
end;

procedure TMapObject.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString
  );
begin
  if Assigned(Collection) then
  begin
    GetMap.NotifyReferenced(AOldIdentifier, ANewIdentifier);
  end;
end;

{ TMapObjects }

constructor TMapObjects.Create(AOwner: TVCMIMap);
begin
  inherited Create;
  FMap  := AOwner;
end;

function TMapObjects.GenerateIdentifier: Integer;
begin
  Result := FNextIdentifier;
  inc(FNextIdentifier);
end;

procedure TMapObjects.ProcessItemIdentifier(AIdentifier: String);
var
  underscore: Integer;
  numeric_id: Integer;
  raw_numeric_id: String;
begin
  underscore := RPos('_', AIdentifier);

  if underscore > 0 then
  begin
    raw_numeric_id := Copy(AIdentifier, underscore+1, MaxInt);

    if TryStrToInt(raw_numeric_id, numeric_id) then
    begin
      FNextIdentifier := Max(FNextIdentifier, numeric_id+1);
    end;
  end;
end;

function TMapObjects.GetOwner: TPersistent;
begin
  Result := FMap;
end;

procedure TMapObjects.ItemAdded(Item: TCollectionItem);
begin
  inherited ItemAdded(Item);

  ProcessItemIdentifier(TNamedCollectionItem(Item).Identifier);
  FMap.NotifyOwnerChanged(TMapObject(Item), TPlayer.none, TMapObject(Item).GetPlayer());
end;

procedure TMapObjects.ItemRemoved(Item: TCollectionItem);
begin
  inherited ItemRemoved(Item);
  FMap.NotifyOwnerChanged(TMapObject(Item),TMapObject(Item).GetPlayer(), TPlayer.none);
end;

procedure TMapObjects.ItemIdentifierChanged(Item: TCollectionItem;
  AOldName: String; ANewName: String);
begin
  inherited ItemIdentifierChanged(Item, AOldName, ANewName);

  ProcessItemIdentifier(ANewName);
end;

{ TPlayerHero }

function TPlayerHero.GetName: TLocalizedString;
begin
  if Assigned(FMapObject) then
    Result := THeroOptions(FMapObject.Options).Name
  else
    Result := '';
end;

function TPlayerHero.GetType: AnsiString;
begin
  if Assigned(FMapObject) and (FMapObject.&Type = TYPE_HERO) then
  begin
    Result := THeroOptions(FMapObject.Options).&type;
  end
  else
    Result := '';
end;

procedure TPlayerHero.SetMapObject(AValue: TMapObject);
begin
  if FMapObject=AValue then Exit;
  FMapObject:=AValue;

  Identifier:=AValue.Identifier;
end;

function TPlayerHero.GetDisplayName: string;
begin
  if not Assigned(FMapObject) then
  begin
    Exit('');
  end;
  Result := Format('%s @ %d %d %d',[(MapObject.Options as THeroOptions).Name, MapObject.X, MapObject.Y, MapObject.L]);
end;

constructor TPlayerInfo.Create(AColor: TPlayerColor; AOwner: IReferenceNotify);
begin
  FColor := AColor;
  FOwner:= AOwner;
  FAllowedFactions := TLogicalIDCondition.Create(AOwner);

  FHeroes := TPlayerHeroes.Create;
  FHeroes.FPOAttachObserver(Self);
  FTowns := TPlayerTowns.Create;
  FTowns.FPOAttachObserver(Self);
end;

destructor TPlayerInfo.Destroy;
begin
  FTowns.Free;
  FHeroes.Free;
  FAllowedFactions.Free;
  inherited Destroy;
end;

procedure TPlayerInfo.Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
begin
  AHandler.JSONToObject(ASrc as TJSONObject, Self);
  // heroes and towns are ignored, they will be refilled later
end;

function TPlayerInfo.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
begin
  Result := AHandler.ObjectToJSON(Self);

  TJSONObject(Result).Add('heroes', AHandler.StreamCollection(Heroes));
  TJSONObject(Result).Add('towns', AHandler.StreamCollection(Towns));
end;

procedure TPlayerInfo.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin

end;

procedure TPlayerInfo.SetAITactics(AValue: TAITactics);
begin
  if FAITactics = AValue then Exit;
  FAITactics := AValue;
end;

function TPlayerInfo.HasMainTown: boolean;
begin
  Result := FMainTown <> '';
end;

procedure TPlayerInfo.SetCanPlay(AValue: TPlayableBy);
begin
  if FCanPlay=AValue then Exit;
  FCanPlay:=AValue;
end;

procedure TPlayerInfo.SetGenerateHeroAtMainTown(AValue: boolean);
begin
  if FGenerateHeroAtMainTown = AValue then Exit;
  FGenerateHeroAtMainTown := AValue;
end;

procedure TPlayerInfo.SetRandomFaction(AValue: boolean);
begin
  if FRandomFaction = AValue then Exit;
  FRandomFaction := AValue;
end;

procedure TPlayerInfo.SetRandomHero(AValue: Boolean);
begin
  if FRandomHero = AValue then Exit;
  FRandomHero := AValue;
end;

{ TPlayerInfos }

function TPlayerInfos.IsPlayable(AIndex: Integer): Boolean;
var
  info: TPlayerInfo;
begin
  info := FColors[TPlayerColor(AIndex)];

  if (info.Heroes.Count = 0) and (info.Towns.Count = 0) then
  begin
    Result := false;
  end
  else begin
    Result := FColors[TPlayerColor(AIndex)].CanPlay <> TPlayableBy.None;
  end;
end;

constructor TPlayerInfos.Create(AOwner: IReferenceNotify);
var
  color: TPlayerColor;
begin
  Fowner := AOwner;
  for color in TPlayerColor do
    FColors[color] := TPlayerInfo.Create(color, Fowner);
end;

destructor TPlayerInfos.Destroy;
var
  color: TPlayerColor;
begin
  for color in TPlayerColor do
    FColors[color].Free;
  inherited Destroy;
end;

function TPlayerInfos.GetPlayerInfo(color: Integer): TPlayerInfo;
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

  lvl := MapLevels.Add;
  lvl.Width:=Params.Width;
  lvl.Height:=Params.Height;
  lvl.Identifier := 'surface';

  if Params.Levels>1 then
  begin
    lvl := MapLevels.Add;
    lvl.Width:=Params.Width;
    lvl.Height:=Params.Height;
    lvl.Identifier := 'underground';
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

  FAllowedAbilities := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedAbilities);

  FAllowedSpells := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedSpells);

  FAllowedArtifacts := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedArtifacts);

  FAllowedHeroes := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedHeroes);

  FPlayers := TPlayerInfos.Create(Self);
  FTeams := TTeamSettings.Create;
  FObjects := TMapObjects.Create(Self);
  AttachTo(FObjects);

  FRumors := TRumors.Create;
  AttachTo(FRumors);

  FLevelLimit:=MAX_HERO_LEVEL;
  FPredefinedHeroes := THeroDefinitions.Create(Self);
  AttachTo(FPredefinedHeroes);

  FTriggeredEvents := TTriggeredEvents.Create;
  AttachTo(FTriggeredEvents);

  FMods := TLogicalIDCondition.Create(nil);//pass nil owner to avoid circular reference notification

  FModUsage := TModUsage.Create(FMods);
end;

destructor TVCMIMap.Destroy;
begin
  FTriggeredEvents.Free;

  FPredefinedHeroes.Free;

  FAllowedArtifacts.Free;
  FAllowedAbilities.Free;
  FAllowedSpells.Free;
  FAllowedHeroes.Free;

  FRumors.Free;
  FObjects.Free;
  FTeams.Free;
  FPlayers.Free;
  FLevels.Free;

  FModUsage.Free;
  FMods.Free;

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
  and (x>=0) and (x<MapLevels[Level].Width)
  and (y>=0) and (y<MapLevels[Level].Height);
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

procedure TVCMIMap.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);

  function ExtractModID(AIdentifier:AnsiString): AnsiString;
  var
    colon_pos: SizeInt;
  begin
    if(AIdentifier = '') then
      exit('');

    colon_pos := pos(':',AIdentifier);

    if colon_pos <= 0 then
      exit('');//object is from core

    Result := copy(AIdentifier, 1, colon_pos-1);
  end;

var
  mod_id: AnsiString;
begin

  mod_id:=ExtractModID(AOldIdentifier);

  if mod_id <> '' then
  begin
    FModUsage.DeReferenceMod(mod_id);
  end;

  mod_id:=ExtractModID(ANewIdentifier);

  if mod_id <> '' then
  begin
    FModUsage.ReferenceMod(mod_id);
  end;
end;

procedure TVCMIMap.NotifyOwnerChanged(AObject: TMapObject; AOldOwner,
  ANewOwner: TPlayer);

  procedure Remove(ACollection: THashedCollection; AIdentifier: string);
  var
    idx: Integer;
  begin
    idx := ACollection.IndexOfName(AIdentifier);
    if idx>=0 then
      ACollection.Delete(idx);
  end;

var
  opt: TPlayerInfo;
  player_hero: TPlayerHero;
  player_town: TPlayerTown;
begin
  if AOldOwner <> TPlayer.none then
  begin
    if AOldOwner in [TPlayer.red..TPlayer.pink] then
    begin
      opt := FPlayers.GetPlayerInfo(Integer(AOldOwner));

      case AObject.&Type of
        TYPE_HERO, TYPE_RANDOMHERO:
            Remove(opt.Heroes, AObject.Identifier);
        TYPE_TOWN, TYPE_RANDOMTOWN:
            Remove(opt.Towns, AObject.Identifier);
      end;
    end
    else begin
      DebugLn(['invalid player color', Integer(AOldOwner)]);
    end;
  end;


  if ANewOwner <> TPlayer.none then
  begin
    if ANewOwner in [TPlayer.red..TPlayer.pink] then
    begin
      opt := FPlayers.GetPlayerInfo(Integer(ANewOwner));

      case AObject.&Type of
        TYPE_HERO, TYPE_RANDOMHERO:
        begin
          player_hero := opt.Heroes.Add;
          player_hero.MapObject := AObject;
        end;
        TYPE_TOWN, TYPE_RANDOMTOWN:
        begin
          player_town := opt.Towns.Add;
          player_town.MapObject := AObject;
        end;
      end;
    end
    else begin
      DebugLn(['invalid player color', Integer(ANewOwner)]);
    end;
  end;


end;

function TVCMIMap.GetHeroName(AObject: TMapObject): TLocalizedString;
var
  opt: THeroOptions;
  definition: THeroDefinition;
  info: THeroInfo;
begin
  Result := '[Unknown]';

  if AObject.&type = TYPE_RANDOMHERO then
  begin
    Result := 'Random hero';
  end;

  opt := AObject.Options as THeroOptions;

  if opt.Name <> '' then
  begin
    Result := opt.Name;
    exit;
  end;

  definition := PredefinedHeroes.FindItem(opt.&type);

  if Assigned(definition) and (definition.Name <> '') then
  begin
    exit(definition.Name);
  end;

  info := ListsManager.Heroes[opt.&type];
  if Assigned(info) then
    Result := info.Name;
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

