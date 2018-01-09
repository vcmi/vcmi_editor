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
unit Map;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, fgl, LCLIntf, fpjson, gvector, gpriorityqueue, gset,
  editor_types, editor_consts, terrain, editor_classes, editor_graphics,
  map_objects, object_options, lists_manager, logical_id_condition,
  logical_event_condition, vcmi_json, locale_manager,
  editor_gl, map_rect, search_index, position, vcmi_fpjsonrtti;

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

  { TModRefCountInfo }

  TModRefCountInfo = class(TNamedCollectionItem)
  private
    FForced: boolean;
    FRefCount: Integer;
    procedure SetForced(AValue: boolean);
    procedure SetRefCount(AValue: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property RefCount: Integer read FRefCount write SetRefCount;
    property Forced: boolean read FForced write SetForced;
  end;


  { TModUsage }

  TModUsage = class (specialize TGNamedCollection <TModRefCountInfo>)
  strict private
    FModsCondition: TLogicalIDCondition;
    procedure UpdateMod(AInfo: TModRefCountInfo);
    procedure UpdateAll;

    procedure ReferenceMod(AIdentifier: AnsiString);
    procedure DeReferenceMod(AIdentifier: AnsiString);
  public
    constructor Create(AModsCondition: TLogicalIDCondition);

    procedure UpdateForced(ASource: TModUsage);
    procedure AddFrom(ASource: TModUsage);
    procedure RemoveFrom(ASource: TModUsage);

    procedure Assign(Source: TPersistent); override;

    procedure ForceMod(AIdentifier: AnsiString; AForce: Boolean);

    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
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

  TPlayerHeroes = class (specialize TGNamedCollection<TPlayerHero>)
  end;

  { TPlayerTown }

  TPlayerTown = class(TNamedCollectionItem)
  private
    FMapObject: TMapObject;
    procedure SetMapObject(AValue: TMapObject);
  public
    property MapObject: TMapObject read FMapObject write SetMapObject;
  end;


  { TPlayerTowns }

  TPlayerTowns = class (specialize TGNamedCollection<TPlayerTown>)
  end;

  { TMainTownInfo }

  TMainTownInfo = class (TPersistent)
  private
    FGenerateHero: boolean;
    FMapObject: TMapObject;
    function GetL: integer;
    function GetX: integer;
    function GetY: integer;
    procedure SetGenerateHero(AValue: boolean);
  public
    procedure SetMapObject(AValue: TMapObject);
    property MapObject: TMapObject read FMapObject write SetMapObject;
  published
    property x: integer read GetX default -1;
    property y: integer read GetY default -1;
    property l: integer read GetL default -1;

    property GenerateHero: boolean read FGenerateHero write SetGenerateHero;
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

    FMainTown: TMainTownInfo;
    FMainHero: AnsiString;
    FRandomFaction: Boolean;
    FTowns: TPlayerTowns;

    function HasMainTown: boolean;
    function IsAllowedFactionsStored: Boolean;
  public
    constructor Create(AColor: TPlayerColor; AOwner: IReferenceNotify);
    destructor Destroy; override;

    property Color: TPlayerColor read FColor;

    procedure TownAdded(AObject: TMapObject);
    procedure TownRemoved(AObject: TMapObject);
    procedure HeroAdded(AObject: TMapObject);
    procedure HeroRemoved(AObject: TMapObject);

    property Towns: TPlayerTowns read FTowns;

    procedure SetMainHero(AObject: TMapObject);
  public // ISerializeSpecial
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;

  public//IFPObserver
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

  published
    property AllowedFactions: TLogicalIDCondition read FAllowedFactions stored IsAllowedFactionsStored;
    property RandomFaction: Boolean read FRandomFaction write FRandomFaction default false;
    property CanPlay: TPlayableBy read FCanPlay write FCanPlay default TPlayableBy.None;
    property MainTown: TMainTownInfo read FMainTown write FMainTown stored HasMainTown;

    //object instance id (not type)
    property MainHero: AnsiString read FMainHero write FMainHero;
  public
    property AITactics: TAITactics read FAITactics write FAITactics; //not used in vcmi (yet)
  public //special streaming
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
    function GetPlayerName(APlayer:TPlayer): TLocalizedString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Include(APlayer: TPlayerColor);
    procedure Exclude(APlayer: TPlayerColor);
    function Has(APlayer: TPlayerColor): Boolean;

    function FormatDescription(PoV: TPlayer): TLocalizedString;
    function IsEmpty: Boolean;
  published
    property Members: TPlayers read FMembers;
  end;

  { TTeamSettings }

  TTeamSettings = class (specialize TGArrayCollection<TTeam>)
  private
    FOwner: TVCMIMap;
  public
    constructor Create(AOwner: TVCMIMap);

    property Owner: TVCMIMap read FOwner;
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
    function IsEmpty: Boolean;
  end;

{$pop}

  { TMapTile }

  PMapTile = ^TMapTile;

  TMapTile = packed record
  strict private
    //start binary compatible with H3 part
    FTerType: TTerrainType;
    FTerSubtype: UInt8;
    FRiverType: TRiverType;
    FRiverDir:  UInt8;
    FRoadType: TRoadType;
    FRoadDir: UInt8;
    FFlags: UInt8;
    //end binary compatible with H3 part
    FOwner: TPlayer;

    FBlockingCount: Int16; //amount of objects blocking this tile
    FActiveCount: Int16; //amount of objects visitable on this tile
    FFlaggableID: Int32; //Internal id of (first) flaggable object in this tile.
  public
    constructor Create(ATerType: TTerrainType; ATerSubtype: UInt8);

    property TerType: TTerrainType read FTerType;
    property TerSubType: UInt8 read FTerSubtype;

    property RiverType:TRiverType read FRiverType;
    property RiverDir:UInt8 read FRiverDir;
    property RoadType:TRoadType read FRoadType;
    property RoadDir:UInt8 read FRoadDir;
    property Flags:UInt8 read FFlags;

    procedure SetRiver(ARiverType: TRiverType; ARriverDir: UInt8; AMir: Uint8); inline;
    procedure SetRoad(ARoadType: TRoadType; ARoadDir: UInt8; AMir: Uint8); inline;
    procedure SetTerrain(TT: TTerrainType; TS: UInt8; AMir: UInt8);inline;

    procedure BlockingObjectAdded(AObject: TMapObject; Active,Blocked: Boolean);
    procedure BlockingObjectRemoved(AObject: TMapObject; Active,Blocked: Boolean);

    function IsBlocked: boolean; inline;
    function IsActive: boolean; inline;

    property FlaggableID:Int32 read FFlaggableID;
    property Owner:TPlayer read FOwner;
  end;

  TTiles = array of array of TMapTile; //X, Y

{$push}
{$m+}
  { TMapLevel }

  TMapLevel = class(TNamedCollectionItem)
  strict private
    FHeight: Integer;
    FTerrain: TFilename;
    FTiles: TTiles;
    FWidth: Integer;
    function GetMap: TVCMIMap;
    function GetTile(X, Y: Integer): PMapTile; inline;
    procedure SetHeight(AValue: Integer);
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

    procedure SetRoad(x,y: integer; ARoadType: TRoadType; ARoadDir: UInt8; AMir: Uint8); inline;
    procedure SetRiver(x,y: integer; ARiverType: TRiverType; ARriverDir: UInt8; AMir: Uint8); inline;
    procedure SetTerrain(X, Y: Integer; TT: TTerrainType; TS: UInt8; mir: UInt8); inline;

    procedure ObjectAdded(AObject:TMapObject); inline;
    procedure ObjectChanged(AObject:TMapObject); inline;
    procedure ObjectRemoved(AObject:TMapObject); inline;

    function GetDimentions: TMapRect;
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
  end;

  { TMapObjectAppearance }

  TMapObjectAppearance = class(TObject, ISerializeNotify, IFPObserver)
  strict private
    FAllowedTerrains: TTerrainTypes;
    FTags: TStrings;
    FMaskWidth, FMaskHeight: Integer;
    FOwner: TMapObject;
    FDef: TAnimation;
    FIsVisitable: Boolean;
    FAnimation: AnsiString;
    FEditorAnimation: AnsiString;
    FMask: TStrings;
    FVisitableFrom: TStrings;
    FzIndex: Integer;
    function IsTagsStored: Boolean;
    procedure SetAnimation(AValue: AnsiString);
    procedure SetEditorAnimation(AValue: AnsiString);
    procedure SetzIndex(AValue: Integer);
    procedure SetDef(AValue:TAnimation);

    procedure AnimationChanged;
    procedure CompactMask;
    procedure UpdateCache;
  public
    constructor Create(AOwner: TMapObject);
    destructor Destroy; override;

    procedure Assign(ASource: TMapObjectTemplate);
    procedure BeforeSerialize(Sender:TObject);
    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);

    procedure OnOwnerChanged(ATiles:TTiles; op: TFPObservedOperation);

    procedure RenderOverlay(AState: TLocalState; X,Y: integer);

    Procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);

    property Visitable: Boolean read FIsVisitable;
    function IsVisitableAt(ALevel, AX, AY: Integer): Boolean; inline;

    function IsMaskedAt(ALevel, AX, AY: Integer; AMask:TMaskFilter): Boolean;

    property Width: Integer read FMaskWidth;
    property Height: Integer read FMaskHeight;

    property Def: TAnimation read FDef;
    procedure GetKeyWords(ATarget: TStrings);

  published
    property Animation: AnsiString read FAnimation write SetAnimation;
    property EditorAnimation: AnsiString read FEditorAnimation write SetEditorAnimation;
    property VisitableFrom: TStrings read FVisitableFrom;
    property AllowedTerrains: TTerrainTypes read FAllowedTerrains write FAllowedTerrains default ALL_TERRAINS;
    property Mask: TStrings read FMask;
    property ZIndex: Integer read FzIndex write SetzIndex default 0;
    property Tags: TStrings read FTags stored IsTagsStored;
  end;

  { TMapObject }

  TMapObject = class (TNamedCollectionItem, IMapObject, ISerializeSpecial)
  strict private
    FMap: TVCMIMap;

    FMapObjectGroup: TMapObjectGroup;
    FMapObjectType: TMapObjectType;

    FModUsage: TModUsage;
    FPosition: TPosition;
    FLastFrame: Integer;
    FOptions: TObjectOptions;
    FPlayer: TPlayer;
    FAppearance: TMapObjectAppearance;
    FAppearanceValid: Boolean;
    function GetCategory: TObjectCategory;
    function GetIdx: integer;
    function GetL: integer; inline;
    function GetX: integer; inline;
    function GetY: integer; inline;

    procedure Render(AState: TLocalState; Frame:integer; Ax,Ay: integer);
    procedure SetL(AValue: integer);
    procedure SetSubtype(AValue: AnsiString);
    procedure SetType(AValue: AnsiString);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);

    procedure UpdateIdentifier;
    procedure RecreateOptions;
  protected
    procedure SetCollection(Value: TCollection); override;
    function GetDisplayName: string; override;
  public
    //create object already on map, f.e. for deserializtion
    constructor Create(ACollection: TCollection); override;

    //create object for this map but not present on map, f.e. before object is added
    constructor CreateIndep(AMap: TVCMIMap);

    destructor Destroy; override;
    procedure RenderStatic(AState: TLocalState); inline;
    procedure RenderStatic(AState: TLocalState; X,Y: integer); inline;
    procedure RenderAnim(AState: TLocalState; ANextFrame: Boolean); inline;

    procedure RenderOverlay(AState: TLocalState;X,Y: integer); inline;

    //for palette
    procedure RenderIcon(AState: TLocalState; AX, AY, dim:integer);

    procedure RenderSelectionRect(AState: TLocalState); inline;

    function CoversTile(ALevel, AX, AY: Integer; AOption: TSelectObjectBy): boolean;
    function IsVisitableAt(ALevel, AX, AY: Integer): boolean;  inline;

    function GetMap:TVCMIMap;

    procedure AssignTemplate(ATemplate: TMapObjectTemplate);

    function FormatDisplayName(ACustomName: TLocalizedString): TLocalizedString;

    function GetRegion: TMapRect;
    //ignores position
    function GetRegion(AX, AY: integer): TMapRect;
    function EqualPosition(APosition: TPosition): Boolean;
    function HasOptions: boolean;

    procedure AddToMask(var Activity: TPassabilityMask; var Blockage: TPassabilityMask; const ARegion: TMapRect);

  public //IMapObject
    function GetType: AnsiString;
    function GetSubtype: AnsiString;

    function GetPlayer: TPlayer;
    procedure SetPlayer(AValue: TPlayer);
    procedure NotifyHeroTypeChanged(AOldType, ANewType: AnsiString);
    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
    procedure InvalidateAppearance;
    procedure ValidateAppearance;

    function GetListsManager: TListsManager;

    property MapObjectGroup: TMapObjectGroup read FMapObjectGroup;
    property MapObjectType: TMapObjectType read FMapObjectType;

    property Category: TObjectCategory read GetCategory;
    procedure GetKeyWords(ATarget: TStrings);
  public //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  published
    property X:integer read GetX write SetX;
    property Y:integer read GetY write SetY;
    property L:integer read GetL write SetL;

    property Template: TMapObjectAppearance read FAppearance;
  public //manual streamimg
    property &Type: AnsiString read GetType write SetType;
    property Subtype: AnsiString read GetSubtype write SetSubtype;

    property Options: TObjectOptions read FOptions;
  end;


  { TDefaultMapObjectCompare }

  TDefaultMapObjectCompare = class
  public
    class function c(a,b: TMapObject): boolean;
  end;

  TMapObjectSet = specialize TSet<TMapObject, TDefaultMapObjectCompare>;

  { TBlitOrderCompare }

  TBlitOrderCompare = class
  public
    class function c(a,b: TMapObject): boolean;
  end;

  TMapObjectQueue = specialize TPriorityQueue<TMapObject, TBlitOrderCompare>;

  { TMapObjectList }

  TMapObjectList = class(specialize TFPGObjectList<TMapObject>)
  public
    constructor Create();
  end;

  { TMapObjectsSelection }

  TMapObjectsSelection = class(TObject, ISearchResult)
  strict private
    FData:TMapObjectList;
    function GetObjcts(AIndex: SizeInt): TMapObject;
  public
    constructor Create();
    destructor Destroy; override;
    property Objcts[AIndex: SizeInt]: TMapObject read GetObjcts;

    property Data:TMapObjectList read FData;

  public//ISearchResult
    procedure Add(AObject: TObject); virtual;
    procedure Clear; virtual;
    function GetCount: SizeInt;
    procedure RenderIcon(AIndex:SizeInt; AState: TLocalState; AX, AY, dim:integer; color: TPlayer = TPlayer.none);
  end;

  { TVisibleObjects }

  TVisibleObjects = class(TMapObjectsSelection)
  strict private
    FRegion:TMapRect;
    FActivity, FBlockage: array of array of boolean;
  public
    constructor Create();
    destructor Destroy; override;

    procedure RenderAnimation(AState: TLocalState; Animate, NewAnimFrame: Boolean);
    procedure RenderOverlay(AState: TLocalState); deprecated;

    procedure Add(AObject: TObject); override;
    procedure Clear; override;

    procedure SetRegion(const ARegion: TMapRect);
  end;

  { TMapObjects }

  TMapObjects = class(specialize TGNamedCollection<TMapObject>)
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
    i18n: TLocaleManager;
    gm: TGraphicsManager;
  end;

  { THeroDefinition }

  THeroDefinition = class (TNamedCollectionItem, ISerializeNotify, IEditableHeroInfo, IHeroInfo)
  private
    FArtifacts: THeroArtifacts;
    FAvailableFor: TPlayers;
    FBiography: TLocalizedString;
    FExperience: UInt64;
    FName: TLocalizedString;
    FPortrait: Int32;
    FPrimarySkills: THeroPrimarySkills;
    FSex: THeroSex;
    FSkills: THeroSecondarySkills;
    FSpellBook: TStrings;

    function IsArtifactsStored: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsSkillsStored: Boolean;
    function IsSpellBookStored: Boolean;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public//ISerializeNotify
    procedure BeforeDeSerialize({%H-}Sender: TObject; {%H-}AData: TJSONData);
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData);
    procedure BeforeSerialize({%H-}Sender: TObject);
    procedure AfterSerialize(Sender: TObject; AData: TJSONData);
  public//IEditableHeroInfo, IHeroInfo
    function GetHeroIdentifier: AnsiString;

    function GetBiography: TLocalizedString;
    procedure SetBiography(const AValue: TLocalizedString);

    function GetExperience: UInt64;
    procedure SetExperience(const AValue: UInt64);

    function GetName: TLocalizedString;
    procedure SetName(const AValue: TLocalizedString);

    function GetPortrait: Int32;
    procedure SetPortrait(const AValue: Int32);

    function GetSex: THeroSex;
    procedure SetSex(const AValue: THeroSex);

    function GetPrimarySkills: THeroPrimarySkills;
    function GetSecondarySkills: THeroSecondarySkills;
  published
    property AvailableFor: TPlayers read FAvailableFor write FAvailableFor default ALL_PLAYERS;

    property Artifacts: THeroArtifacts read FArtifacts stored IsArtifactsStored;
    property Biography: TLocalizedString read FBiography write SetBiography;
    property Experience: UInt64 read GetExperience write SetExperience default 0;
    property Name: TLocalizedString read FName write SetName;
    property PrimarySkills:THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property Skills: THeroSecondarySkills read FSkills stored IsSkillsStored;
    property SpellBook: TStrings read FSpellBook stored IsSpellBookStored;
  public //manual streaming
    property Sex:THeroSex read FSex write SetSex default THeroSex.default;
    property Portrait: Int32 read GetPortrait write SetPortrait default -1;
  end;

  { THeroDefinitions }

  THeroDefinitions = class (specialize TGNamedCollection<THeroDefinition>)
  private
    FOwner: TVCMIMap;
  public
    constructor Create(AOwner: TVCMIMap);

    property Owner: TVCMIMap read FOwner;
  end;

  TInstanceMap = specialize TFPGMap<TInstanceID, TMapObject>;


  { THeroPool }

  //pool of heroes of same class

  THeroPool = class (TNamedCollectionItem)
  strict private
    FPool: TStringList;
    FInfo: THeroClassInfo;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure InitialFill(AClassInfo: THeroClassInfo; AFullList: THeroInfos);

    procedure AddHero(AHeroInfo: THeroInfo);
    procedure RemoveHero(AType: AnsiString);

    function IsEmpty: Boolean;
    function PeekFirst: THeroInfo;

    function FillWithAvilableHeroes(AItems: TStrings; ASelected: THeroInfo): integer;

    property HeroClassInfo: THeroClassInfo read FInfo;
  end;

  { THeroClassPool }

  THeroClassPool = class (specialize TGNamedCollection<THeroPool>)
  strict private
    FFullList: THeroInfos;
    function GetPoolFromType(AType: AnsiString): THeroPool;
  public
    constructor Create(AHeroClassInfos: THeroClassInfos; AHeroInfos: THeroInfos);

    procedure HeroAdded(AObject: TMapObject);
    procedure HeroRemoved(AObject: TMapObject);
    procedure HeroChanged(AObject: TMapObject; AOldType, ANewType: AnsiString);
  end;


  { TVCMIMap }

  TVCMIMap = class (TPersistent, IFPObserver, IReferenceNotify)
  strict private
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
    FGraphicsManager: TGraphicsManager;

    FLevels: TMapLevels;

    FRumors: TRumors;

    FIsDirty: boolean;
    FTrackObjectChanges: Boolean;

    FVisibleObjectsQueue : TMapObjectQueue;

    FDefeatIconIndex: Integer;
    FDefeatString: TLocalizedString;
    FTeams: TTeamSettings;
    FVictoryIconIndex: Integer;
    FVictoryString: TLocalizedString;

    FHeroPool: THeroClassPool;

    FSearchIndexes: array [TObjectCategory] of TSearchIndex;

    function GetCurrentLevelIndex: Integer; inline;

    procedure AttachTo(AObserved: IFPObserved);

    procedure MapObjectAdded(AObject: TMapObject);
    procedure MapObjectRemoved(AObject: TMapObject);

    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure SetCurrentLevelIndex(AValue: Integer);
    procedure SetDifficulty(AValue: TDifficulty);
    procedure SetLevelLimit(AValue: Integer);
    procedure SetDescription(AValue: TLocalizedString);
    procedure SetName(AValue: TLocalizedString);

    procedure SetIsDirty(AValue: Boolean);

    procedure AddToIndex(AObject: TMapObject);
    procedure RemoveFromIndex(AObject: TMapObject);

    procedure BuildSearchIndex;
  private
    FObjectsManager: TObjectsManager;
     var
       FModUsage:TModUsage;

     function IsPredefinedHeroesStored: Boolean;
     function IsRumorsStored: Boolean;
     function IsTeamsStored: Boolean;
     procedure SetDefeatIconIndex(AValue: Integer);
     procedure SetDefeatString(AValue: TLocalizedString);
     procedure SetVictoryIconIndex(AValue: Integer);
     procedure SetVictoryString(AValue: TLocalizedString);
  public
    //create with default params
    constructor CreateDefault(env: TMapEnvironment);
    //create with specified params and set default options
    constructor Create(env: TMapEnvironment; Params: TMapCreateParams);

    constructor CreateEmpty(env: TMapEnvironment);

    destructor Destroy; override;

    procedure FillLevel(TT: TTerrainType);

    function GetTile(Level, X, Y: Integer): PMapTile;

    function IsOnMap(Level, X, Y: Integer): boolean;

    //Left, Right, Top, Bottom - clip rect in Tiles
    procedure RenderTerrain(AState:TLocalState; Left, Right, Top, Bottom: Integer);
    procedure RenderPassability(AState:TLocalState; Left, Right, Top, Bottom: Integer); unimplemented;

    procedure SelectVisibleObjects(ATarget: TVisibleObjects; Left, Right, Top, Bottom: Integer);
    procedure SelectVisibleObjects(ATarget: TVisibleObjects; const ARect: TMapRect);

    // AInput = space separated words or empty string to get all
    procedure SelectByKeywords(ATarget: TMapObjectsSelection; AInput: string; ACategory: TObjectCategory);

    property CurrentLevelIndex: Integer read GetCurrentLevelIndex write SetCurrentLevelIndex;

    procedure SaveToStream(ADest: TStream; AWriter: IMapWriter);

    property IsDirty: Boolean read FIsDirty write SetIsDirty;
    property TrackObjectChanges: Boolean read FTrackObjectChanges;

    property TerrainManager: TTerrainManager read FTerrainManager;
    property ListsManager: TListsManager read FListsManager;
    property GraphicsManager: TGraphicsManager read FGraphicsManager;
    property ObjectsManager: TObjectsManager read FObjectsManager;

    class procedure SelectObjectsOnTile(ASource: TMapObjectList; Level, X, Y: Integer; ATarget: TMapObjectQueue);

    class procedure SelectObjectsOnTile(ASource: TMapObjectList; Level, X, Y: Integer; ATarget: TMapObjectSet;
      ASelectBy:TSelectObjectBy; ASelectFilter: TSelectFilter);

    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
    procedure NotifyOwnerChanged(AObject: TMapObject; AOldOwner, ANewOwner: TPlayer);
    procedure NotifyHeroTypeChanged(AObject: TMapObject; AOldType, ANewType: AnsiString);
    procedure Loaded;

    //actual hero Name
    function GetHeroName(AObject: TMapObject): TLocalizedString;

    function GetCurrentLevelDimensions: TMapRect;

    //hero pool facade

    //for use with combobox items; returns selected index; selected id required
    function FillWithAvilableClasses(AItems: TStrings; ASelected: AnsiString): integer;

    //for use with combobox items; returns selected index
    function FillWithAvilableHeroes(AItems: TStrings; AHeroClass: AnsiString; ASelected: THeroInfo): integer;

  published
    property Name:TLocalizedString read FName write SetName; //+
    property Description:TLocalizedString read FDescription write SetDescription; //+

    property Difficulty: TDifficulty read FDifficulty write SetDifficulty nodefault; //?
    property HeroLevelLimit: Integer read FLevelLimit write SetLevelLimit default 199;//+

    property Players: TPlayerInfos read FPlayers;
    property Teams:TTeamSettings read FTeams stored IsTeamsStored;

    property Rumors: TRumors read FRumors stored IsRumorsStored;

    property AllowedSpells: TLogicalIDCondition read FAllowedSpells;
    property AllowedAbilities: TLogicalIDCondition read FAllowedAbilities;
    property AllowedArtifacts: TLogicalIDCondition read FAllowedArtifacts;
    property AllowedHeroes: TLogicalIDCondition read FAllowedHeroes;

    property MapLevels: TMapLevels read FLevels;

    property PredefinedHeroes: THeroDefinitions read FPredefinedHeroes stored IsPredefinedHeroesStored;

    property TriggeredEvents: TTriggeredEvents read FTriggeredEvents;

    property VictoryString: TLocalizedString read FVictoryString write SetVictoryString;
    property DefeatString: TLocalizedString read FDefeatString write SetDefeatString;
    property VictoryIconIndex: Integer read FVictoryIconIndex write SetVictoryIconIndex;
    property DefeatIconIndex: Integer read FDefeatIconIndex write SetDefeatIconIndex;

    property Mods: TLogicalIDCondition read FMods;
  public //manual streamimg
    property Objects: TMapObjects read FObjects;
  public
    function CurrentLevel: TMapLevel;
    property ModUsage:TModUsage read FModUsage;
  end;

{$pop}

implementation

uses FileUtil, LazLoggerBase, editor_str_consts, editor_utils,
  strutils, typinfo;

{ TVisibleObjects }

constructor TVisibleObjects.Create;
begin
  inherited;
  FRegion.Create();
end;

destructor TVisibleObjects.Destroy;
begin
  inherited Destroy;
end;

procedure TVisibleObjects.RenderAnimation(AState: TLocalState; Animate, NewAnimFrame: Boolean);
var
  o: TMapObject;
begin
  if Animate then
  begin
    for o in Data do
      o.RenderAnim(AState, NewAnimFrame);
  end
  else
  begin
    for o in Data do
      o.RenderStatic(AState);
  end;
end;

procedure TVisibleObjects.RenderOverlay(AState: TLocalState);
const
  ACTIVE_COLOR: TRBGAColor = (r:255; g:255; b:0; a:128);
  BLOCKED_COLOR: TRBGAColor = (r:255; g:0; b:0; a:128);
var
  i, j: Integer;
begin
  AState.StartDrawingRects;
  AState.UseTextures(false, false);

  AState.SetTranslation(FRegion.Left() * TILE_SIZE, FRegion.Top() * TILE_SIZE);

  for i := 0 to FRegion.FWidth - 1 do
  begin
    for j := 0 to FRegion.FHeight - 1 do
    begin
      if FActivity[i,j] then
        AState.RenderSolidRect(i * TILE_SIZE, j * TILE_SIZE, TILE_SIZE, TILE_SIZE, ACTIVE_COLOR)
      else if FBlockage[i,j] then
        AState.RenderSolidRect(i * TILE_SIZE, j * TILE_SIZE, TILE_SIZE, TILE_SIZE, BLOCKED_COLOR);
    end;
  end;
end;

procedure TVisibleObjects.Add(AObject: TObject);
var
  o: TMapObject;
begin
  inherited Add(AObject);
  o := TMapObject(AObject);
  o.AddToMask(FActivity, FBlockage, FRegion);
end;

procedure TVisibleObjects.Clear;
begin
  inherited Clear;
  SetLength(FActivity,0);
  SetLength(FBlockage,0);
  FRegion.Clear();
end;

procedure TVisibleObjects.SetRegion(const ARegion: TMapRect);
begin
  FRegion := ARegion;
  SetLength(FActivity, FRegion.FWidth, FRegion.FHeight);
  SetLength(FBlockage, FRegion.FWidth, FRegion.FHeight);
end;

{ TMapObjectsSelection }

function TMapObjectsSelection.GetObjcts(AIndex: SizeInt): TMapObject;
begin
  Result := FData[AIndex];
end;

constructor TMapObjectsSelection.Create;
begin
  FData := TMapObjectList.Create();
end;

destructor TMapObjectsSelection.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TMapObjectsSelection.Clear;
begin
  FData.Clear;
end;

function TMapObjectsSelection.GetCount: SizeInt;
begin
  Result := FData.Count;
end;

procedure TMapObjectsSelection.RenderIcon(AIndex: SizeInt; AState: TLocalState; AX, AY, dim: integer; color: TPlayer);
var
  o_def: TMapObject;
begin
  o_def := FData.Items[AIndex];
  o_def.RenderIcon(AState, AX, AY, dim);
end;

procedure TMapObjectsSelection.Add(AObject: TObject);
begin
  FData.Add(TMapObject(AObject));
end;

{ TModRefCountInfo }

procedure TModRefCountInfo.SetRefCount(AValue: Integer);
begin
  if FRefCount=AValue then Exit;
  FRefCount:=AValue;
end;

procedure TModRefCountInfo.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TModRefCountInfo then
  begin
    TModRefCountInfo(Dest).Forced:=Forced;
    TModRefCountInfo(Dest).RefCount:=RefCount;
  end;
end;

procedure TModRefCountInfo.SetForced(AValue: boolean);
begin
  if FForced=AValue then Exit;
  FForced:=AValue;
end;

{ TModUsage }

procedure TModUsage.UpdateMod(AInfo: TModRefCountInfo);
var
  idx: Integer;
begin
  if not Assigned(FModsCondition) then
  begin
    Exit;
  end;

  if (AInfo.Forced) or (AInfo.RefCount > 0 ) then
  begin
    FModsCondition.AllOf.Add(AInfo.Identifier);
  end
  else
  begin
    idx := FModsCondition.AllOf.IndexOf(AInfo.Identifier);
    if idx >=0 then
       FModsCondition.AllOf.Delete(idx);
  end;
end;

procedure TModUsage.UpdateAll;
var
  i: Integer;
begin
  if not Assigned(FModsCondition) then
  begin
    Exit;
  end;

  FModsCondition.Clear;

  for i := 0 to Count - 1 do
  begin
    UpdateMod(Items[i]);
  end;
end;

constructor TModUsage.Create(AModsCondition: TLogicalIDCondition);
begin
  inherited Create;
  FModsCondition := AModsCondition;
end;

procedure TModUsage.UpdateForced(ASource: TModUsage);
var
  i: Integer;
  item: TItemType;
begin
  for i := 0 to Count - 1 do
  begin
    ForceMod(Items[i].Identifier, false);
  end;

  for i := 0 to ASource.Count - 1 do
  begin
    item := ASource.Items[i];
    ForceMod(item.Identifier, item.Forced);
  end;
end;

procedure TModUsage.AddFrom(ASource: TModUsage);
var
  i: Integer;
  s_item, d_item: TModRefCountInfo;
begin
  for i := 0 to ASource.Count - 1 do
  begin
    s_item := ASource.Items[i];

    d_item := EnsureItem(s_item.Identifier);

    d_item.RefCount := d_item.RefCount+s_item.RefCount;

    UpdateMod(d_item);
  end;
end;

procedure TModUsage.RemoveFrom(ASource: TModUsage);
var
  i: Integer;
  s_item, d_item: TModRefCountInfo;
begin
  for i := 0 to ASource.Count - 1 do
  begin
    s_item := ASource.Items[i];

    d_item := EnsureItem(s_item.Identifier);

    d_item.RefCount := d_item.RefCount - s_item.RefCount;

    if d_item.RefCount < 0 then
    begin
      raise Exception.Create('Negative mod refcount');
    end;

    UpdateMod(d_item);
  end;
end;

procedure TModUsage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Assigned(FModsCondition) and (Source is TModUsage) then
  begin
    UpdateAll;
  end;
end;

procedure TModUsage.ForceMod(AIdentifier: AnsiString; AForce: Boolean);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := EnsureItem(AIdentifier);

  if mod_info.Forced <> AForce then
  begin
    mod_info.Forced:=AForce;
    UpdateMod(mod_info);
  end;
end;

procedure TModUsage.ReferenceMod(AIdentifier: AnsiString);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := EnsureItem(AIdentifier);
  mod_info.RefCount:=mod_info.RefCount+1;

  if (mod_info.RefCount = 1) and (not mod_info.Forced) then
  begin
    UpdateMod(mod_info);
  end;
end;

procedure TModUsage.DeReferenceMod(AIdentifier: AnsiString);
var
  mod_info: TModRefCountInfo;
begin
  mod_info := EnsureItem(AIdentifier);
  mod_info.RefCount:=mod_info.RefCount-1;

  if mod_info.RefCount < 0 then
  begin
    raise Exception.Create('Negative mod refcount');
  end;

  if (mod_info.RefCount = 0) and (not mod_info.Forced) then
  begin
    UpdateMod(mod_info);
  end;
end;

procedure TModUsage.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
var
  mod_id: AnsiString;
begin
  mod_id:=ExtractModID(AOldIdentifier);

  if mod_id <> '' then
  begin
    DeReferenceMod(mod_id);
  end;

  mod_id:=ExtractModID(ANewIdentifier);

  if mod_id <> '' then
  begin
    ReferenceMod(mod_id);
  end;
end;

{ THeroClassPool }

function THeroClassPool.GetPoolFromType(AType: AnsiString): THeroPool;
var
  info: THeroInfo;
begin
  info := FFullList.FindItem(AType);
  Result := FindItem(info.&Class);
end;

constructor THeroClassPool.Create(AHeroClassInfos: THeroClassInfos; AHeroInfos: THeroInfos);
var
  i: Integer;
  c_info: THeroClassInfo;
  pool: THeroPool;
begin
  inherited Create;
  FFullList := AHeroInfos;

  for i := 0 to AHeroClassInfos.Count - 1 do
  begin
    c_info := AHeroClassInfos.Items[i];

    pool := EnsureItem(c_info.Identifier);

    pool.InitialFill(c_info, AHeroInfos);
  end;
end;

procedure THeroClassPool.HeroAdded(AObject: TMapObject);
var
  options: THeroOptions;
  info: THeroInfo;

  pool: THeroPool;
  i: Integer;
begin
  options := AObject.Options as THeroOptions;

  if options.&type = '' then
  begin
    //select type from pool, but DO NOT track this change

    case AObject.&Type of
      TYPE_HERO:
      begin
        pool := FindItem(AObject.Subtype);

        if pool.IsEmpty then
        begin
          raise EAbort.CreateFmt('No heroes of class %s is avilable',[AObject.Subtype]);
        end
        else
        begin
          info := pool.PeekFirst;
          options.&type := info.Identifier;
        end;
      end;
      TYPE_PRISON:
      begin
        for i := 0 to Count - 1 do
        begin
          pool := Items[i];
          if not pool.IsEmpty then
          begin
            info := pool.PeekFirst;
            options.&type := info.Identifier;
            Break;
          end;
        end;

        if options.&type = '' then
        begin
          raise EAbort.Create('No heroes are avilable');
        end;
      end;
    end;
  end
  else
  begin
    //just track as used
    HeroChanged(AObject, '',  options.&type);
  end;
end;

procedure THeroClassPool.HeroRemoved(AObject: TMapObject);
var
  options: THeroOptions;
begin
  options := AObject.Options as THeroOptions;

  HeroChanged(AObject, options.&type, '');
end;

procedure THeroClassPool.HeroChanged(AObject: TMapObject; AOldType, ANewType: AnsiString);
var
  info: THeroInfo;
begin
  if AOldType <> '' then
  begin
    info := FFullList.FindItem(AOldType);
    GetPoolFromType(AOldType).AddHero(info);
  end;

  if ANewType <> '' then
  begin
    GetPoolFromType(ANewType).RemoveHero(ANewType);
  end
end;

{ THeroPool }

constructor THeroPool.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPool := TStringList.Create;
  FPool.Sorted := true;
  FPool.Duplicates:=dupIgnore;
end;

destructor THeroPool.Destroy;
begin
  FPool.Free;
  inherited Destroy;
end;

procedure THeroPool.InitialFill(AClassInfo: THeroClassInfo; AFullList: THeroInfos);
var
  hero_info: THeroInfo;
  i: Integer;
begin
  FInfo := AClassInfo;

  FPool.Clear;

  for i := 0 to AFullList.Count - 1 do
  begin
    hero_info := AFullList.Items[i];

    if hero_info.&Class = FInfo.Identifier then
    begin
      FPool.AddObject(hero_info.Identifier, hero_info);
    end;
  end;
end;

procedure THeroPool.AddHero(AHeroInfo: THeroInfo);
begin
  FPool.AddObject(AHeroInfo.Identifier, AHeroInfo);
end;

procedure THeroPool.RemoveHero(AType: AnsiString);
var
  idx: Integer;
begin
  idx := FPool.IndexOf(AType);
  if idx >=0 then
    FPool.Delete(idx)
  else
    raise EAbort.CreateFmt('Hero %s is not avilable',[AType]);
end;

function THeroPool.IsEmpty: Boolean;
begin
  Result := FPool.Count = 0;
end;

function THeroPool.PeekFirst: THeroInfo;
begin
  Result := FPool.Objects[0] as THeroInfo;
end;

function THeroPool.FillWithAvilableHeroes(AItems: TStrings; ASelected: THeroInfo): integer;
var
  i: Integer;
  info: THeroInfo;
begin
  Result := -1;
  AItems.Clear;

  if (FPool.Count = 0) and (not Assigned(ASelected)) then
    raise Exception.Create('Hero pool is empty');

  for i := 0 to FPool.Count - 1 do
  begin
    info :=  FPool.Objects[i] as THeroInfo;

    if Assigned(ASelected) and (info.Identifier = ASelected.Identifier) then
    begin
      Result := i;
    end;

    AItems.AddObject(info.Name, info);
  end;

  if Assigned(ASelected) and (Result = -1) then
  begin
    AItems.InsertObject(0, ASelected.Name, ASelected);
    Result := 0;
  end;

  if Result = -1 then
  begin
    Result := 0;
  end;
end;

{ TDefaultMapObjectCompare }

class function TDefaultMapObjectCompare.c(a, b: TMapObject): boolean;
begin
  Result := PtrUInt(Pointer(a)) < PtrUInt(Pointer(b));
end;

{ TMapObjectList }

constructor TMapObjectList.Create;
begin
  inherited Create(False); //map objects are owned by collection so do not try to free them automatically
end;

{ TMainTownInfo }

function TMainTownInfo.GetL: integer;
begin
  if Assigned(FMapObject) then
    Result := FMapObject.L
  else
    Result := -1;
end;

function TMainTownInfo.GetX: integer;
begin
  if Assigned(FMapObject) then
    Result := FMapObject.X
  else
    Result := -1;
end;

function TMainTownInfo.GetY: integer;
begin
  if Assigned(FMapObject) then
    Result := FMapObject.Y
  else
    Result := -1;
end;

procedure TMainTownInfo.SetGenerateHero(AValue: boolean);
begin
  if FGenerateHero=AValue then Exit;
  FGenerateHero:=AValue;
end;

procedure TMainTownInfo.SetMapObject(AValue: TMapObject);
begin
  FMapObject:=AValue;
end;

{ TTeam }

function TTeam.GetPlayerName(APlayer: TPlayer): TLocalizedString;
begin
  if Assigned(Collection) then
  begin
    Result := (Collection as TTeamSettings).Owner.ListsManager.PlayerName[APlayer];
  end
  else
  begin
    if APlayer = TPlayer.NONE then
    begin
      Result := NEWTRAL_PLAYER_NAME;
    end
    else begin
      Result := PLAYER_NAMES[APlayer];
    end;
  end;
end;

procedure TTeam.AssignTo(Dest: TPersistent);
begin
  if Dest is TTeam then
  begin
    TTeam(dest).FMembers := Members;
  end
  else begin
    inherited AssignTo(Dest);
  end;
end;

procedure TTeam.Include(APlayer: TPlayerColor);
var
  item: TCollectionItem;
begin
  if Assigned(Collection) then
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

function TTeam.Has(APlayer: TPlayerColor): Boolean;
begin
  result := FMembers * [APlayer] <> [];
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

      Result := GetPlayerName(iter);
    end
    else
    begin
      Result := Result + ', ' + GetPlayerName(iter);
    end;
  end;
end;

function TTeam.IsEmpty: Boolean;
begin
  Result := FMembers = [];
end;

{ TTeamSettings }

constructor TTeamSettings.Create(AOwner: TVCMIMap);
begin
  FOwner := AOwner;
  inherited Create;
end;

{ TPlayerTown }

procedure TPlayerTown.SetMapObject(AValue: TMapObject);
begin
  if FMapObject=AValue then Exit;
  FMapObject:=AValue;
  Identifier:=AValue.Identifier;
end;

{ TMapObjectAppearance }

procedure TMapObjectAppearance.SetAnimation(AValue: AnsiString);
begin
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;
  AnimationChanged;
end;

function TMapObjectAppearance.IsTagsStored: Boolean;
begin
  Result := FTags.Count>0;
end;

procedure TMapObjectAppearance.SetEditorAnimation(AValue: AnsiString);
begin
  if FEditorAnimation=AValue then Exit;
  FEditorAnimation:=AValue;
  AnimationChanged;
end;

procedure TMapObjectAppearance.SetzIndex(AValue: Integer);
begin
  if FzIndex=AValue then Exit;
  FzIndex:=AValue;
end;

procedure TMapObjectAppearance.SetDef(AValue: TAnimation);
begin
  FDef := AValue;
  FOwner.GetMap.GraphicsManager.LoadGraphics(FDef);
end;

procedure TMapObjectAppearance.AnimationChanged;
begin
  if FEditorAnimation='' then
  begin
    SetDef(FOwner.GetMap.GraphicsManager.GetGraphics(FAnimation));
  end
  else
  begin
    SetDef(FOwner.GetMap.GraphicsManager.GetGraphics(FEditorAnimation));
  end;
end;

procedure TMapObjectAppearance.CompactMask;
var
  stop: Boolean;
  s: AnsiString;
  i: Integer;
begin
  FMask.BeginUpdate;
  try
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

    for i := 0 to FMask.Count - 1 do
    begin
      s := StringReplace(FMask[i], '0', ' ',[rfReplaceAll]);

      FMask[i] := TrimLeft(s);
    end;
  finally
    FMask.EndUpdate;
  end;
end;

procedure TMapObjectAppearance.UpdateCache;
var
  s: String;
  c: Char;
  w: Integer;
begin
  FMaskHeight := Mask.Count;
  w := 1;

  FIsVisitable:=false;
  for s in FMask do
  begin
    w := Max(w, Length(s));
    for c in s do
    begin
      if (c = MASK_ACTIVABLE) or (c = MASK_TRIGGER) then
      begin
        FIsVisitable := true;
      end;
    end;
  end;
  FMaskWidth:=w;
end;

procedure TMapObjectAppearance.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FMask then
  begin
    UpdateCache;
  end;
end;

function TMapObjectAppearance.IsVisitableAt(ALevel, AX, AY: Integer): Boolean;
begin
  Result := IsMaskedAt(ALevel, AX, AY, [MASK_ACTIVABLE, MASK_TRIGGER]);
end;

function TMapObjectAppearance.IsMaskedAt(ALevel, AX, AY: Integer; AMask: TMaskFilter): Boolean;
var
  dx, dy: Integer;
  line: String;
begin
  if ALevel <> FOwner.L then
    Exit(false);

  dx := FOwner.X - AX;
  dy := FOwner.Y - AY;

  if (dx < 0) or (dy < 0) or (dx >= FMaskWidth) or (dy >=  FMaskHeight) then
    Exit(false);

  line := Mask[FMaskHeight - dy - 1];
  if Length(line) < (dx+1) then
     Exit(false);
  Result := line[Length(line) - dx] in AMask;
end;

procedure TMapObjectAppearance.GetKeyWords(ATarget: TStrings);
begin
  ATarget.AddStrings(FTags);
end;

constructor TMapObjectAppearance.Create(AOwner: TMapObject);
begin
  FOwner := AOwner;
  FMask := TStringList.Create;
  FMask.FPOAttachObserver(Self);
  FVisitableFrom := TStringList.Create;
  SetDef(FOwner.GetMap.GraphicsManager.GetGraphics('default'));
  FAllowedTerrains := ALL_TERRAINS;
  FTags := TStringList.Create;
end;

destructor TMapObjectAppearance.Destroy;
begin
  FTags.Free;
  FMask.FPODetachObserver(Self);
  FMask.Free;
  FVisitableFrom.Free;
  inherited Destroy;
end;

procedure TMapObjectAppearance.Assign(ASource: TMapObjectTemplate);
begin
  FMask.BeginUpdate;
  try
    FAnimation := ASource.Animation;
    FEditorAnimation := ASource.EditorAnimation;
    FVisitableFrom.Assign(ASource.VisitableFrom);
    FMask.Assign(ASource.Mask);
    CompactMask;
    SetDef(ASource.Def);
    FZIndex := ASource.ZIndex;
    FAllowedTerrains:=ASource.AllowedTerrains;
    FTags.Assign(ASource.Tags);
  finally
    FMask.EndUpdate;
  end;
end;

procedure TMapObjectAppearance.BeforeSerialize(Sender: TObject);
begin
  CompactMask;
end;

procedure TMapObjectAppearance.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin
  FMask.BeginUpdate;
end;

procedure TMapObjectAppearance.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TMapObjectAppearance.AfterDeSerialize(Sender: TObject; AData: TJSONData
  );
begin
  CompactMask;
  FMask.EndUpdate;
end;

procedure TMapObjectAppearance.OnOwnerChanged(ATiles: TTiles; op: TFPObservedOperation);
var
  i, j, x, y, shift: Integer;
  line: String;

  t: PMapTile;
begin
  for i := 1 to FMaskHeight do
  begin
    line := Mask[i-1];
    y := FOwner.Y - FMaskHeight + i;

    shift := Length(line);

    for j := 1 to Length(line) do
    begin
      x := FOwner.X + j - shift;

      if (x>=0) and (y>=0) and (x < length(ATiles)) and (y < length(ATiles[x])) then
      begin
        t := @ATiles[x,y];

        case line[j] of
          MASK_ACTIVABLE, MASK_TRIGGER:
          begin
            case op of
              TFPObservedOperation.ooAddItem: t^.BlockingObjectAdded(FOwner, true, true);
              TFPObservedOperation.ooDeleteItem: t^.BlockingObjectRemoved(FOwner, true, true);
              TFPObservedOperation.ooChange:
              begin
                t^.BlockingObjectRemoved(FOwner, true, true);
                t^.BlockingObjectAdded(FOwner, true, true);
              end;
            end;
          end;
          MASK_BLOCKED, MASK_HIDDEN:
          begin
            case op of
              TFPObservedOperation.ooAddItem: t^.BlockingObjectAdded(FOwner, False, True);
              TFPObservedOperation.ooDeleteItem: t^.BlockingObjectRemoved(FOwner, False, True);
              TFPObservedOperation.ooChange:
              begin
                t^.BlockingObjectRemoved(FOwner, False, True);
                t^.BlockingObjectAdded(FOwner, False, True);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMapObjectAppearance.RenderOverlay(AState: TLocalState; X, Y: integer);
const
  ACTIVE_COLOR: TRBGAColor = (r:255; g:255; b:0; a:128);
  BLOCKED_COLOR: TRBGAColor = (r:255; g:0; b:0; a:128);
var
  i, j, p, shift: Integer;
  line: String;
begin
  AState.SetTranslation(X-FMaskWidth*TILE_SIZE, Y-FMaskHeight*TILE_SIZE);

  for i := 0 to FMaskHeight - 1 do
  begin
    line := Mask[i];
    shift := FMaskWidth - Length(line);
    for j := 0 to Length(line) - 1 do
    begin
      p := j + shift;
      case line[j+1] of
        MASK_ACTIVABLE, MASK_TRIGGER: AState.RenderSolidRect(p* TILE_SIZE, i * TILE_SIZE, TILE_SIZE, TILE_SIZE, ACTIVE_COLOR);
        MASK_BLOCKED, MASK_HIDDEN: AState.RenderSolidRect(p* TILE_SIZE, i * TILE_SIZE, TILE_SIZE, TILE_SIZE, BLOCKED_COLOR);
      end;
    end;
  end;
end;

{ THeroDefinition }

procedure THeroDefinition.SetExperience(const AValue: UInt64);
begin
  FExperience:=AValue;
end;

procedure THeroDefinition.SetName(const AValue: TLocalizedString);
begin
  FName:=AValue;
end;

procedure THeroDefinition.SetPortrait(const AValue: Int32);
begin
  FPortrait:=AValue;
end;

procedure THeroDefinition.SetSex(const AValue: THeroSex);
begin
  FSex:=AValue;
end;

procedure THeroDefinition.SetBiography(const AValue: TLocalizedString);
begin
  FBiography:=AValue;
end;

function THeroDefinition.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function THeroDefinition.IsSkillsStored: Boolean;
begin
  Result := FSkills.Count > 0;
end;

function THeroDefinition.IsArtifactsStored: Boolean;
begin
  Result := not FArtifacts.IsEmpty;
end;

function THeroDefinition.GetExperience: UInt64;
begin
  Result := FExperience;
end;

function THeroDefinition.GetPortrait: Int32;
begin
  Result := FPortrait;
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

function THeroDefinition.GetPrimarySkills: THeroPrimarySkills;
begin
  Result := FPrimarySkills;
end;

function THeroDefinition.GetSecondarySkills: THeroSecondarySkills;
begin
  Result := FSkills;
end;

function THeroDefinition.IsSpellBookStored: Boolean;
begin
  Result := FSpellBook.Count>0;
end;

constructor THeroDefinition.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSkills := THeroSecondarySkills.Create;
  FArtifacts := THeroArtifacts.Create((ACollection as THeroDefinitions).FOwner);
  FSpellBook := TIdentifierSet.Create((ACollection as THeroDefinitions).FOwner);
  FPrimarySkills := THeroPrimarySkills.Create;

  FSex:=THeroSex.default;
  FAvailableFor := ALL_PLAYERS;
  FPortrait:=-1;
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
  Portrait:=THeroDefinitions(Collection).Owner.ListsManager.LoadHeroPortrait(AData);
end;

procedure THeroDefinition.BeforeSerialize(Sender: TObject);
begin
  //do nothing
end;

procedure THeroDefinition.AfterSerialize(Sender: TObject; AData: TJSONData);
begin
  SaveHeroSex(AData, Sex);
  THeroDefinitions(Collection).Owner.ListsManager.SaveHeroPortrait(AData, Portrait);
end;

function THeroDefinition.GetHeroIdentifier: AnsiString;
begin
  Result := Identifier;
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

function TRumors.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

{ TBlitOrderCompare }

class function TBlitOrderCompare.c(a, b: TMapObject): boolean;
begin
  if a.Template.ZIndex < b.Template.ZIndex then
  begin
    Exit(True);
  end;

  if a.Template.ZIndex > b.Template.ZIndex then
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

  if a.Options.ZIndex < b.Options.ZIndex then
    Exit(True);

  if a.Options.ZIndex > b.Options.ZIndex then
    Exit(False);

  if a.Template.Visitable and not b.Template.Visitable then
    Exit(false);

  if not a.Template.Visitable and b.Template.Visitable then
    Exit(true);

  Exit(a.X < b.X);
end;

{ TMapObject }

function TMapObject.CoversTile(ALevel, AX, AY: Integer; AOption: TSelectObjectBy): boolean;
var
  w: UInt32;
  h: UInt32;
begin
  case AOption of
  TSelectObjectBy.BBox:
  begin
    //TODO: use MASK
    w := Template.Def.Width div TILE_SIZE;
    h := Template.Def.Height div TILE_SIZE;
    Result := (L = ALevel)
      and (X>=AX) and (Y>=AY)
      and (X-w<AX) and (Y-h<AY);
  end;
  TSelectObjectBy.VisibleMask:
  begin
    Result := FAppearance.IsMaskedAt(ALevel, AX, AY, [MASK_VISIBLE, MASK_BLOCKED, MASK_ACTIVABLE]);
  end;
  TSelectObjectBy.BlockMask:
  begin
    Result := FAppearance.IsMaskedAt(ALevel, AX, AY, [MASK_BLOCKED, MASK_HIDDEN, MASK_ACTIVABLE, MASK_TRIGGER]);
  end;
  TSelectObjectBy.VisitableMask:
  begin
    Result := FAppearance.IsVisitableAt(ALevel, AX, AY);
  end;

  end;

end;

function TMapObject.IsVisitableAt(ALevel, AX, AY: Integer): boolean;
begin
  Result := FAppearance.IsVisitableAt(ALevel, AX, AY);
end;

constructor TMapObject.Create(ACollection: TCollection);
begin
  FLastFrame := 0;
  FOptions := TObjectOptions.Create(Self);
  FPlayer:=TPlayer.none;
  FPosition := TPosition.Create;
  FModUsage := TModUsage.Create(nil);
  inherited Create(ACollection);

  FAppearance := TMapObjectAppearance.Create(Self);
end;

constructor TMapObject.CreateIndep(AMap: TVCMIMap);
begin
  FMap := AMap;
  Create(nil);
end;

destructor TMapObject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FModUsage);
  FreeAndNil(FAppearance);
  FreeAndNil(FOptions);
  FreeAndNil(FPosition);
end;

function TMapObject.GetPlayer: TPlayer;
begin
  Result := FPlayer;
end;

function TMapObject.GetIdx: integer;
begin
  Result := Index;
end;

function TMapObject.GetCategory: TObjectCategory;
begin
  if Assigned(FMapObjectGroup) then
  begin
    Result := FMapObjectGroup.Category;
  end
  else
  begin
    Result := TObjectCategory.Other;
  end;
end;

function TMapObject.GetL: integer;
begin
  Result := FPosition.L;
end;

function TMapObject.GetSubtype: AnsiString;
begin
  if Assigned(FMapObjectType) then
    Result := FMapObjectType.Identifier
  else
    Result := '';
end;

function TMapObject.GetType: AnsiString;
begin
  if Assigned(FMapObjectGroup) then
    Result := FMapObjectGroup.Identifier
  else
    Result := '';
end;

function TMapObject.GetX: integer;
begin
  Result := FPosition.X;
end;

function TMapObject.GetY: integer;
begin
  Result := FPosition.Y;
end;

function TMapObject.HasOptions: boolean;
begin
  Result := Assigned(FOptions) and (FOptions.ClassType <> TObjectOptions);
end;

procedure TMapObject.AddToMask(var Activity: TPassabilityMask; var Blockage: TPassabilityMask; const ARegion: TMapRect);
var
  rx, ry, mx, my, cx, cy, shift: Integer;
  line: String;

  own_region: TMapRect;
begin
  own_region := GetRegion().Intersect(ARegion);

  for ry := own_region.Top() to own_region.Bottom() do
  begin
    my := ry - Y + FAppearance.Height - 1;
    cy := ry - ARegion.Top();

    line := FAppearance.Mask[my];

    shift := FAppearance.Width - Length(line);

    for rx := own_region.Left() to own_region.Right() do
    begin
      mx := rx - X + FAppearance.Width - shift;
      cx := rx - ARegion.Left();
      case line[mx] of
        MASK_ACTIVABLE, MASK_TRIGGER:
        begin
          Activity[cx, cy] := true;
          Blockage[cx, cy] := true;
        end;
        MASK_BLOCKED, MASK_HIDDEN:
        begin
          Blockage[cx, cy] := true;
        end;
      end;
    end;
  end;
end;

procedure TMapObject.Render(AState: TLocalState; Frame: integer; Ax, Ay: integer);
var
  owner : TPlayer;
begin
  if HasOptions and Options.CanBeOwned then
    AState.SetUseFlag(true)
  else
    AState.SetUseFlag(false);

  if Assigned(FMapObjectGroup) and FMapObjectGroup.IsHero and (FAppearance.EditorAnimation = '') then
  begin
    Frame := 2;
  end;

  owner := GetPlayer;
  Template.Def.RenderO(AState, Frame, Ax, Ay, owner);

  if (owner <> TPlayer.none) and FMapObjectGroup.IsHeroLike then
  begin
    GetMap.GraphicsManager.GetHeroFlagDef(owner).RenderO(AState, 0, Ax, Ay);//todo: refactor
  end;
end;

procedure TMapObject.RenderAnim(AState: TLocalState; ANextFrame: Boolean);
begin
  if ANextFrame then
  begin
    Inc(FLastFrame);

    if FLastFrame >= Template.Def.FrameCount then
      FLastFrame := 0;
  end;

  Render(AState, FLastFrame,(x+1)*TILE_SIZE,(y+1)*TILE_SIZE);
end;

procedure TMapObject.RenderIcon(AState: TLocalState; AX, AY, dim: integer);
var
  owner : TPlayer;
  Frame: Integer;
begin
  AState.SetTranslation(Ax, Ay);
  Frame := 0;
  if Assigned(FMapObjectGroup) and FMapObjectGroup.IsHero and (FAppearance.EditorAnimation = '') then
  begin
    Frame := 2;
  end;

  owner := GetPlayer;
  Template.Def.RenderIcon(AState, Frame, dim, owner);

  if Assigned(FMapObjectGroup) and (owner <> TPlayer.none) and FMapObjectGroup.IsHeroLike then
  begin
    GetMap.GraphicsManager.GetHeroFlagDef(owner).RenderOverlayIcon(AState, dim, Template.Def.GetSpriteHeight(Frame));//todo: refactor
  end;
end;

procedure TMapObject.RenderOverlay(AState: TLocalState; X, Y: integer);
begin
  Template.RenderOverlay(AState, X, Y);
end;

procedure TMapObject.RenderSelectionRect(AState: TLocalState);
begin
  Template.Def.RenderBorder(AState,X,Y);
end;

procedure TMapObject.RenderStatic(AState: TLocalState);
begin
  Render(AState, FLastFrame, (x+1)*TILE_SIZE,(y+1)*TILE_SIZE);
end;

procedure TMapObject.RenderStatic(AState: TLocalState; X, Y: integer);
begin
  Render(AState, FLastFrame, x,y);
end;

procedure TMapObject.SetL(AValue: integer);
begin
  FPosition.L := AValue;
end;

procedure TMapObject.SetSubtype(AValue: AnsiString);
var
  old_subtype: AnsiString;
begin
  old_subtype:=GetSubtype;

  if old_subtype=AValue then Exit;

  Assert(Assigned(FMapObjectGroup));

  NotifyReferenced(old_subtype,AValue);

  FMapObjectType := FMapObjectGroup.Types.FindItem(AValue);

  RecreateOptions;
end;

procedure TMapObject.SetType(AValue: AnsiString);
var
  old_type: AnsiString;
begin
  old_type:=GetType;

  if old_type=AValue then Exit;

  NotifyReferenced(old_type,AValue);

  FMapObjectGroup := GetMap.ObjectsManager.MapObjectGroups.FindItem(AValue);
  FMapObjectType := nil; //???

  RecreateOptions;

  UpdateIdentifier;
end;

procedure TMapObject.SetX(AValue: integer);
begin
  FPosition.X := AValue;
end;

procedure TMapObject.SetY(AValue: integer);
begin
  FPosition.Y := AValue;
end;

procedure TMapObject.UpdateIdentifier;
begin
  if(GetType <> '') and (Identifier = '') and (Assigned(Collection)) then
  begin
    Identifier := StripScope(GetType)+'_'+IntToStr((Collection as TMapObjects).GenerateIdentifier());
  end;
end;

procedure TMapObject.RecreateOptions;
begin
  FreeAndNil(FOptions);

  if (GetType <> '') and (GetSubtype <> '') then
    FOptions := TObjectOptions.CreateByID(GetType, GetSubtype,Self)
  else
    FOptions := TObjectOptions.Create(Self);
end;

procedure TMapObject.SetCollection(Value: TCollection);
begin
  //notify old collection
  if Assigned(Collection) then
  begin
    GetMap.ModUsage.RemoveFrom(FModUsage);
  end;

  //notify new collection before inherited call because of hero pool
  if Assigned(Value) then
  begin
    TMapObjects(Value).Map.ModUsage.AddFrom(FModUsage);
  end;

  inherited SetCollection(Value);

  if Assigned(Collection) then
  begin
    UpdateIdentifier;
  end;
end;

function TMapObject.GetDisplayName: string;
begin
  Result:=FormatDisplayName('');
end;

function TMapObject.GetMap: TVCMIMap;
begin
  if Assigned(Collection) then
  begin
    Result := (Collection as TMapObjects).Map;
  end
  else
  begin
    Result := FMap;
  end;
end;

procedure TMapObject.AssignTemplate(ATemplate: TMapObjectTemplate);
begin
  Template.Assign(ATemplate);
  &Type:=ATemplate.MapObjectGroup.Identifier;
  Subtype := ATemplate.MapObjectType.Identifier;

  FOptions.AssignTemplate(ATemplate);
  FAppearanceValid:=True;//suppress invalidation
end;

function TMapObject.FormatDisplayName(ACustomName: TLocalizedString
  ): TLocalizedString;
begin
  if ACustomName = '' then
    Result:=Format('%s::%s @ %d %d %d',[&type, subtype,X,Y,L])
  else
    Result:=Format('%s @ %d %d %d',[ACustomName,X,Y,L]);
end;

function TMapObject.GetRegion: TMapRect;
begin
  Result := GetRegion(x,y);
end;

function TMapObject.GetRegion(AX, AY: integer): TMapRect;
begin
  Result.Create();
  Result.FTopLeft.X:=AX+1-FAppearance.Width;
  Result.FTopLeft.Y:=AY+1-FAppearance.Height;
  Result.FWidth:=FAppearance.Width;
  Result.FHeight:=FAppearance.Height;
end;

function TMapObject.EqualPosition(APosition: TPosition): Boolean;
begin
  Result := FPosition.Equals(APosition);
end;

procedure TMapObject.SetPlayer(AValue: TPlayer);
var
  old: TPlayer;
begin
  if FPlayer = AValue then
    Exit;
  old := FPlayer;
  FPlayer:=AValue;

  if Assigned(Collection) then
  begin
    GetMap.NotifyOwnerChanged(self, old, FPlayer);

    if GetMap.TrackObjectChanges then
    begin
      GetMap.MapLevels[L].ObjectChanged(Self);
    end;
  end;
end;

procedure TMapObject.NotifyHeroTypeChanged(AOldType, ANewType: AnsiString);
begin
  GetMap.NotifyHeroTypeChanged(Self, AOldType, ANewType);
end;

procedure TMapObject.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString
  );
begin
  FModUsage.NotifyReferenced(AOldIdentifier, ANewIdentifier);

  if Assigned(Collection) then
  begin
    GetMap.NotifyReferenced(AOldIdentifier, ANewIdentifier);
  end;
end;

procedure TMapObject.InvalidateAppearance;
begin
  FAppearanceValid:=false;
end;

procedure TMapObject.ValidateAppearance;
var
  new_template: TMapObjectTemplate;
begin
  if not FAppearanceValid then
  begin
    //TODO:ValidateAppearance

    if Assigned(FOptions) and Assigned(FMapObjectGroup) and Assigned(FMapObjectType) then
    begin
      new_template := FOptions.SelectTemplate(FMapObjectType);

      if Assigned(new_template) then
      begin
        FAppearance.Assign(new_template);
      end;
    end;

    FAppearanceValid := true;
  end;
end;

function TMapObject.GetListsManager: TListsManager;
begin
  Result := GetMap.ListsManager;
end;

procedure TMapObject.GetKeyWords(ATarget: TStrings);
begin
  //TODO: add keywords based on options

  if Assigned(FMapObjectType) then
  begin
    FMapObjectType.GetKeyWords(ATarget);
  end;

  if Assigned(FAppearance) then
  begin
    FAppearance.GetKeyWords(ATarget);
  end;
end;

function TMapObject.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
var
  opt_json: TJSONData;
  res: TJSONObject;
begin
  res := AHandler.ObjectToJson(Self);

  res.Add('type', &Type);
  res.Add('subtype', Subtype);

  if HasOptions then
  begin
    opt_json := AHandler.ObjectToJsonEx(Options);

    //store only not empty options
    if opt_json.Count > 0 then
    begin
      res.Add('options', opt_json);
    end
    else
    begin
      FreeAndNil(opt_json);
    end;
  end;

  Result := res;
end;

procedure TMapObject.Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
var
  ASrcObj: TJSONObject;
begin
  ASrcObj := ASrc as TJSONObject;

  //type must be loaded first
  //subtype must be loaded right after type
  &Type:=ASrcObj.Strings['type'];

  if ASrcObj.IndexOfName('subtype') >= 0 then
  begin
    Subtype:=ASrcObj.Strings['subtype'];
  end
  else
  begin
    Subtype := '';
  end;

  AHandler.JSONToObject(ASrc as TJSONObject, Self);

  //destream Options after all other properties
  if (ASrcObj.IndexOfName('options') >= 0) and (ASrcObj.Types['options']=jtObject) then
  begin
    AHandler.JSONToObjectEx(TJSONObject(ASrc).Objects['options'], Options);
  end;
  ValidateAppearance;
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
end;

procedure TMapObjects.ItemRemoved(Item: TCollectionItem);
begin
  inherited ItemRemoved(Item);
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

  FCanPlay:=TPlayableBy.PlayerOrAI;
  FMainTown := TMainTownInfo.Create;
end;

destructor TPlayerInfo.Destroy;
begin
  FMainTown.Free;
  FTowns.Free;
  FHeroes.Free;
  FAllowedFactions.Free;
  inherited Destroy;
end;

procedure TPlayerInfo.TownAdded(AObject: TMapObject);
var
  player_town: TPlayerTown;
begin
  player_town := Towns.Add;
  player_town.MapObject := AObject;

  if MainTown.MapObject = nil then
  begin
    MainTown.MapObject :=AObject;
  end;
end;

procedure TPlayerInfo.TownRemoved(AObject: TMapObject);
begin
  Towns.Remove(AObject.Identifier);

  if MainTown.MapObject = AObject then
  begin
    MainTown.MapObject:=nil;

    if Towns.Count > 0 then
    begin
      MainTown.MapObject:= Towns.Items[0].MapObject;
    end;
  end;
end;

procedure TPlayerInfo.HeroAdded(AObject: TMapObject);
var
  player_hero: TPlayerHero;
begin
  player_hero := Heroes.Add;
  player_hero.MapObject := AObject;

  if MainHero = '' then
  begin
    SetMainHero(AObject);
  end;
end;

procedure TPlayerInfo.HeroRemoved(AObject: TMapObject);
begin
  Heroes.Remove(AObject.Identifier);

  if MainHero = AObject.Identifier then
  begin
    MainHero:='';

    if Heroes.Count > 0 then
    begin
      MainHero:=Heroes.Items[0].Identifier;
    end;
  end;
end;

procedure TPlayerInfo.SetMainHero(AObject: TMapObject);
begin
  if Assigned(AObject) then
    FMainHero := AObject.Identifier
  else
    FMainHero:='';
end;

procedure TPlayerInfo.Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
begin
  AHandler.JSONToObject(ASrc as TJSONObject, Self);
  // heroes are ignored, they will be refilled later
end;

function TPlayerInfo.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
begin
  Result := AHandler.ObjectToJSON(Self);

  TJSONObject(Result).Add('heroes', AHandler.StreamCollection(Heroes));
end;

procedure TPlayerInfo.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin

end;

function TPlayerInfo.HasMainTown: boolean;
begin
  Result := Assigned(FMainTown.MapObject);
end;

function TPlayerInfo.IsAllowedFactionsStored: Boolean;
begin
  Result := not FAllowedFactions.IsEmpty;
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

constructor TMapTile.Create(ATerType: TTerrainType; ATerSubtype: UInt8);
begin
  FTerType := ATerType;
  FTerSubtype := ATerSubtype;

  FRiverType:=TRiverType.noRiver;
  FRiverDir:=0;
  FRoadType:=TRoadType.noRoad;
  FRoadDir:=0;
  FFlags:=0;
  FActiveCount:=0;;
  FBlockingCount := 0;
  FFlaggableID := -1;
  FOwner:=TPlayer.none;
end;

procedure TMapTile.SetRiver(ARiverType: TRiverType; ARriverDir: UInt8; AMir: Uint8);
begin
  FRiverType:= ARiverType;
  FRiverDir:=ARriverDir;
  FFlags := (FFlags and $F3) or (AMir shl 2);
end;

procedure TMapTile.SetRoad(ARoadType: TRoadType; ARoadDir: UInt8; AMir: Uint8);
begin
  FRoadType:= ARoadType;
  FRoadDir:=ARoadDir;
  FFlags := (FFlags and $CF) or (AMir shl 4);
end;

procedure TMapTile.SetTerrain(TT: TTerrainType; TS: UInt8; AMir: UInt8);
begin
  FTerType := TT;
  FTerSubtype := TS;
  FFlags := (FFlags and $FC) or (AMir and 3);
end;

procedure TMapTile.BlockingObjectAdded(AObject: TMapObject; Active, Blocked: Boolean);
begin
  if Active then
    inc(FActiveCount);
  if Blocked then
    inc(FBlockingCount);

  if (FFlaggableID = -1) and (AObject.Options.CanBeOwned) then
  begin
    FFlaggableID:=AObject.ID;
    FOwner:=AObject.GetPlayer;
  end;
end;

procedure TMapTile.BlockingObjectRemoved(AObject: TMapObject; Active, Blocked: Boolean);
begin
  if FFlaggableID = AObject.ID then
  begin
    FOwner:=TPlayer.none;
    FFlaggableID := -1;
  end;
  if Active then
    dec(FActiveCount);
  if Blocked then
    dec(FBlockingCount);
end;

function TMapTile.IsBlocked: boolean;
begin
  Result := FBlockingCount > 0;
end;

function TMapTile.IsActive: boolean;
begin
  Result := FActiveCount > 0;
end;

{ TMapLevel }

destructor TMapLevel.Destroy;
begin
  inherited Destroy;
end;

procedure TMapLevel.SetRoad(x, y: integer; ARoadType: TRoadType; ARoadDir: UInt8; AMir: Uint8);
begin
  Tile[x,y]^.SetRoad(ARoadType, ARoadDir, AMir);
end;

procedure TMapLevel.SetRiver(x, y: integer; ARiverType: TRiverType; ARriverDir: UInt8; AMir: Uint8);
begin
  Tile[x,y]^.SetRiver(ARiverType, ARriverDir, AMir);
end;

procedure TMapLevel.SetTerrain(X, Y: Integer; TT: TTerrainType; TS: UInt8; mir: UInt8);
begin
  GetTile(X,Y)^.SetTerrain(tt, ts, mir);
end;

procedure TMapLevel.ObjectAdded(AObject: TMapObject);
begin
  AObject.Template.OnOwnerChanged(FTiles, TFPObservedOperation.ooAddItem);
end;

procedure TMapLevel.ObjectChanged(AObject: TMapObject);
begin
  AObject.Template.OnOwnerChanged(FTiles, TFPObservedOperation.ooChange);
end;

procedure TMapLevel.ObjectRemoved(AObject: TMapObject);
begin
  AObject.Template.OnOwnerChanged(FTiles, TFPObservedOperation.ooDeleteItem);
end;

function TMapLevel.GetDimentions: TMapRect;
begin
  Result.Create;
  Result.FWidth:=Width;
  Result.FHeight:=Height;
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

  t: PMapTile;
begin
  Assert(Height>=0, 'Invalid height');
  Assert(Width>=0, 'Invalid width');
  if (Height=0) or (Width=0) or (Index <0) then Exit;

  tt := Map.TerrainManager.GetDefaultTerrain(Index);

  SetLength(FTiles,FWidth);
  for X := 0 to FWidth - 1 do
  begin
    SetLength(FTiles[X],FHeight);
    for Y := 0 to FHeight - 1 do
    begin
      t := @FTiles[X][Y];
      t^.Create(tt, Map.TerrainManager.GetRandomNormalSubtype(tt));
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

procedure TVCMIMap.MapObjectAdded(AObject: TMapObject);
begin
  if FTrackObjectChanges then
    FLevels[AObject.L].ObjectAdded(AObject);

  NotifyOwnerChanged(AObject, TPlayer.none, AObject.GetPlayer());

  if (AObject.&Type = TYPE_HERO)
    or (AObject.&Type = TYPE_RANDOMHERO)
    or (AObject.&Type = TYPE_PRISON)then
  begin
    FHeroPool.HeroAdded(AObject);
  end;

  if FTrackObjectChanges then
    AddToIndex(AObject);
end;

procedure TVCMIMap.MapObjectRemoved(AObject: TMapObject);
begin
  if FTrackObjectChanges then
    FLevels[AObject.L].ObjectRemoved(AObject);

  NotifyOwnerChanged(AObject, AObject.GetPlayer(),  TPlayer.none);

  if (AObject.&Type = TYPE_HERO)
    or (AObject.&Type = TYPE_RANDOMHERO)
    or (AObject.&Type = TYPE_PRISON)then
  begin
    FHeroPool.HeroRemoved(AObject);
  end;
  RemoveFromIndex(AObject)
end;

constructor TVCMIMap.Create(env: TMapEnvironment; Params: TMapCreateParams);
var
  lvl: TMapLevel;
begin
  CreateEmpty(env);

  TriggeredEvents.AddStandardDefeat();
  TriggeredEvents.AddStandardVictory();

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

  Loaded;
end;


constructor TVCMIMap.CreateEmpty(env: TMapEnvironment);
var
  index: TObjectCategory;
begin
  FTrackObjectChanges := false;
  FTerrainManager := env.tm;
  FListsManager := env.lm;
  FGraphicsManager := env.gm;
  FObjectsManager := env.om;

  FLevels := TMapLevels.Create(Self);

  CurrentLevelIndex := -1;

  Name := rsDefaultMapName;

  FDifficulty := TDifficulty.NORMAL;

  FAllowedAbilities := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedAbilities);

  FAllowedSpells := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedSpells);

  FAllowedArtifacts := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedArtifacts);

  FAllowedHeroes := TLogicalIDCondition.Create(Self);
  AttachTo(FAllowedHeroes);

  FPlayers := TPlayerInfos.Create(Self);
  FTeams := TTeamSettings.Create(Self);
  FObjects := TMapObjects.Create(Self);
  AttachTo(FObjects);

  FRumors := TRumors.Create;
  AttachTo(FRumors);

  FLevelLimit:=MAX_HERO_LEVEL;
  FPredefinedHeroes := THeroDefinitions.Create(Self);
  AttachTo(FPredefinedHeroes);

  FTriggeredEvents := TTriggeredEvents.Create(env.i18n);
  AttachTo(FTriggeredEvents);

  FMods := TLogicalIDCondition.Create(nil);//pass nil owner to avoid circular reference notification

  FModUsage := TModUsage.Create(FMods);

  FVisibleObjectsQueue := TMapObjectQueue.Create;

  FHeroPool := THeroClassPool.Create(env.lm.HeroClassInfos, env.lm.HeroInfos);

  for index in TObjectCategory do
    FSearchIndexes[index] := TSearchIndex.Create();

  FIsDirty := False;
end;

destructor TVCMIMap.Destroy;
var
  index: TSearchIndex;
begin
  for index in FSearchIndexes do
    index.Free;

  FHeroPool.Free;
  FTrackObjectChanges := false;
  FVisibleObjectsQueue.Free;

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
       CurrentLevel.SetTerrain(x,y, tt, TerrainManager.GetRandomNormalSubtype(tt), 0);
    end;
  end;
end;

procedure TVCMIMap.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  o : TMapObject;
begin
  FIsDirty := true;

  if (ASender = FObjects) then
  begin
    o := TMapObject(Data);
    case Operation of
      ooAddItem:
      begin
        MapObjectAdded(o);
      end;
      ooDeleteItem:
      begin
        MapObjectRemoved(o);
      end;
    end;
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

procedure TVCMIMap.RenderTerrain(AState: TLocalState; Left, Right, Top, Bottom: Integer);
var
  i: Integer;
  j, x, y: Integer;
  t: PMapTile;
begin
  AState.SetFlagColor(NEUTRAL_PLAYER_COLOR);
  Right := Min(Right, CurrentLevel.Width - 1);
  Bottom := Min(Bottom, CurrentLevel.Height - 1);

  for i := Left to Right do
  begin
    for j := Top to Bottom do
    begin
      t := GetTile(FCurrentLevel,i,j);


      x := i * TILE_SIZE;
      y := j * TILE_SIZE;

      AState.SetTranslation(x,y);

      FTerrainManager.Render(AState, t^.TerType, t^.TerSubType, t^.Flags);
      FTerrainManager.RenderRiver(AState, t^.RiverType, t^.RiverDir, t^.Flags);
    end;
  end;

  Top := Max(0,Top-1);

  for i := Left to Right do
  begin
    for j := Top to Bottom do
    begin

      x := i * TILE_SIZE;
      y := j * TILE_SIZE + TILE_SIZE div 2;

      AState.SetTranslation(x,y);
      t := GetTile(FCurrentLevel,i,j);
      FTerrainManager.RenderRoad(AState, t^.RoadType, t^.RoadDir, t^.Flags);
    end;
  end;
end;

procedure TVCMIMap.RenderPassability(AState: TLocalState; Left, Right, Top, Bottom: Integer);
begin

end;

procedure TVCMIMap.SelectVisibleObjects(ATarget: TVisibleObjects; Left, Right, Top, Bottom: Integer);
var
  r:TMapRect;
begin
  r.SetFromCorners(Left, Top, Right, Bottom);
  SelectVisibleObjects(ATarget, r);
end;

procedure TVCMIMap.SelectVisibleObjects(ATarget: TVisibleObjects; const ARect: TMapRect);
var
  i: Integer;
  o: TMapObject;
begin
  ATarget.Clear;

  for i := 0 to FObjects.Count - 1 do
  begin
    o := FObjects.Items[i];
    if o.L <> CurrentLevelIndex then
      Continue;

    if o.GetRegion().Intersect(ARect).IsEmpty()
      then Continue; //todo: use visisblity mask

    FVisibleObjectsQueue.Push(o);
  end;

  while not FVisibleObjectsQueue.IsEmpty do
  begin
    ATarget.SetRegion(ARect);
    ATarget.Add(FVisibleObjectsQueue.Top);

    FVisibleObjectsQueue.Pop;
  end;
end;

procedure TVCMIMap.SelectByKeywords(ATarget: TMapObjectsSelection; AInput: string; ACategory: TObjectCategory);
begin
  FSearchIndexes[ACategory].Find(AInput, ATarget);
end;

procedure TVCMIMap.SaveToStream(ADest: TStream; AWriter: IMapWriter);
begin
  AWriter.Write(ADest,Self);
  FIsDirty := False;
end;

class procedure TVCMIMap.SelectObjectsOnTile(ASource: TMapObjectList; Level, X, Y: Integer; ATarget: TMapObjectQueue);
var
  o: TMapObject;
  i: Integer;
begin
  for i := 0 to ASource.Count - 1 do
  begin
    o := ASource[i];

    if o.CoversTile(Level,x,y, TSelectObjectBy.BBox) then
    begin
      ATarget.Push(o);
    end;
  end;
end;

class procedure TVCMIMap.SelectObjectsOnTile(ASource: TMapObjectList; Level, X, Y: Integer; ATarget: TMapObjectSet;
  ASelectBy: TSelectObjectBy; ASelectFilter: TSelectFilter);
var
  AObject: TMapObject;
  i: Integer;

  add: Boolean;
begin
  for i := 0 to ASource.Count - 1 do
  begin
    add := false;
    AObject := ASource[i];

    if TSelectTarget.StaticObjects in ASelectFilter then
    begin
      if not Assigned(AObject.Template) then
        add := true
      else if Assigned(AObject.Template) and not AObject.Template.Visitable then
        add := true;
    end;

    if TSelectTarget.InteractiveObjects in ASelectFilter then
    begin
      if Assigned(AObject.Template) and AObject.Template.Visitable then
        add := true;
    end;

    if add and AObject.CoversTile(Level,x,y, ASelectBy) then
    begin
      ATarget.Insert(AObject);
    end;
  end;
end;

procedure TVCMIMap.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
begin
  FModUsage.NotifyReferenced(AOldIdentifier, ANewIdentifier);
end;

procedure TVCMIMap.NotifyOwnerChanged(AObject: TMapObject; AOldOwner, ANewOwner: TPlayer);
var
  opt: TPlayerInfo;
begin
  if AOldOwner <> TPlayer.none then
  begin
    if AOldOwner in [TPlayer.red..TPlayer.pink] then
    begin
      opt := FPlayers.GetPlayerInfo(Integer(AOldOwner));

      case AObject.&Type of
        TYPE_HERO, TYPE_RANDOMHERO:
          opt.HeroRemoved(AObject);
        TYPE_TOWN, TYPE_RANDOMTOWN:
          opt.TownRemoved(AObject);
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
          opt.HeroAdded(AObject);
        end;
        TYPE_TOWN, TYPE_RANDOMTOWN:
        begin
          opt.TownAdded(AObject);
        end;
      end;
    end
    else begin
      DebugLn(['invalid player color', Integer(ANewOwner)]);
    end;
  end;
end;

procedure TVCMIMap.NotifyHeroTypeChanged(AObject: TMapObject; AOldType, ANewType: AnsiString);
begin
  FHeroPool.HeroChanged(AObject, AOldType, ANewType);
end;

procedure TVCMIMap.Loaded;
var
  i: Integer;
  o: TMapObject;
begin
  for i  := 0 to FObjects.Count  - 1 do
  begin
    o := FObjects[i];
    FLevels[o.L].ObjectAdded(o);
  end;

  BuildSearchIndex;

  FTrackObjectChanges := True;
  FIsDirty := False;
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

function TVCMIMap.GetCurrentLevelDimensions: TMapRect;
begin
  Result := CurrentLevel.GetDimentions;
end;

function TVCMIMap.FillWithAvilableClasses(AItems: TStrings; ASelected: AnsiString): integer;
var
  i: Integer;
  pool: THeroPool;
begin
  if ASelected = '' then
    raise Exception.Create('FillWithAvilableClasses: no class selected');

  Result := -1;
  AItems.Clear;

  for i := 0 to FHeroPool.Count - 1 do
  begin
    pool := FHeroPool.Items[i];

    if ASelected = pool.Identifier then
    begin
      Result := AItems.Count;
      AItems.AddObject(pool.HeroClassInfo.Name, pool.HeroClassInfo);
    end
    else if not pool.IsEmpty then
    begin
      AItems.AddObject(pool.HeroClassInfo.Name, pool.HeroClassInfo);
    end;
  end;

  if Result = -1 then
    raise Exception.CreateFmt('FillWithAvilableClasses: class %s not found ',[ASelected]);
end;

function TVCMIMap.FillWithAvilableHeroes(AItems: TStrings; AHeroClass: AnsiString; ASelected: THeroInfo): integer;
var
  pool: THeroPool;
begin
  pool := FHeroPool.FindItem(AHeroClass);

  if not Assigned(pool) then
    raise Exception.CreateFmt('FillWithAvilableHeroes: class %s not found in pool',[AHeroClass]);

  if ASelected.&Class <> AHeroClass then
  begin
    ASelected := nil;
  end;

  result := pool.FillWithAvilableHeroes(AItems, ASelected);
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
end;

procedure TVCMIMap.SetDifficulty(AValue: TDifficulty);
begin
  if FDifficulty = AValue then Exit;
  FDifficulty := AValue;
end;

procedure TVCMIMap.SetLevelLimit(AValue: Integer);
begin
  if FLevelLimit = AValue then Exit;
  FLevelLimit := AValue;
  if FLevelLimit = 0 then
  begin
    FLevelLimit := MAX_HERO_LEVEL;
  end;
end;

procedure TVCMIMap.SetName(AValue: TLocalizedString);
begin
  FName := AValue;
end;

procedure TVCMIMap.SetIsDirty(AValue: Boolean);
begin
  FIsDirty:=AValue;
end;

procedure TVCMIMap.AddToIndex(AObject: TMapObject);
var
  keywords: TStringList;
begin
  keywords := TStringList.Create;
  keywords.Sorted:=false;
  keywords.Duplicates:=dupAccept;
  try
    AObject.GetKeyWords(keywords);
    FSearchIndexes[AObject.Category].AddToIndex(keywords, AObject);
  finally
    keywords.Free;
  end;
end;

procedure TVCMIMap.RemoveFromIndex(AObject: TMapObject);
begin
  FSearchIndexes[AObject.Category].RemoveFromIndex(AObject);
end;

procedure TVCMIMap.BuildSearchIndex;
var
  i: Integer;
begin
  for i := 0 to FObjects.Count - 1 do
  begin
    AddToIndex(FObjects[i]);
  end;
end;

procedure TVCMIMap.SetVictoryString(AValue: TLocalizedString);
begin
  FVictoryString:=AValue;
end;

procedure TVCMIMap.SetDefeatString(AValue: TLocalizedString);
begin
  FDefeatString:=AValue;
end;

procedure TVCMIMap.SetDefeatIconIndex(AValue: Integer);
begin
  FDefeatIconIndex:=AValue;
end;

function TVCMIMap.IsRumorsStored: Boolean;
begin
  Result := not FRumors.IsEmpty;
end;

function TVCMIMap.IsPredefinedHeroesStored: Boolean;
begin
  Result := FPredefinedHeroes.Count > 0;
end;

function TVCMIMap.IsTeamsStored: Boolean;
begin
  Result := FTeams.Count > 0;
end;

procedure TVCMIMap.SetVictoryIconIndex(AValue: Integer);
begin
  FVictoryIconIndex:=AValue;
end;

end.

