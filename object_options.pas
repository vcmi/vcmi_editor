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

unit object_options;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, math, typinfo, fpjson,

  LazLoggerBase,

  editor_types, editor_classes,
  editor_utils, logical_id_condition, vcmi_json, editor_consts, map_objects, lists_manager;

type

{$push}
{$m+}
  TObjectOptions      = class;
  TObjectOptionsClass = class of TObjectOptions;
  IObjectOptionsVisitor = interface;

  { IMapObject }

  IMapObject = interface(IReferenceNotify)
    function GetType: AnsiString;
    function GetSubtype: AnsiString;

    procedure SetPlayer(AValue: TPlayer);
    function GetPlayer: TPlayer;

    //for hero pool management
    procedure NotifyHeroTypeChanged(AOldType, ANewType: AnsiString);

    //for filtered appearance
    procedure InvalidateAppearance();

    function GetListsManager: TListsManager;
  end;

  { TObjectOptions }

  TObjectOptions = class(TObject, ISerializeNotify)
  private
    FObject: IMapObject;
    function GetOwner: TPlayer;
    procedure SetOwner(AValue: TPlayer);
  public//ISerializeNotify
    procedure BeforeDeSerialize(Sender: TObject; AData: TJSONData); virtual;
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData); virtual;
    procedure BeforeSerialize(Sender: TObject); virtual;
    procedure AfterSerialize(Sender: TObject; AData: TJSONData); virtual;
  public
    constructor Create(AObject: IMapObject); virtual;

    procedure Clear; virtual;

    procedure ApplyVisitor({%H-}AVisitor: IObjectOptionsVisitor); virtual;

    property MapObject: IMapObject read FObject;

    class function CanBeOwned: Boolean; virtual;
    class function MustBeOwned: Boolean; virtual;

    class function GetClassByID(ID: AnsiString; SubID: AnsiString): TObjectOptionsClass;

    class function CreateByID(ID: AnsiString; SubID: AnsiString; AObject: IMapObject): TObjectOptions;

    class function ZIndex: Integer; virtual;

    //initialize options with external template
    procedure AssignTemplate(ATemplate: TMapObjectTemplate); virtual;

    //return template with best fit on current configuration, may return nil
    function SelectTemplate(AType: TMapObjectType): TMapObjectTemplate; virtual;
  public
    property Owner: TPlayer read GetOwner write SetOwner;
  end;
{$pop}

  { TBaseCreatureInstInfo }

  TBaseCreatureInstInfo = class (TCollectionItem)
  private
    FAmount: Integer;
    FType: AnsiString;
    procedure SetAmount(AValue: Integer);
    procedure SetType(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    function IsEmpty: Boolean; virtual;
  published
    property &type: AnsiString read FType write SetType;
    property Amount: Integer read FAmount write SetAmount default 0;
  end;

  { TCreatureInstInfo }

  TCreatureInstInfo = class(TBaseCreatureInstInfo)
  private
    FLevel: Integer;
    FUpgraded: Boolean;
    function GetRawRandom: integer;
    procedure SetRawRandom(AValue: integer);
  public
    property RawRandom: integer Read GetRawRandom write SetRawRandom;
    function IsEmpty: Boolean; override;
  published
    property Level: Integer read FLevel write FLevel default 0;
    property Upgraded: Boolean Read FUpgraded write FUpgraded default false;
  end;

  { TCreatureSet }

  TCreatureSet = class (specialize TGArrayCollection<TCreatureInstInfo>)
  private
    FOwner: IMapObject;
    FTightFormation: Boolean;
    procedure SetTightFormation(AValue: Boolean);
  public
    constructor Create(AOwner: IMapObject);
    procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);

    function IsEmpty: Boolean;

    property TightFormation:Boolean read FTightFormation write SetTightFormation;
  end;

  {$push}
  {$m+}

  { TResourceSet }

  TResourceSet = class abstract
  protected
    function GetAmount(AType: TResType): integer; virtual; abstract;
    procedure SetAmount(AType: TResType; AValue: integer); virtual; abstract;
  public
    property Amount[AType: TResType]: integer read GetAmount write SetAmount;
    function IsEmpty: Boolean; virtual;
    procedure Clear; virtual;
  published
    property Wood: integer index TResType.wood read GetAmount write SetAmount default 0;
    property Mercury: integer index TResType.mercury read GetAmount write SetAmount default 0;
    property Ore: integer index TResType.ore read GetAmount write SetAmount default 0;
    property Sulfur: integer index TResType.sulfur read GetAmount write SetAmount default 0;
    property Crystal: integer index TResType.crystal read GetAmount write SetAmount default 0;
    property Gems: integer index TResType.gems read GetAmount write SetAmount default 0;
    property Gold: integer index TResType.gold read GetAmount write SetAmount default 0;
    property Mithril: integer index TResType.mithril read GetAmount write SetAmount default 0;
  end;

  { TIndepResourceSet }

  TIndepResourceSet = class(TResourceSet)
  strict private
    Fvalue: array[TResType] of Integer;
  protected
    function GetAmount(AType: TResType): integer; override;
    procedure SetAmount(AType: TResType; AValue: integer); override;
  end;

  { TQuest }

  TQuestMission = (None=0, Level=1, PrimaryStat=2, KillHero=3, KillCreature=4, Artifact=5, Army=6, Resources=7, Hero=8, Player=9{, Keymaster=10});

  TQuest = class
  private
    FOwner: IMapObject;
    FCreatures: TCreatureSet;
    FCompletedText: TLocalizedString;
    FFirstVisitText: TLocalizedString;
    FHeroID: AnsiString;
    FHeroLevel: Integer;
    FKillTarget: String;
    FMissionType: TQuestMission;
    FNextVisitText: TLocalizedString;
    FPlayerID: TPlayer;
    FPrimarySkills: THeroPrimarySkills;
    FResources: TResourceSet;
    FTimeLimit: Integer;
    FArtifacts:TStrings;
    function IsCreaturesStored: Boolean;
    function IsArtifactsStored: Boolean;
    function IsHeroIDStored: Boolean;
    function IsHeroLevelStored: Boolean;
    function IsKillTargetStored: Boolean;
    function IsPlayerIDStored: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsResourcesStored: Boolean;
    procedure SetCompletedText(AValue: TLocalizedString);
    procedure SetFirstVisitText(AValue: TLocalizedString);
    procedure SetHeroID(AValue: AnsiString);
    procedure SetHeroLevel(AValue: Integer);
    procedure SetMissionType(AValue: TQuestMission);
    procedure SetNextVisitText(AValue: TLocalizedString);
    procedure SetPlayerID(AValue: TPlayer);
    procedure SetTimeLimit(AValue: Integer);
  public
    constructor Create(AOwner: IMapObject);
    destructor Destroy; override;

    procedure Clear;

    procedure SetKillTarget(AValue: String);
  published
    property FirstVisitText: TLocalizedString read FFirstVisitText write SetFirstVisitText;
    property NextVisitText: TLocalizedString read FNextVisitText write SetNextVisitText;
    property CompletedText: TLocalizedString read FCompletedText write SetCompletedText;

    property MissionType: TQuestMission read FMissionType write SetMissionType default TQuestMission.None;
    property TimeLimit: Integer read FTimeLimit write SetTimeLimit default -1;

    property Artifacts: TStrings read FArtifacts stored IsArtifactsStored;
    property Creatures: TCreatureSet read FCreatures stored IsCreaturesStored;
    property Resources: TResourceSet read FResources stored IsResourcesStored;
    property PrimarySkills: THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property HeroLevel: Integer read FHeroLevel write SetHeroLevel  stored IsHeroLevelStored default 0;
    property Hero: AnsiString read FHeroID write SetHeroID stored IsHeroIDStored;
    property Player: TPlayer read FPlayerID write SetPlayerID stored IsPlayerIDStored default TPlayer.NONE;
    property KillTarget: String read FKillTarget write SetKillTarget stored IsKillTargetStored;
  end;

  { THeroArtifacts }

  THeroArtifacts = class
  strict private
    FOwner: IReferenceNotify;
    FSlots: array[0..ARTIFACT_SLOT_COUNT-1] of AnsiString;
    FBackpack: TStrings;
    function GetBackpack: TStrings;
    function GetBySlotNumber(ASlotID: Integer): AnsiString;
    function IsBackpackStored: Boolean;
    procedure SetBySlotNumber(ASlotID: Integer; AValue: AnsiString);
  public
    constructor Create(AOwner: IReferenceNotify);
    destructor Destroy; override;

    property BySlotNumber[ASlotID: Integer]: AnsiString read GetBySlotNumber write SetBySlotNumber;
    function IsEmpty: Boolean;
    procedure Clear;
  published
    property Head: AnsiString       index 0 read GetBySlotNumber write SetBySlotNumber;
    property Shoulders: AnsiString  index 1 read GetBySlotNumber write SetBySlotNumber;
    property Neck: AnsiString       index 2 read GetBySlotNumber write SetBySlotNumber;
    property RightHand: AnsiString  index 3 read GetBySlotNumber write SetBySlotNumber;
    property LeftHand: AnsiString   index 4 read GetBySlotNumber write SetBySlotNumber;
    property Torso: AnsiString      index 5 read GetBySlotNumber write SetBySlotNumber;
    property RightRing: AnsiString  index 6 read GetBySlotNumber write SetBySlotNumber;
    property LeftRing: AnsiString   index 7 read GetBySlotNumber write SetBySlotNumber;
    property Feet: AnsiString       index 8 read GetBySlotNumber write SetBySlotNumber;
    property Misc1: AnsiString      index 9 read GetBySlotNumber write SetBySlotNumber;
    property Misc2: AnsiString      index 10 read GetBySlotNumber write SetBySlotNumber;
    property Misc3: AnsiString      index 11 read GetBySlotNumber write SetBySlotNumber;
    property Misc4: AnsiString      index 12 read GetBySlotNumber write SetBySlotNumber;
		property Mach1: AnsiString      index 13 read GetBySlotNumber write SetBySlotNumber;
    property Mach2: AnsiString      index 14 read GetBySlotNumber write SetBySlotNumber;
    property Mach3: AnsiString      index 15 read GetBySlotNumber write SetBySlotNumber;
    property Mach4: AnsiString      index 16 read GetBySlotNumber write SetBySlotNumber;
		property Spellbook: AnsiString  index 17 read GetBySlotNumber write SetBySlotNumber;
    property Misc5: AnsiString      index 18 read GetBySlotNumber write SetBySlotNumber;

    property Backpack: TStrings read GetBackpack stored IsBackpackStored;
  end;

  { TReward }

  TReward = class(TNamedCollectionItem, IEmbeddedValue)
  private
    FValue: Int64;
    function GetMetaclass: TMetaclass;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    property Metaclass: TMetaclass read GetMetaclass;
  published
    property Value: Int64 read FValue write FValue;
  end;

  { TRewards }

  TRewards = class(specialize TGNamedCollection<TReward>)
  public
    type TAllowedRewards = set of TMetaclass;
    const IMPOSSIBLE_REWARDS: TAllowedRewards = [TMetaclass.faction, TMetaclass.hero, TMetaclass.heroClass];
  private
    FOwner: IMapObject;
    FAllowedReward: TAllowedRewards;
    procedure SetAllowedReward(AValue: TAllowedRewards);
  protected
    procedure ItemIdentifierChanged(Item: TCollectionItem; AOldName: String; ANewName: String); override;
  public
    constructor Create(AOwner: IMapObject);
    property AllowedReward:TAllowedRewards read FAllowedReward write SetAllowedReward;
    procedure AddOrUpdateReward(AType: TMetaclass; AIdentifier: AnsiString; AValue: Int64);
    function GetValue(AType: TMetaclass; AIdentifier: AnsiString = ''): Int64;
    property Owner: IMapObject read FOwner;
  end;


  { TRewardResourceSet }

  TRewardResourceSet = class(TResourceSet)
  strict private
    FData: TRewards;
  protected
    function GetAmount(AType: TResType): integer; override;
    procedure SetAmount(AType: TResType; AValue: integer); override;
  public
    constructor Create(AData: TRewards);
  end;

  { TRewardPrimarySkills }

  TRewardPrimarySkills = class(TPrimarySkills)
  private
    FData: TRewards;
  protected
    function GetAttack: Integer; override;
    function GetDefence: Integer; override;
    function GetKnowledge: Integer; override;
    function GetSpellpower: Integer; override;
    procedure SetAttack(AValue: Integer); override;
    procedure SetDefence(AValue: Integer); override;
    procedure SetKnowledge(AValue: Integer); override;
    procedure SetSpellpower(AValue: Integer); override;
  public
    constructor Create(AReward: TRewards);
    function IsDefault: Boolean; override;
    procedure Clear; override;
  published
    property Attack default 0;
    property Defence default 0;
    property Spellpower default 0;
    property Knowledge default 0;
  end;

{$pop}

  { TCustomObjectOptions }

  TCustomObjectOptions = class(TObjectOptions)

  end
  unimplemented;

  { TGuardedObjectOptions }

  TGuardedObjectOptions = class abstract (TObjectOptions)
  private
    FGuardMessage: TLocalizedString;
    FGuards: TCreatureSet;
    function IsGuardsStored: Boolean;
    procedure SetGuardMessage(AValue: TLocalizedString);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    procedure Clear; override;
  published
    property Guards: TCreatureSet read FGuards stored IsGuardsStored;
    property GuardMessage:TLocalizedString read FGuardMessage write SetGuardMessage;
  end;

  { TOwnedObjectOptions }

  TOwnedObjectOptions = class (TObjectOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    class function CanBeOwned: Boolean; override;
    procedure Clear; override;
  published
    property Owner default TPlayer.none;
  end;

  { TOwnedArmedObjectOptions }

  TOwnedArmedObjectOptions = class abstract(TOwnedObjectOptions)
  strict private
    FArmy: TCreatureSet;
    function IsArmyStored: Boolean;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property Army: TCreatureSet read FArmy stored IsArmyStored;
  end;

  { TSignBottleOptions }

  TSignBottleOptions = class(TObjectOptions)
  private
    FText: TLocalizedString;
    procedure SetText(AValue: TLocalizedString);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property Text: TLocalizedString read FText write SetText;
  end;

  { TPandorasOptions }

  TPandorasOptions = class (TGuardedObjectOptions)
  private
    FArtifacts: TStrings;
    FCreatures: TCreatureSet;
    FPrimarySkills: TRewardPrimarySkills;
    FResources: TResourceSet;
    FReward: TRewards;
    FSecondarySkills: THeroSecondarySkills;
    FSpells: TStrings;
    function GetExperience: UInt64;
    function GetLuck: Int32;
    function GetMana: Int32;
    function GetMorale: Int32;
    function IsArtifactsStored: Boolean;
    function IsCreaturesStored: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsResourcesStored: Boolean;
    function IsSecondarySkillsStored: Boolean;
    function IsSpellsStored: Boolean;
    procedure SetExperience(AValue: UInt64);
    procedure SetLuck(AValue: Int32);
    procedure SetMana(AValue: Int32);
    procedure SetMorale(AValue: Int32);
  public//ISerializeNotify
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  public
    procedure BeforeSerialize(Sender:TObject); override;
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);override;

    procedure UpdateRewards;
  published
    property Experience: UInt64 read GetExperience write SetExperience default 0;
    property Mana: Int32 read GetMana write SetMana default 0;
    property Morale: Int32 read GetMorale write SetMorale default 0;
    property Luck: Int32 read GetLuck write SetLuck default 0;

    property Resources: TResourceSet read FResources stored IsResourcesStored;
    property PrimarySkills: TRewardPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;

    //not for interative editing, only for serialization
    property Artifacts: TStrings read FArtifacts stored IsArtifactsStored;
    property Creatures: TCreatureSet read FCreatures stored IsCreaturesStored;
    property SecondarySkills: THeroSecondarySkills read FSecondarySkills stored IsSecondarySkillsStored;
    property Spells: TStrings read FSpells stored IsSpellsStored;

    property Reward: TRewards read FReward stored false;//todo: Pandoras box full reward support in format version 1.1
  end;

  { TLocalEventOptions }

  TLocalEventOptions = class(TPandorasOptions)
  private
    FAIActivable: boolean;
    FAvailableFor: TPlayers;
    FHumanActivable: boolean;
    FRemoveAfterVisit: Boolean;
  public
    constructor Create(AObject: IMapObject); override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property HumanActivable: boolean read FHumanActivable write FHumanActivable default True;
    property AIActivable: boolean read FAIActivable write FAIActivable default False;
    property RemoveAfterVisit: Boolean read FRemoveAfterVisit write FRemoveAfterVisit default False;
    property AvailableFor: TPlayers read FAvailableFor write FAvailableFor default ALL_PLAYERS;
  end;

  { THeroOptions }

  THeroOptions = class abstract (TOwnedArmedObjectOptions, IEditableHeroInfo)
  private
    FArtifacts: THeroArtifacts;
    FBiography: TLocalizedString;
    FExperience: UInt64;
    FType: AnsiString;
    FName: TLocalizedString;
    FPatrolRadius: Integer;
    FPortrait: Int32;
    FPrimarySkills: THeroPrimarySkills;
    FSex: THeroSex;
    FSecondarySkills: THeroSecondarySkills;
    FSpellBook: TStrings;

    function GetTightFormation: Boolean;
    function IsArtifactsStored: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsSecondarySkillsStored: Boolean;
    function IsSpellBookStored: Boolean;
    procedure SetType(AValue: AnsiString);
    procedure SetPatrolRadius(AValue: Integer);
    procedure SetTightFormation(AValue: Boolean);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    procedure AfterSerialize(Sender: TObject; AData: TJSONData); override;
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData); override;

    procedure Clear; override;
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
    property &type: AnsiString read FType write SetType;

    property TightFormation: Boolean read GetTightFormation write SetTightFormation default false;
    property PatrolRadius: Integer read FPatrolRadius write SetPatrolRadius default -1;

    property Artifacts: THeroArtifacts read FArtifacts stored IsArtifactsStored;
    property Biography: TLocalizedString read FBiography write SetBiography;
    property Experience: UInt64 read GetExperience write SetExperience default 0;
    property Name: TLocalizedString read FName write SetName;
    property PrimarySkills:THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property SecondarySkills: THeroSecondarySkills read FSecondarySkills stored IsSecondarySkillsStored;
    property SpellBook: TStrings read FSpellBook stored IsSpellBookStored;
  public //manual streaming
    property Sex: THeroSex read FSex write SetSex;
    //internal icon index
    //serialized as index in VCMI image list or as hero identifier
    property Portrait: Int32 read FPortrait write FPortrait;
  end;

  { TNormalHeroOptions }

  TNormalHeroOptions = class(THeroOptions, IEditableHeroInfo)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    class function MustBeOwned: Boolean; override;
    class function ZIndex: Integer; override;
  end;

  { TRandomHeroOptions }

  TRandomHeroOptions = class(THeroOptions, IEditableHeroInfo)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    class function MustBeOwned: Boolean; override;
    class function ZIndex: Integer; override;
  end;

  { TPrisonOptions }

  TPrisonOptions = class(THeroOptions, IEditableHeroInfo)
  public
    constructor Create(AObject: IMapObject); override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  end;

  { TCreatureOptions }

  TCreatureOptions = class(TObjectOptions)
  private
    FCharacter: TCreatureCharacter;
    FAmount: Integer;
    FNeverFlees: boolean;
    FNoGrowing: boolean;
    FReward: TRewards;
    FRewardArtifact: AnsiString;
    FRewardMessage: TLocalizedString;
    FRewardResources: TResourceSet;
    function IsRewardArtifactStored: Boolean;
    function IsRewardMessageStored: Boolean;
    function IsRewardResourcesStored: Boolean;
    function IsRewardStored: Boolean;
    procedure SetAmount(AValue: Integer);
    procedure SetNeverFlees(AValue: boolean);
    procedure SetNoGrowing(AValue: boolean);
    procedure SetRewardArtifact(AValue: AnsiString);
    procedure SetRewardMessage(AValue: TLocalizedString);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property NeverFlees: boolean read FNeverFlees write SetNeverFlees default False;
    property NoGrowing: boolean read FNoGrowing write SetNoGrowing default False;
    property Amount: Integer read FAmount write SetAmount default 0; //0=random
    property Character: TCreatureCharacter read FCharacter write FCharacter nodefault;

    property RewardMessage: TLocalizedString read FRewardMessage write SetRewardMessage stored IsRewardMessageStored;

    property RewardResources: TResourceSet read FRewardResources stored IsRewardResourcesStored;
    property RewardArtifact: AnsiString read FRewardArtifact write SetRewardArtifact stored IsRewardArtifactStored;

    property Reward: TRewards read FReward stored false;//todo: Creature full reward support in format version 1.1
  end;

  { TSeerHutOptions }

  TSeerHutOptions = class(TObjectOptions)
  public
    const
      ALLOWED_REWARDS: TRewards.TAllowedRewards =
        [TMetaclass.artifact, TMetaclass.creature, TMetaclass.experience, TMetaclass.luck, TMetaclass.mana,
        TMetaclass.morale, TMetaclass.movement, TMetaclass.primarySkill, TMetaclass.secondarySkill, TMetaclass.spell,
        TMetaclass.resource];
  private
    FQuest: TQuest;
    FReward: TRewards;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;

    procedure AddReward(AType: TSeerHutReward; AIdentifier: AnsiString; AValue: Int64);
  published
    property Quest: TQuest read FQuest;
    property Reward: TRewards read FReward;
  end;

  { TWitchHutOptions }

  TWitchHutOptions = class(TObjectOptions)
  private
    FAllowedSkills: TLogicalIDCondition;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property AllowedSkills: TLogicalIDCondition read FAllowedSkills;
  end;

  { TScholarOptions }

  TScholarOptions = class(TObjectOptions)
  strict private
    FReward: TRewards;
    FRewardPrimSkill: AnsiString;
    FRewardSecSkill: AnsiString;
    FRewardSpell: AnsiString;
    procedure SetRewardPrimSkill(AValue: AnsiString);
    procedure SetRewardSecSkill(AValue: AnsiString);
    procedure SetRewardSpell(AValue: AnsiString);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

    procedure Clear; override;
  published
    property RewardPrimSkill: AnsiString read FRewardPrimSkill write SetRewardPrimSkill;
    property RewardSkill: AnsiString read FRewardSecSkill write SetRewardSecSkill;
    property RewardSpell: AnsiString read FRewardSpell write SetRewardSpell;

    property Reward: TRewards read FReward stored false;//todo: Shcolar full reward support in format version 1.1
  end;

  { TGarrisonOptions }

  TGarrisonOptions = class(TOwnedArmedObjectOptions)
  private
    FRemovableUnits: Boolean;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property RemovableUnits: Boolean read FRemovableUnits write FRemovableUnits default False;
  end;

  { TArtifactOptions }

  TArtifactOptions = class(TGuardedObjectOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  end;

  { TSpellScrollOptions }

  TSpellScrollOptions = class(TArtifactOptions)
  private
    FSpell: AnsiString;
    procedure SetSpell(AValue: AnsiString);
  public
    constructor Create(AObject: IMapObject); override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property Spell: AnsiString read FSpell write SetSpell;
  end;

  { TResourceOptions }

  TResourceOptions = class(TGuardedObjectOptions)
  private
    FAmount: Integer;
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property Amount: Integer read FAmount write FAmount default 0;
  end;

  { TTownOptions }

  TTownOptions = class(TOwnedArmedObjectOptions, IFPObserver)
  private
    FBuildings: TLogicalIDCondition;
    FHasFort: Boolean;
    FName: TLocalizedString;
    FSpells: TLogicalIDCondition;
    function GetTightFormation: Boolean;
    function HasFortStored: Boolean;
    function IsBuildingsStored: Boolean;
    function IsSpellsStored: Boolean;
    procedure SetHasFort(AValue: Boolean);
    procedure SetTightFormation(AValue: Boolean);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure AssignTemplate(ATemplate: TMapObjectTemplate); override;
    function SelectTemplate(AType: TMapObjectType): TMapObjectTemplate; override;
    procedure Clear; override;
  public //IFPObserver
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  published
    property TightFormation: Boolean read GetTightFormation write SetTightFormation default false;
    property Name: TLocalizedString read FName write FName;
    property Spells: TLogicalIDCondition read FSpells stored IsSpellsStored;
    property Buildings: TLogicalIDCondition read FBuildings stored IsBuildingsStored;

    property HasFort: Boolean read FHasFort write SetHasFort stored HasFortStored;
  end;

  { TAbandonedOptions }

  TAbandonedOptions = class(TObjectOptions)
  private
    FPossibleResources: TStrings;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property PossibleResources: TStrings read FPossibleResources;
  end;

  { TMineOptions }

  TMineOptions = class (TOwnedArmedObjectOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  end;

  { TShrineOptions }

  TShrineOptions = class abstract (TObjectOptions)
  private
    FSpell: AnsiString;
    procedure SetSpell(AValue: AnsiString);
  strict protected
    class function GetSpellLevel: Integer; virtual; abstract;
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
    property SpellLevel: Integer read GetSpellLevel;
  published
    property Spell: AnsiString read FSpell write SetSpell;
  end;

  { TShrine1Options }

  TShrine1Options = class (TShrineOptions)
  strict protected
    class function GetSpellLevel: Integer; override;
  end;

  { TShrine2Options }

  TShrine2Options = class (TShrineOptions)
  strict protected
    class function GetSpellLevel: Integer; override;
  end;

  { TShrine3Options }

  TShrine3Options = class (TShrineOptions)
  strict protected
    class function GetSpellLevel: Integer; override;
  end;

  { TGrailOptions }

  TGrailOptions = class (TObjectOptions)
  private
    FRadius: Integer;
    procedure SetRadius(AValue: Integer);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property Radius: Integer read FRadius write FRadius;
  end;

  { TBaseRandomDwellingOptions }

  TBaseRandomDwellingOptions = class abstract (TObjectOptions)
  private
    FAllowedFactions: TLogicalIDCondition;
    FMaxLevel: UInt8;
    FMinLevel: UInt8;
    FSameAsTown: String;
    procedure SetMaxLevel(AValue: UInt8);
    procedure SetMinLevel(AValue: UInt8);
    function IsAllowedFactionsStored: boolean;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    procedure Clear; override;

    property MinLevel: UInt8 read FMinLevel write SetMinLevel default 0;
    property MaxLevel: UInt8 read FMaxLevel write SetMaxLevel default 7;

    property AllowedFactions: TLogicalIDCondition read FAllowedFactions stored IsAllowedFactionsStored;

    procedure SetSameAsTown(AValue: string);
    property SameAsTown: string read FSameAsTown write SetSameAsTown;

    class function CanBeOwned: Boolean; override;
  published
    property Owner default TPlayer.none;
  end;

  { TRandomDwellingOptions }

  TRandomDwellingOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property MinLevel;
    property MaxLevel;
    property AllowedFactions;
    property SameAsTown;
  end;

  { TRandomDwellingLVLOptions }

  TRandomDwellingLVLOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property AllowedFactions;
    property SameAsTown;
  end;

  { TRandomDwellingTownOptions }

  TRandomDwellingTownOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property MinLevel;
    property MaxLevel;
  end;

  { TQuestGuardOptions }

  TQuestGuardOptions = class (TObjectOptions)
  private
    FQuest: TQuest;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;
  published
    property Quest: TQuest read FQuest;
  end;

  { THeroPlaceholderOptions }

  THeroPlaceholderOptions = class (TObjectOptions)
  private
    FPower: UInt8;
    FTypeID: AnsiString;
    procedure SetPower(AValue: UInt8);
    procedure SetTypeID(AValue: AnsiString);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    procedure Clear; override;

    class function ZIndex: Integer; override;
    class function CanBeOwned: Boolean; override;
    class function MustBeOwned: Boolean; override;
  published
    property &Type: AnsiString read FTypeID write SetTypeID;
    property Power: UInt8 read FPower write SetPower default 0;
    property Owner default TPlayer.none;
  end;


  { IObjectOptionsVisitor }

  IObjectOptionsVisitor = interface
    procedure VisitLocalEvent(AOptions: TLocalEventOptions);
    procedure VisitSignBottle(AOptions: TSignBottleOptions);

    procedure VisitNormalHero(AOptions: TNormalHeroOptions);
    procedure VisitRandomHero(AOptions: TRandomHeroOptions);
    procedure VisitPrison(AOptions: TPrisonOptions);

    procedure VisitMonster(AOptions: TCreatureOptions);
    procedure VisitSeerHut(AOptions: TSeerHutOptions);
    procedure VisitWitchHut(AOptions: TWitchHutOptions);
    procedure VisitScholar(AOptions: TScholarOptions);
    procedure VisitGarrison(AOptions: TGarrisonOptions);
    procedure VisitArtifact(AOptions: TArtifactOptions);
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions);
    procedure VisitResource(AOptions: TResourceOptions);
    procedure VisitTown(AOptions: TTownOptions);
    procedure VisitAbandonedMine(AOptions:TAbandonedOptions);
    procedure VisitMine(AOptions: TMineOptions);
    procedure VisitShrine(AOptions: TShrineOptions);
    procedure VisitPandorasBox(AOptions: TPandorasOptions);
    procedure VisitGrail(AOptions: TGrailOptions);
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions);
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions);
    procedure VisitQuestGuard(AOptions:TQuestGuardOptions);
    procedure VisitHeroPlaceholder(AOptions: THeroPlaceholderOptions);

    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions);
  end;

implementation

{ TCreatureInstInfo }

function TCreatureInstInfo.GetRawRandom: integer;
begin
  Result := (Level - 1) * 2 + ifthen(Upgraded, 1, 0);
end;

procedure TCreatureInstInfo.SetRawRandom(AValue: integer);
var
  random_type: Integer;
begin
  random_type := AValue;

  if (random_type < 0) or (random_type > 13) then
  begin
    raise Exception.CreateFmt('Invalid raw random type %d ', [random_type]);
  end;

  Level:= (random_type div 2) + 1;
  Upgraded:=(random_type mod 2) > 0;
end;

function TCreatureInstInfo.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty and (FLevel = 0) and (FUpgraded = false);
end;

{ TRewardPrimarySkills }

function TRewardPrimarySkills.GetAttack: Integer;
begin
  Result := FData.GetValue(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.attack]);
end;

function TRewardPrimarySkills.GetDefence: Integer;
begin
  Result := FData.GetValue(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.defence]);
end;

function TRewardPrimarySkills.GetKnowledge: Integer;
begin
  Result := FData.GetValue(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.knowledge]);
end;

function TRewardPrimarySkills.GetSpellpower: Integer;
begin
  Result := FData.GetValue(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.spellpower]);
end;

procedure TRewardPrimarySkills.SetAttack(AValue: Integer);
begin
  FData.AddOrUpdateReward(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.attack], AValue);
end;

procedure TRewardPrimarySkills.SetDefence(AValue: Integer);
begin
  FData.AddOrUpdateReward(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.defence], AValue);
end;

procedure TRewardPrimarySkills.SetKnowledge(AValue: Integer);
begin
  FData.AddOrUpdateReward(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.knowledge], AValue);
end;

procedure TRewardPrimarySkills.SetSpellpower(AValue: Integer);
begin
  FData.AddOrUpdateReward(TMetaclass.primarySkill, PRIMARY_SKILL_NAMES[TPrimarySkill.spellpower], AValue);
end;

constructor TRewardPrimarySkills.Create(AReward: TRewards);
begin
  FData := AReward;
  inherited Create;
end;

function TRewardPrimarySkills.IsDefault: Boolean;
begin
  Result := (Attack = 0) and (Defence = 0) and (Spellpower = 0) and (Knowledge = 0);
end;

procedure TRewardPrimarySkills.Clear;
begin
  SetZero;
end;

{ TIndepResourceSet }

function TIndepResourceSet.GetAmount(AType: TResType): integer;
begin
  Result := Fvalue[AType];
end;

procedure TIndepResourceSet.SetAmount(AType: TResType; AValue: integer);
begin
  Fvalue[AType] := AValue;
end;

{ TReward }

function TReward.GetMetaclass: TMetaclass;
var
  scope, ident: AnsiString;
begin
  if not DecodeFullIdentifier(Identifier, result, scope, ident) then
    Result := TMetaclass.invalid;
end;

procedure TReward.AssignTo(Dest: TPersistent);
begin
  if Dest is TReward then
  begin
    Value:=TReward(Dest).Value;
  end;
  inherited AssignTo(Dest);
end;

{ TRewards }

procedure TRewards.SetAllowedReward(AValue: TAllowedRewards);
begin
  FAllowedReward := AValue - IMPOSSIBLE_REWARDS;
end;

procedure TRewards.ItemIdentifierChanged(Item: TCollectionItem; AOldName: String; ANewName: String);
begin
  inherited ItemIdentifierChanged(Item, AOldName, ANewName);
  FOwner.NotifyReferenced(AOldName, ANewName);
end;

constructor TRewards.Create(AOwner: IMapObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TRewards.AddOrUpdateReward(AType: TMetaclass; AIdentifier: AnsiString; AValue: Int64);
var
  metaclass_id, full_id: AnsiString;
  item: TReward;
  idx: Integer;
begin
  if AType = TMetaclass.invalid then
  begin
    DebugLn(['attempt to add invalid reward']);
    Exit;
  end;

  if AType in IMPOSSIBLE_REWARDS then
  begin
    DebugLn(['attempt to add impossible reward']);
    Exit;
  end;

  //if AValue = 0 then
  //begin
  //  DebugLn(['attempt to add reward with zero amount']);
  //  Exit;
  //end;

  metaclass_id := GetEnumName(TypeInfo(TMetaclass), Integer(AType));

  full_id:=EncodeFullIdentifier(metaclass_id,'',AIdentifier);

  idx :=  IndexOfName(full_id);

  if (idx < 0) and (AValue <> 0) then
  begin
    item := Add;
    item.Identifier := full_id;
    item.Value:=AValue;
  end
  else if (idx >= 0) and (AValue = 0) then
  begin
    self.Delete(idx);
  end
  else if (idx >= 0) then
  begin
    item := Items[idx];
    item.Value:=AValue;
  end;
end;

function TRewards.GetValue(AType: TMetaclass; AIdentifier: AnsiString): Int64;
var
  metaclass_id, full_id: String;
  idx: Integer;
  item: TReward;
begin
  metaclass_id := GetEnumName(TypeInfo(TMetaclass), Integer(AType));

  full_id:=EncodeFullIdentifier(metaclass_id,'',AIdentifier);

  idx :=  IndexOfName(full_id);

  if idx < 0 then
  begin
    Result := 0;
  end
  else
  begin
    item := Items[idx];
    Result := item.Value;
  end;
end;

{ TRewardResourceSet }

function TRewardResourceSet.GetAmount(AType: TResType): integer;
begin
  Result := FData.GetValue(TMetaclass.resource, RESOURCE_NAMES[AType]);
end;

procedure TRewardResourceSet.SetAmount(AType: TResType; AValue: integer);
begin
  FData.AddOrUpdateReward(TMetaclass.resource, RESOURCE_NAMES[AType], AValue);
end;

constructor TRewardResourceSet.Create(AData: TRewards);
begin
  inherited Create;
  FData := AData;
end;

{ TMineOptions }

procedure TMineOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitMine(Self);
end;

{ TOwnedArmedObjectOptions }

procedure TOwnedArmedObjectOptions.Clear;
begin
  inherited Clear;
  FArmy.Clear;
end;

function TOwnedArmedObjectOptions.IsArmyStored: Boolean;
begin
  Result := not FArmy.IsEmpty;
end;

constructor TOwnedArmedObjectOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FArmy := TCreatureSet.Create(AObject);
end;

destructor TOwnedArmedObjectOptions.Destroy;
begin
  FArmy.Free;
  inherited Destroy;
end;

{ TObjectOptions }

constructor TObjectOptions.Create(AObject: IMapObject);
begin
  FObject := AObject;
end;

procedure TObjectOptions.Clear;
begin
  //do nothing here
end;

procedure TObjectOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  //do nothing here
end;

class function TObjectOptions.CanBeOwned: Boolean;
begin
  Result := MustBeOwned;
end;

class function TObjectOptions.MustBeOwned: Boolean;
begin
  Result := false;
end;

class function TObjectOptions.GetClassByID(ID: AnsiString; SubID: AnsiString): TObjectOptionsClass;
begin
  Result := TObjectOptions;
  case ID of
    'event':
      Result := TLocalEventOptions;
    'oceanBottle', 'sign':
      Result := TSignBottleOptions;
    'hero':
      Result := TNormalHeroOptions;
    'randomHero':
      Result := TRandomHeroOptions;
    'prison':
      Result := TPrisonOptions;
    'monster',
    'randomMonster',
    'randomMonsterLevel1',
    'randomMonsterLevel2',
    'randomMonsterLevel3',
    'randomMonsterLevel4',
    'randomMonsterLevel5',
    'randomMonsterLevel6',
    'randomMonsterLevel7':
      Result := TCreatureOptions;
    'seerHut':
      Result := TSeerHutOptions;
    'witchHut':
      Result := TWitchHutOptions;
    'scholar':
      Result := TScholarOptions;
    'garrisonHorizontal', 'garrisonVertical':
      Result := TGarrisonOptions;

    'artifact', 'randomArtifact',
    'randomArtifactTreasure',
    'randomArtifactMinor',
    'randomArtifactMajor',
    'randomArtifactRelic':
      Result := TArtifactOptions;
    'spellScroll':
      Result := TSpellScrollOptions;
    'resource',
    'randomResource':
      Result := TResourceOptions;
    'randomTown', 'town':
      Result := TTownOptions;

    'creatureGeneratorCommon',

    //todo:  CREATURE_GENERATOR2, CREATURE_GENERATOR3
    //CREATURE_GENERATOR2,
    //CREATURE_GENERATOR3,

    'creatureGeneratorSpecial':
      Result := TOwnedObjectOptions;
    'mine':
    begin
      if SubID = 'abandoned' then
      begin
        Result := TAbandonedOptions;
      end else
      begin
        Result := TMineOptions;
      end;
    end;
    'abandonedMine':
      Result := TAbandonedOptions;
    'shrineOfMagicLevel1':
      Result := TShrine1Options;
    'shrineOfMagicLevel2':
      Result := TShrine2Options;
    'shrineOfMagicLevel3':
      Result := TShrine3Options;
    'pandoraBox':
      Result := TPandorasOptions;
    'grail':
      Result := TGrailOptions;
    'randomDwelling':
      Result := TRandomDwellingOptions;
    'randomDwellingLvl':
      Result := TRandomDwellingLVLOptions;
    'randomDwellingFaction':
      Result := TRandomDwellingTownOptions;
    'questGuard':
      Result := TQuestGuardOptions;
    'shipyard','lighthouse':
      Result := TOwnedObjectOptions;
    'heroPlaceholder':
      Result := THeroPlaceholderOptions;
  end;
end;

class function TObjectOptions.CreateByID(ID: AnsiString; SubID: AnsiString; AObject: IMapObject): TObjectOptions;
begin
   Result := GetClassByID(Id, SubId).Create(AObject);
end;

class function TObjectOptions.ZIndex: Integer;
begin
  Result := 0;
end;

procedure TObjectOptions.AssignTemplate(ATemplate: TMapObjectTemplate);
begin
  //do nothing by default
end;

function TObjectOptions.SelectTemplate(AType: TMapObjectType): TMapObjectTemplate;
begin
  Result := nil;
end;

procedure TObjectOptions.SetOwner(AValue: TPlayer);
begin
  if not (AValue in [TPlayer.RED..TPlayer.PINK,TPlayer.NONE]) then
  begin
    raise Exception.CreateFmt('Invalid player color %d',[Integer(AValue)]);
  end;
  FObject.SetPlayer(AValue);
end;

function TObjectOptions.GetOwner: TPlayer;
begin
  Result := FObject.GetPlayer;
end;

procedure TObjectOptions.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TObjectOptions.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TObjectOptions.BeforeSerialize(Sender: TObject);
begin

end;

procedure TObjectOptions.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

{ TPrisonOptions }

constructor TPrisonOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
end;

procedure TPrisonOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitPrison(self);
end;

{ TRandomHeroOptions }

procedure TRandomHeroOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitRandomHero(self);
end;

class function TRandomHeroOptions.MustBeOwned: Boolean;
begin
  Result:=true;
end;

class function TRandomHeroOptions.ZIndex: Integer;
begin
  Result:=-100;
end;

{ TNormalHeroOptions }

procedure TNormalHeroOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitNormalHero(self);
end;

class function TNormalHeroOptions.MustBeOwned: Boolean;
begin
  Result:=true;
end;

class function TNormalHeroOptions.ZIndex: Integer;
begin
  Result:=-100;
end;

{ THeroArtifacts }

function THeroArtifacts.GetBackpack: TStrings;
begin
  Result := FBackpack;
end;

function THeroArtifacts.GetBySlotNumber(ASlotID: Integer): AnsiString;
begin
  if ASlotID < 0 then
  begin
    raise Exception.CreateFmt('Invalid artifact slot %d',[ASlotID]);
  end;

  if ASlotID <= High(FSlots) then
  begin
    Result := FSlots[ASlotID];
    Exit;
  end;

  Result := FBackpack[ASlotID-High(FSlots)];

end;

function THeroArtifacts.IsBackpackStored: Boolean;
begin
  Result := FBackpack.Count > 0;
end;

procedure THeroArtifacts.SetBySlotNumber(ASlotID: Integer; AValue: AnsiString);
var
  idx: Integer;
begin
  if ASlotID < 0 then
  begin
    raise Exception.CreateFmt('Invalid artifact slot %d',[ASlotID]);
  end;

  if ASlotID <= High(FSlots) then
  begin
    FOwner.NotifyReferenced(FSlots[ASlotID],AValue);
    FSlots[ASlotID] := AValue;
    Exit;
  end;

  idx:=ASlotID-Length(FSlots); //convert to backback index

  if idx<FBackpack.Count then
  begin
    FBackpack[idx] := AValue;
  end
  else if idx > FBackpack.Count then
  begin
    raise Exception.CreateFmt('Invalid artifact slot %d',[ASlotID]);
  end
  else begin
    FBackpack.Add(AValue);
  end;
end;

constructor THeroArtifacts.Create(AOwner: IReferenceNotify);
begin
  FOwner := AOwner;
  FBackpack := TIdentifierList.Create(AOwner);
end;

destructor THeroArtifacts.Destroy;
begin
  FBackpack.Free;
  inherited Destroy;
end;

function THeroArtifacts.IsEmpty: Boolean;
var
  s: String;
begin
  if not (FBackpack.Count = 0) then
    exit(false);

  for s in FSlots do
    if s <> '' then
      Exit(false);

  Exit(true);
end;

procedure THeroArtifacts.Clear;
var
  slot: Integer;
begin
  FBackpack.Clear;
  for slot in [0..ARTIFACT_SLOT_COUNT-1] do
  begin
    FSlots[slot] := '';
  end;
end;

{ TResourceSet }

function TResourceSet.IsEmpty: Boolean;
var
  typ: TResType;
begin
  for typ in TResType do
    if Amount[typ] <> 0 then
      Exit(False);
  Exit(true);
end;

procedure TResourceSet.Clear;
var
  typ: TResType;
begin
  for typ in TResType do
    SetAmount(typ, 0);
end;

{ TAbandonedOptions }

constructor TAbandonedOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FPossibleResources := TStringList.Create;
end;

destructor TAbandonedOptions.Destroy;
begin
  FPossibleResources.Free;
  inherited Destroy;
end;

procedure TAbandonedOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitAbandonedMine(Self);
end;

procedure TAbandonedOptions.Clear;
begin
  inherited Clear;
  PossibleResources.Clear;
end;

{ TQuest }

procedure TQuest.SetCompletedText(AValue: TLocalizedString);
begin
  FCompletedText := AValue;
end;

function TQuest.IsCreaturesStored: Boolean;
begin
  Result := MissionType = TQuestMission.Army;
end;

function TQuest.IsArtifactsStored: Boolean;
begin
  Result := MissionType = TQuestMission.Artifact;
end;

function TQuest.IsHeroIDStored: Boolean;
begin
  Result := MissionType = TQuestMission.Hero;
end;

function TQuest.IsHeroLevelStored: Boolean;
begin
  Result := MissionType = TQuestMission.Level;
end;

function TQuest.IsKillTargetStored: Boolean;
begin
  Result := MissionType in [TQuestMission.KillCreature, TQuestMission.KillHero];
end;

function TQuest.IsPlayerIDStored: Boolean;
begin
  Result := MissionType = TQuestMission.Player;
end;

function TQuest.IsPrimarySkillsStored: Boolean;
begin
  Result := MissionType = TQuestMission.PrimaryStat;
end;

function TQuest.IsResourcesStored: Boolean;
begin
  Result := MissionType = TQuestMission.Resources;
end;

procedure TQuest.SetFirstVisitText(AValue: TLocalizedString);
begin
  FFirstVisitText := AValue;
end;

procedure TQuest.SetHeroID(AValue: AnsiString);
begin
  if FHeroID=AValue then Exit;
  FOwner.NotifyReferenced(FHeroID,AValue);
  FHeroID:=AValue;
end;

procedure TQuest.SetHeroLevel(AValue: Integer);
begin
  if FHeroLevel=AValue then Exit;
  FHeroLevel:=AValue;
end;

procedure TQuest.SetKillTarget(AValue: String);
begin
  if FKillTarget=AValue then Exit;
  FKillTarget:=AValue;
end;

procedure TQuest.SetMissionType(AValue: TQuestMission);
begin
  if FMissionType=AValue then Exit;
  FMissionType:=AValue;
end;

procedure TQuest.SetNextVisitText(AValue: TLocalizedString);
begin
  FNextVisitText := AValue;
end;

procedure TQuest.SetPlayerID(AValue: TPlayer);
begin
  if FPlayerID=AValue then Exit;
  FPlayerID:=AValue;
end;

procedure TQuest.SetTimeLimit(AValue: Integer);
begin
  if FTimeLimit=AValue then Exit;
  FTimeLimit:=AValue;
end;

constructor TQuest.Create(AOwner: IMapObject);
begin
  FOwner := AOwner;
  TimeLimit := -1;
  FArtifacts := TIdentifierList.Create(AOwner);//todo: should we allow muiltiple same arctifacts for quest

  FCreatures := TCreatureSet.Create(FOwner);
  FResources := TIndepResourceSet.Create;
  FPrimarySkills := THeroPrimarySkills.Create;
  FHeroLevel := -1;
  FPlayerID:=TPlayer.NONE;
end;

destructor TQuest.Destroy;
begin
  FPrimarySkills.Free;
  FResources.Free;
  FCreatures.Free;
  FArtifacts.Free;
  inherited Destroy;
end;

procedure TQuest.Clear;
begin
  FirstVisitText := '';
  NextVisitText := '';
  CompletedText := '';

  MissionType := TQuestMission.None;
  TimeLimit := -1;

  Artifacts.Clear;
  Creatures.Clear;
  Resources.Clear;
  PrimarySkills.Clear;

  HeroLevel := -1;

  Hero:='';

  Player:=TPlayer.NONE;
  KillTarget:='';
end;

{ THeroPlaceholderOptions }

procedure THeroPlaceholderOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitHeroPlaceholder(Self);
end;

procedure THeroPlaceholderOptions.Clear;
begin
  inherited Clear;
  &Type := '';
  Power:=0;
  Owner := TPlayer.none;
end;

class function THeroPlaceholderOptions.ZIndex: Integer;
begin
  Result:=-100;
end;

class function THeroPlaceholderOptions.CanBeOwned: Boolean;
begin
  Result:=True;
end;

class function THeroPlaceholderOptions.MustBeOwned: Boolean;
begin
  Result:=True;
end;

procedure THeroPlaceholderOptions.SetPower(AValue: UInt8);
begin
  FPower := AValue;
end;

procedure THeroPlaceholderOptions.SetTypeID(AValue: AnsiString);
begin
  FTypeID := AValue;
end;

{ TOwnedObjectOptions }

procedure TOwnedObjectOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitOwnedObject(Self);
end;

class function TOwnedObjectOptions.CanBeOwned: Boolean;
begin
  Result:=True;
end;

procedure TOwnedObjectOptions.Clear;
begin
  inherited Clear;
  Owner:=TPlayer.none;
end;

{ TQuestGuardOptions }

procedure TQuestGuardOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitQuestGuard(Self);
end;

procedure TQuestGuardOptions.Clear;
begin
  inherited Clear;
  Quest.Clear;
end;

constructor TQuestGuardOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FQuest := TQuest.Create(AObject);
end;

destructor TQuestGuardOptions.Destroy;
begin
  FQuest.Free;
  inherited Destroy;
end;

{ TBaseRandomDwellingOptions }

procedure TBaseRandomDwellingOptions.SetMaxLevel(AValue: UInt8);
begin
  if FMaxLevel=AValue then Exit;
  FMaxLevel:=AValue;
end;

procedure TBaseRandomDwellingOptions.SetMinLevel(AValue: UInt8);
begin
  if FMinLevel=AValue then Exit;
  FMinLevel:=AValue;
end;

constructor TBaseRandomDwellingOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FAllowedFactions := TLogicalIDCondition.Create(AObject);
  AObject.GetListsManager.FactionInfos.FillWithAllIds(FAllowedFactions, False);
end;

destructor TBaseRandomDwellingOptions.Destroy;
begin
  FAllowedFactions.Free;
  inherited Destroy;
end;

procedure TBaseRandomDwellingOptions.Clear;
begin
  inherited Clear;
  MinLevel:=0;
  MaxLevel:=7;
  AllowedFactions.Clear;
  SameAsTown:='';
  Owner:=TPlayer.none;
end;

function TBaseRandomDwellingOptions.IsAllowedFactionsStored: boolean;
begin
  Result := not FAllowedFactions.IsEmpty;
end;

procedure TBaseRandomDwellingOptions.SetSameAsTown(AValue: string);
begin
  if FSameAsTown=AValue then Exit;
  FSameAsTown:=AValue;
end;

class function TBaseRandomDwellingOptions.CanBeOwned: Boolean;
begin
  Result:=True;
end;

{ TBaseCreatureInstInfo }

constructor TBaseCreatureInstInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

function TBaseCreatureInstInfo.IsEmpty: Boolean;
begin
  Result := (FType = '') and (FAmount = 0);
end;

procedure TBaseCreatureInstInfo.SetAmount(AValue: Integer);
begin
  FAmount := AValue;
end;

procedure TBaseCreatureInstInfo.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;

  (Collection as TCreatureSet).NotifyReferenced(FType, AValue);
  FType:=AValue;
end;

{ TGuardedObjectOptions }

constructor TGuardedObjectOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FGuards := TCreatureSet.Create(AObject);
end;

destructor TGuardedObjectOptions.Destroy;
begin
  FGuards.Free;
  inherited Destroy;
end;

procedure TGuardedObjectOptions.Clear;
begin
  inherited Clear;
  GuardMessage:='';
  Guards.Clear;
end;

procedure TGuardedObjectOptions.SetGuardMessage(AValue: TLocalizedString);
begin
  FGuardMessage := AValue;
end;

function TGuardedObjectOptions.IsGuardsStored: Boolean;
begin
  Result := not FGuards.IsEmpty;
end;

{ TCreatureSet }

procedure TCreatureSet.SetTightFormation(AValue: Boolean);
begin
  if FTightFormation=AValue then Exit;
  FTightFormation:=AValue;
end;

constructor TCreatureSet.Create(AOwner: IMapObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TCreatureSet.NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
begin
  FOwner.NotifyReferenced(AOldIdentifier, ANewIdentifier);
end;

function TCreatureSet.IsEmpty: Boolean;
var
  item: TCollectionItem;
begin
  Result := True;

  for item in self do
  begin
    if not TCreatureInstInfo(item).IsEmpty then
    begin
      Result := False;
      Break;
    end;
  end;
end;

{ TRandomDwellingOptions }

procedure TRandomDwellingOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitRandomDwelling(Self);
end;

{ TRandomDwellingLVLOptions }

procedure TRandomDwellingLVLOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitRandomDwellingLVL(Self);
end;

{ TRandomDwellingTownOptions }

procedure TRandomDwellingTownOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitRandomDwellingTown(Self);
end;

{ TGrailOptions }

procedure TGrailOptions.SetRadius(AValue: Integer);
begin
  Radius:=0;
end;

procedure TGrailOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitGrail(Self);
end;

procedure TGrailOptions.Clear;
begin
  inherited Clear;
end;

{ TPandorasOptions }

function TPandorasOptions.IsResourcesStored: Boolean;
begin
  Result := not FResources.IsEmpty;
end;

function TPandorasOptions.IsSecondarySkillsStored: Boolean;
begin
  Result := FSecondarySkills.Count > 0;
end;

function TPandorasOptions.IsSpellsStored: Boolean;
begin
  Result := FSpells.Count > 0;
end;

procedure TPandorasOptions.SetExperience(AValue: UInt64);
begin
  Reward.AddOrUpdateReward(TMetaclass.experience,'', Int64(AValue));
end;

procedure TPandorasOptions.SetLuck(AValue: Int32);
begin
  Reward.AddOrUpdateReward(TMetaclass.luck,'', AValue);
end;

procedure TPandorasOptions.SetMana(AValue: Int32);
begin
  Reward.AddOrUpdateReward(TMetaclass.mana,'', AValue);
end;

procedure TPandorasOptions.SetMorale(AValue: Int32);
begin
  Reward.AddOrUpdateReward(TMetaclass.morale,'', AValue);
end;

function TPandorasOptions.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function TPandorasOptions.IsArtifactsStored: Boolean;
begin
  Result := FArtifacts.Count > 0;
end;

function TPandorasOptions.GetExperience: UInt64;
begin
  Result := UInt64(Reward.GetValue(TMetaclass.experience));
end;

function TPandorasOptions.GetLuck: Int32;
begin
  Result := Reward.GetValue(TMetaclass.luck);
end;

function TPandorasOptions.GetMana: Int32;
begin
  Result := Reward.GetValue(TMetaclass.mana);
end;

function TPandorasOptions.GetMorale: Int32;
begin
  Result := Reward.GetValue(TMetaclass.morale);
end;

function TPandorasOptions.IsCreaturesStored: Boolean;
begin
  Result := not FCreatures.IsEmpty;
end;

constructor TPandorasOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FReward := TRewards.Create(AObject);
  FReward.AllowedReward := [
    TMetaclass.artifact,
    TMetaclass.creature,
    TMetaclass.experience,
    TMetaclass.luck,
    TMetaclass.mana,
    TMetaclass.morale,
    //TMetaclass.movement,
    TMetaclass.primarySkill,
    TMetaclass.resource,
    TMetaclass.secondarySkill,
    TMetaclass.spell
    ];

  FCreatures := TCreatureSet.Create(AObject);
  FResources := TRewardResourceSet.Create(FReward);
  FPrimarySkills := TRewardPrimarySkills.Create(FReward);
  FSecondarySkills := THeroSecondarySkills.Create;
  FArtifacts := TIdentifierList.Create(AObject);
  FSpells := TIdentifierList.Create(AObject);
end;

destructor TPandorasOptions.Destroy;
begin
  FSpells.Free;
  FArtifacts.Free;
  FSecondarySkills.free;
  FPrimarySkills.free;
  FResources.Free;
  FreeAndNil(FCreatures);
  FReward.Free;
  inherited Destroy;
end;

procedure TPandorasOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitPandorasBox(Self);
end;

procedure TPandorasOptions.Clear;
begin
  inherited Clear;
  Creatures.Clear;
  Experience := 0;
  Mana := 0;
  Morale := 0;
  Luck := 0;
  Resources.Clear;
  PrimarySkills.Clear;
  SecondarySkills.Clear;
  Artifacts.Clear;
  Spells.Clear;
end;

procedure TPandorasOptions.BeforeSerialize(Sender: TObject);
var
  item: TCollectionItem;
  r: TReward;
  metaclass: TMetaclass;
  scope, ident: AnsiString;
  inst_info: TBaseCreatureInstInfo;
  full_ident: String;
  skill_info: THeroSecondarySkill;
begin
  inherited;
  Artifacts.Clear;
  Creatures.Clear;
  SecondarySkills.Clear;
  Spells.Clear;

  for item in Reward do
  begin
    r := item as TReward;

    if DecodeFullIdentifier(r.Identifier, metaclass, scope, ident) then
    begin
      full_ident := EncodeIdentifier(scope,ident);

      case metaclass of
        TMetaclass.artifact:
        begin
          Artifacts.Add(full_ident);
        end;
        TMetaclass.creature:
        begin
          inst_info := Creatures.Add;
          inst_info.Amount:=r.Value;
          inst_info.&type := full_ident;
        end;
        TMetaclass.secondarySkill:
        begin
          skill_info := SecondarySkills.Add;
          skill_info.Identifier:=full_ident;
          skill_info.Level:=TSkillLevel(r.Value);
        end;
        TMetaclass.spell:
        begin
          Spells.Add(full_ident);
        end;
      end;

      if Metaclass = TMetaclass.creature then
      begin
        inst_info := Creatures.Add;
        inst_info.Amount:=r.Value;
        inst_info.&type := full_ident;
      end;
    end;
  end;
end;

procedure TPandorasOptions.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  UpdateRewards;
  inherited;
end;

procedure TPandorasOptions.UpdateRewards;
var
  item: TCollectionItem;
  cr: TBaseCreatureInstInfo;
  s: String;
  skill: THeroSecondarySkill;
begin
  for item in Creatures do
  begin
    cr := item as TBaseCreatureInstInfo;
    Reward.AddOrUpdateReward(TMetaclass.creature, cr.&type, cr.Amount);
  end;

  for s in Artifacts do
  begin
    Reward.AddOrUpdateReward(TMetaclass.artifact, s, 1);
  end;

  for s in Spells do
  begin
    Reward.AddOrUpdateReward(TMetaclass.spell, s, 1);
  end;

  for item in SecondarySkills do
  begin
    skill := item as THeroSecondarySkill;
    Reward.AddOrUpdateReward(TMetaclass.secondarySkill, skill.Identifier, Integer(skill.Level));
  end;
end;

{ TShrineOptions }

procedure TShrineOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitShrine(Self);
end;

procedure TShrineOptions.Clear;
begin
  inherited Clear;
  Spell := '';
end;

procedure TShrineOptions.SetSpell(AValue: AnsiString);
begin
    //TODO: check spell level
  FSpell := AValue;
end;


{ TShrine1Options }

class function TShrine1Options.GetSpellLevel: Integer;
begin
  Result := 1;
end;

{ TShrine2Options }

class function TShrine2Options.GetSpellLevel: Integer;
begin
  Result := 2;
end;

{ TShrine3Options }

class function TShrine3Options.GetSpellLevel: Integer;
begin
  Result := 3;
end;

{ TTownOptions }

procedure TTownOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitTown(Self);
end;

procedure TTownOptions.AssignTemplate(ATemplate: TMapObjectTemplate);
begin
  inherited AssignTemplate(ATemplate);

  //TODO: replace with generic solution
  if ATemplate.Identifier = 'fort' then
  begin
    HasFort:=True;
  end
  else if ATemplate.Identifier = 'village' then
  begin
    HasFort:=false
  end;
end;

function TTownOptions.SelectTemplate(AType: TMapObjectType): TMapObjectTemplate;
var
  template_id: AnsiString;

  canditates: TStringList;
begin
  //todo: replace with logical condition
  Result := nil;
  canditates := TStringList.Create;
  canditates.Sorted := false;
  try
    if Buildings.IsEmpty then
    begin
      if HasFort then
      begin
        canditates.Add('fort');
      end
      else
      begin
        canditates.Add('village');
      end;
    end
    else
    begin
      if Buildings.IsRequired('castle') then
      begin
        canditates.Add('castle');
        canditates.Add('citadel');
        canditates.Add('fort');
      end
      else if Buildings.IsRequired('citadel') then
      begin
        canditates.Add('citadel');
        canditates.Add('fort');
      end
      else if Buildings.IsRequired('fort') then
      begin
        canditates.Add('fort');
      end else
      begin
        canditates.Add('village');
      end;

      if Buildings.IsRequired('capitol') then
        canditates.Insert(0, 'capitol')
    end;

    for template_id in canditates do
    begin
      Result := AType.Templates.FindItem(template_id);
      if Assigned(Result) then
      begin
        break;
      end;
    end;
  finally
    canditates.Free;
  end;
end;

procedure TTownOptions.Clear;
begin
  inherited Clear;
  Army.Clear;
  TightFormation:=False;
  Name:='';
  Spells.Clear;
  Buildings.Clear;
end;

procedure TTownOptions.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FBuildings then
  begin
    MapObject.InvalidateAppearance();
  end;
end;

constructor TTownOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FSpells := TLogicalIDCondition.Create(AObject);
  FBuildings := TLogicalIDCondition.Create(AObject);
  FBuildings.FPOAttachObserver(Self);
end;

destructor TTownOptions.Destroy;
begin
  FBuildings.Free;
  FSpells.Free;
  inherited Destroy;
end;

function TTownOptions.GetTightFormation: Boolean;
begin
  Result := Army.TightFormation;
end;

function TTownOptions.HasFortStored: Boolean;
begin
  Result := FBuildings.IsEmpty;
end;

function TTownOptions.IsBuildingsStored: Boolean;
begin
  Result := not FBuildings.IsEmpty;
end;

function TTownOptions.IsSpellsStored: Boolean;
begin
  Result := not FSpells.IsEmpty;
end;

procedure TTownOptions.SetHasFort(AValue: Boolean);
begin
  if FHasFort=AValue then Exit;
  MapObject.InvalidateAppearance();
  FHasFort:=AValue;
end;

procedure TTownOptions.SetTightFormation(AValue: Boolean);
begin
  Army.TightFormation := AValue;
end;

{ TResourceOptions }

procedure TResourceOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitResource(Self);
end;

procedure TResourceOptions.Clear;
begin
  inherited Clear;
  Amount:=0;
end;

{ TSpellScrollOptions }

procedure TSpellScrollOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSpellScroll(Self);
end;

procedure TSpellScrollOptions.Clear;
begin
  inherited Clear;
  Spell := '';
end;

procedure TSpellScrollOptions.SetSpell(AValue: AnsiString);
begin
  if FSpell=AValue then Exit;
  MapObject.NotifyReferenced(FSpell, AValue);
  FSpell:=AValue;
end;

constructor TSpellScrollOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FSpell := 'magicArrow';
end;

{ TArtifactOptions }

procedure TArtifactOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitArtifact(Self);
end;

{ TGarrisonOptions }

procedure TGarrisonOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitGarrison(Self);
end;

procedure TGarrisonOptions.Clear;
begin
  inherited Clear;
  RemovableUnits:=false;
end;

constructor TGarrisonOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
end;

destructor TGarrisonOptions.Destroy;
begin
  inherited Destroy;
end;

{ TScholarOptions }

procedure TScholarOptions.SetRewardPrimSkill(AValue: AnsiString);
begin
  if FRewardPrimSkill=AValue then Exit;
  FRewardPrimSkill:=AValue;
end;

procedure TScholarOptions.SetRewardSecSkill(AValue: AnsiString);
begin
  if FRewardSecSkill=AValue then Exit;
  FRewardSecSkill:=AValue;
end;

procedure TScholarOptions.SetRewardSpell(AValue: AnsiString);
begin
  if FRewardSpell=AValue then Exit;
  MapObject.NotifyReferenced(FRewardSpell, AValue);
  FRewardSpell:=AValue;
end;

constructor TScholarOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FReward := TRewards.Create(AObject);
end;

destructor TScholarOptions.Destroy;
begin
  FReward.Free;
  inherited Destroy;
end;

procedure TScholarOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitScholar(Self);
end;

procedure TScholarOptions.Clear;
begin
  inherited Clear;
  FRewardPrimSkill := '';
  FRewardSecSkill := '';
  FRewardSpell := '';
end;

{ TWitchHutOptions }

procedure TWitchHutOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitWitchHut(Self);
end;

procedure TWitchHutOptions.Clear;
begin
  inherited Clear;
  AllowedSkills.Clear;
end;

constructor TWitchHutOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FAllowedSkills := TLogicalIDCondition.Create(AObject);

  //todo: make it configurable
  FAllowedSkills.NoneOf.Add(SECONDARY_SKILL_NAMES[6]); //leadership
  FAllowedSkills.NoneOf.Add(SECONDARY_SKILL_NAMES[12]);//necromancy
end;

destructor TWitchHutOptions.Destroy;
begin
  FAllowedSkills.Free;
  inherited Destroy;
end;

{ TSeerHutOptions }

procedure TSeerHutOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSeerHut(self);
end;

procedure TSeerHutOptions.Clear;
begin
  inherited Clear;
  Quest.Clear;
  Reward.Clear;
end;

procedure TSeerHutOptions.AddReward(AType: TSeerHutReward; AIdentifier: AnsiString; AValue: Int64);
const
  SEER_REWARD_TO_REWARD: array[TSeerHutReward] of TMetaclass =
    (TMetaclass.invalid, TMetaclass.experience, TMetaclass.mana, TMetaclass.morale, TMetaclass.luck,
    TMetaclass.resource, TMetaclass.primarySkill, TMetaclass.secondarySkill, TMetaclass.artifact, TMetaclass.spell, TMetaclass.creature);
begin
  FReward.AddOrUpdateReward(SEER_REWARD_TO_REWARD[AType], AIdentifier, AValue);
end;

constructor TSeerHutOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FQuest := TQuest.Create(AObject);
  FReward := TRewards.Create(AObject);
  FReward.AllowedReward := ALLOWED_REWARDS;
end;

destructor TSeerHutOptions.Destroy;
begin
  FReward.Free;
  FQuest.Free;
  inherited Destroy;
end;

{ TCreatureOptions }

procedure TCreatureOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitMonster(Self);
end;

procedure TCreatureOptions.Clear;
begin
  inherited Clear;
  NeverFlees := false;
  NoGrowing := false;
  Amount := 0;
  Character := TCreatureCharacter.compliant;

  RewardMessage := '';
  RewardResources.Clear;
  RewardArtifact:='';
end;

procedure TCreatureOptions.SetNeverFlees(AValue: boolean);
begin
  FNeverFlees := AValue;
end;

procedure TCreatureOptions.SetAmount(AValue: Integer);
begin
  if FAmount=AValue then Exit;
  FAmount:=AValue;
end;

function TCreatureOptions.IsRewardArtifactStored: Boolean;
begin
  Result := (FRewardArtifact <> '');
end;

function TCreatureOptions.IsRewardMessageStored: Boolean;
begin
  Result := (FRewardMessage <> '');
end;

function TCreatureOptions.IsRewardResourcesStored: Boolean;
begin
  Result := (not FRewardResources.IsEmpty);
end;

function TCreatureOptions.IsRewardStored: Boolean;
begin
  Result := false;
end;

procedure TCreatureOptions.SetNoGrowing(AValue: boolean);
begin
  FNoGrowing := AValue;
end;

procedure TCreatureOptions.SetRewardArtifact(AValue: AnsiString);
begin
  if FRewardArtifact=AValue then Exit;
  FRewardArtifact:=AValue;
end;

procedure TCreatureOptions.SetRewardMessage(AValue: TLocalizedString);
begin
  if FRewardMessage=AValue then Exit;
  FRewardMessage:=AValue;
end;

constructor TCreatureOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FReward := TRewards.Create(AObject);
  //FRewardResources := TRewardResourceSet.Create(FReward);
  FRewardResources := TIndepResourceSet.Create();
  FCharacter := TCreatureCharacter.aggressive;
end;

destructor TCreatureOptions.Destroy;
begin
  FRewardResources.Free;
  FReward.Free;
  inherited Destroy;
end;

{ THeroOptions }

procedure THeroOptions.AfterSerialize(Sender: TObject; AData: TJSONData);
begin
  inherited AfterSerialize(Sender, AData);

  SaveHeroSex(AData, Sex);
  MapObject.GetListsManager.SaveHeroPortrait(AData, FPortrait);
end;

procedure THeroOptions.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  inherited AfterSerialize(Sender, AData);

  Sex:=LoadHeroSex(AData);
  FPortrait:=MapObject.GetListsManager.LoadHeroPortrait(AData);
end;

procedure THeroOptions.Clear;
begin
  inherited Clear;
  &type := '';
  TightFormation := false;
  PatrolRadius := -1;
  Artifacts.Clear;
  Biography:='';
  Experience := 0;
  Name:='';
  Portrait:=-1;
  PrimarySkills.Clear;
  SecondarySkills.Clear;
  Sex := THeroSex.default;
end;

function THeroOptions.GetHeroIdentifier: AnsiString;
begin
  Result := &type;
end;

function THeroOptions.GetBiography: TLocalizedString;
begin
  Result := FBiography;
end;

function THeroOptions.GetSex: THeroSex;
begin
  Result := FSex;
end;

function THeroOptions.GetName: TLocalizedString;
begin
  Result := FName;
end;

function THeroOptions.GetPrimarySkills: THeroPrimarySkills;
begin
  Result := FPrimarySkills;
end;

function THeroOptions.GetSecondarySkills: THeroSecondarySkills;
begin
  Result := FSecondarySkills;
end;

procedure THeroOptions.SetExperience(const AValue: UInt64);
begin
  FExperience:=AValue;
end;

procedure THeroOptions.SetBiography(const AValue: TLocalizedString);
begin
  FBiography:=AValue;
end;

function THeroOptions.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function THeroOptions.GetTightFormation: Boolean;
begin
  Result := Army.TightFormation;
end;

function THeroOptions.IsArtifactsStored: Boolean;
begin
  Result := not FArtifacts.IsEmpty;
end;

function THeroOptions.GetExperience: UInt64;
begin
  Result := FExperience;
end;

function THeroOptions.IsSecondarySkillsStored: Boolean;
begin
  Result := FSecondarySkills.Count > 0;
end;

function THeroOptions.IsSpellBookStored: Boolean;
begin
  Result := FSpellBook.Count >0;
end;

procedure THeroOptions.SetType(AValue: AnsiString);
begin
  if FType = AValue then
    Exit;
  FObject.NotifyReferenced(FType, AValue);
  FObject.NotifyHeroTypeChanged(FType, AValue);
  FType:=AValue;
end;

procedure THeroOptions.SetName(const AValue: TLocalizedString);
begin
  FName:=AValue;
end;

function THeroOptions.GetPortrait: Int32;
begin
  Result := FPortrait;
end;

procedure THeroOptions.SetPortrait(const AValue: Int32);
begin
  FPortrait:=AValue;
end;

procedure THeroOptions.SetPatrolRadius(AValue: Integer);
begin
  FPatrolRadius:=AValue;
end;

procedure THeroOptions.SetSex(const AValue: THeroSex);
begin
  FSex:=AValue;
end;

procedure THeroOptions.SetTightFormation(AValue: Boolean);
begin
  Army.TightFormation:=AValue;
end;

constructor THeroOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FArtifacts := THeroArtifacts.Create(AObject);
  FExperience:=0;
  FSecondarySkills := THeroSecondarySkills.Create;
  FPatrolRadius := -1;
  FSex:=THeroSex.default;
  FSpellBook := TIdentifierList.Create(AObject);

  FPrimarySkills := THeroPrimarySkills.Create;
  FPortrait := -1;
end;

destructor THeroOptions.Destroy;
begin
  FPrimarySkills.Free;
  FSpellBook.Free;
  FSecondarySkills.Free;
  FArtifacts.Free;
  inherited Destroy;
end;

{ TLocalEventOptions }

constructor TLocalEventOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  Clear;
end;

procedure TLocalEventOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitLocalEvent(Self);
end;

procedure TLocalEventOptions.Clear;
begin
  inherited Clear;
  HumanActivable := true;
  AIActivable:=False;
  RemoveAfterVisit:=false;
  AvailableFor := ALL_PLAYERS;
end;

{ TSignBottleOptions }

procedure TSignBottleOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSignBottle(Self);
end;

procedure TSignBottleOptions.Clear;
begin
  inherited Clear;
  Text:='';
end;

procedure TSignBottleOptions.SetText(AValue: TLocalizedString);
begin
  FText := AValue;
end;

end.
