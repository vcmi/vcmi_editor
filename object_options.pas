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

unit object_options;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, editor_types, editor_classes, root_manager, editor_utils,
  object_link, logical_id_condition, vcmi_json, editor_consts, fpjson;

type

{$push}
{$m+}
  { TObjectOptions }
  TObjectOptions      = class;
  TObjectOptionsClass = class of TObjectOptions;
  IObjectOptionsVisitor = interface;

  IMapObject = interface
    function GetID: AnsiString;
    function GetSubId: AnsiString;
    procedure NotifyReferenced(AIdentifier: AnsiString);
  end;

  TObjectOptions = class(TObject, ISerializeNotify)
  private
    FObject: IMapObject;
    FOwner: TPlayer;
    procedure SetOwner(AValue: TPlayer);
  public//ISerializeNotify
    procedure BeforeDeSerialize(Sender: TObject; AData: TJSONData); virtual;
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData); virtual;
    procedure BeforeSerialize(Sender: TObject); virtual;
    procedure AfterSerialize(Sender: TObject; AData: TJSONData); virtual;
  public
    constructor Create(AObject: IMapObject); virtual;

    procedure ApplyVisitor({%H-}AVisitor: IObjectOptionsVisitor); virtual;

    property MapObject: IMapObject read FObject;
  public
    property Owner: TPlayer read FOwner write SetOwner;
  end;
{$pop}

  { TCreatureInstInfo }

  TCreatureInstInfo = class (TCollectionItem)
  private
    FAmount: Integer;
    FType: AnsiString;
    procedure SetAmount(AValue: Integer);
    procedure SetType(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;

    function IsEmptyType(): Boolean;
  published
    property &type: AnsiString read FType write SetType;
    property Amount: Integer read FAmount write SetAmount nodefault;
  end;

  { TCreatureSet }

  TCreatureSet = class (specialize TGArrayCollection<TCreatureInstInfo>)
  public
    constructor Create();
  end;

  {$push}
  {$m+}

  { TResourceSet }

  TResourceSet = class
  private
    Fvalue: array[TResType] of Integer;

    function GetAmount(AType: TResType): integer;
    procedure SetAmount(AType: TResType; AValue: integer);
  public
    property Amount[AType: TResType]: integer read GetAmount write SetAmount;
    function IsEmpty: Boolean;
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

  { TQuest }

  TQuestMission = (None=0, Level=1, PrimaryStat=2, KillHero=3, KillCreature=4, Artifact=5, Army=6, Resources=7, Hero=8, Player=9, Keymaster=10);

  TQuest = class
  private
    FArmy: TCreatureSet;
    FCompletedText: TLocalizedString;
    FFirstVisitText: TLocalizedString;
    FHeroID: AnsiString;
    FHeroLevel: Integer;
    FKillTarget: TObjectLink;
    FMissionType: TQuestMission;
    FNextVisitText: TLocalizedString;
    FPlayerID: TPlayer;
    FPrimarySkills: THeroPrimarySkills;
    FResources: TResourceSet;
    FTimeLimit: Integer;
    FArtifacts:TStringList;
    function GetArtifacts: TStrings;
    function IsArmyStored: Boolean;
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
    constructor Create;
    destructor Destroy; override;
  published
    property FirstVisitText: TLocalizedString read FFirstVisitText write SetFirstVisitText;
    property NextVisitText: TLocalizedString read FNextVisitText write SetNextVisitText;
    property CompletedText: TLocalizedString read FCompletedText write SetCompletedText;

    property MissionType: TQuestMission read FMissionType write SetMissionType default TQuestMission.None;
    property TimeLimit: Integer read FTimeLimit write SetTimeLimit default -1;

    property Artifacts: TStrings read GetArtifacts stored IsArtifactsStored;
    property Army: TCreatureSet read FArmy stored IsArmyStored;
    property Resources: TResourceSet read FResources stored IsResourcesStored;
    property PrimarySkills: THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property HeroLevel: Integer read FHeroLevel write SetHeroLevel  stored IsHeroLevelStored default -1;
    property Hero: AnsiString read FHeroID write SetHeroID stored IsHeroIDStored;
    property Player: TPlayer read FPlayerID write SetPlayerID stored IsPlayerIDStored default TPlayer.NONE ;

    property KillTarget: TObjectLink read FKillTarget stored IsKillTargetStored;
  end;

  { THeroArtifacts }

  THeroArtifacts = class
  private
    FSlots: array[0..ARTIFACT_SLOT_COUNT-1] of AnsiString;
    FBackpack: TStringList;
    function GetBackpack: TStrings;
    function GetBySlotNumber(ASlotID: Integer): AnsiString;
    procedure SetBySlotNumber(ASlotID: Integer; AValue: AnsiString);

  public
    constructor Create;
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

    property Backpack: TStrings read GetBackpack;
  end;

{$pop}

  { TCustomObjectOptions }

  TCustomObjectOptions = class(TObjectOptions)

  end;

  { TGuardedObjectOptions }

  TGuardedObjectOptions = class abstract (TObjectOptions)
  private
    FGuardMessage: TLocalizedString;
    FGuards: TCreatureSet;
    procedure SetGuardMessage(AValue: TLocalizedString);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
  published
    property Guards: TCreatureSet read FGuards;
    property GuardMessage:TLocalizedString read FGuardMessage write SetGuardMessage;
  end;

  { TOwnedObjectOptions }

  TOwnedObjectOptions = class (TObjectOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Owner default TPlayer.none;
  end;

  { TSignBottleOptions }

  TSignBottleOptions = class(TObjectOptions)
  private
    FText: TLocalizedString;
    procedure SetText(AValue: TLocalizedString);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Text: TLocalizedString read FText write SetText;
  end;

  { TPandorasOptions }

  TPandorasOptions = class (TGuardedObjectOptions)
  private
    FArtifacts: TStrings;
    FCreatures: TCreatureSet;
    FExperience: UInt64;
    FLuck: Int32;
    FMana: Int32;
    FMorale: Int32;
    FPrimarySkills: THeroPrimarySkills;
    FResources: TResourceSet;
    FSecondarySkills: THeroSecondarySkills;
    FSpells: TStrings;
    function IsArtifactsStored: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsResourcesStored: Boolean;
    function IsSecondarySkillsStored: Boolean;
    function IsSpellsStored: Boolean;
    procedure SetExperience(AValue: UInt64);
    procedure SetLuck(AValue: Int32);
    procedure SetMana(AValue: Int32);
    procedure SetMorale(AValue: Int32);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Creatures: TCreatureSet read FCreatures;

    property Experience: UInt64 read FExperience write SetExperience default 0;
    property Mana: Int32 read FMana write SetMana default 0;
    property Morale: Int32 read FMorale write SetMorale default 0;
    property Luck: Int32 read FLuck write SetLuck default 0;

    property Resources: TResourceSet read FResources stored IsResourcesStored;
    property PrimarySkills: THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property SecondarySkills: THeroSecondarySkills read FSecondarySkills stored IsSecondarySkillsStored;

    property Artifacts: TStrings read FArtifacts stored IsArtifactsStored;
    property Spells:TStrings read FSpells stored IsSpellsStored;
  end;

  { TLocalEventOptions }

  TLocalEventOptions = class(TPandorasOptions)
  private
    FAIActivable: boolean;
    FAvailableFor: TPlayers;
    FHumanActivable: boolean;
    FRemoveAfterVisit: Boolean;
    procedure SetAIActivable(AValue: boolean);
    procedure SetAvailableFor(AValue: TPlayers);
    procedure SetHumanActivable(AValue: boolean);
    procedure SetRemoveAfterVisit(AValue: Boolean);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property HumanActivable: boolean read FHumanActivable write SetHumanActivable default True;
    property AIActivable: boolean read FAIActivable write SetAIActivable default False;
    property RemoveAfterVisit: Boolean read FRemoveAfterVisit write SetRemoveAfterVisit default False;
    property AvailableFor: TPlayers read FAvailableFor write SetAvailableFor default ALL_PLAYERS;
  end;

  { THeroOptions }

  THeroOptions = class(TOwnedObjectOptions)
  private
    FArmy: TCreatureSet;
    FArtifacts: THeroArtifacts;
    FBiography: TLocalizedString;
    FExperience: UInt64;
    FId: AnsiString;
    FName: TLocalizedString;
    FPatrolRadius: Integer;
    FPortrait: AnsiString;
    FPrimarySkills: THeroPrimarySkills;
    FSex: THeroSex;
    FSecondarySkills: THeroSecondarySkills;
    FSpellBook: TStrings;
    FTightFormation: Boolean;
    function IsPrimarySkillsStored: Boolean;
    function IsSecondarySkillsStored: Boolean;
    function IsSpellBookStored: Boolean;
    procedure SetBiography(AValue: TLocalizedString);
    procedure SetExperience(AValue: UInt64);
    procedure SetId(AValue: AnsiString);
    procedure SetName(AValue: TLocalizedString);
    procedure SetPatrolRadius(AValue: Integer);
    procedure SetPortrait(AValue: AnsiString);
    procedure SetSex(AValue: THeroSex);
    procedure SetTightFormation(AValue: Boolean);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

    procedure AfterSerialize(Sender: TObject; AData: TJSONData); override;
    procedure AfterDeSerialize(Sender: TObject; AData: TJSONData); override;
  published
    property &type: AnsiString read FId write SetId;
    property Portrait: AnsiString read FPortrait write SetPortrait;
    property Army: TCreatureSet read FArmy;
    property Artifacts: THeroArtifacts read FArtifacts;
    property Experience: UInt64 read FExperience write SetExperience default 0;
    property Name: TLocalizedString read FName write SetName;
    property Biography: TLocalizedString read FBiography write SetBiography;
    property PrimarySkills:THeroPrimarySkills read FPrimarySkills stored IsPrimarySkillsStored;
    property SecondarySkills: THeroSecondarySkills read FSecondarySkills stored IsSecondarySkillsStored;
    property TightFormation: Boolean read FTightFormation write SetTightFormation;

    property PatrolRadius: Integer read FPatrolRadius write SetPatrolRadius default -1;
    property SpellBook: TStrings read FSpellBook stored IsSpellBookStored;
  public //manual streaming
     property Sex: THeroSex read FSex write SetSex;
  end;

  { TMonsterOptions }

  TMonsterOptions = class(TObjectOptions)
  private
    FCharacter: Integer;
    FCount: Integer;
    FNeverFlees: boolean;
    FNoGrowing: boolean;
    FRewardArtifact: AnsiString;
    FRewardMessage: TLocalizedString;
    FRewardResources: TResourceSet;
    function IsRewardArtifactStored: Boolean;
    function IsRewardMessageStored: Boolean;
    function IsRewardResourcesStored: Boolean;
    procedure SetCharacter(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetNeverFlees(AValue: boolean);
    procedure SetNoGrowing(AValue: boolean);
    procedure SetRewardArtifact(AValue: AnsiString);
    procedure SetRewardMessage(AValue: TLocalizedString);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property NeverFlees: boolean read FNeverFlees write SetNeverFlees default False;
    property NoGrowing: boolean read FNoGrowing write SetNoGrowing default False;
    property Count: Integer read FCount write SetCount default 0; //0=random
    property Character: Integer read FCharacter write SetCharacter nodefault;

    property RewardMessage: TLocalizedString read FRewardMessage write SetRewardMessage stored IsRewardMessageStored;
    property RewardResources: TResourceSet read FRewardResources stored IsRewardResourcesStored;
    property RewardArtifact: AnsiString read FRewardArtifact write SetRewardArtifact stored IsRewardArtifactStored;
  end;

  { TSeerHutOptions }

  TSeerHutOptions = class(TObjectOptions)
  private
    FQuest: TQuest;
    FRewardID: AnsiString;
    FRewardType: TSeerHutReward;
    FRewardValue: Integer;
    procedure SetRewardID(AValue: AnsiString);
    procedure SetRewardType(AValue: TSeerHutReward);
    procedure SetRewardValue(AValue: Integer);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Quest: TQuest read FQuest;

    property RewardType: TSeerHutReward read FRewardType write SetRewardType default TSeerHutReward.NOTHING;
    property Reward: AnsiString read FRewardID write SetRewardID;
    property RewardValue: Integer read FRewardValue write SetRewardValue default 0;
  end;

  { TWitchHutOptions }

  TWitchHutOptions = class(TObjectOptions)
  private
    FAllowedSkills: TStringList;
    function GetAllowedSkills: TStrings;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

  published
    property AllowedSkills: TStrings read GetAllowedSkills;
  end;

  { TScholarOptions }

  TScholarOptions = class(TObjectOptions)
  private
    FRewardPrimSkill: AnsiString;
    FRewardSecSkill: AnsiString;
    FRewardSpell: AnsiString;
    procedure SetRewardPrimSkill(AValue: AnsiString);
    procedure SetRewardSecSkill(AValue: AnsiString);
    procedure SetRewardSpell(AValue: AnsiString);
  public
    constructor Create(AObject: IMapObject); override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

    procedure Clear;
  published
    property RewardPrimSkill: AnsiString read FRewardPrimSkill write SetRewardPrimSkill;
    property RewardSkill: AnsiString read FRewardSecSkill write SetRewardSecSkill;
    property RewardSpell: AnsiString read FRewardSpell write SetRewardSpell;

  end;

  { TGarrisonOptions }

  TGarrisonOptions = class(TOwnedObjectOptions)
  private
    FArmy: TCreatureSet;
    FRemovableUnits: Boolean;
    procedure SetRemovableUnits(AValue: Boolean);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Army: TCreatureSet read FArmy;
    property RemovableUnits: Boolean read FRemovableUnits write SetRemovableUnits;
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
  published
    property Spell: AnsiString read FSpell write SetSpell;
  end;

  { TResourceOptions }

  TResourceOptions = class(TGuardedObjectOptions)
  private
    FAmount: Integer;
    procedure SetAmount(AValue: Integer);
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Amount: Integer read FAmount write SetAmount;
  end;

  { TTownOptions }

  TTownOptions = class(TOwnedObjectOptions)
  private
    FBuildings: TLogicalIDCondition;
    FArmy: TCreatureSet;
    FName: TLocalizedString;
    FSpells: TLogicalIDCondition;
    FTightFormation: Boolean;
    procedure SetName(AValue: TLocalizedString);
    procedure SetTightFormation(AValue: Boolean);
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Army: TCreatureSet read FArmy;
    property TightFormation: Boolean read FTightFormation write SetTightFormation;
    property Name: TLocalizedString read FName write SetName;
    property Spells: TLogicalIDCondition read FSpells;
    property Buildings: TLogicalIDCondition read FBuildings;
  end;

  { TAbandonedOptions }

  TAbandonedOptions = class(TObjectOptions)
  private
    FPossibleResources: TStrings;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

  published
    property PossibleResources: TStrings read FPossibleResources;
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
  published
    property Radius: Integer read FRadius write SetRadius;
  end;

  { TBaseRandomDwellingOptions }

  TBaseRandomDwellingOptions = class abstract (TObjectOptions)
  private
    FAllowedFactions: TStringList;
    FLinked: boolean;
    FMaxLevel: UInt8;
    FMinLevel: UInt8;
    FSameAsTown: TObjectLink;
    function GetAllowedFactions: TStrings;
    function IsSameAsTownStored: Boolean;
    procedure SetLinked(AValue: boolean);
    procedure SetMaxLevel(AValue: UInt8);
    procedure SetMinLevel(AValue: UInt8);
    function IsAllowedFactionsStored: boolean;
  public
    constructor Create(AObject: IMapObject); override;
    destructor Destroy; override;

    property MinLevel: UInt8 read FMinLevel write SetMinLevel default 0;
    property MaxLevel: UInt8 read FMaxLevel write SetMaxLevel default 7;

    property AllowedFactions: TStrings read GetAllowedFactions stored IsAllowedFactionsStored;

    property Linked: boolean read FLinked write SetLinked;

    property SameAsTown: TObjectLink read FSameAsTown stored IsSameAsTownStored;
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
    property Linked;
    property SameAsTown;
  end;

  { TRandomDwellingLVLOptions }

  TRandomDwellingLVLOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property AllowedFactions;
    property Linked;
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
  published
    property &Type: AnsiString read FTypeID write SetTypeID;
    property Power: UInt8 read FPower write SetPower default 0;
    property Owner default TPlayer.none;
  end;


  { IObjectOptionsVisitor }

  IObjectOptionsVisitor = interface
    procedure VisitLocalEvent(AOptions: TLocalEventOptions);
    procedure VisitSignBottle(AOptions: TSignBottleOptions);
    procedure VisitHero(AOptions: THeroOptions);
    procedure VisitMonster(AOptions: TMonsterOptions);
    procedure VisitSeerHut(AOptions: TSeerHutOptions);
    procedure VisitWitchHut(AOptions: TWitchHutOptions);
    procedure VisitScholar(AOptions: TScholarOptions);
    procedure VisitGarrison(AOptions: TGarrisonOptions);
    procedure VisitArtifact(AOptions: TArtifactOptions);
    procedure VisitSpellScroll(AOptions: TSpellScrollOptions);
    procedure VisitResource(AOptions: TResourceOptions);
    procedure VisitTown(AOptions: TTownOptions);
    procedure VisitAbandonedMine(AOptions:TAbandonedOptions);
    procedure VisitShrine(AOptions: TShrineOptions);
    procedure VisitPandorasBox(AOptions: TPandorasOptions);
    procedure VisitGrail(AOptions: TGrailOptions);
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions);
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions);
    procedure VisitQuestGuard(AOptions:TQuestGuardOptions);
    procedure VisitHeroPlaseholder(AOptions: THeroPlaceholderOptions);

    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions);
  end;

function CreateByID(ID: AnsiString; SubID: AnsiString; AObject: IMapObject): TObjectOptions;

implementation


function CreateByID(ID: AnsiString; SubID: AnsiString;AObject: IMapObject): TObjectOptions;
var
  c: TObjectOptionsClass;
begin
  c := TObjectOptions;

  case ID of
    'event':
      c := TLocalEventOptions;
    'oceanBottle', 'sign':
      c := TSignBottleOptions;
    'hero', 'randomHero', 'prison':
      c := THeroOptions;
    'monster',
    'randomMonster',
    'randomMonsterLevel1',
    'randomMonsterLevel2',
    'randomMonsterLevel3',
    'randomMonsterLevel4',
    'randomMonsterLevel5',
    'randomMonsterLevel6',
    'randomMonsterLevel7':
      c := TMonsterOptions;
    'seerHut':
      c := TSeerHutOptions;
    'witchHut':
      c := TWitchHutOptions;
    'scholar':
      c := TScholarOptions;
    'garrisonHorizontal', 'garrisonVertical':
      c := TGarrisonOptions;

    'artifact', 'randomArtifact',
    'randomArtifactTreasure',
    'randomArtifactMinor',
    'randomArtifactMajor',
    'randomArtifactRelic':
      c := TArtifactOptions;
    'spellScroll':
      c := TSpellScrollOptions;
    'resource',
    'randomResource':
      c := TResourceOptions;
    'randomTown', 'town':
      c := TTownOptions;

    'creatureGeneratorCommon',

    //todo:  CREATURE_GENERATOR2, CREATURE_GENERATOR3
    //CREATURE_GENERATOR2,
    //CREATURE_GENERATOR3,

    'creatureGeneratorSpecial':
      c := TOwnedObjectOptions;
    'mine':
    begin
      if SubID = 'abandoned' then
      begin
        c := TAbandonedOptions;
      end else
      begin
        c := TOwnedObjectOptions;
      end;
    end;

    'abandonedMine':
      c := TAbandonedOptions;
    'shrineOfMagicLevel1':
      c := TShrine1Options;
    'shrineOfMagicLevel2':
      c := TShrine2Options;
    'shrineOfMagicLevel3':
      c := TShrine3Options;
    'pandoraBox':
      c := TPandorasOptions;
    'grail':
      c := TGrailOptions;
    'randomDwelling':
      c := TRandomDwellingOptions;
    'randomDwellingLvl':
      c := TRandomDwellingLVLOptions;
    'randomDwellingFaction':
      c := TRandomDwellingTownOptions;
    'questGuard':
      c := TQuestGuardOptions;
    'shipyard','lighthouse':
      c := TOwnedObjectOptions;
  end;

  Result := c.Create(AObject);
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

constructor THeroArtifacts.Create;
begin
  FBackpack := TStringList.Create;
  FBackpack.Sorted := false;
  FBackpack.Duplicates:=dupAccept;
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

function TResourceSet.GetAmount(AType: TResType): integer;
begin
  Result :=  Fvalue[AType];
end;

procedure TResourceSet.SetAmount(AType: TResType; AValue: integer);
begin
  Fvalue[AType] := AValue;
end;

function TResourceSet.IsEmpty: Boolean;
var
  v: Integer;
begin
  for v in Fvalue do
    if v>0 then
      Exit(False);
  Exit(true)
end;

{ TAbandonedOptions }

constructor TAbandonedOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FPossibleResources := CrStrList;
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

{ TQuest }

procedure TQuest.SetCompletedText(AValue: TLocalizedString);
begin
  FCompletedText := AValue;
end;

function TQuest.GetArtifacts: TStrings;
begin
  Result := FArtifacts;
end;

function TQuest.IsArmyStored: Boolean;
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
  FHeroID:=AValue;
end;

procedure TQuest.SetHeroLevel(AValue: Integer);
begin
  if FHeroLevel=AValue then Exit;
  FHeroLevel:=AValue;
end;

procedure TQuest.SetMissionType(AValue: TQuestMission);
begin
  if FMissionType=AValue then Exit;
  FMissionType:=AValue;

  case FMissionType of
    TQuestMission.KillCreature: FKillTarget.&type:=TYPE_MONSTER;
    TQuestMission.KillHero:FKillTarget.&type:=TYPE_HERO;
  end;
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

constructor TQuest.Create;
begin
  TimeLimit := -1;
  FArtifacts := TStringList.Create;
  FArmy := TCreatureSet.Create();
  FResources := TResourceSet.Create;
  FPrimarySkills := THeroPrimarySkills.Create;
  FHeroLevel := -1;
  FPlayerID:=TPlayer.NONE;
  FKillTarget := TObjectLink.Create;
end;

destructor TQuest.Destroy;
begin
  FKillTarget.Free;
  FPrimarySkills.Free;
  FResources.Free;
  FArmy.Free;
  FArtifacts.Free;
  inherited Destroy;
end;

{ THeroPlaceholderOptions }

procedure THeroPlaceholderOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitHeroPlaseholder(Self);
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

{ TQuestGuardOptions }

procedure TQuestGuardOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitQuestGuard(Self);
end;

constructor TQuestGuardOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FQuest := TQuest.Create;
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

function TBaseRandomDwellingOptions.GetAllowedFactions: TStrings;
begin
  Result := FAllowedFactions;
end;

function TBaseRandomDwellingOptions.IsSameAsTownStored: Boolean;
begin
  Result := Linked;
end;

procedure TBaseRandomDwellingOptions.SetLinked(AValue: boolean);
begin
  if FLinked=AValue then Exit;
  FLinked:=AValue;
end;

procedure TBaseRandomDwellingOptions.SetMinLevel(AValue: UInt8);
begin
  if FMinLevel=AValue then Exit;
  FMinLevel:=AValue;
end;

constructor TBaseRandomDwellingOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FAllowedFactions := TStringList.Create;
  FAllowedFactions.Sorted := True;
  FAllowedFactions.Duplicates := dupIgnore;

  RootManager.ListsManager.FactionInfos.FillWithAllIds(FAllowedFactions);
  FSameAsTown := TObjectLink.Create;
  FSameAsTown.&type := TYPE_TOWN;
end;

destructor TBaseRandomDwellingOptions.Destroy;
begin
  FSameAsTown.Free;
  FAllowedFactions.Free;
  inherited Destroy;
end;

function TBaseRandomDwellingOptions.IsAllowedFactionsStored: boolean;
begin
  Result := not Linked;
end;

{ TCreatureInstInfo }

constructor TCreatureInstInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

function TCreatureInstInfo.IsEmptyType: Boolean;
begin
  Result := FType = '';
end;

procedure TCreatureInstInfo.SetAmount(AValue: Integer);
begin
  FAmount := AValue;
end;

procedure TCreatureInstInfo.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;
  FType:=AValue;
end;

{ TGuardedObjectOptions }

constructor TGuardedObjectOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FGuards := TCreatureSet.Create();
end;

destructor TGuardedObjectOptions.Destroy;
begin
  FGuards.Free;
  inherited Destroy;
end;

procedure TGuardedObjectOptions.SetGuardMessage(AValue: TLocalizedString);
begin
  FGuardMessage := AValue;
end;

{ TCreatureSet }

constructor TCreatureSet.Create();
begin
  inherited Create;
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

procedure TGrailOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitGrail(Self);
end;

procedure TGrailOptions.SetRadius(AValue: Integer);
begin
  FRadius := AValue;
end;

{ TPandorasOptions }

procedure TPandorasOptions.SetExperience(AValue: UInt64);
begin
  if FExperience=AValue then Exit;
  FExperience:=AValue;
end;

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

function TPandorasOptions.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function TPandorasOptions.IsArtifactsStored: Boolean;
begin
  Result := FArtifacts.Count > 0;
end;

procedure TPandorasOptions.SetLuck(AValue: Int32);
begin
  if FLuck=AValue then Exit;
  FLuck:=AValue;
end;

procedure TPandorasOptions.SetMana(AValue: Int32);
begin
  if FMana=AValue then Exit;
  FMana:=AValue;
end;

procedure TPandorasOptions.SetMorale(AValue: Int32);
begin
  if FMorale=AValue then Exit;
  FMorale:=AValue;
end;

constructor TPandorasOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FCreatures := TCreatureSet.Create();
  FResources := TResourceSet.Create;
  FPrimarySkills := THeroPrimarySkills.Create;
  FSecondarySkills := THeroSecondarySkills.Create;
  FArtifacts := TStringList.Create;
  FSpells := TStringList.Create;
end;

destructor TPandorasOptions.Destroy;
begin
  FSpells.Free;
  FArtifacts.Free;
  FSecondarySkills.free;
  FPrimarySkills.free;
  FResources.Free;
  FreeAndNil(FCreatures);
  inherited Destroy;
end;

procedure TPandorasOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitPandorasBox(Self);
end;

{ TShrineOptions }

procedure TShrineOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitShrine(Self);
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

constructor TTownOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FArmy := TCreatureSet.Create();
  FSpells := TLogicalIDCondition.Create;
  FBuildings := TLogicalIDCondition.Create;
end;

destructor TTownOptions.Destroy;
begin
  FBuildings.Free;
  FSpells.Free;
  FArmy.Free;
  inherited Destroy;
end;

procedure TTownOptions.SetName(AValue: TLocalizedString);
begin
  FName := AValue;
end;

procedure TTownOptions.SetTightFormation(AValue: Boolean);
begin
  if FTightFormation=AValue then Exit;
  FTightFormation:=AValue;
end;

{ TResourceOptions }

procedure TResourceOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitResource(Self);
end;

procedure TResourceOptions.SetAmount(AValue: Integer);
begin
  FAmount := AValue;
end;

{ TSpellScrollOptions }

procedure TSpellScrollOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSpellScroll(Self);
end;

constructor TSpellScrollOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FSpell := 'magicArrow';
end;

procedure TSpellScrollOptions.SetSpell(AValue: AnsiString);
begin
  if FSpell = AValue then Exit;
  FSpell := AValue;
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

constructor TGarrisonOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FArmy := TCreatureSet.Create();
end;

destructor TGarrisonOptions.Destroy;
begin
  FArmy.Free;
  inherited Destroy;
end;

procedure TGarrisonOptions.SetRemovableUnits(AValue: Boolean);
begin
  FRemovableUnits := AValue;
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
  FRewardSpell:=AValue;
end;

constructor TScholarOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
end;

procedure TScholarOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitScholar(Self);
end;

procedure TScholarOptions.Clear;
begin
  FRewardPrimSkill := '';
  FRewardSecSkill := '';
  FRewardSpell := '';
end;

{ TWitchHutOptions }

procedure TWitchHutOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitWitchHut(Self);
end;

constructor TWitchHutOptions.Create(AObject: IMapObject);
var
  i: Integer;
begin
  inherited Create(AObject);
  FAllowedSkills := TStringList.Create;
  FAllowedSkills.Sorted := True;
  FAllowedSkills.Duplicates := dupIgnore;

  //todo: make it configurable
  for i := 0 to SECONDARY_SKILL_QUANTITY - 1 do
  begin
    FAllowedSkills.Add(SECONDARY_SKILL_NAMES[i]);
  end;
  FAllowedSkills.Delete(FAllowedSkills.IndexOf(SECONDARY_SKILL_NAMES[6]));
  FAllowedSkills.Delete(FAllowedSkills.IndexOf(SECONDARY_SKILL_NAMES[12]));
end;

destructor TWitchHutOptions.Destroy;
begin
  FAllowedSkills.Free;
  inherited Destroy;
end;

function TWitchHutOptions.GetAllowedSkills: TStrings;
begin
  Result := FAllowedSkills;
end;

{ TSeerHutOptions }

procedure TSeerHutOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSeerHut(self);
end;

procedure TSeerHutOptions.SetRewardType(AValue: TSeerHutReward);
begin
  if FRewardType=AValue then Exit;
  FRewardType:=AValue;
end;

procedure TSeerHutOptions.SetRewardValue(AValue: Integer);
begin
  if FRewardValue=AValue then Exit;
  FRewardValue:=AValue;
end;

procedure TSeerHutOptions.SetRewardID(AValue: AnsiString);
begin
  if FRewardID=AValue then Exit;
  FRewardID:=AValue;
end;

constructor TSeerHutOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FQuest := TQuest.Create;
end;

destructor TSeerHutOptions.Destroy;
begin
  FQuest.Free;
  inherited Destroy;
end;

{ TMonsterOptions }

procedure TMonsterOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitMonster(Self);
end;

procedure TMonsterOptions.SetNeverFlees(AValue: boolean);
begin
  FNeverFlees := AValue;
end;

procedure TMonsterOptions.SetCount(AValue: Integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
end;

function TMonsterOptions.IsRewardArtifactStored: Boolean;
begin
  Result := (FRewardArtifact <> '');
end;

function TMonsterOptions.IsRewardMessageStored: Boolean;
begin
  Result := (FRewardMessage <> '');
end;

function TMonsterOptions.IsRewardResourcesStored: Boolean;
begin
  Result := (not FRewardResources.IsEmpty);
end;

procedure TMonsterOptions.SetCharacter(AValue: Integer);
begin
  if FCharacter=AValue then Exit;
  FCharacter:=AValue;
end;

procedure TMonsterOptions.SetNoGrowing(AValue: boolean);
begin
  FNoGrowing := AValue;
end;

procedure TMonsterOptions.SetRewardArtifact(AValue: AnsiString);
begin
  if FRewardArtifact=AValue then Exit;
  FRewardArtifact:=AValue;
end;

procedure TMonsterOptions.SetRewardMessage(AValue: TLocalizedString);
begin
  if FRewardMessage=AValue then Exit;
  FRewardMessage:=AValue;
end;

constructor TMonsterOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FRewardResources := TResourceSet.Create;
end;

destructor TMonsterOptions.Destroy;
begin
  FRewardResources.Free;
  inherited Destroy;
end;

{ THeroOptions }

procedure THeroOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitHero(Self);
end;

procedure THeroOptions.AfterSerialize(Sender: TObject; AData: TJSONData);
begin
  inherited AfterSerialize(Sender, AData);

  SaveHeroSex(AData, Sex);
end;

procedure THeroOptions.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  inherited AfterSerialize(Sender, AData);

  Sex:=LoadHeroSex(AData);
end;

procedure THeroOptions.SetExperience(AValue: UInt64);
begin
  if FExperience=AValue then Exit;
  FExperience:=AValue;
end;

procedure THeroOptions.SetBiography(AValue: TLocalizedString);
begin
  if FBiography=AValue then Exit;
  FBiography:=AValue;
end;

function THeroOptions.IsPrimarySkillsStored: Boolean;
begin
  Result := not FPrimarySkills.IsDefault;
end;

function THeroOptions.IsSecondarySkillsStored: Boolean;
begin
  Result := FSecondarySkills.Count > 0;
end;

function THeroOptions.IsSpellBookStored: Boolean;
begin
  Result := FSpellBook.Count >0;
end;

procedure THeroOptions.SetId(AValue: AnsiString);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure THeroOptions.SetName(AValue: TLocalizedString);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure THeroOptions.SetPatrolRadius(AValue: Integer);
begin
  if FPatrolRadius=AValue then Exit;
  FPatrolRadius:=AValue;
end;

procedure THeroOptions.SetPortrait(AValue: AnsiString);
begin
  if FPortrait=AValue then Exit;
  FPortrait:=AValue;
end;

procedure THeroOptions.SetSex(AValue: THeroSex);
begin
  if FSex=AValue then Exit;
  FSex:=AValue;
end;

procedure THeroOptions.SetTightFormation(AValue: Boolean);
begin
  if FTightFormation=AValue then Exit;
  FTightFormation:=AValue;
end;

constructor THeroOptions.Create(AObject: IMapObject);
begin
  inherited Create(AObject);
  FArmy := TCreatureSet.Create();
  FArtifacts := THeroArtifacts.Create;
  FExperience:=0;
  FSecondarySkills := THeroSecondarySkills.Create;
  FPatrolRadius := -1;
  FSex:=THeroSex.default;
  FSpellBook := CrStrList;

  FPrimarySkills := THeroPrimarySkills.Create;
end;

destructor THeroOptions.Destroy;
begin
  FPrimarySkills.Free;
  FSpellBook.Free;
  FSecondarySkills.Free;
  FArtifacts.Free;
  FArmy.Free;
  inherited Destroy;
end;

{ TLocalEventOptions }

procedure TLocalEventOptions.SetAIActivable(AValue: boolean);
begin
  if FAIActivable=AValue then Exit;
  FAIActivable:=AValue;
end;

procedure TLocalEventOptions.SetAvailableFor(AValue: TPlayers);
begin
  if FAvailableFor=AValue then Exit;
  FAvailableFor:=AValue;
end;

procedure TLocalEventOptions.SetHumanActivable(AValue: boolean);
begin
  if FHumanActivable=AValue then Exit;
  FHumanActivable:=AValue;
end;

procedure TLocalEventOptions.SetRemoveAfterVisit(AValue: Boolean);
begin
  if FRemoveAfterVisit=AValue then Exit;
  FRemoveAfterVisit:=AValue;
end;

procedure TLocalEventOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitLocalEvent(Self);
end;

{ TSignBottleOptions }

procedure TSignBottleOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitSignBottle(Self);
end;

procedure TSignBottleOptions.SetText(AValue: TLocalizedString);
begin
  FText := AValue;
end;

{ TObjectOptions }

constructor TObjectOptions.Create(AObject: IMapObject);
begin
  FOwner := TPlayer.none;
  FObject := AObject;
end;

procedure TObjectOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  //do nothing here
end;

procedure TObjectOptions.SetOwner(AValue: TPlayer);
begin
  if not (AValue in [TPlayer.RED..TPlayer.PINK,TPlayer.NONE]) then
  begin
    raise Exception.CreateFmt('Invalid player color %d',[Integer(AValue)]);
  end;
  FOwner := AValue;
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

end.
