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
  Classes, SysUtils, editor_types, editor_classes, root_manager;

type

{$push}
{$m+}
  { TObjectOptions }
  TObjectOptions      = class;
  TObjectOptionsClass = class of TObjectOptions;
  IObjectOptionsVisitor = interface;


  TObjectOptions = class
  private
    FOwner: TPlayer;
    procedure SetOwner(AValue: TPlayer);
  public
    constructor Create; virtual;

    class function MayBeOwned: Boolean; virtual;

    procedure ApplyVisitor({%H-}AVisitor: IObjectOptionsVisitor); virtual;

  published
    property Owner: TPlayer read FOwner write SetOwner stored MayBeOwned default TPlayer.none;
  end;
{$pop}

  { TCreatureInstInfo }

  TCreatureInstInfo = class (TCollectionItem)
  private
    FCreCount: Integer;
    FCreID: AnsiString;
    FRandomCount: boolean;
    procedure SetCreCount(AValue: Integer);
    procedure SetCreID(AValue: AnsiString);
    procedure SetRandomCount(AValue: boolean);
  public
    constructor Create(ACollection: TCollection); override;
  published
    property CreID: AnsiString read FCreID write SetCreID;
    property CreCount: Integer read FCreCount write SetCreCount default 0;
    property RandomCount: boolean read FRandomCount write SetRandomCount default False;
  end;

  { TCreatureSet }

  TCreatureSet = class (specialize TGArrayCollection<TCreatureInstInfo>)
  private
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer);
    property MaxSize: Integer read FMaxSize;
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
    FMissionType: TQuestMission;
    FNextVisitText: TLocalizedString;
    FResources: TResourceSet;
    FTimeLimit: Integer;
    FArtifacts:TStringList;
    function GetArtifacts: TStrings;
    function IsArmyStored: Boolean;
    function IsArtifactsStored: Boolean;
    function IsResourcesStored: Boolean;
    procedure SetCompletedText(AValue: TLocalizedString);
    procedure SetFirstVisitText(AValue: TLocalizedString);
    procedure SetMissionType(AValue: TQuestMission);
    procedure SetNextVisitText(AValue: TLocalizedString);
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
  end;
{$pop}

  { TGuardedObjectOptions }

  TGuardedObjectOptions = class abstract (TObjectOptions)
  private
    FGuardMessage: TLocalizedString;
    FGuards: TCreatureSet;
    procedure SetGuardMessage(AValue: TLocalizedString);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Guards: TCreatureSet read FGuards;
    property GuardMessage:TLocalizedString read FGuardMessage write SetGuardMessage;
  end;

  { TOwnedObjectOptions }

  TOwnedObjectOptions = class (TObjectOptions)
  public
    class function MayBeOwned: Boolean; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
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
    FCreatures: TCreatureSet;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Creatures: TCreatureSet read FCreatures;
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
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Army: TCreatureSet read FArmy;
  end;

  { TMonsterOptions }

  TMonsterOptions = class(TObjectOptions)
  private
    FCharacter: Integer;
    FCount: Integer;
    FHasReward: Boolean;
    FNeverFlees: boolean;
    FNoGrowing: boolean;
    FRewardArtifact: AnsiString;
    FRewardMessage: TLocalizedString;
    FRewardResources: TResourceSet;
    function GetHasReward: Boolean;
    function IsRewardArtifactStored: Boolean;
    function IsRewardMessageStored: Boolean;
    function IsRewardResourcesStored: Boolean;
    procedure SetCharacter(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetHasReward(AValue: Boolean);
    procedure SetNeverFlees(AValue: boolean);
    procedure SetNoGrowing(AValue: boolean);
    procedure SetRewardArtifact(AValue: AnsiString);
    procedure SetRewardMessage(AValue: TLocalizedString);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property NeverFlees: boolean read FNeverFlees write SetNeverFlees default False;
    property NoGrowing: boolean read FNoGrowing write SetNoGrowing default False;
    property Count: Integer read FCount write SetCount nodefault;
    property Character: Integer read FCharacter write SetCharacter nodefault;

    property HasReward: Boolean read GetHasReward write SetHasReward;

    property RewardMessage: TLocalizedString read FRewardMessage write SetRewardMessage stored IsRewardMessageStored;
    property RewardResources: TResourceSet read FRewardResources stored IsRewardResourcesStored;
    property RewardArtifact: AnsiString read FRewardArtifact write SetRewardArtifact stored IsRewardArtifactStored;
  end;

  { TSeerHutOptions }

  TSeerHutOptions = class(TObjectOptions)
  private
    FQuest: TQuest;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Quest: TQuest read FQuest;
  end;

  { TWitchHutOptions }

  TWitchHutOptions = class(TObjectOptions)
  private
    FAllowedSkills: TStringList;
    function GetAllowedSkills: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;

  published
    property AllowedSkills: TStrings read GetAllowedSkills;
  end;

  TScholarBonus = (primSkill=0,skill=1,spell=2, random=$ff);

  { TScholarOptions }

  TScholarOptions = class(TObjectOptions)
  private
    FBonusType: TScholarBonus;
    FBonusId: AnsiString;
    function IsBonusIdStored: Boolean;
    procedure SetBonusType(AValue: TScholarBonus);
    procedure SetBonusId(AValue: AnsiString);
  public
    constructor Create; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property BonusType: TScholarBonus read FBonusType write SetBonusType;
    property BonusId: AnsiString read FBonusId write SetBonusId stored IsBonusIdStored;
  end;

  { TGarrisonOptions }

  TGarrisonOptions = class(TOwnedObjectOptions)
  private
    FGarrison: TCreatureSet;
    FRemovableUnits: Boolean;
    procedure SetRemovableUnits(AValue: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Garrison: TCreatureSet read FGarrison;
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
    FSpellID: AnsiString;
    procedure SetSpellID(AValue: AnsiString);
  public
    constructor Create; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property SpellID: AnsiString read FSpellID write SetSpellID;
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
    FGarrison: TCreatureSet;
    FName: TLocalizedString;
    procedure SetName(AValue: TLocalizedString);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Garrison: TCreatureSet read FGarrison;
    property Name: TLocalizedString read FName write SetName;
  end;

  { TAbandonedOptions }

  TAbandonedOptions = class(TObjectOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  end;

  { TShrineOptions }

  TShrineOptions = class abstract (TObjectOptions)
  private
    FIsRandom: Boolean;
    FSpellID: AnsiString;
    FSpellLevel: Integer;

    function IsSpellIDStored: Boolean;
    procedure SetIsRandom(AValue: Boolean);
    procedure SetSpellID(AValue: AnsiString);
  strict protected
    class function GetSpellLevel: Integer; virtual; abstract;
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    property SpellLevel: Integer read GetSpellLevel;
  published
    property IsRandom: Boolean read FIsRandom write SetIsRandom default False;
    property SpellID: AnsiString read FSpellID write SetSpellID stored IsSpellIDStored;
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
    function GetAllowedFactions: TStrings;
    procedure SetLinked(AValue: boolean);
    procedure SetMaxLevel(AValue: UInt8);
    procedure SetMinLevel(AValue: UInt8);
    function IsAllowedFactionsStored: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function MayBeOwned: Boolean; override;

    property MinLevel: UInt8 read FMinLevel write SetMinLevel default 0;
    property MaxLevel: UInt8 read FMaxLevel write SetMaxLevel default 7;

    property AllowedFactions: TStrings read GetAllowedFactions stored IsAllowedFactionsStored;

    property Linked: boolean read FLinked write SetLinked;

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
  end;

  { TRandomDwellingLVLOptions }

  TRandomDwellingLVLOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
    property AllowedFactions;
    property Linked;
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
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property Quest: TQuest read FQuest;
  end;

  { THeroPlaceholderOptions }

  THeroPlaceholderOptions = class (TObjectOptions)
  private
    FPower: UInt8;
    FTypeID: THeroID;
    procedure SetPower(AValue: UInt8);
    procedure SetTypeID(AValue: THeroID);
  public
    class function MayBeOwned: Boolean; override;
    procedure ApplyVisitor(AVisitor: IObjectOptionsVisitor); override;
  published
    property TypeID: THeroID read FTypeID write SetTypeID default -1;
    property Power: UInt8 read FPower write SetPower default 0;
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

function CreateByID(ID: AnsiString; SubID: AnsiString): TObjectOptions;

implementation

uses
  editor_consts;

function CreateByID(ID: TObjectTypeID; SubID: TCustomID): TObjectOptions; deprecated;
var
  c: TObjectOptionsClass;
begin
  c := TObjectOptions;
  case TObj(ID) of

    TObj.EVENT:
    begin
      c := TLocalEventOptions;
    end;
    TObj.SIGN, TObj.OCEAN_BOTTLE:
    begin
      c := TSignBottleOptions;
    end;
    HERO, RANDOM_HERO, PRISON:
    begin
      c := THeroOptions;
    end;
    MONSTER,
    RANDOM_MONSTER,
    RANDOM_MONSTER_L1,
    RANDOM_MONSTER_L2,
    RANDOM_MONSTER_L3,
    RANDOM_MONSTER_L4,
    RANDOM_MONSTER_L5,
    RANDOM_MONSTER_L6,
    RANDOM_MONSTER_L7:
    begin
      c := TMonsterOptions;
    end;
    SEER_HUT:
    begin
      c := TSeerHutOptions;
    end;
    WITCH_HUT:
    begin
      c := TWitchHutOptions;
    end;
    SCHOLAR:
    begin
      c := TScholarOptions;
    end;
    GARRISON, GARRISON2:
    begin
      c := TGarrisonOptions;
    end;
    ARTIFACT, RANDOM_ART,
    RANDOM_TREASURE_ART,
    RANDOM_MINOR_ART,
    RANDOM_MAJOR_ART,
    RANDOM_RELIC_ART:
    begin
      c := TArtifactOptions;
    end;
    SPELL_SCROLL:
    begin
      c := TSpellScrollOptions;
    end;
    RESOURCE,
    RANDOM_RESOURCE:
    begin
      c := TResourceOptions;
    end;
    RANDOM_TOWN, TOWN:
      c := TTownOptions;

    CREATURE_GENERATOR1,
    CREATURE_GENERATOR2,
    CREATURE_GENERATOR3,
    CREATURE_GENERATOR4:
      c := TOwnedObjectOptions;
    MINE:
    begin
      if SubID = 7 then
      begin
        c := TAbandonedOptions;
      end else
      begin
        c := TOwnedObjectOptions;
      end;
    end;

    ABANDONED_MINE:
      c := TAbandonedOptions;

    SHRINE_OF_MAGIC_GESTURE:
      c := TShrine1Options;

    SHRINE_OF_MAGIC_INCANTATION:
      c := TShrine2Options;

    SHRINE_OF_MAGIC_THOUGHT:
      c := TShrine3Options;

    PANDORAS_BOX:
      c := TPandorasOptions;
    GRAIL:
      c := TGrailOptions;

    RANDOM_DWELLING:
      c := TRandomDwellingOptions;
    RANDOM_DWELLING_LVL:
      c := TRandomDwellingLVLOptions;
    RANDOM_DWELLING_FACTION:
      c := TRandomDwellingTownOptions;
    QUEST_GUARD:
      c := TQuestGuardOptions;
    SHIPYARD,LIGHTHOUSE:
      c := TOwnedObjectOptions;
  end;
  Result := c.Create;

end;

function CreateByID(ID: AnsiString; SubID: AnsiString): TObjectOptions;
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

  Result := c.Create;
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

function TQuest.IsResourcesStored: Boolean;
begin
  Result := MissionType = TQuestMission.Resources;
end;

procedure TQuest.SetFirstVisitText(AValue: TLocalizedString);
begin
  FFirstVisitText := AValue;
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

procedure TQuest.SetTimeLimit(AValue: Integer);
begin
  if FTimeLimit=AValue then Exit;
  FTimeLimit:=AValue;
end;

constructor TQuest.Create;
begin
  TimeLimit := -1;
  FArtifacts := TStringList.Create;
  FArmy := TCreatureSet.Create(0);
  FResources := TResourceSet.Create;
end;

destructor TQuest.Destroy;
begin
  FResources.Free;
  FArmy.Free;
  FArtifacts.Free;
  inherited Destroy;
end;

{ THeroPlaceholderOptions }

class function THeroPlaceholderOptions.MayBeOwned: Boolean;
begin
  Result := True;
end;

procedure THeroPlaceholderOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitHeroPlaseholder(Self);
end;

procedure THeroPlaceholderOptions.SetPower(AValue: UInt8);
begin
  FPower := AValue;
end;

procedure THeroPlaceholderOptions.SetTypeID(AValue: THeroID);
begin
  FTypeID := AValue;
end;

{ TOwnedObjectOptions }

class function TOwnedObjectOptions.MayBeOwned: Boolean;
begin
  result := True;
end;

procedure TOwnedObjectOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitOwnedObject(Self);
end;

{ TQuestGuardOptions }

procedure TQuestGuardOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitQuestGuard(Self);
end;

constructor TQuestGuardOptions.Create;
begin
  inherited Create;
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

constructor TBaseRandomDwellingOptions.Create;
begin
  inherited Create;
  FAllowedFactions := TStringList.Create;
  FAllowedFactions.Sorted := True;
  FAllowedFactions.Duplicates := dupIgnore;

  RootManager.ListsManager.FactionInfos.FillWithAllIds(FAllowedFactions);
end;

destructor TBaseRandomDwellingOptions.Destroy;
begin
  FAllowedFactions.Free;
  inherited Destroy;
end;

class function TBaseRandomDwellingOptions.MayBeOwned: Boolean;
begin
  Result := True;
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

procedure TCreatureInstInfo.SetCreCount(AValue: Integer);
begin
  FCreCount := AValue;
end;

procedure TCreatureInstInfo.SetCreID(AValue: AnsiString);
begin
  if FCreID=AValue then Exit;
  FCreID:=AValue;
end;


procedure TCreatureInstInfo.SetRandomCount(AValue: boolean);
begin
  if FRandomCount = AValue then Exit;
  FRandomCount := AValue;

  if AValue then
  begin
    FCreCount := 0;
  end;
end;

{ TGuardedObjectOptions }

constructor TGuardedObjectOptions.Create;
begin
  inherited Create;
  FGuards := TCreatureSet.Create(7);
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

constructor TCreatureSet.Create(AMaxSize: Integer);
begin
  inherited Create;
  FMaxSize := AMaxSize;
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

constructor TPandorasOptions.Create;
begin
  inherited Create;
  FCreatures := TCreatureSet.Create(0);
end;

destructor TPandorasOptions.Destroy;
begin
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

function TShrineOptions.IsSpellIDStored: Boolean;
begin
  Result := not IsRandom;
end;

procedure TShrineOptions.SetIsRandom(AValue: Boolean);
begin
  if FIsRandom = AValue then Exit;
  FIsRandom := AValue;
end;

procedure TShrineOptions.SetSpellID(AValue: AnsiString);
begin
    //TODO: check spell level
  FSpellID := AValue;
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

constructor TTownOptions.Create;
begin
  inherited Create;
  FGarrison := TCreatureSet.Create(7);
end;

destructor TTownOptions.Destroy;
begin
  FGarrison.Free;
  inherited Destroy;
end;

procedure TTownOptions.SetName(AValue: TLocalizedString);
begin
  FName := AValue;
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

constructor TSpellScrollOptions.Create;
begin
  inherited Create;
  FSpellID := 'magicArrow';
end;

procedure TSpellScrollOptions.SetSpellID(AValue: AnsiString);
begin
  if FSpellID = AValue then Exit;
  FSpellID := AValue;
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

constructor TGarrisonOptions.Create;
begin
  inherited Create;
  FGarrison := TCreatureSet.Create(7);
end;

destructor TGarrisonOptions.Destroy;
begin
  FGarrison.Free;
  inherited Destroy;
end;

procedure TGarrisonOptions.SetRemovableUnits(AValue: Boolean);
begin
  FRemovableUnits := AValue;
end;

{ TScholarOptions }

procedure TScholarOptions.SetBonusType(AValue: TScholarBonus);
begin
  if FBonusType=AValue then Exit;
  FBonusType:=AValue;
end;

function TScholarOptions.IsBonusIdStored: Boolean;
begin
  Result := BonusType<>TScholarBonus.random;
end;

procedure TScholarOptions.SetBonusId(AValue: AnsiString);
begin
  if FBonusId=AValue then Exit;
  FBonusId:=AValue;
end;

constructor TScholarOptions.Create;
begin
  inherited Create;
  FBonusType:=TScholarBonus.random;
end;

procedure TScholarOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitScholar(Self);
end;

{ TWitchHutOptions }

procedure TWitchHutOptions.ApplyVisitor(AVisitor: IObjectOptionsVisitor);
begin
  AVisitor.VisitWitchHut(Self);
end;

constructor TWitchHutOptions.Create;
var
  i: Integer;
begin
  inherited Create;
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

constructor TSeerHutOptions.Create;
begin
  inherited Create;
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

function TMonsterOptions.GetHasReward: Boolean;
begin
  Result := FHasReward;
end;

function TMonsterOptions.IsRewardArtifactStored: Boolean;
begin
  Result := HasReward and (FRewardArtifact <> '');
end;

function TMonsterOptions.IsRewardMessageStored: Boolean;
begin
  Result := HasReward and (FRewardMessage <> '');
end;

function TMonsterOptions.IsRewardResourcesStored: Boolean;
begin
  Result := HasReward and (not FRewardResources.IsEmpty);
end;

procedure TMonsterOptions.SetCharacter(AValue: Integer);
begin
  if FCharacter=AValue then Exit;
  FCharacter:=AValue;
end;

procedure TMonsterOptions.SetHasReward(AValue: Boolean);
begin
  if FHasReward=AValue then Exit;
  FHasReward:=AValue;
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

constructor TMonsterOptions.Create;
begin
  inherited Create;
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

constructor THeroOptions.Create;
begin
  inherited Create;
  FArmy := TCreatureSet.Create(7);
end;

destructor THeroOptions.Destroy;
begin
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

constructor TObjectOptions.Create;
begin
  FOwner := TPlayer.none;
end;

class function TObjectOptions.MayBeOwned: Boolean;
begin
  Result := False;
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

end.
