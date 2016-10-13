{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit lists_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, gmap, gutil, gvector, fgl, Types, fpjson, filesystem_base, editor_consts,
  editor_types, editor_utils, vcmi_json, h3_txt, base_info, editor_classes,
  logical_id_condition, logical_expression, logical_building_condition;

type

  TListsManager = class;

  {$push}
  {$m+}

  { TTextDataConfig }

  TTextDataConfig = class
  private
    FArtifact: integer;
    FCreature: integer;
    FFaction: integer;
    FHero: integer;
    FHeroClass: integer;
    FSpell: integer;
    procedure SetArtifact(AValue: integer);
    procedure SetCreature(AValue: integer);
    procedure SetFaction(AValue: integer);
    procedure SetHero(AValue: integer);
    procedure SetHeroClass(AValue: integer);
    procedure SetSpell(AValue: integer);
  published
    property HeroClass: integer read FHeroClass write SetHeroClass default HEROCLASS_QUANTITY;
    property Artifact: integer read FArtifact write SetArtifact default ARTIFACT_QUANTITY;
    property Creature: integer read FCreature write SetCreature default CREATURE_QUANTITY;
    property Faction: integer read FFaction write SetFaction default FACTION_QUANTITY;
    property Hero: integer read FHero write SetHero default HERO_QUANTITY;
    property Spell: integer read FSpell write SetSpell default SPELL_QUANTITY;
  end;

  {$pop}

  { TMetaclassInfo }

  TMetaclassInfo = class(TBaseInfo)
  private
    FList: THashedCollection;
    FMaxValue: Int64;
    FMinValue: Int64;
    function GetMetaclass: TMetaclass;
    procedure SetMaxValue(AValue: Int64);
    procedure SetMetaclass(AValue: TMetaclass);
    procedure SetMinValue(AValue: Int64);
  public
    property Metaclass: TMetaclass read GetMetaclass write SetMetaclass;

    function IsEntity: Boolean;

    property MinValue: Int64 read FMinValue write SetMinValue;
    property MaxValue: Int64 read FMaxValue write SetMaxValue;


    //only for entities
    property List: THashedCollection read FList write FList;
  end;

  { TMetaclassInfos }

  TMetaclassInfos = class(specialize TGNamedCollection<TMetaclassInfo>)

  end;

  { TResourceTypeInfo }

  TResourceTypeInfo = class(TBaseInfo)

  end;

  { TResourceTypeInfos }

  TResourceTypeInfos = class(specialize TGNamedCollection<TResourceTypeInfo>)

  end;

  { TPrimSkillInfo }

  TPrimSkillInfo = class(TBaseInfo)

  end;

  { TPrimSkillInfos }

  TPrimSkillInfos = class (specialize TGNamedCollection<TPrimSkillInfo>)
  public

  end;

  { TSkillInfo }

  TSkillInfo = class (TBaseInfo)
  end;

  { TSkillInfos }

  TSkillInfos = class (specialize TGNamedCollection<TSkillInfo>)
  public
    procedure FillWithAllIds(AList: TLogicalIDCondition);
  end;

  TSpellType = (adventure=0, combat=1, ability=2);

  { TSpellInfo }

  TSpellInfo = class (TBaseInfo)
  private
    Ftype: TSpellType;
    FLevel: integer;
    procedure SetType(AValue: TSpellType);
    procedure SetLevel(AValue: integer);
  public
    function IsValid: Boolean; override;
  published
    property level: integer read FLevel write SetLevel;
    property &type: TSpellType read FType write SetType;
  end;

  { TSpellInfos }

  TSpellInfos = class (specialize TGNamedCollection<TSpellInfo>)
  public
    //all except abilities
    procedure FillWithAllIds(AList: TLogicalIDCondition);

    //all except abilities with level = ALevel if ALevel <> 0; fills with Name and TSpellInfo
    procedure FillWithAllIds(AList: TStrings; ALevel: integer);
  end;

  {$push}
  {$m+}

  { TGuildSpell }

  TGuildSpell = class(TNamedCollectionItem, IEmbeddedValue)
  private
    FChance: Integer;
    procedure SetChance(AValue: Integer);
  published
    property Chance: Integer read FChance write SetChance nodefault;
  end;

  { TGuildSpells }

  TGuildSpells = class(specialize TGNamedCollection<TGuildSpell>)
  end;

  { TTownBuilding }

  TTownBuilding = class(TNamedCollectionItem)
  private
    FDescription: TLocalizedString;
    FID: Integer;
    FMode: TBuildMode;
    FName: TLocalizedString;
    FRequires: TBuildingCondition;
    FUpgrades: AnsiString;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
  published
    property ID:Integer read FID write FID;
    property Name: TLocalizedString read FName write FName;
    property Description: TLocalizedString read FDescription write FDescription;
    property Mode: TBuildMode read FMode write FMode default TBuildMode.normal;
    property Upgrades: AnsiString read FUpgrades write FUpgrades;
    property Requires: TBuildingCondition read FRequires;
  end;

  { TTownBuildings }

  TTownBuildings = class(specialize TGNamedCollection<TTownBuilding>)
  end;

  { TTownInfo }

  TTownInfo = class
  private
    FBuildings: TTownBuildings;
    FGuildSpells: TGuildSpells;
    FMageGuild: Integer;
    FMapObject: TJSONObject;
    procedure SetMageGuild(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MapObject: TJSONObject read FMapObject;
    property GuildSpells:TGuildSpells read FGuildSpells;
    property Buildings:TTownBuildings read FBuildings;
    property MageGuild:Integer read FMageGuild write SetMageGuild;
  end;

  {$pop}

  { TFactionInfo }

  TFactionInfo = class(TBaseInfo, ISerializeNotify)
  private
    FHasTown: Boolean;
    FTown: TTownInfo;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property HasTown: Boolean read FHasTown;

  public //ISerializeNotify
    procedure BeforeSerialize(Sender:TObject);
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);
    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
  published
    property Town: TTownInfo read FTown;
  end;

  { TFactionInfos }

  TFactionInfos = class (specialize TGNamedCollection<TFactionInfo>)
  public
    procedure FillWithAllIds(AList: TLogicalIDCondition; AIncludeMods: Boolean);
    procedure FillWithAllIds(AList: TStrings; AIncludeMods: Boolean);
    procedure FillWithTownIds(AList: TStrings; AIncludeMods: Boolean);
  end;


  { THeroClassInfo }

  THeroClassInfo = class(TMapObjectInfo)
  private
    FPrimarySkills: THeroPrimarySkills;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property PrimarySkills: THeroPrimarySkills read FPrimarySkills;
  end;

  { THeroClassInfos }

  THeroClassInfos = class(specialize TGNamedCollection<THeroClassInfo>)
  public
  end;

  {$push}
  {$m+}

  { TBaseGraphics }

  TBaseGraphics = class
  private
    FMap: AnsiString;
    procedure SetMap(AValue: AnsiString);
  public
    procedure AddTemplates(ASubtypeConfig: TJSONObject);
  published
    property Map: AnsiString read FMap write SetMap;
  end;

  { TBaseTexts }

  TBaseTexts = class
  private
    FName: TLocalizedString;
    procedure SetName(AValue: TLocalizedString);
  published
    property Name: TLocalizedString read FName write SetName;
  end;

  { TCreatureName }

  TCreatureName = class
  private
    FPlural: TLocalizedString;
    FSingular: TLocalizedString;
    procedure SetPlural(AValue: TLocalizedString);
    procedure SetSingular(AValue: TLocalizedString);
  published
    property Singular: TLocalizedString read FSingular write SetSingular;
    property Plural: TLocalizedString read FPlural write SetPlural;
  end;

  {$pop}

  TCreatureGraphics = class(TBaseGraphics)

  end;

  { TCreatureInfo }

  TCreatureInfo = class(TBaseInfo)
  private
    FDisabled: Boolean;
    FGraphics: TCreatureGraphics;
    FName: TCreatureName;
    FSpecial: Boolean;
  protected
    function GetName: TLocalizedString; override;
    procedure SetName(const AValue: TLocalizedString); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function IsValid: Boolean; override;
  published
    property Name:TCreatureName read FName;
    property Graphics: TCreatureGraphics read FGraphics;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Special: Boolean read FSpecial write FSpecial;
  end;

  { TCreatureInfos }

  TCreatureInfos = class(specialize TGNamedCollection<TCreatureInfo>)
  public
  end;


  { TArtifactGraphics }

  TArtifactGraphics = class(TBaseGraphics)
  end;

  TArtifactTexts = class(TBaseTexts)
  end;

  TArtifactType = (HERO, CREATURE, COMMANDER);
  TArtifactTypes = set of TArtifactType;


  { TArtifactInfo }

  TArtifactInfo = class(TBaseInfo)
  private
    FClass: TArtifactClass;
    FGraphics: TArtifactGraphics;
    FTexts: TArtifactTexts;
    FSlot: TStrings;
    FType: TArtifactTypes;
    procedure SetType(AValue: TArtifactTypes);
  protected
    function GetName: TLocalizedString; override;
    procedure SetName(const AValue: TLocalizedString); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property &Class: TArtifactClass read FClass write FClass;
    property Graphics: TArtifactGraphics read FGraphics;

    property Slot: TStrings read FSlot;
    property Text: TArtifactTexts read FTexts;

    property &type:TArtifactTypes read FType write SetType;
  end;

  { TArtifactInfos }

  TArtifactInfos = class(specialize TGNamedCollection<TArtifactInfo>)
  public
    //all not special
    procedure FillWithAllIds(AList: TLogicalIDCondition);
  end;

  { THeroTexts }

  THeroTexts = class(TBaseTexts)
  private
    FBiography: TLocalizedString;
    procedure SetBiography(AValue: TLocalizedString);
  published
    property Biography: TLocalizedString read FBiography write SetBiography;
  end;

  { THeroInfo }

  THeroInfo = class(TBaseInfo, IHeroInfo, ISerializeNotify)
  private
    FFemale: Boolean;
    FHeroClass: TIdentifier;
    FSkills: THeroSecondarySkills;
    FSpecial: Boolean;
    FSpellBook: TStrings;
    FTexts: THeroTexts;
    procedure SetFemale(AValue: Boolean);
    procedure SetHeroClass(AValue: TIdentifier);
    procedure SetSpecial(AValue: Boolean);
  protected
    procedure SetName(const AValue: TLocalizedString); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    class function UseMeta: boolean; override;

    //IHeroInfo
    function GetHeroIdentifier: AnsiString;

    function GetBiography: TLocalizedString;
    function GetExperience: UInt64;
    function GetName: TLocalizedString; override;
    function GetPortrait: AnsiString;
    function GetSex: THeroSex;

    function GetPrimarySkills: THeroPrimarySkills;
    function GetSecondarySkills: THeroSecondarySkills;
  public// ISerializeNotify
    procedure BeforeSerialize(Sender:TObject);
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);

    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);

  published
    property Texts: THeroTexts read FTexts;
    property Female: Boolean read FFemale write SetFemale nodefault;
    property Special: Boolean read FSpecial write SetSpecial default False;
    property &Class: TIdentifier read FHeroClass write SetHeroClass;
    property Spellbook: TStrings read FSpellBook;
  public//not "standard" format
    property Skills: THeroSecondarySkills read FSkills;
  end;

  { THeroInfos }

  THeroInfos = class(specialize TGNamedCollection<THeroInfo>)
  private
    FOwner:TListsManager;
  protected
    procedure PushResolveRequest(AObject: TNamedCollectionItem; AMetaClass: TMetaclass; const AProperty: ShortString); override;
  public
    constructor Create(AOwner: TListsManager);
    procedure FillWithNotSpecial(AList: TLogicalIDCondition);
    procedure FillWithHeroesOfClass(ATarget: TStrings; AHeroClass: AnsiString);
  end;

  { TListsManager }

  TListsManager = class (TFSConsumer)
  private
    type
      TSlotMap = specialize gmap.TMap<AnsiString, Integer, TStringCompare>;

      TLessInteger = specialize gutil.TLess<Integer>;

      TBuildingCnv = specialize gmap.TMap<Integer, Integer, TLessInteger>;

      PResolveRequest = ^TResolveRequest;
      TResolveRequest = record
        AObject: TNamedCollectionItem;
        AMetaClass: TMetaclass;
        AProperty: ShortString;
      end;

      TResolveRequests = specialize TVector<TResolveRequest>;
  strict private
    FDestreamer: TVCMIJSONDestreamer;

    FNameMap: TNameToIdMap;

    FMetaclassInfos: TMetaclassInfos;
    FResourceTypeInfos: TResourceTypeInfos;
    FPrimSkillInfos: TPrimSkillInfos;
    FSkillInfos: TSkillInfos;
    FSpellInfos: TSpellInfos;
    FFactionInfos: TFactionInfos;
    FRandomFaction: TFactionInfo;
    FBuildingCnv:TBuildingCnv;
    FBuildingCnvSpec: TJSONArray;

    FHeroClassInfos: THeroClassInfos;
    FCreatureInfos: TCreatureInfos;
    FHeroInfos: THeroInfos;
    FArtifactInfos: TArtifactInfos;
    FArtifactSlotMaps: array[0..ARTIFACT_SLOT_COUNT-1] of TStrings;
    FSlotIds: TSlotMap;

    FResolveRequests: TResolveRequests;

    procedure FillSlotIds;

    procedure LoadMetaclasses;
    procedure LoadBuildings;
    procedure LoadPrimSkills;
    procedure LoadSkills;
    procedure LoadResourceTypes;
    procedure LoadTextDataConfig;

  strict private //Accesors
    function GetPlayerName(const APlayer: TPlayer): TLocalizedString;
  strict private
    FTextDataConfig: TTextDataConfig;

    function GetArtifactSlotMap(ASlot: Integer): TStrings;
    function GetHeroClasses(AId: AnsiString): THeroClassInfo;
    function GetHeroes(AId: AnsiString): THeroInfo;
    procedure MergeLegacy(ASrc: TJsonObjectList; ADest:TJSONObject);
    function AssembleConfig(APaths: TStrings; ALegacyData: TJsonObjectList): TJSONObject;
    procedure FillArtifactCache;

    procedure Load(AProgess: IProgressCallback; APaths: TModdedConfigPaths; ALegacyConfig: TJsonObjectList; ATarget:THashedCollection);

    procedure ResolveIdentifier(var AIdentifier: AnsiString; ALocalScope: AnsiString; AMetaclass: TMetaclass);
    function ResolveIdentifier(AIdentifier: AnsiString; AMetaclass: TMetaclass): Boolean ;

    function ResolveHeroClass(AIdentifier: AnsiString): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PushResolveRequest(AObject: TNamedCollectionItem; AMetaClass: TMetaclass; const AProperty: ShortString);

    procedure PreLoad;

    procedure LoadFactions(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
    procedure LoadHeroClasses(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
    procedure LoadCreatures(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
    procedure LoadArtifacts(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
    procedure LoadSpells(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
    procedure LoadHeroes(AProgess: IProgressCallback; APaths: TModdedConfigPaths);

    procedure ProcessResolveRequests;
  public

    property TextDataConfig: TTextDataConfig read FTextDataConfig;

    property PlayerName[const APlayer: TPlayer]: TLocalizedString read GetPlayerName;

    procedure FillWithPlayers(ATarget: TStrings; AIncludeNeutral: Boolean);

    function SIDIdNID(AID: AnsiString): TCustomID;

    //mods

    function GetEnabledMods: TStringDynArray;

    //metaclasses
    property Metaclasses: TMetaclassInfos read FMetaclassInfos;

    //primary skills
    property PrimarySkills: TPrimSkillInfos read FPrimSkillInfos;

    //secondary skills
    function SkillNidToString (ASkill: TCustomID): AnsiString;
    property SkillInfos: TSkillInfos read FSkillInfos;
    function GetSkill(const AID: AnsiString): TSkillInfo;

    //Spells
    function SpellIndexToString (ASpell: TCustomID): AnsiString;
    property SpellInfos: TSpellInfos read FSpellInfos;
    function GetSpell(const AID: AnsiString): TSpellInfo;

    //Factions
    property FactionInfos:TFactionInfos read FFactionInfos;
    function FactionIndexToString (AIndex: TCustomID):AnsiString;
    function GetFaction(const AID: AnsiString): TFactionInfo;
    property RandomFaction: TFactionInfo read FRandomFaction;

    //Buildings
    function BuildingIdToString (ABuilding: TCustomID): AnsiString; deprecated; //vcmi id to string

    function BuildingIndexToString (ATown, ABuilding: TCustomID): AnsiString; //h3 id to string

    //Hero classes
    property HeroClassInfos:THeroClassInfos read FHeroClassInfos;
    function HeroClassIndexToString (AIndex: TCustomID):AnsiString;
    property HeroClasses[AId: AnsiString]: THeroClassInfo read GetHeroClasses;

    //Creatures
    function CreatureIndexToString (AIndex: TCustomID): AnsiString;
    property CreatureInfos: TCreatureInfos read FCreatureInfos;

    //Artifacts
    function ArtifactIndexToString (AIndex: TCustomID): AnsiString;
    property ArtifactInfos: TArtifactInfos read FArtifactInfos;
    property ArtifactSlotMap[ASlot: Integer]: TStrings read GetArtifactSlotMap;

    //Heroes
    function HeroIndexToString (AIndex: TCustomID): AnsiString;
    property HeroInfos: THeroInfos read FHeroInfos;
    property Heroes[AId: AnsiString]: THeroInfo read GetHeroes;
    procedure FillWithHeroesOfClass(ATarget: TStrings; AHeroClass: AnsiString);
  end;

implementation

uses FileUtil, LazLoggerBase, typinfo;

const
  SEC_SKILL_TRAITS  = 'data\sstraits';
  SPELL_TRAITS      = 'data\sptraits';
  HERO_CLASS_TRAITS = 'data\hctraits';
  CREATURE_TRAITS   = 'data\crtraits';
  ARTIFACT_TRAITS   = 'data\artraits';

  HERO_TRAITS       = 'data\hotraits';
  HERO_BIOS         = 'data\herobios';

  TEXT_DATA_CONFIG  = 'config\defaultMods';

const
  NEWTRAL_PLAYER_NAME = 'No player';
  PLAYER_NAMES: array[TPlayerColor] of AnsiString = (
    //'No player',
    'Player 1 (red)',
    'Player 2 (blue)',
    'Player 3 (tan)',
    'Player 4 (green)',
    'Player 5 (orange)',
    'Player 6 (purple)',
    'Player 7 (teal)',
    'Player 8 (pink)');

{ TMetaclassInfo }

function TMetaclassInfo.GetMetaclass: TMetaclass;
begin
  result := TMetaclass(Index);
end;

procedure TMetaclassInfo.SetMaxValue(AValue: Int64);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=AValue;
end;

procedure TMetaclassInfo.SetMetaclass(AValue: TMetaclass);
begin
  Index:=TCustomID(AValue);
  Identifier:= GetEnumName(TypeInfo(TMetaclass), index);
end;

procedure TMetaclassInfo.SetMinValue(AValue: Int64);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=AValue;
end;

function TMetaclassInfo.IsEntity: Boolean;
begin
  Result := Assigned(FList);
end;


{ TTownBuilding }


constructor TTownBuilding.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRequires := TBuildingCondition.CreateRoot(TBuildingConditionItem);
  FMode:=TBuildMode.normal;
end;

destructor TTownBuilding.Destroy;
begin
  FRequires.Free;
  inherited Destroy;
end;

function TTownBuilding.GetDisplayName: string;
begin
  if FName = '' then
  begin
    Result:=inherited GetDisplayName;
  end
  else begin
    Result:=FName;
  end;

end;

{ TCreatureName }

procedure TCreatureName.SetPlural(AValue: TLocalizedString);
begin
  if FPlural=AValue then Exit;
  FPlural:=AValue;
end;

procedure TCreatureName.SetSingular(AValue: TLocalizedString);
begin
  if FSingular=AValue then Exit;
  FSingular:=AValue;
end;

{ THeroClassInfo }

constructor THeroClassInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPrimarySkills := THeroPrimarySkills.Create;
  FPrimarySkills.SetZero;
end;

destructor THeroClassInfo.Destroy;
begin
  FPrimarySkills.Free;
  inherited Destroy;
end;

{ TGuildSpell }

procedure TGuildSpell.SetChance(AValue: Integer);
begin
  if FChance=AValue then Exit;
  FChance:=AValue;
end;

{ TTownInfo }

procedure TTownInfo.SetMageGuild(AValue: Integer);
begin
  if FMageGuild=AValue then Exit;

  if (AValue <1) or (AValue >5) then
  begin
    raise EConfigurationError.CreateFmt('Invalid magic guild level %d',[AValue]);
  end;

  FMageGuild:=AValue;
end;

constructor TTownInfo.Create;
begin
  inherited Create;
  FMapObject := CreateJSONObject([]);
  FGuildSpells := TGuildSpells.Create;
  FBuildings := TTownBuildings.Create;
end;

destructor TTownInfo.Destroy;
begin
  FBuildings.Free;
  FGuildSpells.Free;
  FMapObject.Free;
  inherited Destroy;
end;

{ THeroInfos }

procedure THeroInfos.PushResolveRequest(AObject: TNamedCollectionItem; AMetaClass: TMetaclass;
  const AProperty: ShortString);
begin
  FOwner.PushResolveRequest(AObject, AMetaClass, AProperty);
end;

constructor THeroInfos.Create(AOwner: TListsManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure THeroInfos.FillWithNotSpecial(AList: TLogicalIDCondition);
var
  obj: THeroInfo;
  idx: Integer;
begin
  for idx := 0 to Count - 1 do
  begin
      obj := Items[idx];
    if obj.Special then
    begin
      AList.NoneOf.Add(obj.Identifier);
    end;
  end;
end;

procedure THeroInfos.FillWithHeroesOfClass(ATarget: TStrings; AHeroClass: AnsiString);
var
  hero_info: THeroInfo;
  i: Integer;
begin
  ATarget.Clear;
  for i := 0 to Count - 1 do
  begin
    hero_info := Items[i];

    if hero_info.&Class = AHeroClass then
    begin
      ATarget.AddObject(hero_info.Name, hero_info);
    end;
  end;
end;

{ THeroInfo }

procedure THeroInfo.SetHeroClass(AValue: TIdentifier);
begin
  if FHeroClass=AValue then Exit;
  FHeroClass:=AValue;
  PushResolveRequest(TMetaclass.HeroClass, 'Class');
end;

procedure THeroInfo.SetSpecial(AValue: Boolean);
begin
  if FSpecial=AValue then Exit;
  FSpecial:=AValue;
end;

function THeroInfo.GetName: TLocalizedString;
begin
  Result:=FTexts.Name;
end;

procedure THeroInfo.SetName(const AValue: TLocalizedString);
begin
  FTexts.Name:=AValue;
end;

procedure THeroInfo.SetFemale(AValue: Boolean);
begin
  if FFemale=AValue then Exit;
  FFemale:=AValue;
end;

constructor THeroInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTexts := THeroTexts.Create;
  FSpellBook := TIdentifierSet.Create(nil);
  FSkills := THeroSecondarySkills.Create;
end;

destructor THeroInfo.Destroy;
begin
  FSkills.Free;
  FSpellBook.Free;
  FTexts.Free;
  inherited Destroy;
end;

class function THeroInfo.UseMeta: boolean;
begin
  Result:=False;
end;

function THeroInfo.GetPortrait: AnsiString;
begin
  Result := Identifier;
end;

function THeroInfo.GetHeroIdentifier: AnsiString;
begin
  Result := Identifier;
end;

function THeroInfo.GetSex: THeroSex;
begin
  if Female then
    Result := THeroSex.female
  else
    Result := THeroSex.male;
end;

function THeroInfo.GetPrimarySkills: THeroPrimarySkills;
var
  c_info: THeroClassInfo;
begin
  if FHeroClass = '' then
  begin
    Result := nil;
    raise Exception.Create('No hero class to get attributes from.');
  end;

  c_info := THeroInfos(Collection).FOwner.HeroClasses[FHeroClass];

  Result := c_info.PrimarySkills;
end;

function THeroInfo.GetSecondarySkills: THeroSecondarySkills;
begin
  Result := FSkills;
end;

function THeroInfo.GetBiography: TLocalizedString;
begin
  Result := FTexts.Biography;
end;

function THeroInfo.GetExperience: UInt64;
begin
  Result := 0;
end;

procedure THeroInfo.BeforeSerialize(Sender: TObject);
begin

end;

procedure THeroInfo.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure THeroInfo.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure THeroInfo.AfterDeSerialize(Sender: TObject; AData: TJSONData);
var
  skill_src: TJSONArray;
  src:TJSONObject;
  iter: TJSONEnum;
  item: THeroSecondarySkill;
begin
  Skills.Clear;
  src := AData as TJSONObject;

  skill_src := src.Arrays['skills'];

  for iter in skill_src do
  begin
    item := Skills.Add;
    item.Identifier:= (iter.Value as TJSONObject).Strings['skill'];

    (Sender as TVCMIJSONDestreamer).JSONToObjectEx(iter.Value,item);
  end;

end;

{ THeroTexts }

procedure THeroTexts.SetBiography(AValue: TLocalizedString);
begin
  if FBiography=AValue then Exit;
  FBiography:=AValue;
end;

{ TBaseTexts }

procedure TBaseTexts.SetName(AValue: TLocalizedString);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

{ TArtifactInfos }

procedure TArtifactInfos.FillWithAllIds(AList: TLogicalIDCondition);
var
  info: TArtifactInfo;
  idx: Integer;
begin
  AList.Clear;
  for idx := 0 to Count - 1 do
  begin
    info := Items[idx];
    if info.&Class <> TArtifactClass.SPECIAL then
      AList.AnyOf.Add(info.Identifier);
  end;

  //todo: exclude overpowered
end;

{ TBaseGraphics }

procedure TBaseGraphics.SetMap(AValue: AnsiString);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TBaseGraphics.AddTemplates(ASubtypeConfig: TJSONObject);
var
  templates: TJSONObject;
begin
  if Map <> '' then
  begin
    templates := ASubtypeConfig.GetOrCreateObject('templates');
    templates.Add('default', CreateJSONObject(['animation', Map]));
  end;
end;

{ TArtifactInfo }

procedure TArtifactInfo.SetType(AValue: TArtifactTypes);
begin
  if FType=AValue then Exit;
  FType:=AValue;
end;

function TArtifactInfo.GetName: TLocalizedString;
begin
  Result:= FTexts.Name;
end;

procedure TArtifactInfo.SetName(const AValue: TLocalizedString);
begin
  FTexts.Name:=AValue;
end;

constructor TArtifactInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FGraphics := TArtifactGraphics.Create;
  FSlot := TStringList.Create;
  FTexts := TArtifactTexts.Create;
end;

destructor TArtifactInfo.Destroy;
begin
  FTexts.Free;
  FSlot.Free;
  FGraphics.Free;
  inherited Destroy;
end;

{ TTextDataConfig }

procedure TTextDataConfig.SetHeroClass(AValue: integer);
begin
  if FHeroClass=AValue then Exit;
  FHeroClass:=AValue;
end;

procedure TTextDataConfig.SetArtifact(AValue: integer);
begin
  if FArtifact=AValue then Exit;
  FArtifact:=AValue;
end;

procedure TTextDataConfig.SetCreature(AValue: integer);
begin
  if FCreature=AValue then Exit;
  FCreature:=AValue;
end;

procedure TTextDataConfig.SetFaction(AValue: integer);
begin
  if FFaction=AValue then Exit;
  FFaction:=AValue;
end;

procedure TTextDataConfig.SetHero(AValue: integer);
begin
  if FHero=AValue then Exit;
  FHero:=AValue;
end;

procedure TTextDataConfig.SetSpell(AValue: integer);
begin
  if FSpell=AValue then Exit;
  FSpell:=AValue;
end;

{ TCreatureInfo }

function TCreatureInfo.GetName: TLocalizedString;
begin
  Result:=FName.Plural;
end;

procedure TCreatureInfo.SetName(const AValue: TLocalizedString);
begin
  FName.Plural := AValue;
end;

constructor TCreatureInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FGraphics := TCreatureGraphics.Create;
  FName := TCreatureName.Create;
end;

destructor TCreatureInfo.Destroy;
begin
  FName.Free;
  FGraphics.Free;
  inherited Destroy;
end;

function TCreatureInfo.IsValid: Boolean;
begin
  Result:=not Special and not Disabled;
end;

{ TFactionInfo }

constructor TFactionInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTown := TTownInfo.Create;
  FHasTown:=false;
end;

destructor TFactionInfo.Destroy;
begin
  FTown.Free;
  inherited Destroy;
end;

procedure TFactionInfo.BeforeSerialize(Sender: TObject);
begin

end;

procedure TFactionInfo.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TFactionInfo.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TFactionInfo.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  FHasTown := (AData as TJSONObject).IndexOfName('town')>=0;
end;

{ TFactionInfos }

procedure TFactionInfos.FillWithAllIds(AList: TLogicalIDCondition; AIncludeMods: Boolean);
var
  faction: TFactionInfo;
  idx: SizeInt;
begin
  AList.Clear;

  for idx := 0 to Count - 1 do
  begin
    faction := Items[idx];
    if AIncludeMods or (ExtractModID(faction.Identifier) = '') then
    begin
      AList.AnyOf.AddObject(faction.Identifier, faction);
    end;
  end;
end;

procedure TFactionInfos.FillWithAllIds(AList: TStrings; AIncludeMods: Boolean);
var
  faction: TFactionInfo;
  idx: SizeInt;
begin
  AList.Clear;

  for idx := 0 to Count - 1 do
  begin
    faction := Items[idx];
    if AIncludeMods or (ExtractModID(faction.Identifier) = '') then
    begin
      AList.AddObject(faction.Identifier, faction);
    end;
  end;
end;

procedure TFactionInfos.FillWithTownIds(AList: TStrings; AIncludeMods: Boolean);
var
  faction: TFactionInfo;
  idx: SizeInt;
begin
  AList.Clear;
  //here was  "for faction in Self do" it crashed on O2+ optimize level, WTF?
  //todo: revert back for fpc 3.0 - seems to be fixed
  for idx := 0 to Count - 1 do
  begin
    faction := Items[idx];
    if faction.HasTown then
      AList.AddObject(faction.Identifier, faction);
  end;
end;

{ TSkillInfos }

procedure TSkillInfos.FillWithAllIds(AList: TLogicalIDCondition);
begin
  //all allowed, permissive
  AList.Clear;
end;

{ TSpellInfos }

procedure TSpellInfos.FillWithAllIds(AList: TLogicalIDCondition);
var
  spell: TSpellInfo;
  idx: Integer;
begin
  AList.Clear;
  for idx := 0 to Count - 1 do
  begin
    spell := Items[idx];
    if spell.IsValid() then
      AList.AnyOf.Add(spell.Identifier);
  end;
end;

procedure TSpellInfos.FillWithAllIds(AList: TStrings; ALevel: integer);
var
  spell: TSpellInfo;
  idx: Integer;
begin
  AList.Clear;
  for idx := 0 to Count - 1 do
  begin
    spell := Items[idx];
    if spell.IsValid() and ((ALevel = 0) or (spell.Level = ALevel)) then
      AList.AddObject(spell.Name, spell);
  end;
end;

{ TSpellInfo }

procedure TSpellInfo.SetType(AValue: TSpellType);
begin
  if Ftype = AValue then Exit;
  Ftype := AValue;
end;

procedure TSpellInfo.SetLevel(AValue: integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

function TSpellInfo.IsValid: Boolean;
begin
  //todo: special flag

  Result := (level>0) and (&type <> TSpellType.ability);
end;

{ TListsManager }

constructor TListsManager.Create(AOwner: TComponent);
var
  i: SizeInt;
begin
  inherited Create(AOwner);
  FTextDataConfig := TTextDataConfig.Create;

  FDestreamer := TVCMIJSONDestreamer.Create(Self);

  FNameMap := TNameToIdMap.Create;

  FMetaclassInfos := TMetaclassInfos.Create;
  FResourceTypeInfos := TResourceTypeInfos.Create;

  FPrimSkillInfos := TPrimSkillInfos.Create();
  FSkillInfos := TSkillInfos.Create();
  FSpellInfos := TSpellInfos.Create();
  FFactionInfos := TFactionInfos.Create();
  FRandomFaction := TFactionInfo.Create(nil);
  FBuildingCnv := TBuildingCnv.Create;
  FBuildingCnvSpec := CreateJSONArray([]);

  FHeroClassInfos := THeroClassInfos.Create();

  FCreatureInfos := TCreatureInfos.Create();

  FArtifactInfos := TArtifactInfos.Create();

  for i in [0..ARTIFACT_SLOT_COUNT-1] do
  begin
    FArtifactSlotMaps[i] := CrStrList;
  end;

  FHeroInfos := THeroInfos.Create(self);

  FSlotIds := TSlotMap.Create;
  FillSlotIds;

  FResolveRequests := TResolveRequests.Create;
end;

destructor TListsManager.Destroy;
var
  i: SizeInt;
begin
  FResolveRequests.Free;
  FSlotIds.Free;
  FHeroInfos.Free;

  for i in [0..ARTIFACT_SLOT_COUNT-1] do
  begin
    FArtifactSlotMaps[i].Free;
  end;
  FArtifactInfos.Free;
  FCreatureInfos.free;
  FHeroClassInfos.Free;

  FFactionInfos.Free;
  FRandomFaction.Free;
  FBuildingCnv.Free;
  FreeAndNil(FBuildingCnvSpec);
  FSpellInfos.Free;
  FSkillInfos.Free;
  FPrimSkillInfos.Free;
  FResourceTypeInfos.Free;
  FMetaclassInfos.Free;

  FNameMap.Free;

  FTextDataConfig.Free;
  inherited Destroy;
end;

procedure TListsManager.PushResolveRequest(AObject: TNamedCollectionItem; AMetaClass: TMetaclass;
  const AProperty: ShortString);
var
  r: TResolveRequest;
begin
  r.AMetaClass:=AMetaClass;
  r.AObject :=AObject;
  r.AProperty:=AProperty;

  FResolveRequests.PushBack(r);
end;

function TListsManager.GetPlayerName(const APlayer: TPlayer): TLocalizedString;
begin
  //TODO: get localized name;
  if APlayer = TPlayer.NONE then
  begin
    Result := NEWTRAL_PLAYER_NAME;
  end
  else begin
    Result := PLAYER_NAMES[APlayer];
  end;
end;

procedure TListsManager.MergeLegacy(ASrc: TJsonObjectList; ADest: TJSONObject);
var
  o: TJSONObject;
  index: LongInt;
  i: Integer;
begin
  ASrc.FreeObjects := false; //todo: index-based extract
  for i := 0 to ADest.Count - 1 do
  begin
    o := ADest.Items[i] as TJSONObject;

    if o.IndexOfName('index')>=0 then
    begin
      index := o.Integers['index'];
      MergeJson(o, ASrc[index]);

      ADest.Items[i] := ASrc[index];
      ASrc[index] := nil;
    end;
  end;

  ASrc.FreeObjects := true;
end;

function TListsManager.GetHeroes(AId: AnsiString): THeroInfo;
begin
   Result := FHeroInfos.FindItem(AId);
end;

function TListsManager.GetHeroClasses(AId: AnsiString): THeroClassInfo;
begin
  Result := FHeroClassInfos.FindItem(AId);
end;

function TListsManager.GetArtifactSlotMap(ASlot: Integer): TStrings;
begin
  Result := FArtifactSlotMaps[ASlot];
end;

function TListsManager.AssembleConfig(APaths: TStrings;
  ALegacyData: TJsonObjectList): TJSONObject;
var
  AConfig: TJsonResource;
  Path: String;
begin
  Result := CreateJSONObject([]);
  try
    for Path in APaths do
    begin
      AConfig := TJsonResource.Create(Path);
      try
        AConfig.Load(ResourceLoader);
        MergeJson(AConfig.Root, Result);
      finally
        FreeAndNil(AConfig);
      end;
    end;

    if Assigned(ALegacyData) then
    begin
      MergeLegacy(ALegacyData, Result);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TListsManager.GetSpell(const AID: AnsiString): TSpellInfo;
begin
   Result := FSpellInfos.FindItem(AID);
end;

function TListsManager.FactionIndexToString(AIndex: TCustomID): AnsiString;
var
  info: TFactionInfo;
  i: Integer;
begin
  Result := '';
  for i := 0 to FFactionInfos.Count - 1 do
  begin
    info := FFactionInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.Identifier);
    end;
  end;

  raise Exception.CreateFmt('Faction not found: %d',[AIndex]);
end;

function TListsManager.GetFaction(const AID: AnsiString): TFactionInfo;
begin
  Result := FFactionInfos.FindItem(AID);
end;

function TListsManager.BuildingIdToString(ABuilding: TCustomID): AnsiString;
begin
  Result := BUILDING_NAMES[ABuilding];
end;

function TListsManager.BuildingIndexToString(ATown, ABuilding: TCustomID): AnsiString;
var
  vcmi_id: TCustomID;
  it: TBuildingCnv.TIterator;
  iter: TJSONEnum;
  o: TJSONObject;
begin
  vcmi_id := ID_INVALID;
  it := FBuildingCnv.Find(ABuilding);

  if Assigned(it) then
  begin
    vcmi_id:=it.Value;
    FreeAndNil(it);
  end
  else if ATown >=0 then
  begin
    for iter in FBuildingCnvSpec do
    begin
      o := iter.Value as TJSONObject;
      if (o.Integers['town'] = ATown) and (o.Integers['h3'] = ABuilding) then
      begin
        vcmi_id := o.Integers['vcmi'];
        Break;
      end;
    end;
  end;

  if vcmi_id = ID_INVALID then
  begin
    raise Exception.CreateFmt('Building %d not found for faction %d',[ABuilding, ATown]);
  end;
  Result := BUILDING_NAMES[vcmi_id];

end;

function TListsManager.HeroClassIndexToString(AIndex: TCustomID): AnsiString;
var
  info: THeroClassInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';

  for i := 0 to FHeroClassInfos.Count - 1 do
  begin
    info := FHeroClassInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.Identifier);
    end;
  end;

  raise Exception.CreateFmt('Hero class index not found: %d',[AIndex]);

end;

function TListsManager.CreatureIndexToString(AIndex: TCustomID): AnsiString;
var
  info: TCreatureInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';

  for i := 0 to FCreatureInfos.Count - 1 do
  begin
    info := FCreatureInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.Identifier);
    end;
  end;

  raise Exception.CreateFmt('Creature index not found: %d',[AIndex]);

end;

function TListsManager.ArtifactIndexToString(AIndex: TCustomID): AnsiString;
var
  info: TArtifactInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';

  for i := 0 to FArtifactInfos.Count - 1 do
  begin
    info := FArtifactInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.Identifier);
    end;
  end;

  raise Exception.CreateFmt('Artifact index not found: %d',[AIndex]);
end;

function TListsManager.HeroIndexToString(AIndex: TCustomID): AnsiString;
var
  info: THeroInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';

  for i := 0 to FHeroInfos.Count - 1 do
  begin
    info := FHeroInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.Identifier);
    end;
  end;

  raise Exception.CreateFmt('Hero index not found: %d',[AIndex]);
end;

procedure TListsManager.FillWithHeroesOfClass(ATarget: TStrings; AHeroClass: AnsiString);
begin
  HeroInfos.FillWithHeroesOfClass(ATarget, AHeroClass);
end;

procedure TListsManager.PreLoad;
begin
  LoadMetaclasses;
  LoadTextDataConfig;
  LoadPrimSkills;
  LoadSkills;
  LoadBuildings;
  LoadResourceTypes;
end;

procedure TListsManager.LoadFactions(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  faction_names, bldgneut, bldgspec, dwelling: TTextResource;
  legacy_data: TJsonObjectList;
  f, build_idx: Integer;
  o, buildings: TJSONObject;
  random_faction: TJsonResource;
begin
  legacy_data := TJsonObjectList.Create(true);

  faction_names := TTextResource.Create('DATA/TOWNTYPE.TXT');
  bldgneut := TTextResource.Create('DATA/BLDGNEUT.TXT');
  bldgspec := TTextResource.Create('DATA/BLDGSPEC.TXT');
  dwelling := TTextResource.Create('DATA/DWELLING.TXT');

  random_faction := TJsonResource.Create('config/factions/random.json');
  try
    DebugLn('Loading factions');
    faction_names.Load(ResourceLoader);

    for f in [0..9] do
    begin
      o := CreateJSONObject([]);

      o.Strings['name'] := faction_names.Value[0,f];

      legacy_data.Add(o);
    end;

    bldgneut.Load(ResourceLoader);
    bldgspec.Load(ResourceLoader);
    dwelling.Load(ResourceLoader);

    for f in [0..8] do
    begin
      o := legacy_data[f];

      buildings := o.GetOrCreateObject('town').GetOrCreateObject('buildings');

      //common buildings
      for build_idx in [0..14] do
      begin
        buildings.GetOrCreateObject(BUILDING_NAMES[build_idx]).Strings['name'] := bldgneut.Value[0,build_idx];
        buildings.GetOrCreateObject(BUILDING_NAMES[build_idx]).Strings['description'] := bldgneut.Value[1,build_idx];
      end;

      //shipyard with the ship
      buildings.GetOrCreateObject(BUILDING_NAMES[20]).Strings['name'] := bldgneut.Value[0,18];
      buildings.GetOrCreateObject(BUILDING_NAMES[20]).Strings['description'] := bldgneut.Value[1,18];

      //blacksmith
      buildings.GetOrCreateObject(BUILDING_NAMES[16]).Strings['name'] := bldgneut.Value[0,19];
      buildings.GetOrCreateObject(BUILDING_NAMES[16]).Strings['description'] := bldgneut.Value[1,19];


      //specail buildings
      for build_idx in [0..8] do
      begin
        buildings.GetOrCreateObject(BUILDING_NAMES[17+build_idx]).Strings['name'] := bldgspec.Value[0, build_idx + f * 11];
        buildings.GetOrCreateObject(BUILDING_NAMES[17+build_idx]).Strings['description'] := bldgspec.Value[1, build_idx + f * 11];
      end;

      // Grail
      buildings.GetOrCreateObject(BUILDING_NAMES[26]).Strings['name'] := bldgspec.Value[0, 9 + f * 11];
      buildings.GetOrCreateObject(BUILDING_NAMES[26]).Strings['description'] := bldgspec.Value[1, 9 + f * 11];
      // Resource silo

      buildings.GetOrCreateObject(BUILDING_NAMES[15]).Strings['name'] := bldgspec.Value[0, 10 + f * 11];
      buildings.GetOrCreateObject(BUILDING_NAMES[15]).Strings['description'] := bldgspec.Value[1, 10 + f * 11];

      //dwellings
      for build_idx in [0..13] do
      begin
        buildings.GetOrCreateObject(BUILDING_NAMES[30+build_idx]).Strings['name'] := dwelling.Value[0, build_idx + f * 14];
        buildings.GetOrCreateObject(BUILDING_NAMES[30+build_idx]).Strings['description'] := dwelling.Value[1, build_idx + f * 14];
      end;

    end;

    Load(AProgess, APaths, legacy_data, FactionInfos);


    //todo: cleanup backward compatibility
    if random_faction.TryLoad(ResourceLoader) then
    begin
       FDestreamer.JSONToObjectEx(random_faction.Root.Objects['random'], FRandomFaction);
    end;

  finally
    random_faction.Free;
    dwelling.Free;
    bldgneut.Free;
    bldgspec.Free;
    faction_names.Free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.LoadHeroClasses(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  hctraits: TTextResource;

  legacy_data: TJsonObjectList;
  i: SizeInt;
  o, p_skills: TJSONObject;
begin
  hctraits := TTextResource.Create(HERO_CLASS_TRAITS);
  legacy_data := TJsonObjectList.Create(true);
  try
    hctraits.Load(ResourceLoader);

    for i in [0..HEROCLASS_QUANTITY-1] do
    begin
      o := CreateJSONObject([]);

      o.Strings['name'] := hctraits.Value[0,i+2];

      p_skills := CreateJSONObject([]);
      p_skills.Integers['attack'] := StrToInt(hctraits.Value[2,i+2]);
      p_skills.Integers['defence'] := StrToInt(hctraits.Value[3,i+2]);
      p_skills.Integers['spellpower'] := StrToInt(hctraits.Value[4,i+2]);
      p_skills.Integers['knowledge'] := StrToInt(hctraits.Value[5,i+2]);

      o.Objects['primarySkills'] := p_skills;
      legacy_data.Add(o);
    end;

    Load(AProgess, APaths, legacy_data, HeroClassInfos);
  finally
    legacy_data.Free;
    hctraits.free;
  end;
end;

procedure TListsManager.LoadCreatures(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  crtraits: TTextResource;
  legacy_data: TJsonObjectList;

  i: SizeInt;
  shift: Integer;

  name_count: integer;
  o: TJSONObject;
begin
  legacy_data := TJsonObjectList.Create(true);
  crtraits := TTextResource.Create(CREATURE_TRAITS);
  try
    crtraits.Load(ResourceLoader);

    if (crtraits.Value[0,1] <> 'Singular') or (crtraits.Value[1,1] <> 'Plural') then
      raise Exception.Create('Invalid crtraits format');

    if crtraits.Value[2,1] = 'Plural2' then
      name_count := 3
    else
      name_count := 2;

    shift := 2;

    for i in [0..TextDataConfig.Creature-1] do
    begin
      o := CreateJSONObject([]);

      while trim(crtraits.Value[0,i+shift]) = '' do
      begin
        inc(shift);
      end;

      o.GetOrCreateObject('name').Strings['singular'] := crtraits.Value[0,i+shift];
      o.GetOrCreateObject('name').Strings['plural'] := crtraits.Value[name_count-1,i+shift];

      legacy_data.Add(o);
    end;

    Load(AProgess, APaths, legacy_data, CreatureInfos);
  finally
    crtraits.free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.LoadArtifacts(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths);
const
  H3_ART_SLOTS : array [0..ARTIFACT_SLOT_COUNT-1] of string =
  (
  'SPELLBOOK',
	'MACH4',
	'MACH3',
	'MACH2',
	'MACH1',
	'MISC5',
	'MISC4',
	'MISC3',
	'MISC2',
	'MISC1',
	'FEET',
	'LEFT_RING',
	'RIGHT_RING',
	'TORSO',
	'LEFT_HAND',
	'RIGHT_HAND',
	'NECK',
	'SHOULDERS',
	'HEAD');

var
  artraits: TTextResource;

  slots: TJSONArray;

  procedure LoadSlots(idx: SizeInt);
  var
    ofs: SizeInt;
  begin
    for ofs in [0..ARTIFACT_SLOT_COUNT-1] do
    begin
      if artraits.Value[2+ofs, idx+2] = 'x' then
      begin
        slots.Add(H3_ART_SLOTS[ofs]);
      end;
    end;
  end;

  function GetArtClass(idx: SizeInt): TArtifactClass;
  var
    v: string;
  begin
    v := artraits.Value[2+ARTIFACT_SLOT_COUNT, idx+2];

    case v of
      'S':Result := TArtifactClass.SPECIAL ;
      'T': Result := TArtifactClass.TREASURE;
      'N': Result := TArtifactClass.MINOR;
      'J': Result := TArtifactClass.MAJOR;
      'R': Result := TArtifactClass.RELIC;
      else
        Result := TArtifactClass.SPECIAL;
    end;
  end;

var
  legacy_data: TJsonObjectList;
  o: TJSONObject;
  i: SizeInt;
  c: TArtifactClass;
begin

  legacy_data := TJsonObjectList.Create(true);
  artraits := TTextResource.Create(ARTIFACT_TRAITS);
  try
    artraits.Load(ResourceLoader);

    for i in [0..TextDataConfig.Artifact-1] do
    begin
      o := CreateJSONObject([]);

      o.GetOrCreateObject('text').Strings['name'] := artraits.Value[0,i+2];

      slots := CreateJSONArray([]);
      LoadSlots(i);
      o.Add('slot', slots);
      c := GetArtClass(i);

      o.Add('class', GetEnumName(TypeInfo(TArtifactClass), Integer(c)));

      legacy_data.Add(o);
    end;

    Load(AProgess, APaths, legacy_data, ArtifactInfos);
  finally
    artraits.free;
    legacy_data.Free;
  end;

  FillArtifactCache;
end;

procedure TListsManager.FillArtifactCache;
var
  item: TCollectionItem;
  info: TArtifactInfo;
  procedure ProcessSlotId(id: String);
  var
    iter: TSlotMap.TIterator;
  begin
    iter := FSlotIds.Find(id);

    if Assigned(iter) then
    begin
      FArtifactSlotMaps[iter.Value].AddObject(info.Identifier, info);
      iter.Free;
    end;
  end;

var
  slot_id: String;
begin
  for item in FArtifactInfos do
  begin
    info := TArtifactInfo(item);
    for slot_id in info.Slot do
    begin
      if slot_id = 'RING' then
      begin
        ProcessSlotId('LEFT_RING');
        ProcessSlotId('RIGHT_RING');
      end
      else if slot_id = 'MISC' then
      begin
        ProcessSlotId('MISC1');
        ProcessSlotId('MISC2');
        ProcessSlotId('MISC3');
        ProcessSlotId('MISC4');
        ProcessSlotId('MISC5');
      end
      else
      begin
        ProcessSlotId(slot_id);
      end;
    end;
  end;
end;

procedure TListsManager.Load(AProgess: IProgressCallback; APaths: TModdedConfigPaths; ALegacyConfig: TJsonObjectList;
  ATarget: THashedCollection);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := CreateJSONObject([]);

  try
    FConfig.Load(AProgess, APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(ALegacyConfig, FCombinedConfig);

    FDestreamer.JSONToObjectEx(FCombinedConfig, ATarget);

  finally
    FCombinedConfig.Free;
    FConfig.Free;
  end;
end;

procedure TListsManager.ResolveIdentifier(var AIdentifier: AnsiString; ALocalScope: AnsiString; AMetaclass: TMetaclass);
var
  idx: SizeInt;
  scopes: TStringDynArray;
  s: String;
begin
  idx := pos(':', AIdentifier);

  if idx = 0 then
  begin
    if ResolveIdentifier(AIdentifier, AMetaclass) then
      exit;//found in core

    if ALocalScope <> '' then
    begin
      if ResolveIdentifier(ALocalScope+':'+ AIdentifier, AMetaclass) then
      begin
        AIdentifier := ALocalScope+':'+ AIdentifier;
        exit;//found local
      end;

      scopes := ResourceLoader.GetModDepenencies(ALocalScope);

      for s in scopes do
      begin
        if ResolveIdentifier(s+':'+ AIdentifier, AMetaclass) then
        begin
          AIdentifier := s+':'+ AIdentifier;
          exit;//found in this scope
        end;
      end;
    end;

    raise Exception.CreateFmt('Identifier resolve failed for %s',[AIdentifier]);
  end
  else
  begin
    if not ResolveIdentifier(AIdentifier, AMetaclass) then
      raise Exception.CreateFmt('Identifier resolve failed for %s',[AIdentifier]);
  end;
end;

function TListsManager.ResolveIdentifier(AIdentifier: AnsiString; AMetaclass: TMetaclass): Boolean;
begin
  case AMetaclass of
    TMetaclass.HeroClass: Result := ResolveHeroClass(AIdentifier) ;
  else
    raise Exception.Create('Unimplmented resolve request');
  end;
end;

function TListsManager.ResolveHeroClass(AIdentifier: AnsiString): Boolean;
begin
  Result := Assigned(FHeroClassInfos.FindItem(AIdentifier));
end;

procedure TListsManager.LoadMetaclasses;
var
  o: TMetaclassInfo;
begin

  //artifact
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.artifact;
  o.List := FArtifactInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //creature
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.creature;
  o.List := FCreatureInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //faction
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.faction;
  o.List := FFactionInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//limit make no sence

  //experience
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.experience;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //hero
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.hero;
  o.List := FHeroInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//limit make no sence

  //heroClass
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.hero;
  o.List := FHeroClassInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//limit make no sence

  //luck
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.luck;
  o.MinValue:=-3;
  o.MaxValue:=3;

  //mana
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.mana;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //morale
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.morale;
  o.MinValue:=-3;
  o.MaxValue:=3;

  //movement
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.movement;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //primarySkill
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.primarySkill;
  o.List := FPrimSkillInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit

  //secondarySkill
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.secondarySkill;
  o.List := FSkillInfos;
  o.MinValue:=0;
  o.MaxValue:=3;//value=mastery

  //spell
  o := FMetaclassInfos.Add;
  o.Metaclass := TMetaclass.spell;
  o.List := FSpellInfos;
  o.MinValue:=0;
  o.MaxValue:=1;

  //resource
  o :=  FMetaclassInfos.Add;
  o.Metaclass:=TMetaclass.resource;
  o.List := FResourceTypeInfos;
  o.MinValue:=0;
  o.MaxValue:=0;//no limit
end;

procedure TListsManager.ProcessResolveRequests;
var
  i: SizeInt;
  r: PResolveRequest;

  pinfo:PPropInfo;
  pType : PTypeInfo;
  tmp: AnsiString;

  o: TNamedCollectionItem;
  local_scope: AnsiString;
begin
  for i := 0 to SizeInt(FResolveRequests.Size) - 1 do
  begin
    r := FResolveRequests.Mutable[i];

    pinfo := FindPropInfo(r^.AObject, r^.AProperty);
    pType := pinfo^.PropType;

    local_scope := '';

    if r^.AObject is TNamedCollectionItem then
    begin
      o := TNamedCollectionItem(r^.AObject);
      local_scope := o.Meta;
    end
    else
      raise Exception.Create('Unimplmented resolve request');

    case pType^.Kind of
      tkAString:
      begin
        tmp := GetStrProp(r^.AObject, pinfo);

        ResolveIdentifier(tmp, local_scope, r^.AMetaClass);

        SetStrProp(r^.AObject, pinfo, tmp);
      end
    else
      raise Exception.Create('Unimplmented resolve request');
    end;
  end;
end;

procedure TListsManager.FillSlotIds;
const
  SLOT_IDS: array[0..ARTIFACT_SLOT_COUNT-1] of AnsiString =
  (
	'HEAD', 'SHOULDERS', 'NECK', 'RIGHT_HAND', 'LEFT_HAND', 'TORSO',
	'RIGHT_RING', 'LEFT_RING', 'FEET',
	'MISC1', 'MISC2', 'MISC3', 'MISC4',
	'MACH1', 'MACH2', 'MACH3', 'MACH4',
	'SPELLBOOK', 'MISC5'
  );
var
  i: SizeInt;
begin
  for i := 0 to ARTIFACT_SLOT_COUNT - 1 do
  begin
    FSlotIds.Insert(SLOT_IDS[i],i);
  end;
end;

procedure TListsManager.LoadBuildings;
var
  buildings5: TJsonResource;
  table: TJSONArray;
  iter: TJSONEnum;
  o: TJSONObject;
begin
  buildings5 := TJsonResource.Create('config/buildings5.json');
  try
    buildings5.Load(ResourceLoader);

    table := buildings5.Root.Arrays['table'];

    for iter in table do
    begin
      o := iter.Value as TJSONObject;

      if o.Integers['town'] = -1 then
      begin
        FBuildingCnv.Insert(o.Integers['h3'], o.Integers['vcmi']);
      end
      else
      begin
        FBuildingCnvSpec.Add(o.Clone);
      end;
    end;

  finally
    buildings5.Free;
  end;
end;

procedure TListsManager.LoadPrimSkills;
var
  prim_skill: TPrimarySkill;
  info: TPrimSkillInfo;
begin
  for prim_skill in TPrimarySkill do
  begin
    info := FPrimSkillInfos.Add;
    info.Identifier:=PRIMARY_SKILL_NAMES[prim_skill];
    //todo: set name

  end;
end;

procedure TListsManager.LoadSkills;
var
  sstraits: TTextResource;
  info: TSkillInfo;
  i: Integer;
begin
  for i := 0 to SECONDARY_SKILL_QUANTITY - 1 do
  begin
    FNameMap.Insert('skill.' + SECONDARY_SKILL_NAMES[i], i);
  end;

  FSkillInfos.Clear;
  sstraits := TTextResource.Create(SEC_SKILL_TRAITS);
  try
    sstraits.Load(ResourceLoader);

    for i := 2 to sstraits.RowCount - 1 do
    begin
      info := FSkillInfos.Add;
      info.Identifier := SECONDARY_SKILL_NAMES[i-2];
      info.Name := sstraits.Value[0,i];

    end;

  finally
    sstraits.Free;
  end;
end;

procedure TListsManager.LoadResourceTypes;
var
  rt: TResType;
  o: TResourceTypeInfo;
begin
  for rt in TResType do
  begin
    o := FResourceTypeInfos.Add;

    o.Index:=Integer(rt);
    o.Identifier:=RESOURCE_NAMES[rt];

    //todo: set name
  end;
end;

procedure TListsManager.LoadTextDataConfig;
var
  config: TJsonResource;
begin
  config := TJsonResource.Create(TEXT_DATA_CONFIG);
  try
    config.Load(ResourceLoader);
    config.DestreamTo(FTextDataConfig, 'textData');
  finally
    config.free;
  end;
end;

procedure TListsManager.LoadSpells(AProgess: IProgressCallback; APaths: TModdedConfigPaths);
var
  sptrairs: TTextResource;

  row: Integer;

  legacy_config: TJsonObjectList; //index = lecacy ID
  i: integer;
  spell_config: TJSONObject;
  loc_name: String;
begin
  sptrairs := TTextResource.Create(SPELL_TRAITS);
  legacy_config := TJsonObjectList.Create(True);

  try
    //PreLoad sptraits
    sptrairs.Load(ResourceLoader);

    row := 2;

    for i in [1..3] do
    begin
      row +=3;

      repeat
        spell_config := CreateJSONObject([]);
        loc_name := sptrairs.Value[0,row];
        spell_config.Strings['name'] := loc_name;
        spell_config.Integers['level'] := StrToIntDef(sptrairs.Value[2,row],0);
        spell_config.Strings['type'] := GetEnumName(TypeInfo(TSpellType), i-1);
        legacy_config.Add(spell_config);
        inc(row);
      until sptrairs.Value[0,row] = '';
    end;

    legacy_config.Add(legacy_config[legacy_config.Count-1].Clone as TJSONObject);

    Load(AProgess, APaths, legacy_config, SpellInfos);
  finally
    legacy_config.Free;
    sptrairs.Free;
  end;
end;

procedure TListsManager.LoadHeroes(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  legacy_data: TJsonObjectList;
  hotraits, herobios: TTextResource;
  i: SizeInt;
  o: TJSONObject;
begin
  legacy_data := TJsonObjectList.Create(true);
  hotraits := TTextResource.Create(HERO_TRAITS);
  herobios := TTextResource.Create(HERO_BIOS);
  try
    hotraits.Load(ResourceLoader);
    herobios.Load(ResourceLoader);

    for i in [0..TextDataConfig.Hero-1] do
    begin
      o := CreateJSONObject([]);

      o.GetOrCreateObject('texts').Strings['name'] := hotraits.Value[0,i+2];
      o.GetOrCreateObject('texts').Strings['biography'] := herobios.Value[0,i];

      legacy_data.Add(o);
    end;

    Load(AProgess, APaths, legacy_data, FHeroInfos);

  finally
    hotraits.free;
    herobios.Free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.FillWithPlayers(ATarget: TStrings;
  AIncludeNeutral: Boolean);
var
  p: TPlayer;
begin
  ATarget.Clear;
  if AIncludeNeutral then
    ATarget.Add(PlayerName[TPlayer.NONE]);
  for p in TPlayerColor do
  begin
    ATarget.Add(PlayerName[p]);
  end;
end;

function TListsManager.SIDIdNID(AID: AnsiString): TCustomID;
var
  it: TNameToIdMap.TIterator;
begin

  it := FNameMap.Find(AID);

  if Assigned(it) then
  begin
    Result := it.Value;
  end
  else
  begin
    Result := ID_INVALID;
    raise Exception.Create('Invalid string id '+AID);
  end;

end;

function TListsManager.GetEnabledMods: TStringDynArray;
begin
  Result := ResourceLoader.GetEnabledMods();
end;

function TListsManager.SkillNidToString(ASkill: TCustomID): AnsiString;
begin
  Result := editor_consts.SECONDARY_SKILL_NAMES[ASkill];
end;

function TListsManager.GetSkill(const AID: AnsiString): TSkillInfo;
begin
  Result := SkillInfos.FindItem(AID);
end;

function TListsManager.SpellIndexToString(ASpell: TCustomID): AnsiString;
var
  info: TSpellInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';
  for i := 0 to FSpellInfos.Count - 1 do
  begin
    info := FSpellInfos[i];
    if info.Index = ASpell then
    begin
      Result := info.Identifier;
      Exit;
    end;
  end;
  //todo: fix PRESET = 254 spell load
  //raise Exception.CreateFmt('Spell not found: %d',[ASpell]);
end;

end.

