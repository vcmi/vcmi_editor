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

unit lists_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,
  gmap, fgl,
  fpjson,
  filesystem_base, editor_consts, editor_types, editor_utils,
  vcmi_json,h3_txt, base_info, editor_classes, logical_id_condition;

type

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

  { TSkillInfo }

  TSkillInfo = class (TBaseInfo)
  protected
    function GetFullID: AnsiString; override;
  end;

  { TSkillInfos }

  TSkillInfos = class (specialize TFPGObjectList<TSkillInfo>)
  public
    procedure FillWithAllIds(AList: TLogicalIDCondition);
  end;

  TSpellType = (Adventure, Combat, Ability);

  { TSpellInfo }

  TSpellInfo = class (TBaseInfo)
  private
    Ftype: TSpellType;
    FLevel: integer;
    procedure setType(AValue: TSpellType);
    procedure SetLevel(AValue: integer);
  protected
    function GetFullID: AnsiString; override;
  public
    property Level: integer read FLevel write SetLevel;
    property SpellType: TSpellType read FType write SetType;
  end;

  { TSpellInfos }

  TSpellInfos = class (specialize TFPGObjectList<TSpellInfo>)
  public
    //all except abilities
    procedure FillWithAllIds(AList: TLogicalIDCondition);
  end;

  {$push}
  {$m+}

  { TGuildSpell }

  TGuildSpell = class(TNamedCollectionItem, IEmbeddedValue)
  private
    FChance: Integer;
    procedure SetChance(AValue: Integer);
  published
    property Chance: Integer read FChance write SetChance;
  end;

  { TGuildSpells }

  TGuildSpells = class(specialize TGNamedCollection<TGuildSpell>)
  end;

  { TTownBuilding }

  TTownBuilding = class(TNamedCollectionItem)

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

  TFactionInfo = class(TBaseInfo)
  private
    FCapitolDefName: AnsiString;
    FCastleDefName: AnsiString;
    FHasTown: Boolean;
    FTown: TTownInfo;
    FVillageDefName: AnsiString;
    procedure SetCapitolDefName(AValue: AnsiString);
    procedure SetCastleDefName(AValue: AnsiString);
    procedure SetHasTown(AValue: Boolean);
    procedure SetVillageDefName(AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    property VillageDefName: AnsiString read FVillageDefName write SetVillageDefName;
    property CastleDefName: AnsiString read FCastleDefName write SetCastleDefName;
    property CapitolDefName:  AnsiString read FCapitolDefName write SetCapitolDefName;
    property HasTown: Boolean read FHasTown write SetHasTown;

    property Town: TTownInfo read FTown;
  end;

  { TFactionInfos }

  TFactionInfoList = specialize TFPGObjectList<TFactionInfo>;

  TFactionInfos = class (TFactionInfoList)
  public
    procedure FillWithAllIds(AList: TStrings);
    procedure FillWithTownIds(AList: TStrings);
  end;


  { THeroClassInfo }

  THeroClassInfo = class(TMapObjectInfo)
  public

  end;

  THeroClassInfoList = specialize TFPGObjectList<THeroClassInfo>;

  { THeroClassInfos }

  THeroClassInfos = class(THeroClassInfoList)
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

  {$pop}

  TCreatureGraphics = class(TBaseGraphics)

  end;

  { TCreatureInfo }

  TCreatureInfo = class(TBaseInfo)
  private
    FGraphics: TCreatureGraphics;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Graphics: TCreatureGraphics read FGraphics;
  end;



  TCreatureInfoList = specialize TFPGObjectList<TCreatureInfo>;

  { TCreatureInfos }

  TCreatureInfos = class(TCreatureInfoList)
  public
  end;


  { TArtifactGraphics }

  TArtifactGraphics = class(TBaseGraphics)
  end;

  { TArtifactInfo }

  TArtifactInfo = class(TBaseInfo)
  private
    FGraphics: TArtifactGraphics;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Graphics: TArtifactGraphics read FGraphics;
  end;

  TArtifactInfoList = specialize TFPGObjectList<TArtifactInfo>;

  { TArtifactInfos }

  TArtifactInfos = class(TArtifactInfoList)
  public
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

  THeroInfo = class(TBaseInfo)
  private
    FFemale: Boolean;
    FHeroClass: AnsiString;
    FSpecial: Boolean;
    FTexts: THeroTexts;
    procedure SetFemale(AValue: Boolean);
    procedure SetHeroClass(AValue: AnsiString);
    procedure SetSpecial(AValue: Boolean);
  protected
    function GetName: TLocalizedString; override;
    procedure SetName(AValue: TLocalizedString); override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Texts: THeroTexts read FTexts;
    property Female: Boolean read FFemale write SetFemale nodefault;
    property Special: Boolean read FSpecial write SetSpecial default False;
    property HeroClass: AnsiString read FHeroClass write SetHeroClass;
  end;

  THeroInfoList = specialize TFPGObjectList<THeroInfo>;

  { THeroInfos }

  THeroInfos = class(THeroInfoList)
  public
    procedure FillWithNotSpecial(AList: TStrings);
  end;

  { TListsManager }

  TListsManager = class (TFSConsumer)
  strict private
    FDestreamer: TVCMIJSONDestreamer;

    FNameMap: TNameToIdMap;
    FSkillInfos: TSkillInfos;
    FSkillMap: TStringList;

    FSpellInfos: TSpellInfos;
    FSpellMap: TStringList;

    FFactionInfos: TFactionInfos;
    FFactionMap: TStringList;

    FHeroClassInfos: THeroClassInfos;
    FHeroClassMap: TStringList;

    FCreatureInfos: TCreatureInfos;
    FCreatureMap: TStringList;

    FHeroInfos: THeroInfos;
    FHeroMap: TStringList;

    procedure LoadSkills;
    procedure LoadTextDataConfig;

    procedure ProcessSpellConfig(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

  strict private //Accesors
    function GetPlayerName(const APlayer: TPlayer): TLocalizedString;
  strict private
    FArtifactInfos: TArtifactInfos;
    FArtifactMap: TStringList;
    FTextDataConfig: TTextDataConfig;

    procedure MergeLegacy(ASrc: TJsonObjectList; ADest:TJSONObject);
    function AssembleConfig(APaths: TStrings; ALegacyData: TJsonObjectList): TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PreLoad;

    procedure LoadFactions(APaths: TModdedConfigPaths); //todo: mod support for factions
    procedure LoadHeroClasses(APaths: TModdedConfigPaths);
    procedure LoadCreatures(APaths: TModdedConfigPaths);
    procedure LoadArtifacts(APaths: TModdedConfigPaths);
    procedure LoadSpells(APaths: TStrings); //todo: mod support for spells
    procedure LoadHeroes(APaths: TModdedConfigPaths);
  public

    property TextDataConfig: TTextDataConfig read FTextDataConfig;

    property PlayerName[const APlayer: TPlayer]: TLocalizedString read GetPlayerName;

    function SIDIdNID(AID: AnsiString): TCustomID;

    //secondary skills
    function SkillNidToString (ASkill: TCustomID): AnsiString;
    property SkillInfos: TSkillInfos read FSkillInfos;
    property SkillMap:TStringList read FSkillMap;

    //Spells
    function SpellIndexToString (ASpell: TCustomID): AnsiString;
    property SpellInfos: TSpellInfos read FSpellInfos;
    function GetSpell(const AID: AnsiString): TSpellInfo;
    property SpellMap: TStringList read FSpellMap;

    //Factions
    property FactionInfos:TFactionInfos read FFactionInfos;
    property FactionMap: TStringList read FFactionMap;
    function FactionIndexToString (AIndex: TCustomID):AnsiString;
    function GetFaction(const AID: AnsiString): TFactionInfo;

    //Hero classes
    property HeroClassInfos:THeroClassInfos read FHeroClassInfos;
    property HeroClassMap: TStringList read FHeroClassMap;
    function HeroClassIndexToString (AIndex: TCustomID):AnsiString;

    //Creatures
    function CreatureIndexToString (AIndex: TCustomID): AnsiString;
    property CreatureInfos: TCreatureInfos read FCreatureInfos;
    property CreatureMap: TStringList read FCreatureMap;

    //Artifacts
    function ArtifactIndexToString (AIndex: TCustomID): AnsiString;
    property ArtifactInfos: TArtifactInfos read FArtifactInfos;
    property ArtifactMap: TStringList read FArtifactMap;

    //Heroes
    function HeroIndexToString (AIndex: TCustomID): AnsiString;
    property HeroInfos: THeroInfos read FHeroInfos;
    property HeroMap: TStringList read FHeroMap;

  end;

implementation

uses FileUtil, LazLoggerBase;

const
  SEC_SKILL_TRAITS  = 'data\sstraits';
  SPELL_TRAITS      = 'data\sptraits';
  HERO_CLASS_TRAITS = 'data\hctraits';
  CREATURE_TRAITS   = 'data\crtraits';
  ARTIFACT_TRAITS   = 'data\artraits';

  HERO_TRAITS       = 'data\hotraits';
  HERO_BIOS         = 'data\herobios';

  TEXT_DATA_CONFIG  = 'config\defaultMods';
  SPELL_INFO_NAME   = 'config\spell_info';

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
  FMageGuild:=AValue;
end;

constructor TTownInfo.Create;
begin
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

procedure THeroInfos.FillWithNotSpecial(AList: TStrings);
var
  obj: THeroInfo;
begin
  for obj in Self do
  begin
    if not obj.Special then
    begin
      AList.Add(obj.ID);
    end;
  end;
end;

{ THeroInfo }

procedure THeroInfo.SetHeroClass(AValue: AnsiString);
begin
  if FHeroClass=AValue then Exit;
  FHeroClass:=AValue;
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

procedure THeroInfo.SetName(AValue: TLocalizedString);
begin
  FTexts.Name:=AValue;
end;

procedure THeroInfo.SetFemale(AValue: Boolean);
begin
  if FFemale=AValue then Exit;
  FFemale:=AValue;
end;

constructor THeroInfo.Create;
begin
  inherited Create;
  FTexts := THeroTexts.Create;
end;

destructor THeroInfo.Destroy;
begin
  FTexts.Free;
  inherited Destroy;
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
begin
  AList.Clear;

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
    templates.Add('default', TJSONObject.Create(['animation', Map]));
  end;
end;

{ TArtifactInfo }

constructor TArtifactInfo.Create;
begin
  FGraphics := TArtifactGraphics.Create;
end;

destructor TArtifactInfo.Destroy;
begin
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

constructor TCreatureInfo.Create;
begin
  FGraphics := TCreatureGraphics.Create;
end;

destructor TCreatureInfo.Destroy;
begin
  FGraphics.Free;
  inherited Destroy;
end;

{ TFactionInfo }

procedure TFactionInfo.SetCapitolDefName(AValue: AnsiString);
begin
  if FCapitolDefName = AValue then Exit;
  FCapitolDefName := AValue;
end;

procedure TFactionInfo.SetCastleDefName(AValue: AnsiString);
begin
  if FCastleDefName = AValue then Exit;
  FCastleDefName := AValue;
end;

procedure TFactionInfo.SetHasTown(AValue: Boolean);
begin
  if FHasTown=AValue then Exit;
  FHasTown:=AValue;
end;

procedure TFactionInfo.SetVillageDefName(AValue: AnsiString);
begin
  if FVillageDefName = AValue then Exit;
  FVillageDefName := AValue;
end;

constructor TFactionInfo.Create;
begin
  inherited Create;
  FTown := TTownInfo.Create;
end;

destructor TFactionInfo.Destroy;
begin
  FTown.Free;
  inherited Destroy;
end;

{ TFactionInfos }

procedure TFactionInfos.FillWithAllIds(AList: TStrings);
var
  faction: TFactionInfo;
begin
  AList.Clear;
  for faction in Self do
  begin
    AList.Add(faction.ID);
  end;
end;

procedure TFactionInfos.FillWithTownIds(AList: TStrings);
var
  faction: TFactionInfo;
begin
  AList.Clear;
  for faction in Self do
  begin
    if faction.HasTown then
      AList.Add(faction.ID);
  end;
end;

{ TSkillInfo }

function TSkillInfo.GetFullID: AnsiString;
begin
  Result := 'skill.'+ID
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
begin
  AList.Clear;
  for spell in Self do
  begin
    if spell.SpellType <> TSpellType.Ability then
      AList.AnyOf.Add(spell.ID);
  end;
end;

{ TSpellInfo }

procedure TSpellInfo.SetType(AValue: TSpellType);
begin
  if Ftype = AValue then Exit;
  Ftype := AValue;
end;

function TSpellInfo.GetFullID: AnsiString;
begin
  Result := 'spell.'+ID;
end;

procedure TSpellInfo.SetLevel(AValue: integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

{ TListsManager }

constructor TListsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextDataConfig := TTextDataConfig.Create;

  FDestreamer := TVCMIJSONDestreamer.Create(Self);

  FNameMap := TNameToIdMap.Create;

  FSkillInfos := TSkillInfos.Create(True);
  FSkillMap := CrStrList;

  FSpellInfos := TSpellInfos.Create(True);
  FSpellMap := CrStrList;

  FFactionInfos := TFactionInfos.Create(True);
  FFactionMap := CrStrList;

  FHeroClassInfos := THeroClassInfos.Create(True);
  FHeroClassMap := CrStrList;

  FCreatureInfos := TCreatureInfos.Create(True);
  FCreatureMap := CrStrList;

  FArtifactInfos := TArtifactInfos.Create(True);
  FArtifactMap := CrStrList;

  FHeroInfos := THeroInfos.Create(True);
  FHeroMap := CrStrList;
end;

destructor TListsManager.Destroy;
begin
  FHeroInfos.Free;
  FHeroMap.Free;

  FArtifactInfos.Free;
  FArtifactMap.Free;

  FCreatureInfos.free;
  FCreatureMap.free;

  FHeroClassMap.Free;
  FHeroClassInfos.Free;

  FFactionMap.Free;
  FFactionInfos.Free;

  FSpellMap.Free;
  FSpellInfos.Free;

  FSkillMap.Free;
  FSkillInfos.Free;

  FNameMap.Free;

  FTextDataConfig.Free;
  inherited Destroy;
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
  for i := 0 to ADest.Count - 1 do
  begin
    o := ADest.Items[i] as TJSONObject;

    if o.IndexOfName('index')>=0 then
    begin
      index := o.Integers['index'];
      MergeJson(o, ASrc[index]);

      ADest.Items[i] := ASrc[index].Clone;
    end;
  end;
end;

function TListsManager.AssembleConfig(APaths: TStrings;
  ALegacyData: TJsonObjectList): TJSONObject;
var
  AConfig: TJsonResource;
  Path: String;
begin
  Result := TJSONObject.Create;
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
var
  idx: Integer;
begin

  idx := FSpellMap.IndexOf(AID);

  if idx < 0 then
  begin
    raise Exception.CreateFmt('Spell not found "%s"',[AID]);
  end;

  Result := TSpellInfo(FSpellMap.Objects[idx]);
end;

function TListsManager.FactionIndexToString(AIndex: TCustomID): AnsiString;
var
  info: TFactionInfo;
  i: Integer;
begin
  //todo:  optimize
  Result := '';
  for i := 0 to FFactionInfos.Count - 1 do
  begin
    info := FFactionInfos[i];
    if info.Index = AIndex then
    begin
      Exit(info.ID);
    end;
  end;

  raise Exception.CreateFmt('Faction not found: %d',[AIndex]);
end;

function TListsManager.GetFaction(const AID: AnsiString): TFactionInfo;
var
  idx: Integer;
begin
  idx := FFactionMap.IndexOf(AID);

  if idx < 0 then
  begin
    raise Exception.CreateFmt('Faction not found "%s"',[AID]);
  end;

  Result := TFactionInfo(FFactionMap.Objects[idx]);
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
      Exit(info.ID);
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
      Exit(info.ID);
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
      Exit(info.ID);
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
      Exit(info.ID);
    end;
  end;

  raise Exception.CreateFmt('Hero index not found: %d',[AIndex]);
end;

procedure TListsManager.PreLoad;
begin
  LoadTextDataConfig;
  LoadSkills;
end;

procedure TListsManager.LoadFactions(APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;

  faction_names: TTextResource;
  legacy_data: TJsonObjectList;
  f,i: Integer;
  o: TJSONObject;
  info: TFactionInfo;
  iter: TJSONEnum;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;
  legacy_data := TJsonObjectList.Create(true);

  faction_names := TTextResource.Create('DATA/TOWNTYPE.TXT');

  try
    DebugLn('Loading factions');
    faction_names.Load(ResourceLoader);

    for f in [0..9] do
    begin
      o := TJSONObject.Create();

      o.Strings['name'] := faction_names.Value[0,f];

      legacy_data.Add(o);
    end;

    FConfig.Load(APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(legacy_data, FCombinedConfig);

    for iter in FCombinedConfig  do
    begin
      info := TFactionInfo.Create;
      info.ID := iter.Key;

      o := iter.Value as TJSONObject;

      FDestreamer.JSONToObject(o, info);

      info.HasTown:=o.IndexOfName('town')>=0;

      FFactionInfos.Add(info);
      FFactionMap.AddObject(info.ID, info);

      DebugLn([info.ID, ' ', info.Name]);
    end;
  finally
    faction_names.Free;
    legacy_data.Free;
    FCombinedConfig.Free;
    FConfig.Free;
  end;
end;

procedure TListsManager.LoadHeroClasses(APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
  hctraits: TTextResource;

  legacy_data: TJsonObjectList;
  i: SizeInt;
  o: TJSONObject;
  iter: TJSONEnum;
  info: THeroClassInfo;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;
  hctraits := TTextResource.Create(HERO_CLASS_TRAITS);
  legacy_data := TJsonObjectList.Create(true);
  try
    hctraits.Load(ResourceLoader);

    for i in [0..HEROCLASS_QUANTITY-1] do
    begin
      o := TJSONObject.Create();

      o.Strings['name'] := hctraits.Value[0,i+2];

      legacy_data.Add(o);
    end;

    FConfig.Load(APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(legacy_data, FCombinedConfig);

    for iter in FCombinedConfig do
    begin
      info := THeroClassInfo.Create;

      info.ID := iter.Key;

      FDestreamer.JSONToObject(iter.Value as TJSONObject, info);

      HeroClassInfos.Add(info);
      HeroClassMap.AddObject(info.ID, info);
    end;
  finally
    legacy_data.Free;
    hctraits.free;
    FCombinedConfig.Free;
    FConfig.Free;
  end;
end;

procedure TListsManager.LoadCreatures(APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;

  crtraits: TTextResource;
  legacy_data: TJsonObjectList;

  i: SizeInt;
  o: TJSONObject;
  iter: TJSONEnum;
  info: TCreatureInfo;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;

  legacy_data := TJsonObjectList.Create(true);
  crtraits := TTextResource.Create(CREATURE_TRAITS);
  try
    crtraits.Load(ResourceLoader);

    for i in [0..TextDataConfig.Creature-1] do
    begin
      o := TJSONObject.Create();

      o.Strings['name'] := crtraits.Value[0,i+1];

      legacy_data.Add(o);
    end;

    FConfig.Load(APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(legacy_data, FCombinedConfig);

    for iter in FCombinedConfig do
    begin
      info := TCreatureInfo.Create;

      info.ID := iter.Key;

      FDestreamer.JSONToObject(iter.Value as TJSONObject, info);

      CreatureInfos.Add(info);
      CreatureMap.AddObject(info.ID, info);
    end;

  finally
    FCombinedConfig.Free;
    FConfig.Free;
    crtraits.free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.LoadArtifacts(APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
  legacy_data: TJsonObjectList;
  artraits: TTextResource;
  iter: TJSONEnum;

  i: SizeInt;
  o: TJSONObject;
  info: TArtifactInfo;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;

  legacy_data := TJsonObjectList.Create(true);
  artraits := TTextResource.Create(ARTIFACT_TRAITS);
  try

    artraits.Load(ResourceLoader);

    for i in [0..TextDataConfig.Artifact-1] do
    begin
      o := TJSONObject.Create();

      o.GetOrCreateObject('text').Strings['name'] := artraits.Value[0,i+2];

      legacy_data.Add(o);
    end;

    FConfig.Load(APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(legacy_data, FCombinedConfig);

    for iter in FCombinedConfig do
    begin
      info := TArtifactInfo.Create;

      info.ID := iter.Key;

      FDestreamer.JSONToObject(iter.Value as TJSONObject, info);

      ArtifactInfos.Add(info);
      ArtifactMap.AddObject(info.ID, info);
    end;

  finally
    FCombinedConfig.Free;
    FConfig.Free;
    artraits.free;
    legacy_data.Free;
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
      info := TSkillInfo.Create;
      info.ID := SECONDARY_SKILL_NAMES[i-2];
      info.Name := sstraits.Value[0,i];
      FSkillInfos.Add(info);
      FSkillMap.AddObject(info.ID,info);
    end;

  finally
    sstraits.Free;
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

procedure TListsManager.LoadSpells(APaths: TStrings);
var
  sptrairs: TTextResource;

  row: Integer;

  legacy_config: TJsonObjectList; //index = lecacy ID
  i: integer;
  spell_config: TJSONObject;
  spell_info: TJsonResource;
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
        spell_config := TJSONObject.Create;
        loc_name := sptrairs.Value[0,row];
        spell_config.Strings['name'] := loc_name;
        spell_config.Integers['level'] := StrToIntDef(sptrairs.Value[2,row],0);
        spell_config.Integers['type'] := i;
        legacy_config.Add(spell_config);
        inc(row);
      until sptrairs.Value[0,row] = '';
    end;

    legacy_config.Add(legacy_config[legacy_config.Count-1].Clone as TJSONObject);

    for i := 0 to APaths.Count - 1 do
    begin
      spell_info := TJsonResource.Create(APaths[i]);
      try
        spell_info.Load(ResourceLoader);
        spell_info.Root.Iterate(@ProcessSpellConfig,legacy_config);
      finally
         spell_info.Free;
      end;
    end;

  finally
    legacy_config.Free;
    sptrairs.Free;
  end;

end;

procedure TListsManager.LoadHeroes(APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
  legacy_data: TJsonObjectList;
  hotraits, herobios: TTextResource;
  iter: TJSONEnum;

  i: SizeInt;
  o: TJSONObject;
  info: THeroInfo;
begin
  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;

  legacy_data := TJsonObjectList.Create(true);
  hotraits := TTextResource.Create(HERO_TRAITS);
  herobios := TTextResource.Create(HERO_BIOS);
  try

    hotraits.Load(ResourceLoader);
    herobios.Load(ResourceLoader);

    for i in [0..TextDataConfig.Hero-1] do
    begin
      o := TJSONObject.Create();

      o.GetOrCreateObject('texts').Strings['name'] := hotraits.Value[0,i+2];
      o.GetOrCreateObject('texts').Strings['biography'] := herobios.Value[0,i];

      legacy_data.Add(o);
    end;

    FConfig.Load(APaths, ResourceLoader, FCombinedConfig);

    MergeLegacy(legacy_data, FCombinedConfig);

    for iter in FCombinedConfig do
    begin
      info := THeroInfo.Create;

      info.ID := iter.Key;

      FDestreamer.JSONToObject(iter.Value as TJSONObject, info);

      FHeroInfos.Add(info);
      FHeroMap.AddObject(info.ID, info);
    end;

  finally
    FCombinedConfig.Free;
    FConfig.Free;
    hotraits.free;
    herobios.Free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.ProcessSpellConfig(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var Continue: Boolean);
const
  SPELL_TYPES: array[1..3] of TSpellType =
    (TSpellType.Adventure,TSpellType.Combat, TSpellType.Ability);

var
  legacy_config: TJsonObjectList absolute Data;

  info: TSpellInfo;
  nid: LongInt;
  lc: TJSONObject;
  sp_type: TSpellType;
begin
  Assert(Data is TJsonObjectList);

  //Aname - spell ID
  //Item - object spell config

  nid := (Item as TJSONObject).Integers['index'];

  lc := legacy_config.Items[nid];

  sp_type := SPELL_TYPES[lc.Integers['type']];
  if sp_type <>TSpellType.Ability then
  begin
    info := TSpellInfo.Create;
    info.ID := AName;
    info.Level := lc.Integers['level'];
    info.Name := lc.Strings['name'];
    info.SpellType := sp_type;
    info.Index := nid;

    FNameMap.Insert('spell.'+info.ID,nid);

    FSpellInfos.Add(info);
    FSpellMap.AddObject(info.ID,info);
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

function TListsManager.SkillNidToString(ASkill: TCustomID): AnsiString;
begin
  Result := editor_consts.SECONDARY_SKILL_NAMES[ASkill];
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
      Result := info.ID;
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Spell not found: %d',[ASpell]);
end;

end.

