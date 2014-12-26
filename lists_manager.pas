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
  filesystem_base, editor_types, editor_utils,
  vcmi_json,h3_txt;

type

  { TBaseInfo }

  TBaseInfo = class abstract
  private
    FID: AnsiString;
    FName: TLocalizedString;
    FIndex: TCustomID;

    procedure SetID(AValue: AnsiString);
    procedure SetName(AValue: TLocalizedString);
    procedure SetIndex(AValue: TCustomID);
  protected
    function GetFullID: AnsiString; virtual;

  public
    constructor Create;
    property ID: AnsiString read FID write SetID;
    property Name: TLocalizedString read FName write SetName;
    property FullID: AnsiString read GetFullID;
    property Index: TCustomID read FIndex write SetIndex default -1;
  end;

  { TSkillInfo }

  TSkillInfo = class (TBaseInfo)
  protected
    function GetFullID: AnsiString; override;
  end;

  { TSkillInfos }

  TSkillInfos = class (specialize TFPGObjectList<TSkillInfo>)
  public
    procedure FillWithAllIds(AList: TStrings);
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
    procedure FillWithAllIds(AList: TStrings);
  end;

  { TFactionInfo }

  TFactionInfo = class(TBaseInfo)
  private
    FCapitolDefName: AnsiString;
    FCastleDefName: AnsiString;
    FGuildLevel: Integer;
    FVillageDefName: AnsiString;
    procedure SetCapitolDefName(AValue: AnsiString);
    procedure SetCastleDefName(AValue: AnsiString);
    procedure SetGuildLevel(AValue: Integer);
    procedure SetVillageDefName(AValue: AnsiString);
  public
    property VillageDefName: AnsiString read FVillageDefName write SetVillageDefName;
    property CastleDefName: AnsiString read FCastleDefName write SetCastleDefName;
    property CapitolDefName:  AnsiString read FCapitolDefName write SetCapitolDefName;
    property GuildLevel: Integer read FGuildLevel write SetGuildLevel;
  end;

  { TFactionInfos }

  TFactionInfos = class (specialize TFPGObjectList<TFactionInfo>)
  public
    procedure FillWithAllIds(AList: TStrings);
  end;



  { TListsManager }

  TListsManager = class (TFSConsumer)
  strict private
    FNameMap: TNameToIdMap;
    FSkillInfos: TSkillInfos;
    FSkillMap: TStringList;

    FSpellInfos: TSpellInfos;
    FSpellMap: TStringList;

    FFactionInfos: TFactionInfos;

    procedure LoadSkills;

    procedure ProcessSpellConfig(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

  strict private //Accesors
    function GetPlayerName(const APlayer: TPlayer): TLocalizedString;
  strict private
    function AssembleConfig(APaths: TStrings; ALegacyData: TJsonObjectList): TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load;

    procedure LoadFactions(APaths: TStrings);
    procedure LoadSpells(APaths: TStrings);
  public
    property PlayerName[const APlayer: TPlayer]: TLocalizedString read GetPlayerName;

    function SIDIdNID(AID: AnsiString): TCustomID;

    function SkillNidToString (ASkill: TCustomID): AnsiString;
    property SkillInfos: TSkillInfos read FSkillInfos;
    property SkillMap:TStringList read FSkillMap;

    //convert legacy id to vcmi id
    function SpellNidToString (ASpell: TCustomID): AnsiString;
    property SpellInfos: TSpellInfos read FSpellInfos;
    function GetSpell(const AID: AnsiString): TSpellInfo;
    property SpellMap: TStringList read FSpellMap;
  end;

implementation

uses FileUtil, LazLoggerBase, editor_consts;

const
  SEC_SKILL_TRAITS = 'data\sstraits';
  SPELL_TRAITS     = 'data\sptraits';

  SPELL_INFO_NAME       = 'config\spell_info';

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

procedure TFactionInfo.SetGuildLevel(AValue: Integer);
begin
  if FGuildLevel = AValue then Exit;
  FGuildLevel := AValue;
end;

procedure TFactionInfo.SetVillageDefName(AValue: AnsiString);
begin
  if FVillageDefName = AValue then Exit;
  FVillageDefName := AValue;
end;

{ TFactionInfos }

procedure TFactionInfos.FillWithAllIds(AList: TStrings);
var
  faction: TFactionInfo;
begin
  for faction in Self do
  begin
    AList.Add(faction.ID);
  end;
end;

{ TSkillInfo }

function TSkillInfo.GetFullID: AnsiString;
begin
  Result := 'skill.'+ID
end;

{ TBaseInfo }

constructor TBaseInfo.Create;
begin
  FIndex := ID_INVALID;
end;

function TBaseInfo.GetFullID: AnsiString;
begin
  Result := ID;
end;

procedure TBaseInfo.SetID(AValue: AnsiString);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TBaseInfo.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TBaseInfo.SetIndex(AValue: TCustomID);
begin
  if FIndex = AValue then Exit;
  FIndex := AValue;
end;

{ TSkillInfos }

procedure TSkillInfos.FillWithAllIds(AList: TStrings);
var
  skill: TSkillInfo;
begin
  for skill in Self do
  begin
    AList.Add(skill.ID);
  end;
end;

{ TSpellInfos }

procedure TSpellInfos.FillWithAllIds(AList: TStrings);
var
  spell: TSpellInfo;
begin
  for spell in Self do
  begin
    if spell.SpellType <> TSpellType.Ability then
      AList.Add(spell.ID);
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
  FNameMap := TNameToIdMap.Create;

  FSkillInfos := TSkillInfos.Create(True);
  FSkillMap := CrStrList;

  FSpellInfos := TSpellInfos.Create(True);
  FSpellMap := CrStrList;

  FFactionInfos := TFactionInfos.Create(True);
end;

destructor TListsManager.Destroy;
begin
  FFactionInfos.Free;

  FSpellMap.Free;
  FSpellInfos.Free;

  FSkillMap.Free;
  FSkillInfos.Free;

  FNameMap.Free;
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

function TListsManager.AssembleConfig(APaths: TStrings;
  ALegacyData: TJsonObjectList): TJSONObject;
var
  AConfig: TJsonResource;
  Path: String;
  i: Integer;
  o: TJSONObject;
  index: LongInt;

begin
  Result := TJSONObject.Create;
  AConfig := TJsonResource.Create;
  try
    try
      for Path in APaths do
      begin
        ResourceLoader.LoadResource(AConfig,TResourceType.Json, Path);

        MergeJson(AConfig.Root, Result);
      end;

      if Assigned(ALegacyData) then
      begin

        for i := 0 to Result.Count - 1 do
        begin
          o := Result.Items[i] as TJSONObject;

          if o.IndexOfName('index')>=0 then
          begin
            index := o.Integers['index'];
            MergeJson(o, ALegacyData[index]);

            Result.Items[i] := ALegacyData[index].Clone;

          end;

        end;

      end;
    finally
      FreeAndNil(AConfig);
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

procedure TListsManager.Load;
begin
  LoadSkills;
end;

procedure TListsManager.LoadFactions(APaths: TStrings);
var
  faction_config: TJSONObject;
  faction_names: TTextResource;
  legacy_data: TJsonObjectList;
  f,i: Integer;
  o: TJSONObject;
  info: TFactionInfo;
begin

  legacy_data := TJsonObjectList.Create(true);

  faction_names := TTextResource.Create;

  ResourceLoader.LoadResource(faction_names,TResourceType.Text,'DATA/TOWNTYPE.TXT');

  for f in [0..9] do
  begin
    o := TJSONObject.Create();

    o.Strings['name'] := faction_names.Value[0,f];

    legacy_data.Add(o);
  end;
  faction_config := AssembleConfig(APaths,legacy_data);

  //loading
  try
    DebugLn('Loading factions');
    for i := 0 to faction_config.Count - 1 do
    begin
      info := TFactionInfo.Create;
      info.ID:=faction_config.Names[i];

      o := faction_config.Items[i] as TJSONObject;
      info.Index:= o.Integers['index'];
      info.Name := o.Strings['name'];

      FFactionInfos.Add(info);

      DebugLn([i, ' ', info.ID, ' ', info.Name]);
    end;
  finally
    faction_config.Free;
    faction_names.Free;
    legacy_data.Free;
  end;
end;

procedure TListsManager.LoadSkills;
var
  sstraits: TTextResource;
  info: TSkillInfo;
  i: Integer;
begin
  for i := 0 to SKILL_QUANTITY - 1 do
  begin
    FNameMap.Insert('skill.' + SKILL_NAMES[i], i);
  end;

  FSkillInfos.Clear;
  sstraits := TTextResource.Create;
  try
    ResourceLoader.LoadResource(sstraits,TResourceType.Text, SEC_SKILL_TRAITS);

    for i := 2 to sstraits.RowCount - 1 do
    begin
      info := TSkillInfo.Create;
      info.ID := SKILL_NAMES[i-2];
      info.Name := sstraits.Value[0,i];
      FSkillInfos.Add(info);
      FSkillMap.AddObject(info.ID,info);
    end;

  finally
    sstraits.Free;
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
  sptrairs := TTextResource.Create;
  legacy_config := TJsonObjectList.Create(True);


  try
    //load sptraits
    ResourceLoader.LoadResource(sptrairs,TResourceType.Text,SPELL_TRAITS);

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
      spell_info := TJsonResource.Create;
      try
        ResourceLoader.LoadResource(spell_info,TResourceType.Json,APaths[i]);
        spell_config := spell_info.Root;
        spell_config.Iterate(@ProcessSpellConfig,legacy_config);
      finally
         spell_info.Free;
      end;
    end;

  finally
    legacy_config.Free;
    sptrairs.Free;
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
  Result := {'skill.' + }editor_consts.SKILL_NAMES[ASkill];
end;

function TListsManager.SpellNidToString(ASpell: TCustomID): AnsiString;
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

