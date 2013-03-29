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

  { TSkillInfo }

  TSkillInfo = class
  private
    FID: AnsiString;
    FName: TLocalizedString;
    procedure SetID(AValue: AnsiString);
    procedure SetName(AValue: TLocalizedString);
  public
    property ID: AnsiString read FID write SetID;
    property Name: TLocalizedString read FName write SetName;
  end;

  { TSkillInfos }

  TSkillInfos = class (specialize TFPGObjectList<TSkillInfo>)
  public
    procedure FillWithAllIds(AList: TStrings);
  end;

  TSpellType = (Adventure, Combat, Ability);

  { TSpellInfo }

  TSpellInfo = class
  private
    FNid: TSpellID;
    Ftype: TSpellType;
    FID: AnsiString;
    FLevel: integer;
    FName: TLocalizedString;
    procedure SetNid(AValue: TSpellID);
    procedure setType(AValue: TSpellType);
    procedure SetID(AValue: AnsiString);
    procedure SetLevel(AValue: integer);
    procedure SetName(AValue: TLocalizedString);
  public
    constructor Create;
    property Nid: TSpellID read FNid write SetNid;
    property ID: AnsiString read FID write SetID;
    property Level: integer read FLevel write SetLevel;
    property Name: TLocalizedString read FName write SetName;
    property SpellType: TSpellType read FType write SetType;
  end;

  { TSpellInfos }

  TSpellInfos = class (specialize TFPGObjectList<TSpellInfo>)
  public
    //all except abilities
    procedure FillWithAllIds(AList: TStrings);
  end;


  { TListsManager }

  TListsManager = class (TFSConsumer)
  strict private
    FNameMap: TNameToIdMap;
    FSkillInfos: TSkillInfos;
    FSpellInfos: TSpellInfos;
    procedure LoadSkills;

    procedure ProcessSpellConfig(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

    procedure LoadSpells;
  strict private //Accesors
    function GetPlayerName(const APlayer: TPlayer): TlocalizedString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load;
  public
    property PlayerName[const APlayer: TPlayer]: TlocalizedString read GetPlayerName;

    function SIDIdNID(AID: AnsiString): TCustomID;

    function SkillNidToString (ASkill: TCustomID): AnsiString;
    property SkillInfos: TSkillInfos read FSkillInfos;

    //convert legacy id to vcmi id
    function SpellNidToString (ASpell: TCustomID): AnsiString;
    property SpellInfos: TSpellInfos read FSpellInfos;
  end;

implementation

uses FileUtil, editor_consts;

const
  SEC_SKILL_TRAITS = 'data\sstraits';
  SPELL_TRAITS     = 'data\sptraits';

  SPELL_INFO_NAME       = 'config\spell_info';

const
  PLAYER_NAMES: array[TPlayer] of AnsiString = (
    'No player',
    'Player 1 (red)',
    'Player 2 (blue)',
    'Player 3 (tan)',
    'Player 4 (green)',
    'Player 5 (orange)',
    'Player 6 (purple)',
    'Player 7 (teal)',
    'Player 8 (pink)');

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

constructor TSpellInfo.Create;
begin
  FNid := -1;
end;

procedure TSpellInfo.SetID(AValue: AnsiString);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TSpellInfo.SetLevel(AValue: integer);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue;
end;

procedure TSpellInfo.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TSpellInfo.SetNid(AValue: TSpellID);
begin
  if FNid = AValue then Exit;
  FNid := AValue;
end;

{ TSkillInfo }

procedure TSkillInfo.SetID(AValue: AnsiString);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TSkillInfo.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

{ TListsManager }

constructor TListsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameMap := TNameToIdMap.Create;

  FSkillInfos := TSkillInfos.Create(True);
  FSpellInfos := TSpellInfos.Create(True);
end;

destructor TListsManager.Destroy;
begin
  FSpellInfos.Free;
  FSkillInfos.Free;
  FNameMap.Free;
  inherited Destroy;
end;

function TListsManager.GetPlayerName(const APlayer: TPlayer): TlocalizedString;
begin
  //TODO: get localized name;
  Result := PLAYER_NAMES[APlayer];
end;

procedure TListsManager.Load;
begin
  LoadSkills;
  LoadSpells;
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
      info.ID := 'skill.'+SKILL_NAMES[i-2];
      info.Name := sstraits.Value[0,i];
      FSkillInfos.Add(info);
    end;

  finally
    sstraits.Free;
  end;
end;

procedure TListsManager.LoadSpells;
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
  spell_info := TJsonResource.Create;

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

    ResourceLoader.LoadResource(spell_info,TResourceType.Json,SPELL_INFO_NAME);

    spell_config := spell_info.Root.Objects['spells'];

    spell_config.Iterate(@ProcessSpellConfig,legacy_config);


  finally
    spell_info.Free;
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
begin
  Assert(Data is TJsonObjectList);

  //Aname - spell ID
  //Item - object spell config

  info := TSpellInfo.Create;

  nid := (Item as TJSONObject).Integers['id'];

  lc := legacy_config.Items[nid];

  info.ID := 'spell.'+AName;
  info.Level := lc.Integers['level'];
  info.Name := lc.Strings['name'];
  info.SpellType := SPELL_TYPES[lc.Integers['type']];
  info.Nid := nid;

  FNameMap.Insert(info.ID,nid);

  FSpellInfos.Add(info);
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
  Result := 'skill.' + editor_consts.SKILL_NAMES[ASkill];
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
    if info.Nid = ASpell then
    begin
      Result := info.ID;
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Spell not found: %d',[ASpell]);
end;

end.

