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
unit map_format_h3m;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, map, math, FileUtil, map_format, terrain, stream_adapter,
  editor_types;

const
  MAP_VERSION_ROE = $0e;
  MAP_VERSION_AB = $15;
  MAP_VERSION_SOD = $1c;
  MAP_VERSION_WOG = $33;
  MAP_VERSION_HOTA = 30;



  TOTAL_FACTIONS = 9;
  TOTAL_FACTIONS_ROE = 8;

  HEROES_QUANTITY=156;

type

   { TMapReaderH3m }

   TMapReaderH3m = class(TBaseMapFormatHandler, IMapReader)
   strict private
     FSrc: TStreamReadAdapter;
     FMapVersion: DWord;
     FMap: TVCMIMap;

     class procedure CheckMapVersion(const AVersion: DWord); static;
   strict private
     function IsNotROE: boolean;

     procedure SkipNotImpl(count: Integer);
   strict private
     procedure ReadPlayerAttrs(Attrs: TPlayerAttrs);
     procedure ReadPlayerAttr(Attr: TPlayerAttr);
     procedure ReadSVLC();
     procedure ReadTeams();
     procedure ReadAllowedHeros();
     procedure ReadDisposedHeros();
     procedure ReadAllowedArtifacts();
     procedure ReadAllowedSpells();
     procedure ReadAllowedAbilities();
     procedure ReadRumors();
     procedure ReadPredefinedHeroes();

     procedure ReadTerrain();

     procedure ReadObjMask(obj: TMapObjectTemplate);
     procedure ReadDefInfo();

     procedure ReadCreatureSet(obj: TMapObject; nomber: Integer);
     procedure ReadArtifactsOfHero(obj: TMapObject);
     procedure ReadArtifactsToSlot(obj: TMapObject; slot: Integer);

     procedure ReadQuest(obj: TMapObject; out mis_type: integer);

     procedure ReadPandorasBox(obj: TMapObject);
     procedure ReadLocalEvent(obj: TMapObject);
     procedure ReadHero(obj: TMapObject);
     procedure ReadMonster(obj: TMapObject);
     procedure ReadSeerHut(obj: TMapObject);

     procedure ReadTown(obj: TMapObject);


     procedure ReadObjects();
     procedure ReadEvents();
   public
     constructor Create(tm: TTerrainManager); override;
     function Read(AStream: TStream): TVCMIMap;
   end;


implementation

uses editor_consts;

{ TMapReaderH3m }

class procedure TMapReaderH3m.CheckMapVersion(const AVersion: DWord);
begin
  if    (AVersion <> MAP_VERSION_AB)
    and (AVersion <> MAP_VERSION_ROE)
    and (AVersion <> MAP_VERSION_SOD)
    and (AVersion <> MAP_VERSION_WOG)
    and (AVersion <> MAP_VERSION_HOTA)then
  begin
    raise Exception.Create('Invalid map format '+IntToHex(AVersion,8));
  end;
end;

constructor TMapReaderH3m.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
end;

function TMapReaderH3m.IsNotROE: boolean;
begin
  Result :=  FMapVersion <> MAP_VERSION_ROE;
end;

function TMapReaderH3m.Read(AStream: TStream): TVCMIMap;
var
  cr_params: TMapCreateParams;
  AreAnyPalyers: boolean;

begin
  AStream.Seek(0,soBeginning);
  FSrc.Create(AStream);
  //main header part
  with FSrc do
  begin
    FMapVersion := ReadDWord;
    CheckMapVersion(FMapVersion);
    AreAnyPalyers := ReadBoolean;
    cr_params.Height := ReadDWord;
    cr_params.Width := cr_params.Height;
    cr_params.Levels := ReadByte + 1; //one level = 0
  end;

  FMap := TVCMIMap.Create(FTM,cr_params);
  Result := FMap;
  try
    //rest of header
    with FSrc do
    begin
      Result.Name := ReadString;
      Result.Description :=  ReadString;

      Result.Difficulty := TDifficulty(ReadByte);

      if FMapVersion <> MAP_VERSION_ROE then
      begin
        Result.HeroLevelLimit := ReadByte;
      end else
      begin
        Result.HeroLevelLimit := 0;
      end;
    end;

    ReadPlayerAttrs(FMap.PlayerAttributes);
    ReadSVLC();
    ReadTeams();
    ReadAllowedHeros();
    ReadDisposedHeros();
    ReadAllowedArtifacts();
    ReadAllowedSpells();
    ReadAllowedAbilities();
    ReadRumors();
    ReadPredefinedHeroes();
    ReadTerrain();
    ReadDefInfo();
    ReadObjects();
    ReadEvents();

  except
    FreeAndNil(Fmap);
    raise;
  end;

  FMap := nil;
end;

procedure TMapReaderH3m.ReadAllowedAbilities;
begin
  //TODO: ReadAllowedAbilities
  if FMapVersion>=MAP_VERSION_SOD then
  begin
    FSrc.Skip(4);
  end;
end;

procedure TMapReaderH3m.ReadAllowedArtifacts;
begin
  //todo: ReadAllowedArtifacts

  if FMapVersion <> MAP_VERSION_ROE then
    FSrc.Skip(ifthen(FMapVersion=MAP_VERSION_AB,17,18));
end;

procedure TMapReaderH3m.ReadAllowedHeros;
var
  cnt: Integer;
begin
  //TODO: ReadAllowedHeros

  cnt := ifthen(FMapVersion=MAP_VERSION_ROE,16,20);

  FSrc.Skip(cnt);

  //unknown plaseholder
  if FMapVersion<>MAP_VERSION_ROE then
  begin
    cnt :=FSrc.ReadDWord;
    FSrc.Skip(cnt);
  end;


end;

procedure TMapReaderH3m.ReadAllowedSpells;
begin
  //TODO: ReadAllowedSpells
  if FMapVersion>=MAP_VERSION_SOD then
  begin
    FSrc.Skip(9);
  end;
end;

procedure TMapReaderH3m.ReadArtifactsOfHero(obj: TMapObject);
var
  i: Integer;
  cnt: Word;
begin
  //todo: ReadArtifactsOfHero
  with FSrc do
  begin
    if not ReadBoolean then exit;

    for i := 0 to 16 - 1 do
    begin
      ReadArtifactsToSlot(obj,i);
    end;

    if FMapVersion>=MAP_VERSION_SOD then
    begin
      ReadArtifactsToSlot(obj,16);
    end;
    ReadArtifactsToSlot(obj,17); //spellbook
    if IsNotROE then
    begin
      ReadArtifactsToSlot(obj,18);
    end;

    cnt := ReadWord;
    for i := 0 to cnt - 1 do
    begin
      ReadArtifactsToSlot(obj,19+i);
    end;
  end;
end;

procedure TMapReaderH3m.ReadArtifactsToSlot(obj: TMapObject; slot: Integer);
var
  artmask: integer;
  aid: Word;
begin
  artmask := ifthen(IsNotROE,$FFFF, $FF);

  if IsNotROE then
  begin
    aid := FSrc.ReadWord;
  end
  else begin
    aid := FSrc.ReadByte;
  end;

end;

procedure TMapReaderH3m.ReadCreatureSet(obj: TMapObject; nomber: Integer);
var
  maxID,
  bytesPerCre, creid,crecnt: Integer;
  version: Boolean;
  i: Integer;
begin
  //todo:ReadCreatureSet

  version := IsNotROE;

//maxID := ifthen(version,$ffff, $ff);

  with FSrc do begin
    for i := 0 to nomber - 1 do
    begin
      if version then
      begin
        creid := ReadWord;
      end
      else begin
        creid := ReadByte;
      end;
      crecnt := ReadWord;

      //if creid = maxID then Continue;

    end;
  end;
end;

procedure TMapReaderH3m.ReadDefInfo;

function read_terrains(): TTerrainTypes;
var
  tt: TTerrainType;
  i: Integer;
  data: Word;
begin
  data:= FSrc.ReadWord;
  Result := [];
  for tt in TTerrainType do
  begin
    i := Integer(tt);

    if ((1 shl i) and data) > 0 then
    begin
      Result +=[tt];
    end;
  end;
end;

var
  obj: TMapObjectTemplate;
  cnt: DWord;
  i: Integer;
  w: Word;
  b: Byte;

begin
  cnt := FSrc.ReadDWord;

  for i := 0 to cnt - 1 do
  begin
    obj :=  TMapObjectTemplate(FMap.Templates.Add);

    obj.Animation := FSrc.ReadString;
    ReadObjMask(obj);

    obj.AllowedTerrains := read_terrains();

    obj.MenuTerrains := read_terrains();

    obj.ID := FSrc.ReadDWord;
    obj.SubID := FSrc.ReadDWord;

    b := FSrc.ReadByte;  //todo: read type
    b := FSrc.ReadByte;
    FSrc.Skip(16); //junk
    obj.ZIndex := b * Z_INDEX_OVERLAY;
  end;
end;

procedure TMapReaderH3m.ReadDisposedHeros;
var
  id: Byte;
  portr: Byte;
  name: String;
  players: Byte;
  hero_count: Byte;
  i: Integer;
begin

  //TODO: read disposed heroes

  if FMapVersion >= MAP_VERSION_SOD then
  begin
    hero_count := FSrc.ReadByte;

    for i := 0 to hero_count - 1 do
    begin
      id := FSrc.ReadByte;
      portr :=FSrc.ReadByte;
      name := FSrc.ReadString;
      players := FSrc.ReadByte;
    end;
  end;

  FSrc.Skip(31);
end;

procedure TMapReaderH3m.ReadEvents;
begin

end;

procedure TMapReaderH3m.ReadHero(obj: TMapObject);
var
  ident: DWord;
  owner: Byte;
  subid: Byte;
  hname: String;
  cnt: DWord;
  i: Integer;
  portr: Byte;
  exper: DWord;
  secSkill: Byte;
  secSkillLevel: Byte;
  formation: Byte;
  patrol: Byte;
  bio: String;
  sex: Byte;
begin
  with FSrc do
  begin
    if IsNotROE then
    begin
      ident := ReadDWord;
    end;
    owner := ReadByte;
    subid := ReadByte;

    if ReadBoolean then
    begin
      hname := ReadString;
    end;

    if FMapVersion > MAP_VERSION_AB then
    begin
      if ReadBoolean then
      begin
        exper := ReadDWord;
      end;

    end
    else
    begin
      exper := ReadDWord;
    end;

    if ReadBoolean then
    begin
      portr := ReadByte;
    end;

    if ReadBoolean then
    begin
      cnt := ReadDWord;
      for i := 0 to cnt - 1 do
      begin
        secSkill :=ReadByte;
        secSkillLevel := ReadByte;
      end;
    end;

    if ReadBoolean then
    begin
      ReadCreatureSet(obj,7);
    end;

    formation := ReadByte;
    ReadArtifactsOfHero(obj);
    patrol := ReadByte;

    if IsNotROE then
    begin
      if ReadBoolean then
      begin
        bio := ReadString;
      end;
      sex := ReadByte;

    end
    else begin

    end;

    if FMapVersion > MAP_VERSION_AB then
    begin
      if ReadBoolean then //spells
      begin
        SkipNotImpl(9);
      end;
    end
    else if FMapVersion = MAP_VERSION_AB then
    begin
      SkipNotImpl(1);
    end;

    if (FMapVersion > MAP_VERSION_AB) and ReadBoolean then //todo:customPrimSkills
    begin
      SkipNotImpl(4);
    end;

    skip(16); //junk
  end;
end;

procedure TMapReaderH3m.ReadLocalEvent(obj: TMapObject);

begin
  //todo: ReadLocalEvent

  ReadPandorasBox(obj);

  with FSrc do
  begin
    {    uchar players;
    uchar isAICan;
    uchar disableAfterFirstDay;
    }
    SkipNotImpl(3);

    skip(4); //junk
  end;

end;

procedure TMapReaderH3m.ReadMonster(obj: TMapObject);
var
  msg: String;
  ident: DWord;
  count: Word;
  character: Byte;
  never_flees: Boolean;
  not_growing: Boolean;
begin
  with FSrc do
  begin
    if IsNotROE then
    begin
      ident := ReadDWord;
    end;

    count := ReadWord;
    character := ReadByte;

    if ReadBoolean then
    begin
      msg := ReadString;
      //resourses
      SkipNotImpl(28);
      //art
      SkipNotImpl(ifthen(IsNotROE,2,1));

    end;
    never_flees := ReadBoolean;
    not_growing := ReadBoolean;

    Skip(2);//junk
  end;
end;

procedure TMapReaderH3m.ReadObjects;
var
  cnt: Integer;
  i: Integer;
  o: TMapObject;
  defnum: DWord;

  id: TObj;
  subid: TCustomID;
  temp: String;
  htid: Byte;
  j: Integer;
  ttt: DWord;
begin
  cnt := FSrc.ReadDWord;

  for i := 0 to cnt - 1 do
  begin
    o := TMapObject(FMap.Objects.Add);
    o.X := FSrc.ReadByte;
    o.Y := FSrc.ReadByte;
    o.L := FSrc.ReadByte;

    o.TemplateID := FSrc.ReadDWord;

    FSrc.Skip(5); //junk

    id := TObj(o.Template.Id);
    subid := o.Template.SubId;

    case id of
      EVENT: begin
        ReadLocalEvent(o);
      end;

      HERO,RANDOM_HERO,PRISON:begin
         ReadHero(o);
      end;

      MONSTER,
      RANDOM_MONSTER,
      RANDOM_MONSTER_L1,
      RANDOM_MONSTER_L2,
      RANDOM_MONSTER_L3,
      RANDOM_MONSTER_L4,
      RANDOM_MONSTER_L5,
      RANDOM_MONSTER_L6,
      RANDOM_MONSTER_L7:begin
        ReadMonster(o);
      end;

      OCEAN_BOTTLE,SIGN:begin
        temp := FSrc.ReadString;
        FSrc.Skip(4);//junk
      end;
      SEER_HUT:begin
        ReadSeerHut(o);
      end;

      WITCH_HUT:begin
        if IsNotROE then
        begin
          SkipNotImpl(4);
        end
        else begin

        end;
      end;

      SCHOLAR:begin
        SkipNotImpl(2);
        FSrc.Skip(6); //junk
      end;

      GARRISON,GARRISON2:begin
        SkipNotImpl(4);
        ReadCreatureSet(o,7);
        if IsNotROE then SkipNotImpl(1);
        FSrc.Skip(8);//junk
      end;

      ARTIFACT,RANDOM_ART,
      RANDOM_TREASURE_ART,
      RANDOM_MINOR_ART,
      RANDOM_MAJOR_ART,
      RANDOM_RELIC_ART,
      SPELL_SCROLL:begin
        with FSrc do
        begin
          if ReadBoolean then
          begin
            temp := ReadString;
            if ReadBoolean then
            begin
               ReadCreatureSet(o,7);
            end;
            skip(4);//junk
          end;

          case id of
            SPELL_SCROLL: SkipNotImpl(4) ;
          end;
        end;
      end;
      RESOURCE,
      RANDOM_RESOURCE:begin
        with FSrc do
        begin
          if ReadBoolean then
          begin
            temp := ReadString;
            if ReadBoolean then
            begin
               ReadCreatureSet(o,7);
            end;
            skip(4);//junk
          end;
          SkipNotImpl(4);
          Skip(4);//junk

        end;
      end;

      RANDOM_TOWN,TOWN:begin
        ReadTown(o);
      end;

      MINE,ABANDONED_MINE:
        SkipNotImpl(4);

      CREATURE_GENERATOR1,
      CREATURE_GENERATOR2,
      CREATURE_GENERATOR3,
      CREATURE_GENERATOR4:SkipNotImpl(4);

      SHRINE_OF_MAGIC_GESTURE,
      SHRINE_OF_MAGIC_INCANTATION,
      SHRINE_OF_MAGIC_THOUGHT:begin
        SkipNotImpl(1);
        fsrc.skip(3); //junk
      end;

      PANDORAS_BOX:
        ReadPandorasBox(o);
      GRAIL:
        SkipNotImpl(4);

      RANDOM_DWELLING,RANDOM_DWELLING_LVL,RANDOM_DWELLING_FACTION:begin
        SkipNotImpl(4); //owner

        case id of
          RANDOM_DWELLING,RANDOM_DWELLING_LVL:begin
            ttt := FSrc.ReadDWord;
            if ttt <> 0 then
              SkipNotImpl(2);
          end;
        end;

        case id of
          RANDOM_DWELLING,RANDOM_DWELLING_FACTION: SkipNotImpl(2) ;
        end;
      end;

      QUEST_GUARD:ReadQuest(o, j);

      SHIPYARD:SkipNotImpl(4); //owner

      HERO_PLACEHOLDER:begin

        with FSrc do
        begin
          Skip(1); //unknown
          htid := ReadByte;
          if htid = $ff then
          begin
            SkipNotImpl(1);
          end
          else begin

          end;
        end;
      end;

      LIGHTHOUSE:SkipNotImpl(4);


    end;
  end;
end;

procedure TMapReaderH3m.ReadObjMask(obj: TMapObjectTemplate);
type
   TFlag = (None=0,Block, Active);
const
  FLAG_CHARS: array [TFlag] of char = (MASK_NOT_VISIBLE, MASK_BLOCKED,MASK_ACTIVABLE);

var
  mask_flags: array[0..5,0..7] of TFlag;
  b: Byte;
  i: Integer;
  j: Byte;
  s: String;
begin
  FillChar(mask_flags,SizeOf(mask_flags),#0);

  for i := Low(mask_flags) to High(mask_flags) do
  begin
    b := FSrc.ReadByte;
    for j := Low(mask_flags[i]) to High(mask_flags[i]) do
    begin
      if ((b shr j) and 1 ) =0 then
        mask_flags[i,j] := TFlag.Block;
    end;
  end;

  for i := Low(mask_flags) to High(mask_flags) do
  begin
    b := FSrc.ReadByte;
    for j := Low(mask_flags[i]) to High(mask_flags[i]) do
    begin
      if ((b shr j) and 1 ) = 1 then
        mask_flags[i,j] := TFlag.Active;
    end;
  end;

  for i := High(mask_flags) downto Low(mask_flags) do
  begin
    s := StringOfChar(MASK_NOT_VISIBLE, 8);
    for j := High(mask_flags[i]) downto Low(mask_flags[i]) do
    begin
      s[j+1] := FLAG_CHARS[mask_flags[i,j]];
    end;
    obj.Mask.Insert(0,s);
  end;



end;

procedure TMapReaderH3m.ReadPandorasBox(obj: TMapObject);
var
  is_guard: Boolean;
  is_guard_msg: Boolean;
  i: Integer;
  exper: DWord;
  mana: DWord;
  morale: ShortInt;
  luck: ShortInt;
  res: DWord;
  prskill: Byte;
  cnt: Integer;
  sskill: Byte;
  sskilllevel: Byte;
  artid: Word;
  mess: String;
  spell: Byte;
begin
  with FSrc do
  begin
    is_guard_msg := ReadBoolean;
    if is_guard_msg then
    begin
      mess := ReadString;

      is_guard := ReadBoolean;
      if is_guard then
      begin
        ReadCreatureSet(obj,7);

      end;
      Skip(4); //junk
    end;

    exper := ReadDWord;
    mana:=ReadDWord;
    morale := Shortint(ReadByte);
    luck := Shortint(ReadByte);

    for i := 1 to 7 do
    begin
      res := ReadDWord;
    end;

    for i := 1 to 4 do
    begin
      prskill := ReadByte;
    end;

    cnt := ReadByte;

    for i := 0 to cnt - 1 do
    begin
      sskill := ReadByte;
      sskilllevel :=ReadByte;
    end;

    cnt := ReadByte;

    for i := 0 to cnt - 1 do
    begin
      if IsNotROE then
      begin
        artid := ReadWord;
      end
      else begin
        artid := ReadByte;
      end;
    end;

    cnt := ReadByte;

    for i := 0 to cnt - 1 do
    begin
      spell := ReadByte;
    end;

    cnt := ReadByte;

    ReadCreatureSet(obj,cnt); //TODO: real creature set

    Skip(8);
  end;
end;

procedure TMapReaderH3m.ReadPlayerAttr(Attr: TPlayerAttr);
var
  faction_mask: Word;
  faction_count: Integer;
  faction_n: TFactionID;
  heroes_count: DWord;
  hero: TCustomHero;
  h: Integer;

begin
  Attr.CanHumanPlay := FSrc.ReadBoolean;
  Attr.CanComputerPlay := FSrc.ReadBoolean;

  if not (Attr.CanComputerPlay or Attr.CanHumanPlay) then
  begin
    if FMapVersion >=MAP_VERSION_SOD then
    begin
      FSrc.Skip(13)
    end
    else begin
     case FMapVersion of
      MAP_VERSION_AB: FSrc.Skip(12);
      MAP_VERSION_ROE: FSrc.Skip(6);
    end;
    end;

    Exit;
  end;

  Attr.AITactics := TAITactics(FSrc.ReadByte);


  if FMapVersion >=MAP_VERSION_SOD then
  begin
    Attr.AreAllowerFactionsSet := FSrc.ReadBoolean;
  end
  else
  begin
    Attr.AreAllowerFactionsSet := True;
  end;

  case FMapVersion of
    MAP_VERSION_ROE:begin
      faction_count := TOTAL_FACTIONS_ROE;
      faction_mask := FSrc.ReadByte;
    end;
    else
       begin
         faction_count := TOTAL_FACTIONS;
         faction_mask := FSrc.ReadWord;
       end;
  end;

  for faction_n := 0 to faction_count - 1 do
  begin
    if ((1 shl faction_n) and faction_mask) > 0 then
    begin
      Attr.AllowedFactions.Insert(faction_n);
    end;
  end;

  Attr.IsFactionRandom := FSrc.ReadBoolean;

  Attr.HasMainTown := FSrc.ReadBoolean;
  if Attr.HasMainTown then
  begin
    if FMapVersion = MAP_VERSION_ROE then
    begin
      Attr.GenerateHeroAtMainTown := True;
      Attr.MainTownType := FACTION_RANDOM;
    end else
    begin
      Attr.GenerateHeroAtMainTown := FSrc.ReadBoolean;
      Attr.MainTownType := FSrc.ReadFaction;
    end;

    with Attr,FSrc do
    begin
      MainTownX := ReadByte;
      MainTownY := ReadByte;
      MainTownL := ReadByte;
    end;

  end; //main town
  with Attr,FSrc do
  begin
    RandomHero := ReadBoolean; //TODO: check
    MainHeroClass := ReadFaction;

    if MainHeroClass <> ID_RANDOM then
    begin
      MainHeroPortrait := ReadIDByte;
      MainHeroName := ReadString;
    end;
  end;

  if FMapVersion <> MAP_VERSION_ROE then
  begin
    FSrc.Skip(1); //unknown byte
    heroes_count := FSrc.ReadDWord;

    for h := 0 to heroes_count - 1 do
    begin
      hero := TCustomHero(Attr.CustomHeros.Add);
      hero.Portrait := FSrc.ReadIDByte;
      hero.Name := FSrc.ReadString;
    end;
  end;

end;

procedure TMapReaderH3m.ReadPlayerAttrs(Attrs: TPlayerAttrs);
var
  player_color: TPlayerColor;
begin
  for player_color in TPlayerColor do
  begin
    ReadPlayerAttr(Attrs.GetAttr(Integer(player_color)));
  end;
end;

procedure TMapReaderH3m.ReadPredefinedHeroes;

var
  enabled: Boolean;
  experience: DWord;
  bio: String;
  cnt: Word;
  i: Integer;
begin

  //TODO:  ReadPredefinedHeroes

  if FMapVersion < MAP_VERSION_SOD then Exit;

  for i := 0 to HEROES_QUANTITY - 1 do
  begin
    enabled := FSrc.ReadBoolean;
    if not enabled then Continue;

    if FSrc.ReadBoolean then
    begin
      experience := FSrc.ReadDWord;
    end;

    if FSrc.ReadBoolean then
    begin
      cnt := FSrc.ReadDWord;
      fsrc.Skip(cnt*2);
    end;

    if FSrc.ReadBoolean then  //arts
    begin
      FSrc.Skip(19*2);

      cnt := FSrc.ReadWord;

       FSrc.Skip(cnt*2);
    end;

    if FSrc.ReadBoolean then  //bio
    begin
      bio :=  FSrc.ReadString;
    end;

    FSrc.Skip(1); //sex

    if FSrc.ReadBoolean then  //spells
    begin
      FSrc.Skip(9);
    end;

    if FSrc.ReadBoolean then
    begin
      FSrc.Skip(4); //prim skills
    end;

  end;
end;

procedure TMapReaderH3m.ReadQuest(obj: TMapObject; out mis_type: integer);
var
  limit: DWord;
  temp: String;
  cnt: Byte;
  i: Integer;
begin
  mis_type := FSrc.ReadByte;

  with FSrc do
  begin
    case mis_type of
      0: Exit;
      2:begin
        SkipNotImpl(4);
      end;
      1,3,4:begin
        SkipNotImpl(4);
      end;
      5:begin
        cnt := ReadByte;
        for i := 0 to cnt - 1 do
        begin
          SkipNotImpl(2);
        end;
      end;
      6:begin
         cnt := ReadByte;
         for i := 0 to cnt - 1 do
        begin
          SkipNotImpl(4); //creatures
        end;
      end;
      7:begin
        SkipNotImpl(28);  //resources
      end;
      8,9:begin
        SkipNotImpl(1);
      end;

    end;

    //
    limit := ReadDWord;
    temp := ReadString;//firstVisitText
    temp := ReadString;//nextVisitText
    temp := ReadString;//completedText
  end;
end;

procedure TMapReaderH3m.ReadRumors;
var
  cnt: DWord;
  name: String;
  txt: String;
  i: Integer;
begin
  //TODO: ReadRumors
  cnt := FSrc.ReadDWord;
  for i := 0 to cnt - 1 do
  begin
    name := FSrc.ReadString;
    txt :=  FSrc.ReadString;
  end;
end;

procedure TMapReaderH3m.ReadSeerHut(obj: TMapObject);
var
    mis_type: Integer;
    reward_type: Byte;
    aid: Byte;
begin
  with FSrc do begin
    if IsNotROE then
    begin
      ReadQuest(obj,mis_type);
    end
    else begin
      aid := FSrc.ReadByte;
      if aid <>255 then
      begin
        mis_type := 5;
      end else begin
        mis_type := 0;
      end;
    end;

    if mis_type>0 then
    begin
      reward_type := ReadByte;
      case reward_type of
        1: SkipNotImpl(4);
        2: SkipNotImpl(4);
        3: SkipNotImpl(1);
        4: SkipNotImpl(1);
        5: SkipNotImpl(5);
        6:SkipNotImpl(2);
        7:SkipNotImpl(2);
        8: SkipNotImpl(ifthen(IsNotROE,2,1));
        9:SkipNotImpl(1);
        10:SkipNotImpl(ifthen(IsNotROE,4,3));

      end;
      skip(2);//junk
    end
    else
      skip(3); // missionType==255 + junk
  end;

end;

procedure TMapReaderH3m.ReadSVLC;
var
  cond: Byte;
begin
  cond := FSrc.ReadByte;
  //TODO:ReadSVC
  if not cond = 255 then raise Exception.Create('SVLC not implenmeted');

  cond := FSrc.ReadByte;
  //TODO:ReadSLC
  if not cond = 255 then raise Exception.Create('SVLC not implenmeted');


end;

procedure TMapReaderH3m.ReadTeams;
var
  team_count: Byte;
  player: TPlayerColor;
  team: Byte;
  attr: TPlayerAttr;
begin
  team_count := FSrc.ReadByte;

  if team_count > 0 then
  begin
    for player in TPlayerColor do
    begin
      team := FSrc.ReadByte;
      FMap.PlayerAttributes.GetAttr(Integer(player)).TeamId := team;
    end;
  end
  else begin
    for player in TPlayerColor do
    begin
      attr :=FMap.PlayerAttributes.GetAttr(Integer(player));
      if attr.CanComputerPlay or attr.CanHumanPlay then
      begin
        attr.TeamId := team_count;
        Inc(team_count);
      end;
    end;
  end;
end;

procedure TMapReaderH3m.ReadTerrain;
  procedure ReadLevel(Level:Integer);
  var
    tile: TMapTile;
    x: Integer;
    y: Integer;
  begin
    for y := 0 to FMap.Height - 1 do
    begin
      for x := 0 to FMap.Width - 1 do
      begin
        tile := FMap.GetTile(Level,x,y);

        with tile, FSrc do
        begin
          TerType := TTerrainType(ReadByte);
          TerSubType := ReadByte;
          RiverType := ReadByte;
          RiverDir := ReadByte;
          RoadType := ReadByte;
          RoadDir := ReadByte;
          Flags := ReadByte;
        end;
      end;
    end;
  end;
var
  i: Integer;
begin
  for i := 0 to FMap.Levels - 1 do
  begin
    ReadLevel(i);
  end;
end;

procedure TMapReaderH3m.ReadTown(obj: TMapObject);
var
  temp: String;
  cnt: DWord;
  i: Integer;
begin
  with FSrc do
  begin
    if IsNotROE then
    begin
      SkipNotImpl(4);
    end;
    SkipNotImpl(1); //owner

    if ReadBoolean then//name
    begin
      temp := ReadString;
    end;

    if ReadBoolean then
    begin
      ReadCreatureSet(obj,7);
    end;

    SkipNotImpl(1); //formation

    if ReadBoolean then //custom buildings
    begin
      SkipNotImpl(6);
      SkipNotImpl(6);
    end
    else
    begin
      SkipNotImpl(1) ;  //hasfort
    end;

    if IsNotROE then
    begin
      SkipNotImpl(9); //obligatorySpells
    end;
    SkipNotImpl(9);//possibleSpells

    cnt := ReadDWord;
    for i := 0 to cnt - 1 do
    begin
      temp := ReadString;
      temp := ReadString;

      SkipNotImpl(28);

      SkipNotImpl(1);
      if FMapVersion > MAP_VERSION_AB then SkipNotImpl(1);
      SkipNotImpl(1);

      SkipNotImpl(3);

      skip(17);

      SkipNotImpl(6);
      SkipNotImpl(14);
      skip(4); //junk
    end;
    if FMapVersion > MAP_VERSION_AB then SkipNotImpl(1);
    Skip(3); //junk
  end;
end;

procedure TMapReaderH3m.SkipNotImpl(count: Integer);
begin
  FSrc.Skip(count);
end;


end.


