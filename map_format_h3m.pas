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
     procedure ReadObjects();
     procedure ReadEvents();
   public
     constructor Create(tm: TTerrainManager); override;
     function Read(AStream: TStream): TVCMIMap;
   end;


implementation

{ TMapReaderH3m }

class procedure TMapReaderH3m.CheckMapVersion(const AVersion: DWord);
begin
  if    (AVersion <> MAP_VERSION_AB)
    and (AVersion <> MAP_VERSION_ROE)
    and (AVersion <> MAP_VERSION_SOD)
    and (AVersion <> MAP_VERSION_WOG) then
  begin
    raise Exception.Create('Invalid map format '+IntToHex(AVersion,8));
  end;
end;

constructor TMapReaderH3m.Create(tm: TTerrainManager);
begin
  inherited Create(tm);
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

procedure TMapReaderH3m.ReadDefInfo;
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

    obj.Filename := FSrc.ReadString;
    ReadObjMask(obj);

    w := FSrc.ReadWord; //todo: terrain, terrain menu
    w := FSrc.ReadWord;

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

procedure TMapReaderH3m.ReadObjects;
begin

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
      s[8-j] := FLAG_CHARS[mask_flags[i,j]];  //todo: fix reversing of mask
    end;
    obj.Mask.Insert(0,s);
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
    case FMapVersion of
      MAP_VERSION_SOD,MAP_VERSION_WOG: FSrc.Skip(13) ;
      MAP_VERSION_AB: FSrc.Skip(12);
      MAP_VERSION_ROE: FSrc.Skip(6);
    end;
    Exit;
  end;

  Attr.AITactics := TAITactics(FSrc.ReadByte);

  case FMapVersion of
    MAP_VERSION_SOD,MAP_VERSION_WOG:
      begin
        Attr.AreAllowerFactionsSet := FSrc.ReadBoolean;
      end;
    else
      begin
        Attr.AreAllowerFactionsSet := True;
      end;
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


end.

