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
  Classes, SysUtils, map, FileUtil, map_format, terrain, stream_adapter,
  editor_types;

const
  MAP_VERSION_ROE = $0e;
  MAP_VERSION_AB = $15;
  MAP_VERSION_SOD = $1c;
  MAP_VERSION_WOG = $33;

  TOTAL_FACTIONS = 9;
  TOTAL_FACTIONS_ROE = 8;

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

      Result.Difficulty := ReadByte;

      if FMapVersion <> MAP_VERSION_ROE then
      begin
        Result.LevelLimit := ReadByte;
      end else
      begin
        Result.LevelLimit := 0;
      end;
    end;

    ReadPlayerAttrs(FMap.PlayerAttributes);
  except
    FreeAndNil(Fmap);
    raise;
  end;

  FMap := nil;
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


end.

