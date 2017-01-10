{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit player_options_frame;

{$I compilersetup.inc}

{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  CheckLst, map, lists_manager, editor_types, editor_str_consts, editor_classes,
  base_info, gui_helpers;

type

  TOnGetVirtualTeam = procedure (APlayer: TPlayerColor; var ATeam: TTeam) of object;
  TOnTeamChanged = procedure (Sender: TObject; APlayer: TPlayerColor; ATeam: TTeam) of object;

  { TPlayerOptionsFrame }

  TPlayerOptionsFrame = class(TFrame)
    AllowedFactionsPermissive: TCheckBox;
    edGenerateHero: TCheckBox;
    edTeam: TComboBox;
    edMainHero: TComboBox;
    edAllowedFactions: TCheckListBox;
    edMainTown: TComboBox;
    edCanPlay: TComboBox;
    CanPlayDescription: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lbTeam: TLabel;
    lbMainHero: TLabel;
    lbMainTown: TLabel;
    lbAllowedFactions: TLabel;
    lbCanPlay: TLabel;
    procedure edTeamChange(Sender: TObject);
  private
    FMap: TVCMIMap;
    FObject: TPlayerInfo;
    FOnAlliesChanged: TOnTeamChanged;
    FOnGetVirtualTeam: TOnGetVirtualTeam;
    FTeamCache: TTeamSettings;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;

    procedure SetTeamCache(AValue: TTeamSettings);
  public
    property Map: TVCMIMap read FMap write SetMap;
    procedure EditObject(AObject: TPlayerInfo);
    procedure Commit;

    procedure FillTeams;

    property OnAlliesChanged: TOnTeamChanged read FOnAlliesChanged write FOnAlliesChanged;
    property OnGetVirtualTeam: TOnGetVirtualTeam read FOnGetVirtualTeam write FOnGetVirtualTeam;
    property TeamCache: TTeamSettings read FTeamCache write SetTeamCache;
  end;

implementation

{$R *.lfm}

{ TPlayerOptionsFrame }

procedure TPlayerOptionsFrame.edTeamChange(Sender: TObject);
begin
  if Assigned(FOnAlliesChanged) then
  begin
    FOnAlliesChanged(Self, FObject.Color, TTeam(edTeam.Items.Objects[edTeam.ItemIndex]));
  end;
end;

procedure TPlayerOptionsFrame.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TPlayerOptionsFrame.ReadData;
var
  i: Integer;
  selected_idx: Integer;

  function TownFilter(ATarget: TBaseInfo): Boolean;
  begin
    Result := TFactionInfo(ATarget).HasTown;
  end;

begin
  //player is playable if it has at least one town or hero

  if (FObject.Heroes.Count = 0) and (FObject.Towns.Count = 0) then
  begin
    edCanPlay.ItemIndex := -1;
    edCanPlay.Text := 'Not playable';
    Enabled:=false;
  end
  else begin
    edCanPlay.ItemIndex := Integer(FObject.CanPlay)-1;
    Enabled:=true;
  end;

  edAllowedFactions.FillFrom(FMap.ListsManager.FactionInfos, FObject.AllowedFactions, @TownFilter);
  AllowedFactionsPermissive.Checked:=FObject.AllowedFactions.IsPermissive;

  selected_idx := 0;
  edMainTown.AddItem(rsNone, nil);

  for i := 0 to FObject.Towns.Count - 1 do
  begin
    edMainTown.AddItem(FObject.Towns[i].MapObject.DisplayName, FObject.Towns[i]);

    if(FObject.MainTown.MapObject = FObject.Towns[i].MapObject) then
      selected_idx := i+1;
  end;
  edMainTown.ItemIndex := selected_idx;

  edGenerateHero.Checked := FObject.MainTown.GenerateHero;

  selected_idx := -1;

  for i := 0 to FObject.Heroes.Count - 1 do
  begin
    edMainHero.AddItem(FObject.Heroes[i].MapObject.FormatDisplayName(FMap.GetHeroName(FObject.Heroes[i].MapObject)), FObject.Heroes[i].MapObject);

    if(FObject.MainHero <> '') and (FObject.MainHero = FObject.Heroes[i].MapObject.Identifier) then
      selected_idx := i;
  end;

  edMainHero.ItemIndex := selected_idx;
  edMainHero.Enabled:=edMainHero.Items.Count > 1;
  FillTeams;
end;

procedure TPlayerOptionsFrame.FillTeams;
var
  i: Integer;
  selected_idx: Integer;
  team: TTeam;

  AllPlayers: TPlayers;
  player_iter: TPlayerColor;
begin
  edTeam.Items.Clear;
  selected_idx := 0;
  edTeam.AddItem(rsNone, nil);
  AllPlayers := ALL_PLAYERS;
  Exclude(AllPlayers, FObject.Color);

  for i := 0 to TeamCache.Count - 1 do
  begin
    team := TeamCache[i];

    edTeam.AddItem(team.FormatDescription(FObject.Color), team);
    AllPlayers -= team.Members;
    if team.has(FObject.Color) then
    begin
      selected_idx := i+1;
    end;
  end;

  for player_iter in AllPlayers do
  begin
    FOnGetVirtualTeam(player_iter, team);
    edTeam.AddItem(team.FormatDescription(FObject.Color), team)
  end;

  edTeam.ItemIndex := selected_idx;
end;

procedure TPlayerOptionsFrame.SetTeamCache(AValue: TTeamSettings);
begin
  if FTeamCache=AValue then Exit;
  FTeamCache:=AValue;
end;

procedure TPlayerOptionsFrame.EditObject(AObject: TPlayerInfo);
begin
  FObject := AObject;
  ReadData;
  edTeam.OnChange := @edTeamChange;
end;

procedure TPlayerOptionsFrame.Commit;
begin
  if edCanPlay.ItemIndex < 0 then
     FObject.CanPlay := TPlayableBy.None
  else
     FObject.CanPlay := TPlayableBy(edCanPlay.ItemIndex+1);

  edAllowedFactions.SaveTo(FObject.AllowedFactions, AllowedFactionsPermissive.Checked);

  if Assigned(edMainTown.Items.Objects[edMainTown.ItemIndex]) then
    FObject.MainTown.MapObject := (edMainTown.Items.Objects[edMainTown.ItemIndex] as TPlayerTown).MapObject
  else
    FObject.MainTown.MapObject := nil;

  FObject.MainTown.GenerateHero := edGenerateHero.Checked;

  if (edMainHero.ItemIndex>=0) and Assigned(edMainHero.Items.Objects[edMainHero.ItemIndex]) then
    FObject.MainHero:=(edMainHero.Items.Objects[edMainHero.ItemIndex] as TMapObject).Identifier
  else
    FObject.MainHero := '';
end;

end.

