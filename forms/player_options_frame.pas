{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge,net

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
unit player_options_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  CheckLst, map, lists_manager, editor_types, editor_str_consts, editor_classes,
  gui_helpers;

type

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
    FOnAlliesChanged: TNotifyEvent;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure FillTeams;
    procedure SetOnAlliesChanged(AValue: TNotifyEvent);
  public
    property Map: TVCMIMap read FMap write SetMap;
    procedure EditObject(AObject: TPlayerInfo);
    procedure Commit;

    property OnAlliesChanged: TNotifyEvent read FOnAlliesChanged write SetOnAlliesChanged;
  end;

implementation

{$R *.lfm}

{ TPlayerOptionsFrame }

procedure TPlayerOptionsFrame.edTeamChange(Sender: TObject);
begin
  if Assigned(FOnAlliesChanged) then
  begin
    FOnAlliesChanged(Self);
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
  team: TTeam;
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

  edAllowedFactions.FillFromCondition(FMap.ListsManager.TownMap, FObject.AllowedFactions);
  AllowedFactionsPermissive.Checked:=FObject.AllowedFactions.IsPermissive;

  selected_idx := 0;
  edMainTown.AddItem(rsNone, nil);

  for i := 0 to FObject.Towns.Count - 1 do
  begin
    edMainTown.AddItem(FObject.Towns[i].MapObject.DisplayName, FObject.Towns[i]);

    if(FObject.MainTown = FObject.Towns[i].Identifier) then
      selected_idx := i+1;
  end;
  edMainTown.ItemIndex := selected_idx;

  edGenerateHero.Checked := FObject.GenerateHeroAtMainTown;

  selected_idx := 0;
  edMainHero.AddItem(rsNone, nil);

  for i := 0 to FObject.Heroes.Count - 1 do
  begin
    edMainHero.AddItem(FObject.Heroes[i].MapObject.FormatDisplayName(FMap.GetHeroName(FObject.Heroes[i].MapObject)), FObject.Heroes[i]);

    if(FObject.MainHero = FObject.Heroes[i].Identifier) then
      selected_idx := i+1;
  end;

  edMainHero.ItemIndex := selected_idx;

  selected_idx := 0;
  edTeam.AddItem(rsNone, nil);

  for i := 0 to FMap.Teams.Count - 1 do
  begin
    team := FMap.Teams[i];

    edTeam.AddItem(team.FormatDescription(FObject.Color), team);
  end;

  edTeam.ItemIndex := selected_idx;
end;

procedure TPlayerOptionsFrame.FillTeams;
begin

end;

procedure TPlayerOptionsFrame.SetOnAlliesChanged(AValue: TNotifyEvent);
begin
  FOnAlliesChanged:=AValue;
end;

procedure TPlayerOptionsFrame.EditObject(AObject: TPlayerInfo);
begin
  FObject := AObject;
  ReadData;
end;

procedure TPlayerOptionsFrame.Commit;
begin
  if edCanPlay.ItemIndex < 0 then
     FObject.CanPlay := TPlayableBy.None
  else
     FObject.CanPlay := TPlayableBy(edCanPlay.ItemIndex+1);

  edAllowedFactions.SaveToCondition(FMap.ListsManager.FactionMap, FObject.AllowedFactions, AllowedFactionsPermissive.Checked);

  if Assigned(edMainTown.Items.Objects[edMainTown.ItemIndex]) then
    FObject.MainTown:=(edMainTown.Items.Objects[edMainTown.ItemIndex] as TPlayerTown).Identifier
  else
    FObject.MainTown := '';

  FObject.GenerateHeroAtMainTown := edGenerateHero.Checked;

  if Assigned(edMainHero.Items.Objects[edMainHero.ItemIndex]) then
    FObject.MainHero:=(edMainHero.Items.Objects[edMainHero.ItemIndex] as TPlayerHero).Identifier
  else
    FObject.MainHero := '';
end;

end.

