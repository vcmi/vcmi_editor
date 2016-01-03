{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit player_options_form;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, map, editor_types, player_options_frame;

type

  { TPlayerOptionsForm }

  TPlayerOptionsForm = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btCancel: TButton;
    btOk: TButton;
    pcMain: TPageControl;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
    FEditors: array[TPlayerColor] of TPlayerOptionsFrame;
    FVirtualTeams: array[TPlayerColor] of TTeam;
    FTeamCache: TTeamSettings;
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure Commit;

    procedure UpdateTeams(Sender: TObject; APlayer: TPlayerColor; ATeam: TTeam);
    procedure GetVirtualTeam(APlayer: TPlayerColor; var ATeam: TTeam);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Map: TVCMIMap read FMap write SetMap;
  end;

implementation

{$R *.lfm}

{ TPlayerOptionsForm }

procedure TPlayerOptionsForm.actSaveExecute(Sender: TObject);
begin
  Commit;
  ModalResult:=mrOK;
end;

procedure TPlayerOptionsForm.actDontSaveExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TPlayerOptionsForm.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
  if Assigned(FMap) then ReadData;
end;

procedure TPlayerOptionsForm.ReadData;
var
  Player: TPlayer;
  page: TTabSheet;
begin
  FTeamCache.Clear;
  FTeamCache.Assign(FMap.Teams);
  for Player in TPlayerColor do
  begin
    page := FEditors[Player].Parent as TTabSheet;
    page.Caption:=FMap.ListsManager.PlayerName[Player];

    FEditors[Player].Map := FMap;
    FEditors[Player].EditObject(FMap.Players.GetPlayerInfo(Integer(Player)));
  end;
end;

procedure TPlayerOptionsForm.Commit;
var
  Player: TPlayer;
begin
  for Player in TPlayerColor do
  begin
    FEditors[Player].Commit;
  end;
  FMap.Teams.Assign(FTeamCache);
end;

procedure TPlayerOptionsForm.UpdateTeams(Sender: TObject;
  APlayer: TPlayerColor; ATeam: TTeam);
var
  Player: TPlayer;
  team: TCollectionItem;
  new_team: TTeam;
begin
  if not Assigned(ATeam) then
  begin
    //team set to none
    for team in FTeamCache do
    begin
      TTeam(team).Exclude(APlayer);
    end;
  end
  else
  if not Assigned(ATeam.Collection) then
  begin
    //team set to virtual team
    new_team := FTeamCache.Add;
    new_team.Assign(ATeam);
    new_team.Include(APlayer);
  end
  else
  begin
    //existing team
    ATeam.Include(APlayer);
  end;

  for team in FTeamCache do
  begin
    if TTeam(team).IsEmpty then team.Free;
  end;

  for Player in TPlayerColor do
  begin
    FEditors[Player].FillTeams;
  end;
end;

procedure TPlayerOptionsForm.GetVirtualTeam(APlayer: TPlayerColor; var ATeam: TTeam);
begin
  ATeam := FVirtualTeams[APlayer];
end;

constructor TPlayerOptionsForm.Create(TheOwner: TComponent);
var
  Player: TPlayer;
  editor: TPlayerOptionsFrame;
  page: TTabSheet;
begin
  inherited Create(TheOwner);

  FTeamCache := TTeamSettings.Create;

  for Player in TPlayerColor do
  begin
    page := TTabSheet.Create(pcMain);
    page.PageControl := pcMain;
    page.TabVisible:=true;

    editor := TPlayerOptionsFrame.Create(page);
    editor.Visible:=true;
    editor.Parent:=page;
    editor.Align:=alClient;
    editor.OnGetVirtualTeam := @GetVirtualTeam;
    editor.OnAlliesChanged := @UpdateTeams;
    editor.TeamCache := FTeamCache;

    FEditors[Player] := editor;

    FVirtualTeams[Player] := TTeam.Create(nil);
    FVirtualTeams[Player].Include(Player);
  end;
end;

destructor TPlayerOptionsForm.Destroy;
var
  Player: TPlayer;
begin
  inherited Destroy;

  for Player in TPlayerColor do
  begin
    FVirtualTeams[Player].Free;
  end;

  FTeamCache.Free;
end;

end.

