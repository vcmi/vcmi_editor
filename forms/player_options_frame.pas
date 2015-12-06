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
  CheckLst, map, lists_manager, editor_types, gui_helpers;

type

  { TPlayerOptionsFrame }

  TPlayerOptionsFrame = class(TFrame)
    cbAllowedFactions: TCheckBox;
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
  private
    FMap: TVCMIMap;
    FObject: TPlayerInfo;
    procedure SetMap(AValue: TVCMIMap);
    procedure ReadData;
    procedure FillTeams;
  public
    property Map: TVCMIMap read FMap write SetMap;
    procedure EditObject(AObject: TPlayerInfo);
    procedure Commit;
  end;

implementation

{$R *.lfm}

{ TPlayerOptionsFrame }

procedure TPlayerOptionsFrame.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TPlayerOptionsFrame.ReadData;
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

  edAllowedFactions.FillFromCondition(FMap.ListsManager.FactionMap, FObject.AllowedFactions);



  edGenerateHero.Checked := FObject.GenerateHeroAtMainTown;
end;

procedure TPlayerOptionsFrame.FillTeams;
begin

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

  edAllowedFactions.SaveToCondition(FMap.ListsManager.FactionMap, FObject.AllowedFactions, true);
end;

end.

