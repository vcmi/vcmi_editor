{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit edit_hero_options;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, map, lists_manager, editor_str_consts, base_options_frame,
  hero_frame, hero_spells_frame, hero_artifacts_frame,
  hero_skills_frame;

type

  { TEditHeroOptions }

  TEditHeroOptions = class(TForm)
    act: TActionList;
    actDontSave: TAction;
    actSave: TAction;
    btCancel: TButton;
    btOk: TButton;
    pcMain: TPageControl;
    procedure actDontSaveExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
    FEditors: TBaseOptionsFrameList;
    function GetListsManager: TListsManager;
    function GetMap: TVCMIMap;
    procedure SetListsManager(AValue: TListsManager);
    procedure SetMap(AValue: TVCMIMap);
  public
    constructor Create(TheOwner: TComponent); override;
    property Map: TVCMIMap read GetMap write SetMap;
    property ListsManager: TListsManager read GetListsManager write SetListsManager;

    procedure EditObject(AObject: THeroDefinition);
  end;


implementation

{$R *.lfm}

{ TEditHeroOptions }

procedure TEditHeroOptions.actSaveExecute(Sender: TObject);
begin
  FEditors.Commit;
  ModalResult:=mrOK;
end;

procedure TEditHeroOptions.actDontSaveExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TEditHeroOptions.SetMap(AValue: TVCMIMap);
begin
  FEditors.Map := AValue;
end;

procedure TEditHeroOptions.SetListsManager(AValue: TListsManager);
begin
  FEditors.ListsManager := AValue;
end;

function TEditHeroOptions.GetListsManager: TListsManager;
begin
  Result := FEditors.ListsManager;
end;

function TEditHeroOptions.GetMap: TVCMIMap;
begin
  Result := FEditors.Map;
end;

constructor TEditHeroOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditors := TBaseOptionsFrameList.Create(Self);

end;

procedure TEditHeroOptions.EditObject(AObject: THeroDefinition);
begin
  FEditors.AddFrame(THeroFrame, AObject, rsTabMainOptions, pcMain);
  FEditors.AddFrame(THeroSkillsFrame, AObject, rsTabSecondarySkills, pcMain);
  FEditors.AddFrame(THeroArtifactsFrame, AObject, rsTabArtifacts, pcMain);
  FEditors.AddFrame(THeroSpellsFrame, AObject, rsTabSpells, pcMain);

  pcMain.ActivePage := pcMain.Pages[0];

  ShowModal;
end;

end.

