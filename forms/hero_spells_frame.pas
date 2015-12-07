{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge.net

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
unit hero_spells_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ExtCtrls, StdCtrls, base_options_frame, gui_helpers, object_options, Map,
  editor_classes;

type
  //todo: use hero definition

  { THeroSpellsFrame }

  THeroSpellsFrame = class(TBaseOptionsFrame)
    cbCustomise: TCheckBox;
    Panel1: TPanel;
    Spellbook: TCheckListBox;
  private
    FDefaults: TStrings;
    FCache: TStrings;
    FTarget:TStrings;

    procedure Load;
    procedure Clear;

    procedure ReadData;
    procedure UpdateControls;
    procedure cbCustomiseChange(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
    procedure VisitHero(AOptions: THeroOptions); override;
    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
  end;

implementation

{$R *.lfm}

{ THeroSpellsFrame }

procedure THeroSpellsFrame.ReadData;
begin
  cbCustomise.OnChange:=nil;
  cbCustomise.Checked:=not (FCache.Count = 0);
  cbCustomise.OnChange:=@cbCustomiseChange;
  UpdateControls;
end;

procedure THeroSpellsFrame.cbCustomiseChange(Sender: TObject);
begin
  UpdateControls;
end;

constructor THeroSpellsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCache := TStringList.Create;
  FDefaults := TStringList.Create;
end;

destructor THeroSpellsFrame.Destroy;
begin
  FDefaults.Free;
  FCache.Free;
  inherited Destroy;
end;

procedure THeroSpellsFrame.Load;
begin
  Spellbook.FillFromList(Map.ListsManager.SpellMap, FCache);
end;

procedure THeroSpellsFrame.Clear;
begin
  Spellbook.SaveToList(FCache);
  Spellbook.FillFromList(Map.ListsManager.SpellMap, FDefaults);
end;

procedure THeroSpellsFrame.UpdateControls;
begin
  Spellbook.Enabled := cbCustomise.Checked;

  if cbCustomise.Checked then
    Load()
  else
    Clear();
end;

procedure THeroSpellsFrame.Commit;
begin
  inherited Commit;
  Spellbook.SaveToList(FCache);
  if not cbCustomise.Checked then
  begin
    FTarget.Clear;
  end
  else if FCache.Count = 0 then
  begin
    FTarget.Clear;
    FTarget.Add('');
  end
  else
  begin
    FTarget.Assign(FCache);
  end;

end;

procedure THeroSpellsFrame.VisitHero(AOptions: THeroOptions);
var
  definition: THeroDefinition;
begin
  inherited VisitHero(AOptions);//continue dispatch
  FTarget := AOptions.SpellBook;
  FCache.Assign(FTarget);

  definition := Map.PredefinedHeroes.FindItem(AOptions.&type);

  if Assigned(definition) then
    FDefaults.Assign(definition.SpellBook)
  else
    FDefaults.Clear;  //todo: use type defaults
  ReadData;
end;

procedure THeroSpellsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  inherited VisitHeroDefinition(AOptions);//continue dispatch
  FTarget := AOptions.SpellBook;
  FCache.Assign(FTarget);
  FDefaults.Clear;//todo: use type defaults
  ReadData;
end;

end.

