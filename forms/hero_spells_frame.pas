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
unit hero_spells_frame;

{$I compilersetup.inc}

{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, Graphics, Dialogs,
  CheckLst, ExtCtrls, StdCtrls, base_options_frame, gui_helpers, object_options,
  Map, editor_classes, lists_manager, base_info;

type
  //todo: use hero definition

  { THeroSpellsFrame }

  THeroSpellsFrame = class(TBaseOptionsFrame)
    cbCustomise: TCheckBox;
    Spellbook: TCheckListBox;
    procedure FrameResize(Sender: TObject);
  private
    FDefaults: TStrings;
    FCache: TStrings;
    FTarget:TStrings;

    procedure Clear;

    procedure ReadData;

    procedure cbCustomiseChange(Sender: TObject);

    procedure LoadDefaultSpells(AHeroId: AnsiString);

    procedure FillSpellbook(ASrc: TStrings);

  protected
    procedure Load; override;
    procedure ApplyDefaults; override;
    procedure ReloadDefaults; override;
    procedure UpdateControls; override;
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

procedure THeroSpellsFrame.LoadDefaultSpells(AHeroId: AnsiString);
var
  hero_info: THeroInfo;
  definition: THeroDefinition;
begin
  FDefaults.Clear;

  if AHeroId = '' then
    Exit;

  hero_info := ListsManager.Heroes[AHeroId];
  definition := nil;

  if FUseMapDefaults then
  begin
    definition := Map.PredefinedHeroes.FindItem(AHeroId);
  end;

  if Assigned(definition) and (definition.Spellbook.Count <> 0) then
    FDefaults.Assign(definition.Spellbook)
  else if Assigned(hero_info) then
    FDefaults.Assign(hero_info.SpellBook);
end;

procedure THeroSpellsFrame.FillSpellbook(ASrc: TStrings);
begin
  Spellbook.FillFrom(ListsManager.SpellInfos, ASrc);
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

procedure THeroSpellsFrame.FrameResize(Sender: TObject);
begin
  Spellbook.Columns := Max(1, (Width div 300) +1);
end;

procedure THeroSpellsFrame.Load;
begin
  FillSpellbook(FCache);
end;

procedure THeroSpellsFrame.ApplyDefaults;
begin
  inherited ApplyDefaults;
  FillSpellbook(FCache);
end;

procedure THeroSpellsFrame.ReloadDefaults;
begin
  inherited ReloadDefaults;
  LoadDefaultSpells(InstanceType);
end;

procedure THeroSpellsFrame.Clear;
begin
  Spellbook.SaveTo(FCache);
  FillSpellbook(FDefaults);
end;

procedure THeroSpellsFrame.UpdateControls;
begin
  inherited UpdateControls;
  Spellbook.Enabled := cbCustomise.Checked;

  if cbCustomise.Checked then
    Load()
  else
    Clear();
end;

procedure THeroSpellsFrame.Commit;
begin
  inherited Commit;
  Spellbook.SaveTo(FCache);
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
begin
  FUseMapDefaults:=True;
  inherited VisitHero(AOptions);//continue dispatch

  FTarget := AOptions.SpellBook;
  FCache.Assign(FTarget);

  LoadDefaultSpells(AOptions.&type);

  ReadData;
end;

procedure THeroSpellsFrame.VisitHeroDefinition(AOptions: THeroDefinition);
begin
  FUseMapDefaults:=False;
  inherited VisitHeroDefinition(AOptions);//continue dispatch

  LoadDefaultSpells(AOptions.Identifier);

  FTarget := AOptions.SpellBook;
  FCache.Assign(FTarget);

  LoadDefaultSpells(AOptions.Identifier);

  ReadData;
end;

end.

