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
unit hero_options_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComboEx, EditBtn,
  ComCtrls, Graphics, Dialogs, hero_frame, gui_helpers, object_options,
  editor_types, base_info, editor_consts;

type

  { THeroOptionsFrame }

  THeroOptionsFrame = class(THeroFrame)
    procedure edHeroClassChange(Sender: TObject);
  private
    FOptions: THeroOptions;

    procedure Load();

    function GetHeroClass():AnsiString;
    function GetHeroClassName():TLocalizedString;

  protected
    procedure VisitNormalHero(AOptions: THeroOptions);override;
    procedure VisitRandomHero(AOptions: THeroOptions);override;
    procedure VisitPrison(AOptions: THeroOptions);override;

    procedure UpdateControls(); override;
  public
    procedure VisitHero(AOptions: THeroOptions); override;

    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ THeroOptionsFrame }

procedure THeroOptionsFrame.edHeroClassChange(Sender: TObject);
var
  base_class_info: TBaseInfo;

  editor: TCustomComboBox;
  idx: Integer;
  hero_type: String;
begin

  if not edType.Visible then
    Exit;

  //fill hero types
  editor := Sender as TCustomComboBox;

  if editor.ItemIndex < 0 then
  begin
    edType.ItemIndex := -1;
    edType.Items.Clear;
    edType.Enabled:=false;
  end
  else begin
    edType.Enabled:=true;
    edType.ItemIndex := -1;

    base_class_info := editor.Items.Objects[editor.ItemIndex] as TBaseInfo;

    ListsManager.FillWithHeroesOfClass(edType.Items, base_class_info.ID);

    hero_type :=  FOptions.&type;

    for idx := 0 to edType.Items.Count - 1 do
    begin
      if (edType.Items.Objects[idx] as TBaseInfo).ID = hero_type then
      begin
        edType.ItemIndex := idx;
        break;
      end;
    end;

    if (edType.ItemIndex = -1) and (edType.Items.Count >0) then
      edType.ItemIndex := 0;
  end;

  edTypeChange(edType);
end;

procedure THeroOptionsFrame.Load;
begin
    cbPortrait.Checked:=FOptions.Portrait <> '';
    cbExperience.Checked := FOptions.Experience <> 0;

    cbName.Checked:=FOptions.Name <> '';
    cbSex.Checked:=FOptions.Sex <> THeroSex.default;
    cbBiography.Checked:=FOptions.Biography <> '';


    edHeroClass.FillFromList(ListsManager.HeroClassMap, GetHeroClass);

    edHeroClassChange(edHeroClass); //also loads type


    if edOwner.Visible then
    begin
      edOwner.ItemIndex := Integer(FOptions.Owner);
    end;

    //TODO: load portrait

    if cbExperience.Checked then
    begin
      edExperience.Text := IntToStr(FOptions.Experience);
    end
    else
    begin
      edExperience.Text := '0';
    end;

    cbName.Checked:= FOptions.Name <> '';
    cbNameChange(cbName);

    case FOptions.PatrolRadius of
      -1: edPatrol.ItemIndex := 0 ;
      0: edPatrol.ItemIndex := 1;
    else
      begin
        edPatrol.Text:=IntToStr(FOptions.PatrolRadius);
      end;
    end;

    UpdateControls();
end;

function THeroOptionsFrame.GetHeroClass: AnsiString;
begin
  if not Assigned(FOptions) then
  begin
    exit('');//just to safe
  end;

  if FOptions.&type = '' then
  begin

    if FOptions.MapObject.GetID = TYPE_HERO then
    begin
      Exit(FOptions.MapObject.GetSubId());
    end;
    exit('');
  end;

  Result := ListsManager.Heroes[FOptions.&type].&Class;
end;

function THeroOptionsFrame.GetHeroClassName: TLocalizedString;
var
  hero_class: String;
begin
  if not Assigned(FOptions) then
  begin
    exit('');
  end;

  if FOptions.&type = '' then
  begin
    exit('');
  end;

  hero_class := ListsManager.Heroes[FOptions.&type].&Class;

  Result := ListsManager.HeroClasses[hero_class].Name;
end;

procedure THeroOptionsFrame.UpdateControls;
begin
  inherited;
end;

procedure THeroOptionsFrame.VisitNormalHero(AOptions: THeroOptions);
begin
  edHeroClass.Enabled:=False;
  Load();
end;

procedure THeroOptionsFrame.VisitRandomHero(AOptions: THeroOptions);
begin
  lbHeroClass.Visible:=False;
  edHeroClass.Visible:=False;
  Placeholder1.Visible:=False;

  lbType.Visible:=False;
  edType.Visible:=False;
  Placeholder2.Visible:=False;

  Load();
end;

procedure THeroOptionsFrame.VisitPrison(AOptions: THeroOptions);
begin
  lbOwner.Visible:=False;
  edOwner.Visible := False;
  Placeholder3.Visible:=False;

  Load();
end;

procedure THeroOptionsFrame.VisitHero(AOptions: THeroOptions);
begin
  FOptions := AOptions;
  inherited VisitHero(AOptions); //continue dispatch
end;

procedure THeroOptionsFrame.Commit;
begin
  inherited Commit;

  if edType.Visible then
  begin
    FOptions.&type := edType.GetValueWithEmptyOption();
  end;

  if edOwner.Visible then
  begin
    FOptions.Owner := TPlayer(edOwner.ItemIndex);
  end;

  if cbExperience.Checked then
  begin
    FOptions.Experience := StrToQWordDef(edExperience.Text, 0);
  end
  else begin
    FOptions.Experience := 0;
  end;
end;

end.

