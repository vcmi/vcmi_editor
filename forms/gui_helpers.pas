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

unit gui_helpers;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, CheckLst,

  base_info, logical_id_condition, editor_str_consts,
  editor_classes;

type

  TBaseInfoFilter = function (ATarget: TBaseInfo): boolean is nested;

  { TStringsHelper }

  TStringsHelper = class helper for TStrings
  public
    function GetBaseInfo(AIndex: Integer): TBaseInfo;

    function FillFrom(AFullList: THashedCollection; ASelected: AnsiString; AFilter: TBaseInfoFilter): integer;
    function FillFrom(AFullList: THashedCollection; ASelected: AnsiString): integer;
    procedure FillFrom(AFullList: THashedCollection);
  end;

  { TCheckListBoxHelper }

  TCheckListBoxHelper = class helper for TCustomCheckListBox
  public
    procedure FillItems(AFullList: THashedCollection);
    procedure LoadItems(ASrc: TStrings);

    procedure FillFrom(AFullList: THashedCollection; ASrc: TStrings);

    procedure SaveTo(ADest: TStrings);
    procedure SaveTo(ADest: TLogicalIDCondition; Permissive: Boolean);
    procedure FillFrom(AFullList: THashedCollection; ASrc: TLogicalIDCondition);
    procedure FillFrom(AFullList: THashedCollection; ASrc: TLogicalIDCondition; AFilter: TBaseInfoFilter);
  end;

  { TListBoxHelper }

  TListBoxHelper = class helper for TCustomListBox
  public
    procedure FillFromList(AFullList: THashedCollection; ASelected: AnsiString);
    procedure FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo);
    procedure FillFromList(AFullList: THashedCollection; ASelected: AnsiString; AFilter: TBaseInfoFilter);
    procedure FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo; AFilter: TBaseInfoFilter);
    function SelectedInfo: TBaseInfo;
    function SelectedIdentifier: AnsiString;

    procedure Select(AFullList: THashedCollection; AIdentifer: AnsiString);
  end;

  { TComboBoxHelper }

  TComboBoxHelper = class helper for TCustomComboBox
  private
    procedure AddEmptyOption(ASelected: AnsiString; AIndex: integer);
  public
    procedure FillFromList(AFullList: THashedCollection; ASelected: AnsiString);
    procedure FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo);

    procedure FillFromListWithEmptyOption(AFullList: THashedCollection; ASelected: AnsiString);
    procedure FillFromListWithEmptyOption(AFullList: TStrings; ASelected: AnsiString);

    //assumes items filed from AFullList
    procedure SetValueWithEmptyOption(AFullList: TStrings; ASelected: AnsiString);

    //assumes items filed from AFullList
    procedure SetValue(AFullList: THashedCollection; ASelected: AnsiString);
    procedure SetValue(AFullList: THashedCollection; ASelected: TBaseInfo);

    function SelectedInfo: TBaseInfo;
    function SelectedIdentifier: AnsiString;
  end;

  { TPageControlHelper }

  TPageControlHelper = class helper for TPageControl
  public
    procedure HideAllTabs;
  end;

implementation

function filter_stub(ATarget: TBaseInfo): Boolean;
begin
  Result := True;
end;


function FillItems(ATarget: TStrings; AFullList: TStrings; ASelected: AnsiString): integer;
var
  i: Integer;
  info: TBaseInfo;
begin
  Result := -1;

  ATarget.Clear;
  for i := 0 to AFullList.Count - 1 do
  begin
    info := AFullList.Objects[i] as TBaseInfo;
    if info.IsValid then
    begin
      ATarget.AddObject(info.Name+'('+info.Identifier+')',info);
      if(ASelected <>'') and (info.Identifier = ASelected) then
      begin
        Result := ATarget.Count - 1;
      end;
    end;
  end;
end;

{ TStringsHelper }

function TStringsHelper.GetBaseInfo(AIndex: Integer): TBaseInfo;
var
  tmp: TObject;
begin
  if AIndex < 0 then
  begin
    Exit(nil);
  end
  else
  begin
    tmp := Self.Objects[AIndex];
    if Assigned(tmp) then
    begin
      Exit(tmp as TBaseInfo);
    end
    else
    begin
      Exit(nil);
    end;
  end;
end;

function TStringsHelper.FillFrom(AFullList: THashedCollection; ASelected: AnsiString; AFilter: TBaseInfoFilter
  ): integer;
var
  i: Integer;
  info: TBaseInfo;
begin
  Result := -1;

  Self.Clear;
  for i := 0 to AFullList.Count - 1 do
  begin
    info := AFullList.Items[i] as TBaseInfo;
    if info.IsValid and AFilter(info) then
    begin
      Self.AddObject(info.Name+'('+info.Identifier+')',info);
      if(ASelected <> '') and (info.Identifier = ASelected) then
      begin
        Result := Self.Count - 1;
      end;
    end;
  end;
end;

function TStringsHelper.FillFrom(AFullList: THashedCollection; ASelected: AnsiString): integer;
begin
  Result := FillFrom(AFullList, ASelected, @filter_stub);
end;

procedure TStringsHelper.FillFrom(AFullList: THashedCollection);
begin
  FillFrom(AFullList, '', @filter_stub);
end;


{ TPageControlHelper }

procedure TPageControlHelper.HideAllTabs;
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    Self.Pages[i].TabVisible := False;
  end;
end;

{ TComboBoxHelper }

procedure TComboBoxHelper.AddEmptyOption(ASelected: AnsiString; AIndex: integer);
begin
  Items.Insert(0, rsEmpty);

  if ASelected = '' then
  begin
    ItemIndex:= 0;
  end
  else
  begin
    ItemIndex := AIndex+1;
  end;
end;

procedure TComboBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: AnsiString);
begin
  text := '';

  ItemIndex := Items.FillFrom(AFullList, ASelected);
end;

procedure TComboBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo);
var
  ID: AnsiString;
begin
  if Assigned(ASelected) then
    ID := ASelected.Identifier
  else
    ID := '';
  FillFromList(AFullList, ID)
end;

procedure TComboBoxHelper.FillFromListWithEmptyOption(AFullList: THashedCollection; ASelected: AnsiString);
var
  idx: Integer;
begin
  idx := Items.FillFrom(AFullList, ASelected);
  AddEmptyOption(ASelected, idx);
end;

procedure TComboBoxHelper.FillFromListWithEmptyOption(AFullList: TStrings; ASelected: AnsiString);
var
  idx: Integer;
begin
  idx := FillItems(Items, AFullList, ASelected);
  AddEmptyOption(ASelected, idx);
end;

procedure TComboBoxHelper.SetValueWithEmptyOption(AFullList: TStrings; ASelected: AnsiString);
var
  idx: Integer;
begin
  idx := AFullList.IndexOf(ASelected);

  itemindex := idx+1;
end;

procedure TComboBoxHelper.SetValue(AFullList: THashedCollection; ASelected: AnsiString);
var
  idx: Integer;
begin
  idx := AFullList.IndexOfName(ASelected);

  itemindex := idx;
end;

procedure TComboBoxHelper.SetValue(AFullList: THashedCollection;
  ASelected: TBaseInfo);
var
  ID: AnsiString;
begin
  if Assigned(ASelected) then
    ID := ASelected.Identifier
  else
    ID := '';
  SetValue(AFullList, ID);
end;

function TComboBoxHelper.SelectedInfo: TBaseInfo;
begin
  Result := Items.GetBaseInfo(ItemIndex);
end;

function TComboBoxHelper.SelectedIdentifier: AnsiString;
var
  info: TBaseInfo;
begin
  info := SelectedInfo();
  if Assigned(info) then
    Result := SelectedInfo.Identifier
  else
    Result := '';
end;

{ TCheckListBoxHelper }

procedure TCheckListBoxHelper.FillItems(AFullList: THashedCollection);
begin
  Items.FillFrom(AFullList);
end;

procedure TCheckListBoxHelper.LoadItems(ASrc: TStrings);
var
  i: Integer;
  info: TBaseInfo;
begin
  if Assigned(ASrc) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      info := Items.Objects[i] as TBaseInfo;
      Checked[i] := ASrc.IndexOf(info.Identifier)>=0;
    end;
  end
  else
  begin
    for i := 0 to Items.Count - 1 do
    begin
      Checked[i] := false;
    end;
  end;
end;

procedure TCheckListBoxHelper.FillFrom(AFullList: THashedCollection; ASrc: TStrings);
var
  i: Integer;
  info: TBaseInfo;
begin
  FillItems(AFullList);

  if Assigned(ASrc) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      info := Items.Objects[i] as TBaseInfo;
      Checked[i] := ASrc.IndexOf(info.Identifier)>=0;
    end;
  end
  else
  begin
    for i := 0 to Items.Count - 1 do
    begin
      Checked[i] := false;
    end;
  end;
end;

procedure TCheckListBoxHelper.SaveTo(ADest: TStrings);
var
  info: TBaseInfo;
  i: Integer;
begin
  ADest.Clear;
  for i := 0 to Items.Count - 1 do
  begin
    info := Items.Objects[i] as TBaseInfo;
    if Checked[i] then
    begin
      ADest.Add(info.Identifier);
    end;
  end;
end;

procedure TCheckListBoxHelper.SaveTo(ADest: TLogicalIDCondition; Permissive: Boolean);
var
  info: TBaseInfo;
  i: Integer;
begin
  ADest.Clear;

  for i := 0 to Items.Count - 1 do
  begin
    info := Items.Objects[i] as TBaseInfo;

    if Permissive then
    begin
     if not Checked[i] then
      begin
        ADest.NoneOf.Add(info.Identifier);
      end;
    end
    else
    begin
      if Checked[i] then
      begin
        ADest.AnyOf.Add(info.Identifier);
      end;
    end;
  end;
end;

procedure TCheckListBoxHelper.FillFrom(AFullList: THashedCollection; ASrc: TLogicalIDCondition);
begin
  FillFrom(AFullList, ASrc, @filter_stub);
end;

procedure TCheckListBoxHelper.FillFrom(AFullList: THashedCollection; ASrc: TLogicalIDCondition; AFilter: TBaseInfoFilter);
var
  i: Integer;
  info: TBaseInfo;
begin
  Items.FillFrom(AFullList, '', AFilter);

  for i := 0 to Items.Count - 1 do
  begin
    info := Items.Objects[i] as TBaseInfo;
    Checked[i] := ASrc.IsAllowed(info.Identifier);
  end;
end;

{ TListBoxHelper }

procedure TListBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: AnsiString);
begin
  ItemIndex := Self.Items.FillFrom(AFullList,ASelected);
end;

procedure TListBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo);
var
  ident: AnsiString;
begin
  if Assigned(ASelected) then
    ident:=ASelected.Identifier
  else
    ident:='';
  FillFromList(AFullList, ident);
end;

procedure TListBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: TBaseInfo; AFilter: TBaseInfoFilter);
begin
  if Assigned(ASelected) then
    FillFromList(AFullList, ASelected.Identifier, AFilter)
  else
    FillFromList(AFullList, '', AFilter)
end;

procedure TListBoxHelper.FillFromList(AFullList: THashedCollection; ASelected: AnsiString; AFilter: TBaseInfoFilter);
begin
  ItemIndex := Self.Items.FillFrom(AFullList, ASelected, AFilter);
end;

function TListBoxHelper.SelectedInfo: TBaseInfo;
begin
  Result := Items.GetBaseInfo(ItemIndex);
end;

function TListBoxHelper.SelectedIdentifier: AnsiString;
begin
  if Assigned(SelectedInfo) then
    Result := SelectedInfo.Identifier
  else
    Result := '';
end;

procedure TListBoxHelper.Select(AFullList: THashedCollection; AIdentifer: AnsiString);
begin
  ItemIndex := AFullList.IndexOfName(AIdentifer);
end;

end.

