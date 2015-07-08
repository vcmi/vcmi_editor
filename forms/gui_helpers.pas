{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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

unit gui_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, CheckLst,

  editor_types, base_info, logical_id_condition, editor_str_consts;

type

  { TCheckListBoxHelper }

  TCheckListBoxHelper = class helper for TCustomCheckListBox
  public
    procedure FillFromList(AFullList: TStrings; ASrc: TStrings);
    procedure SaveToList(ADest: TStrings);

    procedure SaveToCondition(AFullList: TStrings; ADest: TLogicalIDCondition; Permissive: Boolean);

    procedure FillFromCondition(AFullList: TStrings; ASrc: TLogicalIDCondition);
  end;

  { TListBoxHelper }

  TListBoxHelper = class helper for TCustomListBox
  public
    procedure FillFromList(AFullList: TStrings; ASelected: TBaseInfo);
    function SelectedInfo: TBaseInfo;
  end;

  { TComboBoxHelper }

  TComboBoxHelper = class helper for TCustomComboBox
  public
    procedure FillFromList(AFullList: TStrings; ASelected: AnsiString);
    procedure FillFromList(AFullList: TStrings; ASelected: TBaseInfo);

    procedure FillFromListWithEmptyOption(AFullList: TStrings; ASelected: AnsiString);
    function GetValueWithEmptyOption(AFullList: TStrings): AnsiString;
  end;

implementation

procedure FillItems(ATarget: TStrings; AFullList: TStrings);
var
  i: Integer;
  info: TBaseInfo;
begin
  ATarget.Clear;
  for i := 0 to AFullList.Count - 1 do
  begin
    info := AFullList.Objects[i] as TBaseInfo;
    ATarget.AddObject(info.Name+'('+info.ID+')',info);
  end;
end;

procedure FillCheckListBox(ATarget: TCustomCheckListBox; AFullList: TStrings; ASrc: TStrings);
var
  i: Integer;
  info: TBaseInfo;
begin
  FillItems(ATarget.Items, AFullList);

  for i := 0 to ATarget.Items.Count - 1 do
  begin
    info := ATarget.Items.Objects[i] as TBaseInfo;
    ATarget.Checked[i] := ASrc.IndexOf(info.ID)>=0;
  end;
end;

procedure FillCheckListBox(ATarget: TCustomCheckListBox; AFullList: TStrings; ASrc: TLogicalIDCondition);
var
  i: Integer;
  info: TBaseInfo;
begin
  FillItems(ATarget.Items, AFullList);

  for i := 0 to ATarget.Items.Count - 1 do
  begin
    info := ATarget.Items.Objects[i] as TBaseInfo;
    ATarget.Checked[i] := ASrc.IsAllowed(info.ID);
  end;
end;

procedure SaveCheckListBox(ATarget: TCustomCheckListBox; ADest: TStrings);
var
  info: TBaseInfo;
  i: Integer;
begin
  ADest.Clear;
  for i := 0 to ATarget.Items.Count - 1 do
  begin
    info := ATarget.Items.Objects[i] as TBaseInfo;
    if ATarget.Checked[i] then
    begin
      ADest.Add(info.ID);
    end;
  end;
end;

procedure SaveCheckListBox(ATarget: TCustomCheckListBox; AFullList: TStrings; ADest: TLogicalIDCondition; Permissive: Boolean);
var
  info: TBaseInfo;
  i: Integer;

  ban_list: TStringList;
  Aid: String;
begin
  ADest.Clear;

  ban_list := TStringList.Create;

  try
    for i := 0 to ATarget.Items.Count - 1 do
    begin
      info := ATarget.Items.Objects[i] as TBaseInfo;
      if not ATarget.Checked[i] then
      begin
        ban_list.Add(info.ID);
      end;
    end;

    if Permissive then
    begin
      ADest.NoneOf.Assign(ban_list);
    end
    else begin
      for i := 0 to AFullList.Count - 1 do
      begin
        info := AFullList.Objects[i] as TBaseInfo;

        if ban_list.IndexOf(info.ID) < 0 then
        begin
          ADest.AnyOf.Add(info.ID);
        end;
      end;
    end;

  finally
    ban_list.Free;
  end;
end;

{ TComboBoxHelper }

procedure TComboBoxHelper.FillFromList(AFullList: TStrings;
  ASelected: AnsiString);
var
  idx: Integer;
begin
  FillItems(Items, AFullList);

  if ASelected = '' then
  begin
    ItemIndex:= -1;
    text := '';
  end
  else
  begin
    idx := AFullList.IndexOf(ASelected);

    if idx <> -1 then
    begin
      text := (AFullList.Objects[idx] as TBaseInfo).Name;
    end
      else Text := '';

    ItemIndex := idx;
  end;
end;

procedure TComboBoxHelper.FillFromList(AFullList: TStrings; ASelected: TBaseInfo
  );
var
  ID: AnsiString;
begin
  if Assigned(ASelected) then
    ID := ASelected.ID
  else
    ID := '';
  FillFromList(AFullList, ID)
end;

procedure TComboBoxHelper.FillFromListWithEmptyOption(AFullList: TStrings;
  ASelected: AnsiString);
var
  idx: Integer;
begin
  FillItems(Items, AFullList);

  Items.Insert(0, rsEmpty);

  if ASelected = '' then
  begin
    ItemIndex:= 0;
  end
  else
  begin
    idx := AFullList.IndexOf(ASelected);

    ItemIndex := idx+1;
  end;

end;

function TComboBoxHelper.GetValueWithEmptyOption(AFullList: TStrings
  ): AnsiString;
begin
  if ItemIndex <= 0 then
  begin
    Exit('');
  end
  else begin
    exit((Items.Objects[ItemIndex] as TBaseInfo).ID)
  end;
end;

{ TCheckListBoxHelper }

procedure TCheckListBoxHelper.FillFromList(AFullList: TStrings; ASrc: TStrings);
begin
  FillCheckListBox(Self,AFullList,ASrc)
end;

procedure TCheckListBoxHelper.SaveToList(ADest: TStrings);
begin
  SaveCheckListBox(Self,ADest);
end;

procedure TCheckListBoxHelper.SaveToCondition(AFullList: TStrings;
  ADest: TLogicalIDCondition; Permissive: Boolean);
begin
  SaveCheckListBox(Self,AFullList, ADest, Permissive);
end;

procedure TCheckListBoxHelper.FillFromCondition(AFullList: TStrings;
  ASrc: TLogicalIDCondition);
begin
  FillCheckListBox(Self,AFullList,ASrc);
end;


{ TListBoxHelper }

procedure TListBoxHelper.FillFromList(AFullList: TStrings; ASelected: TBaseInfo
  );
begin
  FillItems(Self.Items, AFullList);
  if Assigned(ASelected) then
  begin
    ItemIndex := Items.IndexOfObject(ASelected);
  end;

end;

function TListBoxHelper.SelectedInfo: TBaseInfo;
begin
  Result := Items.Objects[ItemIndex] as TBaseInfo;
end;

end.

