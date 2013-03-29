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
  Classes, SysUtils, {Controls, StdCtrls,} CheckLst,

  editor_types, lists_manager;

type

  { TCheckListBoxHelper }

  TCheckListBoxHelper = class helper for TCustomCheckListBox
  public
    procedure FillFromList(AFullList: TStrings; ASrc: TStrings);
    procedure SaveToList(ADest: TStrings);
  end;


implementation

procedure FillCheckListBox(ATarget: TCustomCheckListBox; AFullList: TStrings; ASrc: TStrings);
var
  i: Integer;
  info: TBaseInfo;
begin
  ATarget.Items.Clear;
  for i := 0 to AFullList.Count - 1 do
  begin
    info := AFullList.Objects[i] as TBaseInfo;
    ATarget.Items.AddObject(info.Name,info);
  end;

  for i := 0 to ATarget.Items.Count - 1 do
  begin
    info := ATarget.Items.Objects[i] as TBaseInfo;
    ATarget.Checked[i] := ASrc.IndexOf(info.ID)>=0;
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

{ TCheckListBoxHelper }

procedure TCheckListBoxHelper.FillFromList(AFullList: TStrings; ASrc: TStrings);
begin
  FillCheckListBox(Self,AFullList,ASrc)
end;

procedure TCheckListBoxHelper.SaveToList(ADest: TStrings);
begin
  SaveCheckListBox(Self,ADest);
end;

end.

