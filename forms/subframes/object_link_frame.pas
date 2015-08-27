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
unit object_link_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, map,
  object_options;

type

  { TObjectLinkFrame }

  TObjectLinkFrame = class(TFrame)
    ObjectList: TListBox;
  private
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
  public
    procedure Commit;
    procedure Load(AIdentifier: String);

    property Map: TVCMIMap read FMap write SetMap;
  end;

implementation

uses editor_consts;

{$R *.lfm}

{ TObjectLinkFrame }

procedure TObjectLinkFrame.SetMap(AValue: TVCMIMap);
begin
  if FMap=AValue then Exit;
  FMap:=AValue;
end;

procedure TObjectLinkFrame.Commit;
var
  map_object: TMapObject;
begin
  map_object := (ObjectList.Items.Objects[ObjectList.ItemIndex] as TMapObject);

  //todo: TObjectLinkFrame.Commit
end;

procedure TObjectLinkFrame.Load(AIdentifier: String);
var
  map_object: TMapObject;
  item: TCollectionItem;

  function GetTownName(): AnsiString;
  begin
    if map_object.GetID = 'randomTown' then
    begin
      Result := 'Random town';
    end
    else
    begin
      Result := Map.ListsManager.GetFaction(map_object.GetSubId).Name;
    end;
  end;
begin
  //todo: TObjectLinkFrame.Load

  //FLink := Alink;
  //
  //for item in FMap.Objects do
  //begin
  //  map_object := item as TMapObject;
  //
  //  if (map_object.Options is TMonsterOptions) and (FLink.&type = TYPE_MONSTER) then
  //  begin
  //
  //  end
  //  else if (map_object.Options is THeroOptions) and (FLink.&type = TYPE_HERO) then
  //  begin
  //
  //  end
  //  else if (map_object.Options is TTownOptions) and ((FLink.&type = TYPE_TOWN) or (FLink.&type = TYPE_RANDOMTOWN)) then
  //  begin
  //    ObjectList.AddItem(Format('%s at %d %d %d',[GetTownName, map_object.L, map_object.X, map_object.Y]),map_object);
  //  end
  //  else
  //    Continue;
  //
  //  if (map_object.L = FLink.L) and (map_object.X = FLink.x) and (map_object.Y = FLink.Y) then
  //  begin
  //    ObjectList.ItemIndex := ObjectList.Count-1; //select recently added object
  //  end;
  //
  //end;

  ObjectList.Invalidate;
end;

end.

