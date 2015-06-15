unit object_link_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, object_link, map,
  object_options;

type

  { TObjectLinkFrame }

  TObjectLinkFrame = class(TFrame)
    ObjectList: TListBox;
  private
    FLink: TObjectLink;
    FMap: TVCMIMap;
    procedure SetMap(AValue: TVCMIMap);
  public
    procedure Commit;
    procedure Load(ALink: TObjectLink);

    property Map: TVCMIMap read FMap write SetMap;
  end;

implementation

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

  FLink.L:=map_object.L;
  FLink.X:=map_object.x;
  FLink.y:=map_object.Y;
end;

procedure TObjectLinkFrame.Load(ALink: TObjectLink);
var
  map_object: TMapObject;
  item: TCollectionItem;

  town: TTownOptions;

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
  FLink := Alink;

  for item in FMap.Objects do
  begin
    map_object := item as TMapObject;

    if (map_object.Options is TMonsterOptions) and (FLink.Metaclass = TObjectLinkMetclass.creature) then
    begin

    end
    else if (map_object.Options is THeroOptions) and (FLink.Metaclass = TObjectLinkMetclass.hero) then
    begin

    end
    else if (map_object.Options is TTownOptions) and (FLink.Metaclass = TObjectLinkMetclass.town) then
    begin
      town := TTownOptions(map_object.Options);
      ObjectList.AddItem(Format('%s at %d %d %d',[GetTownName, map_object.L, map_object.X, map_object.Y]),map_object);
    end
    else
      Continue;

    if (map_object.L = FLink.L) and (map_object.X = FLink.x) and (map_object.Y = FLink.Y) then
    begin
      ObjectList.ItemIndex := ObjectList.Count-1; //select recently added object
    end;

  end;

  ObjectList.Invalidate;
end;

end.

