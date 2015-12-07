{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

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

unit editor_classes;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, editor_types, fpjson, contnrs;

type

  { IProgressCallback }

  IProgressCallback = interface
    function GetMax: Integer;
    procedure SetMax(AValue: Integer);

    property Max: Integer read GetMax write SetMax;

    procedure Advance(ADelta: integer);
  end;


  {
    May contain only one published property serialized directly (w/o object node)
  }

  IEmbeddedValue = interface ['IEmbbeddedValue']

  end;

  { INamedCollection
    Stored as object in JSON
    uses TNamedCollectionItem.Identifier as a name of field }

  INamedCollection = interface ['INamedCollection']

  end;

  { IArrayCollection
    Stored as array in JSON  }

  IArrayCollection = interface ['IArrayCollection']

  end;

  { IEmbeddedCollection }

  IEmbeddedCollection = interface ['{F3D6E58A-CA30-4030-B98B-5E75A5AB796A}']
    function GetCollection: TCollection;
  end;

  { ISerializeNotify }

  ISerializeNotify = interface ['ISerializeNotify']
     procedure BeforeSerialize(Sender:TObject);
     procedure AfterSerialize(Sender:TObject; AData: TJSONData);

     procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
     procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
  end;

  IReferenceNotify = interface
     procedure NotifyReferenced(AOldIdentifier, ANewIdentifier: AnsiString);
  end;

  { TNamedCollectionItem }

  TNamedCollectionItem = class(TCollectionItem)
  private
    FIdentifier: AnsiString;
    procedure SetIdentifier(AValue: AnsiString);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    property Identifier: AnsiString read FIdentifier write SetIdentifier;
  end;

  TNamedCollectionItemClass = class of TNamedCollectionItem;

  { THashedCollection }

  THashedCollection = class(TCollection)
  private
    FHash: TFPHashObjectList;

  protected
    procedure ItemIdentifierChanged(Item: TCollectionItem; AOldName: String; ANewName: String); virtual;
    procedure ItemAdded(Item: TCollectionItem); virtual;
    procedure ItemRemoved(Item: TCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;
  public
    constructor Create(AItemClass: TNamedCollectionItemClass);
    destructor Destroy; override;

    function IndexOfName(const AName: String): Integer;

    function FindItem(const AName: String): TNamedCollectionItem;
  end;


  { TGArrayCollection }

  generic TGArrayCollection <TItem> = class (TCollection, IArrayCollection)
  private
    function GetItems(const Idx: Integer): TItem;
    procedure SetItems(const Idx: Integer; AValue: TItem);
  public
    type
      TItemType = Titem;
    constructor Create;

    function Add: TItem;

    property Items[const Idx: Integer]: TItem read GetItems write SetItems; default;
  end;

  { TGNamedCollection }

  generic TGNamedCollection <TItem> = class (THashedCollection, INamedCollection)
  private
    function GetItems(const Idx: Integer): TItem;
    procedure SetItems(const Idx: Integer; AValue: TItem);
  public
    type
      TItemType = Titem;
    constructor Create;

    function Add: TItem;

    property Items[const Idx: Integer]: TItem read GetItems write SetItems; default;

    function FindItem(const AName: String): TItemType;
  end;

  { TObjectMap }

  generic TObjectMap <TKey, TValue> = class (specialize TFPGMap<TKey, TValue>)
  protected
    procedure Deref(Item: Pointer); override;
  end;

  { THeroPrimarySkills }

  THeroPrimarySkills = class(TPersistent)
  private
    FAttack: Integer;
    FDefence: Integer;
    FKnowledge: Integer;
    FSpellpower: Integer;
  public
    constructor Create;
    function IsDefault: Boolean;
  published
    property Attack: Integer read FAttack write FAttack default -1;
    property Defence: Integer read FDefence write FDefence default -1;
    property Spellpower: Integer read FSpellpower write FSpellpower default -1;
    property Knowledge: Integer read FKnowledge write FKnowledge default -1;
  end;

  { THeroSecondarySkill }

  THeroSecondarySkill = class(TNamedCollectionItem, IEmbeddedValue)
  private
    FLevel: Integer;
    procedure SetLevel(AValue: Integer);
  published
    property Level: Integer read FLevel write SetLevel nodefault;
  end;

  { THeroSecondarySkills }

  THeroSecondarySkills = class(specialize TGNamedCollection<THeroSecondarySkill>)
  end;

  IHeroInfo = interface
    function GetBiography: TLocalizedString;
    function GetSex: THeroSex;
    function GetName: TLocalizedString;
  end;

  { TBaseIdentifierList }

  TBaseIdentifierList = class abstract (TStringList)
  private
    FOwner: IReferenceNotify;
  protected
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create(AOwner: IReferenceNotify); virtual;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
  end;

  { TIdentifierList }

  TIdentifierList = class(TBaseIdentifierList)
  public
    constructor Create(AOwner: IReferenceNotify); override;
  end;

  { TIdentifierSet }

  TIdentifierSet = class(TBaseIdentifierList)
  public
    constructor Create(AOwner: IReferenceNotify); override;
  end;

implementation

{ TIdentifierSet }

constructor TIdentifierSet.Create(AOwner: IReferenceNotify);
begin
  inherited Create(AOwner);
  Sorted:=true;
  Duplicates:=dupIgnore;
end;

{ TIdentifierList }

constructor TIdentifierList.Create(AOwner: IReferenceNotify);
begin
  inherited Create(AOwner);
  Sorted:=false;
  Duplicates:=dupAccept;
end;

{ TBaseIdentifierList }

procedure TBaseIdentifierList.InsertItem(Index: Integer; const S: string; O: TObject
  );
begin
  FOwner.NotifyReferenced('', s);
  inherited InsertItem(Index, S, O);
end;

procedure TBaseIdentifierList.Put(Index: Integer; const S: string);
begin
  FOwner.NotifyReferenced(Strings[Index], s);
  inherited Put(Index, S);
end;

constructor TBaseIdentifierList.Create(AOwner: IReferenceNotify);
begin
  inherited Create;
  FOwner :=  AOwner;
end;

procedure TBaseIdentifierList.Delete(Index: Integer);
begin
  FOwner.NotifyReferenced(Strings[Index], '');
  inherited Delete(Index);
end;

procedure TBaseIdentifierList.Clear;
var
  s: String;
begin
  for s in self do
    FOwner.NotifyReferenced(s, '');

  inherited Clear;
end;

{ TGNamedCollection }

function TGNamedCollection.GetItems(const Idx: Integer): TItem;
begin
  Result := TItem( inherited Items[Idx]);
end;

procedure TGNamedCollection.SetItems(const Idx: Integer; AValue: TItem);
begin
  inherited Items[Idx] := AValue;
end;

constructor TGNamedCollection.Create;
begin
  inherited Create(TItem);
end;

function TGNamedCollection.Add: TItem;
begin
   Result := TItem(inherited Add);
end;

function TGNamedCollection.FindItem(const AName: String): TItemType;
begin
  if AName = '' then
    Result := nil
  else
    Result := TItemType(inherited FindItem(AName));
end;

{ THashedCollection }

procedure THashedCollection.ItemAdded(Item: TCollectionItem);
begin
  ItemIdentifierChanged( Item, '', TNamedCollectionItem(Item).Identifier);
end;

procedure THashedCollection.ItemRemoved(Item: TCollectionItem);
begin
  FHash.Remove(Item);
end;

procedure THashedCollection.ItemIdentifierChanged(Item: TCollectionItem;
  AOldName: String; ANewName: String);
begin
  if(AOldName <> '') and (ANewName <> '') then
  begin
    FHash.Rename(AOldName,ANewName);
  end
  else if (AOldName <> '') then
  begin
    FHash.Remove(Item);
  end
  else if (ANewName <> '') then
  begin
    FHash.Add(ANewName,Item);
  end;
end;

procedure THashedCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);

  Case Action of
    cnAdded                 : ItemAdded(Item);
    cnExtracting, cnDeleting: ItemRemoved(Item);
  end;
end;

constructor THashedCollection.Create(AItemClass: TNamedCollectionItemClass);
begin
  inherited Create(AItemClass);
  FHash := TFPHashObjectList.Create(False);
end;

destructor THashedCollection.Destroy;
begin
  inherited Destroy;
  FHash.Free;//has is used by inherited destructor
end;

function THashedCollection.IndexOfName(const AName: String): Integer;
var
  hash_idx: Integer;

  item : TCollectionItem;
begin

  hash_idx := FHash.FindIndexOf(AName);

  if hash_idx = -1 then
  begin
    Result := -1;
  end
  else
  begin
    item := TCollectionItem(FHash.Items[hash_idx]);

    Result := item.Index;

    Assert(Items[Result] = item, 'THashedCollection desynch');
  end;
end;

function THashedCollection.FindItem(const AName: String): TNamedCollectionItem;
var
  idx: Integer;
begin
  idx := IndexOfName(AName);

  if idx = -1 then
    Result := nil
  else
    Result := TNamedCollectionItem(Items[idx]);
end;

{ TNamedCollectionItem }

procedure TNamedCollectionItem.SetIdentifier(AValue: AnsiString);
begin
  Changed(false);
  if Assigned(Collection) then
    (Collection as THashedCollection).ItemIdentifierChanged(Self, FIdentifier, AValue);
  FIdentifier:=AValue;
end;

function TNamedCollectionItem.GetDisplayName: string;
begin
  Result := FIdentifier;
end;

procedure TNamedCollectionItem.SetDisplayName(const Value: string);
begin
//do nothing here
end;

{ TGCollection }

function TGArrayCollection.Add: TItem;
begin
  Result := TItem(inherited Add);
end;

constructor TGArrayCollection.Create;
begin
  inherited Create(TItem);
end;

function TGArrayCollection.GetItems(const Idx: Integer): TItem;
begin
  Result := TItem( inherited Items[Idx]);
end;

procedure TGArrayCollection.SetItems(const Idx: Integer; AValue: TItem);
begin
  inherited Items[Idx] := AValue;
end;

{ TObjectMap }

procedure TObjectMap.Deref(Item: Pointer);
begin
  Finalize(TKey(Item^));

  TData(Pointer(PByte(Item)+KeySize)^).Free;
end;

{ THeroPrimarySkills }

constructor THeroPrimarySkills.Create;
begin
  Attack :=-1;
  Defence:=-1;
  Spellpower:=-1;
  Knowledge:=-1;
end;

function THeroPrimarySkills.IsDefault: Boolean;
begin
  Result := (Attack = -1) and (Defence = -1) and (Spellpower = -1) and (Knowledge = -1);
end;

{ THeroSecondarySkill }

procedure THeroSecondarySkill.SetLevel(AValue: Integer);
begin
  if FLevel=AValue then Exit;
  if AValue <=0 then
    raise Exception.CreateFmt('Skill level invalid %d',[AValue]);
  FLevel:=AValue;
end;

end.

