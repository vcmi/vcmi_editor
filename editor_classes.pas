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
  Classes, SysUtils, fgl, fpjson, contnrs;

type

  { ISerializeNotify }

  ISerializeNotify = interface ['ISerializeNotify']
     procedure BeforeSerialize(Sender:TObject);
     procedure AfterSerialize(Sender:TObject; AData: TJSONData);

     procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
     procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
  end;

  { TNamedCollectionItem }

  TNamedCollectionItem = class(TCollectionItem)
  private
    FDisplayName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  end;

  { TGCollection }

  generic TGCollection <TItem> = class (TCollection)
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

  {
    May contain only one published property serialized directly (w/o object node)
  }

  IEmbeddedValue = interface ['IEmbbeddedValue']

  end;

  { INamedCollection
    Stored as object in JSON
    uses DisplayName as a name of field }

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

  { TGArrayCollection }

  generic TGArrayCollection <TItem> = class (specialize TGCollection <TItem>, IArrayCollection)

  end;

  { TGNamedCollection }

  generic TGNamedCollection <TItem> = class (specialize TGCollection <TItem>, INamedCollection)

  end;

  { IProgressCallback }

  IProgressCallback = interface
    function GetMax: Integer;
    procedure SetMax(AValue: Integer);

    property Max: Integer read GetMax write SetMax;

    procedure Advance(ADelta: integer);
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


implementation

{ TNamedCollectionItem }

function TNamedCollectionItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TNamedCollectionItem.SetDisplayName(const Value: string);
begin
  inherited SetDisplayName(Value);
  FDisplayName := Value;
end;

{ TGCollection }

function TGCollection.Add: TItem;
begin
  Result := TItem(inherited Add);
end;

constructor TGCollection.Create;
begin
  inherited Create(TItem);
end;

function TGCollection.GetItems(const Idx: Integer): TItem;
begin
  Result := TItem( inherited Items[Idx]);
end;

procedure TGCollection.SetItems(const Idx: Integer; AValue: TItem);
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

