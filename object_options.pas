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

unit object_options;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, editor_types, editor_classes;

type

{$push}
{$m+}
  { TObjectOptions }
  TObjectOptions      = class;
  TObjectOptionsClass = class of TObjectOptions;
  IObjectOptionReader = interface;

  TObjectOptions = class
  private
    FOwner: TPlayer;
    procedure SetOwner(AValue: TPlayer);
  public
    constructor Create; virtual;

    class function MayBeOwned: Boolean; virtual;

    procedure ReadBy(reader: IObjectOptionReader); virtual;

  published
    property Owner: TPlayer read FOwner write SetOwner stored MayBeOwned default TPlayer.none;
  end;
{$pop}

  { TCreatureInfo }

  TCreatureInfo = class (TCollectionItem)
  private
    FCreCount: Integer;
    FCreID: TCustomID;
    FRandomCount: boolean;
    procedure SetCreCount(AValue: Integer);
    procedure SetCreID(AValue: TCustomID);
    procedure SetRandomCount(AValue: boolean);
  public
    constructor Create(ACollection: TCollection); override;
  published
    property CreID: TCustomID read FCreID write SetCreID default -1;
    property CreCount: Integer read FCreCount write SetCreCount default 0;
    property RandomCount: boolean read FRandomCount write SetRandomCount default False;
  end;

  { TCreatureSet }

  TCreatureSet = class (specialize TGArrayCollection<TCreatureInfo>)

  private
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer);
    property MaxSize: Integer read FMaxSize;

  end;

  { TGuardedObjectOptions }

  TGuardedObjectOptions = class abstract (TObjectOptions)
  private
    FGuardMessage: string;
    FGuards: TCreatureSet;
    procedure SetGuardMessage(AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Guards: TCreatureSet read FGuards;
    property GuardMessage:string read FGuardMessage write SetGuardMessage;
  end;

  { TOwnedObjectOptions }

  TOwnedObjectOptions = class (TObjectOptions)
  public
    class function MayBeOwned: Boolean; override;
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TSignBottleOptions }

  TSignBottleOptions = class(TObjectOptions)
  private
    FText: string;
    procedure SetText(AValue: string);
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  published
    property Text: string read FText write SetText;
  end;

  { TPandorasOptions }

  TPandorasOptions = class (TGuardedObjectOptions)
  private
    FCreatures: TCreatureSet;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadBy(reader: IObjectOptionReader); override;
  published
    property Creatures: TCreatureSet read FCreatures;
  end;

  { TLocalEvenOptions }

  TLocalEvenOptions = class(TPandorasOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { THeroOptions }

  THeroOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TMonsterOptions }

  TMonsterOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TSeerHutOptions }

  TSeerHutOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TWitchHutOptions }

  TWitchHutOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TScholarOptions }

  TScholarOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TGarrisonOptions }

  TGarrisonOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TArtifactOptions }

  TArtifactOptions = class(TGuardedObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TSpellScrollOptions }

  TSpellScrollOptions = class(TArtifactOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TResourceOptions }

  TResourceOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TTownOptions }

  TTownOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TShrineOptions }

  TShrineOptions = class(TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TGrailOptions }

  TGrailOptions = class (TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TBaseRandomDwellingOptions }

  TBaseRandomDwellingOptions = class abstract (TObjectOptions)
  public
    class function MayBeOwned: Boolean; override;
  end;

  { TRandomDwellingOptions }

  TRandomDwellingOptions = class (TBaseRandomDwellingOptions)
  private
    FMaxLevel: UInt8;
    FMinLevel: UInt8;
    procedure SetMaxLevel(AValue: UInt8);
    procedure SetMinLevel(AValue: UInt8);
  public

    procedure ReadBy(reader: IObjectOptionReader); override;
  published
    property MinLevel: UInt8 read FMinLevel write SetMinLevel default 0;
    property MaxLevel: UInt8 read FMaxLevel write SetMaxLevel default 0;
  end;

  { TRandomDwellingLVLOptions }

  TRandomDwellingLVLOptions = class (TBaseRandomDwellingOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { TRandomDwellingTownOptions }

  TRandomDwellingTownOptions = class (TBaseRandomDwellingOptions)
  private
    FMaxLevel: UInt8;
    FMinLevel: UInt8;
    procedure SetMaxLevel(AValue: UInt8);
    procedure SetMinLevel(AValue: UInt8);
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  published
    property MinLevel: UInt8 read FMinLevel write SetMinLevel default 0;
    property MaxLevel: UInt8 read FMaxLevel write SetMaxLevel default 0;
  end;

  { TQuestGuardOptions }

  TQuestGuardOptions = class (TObjectOptions)
  public
    procedure ReadBy(reader: IObjectOptionReader); override;
  end;

  { THeroPlaceholderOptions }

  THeroPlaceholderOptions = class (TObjectOptions)
  private
    FPower: UInt8;
    FTypeID: TCustomID;
    procedure SetPower(AValue: UInt8);
    procedure SetTypeID(AValue: TCustomID);
  public
    class function MayBeOwned: Boolean; override;
    procedure ReadBy(reader: IObjectOptionReader); override;
  published
    property TypeID: TCustomID read FTypeID write SetTypeID default -1;
    property Power: UInt8 read FPower write SetPower default 0;
  end;


  { IObjectOptionReader }

  IObjectOptionReader = interface
    procedure ReadLocalEvent(AOptions: TLocalEvenOptions);
    procedure ReadSignBottle(AOptions: TSignBottleOptions);
    procedure ReadHero(AOptions: THeroOptions);
    procedure ReadMonster(AOptions: TMonsterOptions);
    procedure ReadSeerHut(AOptions: TSeerHutOptions);
    procedure ReadWitchHut(AOptions: TWitchHutOptions);
    procedure ReadScholar(AOptions: TScholarOptions);
    procedure ReadGarrison(AOptions: TGarrisonOptions);
    procedure ReadArtifact(AOptions: TArtifactOptions);
    procedure ReadSpellScroll(AOptions: TSpellScrollOptions);
    procedure ReadResource(AOptions: TResourceOptions);
    procedure ReadTown(AOptions: TTownOptions);
    procedure ReadShrine(AOptions: TShrineOptions);
    procedure ReadPandorasBox(AOptions: TPandorasOptions);
    procedure ReadGrail(AOptions: TGrailOptions);
    procedure ReadRandomDwelling(AOptions: TRandomDwellingOptions);
    procedure ReadRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);
    procedure ReadRandomDwellingTown(AOptions: TRandomDwellingTownOptions);
    procedure ReadQuestGuard(AOptions:TQuestGuardOptions);
    procedure ReadHeroPlaseholder(AOptions: THeroPlaceholderOptions);

    procedure ReadOwnedObject(AOptions: TOwnedObjectOptions);
  end;

function CreateByID(ID: TObjectTypeID; SubID: TCustomID): TObjectOptions;

implementation

uses
  editor_consts;

function CreateByID(ID: TObjectTypeID; SubID: TCustomID): TObjectOptions;
var
  c: TObjectOptionsClass;
begin
  //TODO: CreateByID
  c := TObjectOptions;
  case TObj(ID) of

    TObj.EVENT:
    begin
      c := TLocalEvenOptions;
    end;
    TObj.SIGN, TObj.BOAT:
    begin
      c := TSignBottleOptions;
    end;
    HERO, RANDOM_HERO, PRISON:
    begin
      c := THeroOptions;
    end;
    MONSTER,
    RANDOM_MONSTER,
    RANDOM_MONSTER_L1,
    RANDOM_MONSTER_L2,
    RANDOM_MONSTER_L3,
    RANDOM_MONSTER_L4,
    RANDOM_MONSTER_L5,
    RANDOM_MONSTER_L6,
    RANDOM_MONSTER_L7:
    begin
      c := TMonsterOptions;
    end;
    SEER_HUT:
    begin
      c := TSeerHutOptions;
    end;
    WITCH_HUT:
    begin
      c := TWitchHutOptions;
    end;
    SCHOLAR:
    begin
      c := TScholarOptions;
    end;
    GARRISON, GARRISON2:
    begin
      c := TGarrisonOptions;
    end;
    ARTIFACT, RANDOM_ART,
    RANDOM_TREASURE_ART,
    RANDOM_MINOR_ART,
    RANDOM_MAJOR_ART,
    RANDOM_RELIC_ART:
    begin
      c := TArtifactOptions;
    end;
    SPELL_SCROLL:
    begin
      c := TSpellScrollOptions;
    end;
    RESOURCE,
    RANDOM_RESOURCE:
    begin
      c := TResourceOptions;
    end;
    RANDOM_TOWN, TOWN:
      c := TTownOptions;
    MINE,
    ABANDONED_MINE,
    CREATURE_GENERATOR1,
    CREATURE_GENERATOR2,
    CREATURE_GENERATOR3,
    CREATURE_GENERATOR4:
      c := TOwnedObjectOptions;

    SHRINE_OF_MAGIC_GESTURE,
    SHRINE_OF_MAGIC_INCANTATION,
    SHRINE_OF_MAGIC_THOUGHT:
      c := TShrineOptions;
    PANDORAS_BOX:
      c := TPandorasOptions;
    GRAIL:
      c := TGrailOptions;

    RANDOM_DWELLING:
      c := TRandomDwellingOptions;
    RANDOM_DWELLING_LVL:
      c := TRandomDwellingLVLOptions;
    RANDOM_DWELLING_FACTION:
      c := TRandomDwellingTownOptions;
    QUEST_GUARD:
      c := TQuestGuardOptions;
    SHIPYARD,LIGHTHOUSE:
      c := TOwnedObjectOptions;
  end;


  Result := c.Create;
end;

{ THeroPlaceholderOptions }

class function THeroPlaceholderOptions.MayBeOwned: Boolean;
begin
  Result := True;
end;

procedure THeroPlaceholderOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadHeroPlaseholder(Self);
end;

procedure THeroPlaceholderOptions.SetPower(AValue: UInt8);
begin
  if FPower = AValue then Exit;
  FPower := AValue;
end;

procedure THeroPlaceholderOptions.SetTypeID(AValue: TCustomID);
begin
  if FTypeID = AValue then Exit;
  FTypeID := AValue;
end;

{ TOwnedObjectOptions }

class function TOwnedObjectOptions.MayBeOwned: Boolean;
begin
  result := True;
end;

procedure TOwnedObjectOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadOwnedObject(Self);
end;

{ TQuestGuardOptions }

procedure TQuestGuardOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadQuestGuard(Self);
end;

{ TBaseRandomDwellingOptions }

class function TBaseRandomDwellingOptions.MayBeOwned: Boolean;
begin
  Result := True;
end;


{ TCreatureInfo }

constructor TCreatureInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCreID := -1;
end;

procedure TCreatureInfo.SetCreCount(AValue: Integer);
begin
  if FCreCount = AValue then Exit;
  FCreCount := AValue;
end;

procedure TCreatureInfo.SetCreID(AValue: TCustomID);
begin
  if FCreID = AValue then Exit;
  FCreID := AValue;
end;

procedure TCreatureInfo.SetRandomCount(AValue: boolean);
begin
  if FRandomCount = AValue then Exit;
  FRandomCount := AValue;

  if AValue then
  begin
    FCreCount := 0;
  end;
end;

{ TGuardedObjectOptions }

constructor TGuardedObjectOptions.Create;
begin
  inherited Create;
  FGuards := TCreatureSet.Create(7);
end;

destructor TGuardedObjectOptions.Destroy;
begin
  FGuards.Free;
  inherited Destroy;
end;

procedure TGuardedObjectOptions.SetGuardMessage(AValue: string);
begin
  if FGuardMessage = AValue then Exit;
  FGuardMessage := AValue;
end;

{ TCreatureSet }

constructor TCreatureSet.Create(AMaxSize: Integer);
begin
  inherited Create;
  FMaxSize := AMaxSize;
end;

{ TRandomDwellingOptions }

procedure TRandomDwellingOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadRandomDwelling(Self);
end;

procedure TRandomDwellingOptions.SetMaxLevel(AValue: UInt8);
begin
  if FMaxLevel = AValue then Exit;
  FMaxLevel := AValue;
end;

procedure TRandomDwellingOptions.SetMinLevel(AValue: UInt8);
begin
  if FMinLevel = AValue then Exit;
  FMinLevel := AValue;
end;

{ TRandomDwellingLVLOptions }

procedure TRandomDwellingLVLOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadRandomDwellingLVL(Self);
end;

{ TRandomDwellingTownOptions }

procedure TRandomDwellingTownOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadRandomDwellingTown(Self);
end;

procedure TRandomDwellingTownOptions.SetMaxLevel(AValue: UInt8);
begin
  if FMaxLevel = AValue then Exit;
  FMaxLevel := AValue;
end;

procedure TRandomDwellingTownOptions.SetMinLevel(AValue: UInt8);
begin
  if FMinLevel = AValue then Exit;
  FMinLevel := AValue;
end;

{ TGrailOptions }

procedure TGrailOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadGrail(Self);
end;

{ TPandorasOptions }

constructor TPandorasOptions.Create;
begin
  inherited Create;
  FCreatures := TCreatureSet.Create(0);
end;

destructor TPandorasOptions.Destroy;
begin
  FreeAndNil(FCreatures);
  inherited Destroy;
end;

procedure TPandorasOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadPandorasBox(Self);
end;

{ TShrineOptions }

procedure TShrineOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadShrine(Self);
end;

{ TTownOptions }

procedure TTownOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadTown(Self);
end;

{ TResourceOptions }

procedure TResourceOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadResource(Self);
end;

{ TSpellScrollOptions }

procedure TSpellScrollOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadSpellScroll(Self);
end;

{ TArtifactOptions }

procedure TArtifactOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadArtifact(Self);
end;

{ TGarrisonOptions }

procedure TGarrisonOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadGarrison(Self);
end;

{ TScholarOptions }

procedure TScholarOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadScholar(Self);
end;

{ TWitchHutOptions }

procedure TWitchHutOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadWitchHut(Self);
end;

{ TSeerHutOptions }

procedure TSeerHutOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadSeerHut(self);
end;

{ TMonsterOptions }

procedure TMonsterOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadMonster(Self);
end;

{ THeroOptions }

procedure THeroOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadHero(Self);
end;

{ TLocalEvenOptions }

procedure TLocalEvenOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadLocalEvent(Self);
end;

{ TSignBottleOptions }

procedure TSignBottleOptions.ReadBy(reader: IObjectOptionReader);
begin
  reader.ReadSignBottle(Self);
end;

procedure TSignBottleOptions.SetText(AValue: string);
begin
  if FText = AValue then
  begin
    Exit;
  end;
  FText := AValue;
end;

{ TObjectOptions }

constructor TObjectOptions.Create;
begin
  FOwner := TPlayer.none;
end;

class function TObjectOptions.MayBeOwned: Boolean;
begin
  Result := False;
end;

procedure TObjectOptions.ReadBy(reader: IObjectOptionReader);
begin
  //do nothing here
end;

procedure TObjectOptions.SetOwner(AValue: TPlayer);
begin
  if FOwner = AValue then
  begin
    Exit;
  end;
  FOwner := AValue;
end;

end.
