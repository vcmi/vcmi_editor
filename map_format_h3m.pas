{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit map_format_h3m;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, math, fgl, typinfo, FileUtil, map, map_format, terrain,
  stream_adapter, editor_types, object_options, editor_classes, lists_manager,
  map_objects, editor_graphics, logical_id_condition,
  logical_event_condition, logical_expression, position;

const
  MAP_VERSION_ROE = $0e;
  MAP_VERSION_AB = $15;
  MAP_VERSION_SOD = $1c;
  MAP_VERSION_WOG = $33;
  MAP_VERSION_HOTA = 30;

  TOTAL_FACTIONS = 9;
  TOTAL_FACTIONS_ROE = 8;


type
   TOwnerSize = (size1,size4);

  TH3MQuestIdentifierMap = specialize TFPGMap<UInt32, TMapObject>;

  { TLegacyMapObjectTemplate }

  TLegacyMapObjectTemplate = class (TCollectionItem)
  private
    FDef: TDefAnimation;
  private
    FType: AnsiString;
    FAllowedTerrains: TTerrainTypes;
    FMask: TStrings;
    FAnimation: string;
    FMenuTerrains: TTerrainTypes;
    Fsubtype: AnsiString;
    FZIndex: Integer;
    FVisitableFrom: TStrings;
    function GetTID: integer;
    procedure SetType(AValue: AnsiString);
    procedure SetAllowedTerrains(AValue: TTerrainTypes);
    procedure SetAnimation(AValue: string);
    procedure SetMenuTerrains(AValue: TTerrainTypes);
    procedure Setsubtype(AValue: AnsiString);
    procedure SetZIndex(AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property AllowedTerrains: TTerrainTypes read FAllowedTerrains write SetAllowedTerrains;
    property MenuTerrains: TTerrainTypes read FMenuTerrains write SetMenuTerrains;

    property TID: integer read GetTID;

    property Animation:string read FAnimation write SetAnimation;
    property Mask:TStrings read FMask;
    property VisitableFrom: TStrings read FVisitableFrom;

    property &type: AnsiString read FType write SetType;
    property subtype: AnsiString read Fsubtype write Setsubtype;

    property ZIndex: Integer read FZIndex write SetZIndex;
  end;

  { TLegacyMapObjectTemplates }

  TLegacyMapObjectTemplates = class (specialize TGArrayCollection<TLegacyMapObjectTemplate>)
  private
    FGraphicsManager: TGraphicsManager;
  public
    constructor Create(AGraphicsManager: TGraphicsManager);
  end;

  { TQIResolveRequest }

  TQIResolveRequest = class
  public
    Type
      TResolveProc = procedure (AValue: string) of object;
  private
    FIdentifier: UInt32;
    FResolveProc: TResolveProc;
  public
    property Identifier: UInt32 read FIdentifier write FIdentifier;
    property ResolveProc:TResolveProc read FResolveProc write FResolveProc;
  end;

  TQIResolveRequests = specialize TFPGObjectList<TQIResolveRequest>;

  { TPositionResolveRequest }

  TPositionResolveRequest = class
  public
    Type
      TResolveProc = procedure (AValue: TMapObject) of object;
  private
    FAllowedTypes: TStrings;
    FPosition: TPosition;
    FResolveProc: TResolveProc;

    function CheckObjectType(AObject: TMapObject): boolean;
    function CheckObjectPosition(AObject: TMapObject): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property AllowedTypes: TStrings read FAllowedTypes;
    property Position:TPosition read FPosition;
    property ResolveProc:TResolveProc read FResolveProc write FResolveProc;

    function CheckObject(AObject: TMapObject): Boolean;
  end;

  TPositionResolveRequests = specialize TFPGObjectList<TPositionResolveRequest>;

   { TMapReaderH3m }

   TMapReaderH3m = class(TBaseMapFormatHandler, IMapReader, IObjectOptionsVisitor)
   strict private
     FSrc: TStreamReadAdapter;
     FMapVersion: DWord;
     FMap: TVCMIMap;

     FTemplates: TLegacyMapObjectTemplates;

     FCurrentObject: TMapObject;
     FQuestIdentifierMap: TH3MQuestIdentifierMap;
     FQILinksToResolve: TQIResolveRequests;
     FPositionsToResolve: TPositionResolveRequests;

     class procedure CheckMapVersion(const AVersion: DWord); static;

     function IsAtLeastAB: Boolean;
     function IsAtLeastSoD: Boolean;

     function IsWog: Boolean;

     procedure SkipNotImpl(count: Integer);
     procedure PushQIResolveRequest(AIdent: UInt32; AResolveProc: TQIResolveRequest.TResolveProc);
     procedure ResolveQuestIdentifiers;
     procedure ResolveTownPositions;
     procedure ResolveAll;
   strict private
     type
       TIdToString = function(AId: TCustomID): AnsiString of object;

       TCustomIDArray = array of TCustomID;

     procedure ReadBitmask(var ADest: TCustomIDArray; AMaskSize: SizeInt; ACount: SizeInt; Negate: Boolean = True);

     procedure ReadBitmask(ADest: TStrings;AMaskSize: SizeInt; ACount: SizeInt;
       ACallback: TIdToString; Negate: Boolean = True);

     procedure ReadBitmask(ADest: TLogicalIDCondition; AMaskSize: SizeInt; ACount: SizeInt;
       ACallback: TIdToString; Negate: Boolean = True);

     function ReadID1(ACallback: TIdToString; AIDRandom:TCustomID = ID_RANDOM): AnsiString;

     function ReadID(ACallback: TIdToString; ASize: Integer): AnsiString; //none mask : $FF, $FFFF

     procedure ReadArtifactSet(ADest: TStrings);

     procedure ReadCreatureSet(ACreatureSet: TCreatureSet; nomber: Integer);
     procedure ReadCreatureSet(ACreatureSet: TCreatureSet); //any size
     procedure ReadQuestIdentifier;
     procedure ReadResources(AresourceSet: TResourceSet);
     procedure ReadPrimarySkills(ASkills: THeroPrimarySkills);
     procedure MaybeReadSecondarySkills(ASkills: THeroSecondarySkills);
     procedure ReadSecondarySkills4(ASkills: THeroSecondarySkills);
     procedure ReadSecondarySkills1(ASkills: THeroSecondarySkills);

     procedure ReadPosition(APosition: TPosition);
   strict private
     procedure ReadPlayerInfos(Attrs: TPlayerInfos);//+
     procedure ReadPlayerInfo(Attr: TPlayerInfo);//+?
     procedure ReadSVLC();//+
     procedure ReadTeams();//+
     procedure ReadAllowedHeros();//+
     procedure ReadDisposedHeros();//+
     procedure ReadAllowedArtifacts();//+
     procedure ReadAllowedSpells();//+
     procedure ReadAllowedAbilities();//+
     procedure ReadRumors();//+
     procedure ReadPredefinedHeroes();//+

     procedure ReadTerrain();//+

     procedure ReadObjMask(obj: TLegacyMapObjectTemplate);//+
     procedure ReadDefInfo();//+

     procedure MaybeReadArtifactsOfHero(obj: THeroArtifacts);//+
     procedure ReadArtifactsOfHero(obj: THeroArtifacts);//+
     procedure ReadArtifactsToSlot(obj: THeroArtifacts; slot: Integer);//+

     //result = Mission type
     function ReadQuest(obj: TQuest): TQuestMission;   //+

     procedure ReadObjects();//+
     procedure ReadEvents();

     procedure MayBeReadGuards(AOptions: TGuardedObjectOptions);//+
     procedure MayBeReadGuardsWithMessage(AOptions: TGuardedObjectOptions);//+

     procedure ReadOwner(AOptions: TObjectOptions; size: TOwnerSize = TOwnerSize.size4); //+

     procedure ReadHero(AOptions: THeroOptions);//+
   public //IObjectOptionsVisitor
     procedure VisitSignBottle(AOptions: TSignBottleOptions);//+
     procedure VisitLocalEvent(AOptions: TLocalEventOptions);//+

     procedure VisitPrison(AOptions: TPrisonOptions); //+
     procedure VisitNormalHero(AOptions: TNormalHeroOptions); //+
     procedure VisitRandomHero(AOptions: TRandomHeroOptions); //+

     procedure VisitMonster(AOptions: TCreatureOptions);//+
     procedure VisitSeerHut(AOptions: TSeerHutOptions);//+
     procedure VisitWitchHut(AOptions:TWitchHutOptions);//+
     procedure VisitScholar(AOptions: TScholarOptions);//+
     procedure VisitGarrison(AOptions: TGarrisonOptions);//+
     procedure VisitArtifact(AOptions: TArtifactOptions);//+
     procedure VisitSpellScroll(AOptions: TSpellScrollOptions);//+
     procedure VisitResource(AOptions: TResourceOptions);//+
     procedure VisitTown(AOptions: TTownOptions);
     procedure VisitAbandonedMine(AOptions: TAbandonedOptions);//+
     procedure VisitMine(AOptions: TMineOptions);//+
     procedure VisitShrine(AOptions: TShrineOptions);//+
     procedure VisitPandorasBox(AOptions: TPandorasOptions);//+
     procedure VisitGrail(AOptions: TGrailOptions);//+
     procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions);//+
     procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions);//+
     procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions);//+
     procedure VisitQuestGuard(AOptions: TQuestGuardOptions);//+
     procedure VisitOwnedObject(AOptions: TOwnedObjectOptions);//+
     procedure VisitHeroPlaceholder(AOptions: THeroPlaceholderOptions);//+
   public
     constructor Create(AMapEnv: TMapEnvironment); override;
     destructor Destroy; override;
     function Read(AStream: TStream): TVCMIMap;
   end;


implementation

uses LazLoggerBase, editor_consts, editor_utils;

{ TPositionResolveRequest }

function TPositionResolveRequest.CheckObjectType(AObject: TMapObject): boolean;
begin
  if AllowedTypes.Count = 0 then
  begin
    Result := True;
  end
  else
  begin
    Result := AllowedTypes.IndexOf(AObject.&Type) >= 0;
  end;
end;

function TPositionResolveRequest.CheckObjectPosition(AObject: TMapObject): boolean;
begin
  Result := AObject.EqualPosition(FPosition) or AObject.IsVisitableAt(FPosition.L, FPosition.X, FPosition.Y);
end;

constructor TPositionResolveRequest.Create;
begin
  FPosition := TPosition.Create;
  FAllowedTypes := TIdentifierSet.Create(nil);
end;

destructor TPositionResolveRequest.Destroy;
begin
  FAllowedTypes.Free;
  FPosition.Free;
  inherited Destroy;
end;

function TPositionResolveRequest.CheckObject(AObject: TMapObject): Boolean;
begin
  Result := CheckObjectPosition(AObject) and CheckObjectType(AObject);
end;

{ TLegacyMapObjectTemplates }

constructor TLegacyMapObjectTemplates.Create(AGraphicsManager: TGraphicsManager);
begin
  inherited Create;
  FGraphicsManager := AGraphicsManager;
end;

{ TLegacyMapObjectTemplate }

constructor TLegacyMapObjectTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMask := TStringList.Create;
  FAllowedTerrains := ALL_TERRAINS;
  FVisitableFrom := TStringList.Create;
end;

destructor TLegacyMapObjectTemplate.Destroy;
begin
  FVisitableFrom.Free;
  FMask.Free;
  inherited Destroy;
end;

function TLegacyMapObjectTemplate.GetTID: integer;
begin
  Result := inherited ID;
end;

procedure TLegacyMapObjectTemplate.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;
  FType:=AValue;
end;

procedure TLegacyMapObjectTemplate.SetAllowedTerrains(AValue: TTerrainTypes);
begin
  if FAllowedTerrains = AValue then Exit;
  FAllowedTerrains := AValue;
end;

procedure TLegacyMapObjectTemplate.SetAnimation(AValue: string);
begin
  AValue := NormalizeResourceName(AValue);
  if FAnimation = AValue then Exit;
  FAnimation := AValue;

  FDef := (Collection as TLegacyMapObjectTemplates).FGraphicsManager.GetGraphics(FAnimation);
  //todo: load and check
end;

procedure TLegacyMapObjectTemplate.SetMenuTerrains(AValue: TTerrainTypes);
begin
  if FMenuTerrains = AValue then Exit;
  FMenuTerrains := AValue;
end;

procedure TLegacyMapObjectTemplate.Setsubtype(AValue: AnsiString);
begin
  if Fsubtype=AValue then Exit;
  Fsubtype:=AValue;
end;

procedure TLegacyMapObjectTemplate.SetZIndex(AValue: Integer);
begin
  if FZIndex = AValue then Exit;
  FZIndex := AValue;
end;

{ TMapReaderH3m }

class procedure TMapReaderH3m.CheckMapVersion(const AVersion: DWord);
begin
  if    (AVersion <> MAP_VERSION_AB)
    and (AVersion <> MAP_VERSION_ROE)
    and (AVersion <> MAP_VERSION_SOD)
    and (AVersion <> MAP_VERSION_WOG)
    {and (AVersion <> MAP_VERSION_HOTA)}then
  begin
    raise Exception.Create('Invalid map format '+IntToHex(AVersion,8));
  end;
end;

function TMapReaderH3m.IsAtLeastAB: Boolean;
begin
  Result := FMapVersion >= MAP_VERSION_AB;
end;

function TMapReaderH3m.IsAtLeastSoD: Boolean;
begin
  Result := FMapVersion >= MAP_VERSION_SOD;
end;

constructor TMapReaderH3m.Create(AMapEnv: TMapEnvironment);
begin
  inherited Create(AMapEnv);
  FQuestIdentifierMap := TH3MQuestIdentifierMap.Create;
  FQILinksToResolve := TQIResolveRequests.Create(True);
  FPositionsToResolve := TPositionResolveRequests.Create(True);
  FTemplates  := TLegacyMapObjectTemplates.Create(AMapEnv.tm.GraphicsManager);
end;

destructor TMapReaderH3m.Destroy;
begin
  FTemplates.Free;
  FPositionsToResolve.Free;
  FQILinksToResolve.Free;
  FQuestIdentifierMap.Free;
  inherited Destroy;
end;

function TMapReaderH3m.IsWog: Boolean;
begin
  Result :=  FMapVersion >= MAP_VERSION_WOG;
end;

procedure TMapReaderH3m.MayBeReadGuards(AOptions: TGuardedObjectOptions);
var
  is_guard: Boolean;
begin
  is_guard := FSrc.ReadBoolean;
  if is_guard then
  begin
    ReadCreatureSet(AOptions.Guards,7);
  end;
end;

procedure TMapReaderH3m.MayBeReadGuardsWithMessage(
  AOptions: TGuardedObjectOptions);
var
  is_guard_msg: Boolean;
begin
  is_guard_msg := FSrc.ReadBoolean;
  if is_guard_msg then
  begin
    AOptions.GuardMessage := FSrc.ReadLocalizedString;

    MayBeReadGuards(AOptions);

    FSrc.Skip(4); //junk
  end;
end;

function TMapReaderH3m.Read(AStream: TStream): TVCMIMap;
var
  cr_params: TMapCreateParams;
  lvl: TMapLevel;
begin
  FQuestIdentifierMap.Clear;
  FTemplates.Clear;

  AStream.Seek(0,soBeginning);
  FSrc.Create(AStream);
  //main header part
  with FSrc do
  begin
    FMapVersion := ReadDWord;
    CheckMapVersion(FMapVersion);
    ReadBoolean;//are any players
    cr_params.Height := ReadDWord;
    cr_params.Width := cr_params.Height;
    cr_params.Levels := ReadByte + 1; //one level = 0
  end;

  FMap := TVCMIMap.CreateEmpty(FMapEnv);

  lvl := FMap.MapLevels.Add;
  lvl.Width:=cr_params.Width;
  lvl.Height:=cr_params.Height;
  lvl.Identifier := 'surface';
  if cr_params.Levels>1 then
  begin
    lvl := FMap.MapLevels.Add;
    lvl.Width:=cr_params.Width;
    lvl.Height:=cr_params.Height;
    lvl.Identifier := 'underground';
  end;

  FMap.CurrentLevelIndex := 0;

  Result := FMap;
  try
    //rest of header
    with FSrc do
    begin
      Result.Name := ReadLocalizedString;
      Result.Description := ReadLocalizedString;

      Result.Difficulty := TDifficulty(ReadByte);

      if IsAtLeastAB() then
      begin
        Result.HeroLevelLimit := ReadByte;
      end else
      begin
        Result.HeroLevelLimit := 0;
      end;
    end;

    ReadPlayerInfos(FMap.Players);
    ReadSVLC();
    ReadTeams();
    ReadAllowedHeros();
    ReadDisposedHeros();
    ReadAllowedArtifacts();
    ReadAllowedSpells();
    ReadAllowedAbilities();
    ReadRumors();
    ReadPredefinedHeroes();
    ReadTerrain();
    ReadDefInfo();
    ReadObjects();
    ReadEvents();

    ResolveAll();

    if IsWog then
    begin
      FMap.ModUsage.ForceMod('wog', true);
    end;
  except
    FreeAndNil(Fmap);
    raise;
  end;
  Result.Loaded;
  FMap := nil;
end;

procedure TMapReaderH3m.ReadAllowedAbilities;
begin
  if IsAtLeastSoD() then
  begin
    ReadBitmask(FMap.AllowedAbilities,4,SECONDARY_SKILL_QUANTITY,@FMapEnv.lm.SkillNidToString);
  end;
end;

procedure TMapReaderH3m.ReadAllowedArtifacts;
var
  cnt: Integer;
begin
  if IsAtLeastAB() then
  begin
    cnt := ifthen(IsAtLeastAB(),18,17);

    ReadBitmask(FMap.AllowedArtifacts, cnt, ARTIFACT_QUANTITY, @FMapEnv.lm.ArtifactIndexToString, false);
  end;
end;

procedure TMapReaderH3m.ReadAllowedHeros;
var
  cnt: Integer;
begin

  cnt := ifthen(IsAtLeastAB(),20,16);

  ReadBitmask(FMap.AllowedHeroes, cnt, HERO_QUANTITY, @FMapEnv.lm.HeroIndexToString, False);

  //unknown plaseholder
  if IsAtLeastAB() then
  begin
    cnt :=FSrc.ReadDWord;
    FSrc.Skip(cnt);
  end;
end;

procedure TMapReaderH3m.ReadAllowedSpells;
begin
  if IsAtLeastSoD() then
  begin
    ReadBitmask(FMap.AllowedSpells,9,SPELL_QUANTITY_ACTUAL,@(FMapEnv.lm.SpellIndexToString));
  end;
end;

procedure TMapReaderH3m.VisitArtifact(AOptions: TArtifactOptions);
begin
  MayBeReadGuardsWithMessage(AOptions);
end;

procedure TMapReaderH3m.ReadArtifactsOfHero(obj: THeroArtifacts);
var
  i: Integer;
  cnt: Word;
begin
  with FSrc do
  begin
    if not ReadBoolean then exit;

    for i := 0 to 15 do
    begin
      ReadArtifactsToSlot(obj,i);
    end;

    if IsAtLeastSoD() then
    begin
      ReadArtifactsToSlot(obj,16);
    end;
    ReadArtifactsToSlot(obj,17); //spellbook
    if IsAtLeastAB() then
    begin
      ReadArtifactsToSlot(obj,18);
    end;

    cnt := ReadWord;
    for i := 0 to cnt - 1 do
    begin
      ReadArtifactsToSlot(obj,19+i);
    end;
  end;
end;

procedure TMapReaderH3m.ReadArtifactsToSlot(obj: THeroArtifacts; slot: Integer);
var
  artmask: integer;
  aid: Word;

  artId: AnsiString;
begin
  artmask := ifthen(IsAtLeastAB(),$FFFF, $FF);

  if IsAtLeastAB() then
  begin
    aid := FSrc.ReadWord;
  end
  else begin
    aid := FSrc.ReadByte;
  end;

  if aid = artmask then
    exit;

  artId:=FMapEnv.lm.ArtifactIndexToString(aid);
  obj.BySlotNumber[slot] := artId;
end;

procedure TMapReaderH3m.ReadBitmask(var ADest: TCustomIDArray; AMaskSize: SizeInt; ACount: SizeInt; Negate: Boolean);
var
  real_count: SizeInt;

  byte_idx: SizeInt;
  mask_byte: Byte;
  bit: UInt8;
  flag: Boolean;
  id: TCustomID;

begin
  SetLength(ADest, 0);
  SetLength(ADest, ACount);

  real_count := 0;

  for byte_idx := 0 to AMaskSize - 1 do
  begin
    mask_byte := FSrc.ReadByte;

    for bit in [0..7] do
    begin
      flag := (mask_byte and (1 shl bit)) > 0;
      flag := flag xor Negate;
      id := byte_idx*8+bit;
      if flag and (id < ACount) then
      begin
        ADest[real_count] := id;
        inc(real_count);
      end;
    end;
  end;

  SetLength(ADest, real_count);

end;

procedure TMapReaderH3m.ReadBitmask(ADest: TStrings; AMaskSize: SizeInt;
  ACount: SizeInt; ACallback: TIdToString; Negate: Boolean);
var
  tmp: array of TCustomID;

  iter: TCustomID;
begin
  ADest.Clear;

  SetLength(tmp, 0);

  ReadBitmask(tmp, AMaskSize, ACount, Negate);

  for iter in tmp do
  begin
    ADest.Add(ACallback(iter));
  end;

end;

procedure TMapReaderH3m.ReadBitmask(ADest: TLogicalIDCondition;
  AMaskSize: SizeInt; ACount: SizeInt; ACallback: TIdToString; Negate: Boolean);
var
  tmp: TStringList;
begin
  ADest.Clear;
  tmp := TStringList.Create;
  try
    ReadBitmask(tmp, AMaskSize, ACount, ACallback, Negate);
    ADest.AnyOf.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

function TMapReaderH3m.ReadID1(ACallback: TIdToString; AIDRandom: TCustomID
  ): AnsiString;
var
  index: TCustomID;
begin
  index :=  FSrc.ReadIDByte;

  if index = AIDRandom then
  begin
    Result := '';
  end
  else
  begin
     Result := ACallback(index);
  end;
end;

function TMapReaderH3m.ReadID(ACallback: TIdToString; ASize: Integer
  ): AnsiString;
var
  id : TCustomID;
begin
  id := -1;

  if ASize = 1 then
  begin
    id := FSrc.ReadByte;
    if id = $FF then
    begin
      id := -1;
    end;
  end
  else if ASize = 2 then
  begin
    id := FSrc.ReadWord;
    if id = $FFFF then
    begin
      id := -1;
    end;
  end
  else
    Assert(False, 'ReadID: invalid size');

  if id >= 0 then
  begin
    Result := ACallback(id);
  end
  else begin
    Result := '';
  end;
end;

procedure TMapReaderH3m.ReadArtifactSet(ADest: TStrings);
var
  cnt: Byte;
  i: Integer;
  artid: Word;
begin
  ADest.Clear;
  cnt := FSrc.ReadByte();
  for i := 0 to cnt - 1 do
  begin
    artid := FSrc.ReadWord;
    ADest.Add(FMapEnv.lm.ArtifactIndexToString(artid));
  end;
end;

procedure TMapReaderH3m.ReadCreatureSet(ACreatureSet: TCreatureSet;
  nomber: Integer);
var
  maxID,
  creid,crecnt: Integer;
  version: Boolean;
  i: Integer;
  info: TCreatureInstInfo;
begin
  version := IsAtLeastAB();

  maxID := ifthen(version,$ffff, $ff);

  ACreatureSet.Clear;

  with FSrc do begin
    for i := 0 to nomber - 1 do
    begin
      info := ACreatureSet.Add;
      if version then
      begin
        creid := ReadWord;
      end
      else begin
        creid := ReadByte;
      end;
      crecnt := ReadWord;
      if creid = maxID then
        Continue;// empty slot, leave with default values

      if creid > maxID - $f then
      begin
        info.RawRandom:=maxID - creID - 1;
      end
      else
      begin
        info.&type := FMapEnv.lm.CreatureIndexToString(creid);
        info.Amount := crecnt;
      end;
    end;
  end;
end;

procedure TMapReaderH3m.ReadCreatureSet(ACreatureSet: TCreatureSet);
var
  cnt: Byte;
  i: Integer;
  creid: Word;
  crecnt: Word;

  info: TCreatureInstInfo;
begin
  ACreatureSet.Clear;

  cnt := FSrc.ReadByte;
  for i := 0 to cnt - 1 do
  begin
    creid := FSrc.ReadWord;
    crecnt := FSrc.ReadWord;
    info := ACreatureSet.Add;
    info.&type := FMapEnv.lm.CreatureIndexToString(creid);
    info.Amount := crecnt;
  end;
end;

procedure TMapReaderH3m.ReadQuestIdentifier;
var
  ident: DWord;
begin
  if IsAtLeastAB() then
  begin
    ident := FSrc.ReadDWord;
    FQuestIdentifierMap.KeyData[ident] := FCurrentObject;
  end;
end;

procedure TMapReaderH3m.ReadResources(AresourceSet: TResourceSet);
var
  i: Integer;
begin
  for i := 0 to 7 - 1 do
  begin
    AresourceSet.Amount[TResType(i)] := FSrc.ReadInt32;
  end;
end;

procedure TMapReaderH3m.ReadPrimarySkills(ASkills: THeroPrimarySkills);
begin
  ASkills.Attack:=FSrc.ReadByte;
  ASkills.Defence:=FSrc.ReadByte;
  ASkills.Spellpower:=FSrc.ReadByte;
  ASkills.Knowledge:=FSrc.ReadByte;
end;

procedure TMapReaderH3m.MaybeReadSecondarySkills(ASkills: THeroSecondarySkills);
begin
  with FSrc do
    if ReadBoolean then
    begin
      ReadSecondarySkills4(ASkills);
    end;
end;

procedure TMapReaderH3m.ReadSecondarySkills4(ASkills: THeroSecondarySkills);
var
  secSkill: THeroSecondarySkill;
  cnt: DWord;
  i: Integer;
begin
  with FSrc do
  begin
    cnt := ReadDWord;
    for i := 0 to cnt - 1 do
    begin
      secSkill := ASkills.Add;
      secSkill.Identifier :=ReadID(@FMapEnv.lm.SkillNidToString,1);
      secSkill.Level:=TSkillLevel(ReadByte);
    end;
  end;
end;

procedure TMapReaderH3m.ReadSecondarySkills1(ASkills: THeroSecondarySkills);
var
  secSkill: THeroSecondarySkill;
  cnt: Integer;
  i: Integer;
begin
  with FSrc do
  begin
    cnt := ReadByte;
    for i := 0 to cnt - 1 do
    begin
      secSkill := ASkills.Add;
      secSkill.Identifier :=ReadID(@FMapEnv.lm.SkillNidToString,1);
      secSkill.Level:=TSkillLevel(ReadByte);
    end;
  end;

end;

procedure TMapReaderH3m.ReadPosition(APosition: TPosition);
begin
  with APosition, FSrc do
  begin
    X:=ReadByte;
    Y:=ReadByte;
    L:=ReadByte;
  end;
end;

procedure TMapReaderH3m.ReadDefInfo;

function read_terrains(): TTerrainTypes;
var
  tt: TTerrainType;
  i: Integer;
  data: Word;
begin
  data:= FSrc.ReadWord;
  Result := [];
  for tt in TTerrainType do
  begin
    i := Integer(tt);

    if ((1 shl i) and data) > 0 then
    begin
      Result +=[tt];
    end;
  end;
end;

var
  obj: TLegacyMapObjectTemplate;
  cnt: DWord;
  i: Integer;
  b: Byte;
  obj_type: TMapObjectType;
  ID: DWord;
  sub_id: DWord;
  group: Byte;

begin
  cnt := FSrc.ReadDWord;

  for i := 0 to cnt - 1 do
  begin
    obj :=  FTemplates.Add;

    obj.Animation := FSrc.ReadString;
    ReadObjMask(obj);

    obj.AllowedTerrains := read_terrains();
    obj.MenuTerrains := read_terrains();

    ID := FSrc.ReadDWord;
    sub_id :=  FSrc.ReadDWord;
    obj_type := FMapEnv.om.ResolveLegacyID(id,sub_id);

    if Assigned(obj_type) then
    begin
      obj.&type := obj_type.MapObjectGroup.Identifier;
      obj.subtype:=obj_type.Identifier;
    end
    else
    begin
      DebugLn(['Unknown object:  TID ', i, ' ID ', ID, ' subid ',  sub_id]);
    end;

    group := FSrc.ReadByte;

    b := FSrc.ReadByte;
    obj.ZIndex := b * Z_INDEX_OVERLAY;
    FSrc.Skip(16); //junk

    GenerateDefaultVisitableFrom(obj.VisitableFrom,group, TObj(ID));

    //DebugLn(['Read  TID ', i, ' ID ', ID, ' subid ',  sub_id]);
  end;
end;

procedure TMapReaderH3m.MaybeReadArtifactsOfHero(obj: THeroArtifacts);
begin
  if FSrc.ReadBoolean then
  begin
    ReadArtifactsOfHero(obj);
  end;
end;

procedure TMapReaderH3m.ReadDisposedHeros;
var
  players_raw: Byte;
  hero_count: Byte;
  id: Byte;
  i: Integer;
  defintion: THeroDefinition;
  bit: Integer;
  players: TPlayers;
begin

  //ignoring all but availability

  if IsAtLeastSoD() then
  begin
    hero_count := FSrc.ReadByte;

    for i := 0 to hero_count - 1 do
    begin
      id := FSrc.ReadByte;
      FSrc.Skip(1);//portr
      FSrc.SkipString;//name

      players_raw := FSrc.ReadByte;

      if players_raw <> $FF then
      begin
        players := [];
        defintion := FMap.PredefinedHeroes.EnsureItem(FMapEnv.lm.HeroIndexToString(id));

        for bit := 0 to 8 - 1 do
        begin
          if (players_raw and (1 << bit)) > 0 then
            players += [TPlayerColor(bit)];
        end;
        defintion.AvailableFor := players;
      end;
    end;
  end;

  FSrc.Skip(31);
end;

procedure TMapReaderH3m.ReadEvents;
begin
  //TODO: read events
end;

procedure TMapReaderH3m.VisitGarrison(AOptions: TGarrisonOptions);
begin
  ReadOwner(AOptions,TOwnerSize.size1);
  FSrc.Skip(3);
  ReadCreatureSet(AOptions.Army,7);
  if IsAtLeastAB() then
  begin
    AOptions.RemovableUnits := FSrc.ReadBoolean;
  end
  else
  begin
    AOptions.RemovableUnits := True;
  end;

  FSrc.Skip(8);//junk
end;

procedure TMapReaderH3m.VisitGrail(AOptions: TGrailOptions);
begin
  AOptions.Radius := FSrc.ReadDWord;
end;

procedure TMapReaderH3m.ReadHero(AOptions: THeroOptions);
var
  exper: UInt64;
  patrol: Byte;
begin
  with FSrc do
  begin
    ReadQuestIdentifier;

    ReadOwner(AOptions, TOwnerSize.size1);

    AOptions.&type:=ReadID(@FMapEnv.lm.HeroIndexToString,1);

    if ReadBoolean then
    begin
      AOptions.Name := ReadLocalizedString;
    end;

    exper := 0;
    if IsAtLeastSoD() then
    begin
      if ReadBoolean then
      begin
        exper := ReadDWord;
      end;
    end
    else
    begin
      exper := ReadDWord;
    end;

    AOptions.Experience:=exper;

    if ReadBoolean then
    begin
      AOptions.Portrait:=ReadID(@FMapEnv.lm.HeroIndexToString,1);
    end;

    MaybeReadSecondarySkills(AOptions.SecondarySkills);

    if ReadBoolean then
    begin
      ReadCreatureSet(AOptions.Army,7);
    end;

    AOptions.TightFormation  := ReadBoolean;
    ReadArtifactsOfHero(AOptions.Artifacts);

    patrol := ReadByte;
    if patrol = $FF then
    begin
      AOptions.PatrolRadius := -1;
    end
    else begin
      AOptions.PatrolRadius := patrol;
    end;

    AOptions.Sex := THeroSex.default;

    if IsAtLeastAB() then
    begin
      if ReadBoolean then
      begin
        AOptions.Biography := ReadLocalizedString;
      end;
      AOptions.Sex  := THeroSex(ReadByte);
    end;

    if IsAtLeastSoD() then
    begin
      if ReadBoolean then
      begin
        ReadBitmask(AOptions.SpellBook, 9, SPELL_QUANTITY_ACTUAL,@FMapEnv.lm.SpellIndexToString,False);
      end;
    end
    else if IsAtLeastAB() then
    begin
      AOptions.SpellBook.Add(ReadID(@FMapEnv.lm.SpellIndexToString,1));
    end;

    if IsAtLeastSoD() then
    begin
      if ReadBoolean then
        ReadPrimarySkills(AOptions.PrimarySkills);
    end;

    skip(16); //junk
  end;
end;

procedure TMapReaderH3m.VisitHeroPlaceholder(AOptions: THeroPlaceholderOptions);
var
  htid: Byte;
begin
  with FSrc do
  begin
    ReadOwner(AOptions,TOwnerSize.size1);
    htid := ReadByte;
    if htid = $ff then
    begin
      AOptions.Power := ReadByte;
      AOptions.&Type := '';
    end
    else
    begin
      AOptions.Power := 0;
      AOptions.&Type := FMapEnv.lm.HeroIndexToString(htid);
    end;
  end;
end;

procedure TMapReaderH3m.VisitLocalEvent(AOptions: TLocalEventOptions);
var
  PlayerMask: Byte;
  Player: TPlayerColor;
begin
  VisitPandorasBox(AOptions);

  with FSrc do
  begin
    PlayerMask := ReadByte;
    AOptions.AvailableFor:=[];
    for Player in TPlayerColor do
    begin
      if (PlayerMask and (1 << Byte(Player))) > 0 then
      begin
        AOptions.AvailableFor := AOptions.AvailableFor + [Player];
      end;
    end;

    AOptions.HumanActivable := true;
    AOptions.AIActivable:=ReadBoolean;
    AOptions.RemoveAfterVisit:=ReadBoolean;

    skip(4); //junk
  end;
end;

procedure TMapReaderH3m.VisitPrison(AOptions: TPrisonOptions);
begin
  ReadHero(AOptions);
end;

procedure TMapReaderH3m.VisitNormalHero(AOptions: TNormalHeroOptions);
begin
  ReadHero(AOptions);
end;

procedure TMapReaderH3m.VisitRandomHero(AOptions: TRandomHeroOptions);
begin
  ReadHero(AOptions);
end;

procedure TMapReaderH3m.VisitMonster(AOptions: TCreatureOptions);
var
  HasReward: Boolean;
begin
  with FSrc do
  begin
    ReadQuestIdentifier;

    AOptions.Amount := ReadWord;
    AOptions.Character := TCreatureCharacter(ReadByte);

    HasReward:=ReadBoolean;

    if HasReward then
    begin
      AOptions.RewardMessage := ReadLocalizedString;
      ReadResources(AOptions.RewardResources);

      AOptions.RewardArtifact := ReadID(@FMapEnv.lm.ArtifactIndexToString, ifthen(IsAtLeastAB(),2,1));
    end;
    AOptions.NeverFlees := ReadBoolean;
    AOptions.NoGrowing := ReadBoolean;

    Skip(2);//junk
  end;
end;

procedure TMapReaderH3m.ReadObjects;
var
  cnt: Integer;
  i: Integer;
  x,y,l: Byte;
  tid: DWord;
  //spos: Int32;

  template: TLegacyMapObjectTemplate;
begin
  cnt := FSrc.ReadDWord;

  for i := 0 to cnt - 1 do
  begin
    try
      //spos := FSrc.GetPos;
      FCurrentObject := TMapObject(FMap.Objects.Add);
      x:=FSrc.ReadByte;
      y:=FSrc.ReadByte;
      l:= FSrc.ReadByte;
      tid := FSrc.ReadDWord;
      FCurrentObject.X :=x;
      FCurrentObject.Y :=y;
      FCurrentObject.L :=l;

      template := FTemplates[tid];

      FCurrentObject.&Type:=template.&type;
      FCurrentObject.Subtype:=template.subtype;

      FCurrentObject.Template.Animation:=template.Animation;
      //todo: separate legacy editor animations OBJECTS vs EOBJCTS
      FCurrentObject.Template.EditorAnimation := template.Animation;

      FCurrentObject.Template.Mask.Assign(template.Mask);

      FCurrentObject.Template.VisitableFrom.Assign(template.VisitableFrom);
      FCurrentObject.Template.ZIndex:=template.ZIndex;

      //DebugLn(['Reading ', x,' ' , y, ' ', l, ' TID ', tid, ' ID ', FCurrentObject.GetID, ' subid ',  FCurrentObject.GetSubId]);

      FSrc.Skip(5); //junk

      FCurrentObject.Options.ApplyVisitor(Self);
    except
      on e:Exception do
      begin
        raise;
      end;
    end;
  end;
end;

procedure TMapReaderH3m.ReadObjMask(obj: TLegacyMapObjectTemplate);
type
   TFlag = (None=0, Visible, Block, Active);
const
  FLAG_CHARS: array [TFlag] of char = (MASK_NOT_VISIBLE, MASK_VISIBLE, MASK_BLOCKED, MASK_ACTIVABLE);

var
  mask_flags: array[0..5,0..7] of TFlag;
  b: Byte;
  i: Integer;
  j: Byte;
  s: String;

  msk: TMaskResource;

  mask_width, mask_height: Integer;
begin
  mask_width := 8;
  mask_height:= 6;

  msk := TMaskResource.Create(obj.Animation);
  try
    if FMapEnv.lm.ResourceLoader.TryLoadResource(msk, msk.Typ, msk.Path) then
    begin
      mask_width := Max(1, Min(msk.Width, 8));
      mask_height := Max(1, Min(msk.Height, 6));
    end;
  finally
    msk.Free;
  end;

  FillChar(mask_flags,SizeOf(mask_flags),#0);

  for i := 5 downto 6 - mask_height do
  begin
    for j := 7 downto 8 - mask_width do
    begin
      mask_flags[i,j] := TFlag.Visible;
    end;
  end;

  for i := Low(mask_flags) to High(mask_flags) do
  begin
    b := FSrc.ReadByte;
    for j := Low(mask_flags[i]) to High(mask_flags[i]) do
    begin
      if ((b shr j) and 1 ) =0 then
        mask_flags[i,j] := TFlag.Block;
    end;
  end;

  for i := Low(mask_flags) to High(mask_flags) do
  begin
    b := FSrc.ReadByte;
    for j := Low(mask_flags[i]) to High(mask_flags[i]) do
    begin
      if ((b shr j) and 1 ) = 1 then
        mask_flags[i,j] := TFlag.Active;
    end;
  end;

  for i := High(mask_flags) downto Low(mask_flags) do
  begin
    s := StringOfChar(MASK_NOT_VISIBLE, 8);
    for j := High(mask_flags[i]) downto Low(mask_flags[i]) do
    begin
      s[j+1] := FLAG_CHARS[mask_flags[i,j]];
    end;
    obj.Mask.Insert(0,s);
  end;
end;

procedure TMapReaderH3m.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  ReadOwner(AOptions);
end;

procedure TMapReaderH3m.ReadOwner(AOptions: TObjectOptions; size: TOwnerSize);
var
  tmp: Byte;
begin
  tmp := FSrc.ReadByte;
  case size of
    TOwnerSize.size4:FSrc.Skip(3);
  end;

  Assert(IsPublishedProp(AOptions, 'Owner'), 'Attempt to read owner of not ownable object');

  if tmp = 255 then
  begin
    AOptions.Owner := TPlayer.NONE;
  end
  else begin
    AOptions.Owner := TPlayer(tmp);
  end;


end;

procedure TMapReaderH3m.VisitPandorasBox(AOptions: TPandorasOptions);
var
  i: Integer;
  cnt: Integer;
  artid: Word;
  spell: TCustomID;
begin
  with FSrc do
  begin
    MayBeReadGuardsWithMessage(AOptions);

    AOptions.Experience := ReadDWord;
    AOptions.Mana:=ReadInt32;
    AOptions.Morale := Shortint(ReadByte);
    AOptions.Luck := Shortint(ReadByte);

    ReadResources(AOptions.Resources);

    ReadPrimarySkills(AOptions.PrimarySkills);

    ReadSecondarySkills1(AOptions.SecondarySkills);

    cnt := ReadByte;
    for i := 0 to cnt - 1 do
    begin
      if IsAtLeastAB() then
      begin
        artid := ReadWord;
      end
      else begin
        artid := ReadByte;
      end;

      AOptions.Artifacts.Add(FMapEnv.lm.ArtifactIndexToString(artid));
    end;

    cnt := ReadByte;

    for i := 0 to cnt - 1 do
    begin
      spell := ReadByte;
      AOptions.Spells.Add(FMapEnv.lm.SpellIndexToString(spell));
    end;

    cnt := ReadByte;

    ReadCreatureSet(AOptions.Creatures,cnt);

    Skip(8);
  end;
end;

procedure TMapReaderH3m.ReadPlayerInfo(Attr: TPlayerInfo);
var
  faction_mask_size: integer;
  faction_count: Integer;
  heroes_count: DWord;
  h: Integer;
  Main_Hero: TCustomID;
  AllowedFactionsSet: Boolean;
  HasMainTown: Boolean;

  CanHumanPlay: Boolean;
  CanComputerPlay: Boolean;
  request: TPositionResolveRequest;
begin
  CanHumanPlay := FSrc.ReadBoolean;
  CanComputerPlay := FSrc.ReadBoolean;

  if not (CanComputerPlay or CanHumanPlay) then
  begin
    if IsAtLeastSoD() then
    begin
      FSrc.Skip(13);
    end
    else begin
      case FMapVersion of
        MAP_VERSION_AB: FSrc.Skip(12);
        MAP_VERSION_ROE: FSrc.Skip(6);
      else
        Assert(false);
      end;
    end;
    Attr.CanPlay := TPlayableBy.None;
    Exit;
  end;

  Attr.CanPlay := TPlayableBy.PlayerOrAI;

  if not CanHumanPlay then
    Attr.CanPlay := TPlayableBy.AIOnly
  else if not CanComputerPlay then
    Attr.CanPlay := TPlayableBy.PlayerOnly;

  Attr.AITactics := TAITactics(FSrc.ReadByte);


  if IsAtLeastSoD() then
  begin
    AllowedFactionsSet := FSrc.ReadBoolean;
  end
  else
  begin
    AllowedFactionsSet := True;
  end;

  case FMapVersion of
    MAP_VERSION_ROE:
    begin
      faction_count := TOTAL_FACTIONS_ROE;
      faction_mask_size := 1;
    end;
    else
    begin
     faction_count := TOTAL_FACTIONS;
     faction_mask_size := 2;
    end;
  end;

  ReadBitmask(Attr.AllowedFactions,faction_mask_size,faction_count,@FMap.ListsManager.FactionIndexToString, False);

  if not AllowedFactionsSet then
  begin
    Attr.AllowedFactions.Clear;
  end;

  FSrc.Skip(1);//RandomFaction

  HasMainTown := FSrc.ReadBoolean;
  if HasMainTown then
  begin
    if IsAtLeastAB() then
    begin
      Attr.MainTown.GenerateHero := FSrc.ReadBoolean;
      SkipNotImpl(1); //todo: in VCMI it is GenerateHero
    end
    else
    begin
      Attr.MainTown.GenerateHero  := True;
    end;

    request := TPositionResolveRequest.Create;
    request.AllowedTypes.Add(TYPE_TOWN);
    request.AllowedTypes.Add(TYPE_RANDOMTOWN);
    request.ResolveProc := @Attr.MainTown.SetMapObject;

    ReadPosition(request.Position);

    FPositionsToResolve.Add(request);
  end; //main town
  with Attr,FSrc do
  begin
    Skip(1);// RandomHero
    Main_Hero := ReadIDByte;

    if Main_Hero <> ID_RANDOM then
    begin
      MainHero:=FMapEnv.lm.HeroIndexToString(Main_Hero);

      Skip(1); // MainHeroPortrait  unused
      SkipString; // MainHeroName unused
    end;
  end;

  //plased heroes ignored
  if IsAtLeastAB() then
  begin
    FSrc.Skip(1); //unknown byte
    heroes_count := FSrc.ReadDWord;

    for h := 0 to heroes_count - 1 do
    begin
      FSrc.Skip(1); //type
      FSrc.SkipString();//Name
    end;
  end;
end;

procedure TMapReaderH3m.ReadPlayerInfos(Attrs: TPlayerInfos);
var
  player_color: TPlayerColor;
begin
  for player_color in TPlayerColor do
  begin
    ReadPlayerInfo(Attrs.GetPlayerInfo(Integer(player_color)));
  end;
end;

procedure TMapReaderH3m.ReadPredefinedHeroes;
var
  custom: Boolean;
  i: Integer;

  definition:  THeroDefinition;
begin
  if not IsAtLeastSoD() then
    Exit;

  for i := 0 to HERO_QUANTITY - 1 do
  begin
    custom := FSrc.ReadBoolean;
    if not custom then Continue;

    definition := FMap.PredefinedHeroes.Add;
    definition.Identifier := FMapEnv.lm.HeroIndexToString(i);

    if FSrc.ReadBoolean then
    begin
      definition.experience := FSrc.ReadDWord;
    end;

    MaybeReadSecondarySkills(definition.Skills);

    MaybeReadArtifactsOfHero(definition.Artifacts);

    if FSrc.ReadBoolean then
    begin
      definition.Biography :=  FSrc.ReadLocalizedString;
    end;

    definition.Sex := THeroSex(FSrc.ReadByte);

    if FSrc.ReadBoolean then
    begin
      ReadBitmask(definition.SpellBook, 9, SPELL_QUANTITY_ACTUAL, @FMapEnv.lm.SpellIndexToString, false);
    end;

    if FSrc.ReadBoolean then
    begin
      ReadPrimarySkills(definition.PrimarySkills);
    end;
  end;
end;

function TMapReaderH3m.ReadQuest(obj: TQuest): TQuestMission;
var
  limit: DWord;
begin
  obj.Clear;
  Result := TQuestMission(FSrc.ReadByte);

  obj.MissionType:=Result;

  with FSrc do
  begin
    case obj.MissionType of
      TQuestMission.None: Exit;
      TQuestMission.PrimaryStat:begin
        ReadPrimarySkills(obj.PrimarySkills);
      end;
      TQuestMission.Level:begin
        obj.HeroLevel:=ReadDWord;
      end;
      TQuestMission.KillHero,TQuestMission.KillCreature: begin
        PushQIResolveRequest(ReadDWord, @obj.SetKillTarget);
      end;
      TQuestMission.Artifact: begin
        ReadArtifactSet(obj.Artifacts);
      end;
      TQuestMission.Army: begin
        ReadCreatureSet(obj.Army);
      end;
      TQuestMission.Resources: begin
        ReadResources(obj.Resources);
      end;
      TQuestMission.Hero: begin
        obj.Hero:=ReadID(@FMapEnv.lm.HeroIndexToString, 1);
      end;
      TQuestMission.Player: begin
        obj.Player:=TPlayer(ReadByte);
      end;
    end;


    limit := ReadDWord;

    if limit = $ffffffff then
    begin
      obj.TimeLimit:=-1;
    end
    else begin
      obj.TimeLimit:=limit;
    end;

    obj.FirstVisitText := ReadLocalizedString;
    obj.NextVisitText := ReadLocalizedString;
    obj.CompletedText := ReadLocalizedString;
  end;
end;

procedure TMapReaderH3m.VisitQuestGuard(AOptions: TQuestGuardOptions);
begin
  ReadQuest(AOptions.Quest);
end;

procedure TMapReaderH3m.VisitRandomDwelling(AOptions: TRandomDwellingOptions);
var
  ident: DWord;
  linked: Boolean;
begin
  ReadOwner(AOptions);

  ident := FSrc.ReadDWord;
  Linked:=ident <> 0;
  if not Linked then
  begin
    ReadBitmask(AOptions.AllowedFactions,2,9,@FMap.ListsManager.FactionIndexToString, False);
  end
  else
  begin
    PushQIResolveRequest(ident, @AOptions.SetSameAsTown);
  end;

  AOptions.MinLevel := FSrc.ReadByte;
  AOptions.MaxLevel := FSrc.ReadByte;

end;

procedure TMapReaderH3m.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
var
  ident: DWord;
  linked: Boolean;
begin
  ReadOwner(AOptions);

  ident := FSrc.ReadDWord;

  Linked:=ident <> 0;

  if not Linked then
  begin
    ReadBitmask(AOptions.AllowedFactions,2,9,@FMap.ListsManager.FactionIndexToString, False);
  end
  else
  begin
    PushQIResolveRequest(ident, @AOptions.SetSameAsTown);
  end;
end;

procedure TMapReaderH3m.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  ReadOwner(AOptions);

  AOptions.MinLevel := FSrc.ReadByte;
  AOptions.MaxLevel := FSrc.ReadByte;
end;

procedure TMapReaderH3m.VisitResource(AOptions: TResourceOptions);
begin
  MayBeReadGuardsWithMessage(AOptions);
  AOptions.Amount := FSrc.ReadDWord;

  if AOptions.MapObject.GetSubId = 'gold' then
  begin
    AOptions.Amount := AOptions.Amount * 100;
  end;

  FSrc.Skip(4);//junk
end;

procedure TMapReaderH3m.ReadRumors;
var
  cnt: DWord;
  i: Integer;
  r: TRumor;
begin
  cnt := FSrc.ReadDWord;
  for i := 0 to cnt - 1 do
  begin
    r := FMap.Rumors.Add;

    r.Name := FSrc.ReadLocalizedString;
    r.Text := FSrc.ReadLocalizedString;
  end;
end;

procedure TMapReaderH3m.VisitScholar(AOptions: TScholarOptions);
var
  b: Byte;
  typ:TScholarBonus;
begin
  b := FSrc.ReadByte;

  if b=$FF then
  begin
     //leave as default
     FSrc.Skip(1);
  end
  else begin
    typ := TScholarBonus(b);

    case typ of
      TScholarBonus.primSkill:  AOptions.RewardPrimSkill := PRIMARY_SKILL_NAMES[TPrimarySkill(FSrc.ReadByte)];
      TScholarBonus.skill: AOptions.RewardSkill := ReadID(@FMapEnv.lm.SkillNidToString,1);
      TScholarBonus.spell: AOptions.RewardSpell := ReadID(@FMapEnv.lm.SpellIndexToString,1);
    else
      raise Exception.Create('Invalid Scholar bonus type');
    end;
  end;

  FSrc.Skip(6); //junk
end;

procedure TMapReaderH3m.VisitSeerHut(AOptions: TSeerHutOptions);
var
  mis_type: TQuestMission;
  aid: Byte;
  tmp: DWord;
begin
  with FSrc, AOptions do begin
    if IsAtLeastAB() then
    begin
      mis_type := ReadQuest(Quest);
    end
    else begin
      aid := FSrc.ReadByte;
      if aid <>255 then
      begin
        mis_type := TQuestMission.Artifact;
      end else begin
        mis_type := TQuestMission.None;
      end;
    end;

    if mis_type<> TQuestMission.None then
    begin
      RewardType := TSeerHutReward(ReadByte);
      case RewardType of
        TSeerHutReward.experience, TSeerHutReward.manaPoints:
        begin
          AOptions.RewardValue:=ReadInt32;
        end;
        TSeerHutReward.moraleBonus, TSeerHutReward.luckBonus:
        begin
          AOptions.RewardValue:=ReadByte;
        end;

        TSeerHutReward.RESOURCES:
        begin
          Reward := RESOURCE_NAMES[TResType(ReadByte)];

          tmp := ReadDWord;
          tmp := tmp and $00FFFFFF;
          AOptions.RewardValue := tmp
        end;
        TSeerHutReward.primarySkill:
        begin
          Reward := PRIMARY_SKILL_NAMES[TPrimarySkill(ReadByte)];
          RewardValue:=ReadByte;

        end;
        TSeerHutReward.secondarySkill:
        begin
          Reward := SECONDARY_SKILL_NAMES[ReadByte];
          RewardValue:=ReadByte;
        end;
        TSeerHutReward.artifact:
        begin
          if IsAtLeastAB() then
          begin
            tmp := ReadWord;
          end
          else begin
            tmp := ReadByte;
          end;
          Reward:=FMapEnv.lm.ArtifactIndexToString(tmp);
        end;
        TSeerHutReward.spell:
        begin
          Reward:=FMapEnv.lm.SpellIndexToString(ReadByte);
        end;
        TSeerHutReward.creature:
        begin
          if IsAtLeastAB() then
          begin
            tmp := ReadWord;
          end
          else begin
            tmp := ReadByte;
          end;
          Reward:=FMapEnv.lm.CreatureIndexToString(tmp);
          RewardValue:=ReadWord;
        end;
      end;
      skip(2);//junk
    end
    else
      skip(3); //junk
  end;

end;

procedure TMapReaderH3m.VisitShrine(AOptions: TShrineOptions);
var
  raw_id: Byte;
begin
  raw_id := FSrc.ReadByte;

  if raw_id = 255 then
  begin
    AOptions.Spell := '';
  end
  else begin
    AOptions.Spell := FMapEnv.lm.SpellIndexToString(raw_id);
  end;

  fsrc.skip(3); //junk
end;

procedure TMapReaderH3m.VisitSignBottle(AOptions: TSignBottleOptions);
begin
  AOptions.Text := FSrc.ReadLocalizedString;
  FSrc.Skip(4);//junk
end;

procedure TMapReaderH3m.VisitSpellScroll(AOptions: TSpellScrollOptions);
var
  nid : TCustomID;
  sid: AnsiString;
begin
  VisitArtifact(AOptions);

  nid := FSrc.ReadDWord;
  sid := FMapEnv.lm.SpellIndexToString(nid);
  AOptions.Spell := sid;
end;

procedure TMapReaderH3m.ReadSVLC;
type
   TVictoryCondition = ( ARTIFACT, GATHERTROOP, GATHERRESOURCE, BUILDCITY, BUILDGRAIL, BEATHERO,
		CAPTURECITY, BEATMONSTER, TAKEDWELLINGS, TAKEMINES, TRANSPORTITEM, WINSTANDARD = 255);
   TLossCondition = (LOSSCASTLE, LOSSHERO, TIMEEXPIRES, LOSSSTANDARD = 255);

var
  special_victory_condition:TLogicalEventConditionItem;

  procedure SetHaveCondition(ObjType, ObjSubtype: AnsiString; Amount: Int32);
  begin
    special_victory_condition.&type:=ObjType;
    special_victory_condition.subType:=ObjSubtype;
    special_victory_condition.Value := Amount;
    special_victory_condition.ConditionType:=TWinLossCondition.have_0;
  end;

var
  VictoryConditionType: TVictoryCondition;
  LossConditionType: TLossCondition;
  obj_id: TCustomID;
  value: DWord;
  allow_normal_victory: Boolean;
  applies_to_ai: Boolean;

  obj_subtype: AnsiString;

  special_victory, special_defeat: TTriggeredEvent;
  special_defeat_condition:TLogicalEventConditionItem;
  position: TPosition;
  building_1: Byte;
  building_2: Byte;
  child_condition: TLogicalEventConditionItem;

begin
  position := TPosition.Create;
  FMap.TriggeredEvents.Clear;

  VictoryConditionType := TVictoryCondition(FSrc.ReadByte);

  if VictoryConditionType = TVictoryCondition.WINSTANDARD then
  begin
    allow_normal_victory := true;
    FMap.VictoryIconIndex := 11;
    FMap.VictoryString:=FMapEnv.i18n.VictoryTexts[0,0];
  end
  else
  begin
    FMap.VictoryIconIndex := Integer(VictoryConditionType);
    FMap.VictoryString := FMapEnv.i18n.VictoryTexts.Value[0,SizeUInt(VictoryConditionType)+1];

    allow_normal_victory := FSrc.ReadBoolean;
    applies_to_ai := Fsrc.ReadBoolean;

    special_victory := FMap.TriggeredEvents.Add;
    special_victory.Effect.&type:='victory';
    special_victory.Identifier := 'specialVictory';

    special_victory_condition := special_victory.AddCondition;

    if not applies_to_ai then
    begin
      with special_victory_condition.SubExpressions.Add as TLogicalEventConditionItem do
      begin
        ConditionType:=TWinLossCondition.isHuman;
        Value := 1;
      end;

      special_victory_condition.LogicalOperator:=TLogicalOperator.allOf;

      special_victory_condition := special_victory_condition.SubExpressions.Add as TLogicalEventConditionItem;
    end;

    case VictoryConditionType of
      TVictoryCondition.ARTIFACT:
      begin
        obj_id := FSrc.ReadByte;

        obj_subtype := FMapEnv.lm.ArtifactIndexToString(obj_id);

        SetHaveCondition(TYPE_ARTIFACT, obj_subtype, 0);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,281];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,280];

        if IsAtLeastAB() then
          FSrc.Skip(1);
      end;
      TVictoryCondition.GATHERTROOP:
      begin
        obj_id := FSrc.ReadByte;

        obj_subtype := FMapEnv.lm.CreatureIndexToString(obj_id);

        if IsAtLeastAB() then
          FSrc.Skip(1);

        value := FSrc.ReadDWord;

        SetHaveCondition(TYPE_MONSTER, obj_subtype, value);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,277];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,276];
      end;
      TVictoryCondition.GATHERRESOURCE:
      begin
        obj_id := FSrc.ReadByte;
        obj_subtype := RESOURCE_NAMES[TResType(obj_id)];
        value := FSrc.ReadDWord;

        SetHaveCondition(TYPE_RESOURCE, obj_subtype, value);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,279];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,278];
      end;
      TVictoryCondition.BUILDCITY:
      begin
        ReadPosition(Position);
        building_1 := 10 + FSrc.ReadByte;
        building_2 := 7 + FSrc.ReadByte;

        special_victory_condition.LogicalOperator:=TLogicalOperator.allOf;

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.haveBuilding_0;
        child_condition.&type := FMapEnv.lm.BuildingIdToString(building_1);
        child_condition.Position.Assign(position);

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.haveBuilding_0;
        child_condition.&type := FMapEnv.lm.BuildingIdToString(building_2);
        child_condition.Position.Assign(position);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,283];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,282];
      end;
      TVictoryCondition.BUILDGRAIL:
      begin
        ReadPosition(special_victory_condition.Position);
        special_victory_condition.ConditionType:=TWinLossCondition.haveBuilding_0;
        special_victory_condition.&type:='grail';

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,285];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,284];
      end;
      TVictoryCondition.BEATHERO:
      begin
        special_victory_condition.ConditionType:=TWinLossCondition.destroy_0;
        special_victory_condition.&type:=TYPE_HERO;

        ReadPosition(special_victory_condition.Position);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,253];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,252];
      end;
      TVictoryCondition.CAPTURECITY:
      begin
        special_victory_condition.ConditionType:=TWinLossCondition.have_0;
        special_victory_condition.&type:=TYPE_TOWN;
        ReadPosition(special_victory_condition.Position);
        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,250];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,249];
      end;
      TVictoryCondition.BEATMONSTER:
      begin
        special_victory_condition.ConditionType:=TWinLossCondition.destroy_0;
        special_victory_condition.&type:=TYPE_MONSTER;

        ReadPosition(special_victory_condition.Position);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,287];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,286];
      end;
      TVictoryCondition.TAKEDWELLINGS:
      begin
        special_victory_condition.LogicalOperator:=TLogicalOperator.allOf;

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.&type:='creatureGeneratorCommon';

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.&type:='creatureGeneratorSpecial';

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,289];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,288];
      end;
      TVictoryCondition.TAKEMINES:
      begin
        special_victory_condition.LogicalOperator:=TLogicalOperator.allOf;

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.&type:='mine';

        child_condition := special_victory_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.&type:='abandonedMine';

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,291];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,290];
      end;
      TVictoryCondition.TRANSPORTITEM:
      begin
        special_victory_condition.ConditionType:=TWinLossCondition.have_0;
        special_victory_condition.&type:=TYPE_ARTIFACT;
        special_victory_condition.subType:=ReadID1(@FMapEnv.lm.ArtifactIndexToString);
        special_victory_condition.Value:=1;
        ReadPosition(special_victory_condition.Position);

        special_victory.Effect.MessageToSend:=FMapEnv.i18n.GeneralTexts[0,293];
        special_victory.Message:=FMapEnv.i18n.GeneralTexts[0,292];
      end;
      else
        raise Exception.CreateFmt('Invalid victory condition %d',[Integer(VictoryConditionType)]);
    end;
  end;

  if allow_normal_victory then
    FMap.TriggeredEvents.AddStandardVictory();

  FMap.TriggeredEvents.AddStandardDefeat();

  LossConditionType := TLossCondition(FSrc.ReadByte);

  if (LossConditionType = TLossCondition.LOSSSTANDARD) then
  begin
    FMap.DefeatIconIndex := 3;
    FMap.DefeatString := FMapEnv.i18n.LossTexts[0,0];
  end
  else
  begin
    FMap.DefeatIconIndex := Integer(LossConditionType);
    FMap.DefeatString := FMapEnv.i18n.LossTexts.Value[0,SizeUInt(LossConditionType)+1];

    special_defeat := FMap.TriggeredEvents.Add;
    special_defeat.Identifier:='specialDefeat';
    special_defeat.Effect.&type:='defeat';
    special_defeat.Effect.MessageToSend := FMapEnv.i18n.GeneralTexts[0,5];

    special_defeat_condition := special_defeat.AddCondition;

    case LossConditionType of
      TLossCondition.LOSSCASTLE:
      begin
        ReadPosition(position);
        special_defeat_condition.LogicalOperator:=TLogicalOperator.noneOf;

        child_condition := special_defeat_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.Position.Assign(position);
        child_condition.&type:='hero';

        special_defeat.Message:=FMapEnv.i18n.GeneralTexts[0,251];
      end;
      TLossCondition.LOSSHERO:
      begin
        ReadPosition(position);

        child_condition := special_defeat_condition.AddSubCondition;
        child_condition.ConditionType:=TWinLossCondition.have_0;
        child_condition.Position.Assign(position);
        child_condition.&type:='town';

        special_defeat.Message:=FMapEnv.i18n.GeneralTexts[0,253];
      end;
      TLossCondition.TIMEEXPIRES:
      begin
        special_defeat_condition.ConditionType:=TWinLossCondition.daysPassed;
        special_defeat_condition.Value:=FSrc.ReadWord;
        special_defeat.Message:=FMapEnv.i18n.GeneralTexts[0,254];
      end;
    end;

  end;
  position.Free;
end;

procedure TMapReaderH3m.ReadTeams;
var
  team_count: Byte;
  player: TPlayerColor;
  team: Byte;
  i: Integer;
begin
  team_count := FSrc.ReadByte;

  FMap.Teams.Clear;
  for i := 0 to team_count - 1 do
  begin
    FMap.Teams.Add;
  end;

  if team_count > 0 then
  begin
    for player in TPlayerColor do
    begin
      team := FSrc.ReadByte;
      FMap.Teams.Items[team].Include(player);
    end;
  end;
end;

procedure TMapReaderH3m.ReadTerrain;
  procedure ReadLevel(Level:Integer);
  var
    tile: PMapTile;
    x: Integer;
    y: Integer;
  begin
    for y := 0 to FMap.CurrentLevel.Height - 1 do
    begin
      for x := 0 to FMap.CurrentLevel.Width - 1 do
      begin
        tile := FMap.GetTile(Level,x,y);

        FSrc.ReadBuffer(tile^, 7); //7 bytes excluding owner
      end;
    end;
  end;
var
  i: Integer;
begin
  for i := 0 to FMap.MapLevels.Count - 1 do
  begin
    ReadLevel(i);
  end;
end;

procedure TMapReaderH3m.VisitTown(AOptions: TTownOptions);
var
  cnt: DWord;
  i: Integer;
  all_of, none_of: TCustomIDArray;
  faction: TFactionInfo;
  b, faction_index: TCustomID;
begin

  if AOptions.MapObject.GetID = TYPE_TOWN then
  begin
    faction := FMapEnv.lm.GetFaction(AOptions.MapObject.GetSubId);
    faction_index := faction.Index;
  end
  else
  begin
    faction_index := -1;
  end;

  with FSrc do
  begin
    ReadQuestIdentifier;
    ReadOwner(AOptions,TOwnerSize.size1);

    if ReadBoolean then//name
    begin
      AOptions.Name := ReadLocalizedString;
    end;

    if ReadBoolean then
    begin
      ReadCreatureSet(AOptions.Army,7);
    end;

    AOptions.TightFormation := ReadBoolean;

    if ReadBoolean then //custom buildings
    begin
      ReadBitmask(all_of, 6, 48, false);
      ReadBitmask(none_of, 6, 48, false);

      AOptions.Buildings.Clear;

      for b in all_of do
      begin
        AOptions.Buildings.AllOf.Add(FMapEnv.lm.BuildingIndexToString(faction_index, b));
      end;

      for b in none_of do
      begin
        AOptions.Buildings.NoneOf.Add(FMapEnv.lm.BuildingIndexToString(faction_index, b));
      end;
    end
    else
    begin
      AOptions.HasFort:= ReadBoolean;
    end;

    if IsAtLeastAB() then
    begin
      ReadBitmask(AOptions.Spells.AllOf, 9, SPELL_QUANTITY_ACTUAL, @FMapEnv.lm.SpellIndexToString, false);
    end;
    ReadBitmask(AOptions.Spells.NoneOf, 9, SPELL_QUANTITY_ACTUAL, @FMapEnv.lm.SpellIndexToString, false);

    //castle events
    cnt := ReadDWord;
    for i := 0 to cnt - 1 do
    begin
      SkipString;
      SkipString;

      SkipNotImpl(28);

      SkipNotImpl(1);
      if IsAtLeastSoD() then
        SkipNotImpl(1);
      SkipNotImpl(1);

      SkipNotImpl(3);

      skip(17);//junk

      SkipNotImpl(6);
      SkipNotImpl(14);
      skip(4); //junk
    end;
    if IsAtLeastSoD() then
      SkipNotImpl(1); //alignment, connect faction to owner or particular player
    Skip(3); //junk
  end;
end;

procedure TMapReaderH3m.VisitWitchHut(AOptions: TWitchHutOptions);
begin
  if IsAtLeastAB() then
  begin
    AOptions.AllowedSkills.Clear;

    ReadBitmask(AOptions.AllowedSkills, 4, SECONDARY_SKILL_QUANTITY, @FMapEnv.lm.SkillNidToString, false);
  end
  else
  begin
    //all skill allowed
    //leave default
  end;
end;

procedure TMapReaderH3m.SkipNotImpl(count: Integer);
begin
  FSrc.Skip(count);
end;

procedure TMapReaderH3m.PushQIResolveRequest(AIdent: UInt32;
  AResolveProc: TQIResolveRequest.TResolveProc);
var
  request: TQIResolveRequest;
begin
  request := TQIResolveRequest.Create;
  request.ResolveProc := AResolveProc;
  request.Identifier:=AIdent;
  FQILinksToResolve.Add(request);
end;

procedure TMapReaderH3m.ResolveQuestIdentifiers;
var
  req: TQIResolveRequest;
  o: TMapObject;
begin
  while FQILinksToResolve.Count>0 do
  begin
    req := FQILinksToResolve[0];

    o := FQuestIdentifierMap.KeyData[req.Identifier];

    req.ResolveProc(o.Identifier);

    FQILinksToResolve.Delete(0);
  end;
end;

procedure TMapReaderH3m.ResolveTownPositions;
var
  req: TPositionResolveRequest;
  p: TPlayer;
  info: TPlayerInfo;
  i: Integer;
  t: TPlayerTown;
  succ: Boolean;
begin
  while FPositionsToResolve.Count>0 do
  begin
    succ := false;
    req := FPositionsToResolve[0];

    for p in TPlayerColor do
    begin
      info := FMap.Players.GetPlayerInfo(Integer(p));

      for i := 0 to info.Towns.Count - 1 do
      begin
        t := info.Towns[i];

        if req.CheckObject(t.MapObject) then
        begin
          succ := true;
          req.ResolveProc(t.MapObject);
        end;
      end;
    end;

    if not succ then
    begin
      raise Exception.Create('Request not resolved');
    end;

    FPositionsToResolve.Delete(0);
  end;

end;

procedure TMapReaderH3m.ResolveAll;
begin
  ResolveQuestIdentifiers;
  ResolveTownPositions;
end;

procedure TMapReaderH3m.VisitAbandonedMine(AOptions: TAbandonedOptions);
var
  res_mask: Byte;
  bit_idx: Integer;
begin
  AOptions.Clear;
  res_mask := FSrc.ReadByte;

  for bit_idx in [0..7] do
  begin
    if ((1 shl bit_idx) and res_mask) > 0 then
    begin
      AOptions.PossibleResources.Add(RESOURCE_NAMES[TResType(bit_idx)]);
    end;
  end;

  FSrc.Skip(3);
end;

procedure TMapReaderH3m.VisitMine(AOptions: TMineOptions);
begin
  VisitOwnedObject(AOptions);
end;


end.


