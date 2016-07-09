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
unit map_objects;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, math, fgl, typinfo, FileUtil, LazUTF8, fpjson, editor_types,
  filesystem_base, editor_graphics, editor_classes, h3_txt,
  lists_manager, vcmi_json, editor_gl, contnrs, gset, gvector, RegExpr;

type

  TLegacyTemplateId = UInt64;

  TDefBitmask = packed array[0..5] of uint8; //top to bottom, right to left as in H3M

  { TLegacyObjTemplate }

  TLegacyObjTemplate = class
  private
    FFilename: AnsiString;
    FPassability,
    FActions: TDefBitmask;
    FLandscape,FLandEditGroups: uint16;
    FTyp,FSubType: uint32;
    FGroup,FIsOverlay: uint8;
  public
    constructor Create;
    property Filename: AnsiString read FFilename;
    property Actions: TDefBitmask read FActions;
    property Passability: TDefBitmask read FPassability;
    property Landscape: uint16 read FLandscape;
    property LandEditGroups: uint16 read FLandEditGroups;
    property Typ: uint32 read FTyp;
    property SubType: uint32 read FSubType;
    property IsOverlay: uint8 read FIsOverlay;
  end;

  { TMaskResource }

  TMaskResource = class (TBaseResource, IResource)
  private
    FHeight: Byte;
    FMask1: TDefBitmask;
    FMask2: TDefBitmask;
    FWidth: Byte;
    procedure Clear;
    procedure SetHeight(AValue: Byte);
    procedure SetWidth(AValue: Byte);
  public
    constructor Create(APath: AnsiString);
    procedure LoadFromStream(AStream: TStream); override;
    property Width: Byte read FWidth write SetWidth;
    property Height: Byte read FHeight write SetHeight;
    property Mask1:TDefBitmask read FMask1;//todo: rename and use
    property Mask2:TDefBitmask read FMask2;
  end;

  TLegacyObjConfigList = specialize TFPGObjectList<TJSONObject>;
  TLegacyObjConfigFullIdMap = specialize TObjectMap<TLegacyTemplateId, TLegacyObjConfigList>;

  {$push}
  {$m+}

  TMapObjectType = class;
  TMapObjectGroup = class;

  { TMapObjectTemplate }

  TMapObjectTemplate = class (TNamedCollectionItem)
  private
    FDef: TDefAnimation;
    FMapObjectGroup:TMapObjectGroup;
    FMapObjectType: TMapObjectType;
  strict private
    FAllowedTerrains: TTerrainTypes;
    FAnimation: AnsiString;
    FEditorAnimation: AnsiString;
    FTags: TStrings;
    FVisitableFrom: TStrings;
    FMask: TStrings;
    FzIndex: Integer;
    procedure SetAnimation(AValue: AnsiString);
    procedure SetEditorAnimation(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Def: TDefAnimation read FDef;
    property MapObjectGroup: TMapObjectGroup read FMapObjectGroup;
    property MapObjectType: TMapObjectType read FMapObjectType;
    class function UseMeta: boolean; override;

    procedure RenderIcon(AState: TLocalState; AX, AY, dim:integer; color: TPlayer = TPlayer.none);
  published
    property Animation: AnsiString read FAnimation write SetAnimation;
    property EditorAnimation: AnsiString read FEditorAnimation write SetEditorAnimation;
    property VisitableFrom: TStrings read FVisitableFrom;
    property AllowedTerrains: TTerrainTypes read FAllowedTerrains write FAllowedTerrains default ALL_TERRAINS;
    property Mask: TStrings read FMask;
    property ZIndex: Integer read FzIndex write FzIndex default 0;
    property Tags: TStrings read FTags;
  end;

  { TMapObjectTemplates }

  TMapObjectTemplates = class (specialize TGNamedCollection<TMapObjectTemplate>)
  private
    FOwner: TMapObjectType;
  public
    constructor Create(AOwner: TMapObjectType);
    property MapObjectType: TMapObjectType read FOwner;
  end;

  TMapObjectTemplateList = specialize TFPGObjectList<TMapObjectTemplate>;

  { TMapObjectType }

  TMapObjectType = class (TNamedCollectionItem)
  private
    FFilters: TJSONObject;
    FName: TLocalizedString;
    FNid: TCustomID;
    FTemplates: TMapObjectTemplates;
    FMapObjectGroup:TMapObjectGroup;
    function GetIndexAsID: TCustomID;
    procedure SetIndexAsID(AValue: TCustomID);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property MapObjectGroup: TMapObjectGroup read FMapObjectGroup;
    class function UseMeta: boolean; override;
  published
    property Index: TCustomID read GetIndexAsID write SetIndexAsID default ID_INVALID;
    property Templates:TMapObjectTemplates read FTemplates;
    property Name: TLocalizedString read FName write FName;
    property Filters: TJSONObject read FFilters;
  end;

  { TMapObjectTypes }

  TMapObjectTypes = class (specialize TGNamedCollection<TMapObjectType>)
  private
    FOwner: TMapObjectGroup;
  public
    constructor Create(AOwner: TMapObjectGroup);
    property Owner: TMapObjectGroup read FOwner;
  end;

  { TMapObjectGroup }

  TMapObjectGroup = class (TNamedCollectionItem)
  private
    FHandler: AnsiString;
    FName: TLocalizedString;
    FNid: TCustomID;
    FMapObjectTypes: TMapObjectTypes;
    FIsHero, FIsHeroLike: Boolean;
  protected
    function GetDisplayName: string; override;
    procedure SetIdentifier(AValue: AnsiString); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    class function UseMeta: boolean; override;

    property IsHero: Boolean read FIsHero;
    property IsHeroLike: Boolean read FIsHeroLike;
  published
    property Index: TCustomID read FNid write FNid default ID_INVALID;
    property Types:TMapObjectTypes read FMapObjectTypes;
    property Name: TLocalizedString read FName write FName;
    property Handler: AnsiString read FHandler write FHandler;
  end;

  TMapObjectGroups = specialize TGNamedCollection<TMapObjectGroup>;

  {$pop}

  TLegacyIdMap = specialize TObjectMap<TLegacyTemplateId, TMapObjectType>;

  TObjectsManager = class;

  { TObjectsSelection }

  TObjectsSelection = class
  private
    FData: TMapObjectTemplateList;
    function GetCount: Integer;
    function GetObjcts(AIndex: Integer): TMapObjectTemplate;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    property Count:Integer read GetCount;
    property Objcts[AIndex: Integer]: TMapObjectTemplate read GetObjcts;
  end;

  { TObjTemplateCompare }

  TObjTemplateCompare = class
  public
    class function c(a,b: TMapObjectTemplate): boolean;
  end;

  { TSearchIndexBusket }

  TSearchIndexBusket = class
  public
    type
      TDataVector = specialize gvector.TVector<TMapObjectTemplate>;
      TBusketData = specialize gset.TSet<TMapObjectTemplate, TObjTemplateCompare>;
    var
      data: TBusketData;
    constructor Create();
    destructor Destroy; override;
    procedure AddItem(AItem: TMapObjectTemplate);
    procedure Intersect(ATarget:TBusketData);
    procedure SaveTo(ATarget:TBusketData);
  end;

  { TSearchIndexMap }

  TSearchIndexMap = class(specialize TFPGMap<string,TSearchIndexBusket>)
  protected
    procedure Deref(Item: Pointer); override;
  public
    constructor Create;

    //true if found
    function Find(AKeyWord: String; out IdxLow: integer; out IdxHigh: integer): boolean; overload;
  end;

  { TSearchIndex }

  TSearchIndex = class
  private
    FMap: TSearchIndexMap; //contains TSearchIndexBusket
  public
    constructor Create();
    destructor Destroy; override;
    procedure AddToIndex(AKeyWord: String; AItem: TMapObjectTemplate);
    procedure Find(AKeyWord: String; ATarget:TSearchIndexBusket.TBusketData);
    procedure Intersect(AKeyWord: String; ATarget:TSearchIndexBusket.TBusketData);
  end;

  { TObjectsManager }

  TObjectsManager = class (TGraphicsCosnumer)
  strict private
    FMapObjectGroups: TMapObjectGroups;
    FAllTemplates: TMapObjectTemplateList; //for use in index
    FSearchIndex: TSearchIndex;
    FLegacyObjTypes: TLegacyIdMap;
    FProgress: IProgressCallback;
    FTextTokenizer: TRegExpr;
    function TypToId(Typ,SubType: uint32):TLegacyTemplateId; inline;
    procedure LoadLegacy(AProgressCallback: IProgressCallback; AFullIdToDefMap: TLegacyObjConfigFullIdMap);
    procedure MergeLegacy(ACombinedConfig: TJSONObject; AFullIdToDefMap: TLegacyObjConfigFullIdMap);
    procedure HandleInteritanceObjectTemplate(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var Continue: Boolean);
    procedure HandleInteritanceObjectSubType(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var Continue: Boolean);
    procedure HandleInteritanceObjectType(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var Continue: Boolean);
    procedure HandleInteritance(AConfig: TJSONObject);
    procedure AddFactions(AConfig: TJSONObject);
    procedure AddHeroClasses(AConfig: TJSONObject);
    procedure AddCreatures(AConfig: TJSONObject);
    procedure AddArtifacts(AConfig: TJSONObject);
    procedure PopulateMapOfLegacyObjects;
    procedure OnObjectDestream(Sender : TObject; AObject : TObject; JSON : TJSONObject);
  private
    FListsManager: TListsManager;
    procedure SetListsManager(AValue: TListsManager);
    procedure FillWithAllObjects(ATarget: TMapObjectTemplateList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ListsManager:TListsManager read FListsManager write SetListsManager;
    procedure LoadObjects(AProgressCallback: IProgressCallback; APaths: TModdedConfigPaths);
    property MapObjectGroups: TMapObjectGroups read FMapObjectGroups;
    procedure SelectAll(ATarget: TObjectsSelection);
    // AInput = space separated words
    procedure SelectByKeywords(ATarget: TObjectsSelection; AInput: string);
    function ResolveLegacyID(Typ,SubType: uint32):TMapObjectType;
    procedure BuildIndex;

    function FormatObjectName(AType, ASybtype: AnsiString): TLocalizedString;
  end;

implementation

uses
  LazLoggerBase, editor_consts, editor_utils,
  root_manager;

const
  OBJECT_LIST = 'DATA/OBJECTS';
  OBJECT_NAMES = 'DATA/OBJNAMES';

  function CompareStringProxy(const s1,s2: string): integer;
  begin
    Result := UTF8CompareStr(s1, s2);
  end;

  function CompareSearchIndexBusket(const d1,d2: TSearchIndexBusket): integer;
  begin
    Result := PtrInt(d1) - PtrInt(d2);
  end;

{ TSearchIndexMap }

procedure TSearchIndexMap.Deref(Item: Pointer);
begin
  Finalize(string(Item^));
  TSearchIndexBusket(Pointer(PByte(Item)+KeySize)^).Free;
end;

constructor TSearchIndexMap.Create;
begin
  inherited Create;
  OnKeyCompare := @CompareStringProxy;
  OnDataCompare := @CompareSearchIndexBusket;

  Sorted := True;
  Duplicates:=dupError;
end;

function TSearchIndexMap.Find(AKeyWord: String; out IdxLow: integer; out IdxHigh: integer): boolean;

  function PartialMatched(idx: integer): boolean;
  begin
    result := UTF8Pos(AKeyWord,Keys[idx]) = 1;
  end;

begin
  IdxLow := -1;
  IdxHigh := -1;

  if Find(AKeyWord, IdxLow) then
  begin
    IdxHigh:=IdxLow;
    Result := true;
  end
  else
  begin
    //find firts true partial match

    while (IdxLow < Count) and not PartialMatched(IdxLow) do
    begin
      Inc(IdxLow);
    end;

    if IdxLow >= Count then
    begin
      IdxLow := -1;
      IdxHigh := -1;

      Result := false;
      Exit;
    end;

    //find end of matched region

    IdxHigh:=IdxLow;

    while (IdxHigh < Count) and PartialMatched(IdxHigh) do
    begin
      Inc(IdxHigh);
    end;

    IdxHigh:=Min(IdxHigh-1, Count-1);
    Result := true;
  end;
end;

{ TObjTemplateCompare }

class function TObjTemplateCompare.c(a, b: TMapObjectTemplate): boolean;
begin
  Result := PtrInt(a) < PtrInt(b);
end;

{ TSearchIndexBusket }

constructor TSearchIndexBusket.Create;
begin
  data := TBusketData.Create;
end;

destructor TSearchIndexBusket.Destroy;
begin
  data.Free;
  inherited Destroy;
end;

procedure TSearchIndexBusket.AddItem(AItem: TMapObjectTemplate);
begin
  data.Insert(AItem);
end;

procedure TSearchIndexBusket.SaveTo(ATarget: TBusketData);
var
  it: TBusketData.TIterator;
begin
  it := data.Min;

  if Assigned(it) then
  begin
    repeat
      ATarget.Insert(it.Data);
    until not it.Next;
    it.free;
  end;
end;

procedure TSearchIndexBusket.Intersect(ATarget: TBusketData);
var
  it: TBusketData.TIterator;
  n: TBusketData.PNode;
  to_delete: TDataVector;
  i: SizeInt;
begin
  to_delete := TDataVector.Create;

  it := ATarget.Min;

  if Assigned(it) then
  begin
    repeat
      n := data.NFind(it.Data);

      if not Assigned(n) then
      begin
        to_delete.PushBack(it.data);
      end;
    until not it.Next;
    it.free;
  end;

  for i := 0 to SizeInt(to_delete.Size) - 1 do
  begin
    ATarget.Delete(to_delete.Items[i]);
  end;

  to_delete.Free;
end;

{ TSearchIndex }

constructor TSearchIndex.Create;
begin
  FMap := TSearchIndexMap.Create();
end;

destructor TSearchIndex.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure TSearchIndex.AddToIndex(AKeyWord: String; AItem: TMapObjectTemplate);
var
  busket: TSearchIndexBusket;
  idx: Integer;
begin
  idx := -1;

  if not Fmap.Find(AKeyWord, idx) then
  begin
    busket := TSearchIndexBusket.Create();
    FMap.Add(AKeyWord, busket);
  end
  else
  begin
    busket := FMap.Data[idx];
  end;

  busket.AddItem(AItem);
end;

procedure TSearchIndex.Find(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);
var
  idx_low, idx_high, i: Integer;
begin
  idx_low := -1;
  idx_high := -1;

  if Fmap.Find(AKeyWord, idx_low, idx_high)then
  begin
    for i := idx_low to idx_high do
    begin
      FMap.Data[i].SaveTo(ATarget);
    end;
  end;
end;

procedure TSearchIndex.Intersect(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);
var
  idx_low, idx_high, i: Integer;

  temp : TSearchIndexBusket;
begin
  idx_low := -1;
  idx_high := -1;

  if Fmap.Find(AKeyWord, idx_low, idx_high)then
  begin
    temp := TSearchIndexBusket.Create;

    for i := idx_low to idx_high do
    begin
      FMap.Data[i].SaveTo(temp.data);
    end;

    temp.Intersect(ATarget);
    temp.Free;
  end
  else
  begin
    while not ATarget.IsEmpty do
    begin
      ATarget.Delete(ATarget.NMin^.Data);
    end;
  end;
end;

{ TMaskResource }

procedure TMaskResource.Clear;
begin
  Width := 0;
  Height := 0;
end;

procedure TMaskResource.SetHeight(AValue: Byte);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TMaskResource.SetWidth(AValue: Byte);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

constructor TMaskResource.Create(APath: AnsiString);
begin
  inherited Create(TResourceType.Mask, 'SPRITES/'+APath);
end;

procedure TMaskResource.LoadFromStream(AStream: TStream);
begin
  Clear;
  AStream.Read(FWidth,1);
  AStream.Read(FHeight,1);

  AStream.Read(FMask1,6);
  AStream.Read(FMask2,6);
end;

{ TMapObjectTypes }

constructor TMapObjectTypes.Create(AOwner: TMapObjectGroup);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TMapObjectTemplates }

constructor TMapObjectTemplates.Create(AOwner: TMapObjectType);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TObjectsSelection }

function TObjectsSelection.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TObjectsSelection.GetObjcts(AIndex: Integer): TMapObjectTemplate;
begin
  Result := FData.Items[AIndex];
end;

constructor TObjectsSelection.Create;
begin
  FData := TMapObjectTemplateList.Create(False);
end;

destructor TObjectsSelection.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TObjectsSelection.Clear;
begin
  FData.Clear;
end;

{ TMapObjectGroup }

function TMapObjectGroup.GetDisplayName: string;
begin
  if FName<>'' then
  begin
    Result := FName;
  end
  else
  begin
    Result:=inherited GetDisplayName;
  end;
end;

procedure TMapObjectGroup.SetIdentifier(AValue: AnsiString);
begin
  inherited SetIdentifier(AValue);

  FIsHero := AValue = TYPE_HERO;
  FIsHeroLike:= (AValue = TYPE_HERO) or (AValue = TYPE_RANDOMHERO) or (AValue = TYPE_HERO_PLACEHOLDER);
end;

constructor TMapObjectGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMapObjectTypes := TMapObjectTypes.Create(self);
  Index:=ID_INVALID;
end;

destructor TMapObjectGroup.Destroy;
begin
  FMapObjectTypes.Free;
  inherited Destroy;
end;

class function TMapObjectGroup.UseMeta: boolean;
begin
  Result:=true;
end;

{ TMapObjectType }

function TMapObjectType.GetIndexAsID: TCustomID;
begin
  Result := FNid;
end;

procedure TMapObjectType.SetIndexAsID(AValue: TCustomID);
begin
  FNid := AValue;
end;

function TMapObjectType.GetDisplayName: string;
begin
  if FName<>'' then
  begin
    Result := FName;
  end
  else
  begin
    Result:=inherited GetDisplayName;
  end;
end;

constructor TMapObjectType.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  index := ID_INVALID;
  FTemplates := TMapObjectTemplates.Create(Self);
  FMapObjectGroup :=  (ACollection as TMapObjectTypes).Owner;
  FFilters := CreateJSONObject([]);
end;

destructor TMapObjectType.Destroy;
begin
  FFilters.Free;
  FTemplates.Free;
  inherited Destroy;
end;

class function TMapObjectType.UseMeta: boolean;
begin
  Result:=true;
end;

{ TMapObjectTemplate }

procedure TMapObjectTemplate.SetAnimation(AValue: AnsiString);
begin
  AValue := NormalizeResourceName(AValue);
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;

  //use this animation only if there is no editor animation
  //todo: delay animation loading(load after serialize)

  if FEditorAnimation='' then
    FDef := root_manager.RootManager.GraphicsManager.GetGraphics(FAnimation);
end;

procedure TMapObjectTemplate.SetEditorAnimation(AValue: AnsiString);
begin
  AValue := NormalizeResourceName(AValue);

  if FEditorAnimation=AValue then Exit;
  FEditorAnimation:=AValue;

  FDef := root_manager.RootManager.GraphicsManager.GetGraphics(FEditorAnimation);
end;

constructor TMapObjectTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVisitableFrom := TStringList.Create;
  FMask := TStringList.Create;

  AllowedTerrains := ALL_TERRAINS;

  FMapObjectType :=  (ACollection as TMapObjectTemplates).MapObjectType;

  FMapObjectGroup := (ACollection as TMapObjectTemplates).MapObjectType.FMapObjectGroup;
  FTags := TStringList.Create;
end;

destructor TMapObjectTemplate.Destroy;
begin
  FTags.Free;
  FMask.Free;
  FVisitableFrom.Free;
  inherited Destroy;
end;

class function TMapObjectTemplate.UseMeta: boolean;
begin
  Result:=True;
end;

procedure TMapObjectTemplate.RenderIcon(AState: TLocalState; AX, AY, dim: integer; color: TPlayer);
var
  h: integer;
begin
  AState.SetTranslation(Ax,Ay);

  FDef.RenderIcon(AState, dim, color);

  h := FDef.Height;

  if (color <> TPlayer.none) and FMapObjectGroup.IsHeroLike then
  begin
    RootManager.GraphicsManager.GetHeroFlagDef(color).RenderOverlayIcon(AState, dim, h);
  end;
end;

{ TLegacyObjTemplate }

constructor TLegacyObjTemplate.Create;
begin
  inherited;
end;

{ TObjectsManager }

constructor TObjectsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMapObjectGroups := TMapObjectGroups.Create;
  FLegacyObjTypes := TLegacyIdMap.Create;

  FAllTemplates := TMapObjectTemplateList.Create(false);

  FTextTokenizer := TRegExpr.Create('[_\-\s\.:]+');
  FTextTokenizer.Compile
end;

destructor TObjectsManager.Destroy;
begin
  FTextTokenizer.Free;
  FSearchIndex.Free;
  FAllTemplates.Free;
  FLegacyObjTypes.Free;
  FMapObjectGroups.Free;
  inherited Destroy;
end;

procedure TObjectsManager.SetListsManager(AValue: TListsManager);
begin
  if FListsManager=AValue then Exit;
  FListsManager:=AValue;
end;

procedure TObjectsManager.FillWithAllObjects(ATarget: TMapObjectTemplateList);
var
 i,j,k: Integer;
 obj_type: TMapObjectGroup;
 obj_subtype: TMapObjectType;
 obj_template: TMapObjectTemplate;
begin
  ATarget.Clear;
  for i := 0 to FMapObjectGroups.Count - 1 do
  begin
    obj_type := FMapObjectGroups.Items[i];

    for j := 0 to obj_type.Types.Count - 1 do
    begin
      obj_subtype := obj_type.Types[j];

      for k := 0 to obj_subtype.Templates.count - 1 do
      begin
        obj_template := obj_subtype.Templates[k];

        ATarget.Add(obj_template);

        Assert(Assigned(obj_template.FMapObjectGroup));
        Assert(Assigned(obj_template.FMapObjectType));
      end;
    end;
  end;
end;

procedure TObjectsManager.LoadObjects(AProgressCallback: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
  destreamer: TVCMIJSONDestreamer;
  FFullIdToDefMap: TLegacyObjConfigFullIdMap; //type,subtype => template list
  i: Integer;
begin
  FProgress := AProgressCallback;
  FFullIdToDefMap := TLegacyObjConfigFullIdMap.Create;
  LoadLegacy(AProgressCallback, FFullIdToDefMap);

  AProgressCallback.NextStage('Building objects configuration ... ');
  AProgressCallback.Max:=8;

  FConfig := TModdedConfigs.Create;
  FCombinedConfig := CreateJSONObject([]);
  destreamer := TVCMIJSONDestreamer.Create(nil);
  destreamer.AfterReadObject := @OnObjectDestream;
  try
    FConfig.Load(AProgressCallback, APaths,ResourceLoader, FCombinedConfig);
    AddFactions(FCombinedConfig);
    AddHeroClasses(FCombinedConfig);
    AddCreatures(FCombinedConfig);
    AddArtifacts(FCombinedConfig);
    MergeLegacy(FCombinedConfig, FFullIdToDefMap);

    for i := 0 to FFullIdToDefMap.Count - 1 do
    begin
      DebugLn(['unused legacy data ', Hi(FFullIdToDefMap.Keys[i]), ' ' , Lo(FFullIdToDefMap.Keys[i])]);
    end;

    HandleInteritance(FCombinedConfig);
    AProgressCallback.Advance(1);

    FProgress.Max:=FCombinedConfig.Count;
    AProgressCallback.NextStage('Loading objects ... ');
    destreamer.JSONToObject(FCombinedConfig,FMapObjectGroups);

    PopulateMapOfLegacyObjects;
  finally
    FCombinedConfig.Free;
    FConfig.Free;
    destreamer.Free;
    FFullIdToDefMap.Free;
  end;
end;

procedure TObjectsManager.SelectAll(ATarget: TObjectsSelection);
begin
  ATarget.Clear;
  FillWithAllObjects(ATarget.FData);
end;

procedure TObjectsManager.SelectByKeywords(ATarget: TObjectsSelection; AInput: string);
var
  keywords: TStringList;
  data: TSearchIndexBusket.TBusketData;
  i: Integer;
  it: TSearchIndexBusket.TBusketData.TIterator;
begin
  ATarget.Clear;

  AInput := UTF8Trim(UTF8LowerCase(AInput));

  if AInput = '' then
  begin
    SelectAll(ATarget);
    Exit;
  end;

  data := TSearchIndexBusket.TBusketData.Create;

  keywords := TStringList.Create;
  keywords.Sorted:=true;
  keywords.Duplicates:=dupIgnore;

  FTextTokenizer.Split(AInput, keywords);

  FSearchIndex.Find(keywords[0], data);

  if not data.IsEmpty() then
  begin
    for i := 1 to keywords.Count - 1 do
    begin
      FSearchIndex.Intersect(keywords[i], data);
      if data.IsEmpty then
        break;
    end;
  end;

  it := data.Min;

  if Assigned(it) then
  begin
    repeat
      ATarget.FData.Add(it.Data);
    until not it.Next;
    it.free;
  end;

  data.Free;
  keywords.Free;
end;


function TObjectsManager.ResolveLegacyID(Typ, SubType: uint32): TMapObjectType;
var
  full_id: TLegacyTemplateId;
begin
  full_id := TypToId(Typ, SubType);

  if FLegacyObjTypes.IndexOf(full_id) <0 then
  begin
    DebugLn(['Unknown object ', Typ, ' ', SubType]);
    Result := nil;
  end
  else
    Result := FLegacyObjTypes.KeyData[full_id];
end;

procedure TObjectsManager.BuildIndex;
var
  idx: SizeInt;
  obj: TMapObjectTemplate;
  keyword, s: string;
  keywords: TStringList;
  i: Integer;
  obj_type: TMapObjectGroup;
  obj_subtype: TMapObjectType;
begin
  FillWithAllObjects(FAllTemplates);
  FSearchIndex := TSearchIndex.Create();
  keywords := TStringList.Create;
  keywords.Sorted:=true;
  keywords.Duplicates:=dupIgnore;

  for idx := 0 to FAllTemplates.Count - 1 do
  begin
    keywords.Clear;
    obj := FAllTemplates[idx];

    for s in obj.Tags do
    begin
      keywords.Add(s);
    end;

    if obj.Meta <> '' then
    begin
      keyword := NormalizeKeyWord(obj.Meta);
      FTextTokenizer.Split(keyword, keywords)
    end;

    obj_type := obj.MapObjectGroup;
    if obj_type.Name <> '' then
      keyword := obj_type.Name
    else
      keyword := obj_type.Identifier;
    keyword := NormalizeKeyWord(keyword);
    FTextTokenizer.Split(keyword, keywords);

    obj_subtype := obj.MapObjectType;
    if obj_subtype.Name <> '' then
      keyword := obj_subtype.Name
    else
      keyword := obj_subtype.Identifier;
    keyword := NormalizeKeyWord(keyword);
    FTextTokenizer.Split(keyword, keywords);


    for i := 0 to keywords.Count - 1 do
    begin
      FSearchIndex.AddToIndex(keywords[i], obj);
    end;
  end;
  keywords.Free;
end;

function TObjectsManager.FormatObjectName(AType, ASybtype: AnsiString): TLocalizedString;
var
  group: TMapObjectGroup;
  tp: TMapObjectType;

  group_name: TLocalizedString;
  tp_name: String;
begin

  group := FMapObjectGroups.FindItem(AType);

  group_name := AType;

  if Assigned(group) then
  begin
    group_name := group.DisplayName
  end;

  tp := group.Types.FindItem(ASybtype);

  if Assigned(tp) then
  begin
    tp_name := tp.DisplayName
  end;

  Result := Format('%s:%s',[group_name, tp_name]);
end;

function TObjectsManager.TypToId(Typ, SubType: uint32): TLegacyTemplateId;
begin
  Int64Rec(Result).Hi := Typ;
  Int64Rec(Result).Lo := SubType;
end;

procedure TObjectsManager.LoadLegacy(AProgressCallback: IProgressCallback;
  AFullIdToDefMap: TLegacyObjConfigFullIdMap);
var
  row, col: Integer;
  objects_txt: TTextResource;

  procedure CellToStr(var s: string);
  begin
    if not objects_txt.HasCell(col, row) then
       raise Exception.CreateFmt('OBJTXT error cell not exists. row:%d, col:%d',[row,col]);

    s := objects_txt.Value[col,row];
    inc(col);
  end;


  procedure CellToBitMask(var mask: TDefBitmask);
  var
    i,j: Integer;
    s, ss: string;
    m: UInt8;
  begin
    s:='';
    CellToStr(s);
    if not Length(s)=6*8 then
       raise Exception.CreateFmt('OBJTXT Format error. line:%d, data:%s',[row,s]);

    for i:=5 downto 0 do //in object.txt bottom line is first
    begin
      ss := Copy(s,i*8+1,8);
      if not (Length(ss)=8) then
        raise Exception.CreateFmt('OBJTXT Format error. line:%d, data:%s',[row,s]);
      m := 0;
      for j := 0 to 7 do
      begin
        if ss[j+1] = '1' then
          m := m or (1 shl j) ;
      end;
      mask[i] := m;
    end;
  end;


  procedure CellToUint16Mask(var v: uint16);
  var
    temp: string;
    len, i: Integer;
  begin
    temp := '';
    CellToStr(temp);
    len:= Length(temp);
    v := 0;
    for i := len downto 1 do
    begin
      if temp[i] = '1' then
        v := v or 1 shl (len - i);
    end;
  end;

  function CellToInt: uint32;
  begin
    result := StrToIntDef(objects_txt.Value[col,row],0);
    inc(col);
  end;

var
  def: TLegacyObjTemplate;
  s_tmp: string;
  legacy_config: TJSONObject;
  list: TLegacyObjConfigList;
  full_id: TLegacyTemplateId;
  idx: LongInt;
  byte_idx, bit_idx: integer;
  str: String;

  passable, active: Boolean;
  mask_conf, visit_conf, allowedTerrains, tags: TJSONArray;
  //anim: TDefAnimation;
  width_tiles: Integer;
  height_tiles: Integer;

  msk: TMaskResource;
begin
  AProgressCallback.NextStage('Loading legacy objects ... ');

  objects_txt := TTextResource.Create(OBJECT_LIST);
  objects_txt.Delimiter := TTextResource.TDelimiter.Space;

  try
    objects_txt.Load(ResourceLoader);

    AProgressCallback.Max:=objects_txt.RowCount;

    for row := 1 to objects_txt.RowCount-1 do //first row contains no data, so start with 1
    begin
      col := 0;

      def := TLegacyObjTemplate.Create;

      s_tmp := '';

      CellToStr(s_tmp);

      def.FFilename := NormalizeResourceName(s_tmp);


      CellToBitMask(def.FPassability);
      CellToBitMask(def.FActions);
      CellToUint16Mask(def.FLandscape);
      CellToUint16Mask(def.FLandEditGroups);

      def.FTyp := CellToInt;
      def.FSubType := CellToInt;
      def.FGroup := CellToInt;
      def.FIsOverlay := CellToInt;

      //todo: read full mask

      msk := TMaskResource.Create(def.FFilename);

      if ResourceLoader.TryLoadResource(msk, msk.Typ, msk.Path) then
      begin
        width_tiles := msk.Width;
        height_tiles := msk.Height;
      end
      else begin
        width_tiles := 8;
        height_tiles:= 6;
      end;
      msk.Free;

      legacy_config := CreateJSONObject([]);

      legacy_config.Strings['animation'] := def.Filename;
      legacy_config.Integers['zIndex'] := def.IsOverlay * Z_INDEX_OVERLAY;

      mask_conf := CreateJSONArray([]);

      for byte_idx := height_tiles-1 downto 0 do
      begin
        str := '';
        for bit_idx := width_tiles-1 downto 0 do
        begin
          //assume visible
          passable:=(def.FPassability[byte_idx] and (1 shl bit_idx))>0;

          active:=(def.FActions[byte_idx] and (1 shl bit_idx))>0;

          if passable then
            str += 'V'
          else
            if active then
              str += 'A'
            else
              str += 'B';
        end;
        UniqueString(str);
        mask_conf.Add(str);
      end;

      legacy_config.Add('mask', mask_conf);

      allowedTerrains := CreateJSONArray([]);

      for bit_idx := 0 to Integer(TTerrainType.water) do
      begin
         if (def.FLandscape and (1 shl bit_idx)) > 0 then
         begin
           allowedTerrains.Add(GetEnumName(TypeInfo(TTerrainType), bit_idx));
         end;
      end;

      legacy_config.Add('allowedTerrains', allowedTerrains);

      tags := CreateJSONArray([]);

      for bit_idx := 0 to Integer(TTerrainType.water) do
      begin
         if (def.FLandEditGroups and (1 shl bit_idx)) > 0 then
         begin
           tags.Add(GetEnumName(TypeInfo(TTerrainType), bit_idx));
         end;
      end;

      legacy_config.Add('tags', tags);

      visit_conf := CreateJSONArray([]);
      GenerateDefaultVisitableFrom(visit_conf, def.FGroup, TObj(def.Typ) );
      legacy_config.Add('visitableFrom', visit_conf);

      full_id := TypToId(def.FTyp, def.FSubType);
      idx := AFullIdToDefMap.IndexOf(full_id);

      if idx = -1 then
      begin
        list := TLegacyObjConfigList.Create(True);
        AFullIdToDefMap.Add(full_id, list);
      end
      else
      begin
        list := AFullIdToDefMap.Data[idx];
      end;

      list.Add(legacy_config);
      def.Free;

      AProgressCallback.Advance(1);
    end;
  finally
    objects_txt.Free;
  end;
end;

procedure TObjectsManager.MergeLegacy(ACombinedConfig: TJSONObject;
  AFullIdToDefMap: TLegacyObjConfigFullIdMap);
var
  obj_id, obj_subid: Int32;
  i,j,k: Integer;
  idx: Integer;
  obj, subTypes: TJSONObject;
  subTypeObj, templates_obj: TJSONObject;
  full_id: TLegacyTemplateId;
  legacy_data: TLegacyObjConfigList;
  t: TJSONObject;
  objects_names: TTextResource;
begin
  objects_names := TTextResource.Create(OBJECT_NAMES);
  objects_names.Load(ResourceLoader);
  //cycle by type
  for i := 0 to ACombinedConfig.Count - 1 do
  begin
    obj := ACombinedConfig.Items[i] as TJSONObject;
    idx := obj.IndexOfName('index');

    obj_id := -1;

    if idx >=0 then
    begin
      obj_id := obj.Integers['index'];
    end;

    if obj_id < 0 then
    begin
      Continue; //no index property or invalid
    end;

    idx := obj.IndexOfName('name');

    if idx < 0 then
    begin
      obj.Strings['name'] := objects_names.Value[0, obj_id];
    end;

    if obj.IndexOfName('types')<0 then
      Continue;

    subTypes := obj.Objects['types'] as TJSONObject;

    for j := 0 to subTypes.Count - 1 do
    begin
      subTypeObj := subTypes.Items[j] as TJSONObject;

      idx := subTypeObj.IndexOfName('index');

      obj_subid := -1;

      if idx >=0 then
      begin
        obj_subid := subTypeObj.Integers['index'];
      end;

      if obj_subid < 0 then
      begin
        Continue; //no index property or invalid
      end;

      full_id := TypToId(obj_id, obj_subid);

      idx :=  AFullIdToDefMap.IndexOf(full_id);

      if idx < 0 then
      begin
        Continue; //no legacy data for this id
      end;

      legacy_data :=  AFullIdToDefMap.Data[idx];

      //subTypeObj => legacy_data
      templates_obj :=  subTypeObj.GetOrCreateObject('templates');

      if templates_obj.Count = 0 then
      begin
        //add legacy templates only if there are no normal templates
        for k := legacy_data.Count - 1 downto 0 do
        begin
          t := legacy_data.Items[k];

          legacy_data.Extract(t);

          templates_obj.Add('legacy_'+IntToStr(k), t);
        end;
      end;
      AFullIdToDefMap.Remove(full_id); //delete merged data
    end;
  end;
  objects_names.Free;
end;

procedure TObjectsManager.HandleInteritanceObjectTemplate(
  const AName: TJSONStringType; Item: TJSONData; Data: TObject;
  var Continue: Boolean);
var
  obj_template: TVCMIJsonObject;
begin
  obj_template := Item as TVCMIJsonObject;
  if Assigned(data) then
    obj_template.InheritFrom(data as TVCMIJsonObject);
end;

procedure TObjectsManager.HandleInteritanceObjectSubType(
  const AName: TJSONStringType; Item: TJSONData; Data: TObject;
  var Continue: Boolean);
var
  obj_subtype: TVCMIJsonObject;
  base: TJSONObject;
  idx: Integer;
begin
  obj_subtype := Item as TVCMIJsonObject;

  if Assigned(data) then
    obj_subtype.InheritFrom(data as TVCMIJsonObject);

  base := nil;

  idx := obj_subtype.IndexOfName('base');

  if idx>=0 then
  begin
    base := obj_subtype.Objects['base'];
  end;

  //now handle base for templates

  idx :=  obj_subtype.IndexOfName('templates');
  if idx<0 then
  begin
    if Assigned(base) then
      DebugLn([#9,'Subtype ', AName, ' has "base" but no templates'])
    else
      DebugLn([#9,'Subtype ', AName, ' has no templates']);
    exit; //no templates - nothing to do
    //todo: display full name
  end;

  obj_subtype.Objects['templates'].Iterate(@HandleInteritanceObjectTemplate, base);
end;

procedure TObjectsManager.HandleInteritanceObjectType(
  const AName: TJSONStringType; Item: TJSONData; Data: TObject;
  var Continue: Boolean);
var
  obj_type: TJSONObject;
  base: TJSONObject;
  idx: Integer;
begin
  FProgress.Advance(1);
  //AName = object type id
  obj_type  := Item as TJSONObject;

  base := nil;

  idx := obj_type.IndexOfName('base');

  if idx>=0 then
  begin
    base := obj_type.Objects['base'];
  end;

  idx :=  obj_type.IndexOfName('types');

  if idx<0 then
  begin
    if Assigned(base) then
      DebugLn(['Object ', AName, ' has "base" but no types'])
    else
      DebugLn(['Object ', AName, ' has no types']);
    exit; //no types - nothing to do
  end;

  obj_type.Objects['types'].Iterate(@HandleInteritanceObjectSubType, base);
end;

procedure TObjectsManager.HandleInteritance(AConfig: TJSONObject);
begin
  FProgress.Max:=AConfig.Count;
  AConfig.Iterate(@HandleInteritanceObjectType, nil);
end;

procedure TObjectsManager.AddFactions(AConfig: TJSONObject);
var
  town_type, town_types: TJSONObject;
  i: Integer;
  faction: TFactionInfo;
begin
  town_types := AConfig.Objects['town'].GetOrCreateObject('types');

  for i := 0 to ListsManager.FactionInfos.Count - 1 do
  begin
    faction := ListsManager.FactionInfos[i];

    if not faction.HasTown then
      Continue;

    town_type := CreateJSONObject([]);
    if(faction.Index >= 0) then
      town_type.Add('index', faction.Index);

    MergeJson(faction.Town.MapObject, town_type);

    town_types.Add(faction.Identifier , town_type);
  end;
end;

procedure TObjectsManager.AddHeroClasses(AConfig: TJSONObject);
var
  hero_classes: TJSONObject;
  i: Integer;
  hcinfo: THeroClassInfo;
  hc: TJSONObject;
begin
  hero_classes := AConfig.Objects['hero'].GetOrCreateObject('types');

  for i := 0 to ListsManager.HeroClassInfos.Count - 1 do
  begin
    hcinfo := ListsManager.HeroClassInfos[i];
    hc := CreateJSONObject([]);
    if hcinfo.Index >=0 then
      hc.Add('index', hcinfo.Index);

    MergeJson(hcinfo.MapObject, hc);

    hero_classes.Add(hcinfo.Identifier, hc);
  end;
end;

procedure TObjectsManager.AddCreatures(AConfig: TJSONObject);
var
  creatures: TJSONObject;
  cr_info: TCreatureInfo;
  cr: TJSONObject;
  i: Integer;
begin
  creatures := AConfig.Objects['monster'].GetOrCreateObject('types');

  for i := 0 to ListsManager.CreatureInfos.Count - 1 do
  begin
    cr_info := ListsManager.CreatureInfos[i];

    cr := CreateJSONObject([]);
    if cr_info.Index >= 0 then
      cr.Add('index', cr_info.Index);
    cr_info.Graphics.AddTemplates(cr);

    creatures.Add(cr_info.Identifier, cr);
  end;
end;

procedure TObjectsManager.AddArtifacts(AConfig: TJSONObject);
var
  artifacts: TJSONObject;
  info: TArtifactInfo;
  ar: TJSONObject;
  i: Integer;
begin
  artifacts := AConfig.Objects['artifact'].GetOrCreateObject('types');

  for i := 0 to ListsManager.ArtifactInfos.Count - 1 do
  begin
    info := ListsManager.ArtifactInfos[i];
    ar := CreateJSONObject([]);
    if info.Index >= 0 then
      ar.Add('index', info.Index);

    info.Graphics.AddTemplates(ar);
    artifacts.Add(info.Identifier, ar);
  end;
end;

procedure TObjectsManager.PopulateMapOfLegacyObjects;
var
  i,j: SizeInt;
  obj_type: TMapObjectGroup;
  obj_sub_type: TMapObjectType;
  full_id: TLegacyTemplateId;
begin
  for i := 0 to FMapObjectGroups.Count - 1 do
  begin
    obj_type := FMapObjectGroups[i];

    for j := 0 to obj_type.Types.Count - 1 do
    begin
      obj_sub_type := obj_type.Types[j];

      if (obj_type.Index>=0) and (obj_sub_type.Index>=0) then
      begin
        full_id := TypToId(obj_type.Index, obj_sub_type.Index);
        FLegacyObjTypes.KeyData[full_id] := obj_sub_type;
      end;
    end;
  end;
end;

procedure TObjectsManager.OnObjectDestream(Sender: TObject; AObject: TObject;
  JSON: TJSONObject);
begin
  if Assigned(FProgress) and (AObject is TMapObjectGroup) then
  begin
    FProgress.Advance(1);
  end;
end;

end.

