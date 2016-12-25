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

{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, fgl, typinfo, FileUtil, LazUTF8, fpjson,
  editor_types,  filesystem_base, editor_graphics, editor_classes, h3_txt,
  lists_manager, vcmi_json, editor_gl, search_index;

type

  TObjectCategory = (Artifact, Creature, Dwelling, Hero, Other, Resource, Static, Town);

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
    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;
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

  TMapObjectTemplate = class (TNamedCollectionItem, ISerializeNotify)
  strict private
    FDef: TDefAnimation;
    FMapObjectGroup:TMapObjectGroup;
    FMapObjectType: TMapObjectType;

    FAllowedTerrains: TTerrainTypes;
    FAnimation: AnsiString;
    FEditorAnimation: AnsiString;
    FTags: TStrings;
    FVisitableFrom: TStrings;
    FMask: TStrings;
    FzIndex: Integer;
    FIconSpriteIndex: integer;
    procedure SetAnimation(AValue: AnsiString);
    procedure SetEditorAnimation(AValue: AnsiString);

    procedure UpdateAnimation;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Def: TDefAnimation read FDef;
    property MapObjectGroup: TMapObjectGroup read FMapObjectGroup;
    property MapObjectType: TMapObjectType read FMapObjectType;
    class function UseMeta: boolean; override;

    //for palette
    procedure RenderIcon(AState: TLocalState; AX, AY, dim:integer; color: TPlayer = TPlayer.none);
    //for map
    procedure RenderFloating(AState: TLocalState; AX, AY: integer; color: TPlayer = TPlayer.none);

  public //ISerializeNotify
    procedure BeforeSerialize(Sender:TObject);
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);

    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
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
    function HasFilters: Boolean;
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
    FCategory: TObjectCategory;
    FHandler: AnsiString;
    FName: TLocalizedString;
    FNid: TCustomID;
    FMapObjectTypes: TMapObjectTypes;
    FIsHero, FIsHeroLike: Boolean;
    procedure UpdateCategory;
    procedure SetHandler(AValue: AnsiString);
  protected
    function GetDisplayName: string; override;
    procedure SetIdentifier(AValue: AnsiString); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    class function UseMeta: boolean; override;

    property IsHero: Boolean read FIsHero;
    property IsHeroLike: Boolean read FIsHeroLike;
    property Category: TObjectCategory read FCategory;
  published
    property Index: TCustomID read FNid write FNid default ID_INVALID;
    property Types:TMapObjectTypes read FMapObjectTypes;
    property Name: TLocalizedString read FName write FName;
    property Handler: AnsiString read FHandler write SetHandler;
  end;

  TObjectGroupFilter = function (AObject: TMapObjectGroup): boolean is nested;

  TMapObjectGroups = specialize TGNamedCollection<TMapObjectGroup>;

  {$pop}

  TLegacyIdMap = specialize TObjectMap<TLegacyTemplateId, TMapObjectType>;

  TObjectsManager = class;

  { TObjectsSelection }

  TObjectsSelection = class(TObject, ISearchResult)
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
  public//ISearchResult
    procedure Add(AObject: TObject);
  end;

  { TObjectsManager }

  TObjectsManager = class (TGraphicsConsumer)
  strict private
    FMapObjectGroups: TMapObjectGroups;

    FSearchIndexes: array [TObjectCategory] of TSearchIndex;

    FLegacyObjTypes: TLegacyIdMap;
    FProgress: IProgressCallback;
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
    procedure FillWithAllObjects(ATarget: TMapObjectTemplateList; AFilter: TObjectGroupFilter);
    procedure BuildIndex(ACategory: TObjectCategory);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ListsManager:TListsManager read FListsManager write SetListsManager;
    procedure LoadObjects(AProgressCallback: IProgressCallback; APaths: TModdedConfigPaths);
    property MapObjectGroups: TMapObjectGroups read FMapObjectGroups;

    procedure SelectAll(ATarget: TObjectsSelection; ACategory: TObjectCategory);

    // AInput = space separated words
    procedure SelectByKeywords(ATarget: TObjectsSelection; AInput: string; ACategory: TObjectCategory);
    function ResolveLegacyID(Typ,SubType: uint32):TMapObjectType;

    procedure BuildIndex();
    function FormatObjectName(AType, ASybtype: AnsiString): TLocalizedString;
  end;

implementation

uses
  LazLoggerBase, editor_consts, editor_utils,
  root_manager;

const
  OBJECT_LIST = 'DATA/OBJECTS';
  OBJECT_NAMES = 'DATA/OBJNAMES';

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

procedure TMaskResource.LoadFromStream(AFileName: AnsiString; AStream: TStream);
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

procedure TObjectsSelection.Add(AObject: TObject);
begin
  FData.Add(TMapObjectTemplate(AObject));
end;

{ TMapObjectGroup }

procedure TMapObjectGroup.UpdateCategory;
begin
  if (Handler='') or (Identifier = '') then
    exit;

  if IsHeroLike then
    FCategory:=TObjectCategory.Hero

  else if (Handler = 'artifact') or (Handler = 'randomArtifact') then
    FCategory:=TObjectCategory.Artifact
  else if (Handler = 'monster') or (Handler ='randomMonster') then
    FCategory:=TObjectCategory.Creature
  else if (Handler = 'dwelling') or (Handler ='randomDwelling') then
    FCategory:=TObjectCategory.Dwelling
  else if (Handler = 'town') or (Handler = 'randomTown') then
    FCategory:=TObjectCategory.Town
  else if Handler = 'static' then
    FCategory:=TObjectCategory.Static
  else if (Handler = 'resource') or (Handler = 'randomResource') then
    FCategory:=TObjectCategory.Resource
  else if Handler = 'town' then
    FCategory:=TObjectCategory.Town
  else
    FCategory:=TObjectCategory.Other;
end;

procedure TMapObjectGroup.SetHandler(AValue: AnsiString);
begin
  if FHandler=AValue then Exit;
  FHandler:=AValue;
  UpdateCategory;
end;

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
  UpdateCategory;
end;

constructor TMapObjectGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMapObjectTypes := TMapObjectTypes.Create(self);
  Index:=ID_INVALID;
  FCategory:=TObjectCategory.Other;
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

function TMapObjectType.HasFilters: Boolean;
begin
  Result := FFilters.Count <> 0;
end;

{ TMapObjectTemplate }

procedure TMapObjectTemplate.SetAnimation(AValue: AnsiString);
begin
  FAnimation:=NormalizeResourceName(AValue);
end;

procedure TMapObjectTemplate.SetEditorAnimation(AValue: AnsiString);
begin
  FEditorAnimation := NormalizeResourceName(AValue);
end;

procedure TMapObjectTemplate.UpdateAnimation;
begin
  if FEditorAnimation='' then
  begin
    FDef := RootManager.GraphicsManager.GetGraphics(FAnimation);
  end
  else
  begin
    FDef := RootManager.GraphicsManager.GetGraphics(FEditorAnimation);
  end;

  FIconSpriteIndex:=0;

  if MapObjectGroup.IsHero and (FEditorAnimation='') then
  begin
    RootManager.GraphicsManager.LoadGraphics(FDef);
    FIconSpriteIndex := 2;
  end;
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
  FIconSpriteIndex:=0;
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
  AState.SetTranslation(Ax, Ay);

  FDef.RenderIcon(AState, FIconSpriteIndex, dim, color);

  h := FDef.Height;

  if (color <> TPlayer.none) and FMapObjectGroup.IsHeroLike then
  begin
    RootManager.GraphicsManager.GetHeroFlagDef(color).RenderOverlayIcon(AState, dim, h);
  end;
end;

procedure TMapObjectTemplate.RenderFloating(AState: TLocalState; AX, AY: integer; color: TPlayer);
begin
  Def.RenderO(AState, FIconSpriteIndex, AX, AY, color);

  if (color <> TPlayer.none) and FMapObjectGroup.IsHeroLike then
  begin
    RootManager.GraphicsManager.GetHeroFlagDef(color).RenderO(AState, 0, AX, AY);
  end;
end;

procedure TMapObjectTemplate.BeforeSerialize(Sender: TObject);
begin

end;

procedure TMapObjectTemplate.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TMapObjectTemplate.BeforeDeSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TMapObjectTemplate.AfterDeSerialize(Sender: TObject; AData: TJSONData);
begin
  UpdateAnimation;
end;

{ TLegacyObjTemplate }

constructor TLegacyObjTemplate.Create;
begin
  inherited;
end;

{ TObjectsManager }

constructor TObjectsManager.Create(AOwner: TComponent);
var
  index: TObjectCategory;
begin
  inherited Create(AOwner);

  FMapObjectGroups := TMapObjectGroups.Create;
  FLegacyObjTypes := TLegacyIdMap.Create;

  for index in TObjectCategory do
    FSearchIndexes[index] := TSearchIndex.Create();
end;

destructor TObjectsManager.Destroy;
var
  index: TSearchIndex;
begin
  for index in FSearchIndexes do
    index.Free;

  FLegacyObjTypes.Free;
  FMapObjectGroups.Free;
  inherited Destroy;
end;

procedure TObjectsManager.SetListsManager(AValue: TListsManager);
begin
  if FListsManager=AValue then Exit;
  FListsManager:=AValue;
end;

procedure TObjectsManager.FillWithAllObjects(ATarget: TMapObjectTemplateList; AFilter: TObjectGroupFilter);

  procedure CopyTemplates(ASource: TMapObjectType);
  var
    k: Integer;
    obj_template: TMapObjectTemplate;
  begin
    for k := 0 to ASource.Templates.count - 1 do
    begin
      obj_template := ASource.Templates[k];

      ATarget.Add(obj_template);

      Assert(Assigned(obj_template.MapObjectGroup));
      Assert(Assigned(obj_template.MapObjectType));
    end;
  end;

var
 i,j: Integer;
 obj_type: TMapObjectGroup;
 obj_subtype: TMapObjectType;
 obj_template1, obj_template2: TMapObjectTemplate;
begin
  ATarget.Clear;
  for i := 0 to FMapObjectGroups.Count - 1 do
  begin
    obj_type := FMapObjectGroups.Items[i];

    if not AFilter(obj_type) then
      Continue;

    for j := 0 to obj_type.Types.Count - 1 do
    begin
      obj_subtype := obj_type.Types[j];

      //TODO: replace stub with generic solution
      if {obj_subtype.HasFilters} obj_type.Identifier = TYPE_TOWN then
      begin
        obj_template1 := obj_subtype.Templates.FindItem('fort');
        if Assigned(obj_template1) then
        begin
          ATarget.Add(obj_template1);
        end;
        obj_template2 := obj_subtype.Templates.FindItem('village');
        if Assigned(obj_template2) then
        begin
          ATarget.Add(obj_template2);
        end;

        if not Assigned(obj_template1) and not Assigned(obj_template2) then
        begin
          CopyTemplates(obj_subtype);
        end;
      end
      else
      begin
        CopyTemplates(obj_subtype);
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

procedure TObjectsManager.SelectAll(ATarget: TObjectsSelection; ACategory: TObjectCategory);

  function filter(Aobject: TMapObjectGroup): Boolean;
  begin
    Result := Aobject.Category = ACategory;
  end;

begin
  ATarget.Clear;
  FillWithAllObjects(ATarget.FData, @filter);
end;

procedure TObjectsManager.SelectByKeywords(ATarget: TObjectsSelection; AInput: string; ACategory: TObjectCategory);
begin
  ATarget.Clear;

  AInput := UTF8Trim(UTF8LowerCase(AInput));

  if AInput = '' then
  begin
    SelectAll(ATarget, ACategory);
  end
  else
  begin
    FSearchIndexes[ACategory].Find(AInput, ATarget);
  end;
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
  idx: TObjectCategory;
begin
  for idx in TObjectCategory do
  begin
    BuildIndex(idx);
  end;
end;

procedure TObjectsManager.BuildIndex(ACategory: TObjectCategory);
  function filter_stub(AObject: TMapObjectGroup): Boolean;
  begin
    Result := AObject.Category = ACategory;
  end;
var
  idx: SizeInt;
  s: string;
  keywords: TStringList;
  obj_type: TMapObjectGroup;
  obj_subtype: TMapObjectType;
  obj: TMapObjectTemplate;

  FAllTemplates: TMapObjectTemplateList;
begin
  FAllTemplates := TMapObjectTemplateList.Create(false);

  FillWithAllObjects(FAllTemplates, @filter_stub);

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
      keywords.Add(obj.Meta);
    end;

    obj_type := obj.MapObjectGroup;
    if obj_type.Name <> '' then
      keywords.Add(obj_type.Name)
    else
      keywords.Add(obj_type.Identifier);

    obj_subtype := obj.MapObjectType;
    if obj_subtype.Name <> '' then
      keywords.Add(obj_subtype.Name)
    else
      keywords.Add(obj_subtype.Identifier);

    FSearchIndexes[ACategory].AddToIndex(keywords, obj);
  end;
  keywords.Free;
  FAllTemplates.Free;
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

