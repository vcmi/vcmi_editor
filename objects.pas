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
unit objects;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, FileUtil, fpjson, editor_types,
  filesystem_base, editor_graphics, editor_classes, h3_txt,
  lists_manager, vcmi_json;

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

  TObjSubType = class;
  TObjType = class;

  { TObjTemplate }

  TObjTemplate = class (TNamedCollectionItem)
  private
    FDef: TDef;

    FObjType:TObjType;
    FObjSubtype: TObjSubType;

  strict private
    FAllowedTerrains: TTerrainTypes;
    FAnimation: AnsiString;
    FEditorAnimation: AnsiString;
    FVisitableFrom: TStringList;
    FMask: TStringList;
    FzIndex: Integer;
    function GetMask: TStrings;
    function GetVisitableFrom: TStrings;
    procedure SetAllowedTerrains(AValue: TTerrainTypes);
    procedure SetAnimation(AValue: AnsiString);
    procedure SetEditorAnimation(AValue: AnsiString);
    procedure SetZIndex(AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Def: TDef read FDef;

    property ObjType: TObjType read FObjType;
    property ObjSubType: TObjSubType read FObjSubtype;

    class function UseMeta: boolean; override;
  published
    property Animation: AnsiString read FAnimation write SetAnimation;
    property EditorAnimation: AnsiString read FEditorAnimation write SetEditorAnimation;
    property VisitableFrom: TStrings read GetVisitableFrom;
    property AllowedTerrains: TTerrainTypes read FAllowedTerrains write SetAllowedTerrains default ALL_TERRAINS;
    property Mask: TStrings read GetMask;
    property ZIndex: Integer read FzIndex write SetzIndex default 0;
  end;

  { TObjTemplates }

  TObjTemplates = class (specialize TGNamedCollection<TObjTemplate>)
  private
    FObjSubType: TObjSubType;
  public
    constructor Create(AOwner: TObjSubType);
    property ObjSubType: TObjSubType read FObjSubType;
  end;

  TObjTemplatesList = specialize TFPGObjectList<TObjTemplate>;

  { TObjSubType }

  TObjSubType = class (TNamedCollectionItem)
  private
    FName: TLocalizedString;
    FNid: TCustomID;
    FTemplates: TObjTemplates;
    FObjType:TObjType;
    function GetIndexAsID: TCustomID;
    procedure SetIndexAsID(AValue: TCustomID);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property ObjType: TObjType read FObjType;

    class function UseMeta: boolean; override;
  published
    property Index: TCustomID read GetIndexAsID write SetIndexAsID default ID_INVALID;
    property Templates:TObjTemplates read FTemplates;
    property Name: TLocalizedString read FName write FName;
  end;

  { TObjSubTypes }

  TObjSubTypes = class (specialize TGNamedCollection<TObjSubType>)
  private
    FOwner: TObjType;
  public
    constructor Create(AOwner: TObjType);
    property owner: TObjType read FOwner;
  end;

  { TObjType }

  TObjType = class (TNamedCollectionItem)
  private
    FHandler: AnsiString;
    FName: TLocalizedString;
    FNid: TCustomID;
    FSubTypes: TObjSubTypes;
    procedure SetHandler(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    class function UseMeta: boolean; override;
  published
    property Index: TCustomID read FNid write FNid default ID_INVALID;
    property Types:TObjSubTypes read FSubTypes;
    property Name: TLocalizedString read FName write FName;
    property Handler: AnsiString read FHandler write SetHandler;
  end;

  TObjTypes = specialize TGNamedCollection<TObjType>;

  {$pop}

  TLegacyIdMap = specialize TObjectMap<TLegacyTemplateId, TObjSubType>;

  TObjectsManager = class;

  { TObjectsSelection }

  TObjectsSelection = class
  private
    FManager: TObjectsManager;
    FData: TObjTemplatesList;
    function GetCount: Integer;
    function GetObjcts(AIndex: Integer): TObjTemplate;
  public
    constructor Create(AManager: TObjectsManager);
    destructor Destroy; override;
    property Count:Integer read GetCount;
    property Objcts[AIndex: Integer]: TObjTemplate read GetObjcts;
  end;

  { TObjectsManager }

  TObjectsManager = class (TGraphicsCosnumer)
  strict private
    FObjTypes: TObjTypes;

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ListsManager:TListsManager read FListsManager write SetListsManager;

    procedure LoadObjects(AProgressCallback: IProgressCallback; APaths: TModdedConfigPaths);

    property ObjTypes: TObjTypes read FObjTypes;

    function SelectAll: TObjectsSelection;

    function ResolveLegacyID(Typ,SubType: uint32):TObjSubType;
  end;

implementation

uses
  LazLoggerBase, CsvDocument, editor_consts, editor_utils,
  root_manager, math;

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
  inherited Create(TResourceType.Mask, APath);
end;

procedure TMaskResource.LoadFromStream(AStream: TStream);
begin
  Clear;
  AStream.Read(FWidth,1);
  AStream.Read(FHeight,1);

  AStream.Read(FMask1,6);
  AStream.Read(FMask2,6);

end;

{ TObjSubTypes }

constructor TObjSubTypes.Create(AOwner: TObjType);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TObjTemplates }

constructor TObjTemplates.Create(AOwner: TObjSubType);
begin
  inherited Create;
  FObjSubType := AOwner;
end;

{ TObjectsSelection }

function TObjectsSelection.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TObjectsSelection.GetObjcts(AIndex: Integer): TObjTemplate;
begin
  Result := FData.Items[AIndex];
end;

constructor TObjectsSelection.Create(AManager: TObjectsManager);
begin
  FManager := AManager;
  FData := TObjTemplatesList.Create(False);
end;

destructor TObjectsSelection.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

{ TObjType }

procedure TObjType.SetHandler(AValue: AnsiString);
begin
  if FHandler=AValue then Exit;
  FHandler:=AValue;
end;

constructor TObjType.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSubTypes := TObjSubTypes.Create(self);
  Index:=ID_INVALID;
end;

destructor TObjType.Destroy;
begin
  FSubTypes.Free;
  inherited Destroy;
end;

class function TObjType.UseMeta: boolean;
begin
  Result:=true;
end;

{ TObjSubType }

function TObjSubType.GetIndexAsID: TCustomID;
begin
  Result := FNid;
end;

procedure TObjSubType.SetIndexAsID(AValue: TCustomID);
begin
  FNid := AValue;
end;

constructor TObjSubType.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  index := ID_INVALID;
  FTemplates := TObjTemplates.Create(Self);

  FObjType :=  (ACollection as TObjSubTypes).Owner;
end;

destructor TObjSubType.Destroy;
begin
  FTemplates.Free;
  inherited Destroy;
end;

class function TObjSubType.UseMeta: boolean;
begin
  Result:=true;
end;

{ TObjTemplate }

procedure TObjTemplate.SetAnimation(AValue: AnsiString);
begin
  AValue := NormalizeResourceName(AValue);
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;

  //use this animation only if there is no editor animation
  //todo: delay animation loading(load after serialize)

  if FEditorAnimation='' then
    FDef := root_manager.RootManager.GraphicsManager.GetGraphics(FAnimation);
end;

procedure TObjTemplate.SetEditorAnimation(AValue: AnsiString);
begin
  AValue := NormalizeResourceName(AValue);

  if FEditorAnimation=AValue then Exit;
  FEditorAnimation:=AValue;

  FDef := root_manager.RootManager.GraphicsManager.GetGraphics(FEditorAnimation);
end;

procedure TObjTemplate.SetZIndex(AValue: Integer);
begin
  if FzIndex=AValue then Exit;
  FzIndex:=AValue;
end;

function TObjTemplate.GetVisitableFrom: TStrings;
begin
  Result := FVisitableFrom;
end;

function TObjTemplate.GetMask: TStrings;
begin
  Result := FMask;
end;

procedure TObjTemplate.SetAllowedTerrains(AValue: TTerrainTypes);
begin
  if FAllowedTerrains=AValue then Exit;
  FAllowedTerrains:=AValue;
end;

constructor TObjTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVisitableFrom := TStringList.Create;
  FMask := TStringList.Create;

  AllowedTerrains := ALL_TERRAINS;

  FObjSubtype :=  (ACollection as TObjTemplates).ObjSubtype;

  FObjType := (ACollection as TObjTemplates).ObjSubtype.FObjType;
end;

destructor TObjTemplate.Destroy;
begin
  FMask.Free;
  FVisitableFrom.Free;
  inherited Destroy;
end;

class function TObjTemplate.UseMeta: boolean;
begin
  Result:=True;
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

  FObjTypes := TObjTypes.Create;
  FLegacyObjTypes := TLegacyIdMap.Create;
end;

destructor TObjectsManager.Destroy;
begin
  FLegacyObjTypes.Free;
  FObjTypes.Free;
  inherited Destroy;
end;

procedure TObjectsManager.SetListsManager(AValue: TListsManager);
begin
  if FListsManager=AValue then Exit;
  FListsManager:=AValue;
end;

procedure TObjectsManager.LoadObjects(AProgressCallback: IProgressCallback;
  APaths: TModdedConfigPaths);
var
  FConfig: TModdedConfigs;
  FCombinedConfig: TJSONObject;
  destreamer: TVCMIJSONDestreamer;

  FFullIdToDefMap: TLegacyObjConfigFullIdMap; //type,subtype => template list
  i: Integer;
  //lst: TLegacyObjConfigList;
  //item: TJSONObject;
begin
  FProgress := AProgressCallback;
  FFullIdToDefMap := TLegacyObjConfigFullIdMap.Create;
  LoadLegacy(AProgressCallback, FFullIdToDefMap);

  AProgressCallback.NextStage('Building objects configuration ... ');
  AProgressCallback.Max:=8;

  FConfig := TModdedConfigs.Create;
  FCombinedConfig := TJSONObject.Create;
  destreamer := TVCMIJSONDestreamer.Create(nil);
  destreamer.AfterReadObject := @OnObjectDestream;
  try
    FConfig.Load(APaths,ResourceLoader, FCombinedConfig);
    AddFactions(FCombinedConfig);
    AddHeroClasses(FCombinedConfig);
    AddCreatures(FCombinedConfig);
    AddArtifacts(FCombinedConfig);
    MergeLegacy(FCombinedConfig, FFullIdToDefMap);

    for i := 0 to FFullIdToDefMap.Count - 1 do
    begin
      //lst := FFullIdToDefMap.Data[i];
      DebugLn(['unused legacy data ', Hi(FFullIdToDefMap.Keys[i]), ' ' , Lo(FFullIdToDefMap.Keys[i])]);

      //for item in lst do
      //begin
      //  DebugLn(item.AsJSON);
      //end;
    end;

    HandleInteritance(FCombinedConfig);
    AProgressCallback.Advance(1);

    FProgress.Max:=FCombinedConfig.Count;
    AProgressCallback.NextStage('Loading objects ... ');
    destreamer.JSONToObject(FCombinedConfig,FObjTypes);

    PopulateMapOfLegacyObjects;
  finally
    FCombinedConfig.Free;
    FConfig.Free;
    destreamer.Free;

    FFullIdToDefMap.Free;
  end;
end;

function TObjectsManager.SelectAll: TObjectsSelection;
var
 i,j,k: Integer;
 obj_type: TObjType;
 obj_subtype: TObjSubType;
 obj_template: TObjTemplate;
begin
  Result := TObjectsSelection.Create(Self);

  for i := 0 to FObjTypes.Count - 1 do
  begin

    obj_type := FObjTypes.Items[i];

    for j := 0 to obj_type.Types.Count - 1 do
    begin
      obj_subtype := obj_type.Types[j];

      for k := 0 to obj_subtype.Templates.count - 1 do
      begin
        obj_template := obj_subtype.Templates[k];

        Result.FData.Add(obj_template);

        Assert(Assigned(obj_template.FObjType));
        Assert(Assigned(obj_template.FObjSubtype));
      end;
    end;
  end;
end;

function TObjectsManager.ResolveLegacyID(Typ, SubType: uint32): TObjSubType;
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
      i: Integer;
      j: Integer;

      ss: string;
      m: UInt8;
      s: string;
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
      len: Integer;
      i: Integer;
    begin
      temp := '';
      CellToStr(temp);
      len:= Length(temp);
      v := 0;
      for i := len to 1 do
      begin
        if temp[i] = '1' then
          v := v or 1 shl i;
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
    mask_conf, visit_conf: TJSONArray;
    //anim: TDef;
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

      //TODO: allowedTerrains

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
  obj_template: TJSONObject;
  base: TJSONObject;
begin
  DebugLn([#9,#9,AName]);
  obj_template := Item as TJSONObject;

  if Assigned(data) then
  begin
    base := data as TJSONObject;

    obj_template.InheritFrom(base);
  end;
end;

procedure TObjectsManager.HandleInteritanceObjectSubType(
  const AName: TJSONStringType; Item: TJSONData; Data: TObject;
  var Continue: Boolean);
var
  obj_subtype: TJSONObject;
  base: TJSONObject;
  idx: Integer;
begin
  DebugLn([#9,AName]);
  obj_subtype := Item as TJSONObject;

  if Assigned(data) then
  begin
    obj_subtype.InheritFrom(data as TJSONObject);
  end;

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
  //AName = object type id

  DebugLn([AName]);

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

    town_type := TJSONObject.Create;
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
    hc := TJSONObject.Create;
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

    cr := TJSONObject.Create;
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
    ar := TJSONObject.Create;
    if info.Index >= 0 then
      ar.Add('index', info.Index);

    info.Graphics.AddTemplates(ar);
    artifacts.Add(info.Identifier, ar);
  end;

end;

procedure TObjectsManager.PopulateMapOfLegacyObjects;
var
  i,j: SizeInt;
  obj_type: TObjType;
  obj_sub_type: TObjSubType;
  full_id: TLegacyTemplateId;
begin

  for i := 0 to FObjTypes.Count - 1 do
  begin

    obj_type := FObjTypes[i];

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
  if Assigned(FProgress) and (AObject is TObjType) then
  begin
    FProgress.Advance(1);
  end;
end;

end.

