{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit vcmi_json;

{$I compilersetup.inc}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fpjson, fgl, RegExpr, vcmi_fpjsonrtti, typinfo, filesystem_base,
  editor_classes, editor_types, editor_rtti;

type
  TSubstMap = specialize TFPGMap<TJSONStringType,TJSONStringType>;

  { TVCMIJsonString }

  TVCMIJsonString = class (TJSONString)
  private
    FMeta: AnsiString;
    procedure SetMeta(AValue: AnsiString);
  public
    function Clone: TJSONData; override;
    property Meta: AnsiString read FMeta write SetMeta;
  end;

  { TVCMIJsonObject }

  TVCMIJsonObject = class(TJSONObject)
  private
    FMergeOverride: Boolean;
    FMeta: AnsiString;
    procedure SetMergeOverride(AValue: Boolean);
  public
     function Clone: TJSONData; override;
     procedure SetMeta(AValue: AnsiString; Recursive: Boolean = true);
     property Meta: AnsiString read FMeta;

     procedure InheritFrom(ABase:TVCMIJsonObject);

     property MergeOverride: Boolean read FMergeOverride write SetMergeOverride;
  end;

  { TVCMIJsonArray }

  TVCMIJsonArray = class(TJSONArray)
  private
    FMeta: AnsiString;
  public
    function Clone: TJSONData; override;
    procedure SetMeta(AValue: AnsiString; Recursive: Boolean = true);
    property Meta: AnsiString read FMeta;
  end;

  { TVCMIJSONDestreamer }

  TStreamerObjectEvent = procedure (const JSON: TJSONObject; AObject: TObject) of object;

  TVCMIJSONDestreamer = class (TJSONDeStreamer)
  private
    procedure DestreamEmbeddedValue(ASrc: TJSONData; AObject: TObject);
    procedure DestreamCollectionItem(ACollection: TCollection; ASrc: TJSONData; AItem: TCollectionItem);

    procedure CollectionObjCallback(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

    procedure CollectionArrayCallback(Item: TJSONData; Data: TObject; var Continue: Boolean);

    procedure PostProcessTags(AObject: TVCMIJsonObject);
  protected
    procedure DoPreparePropName(var PropName: AnsiString); override;
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;  PropData: TJSONData); override;

    //preprocess comments, postprocess tags
    function ObjectFromString(const JSON: TJSONStringType): TJSONData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure DoDeStreamCollection(Const JSON : TJSONData; ACollection : TCollection);

    Procedure JSONToCollection(Const JSON : TJSONData; ACollection : TCollection); override;
    procedure JSONToObjectEx(const JSON: TJSONData; AObject: TObject); override;

    procedure JSONStreamToObject(AStream: TStream; AObject: TObject; AName: string);

    function JSONStreamToJson(AStream: TStream): TJSONData;
    function JSONStreamToJSONObject(AStream: TStream; AName: string): TJSONObject;
  end;

  { TVCMIJSONStreamer }

  TVCMIJSONStreamer = class (TJSONStreamer)
  private
    function EmbeddedValueToJson(Aobject: TObject): TJSONData;

  protected
    procedure DoBeforeStreamProperty(const AObject: TObject;
      PropertyInfo: PPropInfo; var Skip: boolean); override;
    procedure DoPreparePropName(var PropName: AnsiString); override;

  public
    constructor Create(AOwner: TComponent); override;

    function ObjectToJsonEx(const AObject: TObject): TJSONData; override;

    function StreamCollection(const ACollection: TCollection): TJSONData;
      override;

    function DoStreamCollection(const ACollection: TCollection): TJSONData;
  end;

  ISerializeSpecial = interface ['ISerializeSpecial']
     function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
     procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);
  end;

  { TJsonResource }

  TJsonResource = class (TBaseResource, IResource)
  private
    FRoot: TJSONObject;
    destreamer: TVCMIJSONDestreamer;
  public
    constructor Create(APath: AnsiString);
    destructor Destroy; override;
    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;
    property Root: TJSONObject read FRoot;

    procedure DestreamTo(AObject: TObject; AFieldName: string = '');
  end;


  { TJsonCombinedResource }

  TJsonCombinedResource = class (TJsonResource)
  public
    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;
    procedure Load(ALoader: IResourceLoader); override;
    function TryLoad(ALoader: IResourceLoader): Boolean; override;
  end;

  TJsonObjectList = specialize TFPGObjectList<TJSONObject>;


  { TModdedConfig }

  TModdedConfig = class
  private
    FConfig: TVCMIJSONObject;
    FModId: TModId;
    FPatches: TVCMIJSONObject;
    procedure SetModId(AValue: TModId);
  public
    constructor Create;
    destructor Destroy; override;
    property ModId: TModId read FModId write SetModId;
    property Config: TVCMIJSONObject read FConfig;
    property Patches: TVCMIJSONObject read FPatches;
  end;

  { TModdedConfigs }

  TModdedConfigs = class
  strict private
     type
       TMap = specialize TObjectMap<TModId, TModdedConfig>;
     var
       FMap: TMap;

    procedure Preload(AProgess: IProgressCallback; APaths: TModdedConfigPaths; ALoader:IResourceLoader);
    procedure ExtractPatches;
    procedure ApplyPatches;
    procedure CombineTo(ADest: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(AProgess: IProgressCallback; APaths: TModdedConfigPaths; ALoader:IResourceLoader; ADest: TJSONObject);
  end;

  { TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetOrCreateObject(AName: AnsiString): TJSONObject;
    procedure Assign(AValue: TJSONObject);
  end;

  procedure MergeJson(ASrc: TJSONData; ADest: TJSONData);

  procedure ParseObjectId(AID: AnsiString; out AModId: AnsiString; out AObjectId: AnsiString);

  //serialise helpers

  procedure SaveHeroSex(ADest: TJSONData; AValue: THeroSex);
  function LoadHeroSex(ASrc: TJSONData):THeroSex;

implementation

uses
  LazLoggerBase, editor_consts, editor_utils, rttiutils, types;

var
  rexp_oid: TRegExpr;
  rexp_tags: TRegExpr;

procedure MergeJsonStruct(ASrc: TVCMIJsonObject; ADest: TVCMIJsonObject; AllowOverride: Boolean); forward;
procedure MergeJsonStruct(ASrc: TVCMIJsonArray; ADest: TVCMIJsonArray); forward;

procedure DoSetMeta(ATarget: TJSONData; AValue: AnsiString);
begin
  case ATarget.JSONType of
    jtString: TVCMIJsonString(ATarget).SetMeta(AValue);
    jtObject: TVCMIJsonObject(ATarget).SetMeta(AValue,True);
    jtArray: TVCMIJsonArray(ATarget).SetMeta(AValue,True);
  end;
end;

procedure MakeCamelCase(var s:string);
var
  c: char;
begin
  c := s[1];
  c := lowerCase(c);
  s[1] := c;
end;

procedure MergeJson(ASrc: TJSONData; ADest: TJSONData);
begin
  if(ASrc.JSONType = jtNull) then
  begin
    ADest.Clear;
    exit;
  end;

  if not (ASrc.JSONType = ADest.JSONType) then
  begin
    raise EJSON.CreateFmt('Incompatible JSON values %s -> %s',[JSONTypeName(ASrc.JSONType), JSONTypeName(ADest.JSONType)]);
  end;

  case ASrc.JSONType of
    jtNull: assert(false);
    jtArray: MergeJsonStruct(ASrc as TVCMIJsonArray, ADest as TVCMIJsonArray) ;
    jtBoolean: ADest.AsBoolean := ASrc.AsBoolean ;
    jtNumber:begin
      case TJSONNumber(ASrc).NumberType of
        ntFloat: ADest.AsFloat := ASrc.AsFloat ;
        ntInt64: ADest.AsInt64 := ASrc.AsInt64;
        ntInteger: ADest.AsInteger := ASrc.AsInteger;
      end;
    end;
    jtObject:MergeJsonStruct(ASrc as TVCMIJsonObject, ADest as TVCMIJsonObject, true) ;
    jtString:ADest.AsString := ASrc.AsString;
  else
    begin
      raise EJSON.Create('Unknown JSON type');
    end;
  end;
end;

procedure MergeJsonStruct(ASrc: TVCMIJsonObject; ADest: TVCMIJsonObject; AllowOverride: Boolean);
var
  src_idx, dest_idx: Integer;
  name: TJSONStringType;
begin
  if AllowOverride and ASrc.MergeOverride then
  begin
    ADest.Clear;
    src_idx := ASrc.Count - 1;

    while src_idx >= 0 do
    begin
      name := ASrc.Names[src_idx];
      ADest.Add(name, ASrc.Extract(src_idx));
      dec(src_idx);
    end;
  end
  else
  begin
    src_idx := ASrc.Count - 1;

      while src_idx >= 0 do
      begin
        name := ASrc.Names[src_idx];
        dest_idx := ADest.IndexOfName(name);

        if dest_idx >=0 then
        begin
          if ADest.Types[name] = jtNull then
          begin
            ADest.Delete(dest_idx);
            ADest.Add(name, ASrc.Extract(src_idx));
          end
          else if Asrc.Types[name] = jtNull then
          begin
            ADest.Delete(dest_idx);
          end
          else
          begin
            MergeJson(ASrc.Items[src_idx],ADest.Items[dest_idx]);
          end;
        end
        else
        begin
          ADest.Add(name, ASrc.Extract(src_idx));
        end;
        dec(src_idx);
      end;
  end;
end;

procedure MergeJsonStruct(ASrc: TVCMIJsonArray; ADest: TVCMIJsonArray);
var
  idx: SizeInt;
begin
  if ASrc.Count = 0 then
    exit;

  ADest.Clear;

  idx := ASrc.Count - 1;

  while idx >= 0 do
  begin
    ADest.Insert(0, ASrc.Extract(idx));
    dec(idx);
  end;
end;

procedure ParseObjectId(AID: AnsiString; out AModId: AnsiString; out
  AObjectId: AnsiString);
begin
  if not rexp_oid.Exec(AID) then
  begin
    raise EConfigurationError.CreateFmt('Invalid object ID %s',[AID]);
  end;

  AModId:=rexp_oid.Match[2];
  AObjectId:=rexp_oid.Match[3];
  Assert(Length(AObjectId)>0,'ObjectId is empty');
end;

procedure SaveHeroSex(ADest: TJSONData; AValue: THeroSex);
var
  o: TJSONObject;
begin
  o := ADest as TJSONObject;

  case AValue of
    //THeroSex.default:   o.Add('female');
    THeroSex.male:   o.Add('female', false);
    THeroSex.female:   o.Add('female', True);
  end;
end;

function LoadHeroSex(ASrc: TJSONData): THeroSex;
var
  o: TJSONObject;
  idx: Integer;
begin
  Result :=  THeroSex.default;
  o := ASrc as TJSONObject;

  idx := o.IndexOfName('female');

  if idx > -1 then
  begin
    case o.Get('female', false) of
      false: Result :=  THeroSex.male;
      true: Result :=  THeroSex.female;
    end;
  end;

end;

{ TJsonCombinedResource }

procedure TJsonCombinedResource.LoadFromStream(AFileName: AnsiString;  AStream: TStream);
var
  current: TJSONObject;
begin
  if not Assigned(FRoot) then
  begin
    FRoot := CreateJSONObject([]);
  end;

  current := destreamer.JSONStreamToJSONObject(AStream,'');
  try
    MergeJson(current, Root);
  finally
    current.Free;
  end;
end;

procedure TJsonCombinedResource.Load(ALoader: IResourceLoader);
begin
  ALoader.LoadResourceCombined(Self, Typ, Path);
end;

function TJsonCombinedResource.TryLoad(ALoader: IResourceLoader): Boolean;
begin
  Result:= ALoader.ExistsResource(Self.Typ, Self.Path);

  if Result then
  begin
    Load(ALoader);
  end;
end;

{ TVCMIJsonArray }

function TVCMIJsonArray.Clone: TJSONData;
begin
  Result:=inherited Clone;

  TVCMIJsonArray(Result).SetMeta(Meta, False);
end;

procedure TVCMIJsonArray.SetMeta(AValue: AnsiString; Recursive: Boolean);
var
  iter: TJSONEnum;
begin
  FMeta:=AValue;
  if Recursive then
    for iter in self do
    begin
      DoSetMeta(iter.Value, AValue);
    end;
end;

{ TVCMIJsonObject }

procedure TVCMIJsonObject.SetMergeOverride(AValue: Boolean);
begin
  if FMergeOverride=AValue then Exit;
  FMergeOverride:=AValue;
end;

function TVCMIJsonObject.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TVCMIJsonObject(Result).SetMeta(Meta, False);
end;

procedure TVCMIJsonObject.SetMeta(AValue: AnsiString; Recursive: Boolean);
var
  iter: TJSONEnum;
begin
  FMeta:=AValue;

  if Recursive then
    for iter in self do
    begin
      DoSetMeta(iter.Value, AValue);
    end;
end;

procedure TVCMIJsonObject.InheritFrom(ABase: TVCMIJsonObject);
var
  temp: TVCMIJsonObject;
  s, old_meta: String;
begin
  old_meta := Meta;
  temp := ABase.Clone as TVCMIJsonObject;

  MergeJsonStruct(Self, temp, false);
  Clear;

  while temp.Count > 0 do
  begin
    s := temp.Names[0];
    Self.Add(s, temp.Extract(0));
  end;

  temp.Free;

  SetMeta(old_meta, True);
end;

{ TVCMIJsonString }

procedure TVCMIJsonString.SetMeta(AValue: AnsiString);
begin
  if FMeta=AValue then Exit;
  FMeta:=AValue;
end;

function TVCMIJsonString.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TVCMIJsonString(Result).Meta:=Meta;
end;

{ TJSONObjectHelper }

function TJSONObjectHelper.GetOrCreateObject(AName: AnsiString): TJSONObject;
var
  idx: SizeInt;
begin
  idx := IndexofName(AName);

  if idx < 0 then
  begin
    Result := CreateJSONObject([]);
    add(AName, Result);
  end
  else begin
    Result := Objects[AName];
  end;
end;


procedure TJSONObjectHelper.Assign(AValue: TJSONObject);
var
  iter: TJSONEnum;
begin
  if (Self is TVCMIJsonObject) and (AValue is TVCMIJsonObject) then
  begin
    TVCMIJsonObject(Self).FMeta:=TVCMIJsonObject(AValue).Meta;
  end;

  Self.Clear;

  for iter in AValue do
  begin
    Self.Add(iter.Key, iter.Value.Clone);
  end;
end;

{ TJsonResource }

constructor TJsonResource.Create(APath: AnsiString);
begin
  inherited Create(TResourceType.Json, APath);
  destreamer := TVCMIJSONDestreamer.Create(nil);
end;

procedure TJsonResource.DestreamTo(AObject: TObject; AFieldName: string);
begin
  if AFieldName = '' then
  begin
    destreamer.JSONToObject(FRoot,AObject);
  end
  else begin
    destreamer.JSONToObject(FRoot.Objects[AFieldName],AObject);
  end;
end;

destructor TJsonResource.Destroy;
begin
  FreeAndNil(FRoot);
  destreamer.Free;
end;

procedure TJsonResource.LoadFromStream(AFileName: AnsiString; AStream: TStream);
begin
  FreeAndNil(FRoot);
  FRoot := destreamer.JSONStreamToJSONObject(AStream,'');

  Assert(FRoot is TVCMIJsonObject);
end;

{ TVCMIJSONStreamer }

constructor TVCMIJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TVCMIJSONStreamer.ObjectToJsonEx(const AObject: TObject): TJSONData;
begin
  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).BeforeSerialize(Self);
  end;

  if AObject is ISerializeSpecial then
  begin
    Result := (AObject as ISerializeSpecial).Serialize(Self);
  end
  else if AObject is IEmbeddedValue then
  begin
    Result := EmbeddedValueToJson(AObject);
  end
  else begin
    Result := ObjectToJSON(AObject);
  end;

  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).AfterSerialize(Self, Result);
  end;
end;

function TVCMIJSONStreamer.EmbeddedValueToJson(Aobject: TObject): TJSONData;
var
  PIL: TPropInfoList;
  saved: Boolean;
  I: Integer;
begin
  PIL:=TPropInfoList.Create(AObject,tkProperties);

  saved:=false;

  try
    For I:=0 to PIL.Count-1 do
      begin
        if NeedToStreamProperty(AObject, PIL.Items[i]) then
        begin

          if saved then
          begin
            Error('Invalid embedded value');
          end;

          Result:=StreamProperty(AObject,PIL.Items[i]);
          saved:=true;
        end;
      end;
  finally
    FReeAndNil(Pil);
  end;
end;

procedure TVCMIJSONStreamer.DoBeforeStreamProperty(const AObject: TObject;
  PropertyInfo: PPropInfo; var Skip: boolean);
begin
  inherited DoBeforeStreamProperty(AObject, PropertyInfo, Skip);

  if not IsStoredProp(AObject,PropertyInfo) then
  begin
    Skip := True;
    Exit;
  end;

  Skip := IsDefaultValue(AObject, PropertyInfo);
end;

procedure TVCMIJSONStreamer.DoPreparePropName(var PropName: AnsiString);
begin
  MakeCamelCase(PropName);
end;

function TVCMIJSONStreamer.StreamCollection(const ACollection: TCollection
  ): TJSONData;
begin
  if ACollection is ISerializeNotify then
  begin
    (ACollection as ISerializeNotify).BeforeSerialize(Self);
  end;

  if ACollection is ISerializeSpecial then
  begin
    result := (ACollection as ISerializeSpecial).Serialize(Self);
  end
  else
    Result := DoStreamCollection(ACollection);

  if ACollection is ISerializeNotify then
  begin
    (ACollection as ISerializeNotify).AfterSerialize(Self, Result);
  end;
end;

function TVCMIJSONStreamer.DoStreamCollection(const ACollection: TCollection
  ): TJSONData;
var
  O: TJSONObject;
  A: TJSONArray;
  elem: TCollectionItem;
begin
  if ACollection is INamedCollection then
  begin
    o := CreateJSONObject([]);
    for elem in ACollection do
    begin
      o.Add(TNamedCollectionItem(Elem).Identifier, ObjectToJsonEx(elem));
    end;
    result := o;
  end
  else if ACollection is IArrayCollection then
  begin
    a:=CreateJSONArray([]);
    for elem in ACollection do
    begin
      a.Add(ObjectToJsonEx(elem));
    end;
    result := a;
  end
  else begin
    Result := inherited StreamCollection(ACollection);
  end;
end;

{ TVCMIJSONDestreamer }

procedure TVCMIJSONDestreamer.CollectionArrayCallback(Item: TJSONData;
  Data: TObject; var Continue: Boolean);
var
  ACollection: TCollection;
  new_item: TCollectionItem;
begin
  ACollection:=TCollection(Data);

  new_item := ACollection.Add;

  DestreamCollectionItem(ACollection, Item, new_item);

  Continue := True;
end;

procedure TVCMIJSONDestreamer.PostProcessTags(AObject: TVCMIJsonObject);
var
  substitution_map: TSubstMap;

  iter: TJSONEnum;
  key: TJSONStringType;
  parts: TStrings;

  i: Integer;
  merge_override: Boolean;
  data: TJSONData;
  old_key, new_key: String;

  procedure EnsureMap();
  begin
    if not Assigned(substitution_map) then
    begin
      substitution_map := TSubstMap.Create;
    end;
  end;
begin
  substitution_map := nil;
  parts := TStringList.Create;

  for iter in AObject do
  begin
    key := iter.Key;
    parts.Clear;
    rexp_tags.Split(key, parts);

    if parts.Count > 1 then
    begin
      EnsureMap();
      key := parts[0];

      merge_override := false;

      for i := 1 to parts.Count - 1 do
      begin
        if parts[i] = 'override' then
        begin
          merge_override := true;
        end;
      end;

      if merge_override then
      begin
        data := iter.Value;
        (data as TVCMIJsonObject).MergeOverride:=true;
        substitution_map.Add(iter.Key, key);
      end;
    end;
  end;

  if Assigned(substitution_map) then
  begin
    for i := 0 to Pred(substitution_map.Count) do
    begin
      old_key := substitution_map.Keys[i];
      new_key := substitution_map.Data[i];

      data := AObject.Extract(old_key);
      AObject.Add(new_key, data);
    end;

    FreeAndNil(substitution_map);
  end;

  for iter in AObject do
  begin
    if iter.Value is TVCMIJsonObject then
    begin
      PostProcessTags(TVCMIJsonObject(iter.Value));
    end;
  end;

  FreeAndNil(parts);

end;

procedure TVCMIJSONDestreamer.DestreamEmbeddedValue(ASrc: TJSONData;
  AObject: TObject);
var
  PIL: TPropInfoList;

  info: PPropInfo;
begin
  PIL:=TPropInfoList.Create(AObject,tkProperties);

  if PIL.Count <> 1 then
  begin
    Error('Invallid embedded value');
  end;

  try
      info := PIL[0];

      RestoreProperty(AObject, info, ASrc);
  finally
    FReeAndNil(Pil);
  end;
end;

procedure TVCMIJSONDestreamer.DestreamCollectionItem(ACollection: TCollection;
  ASrc: TJSONData; AItem: TCollectionItem);
var
  O: TJSONObject;
begin
  if AItem is IEmbeddedValue then
  begin
    DestreamEmbeddedValue(ASrc, AItem);
  end
  else if AItem is ISerializeSpecial then
  begin
    (AItem as ISerializeSpecial).Deserialize(Self, ASrc);
  end
  else if ASrc.JSONType in [jtObject]then
  begin
    O := TJSONObject(ASrc);

    JSONToObjectEx(o,AItem);
  end
  else if ASrc.JSONType in [jtArray] then
  begin
    if AItem is IEmbeddedCollection then
    begin
      JSONToCollection(ASrc,(AItem as IEmbeddedCollection).GetCollection);
    end
    else
      Error('Not supported collection element type for item');
  end
  else if ASrc.JSONType in [jtString] then
  begin
    SetStrProp(AItem, 'Value', ASrc.AsString);
  end
  else begin
    Error('Not supported collection element type for item');
  end;
end;

procedure TVCMIJSONDestreamer.CollectionObjCallback(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var Continue: Boolean);
var
  ACollection: THashedCollection;
  new_item: TNamedCollectionItem;
  meta: string;
begin
  ACollection:=THashedCollection(Data);

  if item.JSONType <> jtNull then
  begin
    new_item := TNamedCollectionItem(ACollection.Add);

    if {new_item.UseMeta and} (item is TVCMIJsonObject) then
    begin
      meta := TVCMIJsonObject(Item).Meta;
      new_item.Meta:=Meta;
    end
    else if {new_item.UseMeta and} (item is TVCMIJsonArray) then
    begin
      meta := TVCMIJsonArray(Item).Meta;
      new_item.Meta:=Meta;
    end
    else
    begin
      new_item.Identifier := AName;
    end;

    if new_item.UseMeta and (meta <> '') then
    begin
      new_item.Identifier := NormalizeIndentifier(EncodeIdentifier(Meta, AName));
    end
    else
    begin
      new_item.Identifier := NormalizeIndentifier(AName);
    end;

    DestreamCollectionItem(ACollection, Item, new_item);
  end;

  Continue := True;
end;

constructor TVCMIJSONDestreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TVCMIJSONDestreamer.Destroy;
begin
  inherited Destroy;
end;

procedure TVCMIJSONDestreamer.DoDeStreamCollection(const JSON: TJSONData;
  ACollection: TCollection);
var
  O : TJSONObject;
  A: TJSONArray;
begin
  if (json.JSONType = jtNull) then
  begin
    ACollection.Clear;
  end
  else if (JSON.JSONType = jtObject) and (ACollection is INamedCollection) then
  begin
    ACollection.Clear;

    O := JSON as TJSONObject;
    O.Iterate(@CollectionObjCallback,ACollection);
  end
  else if (JSON.JSONType = jtArray) and (ACollection is IArrayCollection) then
  begin
    ACollection.Clear;
    A := JSON as TJSONArray;

    A.Iterate(@CollectionArrayCallback,ACollection);
  end
  else
  begin
    inherited JSONToCollection(JSON, ACollection);
  end;
end;

procedure TVCMIJSONDestreamer.DoPreparePropName(var PropName: AnsiString);
begin
  MakeCamelCase(PropName);
end;

procedure TVCMIJSONDestreamer.DoRestoreProperty(AObject: TObject;
  PropInfo: PPropInfo; PropData: TJSONData);
Var
  PI : PPropInfo;
  TI : PTypeInfo;

  str_data : TVCMIJsonString;

  value: TJSONStringType;
  //idx: SizeInt;
  //local_scope: TJSONStringType;
begin
  PI:=PropInfo;
  TI:=PropInfo^.PropType;

  if (TI = TypeInfo(TIdentifier)) then
  begin
    value := '';
    //local_scope := '';

    if Assigned(PropData) and (PropData is TVCMIJsonString) then
    begin
      str_data := TVCMIJsonString(PropData);
      value := str_data.AsString;
      //local_scope := str_data.Meta;
    end;

    //todo: may be move identifier resolving here

    //if local_scope <> '' then
    //begin
    //  DebugLn(['Loading ident: ', value, '; local scope: ', local_scope]);
    //
    //  idx := pos(':', value);
    //
    //  if idx = 0 then
    //  begin
    //    value := local_scope +':'+value;//todo: remove
    //  end;
    //end;

    SetStrProp(AObject,PI,value);
  end
  else if not Assigned(PropData) then
    //SetDefault(AObject, PropInfo)
  else
  begin
    inherited DoRestoreProperty(AObject, PropInfo, PropData);
  end;
end;

function TVCMIJSONDestreamer.JSONStreamToJson(AStream: TStream): TJSONData;
var
  src: TStringStream;
begin
  src := TStringStream.Create('');
  src.Size := AStream.Size;
  src.Seek(0,soBeginning);
  try
    src.CopyFrom(AStream,0);
    Result := ObjectFromString(src.DataString);
  finally
    src.Free;
  end;

end;

function TVCMIJSONDestreamer.JSONStreamToJSONObject(AStream: TStream; AName: string): TJSONObject;
var
  root: TJSONObject;
  dt, res_dt:TJSONData;
  free_dt: boolean;
begin
  dt := nil;
  free_dt := true;
  try
    dt := JSONStreamToJson(AStream);

    if not (dt.JSONType = jtObject) then
      Error('Root data shall be object');

    root := TJSONObject(dt);

    if AName = '' then
    begin
      //use root
      Result := TJSONObject(dt);
      free_dt := False;
    end
    else begin
      res_dt := root.Extract(AName);
      if not (res_dt.JSONType = jtObject) then
      begin
        res_dt.Free;
        Error('Property %s shall be object',[AName]);
      end;

      Result := TJSONObject(res_dt);
    end;



  finally
    if free_dt then
      dt.Free;
  end;

end;

procedure TVCMIJSONDestreamer.JSONStreamToObject(AStream: TStream;
  AObject: TObject; AName: string);
var
  o: TJSONObject;
begin
  o := JSONStreamToJSONObject(AStream,AName);
  try
    JSONToObject(o,AObject);

  finally
    o.Free;
  end;
end;

procedure TVCMIJSONDestreamer.JSONToCollection(const JSON: TJSONData;
  ACollection: TCollection);
begin
  if ACollection is ISerializeNotify then
  begin
    (ACollection as ISerializeNotify).BeforeDeSerialize(Self, JSON);
  end;

  if ACollection is ISerializeSpecial then
  begin
    (ACollection as ISerializeSpecial).Deserialize(Self, Json);
  end
  else
    DoDeStreamCollection(JSON, ACollection);

  if ACollection is ISerializeNotify then
  begin
    (ACollection as ISerializeNotify).AfterDeSerialize(Self, JSON);
  end;
end;

procedure TVCMIJSONDestreamer.JSONToObjectEx(const JSON: TJSONData;
  AObject: TObject);
begin
  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).BeforeDeSerialize(Self, JSON);
  end;

  if AObject is IEmbeddedCollection then
  begin
    JSONToCollection(JSON,(AObject as IEmbeddedCollection).GetCollection);
  end
  else if AObject is TJSONObject then
  begin
    (AObject as TJSONObject).Assign(Json as TJSONObject);
  end
  else if AObject is ISerializeSpecial then
  begin
    (AObject as ISerializeSpecial).Deserialize(Self, JSON);
  end
  else if JSON is TJSONObject then
  begin
    JSONToObject(TJSONObject(JSON), AObject);
  end;

  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).AfterDeSerialize(Self, JSON);
  end;
end;


function TVCMIJSONDestreamer.ObjectFromString(const JSON: TJSONStringType): TJSONData;
var
  prepase_buffer: TStringList;
  stm: TStringStream;

  s: AnsiString;
  commentpos: Integer;
  i: Integer;
  char_nomber: Integer;
  in_string: Boolean;
  may_be_comment: Boolean;
  len: Integer;
  result_o: TVCMIJsonObject;
begin
  stm := TStringStream.Create(JSON);
  prepase_buffer:= TStringList.Create;
  try
    stm.Seek(0,soBeginning);
    prepase_buffer.LoadFromStream(stm);

    for i := 0 to prepase_buffer.Count - 1 do
    begin
      s := prepase_buffer[i];

      char_nomber := 1;

      len := Length(s);

      in_string := false;
      may_be_comment := false;

      commentpos := 0;

      while char_nomber <= len do
      begin
        case s[char_nomber] of
          '/': begin
            if may_be_comment then
            begin
              if not in_string then
              begin
                commentpos := char_nomber - 1; //prev char
                break;
              end;
              may_be_comment := false;
            end
            else
            begin
              may_be_comment := true;
            end;


          end;
          '"': begin
            if in_string then
            begin
              in_string:=false;
            end
            else begin
              in_string:=true;
            end;

          end ;
        end;
        inc(char_nomber);
      end;

      //TODO: parse properly
      if (commentpos > 0) then
      begin
        prepase_buffer[i] := Copy(s,1,commentpos - 1);
      end;
    end;

    Result := inherited ObjectFromString(prepase_buffer.Text);

    if Result is TVCMIJsonObject then
    begin
      result_o := TVCMIJsonObject(Result);

      PostProcessTags(result_o);
    end;

  finally
    stm.Free;
    prepase_buffer.Free;
  end;
end;

{ TModdedConfig }

procedure TModdedConfig.SetModId(AValue: TModId);
begin
  if FModId=AValue then Exit;
  FModId:=AValue;
end;

constructor TModdedConfig.Create;
begin
  FConfig := TVCMIJSONObject.Create;
  FPatches := TVCMIJSONObject.Create;
end;

destructor TModdedConfig.Destroy;
begin
  FPatches.Free;
  FConfig.Free;
  inherited Destroy;
end;

{ TModdedConfigs }

constructor TModdedConfigs.Create;
begin
  inherited Create;
  FMap := TMap.Create;

  FMap.OnKeyCompare:=@CompareStr; //todo: compare by mod load priority
  FMap.Sorted:=True;
  FMap.Duplicates:=dupError;
end;

destructor TModdedConfigs.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure TModdedConfigs.Preload(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths; ALoader: IResourceLoader);
var
  AModdedPath: TModdedConfigPath;
  i: SizeInt;

  current: TJsonResource;
  APath, s: String;
  item: TModdedConfig;

  meta: TJSONStringType;
begin
  for i := 0 to SizeInt(APaths.Size) - 1 do
  begin
    AModdedPath := APaths.Items[i];

    item := TModdedConfig.Create;

    item.ModId:=AModdedPath.ModID;

    FMap.Add(item.ModId,item);

    for APath in AModdedPath.Config do
    begin
      current := TJsonResource.Create(APath);

      try
         current.Load(ALoader);
         MergeJson(current.Root, item.Config);
      except
        on e: Exception do
        begin
          s := 'Error loading file '+APath +' '+e.Message;
          AProgess.AddError(s);
        end;
      end;

      current.Free;
    end;

    if AModdedPath.ModID = MODID_CORE then
      meta := ''
    else
      meta := AModdedPath.ModID;

    item.Config.SetMeta(meta,true);
  end;
end;

procedure TModdedConfigs.ExtractPatches;

   procedure MayBeSetFullID(AConfig: TJSONObject; AFullKey, AId: AnsiString; AModID: TModId);
   var
     new_key: AnsiString;
   begin
     if AModID = MODID_CORE then
     begin
       new_key := AId;
     end
     else
     begin
       new_key:= AModID+':'+AId;
     end;

     if new_key<>AFullKey then
     begin
       AConfig.Add(new_key,AConfig.Extract(AFullKey));
     end;

   end;

var
  i,other_mod_index: Integer;
  mod_id, other_mod_id: TModId;
  mod_data, other_mod_data : TModdedConfig;
  object_data: TJSONEnum;
  id: AnsiString;
  config: TVCMIJSONObject;

  all_fields: TStringDynArray;
  key: TJSONStringType;
begin
  for i := 0 to FMap.Count - 1 do
  begin
    mod_id := FMap.Keys[i];
    mod_data:= FMap.Data[i];

    SetLength(all_fields, mod_data.Config.Count);

    for object_data in mod_data.Config do
    begin
      all_fields[object_data.KeyNum] := object_data.Key;
    end;

    for key in all_fields do
    begin
      ParseObjectId(Key, other_mod_id, id);

      if other_mod_id = mod_id then
      begin
        DebugLn(['Redundant namespace definition for "',Key,'"']); //this is error, need to report
        MayBeSetFullID(mod_data.Config, Key, id, mod_id);
      end
      else if (other_mod_id='') then
      begin
        MayBeSetFullID(mod_data.Config, Key, id, mod_id);
      end
      else
      begin
        //this is a patch

        config := mod_data.Config.Extract(key) as TVCMIJSONObject;

        if not FMap.Find(other_mod_id, other_mod_index) then
        begin
          DebugLn(['Mod not found "',other_mod_id,'"']);
          config.Free;
          Continue;
        end;

        other_mod_data := FMap.Data[other_mod_index];

        if other_mod_data.Patches.IndexOfName(id)>=0 then
        begin
          MergeJson(config,other_mod_data.Patches.Objects[id]);
          config.Free;
        end
        else begin
          other_mod_data.Patches.Objects[id] := config;
        end;
      end;
    end;
  end;
end;

procedure TModdedConfigs.ApplyPatches;
var
  mod_data: TModdedConfig;
  i: Integer;
begin
  for i := 0 to FMap.Count - 1 do
  begin
    mod_data:= FMap.Data[i];

    MergeJson(mod_data.Patches, mod_data.Config);
  end;
end;

procedure TModdedConfigs.CombineTo(ADest: TJSONObject);
var
  mod_data: TModdedConfig;
  i: Integer;
begin
  for i := 0 to FMap.Count - 1 do
  begin
    mod_data:= FMap.Data[i];

    MergeJson(mod_data.Config, ADest);
  end;
end;

procedure TModdedConfigs.Load(AProgess: IProgressCallback;
  APaths: TModdedConfigPaths; ALoader: IResourceLoader; ADest: TJSONObject);
begin
  Preload(AProgess, APaths, ALoader);
{
  ExtractPatches;
  ApplyPatches;
}
  CombineTo(ADest);
end;

initialization

  rexp_oid := TRegExpr.Create;
  rexp_oid.Expression := '^((.*):)?(.*)$';
  rexp_oid.Compile;

  rexp_tags := TRegExpr.Create;
  rexp_tags.Expression:= '#';
  rexp_tags.Compile;

  SetJSONInstanceType(TJSONInstanceType.jitObject, TVCMIJsonObject);
  SetJSONInstanceType(TJSONInstanceType.jitString, TVCMIJsonString);
  SetJSONInstanceType(TJSONInstanceType.jitArray,  TVCMIJsonArray);

finalization

  rexp_oid.Free;
  rexp_tags.Free;

end.

