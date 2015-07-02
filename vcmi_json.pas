{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
unit vcmi_json;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, contnrs, fpjson, fgl, RegExpr, vcmi_fpjsonrtti, typinfo, filesystem_base,
  editor_classes, editor_types;

type

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
    FMeta: AnsiString;
  public
     function Clone: TJSONData; override;
     procedure SetMeta(AValue: AnsiString; Recursive: Boolean = true);
     property Meta: AnsiString read FMeta;
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
    procedure DestreamCollectionItem(ACollection: TCollection; ASrc: TJSONData; AItem: TCollectionItem);

    procedure CollectionObjCallback(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

    procedure CollectionArrayCallback(Item: TJSONData; Data: TObject; var Continue: Boolean);
  protected
    procedure DoPreparePropName(var PropName: AnsiString); override;
    //preprocess comments
    function ObjectFromString(const JSON: TJSONStringType): TJSONData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure JSONToCollection(Const JSON : TJSONData; ACollection : TCollection); override;
    procedure JSONToObject(const JSON: TJSONObject; AObject: TObject); override;

    procedure JSONStreamToObject(AStream: TStream; AObject: TObject; AName: string);

    function JSONStreamToJson(AStream: TStream): TJSONData;
    function JSONStreamToJSONObject(AStream: TStream; AName: string): TJSONObject;

  end;

  { TVCMIJSONStreamer }

  TVCMIJSONStreamer = class (TJSONStreamer)
  protected
    procedure DoBeforeStreamProperty(const AObject: TObject;
      PropertyInfo: PPropInfo; var Skip: boolean); override;
    procedure DoPreparePropName(var PropName: AnsiString); override;
  public
    constructor Create(AOwner: TComponent); override;

    function ObjectToJSON(const AObject: TObject): TJSONObject; override;

    function ObjectToJsonEx(const AObject: TObject): TJSONData;

    function StreamCollection(const ACollection: TCollection): TJSONData;
      override;

  end;


  { TJsonResource }

  TJsonResource = class (TBaseResource,IResource)
  private
    FRoot: TJSONObject;
  public
    constructor Create(APath: AnsiString);
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream); override;
    property Root: TJSONObject read FRoot;

    procedure DestreamTo(AObject: TObject; AFieldName: string = '');
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
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure Preload(APaths: TModdedConfigPaths; ALoader:IResourceLoader);
    procedure ExtractPatches;
    procedure ApplyPatches;
    procedure CombineTo(ADest: TJSONObject);

  public
    procedure Load(APaths: TModdedConfigPaths; ALoader:IResourceLoader; ADest: TJSONObject);
  end;



  { TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetOrCreateObject(AName: AnsiString): TJSONObject;
    procedure InheritFrom(ABase:TJSONObject);
    procedure Assign(AValue: TJSONObject);
  end;



  procedure MergeJson(ASrc: TJSONData; ADest: TJSONData);
  procedure InheritJson(ABase: TJSONObject; ADest: TJSONObject);

  procedure MergeJsonStruct(ASrc: TJSONObject; ADest: TJSONObject); overload ;
  procedure MergeJsonStruct(ASrc: TJSONArray; ADest: TJSONArray); overload ;

  procedure ParseObjectId(AID: AnsiString; out AModId: AnsiString; out AObjectId: AnsiString);

implementation

uses
  LazLoggerBase, editor_consts, types;

var
  rexp_oid: TRegExpr;


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
    raise EJSON.Create('Incompatible JSON values');
  end;

  case ASrc.JSONType of
    jtNull: assert(false);
    jtArray: MergeJsonStruct(ASrc as TJSONArray, ADest as TJSONArray) ;
    jtBoolean: ADest.AsBoolean := ASrc.AsBoolean ;
    jtNumber:begin
      case TJSONNumber(ASrc).NumberType of
        ntFloat: ADest.AsFloat := ASrc.AsFloat ;
        ntInt64: ADest.AsInt64 := ASrc.AsInt64;
        ntInteger: ADest.AsInteger := ASrc.AsInteger;
      end;
    end;
    jtObject:MergeJsonStruct(ASrc as TJSONObject, ADest as TJSONObject) ;
    jtString:ADest.AsString := ASrc.AsString;
  else
    begin
      raise EJSON.Create('Unknown JSON type');
    end;
  end;

end;

procedure InheritJson(ABase: TJSONObject; ADest: TJSONObject);
var
  temp: TJSONObject;
begin
  temp := ABase.Clone as TJSONObject;

  MergeJsonStruct(Adest, temp);

  ADest.Assign(temp);

  temp.Free;
end;

procedure MergeJsonStruct(ASrc: TJSONObject; ADest: TJSONObject);
var
  src_idx, dest_idx: Integer;
  name: TJSONStringType;
begin
  for src_idx := 0 to ASrc.Count - 1 do
  begin
    name := ASrc.Names[src_idx];

    dest_idx := ADest.IndexOfName(name);

    if dest_idx >=0 then
    begin
      MergeJson(ASrc.Items[src_idx],ADest.Items[dest_idx]);
    end
    else begin
      ADest.Add(name,ASrc.Items[src_idx].Clone());
    end;
  end;
end;

procedure MergeJsonStruct(ASrc: TJSONArray; ADest: TJSONArray);
begin
  Assert(false,'MergeJsonStruct for arrays Not implemented');
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
    Result := TJSONObject.Create;
    add(AName, Result);
  end
  else begin
    Result := Objects[AName];
  end;
end;

procedure TJSONObjectHelper.InheritFrom(ABase: TJSONObject);
begin
  InheritJson(ABase, self);
end;

procedure TJSONObjectHelper.Assign(AValue: TJSONObject);
var
  iter: TJSONEnum;
begin
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
end;

procedure TJsonResource.DestreamTo(AObject: TObject; AFieldName: string);
var
  destreamer: TVCMIJSONDestreamer;
begin
  destreamer := TVCMIJSONDestreamer.Create(nil);
  try

    if AFieldName = '' then
    begin
      destreamer.JSONToObject(FRoot,AObject);
    end
    else begin
      destreamer.JSONToObject(FRoot.Objects[AFieldName],AObject);
    end;

  finally
    destreamer.Free;
  end;
end;

destructor TJsonResource.Destroy;
begin
  FreeAndNil(FRoot);
end;

procedure TJsonResource.LoadFromStream(AStream: TStream);
var
  destreamer: TVCMIJSONDestreamer;
begin
  destreamer := TVCMIJSONDestreamer.Create(nil);
  try
    FreeAndNil(FRoot);
    FRoot := destreamer.JSONStreamToJSONObject(AStream,'');

    Assert(FRoot is TVCMIJsonObject);
  finally
    destreamer.Free;
  end;
end;

{ TVCMIJSONStreamer }

constructor TVCMIJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TVCMIJSONStreamer.ObjectToJSON(const AObject: TObject): TJSONObject;
begin
  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).BeforeSerialize();
  end;

  Result:=inherited ObjectToJSON(AObject);

  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).AfterSerialize();
  end;
end;

function TVCMIJSONStreamer.ObjectToJsonEx(const AObject: TObject): TJSONData;
begin
  Result := ObjectToJSON(AObject);  //todo:  ObjectToJsonEx
end;

procedure TVCMIJSONStreamer.DoBeforeStreamProperty(const AObject: TObject;
  PropertyInfo: PPropInfo; var Skip: boolean);
var
  PropType: PTypeInfo;
  Value: Int64;
  DefValue: Int64;
  SValue: String;
begin
  inherited DoBeforeStreamProperty(AObject, PropertyInfo, Skip);

  PropType := PropertyInfo^.PropType;

  if not IsStoredProp(AObject,PropertyInfo) then
  begin
    Skip := True;
    Exit;
  end;

  case PropType^.Kind of
    tkInteger, tkInt64, tkChar, tkEnumeration, tkWChar,tkSet, tkBool, tkQWord: begin
      Value := GetOrdProp(AObject, PropertyInfo);
      DefValue := PropertyInfo^.Default;

      if (Value = DefValue) and (DefValue<>longint($80000000)) then
      begin
        Skip := True;
      end;
    end;
    tkString, tkAString:begin
      SValue := GetStrProp(AObject, PropertyInfo);
      if SValue = '' then
      begin
         Skip := True;
      end;
    end;
  end;
end;

procedure TVCMIJSONStreamer.DoPreparePropName(var PropName: AnsiString);
begin
  MakeCamelCase(PropName);
end;

function TVCMIJSONStreamer.StreamCollection(const ACollection: TCollection
  ): TJSONData;
var
  O: TJSONObject;
  A: TJSONArray;
  elem: TCollectionItem;
begin
  if ACollection is INamedCollection then
  begin
    o := TJSONObject.Create;
    for elem in ACollection do
    begin
      o.Add(Elem.DisplayName, ObjectToJSON(elem));
    end;
    result := o;
  end
  else if ACollection is IArrayCollection then
  begin
    a:=TJSONArray.Create;
    for elem in ACollection do
    begin
      a.Add(ObjectToJSON(elem));
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
  O : TJSONObject;
begin
  ACollection:=TCollection(Data);

  new_item := ACollection.Add;

  DestreamCollectionItem(ACollection, Item, new_item);

  Continue := True;
end;

procedure TVCMIJSONDestreamer.DestreamCollectionItem(ACollection: TCollection;
  ASrc: TJSONData; AItem: TCollectionItem);
var
  O: TJSONObject;
begin
  if ASrc.JSONType in [jtObject]then
  begin
    O := TJSONObject(ASrc);

    JSONToObject(o,AItem);
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
  ACollection: TCollection;
  new_item: TCollectionItem;
  O : TJSONObject;
begin
  ACollection:=TCollection(Data);

  new_item := ACollection.Add;
  new_item.DisplayName := AName;

  DestreamCollectionItem(ACollection, Item, new_item);

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

procedure TVCMIJSONDestreamer.DoPreparePropName(var PropName: AnsiString);
begin
  MakeCamelCase(PropName);
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

function TVCMIJSONDestreamer.JSONStreamToJSONObject(AStream: TStream;
  AName: string): TJSONObject;
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
var
  O : TJSONObject;
  A: TJSONArray;
begin
  if (JSON.JSONType = jtObject) and (ACollection is INamedCollection) then
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

procedure TVCMIJSONDestreamer.JSONToObject(const JSON: TJSONObject;
  AObject: TObject);
begin
  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).BeforeDeSerialize();
  end;

  if AObject is IEmbeddedCollection then
  begin
    JSONToCollection(JSON,(AObject as IEmbeddedCollection).GetCollection);
  end
  else if AObject is TJSONObject then
  begin
    (AObject as TJSONObject).Assign(Json);
  end
  else begin
    inherited JSONToObject(JSON, AObject);
  end;

  if AObject is ISerializeNotify then
  begin
    (AObject as ISerializeNotify).AfterDeSerialize();
  end;
end;


function TVCMIJSONDestreamer.ObjectFromString(const JSON: TJSONStringType
  ): TJSONData;
var
  prepase_buffer: TStringList;
  stm: TStringStream;

  s, s1: AnsiString;
  commentpos, quotepos: Integer;
  i: Integer;
  char_nomber: Integer;
  in_string: Boolean;
  may_be_comment: Boolean;
  len: Integer;
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

procedure TModdedConfigs.Preload(APaths: TModdedConfigPaths;
  ALoader: IResourceLoader);
var
  AModdedPath: TModdedConfigPath;
  i: SizeInt;

  current: TJsonResource;
  APath: String;
  item: TModdedConfig;
  key: TModId;
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
      finally
        current.Free;
      end;
    end;
    item.Config.SetMeta(AModdedPath.ModID,true);
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
  id: TJSONStringType;
  config: TVCMIJSONObject;

  all_fields: TStringDynArray;
  key: String;
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
      id := Key;

      ParseObjectId(id, other_mod_id, id);

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
  mod_id: String;
  mod_data: TModdedConfig;
  i: Integer;
begin
  for i := 0 to FMap.Count - 1 do
  begin
    mod_id := FMap.Keys[i];
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

procedure TModdedConfigs.Load(APaths: TModdedConfigPaths;
  ALoader: IResourceLoader; ADest: TJSONObject);
begin
  Preload(APaths,ALoader);
  ExtractPatches;
  ApplyPatches;
  CombineTo(ADest);
end;

initialization

  rexp_oid := TRegExpr.Create;
  rexp_oid.Expression := '^((.*):)?(.*)$';
  rexp_oid.Compile;

  SetJSONInstanceType(TJSONInstanceType.jitObject, TVCMIJsonObject);
  SetJSONInstanceType(TJSONInstanceType.jitString, TVCMIJsonString);
  SetJSONInstanceType(TJSONInstanceType.jitArray,  TVCMIJsonArray);

finalization

  rexp_oid.Free;

end.

