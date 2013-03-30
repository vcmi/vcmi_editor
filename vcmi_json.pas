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
  Classes, SysUtils, fpjson, fgl, vcmi_fpjsonrtti, typinfo, filesystem_base;

type

  { TVCMIJSONDestreamer }

  TOnBeforeReadObject = procedure (const JSON: TJSONObject; AObject: TObject) of object;

  TVCMIJSONDestreamer = class (TJSONDeStreamer)
  private
    FOnBeforeReadObject: TOnBeforeReadObject;
    procedure CollectionObjCallback(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

    procedure CollectionArrayCallback(Item: TJSONData; Data: TObject; var Continue: Boolean);
    procedure SetOnBeforeReadObject(AValue: TOnBeforeReadObject);
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

    property OnBeforeReadObject:TOnBeforeReadObject read FOnBeforeReadObject write SetOnBeforeReadObject;
  end;

  { TVCMIJSONStreamer }

  TVCMIJSONStreamer = class (TJSONStreamer)
  protected
    procedure DoBeforeStreamProperty(const AObject: TObject;
      PropertyInfo: PPropInfo; var Skip: boolean); override;
    procedure DoPreparePropName(var PropName: AnsiString); override;
  public
    constructor Create(AOwner: TComponent); override;
    function StreamCollection(const ACollection: TCollection): TJSONData;
      override;
  end;


  { TJsonResource }

  TJsonResource = class (IResource)
  private
    FRoot: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    property Root: TJSONObject read FRoot;

    procedure DestreamTo(AObject: TObject; AFieldName: string = '');
  end;

  TJsonObjectList = specialize TFPGObjectList<TJSONObject>;

  procedure MergeJson(ASrc: TJSONData; ADest: TJSONData);

  procedure MergeJsonStruct(ASrc: TJSONObject; ADest: TJSONObject); overload ;
  procedure MergeJsonStruct(ASrc: TJSONArray; ADest: TJSONArray); overload ;

implementation

uses editor_classes;

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
  if not (ASrc.JSONType = ADest.JSONType) then
  begin
    raise EJSON.Create('Incompatible JSON values');
  end;

  case ASrc.JSONType of
    jtNull: ;//nothing to do
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
      ADest.Add(name,ASrc.Items[src_idx].Clone()); //todo: use safe extract?
    end;

  end;
end;

procedure MergeJsonStruct(ASrc: TJSONArray; ADest: TJSONArray);
begin
  Assert(false,'MergeJsonStruct for arrays Not implemented');
end;

{ TJsonResource }

constructor TJsonResource.Create;
begin

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
    FRoot := destreamer.JSONStreamToJSONObject(AStream,'');
  finally
    destreamer.Free;
  end;
end;

{ TVCMIJSONStreamer }

constructor TVCMIJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TVCMIJSONStreamer.DoBeforeStreamProperty(const AObject: TObject;
  PropertyInfo: PPropInfo; var Skip: boolean);
var
  PropType: PTypeInfo;
  Value: Int64;
  DefValue: Int64;
begin
  inherited DoBeforeStreamProperty(AObject, PropertyInfo, Skip);

  PropType := PropertyInfo^.PropType;

  if not IsStoredProp(AObject,PropertyInfo) then
  begin
    Skip := True;
    Exit;
  end;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar,tkSet: begin
      Value := GetOrdProp(AObject, PropertyInfo);
      DefValue := PropertyInfo^.Default;

      if (Value = DefValue) and (DefValue<>longint($80000000)) then
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

  if Item.JSONType = jtObject then
  begin
    O := TJSONObject(Item);

    JSONToObject(o,new_item);
  end
  else if Item.JSONType = jtArray then //todo:collection of collections
  begin

  end
  else begin
    Error('Not supported collection element type for item ');
  end;

  Continue := True;
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

  if Item.JSONType in [jtObject] then
  begin
    O :=  TJSONObject(Item);

    JSONToObject(o,new_item);
  end
  else if Item.JSONType in [jtArray] then
  begin
    if new_item is IEmbeddedCollection then
    begin
      JSONToCollection(Item,(new_item as IEmbeddedCollection).GetCollection);
    end
  end
  else begin
    Error('Not supported collection element type for item '+AName);
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
    src.CopyFrom(AStream,AStream.Size);
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
  if Assigned(FOnBeforeReadObject) then
    FOnBeforeReadObject(JSON,AObject);

  if AObject is IEmbeddedCollection then
  begin
    JSONToCollection(JSON,(AObject as IEmbeddedCollection).GetCollection);
  end
  else begin
    inherited JSONToObject(JSON, AObject);
  end;

end;


function TVCMIJSONDestreamer.ObjectFromString(const JSON: TJSONStringType
  ): TJSONData;
var
  prepase_buffer: TStringList;
  stm: TStringStream;

  s: AnsiString;
  commentpos: Integer;
  i: Integer;
begin

  stm := TStringStream.Create(JSON);
  prepase_buffer:= TStringList.Create;
  try
    stm.Seek(0,soBeginning);
    prepase_buffer.LoadFromStream(stm);

    for i := 0 to prepase_buffer.Count - 1 do
    begin
      s := prepase_buffer[i];
      commentpos := Pos('//',s);

      if commentpos > 0 then
      begin
        s := Copy(s,1,commentpos - 1);
        prepase_buffer[i] := s;
      end;
    end;

     Result := inherited ObjectFromString(prepase_buffer.Text);
  finally
    stm.Free;
    prepase_buffer.Free;
  end;


end;

procedure TVCMIJSONDestreamer.SetOnBeforeReadObject(AValue: TOnBeforeReadObject
  );
begin
  if FOnBeforeReadObject = AValue then Exit;
  FOnBeforeReadObject := AValue;
end;

end.

