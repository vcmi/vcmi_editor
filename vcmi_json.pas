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
  Classes, SysUtils, fpjson,vcmi_fpjsonrtti, typinfo;

type

  { TVCMIJSONDestreamer }

  TVCMIJSONDestreamer = class (TJSONDeStreamer)
  private
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
    function StreamCollection(const ACollection: TCollection): TJSONData;
      override;
  end;

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

{ TVCMIJSONStreamer }

procedure TVCMIJSONStreamer.DoBeforeStreamProperty(const AObject: TObject;
  PropertyInfo: PPropInfo; var Skip: boolean);
var
  PropType: PTypeInfo;
  Value: Int64;
  DefValue: Int64;
begin
  inherited DoBeforeStreamProperty(AObject, PropertyInfo, Skip);

  PropType := PropertyInfo^.PropType;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar: begin
      Value := GetOrdProp(AObject, PropertyInfo);
      DefValue := PropertyInfo^.Default;

      if (Value = DefValue) and (DefValue<>longint($80000000)) then
      begin
        Skip := True;
      end;
    end;

     tkSet: begin
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

  if ACollection is TNamedCollection then
  begin
    o := TJSONObject.Create;
    for elem in ACollection do
    begin
      o.Add(Elem.DisplayName, ObjectToJSON(elem));
    end;
    result := o;
  end
  else if ACollection is TArrayCollection then
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

  if Item.JSONType = jtObject then
  begin
    O :=  TJSONObject(Item);

    JSONToObject(o,new_item);
  end
  else if Item.JSONType = jtArray then //collection of collections
  begin
     //todo: collection of collections
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
begin
  dt := nil;
  try
    dt := JSONStreamToJson(AStream);

    if not (dt.JSONType = jtObject) then Error('Root data shall be object');

    root := TJSONObject(dt);

    if AName = '' then
    begin
      //use root
      Result := TJSONObject(dt.Clone);
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
  if (JSON.JSONType = jtObject) and (ACollection is TNamedCollection) then
  begin
    ACollection.Clear;

    O := JSON as TJSONObject;
    O.Iterate(@CollectionObjCallback,ACollection);
  end
  else if (JSON.JSONType = jtArray) and (ACollection is TArrayCollection) then
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
  inherited JSONToObject(JSON, AObject);
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

end.

