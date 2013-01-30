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
  Classes, SysUtils, fpjson,vcmi_fpjsonrtti,jsonparser, typinfo;

type

  { TVCMIJSONDestreamer }

  TVCMIJSONDestreamer = class (TJSONDeStreamer)
  private
    procedure CollectionCallback(Const AName : TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);

  protected
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
      PropData: TJSONData); override;
    //preprocess comments
    function ObjectFromString(const JSON: TJSONStringType): TJSONData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure JSONToCollection(Const JSON : TJSONData; ACollection : TCollection); override;
    procedure JSONToObject(const JSON: TJSONObject; AObject: TObject); override;
  end;

  { TVCMIJSONStreamer }

  TVCMIJSONStreamer = class (TJSONStreamer)

  end;

implementation

{ TVCMIJSONDestreamer }

procedure TVCMIJSONDestreamer.CollectionCallback(const AName: TJSONStringType;
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

procedure TVCMIJSONDestreamer.DoRestoreProperty(AObject: TObject;
  PropInfo: PPropInfo; PropData: TJSONData);
begin

  inherited DoRestoreProperty(AObject, PropInfo, PropData);
end;

procedure TVCMIJSONDestreamer.JSONToCollection(const JSON: TJSONData;
  ACollection: TCollection);
var
  O : TJSONObject;
begin

  //named colection
  if JSON.JSONType = jtObject then
  begin
    ACollection.Clear;

    O := JSON as TJSONObject;

    O.Iterate(@CollectionCallback,ACollection);

  end
  else begin
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

