unit object_link;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editor_types, editor_classes, vcmi_json, fpjson;

type
{$push}
{$m+}
  //todo: update link on object move
  { TObjectLink }

  TObjectLink = class(TObject, ISerializeSpecial)
  private
    FL: integer;
    FsubType: AnsiString;
    FType: AnsiString;
    FX: integer;
    FY: integer;
    procedure SetL(AValue: integer);
    procedure SetsubType(AValue: AnsiString);
    procedure SetType(AValue: AnsiString);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);

    function HasPosition: Boolean;
  public
    constructor Create;
    //ISerializeSpecial
    function Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
    procedure Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData);

    function IsEmpty: Boolean;
  published
    property &type: AnsiString read FType write SetType;
    property subType: AnsiString read FsubType write SetsubType;
  public//special streaming
    property X:integer read FX write SetX;
    property Y:integer read FY write SetY;
    property L:integer read FL write SetL;
  end;

{$pop}
implementation

{ TObjectLink }

procedure TObjectLink.SetType(AValue: AnsiString);
begin
  if FType=AValue then Exit;
  FType:=AValue;
end;

procedure TObjectLink.SetL(AValue: integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
end;

procedure TObjectLink.SetsubType(AValue: AnsiString);
begin
  if FsubType=AValue then Exit;
  FsubType:=AValue;
end;

procedure TObjectLink.SetX(AValue: integer);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TObjectLink.SetY(AValue: integer);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

function TObjectLink.HasPosition: Boolean;
begin
  Result := (X>=0) or (Y>=0) or (L>=0);
end;

constructor TObjectLink.Create;
begin
  FX:=-1;
  FY:=-1;
  FL:=-1;
end;

function TObjectLink.Serialize(AHandler: TVCMIJSONStreamer): TJSONData;
var
  o: TJSONObject;
begin
  o := CreateJSONObject([]);

  if &type <> '' then
  begin
    o.Add('type', &type);

    if subType <> '' then
    begin
      o.Add('subType', subType);
    end;

  end;

  if HasPosition then
    o.Add('position', CreateJSONArray([X,Y,L]));

  Result := o;
end;

procedure TObjectLink.Deserialize(AHandler: TVCMIJSONDestreamer; ASrc: TJSONData
  );
var
  AsrcObj: TJSONObject;
  Position: TJSONArray;
begin
  if ASrc.JSONType <> jtObject then
  begin
    raise Exception.Create('invalid format for object link, object required');
  end;

  AsrcObj := TJSONObject(Asrc);

  AHandler.JSONToObject(AsrcObj, self);

  if AsrcObj.IndexOfName('position') >=0 then
  begin
    Position := AsrcObj.Arrays['position'];

    X := Position.Integers[0];
    Y := Position.Integers[1];
    L := Position.Integers[2];
  end;

end;

function TObjectLink.IsEmpty: Boolean;
begin
  Result := not HasPosition and (FType = '') and (FsubType = '');
end;

end.

