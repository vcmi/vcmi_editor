unit object_link;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editor_types;

type
{$push}
{$m+}

  TObjectLinkMetclass = (hero, town, creature);


  { TObjectLink }

  TObjectLink = class
  private
    FL: integer;
    FMetaclass: TObjectLinkMetclass;
    FX: integer;
    FY: integer;
    procedure SetL(AValue: integer);
    procedure SetMetaclass(AValue: TObjectLinkMetclass);
    procedure SetX(AValue: integer);
    procedure SetY(AValue: integer);
  published
    property Metaclass:TObjectLinkMetclass read FMetaclass write SetMetaclass nodefault;

    property X:integer read FX write SetX nodefault;
    property Y:integer read FY write SetY nodefault;
    property L:integer read FL write SetL nodefault;
  end;

{$pop}
implementation

{ TObjectLink }

procedure TObjectLink.SetMetaclass(AValue: TObjectLinkMetclass);
begin
  if FMetaclass=AValue then Exit;
  FMetaclass:=AValue;
end;

procedure TObjectLink.SetL(AValue: integer);
begin
  if FL=AValue then Exit;
  FL:=AValue;
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

end.

