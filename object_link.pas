unit object_link;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, editor_types, editor_classes;

type
{$push}
{$m+}

  { TObjectLink }

  TObjectLink = class
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
  published
    property &type: AnsiString read FType write SetType;
    property subType: AnsiString read FsubType write SetsubType;

    property X:integer read FX write SetX nodefault;
    property Y:integer read FY write SetY nodefault;
    property L:integer read FL write SetL nodefault;
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

end.

