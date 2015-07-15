{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015 Alexander Shishkin alexvins@users.sourceforge,net

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

unit logical_id_condition;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fpjson, editor_types, editor_utils, editor_classes;

type
  { TLogicalIDCondition }

  TLogicalIDCondition = class(TPersistent, ISerializeNotify, IFPObserver)
  private
    FAllOf: TStrings;
    FAnyOf: TStrings;
    FNoneOf: TStrings;
    function isAllOfStored: Boolean;
    function isAnyOfStored: Boolean;
    function isNoneOfStored: Boolean;
    procedure SetAllOf(AValue: TStrings);
    procedure SetAnyOf(AValue: TStrings);
    procedure SetNoneOf(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Minimize;
    procedure Clear;

    function IsAllowed(AId: AnsiString): Boolean;

    function IsPermissive: Boolean;

  public //ISerializeNotify
    procedure AfterSerialize(Sender:TObject; AData: TJSONData);
    procedure BeforeSerialize(Sender:TObject);
    procedure AfterDeSerialize(Sender:TObject; AData: TJSONData);
    procedure BeforeDeSerialize(Sender:TObject; AData: TJSONData);

  public
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
  published
    property AnyOf: TStrings read FAnyOf write SetAnyOf stored isAnyOfStored;
    property AllOf: TStrings read FAllOf write SetAllOf stored isAllOfStored;
    property NoneOf: TStrings read FNoneOf write SetNoneOf stored isNoneOfStored;
  end;


implementation

{ TLogicalIDCondition }

procedure TLogicalIDCondition.SetAllOf(AValue: TStrings);
begin
  if FAllOf=AValue then Exit;
  FAllOf:=AValue;
end;

function TLogicalIDCondition.isAllOfStored: Boolean;
begin
  Result := FAllOf.Count >0;
end;

function TLogicalIDCondition.isAnyOfStored: Boolean;
begin
  Result := FAnyOf.Count >0;
end;

function TLogicalIDCondition.isNoneOfStored: Boolean;
begin
  Result := FNoneOf.Count >0;
end;

procedure TLogicalIDCondition.SetAnyOf(AValue: TStrings);
begin
  if FAnyOf=AValue then Exit;
  FAnyOf:=AValue;
end;

procedure TLogicalIDCondition.SetNoneOf(AValue: TStrings);
begin
  if FNoneOf=AValue then Exit;
  FNoneOf:=AValue;
end;

constructor TLogicalIDCondition.Create;
begin
  FAllOf := CrStrList;
  FAllOf.FPOAttachObserver(Self);

  FAnyOf := CrStrList;
  FAnyOf.FPOAttachObserver(Self);

  FNoneOf := CrStrList;
  FNoneOf.FPOAttachObserver(Self);
end;

destructor TLogicalIDCondition.Destroy;
begin
  FAnyOf.Free;
  FAllOf.Free;
  FNoneOf.Free;
  inherited Destroy;
end;

procedure TLogicalIDCondition.Minimize;
var
  s: String;
  idx: Integer;
begin
  for s in FNoneOf do
  begin
    idx  := FAnyOf.IndexOf(s);
    if idx >=0 then
    begin
      FAnyOf.Delete(idx);
    end;

    idx  := FAllOf.IndexOf(s);
    if idx >=0 then
    begin
      FAllOf.Delete(idx);
    end;
  end;

  for s in FAllOf do
  begin
    idx  := FAnyOf.IndexOf(s);
    if idx >=0 then
    begin
      FAnyOf.Delete(idx);
    end;
  end;
end;

procedure TLogicalIDCondition.Clear;
begin
  FAllOf.Clear;
  FAnyOf.Clear;
  FNoneOf.Clear;
end;

function TLogicalIDCondition.IsAllowed(AId: AnsiString): Boolean;
begin
  Result := (FNoneOf.IndexOf(AId)<0)
    and ((FAllOf.Count = 0) or (FAllOf.IndexOf(AId) >=0))
    and ((FAnyOf.Count = 0) or (FAnyOf.IndexOf(AId) >=0));
end;

function TLogicalIDCondition.IsPermissive: Boolean;
begin
  Result := (FAnyOf.Count = 0) and (FAllOf.Count = 0);
end;

procedure TLogicalIDCondition.AfterSerialize(Sender: TObject; AData: TJSONData);
begin

end;

procedure TLogicalIDCondition.BeforeSerialize(Sender: TObject);
begin
  Minimize;
end;

procedure TLogicalIDCondition.AfterDeSerialize(Sender: TObject; AData: TJSONData
  );
begin
  Minimize;
end;

procedure TLogicalIDCondition.BeforeDeSerialize(Sender: TObject;
  AData: TJSONData);
begin
  Clear;
end;

procedure TLogicalIDCondition.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  FPONotifyObservers(Self, Operation, Data);
end;

end.

