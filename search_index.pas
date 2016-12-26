{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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

unit search_index;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, gset, gvector, math, RegExpr, LazUTF8, editor_utils, editor_types,  editor_gl;

type
  {$INTERFACES CORBA}

  { ISearchResult }

  ISearchResult = interface
    procedure Add(AObject: TObject);
    procedure Clear;
      function GetCount: SizeInt;
    property Count: SizeInt read GetCount;

    procedure RenderIcon(AIndex:SizeInt; AState: TLocalState; AX, AY, dim:integer; color: TPlayer);
  end;

  { TObjPtrLess }

  TObjPtrLess = class
  public
    class function c(a,b: TObject): boolean;
  end;

  { TSearchIndexBusket }

  TSearchIndexBusket = class
  public
    type
      TDataVector = specialize TVector<TObject>;
      TBusketData = specialize TSet<TObject, TObjPtrLess>;
    var
      data: TBusketData;
    constructor Create();
    destructor Destroy; override;
    procedure AddItem(AItem: TObject);
    procedure RemoveItem(AItem: TObject);
    procedure Intersect(ATarget:TBusketData);
    procedure SaveTo(ATarget:TBusketData);
  end;

  { TSearchIndexMap }

  TSearchIndexMap = class(specialize TFPGMap<string,TSearchIndexBusket>)
  protected
    procedure Deref(Item: Pointer); override;
  public
    constructor Create;

    //true if found
    function Find(AKeyWord: String; out IdxLow: integer; out IdxHigh: integer): boolean; overload;
  end;

  { TSearchIndex }

  TSearchIndex = class
  private
  type
    TObjectsSet = specialize TSet<TObject, TObjPtrLess>;
  private
    FAll: TObjectsSet;
    FMap: TSearchIndexMap;
    FTextTokenizer: TRegExpr;
    procedure AddToIndex(AKeyWord: String; AItem: TObject);
    procedure Find(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);
    procedure Intersect(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);

    procedure SelectAll(AResult: ISearchResult);
  public
    constructor Create();
    destructor Destroy; override;

    procedure AddToIndex(ARawKeyWords: TStrings; AItem: TObject);
    procedure RemoveFromIndex(AItem: TObject);

    // AInput = space separated words or empty string to get all
    procedure Find(AInput: string; AResult: ISearchResult);
  end;

implementation

function CompareSearchIndexBusket(const d1,d2: TSearchIndexBusket): integer;
begin
  Result := PtrInt(d1) - PtrInt(d2);
end;

{ TSearchIndex }

constructor TSearchIndex.Create;
begin
  FMap := TSearchIndexMap.Create();
  FTextTokenizer := TRegExpr.Create('[_\-\s\.:]+');
  FTextTokenizer.Compile;
  FAll := TObjectsSet.Create;
end;

destructor TSearchIndex.Destroy;
begin
  FAll.Free;
  FTextTokenizer.Free;
  FMap.Free;
  inherited Destroy;
end;

procedure TSearchIndex.AddToIndex(AKeyWord: String; AItem: TObject);
var
  busket: TSearchIndexBusket;
  idx: Integer;
begin
  idx := -1;

  if not Fmap.Find(AKeyWord, idx) then
  begin
    busket := TSearchIndexBusket.Create();
    FMap.Add(AKeyWord, busket);
  end
  else
  begin
    busket := FMap.Data[idx];
  end;

  busket.AddItem(AItem);
end;

procedure TSearchIndex.Find(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);
var
  idx_low, idx_high, i: Integer;
begin
  idx_low := -1;
  idx_high := -1;

  if Fmap.Find(AKeyWord, idx_low, idx_high)then
  begin
    for i := idx_low to idx_high do
    begin
      FMap.Data[i].SaveTo(ATarget);
    end;
  end;
end;

procedure TSearchIndex.Intersect(AKeyWord: String; ATarget: TSearchIndexBusket.TBusketData);
var
  idx_low, idx_high, i: Integer;
  temp : TSearchIndexBusket;
begin
  idx_low := -1;
  idx_high := -1;

  if Fmap.Find(AKeyWord, idx_low, idx_high)then
  begin
    temp := TSearchIndexBusket.Create;

    for i := idx_low to idx_high do
    begin
      FMap.Data[i].SaveTo(temp.data);
    end;

    temp.Intersect(ATarget);
    temp.Free;
  end
  else
  begin
    while not ATarget.IsEmpty do
    begin
      ATarget.Delete(ATarget.NMin^.Data);
    end;
  end;
end;

procedure TSearchIndex.AddToIndex(ARawKeyWords: TStrings; AItem: TObject);
var
  keywords: TStringList;
  RawKeyWord, KeyWord: String;
begin
  FAll.Insert(AItem);

  keywords := TStringList.Create;
  keywords.Sorted:=true;
  keywords.Duplicates:=dupIgnore;
  try

    for RawKeyWord in ARawKeyWords do
    begin
      KeyWord := NormalizeKeyWord(RawKeyWord);

      FTextTokenizer.Split(KeyWord, keywords);
    end;

    for KeyWord in keywords do
    begin
      AddToIndex(KeyWord, AItem);
    end;
  finally
    keywords.Free;
  end;
end;

procedure TSearchIndex.RemoveFromIndex(AItem: TObject);
var
  i: Integer;
begin
  FAll.Delete(AItem);
  //TODO: optimize

  for i := 0 to FMap.Count - 1 do
  begin
    FMap.Data[i].RemoveItem(AItem);
  end;
end;

procedure TSearchIndex.Find(AInput: string; AResult: ISearchResult);
var
  keywords: TStringList;
  data: TSearchIndexBusket.TBusketData;
  i: Integer;
  it: TSearchIndexBusket.TBusketData.TIterator;
begin
  AResult.Clear;
  AInput := UTF8Trim(UTF8LowerCase(AInput));

  if AInput = '' then
  begin
    SelectAll(AResult);
    Exit;
  end;

  data := TSearchIndexBusket.TBusketData.Create;

  keywords := TStringList.Create;
  keywords.Sorted:=true;
  keywords.Duplicates:=dupIgnore;

  FTextTokenizer.Split(AInput, keywords);

  Find(keywords[0], data);

  if not data.IsEmpty() then
  begin
    for i := 1 to keywords.Count - 1 do
    begin
      Intersect(keywords[i], data);
      if data.IsEmpty then
        break;
    end;
  end;

  it := data.Min;

  if Assigned(it) then
  begin
    repeat
      AResult.Add(it.Data);
    until not it.Next;
    it.free;
  end;

  data.Free;
  keywords.Free;
end;

procedure TSearchIndex.SelectAll(AResult: ISearchResult);
var
  it: TObjectsSet.TIterator;
begin
  AResult.Clear;
  it := FAll.Min;

  if Assigned(it) then
  begin
    repeat
      AResult.Add(it.Data);
    until not it.Next;
    it.free;
  end;
end;

{ TSearchIndexMap }

procedure TSearchIndexMap.Deref(Item: Pointer);
begin
  Finalize(string(Item^));
  TSearchIndexBusket(Pointer(PByte(Item)+KeySize)^).Free;
end;

constructor TSearchIndexMap.Create;
begin
  inherited Create;
  OnKeyCompare := @CompareStringProxy;
  OnDataCompare := @CompareSearchIndexBusket;

  Sorted := True;
  Duplicates:=dupError;
end;

function TSearchIndexMap.Find(AKeyWord: String; out IdxLow: integer; out IdxHigh: integer): boolean;

  function PartialMatched(idx: integer): boolean;
  begin
    result := UTF8Pos(AKeyWord,Keys[idx]) = 1;
  end;

begin
  IdxLow := -1;
  IdxHigh := -1;

  if Find(AKeyWord, IdxLow) then
  begin
    IdxHigh:=IdxLow;
    Result := true;
  end
  else
  begin
    //find firts true partial match

    while (IdxLow < Count) and not PartialMatched(IdxLow) do
    begin
      Inc(IdxLow);
    end;

    if IdxLow >= Count then
    begin
      IdxLow := -1;
      IdxHigh := -1;

      Result := false;
      Exit;
    end;

    //find end of matched region

    IdxHigh:=IdxLow;

    while (IdxHigh < Count) and PartialMatched(IdxHigh) do
    begin
      Inc(IdxHigh);
    end;

    IdxHigh:=Min(IdxHigh-1, Count-1);
    Result := true;
  end;
end;

{ TSearchIndexBusket }

constructor TSearchIndexBusket.Create;
begin
  data := TBusketData.Create;
end;

destructor TSearchIndexBusket.Destroy;
begin
  data.Free;
  inherited Destroy;
end;

procedure TSearchIndexBusket.AddItem(AItem: TObject);
begin
   data.Insert(AItem);
end;

procedure TSearchIndexBusket.RemoveItem(AItem: TObject);
begin
  data.Delete(AItem);
end;

procedure TSearchIndexBusket.Intersect(ATarget: TBusketData);
var
  it: TBusketData.TIterator;
  n: TBusketData.PNode;
  to_delete: TDataVector;
  i: SizeInt;
begin
  to_delete := TDataVector.Create;

  it := ATarget.Min;

  if Assigned(it) then
  begin
    repeat
      n := data.NFind(it.Data);

      if not Assigned(n) then
      begin
        to_delete.PushBack(it.data);
      end;
    until not it.Next;
    it.free;
  end;

  for i := 0 to SizeInt(to_delete.Size) - 1 do
  begin
    ATarget.Delete(to_delete.Items[i]);
  end;

  to_delete.Free;
end;

procedure TSearchIndexBusket.SaveTo(ATarget: TBusketData);
var
  it: TBusketData.TIterator;
begin
  it := data.Min;

  if Assigned(it) then
  begin
    repeat
      ATarget.Insert(it.Data);
    until not it.Next;
    it.free;
  end;
end;

{ TObjPtrLess }

class function TObjPtrLess.c(a, b: TObject): boolean;
begin
  Result := PtrInt(a) < PtrInt(b);
end;

end.

