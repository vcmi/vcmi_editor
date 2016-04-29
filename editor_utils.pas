{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit editor_utils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Classes, types, editor_types, editor_consts, LazUTF8, fpjson, gmap, FPimage;

type

  { TStringCompare }

  TStringCompare = class
  public
    class function c(a,b: AnsiString): boolean;
  end;

  //TCustomIDCompare = specialize gutil.TLess<TCustomID>;

  TNameToIdMap = specialize TMap<AnsiString,TCustomID,TStringCompare>;

  //TIdToNameMap = specialize TMap<TCustomID,AnsiString,TCustomIDCompare>;

  function ExtractModID(AIdentifier:AnsiString): AnsiString;

  function ExtractModID2(var AIdentifier:AnsiString): AnsiString;

  procedure LeToNInPlase(var Val:Int32); inline;

  function CrStrList: TStringList;

  procedure FillStringArray(ASrc: TStrings; var ADest: TStringDynArray);

  function NormalizeModId(AModId: TModId): TModId;

  function NormalizeResourceName(const AName: string): string;
  function NormalizeKeyWord(const AName: string): string;

  function StripScope(const AIdentifier: string): string;

  procedure GenerateDefaultVisitableFrom(ADest: TStrings; AGroup: UInt8; Atyp: Tobj);
  procedure GenerateDefaultVisitableFrom(ADest: TJSONArray; AGroup: UInt8; Atyp: Tobj);

  function RGBAColorToFpColor(const c: TRBGAColor): TFPColor;

  function DecodeFullIdentifier(const ASource: AnsiString; out AMetaclass: TMetaclass; out AScope: AnsiString; out AIdentifier: AnsiString): Boolean;

implementation

uses
  FileUtil;

function ExtractModID(AIdentifier:AnsiString): AnsiString;
var
  colon_pos: SizeInt;
begin
  if(AIdentifier = '') then
    exit('');

  colon_pos := pos(':',AIdentifier);

  if colon_pos <= 0 then
    exit('');//object is from core

  Result := copy(AIdentifier, 1, colon_pos-1);
end;

function ExtractModID2(var AIdentifier: AnsiString): AnsiString;
var
  colon_pos: SizeInt;
begin
  if(AIdentifier = '') then
    exit('');

  colon_pos := pos(':',AIdentifier);

  if colon_pos <= 0 then
    exit('');//object is from core

  Result := copy(AIdentifier, 1, colon_pos-1);

  AIdentifier:=copy(AIdentifier, colon_pos+1, MaxInt);
end;

procedure LeToNInPlase(var Val:Int32); inline;
begin
  val := LEtoN(val);
end;


function CrStrList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

procedure FillStringArray(ASrc: TStrings; var ADest: TStringDynArray);
var
  i: Integer;
begin
  SetLength(ADest, ASrc.Count);

  for i := 0 to ASrc.Count - 1 do
  begin
    ADest[i] := ASrc[i];
  end;
end;

function NormalizeModId(AModId: TModId): TModId;
begin
  Result := Trim(LowerCase(AModId));
end;

function NormalizeResourceName(const AName: string): string;
begin
  Result := SetDirSeparators(AName);
  Result := UpperCase(Result);
  Result := ExtractFileNameWithoutExt(Result);
end;

function NormalizeKeyWord(const AName: string): string;
begin
  result := UTF8LowerCase(UTF8Trim(AName));
end;

function StripScope(const AIdentifier: string): string;
var
  colon_position: SizeInt;
begin
  colon_position := Pos(':', AIdentifier);

  if colon_position <=0 then
  begin
    Result := AIdentifier;
  end
  else
  begin
    Result := copy(AIdentifier, colon_position+1, MaxInt);
  end;
end;

procedure GenerateDefaultVisitableFrom(ADest: TStrings; AGroup: UInt8;
  Atyp: Tobj);
var
  from_top: Boolean;
  str: String;
begin
  if (AGroup=2) or (AGroup=3) or (AGroup=4) or (AGroup=5) then
    from_top := true
  else
    case TObj(Atyp) of
      TObj.FLOTSAM,
      TObj.SEA_CHEST,
      Tobj.SHIPWRECK_SURVIVOR,
      Tobj.BUOY,
      Tobj.OCEAN_BOTTLE,
      TObj.BOAT,
      TObj.WHIRLPOOL,
      Tobj.GARRISON,
      Tobj.GARRISON2,
      Tobj.SCHOLAR,
      Tobj.CAMPFIRE,
      Tobj.BORDERGUARD,
      Tobj.BORDER_GATE,
      Tobj.QUEST_GUARD,
      Tobj.CORPSE: from_top:= true;
    else
      from_top := false;
    end;

  //top line
  if from_top then
    str := '+++'
  else
    str := '---';
  UniqueString(str);
  ADest.Add(str);

  //middle line
  str := '+-+';
  UniqueString(str);
  ADest.Add(str);

  //bollom line
  str := '+++';
  UniqueString(str);
  ADest.Add(str)
end;

procedure GenerateDefaultVisitableFrom(ADest: TJSONArray; AGroup: UInt8;
  Atyp: Tobj);
var
  tmp: TStringList;
  s: String;
begin
  tmp := TStringList.Create;
  try
    GenerateDefaultVisitableFrom(tmp, AGroup, Atyp);

    ADest.Clear;
    for s in tmp do
    begin
      ADest.Add(s);
    end;
  finally
    tmp.Free;
  end;
end;

function RGBAColorToFpColor(const c: TRBGAColor): TFPColor;
begin
  Result.Red:=c.r + c.r shl 8;
  Result.Green:=c.g + c.g shl 8;
  Result.Blue:= c.b + c.b shl 8;
  Result.Alpha:=c.a + c.a shl 8;
end;

function DecodeFullIdentifier(const ASource: AnsiString; out AMetaclass: TMetaclass; out AScope: AnsiString; out
  AIdentifier: AnsiString): Boolean;
begin

end;

{ TStringCompare }

class function TStringCompare.c(a, b: AnsiString): boolean;
begin
  Result := CompareText(a,b) > 0;
end;


end.

