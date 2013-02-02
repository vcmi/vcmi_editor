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
unit objects;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, fgl, gvector, ghashmap, FileUtil, editor_types, filesystem_base, def;

type

  TdefId = UInt64;

  TDefBitmask = packed array[0..5] of uint8; //top to bottom, right to left as in H3M

  { TObjTemplate }

  TObjTemplate = class
  private
    FDef: TDef;
    filename: AnsiString;
    passability,
    actions: TDefBitmask;
    landscape,land_edit_groups: uint16;
    Typ,SubType: uint32;
    Group,isOverlay: uint8;
    procedure SetDef(AValue: TDef);
  public
    constructor Create;

    property Def: TDef read FDef write SetDef;
  end;

  TDefVector = specialize TFPGObjectList<TObjTemplate>;

  { TDefIdHash }

  TDefIdHash = class
    class function hash(a:TdefId; n:longint):longint;
  end;

  TIdToDefMap = specialize THashmap<TDefId, TObjTemplate, TDefIdHash>;

  { TObjectsManager }

  TObjectsManager = class (TGraphicsCosnumer)
  strict private

    FDefs: TDefVector; //all aviable defs

    FUsedDefs: TDefVector; //defs used in map

    FDefIdMap : TIdToDefMap;

    function TypToId(Typ,SubType: uint32):TDefId; inline;

    procedure LoadObjectsConfig;
    procedure LoadObjectsGraphics;
  private
    FGraphicsManager: TGraphicsManager;
    function GetObjCount: Integer;
    function GetObjcts(AIndex: Integer): TObjTemplate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadObjects;

    procedure BindTextures;

    property Objcts[AIndex: Integer]: TObjTemplate read GetObjcts;
    property ObjCount:Integer read GetObjCount;


  end;

implementation

uses
  CsvDocument;

const
  OBJECT_LIST = 'DATA/ZEOBJTS';

{ TObjTemplate }

constructor TObjTemplate.Create;
begin
  inherited;
end;

procedure TObjTemplate.SetDef(AValue: TDef);
begin
  if FDef = AValue then Exit;
  FDef := AValue;
end;

{ TDefIdHash }

class function TDefIdHash.hash(a: TdefId; n: longint): longint;
begin
  result := (Int64Rec(a).Hi xor Int64Rec(a).Lo) mod Cardinal(n);
end;

{ TObjectsManager }

procedure TObjectsManager.BindTextures;
var
  i: Integer;
begin
  for i := 0 to FDefs.Count - 1 do
  begin
    FDefs[i].Def.BindTextures;
  end;
end;

constructor TObjectsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefs := TDefVector.Create(True);
  FUsedDefs := TDefVector.Create(False);

  FDefIdMap := TIdToDefMap.Create;
end;

destructor TObjectsManager.Destroy;
begin
    FDefIdMap.Free;
  FUsedDefs.Free;
  FDefs.Free;

  inherited Destroy;
end;

function TObjectsManager.GetObjCount: Integer;
begin
  Result := FDefs.Count;
end;

function TObjectsManager.GetObjcts(AIndex: Integer): TObjTemplate;
begin
  Result := FDefs[AIndex];
end;

procedure TObjectsManager.LoadObjects;
begin
  LoadObjectsConfig;
  LoadObjectsGraphics;
end;

procedure TObjectsManager.LoadObjectsConfig;

var
  stm: TStringStream;
  doc: TCSVDocument;
  row, col: Integer;

  procedure CellToStr(var s: string);
  begin
    if not doc.HasCell(col, row) then
       raise Exception.CreateFmt('OBJTXT error cell not exists. row:%d, col:%d',[row,col]);

    s := doc.Cells[col,row];
    inc(col);
  end;


  procedure CellToBitMask(var mask: TDefBitmask);
  var
    i: Integer;
    j: Integer;

    ss: string;
    m: UInt8;
    s: string;
  begin
    s:='';
    CellToStr(s);
    if not Length(s)=6*8 then
       raise Exception.CreateFmt('OBJTXT Format error. line:%d, data:%s',[row,s]);

    for i:=5 downto 0 do //in object.txt bottom line is first
    begin
      ss := Copy(s,i*8+1,8);
      if not (Length(ss)=8) then
        raise Exception.CreateFmt('OBJTXT Format error. line:%d, data:%s',[row,s]);
      m := 0;
      for j := 0 to 7 do
      begin
        if ss[j+1] = '1' then
          m := m or (1 shl j) ;
      end;
      mask[i] := m;
    end;
  end;


  procedure CellToUint16Mask(var v: uint16);
  var
    temp: string;
    len: Integer;
    i: Integer;
  begin
    temp := '';
    CellToStr(temp);
    len:= Length(temp);
    v := 0;
    for i := len to 1 do
    begin
      if temp[i] = '1' then
        v := v or 1 shl i;
    end;
  end;

  function CellToInt: uint32;
  begin
    result := StrToIntDef(doc.Cells[col,row],0);
    inc(col);
  end;

var
  def: TObjTemplate;
  id: TDefId;

  s_tmp: string;
begin
  //todo: suppport for custom object lists

  stm := TStringStream.Create('');
  doc := TCSVDocument.Create;
  doc.Delimiter := #$20; //space
  doc.EqualColCountPerRow := False;
  doc.IgnoreOuterWhitespace := False;
  doc.QuoteOuterWhitespace := False;
  doc.LineEnding := #13#10;

  try
    ResourceLoader.LoadToStream(stm,TResourceType.Text,OBJECT_LIST);
    stm.Seek(0,soBeginning);

    doc.CSVText := stm.DataString;

    for row := 1 to doc.RowCount-1 do //first row contains no data, so start with 1
    begin
      col := 0;

      def := TObjTemplate.Create;

      s_tmp := '';

      CellToStr(s_tmp);

      def.filename := Trim(UpperCase(ExtractFileNameWithoutExt(s_tmp)));


      CellToBitMask(def.passability);
      CellToBitMask(def.actions);
      CellToUint16Mask(def.landscape);
      CellToUint16Mask(def.land_edit_groups);

      def.Typ := CellToInt;
      def.SubType := CellToInt;
      def.Group := CellToInt;
      def.isOverlay := CellToInt;

      id := TypToId(def.Typ,def.SubType);
      FDefIdMap.Insert(id,def);
      FDefs.Add(def);

    end;

  finally
    stm.Free;
    doc.Free;
  end;

end;

procedure TObjectsManager.LoadObjectsGraphics;
var
  i: Integer;
begin
  for i := 0 to FDefs.Count - 1 do
  begin
    FDefs[i].Def := GraphicsManager.GetGraphics(FDefs[i].filename);
  end;
end;


function TObjectsManager.TypToId(Typ, SubType: uint32): TDefId;
begin
  Int64Rec(Result).Hi := Typ;
  Int64Rec(Result).Lo := SubType;
end;

end.

