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
  Classes, SysUtils, fgl, gvector, ghashmap, FileUtil,
  editor_types,
  filesystem_base, base_info, editor_graphics, editor_classes, h3_txt;

type

  TdefId = UInt64;

  TDefBitmask = packed array[0..5] of uint8; //top to bottom, right to left as in H3M

  { TLegacyObjTemplate }

  TLegacyObjTemplate = class
  private
    FDef: TDef;
    FFilename: AnsiString;
    FPassability,
    FActions: TDefBitmask;
    FLandscape,FLandEditGroups: uint16;
    FTyp,FSubType: uint32;
    FGroup,FIsOverlay: uint8;
    procedure SetDef(AValue: TDef);
  public
    constructor Create;

    property Def: TDef read FDef write SetDef;

    property Filename: AnsiString read FFilename;
    property Actions: TDefBitmask read FActions;
    property Passability: TDefBitmask read FPassability;
    property Landscape: uint16 read FLandscape;
    property LandEditGroups: uint16 read FLandEditGroups;
    property Typ: uint32 read FTyp;
    property SubType: uint32 read FSubType;

    property IsOverlay: uint8 read FIsOverlay;

  end;

  TLegacyObjTemplateList = specialize TFPGObjectList<TLegacyObjTemplate>;

  {$push}
  {$m+}

  { TObjTemplate }

  TObjTemplate = class (TNamedCollectionItem)
  private
    FDef: TDef;
  strict private
    FAllowedTerrains: TTerrainTypes;
    FAnimation: AnsiString;
    FVisitableFrom: TStringList;
    FMask: TStringList;
    function GetMask: TStrings;
    function GetVisitableFrom: TStrings;
    procedure SetAllowedTerrains(AValue: TTerrainTypes);
    procedure SetAnimation(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Def: TDef read FDef;
  published
    property Animation: AnsiString read FAnimation write SetAnimation;
    property VisitableFrom: TStrings read GetVisitableFrom;
    property AllowedTerrains: TTerrainTypes read FAllowedTerrains write SetAllowedTerrains default ALL_TERRAINS;
    property Mask: TStrings read GetMask;
  end;

  TObjTemplates = class (specialize TGNamedCollection<TObjTemplate>)
  end;

  { TObjSubType }

  TObjSubType = class (TNamedCollectionItem)
  private
    FName: TLocalizedString;
    FNid: TCustomID;
    FTemplates: TObjTemplates;
    function GetIndexAsID: TCustomID;
    procedure SetIndexAsID(AValue: TCustomID);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Index: TCustomID read GetIndexAsID write SetIndexAsID default -1;
    property Templates:TObjTemplates read FTemplates;
    property Name: TLocalizedString read FName write FName;
  end;

  TObjSubTypes = class (specialize TGNamedCollection<TObjSubType>)

  end;

  { TObjType }

  TObjType = class (TNamedCollectionItem)
  private
    FHandler: AnsiString;
    FName: TLocalizedString;
    FNid: TCustomID;
    FSubTypes: TObjSubTypes;
    procedure SetHandler(AValue: AnsiString);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Index: TCustomID read FNid write FNid default -1;
    property Types:TObjSubTypes read FSubTypes;
    property Name: TLocalizedString read FName write FName;
    property Handler: AnsiString read FHandler write SetHandler;
  end;

  TObjTypes = class (specialize TGNamedCollection<TObjType>)

  end;

  {$pop}

  { TObjectsManager }

  TObjectsManager = class (TGraphicsCosnumer)
  strict private

    FDefs: TLegacyObjTemplateList; //all aviable defs

    FObjTypes: TObjTypes;

    function TypToId(Typ,SubType: uint32):TDefId; inline;


  private
    function GetObjCount: Integer;
    function GetObjcts(AIndex: Integer): TLegacyObjTemplate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadObjects(AProgressCallback: IProgressCallback; APaths: TModdedConfigVector);

    property Objcts[AIndex: Integer]: TLegacyObjTemplate read GetObjcts;
    property ObjCount:Integer read GetObjCount;


  end;

implementation

uses
  CsvDocument, editor_consts, editor_utils;

const
  OBJECT_LIST = 'DATA/OBJECTS';

{ TObjType }

procedure TObjType.SetHandler(AValue: AnsiString);
begin
  if FHandler=AValue then Exit;
  FHandler:=AValue;
end;

constructor TObjType.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSubTypes := TObjSubTypes.Create;
end;

destructor TObjType.Destroy;
begin
  FSubTypes.Free;
  inherited Destroy;
end;

{ TObjSubType }

function TObjSubType.GetIndexAsID: TCustomID;
begin
  Result := FNid;
end;

procedure TObjSubType.SetIndexAsID(AValue: TCustomID);
begin
  FNid := AValue;
end;

constructor TObjSubType.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTemplates := TObjTemplates.Create;
end;

destructor TObjSubType.Destroy;
begin
  FTemplates.Free;
  inherited Destroy;
end;

{ TObjTemplate }

procedure TObjTemplate.SetAnimation(AValue: AnsiString);
begin
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;
end;

function TObjTemplate.GetVisitableFrom: TStrings;
begin
  Result := FVisitableFrom;
end;

function TObjTemplate.GetMask: TStrings;
begin
  Result := FMask;
end;

procedure TObjTemplate.SetAllowedTerrains(AValue: TTerrainTypes);
begin
  if FAllowedTerrains=AValue then Exit;
  FAllowedTerrains:=AValue;
end;

constructor TObjTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVisitableFrom := TStringList.Create;
  FMask := TStringList.Create;
end;

destructor TObjTemplate.Destroy;
begin
  FMask.Free;
  FVisitableFrom.Free;
  inherited Destroy;
end;

{ TLegacyObjTemplate }

constructor TLegacyObjTemplate.Create;
begin
  inherited;
end;

procedure TLegacyObjTemplate.SetDef(AValue: TDef);
begin
  if FDef = AValue then Exit;
  FDef := AValue;
end;

{ TObjectsManager }

constructor TObjectsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefs := TLegacyObjTemplateList.Create(True);
  FObjTypes := TObjTypes.Create;
end;

destructor TObjectsManager.Destroy;
begin
  FObjTypes.Free;
  FDefs.Free;

  inherited Destroy;
end;

function TObjectsManager.GetObjCount: Integer;
begin
  Result := FDefs.Count;
end;

function TObjectsManager.GetObjcts(AIndex: Integer): TLegacyObjTemplate;
begin
  Result := FDefs[AIndex];
end;

procedure TObjectsManager.LoadObjects(AProgressCallback: IProgressCallback;
  APaths: TModdedConfigVector);
var
  row, col: Integer;

  objects_txt: TTextResource;

  procedure CellToStr(var s: string);
  begin
    if not objects_txt.HasCell(col, row) then
       raise Exception.CreateFmt('OBJTXT error cell not exists. row:%d, col:%d',[row,col]);

    s := objects_txt.Value[col,row];
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
    result := StrToIntDef(objects_txt.Value[col,row],0);
    inc(col);
  end;

var
  def: TLegacyObjTemplate;
  id: TDefId;

  s_tmp: string;
  progess_delta: Integer;
begin

  //todo: support for vcmi object lists

  objects_txt := TTextResource.Create;
  objects_txt.Delimiter := TTextResource.TDelimiter.Space;

  try
    ResourceLoader.LoadResource(objects_txt,TResourceType.Text,OBJECT_LIST);

    AProgressCallback.Max := 200;

    progess_delta := objects_txt.RowCount div 200;

    for row := 1 to objects_txt.RowCount-1 do //first row contains no data, so start with 1
    begin

      if (row mod progess_delta) = 0 then
      begin
        AProgressCallback.Advance(1);
      end;

      col := 0;

      def := TLegacyObjTemplate.Create;

      s_tmp := '';

      CellToStr(s_tmp);

      def.FFilename := NormalizeResourceName(s_tmp);


      CellToBitMask(def.FPassability);
      CellToBitMask(def.FActions);
      CellToUint16Mask(def.FLandscape);
      CellToUint16Mask(def.FLandEditGroups);

      def.FTyp := CellToInt;
      def.FSubType := CellToInt;
      def.FGroup := CellToInt;
      def.FIsOverlay := CellToInt;

      id := TypToId(def.FTyp,def.FSubType);
      def.Def := GraphicsManager.GetGraphics(def.FFilename);
      FDefs.Add(def);

    end;

  finally
    objects_txt.Free;
  end;

end;


function TObjectsManager.TypToId(Typ, SubType: uint32): TDefId;
begin
  Int64Rec(Result).Hi := Typ;
  Int64Rec(Result).Lo := SubType;
end;

end.

