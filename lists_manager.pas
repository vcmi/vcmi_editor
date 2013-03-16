{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge,net

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

unit lists_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, filesystem_base, editor_types, editor_utils, CsvDocument,
  gmap, fgl;

type

  { TTextResource }

  TTextResource = class (IResource)
  private
    FDoc: TCSVDocument;
    function GetValue(Col, Row: Integer): TLocalizedString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);

    property Value[Col,Row: Integer]:TLocalizedString read GetValue;
  end;

  { TSkillInfo }

  TSkillInfo = class
  private
    FID: AnsiString;
    FName: TLocalizedString;
    procedure SetID(AValue: AnsiString);
    procedure SetName(AValue: TLocalizedString);
  public
    property ID: AnsiString read FID write SetID;
    property Name: TLocalizedString read FName write SetName;
  end;

  TSkillInfos = specialize TFPGObjectList<TSkillInfo>;

  { TListsManager }

  TListsManager = class (TFSConsumer)
  strict private
    FSkillNameMap: TNameToIdMap;
    FSkillInfos: TSkillInfos;
    procedure LoadSkills;
  strict private //Accesors
    function GetPlayerName(const APlayer: TPlayer): TlocalizedString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load;
  public
    property PlayerName[const APlayer: TPlayer]: TlocalizedString read GetPlayerName;

    //Skill helpers
    function SkillNidToString (ASkill: TSkillID): AnsiString;
    function SkillStringToNid(AID: AnsiString): TSkillID;
    property SkillInfos: TSkillInfos read FSkillInfos;
  end;

implementation

uses FileUtil, editor_consts;

const
  SEC_SKILL_TRAITS = 'data\sstraits';

const
  PLAYER_NAMES: array[TPlayer] of AnsiString = (
    'No player',
    'Player 1 (red)',
    'Player 2 (blue)',
    'Player 3 (tan)',
    'Player 4 (green)',
    'Player 5 (orange)',
    'Player 6 (purple)',
    'Player 7 (teal)',
    'Player 8 (pink)');

{ TSkillInfo }

procedure TSkillInfo.SetID(AValue: AnsiString);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TSkillInfo.SetName(AValue: TLocalizedString);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

{ TTextResource }

constructor TTextResource.Create;
begin
  FDoc := TCSVDocument.Create;
  FDoc.Delimiter := #9; //tab
  FDoc.EqualColCountPerRow := False;
  FDoc.IgnoreOuterWhitespace := False;
  FDoc.QuoteOuterWhitespace := False;
  FDoc.LineEnding := #13#10;
end;

destructor TTextResource.Destroy;
begin
  FDoc.Free;
end;

function TTextResource.GetValue(Col, Row: Integer): TLocalizedString;
begin
  Result := AnsiToUtf8(FDoc.Cells[Col,Row]);
end;

procedure TTextResource.LoadFromStream(AStream: TStream);
begin
  FDoc.LoadFromStream(AStream);
end;

{ TListsManager }

constructor TListsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkillNameMap := TNameToIdMap.Create;

  FSkillInfos := TSkillInfos.Create(True);
end;

destructor TListsManager.Destroy;
begin
  FSkillInfos.Free;
  FSkillNameMap.Free;
  inherited Destroy;
end;

function TListsManager.GetPlayerName(const APlayer: TPlayer): TlocalizedString;
begin
  //TODO: get localized name;
  Result := PLAYER_NAMES[APlayer];
end;

procedure TListsManager.Load;
begin
  LoadSkills;
end;

procedure TListsManager.LoadSkills;
var
  sstraits: TTextResource;
  info: TSkillInfo;
  i: Integer;
begin
  for i := 0 to SKILL_QUANTITY - 1 do
  begin
    FSkillNameMap.Insert(SKILL_NAMES[i], i);
  end;

  FSkillInfos.Clear;
  sstraits := TTextResource.Create;
  try
    ResourceLoader.LoadResource(sstraits,TResourceType.Text, SEC_SKILL_TRAITS);

    for i := 2 to sstraits.FDoc.RowCount - 1 do
    begin
      info := TSkillInfo.Create;
      info.ID := SKILL_NAMES[i-2];
      info.Name := sstraits.Value[0,i];
      FSkillInfos.Add(info);
    end;

  finally
    sstraits.Free;
  end;
end;

function TListsManager.SkillNidToString(ASkill: TSkillID): AnsiString;
begin
  Result := editor_consts.SKILL_NAMES[ASkill];
end;

function TListsManager.SkillStringToNid(AID: AnsiString): TSkillID;
var
  it: TNameToIdMap.TIterator;
begin
  it := FSkillNameMap.Find(AID);

  if Assigned(it) then
  begin
    Result := it.Value;

  end
  else
  begin
    Result := ID_INVALID;
    raise Exception.Create('Invalid secondary skill name '+AID);
  end;
end;

end.

