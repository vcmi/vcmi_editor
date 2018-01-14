{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2017 Alexander Shishkin alexvins@users.sourceforge.net

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
unit h3_txt;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils,
  vcmi_csvdocument,
  filesystem_base,editor_types;
type

  { TTextResource }

  TTextResource = class (TBaseResource, IResource)
  public
    type
      TDelimiter = (Tab,Space);
  private
    FDelimiter: TDelimiter;
    FDoc: TCSVDocument;
    FTopRowSkip: Integer;
    function GetRowCount: Integer;
    function GetValue(Col, Row: Integer): TLocalizedString;
    procedure SetDelimiter(AValue: TDelimiter);
    procedure SetTopRowSkip(AValue: Integer);
  public
    constructor Create(APath: AnsiString);
    destructor Destroy; override;
    procedure LoadFromStream(AFileName: AnsiString; AStream: TStream); override;

    property Value[Col,Row: Integer]:TLocalizedString read GetValue; default;
    function HasCell(Col,Row: Integer):boolean;

    property RowCount: Integer read GetRowCount;
  public
    property Delimiter: TDelimiter read FDelimiter write SetDelimiter;
    property TopRowSkip: Integer read FTopRowSkip write SetTopRowSkip;
  end;


implementation

uses LazUTF8;


{ TTextResource }

constructor TTextResource.Create(APath: AnsiString);
begin
  Inherited Create(TResourceType.Text, APath);
  FDoc := TCSVDocument.Create;

  SetDelimiter(TDelimiter.Tab);

  FDoc.EqualColCountPerRow := False;
  FDoc.IgnoreOuterWhitespace := False;
  FDoc.QuoteOuterWhitespace := False;
  FDoc.LineEnding := #13#10;
  FDoc.QuoteChar := '"';
end;

destructor TTextResource.Destroy;
begin
  FDoc.Free;
end;

function TTextResource.GetRowCount: Integer;
begin
  Result := FDoc.RowCount;
end;

function TTextResource.GetValue(Col, Row: Integer): TLocalizedString;
begin
  //TODO: use codepage from vcmi config
  row := row + TopRowSkip;
  {$IFDEF MSWindows}
  Result := WinCPToUTF8(FDoc.Cells[Col,Row]);
  {$ELSE}
  Result := AnsiToUtf8(FDoc.Cells[Col,Row]);//THIS IS WRONG!
  {$ENDIF}
end;

function TTextResource.HasCell(Col, Row: Integer): boolean;
begin
  row := row + TopRowSkip;
  Result := FDoc.HasCell(Col,Row);
end;

procedure TTextResource.LoadFromStream(AFileName: AnsiString; AStream: TStream);
begin
  FDoc.LoadFromStream(AStream);
end;

procedure TTextResource.SetDelimiter(AValue: TDelimiter);
const
  DELIMITERS : array[TDelimiter] of TCSVChar = (#9,#$20);
begin
  FDelimiter := AValue;
  FDoc.Delimiter := DELIMITERS[AValue];
end;

procedure TTextResource.SetTopRowSkip(AValue: Integer);
begin
  if FTopRowSkip=AValue then Exit;
  FTopRowSkip:=AValue;
end;

end.

