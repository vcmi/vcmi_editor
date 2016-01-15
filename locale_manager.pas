{ This file is a part of Map editor for VCMI project

  Copyright (C) 2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit locale_manager;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, filesystem_base, h3_txt;

type

  { TLocaleManager }

  TLocaleManager = class(TFSConsumer)
  private
    FGeneralTexts: TTextResource;
    FLossTexts: TTextResource;
    FVictoryTexts: TTextResource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadTexts;

    property GeneralTexts: TTextResource read FGeneralTexts;
    property VictoryTexts: TTextResource read FVictoryTexts;
    property LossTexts: TTextResource read FLossTexts;
  end;

implementation

const
  GENERAL_TEXTS_PATH = 'DATA/GENRLTXT';
  VICTORY_PATH = 'DATA/VCDESC';
  LOSS_PATH = 'DATA/LCDESC';

{ TLocaleManager }

constructor TLocaleManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGeneralTexts := TTextResource.Create(GENERAL_TEXTS_PATH);
  FVictoryTexts := TTextResource.Create(VICTORY_PATH);
  FLossTexts := TTextResource.Create(LOSS_PATH);
end;

destructor TLocaleManager.Destroy;
begin
  FLossTexts.Free;
  FVictoryTexts.Free;
  FGeneralTexts.Free;
  inherited Destroy;
end;

procedure TLocaleManager.LoadTexts;
begin
  FGeneralTexts.Load(ResourceLoader);
  FVictoryTexts.Load(ResourceLoader);
  FLossTexts.Load(ResourceLoader);
end;

end.

