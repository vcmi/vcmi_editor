{ This file is a part of Map editor for VCMI project

  Copyright (C) 2017 Alexander Shishkin alexvins@users.sourceforge.net

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

unit vcmi.frames.tools;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ActnList, StdCtrls, Buttons, ExtCtrls;

type

  { TToolsFrame }

  TToolsFrame = class(TFrame)
    act: TActionList;
    actSizeRect: TAction;
    actSize4: TAction;
    actSize2: TAction;
    actSize1: TAction;
    btnBrush1: TSpeedButton;
    btnBrush2: TSpeedButton;
    btnBrush3: TSpeedButton;
    btnBrush4: TSpeedButton;
    btnBrush5: TSpeedButton;
    btnBrush6: TSpeedButton;
    btnBrushArea: TSpeedButton;
    btnBrushArea1: TSpeedButton;
    gbBrushObjects: TGroupBox;
    gbBrushTerrain: TGroupBox;
    gbTerrain: TGroupBox;
    iml: TImageList;
    RiverType: TRadioGroup;
    RoadType: TRadioGroup;
    tsErase: TTabSheet;
    ToolsPages: TPageControl;
    tsRivers: TTabSheet;
    tsRoads: TTabSheet;
    tsTerrain: TTabSheet;
    tsObjects: TTabSheet;
    procedure ToolsPagesChange(Sender: TObject);
    procedure ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TToolsFrame }

procedure TToolsFrame.ToolsPagesChange(Sender: TObject);
begin
  //todo: select active brush
end;

procedure TToolsFrame.ToolsPagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := true;
  //TODO: clear active brush
end;

end.

