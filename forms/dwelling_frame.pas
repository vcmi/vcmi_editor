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
unit dwelling_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, Math, FileUtil, Forms, Controls, StdCtrls, Spin, CheckLst,
  object_options, base_object_options_frame;

type

  { TDwellingFrame }

  TDwellingFrame = class(TBaseObjectOptionsFrame)
    edFaction: TCheckListBox;
    gbLevel: TGroupBox;
    gbFaction: TGroupBox;
    lbMin: TLabel;
    lbMax: TLabel;
    edMin: TSpinEdit;
    edMax: TSpinEdit;
    procedure edMaxChange(Sender: TObject);
    procedure edMinChange(Sender: TObject);
  private
     FOptions: TBaseRandomDwellingOptions;
     FFactionsLoaded, FLevelsLoaded: Boolean;
     procedure SetupControls;
     procedure LoadLevels;
     procedure SaveLevels;

     procedure LoadFactions;
     procedure SaveFactions;

     procedure NormalizeLevels;
  public
     procedure Commit; override;
     procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions); override;
     procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions); override;
     procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions); override;
  end;

implementation

uses gui_helpers;

{$R *.lfm}

{ TDwellingFrame }

procedure TDwellingFrame.edMinChange(Sender: TObject);
begin
  NormalizeLevels;
end;

procedure TDwellingFrame.edMaxChange(Sender: TObject);
begin
  NormalizeLevels;
end;

procedure TDwellingFrame.SetupControls;
begin

end;

procedure TDwellingFrame.LoadLevels;
begin
  edMin.Value := FOptions.MinLevel;
  edMax.Value := FOptions.MaxLevel;
  FLevelsLoaded:=True;
end;

procedure TDwellingFrame.SaveLevels;
begin
  FOptions.MinLevel := edMin.Value;
  FOptions.MaxLevel := edMax.Value;
end;

procedure TDwellingFrame.LoadFactions;
begin
  edFaction.FillFromList(ListsManager.FactionMap, FOptions.AllowedFactions);

  FFactionsLoaded:=True;
end;

procedure TDwellingFrame.SaveFactions;
begin
  edFaction.SaveToList(FOptions.AllowedFactions);
end;

procedure TDwellingFrame.NormalizeLevels;
begin
  edMin.Value:=Min(edMin.Value,edMax.Value);
  edMax.Value:=Max(edMin.Value,edMax.Value);
end;

procedure TDwellingFrame.Commit;
begin
  inherited Commit;
  if FLevelsLoaded then SaveLevels;
  if FFactionsLoaded then SaveFactions;
end;

procedure TDwellingFrame.VisitRandomDwelling(AOptions: TRandomDwellingOptions);
begin
  FOptions := AOptions;
  SetupControls();
  LoadLevels;
  LoadFactions;
end;

procedure TDwellingFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  FOptions := AOptions;
  gbLevel.Enabled:=false;
  SetupControls();
  LoadFactions;
end;

procedure TDwellingFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  FOptions := AOptions;
  gbFaction.Enabled:=false;
  SetupControls();
  LoadLevels;
end;

end.

