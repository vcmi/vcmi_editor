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
  ExtCtrls, object_options, base_object_options_frame, object_link_frame;

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
    pnLink: TPanel;
    pnRandom: TPanel;
    pnLinked: TPanel;
    rbLinked: TRadioButton;
    rbRandom: TRadioButton;
    procedure edMaxChange(Sender: TObject);
    procedure edMinChange(Sender: TObject);
    procedure rbLinkedChange(Sender: TObject);
    procedure rbRandomChange(Sender: TObject);
  private
     FOptions: TBaseRandomDwellingOptions;
     FFactionsLoaded, FLevelsLoaded: Boolean;

     FLinkFrame: TObjectLinkFrame;

     procedure SetupControls;
     procedure UpdateControls;
     procedure LoadLevels;
     procedure SaveLevels;

     procedure LoadFactions;
     procedure SaveFactions;

     procedure LoadLink;
     procedure SaveLink;

     procedure NormalizeLevels;
  public
     constructor Create(TheOwner: TComponent); override;
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

procedure TDwellingFrame.rbLinkedChange(Sender: TObject);
begin
  rbRandom.Checked := not rbLinked.Checked;
  UpdateControls;
end;

procedure TDwellingFrame.rbRandomChange(Sender: TObject);
begin
  rbLinked.Checked := not rbRandom.Checked;
  UpdateControls;
end;

procedure TDwellingFrame.edMaxChange(Sender: TObject);
begin
  NormalizeLevels;
end;

procedure TDwellingFrame.SetupControls;
begin
  FLinkFrame.Map := Map;

  rbLinked.Checked := FOptions.Linked;
  rbLinkedChange(rbLinked);

  UpdateControls;
end;

procedure TDwellingFrame.UpdateControls;
begin
  edFaction.Enabled:=rbRandom.Checked;

  FLinkFrame.Enabled := rbLinked.Checked;
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

procedure TDwellingFrame.LoadLink;
begin
  FLinkFrame.Load(FOptions.SameAsTown);
end;

procedure TDwellingFrame.SaveLink;
begin
  FLinkFrame.Commit;
end;

procedure TDwellingFrame.NormalizeLevels;
begin
  edMin.Value:=Min(edMin.Value,edMax.Value);
  edMax.Value:=Max(edMin.Value,edMax.Value);
end;

constructor TDwellingFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLinkFrame := TObjectLinkFrame.Create(Self);
  FLinkFrame.Parent := pnLink;
  FLinkFrame.Visible := true;
  FLinkFrame.Align:=alClient;
end;

procedure TDwellingFrame.Commit;
begin
  inherited Commit;
  FOptions.Linked:=rbLinked.Checked;
  if FLevelsLoaded then SaveLevels;
  if FFactionsLoaded then SaveFactions;

  SaveLink;
end;

procedure TDwellingFrame.VisitRandomDwelling(AOptions: TRandomDwellingOptions);
begin
  FOptions := AOptions;
  SetupControls();
  LoadLevels;
  LoadFactions;
  LoadLink;
end;

procedure TDwellingFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  FOptions := AOptions;
  gbLevel.Enabled:=false;
  SetupControls();
  LoadFactions;
  LoadLink;
end;

procedure TDwellingFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  FOptions := AOptions;
  gbFaction.Enabled:=false;
  SetupControls();
  LoadLevels;
  LoadLink;
end;

end.

