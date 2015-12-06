unit hero_definition_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComboEx, hero_frame, gui_helpers, Map, lists_manager, editor_types;

type

  { THeroDefinitionFrame }

  THeroDefinitionFrame = class(THeroFrame)
    procedure cbBiographyChange(Sender: TObject);
    procedure cbExperienceChange(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure cbPortraitChange(Sender: TObject);
    procedure cbSexChange(Sender: TObject);
    procedure edNameEditingDone(Sender: TObject);
    procedure edPatrolKeyPress(Sender: TObject; var Key: char);
    procedure edSexChange(Sender: TObject);
  private
    FOptions: THeroDefinition;
  public
    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ THeroDefinitionFrame }

procedure THeroDefinitionFrame.cbBiographyChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.cbExperienceChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.cbNameChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.cbPortraitChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.cbSexChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.edNameEditingDone(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.edPatrolKeyPress(Sender: TObject; var Key: char);
begin
  inherited;
end;

procedure THeroDefinitionFrame.edSexChange(Sender: TObject);
begin
  inherited;
end;

procedure THeroDefinitionFrame.VisitHeroDefinition(AOptions: THeroDefinition);
var
  h_info: THeroInfo;
begin
  FOptions := AOptions;

  lbOwner.Visible:=false;
  edOwner.Visible:=false;
  Placeholder3.Visible:=false;

  lbPatrol.Visible:=false;
  edPatrol.Visible:=false;
  Placeholder5.Visible:=false;

  edHeroClass.Enabled := false;
  edType.Enabled:=false;

  inherited VisitHeroDefinition(AOptions);

  h_info := ListsManager.Heroes[AOptions.Identifier];
  FCurrentHero := h_info;
  edHeroClass.FillFromList(ListsManager.HeroClassMap, h_info.&Class);
  edType.FillFromList(ListsManager.HeroMap, h_info.ID);

  cbPortrait.Checked:=FOptions.Portrait <> '';
  cbExperience.Checked := FOptions.Experience <> 0;

  cbName.Checked:=FOptions.Name <> '';
  if cbName.Checked then
  begin
    FCustomName := FOptions.Name;
  end;
  cbNameChange(cbName);

  cbBiography.Checked:=FOptions.Biography <> '';
  if cbBiography.Checked then
  begin
    FCustomBiography := FOptions.Biography;
  end;

  cbBiographyChange(cbBiography);

  cbPortraitChange(cbPortrait);

  cbSex.Checked:=FOptions.Sex <> THeroSex.default;
  if cbSex.Checked then
  begin
    FCustomFemale:=(FOptions.Sex = THeroSex.female);
  end;

  cbSexChange(cbSex);


  edExperience.Text := IntToStr(FOptions.Experience);

  UpdateControls();
end;

procedure THeroDefinitionFrame.Commit;
begin

end;

end.

