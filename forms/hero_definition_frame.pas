unit hero_definition_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComboEx, ExtCtrls, hero_frame, gui_helpers, Map, lists_manager, editor_types;

type

  { THeroDefinitionFrame }

  THeroDefinitionFrame = class(THeroFrame)
    AvailableFor: TCheckGroup;
    AvailableForLabel: TLabel;
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
    procedure LoadPlayers;
    procedure SavePlayers;

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

procedure THeroDefinitionFrame.LoadPlayers;
var
  p: TPlayerColor;
begin
  for p in TPlayerColor do
  begin
    AvailableFor.Checked[Integer(p)] := FOptions.AvailableFor * [p] <> [];
  end;
end;

procedure THeroDefinitionFrame.SavePlayers;
var
  p: TPlayerColor;
  available_for: TPlayers;
begin
  available_for := [];
  for p in TPlayerColor do
  begin
    if AvailableFor.Checked[Integer(p)] then
      Include(available_for, p);
  end;
  FOptions.AvailableFor := available_for;
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

  LoadPlayers;

  UpdateControls();
end;

procedure THeroDefinitionFrame.Commit;
begin
  if cbExperience.Checked then
   begin
     FOptions.Experience := StrToQWordDef(edExperience.Text, 0);
   end
   else begin
     FOptions.Experience := 0;
   end;

   if cbName.Checked then
   begin
     FOptions.Name := edName.Text;
   end
   else begin
     FOptions.Name := '';
   end;

   if cbSex.Checked then
   begin
     FOptions.Sex := THeroSex(Integer(FCustomFemale));
   end
   else begin
     FOptions.Sex := THeroSex.default;
   end;

   if cbBiography.Checked then
   begin
     FOptions.Biography:=edBiography.Text;
   end
   else
   begin
     FOptions.Biography:='';
   end;
   SavePlayers;
end;

end.

