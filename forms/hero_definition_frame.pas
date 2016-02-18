{ This file is a part of Map editor for VCMI project

  Copyright (C) 2015-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
unit hero_definition_frame;

{$I compilersetup.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComboEx, ExtCtrls, hero_frame, gui_helpers, Map, lists_manager, editor_types;

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
begin
  FOptions := AOptions;
  inherited VisitHeroDefinition(AOptions);

  cbPortrait.Checked:=FOptions.Portrait <> '';
  cbPortraitChange(cbPortrait);

  cbExperience.Checked := FOptions.Experience <> 0;
  edExperience.Text := IntToStr(FOptions.Experience);

  UpdateControls();
end;

procedure THeroDefinitionFrame.Commit;
begin
  Inherited Commit;

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


end;

end.

