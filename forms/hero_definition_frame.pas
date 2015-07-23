unit hero_definition_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, hero_frame,
  Map;

type

  { THeroDefinitionFrame }

  THeroDefinitionFrame = class(THeroFrame)
  private
    FOptions: THeroDefinition;
  public
    procedure VisitHeroDefinition(AOptions: THeroDefinition); override;
    procedure Commit; override;
  end;

implementation

{$R *.lfm}

{ THeroDefinitionFrame }

procedure THeroDefinitionFrame.VisitHeroDefinition(AOptions: THeroDefinition);
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
end;

procedure THeroDefinitionFrame.Commit;
begin
  inherited Commit;
end;

end.

