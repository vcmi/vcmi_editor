unit flaggable_object_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, object_options, base_options_frame;

type

  { TFlaggableFrame }

  TFlaggableFrame = class(TBaseOptionsFrame)
    edOwnerRG: TRadioGroup;
  strict private
    FObject: TObjectOptions;
    procedure SetupControls;
  private
    procedure VisitOptions(AOptions:TObjectOptions);
  public
    { public declarations }
    procedure Commit; override;
    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions); override;
    procedure VisitRandomDwelling(AOptions: TRandomDwellingOptions); override;
    procedure VisitRandomDwellingLVL(AOptions: TRandomDwellingLVLOptions); override;
    procedure VisitRandomDwellingTown(AOptions: TRandomDwellingTownOptions); override;
    procedure VisitGarrison(AOptions: TGarrisonOptions); override;
  end;

implementation

uses Math, editor_types;

{$R *.lfm}

{ TFlaggableFrame }

procedure TFlaggableFrame.Commit;
begin
  inherited Commit;
  WriteOwner(FObject, edOwnerRG);
end;

procedure TFlaggableFrame.SetupControls;
begin
  ListsManager.FillWithPlayers(edOwnerRG.Items, True);
end;

procedure TFlaggableFrame.VisitOptions(AOptions: TObjectOptions);
begin
  SetupControls;
  FObject := AOptions;
  ReadOwner(FObject, edOwnerRG);
end;

procedure TFlaggableFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwelling(AOptions: TRandomDwellingOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwellingLVL(
  AOptions: TRandomDwellingLVLOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitRandomDwellingTown(
  AOptions: TRandomDwellingTownOptions);
begin
  VisitOptions(AOptions);
end;

procedure TFlaggableFrame.VisitGarrison(AOptions: TGarrisonOptions);
begin
  VisitOptions(AOptions);
end;

end.

