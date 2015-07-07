unit flaggable_object_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, object_options, base_object_options_frame;

type

  { TFlaggableFrame }

  TFlaggableFrame = class(TBaseObjectOptionsFrame)
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

  if edOwnerRG.ItemIndex = 0 then
  begin
    FObject.Owner := TPlayer.NONE;
  end
  else begin
    FObject.Owner := TPlayer(edOwnerRG.ItemIndex-1);
  end;
end;

procedure TFlaggableFrame.SetupControls;
var
  p: TPlayer;
begin
  edOwnerRG.Items.Clear;
  edOwnerRG.Items.Add(ListsManager.PlayerName[TPlayer.NONE]);
  for p in TPlayerColor do
  begin
    edOwnerRG.Items.Add(ListsManager.PlayerName[p]);
  end;
end;

procedure TFlaggableFrame.VisitOptions(AOptions: TObjectOptions);
begin
  SetupControls;

  FObject := AOptions;
  if AOptions.Owner = TPlayer.NONE then
  begin
    edOwnerRG.ItemIndex := 0; //no player = 255 -> index 0
  end
  else begin
    edOwnerRG.ItemIndex := Integer(AOptions.Owner)+1;
  end;
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

