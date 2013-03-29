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
    FObject: TOwnedObjectOptions;
    procedure SetupControls;
  private
    { private declarations }
  public
    { public declarations }
    procedure Commit; override;
    procedure VisitOwnedObject(AOptions: TOwnedObjectOptions); override;
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

procedure TFlaggableFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  SetupControls;
  inherited VisitOwnedObject(AOptions);
  FObject := AOptions;
  if AOptions.Owner = TPlayer.NONE then
  begin
    edOwnerRG.ItemIndex := 0; //no player = 255 -> index 0
  end
  else begin
    edOwnerRG.ItemIndex := Integer(AOptions.Owner)+1;
  end;

end;

end.

