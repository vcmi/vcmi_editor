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
  FObject.Owner := TPlayer(edOwnerRG.ItemIndex-1);
end;

procedure TFlaggableFrame.SetupControls;
var
  p: TPlayer;
begin
  edOwnerRG.Items.Clear;
  for p in TPlayer do
  begin
    edOwnerRG.Items.Add(ListsManager.PlayerName[p]);
  end;
end;

procedure TFlaggableFrame.VisitOwnedObject(AOptions: TOwnedObjectOptions);
begin
  SetupControls;
  inherited VisitOwnedObject(AOptions);
  FObject := AOptions;
  edOwnerRG.ItemIndex := Integer(AOptions.Owner)+1; //no player = -1 -> index 0
end;

end.

