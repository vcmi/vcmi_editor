unit resource_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, base_object_options_frame, object_options;

type

  { TResourceFrame }

  TResourceFrame = class(TBaseObjectOptionsFrame)
    cbRandom: TCheckBox;
    GroupAmount: TGroupBox;
    edAmount: TSpinEdit;
    GroupMessage: TGroupBox;
    edMessage: TMemo;
    procedure cbRandomChange(Sender: TObject);
  private
    FOptions: TResourceOptions;
    procedure UpdateControls;
  public
    procedure Commit; override;
    procedure VisitResource(AOptions: TResourceOptions); override;
  end;

implementation

{$R *.lfm}

{ TResourceFrame }

procedure TResourceFrame.cbRandomChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TResourceFrame.UpdateControls;
begin
  edAmount.Enabled:=not cbRandom.Checked;
end;

procedure TResourceFrame.Commit;
begin
  inherited Commit;
  FOptions.GuardMessage := edMessage.Text;

  if cbRandom.Checked then
  begin
    FOptions.Amount:=0;
  end
  else begin
    FOptions.Amount:=edAmount.Value;
  end;
end;

procedure TResourceFrame.VisitResource(AOptions: TResourceOptions);
begin
  inherited VisitResource(AOptions);
  FOptions := AOptions;

  edAmount.Value:=AOptions.Amount;
  cbRandom.Checked:=AOptions.Amount = 0;

  edMessage.Text:=AOptions.GuardMessage;

  UpdateControls;
end;

end.

